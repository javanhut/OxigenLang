pub mod opcode;
pub mod slot_types;

use crate::ast::*;
use crate::diagnostics::registry as codes;
use crate::diagnostics::{Code, Diagnostic};
use crate::token::{Span, Token};
use crate::vm::value::{
    Function, LocalInfo, ObjEnumDef, ParamInfo, Value, VmEnumVariantDef, VmEnumVariantKind, rc_str,
};
use opcode::{Chunk, OpCode};

/// Unwrap nested `Grouped(...)` wrappers to get the inner expression.
fn unwrap_grouped(expr: &Expression) -> &Expression {
    match expr {
        Expression::Grouped(inner) => unwrap_grouped(inner),
        other => other,
    }
}

/// Compile-time literal fold for walrus init RHS expressions. Recursively
/// evaluates pure-literal expression trees (Int / Float / Bool / None /
/// Char / Str leaves combined via Prefix / Infix / Grouped) and returns
/// `Some(Value)` when the entire tree is constant, otherwise `None`.
///
/// Falling back to `None` on failure (rather than partial folding) keeps
/// the contract simple: the caller emits a single `Constant` opcode iff
/// fold succeeds, otherwise compiles the original expression. Operators
/// that can fail at runtime (division/modulo by zero) intentionally
/// return `None` so the runtime error is preserved at the interpreter
/// level, matching `--no-jit` behaviour.
///
/// Shift semantics match `vm::binary_shl/shr` (count masked to low 6
/// bits via `wrapping_shl/shr` on i64). Integer arith uses wrapping
/// ops to match the runtime, which never panics on overflow in release.
fn fold_constant_expression(expr: &Expression) -> Option<Value> {
    match expr {
        Expression::Int { value, .. } => Some(Value::Integer(*value)),
        Expression::Float { value, .. } => Some(Value::Float(*value)),
        Expression::Boolean { value, .. } => Some(Value::Boolean(*value)),
        Expression::NoneExpr { .. } => Some(Value::None),
        Expression::Char { value, .. } => Some(Value::Char(*value)),
        Expression::Str { value, .. } => Some(Value::String(rc_str(value.as_str()))),
        Expression::Grouped(inner) => fold_constant_expression(inner),

        Expression::Prefix {
            operator, right, ..
        } => {
            let r = fold_constant_expression(right)?;
            match (operator.as_str(), r) {
                ("-", Value::Integer(n)) => Some(Value::Integer(n.wrapping_neg())),
                ("-", Value::Float(f)) => Some(Value::Float(-f)),
                ("!" | "not", Value::Boolean(b)) => Some(Value::Boolean(!b)),
                ("~", Value::Integer(n)) => Some(Value::Integer(!n)),
                _ => None,
            }
        }

        Expression::Infix {
            operator,
            left,
            right,
            ..
        } => {
            let l = fold_constant_expression(left)?;
            let r = fold_constant_expression(right)?;
            match (operator.as_str(), &l, &r) {
                // Integer arithmetic
                ("+", Value::Integer(a), Value::Integer(b)) => {
                    Some(Value::Integer(a.wrapping_add(*b)))
                }
                ("-", Value::Integer(a), Value::Integer(b)) => {
                    Some(Value::Integer(a.wrapping_sub(*b)))
                }
                ("*", Value::Integer(a), Value::Integer(b)) => {
                    Some(Value::Integer(a.wrapping_mul(*b)))
                }
                // checked, not wrapping: folding int_min / -1 gave a different answer than the same expression via variables.
                ("/", Value::Integer(a), Value::Integer(b)) if *b != 0 => {
                    a.checked_div(*b).map(Value::Integer)
                }
                ("%", Value::Integer(a), Value::Integer(b)) if *b != 0 => {
                    a.checked_rem(*b).map(Value::Integer)
                }
                // Integer bitwise / shift (same semantics as vm::binary_b*)
                ("&", Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a & b)),
                ("|", Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a | b)),
                ("^", Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a ^ b)),
                ("<<", Value::Integer(a), Value::Integer(b)) => {
                    Some(Value::Integer(a.wrapping_shl(*b as u32)))
                }
                (">>", Value::Integer(a), Value::Integer(b)) => {
                    Some(Value::Integer(a.wrapping_shr(*b as u32)))
                }
                // Integer comparison
                ("==", Value::Integer(a), Value::Integer(b)) => Some(Value::Boolean(a == b)),
                ("!=", Value::Integer(a), Value::Integer(b)) => Some(Value::Boolean(a != b)),
                ("<", Value::Integer(a), Value::Integer(b)) => Some(Value::Boolean(a < b)),
                ("<=", Value::Integer(a), Value::Integer(b)) => Some(Value::Boolean(a <= b)),
                (">", Value::Integer(a), Value::Integer(b)) => Some(Value::Boolean(a > b)),
                (">=", Value::Integer(a), Value::Integer(b)) => Some(Value::Boolean(a >= b)),
                // Float arithmetic + comparison
                ("+", Value::Float(a), Value::Float(b)) => Some(Value::Float(a + b)),
                ("-", Value::Float(a), Value::Float(b)) => Some(Value::Float(a - b)),
                ("*", Value::Float(a), Value::Float(b)) => Some(Value::Float(a * b)),
                ("/", Value::Float(a), Value::Float(b)) if *b != 0.0 => Some(Value::Float(a / b)),
                ("==", Value::Float(a), Value::Float(b)) => Some(Value::Boolean(a == b)),
                ("!=", Value::Float(a), Value::Float(b)) => Some(Value::Boolean(a != b)),
                ("<", Value::Float(a), Value::Float(b)) => Some(Value::Boolean(a < b)),
                ("<=", Value::Float(a), Value::Float(b)) => Some(Value::Boolean(a <= b)),
                (">", Value::Float(a), Value::Float(b)) => Some(Value::Boolean(a > b)),
                (">=", Value::Float(a), Value::Float(b)) => Some(Value::Boolean(a >= b)),
                // Boolean short-circuit (eager since both sides folded)
                ("and", Value::Boolean(a), Value::Boolean(b)) => Some(Value::Boolean(*a && *b)),
                ("or", Value::Boolean(a), Value::Boolean(b)) => Some(Value::Boolean(*a || *b)),
                _ => None,
            }
        }

        _ => None,
    }
}

/// Compilation error with source location.
///
/// Carries a [`Code`] and, where the site has one, a real [`Span`]. Sites that
/// only know a line still work — `into_diagnostic` widens a bare line into a
/// point span — so span plumbing can be finished incrementally rather than as
/// one flag-day change.
#[derive(Debug)]
pub struct CompileError {
    pub message: String,
    pub line: u32,
    pub code: Code,
    pub span: Option<Span>,
    /// Pre-built diagnostic for sites that attach labels, notes or helps.
    diagnostic: Option<Diagnostic>,
}

impl CompileError {
    pub fn into_diagnostic(self) -> Diagnostic {
        if let Some(d) = self.diagnostic {
            return d;
        }
        let span = self
            .span
            .unwrap_or_else(|| Span::new(self.line as usize, 0));
        Diagnostic::error(self.code, span, self.message)
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] Compile error: {}", self.line, self.message)
    }
}

/// Why a binding is (im)mutable, and where that was decided.
///
/// This replaced a bare `mutable: bool`. A boolean records *that* a write is
/// forbidden but not *why*, so the advice had to be guessed at the reporting
/// site — which is how a loop variable came to be told "use `:=` to override",
/// advice that produces a rebinding discarded at the end of the iteration.
/// Carrying the reason (and the span that established it) lets one reporting
/// function derive the right message and point at the declaration.
#[derive(Debug, Clone, Copy)]
enum BindingKind {
    Mutable,
    /// `x <int> = 5` — declared immutable.
    ImmutableConstant {
        decl: Span,
    },
    /// The variable of an `each` loop; rebound every iteration.
    ImmutableLoopVar {
        header: Span,
    },
    /// A block-scoped `struct`/`enum` name.
    ImmutableTypeName {
        decl: Span,
    },
}

impl BindingKind {
    fn is_mutable(self) -> bool {
        matches!(self, BindingKind::Mutable)
    }
}

/// Names of compiler-made locals. A NUL can't appear in an identifier, so
/// user code can neither read nor overwrite them (as `__index__` it could
/// end or redirect a loop).
const LOOP_INDEX: &str = "\u{0}index";
const LOOP_RANGE_END: &str = "\u{0}range_end";
const LOOP_RANGE_LIMIT: &str = "\u{0}range_limit";
const LOOP_ITERABLE: &str = "\u{0}iterable";
const CHOOSE_SUBJECT: &str = "\u{0}choose";

/// Tracks a local variable during compilation.
#[derive(Debug, Clone)]
struct Local {
    name: String,
    depth: i32, // -1 means uninitialized
    is_captured: bool,
    kind: BindingKind,
    type_constraint: Option<String>,
}

/// Describes how an upvalue is captured.
#[derive(Debug, Clone)]
struct UpvalueDesc {
    index: u16,
    is_local: bool,
}

/// A single compilation frame (one per function/script).
struct CompilerFrame {
    function: Function,
    locals: Vec<Local>,
    upvalues: Vec<UpvalueDesc>,
    scope_depth: i32,
    /// Start of the current innermost loop (back-edge target).
    loop_starts: Vec<usize>,
    /// Pending `stop`/break jump offsets to patch.
    loop_exits: Vec<Vec<usize>>,
    /// Pending `skip`/continue jump offsets to patch to the loop's continue
    /// target (each: the index increment; repeat: the back-edge).
    loop_continues: Vec<Vec<usize>>,
    /// Per-loop local-stack floor: the number of locals that must remain when
    /// `skip` jumps to the continue target. `skip` emits a Pop/CloseUpvalue for
    /// every local above this floor (without removing them from compile-time
    /// scope, since code after the `skip` still references them).
    loop_continue_floors: Vec<usize>,
    /// Per-loop local-stack floor for `stop`: the height the loop's EXIT target
    /// expects. For `each` this excludes the loop variable (the exit path never
    /// pushed it that iteration); for `repeat` it is the pre-body height. `stop`
    /// emits a Pop/CloseUpvalue for every local above this floor before jumping
    /// to the exit, so it doesn't leak the loop variable / body locals (which
    /// would corrupt an enclosing loop's iterator).
    loop_exit_floors: Vec<usize>,
    /// Count of error-handler regions currently open lexically in this frame
    /// (incremented by `PushHandler`, decremented by `PopHandler`).
    handler_depth: usize,
    /// `handler_depth` captured at each enclosing loop's entry, so `stop`/`skip`
    /// can emit balancing `PopHandler`s for handlers opened inside the loop body
    /// before jumping out of / around the body.
    loop_handler_depths: Vec<usize>,
    /// Lexical nesting depth of `each` loop BODIES currently being compiled in
    /// this frame. The tree-walker gives every `each` iteration a fresh enclosed
    /// env (so a typed re-declaration `x <int> := …` of an untyped outer global
    /// SHADOWS), while `repeat`/plain blocks reuse the enclosing env (so the same
    /// re-declaration UPDATES the global). `repeat` therefore does NOT bump this;
    /// only `each` bodies do, and the V1-typed update-vs-shadow choice keys off it.
    each_body_depth: usize,
    /// Every `SetLocal` emitted: `(offset, slot, lock of the binding written)`.
    /// A slot's runtime lock is dropped when bindings of different types share
    /// it, so `finalize_local_locks` re-attaches each typed binding's lock to
    /// its own writes.
    local_writes: Vec<(usize, u16, Option<String>)>,
    /// Values the expression being compiled has pushed and not yet consumed
    /// (a call's callee and earlier arguments, an operator's left operand).
    /// They sit above the locals, so a block expression compiled now must
    /// number its locals past them; see `compile_block_as_expression`.
    temps: usize,
}

impl CompilerFrame {
    fn new(name: Option<String>, scope_depth: i32) -> Self {
        let mut function = Function::new(name, 0);
        function.locals.push(LocalInfo::default());
        // Slot 0 is reserved for the function itself (or `self` in methods).
        let locals = vec![Local {
            name: String::new(),
            depth: scope_depth,
            is_captured: false,
            kind: BindingKind::ImmutableConstant {
                decl: Span::default(),
            },
            type_constraint: None,
        }];
        CompilerFrame {
            function,
            locals,
            upvalues: Vec::new(),
            scope_depth,
            loop_starts: Vec::new(),
            loop_exits: Vec::new(),
            loop_continues: Vec::new(),
            loop_continue_floors: Vec::new(),
            loop_exit_floors: Vec::new(),
            handler_depth: 0,
            loop_handler_depths: Vec::new(),
            each_body_depth: 0,
            local_writes: Vec::new(),
            temps: 0,
        }
    }
}

/// Compiles an AST into bytecode.
pub struct Compiler {
    frames: Vec<CompilerFrame>,
    errors: Vec<CompileError>,
    /// When true, the next Let/TypedLet/TypedDeclare should Dup the value
    /// before defining the variable so it's preserved for implicit return.
    dup_next_define: bool,
    /// When true, Choose/If won't pop their result value (used when they're
    /// the last statement of a block that needs to produce a value).
    suppress_statement_pop: bool,
    /// Whether the value produced by the expression currently being compiled
    /// is CONSUMED by an enclosing operation (operator operand, call argument,
    /// assignment/Let RHS, give/return value, etc.) vs. DISCARDED (the top
    /// expression of an expression-statement, whose value is immediately
    /// Pop'd). Default `true`; set `false` only for the single top expression
    /// directly under a `Statement::Expr`. Used to make a `skip`/`stop` whose
    /// value is consumed an `option`/`choose` arm tail a COMPILE error
    /// (matching the tree-walker, which errors at runtime on `INTEGER + SKIP`),
    /// while leaving a `skip`/`stop` in a discarded arm (legitimate loop
    /// control flow) compiling and running.
    value_consumed: bool,
    /// Column stack for source-location tracking. Pushed at the start of
    /// `compile_expression`/`compile_statement` with the current node's
    /// column, popped at exit. `emit_byte` reads the top before each
    /// write so opcodes emitted by a parent *between* two child compiles
    /// (e.g., the `Add` in `compile_left; compile_right; emit Add`) get
    /// the parent's column, not whatever column the last child left
    /// behind on the chunk.
    loc_column_stack: Vec<u32>,
    /// Field names of each declared struct (own fields), keyed by struct name.
    /// Used to resolve bare identifiers in method bodies to `self.field`.
    struct_fields: std::collections::HashMap<String, Vec<String>>,
    /// Parent struct of each declared struct, for walking the inheritance
    /// chain when resolving implicit-self fields.
    struct_parents: std::collections::HashMap<String, Option<String>>,
    /// Stack of resolvable self-field sets for the method currently being
    /// compiled (top = innermost). Empty outside method bodies.
    method_field_stack: Vec<std::collections::HashSet<String>>,
    /// Struct whose `includes` block is being compiled, so each method (and
    /// anything nested in it) can be stamped with its owner for hidden-field
    /// access checks.
    method_struct_stack: Vec<String>,
    /// Names defined as globals (top-level `:=` / `<type>` declarations), in
    /// compilation order. A walrus `:=` of one of these from inside a nested
    /// block updates the global (SetGlobal) rather than creating a shadowing
    /// local — matching the tree-walker, whose `:=` updates an existing binding
    /// found anywhere up the scope chain.
    declared_globals: std::collections::HashSet<String>,
    /// Subset of `declared_globals` that were declared WITH a type annotation
    /// (`x <int> = 0` / `x <int> := 0` / `x <int>`). A typed re-declaration of an
    /// existing *untyped* global from a nested block UPDATES that global (matching
    /// the tree-walker, where `repeat` shares the enclosing environment so
    /// `set_typed` overwrites the existing binding). A typed re-declaration of an
    /// already-*typed* global instead SHADOWS (the documented shadowing example:
    /// `x <int> = 10; each { x <str> = "hi" }` leaves the outer `x` at 10),
    /// matching the tree-walker, where `each` introduces a fresh per-iteration
    /// environment that `set_typed` writes the shadow into.
    typed_globals: std::collections::HashSet<String>,
    /// Id stamped onto the next `Function` finalized: this compilation's
    /// unit number in the high 32 bits, compile order in the low 32. Every
    /// compilation in the process gets its own unit, so no two compiled
    /// files share an id, and a worker recompiling the same program under
    /// the same unit (`Compiler::for_unit`) assigns identical ids (closure
    /// transfer across threads relies on this to identify a function's code).
    fn_counter: u64,
    /// What `Compiler::check` collects on top of compiling; `None` for an
    /// ordinary compile, which records nothing.
    lint: Option<Box<Lint>>,
}

/// Names a program reads and defines as globals, and warnings about code that
/// compiles but is very likely wrong. See `Compiler::check`.
#[derive(Default)]
struct Lint {
    reads: Vec<(String, Span)>,
    defs: std::collections::HashSet<String>,
    warnings: Vec<Diagnostic>,
}

/// A literal whose truth value never changes, as `or`/`and` see it.
/// A literal as it is written in source, quotes and all.
fn written(expr: &Expression, token: &Token) -> String {
    match unwrap_grouped(expr) {
        Expression::Str { value, .. } => format!("{value:?}"),
        _ => token.literal.clone(),
    }
}

fn constant_operand(expr: &Expression) -> Option<&Token> {
    match unwrap_grouped(expr) {
        Expression::Str { token, .. }
        | Expression::Int { token, .. }
        | Expression::Float { token, .. }
        | Expression::Char { token, .. } => Some(token),
        _ => None,
    }
}

fn is_comparison(expr: &Expression) -> bool {
    matches!(unwrap_grouped(expr),
        Expression::Infix { operator, .. } if matches!(operator.as_str(), "==" | "!=" | "<" | ">" | "<=" | ">="))
}

/// The next unused compilation unit number.
static NEXT_UNIT: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self::for_unit(NEXT_UNIT.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }

    /// A compiler whose functions get the ids an earlier compilation of the
    /// same program under `unit` gave them (see `Function::id`).
    pub fn for_unit(unit: u32) -> Self {
        let mut compiler = Compiler {
            frames: Vec::new(),
            errors: Vec::new(),
            dup_next_define: false,
            suppress_statement_pop: false,
            value_consumed: true,
            loc_column_stack: Vec::new(),
            struct_fields: std::collections::HashMap::new(),
            struct_parents: std::collections::HashMap::new(),
            method_field_stack: Vec::new(),
            method_struct_stack: Vec::new(),
            declared_globals: std::collections::HashSet::new(),
            typed_globals: std::collections::HashSet::new(),
            fn_counter: (unit as u64) << 32,
            lint: None,
        };
        compiler.frames.push(CompilerFrame::new(None, 0));
        compiler
    }

    /// Compile `program` for `oxigen check`: its compile errors, plus a name
    /// the program reads that nothing defines (an error, since it fails when
    /// it runs), plus warnings for logic that compiles but is almost
    /// certainly a mistake. `known` says which names exist without being
    /// defined in the program: builtins, and whatever the host adds.
    pub fn check(mut self, program: &Program, known: &dyn Fn(&str) -> bool) -> Vec<Diagnostic> {
        self.lint = Some(Box::default());
        let mut lint = None;
        let mut out = Vec::new();
        if let Err(errors) = self.compile_collecting(program, &mut lint) {
            out.extend(errors.into_iter().map(CompileError::into_diagnostic));
        }
        let Some(lint) = lint else {
            return out;
        };
        let mut reported = std::collections::HashSet::new();
        for (name, span) in lint.reads {
            if lint.defs.contains(&name) || known(&name) || !reported.insert(name.clone()) {
                continue;
            }
            out.push(
                Diagnostic::error(codes::UNDEFINED_NAME, span, format!("undefined name `{name}`"))
                    .label("nothing in this program defines it")
                    .help("define it with `name := value`, import it with `introduce`, or check the spelling"),
            );
        }
        out.extend(lint.warnings);
        out
    }

    /// Compile a program and return the top-level function.
    pub fn compile(self, program: &Program) -> Result<Function, Vec<CompileError>> {
        self.compile_collecting(program, &mut None)
    }

    /// `compile`, handing back what the lint collected (if it was on).
    fn compile_collecting(
        mut self,
        program: &Program,
        lint: &mut Option<Box<Lint>>,
    ) -> Result<Function, Vec<CompileError>> {
        let result = self.compile_program(program);
        *lint = self.lint.take();
        result
    }

    fn compile_program(&mut self, program: &Program) -> Result<Function, Vec<CompileError>> {
        // The last expression statement's value is the program's return value.
        if program.statements.is_empty() {
            self.emit_op(OpCode::None, 0);
            self.emit_op(OpCode::Return, 0);
        } else {
            for (i, stmt) in program.statements.iter().enumerate() {
                let is_last = i == program.statements.len() - 1;
                if is_last {
                    match stmt {
                        Statement::Expr(expr) => {
                            self.compile_expression(expr);
                            self.emit_op(OpCode::Return, 0);
                        }
                        // Named function definitions are not implicitly returned.
                        Statement::Let {
                            value: Expression::FunctionLiteral { .. },
                            ..
                        } => {
                            self.compile_statement(stmt);
                            self.emit_op(OpCode::None, 0);
                            self.emit_op(OpCode::Return, 0);
                        }
                        Statement::Let { .. }
                        | Statement::TypedLet { .. }
                        | Statement::TypedDeclare { .. } => {
                            // Preserve the value for implicit program return
                            self.dup_next_define = true;
                            self.compile_statement(stmt);
                            self.dup_next_define = false;
                            self.emit_op(OpCode::Return, 0);
                        }
                        _ => {
                            self.compile_statement(stmt);
                            self.emit_op(OpCode::None, 0);
                            self.emit_op(OpCode::Return, 0);
                        }
                    }
                } else {
                    self.compile_statement(stmt);
                }
            }
        }

        if !self.errors.is_empty() {
            return Err(std::mem::take(&mut self.errors));
        }

        self.finalize_local_locks();
        let frame = self.frames.pop().unwrap();
        let mut function = frame.function;
        function.id = self.fn_counter;
        function.hidden_globals = program.hidden.clone();
        self.fn_counter += 1;
        Ok(function)
    }

    // ── Helpers ────────────────────────────────────────────────────────

    fn lint_warn(&mut self, span: Span, message: String, help: &str) {
        if let Some(lint) = &mut self.lint {
            lint.warnings
                .push(Diagnostic::warning(codes::SUSPICIOUS_LOGIC, span, message).help(help));
        }
    }

    /// `x == "a" or "b"` and `("a" or "b")` compile, and are almost never
    /// what was meant. A literal on the right of a plain value is fine:
    /// `name or "anonymous"` is a fallback.
    fn lint_logical(&mut self, op: &str, left: &Expression, right: &Expression) {
        if self.lint.is_none() {
            return;
        }
        if let Some(lit) = constant_operand(left) {
            let text = written(left, lit);
            let message = match op {
                "or" => format!("`{text} or ...` is always {text}: a literal is never false"),
                _ => format!(
                    "`{text} and ...` is always the right-hand side: a literal is never false"
                ),
            };
            self.lint_warn(
                lit.span,
                message,
                "to compare against several values, write `x == (a or b)`, or `x == a or x == b`",
            );
        } else if let Some(lit) = constant_operand(right).filter(|_| is_comparison(left)) {
            let text = written(right, lit);
            self.lint_warn(
                lit.span,
                format!("{text} here is a value on its own, not part of the comparison, so this is always true"),
                "`x == a or b` means `(x == a) or b`; write `x == (a or b)`, or `x == a or x == b`",
            );
        }
    }

    /// Give every write to a typed local its binding's lock back where the
    /// slot's own lock could not hold it (another binding of a different type
    /// shares the slot). Those writes become `SetLocalLocked`, which the JIT
    /// declines, so only functions with such a clash leave the fast path.
    fn finalize_local_locks(&mut self) {
        let writes = std::mem::take(&mut self.current_frame_mut().local_writes);
        for (offset, slot, lock) in writes {
            let Some(lock) = lock.filter(|t| t != "GENERIC") else {
                continue;
            };
            let slot_lock = self
                .current_frame()
                .function
                .locals
                .get(slot as usize)
                .and_then(|li| li.type_constraint.as_deref());
            if slot_lock == Some(lock.as_str()) {
                continue;
            }
            let line = self
                .current_frame()
                .function
                .chunk
                .lines
                .get(offset)
                .copied()
                .unwrap_or(0);
            let tc = self.make_constant(Value::String(rc_str(lock.as_str())), line);
            let chunk = self.current_chunk();
            chunk.code[offset] = OpCode::SetLocalLocked as u8;
            chunk.local_locks.push((offset, tc));
        }
        self.current_chunk()
            .local_locks
            .sort_unstable_by_key(|&(o, _)| o);
    }

    fn current_frame(&self) -> &CompilerFrame {
        self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CompilerFrame {
        self.frames.last_mut().unwrap()
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.current_frame_mut().function.chunk
    }

    fn emit_byte(&mut self, byte: u8, line: u32) {
        // A recursive child compile leaves its own cur_column behind, so re-establish the parent's.
        if let Some(&col) = self.loc_column_stack.last() {
            self.current_chunk().set_loc_column(col);
        }
        self.current_chunk().write(byte, line);
    }

    fn emit_op(&mut self, op: OpCode, line: u32) {
        if matches!(op, OpCode::Loop) {
            self.current_frame_mut().function.has_loop = true;
        }
        self.emit_byte(op as u8, line);
    }

    fn emit_op_u16(&mut self, op: OpCode, operand: u16, line: u32) {
        // Every way a program defines a global comes through here.
        if self.lint.is_some()
            && matches!(
                op,
                OpCode::DefineGlobal | OpCode::DefineGlobalTyped | OpCode::SetGlobal
            )
            && let Some(Value::String(name)) = self
                .current_chunk()
                .constants
                .get(operand as usize)
                .cloned()
            && let Some(lint) = &mut self.lint
        {
            lint.defs.insert(name.to_string());
        }
        if op == OpCode::SetLocal {
            let offset = self.current_chunk().len();
            let frame = self.current_frame_mut();
            let lock = frame
                .locals
                .get(operand as usize)
                .and_then(|l| l.type_constraint.clone());
            frame.local_writes.push((offset, operand, lock));
        }
        self.emit_op(op, line);
        let chunk = self.current_chunk();
        chunk.write_u16(operand, line);
    }

    fn emit_op_u8(&mut self, op: OpCode, operand: u8, line: u32) {
        self.emit_op(op, line);
        self.emit_byte(operand, line);
    }

    fn make_constant(&mut self, value: Value, line: u32) -> u16 {
        // Interning lets Value::eq short-circuit on Rc::ptr_eq; bounded by the program text.
        let value = match value {
            Value::String(s) => Value::String(crate::vm::intern::intern(&s)),
            other => other,
        };
        let idx = self.current_chunk().add_constant(value);
        if idx > u16::MAX as usize {
            self.error_coded(codes::CHUNK_LIMIT, "Too many constants in one chunk", line);
        }
        idx as u16
    }

    fn emit_constant(&mut self, value: Value, line: u32) {
        let idx = self.make_constant(value, line);
        self.emit_op_u16(OpCode::Constant, idx, line);
    }

    /// Emit a jump instruction and return the offset to patch later.
    fn emit_jump(&mut self, op: OpCode, line: u32) -> usize {
        self.emit_op(op, line);
        let chunk = self.current_chunk();
        let offset = chunk.len();
        chunk.write_u16(0xFFFF, line); // placeholder
        offset
    }

    /// Emit `PushHandler` with a patchable catch-target offset (patched via
    /// `patch_jump`), tracking the now-open handler region. Returns the operand
    /// position to patch.
    fn emit_push_handler(&mut self, line: u32) -> usize {
        let pos = self.emit_jump(OpCode::PushHandler, line);
        self.current_frame_mut().handler_depth += 1;
        pos
    }

    /// Emit `PopHandler` for a handler region's normal-path exit.
    fn emit_pop_handler(&mut self, line: u32) {
        self.emit_op(OpCode::PopHandler, line);
        self.current_frame_mut().handler_depth -= 1;
    }

    /// Emit `PopHandler` for every handler region opened inside the innermost
    /// loop body, for use right before a `stop`/`skip` jump leaves that body.
    /// Does NOT change the lexical `handler_depth` — the handlers remain open on
    /// the fall-through path of a conditional `stop when`/`skip when`.
    fn emit_loop_handler_unwind(&mut self, line: u32) {
        let floor = self
            .current_frame()
            .loop_handler_depths
            .last()
            .copied()
            .unwrap_or(0);
        let n = self.current_frame().handler_depth.saturating_sub(floor);
        for _ in 0..n {
            self.emit_op(OpCode::PopHandler, line);
        }
    }

    /// Patch a previously emitted jump with the current position.
    fn patch_jump(&mut self, offset: usize) {
        let current = self.current_chunk().len();
        let jump = current - offset - 2; // subtract the 2 bytes of the operand itself
        if jump > u16::MAX as usize {
            self.error_coded(codes::CHUNK_LIMIT, "Jump too large", 0);
            return;
        }
        self.current_chunk().patch_u16(offset, jump as u16);
    }

    /// Emit a backward loop jump.
    fn emit_loop(&mut self, loop_start: usize, line: u32) {
        self.emit_op(OpCode::Loop, line);
        let offset = self.current_chunk().len() + 2 - loop_start;
        if offset > u16::MAX as usize {
            self.error_coded(codes::CHUNK_LIMIT, "Loop body too large", line);
        }
        self.current_chunk().write_u16(offset as u16, line);
    }

    /// Reports with a code and a real span.
    fn error_at(&mut self, code: Code, span: Span, message: &str) {
        self.errors.push(CompileError {
            message: message.to_string(),
            line: span.line() as u32,
            code,
            span: Some(span),
            diagnostic: None,
        });
    }

    /// Reports a fully-formed diagnostic (labels, notes, helps).
    fn report(&mut self, d: Diagnostic) {
        self.errors.push(CompileError {
            message: d.message.clone(),
            line: d.span().line() as u32,
            code: d.code,
            span: Some(d.span()),
            diagnostic: Some(d),
        });
    }

    /// Reports a write to an immutable local.
    ///
    /// One function, three outcomes, each derived from the binding's recorded
    /// provenance rather than guessed here. This is why `BindingKind` exists:
    /// with only a boolean, the reporting site had no way to know that "use
    /// `:=` to override" — correct for a constant — is misleading for a loop
    /// variable, whose rebinding lasts one iteration.
    fn report_immutable_write(
        &mut self,
        name: &str,
        kind: BindingKind,
        write: Span,
        verb: &str,
        suffix: &str,
    ) {
        let d = match kind {
            BindingKind::ImmutableLoopVar { header } => Diagnostic::error(
                codes::IMMUTABLE_ASSIGN,
                write,
                format!("cannot {verb} loop variable `{name}`{suffix}"),
            )
            .label(format!("cannot {verb}{suffix}"))
            .secondary(header, format!("`{name}` is rebound on each iteration"))
            .note("assigning to the loop variable does not change which values are iterated")
            .help("use a separate accumulator, or index into the sequence"),

            BindingKind::ImmutableConstant { decl } => {
                let d = Diagnostic::error(
                    codes::IMMUTABLE_ASSIGN,
                    write,
                    format!("cannot {verb} immutable variable `{name}`{suffix}"),
                )
                .label(format!("cannot {verb}{suffix}"))
                .note("`=` binds immutably; `:=` binds mutably");
                // A synthetic declaration span has nothing useful to point at.
                if decl == Span::default() {
                    d.help(format!(
                        "declare it with `{name} := ...` to allow reassignment"
                    ))
                } else {
                    d.secondary(decl, "declared immutable here with `=`")
                        .help(format!(
                            "declare it with `{name} := ...` to allow reassignment"
                        ))
                }
            }

            BindingKind::ImmutableTypeName { decl } => Diagnostic::error(
                codes::TYPE_NAME_REBIND,
                write,
                format!("cannot {verb} type name `{name}`{suffix}"),
            )
            .label(format!("cannot {verb}{suffix}"))
            .secondary(decl, format!("`{name}` is declared here as a type"))
            .help("choose a different name for the value"),

            BindingKind::Mutable => return,
        };
        self.report(d);
    }

    /// Rejects a struct or enum named like a built-in type. Type checks match
    /// on names, so instances of `struct INTEGER` would pass every `<int>`.
    fn reject_builtin_type_name(&mut self, name: &Identifier) {
        if crate::vm::is_builtin_type_name(&name.value) {
            self.error_at(
                codes::COMPILE_ERROR,
                name.token.span,
                &format!("`{}` is reserved for a built-in type", name.value),
            );
        }
    }

    /// Rejects a pattern declared with more than one parameter.
    ///
    /// A pattern is always called with exactly one argument — the value being
    /// matched (`Call, 1` at every use site) — so there is no syntax that could
    /// ever supply a second. The extra parameters are not merely dead: binding
    /// them to `None` produced a runtime failure inside the synthesized
    /// `__pattern__` function, at a location the user never wrote.
    fn check_pattern_arity(&mut self, params: &[Identifier], decl: Span) {
        if params.len() == 1 {
            return;
        }

        if params.is_empty() {
            // Declaring none only works because a surplus argument is silently discarded.
            self.report(
                Diagnostic::error(
                    codes::PATTERN_ARITY,
                    decl,
                    "a pattern takes exactly one parameter",
                )
                .label("no parameter to bind the matched value to")
                .note("a pattern is called with the value being matched")
                .help("name the value, as in `pattern big(n) when n > 3`"),
            );
            return;
        }

        let first_extra = &params[1];
        let last = params.last().unwrap_or(first_extra);
        let span = first_extra.token.span.to(last.token.span);
        let plural = if params.len() > 2 { "s are" } else { " is" };
        let names = params[1..]
            .iter()
            .map(|p| format!("`{}`", p.value))
            .collect::<Vec<_>>()
            .join(", ");
        self.report(
            Diagnostic::error(
                codes::PATTERN_ARITY,
                span,
                "a pattern takes exactly one parameter",
            )
            .label(format!("{names} never bound"))
            .note(format!(
                "a pattern is called with the value being matched, so only the \
                 first parameter is supplied; the extra parameter{plural} bound \
                 to `None`"
            ))
            .help(
                "remove the extra parameters, or compare against a value from the enclosing scope",
            ),
        );
    }

    /// Reports a misused `skip`/`stop`.
    ///
    /// Which problem this is depends on the *fact*, not on where the keyword
    /// happens to sit. With no enclosing loop the keyword has nothing to
    /// control — that is the error, whether or not it also lands in value
    /// position. Only when a loop IS present is "used as a value" the real
    /// story. Previously the site decided, so `fun f() { skip }` and
    /// `fun f() { skip\n 0 }` — the same mistake — reported different errors
    /// purely because one had `skip` as the last statement.
    fn report_skip_stop_misuse(&mut self, is_skip: bool, span: Span, value_consumed: bool) {
        let kw = if is_skip { "skip" } else { "stop" };
        let in_loop = if is_skip {
            !self.current_frame().loop_continue_floors.is_empty()
        } else {
            !self.current_frame().loop_exit_floors.is_empty()
        };

        if !in_loop {
            self.report(
                Diagnostic::error(
                    codes::SKIP_STOP_OUTSIDE_LOOP,
                    span,
                    format!("'{kw}' used outside of loop"),
                )
                .label(format!("no loop for `{kw}` to control"))
                .note(format!(
                    "`{kw}` applies to the loop it is written inside; a function \
                     called from a loop cannot control it"
                ))
                .help("return a value and let the caller decide"),
            );
            return;
        }

        if value_consumed {
            self.report(
                Diagnostic::error(
                    codes::SKIP_STOP_AS_VALUE,
                    span,
                    format!("'{kw}' cannot be used as a value"),
                )
                .label("this produces no value")
                .note(format!(
                    "`{kw}` jumps before the surrounding expression can produce a \
                     value, so there is nothing to use"
                )),
            );
        }
    }

    /// Reports at a line only. Retained for sites that have no span to hand;
    /// they render without a caret until one is plumbed through.
    /// How to load `self` when the bare, non-local `name` means `self.name`: it
    /// is a field of the struct whose method is being compiled, and nothing
    /// nearer binds it. In a closure nested in the method `self` is an upvalue —
    /// resolving only a local `self` sent the closure's field reads and writes
    /// to globals instead.
    fn implicit_self_for(&mut self, name: &str) -> Option<(OpCode, u16)> {
        if !self
            .method_field_stack
            .last()
            .is_some_and(|f| f.contains(name))
        {
            return None;
        }
        if let Some(slot) = self.resolve_local("self") {
            return Some((OpCode::GetLocal, slot));
        }
        if self.upvalue_binding(name).is_some() || self.upvalue_binding("self").is_none() {
            return None;
        }
        let frame_idx = self.frames.len() - 1;
        self.resolve_upvalue(frame_idx, "self")
            .map(|uv| (OpCode::GetUpvalue, uv))
    }

    /// Push the value of the variable `name`: local, then upvalue, then global.
    fn emit_get_variable(&mut self, name: &str, line: u32) {
        if let Some(slot) = self.resolve_local(name) {
            self.emit_op_u16(OpCode::GetLocal, slot, line);
            return;
        }
        let frame_idx = self.frames.len() - 1;
        if let Some(uv_idx) = self.resolve_upvalue(frame_idx, name) {
            self.emit_op_u16(OpCode::GetUpvalue, uv_idx, line);
        } else {
            let name_const = self.make_constant(Value::String(rc_str(name)), line);
            self.emit_op_u16(OpCode::GetGlobal, name_const, line);
        }
    }

    /// Record `n` values left on the operand stack while the next operand compiles.
    fn hold_temps(&mut self, n: usize) {
        self.current_frame_mut().temps += n;
    }

    /// The operation consuming `n` held values is about to be emitted.
    fn release_temps(&mut self, n: usize) {
        let frame = self.current_frame_mut();
        debug_assert!(frame.temps >= n, "released more temporaries than were held");
        frame.temps = frame.temps.saturating_sub(n);
    }

    /// Argument, parameter and element counts are encoded in one byte; a
    /// larger count used to wrap and silently call or build the wrong thing.
    fn count_u8(&mut self, count: usize, line: u32) -> u8 {
        u8::try_from(count).unwrap_or_else(|_| {
            self.error_coded(
                codes::COMPILE_ERROR,
                &format!("too many items in one list: {count} (at most 255 arguments, parameters or names)"),
                line,
            );
            u8::MAX
        })
    }

    fn error_coded(&mut self, code: Code, message: &str, line: u32) {
        self.errors.push(CompileError {
            message: message.to_string(),
            line,
            code,
            span: None,
            diagnostic: None,
        });
    }

    fn begin_scope(&mut self) {
        self.current_frame_mut().scope_depth += 1;
    }

    fn end_scope(&mut self, line: u32) {
        self.current_frame_mut().scope_depth -= 1;
        let depth = self.current_frame().scope_depth;

        let mut ops: Vec<bool> = Vec::new(); // true = captured, false = not
        while let Some(local) = self.current_frame().locals.last() {
            if local.depth <= depth {
                break;
            }
            ops.push(local.is_captured);
            self.current_frame_mut().locals.pop();
        }

        for captured in ops {
            if captured {
                self.emit_op(OpCode::CloseUpvalue, line);
            } else {
                self.emit_op(OpCode::Pop, line);
            }
        }
    }

    /// Close the current scope when its block produced a RESULT VALUE that is
    /// currently on top of the operand stack (block-as-expression: option/choose
    /// arm bodies, default/error bodies whose tail is a value).
    ///
    /// The plain `end_scope` emits `Pop`/`CloseUpvalue`, both of which act on the
    /// TOP of the operand stack — so calling it here would discard the RESULT
    /// instead of the block's locals (the result sits ABOVE the locals). That is
    /// the long-standing block-as-expression corruption:
    ///   `r := option { True -> {`
    ///   `    y := 5`
    ///   `    y + 1`
    ///   `} }`  →  VM gave `5`, not `6`.
    ///
    /// Cleanup that preserves the result and still closes captured locals:
    ///   stack on entry: [.. L0, L1, .., L_{k-1}, RESULT]
    /// 1. `SetLocal floor` copies RESULT into the lowest local slot (L0), leaving
    ///    RESULT on top:                       [.. RESULT, L1, .., L_{k-1}, RESULT]
    /// 2. `Pop` removes the duplicate top:      [.. RESULT, L1, .., L_{k-1}]
    /// 3. Close/pop L_{k-1}..L1 top-down (each is on top in turn) — `CloseUpvalue`
    ///    when captured (snapshots the still-live slot), else `Pop`:
    ///    end state:                            [.. RESULT]
    ///
    /// The RESULT ends up alone at the floor slot — exactly where the block's
    /// value belongs — with every captured body local closed.
    ///
    /// The one tricky case is when the FLOOR local L0 itself is captured: step 1
    /// would overwrite its slot before its upvalue could be closed (`CloseUpvalue`
    /// only acts on the stack top, and the result sits above L0). That is what the
    /// slot-indexed `CloseUpvalueAt floor` op below handles — it snapshots and
    /// closes the buried floor upvalue IN PLACE (without needing the value on top
    /// or popping) before the stash repurposes the slot. The VM interpreter
    /// implements `CloseUpvalueAt` (`close_upvalue_at_slot`); the JIT does not
    /// allow-list it, so any hot function containing it cleanly falls back to the
    /// interpreter (like `BuildArray`/`Index` and other non-JIT opcodes). All
    /// three backends therefore agree on value-producing blocks with captured
    /// floor locals.
    fn end_scope_keeping_value(&mut self, line: u32) {
        self.current_frame_mut().scope_depth -= 1;
        let depth = self.current_frame().scope_depth;

        // Slots (floor..n) of locals being removed, innermost (top) last.
        let floor = {
            let locals = &self.current_frame().locals;
            let mut f = locals.len();
            while f > 0 && locals[f - 1].depth > depth {
                f -= 1;
            }
            f
        };
        let n = self.current_frame().locals.len();

        // Capture the capture-flags before dropping the locals from compile-time scope.
        let mut captured: Vec<bool> = Vec::with_capacity(n - floor);
        for i in floor..n {
            captured.push(self.current_frame().locals[i].is_captured);
        }
        self.current_frame_mut().locals.truncate(floor);

        // No locals: the result is already correctly on top.
        if floor == n {
            return;
        }

        // The floor local's open upvalue must be closed before the stash overwrites its slot.
        if captured[0] {
            self.emit_op_u16(OpCode::CloseUpvalueAt, floor as u16, line);
        }

        // Stash the result into the floor slot, drop the duplicate, then close/pop body locals top-down.
        if let Some(info) = self.current_frame_mut().function.locals.get_mut(floor) {
            info.type_constraint = None;
        }
        self.emit_op_u16(OpCode::SetLocal, floor as u16, line);
        self.emit_op(OpCode::Pop, line);
        // Locals above the floor, innermost first (top of stack first).
        for &cap in captured.iter().skip(1).rev() {
            self.emit_op(
                if cap {
                    OpCode::CloseUpvalue
                } else {
                    OpCode::Pop
                },
                line,
            );
        }
    }

    /// Declare a local variable and return its stack slot.
    fn add_local(&mut self, name: &str, kind: BindingKind, type_constraint: Option<String>) {
        let depth = self.current_frame().scope_depth;
        let slot = self.current_frame().locals.len();
        // LocalInfo is the runtime mirror for `is_mut`, so provenance stays inside the compiler.
        self.record_local_info(slot, kind.is_mutable(), type_constraint.clone());
        let decl_ip = self.current_chunk().code.len();
        self.current_frame_mut()
            .function
            .local_decls
            .push((decl_ip, slot as u16));
        self.current_frame_mut().locals.push(Local {
            name: name.to_string(),
            depth,
            is_captured: false,
            kind,
            type_constraint,
        });
    }

    fn record_local_info(&mut self, slot: usize, mutable: bool, type_constraint: Option<String>) {
        let locals = &mut self.current_frame_mut().function.locals;
        if locals.len() <= slot {
            locals.resize_with(slot + 1, LocalInfo::default);
            let info = &mut locals[slot];
            info.mutable = mutable;
            info.type_constraint = type_constraint;
            return;
        }

        let info = &mut locals[slot];
        info.mutable = mutable;
        // Function metadata is indexed by physical stack slot, but disjoint
        // lexical scopes can reuse that slot for different locals. A runtime
        // SetLocal cannot tell which lexical binding contributed the metadata,
        // so retain a lock only when every occupant agrees on it. In
        // particular, an unconstrained hidden `each` index followed by a typed
        // branch-local must not retroactively make the index typed.
        if info.type_constraint != type_constraint {
            info.type_constraint = None;
        }
    }

    /// Resolve a local variable by name, returning its stack slot.
    fn resolve_local(&self, name: &str) -> Option<u16> {
        let frame = self.current_frame();
        for (i, local) in frame.locals.iter().enumerate().rev() {
            if local.name == name {
                return Some(i as u16);
            }
        }
        None
    }

    /// True if `name` is bound by the user as a local (current or any
    /// enclosing frame → would resolve as local/upvalue) or a declared
    /// global. Read-only — unlike `resolve_upvalue` it registers no capture.
    /// Used to confirm a bare `range` actually means the builtin.
    fn name_is_user_bound(&self, name: &str) -> bool {
        self.frames
            .iter()
            .any(|f| f.locals.iter().any(|l| l.name == name))
            || self.declared_globals.contains(name)
    }

    /// Resolve an upvalue (captured variable from enclosing scope).
    fn resolve_upvalue(&mut self, frame_idx: usize, name: &str) -> Option<u16> {
        if frame_idx == 0 {
            return None;
        }

        let parent_idx = frame_idx - 1;
        let local_slot = {
            let parent = &self.frames[parent_idx];
            parent
                .locals
                .iter()
                .enumerate()
                .rev()
                .find(|(_, l)| l.name == name)
                .map(|(i, _)| i as u16)
        };

        if let Some(slot) = local_slot {
            self.frames[parent_idx].locals[slot as usize].is_captured = true;
            return Some(self.add_upvalue(frame_idx, slot, true));
        }

        if let Some(upvalue_idx) = self.resolve_upvalue(parent_idx, name) {
            return Some(self.add_upvalue(frame_idx, upvalue_idx, false));
        }

        None
    }

    /// The binding an upvalue named `name` resolves to: the innermost local of
    /// that name in an enclosing frame, the same one `resolve_upvalue` captures.
    fn upvalue_binding(&self, name: &str) -> Option<(BindingKind, Option<String>)> {
        let current = self.frames.len().checked_sub(1)?;
        self.frames[..current].iter().rev().find_map(|frame| {
            frame
                .locals
                .iter()
                .rev()
                .find(|l| l.name == name)
                .map(|l| (l.kind, l.type_constraint.clone()))
        })
    }

    /// Checks a write through an upvalue the way a local write is checked:
    /// immutability at compile time, the declared type lock at run time
    /// (`CheckType` before the `SetUpvalue`). `walrus` writes (`:=`) may
    /// override an immutable constant, as they may for a local.
    fn check_upvalue_write(
        &mut self,
        name: &str,
        span: Span,
        verb: &str,
        suffix: &str,
        walrus: bool,
        line: u32,
    ) {
        let Some((kind, constraint)) = self.upvalue_binding(name) else {
            return;
        };
        let forbidden = match kind {
            BindingKind::Mutable => false,
            BindingKind::ImmutableConstant { .. } => !walrus,
            BindingKind::ImmutableLoopVar { .. } | BindingKind::ImmutableTypeName { .. } => true,
        };
        if forbidden {
            self.report_immutable_write(name, kind, span, verb, suffix);
        }
        if let Some(ty) = constraint.filter(|t| t != "GENERIC") {
            let tc = self.make_constant(Value::String(rc_str(ty.as_str())), line);
            self.emit_op_u16(OpCode::CheckType, tc, line);
        }
    }

    fn add_upvalue(&mut self, frame_idx: usize, index: u16, is_local: bool) -> u16 {
        let frame = &mut self.frames[frame_idx];
        for (i, uv) in frame.upvalues.iter().enumerate() {
            if uv.index == index && uv.is_local == is_local {
                return i as u16;
            }
        }
        let idx = frame.upvalues.len() as u16;
        frame.upvalues.push(UpvalueDesc { index, is_local });
        frame.function.upvalue_count = idx + 1;
        idx
    }

    /// Get the line number from an expression's token.
    fn expr_line(expr: &Expression) -> u32 {
        match expr {
            Expression::Int { token, .. }
            | Expression::Float { token, .. }
            | Expression::Str { token, .. }
            | Expression::Char { token, .. }
            | Expression::Boolean { token, .. }
            | Expression::NoneExpr { token }
            | Expression::Array { token, .. }
            | Expression::Prefix { token, .. }
            | Expression::Infix { token, .. }
            | Expression::Postfix { token, .. }
            | Expression::Call { token, .. }
            | Expression::Index { token, .. }
            | Expression::FunctionLiteral { token, .. }
            | Expression::StructLiteral { token, .. }
            | Expression::DotAccess { token, .. }
            | Expression::Slice { token, .. }
            | Expression::TupleLiteral { token, .. }
            | Expression::MapLiteral { token, .. }
            | Expression::Option { token, .. }
            | Expression::Guard { token, .. }
            | Expression::Log { token, .. }
            | Expression::ErrorConstruct { token, .. }
            | Expression::ValueConstruct { token, .. }
            | Expression::TypeWrap { token, .. }
            | Expression::Fail { token, .. }
            | Expression::Unless { token, .. }
            | Expression::StringInterp { token, .. }
            | Expression::Diverge { token, .. }
            | Expression::DivergeEach { token, .. }
            | Expression::Converge { token, .. }
            | Expression::EnumVariantConstruct { token, .. } => token.span.line() as u32,
            Expression::Ident(ident) => ident.token.span.line() as u32,
            Expression::Grouped(inner) => Self::expr_line(inner),
        }
    }

    fn stmt_line(stmt: &Statement) -> u32 {
        match stmt {
            Statement::Let { name, .. } => name.token.span.line() as u32,
            Statement::Expr(expr) => Self::expr_line(expr),
            Statement::Each { token, .. }
            | Statement::Repeat { token, .. }
            | Statement::Pattern { token, .. }
            | Statement::Choose { token, .. }
            | Statement::If { token, .. }
            | Statement::Give { token, .. }
            | Statement::StructDef { token, .. }
            | Statement::EnumDef { token, .. }
            | Statement::IncludesDef { token, .. }
            | Statement::DotAssign { token, .. }
            | Statement::Introduce { token, .. }
            | Statement::IndexAssign { token, .. }
            | Statement::Main { token, .. }
            | Statement::Test { token, .. } => token.span.line() as u32,
            Statement::TypedLet { name, .. }
            | Statement::TypedDeclare { name, .. }
            | Statement::Assign { name, .. } => name.token.span.line() as u32,
            Statement::Unpack { names, .. } => {
                names.first().map_or(0, |n| n.token.span.line() as u32)
            }
            Statement::Skip { token } | Statement::Stop { token } => token.span.line() as u32,
        }
    }

    /// Column number (1-based; 0 = unknown) for the token that anchors
    /// this expression. Mirrors `expr_line` exactly. Used to attach
    /// caret columns to emitted bytecode for error reporting.
    fn expr_column(expr: &Expression) -> u32 {
        match expr {
            Expression::Int { token, .. }
            | Expression::Float { token, .. }
            | Expression::Str { token, .. }
            | Expression::Char { token, .. }
            | Expression::Boolean { token, .. }
            | Expression::NoneExpr { token }
            | Expression::Array { token, .. }
            | Expression::Prefix { token, .. }
            | Expression::Infix { token, .. }
            | Expression::Postfix { token, .. }
            | Expression::Call { token, .. }
            | Expression::Index { token, .. }
            | Expression::FunctionLiteral { token, .. }
            | Expression::StructLiteral { token, .. }
            | Expression::DotAccess { token, .. }
            | Expression::Slice { token, .. }
            | Expression::TupleLiteral { token, .. }
            | Expression::MapLiteral { token, .. }
            | Expression::Option { token, .. }
            | Expression::Guard { token, .. }
            | Expression::Log { token, .. }
            | Expression::ErrorConstruct { token, .. }
            | Expression::ValueConstruct { token, .. }
            | Expression::TypeWrap { token, .. }
            | Expression::Fail { token, .. }
            | Expression::Unless { token, .. }
            | Expression::StringInterp { token, .. }
            | Expression::Diverge { token, .. }
            | Expression::DivergeEach { token, .. }
            | Expression::Converge { token, .. }
            | Expression::EnumVariantConstruct { token, .. } => token.span.column() as u32,
            Expression::Ident(ident) => ident.token.span.column() as u32,
            Expression::Grouped(inner) => Self::expr_column(inner),
        }
    }

    fn stmt_column(stmt: &Statement) -> u32 {
        match stmt {
            Statement::Let { name, .. } => name.token.span.column() as u32,
            Statement::Expr(expr) => Self::expr_column(expr),
            Statement::Each { token, .. }
            | Statement::Repeat { token, .. }
            | Statement::Pattern { token, .. }
            | Statement::Choose { token, .. }
            | Statement::If { token, .. }
            | Statement::Give { token, .. }
            | Statement::StructDef { token, .. }
            | Statement::EnumDef { token, .. }
            | Statement::IncludesDef { token, .. }
            | Statement::DotAssign { token, .. }
            | Statement::Introduce { token, .. }
            | Statement::IndexAssign { token, .. }
            | Statement::Main { token, .. }
            | Statement::Test { token, .. } => token.span.column() as u32,
            Statement::TypedLet { name, .. }
            | Statement::TypedDeclare { name, .. }
            | Statement::Assign { name, .. } => name.token.span.column() as u32,
            Statement::Unpack { names, .. } => {
                names.first().map_or(0, |n| n.token.span.column() as u32)
            }
            Statement::Skip { token } | Statement::Stop { token } => token.span.line() as u32,
        }
    }

    // ── Statement Compilation ──────────────────────────────────────────

    fn compile_statement(&mut self, stmt: &Statement) {
        let line = Self::stmt_line(stmt);
        let col = Self::stmt_column(stmt);
        self.loc_column_stack.push(col);
        self.compile_statement_inner(stmt, line);
        self.loc_column_stack.pop();
    }

    fn compile_statement_inner(&mut self, stmt: &Statement, line: u32) {
        match stmt {
            Statement::Expr(expr) => {
                // An expression statement's value is discarded, so a skip/stop tail is legitimate loop control flow.
                let prev = self.value_consumed;
                self.value_consumed = false;
                self.compile_expression(expr);
                self.value_consumed = prev;
                self.emit_op(OpCode::Pop, line);
            }

            Statement::Let { name, value } => {
                // `:=` on an existing immutable local used to bypass the mutability check entirely.
                if let Some(slot) = self.resolve_local(&name.value) {
                    match self.current_frame().locals[slot as usize].kind {
                        BindingKind::ImmutableLoopVar { header } => {
                            self.report(
                                Diagnostic::error(
                                    codes::LOOP_VAR_REBIND,
                                    name.token.span,
                                    format!(
                                        "`:=` on loop variable `{}` lasts one iteration",
                                        name.value
                                    ),
                                )
                                .label("discarded at the end of this iteration")
                                .secondary(
                                    header,
                                    format!("`{}` is rebound from the sequence here", name.value),
                                )
                                .note(
                                    "the rebinding applies to the rest of this iteration only; \
                                     it does not change which values are iterated",
                                )
                                .help("bind a differently-named variable"),
                            );
                        }
                        BindingKind::ImmutableTypeName { decl } => {
                            self.report(
                                Diagnostic::error(
                                    codes::TYPE_NAME_REBIND,
                                    name.token.span,
                                    format!("cannot rebind type name `{}`", name.value),
                                )
                                .label("cannot rebind")
                                .secondary(
                                    decl,
                                    format!("`{}` is declared here as a type", name.value),
                                )
                                .help("choose a different name for the value"),
                            );
                        }
                        BindingKind::Mutable | BindingKind::ImmutableConstant { .. } => {}
                    }
                }

                // A named fn must bind its own name before its body compiles so a self-call captures itself.
                let mut predeclared_named_fn = false;
                if let Expression::FunctionLiteral { .. } = value
                    && self.current_frame().scope_depth > 0
                    && self.resolve_local(&name.value).is_none()
                {
                    let frame_idx = self.frames.len() - 1;
                    let is_upvalue = self.resolve_upvalue(frame_idx, &name.value).is_some();
                    let is_global_reassign = self.declared_globals.contains(&name.value);
                    if !is_upvalue && !is_global_reassign {
                        self.add_local(&name.value, BindingKind::Mutable, None);
                        predeclared_named_fn = true;
                    }
                }
                // Pass the name so a function literal's closure carries it instead of showing as anonymous.
                if let Expression::FunctionLiteral {
                    parameters, body, ..
                } = value
                {
                    self.compile_function(Some(&name.value), parameters, body, line);
                } else if let Some(folded) = fold_constant_expression(value) {
                    // Pure-literal init folds to one Constant so the JIT can virtualize cleanly.
                    self.emit_constant(folded, line);
                } else {
                    self.compile_expression(value);
                }
                if self.dup_next_define {
                    self.emit_op(OpCode::Dup, line);
                }
                // A pre-declared named-fn local already owns the slot, so do not re-store or pop it.
                if predeclared_named_fn {
                    return;
                }
                // Walrus updates the variable if it exists in any scope, else creates a new binding.
                if let Some(slot) = self.resolve_local(&name.value) {
                    self.emit_op_u16(OpCode::SetLocal, slot, line);
                    self.emit_op(OpCode::Pop, line);
                } else {
                    let frame_idx = self.frames.len() - 1;
                    if let Some(uv_idx) = self.resolve_upvalue(frame_idx, &name.value) {
                        self.check_upvalue_write(
                            &name.value,
                            name.token.span,
                            "rebind",
                            "",
                            true,
                            line,
                        );
                        self.emit_op_u16(OpCode::SetUpvalue, uv_idx, line);
                        self.emit_op(OpCode::Pop, line);
                    } else if self.current_frame().scope_depth > 0
                        && self.declared_globals.contains(&name.value)
                    {
                        // `:=` on an existing global from a nested scope updates it rather than shadowing.
                        let name_const =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        self.emit_op_u16(OpCode::SetGlobal, name_const, line);
                        self.emit_op(OpCode::Pop, line);
                    } else if self.current_frame().scope_depth > 0 {
                        self.add_local(&name.value, BindingKind::Mutable, None);
                    } else {
                        // Global — DefineGlobal overwrites
                        self.declared_globals.insert(name.value.clone());
                        let name_const =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        self.emit_op_u16(OpCode::DefineGlobal, name_const, line);
                    }
                }
            }

            Statement::TypedLet {
                name,
                type_ann,
                value,
                walrus,
            } => {
                // <generic> with = is not allowed — generic implies type mutability
                if !walrus && matches!(type_ann, TypeAnnotation::Generic) {
                    self.error_coded(
                        codes::GENERIC_WITH_EQUALS,
                        &format!(
                            "<generic> cannot be used with '=' (immutable). use ':=' for '{}'",
                            name.value
                        ),
                        line,
                    );
                }
                // Same fold as Statement::Let; the TypeWrap still fires so `x <int> := 3.9` still truncates.
                if let Some(folded) = fold_constant_expression(value) {
                    self.emit_constant(folded, line);
                } else {
                    self.compile_expression(value);
                }
                let type_name = type_ann.type_name();
                let mutable = *walrus; // walrus (:=) means mutable
                // Walrus converts to the target type; plain `=` type-checks strictly without converting.
                if !matches!(type_ann, TypeAnnotation::Generic | TypeAnnotation::NoneType) {
                    let tc = self.make_constant(Value::String(rc_str(type_name.as_str())), line);
                    self.emit_op_u16(OpCode::TypeWrap, tc, line);
                }
                if self.dup_next_define {
                    self.emit_op(OpCode::Dup, line);
                }
                if self.current_frame().scope_depth > 0 {
                    // An existing local at this or an enclosing scope is updated, matching Let.
                    if let Some(slot) = self.resolve_local(&name.value) {
                        self.emit_op_u16(OpCode::SetLocal, slot, line);
                        self.emit_op(OpCode::Pop, line);
                    } else if self.declared_globals.contains(&name.value)
                        && !self.typed_globals.contains(&name.value)
                        && self.current_frame().each_body_depth == 0
                    {
                        // A typed re-declaration of an untyped global from a nested block re-binds the global with the new type.
                        self.typed_globals.insert(name.value.clone());
                        let name_const =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        let type_const =
                            self.make_constant(Value::String(rc_str(type_name.as_str())), line);
                        self.emit_op_u16(OpCode::DefineGlobalTyped, name_const, line);
                        self.emit_byte(if mutable { 1 } else { 0 }, line);
                        self.current_chunk().write_u16(type_const, line);
                    } else {
                        // Shadows instead when the outer binding is also typed, or when there is no outer binding.
                        let kind = if mutable {
                            BindingKind::Mutable
                        } else {
                            BindingKind::ImmutableConstant {
                                decl: name.token.span,
                            }
                        };
                        self.add_local(&name.value, kind, Some(type_name));
                    }
                } else {
                    self.declared_globals.insert(name.value.clone());
                    self.typed_globals.insert(name.value.clone());
                    let name_const =
                        self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                    let type_const =
                        self.make_constant(Value::String(rc_str(type_name.as_str())), line);
                    self.emit_op_u16(OpCode::DefineGlobalTyped, name_const, line);
                    self.emit_byte(if mutable { 1 } else { 0 }, line);
                    self.current_chunk().write_u16(type_const, line);
                }
            }

            Statement::TypedDeclare { name, type_ann } => {
                let type_name = type_ann.type_name();
                if let TypeAnnotation::Struct(struct_name) = type_ann {
                    // Struct type: call the struct constructor with 0 args to create default instance
                    let sn_const =
                        self.make_constant(Value::String(rc_str(struct_name.as_str())), line);
                    self.emit_op_u16(OpCode::GetGlobal, sn_const, line);
                    self.emit_op_u8(OpCode::Call, 0, line);
                } else {
                    let default = default_for_type(&type_name);
                    self.emit_constant(default, line);
                }
                if self.dup_next_define {
                    self.emit_op(OpCode::Dup, line);
                }
                if self.current_frame().scope_depth > 0 {
                    if let Some(slot) = self.resolve_local(&name.value) {
                        self.emit_op_u16(OpCode::SetLocal, slot, line);
                        self.emit_op(OpCode::Pop, line);
                    } else if self.declared_globals.contains(&name.value)
                        && !self.typed_globals.contains(&name.value)
                        && self.current_frame().each_body_depth == 0
                    {
                        // Zero-init form of the same rule; see the note on TypedLet.
                        self.typed_globals.insert(name.value.clone());
                        let name_const =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        let type_const =
                            self.make_constant(Value::String(rc_str(type_name.as_str())), line);
                        self.emit_op_u16(OpCode::DefineGlobalTyped, name_const, line);
                        self.emit_byte(1, line); // TypedDeclare is mutable
                        self.current_chunk().write_u16(type_const, line);
                    } else {
                        // `x <int>` is a declaration, so inside a block it introduces a fresh block-local.
                        self.add_local(&name.value, BindingKind::Mutable, Some(type_name));
                    }
                } else {
                    self.declared_globals.insert(name.value.clone());
                    self.typed_globals.insert(name.value.clone());
                    let name_const =
                        self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                    let type_const =
                        self.make_constant(Value::String(rc_str(type_name.as_str())), line);
                    self.emit_op_u16(OpCode::DefineGlobalTyped, name_const, line);
                    self.emit_byte(1, line); // TypedDeclare is mutable
                    self.current_chunk().write_u16(type_const, line);
                }
            }

            Statement::Assign { name, value } => {
                if let Some(slot) = self.resolve_local(&name.value)
                    && !self.current_frame().locals[slot as usize].kind.is_mutable()
                {
                    let kind = self.current_frame().locals[slot as usize].kind;
                    self.report_immutable_write(
                        &name.value,
                        kind,
                        name.token.span,
                        "assign to",
                        "",
                    );
                }
                // A bare name that is not a local but is a field of the enclosing struct resolves to self.field.
                if self.resolve_local(&name.value).is_none()
                    && let Some((load_self, self_idx)) = self.implicit_self_for(&name.value)
                {
                    // SetField pops both object and value, so no trailing Pop is needed.
                    self.emit_op_u16(load_self, self_idx, line);
                    self.hold_temps(1);
                    self.compile_expression(value);
                    self.release_temps(1);
                    let field_const =
                        self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                    self.emit_op_u16(OpCode::SetField, field_const, line);
                    return;
                }
                self.compile_expression(value);
                if let Some(slot) = self.resolve_local(&name.value) {
                    self.emit_op_u16(OpCode::SetLocal, slot, line);
                } else {
                    let frame_idx = self.frames.len() - 1;
                    if let Some(uv_idx) = self.resolve_upvalue(frame_idx, &name.value) {
                        self.check_upvalue_write(
                            &name.value,
                            name.token.span,
                            "assign to",
                            "",
                            false,
                            line,
                        );
                        self.emit_op_u16(OpCode::SetUpvalue, uv_idx, line);
                    } else {
                        let name_const =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        self.emit_op_u16(OpCode::SetGlobal, name_const, line);
                    }
                }
                self.emit_op(OpCode::Pop, line); // assignment is a statement, discard value
            }

            Statement::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                self.compile_expression(condition);
                let then_jump = self.emit_jump(OpCode::JumpIfFalse, line);
                self.emit_op(OpCode::Pop, line);

                self.begin_scope();
                for s in consequence {
                    self.compile_statement(s);
                }
                self.end_scope(line);

                // Always jump over the false-path Pop (and alternative if present)
                let else_jump = self.emit_jump(OpCode::Jump, line);
                self.patch_jump(then_jump);
                self.emit_op(OpCode::Pop, line);

                if let Some(alt) = alternative {
                    self.begin_scope();
                    for s in alt {
                        self.compile_statement(s);
                    }
                    self.end_scope(line);
                }
                self.patch_jump(else_jump);
            }

            Statement::Repeat {
                condition, body, ..
            } => {
                let loop_start = self.current_chunk().len();
                self.current_frame_mut().loop_starts.push(loop_start);
                self.current_frame_mut().loop_exits.push(Vec::new());
                self.current_frame_mut().loop_continues.push(Vec::new());
                // `skip` cleans body locals to the pre-body height, then re-checks via the back-edge.
                let continue_floor = self.current_frame().locals.len();
                self.current_frame_mut()
                    .loop_continue_floors
                    .push(continue_floor);
                // `repeat` has no loop variable, so `stop` exits to the same height `skip` continues to.
                self.current_frame_mut()
                    .loop_exit_floors
                    .push(continue_floor);
                let handler_floor = self.current_frame().handler_depth;
                self.current_frame_mut()
                    .loop_handler_depths
                    .push(handler_floor);

                self.compile_expression(condition);
                let exit_jump = self.emit_jump(OpCode::JumpIfFalse, line);
                self.emit_op(OpCode::Pop, line);

                self.begin_scope();
                for s in body {
                    self.compile_statement(s);
                }
                self.end_scope(line);

                // Continue target: `skip` lands here (stack at pre-body height).
                let continues = self.current_frame_mut().loop_continues.pop().unwrap();
                for c in continues {
                    self.patch_jump(c);
                }
                self.current_frame_mut().loop_continue_floors.pop();
                self.current_frame_mut().loop_exit_floors.pop();
                self.current_frame_mut().loop_handler_depths.pop();

                self.emit_loop(loop_start, line);

                self.patch_jump(exit_jump);
                self.emit_op(OpCode::Pop, line);

                let exits = self.current_frame_mut().loop_exits.pop().unwrap();
                for exit in exits {
                    self.patch_jump(exit);
                }
                self.current_frame_mut().loop_starts.pop();
            }

            Statement::Each {
                token,
                variable,
                index_variable,
                iterable,
                body,
            } => {
                // Lower builtin `range` to a counting loop: no array is materialised and the body stays JIT-eligible.
                let range_args: Option<(&[Expression], i64)> = match iterable {
                    Expression::Call {
                        function,
                        args,
                        named_args,
                        ..
                    } if named_args.is_empty()
                        && index_variable.is_none()
                        && (1..=3).contains(&args.len())
                        && matches!(&**function, Expression::Ident(id) if id.value == "range")
                        && !self.name_is_user_bound("range") =>
                    {
                        match args.get(2) {
                            None => Some((args.as_slice(), 1)),
                            Some(step) => match literal_int(step) {
                                Some(step) if step != 0 => Some((args.as_slice(), step)),
                                _ => None,
                            },
                        }
                    }
                    _ => None,
                };
                let range_step = range_args.map(|(_, step)| step).unwrap_or(1);

                self.begin_scope();

                // iter_slot is the array path only; index_slot is the counter, end_slot the exclusive bound,
                // limit_slot the last counter value that may still be advanced (see `RangeLimit`).
                let (index_slot, iter_slot, end_slot, limit_slot) =
                    if let Some((args, _)) = range_args {
                        // counter = start (0 for 1-arg form), evaluated once
                        if args.len() == 1 {
                            self.emit_constant(Value::Integer(0), line);
                        } else {
                            self.compile_expression(&args[0]);
                        }
                        self.add_local(LOOP_INDEX, BindingKind::Mutable, None);
                        // range(n) puts the bound first; every other form puts it second, since a third argument is the step.
                        self.compile_expression(if args.len() == 1 { &args[0] } else { &args[1] });
                        self.add_local(
                            LOOP_RANGE_END,
                            BindingKind::ImmutableConstant {
                                decl: Span::default(),
                            },
                            None,
                        );
                        let i = self.resolve_local(LOOP_INDEX).unwrap();
                        let e = self.resolve_local(LOOP_RANGE_END).unwrap();
                        // Checks both bounds are ints, as the builtin does, and bounds the counter so it never wraps.
                        self.emit_op_u16(OpCode::GetLocal, i, line);
                        self.emit_op_u16(OpCode::GetLocal, e, line);
                        let step_idx = self.make_constant(Value::Integer(range_step), line);
                        self.emit_op_u16(OpCode::RangeLimit, step_idx, line);
                        self.add_local(
                            LOOP_RANGE_LIMIT,
                            BindingKind::ImmutableConstant {
                                decl: Span::default(),
                            },
                            None,
                        );
                        let l = self.resolve_local(LOOP_RANGE_LIMIT).unwrap();
                        (i, 0, e, l)
                    } else {
                        self.compile_expression(iterable);
                        self.add_local(
                            LOOP_ITERABLE,
                            BindingKind::ImmutableConstant {
                                decl: Span::default(),
                            },
                            None,
                        );
                        self.emit_constant(Value::Integer(0), line);
                        self.add_local(LOOP_INDEX, BindingKind::Mutable, None);
                        let it = self.resolve_local(LOOP_ITERABLE).unwrap();
                        let i = self.resolve_local(LOOP_INDEX).unwrap();
                        (i, it, 0, 0)
                    };
                let is_range = range_args.is_some();
                // Counting up runs while index < bound, down while index > bound: the bound stays exclusive either way.
                let range_cmp = if range_step > 0 {
                    OpCode::Less
                } else {
                    OpCode::Greater
                };

                // A range loop tests `index < end` once here and then `index < limit` before each advance.
                let entry_exit = if is_range {
                    self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                    self.emit_op_u16(OpCode::GetLocal, end_slot, line);
                    self.emit_op(range_cmp, line);
                    let jump = self.emit_jump(OpCode::JumpIfFalse, line);
                    self.emit_op(OpCode::Pop, line);
                    Some(jump)
                } else {
                    None
                };

                let loop_start = self.current_chunk().len();
                self.current_frame_mut().loop_starts.push(loop_start);
                self.current_frame_mut().loop_exits.push(Vec::new());
                self.current_frame_mut().loop_continues.push(Vec::new());

                // Array loop condition (leaves a bool both branches Pop).
                let header_exit = if is_range {
                    None
                } else {
                    self.emit_op_u16(OpCode::GetLocal, iter_slot, line);
                    self.emit_op(OpCode::IterLen, line);
                    self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                    self.emit_op(OpCode::Greater, line);
                    let jump = self.emit_jump(OpCode::JumpIfFalse, line);
                    self.emit_op(OpCode::Pop, line);
                    Some(jump)
                };

                // Push the counter (range) or iterable[index]; the two-name form pushes key and value.
                if is_range {
                    self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                } else {
                    self.emit_op_u16(OpCode::GetLocal, iter_slot, line);
                    self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                    self.emit_op(
                        if index_variable.is_some() {
                            OpCode::IterEntry
                        } else {
                            OpCode::IterGet
                        },
                        line,
                    );
                }

                // `stop` exits to a scope that never bound the loop variable, so its floor is the pre-binding height.
                let exit_floor = self.current_frame().locals.len();
                self.current_frame_mut().loop_exit_floors.push(exit_floor);
                // IterEntry pushes key then value, so the key takes the lower slot.
                let header = token.span;
                if let Some(index) = index_variable {
                    self.add_local(&index.value, BindingKind::ImmutableLoopVar { header }, None);
                }
                self.add_local(
                    &variable.value,
                    BindingKind::ImmutableLoopVar { header },
                    None,
                );
                // `skip` cleans to here, keeping the loop variable, then jumps to the index increment.
                let continue_floor = self.current_frame().locals.len();
                self.current_frame_mut()
                    .loop_continue_floors
                    .push(continue_floor);
                let handler_floor = self.current_frame().handler_depth;
                self.current_frame_mut()
                    .loop_handler_depths
                    .push(handler_floor);

                // Compile body in its own scope (so TypedLet locals are cleaned up per iteration)
                self.begin_scope();
                // The tree-walker runs each iteration in a fresh env, so a typed re-declaration must shadow here.
                self.current_frame_mut().each_body_depth += 1;
                for s in body {
                    self.compile_statement(s);
                }
                self.current_frame_mut().each_body_depth -= 1;
                self.end_scope(line);

                // `skip` lands here with the loop variable still on top.
                let continues = self.current_frame_mut().loop_continues.pop().unwrap();
                for c in continues {
                    self.patch_jump(c);
                }
                self.current_frame_mut().loop_continue_floors.pop();
                self.current_frame_mut().loop_exit_floors.pop();
                self.current_frame_mut().loop_handler_depths.pop();

                // Closing per iteration gives each closure its own value; the two-name form popped value-first.
                for _ in 0..(1 + usize::from(index_variable.is_some())) {
                    let captured = self
                        .current_frame()
                        .locals
                        .last()
                        .map(|l| l.is_captured)
                        .unwrap_or(false);
                    self.current_frame_mut().locals.pop();
                    self.emit_op(
                        if captured {
                            OpCode::CloseUpvalue
                        } else {
                            OpCode::Pop
                        },
                        line,
                    );
                }

                // `index < limit` is `index + step < end` without computing a sum that could wrap.
                let advance_exit = if is_range {
                    self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                    self.emit_op_u16(OpCode::GetLocal, limit_slot, line);
                    self.emit_op(range_cmp, line);
                    let jump = self.emit_jump(OpCode::JumpIfFalse, line);
                    self.emit_op(OpCode::Pop, line);
                    Some(jump)
                } else {
                    None
                };

                // Advance the index by the range's step (1 for the array path).
                self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                self.emit_constant(Value::Integer(range_step), line);
                self.emit_op(OpCode::Add, line);
                self.emit_op_u16(OpCode::SetLocal, index_slot, line);
                self.emit_op(OpCode::Pop, line);

                self.emit_loop(loop_start, line);

                // Each condition exit pops its own bool: the JIT pairs a branch with its landing Pop.
                for jump in [header_exit, advance_exit].into_iter().flatten() {
                    self.patch_jump(jump);
                }
                self.emit_op(OpCode::Pop, line);
                if let Some(entry_exit) = entry_exit {
                    let over = self.emit_jump(OpCode::Jump, line);
                    self.patch_jump(entry_exit);
                    self.emit_op(OpCode::Pop, line);
                    self.patch_jump(over);
                }

                let exits = self.current_frame_mut().loop_exits.pop().unwrap();
                for exit in exits {
                    self.patch_jump(exit);
                }
                self.current_frame_mut().loop_starts.pop();

                self.end_scope(line);
            }

            Statement::Skip { token } => {
                // Clean locals above the continue floor without removing them from scope: later code still references them.
                if self.current_frame().loop_continue_floors.is_empty() {
                    self.report_skip_stop_misuse(true, token.span, false);
                } else {
                    let floor = *self.current_frame().loop_continue_floors.last().unwrap();
                    let n = self.current_frame().locals.len();
                    for i in (floor..n).rev() {
                        let captured = self.current_frame().locals[i].is_captured;
                        self.emit_op(
                            if captured {
                                OpCode::CloseUpvalue
                            } else {
                                OpCode::Pop
                            },
                            line,
                        );
                    }
                    self.emit_loop_handler_unwind(line);
                    let jump = self.emit_jump(OpCode::Jump, line);
                    self.current_frame_mut()
                        .loop_continues
                        .last_mut()
                        .unwrap()
                        .push(jump);
                }
            }

            Statement::Stop { token } => {
                // Same for break; leaving them on the operand stack would corrupt an enclosing expression.
                if self.current_frame().loop_exit_floors.is_empty() {
                    self.report_skip_stop_misuse(false, token.span, false);
                } else {
                    let floor = *self.current_frame().loop_exit_floors.last().unwrap();
                    let n = self.current_frame().locals.len();
                    for i in (floor..n).rev() {
                        let captured = self.current_frame().locals[i].is_captured;
                        self.emit_op(
                            if captured {
                                OpCode::CloseUpvalue
                            } else {
                                OpCode::Pop
                            },
                            line,
                        );
                    }
                    self.emit_loop_handler_unwind(line);
                    let exit = self.emit_jump(OpCode::Jump, line);
                    self.current_frame_mut()
                        .loop_exits
                        .last_mut()
                        .unwrap()
                        .push(exit);
                }
            }

            Statement::Give { value, .. } => {
                self.compile_expression(value);
                self.emit_op(OpCode::Return, line);
            }

            Statement::Unpack {
                names,
                value,
                values,
                reassign,
            } => {
                // Every value is computed before any name is bound, so `a, b = b, a` swaps.
                let local_define = !*reassign && self.current_frame().scope_depth > 0;
                // A `:=` naming an existing variable updates it, as `x := v` does. Then
                // the new names need their slots before the values are pushed, so they
                // get `None` placeholders and every name is stored from the top down.
                let fresh: Vec<bool> = names
                    .iter()
                    .map(|n| n.value == "_" || self.unpack_name_is_fresh(&n.value))
                    .collect();
                let placeholders = local_define && fresh.iter().any(|f| !f);
                if placeholders {
                    for (name, _) in names.iter().zip(&fresh).filter(|(_, f)| **f) {
                        self.emit_op(OpCode::None, line);
                        self.add_local(&name.value, BindingKind::Mutable, None);
                    }
                }

                if let Some(exprs) = values {
                    // Multi-expression: a, b := expr1, expr2
                    if exprs.len() != names.len() {
                        self.error_coded(
                            codes::COMPILE_ERROR,
                            &format!("expected {} values, got {}", names.len(), exprs.len()),
                            line,
                        );
                        return;
                    }
                    for expr in exprs {
                        self.compile_expression(expr);
                        self.hold_temps(1);
                    }
                    self.release_temps(exprs.len());
                } else {
                    // Single-expression unpack: a, b := tuple_or_array
                    self.compile_expression(value);
                    let count = self.count_u8(names.len(), line);
                    self.emit_op_u8(OpCode::Unpack, count, line);
                }

                if *reassign || placeholders {
                    // Stores pop from the top, so iterate in reverse to pair name[last] with element[last].
                    for name in names.iter().rev() {
                        self.compile_unpack_bind(name, !*reassign, line);
                    }
                } else if local_define {
                    // add_local assigns sequential slots matching push order, so iterate forward, even for `_`.
                    for name in names {
                        self.compile_unpack_define(name, line);
                    }
                } else {
                    // DefineGlobal pops from the top, so bind the last name first to preserve positional order.
                    for name in names.iter().rev() {
                        self.compile_unpack_define(name, line);
                    }
                }
            }

            Statement::DotAssign {
                object,
                field,
                value,
                ..
            } => {
                self.compile_expression(object);
                self.hold_temps(1);
                self.compile_expression(value);
                self.release_temps(1);
                let field_const =
                    self.make_constant(Value::String(rc_str(field.value.as_str())), line);
                self.emit_op_u16(OpCode::SetField, field_const, line);
            }

            Statement::IndexAssign {
                object,
                index,
                value,
                ..
            } => {
                self.compile_expression(object);
                self.hold_temps(1);
                self.compile_expression(index);
                self.hold_temps(1);
                self.compile_expression(value);
                self.release_temps(2);
                self.emit_op(OpCode::IndexAssign, line);
            }

            Statement::Main { body, .. } => {
                let skip_jump = self.emit_jump(OpCode::Main, line);
                for s in body {
                    self.compile_statement(s);
                }
                self.patch_jump(skip_jump);
            }

            // <test> blocks run only under `oxigen test`, via the tree-walking runner.
            Statement::Test { .. } => {}

            Statement::StructDef {
                name,
                parent,
                fields,
                ..
            } => {
                self.reject_builtin_type_name(name);
                let field_info: Vec<(String, String, bool)> = fields
                    .iter()
                    .map(|f| (f.name.value.clone(), f.type_ann.type_name(), f.hidden))
                    .collect();

                // Record fields and parent so a bare identifier in a method body resolves to self.field.
                self.struct_fields.insert(
                    name.value.clone(),
                    fields.iter().map(|f| f.name.value.clone()).collect(),
                );
                self.struct_parents
                    .insert(name.value.clone(), parent.as_ref().map(|p| p.value.clone()));

                let struct_def =
                    Value::StructDef(std::rc::Rc::new(crate::vm::value::ObjStructDef {
                        name: name.value.clone(),
                        fields: field_info,
                        methods: std::cell::RefCell::new(std::collections::HashMap::new()),
                        parent: parent.as_ref().map(|p| p.value.clone()),
                        layout: std::cell::OnceCell::new(),
                        module_globals: crate::vm::value::ModuleGlobals::none(),
                    }));
                let const_idx = self.make_constant(struct_def, line);
                self.emit_op_u16(OpCode::Constant, const_idx, line);

                if self.current_frame().scope_depth > 0 {
                    self.add_local(
                        &name.value,
                        BindingKind::ImmutableTypeName {
                            decl: name.token.span,
                        },
                        None,
                    );
                } else {
                    let name_const =
                        self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                    self.emit_op_u16(OpCode::DefineGlobal, name_const, line);
                }
            }

            Statement::EnumDef { name, variants, .. } => {
                self.reject_builtin_type_name(name);
                let mut next_auto: i64 = 0;
                let mut resolved: Vec<VmEnumVariantDef> = Vec::new();
                for variant in variants {
                    let kind = match &variant.kind {
                        VariantKind::Unit(None) => {
                            let disc = next_auto;
                            next_auto = disc + 1;
                            VmEnumVariantKind::Unit(Some(Value::Integer(disc)))
                        }
                        VariantKind::Unit(Some(expr)) => {
                            // Non-literal discriminants produce a variant with no value.
                            let val = match expr {
                                Expression::Int { value, .. } => {
                                    next_auto = *value + 1;
                                    Some(Value::Integer(*value))
                                }
                                Expression::Float { value, .. } => Some(Value::Float(*value)),
                                Expression::Str { value, .. } => {
                                    Some(Value::String(rc_str(value.as_str())))
                                }
                                Expression::Boolean { value, .. } => Some(Value::Boolean(*value)),
                                Expression::NoneExpr { .. } => Some(Value::None),
                                _ => None,
                            };
                            VmEnumVariantKind::Unit(val)
                        }
                        VariantKind::Tuple(params) => VmEnumVariantKind::Tuple(
                            params.iter().map(|(id, _)| id.value.clone()).collect(),
                        ),
                        VariantKind::Struct(fields) => VmEnumVariantKind::Struct(
                            fields.iter().map(|f| f.name.value.clone()).collect(),
                        ),
                    };
                    let field_types = match &variant.kind {
                        VariantKind::Unit(_) => Vec::new(),
                        VariantKind::Tuple(params) => {
                            params.iter().map(|(_, ty)| ty.type_name()).collect()
                        }
                        VariantKind::Struct(fields) => {
                            fields.iter().map(|f| f.type_ann.type_name()).collect()
                        }
                    };
                    resolved.push(VmEnumVariantDef {
                        name: variant.name.value.clone(),
                        kind,
                        field_types,
                    });
                }
                let enum_val = Value::EnumDef(std::rc::Rc::new(ObjEnumDef {
                    name: name.value.clone(),
                    variants: resolved,
                }));
                let const_idx = self.make_constant(enum_val, line);
                self.emit_op_u16(OpCode::Constant, const_idx, line);
                if self.current_frame().scope_depth > 0 {
                    self.add_local(
                        &name.value,
                        BindingKind::ImmutableTypeName {
                            decl: name.token.span,
                        },
                        None,
                    );
                } else {
                    let name_const =
                        self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                    self.emit_op_u16(OpCode::DefineGlobal, name_const, line);
                }
            }

            Statement::IncludesDef {
                struct_name,
                methods,
                ..
            } => {
                // Make the struct's fields resolvable as implicit-self while compiling method bodies.
                let self_fields = self.collect_self_fields(&struct_name.value);
                self.method_field_stack.push(self_fields);
                self.method_struct_stack.push(struct_name.value.clone());
                // Compile each method with an implicit `self` first parameter
                for (method_name, method_expr) in methods {
                    if let Expression::FunctionLiteral {
                        parameters, body, ..
                    } = method_expr
                    {
                        let mut method_params = vec![TypedParam {
                            ident: Identifier {
                                token: crate::token::Token {
                                    token_type: crate::token::TokenType::SelfKw,
                                    literal: "self".to_string(),
                                    span: crate::token::Span::new(line as usize, 0),
                                },
                                value: "self".to_string(),
                            },
                            type_ann: None,
                            default: None,
                            optional: false,
                        }];
                        method_params.extend(parameters.iter().cloned());
                        self.compile_function(Some(&method_name.value), &method_params, body, line);
                    } else {
                        self.compile_expression(method_expr);
                    }
                    self.emit_constant(Value::String(rc_str(method_name.value.as_str())), line);
                }
                self.method_field_stack.pop();
                self.method_struct_stack.pop();
                let struct_const =
                    self.make_constant(Value::String(rc_str(struct_name.value.as_str())), line);
                self.emit_op_u16(OpCode::DefineMethod, struct_const, line);
                let count = self.count_u8(methods.len(), line);
                self.emit_byte(count, line);
            }

            Statement::Pattern {
                name,
                params,
                condition,
                ..
            } => {
                self.check_pattern_arity(params, name.token.span);
                // Compile the condition as a closure, store as __pattern_<name> global
                let param_names: Vec<String> = params.iter().map(|p| p.value.clone()).collect();
                self.compile_pattern_function(&param_names, condition, line);
                let pattern_global = format!("__pattern_{}", name.value);
                let name_const =
                    self.make_constant(Value::String(rc_str(pattern_global.as_str())), line);
                self.emit_op_u16(OpCode::DefineGlobal, name_const, line);
            }

            Statement::Choose { subject, arms, .. } => {
                // Capture before compiling the subject, which resets the flag.
                let choose_consumed = self.suppress_statement_pop;
                // The subject and any inline patterns are locals of this `choose`. As
                // globals (`__choose_tmp__`, `__pattern_<name>`) a pattern condition
                // that ran another `choose` overwrote the subject, and an inline
                // pattern replaced any top-level pattern of the same name.
                self.begin_scope();
                self.compile_expression(subject);
                let subject_slot = self.current_frame().locals.len() as u16;
                self.add_local(CHOOSE_SUBJECT, BindingKind::Mutable, None);

                let mut inline_slots: Vec<Option<u16>> = Vec::with_capacity(arms.len());
                for arm in arms {
                    if arm.pattern_name == "else" {
                        break;
                    }
                    let slot = if let (Some(params), Some(condition)) =
                        (&arm.inline_params, &arm.inline_condition)
                    {
                        // ChooseArm::pattern_name has no token, so an inline arm can only be located by line.
                        self.check_pattern_arity(params, Span::new(line as usize, 0));
                        let param_names: Vec<String> =
                            params.iter().map(|p| p.value.clone()).collect();
                        self.compile_pattern_function(&param_names, condition, line);
                        let slot = self.current_frame().locals.len() as u16;
                        self.add_local(
                            &format!("__pattern_{}", arm.pattern_name),
                            BindingKind::Mutable,
                            None,
                        );
                        Some(slot)
                    } else {
                        None
                    };
                    inline_slots.push(slot);
                }

                let mut end_jumps = Vec::new();
                let mut has_else = false;

                for (i, arm) in arms.iter().enumerate() {
                    if arm.pattern_name == "else" {
                        has_else = true;
                        self.compile_block_as_expression(&arm.body, line, choose_consumed);
                        break;
                    }

                    match inline_slots[i] {
                        Some(slot) => self.emit_op_u16(OpCode::GetLocal, slot, line),
                        None => {
                            self.emit_get_variable(&format!("__pattern_{}", arm.pattern_name), line)
                        }
                    }
                    self.emit_op_u16(OpCode::GetLocal, subject_slot, line);
                    self.emit_op_u8(OpCode::Call, 1, line);

                    let skip = self.emit_jump(OpCode::JumpIfFalse, line);
                    self.emit_op(OpCode::Pop, line); // pop True

                    // Compile arm body — result goes on stack
                    self.compile_block_as_expression(&arm.body, line, choose_consumed);
                    let end = self.emit_jump(OpCode::Jump, line);
                    end_jumps.push(end);

                    self.patch_jump(skip);
                    self.emit_op(OpCode::Pop, line); // pop False
                }

                if !has_else {
                    self.emit_op(OpCode::None, line);
                }

                for j in end_jumps {
                    self.patch_jump(j);
                }
                self.end_scope_keeping_value(line);

                // Choose is a statement — discard the result value (unless suppressed)
                if !self.suppress_statement_pop {
                    self.emit_op(OpCode::Pop, line);
                }
            }

            Statement::Introduce {
                path, selective, ..
            } => {
                let path_str = if path.is_relative {
                    let mut s = String::new();
                    for _ in 0..path.parent_levels {
                        s.push_str("../");
                    }
                    if path.parent_levels == 0 {
                        s.push_str("./");
                    }
                    s.push_str(&path.segments.join("/"));
                    s
                } else {
                    path.segments.join("/")
                };

                let path_const = self.make_constant(Value::String(rc_str(path_str.as_str())), line);

                if let Some(lint) = &mut self.lint {
                    match selective {
                        Some(names) => lint.defs.extend(names.iter().map(|n| n.value.clone())),
                        // A namespace import binds the path's last segment.
                        None => lint.defs.extend(path.segments.last().cloned()),
                    }
                }

                if let Some(names) = selective {
                    // Selective import: push the names
                    for name in names {
                        let nc =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        self.emit_op_u16(OpCode::Constant, nc, line);
                    }
                    self.emit_op_u16(OpCode::Import, path_const, line);
                    let count = self.count_u8(names.len(), line);
                    self.emit_byte(count, line);
                } else {
                    self.emit_op_u16(OpCode::Import, path_const, line);
                    self.emit_byte(0, line); // no selective names
                }
            }
        }
    }

    // ── Expression Compilation ─────────────────────────────────────────

    fn compile_expression(&mut self, expr: &Expression) {
        let line = Self::expr_line(expr);
        let col = Self::expr_column(expr);
        self.loc_column_stack.push(col);
        self.compile_expression_inner(expr, line);
        self.loc_column_stack.pop();
    }

    fn compile_expression_inner(&mut self, expr: &Expression, line: u32) {
        // Sub-expressions default to consumed: only a transparent wrapper forwards the captured context.
        let consumed = self.value_consumed;
        self.value_consumed = true;

        // Fold pure-literal compound nodes; leaves keep their True/False/None fast-path opcodes.
        if matches!(expr, Expression::Prefix { .. } | Expression::Infix { .. })
            && let Some(folded) = fold_constant_expression(expr)
        {
            self.emit_constant(folded, line);
            return;
        }

        match expr {
            Expression::Int { value, .. } => {
                self.emit_constant(Value::Integer(*value), line);
            }
            Expression::Float { value, .. } => {
                self.emit_constant(Value::Float(*value), line);
            }
            Expression::Char { value, .. } => {
                self.emit_constant(Value::Char(*value), line);
            }
            Expression::Str { value, .. } => {
                self.emit_constant(Value::String(rc_str(value.as_str())), line);
            }
            Expression::Boolean { value, .. } => {
                self.emit_op(if *value { OpCode::True } else { OpCode::False }, line);
            }
            Expression::NoneExpr { .. } => {
                self.emit_op(OpCode::None, line);
            }

            Expression::Grouped(inner) => {
                // A group is transparent: forward the context so a discarded option with a `stop` tail stays legal.
                self.value_consumed = consumed;
                self.compile_expression(inner);
            }

            // diverge/converge lower to __spawn/__join_task, whose value is this node's value.
            Expression::Diverge { token, body } => {
                self.value_consumed = consumed;
                self.compile_expression(&crate::ast::desugar_diverge(token, body));
            }
            Expression::DivergeEach {
                token,
                variable,
                iterable,
                body,
            } => {
                self.value_consumed = consumed;
                self.compile_expression(&crate::ast::desugar_diverge_each(
                    token, variable, iterable, body,
                ));
            }
            Expression::Converge {
                token,
                task,
                timeout,
            } => {
                self.value_consumed = consumed;
                self.compile_expression(&crate::ast::desugar_converge(
                    token,
                    task,
                    timeout.as_deref(),
                ));
            }

            Expression::Prefix {
                operator, right, ..
            } => {
                self.compile_expression(right);
                match operator.as_str() {
                    "-" => self.emit_op(OpCode::Negate, line),
                    "!" | "not" => self.emit_op(OpCode::Not, line),
                    "~" => self.emit_op(OpCode::BitNot, line),
                    _ => self.error_coded(
                        codes::UNKNOWN_OPERATOR,
                        &format!("unknown prefix operator: {}", operator),
                        line,
                    ),
                }
            }

            Expression::Infix {
                operator,
                left,
                right,
                ..
            } => {
                if operator == "and" || operator == "or" {
                    self.lint_logical(operator, left, right);
                }
                // Short-circuit logical operators
                if operator == "and" {
                    self.compile_expression(left);
                    let jump = self.emit_jump(OpCode::JumpIfFalse, line);
                    self.emit_op(OpCode::Pop, line);
                    self.compile_expression(right);
                    self.patch_jump(jump);
                    return;
                }
                if operator == "or" {
                    self.compile_expression(left);
                    let jump = self.emit_jump(OpCode::JumpIfTrue, line);
                    self.emit_op(OpCode::Pop, line);
                    self.compile_expression(right);
                    self.patch_jump(jump);
                    return;
                }

                // Distributed comparison: (a or b) == c  →  a == c or b == c
                if matches!(operator.as_str(), "==" | "!=" | "<" | ">" | "<=" | ">=") {
                    let left_inner = unwrap_grouped(left);
                    let right_inner = unwrap_grouped(right);
                    let left_is_logical = matches!(left_inner,
                        Expression::Infix { operator: op, .. } if op == "or" || op == "and"
                    );
                    let right_is_logical = matches!(right_inner,
                        Expression::Infix { operator: op, .. } if op == "or" || op == "and"
                    );
                    if left_is_logical && !right_is_logical {
                        self.compile_distributed_comparison(
                            operator, left_inner, right, true, line,
                        );
                        return;
                    }
                    if self.lint.is_some() && operator == "!=" {
                        let group = if left_is_logical {
                            left_inner
                        } else {
                            right_inner
                        };
                        if let Expression::Infix {
                            operator: op,
                            token,
                            ..
                        } = group
                            && op == "or"
                            && (left_is_logical != right_is_logical)
                        {
                            self.lint_warn(
                                token.span,
                                "`x != (a or b)` is true unless a and b are equal, since x can only equal one of them".into(),
                                "to check that x is neither, write `x != a and x != b`",
                            );
                        }
                    }
                    if right_is_logical && !left_is_logical {
                        self.compile_distributed_comparison(
                            operator,
                            right_inner,
                            left,
                            false,
                            line,
                        );
                        return;
                    }
                }

                self.compile_expression(left);
                self.hold_temps(1);
                self.compile_expression(right);
                self.release_temps(1);
                match operator.as_str() {
                    "+" => self.emit_op(OpCode::Add, line),
                    "-" => self.emit_op(OpCode::Subtract, line),
                    "*" => self.emit_op(OpCode::Multiply, line),
                    "/" => self.emit_op(OpCode::Divide, line),
                    "%" => self.emit_op(OpCode::Modulo, line),
                    "==" => self.emit_op(OpCode::Equal, line),
                    "!=" => self.emit_op(OpCode::NotEqual, line),
                    "<" => self.emit_op(OpCode::Less, line),
                    "<=" => self.emit_op(OpCode::LessEqual, line),
                    ">" => self.emit_op(OpCode::Greater, line),
                    ">=" => self.emit_op(OpCode::GreaterEqual, line),
                    "&" => self.emit_op(OpCode::BitAnd, line),
                    "|" => self.emit_op(OpCode::BitOr, line),
                    "^" => self.emit_op(OpCode::BitXor, line),
                    "<<" => self.emit_op(OpCode::ShiftLeft, line),
                    ">>" => self.emit_op(OpCode::ShiftRight, line),
                    _ => self.error_coded(
                        codes::UNKNOWN_OPERATOR,
                        &format!("unknown infix operator: {}", operator),
                        line,
                    ),
                }
            }

            Expression::Postfix { operator, left, .. } => {
                // Postfix requires an identifier
                if let Expression::Ident(ident) = left.as_ref() {
                    if let Some(slot) = self.resolve_local(&ident.value)
                        && !self.current_frame().locals[slot as usize].kind.is_mutable()
                    {
                        let kind = self.current_frame().locals[slot as usize].kind;
                        self.report_immutable_write(
                            &ident.value,
                            kind,
                            ident.token.span,
                            "mutate",
                            &format!(" with `{operator}`"),
                        );
                    }
                    // Load current value (return value is the pre-increment value)
                    self.compile_identifier(ident);
                    // Duplicate for the return value
                    self.emit_op(OpCode::Dup, line);
                    self.emit_constant(Value::Integer(1), line);
                    match operator.as_str() {
                        "++" => self.emit_op(OpCode::Add, line),
                        "--" => self.emit_op(OpCode::Subtract, line),
                        _ => self.error_coded(
                            codes::UNKNOWN_OPERATOR,
                            &format!("unknown postfix operator: {}", operator),
                            line,
                        ),
                    }
                    if let Some(slot) = self.resolve_local(&ident.value) {
                        self.emit_op_u16(OpCode::SetLocal, slot, line);
                    } else {
                        let frame_idx = self.frames.len() - 1;
                        if let Some(uv_idx) = self.resolve_upvalue(frame_idx, &ident.value) {
                            self.check_upvalue_write(
                                &ident.value,
                                ident.token.span,
                                "mutate",
                                &format!(" with `{operator}`"),
                                false,
                                line,
                            );
                            self.emit_op_u16(OpCode::SetUpvalue, uv_idx, line);
                        } else {
                            let name_const = self
                                .make_constant(Value::String(rc_str(ident.value.as_str())), line);
                            self.emit_op_u16(OpCode::SetGlobal, name_const, line);
                        }
                    }
                    self.emit_op(OpCode::Pop, line); // pop the stored (new) value, leaving pre-inc
                } else {
                    self.error_coded(
                        codes::UNKNOWN_OPERATOR,
                        "postfix operator requires identifier",
                        line,
                    );
                }
            }

            Expression::Ident(ident) => {
                self.compile_identifier(ident);
            }

            Expression::Array { elements, .. } => {
                for elem in elements {
                    self.compile_expression(elem);
                    self.hold_temps(1);
                }
                self.release_temps(elements.len());
                let count = elements.len() as u16;
                self.emit_op_u16(OpCode::BuildArray, count, line);
            }

            Expression::TupleLiteral { elements, .. } => {
                for elem in elements {
                    self.compile_expression(elem);
                    self.hold_temps(1);
                }
                self.release_temps(elements.len());
                let count = elements.len() as u16;
                self.emit_op_u16(OpCode::BuildTuple, count, line);
            }

            Expression::MapLiteral { entries, .. } => {
                for (key, val) in entries {
                    self.compile_expression(key);
                    self.hold_temps(1);
                    self.compile_expression(val);
                    self.hold_temps(1);
                }
                self.release_temps(entries.len() * 2);
                let count = entries.len() as u16;
                self.emit_op_u16(OpCode::BuildMap, count, line);
            }

            Expression::StringInterp { parts, .. } => {
                for part in parts {
                    match part {
                        StringInterpPart::Literal(s) => {
                            self.emit_constant(Value::String(rc_str(s.as_str())), line);
                        }
                        StringInterpPart::Expr(expr) => {
                            self.compile_expression(expr);
                        }
                    }
                    self.hold_temps(1);
                }
                self.release_temps(parts.len());
                let count = parts.len() as u16;
                self.emit_op_u16(OpCode::StringInterp, count, line);
            }

            Expression::Call {
                token,
                function,
                args,
                named_args,
            } => {
                // Detect special builtins that need AST-level access
                if let Expression::Ident(ident) = function.as_ref() {
                    let call_span = ident.token.span.to(token.span);
                    match ident.value.as_str() {
                        "is_mut" => {
                            self.compile_is_mut(args, call_span);
                            return;
                        }
                        "is_type" => {
                            self.compile_is_type(args, call_span);
                            return;
                        }
                        "is_type_mut" => {
                            self.compile_is_type_mut(args, call_span);
                            return;
                        }
                        _ => {}
                    }
                }

                // Method call: obj.method(args) → MethodCall opcode
                if let Expression::DotAccess { left, field, .. } = function.as_ref() {
                    self.compile_expression(left);
                    self.hold_temps(1);
                    for arg in args {
                        self.compile_expression(arg);
                        self.hold_temps(1);
                    }
                    let method_const =
                        self.make_constant(Value::String(rc_str(field.value.as_str())), line);
                    if named_args.is_empty() {
                        self.release_temps(args.len() + 1);
                        self.emit_op_u16(OpCode::MethodCall, method_const, line);
                        let count = self.count_u8(args.len(), line);
                        self.emit_byte(count, line);
                    } else {
                        for (name, val_expr) in named_args {
                            self.emit_constant(Value::String(rc_str(name.as_str())), line);
                            self.hold_temps(1);
                            self.compile_expression(val_expr);
                            self.hold_temps(1);
                        }
                        self.release_temps(args.len() + 1 + named_args.len() * 2);
                        self.emit_op_u16(OpCode::MethodCallNamed, method_const, line);
                        let count = self.count_u8(args.len(), line);
                        self.emit_byte(count, line);
                        let count = self.count_u8(named_args.len(), line);
                        self.emit_byte(count, line);
                    }
                    return;
                }

                // Normal function call
                self.compile_expression(function);
                self.hold_temps(1);
                for arg in args {
                    self.compile_expression(arg);
                    self.hold_temps(1);
                }

                if named_args.is_empty() {
                    self.release_temps(args.len() + 1);
                    let count = self.count_u8(args.len(), line);
                    self.emit_op_u8(OpCode::Call, count, line);
                } else {
                    // Push named args: name string, then value
                    for (name, val_expr) in named_args {
                        self.emit_constant(Value::String(rc_str(name.as_str())), line);
                        self.hold_temps(1);
                        self.compile_expression(val_expr);
                        self.hold_temps(1);
                    }
                    self.release_temps(args.len() + 1 + named_args.len() * 2);
                    self.emit_op(OpCode::CallNamed, line);
                    let count = self.count_u8(args.len(), line);
                    self.emit_byte(count, line);
                    let count = self.count_u8(named_args.len(), line);
                    self.emit_byte(count, line);
                }
            }

            Expression::Index { left, index, .. } => {
                self.compile_expression(left);
                self.hold_temps(1);
                self.compile_expression(index);
                self.release_temps(1);
                self.emit_op(OpCode::Index, line);
            }

            Expression::Slice {
                left, start, end, ..
            } => {
                self.compile_expression(left);
                self.hold_temps(1);
                let mut flags: u8 = 0;
                if let Some(s) = start {
                    self.compile_expression(s);
                    self.hold_temps(1);
                    flags |= 0x01;
                }
                if let Some(e) = end {
                    self.compile_expression(e);
                    self.hold_temps(1);
                    flags |= 0x02;
                }
                self.release_temps(1 + start.is_some() as usize + end.is_some() as usize);
                self.emit_op_u8(OpCode::Slice, flags, line);
            }

            Expression::DotAccess { left, field, .. } => {
                self.compile_expression(left);
                let field_const =
                    self.make_constant(Value::String(rc_str(field.value.as_str())), line);
                self.emit_op_u16(OpCode::GetField, field_const, line);
            }

            Expression::FunctionLiteral {
                parameters, body, ..
            } => {
                self.compile_function(None, parameters, body, line);
            }

            Expression::StructLiteral {
                struct_name,
                field_values,
                ..
            } => {
                let name_const =
                    self.make_constant(Value::String(rc_str(struct_name.as_str())), line);
                for (fname, fval) in field_values {
                    self.emit_constant(Value::String(rc_str(fname.as_str())), line);
                    self.hold_temps(1);
                    self.compile_expression(fval);
                    self.hold_temps(1);
                }
                self.release_temps(field_values.len() * 2);
                self.emit_op_u16(OpCode::StructLiteral, name_const, line);
                let count = self.count_u8(field_values.len(), line);
                self.emit_byte(count, line);
            }

            Expression::EnumVariantConstruct {
                enum_name,
                variant_name,
                kind,
                ..
            } => {
                // Push the EnumDef then the payload pieces, then emit Make.
                self.compile_expression(&Expression::Ident(Identifier {
                    token: crate::token::Token {
                        token_type: crate::token::TokenType::Ident,
                        literal: enum_name.clone(),
                        span: crate::token::Span::new(line as usize, 0),
                    },
                    value: enum_name.clone(),
                }));
                self.hold_temps(1);
                let variant_const =
                    self.make_constant(Value::String(rc_str(variant_name.as_str())), line);
                match kind {
                    EnumConstructKind::Tuple(args) => {
                        for arg in args {
                            self.compile_expression(arg);
                            self.hold_temps(1);
                        }
                        self.release_temps(args.len() + 1);
                        self.emit_op_u16(OpCode::MakeEnumVariantTuple, variant_const, line);
                        let count = self.count_u8(args.len(), line);
                        self.emit_byte(count, line);
                    }
                    EnumConstructKind::Struct(fields) => {
                        for (fname, fval) in fields {
                            self.emit_constant(Value::String(rc_str(fname.as_str())), line);
                            self.hold_temps(1);
                            self.compile_expression(fval);
                            self.hold_temps(1);
                        }
                        self.release_temps(fields.len() * 2 + 1);
                        self.emit_op_u16(OpCode::MakeEnumVariantStruct, variant_const, line);
                        let count = self.count_u8(fields.len(), line);
                        self.emit_byte(count, line);
                    }
                }
            }

            Expression::Unless {
                consequence,
                condition,
                alternative,
                ..
            } => {
                // `unless` is: consequence unless condition, else alternative.
                self.compile_expression(condition);
                let then_jump = self.emit_jump(OpCode::JumpIfFalse, line);
                self.emit_op(OpCode::Pop, line);
                // Condition true → alternative
                self.compile_expression(alternative);
                let end_jump = self.emit_jump(OpCode::Jump, line);
                self.patch_jump(then_jump);
                self.emit_op(OpCode::Pop, line);
                // Condition false → consequence
                self.compile_expression(consequence);
                self.patch_jump(end_jump);
            }

            Expression::Option {
                arms,
                default,
                error_default,
                ..
            } => {
                let mut end_jumps = Vec::new();

                // An <Error> arm fires only when evaluating an arm raises, never on no-match.
                let err_handler = if error_default.is_some() {
                    Some(self.emit_push_handler(line))
                } else {
                    None
                };

                for arm in arms {
                    self.compile_expression(&arm.condition);
                    let skip = self.emit_jump(OpCode::JumpIfFalse, line);
                    self.emit_op(OpCode::Pop, line);
                    // `consumed` was captured before the reset; a skip/stop tail is only rejected when true.
                    self.compile_block_as_expression(&arm.body, line, consumed);
                    let end = self.emit_jump(OpCode::Jump, line);
                    end_jumps.push(end);
                    self.patch_jump(skip);
                    self.emit_op(OpCode::Pop, line);
                }

                // The <Error> arm is never the no-match default.
                if let Some(default_body) = default {
                    self.compile_block_as_expression(default_body, line, consumed);
                } else {
                    self.emit_op(OpCode::None, line);
                }

                // Normal-path convergence: arms jump here; default falls through.
                for j in end_jumps {
                    self.patch_jump(j);
                }

                if let (Some(handler), Some(error_body)) = (err_handler, error_default) {
                    // Normal path: remove the handler and skip the catch code.
                    self.emit_pop_handler(line);
                    let skip_catch = self.emit_jump(OpCode::Jump, line);
                    // The handler unwinds here with the error value on top; discard it before the <Error> body.
                    self.patch_jump(handler);
                    self.emit_op(OpCode::Pop, line);
                    self.compile_block_as_expression(error_body, line, consumed);
                    self.patch_jump(skip_catch);
                }
            }

            Expression::Guard {
                value,
                binding,
                error_tag,
                fallback,
                ..
            } => {
                // The handler makes a raise during `value` land at Guard as an in-band error rather than unwinding past it.
                let handler = self.emit_push_handler(line);
                self.compile_expression(value);
                self.emit_pop_handler(line);
                self.patch_jump(handler); // catch target = the Guard opcode below

                let binding_const =
                    self.make_constant(Value::String(rc_str(binding.value.as_str())), line);
                // 0xFFFF means no tag filter; otherwise it is the constant index of the tag string.
                let tag_const = match error_tag {
                    Some(t) => self.make_constant(Value::String(rc_str(t.as_str())), line),
                    None => 0xFFFF,
                };

                self.emit_op(OpCode::Guard, line);
                let guard_jump_pos = self.current_chunk().len();
                self.current_chunk().write_u16(0xFFFF, line); // jump offset placeholder
                // Binding constant (read by VM after jump offset)
                self.emit_byte((binding_const >> 8) as u8, line);
                self.emit_byte(binding_const as u8, line);
                // Tag filter constant
                self.emit_byte((tag_const >> 8) as u8, line);
                self.emit_byte(tag_const as u8, line);

                // If not error, value stays on stack → jump over fallback
                let end_jump = self.emit_jump(OpCode::Jump, line);

                // VM IP is past all 6 operand bytes, so offset = fallback_start - (pos + 6).
                let fallback_start = self.current_chunk().len();
                let offset = fallback_start - guard_jump_pos - 6;
                self.current_chunk()
                    .patch_u16(guard_jump_pos, offset as u16);

                // Error case: the error is left on the stack and bound as a local of the
                // fallback alone. It used to go into the VM's globals, clobbering a
                // global of that name and losing to any local of that name.
                self.with_held_as_locals(|c| {
                    c.begin_scope();
                    c.add_local(&binding.value, BindingKind::Mutable, None);
                    c.compile_expression(fallback);
                    c.end_scope_keeping_value(line);
                });
                self.patch_jump(end_jump);
            }

            Expression::ErrorConstruct { tag, value, .. } => {
                self.compile_expression(value);
                if let Some(tag_str) = tag {
                    self.emit_constant(Value::String(rc_str(tag_str.as_str())), line);
                    self.emit_op_u8(OpCode::ErrorConstruct, 1, line);
                } else {
                    self.emit_op_u8(OpCode::ErrorConstruct, 0, line);
                }
            }

            Expression::ValueConstruct { value, .. } => {
                self.compile_expression(value);
                self.emit_op(OpCode::ValueConstruct, line);
            }

            Expression::Fail { value, .. } => {
                self.compile_expression(value);
                self.emit_op(OpCode::Fail, line);
            }

            Expression::TypeWrap { target, value, .. } => {
                let type_str = target.type_name();
                // Catch target is the TypeWrap, which turns the in-band error into the Error side of the union.
                let is_error_union = type_str.contains(" || ") && {
                    let parts: Vec<&str> = type_str.split(" || ").collect();
                    parts.contains(&"VALUE") && parts.iter().any(|p| p.starts_with("ERROR"))
                };
                let handler = if is_error_union {
                    Some(self.emit_push_handler(line))
                } else {
                    None
                };
                self.compile_expression(value);
                if let Some(handler) = handler {
                    self.emit_pop_handler(line);
                    self.patch_jump(handler); // catch target = the TypeWrap opcode
                }
                let type_const = self.make_constant(Value::String(rc_str(type_str.as_str())), line);
                self.emit_op_u16(OpCode::TypeWrap, type_const, line);
            }

            Expression::Log {
                tag,
                sub_tag,
                message,
                ..
            } => {
                let mut flags: u8 = 0;
                if let Some(t) = tag {
                    self.emit_constant(Value::String(rc_str(t.as_str())), line);
                    flags |= 0x01;
                }
                if let Some(st) = sub_tag {
                    self.emit_constant(Value::String(rc_str(st.as_str())), line);
                    flags |= 0x02;
                }
                if let Some(msg) = message {
                    self.compile_expression(msg);
                    flags |= 0x04;
                }
                self.emit_op_u8(OpCode::Log, flags, line);
            }
        }
    }

    /// Compile a block of statements as an expression — the last Expr statement's
    /// value is kept on the stack (not popped). Used for option arms, function bodies, etc.
    /// Compile the last statement of a block keeping its value on the stack.
    /// For Expr, keeps the expression value. For Choose/If, suppresses the
    /// trailing Pop so the result is preserved.
    /// Emit a compile error if a statement in a value-producing position is a
    /// bare `skip`/`stop` AND `consumed` is true (the produced value is used by
    /// an enclosing operation). The tree-walker treats these as `Object::Skip`/
    /// `Object::Stop` sentinels which error the moment they are consumed as a
    /// value (e.g. `1 + skip` => `INTEGER + SKIP`). VM/JIT must reject the same
    /// programs instead of silently pushing a bogus value. A `skip`/`stop` whose
    /// value is DISCARDED (the enclosing `option`/`choose` is a bare expression-
    /// statement) is legitimate loop control flow (`consumed == false`) and is
    /// left to compile + run.
    fn compile_last_statement_as_value(&mut self, stmt: &Statement, line: u32, consumed: bool) {
        match stmt {
            Statement::Skip { token } | Statement::Stop { token } if consumed => {
                let is_skip = matches!(stmt, Statement::Skip { .. });
                self.report_skip_stop_misuse(is_skip, token.span, true);
            }
            Statement::Expr(expr) => {
                // Forward the context, or a nested option with a skip/stop tail is wrongly rejected.
                let prev = self.value_consumed;
                self.value_consumed = consumed;
                self.compile_expression(expr);
                self.value_consumed = prev;
            }
            Statement::Choose { .. } => {
                // Choose produces a value but normally pops it.
                self.suppress_statement_pop = true;
                self.compile_statement(stmt);
                self.suppress_statement_pop = false;
            }
            Statement::If {
                condition,
                consequence,
                alternative: Some(alternative),
                ..
            } => {
                // Both branches produce a value; a one-armed guard does not. Each arm is
                // a value block — compiled as statements, the arms' values were popped.
                self.compile_expression(condition);
                let else_jump = self.emit_jump(OpCode::JumpIfFalse, line);
                self.emit_op(OpCode::Pop, line);
                self.compile_block_as_expression(consequence, line, consumed);
                let end_jump = self.emit_jump(OpCode::Jump, line);
                self.patch_jump(else_jump);
                self.emit_op(OpCode::Pop, line);
                self.compile_block_as_expression(alternative, line, consumed);
                self.patch_jump(end_jump);
            }
            _ => {
                self.compile_statement(stmt);
                self.emit_op(OpCode::None, line);
            }
        }
    }

    /// Define a local/global for unpack. For "_", still occupies the stack slot
    /// via add_local so it doesn't shift positions for subsequent names.
    fn compile_unpack_define(&mut self, name: &crate::ast::Identifier, line: u32) {
        if self.current_frame().scope_depth > 0 {
            // add_local for both "_" and real names — "_" just occupies the slot
            self.add_local(&name.value, BindingKind::Mutable, None);
        } else if name.value == "_" {
            self.emit_op(OpCode::Pop, line);
        } else {
            let name_const = self.make_constant(Value::String(rc_str(name.value.as_str())), line);
            self.emit_op_u16(OpCode::DefineGlobal, name_const, line);
        }
    }

    /// Store the stack top into an existing variable for unpack, then pop it.
    /// `walrus` (`:=`) may override an immutable constant, as `x := v` may.
    fn compile_unpack_bind(&mut self, name: &crate::ast::Identifier, walrus: bool, line: u32) {
        if name.value == "_" {
            self.emit_op(OpCode::Pop, line);
            return;
        }
        if let Some(slot) = self.resolve_local(&name.value) {
            let kind = self.current_frame().locals[slot as usize].kind;
            let forbidden = match kind {
                BindingKind::Mutable => false,
                BindingKind::ImmutableConstant { .. } => !walrus,
                BindingKind::ImmutableLoopVar { .. } | BindingKind::ImmutableTypeName { .. } => {
                    true
                }
            };
            if forbidden {
                self.report_immutable_write(&name.value, kind, name.token.span, "assign to", "");
            }
            self.emit_op_u16(OpCode::SetLocal, slot, line);
        } else {
            let frame_idx = self.frames.len() - 1;
            if let Some(uv_idx) = self.resolve_upvalue(frame_idx, &name.value) {
                self.check_upvalue_write(
                    &name.value,
                    name.token.span,
                    "assign to",
                    "",
                    walrus,
                    line,
                );
                self.emit_op_u16(OpCode::SetUpvalue, uv_idx, line);
            } else {
                let name_const =
                    self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                self.emit_op_u16(OpCode::SetGlobal, name_const, line);
            }
        }
        self.emit_op(OpCode::Pop, line);
    }

    /// True when `:=` on `name` would declare a new local rather than update
    /// an existing local, upvalue or global (the same rule `x := v` follows).
    fn unpack_name_is_fresh(&self, name: &str) -> bool {
        self.resolve_local(name).is_none()
            && !self.declared_globals.contains(name)
            && self.upvalue_binding(name).is_none()
    }

    /// Compile a block as a value-producing expression (option/choose arm,
    /// default/error body). `consumed` says whether the surrounding construct's
    /// value is used by an enclosing operation: when true, a bare `skip`/`stop`
    /// tail is a COMPILE error (it would become a consumed sentinel, which the
    /// tree-walker errors on, e.g. `INTEGER + SKIP`); when false (the construct
    /// is a bare expression-statement whose value is discarded), a `skip`/`stop`
    /// tail is legitimate loop control flow and is left to compile + run.
    fn compile_block_as_expression(&mut self, stmts: &[Statement], line: u32, consumed: bool) {
        if stmts.is_empty() {
            self.emit_op(OpCode::None, line);
            return;
        }
        // Values the enclosing expression already pushed occupy the next stack
        // slots. Unnamed placeholder locals stand in for them, so the block's own
        // locals are numbered past them; they live in the enclosing scope, so the
        // block's cleanup leaves them alone, and a `skip`/`stop` out of here pops
        // them with everything else above the loop's floor.
        self.with_held_as_locals(|c| c.compile_block_body_as_expression(stmts, line, consumed));
    }

    /// Run `body` with the held temporaries standing in as unnamed locals, so
    /// any local it declares gets the stack slot it will really occupy.
    fn with_held_as_locals(&mut self, body: impl FnOnce(&mut Self)) {
        let held = std::mem::take(&mut self.current_frame_mut().temps);
        let outer_depth = self.current_frame().scope_depth;
        for _ in 0..held {
            self.current_frame_mut().locals.push(Local {
                name: String::new(),
                depth: outer_depth,
                is_captured: false,
                kind: BindingKind::Mutable,
                type_constraint: None,
            });
        }
        body(self);
        let frame = self.current_frame_mut();
        let keep = frame.locals.len() - held;
        frame.locals.truncate(keep);
        frame.temps = held;
    }

    fn compile_block_body_as_expression(&mut self, stmts: &[Statement], line: u32, consumed: bool) {
        // Wrap in a scope so locals are cleaned up.
        self.begin_scope();
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            if is_last {
                // Compile the last statement inside the scope so it can reference block locals.
                match stmt {
                    Statement::Expr(_)
                    | Statement::If {
                        alternative: Some(_),
                        ..
                    } => {
                        self.compile_last_statement_as_value(stmt, line, consumed);
                        self.end_scope_keeping_value(line);
                    }
                    Statement::Skip { .. } | Statement::Stop { .. } if consumed => {
                        // Matches the tree-walker's error on `INTEGER + SKIP`.
                        self.compile_last_statement_as_value(stmt, line, consumed);
                        self.end_scope_keeping_value(line);
                    }
                    _ => {
                        self.compile_statement(stmt);
                        self.end_scope(line);
                        self.emit_op(OpCode::None, line);
                    }
                }
            } else if consumed && matches!(stmt, Statement::Skip { .. } | Statement::Stop { .. }) {
                // A non-tail skip/stop jumps before the tail value exists, so the block's value is ill-defined.
                let is_skip = matches!(stmt, Statement::Skip { .. });
                let span = match stmt {
                    Statement::Skip { token } | Statement::Stop { token } => token.span,
                    _ => unreachable!("guarded by the matches! above"),
                };
                self.report_skip_stop_misuse(is_skip, span, true);
            } else {
                self.compile_statement(stmt);
            }
        }
    }

    /// Compile a distributed comparison: `(a or b) == c` → `a == c or b == c`.
    /// Recursively walks the logical tree and emits comparisons at each leaf.
    fn compile_distributed_comparison(
        &mut self,
        cmp_op: &str,
        logical_expr: &Expression,
        cmp_value_expr: &Expression,
        value_on_right: bool,
        line: u32,
    ) {
        let inner = unwrap_grouped(logical_expr);
        if let Expression::Infix {
            operator,
            left,
            right,
            ..
        } = inner
        {
            if operator == "or" {
                self.compile_distributed_comparison(
                    cmp_op,
                    left,
                    cmp_value_expr,
                    value_on_right,
                    line,
                );
                let jump = self.emit_jump(OpCode::JumpIfTrue, line);
                self.emit_op(OpCode::Pop, line);
                self.compile_distributed_comparison(
                    cmp_op,
                    right,
                    cmp_value_expr,
                    value_on_right,
                    line,
                );
                self.patch_jump(jump);
                return;
            }
            if operator == "and" {
                self.compile_distributed_comparison(
                    cmp_op,
                    left,
                    cmp_value_expr,
                    value_on_right,
                    line,
                );
                let jump = self.emit_jump(OpCode::JumpIfFalse, line);
                self.emit_op(OpCode::Pop, line);
                self.compile_distributed_comparison(
                    cmp_op,
                    right,
                    cmp_value_expr,
                    value_on_right,
                    line,
                );
                self.patch_jump(jump);
                return;
            }
        }
        if value_on_right {
            self.compile_expression(logical_expr);
            self.compile_expression(cmp_value_expr);
        } else {
            self.compile_expression(cmp_value_expr);
            self.compile_expression(logical_expr);
        }
        self.emit_comparison_op(cmp_op, line);
    }

    fn emit_comparison_op(&mut self, op: &str, line: u32) {
        match op {
            "==" => self.emit_op(OpCode::Equal, line),
            "!=" => self.emit_op(OpCode::NotEqual, line),
            "<" => self.emit_op(OpCode::Less, line),
            "<=" => self.emit_op(OpCode::LessEqual, line),
            ">" => self.emit_op(OpCode::Greater, line),
            ">=" => self.emit_op(OpCode::GreaterEqual, line),
            _ => self.error_coded(
                codes::UNKNOWN_OPERATOR,
                &format!("unknown comparison operator: {}", op),
                line,
            ),
        }
    }

    /// Compile `is_mut(x)` — check if variable is mutable.
    fn compile_is_mut(&mut self, args: &[Expression], span: Span) {
        let line = span.line() as u32;
        if args.len() != 1 {
            self.error_at(
                codes::INTRINSIC_MISUSE,
                span,
                "is_mut() takes exactly 1 argument",
            );
            self.emit_op(OpCode::False, line);
            return;
        }
        if let Expression::Ident(ident) = &args[0] {
            // Check if it's a local — resolve at compile time
            if let Some(slot) = self.resolve_local(&ident.value) {
                let mutable = self.current_frame().locals[slot as usize].kind.is_mutable();
                self.emit_op(if mutable { OpCode::True } else { OpCode::False }, line);
            } else {
                // Global — emit runtime check
                let name_const =
                    self.make_constant(Value::String(rc_str(ident.value.as_str())), line);
                self.emit_op_u16(OpCode::IsMut, name_const, line);
            }
        } else if let Expression::DotAccess { .. } = &args[0] {
            // Struct fields are always mutable
            self.emit_op(OpCode::True, line);
        } else {
            self.error_at(
                codes::INTRINSIC_MISUSE,
                span,
                "argument to is_mut must be a variable name",
            );
            self.emit_op(OpCode::False, line);
        }
    }

    /// Compile `is_type(x, int)` — check if value matches type.
    fn compile_is_type(&mut self, args: &[Expression], span: Span) {
        let line = span.line() as u32;
        if args.len() != 2 {
            self.error_at(
                codes::INTRINSIC_MISUSE,
                span,
                "is_type() takes exactly 2 arguments",
            );
            self.emit_op(OpCode::False, line);
            return;
        }
        self.compile_expression(&args[0]);
        // Second argument should be a type name identifier
        if let Expression::Ident(type_ident) = &args[1] {
            let type_const =
                self.make_constant(Value::String(rc_str(type_ident.value.as_str())), line);
            self.emit_op_u16(OpCode::IsType, type_const, line);
        } else {
            self.error_at(
                codes::INTRINSIC_MISUSE,
                span,
                "second argument to is_type must be a type name",
            );
            self.emit_op(OpCode::Pop, line);
            self.emit_op(OpCode::False, line);
        }
    }

    /// Compile `is_type_mut(x)` — check if variable has no type constraint.
    fn compile_is_type_mut(&mut self, args: &[Expression], span: Span) {
        let line = span.line() as u32;
        if args.len() != 1 {
            self.error_at(
                codes::INTRINSIC_MISUSE,
                span,
                "is_type_mut() takes exactly 1 argument",
            );
            self.emit_op(OpCode::False, line);
            return;
        }
        if let Expression::Ident(ident) = &args[0] {
            // Check if it's a local — resolve at compile time
            if let Some(slot) = self.resolve_local(&ident.value) {
                let has_constraint = self.current_frame().locals[slot as usize]
                    .type_constraint
                    .is_some();
                self.emit_op(
                    if has_constraint {
                        OpCode::False
                    } else {
                        OpCode::True
                    },
                    line,
                );
            } else {
                // Global — emit runtime check
                let name_const =
                    self.make_constant(Value::String(rc_str(ident.value.as_str())), line);
                self.emit_op_u16(OpCode::IsTypeMut, name_const, line);
            }
        } else if let Expression::DotAccess { .. } = &args[0] {
            // Struct fields have locked types
            self.emit_op(OpCode::False, line);
        } else {
            self.error_at(
                codes::INTRINSIC_MISUSE,
                span,
                "argument to is_type_mut must be a variable name",
            );
            self.emit_op(OpCode::False, line);
        }
    }

    fn compile_identifier(&mut self, ident: &Identifier) {
        let line = ident.token.span.line() as u32;
        let col = ident.token.span.column() as u32;
        self.loc_column_stack.push(col);
        self.compile_identifier_inner(ident, line);
        self.loc_column_stack.pop();
    }

    /// Collect the field names visible to a method on `struct_name`, walking
    /// the inheritance chain. Used for implicit-self field resolution.
    fn collect_self_fields(&self, struct_name: &str) -> std::collections::HashSet<String> {
        let mut set = std::collections::HashSet::new();
        let mut cur = Some(struct_name.to_string());
        let mut guard = 0;
        while let Some(name) = cur {
            if let Some(fields) = self.struct_fields.get(&name) {
                for f in fields {
                    set.insert(f.clone());
                }
            }
            cur = self.struct_parents.get(&name).cloned().flatten();
            guard += 1;
            if guard > 64 {
                break; // defensive against a malformed inheritance cycle
            }
        }
        set
    }

    fn compile_identifier_inner(&mut self, ident: &Identifier, line: u32) {
        // Try local first — a parameter or local shadows a same-named field.
        if let Some(slot) = self.resolve_local(&ident.value) {
            self.emit_op_u16(OpCode::GetLocal, slot, line);
            return;
        }
        if let Some((load_self, self_idx)) = self.implicit_self_for(&ident.value) {
            self.emit_op_u16(load_self, self_idx, line);
            let field_const = self.make_constant(Value::String(rc_str(ident.value.as_str())), line);
            self.emit_op_u16(OpCode::GetField, field_const, line);
            return;
        }
        let frame_idx = self.frames.len() - 1;
        if let Some(uv_idx) = self.resolve_upvalue(frame_idx, &ident.value) {
            self.emit_op_u16(OpCode::GetUpvalue, uv_idx, line);
            return;
        }
        if let Some(lint) = &mut self.lint {
            lint.reads.push((ident.value.clone(), ident.token.span));
        }
        let name_const = self.make_constant(Value::String(rc_str(ident.value.as_str())), line);
        self.emit_op_u16(OpCode::GetGlobal, name_const, line);
    }

    fn compile_function(
        &mut self,
        name: Option<&str>,
        parameters: &[TypedParam],
        body: &[Statement],
        line: u32,
    ) {
        // Push a new compiler frame for this function.
        // A method carries the struct it was defined in, and so does anything
        // nested inside it — a lambda in a method body still counts as that
        // struct's code, which is what lets it touch `self`'s hidden fields.
        let owning_struct = self
            .method_struct_stack
            .last()
            .cloned()
            .or_else(|| self.current_frame().function.method_of.clone());
        self.frames
            .push(CompilerFrame::new(name.map(|s| s.to_string()), 1));
        self.current_frame_mut().function.method_of = owning_struct;
        let arity = self.count_u8(parameters.len(), line);
        self.current_frame_mut().function.arity = arity;

        let params: Vec<ParamInfo> = parameters
            .iter()
            .map(|p| ParamInfo {
                name: p.ident.value.clone(),
                has_default: p.default.is_some(),
                optional: p.optional,
                type_ann: p.type_ann.as_ref().map(|t| t.type_name()),
            })
            .collect();
        self.current_frame_mut().function.params = params;

        for param in parameters {
            self.add_local(
                &param.ident.value,
                BindingKind::Mutable,
                param.type_ann.as_ref().map(|t| t.type_name()),
            );
        }

        for (i, param) in parameters.iter().enumerate() {
            if let Some(default_expr) = &param.default {
                let slot = (i + 1) as u16; // +1 for slot 0 reserved
                // Check if param is None (not provided), if so set default
                self.emit_op_u16(OpCode::GetLocal, slot, line);
                self.emit_op(OpCode::None, line);
                self.emit_op(OpCode::Equal, line);
                let skip_default = self.emit_jump(OpCode::JumpIfFalse, line);
                self.emit_op(OpCode::Pop, line);
                self.compile_expression(default_expr);
                self.emit_op_u16(OpCode::SetLocal, slot, line);
                self.emit_op(OpCode::Pop, line);
                let end = self.emit_jump(OpCode::Jump, line);
                self.patch_jump(skip_default);
                self.emit_op(OpCode::Pop, line);
                self.patch_jump(end);
            }
        }

        // Compile body — last expression/statement is implicit return
        if body.is_empty() {
            self.emit_op(OpCode::None, line);
            self.emit_op(OpCode::Return, line);
        } else {
            for (i, stmt) in body.iter().enumerate() {
                let is_last = i == body.len() - 1;
                if is_last {
                    // A function's tail value is consumed by the caller, so a bare skip/stop tail is a value error.
                    self.compile_last_statement_as_value(stmt, line, true);
                    self.emit_op(OpCode::Return, line);
                } else {
                    self.compile_statement(stmt);
                }
            }
        }

        self.finalize_local_locks();
        let frame = self.frames.pop().unwrap();
        let mut function = frame.function;
        function.id = self.fn_counter;
        self.fn_counter += 1;
        let upvalues = frame.upvalues;

        let (uv_kinds, uv_values) = crate::vm::value::make_upvalue_int_caches(0);
        let func_const = self.make_constant(
            Value::Closure(std::rc::Rc::new(crate::vm::value::ObjClosure {
                function: std::rc::Rc::new(function),
                upvalues: Vec::new(), // placeholder, VM fills real upvalues
                module_globals: crate::vm::value::ModuleGlobals::none(),
                call_count: std::cell::Cell::new(0),
                loop_count: std::cell::Cell::new(0),
                jit_state: std::cell::Cell::new(0),
                jit_bailouts: std::cell::Cell::new(0),
                jit_thunk: std::cell::Cell::new(None),
                specialized_thunk: std::cell::Cell::new(None),
                specialized_arity: std::cell::Cell::new(0),
                specialized_kind: std::cell::Cell::new(0),
                lean_thunk: std::cell::Cell::new(None),
                lean_arity: std::cell::Cell::new(0),
                upvalue_int_kinds: uv_kinds,
                upvalue_int_values: uv_values,
            })),
            line,
        );

        self.emit_op_u16(OpCode::Closure, func_const, line);
        // Followed by upvalue descriptors
        for uv in &upvalues {
            self.emit_byte(if uv.is_local { 1 } else { 0 }, line);
            self.current_chunk().write_u16(uv.index, line);
        }
    }

    fn compile_pattern_function(
        &mut self,
        param_names: &[String],
        condition: &Expression,
        line: u32,
    ) {
        let params: Vec<TypedParam> = param_names
            .iter()
            .map(|name| TypedParam {
                ident: Identifier {
                    token: crate::token::Token {
                        token_type: crate::token::TokenType::Ident,
                        literal: name.clone(),
                        span: crate::token::Span::new(line as usize, 0),
                    },
                    value: name.clone(),
                },
                type_ann: None,
                default: None,
                optional: false,
            })
            .collect();

        let body = vec![Statement::Give {
            token: crate::token::Token {
                token_type: crate::token::TokenType::Give,
                literal: "give".to_string(),
                span: crate::token::Span::new(line as usize, 0),
            },
            value: condition.clone(),
        }];

        self.compile_function(Some("__pattern__"), &params, &body, line);
    }
}

/// Default value for a type annotation.
fn default_for_type(type_name: &str) -> Value {
    match type_name {
        "INTEGER" => Value::Integer(0),
        "FLOAT" => Value::Float(0.0),
        "STRING" => Value::String(rc_str("")),
        "BOOLEAN" => Value::Boolean(false),
        "CHAR" => Value::Char('\0'),
        "BYTE" => Value::Byte(0),
        "UINT" => Value::Uint(0),
        "ARRAY" => Value::Array(std::rc::Rc::new(std::cell::RefCell::new(Vec::new().into()))),
        "TUPLE" => Value::Tuple(std::rc::Rc::new(Vec::new().into())),
        "MAP" => Value::Map(std::rc::Rc::new(std::cell::RefCell::new(
            crate::vm::collections::OxMap::new(),
        ))),
        "SET" => Value::Set(std::rc::Rc::new(std::cell::RefCell::new(
            crate::vm::collections::OxSet::new(),
        ))),
        _ => Value::None,
    }
}

/// An integer the compiler can read straight out of the AST. A negated literal
/// (`-1`) parses as a prefix expression rather than a negative `Int`, so match
/// that shape too — otherwise `range(a, b, -1)` misses the counting-loop
/// lowering and falls back to materialising an array.
fn literal_int(expr: &Expression) -> Option<i64> {
    match expr {
        Expression::Int { value, .. } => Some(*value),
        Expression::Grouped(inner) => literal_int(inner),
        Expression::Prefix {
            operator, right, ..
        } if operator == "-" => literal_int(right).and_then(i64::checked_neg),
        _ => None,
    }
}
