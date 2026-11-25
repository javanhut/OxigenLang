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
/// Operators that can fail at runtime (division/modulo by zero) return `None`.
/// Shifts mask the count to the low 6 bits and integer arithmetic wraps,
/// matching the VM.
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
                ("+", Value::Integer(a), Value::Integer(b)) => {
                    Some(Value::Integer(a.wrapping_add(*b)))
                }
                ("-", Value::Integer(a), Value::Integer(b)) => {
                    Some(Value::Integer(a.wrapping_sub(*b)))
                }
                ("*", Value::Integer(a), Value::Integer(b)) => {
                    Some(Value::Integer(a.wrapping_mul(*b)))
                }
                ("/", Value::Integer(a), Value::Integer(b)) if *b != 0 => {
                    a.checked_div(*b).map(Value::Integer)
                }
                ("%", Value::Integer(a), Value::Integer(b)) if *b != 0 => {
                    a.checked_rem(*b).map(Value::Integer)
                }
                ("&", Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a & b)),
                ("|", Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a | b)),
                ("^", Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a ^ b)),
                ("<<", Value::Integer(a), Value::Integer(b)) => {
                    Some(Value::Integer(a.wrapping_shl(*b as u32)))
                }
                (">>", Value::Integer(a), Value::Integer(b)) => {
                    Some(Value::Integer(a.wrapping_shr(*b as u32)))
                }
                ("==", Value::Integer(a), Value::Integer(b)) => Some(Value::Boolean(a == b)),
                ("!=", Value::Integer(a), Value::Integer(b)) => Some(Value::Boolean(a != b)),
                ("<", Value::Integer(a), Value::Integer(b)) => Some(Value::Boolean(a < b)),
                ("<=", Value::Integer(a), Value::Integer(b)) => Some(Value::Boolean(a <= b)),
                (">", Value::Integer(a), Value::Integer(b)) => Some(Value::Boolean(a > b)),
                (">=", Value::Integer(a), Value::Integer(b)) => Some(Value::Boolean(a >= b)),
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
/// Carries a [`Code`] and, where available, a [`Span`]. A bare line is widened
/// into a point span by `into_diagnostic`.
#[derive(Debug)]
pub struct CompileError {
    pub message: String,
    pub line: u32,
    pub code: Code,
    pub span: Option<Span>,
    /// Pre-built diagnostic carrying labels, notes or helps.
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
/// Used to derive the message and declaration span when a write to an immutable
/// binding is reported.
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
    /// `lock name = v`: never written again, not even with `:=`.
    Locked {
        decl: Span,
    },
}

impl BindingKind {
    fn is_mutable(self) -> bool {
        matches!(self, BindingKind::Mutable)
    }
}

/// Names of compiler-made locals. They contain a NUL, so user code cannot read or write them.
const LOOP_INDEX: &str = "\u{0}index";
const LOOP_RANGE_END: &str = "\u{0}range_end";
const LOOP_RANGE_LIMIT: &str = "\u{0}range_limit";
const LOOP_ITERABLE: &str = "\u{0}iterable";
const CHOOSE_SUBJECT: &str = "\u{0}choose";
const POSTFIX_OBJECT: &str = "\u{0}postfix_object";
const POSTFIX_INDEX: &str = "\u{0}postfix_index";
const POSTFIX_OLD: &str = "\u{0}postfix_old";

/// What a non-identifier `++`/`--` writes back to.
#[derive(Clone, Copy)]
enum PostfixPlace<'a> {
    Index(&'a Expression),
    Field(&'a str),
}

/// Tracks a local variable during compilation.
#[derive(Debug, Clone)]
struct Local {
    name: String,
    depth: i32,
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
    /// every local above this floor.
    loop_continue_floors: Vec<usize>,
    /// Per-loop local-stack floor for `stop`: the height the loop's exit target
    /// expects. For `each` this excludes the loop variable; for `repeat` it is the
    /// pre-body height. `stop` emits a Pop/CloseUpvalue for every local above this
    /// floor before jumping to the exit.
    loop_exit_floors: Vec<usize>,
    /// Count of error-handler regions currently open lexically in this frame
    /// (incremented by `PushHandler`, decremented by `PopHandler`).
    handler_depth: usize,
    /// `handler_depth` captured at each enclosing loop's entry, used to emit
    /// balancing `PopHandler`s for handlers opened inside the loop body on
    /// `stop`/`skip`.
    loop_handler_depths: Vec<usize>,
    /// Lexical nesting depth of `each` loop bodies currently being compiled in
    /// this frame. `repeat` does not increment it; the V1-typed update-vs-shadow
    /// choice for typed re-declarations depends on it.
    each_body_depth: usize,
    /// Constant index of the declared return type's name, for
    /// `fun f() <int> { ... }`. Every `Return` in the frame is preceded by a
    /// `CheckReturn` against it. `None` when no return type was written (or it was
    /// `<generic>`, which accepts anything).
    return_type: Option<u16>,
    /// Every `SetLocal` emitted: `(offset, slot, lock of the binding written)`.
    /// `finalize_local_locks` uses it to re-attach each typed binding's lock to its
    /// own writes.
    local_writes: Vec<(usize, u16, Option<String>)>,
    /// Values the expression being compiled has pushed and not yet consumed
    /// (a call's callee and earlier arguments, an operator's left operand). They sit
    /// above the locals.
    temps: usize,
}

impl CompilerFrame {
    fn new(name: Option<String>, scope_depth: i32) -> Self {
        let mut function = Function::new(name, 0);
        function.locals.push(LocalInfo::default());
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
            return_type: None,
            local_writes: Vec::new(),
            temps: 0,
        }
    }
}

/// Compiles an AST into bytecode.
pub struct Compiler {
    frames: Vec<CompilerFrame>,
    errors: Vec<CompileError>,
    /// When true, the next Let/TypedLet/TypedDeclare Dups the value before
    /// defining the variable so it is preserved for implicit return.
    dup_next_define: bool,
    /// When true, Choose/If do not pop their result value (set when they are
    /// the last statement of a block that produces a value).
    suppress_statement_pop: bool,
    /// Whether the value produced by the expression currently being compiled is
    /// consumed by an enclosing operation (operator operand, call argument,
    /// assignment RHS, give/return value) rather than discarded (the top expression
    /// of an expression-statement). Defaults to `true`. A `skip`/`stop` whose value
    /// is consumed as an `option`/`choose` arm tail is a compile error.
    value_consumed: bool,
    /// Column stack for source-location tracking. Pushed at the start of
    /// `compile_expression`/`compile_statement` with the current node's column and
    /// popped at exit. `emit_byte` reads the top before each write.
    loc_column_stack: Vec<u32>,
    /// Field names of each declared struct (own fields), keyed by struct name.
    /// Used to resolve bare identifiers in method bodies to `self.field`.
    struct_fields: std::collections::HashMap<String, Vec<String>>,
    /// Parent struct of each declared struct, for walking the inheritance chain when resolving implicit-self fields.
    struct_parents: std::collections::HashMap<String, Option<String>>,
    /// Stack of resolvable self-field sets for the method currently being
    /// compiled (top = innermost). Empty outside method bodies.
    method_field_stack: Vec<std::collections::HashSet<String>>,
    /// Struct whose `includes` block is being compiled; each method (and anything
    /// nested in it) is stamped with this owner for hidden-field access checks.
    method_struct_stack: Vec<String>,
    /// Names defined as globals (top-level `:=` / `<type>` declarations), in
    /// compilation order. A walrus `:=` of one of these from inside a nested block
    /// updates the global (SetGlobal) rather than creating a local.
    declared_globals: std::collections::HashSet<String>,
    /// Subset of `declared_globals` declared with a type annotation
    /// (`x <int> = 0` / `x <int> := 0` / `x <int>`). A typed re-declaration of an
    /// existing untyped global from a nested block updates that global; a typed
    /// re-declaration of an already-typed global shadows it.
    typed_globals: std::collections::HashSet<String>,
    /// Globals declared with `lock`, with where. Writes to them are compile errors once the declaration has been seen.
    locked_globals: std::collections::HashMap<String, Span>,
    /// Globals currently declared immutable (`x <T> = v`), with where.
    immutable_globals: std::collections::HashMap<String, Span>,
    /// Globals some typed declaration makes mutable (`x <T> := v`, `x <T>`). A
    /// write to an immutable global is reported only once the whole program is
    /// compiled and the name never became mutable.
    made_mutable_globals: std::collections::HashSet<String>,
    /// Writes to immutable globals: (name, declaration, write, verb, suffix).
    pending_immutable_writes: Vec<(String, Span, Span, String, String)>,
    /// Id stamped onto the next `Function` finalized: this compilation's unit
    /// number in the high 32 bits, compile order in the low 32. A recompilation under
    /// the same unit (`Compiler::for_unit`) assigns identical ids.
    fn_counter: u64,
    /// What `Compiler::check` collects on top of compiling; `None` for an ordinary compile.
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

    /// A compiler whose functions get the ids an earlier compilation of the same program under `unit` gave them (see `Function::id`).
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
            locked_globals: std::collections::HashMap::new(),
            immutable_globals: std::collections::HashMap::new(),
            made_mutable_globals: std::collections::HashSet::new(),
            pending_immutable_writes: Vec::new(),
            typed_globals: std::collections::HashSet::new(),
            fn_counter: (unit as u64) << 32,
            lint: None,
        };
        compiler.frames.push(CompilerFrame::new(None, 0));
        compiler
    }

    /// Compile `program` for `oxigen check`: its compile errors, plus a name the
    /// program reads that nothing defines, plus warnings for logic that compiles but
    /// is almost certainly a mistake. `known` lists names that exist without being
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

    /// Like `compile`, returning what the lint collected (if it was on).
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

        self.report_immutable_global_writes();
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

    fn lint_warn(&mut self, span: Span, message: String, help: &str) {
        if let Some(lint) = &mut self.lint {
            lint.warnings
                .push(Diagnostic::warning(codes::SUSPICIOUS_LOGIC, span, message).help(help));
        }
    }

    /// Warns on a literal on the right of `or`/`and` where a comparison was likely
    /// meant: `x == "a" or "b"` and `("a" or "b")`. A literal on the right of a plain
    /// value is a fallback (`name or "anonymous"`) and is not reported.
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

    /// Gives every write to a typed local its binding's lock where the slot's own
    /// lock cannot hold it (another binding of a different type shares the slot).
    /// Those writes become `SetLocalLocked`.
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
        chunk.write_u16(0xFFFF, line);
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
    /// loop body, before a `stop`/`skip` jump leaves that body. Does not change the
    /// lexical `handler_depth`.
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
        let jump = current - offset - 2;
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
    /// The message and spans are derived from the binding's recorded provenance
    /// (`BindingKind`).
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

            BindingKind::Locked { decl } => Diagnostic::error(
                codes::IMMUTABLE_ASSIGN,
                write,
                format!("cannot {verb} locked variable `{name}`{suffix}"),
            )
            .label(format!("cannot {verb}{suffix}"))
            .secondary(decl, format!("`{name}` is locked here"))
            .note(
                "`lock` makes a variable and its value permanently read-only: it can't be \
                 reassigned or overridden with `:=`",
            )
            .help(format!(
                "make a changeable copy with `new_name := copy({name})`"
            )),

            BindingKind::Mutable => return,
        };
        self.report(d);
    }

    /// Rejects a struct or enum named like a built-in type.
    fn reject_builtin_type_name(&mut self, name: &Identifier) {
        if crate::vm::is_builtin_type_name(&name.value) {
            self.error_at(
                codes::COMPILE_ERROR,
                name.token.span,
                &format!("`{}` is reserved for a built-in type", name.value),
            );
        }
    }

    /// Rejects a pattern declared with more than one parameter. A pattern is
    /// always called with exactly one argument, the value being matched.
    fn check_pattern_arity(&mut self, params: &[Identifier], decl: Span) {
        if params.len() == 1 {
            return;
        }

        if params.is_empty() {
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

    /// Reports a misused `skip`/`stop`: either there is no enclosing loop, or the
    /// keyword is used as a value.
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

    /// Reports at a line only, for sites that have no span.
    ///
    /// How to load `self` when the bare, non-local `name` means `self.name`: `name`
    /// is a field of the struct whose method is being compiled and nothing nearer
    /// binds it. Inside a closure nested in the method, `self` is an upvalue.
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

    /// Argument, parameter and element counts are encoded in one byte; larger counts are rejected.
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

        let mut ops: Vec<bool> = Vec::new();
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

    /// Close the current scope when its block produced a result value that is
    /// currently on top of the operand stack (block-as-expression: option/choose arm
    /// bodies, default/error bodies whose tail is a value).
    ///
    /// Stack on entry: `[.. L0, L1, .., L_{k-1}, RESULT]`. `SetLocal floor` copies
    /// RESULT into the lowest local slot, `Pop` removes the duplicate, then the
    /// remaining locals are closed or popped top-down (`CloseUpvalue` when captured,
    /// else `Pop`). The result ends alone at the floor slot. If the floor local is
    /// itself captured, `CloseUpvalueAt floor` closes it in place first; the JIT does
    /// not support that op, so such functions run in the interpreter.
    fn end_scope_keeping_value(&mut self, line: u32) {
        self.current_frame_mut().scope_depth -= 1;
        let depth = self.current_frame().scope_depth;

        let floor = {
            let locals = &self.current_frame().locals;
            let mut f = locals.len();
            while f > 0 && locals[f - 1].depth > depth {
                f -= 1;
            }
            f
        };
        let n = self.current_frame().locals.len();

        let mut captured: Vec<bool> = Vec::with_capacity(n - floor);
        for i in floor..n {
            captured.push(self.current_frame().locals[i].is_captured);
        }
        self.current_frame_mut().locals.truncate(floor);

        if floor == n {
            return;
        }

        if captured[0] {
            self.emit_op_u16(OpCode::CloseUpvalueAt, floor as u16, line);
        }

        if let Some(info) = self.current_frame_mut().function.locals.get_mut(floor) {
            info.type_constraint = None;
        }
        self.emit_op_u16(OpCode::SetLocal, floor as u16, line);
        self.emit_op(OpCode::Pop, line);
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

    /// True if `name` is bound by the user as a local (current or any enclosing
    /// frame) or a declared global. Read-only: registers no capture. Used to confirm
    /// a bare `range` means the builtin.
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

    /// Checks a write through an upvalue like a local write: immutability at
    /// compile time, the declared type lock at run time (`CheckType` before the
    /// `SetUpvalue`). `walrus` writes (`:=`) may override an immutable constant.
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
            BindingKind::ImmutableLoopVar { .. }
            | BindingKind::ImmutableTypeName { .. }
            | BindingKind::Locked { .. } => true,
        };
        if forbidden {
            self.report_immutable_write(name, kind, span, verb, suffix);
        }
        if let Some(ty) = constraint.filter(|t| t != "GENERIC") {
            let tc = self.make_constant(Value::String(rc_str(ty.as_str())), line);
            self.emit_op_u16(OpCode::CheckType, tc, line);
        }
    }

    /// Checks a write to `name` when it means a global (no local or captured
    /// variable of that name is in the way). A `lock`ed global is reported
    /// immediately. An immutable one (`x <T> = v`) is queued for the end of the
    /// program, unless `rebind` (`:=` may override an immutable binding).
    fn check_global_write(&mut self, name: &Identifier, verb: &str, suffix: &str, rebind: bool) {
        if self.resolve_local(&name.value).is_some() || self.upvalue_binding(&name.value).is_some()
        {
            return;
        }
        if let Some(&decl) = self.locked_globals.get(&name.value) {
            self.report_immutable_write(
                &name.value,
                BindingKind::Locked { decl },
                name.token.span,
                verb,
                suffix,
            );
        } else if !rebind && let Some(&decl) = self.immutable_globals.get(&name.value) {
            self.pending_immutable_writes.push((
                name.value.clone(),
                decl,
                name.token.span,
                verb.to_string(),
                suffix.to_string(),
            ));
        }
    }

    /// A typed declaration of global `name` was compiled.
    fn note_global_typed_decl(&mut self, name: &Identifier, mutable: bool) {
        if mutable {
            self.immutable_globals.remove(&name.value);
            self.made_mutable_globals.insert(name.value.clone());
        } else {
            self.immutable_globals
                .insert(name.value.clone(), name.token.span);
        }
    }

    /// Reports the queued writes to globals that stayed immutable.
    fn report_immutable_global_writes(&mut self) {
        for (name, decl, write, verb, suffix) in std::mem::take(&mut self.pending_immutable_writes)
        {
            if self.made_mutable_globals.contains(&name) {
                continue;
            }
            self.report_immutable_write(
                &name,
                BindingKind::ImmutableConstant { decl },
                write,
                &verb,
                &suffix,
            );
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
            | Statement::CompoundAssign { token, .. }
            | Statement::Lock { token, .. }
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

    /// Column number (1-based; 0 = unknown) for the token that anchors this
    /// expression. Mirrors `expr_line`.
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
            | Statement::CompoundAssign { token, .. }
            | Statement::Lock { token, .. }
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
                let prev = self.value_consumed;
                self.value_consumed = false;
                self.compile_expression(expr);
                self.value_consumed = prev;
                self.emit_op(OpCode::Pop, line);
            }

            Statement::Let { name, value } => {
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
                        kind @ BindingKind::Locked { .. } => {
                            self.report_immutable_write(
                                &name.value,
                                kind,
                                name.token.span,
                                "rebind",
                                "",
                            );
                        }
                        BindingKind::Mutable | BindingKind::ImmutableConstant { .. } => {}
                    }
                } else {
                    self.check_global_write(name, "rebind", "", true);
                }

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
                if let Expression::FunctionLiteral {
                    parameters,
                    return_type,
                    body,
                    ..
                } = value
                {
                    self.compile_function(
                        Some(&name.value),
                        parameters,
                        return_type.as_ref(),
                        body,
                        line,
                    );
                } else if let Some(folded) = fold_constant_expression(value) {
                    self.emit_constant(folded, line);
                } else {
                    self.compile_expression(value);
                }
                if self.dup_next_define {
                    self.emit_op(OpCode::Dup, line);
                }
                if predeclared_named_fn {
                    return;
                }
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
                        let name_const =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        self.emit_op_u16(OpCode::SetGlobal, name_const, line);
                        self.emit_op(OpCode::Pop, line);
                    } else if self.current_frame().scope_depth > 0 {
                        self.add_local(&name.value, BindingKind::Mutable, None);
                    } else {
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
                if let Some(folded) = fold_constant_expression(value) {
                    self.emit_constant(folded, line);
                } else {
                    self.compile_expression(value);
                }
                let type_name = type_ann.type_name();
                let mutable = *walrus;
                if !matches!(type_ann, TypeAnnotation::Generic | TypeAnnotation::NoneType) {
                    let tc = self.make_constant(Value::String(rc_str(type_name.as_str())), line);
                    self.emit_op_u16(OpCode::TypeWrap, tc, line);
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
                        self.note_global_typed_decl(name, mutable);
                        self.typed_globals.insert(name.value.clone());
                        let name_const =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        let type_const =
                            self.make_constant(Value::String(rc_str(type_name.as_str())), line);
                        self.emit_op_u16(OpCode::DefineGlobalTyped, name_const, line);
                        self.emit_byte(if mutable { 1 } else { 0 }, line);
                        self.current_chunk().write_u16(type_const, line);
                    } else {
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
                    self.check_global_write(name, "rebind", "", true);
                    self.note_global_typed_decl(name, mutable);
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
                let struct_name = match type_ann {
                    TypeAnnotation::Struct(name) => Some(name.clone()),
                    TypeAnnotation::Qualified { module, name } => Some(format!("{module}.{name}")),
                    _ => None,
                };
                if let Some(struct_name) = struct_name {
                    let sn_const = self.make_constant(Value::String(rc_str(struct_name)), line);
                    self.emit_op_u16(OpCode::StructLiteral, sn_const, line);
                    self.emit_byte(0, line);
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
                        self.note_global_typed_decl(name, true);
                        self.typed_globals.insert(name.value.clone());
                        let name_const =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        let type_const =
                            self.make_constant(Value::String(rc_str(type_name.as_str())), line);
                        self.emit_op_u16(OpCode::DefineGlobalTyped, name_const, line);
                        self.emit_byte(1, line);
                        self.current_chunk().write_u16(type_const, line);
                    } else {
                        self.add_local(&name.value, BindingKind::Mutable, Some(type_name));
                    }
                } else {
                    self.check_global_write(name, "rebind", "", true);
                    self.note_global_typed_decl(name, true);
                    self.declared_globals.insert(name.value.clone());
                    self.typed_globals.insert(name.value.clone());
                    let name_const =
                        self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                    let type_const =
                        self.make_constant(Value::String(rc_str(type_name.as_str())), line);
                    self.emit_op_u16(OpCode::DefineGlobalTyped, name_const, line);
                    self.emit_byte(1, line);
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
                if self.resolve_local(&name.value).is_none()
                    && let Some((load_self, self_idx)) = self.implicit_self_for(&name.value)
                {
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
                        self.check_global_write(name, "assign to", "", false);
                        let name_const =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        self.emit_op_u16(OpCode::SetGlobal, name_const, line);
                    }
                }
                self.emit_op(OpCode::Pop, line);
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
                let continue_floor = self.current_frame().locals.len();
                self.current_frame_mut()
                    .loop_continue_floors
                    .push(continue_floor);
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

                let (index_slot, iter_slot, end_slot, limit_slot) =
                    if let Some((args, _)) = range_args {
                        if args.len() == 1 {
                            self.emit_constant(Value::Integer(0), line);
                        } else {
                            self.compile_expression(&args[0]);
                        }
                        self.add_local(LOOP_INDEX, BindingKind::Mutable, None);
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
                let range_cmp = if range_step > 0 {
                    OpCode::Less
                } else {
                    OpCode::Greater
                };

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

                let exit_floor = self.current_frame().locals.len();
                self.current_frame_mut().loop_exit_floors.push(exit_floor);
                let header = token.span;
                if let Some(index) = index_variable {
                    self.add_local(&index.value, BindingKind::ImmutableLoopVar { header }, None);
                }
                self.add_local(
                    &variable.value,
                    BindingKind::ImmutableLoopVar { header },
                    None,
                );
                let continue_floor = self.current_frame().locals.len();
                self.current_frame_mut()
                    .loop_continue_floors
                    .push(continue_floor);
                let handler_floor = self.current_frame().handler_depth;
                self.current_frame_mut()
                    .loop_handler_depths
                    .push(handler_floor);

                self.begin_scope();
                self.current_frame_mut().each_body_depth += 1;
                for s in body {
                    self.compile_statement(s);
                }
                self.current_frame_mut().each_body_depth -= 1;
                self.end_scope(line);

                let continues = self.current_frame_mut().loop_continues.pop().unwrap();
                for c in continues {
                    self.patch_jump(c);
                }
                self.current_frame_mut().loop_continue_floors.pop();
                self.current_frame_mut().loop_exit_floors.pop();
                self.current_frame_mut().loop_handler_depths.pop();

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

                self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                self.emit_constant(Value::Integer(range_step), line);
                self.emit_op(OpCode::Add, line);
                self.emit_op_u16(OpCode::SetLocal, index_slot, line);
                self.emit_op(OpCode::Pop, line);

                self.emit_loop(loop_start, line);

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
                self.emit_return(line);
            }

            Statement::Unpack {
                names,
                value,
                values,
                reassign,
            } => {
                let local_define = !*reassign && self.current_frame().scope_depth > 0;
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
                    self.compile_expression(value);
                    let count = self.count_u8(names.len(), line);
                    self.emit_op_u8(OpCode::Unpack, count, line);
                }

                if *reassign || placeholders {
                    for name in names.iter().rev() {
                        self.compile_unpack_bind(name, !*reassign, line);
                    }
                } else if local_define {
                    for name in names {
                        self.compile_unpack_define(name, line);
                    }
                } else {
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

            Statement::Lock {
                name,
                type_ann,
                value,
                ..
            } => self.compile_lock(name, type_ann.as_ref(), value, line),

            Statement::CompoundAssign {
                token,
                target,
                operator,
                value,
            } => {
                let step = match operator.as_str() {
                    "+" => OpCode::Add,
                    "-" => OpCode::Subtract,
                    "*" => OpCode::Multiply,
                    "/" => OpCode::Divide,
                    _ => {
                        self.error_coded(
                            codes::UNKNOWN_OPERATOR,
                            &format!("unknown compound operator: {}=", operator),
                            line,
                        );
                        return;
                    }
                };
                match target {
                    Expression::Ident(name) => {
                        let assign = Statement::Assign {
                            name: name.clone(),
                            value: Expression::Infix {
                                token: token.clone(),
                                left: Box::new(target.clone()),
                                operator: operator.clone(),
                                right: Box::new(value.clone()),
                            },
                        };
                        self.compile_statement(&assign);
                    }
                    Expression::Index {
                        left: object,
                        index,
                        ..
                    } => self.compile_place_update(
                        object,
                        PostfixPlace::Index(index),
                        step,
                        Some(value),
                        line,
                    ),
                    Expression::DotAccess {
                        left: object,
                        field,
                        ..
                    } => self.compile_place_update(
                        object,
                        PostfixPlace::Field(&field.value),
                        step,
                        Some(value),
                        line,
                    ),
                    _ => self.error_coded(
                        codes::UNKNOWN_OPERATOR,
                        &format!("`{operator}=` needs a variable, index or field on its left"),
                        line,
                    ),
                }
            }

            Statement::Main { body, .. } => {
                let skip_jump = self.emit_jump(OpCode::Main, line);
                for s in body {
                    self.compile_statement(s);
                }
                self.patch_jump(skip_jump);
            }

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
                let self_fields = self.collect_self_fields(&struct_name.value);
                self.method_field_stack.push(self_fields);
                self.method_struct_stack.push(struct_name.value.clone());
                for (method_name, method_expr) in methods {
                    if let Expression::FunctionLiteral {
                        parameters,
                        return_type,
                        body,
                        ..
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
                        self.compile_function(
                            Some(&method_name.value),
                            &method_params,
                            return_type.as_ref(),
                            body,
                            line,
                        );
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
                let param_names: Vec<String> = params.iter().map(|p| p.value.clone()).collect();
                self.compile_pattern_function(&param_names, condition, line);
                let pattern_global = format!("__pattern_{}", name.value);
                let name_const =
                    self.make_constant(Value::String(rc_str(pattern_global.as_str())), line);
                self.emit_op_u16(OpCode::DefineGlobal, name_const, line);
            }

            Statement::Choose { subject, arms, .. } => {
                let choose_consumed = self.suppress_statement_pop;
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
                    self.emit_op(OpCode::Pop, line);

                    self.compile_block_as_expression(&arm.body, line, choose_consumed);
                    let end = self.emit_jump(OpCode::Jump, line);
                    end_jumps.push(end);

                    self.patch_jump(skip);
                    self.emit_op(OpCode::Pop, line);
                }

                if !has_else {
                    self.emit_op(OpCode::None, line);
                }

                for j in end_jumps {
                    self.patch_jump(j);
                }
                self.end_scope_keeping_value(line);

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
                        None => lint.defs.extend(path.segments.last().cloned()),
                    }
                }

                if let Some(names) = selective {
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
                    self.emit_byte(0, line);
                }
            }
        }
    }

    fn compile_expression(&mut self, expr: &Expression) {
        let line = Self::expr_line(expr);
        let col = Self::expr_column(expr);
        self.loc_column_stack.push(col);
        self.compile_expression_inner(expr, line);
        self.loc_column_stack.pop();
    }

    fn compile_expression_inner(&mut self, expr: &Expression, line: u32) {
        let consumed = self.value_consumed;
        self.value_consumed = true;

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
                self.value_consumed = consumed;
                self.compile_expression(inner);
            }

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
                if let Expression::Ident(ident) = left.as_ref()
                    && self.resolve_local(&ident.value).is_none()
                    && self.implicit_self_for(&ident.value).is_some()
                {
                    let self_expr = Expression::Ident(Identifier {
                        token: ident.token.clone(),
                        value: "self".to_string(),
                    });
                    self.compile_postfix_place(
                        &self_expr,
                        PostfixPlace::Field(&ident.value),
                        operator,
                        line,
                    );
                } else if let Expression::Ident(ident) = left.as_ref() {
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
                    self.compile_identifier(ident);
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
                            self.check_global_write(
                                ident,
                                "mutate",
                                &format!(" with `{operator}`"),
                                false,
                            );
                            let name_const = self
                                .make_constant(Value::String(rc_str(ident.value.as_str())), line);
                            self.emit_op_u16(OpCode::SetGlobal, name_const, line);
                        }
                    }
                    self.emit_op(OpCode::Pop, line);
                } else if let Expression::Index {
                    left: object,
                    index,
                    ..
                } = left.as_ref()
                {
                    self.compile_postfix_place(object, PostfixPlace::Index(index), operator, line);
                } else if let Expression::DotAccess {
                    left: object,
                    field,
                    ..
                } = left.as_ref()
                {
                    self.compile_postfix_place(
                        object,
                        PostfixPlace::Field(&field.value),
                        operator,
                        line,
                    );
                } else {
                    self.error_coded(
                        codes::UNKNOWN_OPERATOR,
                        "postfix operator requires a variable, index or field",
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
                parameters,
                return_type,
                body,
                ..
            } => {
                self.compile_function(None, parameters, return_type.as_ref(), body, line);
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
                self.compile_expression(condition);
                let then_jump = self.emit_jump(OpCode::JumpIfFalse, line);
                self.emit_op(OpCode::Pop, line);
                self.compile_expression(alternative);
                let end_jump = self.emit_jump(OpCode::Jump, line);
                self.patch_jump(then_jump);
                self.emit_op(OpCode::Pop, line);
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

                let err_handler = if error_default.is_some() {
                    Some(self.emit_push_handler(line))
                } else {
                    None
                };

                for arm in arms {
                    self.compile_expression(&arm.condition);
                    let skip = self.emit_jump(OpCode::JumpIfFalse, line);
                    self.emit_op(OpCode::Pop, line);
                    self.compile_block_as_expression(&arm.body, line, consumed);
                    let end = self.emit_jump(OpCode::Jump, line);
                    end_jumps.push(end);
                    self.patch_jump(skip);
                    self.emit_op(OpCode::Pop, line);
                }

                if let Some(default_body) = default {
                    self.compile_block_as_expression(default_body, line, consumed);
                } else {
                    self.emit_op(OpCode::None, line);
                }

                for j in end_jumps {
                    self.patch_jump(j);
                }

                if let (Some(handler), Some(error_body)) = (err_handler, error_default) {
                    self.emit_pop_handler(line);
                    let skip_catch = self.emit_jump(OpCode::Jump, line);
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
                let handler = self.emit_push_handler(line);
                self.compile_expression(value);
                self.emit_pop_handler(line);
                self.patch_jump(handler);

                let binding_const =
                    self.make_constant(Value::String(rc_str(binding.value.as_str())), line);
                let tag_const = match error_tag {
                    Some(t) => self.make_constant(Value::String(rc_str(t.as_str())), line),
                    None => 0xFFFF,
                };

                self.emit_op(OpCode::Guard, line);
                let guard_jump_pos = self.current_chunk().len();
                self.current_chunk().write_u16(0xFFFF, line);
                self.emit_byte((binding_const >> 8) as u8, line);
                self.emit_byte(binding_const as u8, line);
                self.emit_byte((tag_const >> 8) as u8, line);
                self.emit_byte(tag_const as u8, line);

                let end_jump = self.emit_jump(OpCode::Jump, line);

                let fallback_start = self.current_chunk().len();
                let offset = fallback_start - guard_jump_pos - 6;
                self.current_chunk()
                    .patch_u16(guard_jump_pos, offset as u16);

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
                    self.patch_jump(handler);
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

    /// Compile a block of statements as an expression: the last Expr statement's
    /// value is kept on the stack. Compile the last statement of a block keeping its
    /// value on the stack (for Choose/If the trailing Pop is suppressed). Emits a
    /// compile error if a bare `skip`/`stop` is in a value-producing position and
    /// `consumed` is true.
    fn compile_last_statement_as_value(&mut self, stmt: &Statement, line: u32, consumed: bool) {
        match stmt {
            Statement::Skip { token } | Statement::Stop { token } if consumed => {
                let is_skip = matches!(stmt, Statement::Skip { .. });
                self.report_skip_stop_misuse(is_skip, token.span, true);
            }
            Statement::Expr(expr) => {
                let prev = self.value_consumed;
                self.value_consumed = consumed;
                self.compile_expression(expr);
                self.value_consumed = prev;
            }
            Statement::Choose { .. } => {
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
    /// via add_local.
    fn compile_unpack_define(&mut self, name: &crate::ast::Identifier, line: u32) {
        if self.current_frame().scope_depth > 0 {
            self.add_local(&name.value, BindingKind::Mutable, None);
        } else if name.value == "_" {
            self.emit_op(OpCode::Pop, line);
        } else {
            self.check_global_write(name, "rebind", "", true);
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
                BindingKind::ImmutableLoopVar { .. }
                | BindingKind::ImmutableTypeName { .. }
                | BindingKind::Locked { .. } => true,
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
                self.check_global_write(name, "assign to", "", false);
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
    /// value is used by an enclosing operation: when true, a bare `skip`/`stop` tail
    /// is a compile error; when false it is loop control flow.
    fn compile_block_as_expression(&mut self, stmts: &[Statement], line: u32, consumed: bool) {
        if stmts.is_empty() {
            self.emit_op(OpCode::None, line);
            return;
        }
        self.with_held_as_locals(|c| c.compile_block_body_as_expression(stmts, line, consumed));
    }

    /// `lock name = value` / `lock name <type> = value`. The value becomes a
    /// deep read-only copy (`OpCode::Lock`) and the name a binding nothing can
    /// write: not `=`, `:=`, `++` or a compound assignment.
    fn compile_lock(
        &mut self,
        name: &Identifier,
        type_ann: Option<&TypeAnnotation>,
        value: &Expression,
        line: u32,
    ) {
        let decl = name.token.span;
        let depth = self.current_frame().scope_depth;
        let clashes = if depth > 0 {
            self.current_frame()
                .locals
                .iter()
                .rev()
                .take_while(|l| l.depth >= depth)
                .any(|l| l.name == name.value)
        } else {
            self.declared_globals.contains(&name.value)
        };
        if clashes {
            match self.locked_globals.get(&name.value).copied() {
                Some(prev) if depth == 0 => self.report_immutable_write(
                    &name.value,
                    BindingKind::Locked { decl: prev },
                    decl,
                    "re-lock",
                    "",
                ),
                _ => self.report(
                    Diagnostic::error(
                        codes::IMMUTABLE_ASSIGN,
                        decl,
                        format!("`{}` already exists here", name.value),
                    )
                    .label("`lock` declares a new variable")
                    .help(format!(
                        "lock a copy under a new name: `lock {}_locked = {}`",
                        name.value, name.value
                    )),
                ),
            }
        }

        self.compile_expression(value);
        let type_name = type_ann.map(|t| t.type_name());
        if let Some(t) = type_ann
            && !matches!(t, TypeAnnotation::Generic | TypeAnnotation::NoneType)
        {
            let tc = self.make_constant(Value::String(rc_str(t.type_name().as_str())), line);
            self.emit_op_u16(OpCode::TypeWrap, tc, line);
        }
        self.emit_op(OpCode::Lock, line);
        if self.dup_next_define {
            self.emit_op(OpCode::Dup, line);
        }

        if depth > 0 {
            self.add_local(&name.value, BindingKind::Locked { decl }, type_name);
        } else {
            self.declared_globals.insert(name.value.clone());
            if type_name.is_some() {
                self.typed_globals.insert(name.value.clone());
            }
            self.locked_globals.insert(name.value.clone(), decl);
            let name_const = self.make_constant(Value::String(rc_str(name.value.as_str())), line);
            let type_const = self.make_constant(
                Value::String(rc_str(type_name.as_deref().unwrap_or("GENERIC"))),
                line,
            );
            self.emit_op_u16(OpCode::DefineGlobalTyped, name_const, line);
            self.emit_byte(2, line);
            self.current_chunk().write_u16(type_const, line);
        }
    }

    /// `object[index]++` / `object.field++` (and `--`). The expression's value
    /// is the old one, as for `x++`.
    fn compile_postfix_place(
        &mut self,
        object: &Expression,
        place: PostfixPlace<'_>,
        operator: &str,
        line: u32,
    ) {
        let step = match operator {
            "++" => OpCode::Add,
            "--" => OpCode::Subtract,
            _ => {
                self.error_coded(
                    codes::UNKNOWN_OPERATOR,
                    &format!("unknown postfix operator: {}", operator),
                    line,
                );
                return;
            }
        };
        self.compile_place_update(object, place, step, None, line);
    }

    /// Rewrite `object[index]` or `object.field` to `old <step> rhs`, where `rhs`
    /// is `1` for `++`/`--` (leaving the old value as the expression's result) and
    /// the right-hand side for `+=` and friends (leaving nothing). The object and
    /// index are evaluated once, into hidden locals; a missing map key is an error.
    fn compile_place_update(
        &mut self,
        object: &Expression,
        place: PostfixPlace<'_>,
        step: OpCode,
        rhs: Option<&Expression>,
        line: u32,
    ) {
        let field_const = match place {
            PostfixPlace::Field(name) => {
                Some(self.make_constant(Value::String(rc_str(name)), line))
            }
            PostfixPlace::Index(_) => None,
        };

        self.with_held_as_locals(|c| {
            c.begin_scope();

            c.compile_expression(object);
            let object_slot = c.current_frame().locals.len() as u16;
            c.add_local(POSTFIX_OBJECT, BindingKind::Mutable, None);

            let index_slot = if let PostfixPlace::Index(index) = place {
                c.compile_expression(index);
                let slot = c.current_frame().locals.len() as u16;
                c.add_local(POSTFIX_INDEX, BindingKind::Mutable, None);
                Some(slot)
            } else {
                None
            };

            c.emit_op_u16(OpCode::GetLocal, object_slot, line);
            match (index_slot, field_const) {
                (Some(slot), _) => {
                    c.emit_op_u16(OpCode::GetLocal, slot, line);
                    c.emit_op(OpCode::IndexExisting, line);
                }
                (None, Some(field)) => c.emit_op_u16(OpCode::GetField, field, line),
                (None, None) => unreachable!("place is an index or a field"),
            }
            let old_slot = c.current_frame().locals.len() as u16;
            c.add_local(POSTFIX_OLD, BindingKind::Mutable, None);

            c.emit_op_u16(OpCode::GetLocal, object_slot, line);
            if let Some(slot) = index_slot {
                c.emit_op_u16(OpCode::GetLocal, slot, line);
            }
            c.emit_op_u16(OpCode::GetLocal, old_slot, line);
            match rhs {
                Some(value) => {
                    let held = 2 + index_slot.is_some() as usize;
                    c.hold_temps(held);
                    c.compile_expression(value);
                    c.release_temps(held);
                }
                None => c.emit_constant(Value::Integer(1), line),
            }
            c.emit_op(step, line);
            match field_const {
                Some(field) => c.emit_op_u16(OpCode::SetField, field, line),
                None => c.emit_op(OpCode::IndexAssign, line),
            }

            if rhs.is_some() {
                c.end_scope(line);
            } else {
                c.emit_op_u16(OpCode::GetLocal, old_slot, line);
                c.end_scope_keeping_value(line);
            }
        });
    }

    /// Run `body` with the held temporaries standing in as unnamed locals, so any local it declares gets its real stack slot.
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
        self.begin_scope();
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            if is_last {
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
            if let Some(slot) = self.resolve_local(&ident.value) {
                let mutable = self.current_frame().locals[slot as usize].kind.is_mutable();
                self.emit_op(if mutable { OpCode::True } else { OpCode::False }, line);
            } else {
                let name_const =
                    self.make_constant(Value::String(rc_str(ident.value.as_str())), line);
                self.emit_op_u16(OpCode::IsMut, name_const, line);
            }
        } else if let Expression::DotAccess { .. } = &args[0] {
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
                let name_const =
                    self.make_constant(Value::String(rc_str(ident.value.as_str())), line);
                self.emit_op_u16(OpCode::IsTypeMut, name_const, line);
            }
        } else if let Expression::DotAccess { .. } = &args[0] {
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

    /// Collect the field names visible to a method on `struct_name`, walking the
    /// inheritance chain. Used for implicit-self field resolution.
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
                break;
            }
        }
        set
    }

    fn compile_identifier_inner(&mut self, ident: &Identifier, line: u32) {
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
        return_type: Option<&TypeAnnotation>,
        body: &[Statement],
        line: u32,
    ) {
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

        if let Some(rt) = return_type
            && !matches!(rt, TypeAnnotation::Generic)
        {
            let tc = self.make_constant(Value::String(rc_str(rt.type_name().as_str())), line);
            self.current_frame_mut().return_type = Some(tc);
        }

        for param in parameters {
            self.add_local(
                &param.ident.value,
                BindingKind::Mutable,
                param.type_ann.as_ref().map(|t| t.type_name()),
            );
        }

        for (i, param) in parameters.iter().enumerate() {
            if let Some(default_expr) = &param.default {
                let slot = (i + 1) as u16;
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

        if body.is_empty() {
            self.emit_op(OpCode::None, line);
            self.emit_return(line);
        } else {
            for (i, stmt) in body.iter().enumerate() {
                let is_last = i == body.len() - 1;
                if is_last {
                    self.compile_last_statement_as_value(stmt, line, true);
                    self.emit_return(line);
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
                upvalues: Vec::new(),
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
        for uv in &upvalues {
            self.emit_byte(if uv.is_local { 1 } else { 0 }, line);
            self.current_chunk().write_u16(uv.index, line);
        }
    }

    /// Return the value on top of the stack, first checking it against the
    /// frame's declared return type when there is one.
    fn emit_return(&mut self, line: u32) {
        if let Some(tc) = self.current_frame().return_type {
            self.emit_op_u16(OpCode::CheckReturn, tc, line);
        }
        self.emit_op(OpCode::Return, line);
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

        self.compile_function(Some("__pattern__"), &params, None, &body, line);
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

/// An integer the compiler can read straight out of the AST, including a negated literal (`-1`).
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
