pub mod opcode;
pub mod slot_types;

use crate::ast::*;
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
                ("/", Value::Integer(a), Value::Integer(b)) if *b != 0 => {
                    Some(Value::Integer(a.wrapping_div(*b)))
                }
                ("%", Value::Integer(a), Value::Integer(b)) if *b != 0 => {
                    Some(Value::Integer(a.wrapping_rem(*b)))
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
#[derive(Debug)]
pub struct CompileError {
    pub message: String,
    pub line: u32,
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] Compile error: {}", self.line, self.message)
    }
}

/// Tracks a local variable during compilation.
#[derive(Debug, Clone)]
struct Local {
    name: String,
    depth: i32, // -1 means uninitialized
    is_captured: bool,
    mutable: bool,
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
            mutable: false,
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
    /// Monotonic id stamped onto each `Function` at finalization, in compile
    /// order. Deterministic for a given source, so a worker compiling the same
    /// source assigns identical ids (closure transfer across threads relies on
    /// this to identify a lambda's code).
    fn_counter: u32,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
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
            declared_globals: std::collections::HashSet::new(),
            typed_globals: std::collections::HashSet::new(),
            fn_counter: 0,
        };
        // Push the top-level script frame.
        compiler.frames.push(CompilerFrame::new(None, 0));
        compiler
    }

    /// Compile a program and return the top-level function.
    pub fn compile(mut self, program: &Program) -> Result<Function, Vec<CompileError>> {
        // Compile all statements. The last expression statement's value is
        // kept as the program's return value (implicit return, like functions).
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
                        // Named function definitions should not be implicitly
                        // returned — treat them like other statements.
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
            return Err(self.errors);
        }

        let frame = self.frames.pop().unwrap();
        let mut function = frame.function;
        function.id = self.fn_counter;
        self.fn_counter += 1;
        Ok(function)
    }

    // ── Helpers ────────────────────────────────────────────────────────

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
        // Re-establish the column from the active scope's top, in case a
        // recursive child compile updated the chunk's `cur_column` to its
        // own and then returned. Without this the parent's emit_op-after-
        // child-compile would inherit the child's column.
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
        self.emit_op(op, line);
        let chunk = self.current_chunk();
        chunk.write_u16(operand, line);
    }

    fn emit_op_u8(&mut self, op: OpCode, operand: u8, line: u32) {
        self.emit_op(op, line);
        self.emit_byte(operand, line);
    }

    fn make_constant(&mut self, value: Value, line: u32) -> u16 {
        // Intern constant-pool strings so equal literals / identifiers /
        // field names share one canonical `Rc<String>`. This is bounded
        // by the program text (see vm::intern) and lets the `Rc::ptr_eq`
        // fast path in `Value::eq` short-circuit their comparisons.
        let value = match value {
            Value::String(s) => Value::String(crate::vm::intern::intern(&s)),
            other => other,
        };
        let idx = self.current_chunk().add_constant(value);
        if idx > u16::MAX {
            self.error("Too many constants in one chunk", line);
        }
        idx
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
            self.error("Jump too large", 0);
            return;
        }
        self.current_chunk().patch_u16(offset, jump as u16);
    }

    /// Emit a backward loop jump.
    fn emit_loop(&mut self, loop_start: usize, line: u32) {
        self.emit_op(OpCode::Loop, line);
        let offset = self.current_chunk().len() + 2 - loop_start;
        if offset > u16::MAX as usize {
            self.error("Loop body too large", line);
        }
        self.current_chunk().write_u16(offset as u16, line);
    }

    fn error(&mut self, message: &str, line: u32) {
        self.errors.push(CompileError {
            message: message.to_string(),
            line,
        });
    }

    fn begin_scope(&mut self) {
        self.current_frame_mut().scope_depth += 1;
    }

    fn end_scope(&mut self, line: u32) {
        self.current_frame_mut().scope_depth -= 1;
        let depth = self.current_frame().scope_depth;

        // Collect which locals to pop and whether they need CloseUpvalue.
        let mut ops: Vec<bool> = Vec::new(); // true = captured, false = not
        while let Some(local) = self.current_frame().locals.last() {
            if local.depth <= depth {
                break;
            }
            ops.push(local.is_captured);
            self.current_frame_mut().locals.pop();
        }

        // Emit the ops
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
    ///   `r := option { True -> { y := 5  y + 1 } }`  →  VM gave `5`, not `6`.
    ///
    /// Cleanup that preserves the result and still closes captured locals:
    ///   stack on entry: [.. L0, L1, .., L_{k-1}, RESULT]
    /// 1. `SetLocal floor` copies RESULT into the lowest local slot (L0), leaving
    ///    RESULT on top:                       [.. RESULT, L1, .., L_{k-1}, RESULT]
    /// 2. `Pop` removes the duplicate top:      [.. RESULT, L1, .., L_{k-1}]
    /// 3. Close/pop L_{k-1}..L1 top-down (each is on top in turn) — `CloseUpvalue`
    ///    when captured (snapshots the still-live slot), else `Pop`:
    ///                                          [.. RESULT]
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

        // Capture flags for the locals being removed, then drop them from the
        // compile-time scope.
        let mut captured: Vec<bool> = Vec::with_capacity(n - floor);
        for i in floor..n {
            captured.push(self.current_frame().locals[i].is_captured);
        }
        self.current_frame_mut().locals.truncate(floor);

        // No locals: the result is already correctly on top.
        if floor == n {
            return;
        }

        // Captured floor local: the value-preserving stash below overwrites the
        // floor slot with the block's result, so the floor local's open upvalue
        // must be closed (snapshotted from its live slot value) BEFORE the stash.
        // `CloseUpvalueAt floor` does exactly that — it closes the upvalue at the
        // buried floor slot in place, without requiring the value on top and
        // without popping. After this the slot is free to be repurposed, and the
        // remaining body locals (floor+1..n) are closed/popped top-down below.
        if captured[0] {
            self.emit_op_u16(OpCode::CloseUpvalueAt, floor as u16, line);
        }

        // Value-preserving cleanup. Stash the result into the floor slot, drop
        // the duplicate, then close/pop the remaining body locals top-down.
        // The floor slot is being repurposed to hold the block's RESULT, whose
        // type is unrelated to the (now-removed) floor local's declared type, so
        // clear that slot's type lock first — otherwise the SetLocal below would
        // enforce the stale constraint against the result value.
        if let Some(info) = self.current_frame_mut().function.locals.get_mut(floor) {
            info.type_constraint = None;
        }
        self.emit_op_u16(OpCode::SetLocal, floor as u16, line);
        self.emit_op(OpCode::Pop, line);
        // Locals above the floor, innermost first (top of stack first).
        for &cap in captured.iter().skip(1).rev() {
            self.emit_op(
                if cap { OpCode::CloseUpvalue } else { OpCode::Pop },
                line,
            );
        }
    }

    /// Declare a local variable and return its stack slot.
    fn add_local(&mut self, name: &str, mutable: bool, type_constraint: Option<String>) {
        let depth = self.current_frame().scope_depth;
        let slot = self.current_frame().locals.len();
        self.record_local_info(slot, mutable, type_constraint.clone());
        self.current_frame_mut().locals.push(Local {
            name: name.to_string(),
            depth,
            is_captured: false,
            mutable,
            type_constraint,
        });
    }

    fn record_local_info(&mut self, slot: usize, mutable: bool, type_constraint: Option<String>) {
        let locals = &mut self.current_frame_mut().function.locals;
        if locals.len() <= slot {
            locals.resize_with(slot + 1, LocalInfo::default);
        }

        let info = &mut locals[slot];
        info.mutable = mutable;
        match (&info.type_constraint, type_constraint) {
            (None, next) => info.type_constraint = next,
            (Some(current), Some(next)) if current == &next => {}
            (Some(_), None) => {}
            (Some(_), Some(_)) => {
                // The same stack slot can be reused by disjoint lexical
                // scopes. If those scopes disagree on the type lock, keep
                // the slot conservative for whole-function JIT decisions.
                info.type_constraint = None;
            }
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

        // Check if it's a local in the enclosing frame.
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

        // Recursively check further enclosing scopes.
        if let Some(upvalue_idx) = self.resolve_upvalue(parent_idx, name) {
            return Some(self.add_upvalue(frame_idx, upvalue_idx, false));
        }

        None
    }

    fn add_upvalue(&mut self, frame_idx: usize, index: u16, is_local: bool) -> u16 {
        let frame = &mut self.frames[frame_idx];
        // Check if we already have this upvalue.
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
            | Expression::EnumVariantConstruct { token, .. } => token.span.line as u32,
            Expression::Ident(ident) => ident.token.span.line as u32,
            Expression::Grouped(inner) => Self::expr_line(inner),
        }
    }

    fn stmt_line(stmt: &Statement) -> u32 {
        match stmt {
            Statement::Let { name, .. } => name.token.span.line as u32,
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
            | Statement::Test { token, .. } => token.span.line as u32,
            Statement::TypedLet { name, .. }
            | Statement::TypedDeclare { name, .. }
            | Statement::Assign { name, .. } => name.token.span.line as u32,
            Statement::Unpack { names, .. } => {
                names.first().map_or(0, |n| n.token.span.line as u32)
            }
            Statement::Skip | Statement::Stop => 0,
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
            | Expression::EnumVariantConstruct { token, .. } => token.span.column as u32,
            Expression::Ident(ident) => ident.token.span.column as u32,
            Expression::Grouped(inner) => Self::expr_column(inner),
        }
    }

    fn stmt_column(stmt: &Statement) -> u32 {
        match stmt {
            Statement::Let { name, .. } => name.token.span.column as u32,
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
            | Statement::Test { token, .. } => token.span.column as u32,
            Statement::TypedLet { name, .. }
            | Statement::TypedDeclare { name, .. }
            | Statement::Assign { name, .. } => name.token.span.column as u32,
            Statement::Unpack { names, .. } => {
                names.first().map_or(0, |n| n.token.span.column as u32)
            }
            Statement::Skip | Statement::Stop => 0,
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
                // The value of an expression-statement's top expression is
                // DISCARDED (Pop). A `skip`/`stop` tail in an `option`/`choose`
                // here is legitimate loop control flow, so mark it not-consumed.
                let prev = self.value_consumed;
                self.value_consumed = false;
                self.compile_expression(expr);
                self.value_consumed = prev;
                self.emit_op(OpCode::Pop, line);
            }

            Statement::Let { name, value } => {
                // V3: a NAMED function declared as a new local must bind its own
                // name in the current scope BEFORE its body is compiled, so a
                // recursive self-call inside the body can capture itself as an
                // upvalue — mirroring the tree-walker, where the closure shares
                // the same env Rc into which the name is later inserted.
                //
                // Only do this for a genuinely-new local (not a reassignment of
                // an existing local/upvalue, and not the global/top-level path,
                // which already resolves self-recursion via a global lookup).
                // We reserve the slot first; `compile_function`'s `Closure` op
                // pushes the closure into exactly that slot (the next free stack
                // position), and the body's open upvalue captures that live slot.
                let mut predeclared_named_fn = false;
                if let Expression::FunctionLiteral { .. } = value
                    && self.current_frame().scope_depth > 0
                        && self.resolve_local(&name.value).is_none()
                    {
                        let frame_idx = self.frames.len() - 1;
                        let is_upvalue = self.resolve_upvalue(frame_idx, &name.value).is_some();
                        let is_global_reassign =
                            self.declared_globals.contains(&name.value);
                        if !is_upvalue && !is_global_reassign {
                            self.add_local(&name.value, true, None);
                            predeclared_named_fn = true;
                        }
                    }
                // If the value is a function literal, pass the name so the
                // closure carries it (instead of showing as "anonymous").
                if let Expression::FunctionLiteral {
                    parameters, body, ..
                } = value
                {
                    self.compile_function(Some(&name.value), parameters, body, line);
                } else if let Some(folded) = fold_constant_expression(value) {
                    // Tier 2.1 (iii): pure-literal init RHS folds to a
                    // single Constant. Matches Oxigen's design intent —
                    // each init is specific so the JIT can fold and
                    // virtualize cleanly. Side effect: the multi-op-init
                    // mishap (`c := 1 + 5; c + 1` returning 2 in JIT)
                    // disappears for foldable RHS because the init-slot
                    // path now sees a single Constant.
                    self.emit_constant(folded, line);
                } else {
                    self.compile_expression(value);
                }
                if self.dup_next_define {
                    self.emit_op(OpCode::Dup, line);
                }
                // A pre-declared named-fn local (V3) already owns the slot the
                // `Closure` op just pushed into; the binding is complete, so do
                // not re-store/pop it. `dup_next_define` (block-value use) still
                // leaves a copy on the stack for the surrounding expression.
                if predeclared_named_fn {
                    return;
                }
                // Walrus := : if the variable already exists in any scope, update it.
                // Otherwise, create a new binding.
                if let Some(slot) = self.resolve_local(&name.value) {
                    self.emit_op_u16(OpCode::SetLocal, slot, line);
                    self.emit_op(OpCode::Pop, line);
                } else {
                    let frame_idx = self.frames.len() - 1;
                    if let Some(uv_idx) = self.resolve_upvalue(frame_idx, &name.value) {
                        self.emit_op_u16(OpCode::SetUpvalue, uv_idx, line);
                        self.emit_op(OpCode::Pop, line);
                    } else if self.current_frame().scope_depth > 0
                        && self.declared_globals.contains(&name.value)
                    {
                        // `:=` of an existing global from a nested scope updates
                        // the global (matches the tree-walker's update-up-chain),
                        // instead of creating a shadowing local that never stores.
                        let name_const =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        self.emit_op_u16(OpCode::SetGlobal, name_const, line);
                        self.emit_op(OpCode::Pop, line);
                    } else if self.current_frame().scope_depth > 0 {
                        // New local variable
                        self.add_local(&name.value, true, None);
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
                    self.error(
                        &format!(
                            "<generic> cannot be used with '=' (immutable). use ':=' for '{}'",
                            name.value
                        ),
                        line,
                    );
                }
                // Tier 2.1 (iii): same fold as Statement::Let. The
                // subsequent TypeWrap still fires (runtime contract) for
                // non-Generic/None annotations, so the conversion
                // semantics for Form 3 (e.g., `x <int> := 3.9` → 3) are
                // preserved when the RHS is non-foldable. For pure-
                // literal RHS that already matches the target type the
                // wrap is a no-op.
                if let Some(folded) = fold_constant_expression(value) {
                    self.emit_constant(folded, line);
                } else {
                    self.compile_expression(value);
                }
                let type_name = type_ann.type_name();
                let mutable = *walrus; // walrus (:=) means mutable
                // Walrus := converts the value to the target type.
                // Non-walrus = does strict type checking (no conversion).
                if !matches!(type_ann, TypeAnnotation::Generic | TypeAnnotation::NoneType) {
                    let tc = self.make_constant(Value::String(rc_str(type_name.as_str())), line);
                    self.emit_op_u16(OpCode::TypeWrap, tc, line);
                }
                if self.dup_next_define {
                    self.emit_op(OpCode::Dup, line);
                }
                if self.current_frame().scope_depth > 0 {
                    // If a local with the same name already exists at the current or
                    // enclosing scope, update it (same as Let behavior for := re-declarations)
                    if let Some(slot) = self.resolve_local(&name.value) {
                        self.emit_op_u16(OpCode::SetLocal, slot, line);
                        self.emit_op(OpCode::Pop, line);
                    } else if self.declared_globals.contains(&name.value)
                        && !self.typed_globals.contains(&name.value)
                        && self.current_frame().each_body_depth == 0
                    {
                        // V1-typed UPDATE: a typed re-declaration of an existing
                        // UNTYPED global from a nested block updates the global
                        // (re-binding it WITH the new type) rather than creating a
                        // shadowing local that never stores. Mirrors the tree-
                        // walker, where `repeat` shares the enclosing env so
                        // `set_typed` overwrites the existing untyped binding.
                        // Without this, `x := 0; repeat when x <= 5 { x <int> := x + 1 }`
                        // shadows and hangs. The value on the stack was already
                        // TypeWrapped above, so DefineGlobalTyped re-binds with the
                        // type lock (matching `set_typed`).
                        // Inside an `each` body (each_body_depth > 0) the tree-walker
                        // uses a fresh per-iteration env, so we fall through to the
                        // SHADOW branch instead — `x := 0; each i in [1,2,3] { x <int>
                        // := x + 1 }` leaves the outer `x` at 0.
                        self.typed_globals.insert(name.value.clone());
                        let name_const =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        let type_const =
                            self.make_constant(Value::String(rc_str(type_name.as_str())), line);
                        self.emit_op_u16(OpCode::DefineGlobalTyped, name_const, line);
                        self.emit_byte(if mutable { 1 } else { 0 }, line);
                        self.current_chunk().write_u16(type_const, line);
                    } else {
                        // A typed re-declaration in an inner scope SHADOWS when the
                        // outer binding is ALSO typed (documented behavior — the
                        // "Shadowing" example `x <int> = 10; each { x <str> = "hi" }`
                        // leaves the outer `x` untouched), or when there is no outer
                        // global of that name. Mirrors the tree-walker, where `each`
                        // introduces a fresh per-iteration env that the shadow is
                        // written into.
                        self.add_local(&name.value, mutable, Some(type_name));
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
                        // V1-typed UPDATE (zero-init form): a typed re-declaration
                        // of an existing UNTYPED global from a nested block updates
                        // the global (re-binding with the new type) instead of
                        // shadowing. See the matching note on `TypedLet`. Inside an
                        // `each` body we fall through to SHADOW (fresh per-iteration
                        // env in the tree-walker).
                        self.typed_globals.insert(name.value.clone());
                        let name_const =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        let type_const =
                            self.make_constant(Value::String(rc_str(type_name.as_str())), line);
                        self.emit_op_u16(OpCode::DefineGlobalTyped, name_const, line);
                        self.emit_byte(1, line); // TypedDeclare is mutable
                        self.current_chunk().write_u16(type_const, line);
                    } else {
                        // Zero-init `x <int>` is a declaration, not a
                        // reassignment, so inside a block it introduces a fresh
                        // block-local (shadowing any typed global, or any local) —
                        // matching the tree-walker, which leaves the outer value
                        // untouched.
                        self.add_local(&name.value, true, Some(type_name));
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
                // Check immutability for locals at compile time
                if let Some(slot) = self.resolve_local(&name.value)
                    && !self.current_frame().locals[slot as usize].mutable {
                        self.error(
                            &format!(
                                "cannot reassign immutable variable '{}'. use := to override",
                                name.value
                            ),
                            line,
                        );
                    }
                // Implicit-self field WRITE: a bare-name assignment inside a
                // method whose name is NOT a local/param but IS a field of the
                // enclosing method's struct resolves to `self.field = value` —
                // mirroring the tree-walker and the implicit-self READ in
                // compile_identifier_inner. The read path resolves a bare name
                // as local -> field -> upvalue -> global (field BEFORE upvalue
                // and global, with NO global short-circuit), so the write must
                // use the SAME order: only a local/param of the same name
                // shadows the field (handled by the SetLocal branch below); a
                // same-named upvalue OR global does NOT win over the field.
                if self.resolve_local(&name.value).is_none() {
                    let is_field = self
                        .method_field_stack
                        .last()
                        .is_some_and(|f| f.contains(&name.value));
                    if is_field
                        && let Some(self_slot) = self.resolve_local("self") {
                            // self.field = value : GetLocal self, RHS, SetField.
                            // SetField pops both object and value (net -2), so
                            // no trailing Pop is needed (mirrors DotAssign).
                            self.emit_op_u16(OpCode::GetLocal, self_slot, line);
                            self.compile_expression(value);
                            let field_const = self.make_constant(
                                Value::String(rc_str(name.value.as_str())),
                                line,
                            );
                            self.emit_op_u16(OpCode::SetField, field_const, line);
                            return;
                        }
                }
                self.compile_expression(value);
                if let Some(slot) = self.resolve_local(&name.value) {
                    self.emit_op_u16(OpCode::SetLocal, slot, line);
                } else {
                    let frame_idx = self.frames.len() - 1;
                    if let Some(uv_idx) = self.resolve_upvalue(frame_idx, &name.value) {
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
                self.emit_op(OpCode::Pop, line); // pop condition (true path)

                // Compile consequence in its own scope
                self.begin_scope();
                for s in consequence {
                    self.compile_statement(s);
                }
                self.end_scope(line);

                // Always jump over the false-path Pop (and alternative if present)
                let else_jump = self.emit_jump(OpCode::Jump, line);
                self.patch_jump(then_jump);
                self.emit_op(OpCode::Pop, line); // pop condition (false path)

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
                // `skip` cleans body locals back to the pre-body height, then
                // re-checks the condition via the back-edge below.
                let continue_floor = self.current_frame().locals.len();
                self.current_frame_mut()
                    .loop_continue_floors
                    .push(continue_floor);
                // `repeat` has no loop variable, so `stop` exits to the same
                // pre-body height that `skip` continues to.
                self.current_frame_mut().loop_exit_floors.push(continue_floor);
                let handler_floor = self.current_frame().handler_depth;
                self.current_frame_mut()
                    .loop_handler_depths
                    .push(handler_floor);

                // Compile condition
                self.compile_expression(condition);
                let exit_jump = self.emit_jump(OpCode::JumpIfFalse, line);
                self.emit_op(OpCode::Pop, line); // pop condition

                // Compile body
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

                // Loop back
                self.emit_loop(loop_start, line);

                // Patch exit
                self.patch_jump(exit_jump);
                self.emit_op(OpCode::Pop, line); // pop condition

                // Patch any `stop` jumps
                let exits = self.current_frame_mut().loop_exits.pop().unwrap();
                for exit in exits {
                    self.patch_jump(exit);
                }
                self.current_frame_mut().loop_starts.pop();
            }

            Statement::Each {
                variable,
                iterable,
                body,
                ..
            } => {
                // `each i in range(a)` / `range(a, b)` with the *builtin* range
                // (not user-shadowed): lower to a counting loop. No array is
                // materialised, and with IterLen/IterGet gone the body is
                // JIT-eligible. `range` is always ascending, step 1, `a..b`.
                let range_args: Option<&[Expression]> = match iterable {
                    Expression::Call {
                        function,
                        args,
                        named_args,
                        ..
                    } if named_args.is_empty()
                        && (args.len() == 1 || args.len() == 2)
                        && matches!(&**function, Expression::Ident(id) if id.value == "range")
                        && !self.name_is_user_bound("range") =>
                    {
                        Some(args.as_slice())
                    }
                    _ => None,
                };

                self.begin_scope();

                // `iter_slot` holds the iterable (array path); unused for range.
                // `index_slot` is the counter; `end_slot` the exclusive bound.
                let (index_slot, iter_slot, end_slot) = if let Some(args) = range_args {
                    // counter = start (0 for 1-arg form), evaluated once
                    if args.len() == 1 {
                        self.emit_constant(Value::Integer(0), line);
                    } else {
                        self.compile_expression(&args[0]);
                    }
                    self.add_local("__index__", true, None);
                    // end = last arg, evaluated once
                    self.compile_expression(&args[args.len() - 1]);
                    self.add_local("__range_end__", false, None);
                    let i = self.resolve_local("__index__").unwrap();
                    let e = self.resolve_local("__range_end__").unwrap();
                    (i, 0, e)
                } else {
                    self.compile_expression(iterable);
                    self.add_local("__iterable__", false, None);
                    self.emit_constant(Value::Integer(0), line);
                    self.add_local("__index__", true, None);
                    let it = self.resolve_local("__iterable__").unwrap();
                    let i = self.resolve_local("__index__").unwrap();
                    (i, it, 0)
                };
                let is_range = range_args.is_some();

                let loop_start = self.current_chunk().len();
                self.current_frame_mut().loop_starts.push(loop_start);
                self.current_frame_mut().loop_exits.push(Vec::new());
                self.current_frame_mut().loop_continues.push(Vec::new());

                // Loop condition (leaves a bool both branches Pop).
                if is_range {
                    // index < end
                    self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                    self.emit_op_u16(OpCode::GetLocal, end_slot, line);
                    self.emit_op(OpCode::Less, line);
                } else {
                    // len(iterable) > index
                    self.emit_op_u16(OpCode::GetLocal, iter_slot, line);
                    self.emit_op(OpCode::IterLen, line);
                    self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                    self.emit_op(OpCode::Greater, line);
                }
                let exit_jump = self.emit_jump(OpCode::JumpIfFalse, line);
                self.emit_op(OpCode::Pop, line);

                // Push the loop value: the counter (range) or iterable[index].
                if is_range {
                    self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                } else {
                    self.emit_op_u16(OpCode::GetLocal, iter_slot, line);
                    self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                    self.emit_op(OpCode::IterGet, line);
                }

                // `stop` exits to the loop's outer scope, which does NOT include
                // the loop variable (the exit path never bound it that
                // iteration), so its floor is the height before binding it.
                let exit_floor = self.current_frame().locals.len();
                self.current_frame_mut().loop_exit_floors.push(exit_floor);
                // Bind to loop variable
                self.add_local(&variable.value, false, None);
                // `skip` cleans body locals back to here (keeping the loop
                // variable, which the shared pop below closes/pops), then jumps
                // to the continue target (the index increment).
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
                // Mark that we're inside an `each` body: the tree-walker runs each
                // iteration in a fresh enclosed env, so a typed re-declaration of an
                // untyped outer global must SHADOW here (not update the global).
                self.current_frame_mut().each_body_depth += 1;
                for s in body {
                    self.compile_statement(s);
                }
                self.current_frame_mut().each_body_depth -= 1;
                self.end_scope(line);

                // Continue target: `skip` lands here, with the loop variable
                // still on top, then runs the shared pop + increment below.
                let continues = self.current_frame_mut().loop_continues.pop().unwrap();
                for c in continues {
                    self.patch_jump(c);
                }
                self.current_frame_mut().loop_continue_floors.pop();
                self.current_frame_mut().loop_exit_floors.pop();
                self.current_frame_mut().loop_handler_depths.pop();

                // Pop (or close, when captured by a closure) the loop variable.
                // Closing per iteration gives each closure its own value.
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

                // Increment index
                self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                self.emit_constant(Value::Integer(1), line);
                self.emit_op(OpCode::Add, line);
                self.emit_op_u16(OpCode::SetLocal, index_slot, line);
                self.emit_op(OpCode::Pop, line);

                // Loop back
                self.emit_loop(loop_start, line);

                // Patch exit
                self.patch_jump(exit_jump);
                self.emit_op(OpCode::Pop, line);

                // Patch stop jumps
                let exits = self.current_frame_mut().loop_exits.pop().unwrap();
                for exit in exits {
                    self.patch_jump(exit);
                }
                self.current_frame_mut().loop_starts.pop();

                self.end_scope(line);
            }

            Statement::Skip => {
                // Continue: clean up every local above the innermost loop's
                // continue floor (without removing them from compile-time scope,
                // since code after the `skip` still references them), then jump
                // to that loop's continue target.
                if self.current_frame().loop_continue_floors.is_empty() {
                    self.error("'skip' used outside of loop", line);
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

            Statement::Stop => {
                // Break: clean up every local above the loop's exit floor
                // (loop variable + body locals — without removing them from
                // compile-time scope), then jump to the loop's exit target.
                // Leaving them on the operand stack would corrupt an enclosing
                // loop's iterator.
                if self.current_frame().loop_exit_floors.is_empty() {
                    self.error("'stop' used outside of loop", line);
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
                if let Some(exprs) = values {
                    // Multi-expression: a, b := expr1, expr2
                    for (name, expr) in names.iter().zip(exprs.iter()) {
                        self.compile_expression(expr);
                        self.compile_unpack_bind(name, *reassign, line);
                    }
                } else {
                    // Single-expression unpack: a, b := tuple_or_array
                    self.compile_expression(value);
                    self.emit_op_u8(OpCode::Unpack, names.len() as u8, line);
                    if *reassign {
                        // Reassign pops from top of stack, so iterate in reverse
                        // so that name[last] gets element[last] (top), etc.
                        for name in names.iter().rev() {
                            self.compile_unpack_bind(name, true, line);
                        }
                    } else if self.current_frame().scope_depth > 0 {
                        // Local define path: add_local assigns sequential stack slots
                        // matching the Unpack push order, so iterate forward. Use
                        // add_local even for "_" to occupy the slot correctly.
                        for name in names {
                            self.compile_unpack_define(name, line);
                        }
                    } else {
                        // Global define path: DefineGlobal pops from the top of the
                        // stack and Unpack pushes elements in order (last element on
                        // top), so bind the last name first to preserve positional
                        // order (a, b, c := [1, 2, 3] => a=1, b=2, c=3).
                        for name in names.iter().rev() {
                            self.compile_unpack_define(name, line);
                        }
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
                self.compile_expression(value);
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
                self.compile_expression(index);
                self.compile_expression(value);
                self.emit_op(OpCode::IndexAssign, line);
            }

            Statement::Main { body, .. } => {
                let skip_jump = self.emit_jump(OpCode::Main, line);
                for s in body {
                    self.compile_statement(s);
                }
                self.patch_jump(skip_jump);
            }

            // `<test>` blocks are only executed by `oxigen test` (via the
            // tree-walking runner). Under normal compilation/`run`, skip them.
            Statement::Test { .. } => {}

            Statement::StructDef {
                name,
                parent,
                fields,
                ..
            } => {
                // Compile struct definition
                let field_info: Vec<(String, String, bool)> = fields
                    .iter()
                    .map(|f| (f.name.value.clone(), f.type_ann.type_name(), f.hidden))
                    .collect();

                // Record field names + parent so method bodies can resolve a
                // bare identifier to `self.field` (implicit self). Hidden
                // fields are included — they're accessible within methods.
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
                        module_globals: std::cell::RefCell::new(None),
                    }));
                let const_idx = self.make_constant(struct_def, line);
                self.emit_op_u16(OpCode::Constant, const_idx, line);

                if self.current_frame().scope_depth > 0 {
                    self.add_local(&name.value, false, None);
                } else {
                    let name_const =
                        self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                    self.emit_op_u16(OpCode::DefineGlobal, name_const, line);
                }
            }

            Statement::EnumDef { name, variants, .. } => {
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
                            // Simple literal folding for discriminants. Non-literal
                            // expressions produce a variant with no value.
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
                    resolved.push(VmEnumVariantDef {
                        name: variant.name.value.clone(),
                        kind,
                    });
                }
                let enum_val = Value::EnumDef(std::rc::Rc::new(ObjEnumDef {
                    name: name.value.clone(),
                    variants: resolved,
                }));
                let const_idx = self.make_constant(enum_val, line);
                self.emit_op_u16(OpCode::Constant, const_idx, line);
                if self.current_frame().scope_depth > 0 {
                    self.add_local(&name.value, false, None);
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
                // Make the struct's fields resolvable as implicit-self while
                // compiling the method bodies.
                let self_fields = self.collect_self_fields(&struct_name.value);
                self.method_field_stack.push(self_fields);
                // Compile each method with an implicit `self` first parameter
                for (method_name, method_expr) in methods {
                    if let Expression::FunctionLiteral {
                        parameters, body, ..
                    } = method_expr
                    {
                        // Add `self` as implicit first parameter
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
                    // Push method name string
                    self.emit_constant(Value::String(rc_str(method_name.value.as_str())), line);
                }
                self.method_field_stack.pop();
                // Emit DefineMethod opcode
                let struct_const =
                    self.make_constant(Value::String(rc_str(struct_name.value.as_str())), line);
                self.emit_op_u16(OpCode::DefineMethod, struct_const, line);
                self.emit_byte(methods.len() as u8, line);
            }

            Statement::Pattern {
                name,
                params,
                condition,
                ..
            } => {
                // Compile the condition as a closure, store as __pattern_<name> global
                let param_names: Vec<String> = params.iter().map(|p| p.value.clone()).collect();
                self.compile_pattern_function(&param_names, condition, line);
                let pattern_global = format!("__pattern_{}", name.value);
                let name_const =
                    self.make_constant(Value::String(rc_str(pattern_global.as_str())), line);
                self.emit_op_u16(OpCode::DefineGlobal, name_const, line);
            }

            Statement::Choose { subject, arms, .. } => {
                // A `choose` statement's value is consumed only when an
                // enclosing block needs it (`suppress_statement_pop`); otherwise
                // it is discarded, so a `skip`/`stop` arm tail is legit control
                // flow. Capture before compiling the subject (which clears it).
                let choose_consumed = self.suppress_statement_pop;
                // Store subject as a temporary global so pattern functions can access it
                // without affecting the local stack layout
                self.compile_expression(subject);
                let temp_name = "__choose_tmp__";
                let temp_const = self.make_constant(Value::String(rc_str(temp_name)), line);
                self.emit_op_u16(OpCode::DefineGlobal, temp_const, line);

                let mut end_jumps = Vec::new();
                let mut has_else = false;

                for arm in arms {
                    if arm.pattern_name == "else" {
                        has_else = true;
                        self.compile_block_as_expression(&arm.body, line, choose_consumed);
                        break;
                    }

                    // Register inline pattern if present
                    if let (Some(params), Some(condition)) =
                        (&arm.inline_params, &arm.inline_condition)
                    {
                        let param_names: Vec<String> =
                            params.iter().map(|p| p.value.clone()).collect();
                        self.compile_pattern_function(&param_names, condition, line);
                        let pattern_global = format!("__pattern_{}", arm.pattern_name);
                        let nc = self
                            .make_constant(Value::String(rc_str(pattern_global.as_str())), line);
                        self.emit_op_u16(OpCode::DefineGlobal, nc, line);
                    }

                    // Call pattern function with subject
                    let pattern_global = format!("__pattern_{}", arm.pattern_name);
                    let pg_const =
                        self.make_constant(Value::String(rc_str(pattern_global.as_str())), line);
                    self.emit_op_u16(OpCode::GetGlobal, pg_const, line);
                    let subj_const = self.make_constant(Value::String(rc_str(temp_name)), line);
                    self.emit_op_u16(OpCode::GetGlobal, subj_const, line);
                    self.emit_op_u8(OpCode::Call, 1, line);

                    // Check result
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

                // Choose is a statement — discard the result value (unless suppressed)
                if !self.suppress_statement_pop {
                    self.emit_op(OpCode::Pop, line);
                }
            }

            Statement::Introduce {
                path, selective, ..
            } => {
                // Push the module path as a string
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

                if let Some(names) = selective {
                    // Selective import: push the names
                    for name in names {
                        let nc =
                            self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                        self.emit_op_u16(OpCode::Constant, nc, line);
                    }
                    self.emit_op_u16(OpCode::Import, path_const, line);
                    self.emit_byte(names.len() as u8, line);
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
        // Capture whether THIS expression's value is consumed, then default
        // nested sub-expressions to consumed=true: any operand/argument/RHS a
        // node recurses into has its value used by the current operation. Only
        // a transparent wrapper (`Grouped`) and `option`/`choose` forward the
        // captured context; everything else descends as consumed.
        let consumed = self.value_consumed;
        self.value_consumed = true;

        // Fold pure-literal arithmetic/comparison/bitwise subtrees to a single
        // constant (e.g. `60*60*24`, `2 < 3`). Leaves keep their existing fast
        // paths (True/False/None opcodes), so only fold compound nodes.
        if matches!(expr, Expression::Prefix { .. } | Expression::Infix { .. })
            && let Some(folded) = fold_constant_expression(expr) {
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
                // A parenthesised group is transparent: it neither consumes nor
                // discards its inner value, so forward the captured context so a
                // discarded `(option{... -> {stop}})` stays legitimate.
                self.value_consumed = consumed;
                self.compile_expression(inner);
            }

            // diverge/converge lower to __spawn/__join_task. The lowered call's
            // value IS this node's value, so forward the captured context the
            // same way Grouped does.
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
                    _ => self.error(&format!("unknown prefix operator: {}", operator), line),
                }
            }

            Expression::Infix {
                operator,
                left,
                right,
                ..
            } => {
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
                self.compile_expression(right);
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
                    _ => self.error(&format!("unknown infix operator: {}", operator), line),
                }
            }

            Expression::Postfix { operator, left, .. } => {
                // Postfix requires an identifier
                if let Expression::Ident(ident) = left.as_ref() {
                    // Check immutability
                    if let Some(slot) = self.resolve_local(&ident.value)
                        && !self.current_frame().locals[slot as usize].mutable {
                            self.error(
                                &format!(
                                    "cannot mutate immutable variable '{}' with {}",
                                    ident.value, operator
                                ),
                                line,
                            );
                        }
                    // Load current value (return value is the pre-increment value)
                    self.compile_identifier(ident);
                    // Duplicate for the return value
                    self.emit_op(OpCode::Dup, line);
                    // Push 1
                    self.emit_constant(Value::Integer(1), line);
                    // Add or subtract
                    match operator.as_str() {
                        "++" => self.emit_op(OpCode::Add, line),
                        "--" => self.emit_op(OpCode::Subtract, line),
                        _ => self.error(&format!("unknown postfix operator: {}", operator), line),
                    }
                    // Store back
                    if let Some(slot) = self.resolve_local(&ident.value) {
                        self.emit_op_u16(OpCode::SetLocal, slot, line);
                    } else {
                        let frame_idx = self.frames.len() - 1;
                        if let Some(uv_idx) = self.resolve_upvalue(frame_idx, &ident.value) {
                            self.emit_op_u16(OpCode::SetUpvalue, uv_idx, line);
                        } else {
                            let name_const = self
                                .make_constant(Value::String(rc_str(ident.value.as_str())), line);
                            self.emit_op_u16(OpCode::SetGlobal, name_const, line);
                        }
                    }
                    self.emit_op(OpCode::Pop, line); // pop the stored (new) value, leaving pre-inc
                } else {
                    self.error("postfix operator requires identifier", line);
                }
            }

            Expression::Ident(ident) => {
                self.compile_identifier(ident);
            }

            Expression::Array { elements, .. } => {
                for elem in elements {
                    self.compile_expression(elem);
                }
                let count = elements.len() as u16;
                self.emit_op_u16(OpCode::BuildArray, count, line);
            }

            Expression::TupleLiteral { elements, .. } => {
                for elem in elements {
                    self.compile_expression(elem);
                }
                let count = elements.len() as u16;
                self.emit_op_u16(OpCode::BuildTuple, count, line);
            }

            Expression::MapLiteral { entries, .. } => {
                for (key, val) in entries {
                    self.compile_expression(key);
                    self.compile_expression(val);
                }
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
                }
                let count = parts.len() as u16;
                self.emit_op_u16(OpCode::StringInterp, count, line);
            }

            Expression::Call {
                function,
                args,
                named_args,
                ..
            } => {
                // Detect special builtins that need AST-level access
                if let Expression::Ident(ident) = function.as_ref() {
                    match ident.value.as_str() {
                        "is_mut" => {
                            self.compile_is_mut(args, line);
                            return;
                        }
                        "is_type" => {
                            self.compile_is_type(args, line);
                            return;
                        }
                        "is_type_mut" => {
                            self.compile_is_type_mut(args, line);
                            return;
                        }
                        _ => {}
                    }
                }

                // Method call: obj.method(args) → MethodCall opcode
                if let Expression::DotAccess { left, field, .. } = function.as_ref() {
                    self.compile_expression(left);
                    for arg in args {
                        self.compile_expression(arg);
                    }
                    let method_const =
                        self.make_constant(Value::String(rc_str(field.value.as_str())), line);
                    if named_args.is_empty() {
                        self.emit_op_u16(OpCode::MethodCall, method_const, line);
                        self.emit_byte(args.len() as u8, line);
                    } else {
                        for (name, val_expr) in named_args {
                            self.emit_constant(Value::String(rc_str(name.as_str())), line);
                            self.compile_expression(val_expr);
                        }
                        self.emit_op_u16(OpCode::MethodCallNamed, method_const, line);
                        self.emit_byte(args.len() as u8, line);
                        self.emit_byte(named_args.len() as u8, line);
                    }
                    return;
                }

                // Normal function call
                self.compile_expression(function);
                for arg in args {
                    self.compile_expression(arg);
                }

                if named_args.is_empty() {
                    self.emit_op_u8(OpCode::Call, args.len() as u8, line);
                } else {
                    // Push named args: name string, then value
                    for (name, val_expr) in named_args {
                        self.emit_constant(Value::String(rc_str(name.as_str())), line);
                        self.compile_expression(val_expr);
                    }
                    self.emit_op(OpCode::CallNamed, line);
                    self.emit_byte(args.len() as u8, line);
                    self.emit_byte(named_args.len() as u8, line);
                }
            }

            Expression::Index { left, index, .. } => {
                self.compile_expression(left);
                self.compile_expression(index);
                self.emit_op(OpCode::Index, line);
            }

            Expression::Slice {
                left, start, end, ..
            } => {
                self.compile_expression(left);
                let mut flags: u8 = 0;
                if let Some(s) = start {
                    self.compile_expression(s);
                    flags |= 0x01;
                }
                if let Some(e) = end {
                    self.compile_expression(e);
                    flags |= 0x02;
                }
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
                // Push struct name
                let name_const =
                    self.make_constant(Value::String(rc_str(struct_name.as_str())), line);
                // Push field name-value pairs
                for (fname, fval) in field_values {
                    self.emit_constant(Value::String(rc_str(fname.as_str())), line);
                    self.compile_expression(fval);
                }
                self.emit_op_u16(OpCode::StructLiteral, name_const, line);
                self.emit_byte(field_values.len() as u8, line);
            }

            Expression::EnumVariantConstruct {
                enum_name,
                variant_name,
                kind,
                ..
            } => {
                // Push the EnumDef (looked up by name) then the payload pieces,
                // then emit the Make opcode.
                self.compile_expression(&Expression::Ident(Identifier {
                    token: crate::token::Token {
                        token_type: crate::token::TokenType::Ident,
                        literal: enum_name.clone(),
                        span: crate::token::Span::new(line as usize, 0),
                    },
                    value: enum_name.clone(),
                }));
                let variant_const =
                    self.make_constant(Value::String(rc_str(variant_name.as_str())), line);
                match kind {
                    EnumConstructKind::Tuple(args) => {
                        for arg in args {
                            self.compile_expression(arg);
                        }
                        self.emit_op_u16(OpCode::MakeEnumVariantTuple, variant_const, line);
                        self.emit_byte(args.len() as u8, line);
                    }
                    EnumConstructKind::Struct(fields) => {
                        for (fname, fval) in fields {
                            self.emit_constant(Value::String(rc_str(fname.as_str())), line);
                            self.compile_expression(fval);
                        }
                        self.emit_op_u16(OpCode::MakeEnumVariantStruct, variant_const, line);
                        self.emit_byte(fields.len() as u8, line);
                    }
                }
            }

            Expression::Unless {
                consequence,
                condition,
                alternative,
                ..
            } => {
                // unless is: consequence unless condition then alternative
                // If condition is true, use alternative; else use consequence
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

                // An `<Error>` arm runs only when evaluating an arm (condition,
                // body, or default) raises a runtime error — NOT on no-match.
                // Install a handler covering the arm evaluation; its catch target
                // is the error body emitted after the normal path.
                let err_handler = if error_default.is_some() {
                    Some(self.emit_push_handler(line))
                } else {
                    None
                };

                for arm in arms {
                    self.compile_expression(&arm.condition);
                    let skip = self.emit_jump(OpCode::JumpIfFalse, line);
                    self.emit_op(OpCode::Pop, line);
                    // Condition true → compile body (last expr is the value).
                    // `consumed` (captured before the reset above) says whether
                    // this whole `option` value is consumed by an enclosing
                    // operation; a `skip`/`stop` tail is only rejected then.
                    self.compile_block_as_expression(&arm.body, line, consumed);
                    let end = self.emit_jump(OpCode::Jump, line);
                    end_jumps.push(end);
                    self.patch_jump(skip);
                    self.emit_op(OpCode::Pop, line);
                }

                // No-match default (the `<Error>` arm is never the no-match
                // default — it only fires on error, handled below).
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
                    // Catch target: the handler unwinds here with the error value
                    // on top. Discard it, then run the `<Error>` body.
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
                // Install a handler so a runtime error / `<fail>` raised while
                // evaluating `value` lands at the Guard opcode as an in-band
                // error value, rather than unwinding past it.
                let handler = self.emit_push_handler(line);
                self.compile_expression(value);
                self.emit_pop_handler(line);
                self.patch_jump(handler); // catch target = the Guard opcode below

                let binding_const =
                    self.make_constant(Value::String(rc_str(binding.value.as_str())), line);
                // Tag filter (`<guard<Error<tag>>>`): 0xFFFF = no filter (catch
                // any error); otherwise the constant index of the tag string.
                let tag_const = match error_tag {
                    Some(t) => self.make_constant(Value::String(rc_str(t.as_str())), line),
                    None => 0xFFFF,
                };

                // Emit Guard opcode with jump offset placeholder
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

                // Patch guard jump: VM IP is after all 6 operand bytes
                // (2 jump + 2 binding + 2 tag), so offset = fallback_start - (pos + 6)
                let fallback_start = self.current_chunk().len();
                let offset = fallback_start - guard_jump_pos - 6;
                self.current_chunk()
                    .patch_u16(guard_jump_pos, offset as u16);

                // Error case: compile fallback expression
                self.compile_expression(fallback);
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
                // `<type<Error || Value>>(expr)` normalizes a failure into an
                // Error value instead of propagating: install a handler whose
                // catch target is the TypeWrap opcode (which turns the in-band
                // error value into the Error side of the union). Plain casts
                // (e.g. `<int>(...)`) get no handler — their errors propagate.
                let is_error_union = type_str.contains(" || ") && {
                    let parts: Vec<&str> = type_str.split(" || ").collect();
                    parts.contains(&"VALUE")
                        && parts.iter().any(|p| p.starts_with("ERROR"))
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
            Statement::Skip | Statement::Stop if consumed => {
                let kw = if matches!(stmt, Statement::Skip) { "skip" } else { "stop" };
                self.error(
                    &format!("'{kw}' cannot be used as a value"),
                    line,
                );
            }
            Statement::Expr(expr) => {
                // Propagate the consumed/discarded context into the inner
                // expression. Without this, a nested option/choose whose tail is
                // `skip`/`stop` captures the stale `value_consumed = true` and is
                // wrongly rejected even when the OUTER construct is discarded
                // (legitimate loop control flow), e.g.
                //   each i in range(2){ choose i { else -> { option { True -> { skip } } } } }
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
                alternative: Some(_),
                ..
            } => {
                // If/else with both branches produces a value.
                // One-armed if (guard) does not.
                self.suppress_statement_pop = true;
                self.compile_statement(stmt);
                self.suppress_statement_pop = false;
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
            self.add_local(&name.value, true, None);
        } else {
            if name.value == "_" {
                self.emit_op(OpCode::Pop, line);
            } else {
                let name_const =
                    self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                self.emit_op_u16(OpCode::DefineGlobal, name_const, line);
            }
        }
    }

    fn compile_unpack_bind(&mut self, name: &crate::ast::Identifier, reassign: bool, line: u32) {
        if name.value == "_" {
            self.emit_op(OpCode::Pop, line);
        } else if reassign {
            // Reassignment: set existing variable
            if let Some(slot) = self.resolve_local(&name.value) {
                self.emit_op_u16(OpCode::SetLocal, slot, line);
                self.emit_op(OpCode::Pop, line);
            } else {
                let frame_idx = self.frames.len() - 1;
                if let Some(uv_idx) = self.resolve_upvalue(frame_idx, &name.value) {
                    self.emit_op_u16(OpCode::SetUpvalue, uv_idx, line);
                    self.emit_op(OpCode::Pop, line);
                } else {
                    let name_const =
                        self.make_constant(Value::String(rc_str(name.value.as_str())), line);
                    self.emit_op_u16(OpCode::SetGlobal, name_const, line);
                    self.emit_op(OpCode::Pop, line);
                }
            }
        } else if self.current_frame().scope_depth > 0 {
            self.add_local(&name.value, true, None);
        } else {
            let name_const = self.make_constant(Value::String(rc_str(name.value.as_str())), line);
            self.emit_op_u16(OpCode::DefineGlobal, name_const, line);
        }
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
        // Wrap in a scope so locals are cleaned up.
        self.begin_scope();
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            if is_last {
                // For the last statement, we need its value on the stack.
                // Compile within the scope so the expression can reference
                // block locals, then clean up.
                match stmt {
                    Statement::Expr(_) => {
                        self.compile_last_statement_as_value(stmt, line, consumed);
                        self.end_scope_keeping_value(line);
                    }
                    Statement::Skip | Statement::Stop if consumed => {
                        // A `skip`/`stop` whose value is consumed by an enclosing
                        // expression (e.g. `1 + option{... -> {skip}}`) is the
                        // same error the tree-walker raises on `INTEGER + SKIP`.
                        self.compile_last_statement_as_value(stmt, line, consumed);
                        self.end_scope_keeping_value(line);
                    }
                    _ => {
                        self.compile_statement(stmt);
                        self.end_scope(line);
                        self.emit_op(OpCode::None, line);
                    }
                }
            } else if consumed && matches!(stmt, Statement::Skip | Statement::Stop) {
                // A NON-tail bare `skip`/`stop` in a block whose value is CONSUMED
                // makes the block's value ill-defined: the `skip`/`stop` jumps
                // before the tail value is produced, so the enclosing expression
                // never receives a value. Reject it like the tail case (the
                // tree-walker surfaces the same error via the eval_program gate).
                // Discarded blocks (`consumed == false`) keep legitimate loop
                // control flow, and a `skip`/`stop` nested inside an inner loop is
                // a Statement::Each/Repeat here, not a bare Skip/Stop, so it is
                // unaffected.
                let kw = if matches!(stmt, Statement::Skip) { "skip" } else { "stop" };
                self.error(&format!("'{kw}' cannot be used as a value"), line);
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
        // Leaf: compile both sides and emit comparison
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
            _ => self.error(&format!("unknown comparison operator: {}", op), line),
        }
    }

    /// Compile `is_mut(x)` — check if variable is mutable.
    fn compile_is_mut(&mut self, args: &[Expression], line: u32) {
        if args.len() != 1 {
            self.error("is_mut() takes exactly 1 argument", line);
            self.emit_op(OpCode::False, line);
            return;
        }
        if let Expression::Ident(ident) = &args[0] {
            // Check if it's a local — resolve at compile time
            if let Some(slot) = self.resolve_local(&ident.value) {
                let mutable = self.current_frame().locals[slot as usize].mutable;
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
            self.error("argument to is_mut must be a variable name", line);
            self.emit_op(OpCode::False, line);
        }
    }

    /// Compile `is_type(x, int)` — check if value matches type.
    fn compile_is_type(&mut self, args: &[Expression], line: u32) {
        if args.len() != 2 {
            self.error("is_type() takes exactly 2 arguments", line);
            self.emit_op(OpCode::False, line);
            return;
        }
        // Evaluate the first argument (the value)
        self.compile_expression(&args[0]);
        // Second argument should be a type name identifier
        if let Expression::Ident(type_ident) = &args[1] {
            let type_const =
                self.make_constant(Value::String(rc_str(type_ident.value.as_str())), line);
            self.emit_op_u16(OpCode::IsType, type_const, line);
        } else {
            self.error("second argument to is_type must be a type name", line);
            self.emit_op(OpCode::Pop, line);
            self.emit_op(OpCode::False, line);
        }
    }

    /// Compile `is_type_mut(x)` — check if variable has no type constraint.
    fn compile_is_type_mut(&mut self, args: &[Expression], line: u32) {
        if args.len() != 1 {
            self.error("is_type_mut() takes exactly 1 argument", line);
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
            self.error("argument to is_type_mut must be a variable name", line);
            self.emit_op(OpCode::False, line);
        }
    }

    fn compile_identifier(&mut self, ident: &Identifier) {
        let line = ident.token.span.line as u32;
        let col = ident.token.span.column as u32;
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
        // Implicit self: a bare identifier matching a field of the enclosing
        // method's struct resolves to `self.field`, mirroring the interpreter.
        // Gated on `self` being a local of the current frame, so nested
        // closures (where `self` is not a direct local) fall through instead.
        if self
            .method_field_stack
            .last()
            .is_some_and(|f| f.contains(&ident.value))
            && let Some(self_slot) = self.resolve_local("self") {
                self.emit_op_u16(OpCode::GetLocal, self_slot, line);
                let field_const =
                    self.make_constant(Value::String(rc_str(ident.value.as_str())), line);
                self.emit_op_u16(OpCode::GetField, field_const, line);
                return;
            }
        // Try upvalue
        let frame_idx = self.frames.len() - 1;
        if let Some(uv_idx) = self.resolve_upvalue(frame_idx, &ident.value) {
            self.emit_op_u16(OpCode::GetUpvalue, uv_idx, line);
            return;
        }
        // Fall back to global
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
        self.frames
            .push(CompilerFrame::new(name.map(|s| s.to_string()), 1));
        self.current_frame_mut().function.arity = parameters.len() as u8;

        // Compile parameter info
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

        // Add parameters as locals
        for param in parameters {
            self.add_local(
                &param.ident.value,
                true,
                param.type_ann.as_ref().map(|t| t.type_name()),
            );
        }

        // Compile default parameter initialization
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
                    // A function's tail value is returned to (consumed by) the
                    // caller, so a bare `skip`/`stop` tail is a value error.
                    self.compile_last_statement_as_value(stmt, line, true);
                    self.emit_op(OpCode::Return, line);
                } else {
                    self.compile_statement(stmt);
                }
            }
        }

        // Pop this frame and get the function + upvalues
        let frame = self.frames.pop().unwrap();
        let mut function = frame.function;
        function.id = self.fn_counter;
        self.fn_counter += 1;
        let upvalues = frame.upvalues;

        // Add the function as a constant in the enclosing scope
        let (uv_kinds, uv_values) = crate::vm::value::make_upvalue_int_caches(0);
        let func_const = self.make_constant(
            Value::Closure(std::rc::Rc::new(crate::vm::value::ObjClosure {
                function: std::rc::Rc::new(function),
                upvalues: Vec::new(), // placeholder, VM fills real upvalues
                module_globals: std::cell::RefCell::new(None),
                call_count: std::cell::Cell::new(0),
                loop_count: std::cell::Cell::new(0),
                jit_state: std::cell::Cell::new(0),
                jit_bailouts: std::cell::Cell::new(0),
                jit_thunk: std::cell::Cell::new(None),
                specialized_thunk: std::cell::Cell::new(None),
                specialized_arity: std::cell::Cell::new(0),
                specialized_kind: std::cell::Cell::new(0),
                upvalue_int_kinds: uv_kinds,
                upvalue_int_values: uv_values,
            })),
            line,
        );

        // Emit Closure opcode
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
        // Create a mini function for the pattern condition
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

        // Wrap in a give statement
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
        "ARRAY" => Value::Array(std::rc::Rc::new(std::cell::RefCell::new(Vec::new()))),
        "TUPLE" => Value::Tuple(std::rc::Rc::new(Vec::new())),
        "MAP" => Value::Map(std::rc::Rc::new(std::cell::RefCell::new(
            crate::vm::collections::OxMap::new(),
        ))),
        "SET" => Value::Set(std::rc::Rc::new(std::cell::RefCell::new(
            crate::vm::collections::OxSet::new(),
        ))),
        _ => Value::None,
    }
}
