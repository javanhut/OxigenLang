pub mod opcode;

use crate::ast::*;
use crate::vm::value::{
    Function, LocalInfo, ObjEnumDef, ParamInfo, Value, VmEnumVariantDef, VmEnumVariantKind,
};
use opcode::{Chunk, OpCode};

/// Unwrap nested `Grouped(...)` wrappers to get the inner expression.
fn unwrap_grouped(expr: &Expression) -> &Expression {
    match expr {
        Expression::Grouped(inner) => unwrap_grouped(inner),
        other => other,
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
    /// Start of the current innermost loop (for `skip`/continue).
    loop_starts: Vec<usize>,
    /// Pending `stop`/break jump offsets to patch.
    loop_exits: Vec<Vec<usize>>,
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
}

impl Compiler {
    pub fn new() -> Self {
        let mut compiler = Compiler {
            frames: Vec::new(),
            errors: Vec::new(),
            dup_next_define: false,
            suppress_statement_pop: false,
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
        Ok(frame.function)
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
            | Statement::ContainsDef { token, .. }
            | Statement::DotAssign { token, .. }
            | Statement::Introduce { token, .. }
            | Statement::IndexAssign { token, .. }
            | Statement::Main { token, .. } => token.span.line as u32,
            Statement::TypedLet { name, .. }
            | Statement::TypedDeclare { name, .. }
            | Statement::Assign { name, .. } => name.token.span.line as u32,
            Statement::Unpack { names, .. } => {
                names.first().map_or(0, |n| n.token.span.line as u32)
            }
            Statement::Skip | Statement::Stop => 0,
        }
    }

    // ── Statement Compilation ──────────────────────────────────────────

    fn compile_statement(&mut self, stmt: &Statement) {
        let line = Self::stmt_line(stmt);
        match stmt {
            Statement::Expr(expr) => {
                self.compile_expression(expr);
                self.emit_op(OpCode::Pop, line);
            }

            Statement::Let { name, value } => {
                // If the value is a function literal, pass the name so the
                // closure carries it (instead of showing as "anonymous").
                if let Expression::FunctionLiteral {
                    parameters, body, ..
                } = value
                {
                    self.compile_function(Some(&name.value), parameters, body, line);
                } else {
                    self.compile_expression(value);
                }
                if self.dup_next_define {
                    self.emit_op(OpCode::Dup, line);
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
                    } else if self.current_frame().scope_depth > 0 {
                        // New local variable
                        self.add_local(&name.value, true, None);
                    } else {
                        // Global — DefineGlobal overwrites
                        let name_const =
                            self.make_constant(Value::String(name.value.as_str().into()), line);
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
                self.compile_expression(value);
                let type_name = type_ann.type_name();
                let mutable = *walrus; // walrus (:=) means mutable
                // Walrus := converts the value to the target type.
                // Non-walrus = does strict type checking (no conversion).
                if !matches!(type_ann, TypeAnnotation::Generic | TypeAnnotation::NoneType) {
                    let tc = self.make_constant(Value::String(type_name.as_str().into()), line);
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
                    } else {
                        self.add_local(&name.value, mutable, Some(type_name));
                    }
                } else {
                    let name_const =
                        self.make_constant(Value::String(name.value.as_str().into()), line);
                    let type_const =
                        self.make_constant(Value::String(type_name.as_str().into()), line);
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
                        self.make_constant(Value::String(struct_name.as_str().into()), line);
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
                    } else {
                        self.add_local(&name.value, true, Some(type_name));
                    }
                } else {
                    let name_const =
                        self.make_constant(Value::String(name.value.as_str().into()), line);
                    let type_const =
                        self.make_constant(Value::String(type_name.as_str().into()), line);
                    self.emit_op_u16(OpCode::DefineGlobalTyped, name_const, line);
                    self.emit_byte(1, line); // TypedDeclare is mutable
                    self.current_chunk().write_u16(type_const, line);
                }
            }

            Statement::Assign { name, value } => {
                // Check immutability for locals at compile time
                if let Some(slot) = self.resolve_local(&name.value) {
                    if !self.current_frame().locals[slot as usize].mutable {
                        self.error(
                            &format!(
                                "cannot reassign immutable variable '{}'. use := to override",
                                name.value
                            ),
                            line,
                        );
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
                            self.make_constant(Value::String(name.value.as_str().into()), line);
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
                // Compile iterable, store it in a local
                self.compile_expression(iterable);
                self.begin_scope();

                // Local for the iterable itself
                self.add_local("__iterable__", false, None);

                // Local for the index counter
                self.emit_constant(Value::Integer(0), line);
                self.add_local("__index__", true, None);

                // Get iterable length
                let iterable_slot = self.resolve_local("__iterable__").unwrap();
                let index_slot = self.resolve_local("__index__").unwrap();

                let loop_start = self.current_chunk().len();
                self.current_frame_mut().loop_starts.push(loop_start);
                self.current_frame_mut().loop_exits.push(Vec::new());

                // Check index < len: push iterable, push IterLen, push index, compare
                self.emit_op_u16(OpCode::GetLocal, iterable_slot, line);
                self.emit_op(OpCode::IterLen, line);
                self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                self.emit_op(OpCode::Greater, line); // len > index
                let exit_jump = self.emit_jump(OpCode::JumpIfFalse, line);
                self.emit_op(OpCode::Pop, line);

                // Get element: iterable[index]
                self.emit_op_u16(OpCode::GetLocal, iterable_slot, line);
                self.emit_op_u16(OpCode::GetLocal, index_slot, line);
                self.emit_op(OpCode::IterGet, line);

                // Bind to loop variable
                self.add_local(&variable.value, false, None);

                // Compile body in its own scope (so TypedLet locals are cleaned up per iteration)
                self.begin_scope();
                for s in body {
                    self.compile_statement(s);
                }
                self.end_scope(line);

                // Pop the loop variable
                self.current_frame_mut().locals.pop();
                self.emit_op(OpCode::Pop, line);

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
                // Jump to loop start (continue)
                if let Some(&loop_start) = self.current_frame().loop_starts.last() {
                    // Clean up locals in loop body scope before jumping
                    self.emit_loop(loop_start, line);
                } else {
                    self.error("'skip' used outside of loop", line);
                }
            }

            Statement::Stop => {
                // Jump to loop exit (break)
                if self.current_frame().loop_exits.is_empty() {
                    self.error("'stop' used outside of loop", line);
                } else {
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
                    } else {
                        // Define path: add_local assigns sequential stack slots,
                        // so iterate forward. Use add_local even for "_" to
                        // occupy the slot correctly without shifting the stack.
                        for name in names {
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
                    self.make_constant(Value::String(field.value.as_str().into()), line);
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

                let struct_def =
                    Value::StructDef(std::rc::Rc::new(crate::vm::value::ObjStructDef {
                        name: name.value.clone(),
                        fields: field_info,
                        methods: std::cell::RefCell::new(std::collections::HashMap::new()),
                        parent: parent.as_ref().map(|p| p.value.clone()),
                        layout: std::cell::OnceCell::new(),
                    }));
                let const_idx = self.make_constant(struct_def, line);
                self.emit_op_u16(OpCode::Constant, const_idx, line);

                if self.current_frame().scope_depth > 0 {
                    self.add_local(&name.value, false, None);
                } else {
                    let name_const =
                        self.make_constant(Value::String(name.value.as_str().into()), line);
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
                                    Some(Value::String(value.as_str().into()))
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
                        self.make_constant(Value::String(name.value.as_str().into()), line);
                    self.emit_op_u16(OpCode::DefineGlobal, name_const, line);
                }
            }

            Statement::ContainsDef {
                struct_name,
                methods,
                ..
            } => {
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
                    self.emit_constant(Value::String(method_name.value.as_str().into()), line);
                }
                // Emit DefineMethod opcode
                let struct_const =
                    self.make_constant(Value::String(struct_name.value.as_str().into()), line);
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
                    self.make_constant(Value::String(pattern_global.as_str().into()), line);
                self.emit_op_u16(OpCode::DefineGlobal, name_const, line);
            }

            Statement::Choose { subject, arms, .. } => {
                // Store subject as a temporary global so pattern functions can access it
                // without affecting the local stack layout
                self.compile_expression(subject);
                let temp_name = "__choose_tmp__";
                let temp_const = self.make_constant(Value::String(temp_name.into()), line);
                self.emit_op_u16(OpCode::DefineGlobal, temp_const, line);

                let mut end_jumps = Vec::new();
                let mut has_else = false;

                for arm in arms {
                    if arm.pattern_name == "else" {
                        has_else = true;
                        self.compile_block_as_expression(&arm.body, line);
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
                        let nc =
                            self.make_constant(Value::String(pattern_global.as_str().into()), line);
                        self.emit_op_u16(OpCode::DefineGlobal, nc, line);
                    }

                    // Call pattern function with subject
                    let pattern_global = format!("__pattern_{}", arm.pattern_name);
                    let pg_const =
                        self.make_constant(Value::String(pattern_global.as_str().into()), line);
                    self.emit_op_u16(OpCode::GetGlobal, pg_const, line);
                    let subj_const = self.make_constant(Value::String(temp_name.into()), line);
                    self.emit_op_u16(OpCode::GetGlobal, subj_const, line);
                    self.emit_op_u8(OpCode::Call, 1, line);

                    // Check result
                    let skip = self.emit_jump(OpCode::JumpIfFalse, line);
                    self.emit_op(OpCode::Pop, line); // pop True

                    // Compile arm body — result goes on stack
                    self.compile_block_as_expression(&arm.body, line);
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

                let path_const = self.make_constant(Value::String(path_str.as_str().into()), line);

                if let Some(names) = selective {
                    // Selective import: push the names
                    for name in names {
                        let nc =
                            self.make_constant(Value::String(name.value.as_str().into()), line);
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
                self.emit_constant(Value::String(value.as_str().into()), line);
            }
            Expression::Boolean { value, .. } => {
                self.emit_op(if *value { OpCode::True } else { OpCode::False }, line);
            }
            Expression::NoneExpr { .. } => {
                self.emit_op(OpCode::None, line);
            }

            Expression::Grouped(inner) => {
                self.compile_expression(inner);
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
                    if let Some(slot) = self.resolve_local(&ident.value) {
                        if !self.current_frame().locals[slot as usize].mutable {
                            self.error(
                                &format!(
                                    "cannot mutate immutable variable '{}' with {}",
                                    ident.value, operator
                                ),
                                line,
                            );
                        }
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
                                .make_constant(Value::String(ident.value.as_str().into()), line);
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
                            self.emit_constant(Value::String(s.as_str().into()), line);
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
                        self.make_constant(Value::String(field.value.as_str().into()), line);
                    if named_args.is_empty() {
                        self.emit_op_u16(OpCode::MethodCall, method_const, line);
                        self.emit_byte(args.len() as u8, line);
                    } else {
                        for (name, val_expr) in named_args {
                            self.emit_constant(Value::String(name.as_str().into()), line);
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
                        self.emit_constant(Value::String(name.as_str().into()), line);
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
                    self.make_constant(Value::String(field.value.as_str().into()), line);
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
                    self.make_constant(Value::String(struct_name.as_str().into()), line);
                // Push field name-value pairs
                for (fname, fval) in field_values {
                    self.emit_constant(Value::String(fname.as_str().into()), line);
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
                    self.make_constant(Value::String(variant_name.as_str().into()), line);
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
                            self.emit_constant(Value::String(fname.as_str().into()), line);
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

                for arm in arms {
                    self.compile_expression(&arm.condition);
                    let skip = self.emit_jump(OpCode::JumpIfFalse, line);
                    self.emit_op(OpCode::Pop, line);
                    // Condition true → compile body (last expr is the value)
                    self.compile_block_as_expression(&arm.body, line);
                    let end = self.emit_jump(OpCode::Jump, line);
                    end_jumps.push(end);
                    self.patch_jump(skip);
                    self.emit_op(OpCode::Pop, line);
                }

                // Default arm
                if let Some(default_body) = default {
                    self.compile_block_as_expression(default_body, line);
                } else if let Some(error_body) = error_default {
                    self.compile_block_as_expression(error_body, line);
                } else {
                    self.emit_op(OpCode::None, line);
                }

                for j in end_jumps {
                    self.patch_jump(j);
                }
            }

            Expression::Guard {
                value,
                binding,
                fallback,
                ..
            } => {
                self.compile_expression(value);
                let binding_const =
                    self.make_constant(Value::String(binding.value.as_str().into()), line);

                // Emit Guard opcode with jump offset placeholder
                self.emit_op(OpCode::Guard, line);
                let guard_jump_pos = self.current_chunk().len();
                self.current_chunk().write_u16(0xFFFF, line); // jump offset placeholder
                // Binding constant (read by VM after jump offset)
                self.emit_byte((binding_const >> 8) as u8, line);
                self.emit_byte(binding_const as u8, line);

                // If not error, value stays on stack → jump over fallback
                let end_jump = self.emit_jump(OpCode::Jump, line);

                // Patch guard jump: VM IP is after all 4 operand bytes (2 jump + 2 binding)
                // So offset = fallback_start - (guard_jump_pos + 4)
                let fallback_start = self.current_chunk().len();
                let offset = fallback_start - guard_jump_pos - 4;
                self.current_chunk()
                    .patch_u16(guard_jump_pos, offset as u16);

                // Error case: compile fallback expression
                self.compile_expression(fallback);
                self.patch_jump(end_jump);
            }

            Expression::ErrorConstruct { tag, value, .. } => {
                self.compile_expression(value);
                if let Some(tag_str) = tag {
                    self.emit_constant(Value::String(tag_str.as_str().into()), line);
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
                self.compile_expression(value);
                let type_str = target.type_name();
                let type_const = self.make_constant(Value::String(type_str.as_str().into()), line);
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
                    self.emit_constant(Value::String(t.as_str().into()), line);
                    flags |= 0x01;
                }
                if let Some(st) = sub_tag {
                    self.emit_constant(Value::String(st.as_str().into()), line);
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
    fn compile_last_statement_as_value(&mut self, stmt: &Statement, line: u32) {
        match stmt {
            Statement::Expr(expr) => {
                self.compile_expression(expr);
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
                    self.make_constant(Value::String(name.value.as_str().into()), line);
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
                        self.make_constant(Value::String(name.value.as_str().into()), line);
                    self.emit_op_u16(OpCode::SetGlobal, name_const, line);
                    self.emit_op(OpCode::Pop, line);
                }
            }
        } else if self.current_frame().scope_depth > 0 {
            self.add_local(&name.value, true, None);
        } else {
            let name_const = self.make_constant(Value::String(name.value.as_str().into()), line);
            self.emit_op_u16(OpCode::DefineGlobal, name_const, line);
        }
    }

    fn compile_block_as_expression(&mut self, stmts: &[Statement], line: u32) {
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
                        self.compile_last_statement_as_value(stmt, line);
                        self.end_scope(line);
                    }
                    _ => {
                        self.compile_statement(stmt);
                        self.end_scope(line);
                        self.emit_op(OpCode::None, line);
                    }
                }
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
                    self.make_constant(Value::String(ident.value.as_str().into()), line);
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
                self.make_constant(Value::String(type_ident.value.as_str().into()), line);
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
                    self.make_constant(Value::String(ident.value.as_str().into()), line);
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
        // Try local first
        if let Some(slot) = self.resolve_local(&ident.value) {
            self.emit_op_u16(OpCode::GetLocal, slot, line);
            return;
        }
        // Try upvalue
        let frame_idx = self.frames.len() - 1;
        if let Some(uv_idx) = self.resolve_upvalue(frame_idx, &ident.value) {
            self.emit_op_u16(OpCode::GetUpvalue, uv_idx, line);
            return;
        }
        // Fall back to global
        let name_const = self.make_constant(Value::String(ident.value.as_str().into()), line);
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
                    self.compile_last_statement_as_value(stmt, line);
                    self.emit_op(OpCode::Return, line);
                } else {
                    self.compile_statement(stmt);
                }
            }
        }

        // Pop this frame and get the function + upvalues
        let frame = self.frames.pop().unwrap();
        let function = frame.function;
        let upvalues = frame.upvalues;

        // Add the function as a constant in the enclosing scope
        let func_const = self.make_constant(
            Value::Closure(std::rc::Rc::new(crate::vm::value::ObjClosure {
                function: std::rc::Rc::new(function),
                upvalues: Vec::new(), // placeholder, VM fills real upvalues
                call_count: std::cell::Cell::new(0),
                loop_count: std::cell::Cell::new(0),
                jit_state: std::cell::Cell::new(0),
                jit_thunk: std::cell::Cell::new(None),
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
        "STRING" => Value::String("".into()),
        "BOOLEAN" => Value::Boolean(false),
        "CHAR" => Value::Char('\0'),
        "BYTE" => Value::Byte(0),
        "UINT" => Value::Uint(0),
        "ARRAY" => Value::Array(std::rc::Rc::new(std::cell::RefCell::new(Vec::new()))),
        "TUPLE" => Value::Tuple(std::rc::Rc::new(Vec::new())),
        "MAP" => Value::Map(std::rc::Rc::new(std::cell::RefCell::new(Vec::new()))),
        "SET" => Value::Set(std::rc::Rc::new(std::cell::RefCell::new(Vec::new()))),
        _ => Value::None,
    }
}
