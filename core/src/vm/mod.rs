pub mod builtins;
pub mod value;

use crate::compiler::opcode::OpCode;
use crate::jit::JitEngine;
use crate::vm::value::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

const STACK_MAX: usize = 262144; // 256K stack slots
const FRAMES_MAX: usize = 16384; // 16K call frames — supports deep recursion

/// A call frame: one per active function invocation.
#[derive(Debug)]
struct CallFrame {
    closure: Rc<ObjClosure>,
    ip: usize,
    slot_offset: usize,
    /// When executing a module function, holds the module's globals
    /// so `GetGlobal` can resolve module-scoped variables (e.g. `io` in `toml`).
    module_globals: Option<Rc<HashMap<String, Value>>>,
}

impl CallFrame {
    fn read_byte(&mut self) -> u8 {
        let byte = self.closure.function.chunk.code[self.ip];
        self.ip += 1;
        byte
    }

    fn read_u16(&mut self) -> u16 {
        let hi = self.closure.function.chunk.code[self.ip] as u16;
        let lo = self.closure.function.chunk.code[self.ip + 1] as u16;
        self.ip += 2;
        (hi << 8) | lo
    }

    fn read_constant(&self, idx: u16) -> &Value {
        &self.closure.function.chunk.constants[idx as usize]
    }
}

/// Opaque handle returned by `VM::frames_pop` to the JIT runtime so it can
/// complete a Return without depending on the private `CallFrame` layout.
#[allow(dead_code)]
pub(crate) struct CallFrameHandle {
    pub(crate) slot_offset: usize,
}

/// Runtime error with source information.
#[derive(Debug)]
pub struct VMError {
    pub message: String,
    pub line: u32,
}

impl std::fmt::Display for VMError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

/// The OxigenLang virtual machine.
pub struct VM {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: HashMap<String, Value>,
    open_upvalues: Vec<Rc<RefCell<Upvalue>>>,

    // Module system
    pub current_file: Option<PathBuf>,
    pub stdlib_path: PathBuf,
    module_cache: HashMap<PathBuf, Rc<ObjModule>>,
    import_stack: Vec<PathBuf>,
    pub is_main_context: bool,

    // Source for error messages
    source: String,

    // Script arguments
    script_args: Vec<String>,

    // Global variable metadata for is_mut/is_type_mut
    global_mutability: HashMap<String, bool>,
    global_type_constraints: HashMap<String, Option<String>>,

    /// Version counter for the globals map — bumped on every write.
    /// Used by the JIT's GetGlobal inline cache to detect staleness.
    pub(crate) globals_version: u64,

    /// Baseline JIT engine. Stub/no-op when the `jit` feature is off.
    pub jit: JitEngine,
}

impl VM {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        builtins::register_builtins(&mut globals);

        VM {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            globals,
            open_upvalues: Vec::new(),
            current_file: None,
            stdlib_path: crate::evaluator::find_stdlib_path(),
            module_cache: HashMap::new(),
            import_stack: Vec::new(),
            is_main_context: true,
            source: String::new(),
            script_args: Vec::new(),
            global_mutability: HashMap::new(),
            global_type_constraints: HashMap::new(),
            globals_version: 0,
            jit: JitEngine::new(),
        }
    }

    pub fn set_source(&mut self, source: &str) {
        self.source = source.to_string();
    }

    pub fn set_file(&mut self, path: PathBuf) {
        self.current_file = Some(path);
    }

    pub fn set_script_args(&mut self, args: &[String]) {
        self.script_args = args.to_vec();
        // Make script args available as __args global
        let args_val: Vec<Value> = args
            .iter()
            .map(|a| Value::String(a.as_str().into()))
            .collect();
        self.globals.insert(
            "__args".to_string(),
            Value::Array(Rc::new(RefCell::new(args_val))),
        );
    }

    fn maybe_compile_current_loop_hot_function(&mut self, frame_idx: usize) {
        let closure = Rc::clone(&self.frames[frame_idx].closure);
        let loop_count = closure.loop_count.get().saturating_add(1);
        closure.loop_count.set(loop_count);

        if closure.jit_state.get() != 0 || loop_count < self.jit.loop_threshold() {
            return;
        }

        if let Some(thunk) = self
            .jit
            .maybe_compile_loop_thunk(&closure.function, loop_count)
        {
            closure.jit_thunk.set(Some(thunk));
            closure.jit_state.set(1);
        } else {
            closure.jit_state.set(2);
        }
    }

    /// Run a compiled function.
    pub fn run(&mut self, function: Function) -> Result<Value, VMError> {
        let closure = Rc::new(ObjClosure {
            function: Rc::new(function),
            upvalues: Vec::new(),
            call_count: std::cell::Cell::new(0),
            loop_count: std::cell::Cell::new(0),
            jit_state: std::cell::Cell::new(0),
            jit_thunk: std::cell::Cell::new(None),
        });

        // Push the closure itself onto the stack (slot 0) — this is the
        // callee slot that `call_closure` expects.
        self.stack.push(Value::Closure(Rc::clone(&closure)));

        // Route the top-level through `call_closure` so it participates in
        // JIT tiering just like any other call. `call_closure` pushes the
        // frame, bumps the counter, and (if hot) hands off to the JIT; if
        // the JIT drives the call to completion the frame is already gone
        // and `execute_until(0)` is a no-op.
        self.call_closure(closure, 0, &[], None)?;

        self.execute_until(0)?;
        Ok(self.pop())
    }

    // ── Opcode handlers (shared between interpreter and JIT runtime) ──

    /// Read the name string at constant index `name_idx`. The compiler
    /// guarantees globals use string constants, but we defensively return
    /// an error if that invariant is broken.
    fn name_constant(&self, name_idx: u16) -> Result<Rc<str>, VMError> {
        match self.current_constant(name_idx) {
            Value::String(s) => Ok(s),
            _ => Err(self.runtime_error("invalid global name")),
        }
    }

    pub(crate) fn handle_get_global(&mut self, name_idx: u16) -> Result<(), VMError> {
        let name = self.name_constant(name_idx)?;
        let frame = self.frames.last().unwrap();
        let found = frame
            .module_globals
            .as_ref()
            .and_then(|mg| mg.get(name.as_ref()))
            .cloned()
            .or_else(|| self.globals.get(name.as_ref()).cloned());
        match found {
            Some(val) => {
                self.push(val);
                Ok(())
            }
            None => Err(self.runtime_error_hint(
                &format!("undefined variable: {}", name),
                "use `:=` to declare new variables",
            )),
        }
    }

    pub(crate) fn handle_set_global(&mut self, name_idx: u16) -> Result<(), VMError> {
        let name = self.name_constant(name_idx)?;
        if !self.globals.contains_key(name.as_ref()) {
            return Err(self.runtime_error_hint(
                &format!("undefined variable: {}", name),
                "use `:=` to declare new variables, or `=` to reassign an existing typed variable",
            ));
        }
        if let Some(false) = self.global_mutability.get(name.as_ref()) {
            return Err(self.runtime_error_hint(
                &format!("cannot reassign immutable variable '{}'", name),
                "use `:=` to override an immutable binding",
            ));
        }
        let val = self.peek(0).clone();
        self.globals.insert(name.to_string(), val);
        self.globals_version = self.globals_version.wrapping_add(1);
        Ok(())
    }

    pub(crate) fn handle_define_global(&mut self, name_idx: u16) -> Result<(), VMError> {
        let name = self.name_constant(name_idx)?;
        let val = self.pop();
        self.globals.insert(name.to_string(), val);
        self.globals_version = self.globals_version.wrapping_add(1);
        Ok(())
    }

    pub(crate) fn handle_define_global_typed(
        &mut self,
        name_idx: u16,
        mutable: bool,
        type_idx: u16,
    ) -> Result<(), VMError> {
        let name = self.name_constant(name_idx)?;
        let type_name = self.current_constant(type_idx);
        let val = self.pop();
        self.globals.insert(name.to_string(), val);
        self.global_mutability.insert(name.to_string(), mutable);
        if let Value::String(tn) = type_name {
            self.global_type_constraints
                .insert(name.to_string(), Some(tn.to_string()));
        }
        self.globals_version = self.globals_version.wrapping_add(1);
        Ok(())
    }

    pub(crate) fn handle_get_upvalue(&mut self, idx: u16) -> Result<(), VMError> {
        let upvalue = Rc::clone(&self.frames.last().unwrap().closure.upvalues[idx as usize]);
        let val = match &*upvalue.borrow() {
            Upvalue::Open(slot) => self.stack[*slot].clone(),
            Upvalue::Closed(val) => val.clone(),
        };
        self.push(val);
        Ok(())
    }

    pub(crate) fn handle_set_upvalue(&mut self, idx: u16) -> Result<(), VMError> {
        let val = self.peek(0).clone();
        let upvalue = Rc::clone(&self.frames.last().unwrap().closure.upvalues[idx as usize]);
        match &mut *upvalue.borrow_mut() {
            Upvalue::Open(slot) => {
                self.stack[*slot] = val;
            }
            Upvalue::Closed(v) => {
                *v = val;
            }
        }
        Ok(())
    }

    pub(crate) fn handle_close_upvalue(&mut self) {
        let stack_top = self.stack.len() - 1;
        self.close_upvalues(stack_top);
        self.pop();
    }

    pub(crate) fn handle_struct_literal(
        &mut self,
        name_idx: u16,
        field_count: u8,
    ) -> Result<(), VMError> {
        let name = self.current_constant(name_idx);
        let Value::String(struct_name) = name else {
            return Err(self.runtime_error("expected struct name"));
        };
        let field_count = field_count as usize;
        let mut fields = HashMap::new();
        let start = self.stack.len() - field_count * 2;
        let flat: Vec<Value> = self.stack.drain(start..).collect();
        for pair in flat.chunks(2) {
            if let Value::String(fname) = &pair[0] {
                fields.insert(fname.to_string(), pair[1].clone());
            }
        }
        self.push(Value::StructInstance(Rc::new(ObjStructInstance {
            struct_name: struct_name.to_string(),
            fields: RefCell::new(fields),
        })));
        Ok(())
    }

    pub(crate) fn handle_get_field(&mut self, field_idx: u16) -> Result<(), VMError> {
        let field_name = self.current_constant(field_idx);
        let object = self.pop();
        let Value::String(fname) = &field_name else {
            return Err(self.runtime_error("invalid field name"));
        };

        match &object {
            Value::StructInstance(inst) => {
                let field_val = {
                    let fields = inst.fields.borrow();
                    fields.get(fname.as_ref()).cloned()
                };
                if let Some(val) = field_val {
                    self.push(val);
                    return Ok(());
                }
                let method_val =
                    if let Some(Value::StructDef(def)) = self.globals.get(&inst.struct_name) {
                        let methods = def.methods.borrow();
                        methods.get(fname.as_ref()).cloned()
                    } else {
                        None
                    };
                if let Some(method) = method_val {
                    self.push(method);
                    Ok(())
                } else {
                    Err(self.runtime_error_hint(
                        &format!("field '{}' not found on {}", fname, inst.struct_name),
                        "check the struct definition for available fields and methods",
                    ))
                }
            }
            Value::Module(m) => match m.globals.get(fname.as_ref()) {
                Some(val) => {
                    self.push(val.clone());
                    Ok(())
                }
                None => Err(self.runtime_error_hint(
                    &format!("module '{}' has no member '{}'", m.name, fname),
                    "check the module's public API for available members",
                )),
            },
            Value::ErrorValue { msg, tag } => match fname.as_ref() {
                "msg" => {
                    self.push(Value::String(Rc::clone(msg)));
                    Ok(())
                }
                "tag" => {
                    match tag {
                        Some(t) => self.push(Value::String(Rc::clone(t))),
                        None => self.push(Value::None),
                    }
                    Ok(())
                }
                _ => Err(self.runtime_error(&format!("ErrorValue has no field '{}'", fname))),
            },
            Value::Map(entries) => {
                let entries = entries.borrow();
                let key = Value::String(Rc::clone(fname));
                let mut found = None;
                for (k, v) in entries.iter() {
                    if *k == key {
                        found = Some(v.clone());
                        break;
                    }
                }
                drop(entries);
                self.push(found.unwrap_or(Value::None));
                Ok(())
            }
            Value::EnumDef(def) => {
                let variant = def
                    .variants
                    .iter()
                    .find(|v| v.name.as_str() == fname.as_ref());
                match variant {
                    Some(v) => match &v.kind {
                        crate::vm::value::VmEnumVariantKind::Unit(disc) => {
                            self.push(Value::EnumInstance(Rc::new(
                                crate::vm::value::ObjEnumInstance {
                                    enum_name: def.name.clone(),
                                    variant_name: v.name.clone(),
                                    payload: crate::vm::value::VmEnumPayload::Unit(
                                        disc.clone(),
                                    ),
                                },
                            )));
                            Ok(())
                        }
                        crate::vm::value::VmEnumVariantKind::Tuple(_) => {
                            Err(self.runtime_error_hint(
                                &format!(
                                    "variant '{}' of enum {} requires arguments",
                                    v.name, def.name
                                ),
                                "use EnumName.Variant(args) to construct tuple variants",
                            ))
                        }
                        crate::vm::value::VmEnumVariantKind::Struct(_) => {
                            Err(self.runtime_error_hint(
                                &format!(
                                    "variant '{}' of enum {} requires struct fields",
                                    v.name, def.name
                                ),
                                "use EnumName.Variant { field: value } for struct variants",
                            ))
                        }
                    },
                    None => Err(self.runtime_error(&format!(
                        "enum {} has no variant '{}'",
                        def.name, fname
                    ))),
                }
            }
            Value::EnumInstance(inst) => {
                let name = fname.as_ref();
                match name {
                    "name" => {
                        self.push(Value::String(inst.variant_name.as_str().into()));
                        Ok(())
                    }
                    "value" => match &inst.payload {
                        crate::vm::value::VmEnumPayload::Unit(Some(v)) => {
                            self.push(v.clone());
                            Ok(())
                        }
                        crate::vm::value::VmEnumPayload::Unit(None) => {
                            self.push(Value::None);
                            Ok(())
                        }
                        _ => Err(self
                            .runtime_error(".value is only defined on unit variants")),
                    },
                    other => match &inst.payload {
                        crate::vm::value::VmEnumPayload::Tuple(items) => {
                            let idx_opt = other.parse::<usize>().ok().or_else(|| {
                                self.globals.get(&inst.enum_name).and_then(|gv| {
                                    if let Value::EnumDef(def) = gv {
                                        def.variants
                                            .iter()
                                            .find(|v| v.name == inst.variant_name)
                                            .and_then(|v| match &v.kind {
                                                crate::vm::value::VmEnumVariantKind::Tuple(
                                                    names,
                                                ) => names.iter().position(|n| n == other),
                                                _ => None,
                                            })
                                    } else {
                                        None
                                    }
                                })
                            });
                            match idx_opt.and_then(|i| items.get(i).cloned()) {
                                Some(v) => {
                                    self.push(v);
                                    Ok(())
                                }
                                None => Err(self.runtime_error(&format!(
                                    "tuple variant {}.{} has no field '{}'",
                                    inst.enum_name, inst.variant_name, other
                                ))),
                            }
                        }
                        crate::vm::value::VmEnumPayload::Struct(fields) => {
                            match fields.iter().find(|(k, _)| k == other) {
                                Some((_, v)) => {
                                    self.push(v.clone());
                                    Ok(())
                                }
                                None => Err(self.runtime_error(&format!(
                                    "struct variant {}.{} has no field '{}'",
                                    inst.enum_name, inst.variant_name, other
                                ))),
                            }
                        }
                        crate::vm::value::VmEnumPayload::Unit(_) => {
                            Err(self.runtime_error(&format!(
                                "unit variant {}.{} has no field '{}'",
                                inst.enum_name, inst.variant_name, other
                            )))
                        }
                    },
                }
            }
            _ => Err(self.runtime_error_hint(
                &format!("cannot access field '{}' on {}", fname, object.type_name()),
                "field access is supported on structs, enums, modules, maps, and error values",
            )),
        }
    }

    pub(crate) fn handle_set_field(&mut self, field_idx: u16) -> Result<(), VMError> {
        let field_name = self.current_constant(field_idx);
        let value = self.pop();
        let object = self.pop();
        let Value::String(fname) = &field_name else {
            return Err(self.runtime_error("invalid field name"));
        };
        match &object {
            Value::StructInstance(inst) => {
                inst.fields.borrow_mut().insert(fname.to_string(), value);
                Ok(())
            }
            Value::Map(entries) => {
                let mut entries = entries.borrow_mut();
                let key = Value::String(Rc::clone(fname));
                let mut found = false;
                for (k, v) in entries.iter_mut() {
                    if *k == key {
                        *v = value.clone();
                        found = true;
                        break;
                    }
                }
                if !found {
                    entries.push((key, value));
                }
                Ok(())
            }
            _ => Err(self.runtime_error_hint(
                &format!("cannot set field on {}", object.type_name()),
                "field assignment is supported on struct instances and maps",
            )),
        }
    }

    pub(crate) fn handle_define_method(
        &mut self,
        struct_name_idx: u16,
        method_count: u8,
    ) -> Result<(), VMError> {
        let struct_name = self.current_constant(struct_name_idx);
        let Value::String(name_str) = struct_name else {
            return Err(self.runtime_error("invalid struct name"));
        };
        let method_count = method_count as usize;
        let start = self.stack.len() - method_count * 2;
        let flat: Vec<Value> = self.stack.drain(start..).collect();
        if let Some(Value::StructDef(def)) = self.globals.get(name_str.as_ref()) {
            let mut methods = def.methods.borrow_mut();
            for pair in flat.chunks(2) {
                if let Value::String(mname) = &pair[1] {
                    methods.insert(mname.to_string(), pair[0].clone());
                }
            }
        }
        Ok(())
    }

    pub(crate) fn handle_method_call(
        &mut self,
        method_idx: u16,
        arg_count: u8,
    ) -> Result<(), VMError> {
        let method_name = self.current_constant(method_idx);
        let Value::String(mname) = method_name else {
            return Err(self.runtime_error("invalid method name"));
        };
        self.call_method(&mname, arg_count as usize, &[])
    }

    /// Handle the `Closure` opcode. Reads the upvalue descriptors starting
    /// at `descriptors_offset` in the current frame's chunk. On return the
    /// new closure is on top of the stack.
    pub(crate) fn handle_closure(
        &mut self,
        fn_const_idx: u16,
        descriptors_offset: usize,
    ) -> Result<(), VMError> {
        let constant = self.current_constant(fn_const_idx);
        let Value::Closure(template) = constant else {
            return Err(self.runtime_error("expected closure constant"));
        };

        let upvalue_count = template.function.upvalue_count as usize;

        // Snapshot what we need from the current frame so we can call
        // mutable methods below without conflicting borrows.
        let (chunk_rc, parent_upvalues, slot_offset) = {
            let frame = self.frames.last().unwrap();
            (
                Rc::clone(&frame.closure.function),
                frame.closure.upvalues.clone(),
                frame.slot_offset,
            )
        };
        let code = &chunk_rc.chunk.code;

        let mut upvalues = Vec::with_capacity(upvalue_count);
        let mut off = descriptors_offset;
        for _ in 0..upvalue_count {
            let is_local = code[off] == 1;
            off += 1;
            let index = ((code[off] as u16) << 8) | (code[off + 1] as u16);
            off += 2;

            if is_local {
                let abs_slot = slot_offset + index as usize;
                let upvalue = self.capture_upvalue(abs_slot);
                upvalues.push(upvalue);
            } else {
                upvalues.push(Rc::clone(&parent_upvalues[index as usize]));
            }
        }

        let closure = Rc::new(ObjClosure {
            function: Rc::clone(&template.function),
            upvalues,
            call_count: std::cell::Cell::new(0),
            loop_count: std::cell::Cell::new(0),
            jit_state: std::cell::Cell::new(0),
            jit_thunk: std::cell::Cell::new(None),
        });
        self.push(Value::Closure(closure));
        Ok(())
    }

    // ── Stack Operations ───────────────────────────────────────────────

    pub(crate) fn push(&mut self, value: Value) {
        if self.stack.len() >= STACK_MAX {
            panic!("stack overflow: exceeded {} slots", STACK_MAX);
        }
        self.stack.push(value);
    }

    pub(crate) fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack underflow")
    }

    pub(crate) fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - distance]
    }

    /// Stack slot `n` counted from 0 at the bottom. Used by the JIT
    /// runtime to read/write locals without constructing a CallFrame
    /// reference.
    #[allow(dead_code)]
    pub(crate) fn stack_slot(&self, n: usize) -> &Value {
        &self.stack[n]
    }

    #[allow(dead_code)]
    pub(crate) fn set_stack_slot(&mut self, n: usize, v: Value) {
        self.stack[n] = v;
    }

    #[allow(dead_code)]
    pub(crate) fn current_slot_offset(&self) -> usize {
        self.frames.last().map(|f| f.slot_offset).unwrap_or(0)
    }

    /// Clone the constant at `idx` from the current frame's chunk. Used by
    /// the JIT runtime's `jit_push_constant` helper.
    pub(crate) fn current_constant(&self, idx: u16) -> Value {
        let frame = self.frames.last().unwrap();
        frame.closure.function.chunk.constants[idx as usize].clone()
    }

    /// Pop the topmost call frame. Returns `None` if there is none.
    /// Used by the JIT runtime's `jit_op_return` helper.
    #[allow(dead_code)]
    pub(crate) fn frames_pop(&mut self) -> Option<CallFrameHandle> {
        self.frames.pop().map(|f| CallFrameHandle {
            slot_offset: f.slot_offset,
        })
    }

    #[allow(dead_code)]
    pub(crate) fn stack_truncate(&mut self, len: usize) {
        self.stack.truncate(len);
    }

    #[allow(dead_code)]
    pub(crate) fn frames_len(&self) -> usize {
        self.frames.len()
    }

    #[allow(dead_code)]
    pub(crate) fn stack_as_mut_ptr(&mut self) -> *mut Value {
        self.stack.as_mut_ptr()
    }

    #[allow(dead_code)]
    pub(crate) fn stack_len(&self) -> usize {
        self.stack.len()
    }

    /// Shrink the stack by `n` elements, dropping the values. Used by the
    /// JIT's inline int fast path to commit a binop result.
    #[allow(dead_code)]
    pub(crate) fn stack_shrink(&mut self, n: usize) {
        let new_len = self.stack.len().saturating_sub(n);
        self.stack.truncate(new_len);
    }

    fn current_line(&self) -> u32 {
        let frame = self.frames.last().unwrap();
        let ip = if frame.ip > 0 { frame.ip - 1 } else { 0 };
        if ip < frame.closure.function.chunk.lines.len() {
            frame.closure.function.chunk.lines[ip]
        } else {
            0
        }
    }

    fn format_error(&self, message: &str, hint: Option<&str>) -> String {
        let line_num = self.current_line() as usize;
        let mut out = String::new();

        out.push_str(&format!("error: {}\n", message));
        out.push_str(&format!("  --> line {}\n", line_num));

        if !self.source.is_empty() {
            if let Some(source_line) = self.source.lines().nth(line_num.saturating_sub(1)) {
                let line_str = format!("{}", line_num);
                let padding = " ".repeat(line_str.len());
                out.push_str(&format!("{} |\n", padding));
                out.push_str(&format!("{} | {}\n", line_str, source_line));
                out.push_str(&format!("{} |", padding));
            }
        }

        if let Some(hint) = hint {
            out.push_str(&format!("\n  = hint: {}", hint));
        }

        out
    }

    pub(crate) fn runtime_error(&self, message: &str) -> VMError {
        let line = self.current_line();
        VMError {
            message: self.format_error(message, None),
            line,
        }
    }

    pub(crate) fn runtime_error_hint(&self, message: &str, hint: &str) -> VMError {
        let line = self.current_line();
        VMError {
            message: self.format_error(message, Some(hint)),
            line,
        }
    }

    // ── Main Execution Loop ────────────────────────────────────────────

    /// Drive the interpreter until the current frame's depth returns to
    /// `stop_depth`. The top-level entry point uses `stop_depth = 0`; the
    /// JIT helper passes in its pre-call depth so it can re-enter the
    /// interpreter for a single callee activation without restarting the
    /// outer loop.
    ///
    /// On normal exit the last-popped frame's return value sits on top of
    /// the stack. `run()` consumes it; the JIT helper leaves it for its
    /// caller.
    pub(crate) fn execute_until(&mut self, stop_depth: usize) -> Result<(), VMError> {
        // If the frame we were supposed to drive has already been popped
        // (e.g. the JIT ran the whole call to completion before we got
        // here), there is nothing to do.
        if self.frames.len() <= stop_depth {
            return Ok(());
        }
        loop {
            let frame_idx = self.frames.len() - 1;

            let instruction = self.frames[frame_idx].read_byte();
            let op = match OpCode::from_byte(instruction) {
                Some(op) => op,
                None => {
                    return Err(self.runtime_error(&format!("unknown opcode: {}", instruction)));
                }
            };

            match op {
                // ── Constants & Literals ────────────────────────────
                OpCode::Constant => {
                    let idx = self.frames[frame_idx].read_u16();
                    let value = self.frames[frame_idx].read_constant(idx).clone();
                    self.push(value);
                }
                OpCode::None => self.push(Value::None),
                OpCode::True => self.push(Value::Boolean(true)),
                OpCode::False => self.push(Value::Boolean(false)),

                // ── Stack Manipulation ──────────────────────────────
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::Dup => {
                    let val = self.peek(0).clone();
                    self.push(val);
                }

                // ── Arithmetic ──────────────────────────────────────
                OpCode::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    let result = self.binary_add(a, b)?;
                    self.push(result);
                }
                OpCode::Subtract => {
                    let b = self.pop();
                    let a = self.pop();
                    let result = self.binary_sub(a, b)?;
                    self.push(result);
                }
                OpCode::Multiply => {
                    let b = self.pop();
                    let a = self.pop();
                    let result = self.binary_mul(a, b)?;
                    self.push(result);
                }
                OpCode::Divide => {
                    let b = self.pop();
                    let a = self.pop();
                    let result = self.binary_div(a, b)?;
                    self.push(result);
                }
                OpCode::Modulo => {
                    let b = self.pop();
                    let a = self.pop();
                    let result = self.binary_mod(a, b)?;
                    self.push(result);
                }

                // ── Comparison ──────────────────────────────────────
                OpCode::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Boolean(a == b));
                }
                OpCode::NotEqual => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Boolean(a != b));
                }
                OpCode::Less => {
                    let b = self.pop();
                    let a = self.pop();
                    let result = self.compare_less(a, b)?;
                    self.push(result);
                }
                OpCode::LessEqual => {
                    let b = self.pop();
                    let a = self.pop();
                    let result = self.compare_less_equal(a, b)?;
                    self.push(result);
                }
                OpCode::Greater => {
                    let b = self.pop();
                    let a = self.pop();
                    let result = self.compare_greater(a, b)?;
                    self.push(result);
                }
                OpCode::GreaterEqual => {
                    let b = self.pop();
                    let a = self.pop();
                    let result = self.compare_greater_equal(a, b)?;
                    self.push(result);
                }

                // ── Logical ─────────────────────────────────────────
                OpCode::Not => {
                    let val = self.pop();
                    self.push(Value::Boolean(!val.is_truthy()));
                }

                // ── Bitwise ─────────────────────────────────────────
                OpCode::BitAnd => {
                    let b = self.pop();
                    let a = self.pop();
                    match (&a, &b) {
                        (Value::Integer(l), Value::Integer(r)) => self.push(Value::Integer(l & r)),
                        _ => return Err(self.runtime_error("bitwise & requires integers")),
                    }
                }
                OpCode::BitOr => {
                    let b = self.pop();
                    let a = self.pop();
                    match (&a, &b) {
                        (Value::Integer(l), Value::Integer(r)) => self.push(Value::Integer(l | r)),
                        _ => return Err(self.runtime_error("bitwise | requires integers")),
                    }
                }
                OpCode::BitXor => {
                    let b = self.pop();
                    let a = self.pop();
                    match (&a, &b) {
                        (Value::Integer(l), Value::Integer(r)) => self.push(Value::Integer(l ^ r)),
                        _ => return Err(self.runtime_error("bitwise ^ requires integers")),
                    }
                }
                OpCode::BitNot => {
                    let val = self.pop();
                    match &val {
                        Value::Integer(n) => self.push(Value::Integer(!n)),
                        _ => return Err(self.runtime_error("bitwise ~ requires an integer")),
                    }
                }
                OpCode::ShiftLeft => {
                    let b = self.pop();
                    let a = self.pop();
                    match (&a, &b) {
                        (Value::Integer(l), Value::Integer(r)) => self.push(Value::Integer(l << r)),
                        _ => return Err(self.runtime_error("<< requires integers")),
                    }
                }
                OpCode::ShiftRight => {
                    let b = self.pop();
                    let a = self.pop();
                    match (&a, &b) {
                        (Value::Integer(l), Value::Integer(r)) => self.push(Value::Integer(l >> r)),
                        _ => return Err(self.runtime_error(">> requires integers")),
                    }
                }

                // ── Unary ───────────────────────────────────────────
                OpCode::Negate => {
                    let val = self.pop();
                    match val {
                        Value::Integer(n) => self.push(Value::Integer(-n)),
                        Value::Float(f) => self.push(Value::Float(-f)),
                        _ => {
                            return Err(self.runtime_error_hint(
                                &format!(
                                    "negation is only supported for numbers, got {}",
                                    val.type_name()
                                ),
                                "only <int> and <float> values can be negated",
                            ));
                        }
                    }
                }

                // ── Postfix ─────────────────────────────────────────
                OpCode::Increment | OpCode::Decrement => {
                    // Handled by compiler emitting explicit add/sub + store
                    return Err(self.runtime_error("unexpected increment/decrement opcode"));
                }

                // ── Variables ───────────────────────────────────────
                OpCode::GetLocal => {
                    let slot = self.frames[frame_idx].read_u16() as usize;
                    let offset = self.frames[frame_idx].slot_offset;
                    let value = self.stack[offset + slot].clone();
                    self.push(value);
                }
                OpCode::SetLocal => {
                    let slot = self.frames[frame_idx].read_u16() as usize;
                    let offset = self.frames[frame_idx].slot_offset;
                    let value = self.peek(0).clone();
                    self.stack[offset + slot] = value;
                }
                OpCode::GetGlobal => {
                    let idx = self.frames[frame_idx].read_u16();
                    self.handle_get_global(idx)?;
                }
                OpCode::SetGlobal => {
                    let idx = self.frames[frame_idx].read_u16();
                    self.handle_set_global(idx)?;
                }
                OpCode::DefineGlobal => {
                    let idx = self.frames[frame_idx].read_u16();
                    self.handle_define_global(idx)?;
                }
                OpCode::DefineGlobalTyped => {
                    let name_idx = self.frames[frame_idx].read_u16();
                    let mutable = self.frames[frame_idx].read_byte() == 1;
                    let type_idx = self.frames[frame_idx].read_u16();
                    self.handle_define_global_typed(name_idx, mutable, type_idx)?;
                }

                // ── Upvalues ────────────────────────────────────────
                OpCode::GetUpvalue => {
                    let idx = self.frames[frame_idx].read_u16();
                    self.handle_get_upvalue(idx)?;
                }
                OpCode::SetUpvalue => {
                    let idx = self.frames[frame_idx].read_u16();
                    self.handle_set_upvalue(idx)?;
                }
                OpCode::CloseUpvalue => {
                    self.handle_close_upvalue();
                }

                // ── Control Flow ────────────────────────────────────
                OpCode::Jump => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    self.frames[frame_idx].ip += offset;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    if !self.peek(0).is_truthy() {
                        self.frames[frame_idx].ip += offset;
                    }
                }
                OpCode::JumpIfTrue => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    if self.peek(0).is_truthy() {
                        self.frames[frame_idx].ip += offset;
                    }
                }
                OpCode::Loop => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    self.maybe_compile_current_loop_hot_function(frame_idx);
                    self.frames[frame_idx].ip -= offset;
                }
                OpCode::PopJumpIfFalse => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    let val = self.pop();
                    if !val.is_truthy() {
                        self.frames[frame_idx].ip += offset;
                    }
                }

                // ── Functions ───────────────────────────────────────
                OpCode::Closure => {
                    let fn_idx = self.frames[frame_idx].read_u16();
                    let upvalue_count = match self.frames[frame_idx].read_constant(fn_idx) {
                        Value::Closure(t) => t.function.upvalue_count as usize,
                        _ => {
                            return Err(self.runtime_error("expected closure constant"));
                        }
                    };
                    let descriptors_offset = self.frames[frame_idx].ip;
                    // Skip past the upvalue descriptors; `handle_closure`
                    // reads them directly from the chunk.
                    self.frames[frame_idx].ip += 3 * upvalue_count;
                    self.handle_closure(fn_idx, descriptors_offset)?;
                }

                OpCode::Call => {
                    let arg_count = self.frames[frame_idx].read_byte() as usize;
                    self.call_value(arg_count, &[])?;
                }

                OpCode::CallNamed => {
                    let pos_count = self.frames[frame_idx].read_byte() as usize;
                    let named_count = self.frames[frame_idx].read_byte() as usize;

                    // Collect named args from stack (name, value pairs)
                    let mut named_args = Vec::with_capacity(named_count);
                    for _ in 0..named_count {
                        let val = self.pop();
                        let name = self.pop();
                        if let Value::String(name_str) = name {
                            named_args.push((name_str.to_string(), val));
                        }
                    }
                    named_args.reverse();

                    self.call_value(pos_count, &named_args)?;
                }

                OpCode::Return => {
                    let result = self.pop();
                    let frame = self.frames.pop().unwrap();

                    // Close upvalues
                    self.close_upvalues(frame.slot_offset);

                    // Pop the function's stack slots
                    self.stack.truncate(frame.slot_offset);

                    // Leave the return value on the stack for the caller.
                    // The top-level `run()` pops it after `execute_until`
                    // returns. Nested frames consume it as the `Call`
                    // opcode's result.
                    self.push(result);

                    if self.frames.len() <= stop_depth {
                        return Ok(());
                    }
                }

                // ── Collections ─────────────────────────────────────
                OpCode::BuildArray => {
                    let count = self.frames[frame_idx].read_u16() as usize;
                    let start = self.stack.len() - count;
                    let elements: Vec<Value> = self.stack.drain(start..).collect();
                    self.push(Value::Array(Rc::new(RefCell::new(elements))));
                }
                OpCode::BuildTuple => {
                    let count = self.frames[frame_idx].read_u16() as usize;
                    let start = self.stack.len() - count;
                    let elements: Vec<Value> = self.stack.drain(start..).collect();
                    self.push(Value::Tuple(Rc::new(elements)));
                }
                OpCode::BuildMap => {
                    let count = self.frames[frame_idx].read_u16() as usize;
                    let start = self.stack.len() - count * 2;
                    let flat: Vec<Value> = self.stack.drain(start..).collect();
                    let mut entries = Vec::with_capacity(count);
                    for pair in flat.chunks(2) {
                        entries.push((pair[0].clone(), pair[1].clone()));
                    }
                    self.push(Value::Map(Rc::new(RefCell::new(entries))));
                }
                OpCode::BuildSet => {
                    let count = self.frames[frame_idx].read_u16() as usize;
                    let start = self.stack.len() - count;
                    let elements: Vec<Value> = self.stack.drain(start..).collect();
                    // Deduplicate
                    let mut items = Vec::new();
                    for elem in elements {
                        if !items.iter().any(|i: &Value| i == &elem) {
                            items.push(elem);
                        }
                    }
                    self.push(Value::Set(Rc::new(RefCell::new(items))));
                }

                OpCode::Index => {
                    let index = self.pop();
                    let collection = self.pop();
                    let result = self.eval_index(collection, index)?;
                    self.push(result);
                }

                OpCode::IndexAssign => {
                    let value = self.pop();
                    let index = self.pop();
                    let collection = self.pop();
                    self.eval_index_assign(collection, index, value)?;
                }

                OpCode::Slice => {
                    let flags = self.frames[frame_idx].read_byte();
                    let has_end = flags & 0x02 != 0;
                    let has_start = flags & 0x01 != 0;

                    let end = if has_end { Some(self.pop()) } else { None };
                    let start = if has_start { Some(self.pop()) } else { None };
                    let collection = self.pop();

                    let result = self.eval_slice(collection, start, end)?;
                    self.push(result);
                }

                // ── Struct Operations ───────────────────────────────
                OpCode::StructDef => {
                    // Already handled by compiler pushing struct as constant
                    let _idx = self.frames[frame_idx].read_u16();
                    // This opcode is currently unused; structs are loaded via Constant
                }

                OpCode::StructLiteral => {
                    let name_idx = self.frames[frame_idx].read_u16();
                    let field_count = self.frames[frame_idx].read_byte();
                    self.handle_struct_literal(name_idx, field_count)?;
                }

                OpCode::GetField => {
                    let field_idx = self.frames[frame_idx].read_u16();
                    self.handle_get_field(field_idx)?;
                }

                OpCode::SetField => {
                    let field_idx = self.frames[frame_idx].read_u16();
                    self.handle_set_field(field_idx)?;
                }

                OpCode::DefineMethod => {
                    let struct_name_idx = self.frames[frame_idx].read_u16();
                    let method_count = self.frames[frame_idx].read_byte();
                    self.handle_define_method(struct_name_idx, method_count)?;
                }

                // ── Pattern Matching ────────────────────────────────
                // Patterns are now compiled as closures stored as __pattern_<name> globals.
                // DefinePattern and TestPattern opcodes are no longer emitted by the compiler.
                // These stubs exist only for backwards compatibility.
                OpCode::DefinePattern => {
                    let _name_idx = self.frames[frame_idx].read_u16();
                    let _param_count = self.frames[frame_idx].read_byte();
                }

                OpCode::TestPattern => {
                    let _offset = self.frames[frame_idx].read_u16();
                    let _hi = self.frames[frame_idx].read_byte();
                    let _lo = self.frames[frame_idx].read_byte();
                }

                // ── Error Handling ──────────────────────────────────
                OpCode::ErrorConstruct => {
                    let has_tag = self.frames[frame_idx].read_byte();
                    if has_tag == 1 {
                        let tag = self.pop();
                        let value = self.pop();
                        let tag_str = if let Value::String(s) = tag {
                            s
                        } else {
                            format!("{}", tag).into()
                        };
                        self.push(Value::ErrorValue {
                            msg: format!("{}", value).into(),
                            tag: Some(tag_str),
                        });
                    } else {
                        let value = self.pop();
                        self.push(Value::ErrorValue {
                            msg: format!("{}", value).into(),
                            tag: None,
                        });
                    }
                }

                OpCode::ValueConstruct => {
                    let value = self.pop();
                    self.push(Value::Wrapped(Rc::new(value)));
                }

                OpCode::Guard => {
                    let jump_offset = self.frames[frame_idx].read_u16() as usize;
                    let binding_hi = self.frames[frame_idx].read_byte();
                    let binding_lo = self.frames[frame_idx].read_byte();
                    let binding_idx = ((binding_hi as u16) << 8) | (binding_lo as u16);
                    let binding_name = self.frames[frame_idx].read_constant(binding_idx).clone();

                    let value = self.peek(0).clone();
                    if let Value::ErrorValue { .. } = &value {
                        // Error case: bind the error value, jump to fallback
                        if let Value::String(name) = binding_name {
                            self.globals.insert(name.to_string(), value);
                        }
                        self.pop(); // pop the error
                        self.frames[frame_idx].ip += jump_offset;
                    }
                    // Not an error: keep value on stack as-is (including Value(...) wrappers)
                }

                OpCode::Fail => {
                    let value = self.pop();
                    match value {
                        Value::ErrorValue { msg, .. } => {
                            return Err(VMError {
                                message: msg.to_string(),
                                line: self.current_line(),
                            });
                        }
                        _ => {
                            return Err(VMError {
                                message: format!("{}", value),
                                line: self.current_line(),
                            });
                        }
                    }
                }

                // ── Modules ─────────────────────────────────────────
                OpCode::Import => {
                    let path_idx = self.frames[frame_idx].read_u16();
                    let selective_count = self.frames[frame_idx].read_byte() as usize;
                    let path = self.frames[frame_idx].read_constant(path_idx).clone();

                    // Collect selective names
                    let mut selective_names = Vec::new();
                    for _ in 0..selective_count {
                        let name = self.pop();
                        if let Value::String(s) = name {
                            selective_names.push(s.to_string());
                        }
                    }
                    selective_names.reverse();

                    if let Value::String(path_str) = path {
                        self.import_module(&path_str, &selective_names)?;
                    }
                }

                OpCode::GetModuleField => {
                    let field_idx = self.frames[frame_idx].read_u16();
                    let _field_name = self.frames[frame_idx].read_constant(field_idx).clone();
                    // Handled by GetField on module values
                }

                // ── String Interpolation ────────────────────────────
                OpCode::StringInterp => {
                    let count = self.frames[frame_idx].read_u16() as usize;
                    let start = self.stack.len() - count;
                    let parts: Vec<Value> = self.stack.drain(start..).collect();
                    let mut result = String::new();
                    for part in &parts {
                        result.push_str(&format!("{}", part));
                    }
                    self.push(Value::String(result.into()));
                }

                // ── Misc ────────────────────────────────────────────
                OpCode::Log => {
                    let flags = self.frames[frame_idx].read_byte();
                    let has_msg = flags & 0x04 != 0;
                    let has_sub = flags & 0x02 != 0;
                    let has_tag = flags & 0x01 != 0;

                    let msg = if has_msg { Some(self.pop()) } else { None };
                    let sub = if has_sub { Some(self.pop()) } else { None };
                    let tag = if has_tag { Some(self.pop()) } else { None };

                    let tag_str = match (&tag, &sub) {
                        (Some(t), Some(s)) => {
                            format!(
                                "[{}:{}]",
                                format!("{}", t).to_uppercase(),
                                format!("{}", s).to_uppercase()
                            )
                        }
                        (Some(t), None) => format!("[{}]", format!("{}", t).to_uppercase()),
                        _ => String::new(),
                    };
                    let msg_str = msg.map(|m| format!("{}", m));

                    // Timestamp (same algorithm as tree-walker evaluator)
                    let now = std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap_or_default();
                    let secs = now.as_secs();
                    let (hours, mins, seconds) = ((secs / 3600) % 24, (secs / 60) % 60, secs % 60);
                    let days = secs / 86400;
                    let z = days + 719468;
                    let era = z / 146097;
                    let doe = z - era * 146097;
                    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
                    let y = yoe + era * 400;
                    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
                    let mp = (5 * doy + 2) / 153;
                    let d = doy - (153 * mp + 2) / 5 + 1;
                    let m = if mp < 10 { mp + 3 } else { mp - 9 };
                    let year = if m <= 2 { y + 1 } else { y };
                    let timestamp = format!(
                        "{:04}-{:02}-{:02} {:02}:{:02}:{:02}",
                        year, m, d, hours, mins, seconds
                    );

                    let output = match (tag_str.is_empty(), &msg_str) {
                        (true, Some(msg)) => format!("{}: {}", timestamp, msg),
                        (false, Some(msg)) => format!("{}: {} {}", timestamp, tag_str, msg),
                        (false, None) => format!("{}: {}", timestamp, tag_str),
                        (true, None) => format!("{}: <empty log>", timestamp),
                    };

                    println!("{}", output);
                    self.push(Value::None);
                }

                OpCode::Unpack => {
                    let count = self.frames[frame_idx].read_byte() as usize;
                    let value = self.pop();
                    match value {
                        Value::Array(arr) => {
                            let borrowed = arr.borrow();
                            for i in 0..count {
                                let val = borrowed.get(i).cloned().unwrap_or(Value::None);
                                self.push(val);
                            }
                        }
                        Value::Tuple(t) => {
                            for i in 0..count {
                                let val = t.get(i).cloned().unwrap_or(Value::None);
                                self.push(val);
                            }
                        }
                        _ => {
                            return Err(self.runtime_error_hint(
                                &format!(
                                    "can only unpack arrays and tuples, got {}",
                                    value.type_name()
                                ),
                                "unpack syntax: `a, b := [1, 2]` or `a, b := (1, 2)`",
                            ));
                        }
                    }
                }

                OpCode::Main => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    if !self.is_main_context {
                        self.frames[frame_idx].ip += offset;
                    }
                }

                OpCode::Unless => {
                    let offset = self.frames[frame_idx].read_u16() as usize;
                    if !self.peek(0).is_truthy() {
                        self.frames[frame_idx].ip += offset;
                    }
                }

                // ── Iteration Support ───────────────────────────────
                OpCode::IterLen => {
                    // Stack: [iterable] -> [length]
                    // The iterable is accessed via GetLocal, so we just compute length
                    // from what's on top of stack and replace it.
                    let iterable = self.pop();
                    let len = match &iterable {
                        Value::Array(a) => a.borrow().len() as i64,
                        Value::String(s) => s.chars().count() as i64,
                        Value::Tuple(t) => t.len() as i64,
                        Value::Set(s) => s.borrow().len() as i64,
                        Value::Map(m) => m.borrow().len() as i64,
                        _ => {
                            return Err(self.runtime_error_hint(
                                &format!("cannot iterate over {}", iterable.type_name()),
                                "each loops work with arrays, tuples, strings, sets, and maps",
                            ));
                        }
                    };
                    self.push(Value::Integer(len));
                }

                OpCode::TypeWrap => {
                    let type_idx = self.frames[frame_idx].read_u16();
                    let type_name = self.frames[frame_idx].read_constant(type_idx).clone();
                    let value = self.pop();

                    if let Value::String(target) = type_name {
                        let result = self.type_wrap(&target, value)?;
                        self.push(result);
                    } else {
                        return Err(self.runtime_error("invalid type name in TypeWrap"));
                    }
                }

                OpCode::IterGet => {
                    let index = self.pop();
                    let iterable = self.pop();
                    let idx = match index {
                        Value::Integer(i) => i as usize,
                        _ => return Err(self.runtime_error("index must be integer")),
                    };
                    let val = match &iterable {
                        Value::Array(a) => a.borrow().get(idx).cloned().unwrap_or(Value::None),
                        Value::String(s) => s
                            .chars()
                            .nth(idx)
                            .map(|c| Value::String(c.to_string().into()))
                            .unwrap_or(Value::None),
                        Value::Tuple(t) => t.get(idx).cloned().unwrap_or(Value::None),
                        Value::Set(s) => s.borrow().get(idx).cloned().unwrap_or(Value::None),
                        Value::Map(m) => {
                            let borrowed = m.borrow();
                            if let Some((k, v)) = borrowed.get(idx) {
                                // For maps, yield (key, value) tuple
                                Value::Tuple(Rc::new(vec![k.clone(), v.clone()]))
                            } else {
                                Value::None
                            }
                        }
                        _ => return Err(self.runtime_error("cannot iterate")),
                    };
                    self.push(val);
                }

                // ── Method Call ──────────────────────────────────
                OpCode::MethodCall => {
                    let method_idx = self.frames[frame_idx].read_u16();
                    let arg_count = self.frames[frame_idx].read_byte();
                    self.handle_method_call(method_idx, arg_count)?;
                }

                OpCode::MethodCallNamed => {
                    let method_idx = self.frames[frame_idx].read_u16();
                    let arg_count = self.frames[frame_idx].read_byte() as usize;
                    let named_count = self.frames[frame_idx].read_byte() as usize;
                    let method_name = self.frames[frame_idx]
                        .read_constant(method_idx)
                        .clone();

                    let mut named_args = Vec::with_capacity(named_count);
                    for _ in 0..named_count {
                        let val = self.pop();
                        let name = self.pop();
                        if let Value::String(name_str) = name {
                            named_args.push((name_str.to_string(), val));
                        }
                    }
                    named_args.reverse();

                    if let Value::String(mname) = method_name {
                        self.call_method(&mname, arg_count, &named_args)?;
                    } else {
                        return Err(self.runtime_error("invalid method name"));
                    }
                }

                // ── Type Introspection ──────────────────────────
                OpCode::IsMut => {
                    let name_idx = self.frames[frame_idx].read_u16();
                    let name = self.frames[frame_idx].read_constant(name_idx).clone();
                    if let Value::String(var_name) = name {
                        let is_mut = self
                            .global_mutability
                            .get(var_name.as_ref())
                            .copied()
                            .unwrap_or(true); // untyped globals are mutable
                        self.push(Value::Boolean(is_mut));
                    } else {
                        self.push(Value::Boolean(true));
                    }
                }

                OpCode::IsType => {
                    let type_idx = self.frames[frame_idx].read_u16();
                    let type_name = self.frames[frame_idx].read_constant(type_idx).clone();
                    let value = self.pop();

                    if let Value::String(tname) = type_name {
                        let matches = match (tname.as_ref(), &value) {
                            ("int", Value::Integer(_)) => true,
                            ("float", Value::Float(_)) => true,
                            ("str", Value::String(_)) => true,
                            ("char", Value::Char(_)) => true,
                            ("bool", Value::Boolean(_)) => true,
                            ("array", Value::Array(_)) => true,
                            ("byte", Value::Byte(_)) => true,
                            ("uint", Value::Uint(_)) => true,
                            ("tuple", Value::Tuple(_)) => true,
                            ("map", Value::Map(_)) => true,
                            ("set", Value::Set(_)) => true,
                            ("None", Value::None) => true,
                            ("fun", Value::Closure(_)) => true,
                            ("fun", Value::Builtin(_)) => true,
                            (name, Value::StructInstance(inst)) => inst.struct_name == name,
                            _ => false,
                        };
                        self.push(Value::Boolean(matches));
                    } else {
                        self.push(Value::Boolean(false));
                    }
                }

                OpCode::IsTypeMut => {
                    let name_idx = self.frames[frame_idx].read_u16();
                    let name = self.frames[frame_idx].read_constant(name_idx).clone();
                    if let Value::String(var_name) = name {
                        let has_constraint = self
                            .global_type_constraints
                            .get(var_name.as_ref())
                            .is_some();
                        self.push(Value::Boolean(!has_constraint));
                    } else {
                        self.push(Value::Boolean(true));
                    }
                }

                OpCode::EnumDef => {
                    // EnumDefs flow through the constant pool / DefineGlobal; this
                    // opcode is reserved for future use (explicit runtime registration).
                    let _idx = self.frames[frame_idx].read_u16();
                }

                OpCode::MakeEnumVariantUnit => {
                    // Reserved — unit variants are constructed through GetField on
                    // an EnumDef receiver. This opcode exists so compilers can emit
                    // a direct unit construction in the future.
                    let variant_idx = self.frames[frame_idx].read_u16();
                    let variant_name = self.frames[frame_idx].read_constant(variant_idx).clone();
                    let enum_val = self.pop();
                    if let (Value::EnumDef(def), Value::String(vname)) = (&enum_val, &variant_name) {
                        let variant = def.variants.iter().find(|v| v.name.as_str() == vname.as_ref());
                        match variant {
                            Some(v) => {
                                if let crate::vm::value::VmEnumVariantKind::Unit(disc) = &v.kind {
                                    self.push(Value::EnumInstance(Rc::new(
                                        crate::vm::value::ObjEnumInstance {
                                            enum_name: def.name.clone(),
                                            variant_name: v.name.clone(),
                                            payload: crate::vm::value::VmEnumPayload::Unit(
                                                disc.clone(),
                                            ),
                                        },
                                    )));
                                } else {
                                    return Err(self.runtime_error(&format!(
                                        "variant {}.{} is not a unit variant",
                                        def.name, v.name
                                    )));
                                }
                            }
                            None => {
                                return Err(self.runtime_error(&format!(
                                    "enum {} has no variant '{}'",
                                    def.name, vname
                                )));
                            }
                        }
                    } else {
                        return Err(self.runtime_error(
                            "MakeEnumVariantUnit requires an EnumDef on the stack",
                        ));
                    }
                }

                OpCode::MakeEnumVariantTuple => {
                    let variant_idx = self.frames[frame_idx].read_u16();
                    let arg_count = self.frames[frame_idx].read_byte() as usize;
                    let variant_name = self.frames[frame_idx].read_constant(variant_idx).clone();
                    let start = self.stack.len() - arg_count;
                    let args: Vec<Value> = self.stack.drain(start..).collect();
                    let enum_val = self.pop();
                    let vname = if let Value::String(s) = &variant_name {
                        Rc::clone(s)
                    } else {
                        return Err(self.runtime_error("expected variant name string"));
                    };
                    if let Value::EnumDef(def) = &enum_val {
                        let variant = def.variants.iter().find(|v| v.name.as_str() == vname.as_ref());
                        match variant {
                            Some(v) => {
                                if let crate::vm::value::VmEnumVariantKind::Tuple(expected) = &v.kind {
                                    if expected.len() != args.len() {
                                        return Err(self.runtime_error(&format!(
                                            "tuple variant {}.{} expects {} arguments, got {}",
                                            def.name,
                                            v.name,
                                            expected.len(),
                                            args.len()
                                        )));
                                    }
                                    self.push(Value::EnumInstance(Rc::new(
                                        crate::vm::value::ObjEnumInstance {
                                            enum_name: def.name.clone(),
                                            variant_name: v.name.clone(),
                                            payload: crate::vm::value::VmEnumPayload::Tuple(args),
                                        },
                                    )));
                                } else {
                                    return Err(self.runtime_error(&format!(
                                        "variant {}.{} is not a tuple variant",
                                        def.name, v.name
                                    )));
                                }
                            }
                            None => {
                                return Err(self.runtime_error(&format!(
                                    "enum {} has no variant '{}'",
                                    def.name, vname
                                )));
                            }
                        }
                    } else {
                        return Err(self.runtime_error(
                            "MakeEnumVariantTuple requires an EnumDef on the stack",
                        ));
                    }
                }

                OpCode::MakeEnumVariantStruct => {
                    let variant_idx = self.frames[frame_idx].read_u16();
                    let field_count = self.frames[frame_idx].read_byte() as usize;
                    let variant_name = self.frames[frame_idx].read_constant(variant_idx).clone();
                    let start = self.stack.len() - field_count * 2;
                    let flat: Vec<Value> = self.stack.drain(start..).collect();
                    let mut fields: Vec<(String, Value)> = Vec::new();
                    for pair in flat.chunks(2) {
                        if let Value::String(fname) = &pair[0] {
                            fields.push((fname.to_string(), pair[1].clone()));
                        }
                    }
                    let enum_val = self.pop();
                    let vname = if let Value::String(s) = &variant_name {
                        Rc::clone(s)
                    } else {
                        return Err(self.runtime_error("expected variant name string"));
                    };
                    if let Value::EnumDef(def) = &enum_val {
                        let variant = def.variants.iter().find(|v| v.name.as_str() == vname.as_ref());
                        match variant {
                            Some(v) => {
                                if let crate::vm::value::VmEnumVariantKind::Struct(expected) = &v.kind {
                                    if expected.len() != fields.len() {
                                        return Err(self.runtime_error(&format!(
                                            "struct variant {}.{} expects {} fields, got {}",
                                            def.name,
                                            v.name,
                                            expected.len(),
                                            fields.len()
                                        )));
                                    }
                                    for (fname, _) in &fields {
                                        if !expected.contains(fname) {
                                            return Err(self.runtime_error(&format!(
                                                "struct variant {}.{} has no field '{}'",
                                                def.name, v.name, fname
                                            )));
                                        }
                                    }
                                    self.push(Value::EnumInstance(Rc::new(
                                        crate::vm::value::ObjEnumInstance {
                                            enum_name: def.name.clone(),
                                            variant_name: v.name.clone(),
                                            payload: crate::vm::value::VmEnumPayload::Struct(
                                                fields,
                                            ),
                                        },
                                    )));
                                } else {
                                    return Err(self.runtime_error(&format!(
                                        "variant {}.{} is not a struct variant",
                                        def.name, v.name
                                    )));
                                }
                            }
                            None => {
                                return Err(self.runtime_error(&format!(
                                    "enum {} has no variant '{}'",
                                    def.name, vname
                                )));
                            }
                        }
                    } else {
                        return Err(self.runtime_error(
                            "MakeEnumVariantStruct requires an EnumDef on the stack",
                        ));
                    }
                }
            }
        }
    }

    // ── Arithmetic Helpers ──────────────────────────────────────────────

    pub(crate) fn binary_add(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l + r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(*l as f64 + r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l + *r as f64)),
            (Value::String(l), Value::String(r)) => {
                let mut s = String::with_capacity(l.len() + r.len());
                s.push_str(l);
                s.push_str(r);
                Ok(Value::String(s.into()))
            }
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Integer(*l as i64 + *r as i64)),
            (Value::Byte(l), Value::Integer(r)) => Ok(Value::Integer(*l as i64 + r)),
            (Value::Integer(l), Value::Byte(r)) => Ok(Value::Integer(l + *r as i64)),
            (Value::Uint(l), Value::Uint(r)) => Ok(Value::Uint(l.wrapping_add(*r))),
            (Value::Uint(l), Value::Integer(r)) => Ok(Value::Integer(*l as i64 + r)),
            (Value::Integer(l), Value::Uint(r)) => Ok(Value::Integer(l + *r as i64)),
            (Value::Byte(l), Value::Float(r)) => Ok(Value::Float(*l as f64 + r)),
            (Value::Float(l), Value::Byte(r)) => Ok(Value::Float(l + *r as f64)),
            (Value::Uint(l), Value::Float(r)) => Ok(Value::Float(*l as f64 + r)),
            (Value::Float(l), Value::Uint(r)) => Ok(Value::Float(l + *r as f64)),
            (Value::String(l), Value::Char(r)) => {
                let mut s = String::with_capacity(l.len() + r.len_utf8());
                s.push_str(l);
                s.push(*r);
                Ok(Value::String(s.into()))
            }
            (Value::Char(l), Value::String(r)) => {
                let mut s = String::with_capacity(l.len_utf8() + r.len());
                s.push(*l);
                s.push_str(r);
                Ok(Value::String(s.into()))
            }
            (Value::Char(l), Value::Char(r)) => {
                let mut s = String::with_capacity(l.len_utf8() + r.len_utf8());
                s.push(*l);
                s.push(*r);
                Ok(Value::String(s.into()))
            }
            (Value::Tuple(l), Value::Tuple(r)) => {
                let mut new = (**l).clone();
                new.extend((**r).clone());
                Ok(Value::Tuple(Rc::new(new)))
            }
            _ => Err(self.runtime_error_hint(
                &format!("type mismatch: {} + {}", a.type_name(), b.type_name()),
                "operands must be compatible numeric types, strings, or collections",
            )),
        }
    }

    pub(crate) fn binary_sub(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l - r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(*l as f64 - r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l - *r as f64)),
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Integer(*l as i64 - *r as i64)),
            (Value::Byte(l), Value::Integer(r)) => Ok(Value::Integer(*l as i64 - r)),
            (Value::Integer(l), Value::Byte(r)) => Ok(Value::Integer(l - *r as i64)),
            (Value::Uint(l), Value::Uint(r)) => {
                if r > l {
                    Err(self.runtime_error("unsigned integer underflow"))
                } else {
                    Ok(Value::Uint(l - r))
                }
            }
            (Value::Uint(l), Value::Integer(r)) => Ok(Value::Integer(*l as i64 - r)),
            (Value::Integer(l), Value::Uint(r)) => Ok(Value::Integer(l - *r as i64)),
            _ => Err(self.runtime_error_hint(
                &format!("type mismatch: {} - {}", a.type_name(), b.type_name()),
                "operands must be compatible numeric types",
            )),
        }
    }

    pub(crate) fn binary_mul(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l * r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(*l as f64 * r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l * *r as f64)),
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Integer(*l as i64 * *r as i64)),
            (Value::Uint(l), Value::Uint(r)) => Ok(Value::Uint(l.wrapping_mul(*r))),
            _ => Err(self.runtime_error_hint(
                &format!("type mismatch: {} * {}", a.type_name(), b.type_name()),
                "operands must be compatible numeric types",
            )),
        }
    }

    pub(crate) fn binary_div(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => {
                if *r == 0 {
                    Err(self.runtime_error("division by zero"))
                } else {
                    Ok(Value::Integer(l / r))
                }
            }
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l / r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(*l as f64 / r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l / *r as f64)),
            (Value::Uint(l), Value::Uint(r)) => {
                if *r == 0 {
                    Err(self.runtime_error("division by zero"))
                } else {
                    Ok(Value::Uint(l / r))
                }
            }
            _ => Err(self.runtime_error_hint(
                &format!("type mismatch: {} / {}", a.type_name(), b.type_name()),
                "operands must be compatible numeric types",
            )),
        }
    }

    pub(crate) fn binary_mod(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => {
                if *r == 0 {
                    Err(self.runtime_error("modulo by zero"))
                } else {
                    Ok(Value::Integer(l % r))
                }
            }
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l % r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(*l as f64 % r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l % *r as f64)),
            (Value::Uint(l), Value::Uint(r)) => {
                if *r == 0 {
                    Err(self.runtime_error("modulo by zero"))
                } else {
                    Ok(Value::Uint(l % r))
                }
            }
            _ => Err(self.runtime_error_hint(
                &format!("type mismatch: {} % {}", a.type_name(), b.type_name()),
                "operands must be compatible numeric types",
            )),
        }
    }

    // ── Comparison Helpers ──────────────────────────────────────────────

    pub(crate) fn compare_less(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l < r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l < r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Boolean((*l as f64) < *r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Boolean(*l < (*r as f64))),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l < r)),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l < r)),
            (Value::Uint(l), Value::Uint(r)) => Ok(Value::Boolean(l < r)),
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Boolean(l < r)),
            _ => Err(self.runtime_error_hint(
                &format!("cannot compare {} < {}", a.type_name(), b.type_name()),
                "comparison requires matching numeric types or chars",
            )),
        }
    }

    pub(crate) fn compare_less_equal(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l <= r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l <= r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Boolean((*l as f64) <= *r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Boolean(*l <= (*r as f64))),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l <= r)),
            (Value::Uint(l), Value::Uint(r)) => Ok(Value::Boolean(l <= r)),
            _ => Err(self.runtime_error_hint(
                &format!("cannot compare {} <= {}", a.type_name(), b.type_name()),
                "comparison requires matching numeric types or chars",
            )),
        }
    }

    pub(crate) fn compare_greater(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l > r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l > r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Boolean((*l as f64) > *r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Boolean(*l > (*r as f64))),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l > r)),
            (Value::Uint(l), Value::Uint(r)) => Ok(Value::Boolean(l > r)),
            _ => Err(self.runtime_error_hint(
                &format!("cannot compare {} > {}", a.type_name(), b.type_name()),
                "comparison requires matching numeric types or chars",
            )),
        }
    }

    pub(crate) fn compare_greater_equal(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l >= r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l >= r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Boolean((*l as f64) >= *r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Boolean(*l >= (*r as f64))),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l >= r)),
            (Value::Uint(l), Value::Uint(r)) => Ok(Value::Boolean(l >= r)),
            _ => Err(self.runtime_error_hint(
                &format!("cannot compare {} >= {}", a.type_name(), b.type_name()),
                "comparison requires matching numeric types or chars",
            )),
        }
    }

    // ── Function Calling ────────────────────────────────────────────────

    pub(crate) fn call_value(
        &mut self,
        arg_count: usize,
        named_args: &[(String, Value)],
    ) -> Result<(), VMError> {
        let callee_idx = self.stack.len() - 1 - arg_count;
        let callee = self.stack[callee_idx].clone();

        match callee {
            Value::Closure(closure) => self.call_closure(closure, arg_count, named_args, None),
            Value::Builtin(func) => {
                let start = self.stack.len() - arg_count;
                let args: Vec<Value> = self.stack.drain(start..).collect();
                self.pop(); // pop the builtin itself

                if !named_args.is_empty() {
                    return Err(self.runtime_error(
                        "named arguments are not supported for built-in functions",
                    ));
                }

                let result = func(args);
                if let Value::Error(ref msg) = result {
                    return Err(VMError {
                        message: msg.to_string(),
                        line: self.current_line(),
                    });
                }
                self.push(result);
                Ok(())
            }
            Value::StructDef(def) => {
                // Struct constructor call (positional args)
                let start = self.stack.len() - arg_count;
                let args: Vec<Value> = self.stack.drain(start..).collect();
                self.pop(); // pop the struct def

                let mut fields = HashMap::new();
                for (i, (field_name, field_type, _)) in def.fields.iter().enumerate() {
                    let val = args
                        .get(i)
                        .cloned()
                        .unwrap_or_else(|| Self::zero_value_for_type(field_type));
                    fields.insert(field_name.clone(), val);
                }

                self.push(Value::StructInstance(Rc::new(ObjStructInstance {
                    struct_name: def.name.clone(),
                    fields: RefCell::new(fields),
                })));
                Ok(())
            }
            _ => Err(self.runtime_error_hint(
                &format!("cannot call {}", callee.type_name()),
                "only functions, closures, builtins, and struct constructors are callable",
            )),
        }
    }

    /// Fast positional-only closure call used by the JIT `Call` helper.
    /// Returns `Ok(false)` when the callee is not a closure so the caller
    /// can fall back to the generic `call_value` path for builtins,
    /// constructors, and future callable forms.
    #[cfg_attr(not(feature = "jit"), allow(dead_code))]
    pub(crate) fn call_closure_from_stack(&mut self, arg_count: usize) -> Result<bool, VMError> {
        let callee_idx = self.stack.len() - 1 - arg_count;
        let Value::Closure(closure) = &self.stack[callee_idx] else {
            return Ok(false);
        };
        let closure = Rc::clone(closure);
        self.call_closure(closure, arg_count, &[], None)?;
        Ok(true)
    }

    /// Call a method on an object. Stack: [instance, arg1, ..., argN]
    pub(crate) fn call_method(
        &mut self,
        method_name: &str,
        arg_count: usize,
        named_args: &[(String, Value)],
    ) -> Result<(), VMError> {
        let instance_idx = self.stack.len() - 1 - arg_count;
        let instance = self.stack[instance_idx].clone();

        match &instance {
            Value::StructInstance(inst) => {
                // Check instance fields first (for callable fields)
                let field_val = inst.fields.borrow().get(method_name).cloned();
                if let Some(Value::Closure(closure)) = field_val {
                    // Callable field — treat as regular call
                    self.stack[instance_idx] = Value::Closure(Rc::clone(&closure));
                    return self.call_closure(closure, arg_count, named_args, None);
                }

                // Look up method on struct def (with inheritance)
                let method = self.find_struct_method(&inst.struct_name, method_name)?;

                if let Value::Closure(closure) = method {
                    // Method has implicit `self` as first param.
                    // Rearrange stack: [instance, arg1, ...] → [closure, instance, arg1, ...]
                    // The instance becomes the `self` argument.
                    self.stack[instance_idx] = Value::Closure(Rc::clone(&closure));
                    // Insert instance as first arg (self) right after the closure
                    self.stack.insert(instance_idx + 1, instance);
                    return self.call_closure(closure, arg_count + 1, named_args, None);
                }

                Err(self.runtime_error_hint(
                    &format!("method '{}' not found on {}", method_name, inst.struct_name),
                    "check the struct definition for available methods",
                ))
            }
            Value::Array(_)
            | Value::String(_)
            | Value::Map(_)
            | Value::Set(_)
            | Value::Tuple(_) => {
                // Built-in method syntax: collection.method(args)
                // Convert to builtin call: __method(collection, args)
                let builtin_name = format!("__{}", method_name);
                if let Some(builtin) = self.globals.get(&builtin_name).cloned() {
                    // Rearrange: [instance, arg1, ...] → [builtin, instance, arg1, ...]
                    self.stack[instance_idx] = builtin;
                    self.stack.insert(instance_idx + 1, instance);
                    return self.call_value(arg_count + 1, named_args);
                }
                // Try without __ prefix
                if let Some(builtin) = self.globals.get(method_name).cloned() {
                    self.stack[instance_idx] = builtin;
                    self.stack.insert(instance_idx + 1, instance);
                    return self.call_value(arg_count + 1, named_args);
                }
                Err(self.runtime_error_hint(
                    &format!(
                        "method '{}' not found on {}",
                        method_name,
                        instance.type_name()
                    ),
                    "check available built-in methods for this type",
                ))
            }
            Value::Module(m) => {
                // Module method call: module.func(args)
                if let Some(func) = m.globals.get(method_name).cloned() {
                    self.stack[instance_idx] = func.clone();
                    // Pass the module's globals so the function can access
                    // module-scoped variables (e.g. `io` inside `toml`).
                    if let Value::Closure(closure) = func {
                        return self.call_closure(
                            closure,
                            arg_count,
                            named_args,
                            Some(Rc::clone(&m.globals)),
                        );
                    }
                    return self.call_value(arg_count, named_args);
                }
                Err(self.runtime_error_hint(
                    &format!("module '{}' has no function '{}'", m.name, method_name),
                    "check the module's public API for available functions",
                ))
            }
            Value::EnumDef(def) => {
                // Tuple-variant construction: `EnumName.Variant(args)`.
                let variant = def
                    .variants
                    .iter()
                    .find(|v| v.name == method_name)
                    .cloned();
                match variant {
                    Some(v) => match v.kind {
                        crate::vm::value::VmEnumVariantKind::Tuple(expected) => {
                            if expected.len() != arg_count {
                                return Err(self.runtime_error(&format!(
                                    "tuple variant {}.{} expects {} arguments, got {}",
                                    def.name,
                                    v.name,
                                    expected.len(),
                                    arg_count
                                )));
                            }
                            let start = self.stack.len() - arg_count;
                            let args: Vec<Value> = self.stack.drain(start..).collect();
                            self.pop(); // receiver (EnumDef)
                            self.push(Value::EnumInstance(Rc::new(
                                crate::vm::value::ObjEnumInstance {
                                    enum_name: def.name.clone(),
                                    variant_name: v.name.clone(),
                                    payload: crate::vm::value::VmEnumPayload::Tuple(args),
                                },
                            )));
                            Ok(())
                        }
                        crate::vm::value::VmEnumVariantKind::Unit(_) => Err(self.runtime_error_hint(
                            &format!(
                                "unit variant {}.{} cannot be called",
                                def.name, v.name
                            ),
                            "access unit variants without parentheses",
                        )),
                        crate::vm::value::VmEnumVariantKind::Struct(_) => {
                            Err(self.runtime_error_hint(
                                &format!(
                                    "struct variant {}.{} must be constructed with braces",
                                    def.name, v.name
                                ),
                                "use EnumName.Variant { field: value }",
                            ))
                        }
                    },
                    None => Err(self.runtime_error(&format!(
                        "enum {} has no variant '{}'",
                        def.name, method_name
                    ))),
                }
            }
            _ => Err(self.runtime_error_hint(
                &format!(
                    "cannot call method '{}' on {}",
                    method_name,
                    instance.type_name()
                ),
                "methods can only be called on struct instances, collections, and modules",
            )),
        }
    }

    /// Look up a method on a struct def, following the inheritance chain.
    fn find_struct_method(&self, struct_name: &str, method_name: &str) -> Result<Value, VMError> {
        let mut current = struct_name.to_string();
        loop {
            if let Some(Value::StructDef(def)) = self.globals.get(&current) {
                let methods = def.methods.borrow();
                if let Some(method) = methods.get(method_name) {
                    return Ok(method.clone());
                }
                if let Some(parent) = &def.parent {
                    current = parent.clone();
                    continue;
                }
            }
            return Err(self.runtime_error_hint(
                &format!("method '{}' not found on {}", method_name, struct_name),
                "check the struct definition and its parent for available methods",
            ));
        }
    }

    fn call_closure(
        &mut self,
        closure: Rc<ObjClosure>,
        arg_count: usize,
        named_args: &[(String, Value)],
        module_globals: Option<Rc<HashMap<String, Value>>>,
    ) -> Result<(), VMError> {
        let expected = closure.function.arity as usize;

        // Bump the JIT's hot-function counter. Actual JIT invocation is
        // attempted after the frame is pushed (see below) so the JIT helper
        // can drive `execute_until` for its own frame.
        closure
            .call_count
            .set(closure.call_count.get().saturating_add(1));

        // Handle named arguments: fill in any missing positional args
        if !named_args.is_empty() {
            // Extend stack to full arity if needed
            let total_provided = arg_count + named_args.len();
            if total_provided > expected && !closure.function.params.iter().any(|p| p.optional) {
                return Err(self.runtime_error_hint(
                    &format!("expected {} arguments but got {}", expected, total_provided),
                    "check the function signature for required and optional parameters",
                ));
            }

            // Fill positional slots up to arity with None (will be overwritten by named args)
            for _ in arg_count..expected {
                self.push(Value::None);
            }

            // Place named args into their correct slots
            let slot_base = self.stack.len() - expected;
            for (name, val) in named_args {
                if let Some(idx) = closure.function.params.iter().position(|p| p.name == *name) {
                    self.stack[slot_base + idx] = val.clone();
                }
            }
        } else if arg_count < expected {
            // Fill missing args with None (for optional/default params)
            for _ in arg_count..expected {
                self.push(Value::None);
            }
        }

        if self.frames.len() >= FRAMES_MAX {
            return Err(self.runtime_error_hint(
                "stack overflow",
                "check for infinite recursion or deeply nested calls",
            ));
        }

        let slot_offset = self.stack.len() - expected - 1; // -1 for the function itself

        // Inherit module globals from the current frame if not explicitly provided,
        // so nested calls within a module function retain module scope access.
        let inherited_mg =
            module_globals.or_else(|| self.frames.last().and_then(|f| f.module_globals.clone()));

        // Remember the pre-call depth so the JIT helper (if we enter it)
        // can drive `execute_until` for exactly this one activation.
        let stop_depth = self.frames.len();

        self.frames.push(CallFrame {
            closure: Rc::clone(&closure),
            ip: 0,
            slot_offset,
            module_globals: inherited_mg,
        });

        // Ask the JIT: is this function compiled (or can it be)? If so,
        // invoke it; it will drive `execute_until(stop_depth)` for this one
        // frame and leave the return value on the stack. A bailout means
        // we should just return — the outer `execute_until` will pick up
        // the pushed frame and dispatch as usual.
        let count = closure.call_count.get();
        let thunk = match closure.jit_state.get() {
            1 => closure.jit_thunk.get(),
            2 => None,
            _ => {
                let thunk = self
                    .jit
                    .maybe_compile_loop_entry_thunk(&closure.function)
                    .or_else(|| self.jit.maybe_compile_thunk(&closure.function, count));
                if let Some(thunk) = thunk {
                    closure.jit_thunk.set(Some(thunk));
                    closure.jit_state.set(1);
                } else if count >= self.jit.threshold() {
                    closure.jit_state.set(2);
                }
                thunk
            }
        };

        if let Some(thunk) = thunk {
            let vm_ptr = self as *mut VM;
            let exit = unsafe { self.jit.invoke_thunk(vm_ptr, thunk, stop_depth) };
            match exit {
                crate::jit::JitExit::Returned => {}
                crate::jit::JitExit::Bailout => {
                    closure.jit_thunk.set(None);
                    closure.jit_state.set(2);
                }
                crate::jit::JitExit::RuntimeError(err) => return Err(err),
            }
        }

        Ok(())
    }

    // ── Upvalue Management ──────────────────────────────────────────────

    pub(crate) fn capture_upvalue(&mut self, stack_slot: usize) -> Rc<RefCell<Upvalue>> {
        // Check if we already have an open upvalue for this slot
        for uv in &self.open_upvalues {
            if let Upvalue::Open(slot) = &*uv.borrow() {
                if *slot == stack_slot {
                    return Rc::clone(uv);
                }
            }
        }

        let upvalue = Rc::new(RefCell::new(Upvalue::Open(stack_slot)));
        self.open_upvalues.push(Rc::clone(&upvalue));
        upvalue
    }

    pub(crate) fn close_upvalues(&mut self, last: usize) {
        let mut i = 0;
        while i < self.open_upvalues.len() {
            let should_close = {
                let uv = self.open_upvalues[i].borrow();
                matches!(&*uv, Upvalue::Open(slot) if *slot >= last)
            };

            if should_close {
                let uv = self.open_upvalues.remove(i);
                let value = {
                    let borrow = uv.borrow();
                    if let Upvalue::Open(slot) = &*borrow {
                        self.stack[*slot].clone()
                    } else {
                        continue;
                    }
                };
                *uv.borrow_mut() = Upvalue::Closed(value);
            } else {
                i += 1;
            }
        }
    }

    // ── Index Operations ────────────────────────────────────────────────

    pub(crate) fn eval_index(&self, collection: Value, index: Value) -> Result<Value, VMError> {
        match (&collection, &index) {
            (Value::Array(arr), Value::Integer(i)) => {
                let borrowed = arr.borrow();
                let idx = if *i < 0 {
                    (borrowed.len() as i64 + i) as usize
                } else {
                    *i as usize
                };
                Ok(borrowed.get(idx).cloned().unwrap_or(Value::None))
            }
            (Value::String(s), Value::Integer(i)) => {
                let idx = if *i < 0 {
                    (s.len() as i64 + i) as usize
                } else {
                    *i as usize
                };
                Ok(s.chars()
                    .nth(idx)
                    .map(|c| Value::String(c.to_string().into()))
                    .unwrap_or(Value::None))
            }
            (Value::Tuple(t), Value::Integer(i)) => {
                let idx = if *i < 0 {
                    (t.len() as i64 + i) as usize
                } else {
                    *i as usize
                };
                Ok(t.get(idx).cloned().unwrap_or(Value::None))
            }
            (Value::Map(m), _) => {
                let borrowed = m.borrow();
                for (k, v) in borrowed.iter() {
                    if k == &index {
                        return Ok(v.clone());
                    }
                }
                Ok(Value::None)
            }
            _ => Err(self.runtime_error_hint(
                &format!("cannot index {} with {}", collection.type_name(), index.type_name()),
                "arrays and tuples use <int> indices, maps use key values, strings use <int> indices",
            )),
        }
    }

    fn eval_index_assign(
        &self,
        collection: Value,
        index: Value,
        value: Value,
    ) -> Result<(), VMError> {
        match (&collection, &index) {
            (Value::Array(arr), Value::Integer(i)) => {
                let mut borrowed = arr.borrow_mut();
                let idx = if *i < 0 {
                    (borrowed.len() as i64 + i) as usize
                } else {
                    *i as usize
                };
                if idx < borrowed.len() {
                    borrowed[idx] = value;
                }
                Ok(())
            }
            (Value::Map(m), _) => {
                let mut borrowed = m.borrow_mut();
                for entry in borrowed.iter_mut() {
                    if entry.0 == index {
                        entry.1 = value;
                        return Ok(());
                    }
                }
                borrowed.push((index, value));
                Ok(())
            }
            _ => Err(self.runtime_error_hint(
                &format!("cannot assign to index on {}", collection.type_name()),
                "index assignment is supported on arrays and maps",
            )),
        }
    }

    fn eval_slice(
        &self,
        collection: Value,
        start: Option<Value>,
        end: Option<Value>,
    ) -> Result<Value, VMError> {
        match &collection {
            Value::Array(arr) => {
                let borrowed = arr.borrow();
                let len = borrowed.len() as i64;
                let s = match start {
                    Some(Value::Integer(i)) => {
                        if i < 0 {
                            (len + i) as usize
                        } else {
                            i as usize
                        }
                    }
                    None => 0,
                    _ => return Err(self.runtime_error("slice index must be integer")),
                };
                let e = match end {
                    Some(Value::Integer(i)) => {
                        if i < 0 {
                            (len + i) as usize
                        } else {
                            i as usize
                        }
                    }
                    None => len as usize,
                    _ => return Err(self.runtime_error("slice index must be integer")),
                };
                let s = s.min(len as usize);
                let e = e.min(len as usize);
                let sliced: Vec<Value> = if s <= e {
                    borrowed[s..e].to_vec()
                } else {
                    Vec::new()
                };
                Ok(Value::Array(Rc::new(RefCell::new(sliced))))
            }
            Value::String(s) => {
                let len = s.len() as i64;
                let start_idx = match start {
                    Some(Value::Integer(i)) => {
                        if i < 0 {
                            (len + i) as usize
                        } else {
                            i as usize
                        }
                    }
                    None => 0,
                    _ => return Err(self.runtime_error("slice index must be integer")),
                };
                let end_idx = match end {
                    Some(Value::Integer(i)) => {
                        if i < 0 {
                            (len + i) as usize
                        } else {
                            i as usize
                        }
                    }
                    None => len as usize,
                    _ => return Err(self.runtime_error("slice index must be integer")),
                };
                let start_idx = start_idx.min(s.len());
                let end_idx = end_idx.min(s.len());
                if start_idx <= end_idx {
                    Ok(Value::String(s[start_idx..end_idx].into()))
                } else {
                    Ok(Value::String("".into()))
                }
            }
            _ => Err(self.runtime_error_hint(
                &format!("cannot slice {}", collection.type_name()),
                "slicing is supported on arrays, strings, and tuples: collection[start:end]",
            )),
        }
    }

    // ── Module System ───────────────────────────────────────────────────

    fn import_module(&mut self, path_str: &str, selective_names: &[String]) -> Result<(), VMError> {
        // Resolve the module path
        let module_path = self.resolve_module_path(path_str)?;

        // Check cache
        if let Some(cached) = self.module_cache.get(&module_path).cloned() {
            return self.bind_module(&cached, path_str, selective_names);
        }

        // Check circular imports
        if self.import_stack.contains(&module_path) {
            return Err(self.runtime_error_hint(
                &format!("circular import: {}", module_path.display()),
                "two modules are importing each other — restructure to break the cycle",
            ));
        }

        // Read, lex, parse, compile, and execute the module
        let source = std::fs::read_to_string(&module_path).map_err(|e| {
            self.runtime_error(&format!("cannot read module '{}': {}", path_str, e))
        })?;

        let lexer = crate::lexer::Lexer::new(&source);
        let mut parser = crate::parser::Parser::new(lexer, &source);
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            return Err(self.runtime_error(&format!(
                "parse errors in module '{}': {}",
                path_str,
                parser.format_errors()
            )));
        }

        let compiler = crate::compiler::Compiler::new();
        let function = compiler.compile(&program).map_err(|errors| {
            let msgs: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
            self.runtime_error(&format!(
                "compile errors in module '{}': {}",
                path_str,
                msgs.join("; ")
            ))
        })?;

        // Execute in a sub-VM
        let mut sub_vm = VM::new();
        sub_vm.set_source(&source);
        sub_vm.set_file(module_path.clone());
        sub_vm.is_main_context = false;
        self.import_stack.push(module_path.clone());

        let _result = sub_vm.run(function).map_err(|e| {
            self.runtime_error(&format!("error in module '{}': {}", path_str, e.message))
        })?;

        self.import_stack.pop();

        // Create module from sub-VM globals (excluding builtins)
        let module = Rc::new(ObjModule {
            name: path_str.to_string(),
            globals: Rc::new(sub_vm.globals),
        });

        self.module_cache.insert(module_path, Rc::clone(&module));

        self.bind_module(&module, path_str, selective_names)
    }

    fn bind_module(
        &mut self,
        module: &Rc<ObjModule>,
        path_str: &str,
        selective_names: &[String],
    ) -> Result<(), VMError> {
        if selective_names.is_empty() {
            // Namespace import
            let name = path_str.rsplit('/').next().unwrap_or(path_str).to_string();
            self.globals.insert(name, Value::Module(Rc::clone(module)));
        } else {
            // Selective import
            for name in selective_names {
                if let Some(val) = module.globals.get(name) {
                    self.globals.insert(name.clone(), val.clone());
                }
            }
        }
        Ok(())
    }

    fn resolve_module_path(&self, path_str: &str) -> Result<PathBuf, VMError> {
        // Relative import
        if path_str.starts_with("./") || path_str.starts_with("../") {
            if let Some(current) = &self.current_file {
                let base = current.parent().unwrap_or(current);
                let resolved = base.join(path_str).with_extension("oxi");
                if resolved.exists() {
                    return resolved
                        .canonicalize()
                        .map_err(|e| self.runtime_error(&format!("cannot resolve path: {}", e)));
                }
            }
        }

        // Stdlib import
        let stdlib = self.stdlib_path.join(path_str).with_extension("oxi");
        if stdlib.exists() {
            return stdlib
                .canonicalize()
                .map_err(|e| self.runtime_error(&format!("cannot resolve stdlib path: {}", e)));
        }

        // Relative to current file (without ./ prefix)
        if let Some(current) = &self.current_file {
            let base = current.parent().unwrap_or(current);
            let resolved = base.join(path_str).with_extension("oxi");
            if resolved.exists() {
                return resolved
                    .canonicalize()
                    .map_err(|e| self.runtime_error(&format!("cannot resolve path: {}", e)));
            }
        }

        Err(self.runtime_error_hint(
            &format!("module not found: {}", path_str),
            "check that the module file exists and the path is correct",
        ))
    }

    // ── Type Wrap / Conversion ──────────────────────────────────────────

    /// Implements `<type<T>>(value)` — the angle-bracket type wrap form.
    /// Handles Error/Value union wrapping, type conversion, etc.
    /// Return the zero/default value for a type name.
    fn zero_value_for_type(type_name: &str) -> Value {
        match type_name {
            "INTEGER" => Value::Integer(0),
            "FLOAT" => Value::Float(0.0),
            "STRING" => Value::String("".into()),
            "BOOLEAN" => Value::Boolean(false),
            "CHAR" => Value::Char('\0'),
            "BYTE" => Value::Byte(0),
            "UINT" => Value::Uint(0),
            "ARRAY" => Value::Array(Rc::new(RefCell::new(Vec::new()))),
            "TUPLE" => Value::Tuple(Rc::new(Vec::new())),
            "MAP" => Value::Map(Rc::new(RefCell::new(Vec::new()))),
            "SET" => Value::Set(Rc::new(RefCell::new(Vec::new()))),
            "NONE" => Value::None,
            "GENERIC" => Value::None,
            _ => Value::None,
        }
    }

    pub(crate) fn type_wrap(&self, target: &str, value: Value) -> Result<Value, VMError> {
        // Check for Error/Value union pattern: "VALUE || ERROR" or "ERROR || VALUE"
        if target.contains(" || ") {
            let parts: Vec<&str> = target.split(" || ").collect();
            let has_value = parts.iter().any(|p| *p == "VALUE");
            let has_error = parts.iter().any(|p| p.starts_with("ERROR"));
            if has_value && has_error {
                // Error/Value union: wrap based on whether value is an error
                if let Some((tag, msg)) = Self::error_info_from_value(&value) {
                    // Find the error tag from the target spec
                    let default_tag = parts.iter().find_map(|p| {
                        if p.starts_with("ERROR<") && p.ends_with('>') {
                            Some(p[6..p.len() - 1].to_string())
                        } else {
                            None
                        }
                    });
                    let final_tag = tag.or(default_tag);
                    return Ok(Value::ErrorValue {
                        msg: msg.into(),
                        tag: final_tag.map(|t| t.into()),
                    });
                } else {
                    return Ok(Value::Wrapped(Rc::new(value)));
                }
            }
            // General union: try each type
            let actual = value.effective_type_name();
            for member in parts {
                if member == &actual || member == "GENERIC" {
                    return Ok(value);
                }
                if let Ok(converted) = self.convert_to_type(&value, member) {
                    return Ok(converted);
                }
            }
            return Err(self.runtime_error(&format!("cannot convert {} to {}", actual, target)));
        }

        // Single type target
        match target {
            "GENERIC" => Ok(value),
            "VALUE" => Ok(Value::Wrapped(Rc::new(value))),
            "ERROR" => {
                if let Some((tag, msg)) = Self::error_info_from_value(&value) {
                    Ok(Value::ErrorValue {
                        msg: msg.into(),
                        tag: tag.map(|t| t.into()),
                    })
                } else {
                    Err(self
                        .runtime_error(&format!("cannot convert {} to ERROR", value.type_name())))
                }
            }
            t if t.starts_with("ERROR<") && t.ends_with('>') => {
                let wanted_tag = &t[6..t.len() - 1];
                if let Some((tag, msg)) = Self::error_info_from_value(&value) {
                    if tag.as_deref() == Some(wanted_tag) {
                        Ok(Value::ErrorValue {
                            msg: msg.into(),
                            tag: Some(wanted_tag.into()),
                        })
                    } else {
                        Err(self.runtime_error(&format!(
                            "cannot convert ERROR<{}> to ERROR<{}>",
                            tag.unwrap_or_default(),
                            wanted_tag
                        )))
                    }
                } else {
                    Err(self.runtime_error(&format!(
                        "cannot convert {} to {}",
                        value.type_name(),
                        t
                    )))
                }
            }
            _ => self.convert_to_type(&value, target),
        }
    }

    /// Extract error info from a value (tag, message).
    fn error_info_from_value(value: &Value) -> Option<(Option<String>, String)> {
        match value {
            Value::ErrorValue { msg, tag } => {
                Some((tag.as_ref().map(|t| t.to_string()), msg.to_string()))
            }
            Value::Error(msg) => Some((None, msg.to_string())),
            _ => None,
        }
    }

    /// Convert a value to a target type.
    fn convert_to_type(&self, value: &Value, target: &str) -> Result<Value, VMError> {
        match target {
            "GENERIC" => Ok(value.clone()),
            "NONE" => {
                match value {
                    Value::None => Ok(Value::None),
                    _ => Err(self
                        .runtime_error(&format!("cannot convert {} to NONE", value.type_name()))),
                }
            }
            "INTEGER" => match value {
                Value::Integer(_) => Ok(value.clone()),
                Value::Float(f) => Ok(Value::Integer(*f as i64)),
                Value::String(s) => s.parse::<i64>().map(Value::Integer).map_err(|_| {
                    self.runtime_error(&format!("cannot convert STRING \"{}\" to INTEGER", s))
                }),
                Value::Boolean(b) => Ok(Value::Integer(if *b { 1 } else { 0 })),
                Value::Byte(b) => Ok(Value::Integer(*b as i64)),
                Value::Uint(u) => Ok(Value::Integer(*u as i64)),
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => {
                    Err(self
                        .runtime_error(&format!("cannot convert {} to INTEGER", value.type_name())))
                }
            },
            "FLOAT" => {
                match value {
                    Value::Float(_) => Ok(value.clone()),
                    Value::Integer(n) => Ok(Value::Float(*n as f64)),
                    Value::String(s) => s.parse::<f64>().map(Value::Float).map_err(|_| {
                        self.runtime_error(&format!("cannot convert STRING \"{}\" to FLOAT", s))
                    }),
                    Value::Boolean(b) => Ok(Value::Float(if *b { 1.0 } else { 0.0 })),
                    Value::Byte(b) => Ok(Value::Float(*b as f64)),
                    Value::Uint(u) => Ok(Value::Float(*u as f64)),
                    _ => Err(self
                        .runtime_error(&format!("cannot convert {} to FLOAT", value.type_name()))),
                }
            }
            "STRING" => Ok(Value::String(format!("{}", value).into())),
            "BOOLEAN" => match value {
                Value::Boolean(_) => Ok(value.clone()),
                _ => Ok(Value::Boolean(value.is_truthy())),
            },
            "CHAR" => {
                match value {
                    Value::Char(_) => Ok(value.clone()),
                    Value::Integer(n) => char::from_u32(*n as u32)
                        .map(Value::Char)
                        .ok_or_else(|| self.runtime_error(&format!("invalid char code: {}", n))),
                    Value::String(s) => {
                        let mut chars = s.chars();
                        match (chars.next(), chars.next()) {
                            (Some(c), None) => Ok(Value::Char(c)),
                            _ => Err(self
                                .runtime_error(&format!("cannot convert string '{}' to CHAR", s))),
                        }
                    }
                    _ => Err(self
                        .runtime_error(&format!("cannot convert {} to CHAR", value.type_name()))),
                }
            }
            "BYTE" => match value {
                Value::Byte(_) => Ok(value.clone()),
                Value::Integer(n) => {
                    if *n < 0 || *n > 255 {
                        Err(self.runtime_error(&format!(
                            "cannot convert INTEGER {} to BYTE (0-255)",
                            n
                        )))
                    } else {
                        Ok(Value::Byte(*n as u8))
                    }
                }
                Value::Uint(u) => {
                    if *u > 255 {
                        Err(self
                            .runtime_error(&format!("cannot convert UINT {} to BYTE (0-255)", u)))
                    } else {
                        Ok(Value::Byte(*u as u8))
                    }
                }
                Value::Char(c) => Ok(Value::Byte(*c as u8)),
                Value::Boolean(b) => Ok(Value::Byte(if *b { 1 } else { 0 })),
                _ => {
                    Err(self
                        .runtime_error(&format!("cannot convert {} to BYTE", value.type_name())))
                }
            },
            "UINT" => {
                match value {
                    Value::Uint(_) => Ok(value.clone()),
                    Value::Integer(n) => Ok(Value::Uint(*n as u64)),
                    Value::Float(f) => Ok(Value::Uint(*f as u64)),
                    Value::Byte(b) => Ok(Value::Uint(*b as u64)),
                    _ => Err(self
                        .runtime_error(&format!("cannot convert {} to UINT", value.type_name()))),
                }
            }
            "ARRAY" => {
                match value {
                    Value::Array(_) => Ok(value.clone()),
                    Value::String(s) => {
                        let elements: Vec<Value> = s
                            .chars()
                            .map(|c| Value::String(c.to_string().into()))
                            .collect();
                        Ok(Value::Array(Rc::new(RefCell::new(elements))))
                    }
                    _ => Err(self
                        .runtime_error(&format!("cannot convert {} to ARRAY", value.type_name()))),
                }
            }
            "TUPLE" => {
                match value {
                    Value::Tuple(_) => Ok(value.clone()),
                    _ => Err(self
                        .runtime_error(&format!("cannot convert {} to TUPLE", value.type_name()))),
                }
            }
            "MAP" => match value {
                Value::Map(_) => Ok(value.clone()),
                _ => {
                    Err(self.runtime_error(&format!("cannot convert {} to MAP", value.type_name())))
                }
            },
            "SET" => match value {
                Value::Set(_) => Ok(value.clone()),
                _ => {
                    Err(self.runtime_error(&format!("cannot convert {} to SET", value.type_name())))
                }
            },
            "VALUE" => Ok(Value::Wrapped(Rc::new(value.clone()))),
            "ENUM" => match value {
                Value::EnumInstance(_) | Value::EnumDef(_) => Ok(value.clone()),
                _ => Err(self.runtime_error(&format!(
                    "cannot convert {} to ENUM",
                    value.type_name()
                ))),
            },
            _ => {
                // Struct type or unknown — check if value matches
                if value.effective_type_name() == target {
                    Ok(value.clone())
                } else {
                    Err(self.runtime_error(&format!(
                        "cannot convert {} to {}",
                        value.effective_type_name(),
                        target
                    )))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use std::path::PathBuf;

    fn test_vm(input: &str) -> Result<Value, VMError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer, input);
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            panic!("Parser errors:\n{}", parser.format_errors());
        }

        let compiler = Compiler::new();
        let function = compiler.compile(&program).unwrap_or_else(|errors| {
            let messages: Vec<String> = errors.iter().map(|err| err.to_string()).collect();
            panic!("Compile errors: {}", messages.join("; "));
        });

        let mut vm = VM::new();
        vm.set_source(input);
        vm.set_file(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/vm_inline.oxi"));
        vm.run(function)
    }

    #[test]
    fn test_vm_module_dot_call_preserves_named_args() {
        let stdlib_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("core crate should live under repo root")
            .join("stdlib");
        let input = format!(
            r#"
                introduce os
                os.is_dir(path="{}")
            "#,
            stdlib_dir.display()
        );

        match test_vm(&input) {
            Ok(Value::Boolean(result)) => assert!(result),
            Ok(other) => panic!("expected Ok(Boolean(true)), got {}", other.type_name()),
            Err(err) => panic!("expected Ok(Boolean(true)), got error: {}", err.message),
        }
    }

    #[test]
    fn test_vm_struct_method_accepts_named_args() {
        let input = r#"
            struct Counter {
                count <int>
            }
            Counter contains {
                fun add(delta) { self.count + delta }
            }
            c := Counter(1)
            c.add(delta=4)
        "#;

        match test_vm(input) {
            Ok(Value::Integer(result)) => assert_eq!(result, 5),
            Ok(other) => panic!("expected Ok(Integer(5)), got {}", other.type_name()),
            Err(err) => panic!("expected Ok(Integer(5)), got error: {}", err.message),
        }
    }

    #[test]
    fn test_vm_builtin_method_named_args_fail_clearly() {
        let input = r#"
            text := "hello"
            text.upper(value="ignored")
        "#;

        match test_vm(input) {
            Err(err) => assert!(
                err.message
                    .contains("named arguments are not supported for built-in functions"),
                "unexpected error: {}",
                err.message
            ),
            Ok(other) => panic!(
                "expected builtin named-arg error, got {}",
                other.type_name()
            ),
        }
    }
}
