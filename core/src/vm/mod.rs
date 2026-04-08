pub mod builtins;
pub mod value;

use crate::compiler::opcode::OpCode;
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

/// Runtime error with source information.
#[derive(Debug)]
pub struct VMError {
    pub message: String,
    pub line: u32,
}

impl std::fmt::Display for VMError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] Runtime error: {}", self.line, self.message)
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
        let args_val: Vec<Value> = args.iter().map(|a| Value::String(a.as_str().into())).collect();
        self.globals.insert(
            "__args".to_string(),
            Value::Array(Rc::new(RefCell::new(args_val))),
        );
    }

    /// Run a compiled function.
    pub fn run(&mut self, function: Function) -> Result<Value, VMError> {
        let closure = Rc::new(ObjClosure {
            function: Rc::new(function),
            upvalues: Vec::new(),
        });

        // Push the closure itself onto the stack (slot 0)
        self.stack.push(Value::Closure(Rc::clone(&closure)));

        self.frames.push(CallFrame {
            closure,
            ip: 0,
            slot_offset: 0,
        });

        self.execute()
    }

    // ── Stack Operations ───────────────────────────────────────────────

    fn push(&mut self, value: Value) {
        if self.stack.len() >= STACK_MAX {
            panic!("stack overflow: exceeded {} slots", STACK_MAX);
        }
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack underflow")
    }

    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - distance]
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

    fn runtime_error(&self, message: &str) -> VMError {
        VMError {
            message: message.to_string(),
            line: self.current_line(),
        }
    }

    // ── Main Execution Loop ────────────────────────────────────────────

    fn execute(&mut self) -> Result<Value, VMError> {
        loop {
            let frame_idx = self.frames.len() - 1;

            let instruction = self.frames[frame_idx].read_byte();
            let op = match OpCode::from_byte(instruction) {
                Some(op) => op,
                None => {
                    return Err(self.runtime_error(&format!(
                        "unknown opcode: {}",
                        instruction
                    )));
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
                        (Value::Integer(l), Value::Integer(r)) => {
                            self.push(Value::Integer(l << r))
                        }
                        _ => return Err(self.runtime_error("<< requires integers")),
                    }
                }
                OpCode::ShiftRight => {
                    let b = self.pop();
                    let a = self.pop();
                    match (&a, &b) {
                        (Value::Integer(l), Value::Integer(r)) => {
                            self.push(Value::Integer(l >> r))
                        }
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
                            return Err(
                                self.runtime_error("negation is only supported for numbers")
                            )
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
                    let name = self.frames[frame_idx].read_constant(idx).clone();
                    if let Value::String(name_str) = &name {
                        match self.globals.get(name_str.as_ref()) {
                            Some(val) => self.push(val.clone()),
                            None => {
                                return Err(self.runtime_error(&format!(
                                    "undefined variable: {}",
                                    name_str
                                )));
                            }
                        }
                    } else {
                        return Err(self.runtime_error("invalid global name"));
                    }
                }
                OpCode::SetGlobal => {
                    let idx = self.frames[frame_idx].read_u16();
                    let name = self.frames[frame_idx].read_constant(idx).clone();
                    if let Value::String(name_str) = &name {
                        if !self.globals.contains_key(name_str.as_ref()) {
                            return Err(self.runtime_error(&format!(
                                "undefined variable: {}",
                                name_str
                            )));
                        }
                        // Check immutability
                        if let Some(false) = self.global_mutability.get(name_str.as_ref()) {
                            return Err(self.runtime_error(&format!(
                                "cannot reassign immutable variable '{}'. use := to override",
                                name_str
                            )));
                        }
                        let val = self.peek(0).clone();
                        self.globals.insert(name_str.to_string(), val);
                    }
                }
                OpCode::DefineGlobal => {
                    let idx = self.frames[frame_idx].read_u16();
                    let name = self.frames[frame_idx].read_constant(idx).clone();
                    if let Value::String(name_str) = &name {
                        let val = self.pop();
                        self.globals.insert(name_str.to_string(), val);
                    }
                }
                OpCode::DefineGlobalTyped => {
                    let name_idx = self.frames[frame_idx].read_u16();
                    let mutable = self.frames[frame_idx].read_byte() == 1;
                    let type_idx = self.frames[frame_idx].read_u16();
                    let name = self.frames[frame_idx].read_constant(name_idx).clone();
                    let type_name = self.frames[frame_idx].read_constant(type_idx).clone();

                    if let Value::String(name_str) = &name {
                        let val = self.pop();
                        self.globals.insert(name_str.to_string(), val);
                        self.global_mutability.insert(name_str.to_string(), mutable);
                        if let Value::String(tn) = type_name {
                            self.global_type_constraints
                                .insert(name_str.to_string(), Some(tn.to_string()));
                        }
                    }
                }

                // ── Upvalues ────────────────────────────────────────
                OpCode::GetUpvalue => {
                    let idx = self.frames[frame_idx].read_u16() as usize;
                    let upvalue = Rc::clone(
                        &self.frames[frame_idx].closure.upvalues[idx],
                    );
                    let val = match &*upvalue.borrow() {
                        Upvalue::Open(slot) => self.stack[*slot].clone(),
                        Upvalue::Closed(val) => val.clone(),
                    };
                    self.push(val);
                }
                OpCode::SetUpvalue => {
                    let idx = self.frames[frame_idx].read_u16() as usize;
                    let val = self.peek(0).clone();
                    let upvalue = Rc::clone(
                        &self.frames[frame_idx].closure.upvalues[idx],
                    );
                    match &mut *upvalue.borrow_mut() {
                        Upvalue::Open(slot) => {
                            self.stack[*slot] = val;
                        }
                        Upvalue::Closed(v) => {
                            *v = val;
                        }
                    }
                }
                OpCode::CloseUpvalue => {
                    let stack_top = self.stack.len() - 1;
                    self.close_upvalues(stack_top);
                    self.pop();
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
                    let idx = self.frames[frame_idx].read_u16();
                    let constant = self.frames[frame_idx].read_constant(idx).clone();

                    if let Value::Closure(template) = constant {
                        let upvalue_count = template.function.upvalue_count as usize;
                        let mut upvalues = Vec::with_capacity(upvalue_count);

                        for _ in 0..upvalue_count {
                            let is_local = self.frames[frame_idx].read_byte() == 1;
                            let index = self.frames[frame_idx].read_u16() as usize;

                            if is_local {
                                let abs_slot =
                                    self.frames[frame_idx].slot_offset + index;
                                let upvalue = self.capture_upvalue(abs_slot);
                                upvalues.push(upvalue);
                            } else {
                                let uv = Rc::clone(
                                    &self.frames[frame_idx].closure.upvalues[index],
                                );
                                upvalues.push(uv);
                            }
                        }

                        let closure = Rc::new(ObjClosure {
                            function: Rc::clone(&template.function),
                            upvalues,
                        });
                        self.push(Value::Closure(closure));
                    } else {
                        return Err(self.runtime_error("expected closure constant"));
                    }
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

                    if self.frames.is_empty() {
                        return Ok(result);
                    }

                    self.push(result);
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
                    let field_count = self.frames[frame_idx].read_byte() as usize;
                    let name = self.frames[frame_idx].read_constant(name_idx).clone();

                    if let Value::String(struct_name) = name {
                        // Pop field name-value pairs
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
                    } else {
                        return Err(self.runtime_error("expected struct name"));
                    }
                }

                OpCode::GetField => {
                    let field_idx = self.frames[frame_idx].read_u16();
                    let field_name = self.frames[frame_idx].read_constant(field_idx).clone();
                    let object = self.pop();

                    if let Value::String(fname) = &field_name {
                        match &object {
                            Value::StructInstance(inst) => {
                                let field_val = {
                                    let fields = inst.fields.borrow();
                                    fields.get(fname.as_ref()).cloned()
                                };
                                if let Some(val) = field_val {
                                    self.push(val);
                                } else {
                                    // Check methods on the struct def
                                    let method_val = {
                                        if let Some(Value::StructDef(def)) =
                                            self.globals.get(&inst.struct_name)
                                        {
                                            let methods = def.methods.borrow();
                                            methods.get(fname.as_ref()).cloned()
                                        } else {
                                            None
                                        }
                                    };
                                    if let Some(method) = method_val {
                                        self.push(method);
                                        // TODO: bind self
                                    } else {
                                        return Err(self.runtime_error(&format!(
                                            "field '{}' not found on {}",
                                            fname, inst.struct_name
                                        )));
                                    }
                                }
                            }
                            Value::Module(m) => {
                                match m.globals.get(fname.as_ref()) {
                                    Some(val) => self.push(val.clone()),
                                    None => {
                                        return Err(self.runtime_error(&format!(
                                            "module '{}' has no member '{}'",
                                            m.name, fname
                                        )));
                                    }
                                }
                            }
                            Value::ErrorValue { msg, tag } => {
                                match fname.as_ref() {
                                    "msg" => self.push(Value::String(Rc::clone(msg))),
                                    "tag" => match tag {
                                        Some(t) => self.push(Value::String(Rc::clone(t))),
                                        None => self.push(Value::None),
                                    },
                                    _ => {
                                        return Err(self.runtime_error(&format!(
                                            "ErrorValue has no field '{}'",
                                            fname
                                        )));
                                    }
                                }
                            }
                            _ => {
                                return Err(self.runtime_error(&format!(
                                    "cannot access field '{}' on {}",
                                    fname,
                                    object.type_name()
                                )));
                            }
                        }
                    }
                }

                OpCode::SetField => {
                    let field_idx = self.frames[frame_idx].read_u16();
                    let field_name = self.frames[frame_idx].read_constant(field_idx).clone();
                    let value = self.pop();
                    let object = self.pop();

                    if let Value::String(fname) = &field_name {
                        match &object {
                            Value::StructInstance(inst) => {
                                inst.fields
                                    .borrow_mut()
                                    .insert(fname.to_string(), value);
                            }
                            _ => {
                                return Err(self.runtime_error(&format!(
                                    "cannot set field on {}",
                                    object.type_name()
                                )));
                            }
                        }
                    }
                }

                OpCode::DefineMethod => {
                    let struct_name_idx = self.frames[frame_idx].read_u16();
                    let method_count = self.frames[frame_idx].read_byte() as usize;
                    let struct_name = self.frames[frame_idx]
                        .read_constant(struct_name_idx)
                        .clone();

                    if let Value::String(name_str) = struct_name {
                        // Pop method name-closure pairs from stack
                        let start = self.stack.len() - method_count * 2;
                        let flat: Vec<Value> = self.stack.drain(start..).collect();

                        if let Some(Value::StructDef(def)) =
                            self.globals.get(name_str.as_ref())
                        {
                            let mut methods = def.methods.borrow_mut();
                            for pair in flat.chunks(2) {
                                if let Value::String(mname) = &pair[1] {
                                    methods.insert(mname.to_string(), pair[0].clone());
                                }
                            }
                        }
                    }
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
                    let binding_name = self.frames[frame_idx]
                        .read_constant(binding_idx)
                        .clone();

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

                    let tag_str = tag.map(|t| format!("{}", t)).unwrap_or_default();
                    let sub_str = sub.map(|s| format!(":{}", s)).unwrap_or_default();
                    let msg_str = msg.map(|m| format!("{}", m)).unwrap_or_default();
                    println!("[LOG{}{}] {}", tag_str, sub_str, msg_str);
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
                            return Err(
                                self.runtime_error("can only unpack arrays and tuples")
                            );
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
                            return Err(
                                self.runtime_error(&format!("cannot iterate over {}", iterable.type_name()))
                            )
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
                        Value::String(s) => {
                            s.chars()
                                .nth(idx)
                                .map(|c| Value::String(c.to_string().into()))
                                .unwrap_or(Value::None)
                        }
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
                    let arg_count = self.frames[frame_idx].read_byte() as usize;
                    let method_name = self.frames[frame_idx]
                        .read_constant(method_idx)
                        .clone();

                    if let Value::String(mname) = method_name {
                        self.call_method(&mname, arg_count)?;
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
            }
        }
    }

    // ── Arithmetic Helpers ──────────────────────────────────────────────

    fn binary_add(&self, a: Value, b: Value) -> Result<Value, VMError> {
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
            _ => Err(self.runtime_error(&format!(
                "type mismatch: {} + {}",
                a.type_name(),
                b.type_name()
            ))),
        }
    }

    fn binary_sub(&self, a: Value, b: Value) -> Result<Value, VMError> {
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
            _ => Err(self.runtime_error(&format!(
                "type mismatch: {} - {}",
                a.type_name(),
                b.type_name()
            ))),
        }
    }

    fn binary_mul(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l * r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(*l as f64 * r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l * *r as f64)),
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Integer(*l as i64 * *r as i64)),
            (Value::Uint(l), Value::Uint(r)) => Ok(Value::Uint(l.wrapping_mul(*r))),
            _ => Err(self.runtime_error(&format!(
                "type mismatch: {} * {}",
                a.type_name(),
                b.type_name()
            ))),
        }
    }

    fn binary_div(&self, a: Value, b: Value) -> Result<Value, VMError> {
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
            _ => Err(self.runtime_error(&format!(
                "type mismatch: {} / {}",
                a.type_name(),
                b.type_name()
            ))),
        }
    }

    fn binary_mod(&self, a: Value, b: Value) -> Result<Value, VMError> {
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
            _ => Err(self.runtime_error(&format!(
                "type mismatch: {} % {}",
                a.type_name(),
                b.type_name()
            ))),
        }
    }

    // ── Comparison Helpers ──────────────────────────────────────────────

    fn compare_less(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l < r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l < r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Boolean((*l as f64) < *r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Boolean(*l < (*r as f64))),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l < r)),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l < r)),
            (Value::Uint(l), Value::Uint(r)) => Ok(Value::Boolean(l < r)),
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Boolean(l < r)),
            _ => Err(self.runtime_error(&format!(
                "cannot compare {} < {}",
                a.type_name(),
                b.type_name()
            ))),
        }
    }

    fn compare_less_equal(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l <= r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l <= r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Boolean((*l as f64) <= *r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Boolean(*l <= (*r as f64))),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l <= r)),
            (Value::Uint(l), Value::Uint(r)) => Ok(Value::Boolean(l <= r)),
            _ => Err(self.runtime_error(&format!(
                "cannot compare {} <= {}",
                a.type_name(),
                b.type_name()
            ))),
        }
    }

    fn compare_greater(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l > r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l > r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Boolean((*l as f64) > *r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Boolean(*l > (*r as f64))),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l > r)),
            (Value::Uint(l), Value::Uint(r)) => Ok(Value::Boolean(l > r)),
            _ => Err(self.runtime_error(&format!(
                "cannot compare {} > {}",
                a.type_name(),
                b.type_name()
            ))),
        }
    }

    fn compare_greater_equal(&self, a: Value, b: Value) -> Result<Value, VMError> {
        match (&a, &b) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l >= r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l >= r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Boolean((*l as f64) >= *r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Boolean(*l >= (*r as f64))),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l >= r)),
            (Value::Uint(l), Value::Uint(r)) => Ok(Value::Boolean(l >= r)),
            _ => Err(self.runtime_error(&format!(
                "cannot compare {} >= {}",
                a.type_name(),
                b.type_name()
            ))),
        }
    }

    // ── Function Calling ────────────────────────────────────────────────

    fn call_value(
        &mut self,
        arg_count: usize,
        named_args: &[(String, Value)],
    ) -> Result<(), VMError> {
        let callee_idx = self.stack.len() - 1 - arg_count;
        let callee = self.stack[callee_idx].clone();

        match callee {
            Value::Closure(closure) => {
                self.call_closure(closure, arg_count, named_args)
            }
            Value::Builtin(func) => {
                let start = self.stack.len() - arg_count;
                let args: Vec<Value> = self.stack.drain(start..).collect();
                self.pop(); // pop the builtin itself

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
                    let val = args.get(i).cloned().unwrap_or_else(|| {
                        Self::zero_value_for_type(field_type)
                    });
                    fields.insert(field_name.clone(), val);
                }

                self.push(Value::StructInstance(Rc::new(ObjStructInstance {
                    struct_name: def.name.clone(),
                    fields: RefCell::new(fields),
                })));
                Ok(())
            }
            _ => Err(self.runtime_error(&format!(
                "cannot call {}",
                callee.type_name()
            ))),
        }
    }

    /// Call a method on an object. Stack: [instance, arg1, ..., argN]
    fn call_method(&mut self, method_name: &str, arg_count: usize) -> Result<(), VMError> {
        let instance_idx = self.stack.len() - 1 - arg_count;
        let instance = self.stack[instance_idx].clone();

        match &instance {
            Value::StructInstance(inst) => {
                // Check instance fields first (for callable fields)
                let field_val = inst.fields.borrow().get(method_name).cloned();
                if let Some(Value::Closure(closure)) = field_val {
                    // Callable field — treat as regular call
                    self.stack[instance_idx] = Value::Closure(Rc::clone(&closure));
                    return self.call_closure(closure, arg_count, &[]);
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
                    return self.call_closure(closure, arg_count + 1, &[]);
                }

                Err(self.runtime_error(&format!(
                    "method '{}' not found on {}",
                    method_name, inst.struct_name
                )))
            }
            Value::Array(_) | Value::String(_) | Value::Map(_) | Value::Set(_) | Value::Tuple(_) => {
                // Built-in method syntax: collection.method(args)
                // Convert to builtin call: __method(collection, args)
                let builtin_name = format!("__{}", method_name);
                if let Some(builtin) = self.globals.get(&builtin_name).cloned() {
                    // Rearrange: [instance, arg1, ...] → [builtin, instance, arg1, ...]
                    self.stack[instance_idx] = builtin;
                    self.stack.insert(instance_idx + 1, instance);
                    return self.call_value(arg_count + 1, &[]);
                }
                // Try without __ prefix
                if let Some(builtin) = self.globals.get(method_name).cloned() {
                    self.stack[instance_idx] = builtin;
                    self.stack.insert(instance_idx + 1, instance);
                    return self.call_value(arg_count + 1, &[]);
                }
                Err(self.runtime_error(&format!(
                    "method '{}' not found on {}",
                    method_name,
                    instance.type_name()
                )))
            }
            Value::Module(m) => {
                // Module method call: module.func(args)
                if let Some(func) = m.globals.get(method_name).cloned() {
                    self.stack[instance_idx] = func;
                    return self.call_value(arg_count, &[]);
                }
                Err(self.runtime_error(&format!(
                    "module '{}' has no function '{}'",
                    m.name, method_name
                )))
            }
            _ => Err(self.runtime_error(&format!(
                "cannot call method '{}' on {}",
                method_name,
                instance.type_name()
            ))),
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
            return Err(self.runtime_error(&format!(
                "method '{}' not found on {}",
                method_name, struct_name
            )));
        }
    }

    fn call_closure(
        &mut self,
        closure: Rc<ObjClosure>,
        arg_count: usize,
        named_args: &[(String, Value)],
    ) -> Result<(), VMError> {
        let expected = closure.function.arity as usize;

        // Handle named arguments: fill in any missing positional args
        if !named_args.is_empty() {
            // Extend stack to full arity if needed
            let total_provided = arg_count + named_args.len();
            if total_provided > expected && !closure.function.params.iter().any(|p| p.optional) {
                return Err(self.runtime_error(&format!(
                    "expected {} arguments but got {}",
                    expected, total_provided
                )));
            }

            // Fill positional slots up to arity with None (will be overwritten by named args)
            for _ in arg_count..expected {
                self.push(Value::None);
            }

            // Place named args into their correct slots
            let slot_base = self.stack.len() - expected;
            for (name, val) in named_args {
                if let Some(idx) = closure
                    .function
                    .params
                    .iter()
                    .position(|p| p.name == *name)
                {
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
            return Err(self.runtime_error("stack overflow"));
        }

        let slot_offset = self.stack.len() - expected - 1; // -1 for the function itself

        self.frames.push(CallFrame {
            closure,
            ip: 0,
            slot_offset,
        });

        Ok(())
    }

    // ── Upvalue Management ──────────────────────────────────────────────

    fn capture_upvalue(&mut self, stack_slot: usize) -> Rc<RefCell<Upvalue>> {
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

    fn close_upvalues(&mut self, last: usize) {
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

    fn eval_index(&self, collection: Value, index: Value) -> Result<Value, VMError> {
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
            _ => Err(self.runtime_error(&format!(
                "cannot index {} with {}",
                collection.type_name(),
                index.type_name()
            ))),
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
            _ => Err(self.runtime_error(&format!(
                "cannot assign to index on {}",
                collection.type_name()
            ))),
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
                        if i < 0 { (len + i) as usize } else { i as usize }
                    }
                    None => 0,
                    _ => return Err(self.runtime_error("slice index must be integer")),
                };
                let e = match end {
                    Some(Value::Integer(i)) => {
                        if i < 0 { (len + i) as usize } else { i as usize }
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
                        if i < 0 { (len + i) as usize } else { i as usize }
                    }
                    None => 0,
                    _ => return Err(self.runtime_error("slice index must be integer")),
                };
                let end_idx = match end {
                    Some(Value::Integer(i)) => {
                        if i < 0 { (len + i) as usize } else { i as usize }
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
            _ => Err(self.runtime_error(&format!(
                "cannot slice {}",
                collection.type_name()
            ))),
        }
    }

    // ── Module System ───────────────────────────────────────────────────

    fn import_module(
        &mut self,
        path_str: &str,
        selective_names: &[String],
    ) -> Result<(), VMError> {
        // Resolve the module path
        let module_path = self.resolve_module_path(path_str)?;

        // Check cache
        if let Some(cached) = self.module_cache.get(&module_path).cloned() {
            return self.bind_module(&cached, path_str, selective_names);
        }

        // Check circular imports
        if self.import_stack.contains(&module_path) {
            return Err(self.runtime_error(&format!(
                "circular import: {}",
                module_path.display()
            )));
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
            globals: sub_vm.globals,
        });

        self.module_cache
            .insert(module_path, Rc::clone(&module));

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
            let name = path_str
                .rsplit('/')
                .next()
                .unwrap_or(path_str)
                .to_string();
            self.globals
                .insert(name, Value::Module(Rc::clone(module)));
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
                    return resolved.canonicalize().map_err(|e| {
                        self.runtime_error(&format!("cannot resolve path: {}", e))
                    });
                }
            }
        }

        // Stdlib import
        let stdlib = self.stdlib_path.join(path_str).with_extension("oxi");
        if stdlib.exists() {
            return stdlib.canonicalize().map_err(|e| {
                self.runtime_error(&format!("cannot resolve stdlib path: {}", e))
            });
        }

        // Relative to current file (without ./ prefix)
        if let Some(current) = &self.current_file {
            let base = current.parent().unwrap_or(current);
            let resolved = base.join(path_str).with_extension("oxi");
            if resolved.exists() {
                return resolved.canonicalize().map_err(|e| {
                    self.runtime_error(&format!("cannot resolve path: {}", e))
                });
            }
        }

        Err(self.runtime_error(&format!("module not found: {}", path_str)))
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

    fn type_wrap(&self, target: &str, value: Value) -> Result<Value, VMError> {
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
            return Err(self.runtime_error(&format!(
                "cannot convert {} to {}",
                actual, target
            )));
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
                    Err(self.runtime_error(&format!(
                        "cannot convert {} to ERROR",
                        value.type_name()
                    )))
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
            "NONE" => match value {
                Value::None => Ok(Value::None),
                _ => Err(self.runtime_error(&format!(
                    "cannot convert {} to NONE",
                    value.type_name()
                ))),
            },
            "INTEGER" => match value {
                Value::Integer(_) => Ok(value.clone()),
                Value::Float(f) => Ok(Value::Integer(*f as i64)),
                Value::String(s) => s
                    .parse::<i64>()
                    .map(Value::Integer)
                    .map_err(|_| {
                        self.runtime_error(&format!(
                            "cannot convert STRING \"{}\" to INTEGER",
                            s
                        ))
                    }),
                Value::Boolean(b) => Ok(Value::Integer(if *b { 1 } else { 0 })),
                Value::Byte(b) => Ok(Value::Integer(*b as i64)),
                Value::Uint(u) => Ok(Value::Integer(*u as i64)),
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => Err(self.runtime_error(&format!(
                    "cannot convert {} to INTEGER",
                    value.type_name()
                ))),
            },
            "FLOAT" => match value {
                Value::Float(_) => Ok(value.clone()),
                Value::Integer(n) => Ok(Value::Float(*n as f64)),
                Value::String(s) => s.parse::<f64>().map(Value::Float).map_err(|_| {
                    self.runtime_error(&format!(
                        "cannot convert STRING \"{}\" to FLOAT",
                        s
                    ))
                }),
                Value::Boolean(b) => Ok(Value::Float(if *b { 1.0 } else { 0.0 })),
                Value::Byte(b) => Ok(Value::Float(*b as f64)),
                Value::Uint(u) => Ok(Value::Float(*u as f64)),
                _ => Err(self.runtime_error(&format!(
                    "cannot convert {} to FLOAT",
                    value.type_name()
                ))),
            },
            "STRING" => Ok(Value::String(format!("{}", value).into())),
            "BOOLEAN" => match value {
                Value::Boolean(_) => Ok(value.clone()),
                _ => Ok(Value::Boolean(value.is_truthy())),
            },
            "CHAR" => match value {
                Value::Char(_) => Ok(value.clone()),
                Value::Integer(n) => char::from_u32(*n as u32)
                    .map(Value::Char)
                    .ok_or_else(|| {
                        self.runtime_error(&format!("invalid char code: {}", n))
                    }),
                Value::String(s) => {
                    let mut chars = s.chars();
                    match (chars.next(), chars.next()) {
                        (Some(c), None) => Ok(Value::Char(c)),
                        _ => Err(self.runtime_error(&format!(
                            "cannot convert string '{}' to CHAR",
                            s
                        ))),
                    }
                }
                _ => Err(self.runtime_error(&format!(
                    "cannot convert {} to CHAR",
                    value.type_name()
                ))),
            },
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
                        Err(self.runtime_error(&format!(
                            "cannot convert UINT {} to BYTE (0-255)",
                            u
                        )))
                    } else {
                        Ok(Value::Byte(*u as u8))
                    }
                }
                Value::Char(c) => Ok(Value::Byte(*c as u8)),
                Value::Boolean(b) => Ok(Value::Byte(if *b { 1 } else { 0 })),
                _ => Err(self.runtime_error(&format!(
                    "cannot convert {} to BYTE",
                    value.type_name()
                ))),
            },
            "UINT" => match value {
                Value::Uint(_) => Ok(value.clone()),
                Value::Integer(n) => Ok(Value::Uint(*n as u64)),
                Value::Float(f) => Ok(Value::Uint(*f as u64)),
                Value::Byte(b) => Ok(Value::Uint(*b as u64)),
                _ => Err(self.runtime_error(&format!(
                    "cannot convert {} to UINT",
                    value.type_name()
                ))),
            },
            "ARRAY" => match value {
                Value::Array(_) => Ok(value.clone()),
                Value::String(s) => {
                    let elements: Vec<Value> =
                        s.chars().map(|c| Value::String(c.to_string().into())).collect();
                    Ok(Value::Array(Rc::new(RefCell::new(elements))))
                }
                _ => Err(self.runtime_error(&format!(
                    "cannot convert {} to ARRAY",
                    value.type_name()
                ))),
            },
            "TUPLE" => match value {
                Value::Tuple(_) => Ok(value.clone()),
                _ => Err(self.runtime_error(&format!(
                    "cannot convert {} to TUPLE",
                    value.type_name()
                ))),
            },
            "MAP" => match value {
                Value::Map(_) => Ok(value.clone()),
                _ => Err(self.runtime_error(&format!(
                    "cannot convert {} to MAP",
                    value.type_name()
                ))),
            },
            "SET" => match value {
                Value::Set(_) => Ok(value.clone()),
                _ => Err(self.runtime_error(&format!(
                    "cannot convert {} to SET",
                    value.type_name()
                ))),
            },
            "VALUE" => Ok(Value::Wrapped(Rc::new(value.clone()))),
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
