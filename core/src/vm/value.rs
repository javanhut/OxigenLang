use crate::compiler::opcode::Chunk;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

/// Built-in function signature for the VM.
pub type BuiltinFn = fn(Vec<Value>) -> Value;

/// Runtime value for the OxigenLang VM.
///
/// Unlike the tree-walking interpreter's `Object`, control flow signals
/// (Return, Skip, Stop) are NOT values — they are handled by VM mechanics.
#[derive(Clone)]
pub enum Value {
    // Primitives
    Integer(i64),
    Float(f64),
    Char(char),
    Boolean(bool),
    Byte(u8),
    Uint(u64),
    None,

    // Heap-allocated
    String(Rc<str>),
    Array(Rc<RefCell<Vec<Value>>>),
    Tuple(Rc<Vec<Value>>),
    Map(Rc<RefCell<Vec<(Value, Value)>>>),
    Set(Rc<RefCell<Vec<Value>>>),

    // Functions
    Closure(Rc<ObjClosure>),
    Builtin(BuiltinFn),

    // Structs
    StructDef(Rc<ObjStructDef>),
    StructInstance(Rc<ObjStructInstance>),

    // Module
    Module(Rc<ObjModule>),

    // Error handling
    ErrorValue { msg: Rc<str>, tag: Option<Rc<str>> },
    /// Value(...) wrapper (success side of error handling)
    Wrapped(Rc<Value>),
    /// Terminal error — stops execution
    Error(Rc<str>),
}

/// A compiled function (not yet a closure — no captured upvalues).
#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<String>,
    pub arity: u8,
    pub chunk: Chunk,
    pub upvalue_count: u16,
    pub params: Vec<ParamInfo>,
}

impl Function {
    pub fn new(name: Option<String>, arity: u8) -> Self {
        Function {
            name,
            arity,
            chunk: Chunk::new(),
            upvalue_count: 0,
            params: Vec::new(),
        }
    }
}

/// Metadata for a function parameter (for named args, defaults, optional).
#[derive(Debug, Clone)]
pub struct ParamInfo {
    pub name: String,
    pub has_default: bool,
    pub optional: bool,
    pub type_ann: Option<String>,
}

/// A closure: a compiled function + captured upvalues.
#[derive(Debug)]
pub struct ObjClosure {
    pub function: Rc<Function>,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

/// An upvalue captures a variable from an enclosing scope.
#[derive(Debug, Clone)]
pub enum Upvalue {
    /// Points to a stack slot (variable still on the stack).
    Open(usize),
    /// Variable has been moved off the stack into this value.
    Closed(Value),
}

/// Struct definition at runtime.
#[derive(Debug)]
pub struct ObjStructDef {
    pub name: String,
    pub fields: Vec<(String, String, bool)>, // (name, type, hidden)
    pub methods: RefCell<HashMap<String, Value>>,
    pub parent: Option<String>,
}

/// A struct instance at runtime.
#[derive(Debug)]
pub struct ObjStructInstance {
    pub struct_name: String,
    pub fields: RefCell<HashMap<String, Value>>,
}

/// A loaded module.
#[derive(Debug)]
pub struct ObjModule {
    pub name: String,
    pub globals: Rc<HashMap<String, Value>>,
}

// ── Value methods ──────────────────────────────────────────────────────

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::None => false,
            Value::Integer(n) => *n != 0,
            Value::Float(f) => *f != 0.0,
            Value::Char(c) => *c != '\0',
            Value::String(s) => !s.is_empty(),
            Value::Array(arr) => !arr.borrow().is_empty(),
            Value::Error(_) => false,
            Value::ErrorValue { .. } => true,
            Value::Wrapped(_) => true,
            Value::StructInstance(_) => true,
            Value::Module(_) => true,
            Value::Byte(n) => *n != 0,
            Value::Uint(n) => *n != 0,
            Value::Tuple(t) => !t.is_empty(),
            Value::Map(m) => !m.borrow().is_empty(),
            Value::Set(s) => !s.borrow().is_empty(),
            _ => true,
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Value::Error(_))
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "INTEGER",
            Value::Float(_) => "FLOAT",
            Value::Char(_) => "CHAR",
            Value::String(_) => "STRING",
            Value::Boolean(_) => "BOOLEAN",
            Value::Array(_) => "ARRAY",
            Value::Closure(_) => "FUNCTION",
            Value::StructDef(_) => "STRUCT_DEF",
            Value::StructInstance(_) => "STRUCT",
            Value::Byte(_) => "BYTE",
            Value::Uint(_) => "UINT",
            Value::Tuple(_) => "TUPLE",
            Value::Map(_) => "MAP",
            Value::Set(_) => "SET",
            Value::Module(_) => "MODULE",
            Value::Builtin(_) => "BUILTIN",
            Value::None => "NONE",
            Value::ErrorValue { .. } => "ERROR",
            Value::Wrapped(_) => "VALUE",
            Value::Error(_) => "ERROR",
        }
    }

    pub fn effective_type_name(&self) -> String {
        match self {
            Value::StructInstance(inst) => inst.struct_name.clone(),
            Value::ErrorValue { tag, .. } => match tag {
                Some(tag) => format!("ERROR<{}>", tag),
                None => "ERROR".to_string(),
            },
            Value::Wrapped(_) => "VALUE".to_string(),
            other => other.type_name().to_string(),
        }
    }
}

// ── Display ────────────────────────────────────────────────────────────

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::Char(c) => write!(f, "`{}`", c),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", if *b { "True" } else { "False" }),
            Value::Array(elements) => {
                let items: Vec<String> = elements.borrow().iter().map(|e| e.to_string()).collect();
                write!(f, "[{}]", items.join(", "))
            }
            Value::Closure(c) => {
                let name = c.function.name.as_deref().unwrap_or("anonymous");
                write!(f, "fun {}()", name)
            }
            Value::StructDef(sd) => write!(f, "struct {}", sd.name),
            Value::StructInstance(si) => {
                let fields = si.fields.borrow();
                let items: Vec<String> =
                    fields.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                write!(f, "{} {{ {} }}", si.struct_name, items.join(", "))
            }
            Value::Byte(n) => write!(f, "{}", n),
            Value::Uint(n) => write!(f, "{}", n),
            Value::Tuple(elements) => {
                let items: Vec<String> = elements.iter().map(|e| e.to_string()).collect();
                if elements.len() == 1 {
                    write!(f, "({},)", items[0])
                } else {
                    write!(f, "({})", items.join(", "))
                }
            }
            Value::Map(entries) => {
                let entries = entries.borrow();
                let items: Vec<String> = entries
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect();
                write!(f, "{{{}}}", items.join(", "))
            }
            Value::Set(elements) => {
                let items: Vec<String> =
                    elements.borrow().iter().map(|e| e.to_string()).collect();
                write!(f, "set({})", items.join(", "))
            }
            Value::Module(m) => write!(f, "module <{}>", m.name),
            Value::Builtin(_) => write!(f, "builtin function"),
            Value::None => write!(f, "None"),
            Value::ErrorValue { msg, tag } => match tag {
                Some(tag) => write!(f, "Error {{ tag: {}, msg: {} }}", tag, msg),
                None => write!(f, "Error {{ msg: {} }}", msg),
            },
            Value::Wrapped(val) => write!(f, "Value({})", val),
            Value::Error(msg) => write!(f, "Error: {}", msg),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "Integer({})", n),
            Value::Float(n) => write!(f, "Float({})", n),
            Value::Char(c) => write!(f, "Char({:?})", c),
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Array(a) => write!(f, "Array({:?})", a.borrow()),
            Value::Closure(c) => write!(f, "Closure({:?})", c.function.name),
            Value::StructDef(sd) => write!(f, "StructDef({})", sd.name),
            Value::StructInstance(si) => {
                write!(f, "StructInstance({}, {:?})", si.struct_name, si.fields.borrow())
            }
            Value::Byte(n) => write!(f, "Byte({})", n),
            Value::Uint(n) => write!(f, "Uint({})", n),
            Value::Tuple(t) => write!(f, "Tuple({:?})", t),
            Value::Map(m) => write!(f, "Map({:?})", m.borrow()),
            Value::Set(s) => write!(f, "Set({:?})", s.borrow()),
            Value::Module(m) => write!(f, "Module({})", m.name),
            Value::Builtin(_) => write!(f, "Builtin"),
            Value::None => write!(f, "None"),
            Value::ErrorValue { msg, tag } => {
                write!(f, "ErrorValue(tag={:?}, msg={:?})", tag, msg)
            }
            Value::Wrapped(v) => write!(f, "Wrapped({:?})", v),
            Value::Error(msg) => write!(f, "Error({:?})", msg),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::None, Value::None) => true,
            (Value::Byte(a), Value::Byte(b)) => a == b,
            (Value::Uint(a), Value::Uint(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => *a.borrow() == *b.borrow(),
            (Value::Tuple(a), Value::Tuple(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => *a.borrow() == *b.borrow(),
            (Value::Set(a), Value::Set(b)) => {
                let a = a.borrow();
                let b = b.borrow();
                a.len() == b.len() && a.iter().all(|item| b.iter().any(|bitem| item == bitem))
            }
            (
                Value::ErrorValue { msg: a, tag: at },
                Value::ErrorValue { msg: b, tag: bt },
            ) => a == b && at == bt,
            (Value::Wrapped(a), Value::Wrapped(b)) => a == b,
            (Value::StructInstance(a), Value::StructInstance(b)) => {
                a.struct_name == b.struct_name && *a.fields.borrow() == *b.fields.borrow()
            }
            (Value::Module(a), Value::Module(b)) => a.name == b.name,
            _ => false,
        }
    }
}
