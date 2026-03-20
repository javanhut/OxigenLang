pub mod environment;

use crate::ast::{Expression, Identifier, Statement};
use environment::Environment;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub type BuiltinFunction = fn(Vec<Rc<Object>>) -> Rc<Object>;

#[derive(Clone)]
pub enum Object {
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
    Boolean(bool),
    Array(Vec<Rc<Object>>),
    Function {
        parameters: Vec<Identifier>,
        body: Vec<Statement>,
        env: Rc<RefCell<Environment>>,
    },
    Pattern {
        name: String,
        parameters: Vec<Identifier>,
        condition: Expression,
    },
    StructDef {
        name: String,
        fields: Vec<(String, String)>,
        methods: HashMap<String, Rc<Object>>,
        parent: Option<String>,
    },
    StructInstance {
        struct_name: String,
        fields: Rc<RefCell<HashMap<String, Rc<Object>>>>,
    },
    Builtin(BuiltinFunction),
    Return(Rc<Object>),
    Skip,
    Stop,
    None,
    Error(String),
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Boolean(b) => *b,
            Object::None => false,
            Object::Integer(n) => *n != 0,
            Object::Float(f) => *f != 0.0,
            Object::Char(c) => *c != '\0',
            Object::String(s) => !s.is_empty(),
            Object::Array(arr) => !arr.is_empty(),
            Object::Error(_) => false,
            Object::StructInstance { .. } => true,
            _ => true,
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Object::Error(_))
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Float(_) => "FLOAT",
            Object::Char(_) => "CHAR",
            Object::String(_) => "STRING",
            Object::Boolean(_) => "BOOLEAN",
            Object::Array(_) => "ARRAY",
            Object::Function { .. } => "FUNCTION",
            Object::Pattern { .. } => "PATTERN",
            Object::StructDef { .. } => "STRUCT_DEF",
            Object::StructInstance { .. } => "STRUCT",
            Object::Builtin(_) => "BUILTIN",
            Object::Return(_) => "RETURN",
            Object::Skip => "SKIP",
            Object::Stop => "STOP",
            Object::None => "NONE",
            Object::Error(_) => "ERROR",
        }
    }

    pub fn struct_type_name(&self) -> Option<&str> {
        match self {
            Object::StructInstance { struct_name, .. } => Some(struct_name),
            _ => None,
        }
    }

    pub fn effective_type_name(&self) -> String {
        match self {
            Object::StructInstance { struct_name, .. } => struct_name.clone(),
            other => other.type_name().to_string(),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(n) => write!(f, "{}", n),
            Object::Float(n) => write!(f, "{}", n),
            Object::Char(c) => write!(f, "`{}`", c),
            Object::String(s) => write!(f, "{}", s),
            Object::Boolean(b) => write!(f, "{}", if *b { "True" } else { "False" }),
            Object::Array(elements) => {
                let items: Vec<String> = elements.iter().map(|e| e.to_string()).collect();
                write!(f, "[{}]", items.join(", "))
            }
            Object::Function { parameters, .. } => {
                let params: Vec<String> = parameters.iter().map(|p| p.value.clone()).collect();
                write!(f, "fun({})", params.join(", "))
            }
            Object::Pattern { name, .. } => write!(f, "pattern {}", name),
            Object::StructDef { name, .. } => write!(f, "struct {}", name),
            Object::StructInstance { struct_name, fields } => {
                let fields_ref = fields.borrow();
                let items: Vec<String> = fields_ref
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect();
                write!(f, "{} {{ {} }}", struct_name, items.join(", "))
            }
            Object::Builtin(_) => write!(f, "builtin function"),
            Object::Return(val) => write!(f, "{}", val),
            Object::Skip => write!(f, "skip"),
            Object::Stop => write!(f, "stop"),
            Object::None => write!(f, "None"),
            Object::Error(msg) => write!(f, "Error: {}", msg),
        }
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(n) => write!(f, "Integer({})", n),
            Object::Float(n) => write!(f, "Float({})", n),
            Object::Char(c) => write!(f, "Char({:?})", c),
            Object::String(s) => write!(f, "String({:?})", s),
            Object::Boolean(b) => write!(f, "Boolean({})", b),
            Object::Array(elements) => write!(f, "Array({:?})", elements),
            Object::Function { parameters, .. } => {
                write!(f, "Function({:?})", parameters)
            }
            Object::Pattern { name, .. } => write!(f, "Pattern({})", name),
            Object::StructDef { name, .. } => write!(f, "StructDef({})", name),
            Object::StructInstance { struct_name, fields } => {
                write!(f, "StructInstance({}, {:?})", struct_name, fields.borrow())
            }
            Object::Builtin(_) => write!(f, "Builtin"),
            Object::Return(val) => write!(f, "Return({:?})", val),
            Object::Skip => write!(f, "Skip"),
            Object::Stop => write!(f, "Stop"),
            Object::None => write!(f, "None"),
            Object::Error(msg) => write!(f, "Error({:?})", msg),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Integer(a), Object::Integer(b)) => a == b,
            (Object::Float(a), Object::Float(b)) => a == b,
            (Object::Char(a), Object::Char(b)) => a == b,
            (Object::String(a), Object::String(b)) => a == b,
            (Object::Boolean(a), Object::Boolean(b)) => a == b,
            (Object::None, Object::None) => true,
            (Object::Array(a), Object::Array(b)) => a == b,
            (Object::StructInstance { struct_name: a_name, fields: a_fields },
             Object::StructInstance { struct_name: b_name, fields: b_fields }) => {
                a_name == b_name && *a_fields.borrow() == *b_fields.borrow()
            }
            _ => false,
        }
    }
}
