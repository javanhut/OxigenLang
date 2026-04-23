pub mod environment;

use crate::ast::{Statement, TypedParam};
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
        parameters: Vec<TypedParam>,
        body: Vec<Statement>,
        env: Rc<RefCell<Environment>>,
    },
    StructDef {
        name: String,
        fields: Vec<(String, String, bool)>,
        methods: HashMap<String, Rc<Object>>,
        parent: Option<String>,
    },
    StructInstance {
        struct_name: String,
        fields: Rc<RefCell<HashMap<String, Rc<Object>>>>,
    },
    Byte(u8),
    Uint(u64),
    Tuple(Vec<Rc<Object>>),
    Map(Vec<(Rc<Object>, Rc<Object>)>),
    Set(Vec<Rc<Object>>),
    BoundMethod {
        parameters: Vec<TypedParam>,
        body: Vec<Statement>,
        env: Rc<RefCell<Environment>>,
        instance_fields: Rc<RefCell<HashMap<String, Rc<Object>>>>,
        field_names: Vec<String>,
    },
    Module {
        name: String,
        env: Rc<RefCell<Environment>>,
    },
    Builtin(BuiltinFunction),
    Return(Rc<Object>),
    Skip,
    Stop,
    None,
    ErrorValue {
        msg: String,
        tag: Option<String>,
    },
    Value(Rc<Object>),
    Error(String),
    EnumDef {
        name: String,
        variants: Vec<EnumVariantDef>,
    },
    EnumInstance {
        enum_name: String,
        variant_name: String,
        payload: EnumPayload,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariantDef {
    pub name: String,
    pub kind: EnumVariantKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum EnumVariantKind {
    Unit(Option<Rc<Object>>),
    Tuple(Vec<String>),
    Struct(Vec<String>),
}

#[derive(Clone, Debug)]
pub enum EnumPayload {
    Unit(Option<Rc<Object>>),
    Tuple(Vec<Rc<Object>>),
    Struct(Vec<(String, Rc<Object>)>),
}

impl PartialEq for EnumPayload {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (EnumPayload::Unit(a), EnumPayload::Unit(b)) => match (a, b) {
                (Some(av), Some(bv)) => av == bv,
                (None, None) => true,
                _ => false,
            },
            (EnumPayload::Tuple(a), EnumPayload::Tuple(b)) => a == b,
            (EnumPayload::Struct(a), EnumPayload::Struct(b)) => a == b,
            _ => false,
        }
    }
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
            Object::ErrorValue { .. } => true,
            Object::Value(_) => true,
            Object::StructInstance { .. } => true,
            Object::Module { .. } => true,
            Object::Byte(n) => *n != 0,
            Object::Uint(n) => *n != 0,
            Object::Tuple(t) => !t.is_empty(),
            Object::Map(m) => !m.is_empty(),
            Object::Set(s) => !s.is_empty(),
            Object::EnumDef { .. } => true,
            Object::EnumInstance { .. } => true,
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
            Object::StructDef { .. } => "STRUCT_DEF",
            Object::StructInstance { .. } => "STRUCT",
            Object::BoundMethod { .. } => "FUNCTION",
            Object::Byte(_) => "BYTE",
            Object::Uint(_) => "UINT",
            Object::Tuple(_) => "TUPLE",
            Object::Map(_) => "MAP",
            Object::Set(_) => "SET",
            Object::Module { .. } => "MODULE",
            Object::Builtin(_) => "BUILTIN",
            Object::Return(_) => "RETURN",
            Object::Skip => "SKIP",
            Object::Stop => "STOP",
            Object::None => "NONE",
            Object::ErrorValue { .. } => "ERROR",
            Object::Value(_) => "VALUE",
            Object::Error(_) => "ERROR",
            Object::EnumDef { .. } => "ENUM_DEF",
            Object::EnumInstance { .. } => "ENUM",
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
            Object::ErrorValue { tag, .. } => match tag {
                Some(tag) => format!("ERROR<{}>", tag),
                None => "ERROR".to_string(),
            },
            Object::Value(_) => "VALUE".to_string(),
            Object::EnumDef { name, .. } => name.clone(),
            Object::EnumInstance { enum_name, .. } => enum_name.clone(),
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
                let params: Vec<String> =
                    parameters.iter().map(|p| p.ident.value.clone()).collect();
                write!(f, "fun({})", params.join(", "))
            }
            Object::StructDef { name, .. } => write!(f, "struct {}", name),
            Object::StructInstance {
                struct_name,
                fields,
            } => {
                let fields_ref = fields.borrow();
                let items: Vec<String> = fields_ref
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect();
                write!(f, "{} {{ {} }}", struct_name, items.join(", "))
            }
            Object::BoundMethod { parameters, .. } => {
                let params: Vec<String> =
                    parameters.iter().map(|p| p.ident.value.clone()).collect();
                write!(f, "fun({})", params.join(", "))
            }
            Object::Byte(n) => write!(f, "{}", n),
            Object::Uint(n) => write!(f, "{}", n),
            Object::Tuple(elements) => {
                let items: Vec<String> = elements.iter().map(|e| e.to_string()).collect();
                if elements.len() == 1 {
                    write!(f, "({},)", items[0])
                } else {
                    write!(f, "({})", items.join(", "))
                }
            }
            Object::Map(entries) => {
                let items: Vec<String> = entries
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect();
                write!(f, "{{{}}}", items.join(", "))
            }
            Object::Set(elements) => {
                let items: Vec<String> = elements.iter().map(|e| e.to_string()).collect();
                write!(f, "set({})", items.join(", "))
            }
            Object::Module { name, .. } => write!(f, "module <{}>", name),
            Object::Builtin(_) => write!(f, "builtin function"),
            Object::Return(val) => write!(f, "{}", val),
            Object::Skip => write!(f, "skip"),
            Object::Stop => write!(f, "stop"),
            Object::None => write!(f, "None"),
            Object::ErrorValue { msg, tag } => match tag {
                Some(tag) => write!(f, "Error {{ tag: {}, msg: {} }}", tag, msg),
                None => write!(f, "Error {{ msg: {} }}", msg),
            },
            Object::Value(val) => write!(f, "Value({})", val),
            Object::Error(msg) => write!(f, "Error: {}", msg),
            Object::EnumDef { name, .. } => write!(f, "enum {}", name),
            Object::EnumInstance {
                enum_name,
                variant_name,
                payload,
            } => match payload {
                EnumPayload::Unit(_) => write!(f, "{}.{}", enum_name, variant_name),
                EnumPayload::Tuple(items) => {
                    let s: Vec<String> = items.iter().map(|v| v.to_string()).collect();
                    write!(f, "{}.{}({})", enum_name, variant_name, s.join(", "))
                }
                EnumPayload::Struct(fields) => {
                    let s: Vec<String> = fields
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, v))
                        .collect();
                    write!(f, "{}.{} {{ {} }}", enum_name, variant_name, s.join(", "))
                }
            },
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
            Object::StructDef { name, .. } => write!(f, "StructDef({})", name),
            Object::StructInstance {
                struct_name,
                fields,
            } => {
                write!(f, "StructInstance({}, {:?})", struct_name, fields.borrow())
            }
            Object::BoundMethod { parameters, .. } => write!(f, "BoundMethod({:?})", parameters),
            Object::Byte(n) => write!(f, "Byte({})", n),
            Object::Uint(n) => write!(f, "Uint({})", n),
            Object::Tuple(elements) => write!(f, "Tuple({:?})", elements),
            Object::Map(entries) => write!(f, "Map({:?})", entries),
            Object::Set(elements) => write!(f, "Set({:?})", elements),
            Object::Module { name, .. } => write!(f, "Module({})", name),
            Object::Builtin(_) => write!(f, "Builtin"),
            Object::Return(val) => write!(f, "Return({:?})", val),
            Object::Skip => write!(f, "Skip"),
            Object::Stop => write!(f, "Stop"),
            Object::None => write!(f, "None"),
            Object::ErrorValue { msg, tag } => {
                write!(f, "ErrorValue(tag={:?}, msg={:?})", tag, msg)
            }
            Object::Value(val) => write!(f, "Value({:?})", val),
            Object::Error(msg) => write!(f, "Error({:?})", msg),
            Object::EnumDef { name, variants } => {
                write!(f, "EnumDef({}, {:?})", name, variants)
            }
            Object::EnumInstance {
                enum_name,
                variant_name,
                payload,
            } => write!(
                f,
                "EnumInstance({}, {}, {:?})",
                enum_name, variant_name, payload
            ),
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
            (
                Object::ErrorValue { msg: a, tag: a_tag },
                Object::ErrorValue { msg: b, tag: b_tag },
            ) => a == b && a_tag == b_tag,
            (Object::Value(a), Object::Value(b)) => a == b,
            (Object::Array(a), Object::Array(b)) => a == b,
            (Object::Byte(a), Object::Byte(b)) => a == b,
            (Object::Uint(a), Object::Uint(b)) => a == b,
            (Object::Tuple(a), Object::Tuple(b)) => a == b,
            (Object::Map(a), Object::Map(b)) => a == b,
            (Object::Set(a), Object::Set(b)) => {
                a.len() == b.len() && a.iter().all(|item| b.iter().any(|bitem| item == bitem))
            }
            (
                Object::StructInstance {
                    struct_name: a_name,
                    fields: a_fields,
                },
                Object::StructInstance {
                    struct_name: b_name,
                    fields: b_fields,
                },
            ) => a_name == b_name && *a_fields.borrow() == *b_fields.borrow(),
            (Object::Module { name: a, .. }, Object::Module { name: b, .. }) => a == b,
            (
                Object::EnumInstance {
                    enum_name: a_e,
                    variant_name: a_v,
                    payload: a_p,
                },
                Object::EnumInstance {
                    enum_name: b_e,
                    variant_name: b_v,
                    payload: b_p,
                },
            ) => a_e == b_e && a_v == b_v && a_p == b_p,
            (Object::EnumDef { name: a, .. }, Object::EnumDef { name: b, .. }) => a == b,
            _ => false,
        }
    }
}
