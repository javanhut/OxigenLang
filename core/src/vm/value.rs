use crate::compiler::opcode::Chunk;
use crate::jit::CompiledThunk;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

/// Built-in function signature for the VM.
pub type BuiltinFn = fn(Vec<Value>) -> Value;

/// Numeric tag of `Value::Integer` under the `#[repr(u8)]` layout. The
/// JIT's fast path loads this byte from stack memory to decide between
/// inline arithmetic and the slow-path helper.
pub const VALUE_TAG_INTEGER: u8 = 0;

/// Numeric tag of `Value::Float`. Used by the JIT's inline GetLocal fast
/// path to skip the generic clone-and-push helper for float locals.
pub const VALUE_TAG_FLOAT: u8 = 1;

/// Numeric tag of `Value::Closure`. Used by the JIT's inline Call guard
/// to verify the callee on the stack is a Closure before loading its
/// `Rc<ObjClosure>` pointer for identity comparison. Keep in sync with
/// the variant order in `Value`.
pub const VALUE_TAG_CLOSURE: u8 = 12;
pub const VALUE_TAG_STRUCT_INSTANCE: u8 = 15;

/// Byte offset of the i64 payload inside a `Value::Integer(i64)`. With
/// `#[repr(u8)]` the discriminant occupies byte 0, and the payload is
/// aligned to the variant's natural alignment (8 for i64 / Rc). Used by
/// the JIT fast path.
pub const VALUE_INT_PAYLOAD_OFFSET: usize = 8;
/// Current `Rc<T>` payload offset from the raw `RcBox<T>` pointer stored in
/// `Value` payloads: two usize refcounts before `T`.
pub const RC_VALUE_OFFSET: usize = 16;
pub const REF_CELL_VALUE_OFFSET: usize = 8;

/// `size_of::<Value>()` — pinned by `#[repr(u8)]`. Const-evaluated at
/// build time.
pub const VALUE_SIZE: usize = core::mem::size_of::<Value>();

pub const STRUCT_INSTANCE_DEF_OFFSET: usize = core::mem::offset_of!(ObjStructInstance, def);
pub const STRUCT_INSTANCE_FIELDS_OFFSET: usize = core::mem::offset_of!(ObjStructInstance, fields);

#[cfg(test)]
mod layout_tests {
    use super::*;

    /// The JIT's inline int fast path relies on these exact layout
    /// invariants — if any of them break, disable the fast path until
    /// the layout is re-verified.
    #[test]
    fn value_integer_layout_is_pinned() {
        let v = Value::Integer(0xDEAD_BEEF_CAFE_BABE_u64 as i64);
        let ptr = &v as *const Value as *const u8;
        // Tag byte at offset 0.
        let tag = unsafe { *ptr };
        assert_eq!(tag, VALUE_TAG_INTEGER, "Integer tag should be 0");

        // i64 payload at VALUE_INT_PAYLOAD_OFFSET, host-endian.
        let payload = unsafe {
            ptr.add(VALUE_INT_PAYLOAD_OFFSET)
                .cast::<i64>()
                .read_unaligned()
        };
        assert_eq!(payload, 0xDEAD_BEEF_CAFE_BABE_u64 as i64);

        // Value size is a multiple of its alignment (8).
        assert_eq!(VALUE_SIZE % 8, 0);
    }

    #[test]
    fn value_float_layout_is_pinned() {
        let v = Value::Float(3.141592653589793);
        let ptr = &v as *const Value as *const u8;
        let tag = unsafe { *ptr };
        assert_eq!(tag, VALUE_TAG_FLOAT, "Float tag should be 1");
        let payload = unsafe {
            ptr.add(VALUE_INT_PAYLOAD_OFFSET)
                .cast::<u64>()
                .read_unaligned()
        };
        assert_eq!(f64::from_bits(payload), 3.141592653589793);
    }

    /// The JIT's inline Call guard reads the tag byte at offset 0 and
    /// the raw `Rc<ObjClosure>` bit pattern at offset 8 of a
    /// `Value::Closure`. The bit pattern is the `NonNull<RcBox<T>>` the
    /// `Rc` wraps (NOT `Rc::as_ptr`, which returns a pointer to T inside
    /// the box, offset past the refcount header). The cache populate
    /// code must use the same bit pattern so identity compare works.
    #[test]
    fn value_closure_layout_is_pinned() {
        use std::cell::Cell;
        let func = Rc::new(Function::new(None, 0));
        let obj = Rc::new(ObjClosure {
            function: func,
            upvalues: Vec::new(),
            call_count: Cell::new(0),
            loop_count: Cell::new(0),
            jit_state: Cell::new(0),
            jit_thunk: Cell::new(None),
        });
        // Read the Rc's raw bit pattern the same way the JIT does.
        let expected_raw: usize = unsafe {
            *(&obj as *const Rc<ObjClosure> as *const usize)
        };
        let v = Value::Closure(Rc::clone(&obj));
        let ptr = &v as *const Value as *const u8;
        let tag = unsafe { *ptr };
        assert_eq!(tag, VALUE_TAG_CLOSURE, "Closure tag should be 12");
        let rc_ptr = unsafe {
            ptr.add(VALUE_INT_PAYLOAD_OFFSET)
                .cast::<usize>()
                .read_unaligned()
        };
        assert_eq!(
            rc_ptr, expected_raw,
            "Value::Closure payload at offset 8 must equal the raw Rc bit pattern"
        );
        drop(v);
        drop(obj);
    }
}

/// Runtime value for the OxigenLang VM.
///
/// Unlike the tree-walking interpreter's `Object`, control flow signals
/// (Return, Skip, Stop) are NOT values — they are handled by VM mechanics.
///
/// `#[repr(u8)]` pins the discriminant byte to offset 0 so the JIT's
/// inline int+int fast path can read the tag directly from memory without
/// going through Rust enum pattern matching. The variant order below
/// defines the numeric tag values — do NOT reorder without updating
/// [`VALUE_TAG_INTEGER`] below.
#[derive(Clone)]
#[repr(u8)]
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

    // Enums
    EnumDef(Rc<ObjEnumDef>),
    EnumInstance(Rc<ObjEnumInstance>),

    // Module
    Module(Rc<ObjModule>),

    // Error handling
    ErrorValue {
        msg: Rc<str>,
        tag: Option<Rc<str>>,
    },
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
    pub locals: Vec<LocalInfo>,
    /// Whether the compiler emitted at least one backward-loop opcode for
    /// this function. Used by smart JIT tiering to compile loop-heavy
    /// single-call functions at entry instead of after they finish.
    pub has_loop: bool,
}

impl Function {
    pub fn new(name: Option<String>, arity: u8) -> Self {
        Function {
            name,
            arity,
            chunk: Chunk::new(),
            upvalue_count: 0,
            params: Vec::new(),
            locals: Vec::new(),
            has_loop: false,
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

/// Compiler-provided metadata for a runtime local slot. `mutable` tracks
/// value mutability; `type_constraint` tracks Oxigen's separate type lock.
#[derive(Debug, Clone, Default)]
pub struct LocalInfo {
    pub mutable: bool,
    pub type_constraint: Option<String>,
}

/// A closure: a compiled function + captured upvalues.
#[derive(Debug)]
pub struct ObjClosure {
    pub function: Rc<Function>,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
    /// How many times this closure has been called. Used by the JIT engine
    /// for hot-function detection. Interior mutability keeps closures
    /// cheaply shareable through `Rc`.
    pub call_count: Cell<u32>,
    /// Number of loop backedges executed by this closure. Used by smart
    /// JIT tiering so single-call long-running loops can compile.
    pub loop_count: Cell<u32>,
    /// Cached JIT state for this closure: 0 = unknown/cold, 1 = compiled,
    /// 2 = compile failed. This avoids a compiled-entry HashMap lookup on
    /// every hot recursive call.
    pub jit_state: Cell<u8>,
    /// Cached native entry point for compiled closures. Present only when
    /// `jit_state == 1`.
    pub jit_thunk: Cell<Option<CompiledThunk>>,
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
    pub fields: Vec<(String, String, bool)>, // (name, type, hidden) — own fields only
    pub methods: RefCell<HashMap<String, Value>>,
    pub parent: Option<String>,
    /// Cached flattened field layout — built on first instance creation by
    /// walking the inheritance chain. The Vec preserves insertion order
    /// (parent fields first, then own) and the HashMap maps name → index.
    /// OnceCell: each struct def resolves its layout exactly once.
    pub layout: std::cell::OnceCell<std::rc::Rc<FieldLayout>>,
}

/// Flattened field layout for a struct (including inherited fields).
/// `slots` owns the name+type+hidden tuples in canonical order; `indices`
/// maps field names to slot indices for O(1) name → offset resolution.
#[derive(Debug)]
pub struct FieldLayout {
    pub slots: Vec<(String, String, bool)>,
    pub indices: HashMap<String, usize>,
}

/// A struct instance at runtime. Fields are stored in a `Vec<Value>`
/// indexed by the struct def's `FieldLayout`, so field access is a direct
/// offset after the layout is resolved — no HashMap lookup per access.
#[derive(Debug)]
pub struct ObjStructInstance {
    pub struct_name: String,
    pub fields: RefCell<Vec<Value>>,
    /// Reference to the resolved layout so field indices stay correct.
    pub layout: std::rc::Rc<FieldLayout>,
    /// Direct reference to the struct def so method-call ICs don't need
    /// to look up `globals[struct_name]` on every dispatch. Stored as an
    /// `Rc` so identity compares match the def's stable allocation.
    pub def: std::rc::Rc<ObjStructDef>,
}

#[derive(Debug, Clone)]
pub enum VmEnumVariantKind {
    Unit(Option<Value>),
    Tuple(Vec<String>),
    Struct(Vec<String>),
}

#[derive(Debug, Clone)]
pub struct VmEnumVariantDef {
    pub name: String,
    pub kind: VmEnumVariantKind,
}

#[derive(Debug)]
pub struct ObjEnumDef {
    pub name: String,
    pub variants: Vec<VmEnumVariantDef>,
}

#[derive(Debug, Clone)]
pub enum VmEnumPayload {
    Unit(Option<Value>),
    Tuple(Vec<Value>),
    Struct(Vec<(String, Value)>),
}

#[derive(Debug)]
pub struct ObjEnumInstance {
    pub enum_name: String,
    pub variant_name: String,
    pub payload: VmEnumPayload,
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
            Value::EnumDef(_) => true,
            Value::EnumInstance(_) => true,
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
            Value::EnumDef(_) => "ENUM_DEF",
            Value::EnumInstance(_) => "ENUM",
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
            Value::EnumDef(d) => d.name.clone(),
            Value::EnumInstance(i) => i.enum_name.clone(),
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
                let items: Vec<String> = si
                    .layout
                    .slots
                    .iter()
                    .enumerate()
                    .map(|(i, slot)| format!("{}: {}", slot.0, fields[i]))
                    .collect();
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
                let items: Vec<String> = elements.borrow().iter().map(|e| e.to_string()).collect();
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
            Value::EnumDef(d) => write!(f, "enum {}", d.name),
            Value::EnumInstance(i) => match &i.payload {
                VmEnumPayload::Unit(_) => write!(f, "{}.{}", i.enum_name, i.variant_name),
                VmEnumPayload::Tuple(items) => {
                    let s: Vec<String> = items.iter().map(|v| v.to_string()).collect();
                    write!(f, "{}.{}({})", i.enum_name, i.variant_name, s.join(", "))
                }
                VmEnumPayload::Struct(fields) => {
                    let s: Vec<String> = fields
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, v))
                        .collect();
                    write!(f, "{}.{} {{ {} }}", i.enum_name, i.variant_name, s.join(", "))
                }
            },
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
                write!(
                    f,
                    "StructInstance({}, {:?})",
                    si.struct_name,
                    si.fields.borrow()
                )
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
            Value::EnumDef(d) => write!(f, "EnumDef({})", d.name),
            Value::EnumInstance(i) => {
                write!(
                    f,
                    "EnumInstance({}, {}, {:?})",
                    i.enum_name, i.variant_name, i.payload
                )
            }
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
            (Value::ErrorValue { msg: a, tag: at }, Value::ErrorValue { msg: b, tag: bt }) => {
                a == b && at == bt
            }
            (Value::Wrapped(a), Value::Wrapped(b)) => a == b,
            (Value::StructInstance(a), Value::StructInstance(b)) => {
                a.struct_name == b.struct_name && *a.fields.borrow() == *b.fields.borrow()
            }
            (Value::Module(a), Value::Module(b)) => a.name == b.name,
            (Value::EnumDef(a), Value::EnumDef(b)) => a.name == b.name,
            (Value::EnumInstance(a), Value::EnumInstance(b)) => {
                a.enum_name == b.enum_name
                    && a.variant_name == b.variant_name
                    && payload_eq(&a.payload, &b.payload)
            }
            _ => false,
        }
    }
}

fn payload_eq(a: &VmEnumPayload, b: &VmEnumPayload) -> bool {
    match (a, b) {
        (VmEnumPayload::Unit(av), VmEnumPayload::Unit(bv)) => match (av, bv) {
            (Some(x), Some(y)) => x == y,
            (None, None) => true,
            _ => false,
        },
        (VmEnumPayload::Tuple(x), VmEnumPayload::Tuple(y)) => x == y,
        (VmEnumPayload::Struct(x), VmEnumPayload::Struct(y)) => {
            x.len() == y.len()
                && x.iter()
                    .zip(y.iter())
                    .all(|((ak, av), (bk, bv))| ak == bk && av == bv)
        }
        _ => false,
    }
}
