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
/// Numeric tag of `Value::None` — the seventh variant under the
/// `#[repr(u8)]` layout. The JIT's inline MethodCall expansion writes
/// this tag into the receiver slot after running the inlined body to
/// leave a "return value of None" on the stack. Pinned by
/// `value_none_tag_is_pinned`.
pub const VALUE_TAG_NONE: u8 = 6;

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
/// Byte offset of the field-storage raw pointer inside `ObjStructInstance`.
/// The JIT loads this as `ptr_ty` to reach the start of the `Value` buffer
/// owned by the instance without going through a `RefCell`/`Vec`
/// indirection. Pinned by `layout_tests::struct_instance_fields_layout_is_pinned`.
pub const STRUCT_INSTANCE_FIELDS_PTR_OFFSET: usize = core::mem::offset_of!(ObjStructInstance, fields)
    + core::mem::offset_of!(InstanceFields, ptr);
pub const STRUCT_INSTANCE_FIELDS_LEN_OFFSET: usize = core::mem::offset_of!(ObjStructInstance, fields)
    + core::mem::offset_of!(InstanceFields, len);

#[cfg(test)]
mod layout_tests {
    use super::*;

    /// The JIT's inline int fast path relies on these exact layout
    /// invariants — if any of them break, disable the fast path until
    /// the layout is re-verified.
    /// Pinned at 40 bytes. The JIT emits stack stride arithmetic using
    /// the `VALUE_SIZE` constant (which is `size_of::<Value>()`), so a
    /// change here is picked up automatically in emitted IR — but a
    /// change ALSO shifts the interaction with `VALUE_INT_PAYLOAD_OFFSET`
    /// (the payload still starts at 8), per-Value cache pressure, and
    /// the stack-capacity memory footprint (STACK_MAX * VALUE_SIZE).
    /// Any move off 40 is a perf event worth rebenchmarking.
    ///
    /// 16 is the aligned width of an 8-byte Rc pointer plus a 1-byte tag
    /// and 7-byte alignment padding. A1.1a boxed `ErrorValue`; A1.1b
    /// converted `Rc<str>` → `Rc<String>` in the two remaining fat-
    /// pointer variants (`String`, `Error`). Eventual target is 8 via
    /// NaN-box (A1.2+).
    #[test]
    fn value_size_is_pinned_at_16() {
        assert_eq!(
            VALUE_SIZE, 16,
            "VALUE_SIZE is baked into JIT inline stack ops. A change \
             here requires rebenching the bench suite and auditing \
             the emit_inline_* functions in engine.rs."
        );
    }

    /// The JIT's inline `replace_top2_with_bool` writes a Value::Boolean
    /// bit pattern directly: tag byte at offset 0, bool payload byte at
    /// offset 1. If the enum is reordered or Rust's repr(u8) layout for
    /// Bool changes, every fused int comparison silently corrupts values.
    #[test]
    fn value_bool_tag_and_payload_are_pinned() {
        let v_true = Value::Boolean(true);
        let ptr = &v_true as *const Value as *const u8;
        let tag = unsafe { *ptr };
        assert_eq!(tag, 3, "Boolean tag must be 3 (4th variant in Value enum)");
        let payload_byte = unsafe { *ptr.add(1) };
        assert_eq!(
            payload_byte, 1,
            "Boolean(true) payload must be at byte offset 1 with value 1"
        );

        let v_false = Value::Boolean(false);
        let ptr_f = &v_false as *const Value as *const u8;
        let tag_f = unsafe { *ptr_f };
        let payload_f = unsafe { *ptr_f.add(1) };
        assert_eq!(tag_f, 3);
        assert_eq!(payload_f, 0, "Boolean(false) payload byte must be 0");
    }

    /// The JIT's inline MethodCall expansion writes a `Value::None` tag
    /// byte at the receiver slot after running the inlined method body.
    /// If the variant order in `Value` changes, that write lands on the
    /// wrong slot semantics and every inline method-call corrupts the
    /// return value.
    #[test]
    fn value_none_tag_is_pinned() {
        let v = Value::None;
        let ptr = &v as *const Value as *const u8;
        let tag = unsafe { *ptr };
        assert_eq!(
            tag, VALUE_TAG_NONE,
            "None tag must equal VALUE_TAG_NONE (= 6, the 7th variant)",
        );
    }

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
            specialized_thunk: Cell::new(None),
            specialized_arity: Cell::new(0),
            specialized_kind: Cell::new(0),
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

    /// The JIT's inline MethodCall fast path relies on `Rc<T>` being
    /// `NonNull<RcBox<T>>` with `strong: Cell<usize>` at RcBox offset 0.
    /// If a future Rust reorders `RcBox` or changes the `Cell<usize>`
    /// layout, the JIT's inline refcount bump would corrupt the wrong
    /// field and cause silent memory corruption. Catch that at
    /// `cargo test` time rather than at user segfault time.
    #[test]
    fn rc_strong_count_lives_at_rcbox_offset_zero() {
        use std::cell::Cell;
        let func = Rc::new(Function::new(None, 0));
        let obj = Rc::new(ObjClosure {
            function: func,
            upvalues: Vec::new(),
            call_count: Cell::new(0),
            loop_count: Cell::new(0),
            jit_state: Cell::new(0),
            jit_thunk: Cell::new(None),
            specialized_thunk: Cell::new(None),
            specialized_arity: Cell::new(0),
            specialized_kind: Cell::new(0),
        });
        // Read the RcBox pointer the same way the JIT does: raw bit
        // pattern of the `Rc`, which is `NonNull<RcBox<T>>`.
        let rcbox_ptr: *const usize =
            unsafe { *(&obj as *const Rc<ObjClosure> as *const *const usize) };
        let before = unsafe { *rcbox_ptr };
        assert!(before >= 1, "strong count should reflect the live Rc");
        let extra = Rc::clone(&obj);
        let after = unsafe { *rcbox_ptr };
        assert_eq!(
            after,
            before + 1,
            "Rc::clone must bump the usize at RcBox+0 — if this fails, \
             the JIT MethodCall inline refcount bump is unsound."
        );
        drop(extra);
        let restored = unsafe { *rcbox_ptr };
        assert_eq!(restored, before, "drop must restore the strong count");
    }

    /// The JIT's inline GetField / SetField / struct-field-add fast paths
    /// load the field buffer pointer from `ObjStructInstance.fields.ptr`
    /// at a constant byte offset, then index it directly. If the struct
    /// layout is reordered or `InstanceFields` changes shape, every
    /// inline field access would silently read/write the wrong slot.
    /// Pinned here so a layout change fails at `cargo test` time.
    #[test]
    fn struct_instance_fields_layout_is_pinned() {
        use crate::vm::value::{
            FieldLayout, ObjStructDef, ObjStructInstance, Value, STRUCT_INSTANCE_DEF_OFFSET,
            STRUCT_INSTANCE_FIELDS_LEN_OFFSET, STRUCT_INSTANCE_FIELDS_PTR_OFFSET,
            VALUE_INT_PAYLOAD_OFFSET, VALUE_SIZE,
        };
        let mut indices = HashMap::new();
        indices.insert("a".to_string(), 0);
        indices.insert("b".to_string(), 1);
        let layout = Rc::new(FieldLayout {
            slots: vec![
                ("a".to_string(), "int".to_string(), false),
                ("b".to_string(), "int".to_string(), false),
            ],
            indices,
        });
        let def = Rc::new(ObjStructDef {
            name: "Pair".to_string(),
            fields: vec![
                ("a".to_string(), "int".to_string(), false),
                ("b".to_string(), "int".to_string(), false),
            ],
            methods: RefCell::new(HashMap::new()),
            parent: None,
            layout: std::cell::OnceCell::new(),
        });
        let inst = ObjStructInstance::new(
            "Pair".to_string(),
            vec![Value::Integer(0xAAAA), Value::Integer(0xBBBB)],
            layout,
            def,
        );
        let base = &inst as *const ObjStructInstance as *const u8;
        // Reading through the pinned offset must reach the raw fields ptr.
        let ptr_read =
            unsafe { base.add(STRUCT_INSTANCE_FIELDS_PTR_OFFSET).cast::<*mut Value>().read() };
        assert_eq!(
            ptr_read, inst.fields.ptr,
            "STRUCT_INSTANCE_FIELDS_PTR_OFFSET must reach ObjStructInstance.fields.ptr",
        );
        let len_read = unsafe { base.add(STRUCT_INSTANCE_FIELDS_LEN_OFFSET).cast::<u32>().read() };
        assert_eq!(
            len_read, inst.fields.len,
            "STRUCT_INSTANCE_FIELDS_LEN_OFFSET must reach ObjStructInstance.fields.len",
        );
        // Reading a field through raw-ptr arithmetic matches `get_field`.
        let slot_ptr = unsafe { ptr_read.add(1) } as *const u8;
        let tag = unsafe { *slot_ptr };
        let payload = unsafe { slot_ptr.add(VALUE_INT_PAYLOAD_OFFSET).cast::<i64>().read() };
        assert_eq!(tag, 0, "field 1 tag must be Integer (0)");
        assert_eq!(payload, 0xBBBB, "field 1 payload must match constructor");
        let _ = VALUE_SIZE;
        let _ = STRUCT_INSTANCE_DEF_OFFSET;
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

    // Heap-allocated. `String` holds `Rc<String>` (not `Rc<str>`) so the
    // pointer stays thin (8 B); the fat variant wasted 8 B per Value
    // across every stack slot. See roadmap A1.1b.
    String(Rc<String>),
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
    /// Boxed to keep `Value` down to 24 bytes. The inline form would
    /// carry two `Rc<str>` fat pointers (32 B) which dominated the enum
    /// size. See `docs/optimization-roadmap.md` A1 for the broader plan.
    ErrorValue(Rc<ErrorValueData>),
    /// Value(...) wrapper (success side of error handling)
    Wrapped(Rc<Value>),
    /// Terminal error — stops execution. Thin `Rc<String>` for the same
    /// reason as `String` above.
    Error(Rc<String>),
}

/// Boxed payload of `Value::ErrorValue`. Kept off the enum so `Value`
/// stays small; cloned by `Rc::clone` on every enum clone.
#[derive(Debug, Clone)]
pub struct ErrorValueData {
    pub msg: Rc<String>,
    pub tag: Option<Rc<String>>,
}

/// Helper to produce `Rc<String>` from anything that can be turned into
/// a `String`. Replaces the `.into()` → `Rc<str>` idiom that broke when
/// `String(Rc<str>)` became `String(Rc<String>)` in A1.1b.
#[inline]
pub fn rc_str(s: impl Into<String>) -> Rc<String> {
    Rc::new(s.into())
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
    /// A2: specialized i64-ABI entry point, populated alongside
    /// `jit_thunk` when the function qualifies per
    /// `slot_types.specialized_entry_eligible`. Opaque pointer — only
    /// invoked via Cranelift `call_indirect` with the specialized
    /// signature `fn(*mut VM, i64, ..., i64) -> (i64, u32)`. None when
    /// the function is ineligible or not yet compiled.
    pub specialized_thunk: Cell<Option<*const ()>>,
    /// Arity of the specialized entry point. Always equal to
    /// `function.arity` when `specialized_thunk` is Some, 0 otherwise.
    /// Callers validate this against the Call opcode's arg_count
    /// before dispatching to the specialized entry.
    pub specialized_arity: Cell<u8>,
    /// Discriminator for `specialized_thunk`: does it point at a
    /// forward trampoline (A2 adapter) or a native int body?
    /// Encoded as `u8` so it can sit in a Cell alongside the other
    /// JIT-state fields without needing Debug/PartialEq on the enum.
    /// 0 = None (no specialized entry)
    /// 1 = ForwardTrampoline
    /// 2 = NativeIntBody
    /// A3 direct-call dispatch gates on `== 2`; trampolines never
    /// receive direct-call traffic.
    pub specialized_kind: Cell<u8>,
}

/// u8 encoding of `engine::SpecializedEntryKind` stored in
/// `ObjClosure.specialized_kind`. Callers should treat these as
/// opaque tags and compare to the constants.
pub const SPECIALIZED_KIND_NONE: u8 = 0;
pub const SPECIALIZED_KIND_FORWARD_TRAMPOLINE: u8 = 1;
pub const SPECIALIZED_KIND_NATIVE_INT_BODY: u8 = 2;

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

/// JIT-addressable field storage header for an `ObjStructInstance`.
///
/// `#[repr(C)]` so the layout is stable: the JIT can load `ptr` at a
/// fixed offset from the instance and index directly into the heap
/// buffer. Owns the buffer; freed when the enclosing `ObjStructInstance`
/// is dropped.
///
/// Field count is fixed at construction and never grows, so `cap == len`
/// in practice. `cap` is retained because the backing allocation was
/// produced by `Vec` which may round up capacity.
#[repr(C)]
#[derive(Debug)]
pub struct InstanceFields {
    pub ptr: *mut Value,
    pub len: u32,
    pub cap: u32,
}

/// A struct instance at runtime. Fields live in a raw heap buffer indexed
/// by the struct def's `FieldLayout`. `#[repr(C)]` + hot fields first so
/// the JIT can reach `fields.ptr` / `def` at pinned offsets.
///
/// Single-threaded by construction (the enclosing `Rc` is `!Sync`), so
/// field mutations go through `set_field(&self, …)` which writes through
/// a raw pointer without a `RefCell` runtime borrow check. No caller
/// holds a `field_slice` borrow across a mutation — all mutation sites
/// take the value by clone first (audited in plan Phase 1).
#[repr(C)]
#[derive(Debug)]
pub struct ObjStructInstance {
    pub fields: InstanceFields,
    pub struct_name: String,
    /// Reference to the resolved layout so field indices stay correct.
    pub layout: std::rc::Rc<FieldLayout>,
    /// Direct reference to the struct def so method-call ICs don't need
    /// to look up `globals[struct_name]` on every dispatch. Stored as an
    /// `Rc` so identity compares match the def's stable allocation.
    pub def: std::rc::Rc<ObjStructDef>,
}

impl ObjStructInstance {
    /// Build a new instance from a `Vec<Value>` of fields. Takes ownership
    /// of the Vec's heap allocation — it will be freed in `Drop`.
    #[inline]
    pub fn new(
        struct_name: String,
        fields: Vec<Value>,
        layout: std::rc::Rc<FieldLayout>,
        def: std::rc::Rc<ObjStructDef>,
    ) -> Self {
        let mut v = std::mem::ManuallyDrop::new(fields);
        let ptr = v.as_mut_ptr();
        let len = v.len() as u32;
        let cap = v.capacity() as u32;
        ObjStructInstance {
            fields: InstanceFields { ptr, len, cap },
            struct_name,
            layout,
            def,
        }
    }

    /// Immutable view of all field slots. Used for equality, formatting,
    /// and read-only field reads via `get_field`.
    #[inline(always)]
    pub fn field_slice(&self) -> &[Value] {
        if self.fields.ptr.is_null() {
            return &[];
        }
        unsafe { std::slice::from_raw_parts(self.fields.ptr, self.fields.len as usize) }
    }

    /// Clone-read one field by index.
    #[inline(always)]
    pub fn get_field(&self, idx: usize) -> Value {
        debug_assert!(idx < self.fields.len as usize);
        unsafe { (*self.fields.ptr.add(idx)).clone() }
    }

    /// Replace one field by index. Drops the previous `Value` (which
    /// decrements any embedded Rc refcounts). Callers must not hold a
    /// `field_slice` reference when invoking this.
    #[inline(always)]
    pub fn set_field(&self, idx: usize, value: Value) {
        debug_assert!(idx < self.fields.len as usize);
        unsafe {
            let slot = self.fields.ptr.add(idx);
            std::ptr::drop_in_place(slot);
            std::ptr::write(slot, value);
        }
    }
}

impl Drop for ObjStructInstance {
    fn drop(&mut self) {
        if !self.fields.ptr.is_null() {
            // Reconstitute the original `Vec<Value>` so its allocator
            // frees the buffer and Drops each contained `Value`.
            unsafe {
                let _ = Vec::from_raw_parts(
                    self.fields.ptr,
                    self.fields.len as usize,
                    self.fields.cap as usize,
                );
            }
            self.fields.ptr = std::ptr::null_mut();
        }
    }
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
            Value::ErrorValue(data) => match &data.tag {
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
                let fields = si.field_slice();
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
            Value::ErrorValue(data) => match &data.tag {
                Some(tag) => write!(f, "Error {{ tag: {}, msg: {} }}", tag, data.msg),
                None => write!(f, "Error {{ msg: {} }}", data.msg),
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
                    si.field_slice()
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
            Value::ErrorValue(data) => {
                write!(f, "ErrorValue(tag={:?}, msg={:?})", data.tag, data.msg)
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
            (Value::ErrorValue(a), Value::ErrorValue(b)) => {
                a.msg == b.msg && a.tag == b.tag
            }
            (Value::Wrapped(a), Value::Wrapped(b)) => a == b,
            (Value::StructInstance(a), Value::StructInstance(b)) => {
                a.struct_name == b.struct_name && a.field_slice() == b.field_slice()
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
