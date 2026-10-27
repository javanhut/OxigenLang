use crate::compiler::opcode::Chunk;
use crate::jit::CompiledThunk;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

/// Built-in function signature for the VM.
pub type BuiltinFn = fn(&[Value]) -> Value;

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
pub const STRUCT_INSTANCE_FIELDS_PTR_OFFSET: usize =
    core::mem::offset_of!(ObjStructInstance, fields) + core::mem::offset_of!(InstanceFields, ptr);
pub const STRUCT_INSTANCE_FIELDS_LEN_OFFSET: usize =
    core::mem::offset_of!(ObjStructInstance, fields) + core::mem::offset_of!(InstanceFields, len);

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
        let v = Value::Float(std::f64::consts::PI);
        let ptr = &v as *const Value as *const u8;
        let tag = unsafe { *ptr };
        assert_eq!(tag, VALUE_TAG_FLOAT, "Float tag should be 1");
        let payload = unsafe {
            ptr.add(VALUE_INT_PAYLOAD_OFFSET)
                .cast::<u64>()
                .read_unaligned()
        };
        assert_eq!(f64::from_bits(payload), std::f64::consts::PI);
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
        let (kinds, values) = super::make_upvalue_int_caches(0);
        let obj = Rc::new(ObjClosure {
            function: func,
            upvalues: Vec::new(),
            module_globals: crate::vm::value::ModuleGlobals::none(),
            call_count: Cell::new(0),
            loop_count: Cell::new(0),
            jit_state: Cell::new(0),
            jit_bailouts: Cell::new(0),
            jit_thunk: Cell::new(None),
            specialized_thunk: Cell::new(None),
            specialized_arity: Cell::new(0),
            specialized_kind: Cell::new(0),
            lean_thunk: Cell::new(None),
            lean_arity: Cell::new(0),
            upvalue_int_kinds: kinds,
            upvalue_int_values: values,
        });
        // Read the Rc's raw bit pattern the same way the JIT does.
        let expected_raw: usize = unsafe { *(&obj as *const Rc<ObjClosure> as *const usize) };
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

    /// JIT-emitted call sequences read a callee's owning module with a single
    /// pointer load at `offset_of!(ObjClosure, module_globals)`, so the raw
    /// mirror must stay the first field of the `#[repr(C)]` `ModuleGlobals`.
    /// If it moves, every call under a module frame stores a garbage pointer
    /// into the new `JitFrame` and global lookups read freed memory.
    #[test]
    fn module_globals_ptr_is_first_field() {
        let dict = Rc::new(HashMap::from([("x".to_string(), Value::Integer(1))]));
        let mg = ModuleGlobals::from(Some(Rc::clone(&dict)));

        let first_word =
            unsafe { *(&mg as *const ModuleGlobals as *const *const HashMap<String, Value>) };
        assert_eq!(
            first_word,
            Rc::as_ptr(&dict),
            "raw view must be at offset 0"
        );
        assert_eq!(first_word, mg.as_ptr());
        assert!(mg.get().unwrap().contains_key("x"));

        mg.set(None);
        assert!(mg.as_ptr().is_null(), "clearing must clear both views");
        assert!(mg.get().is_none());
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
        let (kinds, values) = super::make_upvalue_int_caches(0);
        let obj = Rc::new(ObjClosure {
            function: func,
            upvalues: Vec::new(),
            module_globals: crate::vm::value::ModuleGlobals::none(),
            call_count: Cell::new(0),
            loop_count: Cell::new(0),
            jit_state: Cell::new(0),
            jit_bailouts: Cell::new(0),
            jit_thunk: Cell::new(None),
            specialized_thunk: Cell::new(None),
            specialized_arity: Cell::new(0),
            specialized_kind: Cell::new(0),
            lean_thunk: Cell::new(None),
            lean_arity: Cell::new(0),
            upvalue_int_kinds: kinds,
            upvalue_int_values: values,
        });
        // Read the raw Rc bit pattern the way the JIT does: NonNull<RcBox<T>>.
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
            FieldLayout, ObjStructDef, ObjStructInstance, STRUCT_INSTANCE_DEF_OFFSET,
            STRUCT_INSTANCE_FIELDS_LEN_OFFSET, STRUCT_INSTANCE_FIELDS_PTR_OFFSET,
            VALUE_INT_PAYLOAD_OFFSET, VALUE_SIZE, Value,
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
            module_globals: crate::vm::value::ModuleGlobals::none(),
        });
        let inst = ObjStructInstance::new(
            "Pair".to_string(),
            vec![Value::Integer(0xAAAA), Value::Integer(0xBBBB)],
            layout,
            def,
        );
        let base = &inst as *const ObjStructInstance as *const u8;
        // Reading through the pinned offset must reach the raw fields ptr.
        let ptr_read = unsafe {
            base.add(STRUCT_INSTANCE_FIELDS_PTR_OFFSET)
                .cast::<*mut Value>()
                .read()
        };
        assert_eq!(
            ptr_read, inst.fields.ptr,
            "STRUCT_INSTANCE_FIELDS_PTR_OFFSET must reach ObjStructInstance.fields.ptr",
        );
        let len_read = unsafe {
            base.add(STRUCT_INSTANCE_FIELDS_LEN_OFFSET)
                .cast::<u32>()
                .read()
        };
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

/// The elements of an array or tuple: a `Vec<Value>` (it derefs to one) that
/// frees deep nesting without recursing.
///
/// Dropping `[[[...]]]` through the generated drop glue takes one native frame
/// per level, so a few hundred thousand levels overflow the stack. `Value`
/// itself can't implement `Drop` without forbidding hosts from moving out of
/// it in a `match`, so the containers do: see [`drop_values`].
/// `repr(transparent)` keeps the layout the JIT reads identical to the `Vec`.
#[derive(Clone, Default, PartialEq)]
#[repr(transparent)]
pub struct ValueVec(pub Vec<Value>);

impl ValueVec {
    /// The elements, leaving this empty.
    pub fn into_vec(mut self) -> Vec<Value> {
        std::mem::take(&mut self.0)
    }
}

impl std::ops::Deref for ValueVec {
    type Target = Vec<Value>;
    #[inline(always)]
    fn deref(&self) -> &Vec<Value> {
        &self.0
    }
}

impl std::ops::DerefMut for ValueVec {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Vec<Value> {
        &mut self.0
    }
}

impl From<Vec<Value>> for ValueVec {
    #[inline(always)]
    fn from(v: Vec<Value>) -> Self {
        ValueVec(v)
    }
}

impl FromIterator<Value> for ValueVec {
    fn from_iter<I: IntoIterator<Item = Value>>(iter: I) -> Self {
        ValueVec(iter.into_iter().collect())
    }
}

impl IntoIterator for ValueVec {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Value>;
    fn into_iter(self) -> Self::IntoIter {
        self.into_vec().into_iter()
    }
}

impl<'a> IntoIterator for &'a ValueVec {
    type Item = &'a Value;
    type IntoIter = std::slice::Iter<'a, Value>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl fmt::Debug for ValueVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Drop for ValueVec {
    fn drop(&mut self) {
        if self.0.iter().any(Value::may_own_values) {
            drop_nested(std::mem::take(&mut self.0));
        }
    }
}

thread_local! {
    /// How many container drops are on the native stack right now.
    static DROP_DEPTH: Cell<u32> = const { Cell::new(0) };
}

/// Nesting the ordinary recursive drop may take before [`drop_values`] takes
/// over. Shallow values, the common case, keep the plain drop glue.
const RECURSIVE_DROP_LIMIT: u32 = 64;

/// Drops the contents of a container that holds other containers: by plain
/// recursion while shallow, iteratively once deep.
pub(crate) fn drop_nested(values: Vec<Value>) {
    DROP_DEPTH.with(|depth| {
        let d = depth.get();
        if d < RECURSIVE_DROP_LIMIT {
            depth.set(d + 1);
            drop(values);
            depth.set(d);
        } else {
            drop_values(values);
        }
    });
}

/// Drops `pending` and everything only it keeps alive, without recursing.
///
/// A container whose last reference is being dropped hands its elements back
/// to the work list before it goes, so it is freed empty; a shared one only
/// loses a reference. Native stack use stays constant however deep the value
/// is, and the work list reuses `pending`'s own buffer.
pub(crate) fn drop_values(mut pending: Vec<Value>) {
    while let Some(value) = pending.pop() {
        match value {
            Value::Array(rc) => {
                if let Ok(cell) = Rc::try_unwrap(rc) {
                    pending.append(&mut cell.into_inner().0);
                }
            }
            Value::Tuple(rc) => {
                if let Ok(mut items) = Rc::try_unwrap(rc) {
                    pending.append(&mut items.0);
                }
            }
            Value::Map(rc) => {
                if let Ok(cell) = Rc::try_unwrap(rc) {
                    cell.into_inner().drain_into(&mut pending);
                }
            }
            Value::Set(rc) => {
                if let Ok(cell) = Rc::try_unwrap(rc) {
                    cell.into_inner().drain_into(&mut pending);
                }
            }
            Value::StructInstance(rc) => {
                if let Ok(mut inst) = Rc::try_unwrap(rc) {
                    pending.extend(inst.take_fields());
                }
            }
            Value::EnumInstance(rc) => {
                if let Ok(mut inst) = Rc::try_unwrap(rc) {
                    inst.take_payload_into(&mut pending);
                }
            }
            Value::Wrapped(rc) => {
                if let Ok(inner) = Rc::try_unwrap(rc) {
                    pending.push(inner);
                }
            }
            other => drop(other),
        }
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

    // Rc<String> not Rc<str>, so the pointer stays thin; the fat variant cost 8 B per Value.
    String(Rc<String>),
    Array(Rc<RefCell<ValueVec>>),
    Tuple(Rc<ValueVec>),
    Map(Rc<RefCell<crate::vm::collections::OxMap>>),
    Set(Rc<RefCell<crate::vm::collections::OxSet>>),

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

    // Concurrency: a spawned task's handle. Auto-joins when consumed.
    Task(Rc<crate::concurrent::TaskHandle>),

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

    // Host embedding. Both are `Rc`s: the JIT treats every tag past `None`
    // except `Builtin` as an `Rc` it may refcount inline.
    /// A Rust closure the host registered (see `VM::register_native`).
    Native(Rc<NativeFunction>),
    /// A Rust value the host handed to the script (see [`Value::host`]).
    Host(Rc<HostValue>),

    /// `vec2`/`vec3`/`vec4` (see `vm::vector`). An `Rc` for the same reason.
    Vector(Rc<crate::vm::vector::Vector>),
}

/// The closure behind a [`Value::Native`].
pub type NativeFn = dyn Fn(&[Value]) -> Result<Value, String>;

/// A host-registered function: its name for messages, and the closure.
pub struct NativeFunction {
    pub name: String,
    pub(crate) func: Box<NativeFn>,
}

/// The payload of a [`Value::Host`]: a Rust value, the name `type()`
/// reports for it, and how to compare two of them.
pub struct HostValue {
    pub type_name: String,
    data: Box<dyn std::any::Any>,
    eq: fn(&dyn std::any::Any, &dyn std::any::Any) -> bool,
}

impl HostValue {
    fn same_value(&self, other: &HostValue) -> bool {
        (self.eq)(self.data.as_ref(), other.data.as_ref())
    }
}

/// "Read-only view" enum that mirrors the variant structure of
/// [`Value`]. Returned from [`Value::repr`].
///
/// Why this exists: A1.2.5 (NaN-box flag-day) replaces the storage of
/// `Value` with an 8-byte NaN-boxed payload. Pattern matching directly
/// on `Value::Variant(...)` will break at that swap. Migrating call
/// sites from `match &v { Value::X(p) => ... }` to
/// `match v.repr() { ValueRepr::X(p) => ... }` decouples consumers
/// from the storage shape — the swap then becomes a localized change
/// to `Value::repr()` (re-materializing variants from NaN-box accessors)
/// without touching every call site.
///
/// Encoding choices:
/// - Primitives are by value (no `&` / `*` indirection at the call
///   site), matching what NanValue's accessors return after the swap.
/// - Rc-bearing variants borrow the existing `Rc<T>` from the enum
///   payload during Phase 1, so this view is allocation-free. After
///   the swap, the impl will reconstruct typed `Rc` references via
///   `ManuallyDrop` so consumer code stays unchanged.
///
/// Variant names match `Value` exactly so the migration is a
/// sed-friendly rename: `Value::` → `ValueRepr::` inside match arms.
pub enum ValueRepr<'a> {
    Integer(i64),
    Float(f64),
    Char(char),
    Boolean(bool),
    Byte(u8),
    Uint(u64),
    None,
    String(&'a Rc<String>),
    Array(&'a Rc<RefCell<ValueVec>>),
    Tuple(&'a Rc<ValueVec>),
    Map(&'a Rc<RefCell<crate::vm::collections::OxMap>>),
    Set(&'a Rc<RefCell<crate::vm::collections::OxSet>>),
    Closure(&'a Rc<ObjClosure>),
    Builtin(BuiltinFn),
    StructDef(&'a Rc<ObjStructDef>),
    StructInstance(&'a Rc<ObjStructInstance>),
    EnumDef(&'a Rc<ObjEnumDef>),
    EnumInstance(&'a Rc<ObjEnumInstance>),
    Module(&'a Rc<ObjModule>),
    ErrorValue(&'a Rc<ErrorValueData>),
    Wrapped(&'a Rc<Value>),
    Error(&'a Rc<String>),
    Task(&'a Rc<crate::concurrent::TaskHandle>),
    Native(&'a Rc<NativeFunction>),
    Host(&'a Rc<HostValue>),
    Vector(&'a Rc<crate::vm::vector::Vector>),
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

/// The file a function's code was compiled from.
///
/// A frame's line number only means something alongside the source it indexes
/// into. Without this, every frame rendered against whichever file the VM
/// happened to be started with, so an error raised inside an imported function
/// underlined an unrelated line of the entry script.
///
/// One instance is shared by every function compiled from the same file.
#[derive(Debug)]
pub struct ModuleOrigin {
    pub file: String,
    pub source: String,
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
    /// `(ip, slot)` for every local declaration: `ip` is the code length when
    /// the compiler bound the name, so the initializer (if any) is the
    /// instruction ending there. Slot reuse across scopes means one slot can
    /// appear several times. The JIT's slot-type pass reads it to tell a
    /// local's initializer from a transient that happens to share its slot.
    pub local_decls: Vec<(usize, u16)>,
    /// Whether the compiler emitted at least one backward-loop opcode for
    /// this function. Used by smart JIT tiering to compile loop-heavy
    /// single-call functions at entry instead of after they finish.
    pub has_loop: bool,
    /// Unique in the process: the compilation unit in the high 32 bits,
    /// compile order in the low 32. The concurrency layer uses it to identify
    /// a closure's code across threads — a worker rebuilds the same
    /// id→Function table by compiling the same program under the same unit.
    pub id: u64,
    /// Which file this was compiled from. Stamped after compilation by
    /// `set_module_origin` — the same post-hoc wiring `module_globals` uses,
    /// since the compiler itself has no notion of files. `None` means the
    /// entry script, whose source the VM already holds.
    pub origin: RefCell<Option<Rc<ModuleOrigin>>>,
    /// Top-level names this file declared with `hide`. Only ever non-empty on
    /// a script function (the whole-file body); `import_module` reads it to
    /// build the module's private set.
    pub hidden_globals: Vec<String>,
    /// When this function is a struct method, the name of the struct whose
    /// `includes` block defined it — nested closures inherit it. Hidden-field
    /// access is permitted only from code carrying the owning struct's name.
    pub method_of: Option<String>,
    /// Whether any parameter carries a type annotation. Computed on first use
    /// (after the compiler has filled `params`) so every call path can afford
    /// to ask before choosing a fast path that skips the parameter check.
    pub(crate) typed_params: std::cell::OnceCell<bool>,
}

impl Value {
    /// A Rust value handed to scripts. Scripts can store and pass it but not
    /// look inside; `type()` reports `type_name`, and two host values are
    /// equal when they hold equal values of the same Rust type.
    pub fn host<T: std::any::Any + PartialEq>(type_name: &str, value: T) -> Value {
        fn eq<T: std::any::Any + PartialEq>(a: &dyn std::any::Any, b: &dyn std::any::Any) -> bool {
            match (a.downcast_ref::<T>(), b.downcast_ref::<T>()) {
                (Some(a), Some(b)) => a == b,
                _ => false,
            }
        }
        Value::Host(Rc::new(HostValue {
            type_name: type_name.to_string(),
            data: Box::new(value),
            eq: eq::<T>,
        }))
    }

    /// The Rust value inside a host value, if it holds a `T`.
    pub fn host_ref<T: std::any::Any>(&self) -> Option<&T> {
        match self {
            Value::Host(h) => h.data.downcast_ref::<T>(),
            _ => None,
        }
    }
}

impl Function {
    /// Stack slots that must be free to call this function.
    pub(crate) fn stack_reserve(&self) -> usize {
        self.chunk.stack_bound().max(crate::vm::STACK_GUARD_MARGIN)
    }

    pub fn new(name: Option<String>, arity: u8) -> Self {
        Function {
            name,
            arity,
            chunk: Chunk::new(),
            upvalue_count: 0,
            params: Vec::new(),
            locals: Vec::new(),
            local_decls: Vec::new(),
            has_loop: false,
            id: 0,
            origin: RefCell::new(None),
            hidden_globals: Vec::new(),
            method_of: None,
            typed_params: std::cell::OnceCell::new(),
        }
    }

    pub(crate) fn has_typed_params(&self) -> bool {
        *self
            .typed_params
            .get_or_init(|| self.params.iter().any(|p| p.type_ann.is_some()))
    }
}

/// Stamp `origin` onto a function and every function nested inside it.
///
/// Nested functions live in the constant pool as template closures, so the
/// walk has to recurse through constants rather than just the top level —
/// otherwise a helper defined inside an imported function still renders
/// against the wrong file.
pub fn set_module_origin(function: &Function, origin: &Rc<ModuleOrigin>) {
    if function.origin.borrow().is_some() {
        return; // already stamped — also stops cycles through shared constants
    }
    *function.origin.borrow_mut() = Some(Rc::clone(origin));
    for constant in &function.chunk.constants {
        if let ValueRepr::Closure(closure) = constant.repr() {
            set_module_origin(&closure.function, origin);
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

/// The globals of the module a closure or struct def was defined in — the
/// scope its free names resolve against. `None` for definitions in the main
/// script, which resolve against the VM's own globals.
///
/// Wired post-hoc: a module runs in a sub-VM, so its definitions are tagged
/// once it finishes loading (`VM::import_module`).
///
/// `ptr` mirrors `rc` so JIT-emitted code can read the dict with one
/// pointer-sized load at a pinned offset — it cannot walk a
/// `RefCell<Option<Rc<_>>>`, whose layout is not guaranteed. `#[repr(C)]`
/// keeps `ptr` at offset 0, and `set` is the only writer, so the two views
/// cannot drift apart.
#[repr(C)]
#[derive(Debug, Default)]
pub struct ModuleGlobals {
    ptr: Cell<*const HashMap<String, Value>>,
    rc: RefCell<Option<Rc<HashMap<String, Value>>>>,
}

impl ModuleGlobals {
    /// Defined in the main script — no owning module.
    pub fn none() -> Self {
        Self::default()
    }

    pub fn get(&self) -> Option<Rc<HashMap<String, Value>>> {
        self.rc.borrow().clone()
    }

    pub fn set(&self, globals: Option<Rc<HashMap<String, Value>>>) {
        self.ptr
            .set(globals.as_ref().map_or(std::ptr::null(), Rc::as_ptr));
        *self.rc.borrow_mut() = globals;
    }

    /// Raw view for the JIT, which stores it into a `JitFrame`. Null = none.
    pub fn as_ptr(&self) -> *const HashMap<String, Value> {
        self.ptr.get()
    }
}

impl From<Option<Rc<HashMap<String, Value>>>> for ModuleGlobals {
    fn from(globals: Option<Rc<HashMap<String, Value>>>) -> Self {
        let mg = Self::none();
        mg.set(globals);
        mg
    }
}

/// A closure: a compiled function + captured upvalues.
#[derive(Debug)]
pub struct ObjClosure {
    pub function: Rc<Function>,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
    /// The module this closure was defined in. Inherited at *creation* time
    /// by nested closures, never at call time — a callback handed to another
    /// module still resolves its names in the file it was written in.
    pub module_globals: ModuleGlobals,
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
    /// Consecutive JIT entry bailouts (guard failures). Reset to 0 by every
    /// successful JIT return; once it hits `JIT_BAILOUT_LIMIT` the closure
    /// deopts permanently (`jit_state = 2`). Without this, a single
    /// mistyped call (e.g. one Float where Int was specialized) would
    /// silently drop the closure to interpreter speed for the rest of the
    /// process.
    pub jit_bailouts: Cell<u8>,
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
    /// Phase 2.2: pointer to the lean integer entry
    /// (`fn(i64 args.., i64 budget) -> i64`), when the function qualified.
    /// Opaque — only the JIT boundary knows the real signature.
    pub lean_thunk: Cell<Option<*const ()>>,
    /// Arity the lean entry expects, so the boundary can read exactly that many
    /// integer arguments before committing to the call.
    pub lean_arity: Cell<u8>,
    /// B2.2: JIT-visible parallel cache for the `Closed(Integer)`
    /// upvalue shape. `upvalue_int_kinds[i] == 1` means upvalue `i` is
    /// currently `Upvalue::Closed(Value::Integer(_))` and the i64 is in
    /// `upvalue_int_values[i]`. Any other state (`Open`, `Closed(non-
    /// Integer)`, never observed) maps to `kinds[i] == 0`. The JIT's
    /// inline GetUpvalue path reads `kinds[i]` to gate against
    /// `jit_get_upvalue` helper dispatch. `Cell<u8>` and `Cell<i64>` are
    /// `#[repr(transparent)]` so `Box<[Cell<T>]>` matches `Box<[T]>`
    /// layout — the JIT loads through these at known struct offsets.
    /// Populated lazily by `jit_get_upvalue` once it observes the
    /// `Closed(Integer)` shape. `upvalue_int_kinds` has one entry per
    /// upvalue; `upvalue_int_values` has one more, at the front:
    /// `values[0]` is the `crate::jit::UPVALUE_WRITE_EPOCH` the cache was
    /// filled at and `values[i + 1]` caches upvalue `i`. A cache whose epoch
    /// is not the current one is empty — another closure (or the
    /// interpreter) may have written a shared cell since.
    pub upvalue_int_kinds: Box<[Cell<u8>]>,
    pub upvalue_int_values: Box<[Cell<i64>]>,
}

/// Construct the empty parallel kinds/values arrays for an `ObjClosure`
/// holding `n` upvalues. All slots start at `kind = 0` (fallback) so the
/// JIT inline GetUpvalue path defers to `jit_get_upvalue` until the
/// helper observes `Closed(Integer)` and populates the cache.
#[inline]
#[allow(clippy::type_complexity)]
pub fn make_upvalue_int_caches(n: usize) -> (Box<[Cell<u8>]>, Box<[Cell<i64>]>) {
    let kinds = (0..n)
        .map(|_| Cell::new(0u8))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    // values[0] is the fill epoch; -1 never matches, so a new cache starts empty.
    let values = std::iter::once(Cell::new(-1i64))
        .chain((0..n).map(|_| Cell::new(0i64)))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    (kinds, values)
}

impl ObjClosure {
    /// Build a closure whose upvalues are all `Closed` over `upvalue_values`,
    /// with cold JIT state and no module globals. Used to snapshot a closure
    /// for cross-thread transfer and to rebuild it on a worker.
    pub fn closed(function: Rc<Function>, upvalue_values: Vec<Value>) -> ObjClosure {
        let upvalues: Vec<Rc<RefCell<Upvalue>>> = upvalue_values
            .into_iter()
            .map(|v| Rc::new(RefCell::new(Upvalue::Closed(v))))
            .collect();
        let (upvalue_int_kinds, upvalue_int_values) = make_upvalue_int_caches(upvalues.len());
        ObjClosure {
            function,
            upvalues,
            module_globals: crate::vm::value::ModuleGlobals::none(),
            call_count: Cell::new(0),
            loop_count: Cell::new(0),
            jit_state: Cell::new(0),
            jit_bailouts: Cell::new(0),
            jit_thunk: Cell::new(None),
            specialized_thunk: Cell::new(None),
            specialized_arity: Cell::new(0),
            specialized_kind: Cell::new(0),
            lean_thunk: Cell::new(None),
            lean_arity: Cell::new(0),
            upvalue_int_kinds,
            upvalue_int_values,
        }
    }
}

/// u8 encoding of `engine::SpecializedEntryKind` stored in
/// `ObjClosure.specialized_kind`. Callers should treat these as
/// opaque tags and compare to the constants.
pub const SPECIALIZED_KIND_NONE: u8 = 0;
pub const SPECIALIZED_KIND_NATIVE_INT_BODY: u8 = 2;
/// B2.2: closure-aware specialized body. Differs from
/// `NATIVE_INT_BODY` only in the ABI: the thunk takes
/// `(*mut VM, *const ObjClosure, i64...)` so `GetUpvalue` can read
/// through the closure register instead of walking the JitFrame.
pub const SPECIALIZED_KIND_NATIVE_INT_BODY_WITH_CLOSURE: u8 = 3;

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
    /// HashMap<String, Value> of the module that defined this struct. Set after the
    /// defining module finishes loading (see `VM::import_module`); read
    /// in `call_method` so methods called from another module still see
    /// their own module's top-level functions (e.g. `Parser` in
    /// stdlib/parse_args.oxi calling its file-local `normalize_array`).
    /// `None` for structs defined in the main script.
    pub module_globals: ModuleGlobals,
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

impl ObjStructInstance {
    /// The fields, leaving the instance with none; its buffer goes with them.
    pub(crate) fn take_fields(&mut self) -> Vec<Value> {
        if self.fields.ptr.is_null() {
            return Vec::new();
        }
        // Reconstitute the Vec `new` leaked, so its allocator frees the buffer.
        let fields = unsafe {
            Vec::from_raw_parts(
                self.fields.ptr,
                self.fields.len as usize,
                self.fields.cap as usize,
            )
        };
        self.fields.ptr = std::ptr::null_mut();
        self.fields.len = 0;
        fields
    }
}

impl Drop for ObjStructInstance {
    fn drop(&mut self) {
        let fields = self.take_fields();
        // A long linked list of nodes would otherwise drop one frame per node.
        if fields.iter().any(Value::may_own_values) {
            drop_nested(fields);
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
    /// Declared type of each payload field, parallel to the names in `kind`
    /// (empty for unit variants). Checked when the variant is constructed.
    pub field_types: Vec<String>,
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

impl ObjEnumInstance {
    /// Move the payload's values into `out`, leaving a unit payload.
    pub(crate) fn take_payload_into(&mut self, out: &mut Vec<Value>) {
        match std::mem::replace(&mut self.payload, VmEnumPayload::Unit(None)) {
            VmEnumPayload::Unit(v) => out.extend(v),
            VmEnumPayload::Tuple(vs) => out.extend(vs),
            VmEnumPayload::Struct(fields) => out.extend(fields.into_iter().map(|(_, v)| v)),
        }
    }
}

impl Drop for ObjEnumInstance {
    fn drop(&mut self) {
        // An enum `Cons` chain would otherwise drop one frame per link.
        let mut payload = Vec::new();
        self.take_payload_into(&mut payload);
        if payload.iter().any(Value::may_own_values) {
            drop_nested(payload);
        }
    }
}

/// A loaded module.
#[derive(Debug)]
pub struct ObjModule {
    pub name: String,
    pub globals: Rc<HashMap<String, Value>>,
    /// Names the module declared with `hide`. They stay in `globals` so the
    /// module's own functions can still call them; what they are barred from
    /// is crossing the module boundary — `mod.name` and
    /// `introduce {name} from mod` both refuse.
    pub hidden: std::collections::HashSet<String>,
}

// ── Value methods ──────────────────────────────────────────────────────

impl Value {
    /// A new array holding `items`.
    pub fn array(items: Vec<Value>) -> Value {
        Value::Array(Rc::new(RefCell::new(items.into())))
    }

    /// A new tuple holding `items`.
    pub fn tuple(items: Vec<Value>) -> Value {
        Value::Tuple(Rc::new(items.into()))
    }

    /// Whether dropping this value can drop other values: the containers
    /// [`drop_values`] unpacks instead of recursing into.
    #[inline]
    pub(crate) fn may_own_values(&self) -> bool {
        matches!(
            self,
            Value::Array(_)
                | Value::Tuple(_)
                | Value::Map(_)
                | Value::Set(_)
                | Value::StructInstance(_)
                | Value::EnumInstance(_)
                | Value::Wrapped(_)
        )
    }

    // Mirror NanValue's accessors so call sites stop relying on the enum's pattern shape.

    #[inline]
    pub fn as_integer(&self) -> Option<i64> {
        if let Value::Integer(n) = self {
            Some(*n)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_float(&self) -> Option<f64> {
        if let Value::Float(f) = self {
            Some(*f)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_char(&self) -> Option<char> {
        if let Value::Char(c) = self {
            Some(*c)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_bool(&self) -> Option<bool> {
        if let Value::Boolean(b) = self {
            Some(*b)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_byte(&self) -> Option<u8> {
        if let Value::Byte(b) = self {
            Some(*b)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_uint(&self) -> Option<u64> {
        if let Value::Uint(n) = self {
            Some(*n)
        } else {
            None
        }
    }
    #[inline]
    pub fn is_none(&self) -> bool {
        matches!(self, Value::None)
    }
    #[inline]
    pub fn as_string(&self) -> Option<&Rc<String>> {
        if let Value::String(s) = self {
            Some(s)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_array(&self) -> Option<&Rc<RefCell<ValueVec>>> {
        if let Value::Array(a) = self {
            Some(a)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_tuple(&self) -> Option<&Rc<ValueVec>> {
        if let Value::Tuple(t) = self {
            Some(t)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_map(&self) -> Option<&Rc<RefCell<crate::vm::collections::OxMap>>> {
        if let Value::Map(m) = self {
            Some(m)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_set(&self) -> Option<&Rc<RefCell<crate::vm::collections::OxSet>>> {
        if let Value::Set(s) = self {
            Some(s)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_closure(&self) -> Option<&Rc<ObjClosure>> {
        if let Value::Closure(c) = self {
            Some(c)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_builtin(&self) -> Option<BuiltinFn> {
        if let Value::Builtin(f) = self {
            Some(*f)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_struct_def(&self) -> Option<&Rc<ObjStructDef>> {
        if let Value::StructDef(d) = self {
            Some(d)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_struct_instance(&self) -> Option<&Rc<ObjStructInstance>> {
        if let Value::StructInstance(i) = self {
            Some(i)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_enum_def(&self) -> Option<&Rc<ObjEnumDef>> {
        if let Value::EnumDef(d) = self {
            Some(d)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_enum_instance(&self) -> Option<&Rc<ObjEnumInstance>> {
        if let Value::EnumInstance(i) = self {
            Some(i)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_module(&self) -> Option<&Rc<ObjModule>> {
        if let Value::Module(m) = self {
            Some(m)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_error_value(&self) -> Option<&Rc<ErrorValueData>> {
        if let Value::ErrorValue(d) = self {
            Some(d)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_wrapped(&self) -> Option<&Rc<Value>> {
        if let Value::Wrapped(v) = self {
            Some(v)
        } else {
            None
        }
    }
    #[inline]
    pub fn as_error_string(&self) -> Option<&Rc<String>> {
        if let Value::Error(s) = self {
            Some(s)
        } else {
            None
        }
    }

    /// Borrow as a [`ValueRepr`] — the migration-friendly view enum.
    /// See `ValueRepr` doc-comment for rationale (A1.2.5 flag-day prep).
    #[inline]
    pub fn repr(&self) -> ValueRepr<'_> {
        match self {
            Value::Integer(n) => ValueRepr::Integer(*n),
            Value::Float(f) => ValueRepr::Float(*f),
            Value::Char(c) => ValueRepr::Char(*c),
            Value::Boolean(b) => ValueRepr::Boolean(*b),
            Value::Byte(b) => ValueRepr::Byte(*b),
            Value::Uint(n) => ValueRepr::Uint(*n),
            Value::None => ValueRepr::None,
            Value::String(s) => ValueRepr::String(s),
            Value::Array(a) => ValueRepr::Array(a),
            Value::Tuple(t) => ValueRepr::Tuple(t),
            Value::Map(m) => ValueRepr::Map(m),
            Value::Set(s) => ValueRepr::Set(s),
            Value::Closure(c) => ValueRepr::Closure(c),
            Value::Builtin(f) => ValueRepr::Builtin(*f),
            Value::StructDef(d) => ValueRepr::StructDef(d),
            Value::StructInstance(i) => ValueRepr::StructInstance(i),
            Value::EnumDef(d) => ValueRepr::EnumDef(d),
            Value::EnumInstance(i) => ValueRepr::EnumInstance(i),
            Value::Module(m) => ValueRepr::Module(m),
            Value::ErrorValue(d) => ValueRepr::ErrorValue(d),
            Value::Wrapped(v) => ValueRepr::Wrapped(v),
            Value::Error(s) => ValueRepr::Error(s),
            Value::Native(f) => ValueRepr::Native(f),
            Value::Host(h) => ValueRepr::Host(h),
            Value::Vector(v) => ValueRepr::Vector(v),
            Value::Task(h) => ValueRepr::Task(h),
        }
    }

    /// Produce a value that is independent from `self` for the mutable
    /// heap-collection variants (Array, Map, Set), recursively deep-copying
    /// their contents so the result shares no `Rc<RefCell<…>>` backing store
    /// with the original. All other variants fall back to an ordinary
    /// `clone()` (a cheap `Rc` refcount bump or a `Copy`), so this is only
    /// more expensive than `clone()` when a fresh, unaliased collection is
    /// actually required.
    ///
    /// V5 fix: a typed zero-init heap default — `arr <array>` with no
    /// initializer — is produced once by the compiler and parked in the
    /// function's constant pool. A plain `clone()` of that constant on every
    /// call shares the SAME `Rc<RefCell<ValueVec>>`, so mutations from one
    /// call leak into the next (`b()` called 3× printed `1,2,3` instead of
    /// `1,1,1`). Materializing the default through `fresh_clone` gives each
    /// initialization its own empty collection, matching the tree-walker.
    pub fn fresh_clone(&self) -> Value {
        match self {
            Value::Array(arr) => {
                let fresh: Vec<Value> = arr.borrow().iter().map(|v| v.fresh_clone()).collect();
                Value::Array(Rc::new(RefCell::new(fresh.into())))
            }
            Value::Map(map) => {
                let mut fresh = crate::vm::collections::OxMap::new();
                for (k, v) in map.borrow().iter() {
                    fresh.insert(k.fresh_clone(), v.fresh_clone());
                }
                Value::Map(Rc::new(RefCell::new(fresh)))
            }
            Value::Set(set) => {
                let mut fresh = crate::vm::collections::OxSet::new();
                for v in set.borrow().iter() {
                    fresh.insert(v.fresh_clone());
                }
                Value::Set(Rc::new(RefCell::new(fresh)))
            }
            // Tuples are immutable, so sharing the Rc is observationally identical to a deep copy.
            other => other.clone(),
        }
    }

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
            Value::Task(_) => "TASK",
            Value::Native(_) => "BUILTIN",
            Value::Host(_) => "HOST",
            Value::Vector(v) => v.type_name(),
        }
    }

    /// `type_matches(expected, &self.effective_type_name())`, without
    /// building the name. Type locks and typed parameters run this on every
    /// store and call.
    pub fn effective_type_is(&self, expected: &str) -> bool {
        use crate::vm::type_matches;
        match self {
            Value::StructInstance(inst) => type_matches(expected, &inst.struct_name),
            Value::ErrorValue(data) => match &data.tag {
                Some(_) => type_matches(expected, &self.effective_type_name()),
                None => type_matches(expected, "ERROR"),
            },
            Value::Wrapped(_) => type_matches(expected, "VALUE"),
            Value::EnumDef(d) => type_matches(expected, &d.name),
            Value::EnumInstance(i) => type_matches(expected, &i.enum_name),
            Value::Host(h) => type_matches(expected, &h.type_name),
            other => type_matches(expected, other.type_name()),
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
            Value::Host(h) => h.type_name.clone(),
            other => other.type_name().to_string(),
        }
    }
}

// ── Display ────────────────────────────────────────────────────────────

/// How deep `Display` nests before eliding the rest as `...`.
const MAX_DISPLAY_DEPTH: usize = 256;
/// How deep `==` compares before giving up (unequal).
const MAX_EQ_DEPTH: usize = 1024;

thread_local! {
    /// Containers being displayed right now, outermost first. A container
    /// already on it is a cycle; its length is the nesting depth.
    static DISPLAYING: std::cell::RefCell<Vec<usize>> = const { std::cell::RefCell::new(Vec::new()) };
    /// Container pairs being compared right now, for `==`'s cycle check.
    static COMPARING: std::cell::RefCell<Vec<(usize, usize)>> = const { std::cell::RefCell::new(Vec::new()) };
}

/// Display one container. A cyclic value (or one nested past
/// `MAX_DISPLAY_DEPTH`) prints `open...close` instead of recursing until the
/// native stack overflows.
fn nested(
    f: &mut fmt::Formatter<'_>,
    ptr: usize,
    open: &str,
    close: &str,
    body: impl FnOnce(&mut fmt::Formatter<'_>) -> fmt::Result,
) -> fmt::Result {
    let elide = DISPLAYING.with(|d| {
        let d = d.borrow();
        d.len() >= MAX_DISPLAY_DEPTH || d.contains(&ptr)
    });
    if elide {
        return write!(f, "{open}...{close}");
    }
    DISPLAYING.with(|d| d.borrow_mut().push(ptr));
    let result = body(f);
    DISPLAYING.with(|d| d.borrow_mut().pop());
    result
}

/// Compare two containers. The same pair met again while it is still being
/// compared is a cycle, and assumed equal (the rest of the comparison decides);
/// past `MAX_EQ_DEPTH` the values count as unequal rather than overflow the stack.
fn compare_nested(a: usize, b: usize, body: impl FnOnce() -> bool) -> bool {
    if a == b {
        return true;
    }
    let state = COMPARING.with(|c| {
        let c = c.borrow();
        if c.contains(&(a, b)) {
            Some(true)
        } else if c.len() >= MAX_EQ_DEPTH {
            Some(false)
        } else {
            None
        }
    });
    if let Some(answer) = state {
        return answer;
    }
    COMPARING.with(|c| c.borrow_mut().push((a, b)));
    let result = body();
    COMPARING.with(|c| c.borrow_mut().pop());
    result
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::Char(c) => write!(f, "`{}`", c),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", if *b { "True" } else { "False" }),
            Value::Array(elements) => nested(f, Rc::as_ptr(elements) as usize, "[", "]", |f| {
                let items: Vec<String> = elements.borrow().iter().map(|e| e.to_string()).collect();
                write!(f, "[{}]", items.join(", "))
            }),
            Value::Closure(c) => {
                let name = c.function.name.as_deref().unwrap_or("anonymous");
                write!(f, "fun {}()", name)
            }
            Value::StructDef(sd) => write!(f, "struct {}", sd.name),
            Value::StructInstance(si) => {
                let open = format!("{} {{ ", si.struct_name);
                nested(f, Rc::as_ptr(si) as usize, &open, " }", |f| {
                    let fields = si.field_slice();
                    let items: Vec<String> = si
                        .layout
                        .slots
                        .iter()
                        .enumerate()
                        .map(|(i, slot)| format!("{}: {}", slot.0, fields[i]))
                        .collect();
                    write!(f, "{} {{ {} }}", si.struct_name, items.join(", "))
                })
            }
            Value::Byte(n) => write!(f, "{}", n),
            Value::Uint(n) => write!(f, "{}", n),
            Value::Tuple(elements) => nested(f, Rc::as_ptr(elements) as usize, "(", ")", |f| {
                let items: Vec<String> = elements.iter().map(|e| e.to_string()).collect();
                if elements.len() == 1 {
                    write!(f, "({},)", items[0])
                } else {
                    write!(f, "({})", items.join(", "))
                }
            }),
            Value::Map(entries) => nested(f, Rc::as_ptr(entries) as usize, "{", "}", |f| {
                let entries = entries.borrow();
                let items: Vec<String> = entries
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect();
                write!(f, "{{{}}}", items.join(", "))
            }),
            Value::Set(elements) => nested(f, Rc::as_ptr(elements) as usize, "set(", ")", |f| {
                let items: Vec<String> = elements.borrow().iter().map(|e| e.to_string()).collect();
                write!(f, "set({})", items.join(", "))
            }),
            Value::Module(m) => write!(f, "module <{}>", m.name),
            Value::Builtin(_) => write!(f, "builtin function"),
            Value::None => write!(f, "None"),
            Value::ErrorValue(data) => match &data.tag {
                Some(tag) => write!(f, "Error {{ tag: {}, msg: {} }}", tag, data.msg),
                None => write!(f, "Error {{ msg: {} }}", data.msg),
            },
            Value::Wrapped(val) => nested(f, Rc::as_ptr(val) as usize, "Value(", ")", |f| {
                write!(f, "Value({})", val)
            }),
            Value::Error(msg) => write!(f, "Error: {}", msg),
            Value::EnumDef(d) => write!(f, "enum {}", d.name),
            Value::EnumInstance(i) => {
                nested(f, Rc::as_ptr(i) as usize, "", "", |f| match &i.payload {
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
                        write!(
                            f,
                            "{}.{} {{ {} }}",
                            i.enum_name,
                            i.variant_name,
                            s.join(", ")
                        )
                    }
                })
            }
            Value::Task(_) => write!(f, "<task>"),
            Value::Native(n) => write!(f, "<builtin {}>", n.name),
            Value::Host(h) => write!(f, "<{}>", h.type_name),
            Value::Vector(v) => write!(f, "{}", v),
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
            Value::Native(n) => write!(f, "Native({})", n.name),
            Value::Host(h) => write!(f, "Host({})", h.type_name),
            Value::Vector(v) => write!(f, "Vector({})", v),
            Value::EnumDef(d) => write!(f, "EnumDef({})", d.name),
            Value::EnumInstance(i) => {
                write!(
                    f,
                    "EnumInstance({}, {}, {:?})",
                    i.enum_name, i.variant_name, i.payload
                )
            }
            Value::Task(_) => write!(f, "Task"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            // Interned constant-pool strings make the common comparison a single pointer check.
            (Value::String(a), Value::String(b)) => Rc::ptr_eq(a, b) || a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::None, Value::None) => true,
            (Value::Byte(a), Value::Byte(b)) => a == b,
            (Value::Uint(a), Value::Uint(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => {
                compare_nested(Rc::as_ptr(a) as usize, Rc::as_ptr(b) as usize, || {
                    *a.borrow() == *b.borrow()
                })
            }
            (Value::Tuple(a), Value::Tuple(b)) => {
                compare_nested(Rc::as_ptr(a) as usize, Rc::as_ptr(b) as usize, || a == b)
            }
            (Value::Map(a), Value::Map(b)) => {
                compare_nested(Rc::as_ptr(a) as usize, Rc::as_ptr(b) as usize, || {
                    *a.borrow() == *b.borrow()
                })
            }
            (Value::Set(a), Value::Set(b)) => {
                compare_nested(Rc::as_ptr(a) as usize, Rc::as_ptr(b) as usize, || {
                    *a.borrow() == *b.borrow()
                })
            }
            (Value::ErrorValue(a), Value::ErrorValue(b)) => a.msg == b.msg && a.tag == b.tag,
            (Value::Wrapped(a), Value::Wrapped(b)) => a == b,
            (Value::StructInstance(a), Value::StructInstance(b)) => {
                a.struct_name == b.struct_name
                    && compare_nested(Rc::as_ptr(a) as usize, Rc::as_ptr(b) as usize, || {
                        a.field_slice() == b.field_slice()
                    })
            }
            (Value::Module(a), Value::Module(b)) => a.name == b.name,
            (Value::Native(a), Value::Native(b)) => Rc::ptr_eq(a, b),
            (Value::Host(a), Value::Host(b)) => Rc::ptr_eq(a, b) || a.same_value(b),
            (Value::Vector(a), Value::Vector(b)) => a == b,
            (Value::EnumDef(a), Value::EnumDef(b)) => a.name == b.name,
            (Value::EnumInstance(a), Value::EnumInstance(b)) => {
                a.enum_name == b.enum_name
                    && a.variant_name == b.variant_name
                    && compare_nested(Rc::as_ptr(a) as usize, Rc::as_ptr(b) as usize, || {
                        payload_eq(&a.payload, &b.payload)
                    })
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
