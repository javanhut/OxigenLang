//! Inline-cache (IC) entry types — one struct per call-site / lookup-site
//! cache, all laid out `#[repr(C)]` so the JIT can bake byte offsets
//! into emitted IR. The `OFFSET_*` constants give the pinned field offsets.

#![cfg(feature = "jit")]

use crate::vm::value::{ObjClosure, ObjStructDef, Value};

/// Per-call-site GetGlobal cache. `version` tracks the per-name version
/// cell (owned by `JitInner::global_version_cells`) at the time the entry
/// was populated; a mismatch means this global has been reassigned and the
/// cached `value` is stale.
///
/// `version_cell` starts pointing at `GLOBAL_VERSION_SENTINEL` and `version`
/// starts at `u64::MAX`, so the first probe always misses; the miss path
/// repoints it at the real per-name cell.
#[repr(C)]
pub(crate) struct GlobalCacheEntry {
    pub version: u64,
    pub value: Value,
    pub version_cell: *const u64,
}

impl GlobalCacheEntry {
    /// Byte offset of `version_cell`, loaded from emitted IR. (`version`
    /// is at 0 and `value` at 8.)
    pub(crate) const OFFSET_VERSION_CELL: i32 =
        std::mem::offset_of!(GlobalCacheEntry, version_cell) as i32;
}

/// See `GlobalCacheEntry::version_cell`.
pub(crate) static GLOBAL_VERSION_SENTINEL: u64 = 0;

/// Per-call-site Call cache — readable from Cranelift IR.
///
/// Layout (pinned by `#[repr(C)]`):
/// - offset 0: `closure_raw` — raw `*const ObjClosure` for identity
///   compare. `null` means the cache is empty.
/// - offset 8: `thunk` — raw function pointer for the cached thunk.
///   Valid iff `closure_raw != null`.
/// - offset 16: `arity` — the callee's arity.
/// - after that: `_keeper` keeps the `Rc` alive so the raw pointer
///   stays valid. Rust-only; never read from IR.
#[repr(C)]
pub(crate) struct CallCacheEntry {
    pub closure_raw: *const ObjClosure,
    pub thunk_raw: *const (),
    pub arity: u8,
    /// u8 cache of `closure.specialized_kind` at the time the
    /// IC was populated. Set to 0 when the closure has no specialized entry.
    pub specialized_kind: u8,
    /// Inline expansion of the callee's body at this call site.
    /// Populated on a normal IC miss once the real callee is known. When set,
    /// the Call IR evaluates the body directly without pushing a `JitFrame`.
    /// See `ClosureInlineKind`.
    pub inline_kind: ClosureInlineKind,
    /// ABI alignment padding — never read; `pub(crate)` so the struct
    /// can be initialized from outside this module.
    pub(crate) _pad: [u8; 1],
    /// Which upvalue slot `ClosureInlineKind::AddUpvalueArg` reads. Meaningful
    /// only when `inline_kind` is non-`None`.
    pub inline_upvalue_index: u32,
    /// Cache of `closure.specialized_thunk` (or null when none).
    pub specialized_thunk: *const (),
    pub _keeper: Option<std::rc::Rc<ObjClosure>>,
}

impl CallCacheEntry {
    pub(crate) const OFFSET_CLOSURE_RAW: i32 = 0;
    /// Reserved; not yet used.
    #[allow(dead_code)]
    pub(crate) const OFFSET_THUNK_RAW: i32 = 8;
    pub(crate) const OFFSET_SPECIALIZED_KIND: i32 = 17;
    pub(crate) const OFFSET_INLINE_KIND: i32 = 18;
    pub(crate) const OFFSET_INLINE_UPVALUE_INDEX: i32 = 20;
    pub(crate) const OFFSET_SPECIALIZED_THUNK: i32 = 24;
}

/// Kind tag for inline expansion of a *closure* body at its call site — the
/// `Call` analogue of `MethodInlineKind`.
///
/// `#[repr(u8)]` so the JIT reads a single byte at `OFFSET_INLINE_KIND`.
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum ClosureInlineKind {
    /// No inline expansion known; take the normal dispatch path.
    None = 0,
    /// Body is exactly `GetUpvalue(k); GetLocal(1); Add; Return` with arity 1 —
    /// i.e. `fun(y) { x + y }` over an immutable captured `x`. Evaluates to a
    /// single wrapping integer add at the caller.
    AddUpvalueArg = 1,
}

/// Kind tag for inline expansion of a struct-method body at its caller.
/// Populated by `jit_op_method_call_ic` when the resolved callee's
/// bytecode matches a whitelisted peephole shape. On subsequent hits
/// the MethodCall IR reads `inline_kind` and emits the peephole
/// operation directly.
///
/// `#[repr(u8)]` so the JIT can read a single byte at the pinned
/// offset.
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum MethodInlineKind {
    /// No inline expansion known; take the normal JitFrame + thunk
    /// path.
    None = 0,
    /// Body is `self.f = self.f + <const addend>; return` — arity
    /// must be 0.
    FieldAddConst = 1,
    /// Body is `self.f = self.f + <arg>; return` — arity must be 1.
    FieldAddLocal = 2,
}

/// Per-MethodCall-site cache. Stores the struct def pointer (identity-
/// compared, held alive by the `Rc`) and the resolved method closure.
///
/// Also carries optional inline-expansion info (`inline_kind` and
/// friends). When set, the JIT's MethodCall hit path emits the peephole
/// operation inline at the caller. See `MethodInlineKind` above.
#[repr(C)]
pub(crate) struct MethodCacheEntry {
    pub struct_def_raw: *const ObjStructDef,
    pub closure_raw: *const ObjClosure,
    pub thunk_raw: *const (),
    pub arity: u8,
    pub inline_kind: MethodInlineKind,
    pub(crate) _pad: [u8; 6],
    pub inline_field_index: u32,
    pub(crate) _pad2: [u8; 4],
    pub inline_addend: i64,
    pub struct_def: Option<std::rc::Rc<ObjStructDef>>,
    pub closure: Option<std::rc::Rc<ObjClosure>>,
}

impl MethodCacheEntry {
    pub(crate) const OFFSET_STRUCT_DEF_RAW: i32 = 0;
    pub(crate) const OFFSET_CLOSURE_RAW: i32 = 8;
    pub(crate) const OFFSET_THUNK_RAW: i32 = 16;
    pub(crate) const OFFSET_INLINE_KIND: i32 = 25;
    pub(crate) const OFFSET_INLINE_FIELD_INDEX: i32 = 32;
    pub(crate) const OFFSET_INLINE_ADDEND: i32 = 40;
}

#[repr(u8)]
#[derive(Clone, Copy)]
pub(crate) enum FieldCacheKind {
    Invalid = 0,
    InstanceField = 1,
    DefMethod = 2,
}

#[repr(C)]
pub(crate) struct FieldCacheEntry {
    pub struct_def_raw: *const ObjStructDef,
    pub field_index: u32,
    pub kind: FieldCacheKind,
    pub(crate) _pad: [u8; 3],
    pub closure_raw: *const ObjClosure,
    pub thunk_raw: *const (),
    pub struct_def: Option<std::rc::Rc<ObjStructDef>>,
    pub closure: Option<std::rc::Rc<ObjClosure>>,
}

#[allow(dead_code)]
impl FieldCacheEntry {
    pub(crate) const OFFSET_STRUCT_DEF_RAW: i32 = 0;
    pub(crate) const OFFSET_FIELD_INDEX: i32 = 8;
    pub(crate) const OFFSET_KIND: i32 = 12;
    pub(crate) const OFFSET_CLOSURE_RAW: i32 = 16;
}
