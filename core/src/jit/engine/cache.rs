//! Inline-cache (IC) entry types — one struct per call-site / lookup-site
//! cache, all laid out `#[repr(C)]` so the JIT can bake byte offsets
//! into emitted IR.
//!
//! Pulled out of the original monolithic `engine.rs` so the IC layouts
//! live next to one another without being interleaved with the
//! orchestration code. The offset constants are pinned by the JIT-side
//! load/store sites; changing field order requires updating the
//! `OFFSET_*` constants below in lockstep.

#![cfg(feature = "jit")]

use crate::vm::value::{ObjClosure, ObjStructDef, Value};

/// Per-call-site GetGlobal cache. `version` tracks the VM's
/// `globals_version` at the time the entry was populated; a mismatch
/// means the globals map has been mutated and the cached `value` is
/// stale.
#[repr(C)]
pub(crate) struct GlobalCacheEntry {
    pub version: u64,
    pub value: Value,
}

/// Per-call-site Call cache — readable from Cranelift IR.
///
/// Layout (pinned by `#[repr(C)]`):
/// - offset 0: `closure_raw` — raw `*const ObjClosure` for identity
///   compare. `null` means the cache is empty.
/// - offset 8: `thunk` — raw function pointer for the cached thunk.
///   Valid iff `closure_raw != null`.
/// - offset 16: `arity` — the callee's arity; used to compute the
///   stack slot_offset without reading the closure.
/// - after that: `_keeper` keeps the `Rc` alive so the raw pointer
///   stays valid (Rc would be freed without this). Rust-only; never
///   read from IR.
#[repr(C)]
pub(crate) struct CallCacheEntry {
    pub closure_raw: *const ObjClosure,
    pub thunk_raw: *const (),
    pub arity: u8,
    /// B2.2.f: u8 cache of `closure.specialized_kind` at the time the
    /// IC was populated. Reading this from the constant cache_ptr is
    /// faster (and avoids a Cranelift folding issue we hit reading
    /// `closure.specialized_kind` directly via offset_of!) than going
    /// through the closure object on every call. Set to 0 when the
    /// closure has no specialized entry. Stable for the cache's
    /// lifetime — the closure can't change `specialized_kind` after
    /// install.
    pub specialized_kind: u8,
    /// ABI alignment padding — never read; `pub(crate)` so the struct
    /// can be initialized from outside this module.
    pub(crate) _pad: [u8; 6],
    /// B2.2.f: cache of `closure.specialized_thunk` (or null when
    /// none). Same rationale as `specialized_kind`.
    pub specialized_thunk: *const (),
    pub _keeper: Option<std::rc::Rc<ObjClosure>>,
}

impl CallCacheEntry {
    pub(crate) const OFFSET_CLOSURE_RAW: i32 = 0;
    /// Reserved for a future pass that loads the thunk directly in IR
    /// and emits an indirect call, skipping the `jit_op_call_hit`
    /// helper's FFI crossing entirely. Not yet wired.
    #[allow(dead_code)]
    pub(crate) const OFFSET_THUNK_RAW: i32 = 8;
    pub(crate) const OFFSET_SPECIALIZED_KIND: i32 = 17;
    pub(crate) const OFFSET_SPECIALIZED_THUNK: i32 = 24;
}

/// Kind tag for inline expansion of a struct-method body at its caller.
/// Populated by `jit_op_method_call_ic` when the resolved callee's
/// bytecode matches a whitelisted peephole shape. On subsequent hits
/// the MethodCall IR reads `inline_kind` and emits the peephole
/// operation directly instead of pushing a `JitFrame` and calling the
/// thunk.
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
/// compared, held alive by the `Rc`) and the resolved method closure
/// so a hit skips `find_struct_method`'s two HashMap lookups entirely.
///
/// Also carries optional inline-expansion info (`inline_kind` and
/// friends). When set, the JIT's MethodCall hit path bypasses the
/// JitFrame push + thunk dispatch entirely and emits the peephole
/// operation (currently just the struct-field-add shape) inline at the
/// caller. See `MethodInlineKind` above.
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
