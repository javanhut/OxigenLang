//! Cranelift-backed JIT engine internals. Only compiled when the `jit`
//! feature is enabled.
//!
//! Milestone 1, Steps 3+4: real bytecode translation for control-flow,
//! locals, and arithmetic. Every opcode becomes exactly one `call` to an
//! `extern "C"` runtime helper, plus any branching. Call/Return-chain
//! mechanics are still handled by the runtime helpers; no inline fast
//! paths yet.

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use cranelift_codegen::Context;
use cranelift_codegen::ir::{
    AbiParam, Block, FuncRef, InstBuilder, Signature, UserFuncName, types,
};
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module, default_libcall_names};

use crate::compiler::opcode::{Chunk, OpCode};
use crate::vm::VM;
use crate::vm::VMError;
use crate::vm::{JitFrame, JitFrameView, StackView};
use crate::vm::value::{
    Function, RC_VALUE_OFFSET, STRUCT_INSTANCE_DEF_OFFSET, STRUCT_INSTANCE_FIELDS_PTR_OFFSET,
    VALUE_INT_PAYLOAD_OFFSET, VALUE_SIZE, VALUE_TAG_CLOSURE, VALUE_TAG_FLOAT, VALUE_TAG_INTEGER,
    VALUE_TAG_NONE, VALUE_TAG_STRUCT_INSTANCE,
};

/// Byte offset of the `stack_view.ptr` field from the base of `VM`.
/// Computed once and baked into emitted IR as a constant.
#[inline(always)]
fn vm_stack_view_ptr_offset() -> i32 {
    (VM::stack_view_offset() + StackView::OFFSET_PTR as usize) as i32
}

/// Byte offset of the `stack_view.len` field from the base of `VM`.
#[inline(always)]
fn vm_stack_view_len_offset() -> i32 {
    (VM::stack_view_offset() + StackView::OFFSET_LEN as usize) as i32
}

#[allow(dead_code)]
#[inline(always)]
fn vm_jit_frame_view_ptr_offset() -> i32 {
    (VM::jit_frame_view_offset() + JitFrameView::OFFSET_PTR as usize) as i32
}

#[allow(dead_code)]
#[inline(always)]
fn vm_jit_frame_view_len_offset() -> i32 {
    (VM::jit_frame_view_offset() + JitFrameView::OFFSET_LEN as usize) as i32
}

use super::{CompiledEntries, CompiledThunk, SpecializedThunkRaw};
use super::runtime;
use super::scan;

/// Lightweight JIT instrumentation counters. Each field is a `Cell<u64>`
/// so increments compile to plain load + inc + store in the JIT IR. On
/// process exit, `JitInner::drop` dumps the counters to stderr when the
/// env var `OXIGEN_JIT_STATS` is set. Off by default — zero text output.
///
/// Added in stage A0 ahead of the Plan A calling-convention work so a
/// weak benchmark result is debuggable ("did the specialized path never
/// get generated? never get hit? get hit but bail?").
#[repr(C)]
pub(crate) struct JitCounters {
    pub specialized_entry_compiled: std::cell::Cell<u64>,
    pub specialized_entry_called: std::cell::Cell<u64>,
    pub specialized_call_ic_hit: std::cell::Cell<u64>,
    pub specialized_call_ic_miss: std::cell::Cell<u64>,
    pub specialized_call_no_entry: std::cell::Cell<u64>,
    pub specialized_call_arity_mismatch: std::cell::Cell<u64>,
    pub specialized_call_bailout: std::cell::Cell<u64>,
    pub self_recursion_direct_call: std::cell::Cell<u64>,
    pub virt_divmod_fast: std::cell::Cell<u64>,
    pub virt_divmod_slow_zero: std::cell::Cell<u64>,
    pub virt_divmod_slow_overflow: std::cell::Cell<u64>,
}

impl JitCounters {
    fn new() -> Self {
        Self {
            specialized_entry_compiled: std::cell::Cell::new(0),
            specialized_entry_called: std::cell::Cell::new(0),
            specialized_call_ic_hit: std::cell::Cell::new(0),
            specialized_call_ic_miss: std::cell::Cell::new(0),
            specialized_call_no_entry: std::cell::Cell::new(0),
            specialized_call_arity_mismatch: std::cell::Cell::new(0),
            specialized_call_bailout: std::cell::Cell::new(0),
            self_recursion_direct_call: std::cell::Cell::new(0),
            virt_divmod_fast: std::cell::Cell::new(0),
            virt_divmod_slow_zero: std::cell::Cell::new(0),
            virt_divmod_slow_overflow: std::cell::Cell::new(0),
        }
    }

    fn dump(&self) {
        eprintln!("[jit stats]");
        eprintln!("  specialized_entry_compiled:        {}", self.specialized_entry_compiled.get());
        eprintln!("  specialized_entry_called:          {}", self.specialized_entry_called.get());
        eprintln!("  specialized_call_ic_hit:           {}", self.specialized_call_ic_hit.get());
        eprintln!("  specialized_call_ic_miss:          {}", self.specialized_call_ic_miss.get());
        eprintln!("  specialized_call_no_entry:         {}", self.specialized_call_no_entry.get());
        eprintln!("  specialized_call_arity_mismatch:   {}", self.specialized_call_arity_mismatch.get());
        eprintln!("  specialized_call_bailout:          {}", self.specialized_call_bailout.get());
        eprintln!("  self_recursion_direct_call:        {}", self.self_recursion_direct_call.get());
        eprintln!("  virt_divmod_fast:                  {}", self.virt_divmod_fast.get());
        eprintln!("  virt_divmod_slow_zero:             {}", self.virt_divmod_slow_zero.get());
        eprintln!("  virt_divmod_slow_overflow:         {}", self.virt_divmod_slow_overflow.get());
    }
}

pub(super) struct JitInner {
    module: JITModule,
    ctx: Context,
    fbc: FunctionBuilderContext,
    compiled: HashMap<*const Function, Entry>,
    /// Keeps `Rc<Function>`s alive so the pointer keys above remain valid.
    retained: HashMap<*const Function, Rc<Function>>,

    /// Cached helper `FuncId`s — registered once at module init.
    helpers: HelperIds,

    /// Pre-call frame depth, stashed by `invoke` before jumping to compiled
    /// code and read by runtime helpers through the VM pointer.
    current_stop_depth: usize,
    pending_error: Option<VMError>,

    /// Monotonic counter for unique thunk names.
    next_id: u32,

    /// Plan A0 instrumentation counters. Lives in a stable heap allocation
    /// (Box) so emitted IR can bake the raw pointer. Raw pointer access is
    /// sound because JitInner is single-owner and the counters box is only
    /// mutated through single-threaded JIT execution.
    pub(crate) counters: Box<JitCounters>,

    /// Per-call-site inline caches for `GetGlobal`. Each compiled
    /// GetGlobal gets its own `Box<GlobalCacheEntry>`; the raw pointer
    /// is baked into emitted IR. Boxes stay put even as the `Vec`
    /// reallocates, so those baked pointers remain valid for the JIT's
    /// lifetime.
    global_caches: Vec<Box<GlobalCacheEntry>>,

    /// Per-call-site inline caches for `Call`. Same pattern as
    /// `global_caches`: each compiled Call op gets its own
    /// `Box<CallCacheEntry>`, and the raw pointer is baked into IR.
    call_caches: Vec<Box<CallCacheEntry>>,

    /// Per-call-site inline caches for `MethodCall`. Each compiled
    /// MethodCall op gets its own `Box<MethodCacheEntry>`.
    method_caches: Vec<Box<MethodCacheEntry>>,

    /// Per-field-op inline caches for `GetField`/`SetField`.
    field_caches: Vec<Box<FieldCacheEntry>>,
}

/// Per-call-site GetGlobal cache. `version` tracks the VM's
/// `globals_version` at the time the entry was populated; a mismatch
/// means the globals map has been mutated and the cached `value` is
/// stale.
#[repr(C)]
pub(crate) struct GlobalCacheEntry {
    pub version: u64,
    pub value: crate::vm::value::Value,
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
    pub closure_raw: *const crate::vm::value::ObjClosure,
    pub thunk_raw: *const (),
    pub arity: u8,
    _pad: [u8; 7],
    pub _keeper: Option<std::rc::Rc<crate::vm::value::ObjClosure>>,
}

impl CallCacheEntry {
    pub(crate) const OFFSET_CLOSURE_RAW: i32 = 0;
    /// Reserved for a future pass that loads the thunk directly in IR
    /// and emits an indirect call, skipping the `jit_op_call_hit`
    /// helper's FFI crossing entirely. Not yet wired.
    #[allow(dead_code)]
    pub(crate) const OFFSET_THUNK_RAW: i32 = 8;
}

/// Kind tag for inline expansion of a struct-method body at its caller.
/// Populated by `jit_op_method_call_ic` when the resolved callee's
/// bytecode matches a whitelisted peephole shape. On subsequent hits the
/// MethodCall IR reads `inline_kind` and emits the peephole operation
/// directly instead of pushing a `JitFrame` and calling the thunk.
///
/// `#[repr(u8)]` so the JIT can read a single byte at the pinned offset.
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum MethodInlineKind {
    /// No inline expansion known; take the normal JitFrame + thunk path.
    None = 0,
    /// Body is `self.f = self.f + <const addend>; return` — arity must be 0.
    FieldAddConst = 1,
    /// Body is `self.f = self.f + <arg>; return` — arity must be 1.
    FieldAddLocal = 2,
}

/// Per-MethodCall-site cache. Stores the struct def pointer (identity-
/// compared, held alive by the `Rc`) and the resolved method closure so
/// that a hit skips `find_struct_method`'s two HashMap lookups entirely.
///
/// Also carries optional inline-expansion info (`inline_kind` and
/// friends). When set, the JIT's MethodCall hit path bypasses the
/// JitFrame push + thunk dispatch entirely and emits the peephole
/// operation (currently just the struct-field-add shape) inline at the
/// caller. See `MethodInlineKind` above.
#[repr(C)]
pub(crate) struct MethodCacheEntry {
    pub struct_def_raw: *const crate::vm::value::ObjStructDef,
    pub closure_raw: *const crate::vm::value::ObjClosure,
    pub thunk_raw: *const (),
    pub arity: u8,
    pub inline_kind: MethodInlineKind,
    _pad: [u8; 6],
    pub inline_field_index: u32,
    _pad2: [u8; 4],
    pub inline_addend: i64,
    pub struct_def: Option<std::rc::Rc<crate::vm::value::ObjStructDef>>,
    pub closure: Option<std::rc::Rc<crate::vm::value::ObjClosure>>,
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
    pub struct_def_raw: *const crate::vm::value::ObjStructDef,
    pub field_index: u32,
    pub kind: FieldCacheKind,
    _pad: [u8; 3],
    pub closure_raw: *const crate::vm::value::ObjClosure,
    pub thunk_raw: *const (),
    pub struct_def: Option<std::rc::Rc<crate::vm::value::ObjStructDef>>,
    pub closure: Option<std::rc::Rc<crate::vm::value::ObjClosure>>,
}

#[allow(dead_code)]
impl FieldCacheEntry {
    pub(crate) const OFFSET_STRUCT_DEF_RAW: i32 = 0;
    pub(crate) const OFFSET_FIELD_INDEX: i32 = 8;
    pub(crate) const OFFSET_KIND: i32 = 12;
    pub(crate) const OFFSET_CLOSURE_RAW: i32 = 16;
}

enum Entry {
    Compiled { entries: CompiledEntries },
    Failed,
}

/// Which entry point the compile_function IR emitter is currently
/// producing. Used as a compile-time branch variable at three
/// divergence points (entry block params, prologue, OpCode::Return
/// success path) — everywhere else the emission logic is shared.
///
/// The ABI invariant: BOTH entry kinds return a single `i32` status
/// value. Specialized entries carry their `i64` payload through an
/// out-param pointer supplied by the caller; see `ret_out_param_index`.
/// This keeps every existing `return_(&[status])` site in the dispatch
/// loop unchanged, avoiding an audit of ~1900 lines of IR.
#[allow(dead_code)] // Generic variant not yet routed through this enum
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum EntryKind {
    /// `fn(*mut VM) -> u32`. Caller pushes args onto the VM stack as
    /// `Value`. Entry reads them through `stack_view`, optionally
    /// through B2.2a tag guards for int mirrors. Successful
    /// `OpCode::Return` calls the `op_return` helper.
    Generic,
    /// `fn(*mut VM, i64, ..., i64, *mut i64) -> u32`. Caller passes
    /// args in registers and supplies an i64 out-param for the return
    /// payload. Entry writes each i64 back to the backing stack slot
    /// for helper compat + `def_var`s the mirror Variable directly —
    /// no tag guard (args are trusted Int by the caller's contract).
    /// Successful `OpCode::Return` stores the payload via `*ret_out`,
    /// tears down the frame inline, and `return_(&[0])`. Early
    /// error/bailout returns remain shaped identically to Generic
    /// (`return_(&[status])`) thanks to the single-status ABI.
    IntSpecialized {
        /// Block-param index of the `*mut i64` out pointer — the last
        /// entry-block param. Arity is `ret_out_param_index - 1` (vm
        /// is index 0).
        ret_out_param_index: usize,
    },
}

/// What kind of specialized entry a CompiledEntries carries. Observable
/// on the VM side (ObjClosure) so call sites can gate A3's direct-call
/// path on having a real body, not a trampoline.
#[allow(dead_code)] // NativeIntBody variant lands in a later commit
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum SpecializedEntryKind {
    /// A2's adapter: boxes i64 args → calls generic → unboxes result.
    /// Correctness-preserving but pays more than it saves vs the IC
    /// path. A3 must NOT direct-call trampolines.
    ForwardTrampoline,
    /// The real specialized body: runs the function's opcodes with
    /// i64-resident args, no box/unbox round trip. Safe A3 target.
    NativeIntBody,
}

pub(super) enum InvokeOutcome {
    Returned,
    Bailout,
    RuntimeError(VMError),
}

/// FuncIds of every runtime helper the translator might call.
struct HelperIds {
    // Push
    push_constant: FuncId,       // (vm, u32)
    push_integer_inline: FuncId, // (vm, i64)
    push_float_inline: FuncId,   // (vm, i64 bits-of-f64)
    push_none: FuncId,           // (vm)
    push_true: FuncId,           // (vm)
    push_false: FuncId,          // (vm)

    // Stack manip
    pop: FuncId, // (vm)
    dup: FuncId, // (vm)

    // Collections
    build_array: FuncId,          // (vm, u32)
    index_fast_array_int: FuncId, // (vm) -> u32
    type_wrap: FuncId,            // (vm, u32) -> u32
    local_add_array_mod_index: FuncId, // (vm, u32, u32, u32, i64, u32) -> u32
    struct_field_add_const: FuncId, // (vm, u32, u32, i64) -> u32
    struct_field_add_local: FuncId, // (vm, u32, u32, u32) -> u32

    // Locals
    get_local: FuncId, // (vm, u32)
    set_local: FuncId, // (vm, u32)

    // Arithmetic (fallible)
    add: FuncId,
    sub: FuncId,
    mul: FuncId,
    div: FuncId,
    modu: FuncId,

    // Comparison
    eq: FuncId, // infallible
    ne: FuncId, // infallible
    lt: FuncId,
    le: FuncId,
    gt: FuncId,
    ge: FuncId,

    // Logical / unary
    not: FuncId,    // infallible
    negate: FuncId, // fallible

    // Branching truthy checks
    peek_truthy: FuncId, // (vm) -> u32
    pop_truthy: FuncId,  // (vm) -> u32

    // Return
    op_return: FuncId, // (vm)

    // Call (fallible, takes arg_count)
    op_call: FuncId,    // (vm, u32) -> u32
    op_call_hit: FuncId,  // (vm, *mut CallCacheEntry) -> u32
    op_call_miss: FuncId, // (vm, u32, *mut CallCacheEntry) -> u32

    // Globals
    get_global: FuncId,          // (vm, u32) -> u32
    get_global_ic: FuncId,       // (vm, *mut GlobalCacheEntry, u32) -> u32
    set_global: FuncId,          // (vm, u32) -> u32
    define_global: FuncId,       // (vm, u32) -> u32
    define_global_typed: FuncId, // (vm, u32, u32, u32) -> u32

    // Upvalues
    get_upvalue: FuncId,   // (vm, u32)
    set_upvalue: FuncId,   // (vm, u32)
    close_upvalue: FuncId, // (vm)

    // Closure
    op_closure: FuncId, // (vm, u32, u32) -> u32

    // Struct ops
    op_struct_def: FuncId,     // (vm, u32)
    op_struct_literal: FuncId, // (vm, u32, u32) -> u32
    op_get_field_ic_miss: FuncId, // (vm, u32, *mut FieldCacheEntry) -> u32
    op_set_field_ic_miss: FuncId, // (vm, u32, *mut FieldCacheEntry) -> u32
    op_define_method: FuncId,  // (vm, u32, u32) -> u32
    op_method_call: FuncId,    // (vm, u32, u32) -> u32
    op_method_call_ic: FuncId, // (vm, u32, u32, *mut MethodCacheEntry) -> u32

    // Inline int fast-path support
    stack_as_mut_ptr: FuncId,       // (vm) -> *mut Value (pointer-sized)
    stack_len: FuncId,              // (vm) -> i64
    stack_pop_one: FuncId,          // (vm)
    stack_pop_n: FuncId,            // (vm, u32)
    stack_commit_len: FuncId,       // (vm, u64)
    stack_truncate: FuncId,         // (vm, i64)
    replace_top2_with_bool: FuncId, // (vm, u32)
    current_slot_offset: FuncId,    // (vm) -> i64
}

/// FuncRefs for the current function's context — the in-function "import
/// table" of declared helpers.
struct HelperRefs {
    push_constant: FuncRef,
    #[allow(dead_code)]
    push_integer_inline: FuncRef,
    #[allow(dead_code)]
    push_float_inline: FuncRef,
    push_none: FuncRef,
    push_true: FuncRef,
    push_false: FuncRef,
    pop: FuncRef,
    dup: FuncRef,
    build_array: FuncRef,
    index_fast_array_int: FuncRef,
    type_wrap: FuncRef,
    local_add_array_mod_index: FuncRef,
    struct_field_add_const: FuncRef,
    struct_field_add_local: FuncRef,
    get_local: FuncRef,
    set_local: FuncRef,
    add: FuncRef,
    sub: FuncRef,
    mul: FuncRef,
    div: FuncRef,
    modu: FuncRef,
    eq: FuncRef,
    ne: FuncRef,
    lt: FuncRef,
    le: FuncRef,
    gt: FuncRef,
    ge: FuncRef,
    not: FuncRef,
    negate: FuncRef,
    peek_truthy: FuncRef,
    pop_truthy: FuncRef,
    op_return: FuncRef,
    #[allow(dead_code)]
    op_call: FuncRef,
    #[allow(dead_code)]
    op_call_hit: FuncRef,
    op_call_miss: FuncRef,
    #[allow(dead_code)]
    get_global: FuncRef,
    get_global_ic: FuncRef,
    set_global: FuncRef,
    define_global: FuncRef,
    define_global_typed: FuncRef,
    get_upvalue: FuncRef,
    set_upvalue: FuncRef,
    close_upvalue: FuncRef,
    op_closure: FuncRef,
    op_struct_def: FuncRef,
    op_struct_literal: FuncRef,
    op_get_field_ic_miss: FuncRef,
    op_set_field_ic_miss: FuncRef,
    op_define_method: FuncRef,
    #[allow(dead_code)]
    op_method_call: FuncRef,
    op_method_call_ic: FuncRef,
    // `stack_as_mut_ptr` and `stack_len` are still registered as runtime
    // helpers for backwards compatibility, but the hot JIT paths now
    // read `vm.stack_view.{ptr, len}` directly via `emit_load_stack_*`
    // — no FFI crossing.
    #[allow(dead_code)]
    stack_as_mut_ptr: FuncRef,
    #[allow(dead_code)]
    stack_len: FuncRef,
    #[allow(dead_code)]
    stack_pop_one: FuncRef,
    stack_pop_n: FuncRef,
    stack_commit_len: FuncRef,
    stack_truncate: FuncRef,
    replace_top2_with_bool: FuncRef,
    #[allow(dead_code)]
    current_slot_offset: FuncRef,
}

impl JitInner {
    pub fn new() -> Self {
        let flag_builder = settings::builder();
        let flags = settings::Flags::new(flag_builder);

        let isa_builder =
            cranelift_native::builder().expect("cranelift-native should detect host ISA");
        let isa = isa_builder
            .finish(flags)
            .expect("ISA builder should finalize");

        let mut jit_builder = JITBuilder::with_isa(isa, default_libcall_names());
        // Register every runtime helper so Cranelift's linker can resolve
        // them at finalize time.
        register_helpers(&mut jit_builder);
        let mut module = JITModule::new(jit_builder);

        let helpers = declare_helpers(&mut module);

        Self {
            module,
            ctx: Context::new(),
            fbc: FunctionBuilderContext::new(),
            compiled: HashMap::new(),
            retained: HashMap::new(),
            helpers,
            current_stop_depth: 0,
            pending_error: None,
            next_id: 0,
            counters: Box::new(JitCounters::new()),
            global_caches: Vec::new(),
            call_caches: Vec::new(),
            method_caches: Vec::new(),
            field_caches: Vec::new(),
        }
    }

    /// Stable raw pointer to the counters block, safe to bake into
    /// emitted IR. The `Box` keeps the allocation pinned for the
    /// JitInner's lifetime.
    pub(crate) fn counters_ptr(&self) -> *const JitCounters {
        self.counters.as_ref() as *const _
    }

    /// Dump counters to stderr. Triggered from the Drop impl when the
    /// env var is set. Public so other code (a future /jit-stats flag)
    /// can invoke it on demand.
    pub(crate) fn dump_counters(&self) {
        self.counters.dump();
    }

    /// Allocate a fresh inline-cache slot for a GetGlobal call site.
    /// Returns a raw pointer whose address is stable for the JitInner's
    /// lifetime (because the Box's heap allocation doesn't move).
    pub(crate) fn alloc_global_cache(&mut self) -> *mut GlobalCacheEntry {
        self.global_caches.push(Box::new(GlobalCacheEntry {
            version: u64::MAX,
            value: crate::vm::value::Value::None,
        }));
        let b = self.global_caches.last_mut().unwrap();
        b.as_mut() as *mut _
    }

    /// Allocate a fresh inline-cache slot for a Call site. Returns a raw
    /// pointer with stable address for the JitInner's lifetime.
    pub(crate) fn alloc_call_cache(&mut self) -> *mut CallCacheEntry {
        self.call_caches.push(Box::new(CallCacheEntry {
            closure_raw: std::ptr::null(),
            thunk_raw: std::ptr::null(),
            arity: 0,
            _pad: [0; 7],
            _keeper: None,
        }));
        let b = self.call_caches.last_mut().unwrap();
        b.as_mut() as *mut _
    }

    /// Allocate a fresh inline-cache slot for a MethodCall site.
    pub(crate) fn alloc_method_cache(&mut self) -> *mut MethodCacheEntry {
        self.method_caches.push(Box::new(MethodCacheEntry {
            struct_def_raw: std::ptr::null(),
            closure_raw: std::ptr::null(),
            thunk_raw: std::ptr::null(),
            arity: 0,
            inline_kind: MethodInlineKind::None,
            _pad: [0; 6],
            inline_field_index: 0,
            _pad2: [0; 4],
            inline_addend: 0,
            struct_def: None,
            closure: None,
        }));
        let b = self.method_caches.last_mut().unwrap();
        b.as_mut() as *mut _
    }

    pub(crate) fn alloc_field_cache(&mut self) -> *mut FieldCacheEntry {
        self.field_caches.push(Box::new(FieldCacheEntry {
            struct_def_raw: std::ptr::null(),
            field_index: 0,
            kind: FieldCacheKind::Invalid,
            _pad: [0; 3],
            closure_raw: std::ptr::null(),
            thunk_raw: std::ptr::null(),
            struct_def: None,
            closure: None,
        }));
        let b = self.field_caches.last_mut().unwrap();
        b.as_mut() as *mut _
    }

    pub fn current_stop_depth(&self) -> usize {
        self.current_stop_depth
    }

    pub fn stash_error(&mut self, err: VMError) {
        self.pending_error = Some(err);
    }

    #[allow(dead_code)]
    pub fn has_pending_error(&self) -> bool {
        self.pending_error.is_some()
    }

    pub fn compiled_ok_count(&self) -> usize {
        self.compiled
            .values()
            .filter(|e| matches!(e, Entry::Compiled { .. }))
            .count()
    }

    pub fn compile_failed_count(&self) -> usize {
        self.compiled
            .values()
            .filter(|e| matches!(e, Entry::Failed))
            .count()
    }

    pub fn maybe_compile(&mut self, func: &Rc<Function>) -> bool {
        self.maybe_compile_thunk(func).is_some()
    }

    /// Primary JIT-compile hook. Returns the generic thunk (`fn(*mut VM)
    /// -> u32`) on success. Specialized entry, when generated, is
    /// available via `maybe_compile_entries`.
    pub fn maybe_compile_thunk(&mut self, func: &Rc<Function>) -> Option<CompiledThunk> {
        self.maybe_compile_entries(func).map(|e| e.generic)
    }

    /// Like `maybe_compile_thunk` but exposes the full `CompiledEntries`.
    /// Callers that care about the specialized entry (VM's closure
    /// installer) go through this; everyone else can keep using
    /// `maybe_compile_thunk`.
    pub fn maybe_compile_entries(&mut self, func: &Rc<Function>) -> Option<&CompiledEntries> {
        let key = Rc::as_ptr(func);
        if !self.compiled.contains_key(&key) {
            if scan::scan(&func.chunk).is_err() {
                self.compiled.insert(key, Entry::Failed);
                return None;
            }
            match self.compile_function(func) {
                Ok(entries) => {
                    self.retained.insert(key, Rc::clone(func));
                    self.compiled.insert(key, Entry::Compiled { entries });
                }
                Err(_) => {
                    self.compiled.insert(key, Entry::Failed);
                }
            }
        }
        match self.compiled.get(&key) {
            Some(Entry::Compiled { entries }) => Some(entries),
            _ => None,
        }
    }

    pub unsafe fn invoke(
        &mut self,
        vm: *mut VM,
        func: &Rc<Function>,
        stop_depth: usize,
    ) -> InvokeOutcome {
        let key = Rc::as_ptr(func);
        let thunk = match self.compiled.get(&key) {
            Some(Entry::Compiled { entries }) => entries.generic,
            _ => return InvokeOutcome::Bailout,
        };

        unsafe { self.invoke_thunk(vm, thunk, stop_depth) }
    }

    #[inline(never)]
    pub unsafe fn invoke_thunk(
        &mut self,
        vm: *mut VM,
        thunk: CompiledThunk,
        stop_depth: usize,
    ) -> InvokeOutcome {
        // CRITICAL ALIASING NOTE: the `thunk(vm)` call is opaque
        // `extern "C"` code that re-enters the VM through the raw `vm`
        // pointer. Inside, JIT-emitted helpers call
        // `vm.jit.stash_error(err)`, which reaches back to this very
        // `JitInner` through `vm.jit.inner.as_mut()`. LLVM can't see
        // that `vm.jit.inner` and `&mut self` point at the same
        // allocation, so it assumes `thunk(vm)` cannot modify
        // `self.pending_error` and caches the pre-call `None`. The
        // post-call read then returns that stale `None`, and the real
        // error gets replaced with
        // "JIT runtime error with no stashed detail".
        //
        // Fix: access `pending_error` through a raw pointer and use
        // volatile reads/writes across the opaque call boundary, so
        // the compiler cannot speculate the value past the thunk.
        let self_ptr: *mut JitInner = self as *mut JitInner;
        let pending_ptr = unsafe { &raw mut (*self_ptr).pending_error };
        let stop_depth_ptr = unsafe { &raw mut (*self_ptr).current_stop_depth };

        unsafe {
            stop_depth_ptr.write_volatile(stop_depth);
            pending_ptr.write_volatile(None);
        }

        // Flip VM into "JIT executing" mode so `current_constant`,
        // `active_closure`, etc. read from `jit_frame_view` (the thunk's
        // frame) instead of `frames` (a paused interpreter frame from
        // an outer `execute_until`, if any). Save + restore to handle
        // nested invoke_thunk calls correctly.
        let vm_ref = unsafe { &*vm };
        let prev_jit_exec = vm_ref.jit_executing.replace(true);

        let status = unsafe { thunk(vm) };

        vm_ref.jit_executing.set(prev_jit_exec);
        unsafe {
            stop_depth_ptr.write_volatile(0);
        }
        match status {
            0 => InvokeOutcome::Returned,
            1 => {
                // Volatile read to force LLVM to reload the stashed Option.
                let err_opt: Option<VMError> = unsafe { pending_ptr.read_volatile() };
                unsafe { pending_ptr.write_volatile(None) };
                let err = err_opt.unwrap_or_else(|| VMError {
                    message: "JIT runtime error with no stashed detail".to_string(),
                    line: 0,
                });
                InvokeOutcome::RuntimeError(err)
            }
            _ => InvokeOutcome::Bailout,
        }
    }

    fn compile_function(&mut self, func: &Rc<Function>) -> Result<CompiledEntries, ()> {
        let chunk = &func.chunk;
        let code = &chunk.code;

        let info = scan::scan(chunk).map_err(|_| ())?;

        // A0: emit counter-bump IR only when OXIGEN_JIT_STATS is set at
        // compile time of this function. Keeps the normal path cost-free.
        let counters_ptr_opt: Option<*const JitCounters> =
            if std::env::var("OXIGEN_JIT_STATS").is_ok() {
                Some(self.counters.as_ref() as *const _)
            } else {
                None
            };

        // B2.1b: run the typed slot analysis. Consumed further down to
        // declare Cranelift Variables for virtualizable Int64 locals.
        // Runs before any IR is emitted so the analysis result is stable
        // across the whole compile_function call.
        let slot_types = crate::compiler::slot_types::analyze(func);
        #[cfg(debug_assertions)]
        {
            if let Err(msg) = crate::compiler::slot_types::certify(func, &slot_types) {
                // Certifier failure is a soft error: we log and continue
                // without virtualizing. B2.1b codegen is safe because
                // `int_locals` below is populated only from entries we
                // then consume; a skipped declaration just means the
                // slot stays on the Value path.
                eprintln!(
                    "[jit] slot_types certify failed for {:?}: {}",
                    func.name.as_deref().unwrap_or("<anonymous>"),
                    msg,
                );
            }
        }

        // Pre-allocate one inline-cache slot per GetGlobal opcode in the
        // bytecode so the emit pass can hand them out sequentially
        // without needing to re-borrow `self` from inside the builder
        // scope. Boxes give us stable heap addresses to bake into IR.
        let (get_global_count, call_count, method_call_count, field_op_count) =
            count_ic_sites(chunk);
        let mut global_cache_ptrs: Vec<*mut GlobalCacheEntry> =
            Vec::with_capacity(get_global_count);
        for _ in 0..get_global_count {
            global_cache_ptrs.push(self.alloc_global_cache());
        }
        let mut call_cache_ptrs: Vec<*mut CallCacheEntry> = Vec::with_capacity(call_count);
        for _ in 0..call_count {
            call_cache_ptrs.push(self.alloc_call_cache());
        }
        let mut method_cache_ptrs: Vec<*mut MethodCacheEntry> =
            Vec::with_capacity(method_call_count);
        for _ in 0..method_call_count {
            method_cache_ptrs.push(self.alloc_method_cache());
        }
        let mut field_cache_ptrs: Vec<*mut FieldCacheEntry> = Vec::with_capacity(field_op_count);
        for _ in 0..field_op_count {
            field_cache_ptrs.push(self.alloc_field_cache());
        }

        // Generic thunk signature: fn(*mut VM) -> u32.
        let ptr_ty = self.module.target_config().pointer_type();
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(ptr_ty));
        sig.returns.push(AbiParam::new(types::I32));

        self.next_id = self.next_id.wrapping_add(1);
        let generic_seq_id = self.next_id;
        let thunk_name = format!("oxigen_jit_thunk_{}_{:p}", generic_seq_id, Rc::as_ptr(func));
        let thunk_id = self
            .module
            .declare_function(&thunk_name, Linkage::Local, &sig)
            .map_err(|_| ())?;

        // A2: specialized thunk signature, declared UPFRONT when
        // eligible so either body can reference the other by FuncId
        // via `declare_func_in_func`. Signature:
        //   fn(*mut VM, i64, ..., i64) -> (i64, u32)
        // Arity i64 params, then a (payload, status) multi-return.
        let (spec_thunk_id, spec_seq_id): (Option<FuncId>, Option<u32>) =
            if slot_types.specialized_entry_eligible {
                let arity = func.arity as usize;
                let mut spec_sig = self.module.make_signature();
                spec_sig.params.push(AbiParam::new(ptr_ty));
                for _ in 0..arity {
                    spec_sig.params.push(AbiParam::new(types::I64));
                }
                spec_sig.returns.push(AbiParam::new(types::I64));
                spec_sig.returns.push(AbiParam::new(types::I32));
                self.next_id = self.next_id.wrapping_add(1);
                let ssid = self.next_id;
                let spec_name = format!(
                    "oxigen_jit_specialized_{}_{:p}",
                    ssid,
                    Rc::as_ptr(func)
                );
                let sid = self
                    .module
                    .declare_function(&spec_name, Linkage::Local, &spec_sig)
                    .map_err(|_| ())?;
                (Some(sid), Some(ssid))
            } else {
                (None, None)
            };

        self.ctx.func.signature = sig;
        self.ctx.func.name = UserFuncName::user(0, generic_seq_id);

        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.fbc);

            let refs = declare_helper_refs(&self.helpers, &mut self.module, &mut builder);

            // Create blocks.
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);

            let mut blocks: HashMap<usize, Block> = HashMap::new();
            blocks.insert(0, entry_block);
            for &target in &info.branch_targets {
                blocks
                    .entry(target)
                    .or_insert_with(|| builder.create_block());
            }

            builder.switch_to_block(entry_block);
            let vm_val = builder.block_params(entry_block)[0];

            let thunk_sig = {
                let mut sig = self.module.make_signature();
                sig.params.push(AbiParam::new(ptr_ty));
                sig.returns.push(AbiParam::new(types::I32));
                builder.import_signature(sig)
            };

            // Cache the frame's slot_offset in SSA by reading the active
            // JIT frame directly. The slot offset is immutable for the
            // lifetime of this activation.
            let slot_offset_val = emit_load_top_jit_frame_slot_offset(&mut builder, vm_val);

            // B2.1b: per-function virtualization state. For each
            // virtualizable slot with a recognized bytecode-level
            // initializer (excludes params — those are B2.2a's job),
            // allocate a Cranelift `Variable` to hold its authoritative
            // value in SSA. The backing VM stack slot is still
            // populated at initialization so stack shape remains valid;
            // `flush_all` (below) spills the Variable back into the
            // backing slot before any generic runtime op can observe
            // the stack.
            //
            // In B2.1b the state is allocated and Variables are
            // declared + def_var'd at their initialization IP, but no
            // opcode CONSUMES them yet. B2.1c wires up GetLocal /
            // SetLocal / Pop to actually use the virtualized path.
            let mut int_locals: HashMap<u16, Variable> = HashMap::new();
            let mut live_int_slots: HashSet<u16> = HashSet::new();
            // `expr_stack` is infrastructure for B2.1c+. Not consumed
            // yet — kept declared so the helper signatures stay
            // consistent across stages.
            let mut expr_stack: Vec<cranelift_codegen::ir::Value> = Vec::new();
            let _ = &mut expr_stack;
            // B2.1e: cleanup-Pop IPs that the virtual-branch path has
            // elided. When a virtual icmp+brif fires without
            // materializing a Boolean on the stack, the compiler's
            // subsequent Pop(s) (on both the fall-through and the
            // jump-target paths) have nothing to pop. Add them here
            // so the Pop handler treats them as virtual no-ops.
            let mut virt_branch_elided_pops: HashSet<usize> = HashSet::new();

            // Declare one Variable per virtualizable slot with a
            // recognized bytecode-level initializer. Variable IDs come
            // from an independent counter so they don't collide with
            // any future non-slot Variables.
            let mut next_var_id: u32 = 0;
            {
                let mut needs_var: Vec<u16> = Vec::new();
                for &slot in slot_types.local_init_result_ip.values() {
                    if slot_types.is_virtualizable(slot) && !int_locals.contains_key(&slot) {
                        needs_var.push(slot);
                    }
                }
                // Sort for deterministic Variable assignment so two
                // identical compilations produce identical IR —
                // important for any future incremental-compile or
                // golden-file testing.
                needs_var.sort_unstable();
                needs_var.dedup();
                for slot in needs_var {
                    let var = Variable::from_u32(next_var_id);
                    next_var_id += 1;
                    builder.declare_var(var, types::I64);
                    int_locals.insert(slot, var);
                }
            }

            // Entry-time Int-param virtualization. Two sources feed into
            // this prologue:
            //
            //   1. B2.2a `int_mirror_param_slots`: Value-typed params used
            //      only as Ints (per int-demand heuristic). Read-only by
            //      eligibility rule — Variable lives in `param_mirrors`.
            //
            //   2. B2.1 Int64-typed params: params declared `<int>` in
            //      source. May be written in the body (e.g., collatz's
            //      `n = n / 2`), so the Variable lives in `int_locals`
            //      where SetLocal's virt-def_var path can see it.
            //
            // Both go through the same one-shot tag guard: if the
            // runtime tag isn't Integer, bail out to the interpreter.
            // On success, extract payload + def_var.
            //
            // Tracks the block the main dispatch loop should start
            // emitting into. Normally equals `entry_block`; shifts to
            // post-prologue when this block emits any tag guards.
            let mut effective_entry_block = entry_block;

            // (1) Read-only Value-typed param mirrors (B2.2a).
            let mut param_mirrors: HashMap<u16, Variable> = HashMap::new();
            if !slot_types.int_mirror_param_slots.is_empty() {
                let mut eligible: Vec<u16> =
                    slot_types.int_mirror_param_slots.iter().copied().collect();
                eligible.sort_unstable();
                for slot in eligible {
                    let var = Variable::from_u32(next_var_id);
                    next_var_id += 1;
                    builder.declare_var(var, types::I64);
                    param_mirrors.insert(slot, var);
                }
            }

            // (2) Int64-typed params that weren't already given a Variable
            // by the Constant-init pass. These feed into int_locals so
            // SetLocal's def_var path picks them up (writable params).
            let mut int_typed_param_slots: Vec<u16> = Vec::new();
            for slot in 1..=(func.arity as u16) {
                if slot_types.is_virtualizable(slot) && !int_locals.contains_key(&slot) {
                    let var = Variable::from_u32(next_var_id);
                    next_var_id += 1;
                    builder.declare_var(var, types::I64);
                    int_locals.insert(slot, var);
                    int_typed_param_slots.push(slot);
                }
            }
            int_typed_param_slots.sort_unstable();

            // Combined slot list for the prologue. Sort for deterministic
            // IR output; `int_typed_param_slots` and `param_mirrors` are
            // disjoint by construction (the int-mirror analysis excludes
            // slots that are already Int64-typed).
            let mut prologue_slots: Vec<u16> = param_mirrors
                .keys()
                .copied()
                .chain(int_typed_param_slots.iter().copied())
                .collect();
            prologue_slots.sort_unstable();

            if !prologue_slots.is_empty() {
                use cranelift_codegen::ir::MemFlags;
                let flags = MemFlags::trusted();

                let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                let integer_tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);

                let bailout_block = builder.create_block();

                for slot in &prologue_slots {
                    let slot_const = builder.ins().iconst(types::I64, *slot as i64);
                    let abs_slot = builder.ins().iadd(slot_offset_val, slot_const);
                    let byte_off = builder.ins().imul(abs_slot, value_size);
                    let slot_ptr = builder.ins().iadd(stack_ptr, byte_off);

                    let tag = builder.ins().load(types::I8, flags, slot_ptr, 0);
                    let is_int = builder.ins().icmp(
                        cranelift_codegen::ir::condcodes::IntCC::Equal,
                        tag,
                        integer_tag,
                    );
                    let ok_block = builder.create_block();
                    builder
                        .ins()
                        .brif(is_int, ok_block, &[], bailout_block, &[]);
                    builder.switch_to_block(ok_block);

                    let payload = builder.ins().load(
                        types::I64,
                        flags,
                        slot_ptr,
                        VALUE_INT_PAYLOAD_OFFSET as i32,
                    );
                    // Route the def_var through whichever map owns the
                    // slot. `param_mirrors` for read-only Value-typed
                    // params; `int_locals` for Int64-typed (writable)
                    // params.
                    if let Some(&var) = param_mirrors.get(slot) {
                        builder.def_var(var, payload);
                    } else if let Some(&var) = int_locals.get(slot) {
                        builder.def_var(var, payload);
                        live_int_slots.insert(*slot);
                    }
                }

                // All guards passed — jump into a fresh "prologue_done"
                // block that the main dispatch loop will emit into.
                let prologue_done = builder.create_block();
                builder.ins().jump(prologue_done, &[]);

                // Bailout path: return status 2. VM's InvokeOutcome
                // matches any status other than 0/1 to Bailout, which
                // pops the JIT frame and reinstates the interpreter.
                builder.switch_to_block(bailout_block);
                let two = builder.ins().iconst(types::I32, 2);
                builder.ins().return_(&[two]);

                // Resume emission at the prologue-complete block so
                // the main dispatch loop writes the actual function
                // body there, not into the bailout block.
                builder.switch_to_block(prologue_done);
                effective_entry_block = prologue_done;
                // The dispatch loop looks up blocks[0] on the first
                // iteration and would otherwise jump back into the
                // already-terminated entry_block. Remap IP 0 to the
                // post-prologue block.
                blocks.insert(0, prologue_done);
            }

            let mut current_block = effective_entry_block;
            let mut terminated = false;
            let mut ip: usize = 0;
            // Counter for handing out the pre-allocated inline-cache
            // slots as we emit GetGlobal opcodes.
            let mut ic_ix: usize = 0;
            // Same, but for Call opcodes.
            let mut call_ic_ix: usize = 0;
            // Same, but for MethodCall opcodes.
            let mut method_ic_ix: usize = 0;
            let mut field_ic_ix: usize = 0;

            while ip < code.len() {
                // If this ip starts a new block, switch to it (and glue
                // the current block to it if fall-through).
                if let Some(&block) = blocks.get(&ip) {
                    if block != current_block {
                        if !terminated {
                            builder.ins().jump(block, &[]);
                        }
                        builder.switch_to_block(block);
                        current_block = block;
                        terminated = false;
                    }
                }

                let op = OpCode::from_byte(code[ip]).ok_or(())?;
                let line = chunk.lines.get(ip).copied().unwrap_or(0);
                emit_store_current_line(&mut builder, vm_val, line);

                // B2.1c: light flush before any op we aren't
                // explicitly virtualizing. Only drains `expr_stack`
                // (materializes staged int temps onto the real VM
                // stack) — does NOT spill virtualized locals' Variables
                // to backing. Those stay live-in-SSA; `Vec::len` isn't
                // touched either because the push helpers update
                // `stack_view.len` inline and non-helper ops don't
                // observe `Vec::len` directly.
                //
                // The three opcode families below handle their own
                // flush decisions (GetLocal may be a peephole; SetLocal
                // may virtualize via `expr_stack.last()`; Pop may
                // consume from `expr_stack`).
                let handles_own_flush = matches!(
                    op,
                    OpCode::GetLocal
                        | OpCode::SetLocal
                        | OpCode::Pop
                        | OpCode::Constant
                        | OpCode::Add
                        | OpCode::Subtract
                        | OpCode::Multiply
                        | OpCode::Divide
                        | OpCode::Modulo
                        | OpCode::Less
                        | OpCode::LessEqual
                        | OpCode::Greater
                        | OpCode::GreaterEqual
                );
                if !handles_own_flush && !expr_stack.is_empty() {
                    flush_expr_stack_to_stack_view(&mut builder, vm_val, &mut expr_stack);
                }

                match op {
                    OpCode::Constant => {
                        let idx = read_u16(code, ip + 1);
                        let init_slot = slot_types.local_init_result_ip.get(&ip).copied();
                        match &chunk.constants[idx as usize] {
                            crate::vm::value::Value::Integer(n) => {
                                let v = builder.ins().iconst(types::I64, *n);

                                if let Some(slot) = init_slot {
                                    // Local-initializer Constant: still
                                    // materialize backing slot so stack
                                    // shape stays valid, and def_var.
                                    emit_inline_push_integer(&mut builder, vm_val, v);
                                    if let Some(&var) = int_locals.get(&slot) {
                                        builder.def_var(var, v);
                                        live_int_slots.insert(slot);
                                    }
                                } else {
                                    // Expression-temp Integer: stage on
                                    // expr_stack. No memory op yet.
                                    expr_stack.push(v);
                                }
                            }
                            crate::vm::value::Value::Float(f) => {
                                if !expr_stack.is_empty() {
                                    flush_expr_stack_to_stack_view(
                                        &mut builder,
                                        vm_val,
                                        &mut expr_stack,
                                    );
                                }
                                let bits = f.to_bits() as i64;
                                let v = builder.ins().iconst(types::I64, bits);
                                emit_inline_push_float(&mut builder, vm_val, v);
                            }
                            _ => {
                                if !expr_stack.is_empty() {
                                    flush_expr_stack_to_stack_view(
                                        &mut builder,
                                        vm_val,
                                        &mut expr_stack,
                                    );
                                }
                                // Fall back to the generic helper for
                                // strings, closures, etc.
                                let idx_val = builder.ins().iconst(types::I32, idx as i64);
                                builder.ins().call(refs.push_constant, &[vm_val, idx_val]);
                            }
                        }
                        ip += 3;
                    }
                    OpCode::None => {
                        builder.ins().call(refs.push_none, &[vm_val]);
                        ip += 1;
                    }
                    OpCode::True => {
                        builder.ins().call(refs.push_true, &[vm_val]);
                        ip += 1;
                    }
                    OpCode::False => {
                        builder.ins().call(refs.push_false, &[vm_val]);
                        ip += 1;
                    }
                    OpCode::Pop => {
                        // IP-dispatched Pop handling:
                        //   1. Virtual branch elided (B2.1e): the virt
                        //      compare+branch path didn't materialize a
                        //      Boolean — its cleanup Pops have nothing
                        //      to remove. Skip entirely.
                        //   2. Scope teardown: the slot this Pop is
                        //      destroying goes out of scope. Remove
                        //      from live_int_slots so later flushes
                        //      don't store into the dead slot; then
                        //      run the existing physical Pop.
                        //   3. Expression cleanup: a staged temp is
                        //      being removed. Pop expr_stack — no
                        //      memory op.
                        //   4. Everything else: ordinary Pop on the
                        //      real stack.
                        if virt_branch_elided_pops.contains(&ip) {
                            // Virtual no-op — no mem op, no expr_stack change.
                        } else if let Some(slot) = slot_types.scope_pop_for(ip) {
                            live_int_slots.remove(&slot);
                            emit_inline_pop_tag_gated(&mut builder, &refs, vm_val);
                        } else if !expr_stack.is_empty() {
                            expr_stack.pop();
                        } else {
                            emit_inline_pop_tag_gated(&mut builder, &refs, vm_val);
                        }
                        ip += 1;
                    }
                    OpCode::Dup => {
                        builder.ins().call(refs.dup, &[vm_val]);
                        ip += 1;
                    }
                    OpCode::BuildArray => {
                        let count = read_u16(code, ip + 1);
                        let count_val = builder.ins().iconst(types::I32, count as i64);
                        builder.ins().call(refs.build_array, &[vm_val, count_val]);
                        ip += 3;
                    }
                    OpCode::Index => {
                        let call = builder.ins().call(refs.index_fast_array_int, &[vm_val]);
                        let status = builder.inst_results(call)[0];
                        emit_early_exit_on_err(&mut builder, status);
                        ip += 1;
                    }
                    OpCode::TypeWrap => {
                        let idx = read_u16(code, ip + 1);
                        let idx_val = builder.ins().iconst(types::I32, idx as i64);
                        let call = builder.ins().call(refs.type_wrap, &[vm_val, idx_val]);
                        let status = builder.inst_results(call)[0];
                        emit_early_exit_on_err(&mut builder, status);
                        ip += 3;
                    }
                    OpCode::GetLocal => {
                        // Peephole matches read the real VM stack; if we
                        // have staged int temps, flush them first so the
                        // peephole sees a consistent stack.
                        if let Some(m) =
                            match_struct_field_add_update(code, chunk, ip, &blocks)
                        {
                            if !expr_stack.is_empty() {
                                flush_expr_stack_to_stack_view(
                                    &mut builder,
                                    vm_val,
                                    &mut expr_stack,
                                );
                            }
                            let cache_ptr = field_cache_ptrs[field_ic_ix];
                            field_ic_ix += 2;
                            emit_inline_struct_field_add(
                                &mut builder,
                                &refs,
                                ptr_ty,
                                vm_val,
                                slot_offset_val,
                                m.self_slot,
                                m.field_idx,
                                cache_ptr,
                                m.shape,
                            );
                            ip += m.len;
                        } else if let Some(m) = match_local_array_mod_index_add_update(
                            code, chunk, ip, &blocks,
                        )
                            .filter(|m| !int_locals.contains_key(&m.dst_slot))
                        {
                            if !expr_stack.is_empty() {
                                flush_expr_stack_to_stack_view(
                                    &mut builder,
                                    vm_val,
                                    &mut expr_stack,
                                );
                            }
                            let dst = builder.ins().iconst(types::I32, m.dst_slot as i64);
                            let array = builder.ins().iconst(types::I32, m.array_slot as i64);
                            let index = builder.ins().iconst(types::I32, m.index_slot as i64);
                            let modulus = builder.ins().iconst(types::I64, m.modulus);
                            let pop_after =
                                builder.ins().iconst(types::I32, if m.pop_after { 1 } else { 0 });
                            let call = builder.ins().call(
                                refs.local_add_array_mod_index,
                                &[vm_val, dst, array, index, modulus, pop_after],
                            );
                            let status = builder.inst_results(call)[0];
                            emit_early_exit_on_err(&mut builder, status);
                            ip += m.len;
                        } else if let Some(m) = match_local_arith_update(code, ip, &blocks)
                            .filter(|m| !int_locals.contains_key(&m.dst_slot)
                                && !int_locals.contains_key(&m.rhs_slot))
                        {
                            if !expr_stack.is_empty() {
                                flush_expr_stack_to_stack_view(
                                    &mut builder,
                                    vm_val,
                                    &mut expr_stack,
                                );
                            }
                            emit_inline_local_scaled_arith_update(
                                &mut builder,
                                &refs,
                                vm_val,
                                slot_offset_val,
                                m.dst_slot,
                                m.rhs_slot,
                                1,
                                m.op,
                                m.pop_after,
                            );
                            ip += m.len;
                        } else if let Some(m) =
                            match_local_scaled_arith_update(code, chunk, ip, &blocks)
                                .filter(|m| !int_locals.contains_key(&m.dst_slot)
                                    && !int_locals.contains_key(&m.rhs_slot))
                        {
                            if !expr_stack.is_empty() {
                                flush_expr_stack_to_stack_view(
                                    &mut builder,
                                    vm_val,
                                    &mut expr_stack,
                                );
                            }
                            emit_inline_local_scaled_arith_update(
                                &mut builder,
                                &refs,
                                vm_val,
                                slot_offset_val,
                                m.dst_slot,
                                m.rhs_slot,
                                m.constant,
                                m.op,
                                m.pop_after,
                            );
                            ip += m.len;
                        } else if let Some(m) =
                            match_local_const_arith_update(code, chunk, ip, &blocks)
                                .filter(|m| !int_locals.contains_key(&m.slot))
                        {
                            if !expr_stack.is_empty() {
                                flush_expr_stack_to_stack_view(
                                    &mut builder,
                                    vm_val,
                                    &mut expr_stack,
                                );
                            }
                            emit_inline_local_const_arith_update(
                                &mut builder,
                                &refs,
                                vm_val,
                                slot_offset_val,
                                m.slot,
                                m.constant,
                                m.op,
                                m.pop_after,
                            );
                            ip += m.len;
                        } else {
                            // Simple GetLocal. B2.1c: virtualize if
                            // the slot has a declared Variable AND is
                            // marked live. `use_var` produces an SSA
                            // value that Cranelift resolves via phi
                            // at block joins, so back-edges that
                            // def_var (via SetLocal fallback or
                            // virtualized SetLocal) are correctly
                            // tracked — provided that NO other
                            // compile-time path writes to this slot
                            // outside of our virtualization path.
                            // The peephole guards above ensure that:
                            // local_{arith,scaled,const}_arith_update
                            // peepholes that would touch a virtualized
                            // slot's backing without def_var'ing are
                            // skipped, so the individual
                            // GetLocal/Constant/Add/SetLocal sequence
                            // runs and B2.1c handles it.
                            let slot = read_u16(code, ip + 1);
                            if let Some(&var) = int_locals.get(&slot) {
                                if live_int_slots.contains(&slot) {
                                    let val = builder.use_var(var);
                                    expr_stack.push(val);
                                    ip += 3;
                                    continue;
                                }
                            }
                            // B2.2a: eligible-param mirror. Read-only
                            // Value params that passed the entry tag
                            // guard live in a Cranelift Variable for
                            // the lifetime of the invocation. Push
                            // use_var as a virt Int so downstream
                            // opcodes (virt arith/cmp, SetLocal peek)
                            // can consume it without a memory op.
                            if let Some(&var) = param_mirrors.get(&slot) {
                                let val = builder.use_var(var);
                                expr_stack.push(val);
                                ip += 3;
                                continue;
                            }
                            if !expr_stack.is_empty() {
                                flush_expr_stack_to_stack_view(
                                    &mut builder,
                                    vm_val,
                                    &mut expr_stack,
                                );
                            }
                            emit_inline_get_local(
                                &mut builder,
                                &refs,
                                vm_val,
                                slot_offset_val,
                                slot,
                            );
                            ip += 3;
                        }
                    }
                    OpCode::SetLocal => {
                        // B2.1c: virtualize when the slot has a
                        // declared Variable AND we have a staged int
                        // on top of expr_stack. SetLocal is PEEK-not-
                        // pop (matches VM semantics — Oxigen uses
                        // `self.peek(0).clone()`), so leave the value
                        // on expr_stack; the following Pop drains it.
                        let slot = read_u16(code, ip + 1);
                        if let (Some(&var), Some(&top)) =
                            (int_locals.get(&slot), expr_stack.last())
                        {
                            builder.def_var(var, top);
                            live_int_slots.insert(slot);
                            ip += 3;
                        } else {
                            if !expr_stack.is_empty() {
                                flush_expr_stack_to_stack_view(
                                    &mut builder,
                                    vm_val,
                                    &mut expr_stack,
                                );
                            }
                            // Peek top-of-stack's i64 payload BEFORE
                            // emit_inline_set_local runs. Using the
                            // top (which never changes under peek-
                            // not-pop) avoids the store-then-load
                            // reordering concern of reading stack[slot]
                            // after the write. We pass this SSA value
                            // to both the real store (indirectly via
                            // emit_inline_set_local) and def_var.
                            //
                            // Without this re-sync the Variable's
                            // Cranelift SSA value would be stuck at the
                            // Constant-init def forever — a compile-
                            // time snapshot that never reflects runtime
                            // loop iterations. use_var would return 1
                            // on every iteration, and the loop would
                            // never terminate.
                            let top_payload = int_locals.get(&slot).map(|_| {
                                use cranelift_codegen::ir::MemFlags;
                                let flags = MemFlags::trusted();
                                let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                                let stack_len = emit_load_stack_len(&mut builder, vm_val);
                                let value_size =
                                    builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                                let one = builder.ins().iconst(types::I64, 1);
                                let top_idx = builder.ins().isub(stack_len, one);
                                let top_off = builder.ins().imul(top_idx, value_size);
                                let top_addr = builder.ins().iadd(stack_ptr, top_off);
                                builder.ins().load(
                                    types::I64,
                                    flags,
                                    top_addr,
                                    VALUE_INT_PAYLOAD_OFFSET as i32,
                                )
                            });
                            emit_inline_set_local(
                                &mut builder,
                                &refs,
                                vm_val,
                                slot_offset_val,
                                slot,
                            );
                            if let (Some(&var), Some(payload)) = (int_locals.get(&slot), top_payload) {
                                builder.def_var(var, payload);
                                live_int_slots.insert(slot);
                            }
                            ip += 3;
                        }
                    }

                    // Fallible arithmetic/comparison: call helper, return
                    // early with the helper's error status if non-zero.
                    //
                    // Add/Sub/Mul get an inline int+int fast path via a
                    // direct tag check on the stack — skips the full
                    // dispatch in `binary_add` etc. when both operands
                    // are `Value::Integer`.
                    OpCode::Add | OpCode::Subtract | OpCode::Multiply => {
                        // Virtual Int arithmetic: if both operands are
                        // staged as Int in expr_stack, fold into a
                        // register iadd/isub/imul. No stack memory ops.
                        if expr_stack.len() >= 2 {
                            let rhs = expr_stack.pop().unwrap();
                            let lhs = expr_stack.pop().unwrap();
                            let result = match op {
                                OpCode::Add => builder.ins().iadd(lhs, rhs),
                                OpCode::Subtract => builder.ins().isub(lhs, rhs),
                                OpCode::Multiply => builder.ins().imul(lhs, rhs),
                                _ => unreachable!(),
                            };
                            expr_stack.push(result);
                        } else {
                            if !expr_stack.is_empty() {
                                flush_expr_stack_to_stack_view(
                                    &mut builder,
                                    vm_val,
                                    &mut expr_stack,
                                );
                            }
                            let (arith_op, slow) = match op {
                                OpCode::Add => (IntArithOp::Add, refs.add),
                                OpCode::Subtract => (IntArithOp::Sub, refs.sub),
                                OpCode::Multiply => (IntArithOp::Mul, refs.mul),
                                _ => unreachable!(),
                            };
                            emit_int_fast_arith(&mut builder, &refs, vm_val, arith_op, slow);
                        }
                        ip += 1;
                    }
                    OpCode::Divide | OpCode::Modulo => {
                        let is_mod = matches!(op, OpCode::Modulo);
                        // Virtual path: operands on expr_stack as virt Ints.
                        // Emit register-resident sdiv/srem with zero +
                        // i64::MIN/-1 guards; fall back via slow block that
                        // re-boxes operands to the VM stack and calls the
                        // existing helper.
                        if expr_stack.len() >= 2 {
                            let rhs = expr_stack.pop().unwrap();
                            let lhs = expr_stack.pop().unwrap();
                            let slow_helper = if is_mod { refs.modu } else { refs.div };
                            let result = emit_int_virt_divmod(
                                &mut builder,
                                vm_val,
                                is_mod,
                                lhs,
                                rhs,
                                slow_helper,
                                counters_ptr_opt,
                            );
                            expr_stack.push(result);
                        } else {
                            if !expr_stack.is_empty() {
                                flush_expr_stack_to_stack_view(
                                    &mut builder,
                                    vm_val,
                                    &mut expr_stack,
                                );
                            }
                            let slow = if is_mod { refs.modu } else { refs.div };
                            emit_int_fast_divmod(&mut builder, &refs, vm_val, is_mod, slow);
                        }
                        ip += 1;
                    }
                    OpCode::Less | OpCode::LessEqual | OpCode::Greater | OpCode::GreaterEqual => {
                        use cranelift_codegen::ir::condcodes::IntCC;
                        let (cc, slow_helper) = match op {
                            OpCode::Less => (IntCC::SignedLessThan, refs.lt),
                            OpCode::LessEqual => (IntCC::SignedLessThanOrEqual, refs.le),
                            OpCode::Greater => (IntCC::SignedGreaterThan, refs.gt),
                            OpCode::GreaterEqual => (IntCC::SignedGreaterThanOrEqual, refs.ge),
                            _ => unreachable!(),
                        };

                        // Peephole fusion: if a conditional branch directly
                        // follows the comparison we can collapse the entire
                        // Boolean round-trip + `peek_truthy`/`pop_truthy`
                        // helper call into a single `icmp + brif` on the
                        // integer fast path. A safe peek requires that the
                        // next byte is still inside `code` and isn't itself
                        // a branch target (if it were, some other block
                        // expects the Boolean Value on the stack).
                        let next_is_branch = ip + 3 < code.len();
                        let next_op = if next_is_branch {
                            OpCode::from_byte(code[ip + 1])
                        } else {
                            None
                        };
                        let fuseable = matches!(
                            next_op,
                            Some(OpCode::JumpIfFalse)
                                | Some(OpCode::JumpIfTrue)
                                | Some(OpCode::PopJumpIfFalse)
                        ) && !blocks.contains_key(&(ip + 1));

                        if fuseable {
                            let branch_op = next_op.unwrap();
                            let branch_ip = ip + 1;
                            let off = read_u16(code, branch_ip + 1) as usize;
                            let target_ip = branch_ip + 3 + off;
                            let next_ip = branch_ip + 3;
                            let target_block = blocks[&target_ip];
                            let fall_block = *blocks
                                .entry(next_ip)
                                .or_insert_with(|| builder.create_block());

                            // B2.1e: virtual compare + virtual branch.
                            // When both operands are staged as virt Ints
                            // on expr_stack, we can emit `icmp + brif`
                            // directly with zero memory traffic —
                            // skipping both the stack load of the
                            // operands and the Boolean materialization
                            // that the non-virtual fused path uses for
                            // JumpIfFalse/JumpIfTrue.
                            //
                            // Eligibility for the Bool-less path:
                            // * PopJumpIfFalse — no cleanup Pops at all.
                            // * JumpIfFalse/JumpIfTrue — the cleanup Pops
                            //   at both branch successors must be
                            //   classified in condition_cleanup_pop_ips,
                            //   so we know where to elide them.
                            let virt_eligible = expr_stack.len() >= 2 && match branch_op {
                                OpCode::PopJumpIfFalse => true,
                                OpCode::JumpIfFalse | OpCode::JumpIfTrue => {
                                    slot_types.condition_cleanup_pop_ips.contains(&next_ip)
                                        && slot_types
                                            .condition_cleanup_pop_ips
                                            .contains(&target_ip)
                                }
                                _ => false,
                            };

                            if virt_eligible {
                                let rhs = expr_stack.pop().unwrap();
                                let lhs = expr_stack.pop().unwrap();
                                let pred = builder.ins().icmp(cc, lhs, rhs);
                                let (true_block, false_block) = match branch_op {
                                    // JumpIfFalse/PopJumpIfFalse: branch
                                    // taken when predicate is FALSE.
                                    OpCode::JumpIfFalse | OpCode::PopJumpIfFalse => {
                                        (fall_block, target_block)
                                    }
                                    // JumpIfTrue: branch taken when TRUE.
                                    OpCode::JumpIfTrue => (target_block, fall_block),
                                    _ => unreachable!(),
                                };
                                builder.ins().brif(pred, true_block, &[], false_block, &[]);
                                if matches!(
                                    branch_op,
                                    OpCode::JumpIfFalse | OpCode::JumpIfTrue
                                ) {
                                    // Mark both cleanup Pops as elided —
                                    // the Pop handler will virtual-no-op.
                                    virt_branch_elided_pops.insert(next_ip);
                                    virt_branch_elided_pops.insert(target_ip);
                                }
                            } else {
                                if !expr_stack.is_empty() {
                                    flush_expr_stack_to_stack_view(
                                        &mut builder,
                                        vm_val,
                                        &mut expr_stack,
                                    );
                                }
                                emit_fused_int_cmp_branch(
                                    &mut builder,
                                    &refs,
                                    vm_val,
                                    cc,
                                    slow_helper,
                                    branch_op,
                                    target_block,
                                    fall_block,
                                );
                            }
                            terminated = true;
                            ip = branch_ip + 3;
                        } else {
                            if !expr_stack.is_empty() {
                                flush_expr_stack_to_stack_view(
                                    &mut builder,
                                    vm_val,
                                    &mut expr_stack,
                                );
                            }
                            emit_int_fast_cmp(&mut builder, &refs, vm_val, cc, slow_helper);
                            ip += 1;
                        }
                    }

                    OpCode::Negate => {
                        emit_fallible(&mut builder, refs.negate, vm_val);
                        ip += 1;
                    }

                    // Infallible
                    OpCode::Equal => {
                        emit_int_fast_eq(&mut builder, &refs, vm_val, true);
                        ip += 1;
                    }
                    OpCode::NotEqual => {
                        emit_int_fast_eq(&mut builder, &refs, vm_val, false);
                        ip += 1;
                    }
                    OpCode::Not => {
                        builder.ins().call(refs.not, &[vm_val]);
                        ip += 1;
                    }

                    // Control flow
                    OpCode::Jump => {
                        let off = read_u16(code, ip + 1) as usize;
                        let target_ip = ip + 3 + off;
                        let target = blocks[&target_ip];
                        builder.ins().jump(target, &[]);
                        terminated = true;
                        ip += 3;
                    }
                    OpCode::Loop => {
                        let off = read_u16(code, ip + 1) as usize;
                        let target_ip = (ip + 3) - off;
                        let target = blocks[&target_ip];
                        builder.ins().jump(target, &[]);
                        terminated = true;
                        ip += 3;
                    }
                    OpCode::JumpIfFalse => {
                        let off = read_u16(code, ip + 1) as usize;
                        let target_ip = ip + 3 + off;
                        let next_ip = ip + 3;
                        let target_block = blocks[&target_ip];
                        let fall_block = *blocks
                            .entry(next_ip)
                            .or_insert_with(|| builder.create_block());

                        let call = builder.ins().call(refs.peek_truthy, &[vm_val]);
                        let truthy = builder.inst_results(call)[0];
                        // truthy != 0 -> fall through; else -> target
                        builder
                            .ins()
                            .brif(truthy, fall_block, &[], target_block, &[]);
                        terminated = true;
                        ip += 3;
                    }
                    OpCode::JumpIfTrue => {
                        let off = read_u16(code, ip + 1) as usize;
                        let target_ip = ip + 3 + off;
                        let next_ip = ip + 3;
                        let target_block = blocks[&target_ip];
                        let fall_block = *blocks
                            .entry(next_ip)
                            .or_insert_with(|| builder.create_block());

                        let call = builder.ins().call(refs.peek_truthy, &[vm_val]);
                        let truthy = builder.inst_results(call)[0];
                        // truthy != 0 -> target; else -> fall through
                        builder
                            .ins()
                            .brif(truthy, target_block, &[], fall_block, &[]);
                        terminated = true;
                        ip += 3;
                    }
                    OpCode::PopJumpIfFalse => {
                        let off = read_u16(code, ip + 1) as usize;
                        let target_ip = ip + 3 + off;
                        let next_ip = ip + 3;
                        let target_block = blocks[&target_ip];
                        let fall_block = *blocks
                            .entry(next_ip)
                            .or_insert_with(|| builder.create_block());

                        let call = builder.ins().call(refs.pop_truthy, &[vm_val]);
                        let truthy = builder.inst_results(call)[0];
                        builder
                            .ins()
                            .brif(truthy, fall_block, &[], target_block, &[]);
                        terminated = true;
                        ip += 3;
                    }

                    // ── Globals ─────────────────────────────────────
                    OpCode::GetGlobal => {
                        let idx = read_u16(code, ip + 1);
                        let cache_ptr = global_cache_ptrs[ic_ix];
                        ic_ix += 1;
                        let idx_val = builder.ins().iconst(types::I32, idx as i64);
                        let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);
                        // Inline the cache-hit path: version check, then
                        // tag-gated Rc-strongcount bump + 40-byte copy to
                        // stack top. Miss falls through to the helper.
                        use cranelift_codegen::ir::MemFlags;
                        use cranelift_codegen::ir::condcodes::IntCC;
                        let flags = MemFlags::trusted();
                        let cache_ver = builder
                            .ins()
                            .load(types::I64, flags, cache_val, 0);
                        let vm_ver = builder.ins().load(
                            types::I64,
                            flags,
                            vm_val,
                            VM::globals_version_offset() as i32,
                        );
                        let is_hit = builder.ins().icmp(IntCC::Equal, cache_ver, vm_ver);
                        let hit_block = builder.create_block();
                        let miss_block = builder.create_block();
                        let cont_block = builder.create_block();
                        builder
                            .ins()
                            .brif(is_hit, hit_block, &[], miss_block, &[]);

                        // ── Hit ──
                        builder.switch_to_block(hit_block);
                        // cache.value sits at byte offset 8 of the cache.
                        let cache_value_ptr = builder.ins().iadd_imm(cache_val, 8);
                        // If the value is heap-backed (tag > 6), bump the
                        // Rc strong count at the payload pointer (RcBox
                        // offset 0; pinned by rc_strong_count_lives_at_
                        // rcbox_offset_zero test in vm/value.rs).
                        let tag = builder.ins().load(types::I8, flags, cache_value_ptr, 0);
                        let six = builder.ins().iconst(types::I8, 6);
                        let is_heap = builder.ins().icmp(IntCC::UnsignedGreaterThan, tag, six);
                        let bump_block = builder.create_block();
                        let post_bump = builder.create_block();
                        builder.ins().brif(is_heap, bump_block, &[], post_bump, &[]);

                        builder.switch_to_block(bump_block);
                        let rc_ptr = builder.ins().load(
                            types::I64,
                            flags,
                            cache_value_ptr,
                            VALUE_INT_PAYLOAD_OFFSET as i32,
                        );
                        let strong = builder.ins().load(types::I64, flags, rc_ptr, 0);
                        let strong_new = builder.ins().iadd_imm(strong, 1);
                        builder.ins().store(flags, strong_new, rc_ptr, 0);
                        builder.ins().jump(post_bump, &[]);

                        builder.switch_to_block(post_bump);
                        // Copy the 40-byte Value from cache to stack[top].
                        let stack_ptr_v = emit_load_stack_ptr(&mut builder, vm_val);
                        let top_v = emit_load_stack_len(&mut builder, vm_val);
                        let vsize = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                        let off = builder.ins().imul(top_v, vsize);
                        let dst = builder.ins().iadd(stack_ptr_v, off);
                        emit_copy_value(&mut builder, cache_value_ptr, dst);
                        let one_v = builder.ins().iconst(types::I64, 1);
                        let new_top = builder.ins().iadd(top_v, one_v);
                        builder.ins().store(
                            flags,
                            new_top,
                            vm_val,
                            vm_stack_view_len_offset(),
                        );
                        builder.ins().jump(cont_block, &[]);

                        // ── Miss ──
                        builder.switch_to_block(miss_block);
                        let call = builder
                            .ins()
                            .call(refs.get_global_ic, &[vm_val, cache_val, idx_val]);
                        let status = builder.inst_results(call)[0];
                        let err_block = builder.create_block();
                        builder
                            .ins()
                            .brif(status, err_block, &[], cont_block, &[]);
                        builder.switch_to_block(err_block);
                        builder.ins().return_(&[status]);

                        builder.switch_to_block(cont_block);
                        ip += 3;
                    }
                    OpCode::SetGlobal => {
                        let idx = read_u16(code, ip + 1);
                        let idx_val = builder.ins().iconst(types::I32, idx as i64);
                        let call = builder.ins().call(refs.set_global, &[vm_val, idx_val]);
                        let status = builder.inst_results(call)[0];
                        emit_early_exit_on_err(&mut builder, status);
                        ip += 3;
                    }
                    OpCode::DefineGlobal => {
                        let idx = read_u16(code, ip + 1);
                        let idx_val = builder.ins().iconst(types::I32, idx as i64);
                        let call = builder.ins().call(refs.define_global, &[vm_val, idx_val]);
                        let status = builder.inst_results(call)[0];
                        emit_early_exit_on_err(&mut builder, status);
                        ip += 3;
                    }
                    OpCode::DefineGlobalTyped => {
                        let name_idx = read_u16(code, ip + 1);
                        let mutable = code[ip + 3];
                        let type_idx = read_u16(code, ip + 4);
                        let name_val = builder.ins().iconst(types::I32, name_idx as i64);
                        let mut_val = builder.ins().iconst(types::I32, mutable as i64);
                        let ty_val = builder.ins().iconst(types::I32, type_idx as i64);
                        let call = builder.ins().call(
                            refs.define_global_typed,
                            &[vm_val, name_val, mut_val, ty_val],
                        );
                        let status = builder.inst_results(call)[0];
                        emit_early_exit_on_err(&mut builder, status);
                        ip += 6;
                    }

                    // ── Upvalues ────────────────────────────────────
                    OpCode::GetUpvalue => {
                        let idx = read_u16(code, ip + 1);
                        let idx_val = builder.ins().iconst(types::I32, idx as i64);
                        builder.ins().call(refs.get_upvalue, &[vm_val, idx_val]);
                        ip += 3;
                    }
                    OpCode::SetUpvalue => {
                        let idx = read_u16(code, ip + 1);
                        let idx_val = builder.ins().iconst(types::I32, idx as i64);
                        builder.ins().call(refs.set_upvalue, &[vm_val, idx_val]);
                        ip += 3;
                    }
                    OpCode::CloseUpvalue => {
                        builder.ins().call(refs.close_upvalue, &[vm_val]);
                        ip += 1;
                    }

                    // ── Closure (variable length) ────────────────────
                    OpCode::Closure => {
                        let fn_idx = read_u16(code, ip + 1);
                        let upvalue_count = match &chunk.constants[fn_idx as usize] {
                            crate::vm::value::Value::Closure(t) => {
                                t.function.upvalue_count as usize
                            }
                            _ => return Err(()),
                        };
                        let descriptors_offset = ip + 3;
                        let fn_val = builder.ins().iconst(types::I32, fn_idx as i64);
                        let off_val = builder.ins().iconst(types::I32, descriptors_offset as i64);
                        let call = builder
                            .ins()
                            .call(refs.op_closure, &[vm_val, fn_val, off_val]);
                        let status = builder.inst_results(call)[0];
                        emit_early_exit_on_err(&mut builder, status);
                        ip = descriptors_offset + 3 * upvalue_count;
                    }

                    OpCode::Call => {
                        let arg_count = code[ip + 1];
                        let cache_ptr = call_cache_ptrs[call_ic_ix];
                        call_ic_ix += 1;

                        // Inline IR guard backed by `vm.stack_view`:
                        // 1. Load stack base ptr + current len via direct
                        //    memory reads (no FFI).
                        // 2. Locate the callee Value at stack[len-1-ac].
                        // 3. Check its tag byte equals `VALUE_TAG_CLOSURE`.
                        // 4. Load its Rc pointer (offset 8) and compare to
                        //    `cache.closure_raw`.
                        // 5. On match → push a JIT frame and indirect-call
                        //    the cached thunk. On miss → call
                        //    `jit_op_call_miss` (full fallback + populate
                        //    cache).
                        use cranelift_codegen::ir::MemFlags;
                        use cranelift_codegen::ir::condcodes::IntCC;

                        let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                        let stack_len = emit_load_stack_len(&mut builder, vm_val);

                        let value_size =
                            builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                        let offset_from_top =
                            builder.ins().iconst(types::I64, (arg_count as i64 + 1) as i64);
                        let callee_slot = builder.ins().isub(stack_len, offset_from_top);
                        let callee_off = builder.ins().imul(callee_slot, value_size);
                        let callee_ptr = builder.ins().iadd(stack_ptr, callee_off);

                        let flags = MemFlags::trusted();
                        let tag = builder.ins().load(types::I8, flags, callee_ptr, 0);
                        let closure_tag =
                            builder.ins().iconst(types::I8, VALUE_TAG_CLOSURE as i64);
                        let is_closure =
                            builder.ins().icmp(IntCC::Equal, tag, closure_tag);
                        // Used in both successor paths — define in entry.
                        let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);

                        let check_rc_block = builder.create_block();
                        let hit_block = builder.create_block();
                        let miss_block = builder.create_block();
                        let ok_block = builder.create_block();
                        let err_block = builder.create_block();
                        builder.append_block_param(err_block, types::I32);

                        builder.ins().brif(
                            is_closure,
                            check_rc_block,
                            &[],
                            miss_block,
                            &[],
                        );

                        // Tag matched → compare Rc pointer with cache.
                        builder.switch_to_block(check_rc_block);
                        let curr_rc = builder.ins().load(
                            ptr_ty,
                            flags,
                            callee_ptr,
                            VALUE_INT_PAYLOAD_OFFSET as i32,
                        );
                        let cached_rc = builder.ins().load(
                            ptr_ty,
                            flags,
                            cache_val,
                            CallCacheEntry::OFFSET_CLOSURE_RAW,
                        );
                        let rc_matches =
                            builder.ins().icmp(IntCC::Equal, curr_rc, cached_rc);
                        builder
                            .ins()
                            .brif(rc_matches, hit_block, &[], miss_block, &[]);

                        // Hit — cached closure matches.
                        builder.switch_to_block(hit_block);
                        let jit_frames_ptr = builder.ins().load(
                            ptr_ty,
                            flags,
                            vm_val,
                            vm_jit_frame_view_ptr_offset(),
                        );
                        let jit_frames_len = builder.ins().load(
                            types::I64,
                            flags,
                            vm_val,
                            vm_jit_frame_view_len_offset(),
                        );
                        let frame_size =
                            builder.ins().iconst(types::I64, std::mem::size_of::<JitFrame>() as i64);
                        let frame_off = builder.ins().imul(jit_frames_len, frame_size);
                        let new_frame_ptr = builder.ins().iadd(jit_frames_ptr, frame_off);
                        let caller_frame_ptr = emit_load_top_jit_frame_ptr(&mut builder, vm_val);
                        let module_globals = builder.ins().load(
                            ptr_ty,
                            flags,
                            caller_frame_ptr,
                            JitFrame::OFFSET_MODULE_GLOBALS,
                        );
                        let thunk_raw = builder.ins().load(
                            ptr_ty,
                            flags,
                            cache_val,
                            CallCacheEntry::OFFSET_THUNK_RAW,
                        );
                        let line_val = builder.ins().iconst(types::I32, line as i64);
                        // `curr_rc` is the raw `NonNull<RcBox<T>>` pointer
                        // loaded from a `Value::Closure` payload; adjust by
                        // `RC_VALUE_OFFSET` so `JitFrame.closure_raw`
                        // points to the `ObjClosure` itself — matching what
                        // `Rc::as_ptr(&closure)` returns on the Rust-side
                        // push in `call_closure_fast_path`. Without this,
                        // `active_closure()`'s `&*closure_raw` would
                        // dereference into the RcBox refcount header and
                        // produce garbage fields (→ segfault on upvalue
                        // / field / constant access inside the callee).
                        let closure_t_ptr = builder
                            .ins()
                            .iadd_imm(curr_rc, RC_VALUE_OFFSET as i64);
                        builder.ins().store(
                            flags,
                            closure_t_ptr,
                            new_frame_ptr,
                            JitFrame::OFFSET_CLOSURE_RAW,
                        );
                        builder.ins().store(
                            flags,
                            callee_slot,
                            new_frame_ptr,
                            JitFrame::OFFSET_SLOT_OFFSET,
                        );
                        builder.ins().store(
                            flags,
                            module_globals,
                            new_frame_ptr,
                            JitFrame::OFFSET_MODULE_GLOBALS,
                        );
                        builder.ins().store(
                            flags,
                            line_val,
                            new_frame_ptr,
                            JitFrame::OFFSET_LINE,
                        );
                        let one64 = builder.ins().iconst(types::I64, 1);
                        let new_len = builder.ins().iadd(jit_frames_len, one64);
                        builder.ins().store(
                            flags,
                            new_len,
                            vm_val,
                            vm_jit_frame_view_len_offset(),
                        );
                        let hit_call =
                            builder.ins().call_indirect(thunk_sig, thunk_raw, &[vm_val]);
                        let hit_status = builder.inst_results(hit_call)[0];
                        let restore_err_block = builder.create_block();
                        builder.append_block_param(restore_err_block, types::I32);
                        builder.ins().brif(
                            hit_status,
                            restore_err_block,
                            &[hit_status],
                            ok_block,
                            &[],
                        );
                        builder.switch_to_block(restore_err_block);
                        let err_status = builder.block_params(restore_err_block)[0];
                        builder.ins().store(
                            flags,
                            jit_frames_len,
                            vm_val,
                            vm_jit_frame_view_len_offset(),
                        );
                        builder.ins().jump(err_block, &[err_status]);

                        // Miss — fallback + populate cache.
                        builder.switch_to_block(miss_block);
                        let ac_val =
                            builder.ins().iconst(types::I32, arg_count as i64);
                        let miss_call = builder.ins().call(
                            refs.op_call_miss,
                            &[vm_val, ac_val, cache_val],
                        );
                        let miss_status = builder.inst_results(miss_call)[0];
                        builder.ins().brif(
                            miss_status,
                            err_block,
                            &[miss_status],
                            ok_block,
                            &[],
                        );

                        // Error exit.
                        builder.switch_to_block(err_block);
                        let err_status = builder.block_params(err_block)[0];
                        builder.ins().return_(&[err_status]);

                        builder.switch_to_block(ok_block);
                        ip += 2;
                    }

                    // ── Struct ops ──────────────────────────────────
                    OpCode::StructDef => {
                        let idx = read_u16(code, ip + 1);
                        let idx_val = builder.ins().iconst(types::I32, idx as i64);
                        builder.ins().call(refs.op_struct_def, &[vm_val, idx_val]);
                        ip += 3;
                    }
                    OpCode::StructLiteral => {
                        let name_idx = read_u16(code, ip + 1);
                        let field_count = code[ip + 3];
                        let name_val = builder.ins().iconst(types::I32, name_idx as i64);
                        let fc_val = builder.ins().iconst(types::I32, field_count as i64);
                        let call = builder
                            .ins()
                            .call(refs.op_struct_literal, &[vm_val, name_val, fc_val]);
                        let status = builder.inst_results(call)[0];
                        emit_early_exit_on_err(&mut builder, status);
                        ip += 4;
                    }
                    OpCode::GetField => {
                        let idx = read_u16(code, ip + 1);
                        let idx_val = builder.ins().iconst(types::I32, idx as i64);
                        let cache_ptr = field_cache_ptrs[field_ic_ix];
                        field_ic_ix += 1;
                        let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);
                        let ok_block = builder.create_block();
                        let err_block = builder.create_block();
                        builder.append_block_param(err_block, types::I32);
                        let call = builder
                            .ins()
                            .call(refs.op_get_field_ic_miss, &[vm_val, idx_val, cache_val]);
                        let status = builder.inst_results(call)[0];
                        builder
                            .ins()
                            .brif(status, err_block, &[status], ok_block, &[]);

                        builder.switch_to_block(err_block);
                        let err_status = builder.block_params(err_block)[0];
                        builder.ins().return_(&[err_status]);
                        builder.switch_to_block(ok_block);
                        ip += 3;
                    }
                    OpCode::SetField => {
                        let idx = read_u16(code, ip + 1);
                        let idx_val = builder.ins().iconst(types::I32, idx as i64);
                        let cache_ptr = field_cache_ptrs[field_ic_ix];
                        field_ic_ix += 1;
                        let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);
                        let ok_block = builder.create_block();
                        let err_block = builder.create_block();
                        builder.append_block_param(err_block, types::I32);
                        let call = builder
                            .ins()
                            .call(refs.op_set_field_ic_miss, &[vm_val, idx_val, cache_val]);
                        let status = builder.inst_results(call)[0];
                        builder
                            .ins()
                            .brif(status, err_block, &[status], ok_block, &[]);

                        builder.switch_to_block(err_block);
                        let err_status = builder.block_params(err_block)[0];
                        builder.ins().return_(&[err_status]);
                        builder.switch_to_block(ok_block);
                        ip += 3;
                    }
                    OpCode::DefineMethod => {
                        let struct_name_idx = read_u16(code, ip + 1);
                        let method_count = code[ip + 3];
                        let sn_val = builder.ins().iconst(types::I32, struct_name_idx as i64);
                        let mc_val = builder.ins().iconst(types::I32, method_count as i64);
                        let call = builder
                            .ins()
                            .call(refs.op_define_method, &[vm_val, sn_val, mc_val]);
                        let status = builder.inst_results(call)[0];
                        emit_early_exit_on_err(&mut builder, status);
                        ip += 4;
                    }
                    OpCode::MethodCall => {
                        let method_idx = read_u16(code, ip + 1);
                        let arg_count = code[ip + 3];
                        let cache_ptr = method_cache_ptrs[method_ic_ix];
                        method_ic_ix += 1;

                        // Inline IR fast path for method calls with ≤1
                        // explicit arg (i.e. getter- and single-arg
                        // setter-style methods — the vast majority of
                        // hot struct_method work). Correctness hinges
                        // on three things handled below:
                        //   1. The synthesized `Value::Closure` is a new
                        //      live Rc reference — we bump the method
                        //      closure's `RcBox` strong count before
                        //      `emit_write_closure_value` stamps it.
                        //      The `emit_copy_value` calls are moves
                        //      (source slots are overwritten), so no
                        //      bump is needed for the receiver/arg.
                        //   2. After raw stores past `Vec::len`, we call
                        //      `jit_stack_commit_len` to sync the
                        //      `Vec<Value>` backing store's length so
                        //      bounds-checked helpers see the new slots.
                        //   3. On error, `restore_err_block` rolls back
                        //      the JitFrame we pushed (pre-call len).
                        if arg_count <= 1 {
                            use cranelift_codegen::ir::MemFlags;
                            use cranelift_codegen::ir::condcodes::IntCC;

                            let mi_val = builder.ins().iconst(types::I32, method_idx as i64);
                            let ac_val = builder.ins().iconst(types::I32, arg_count as i64);
                            let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);
                            let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                            let stack_len = emit_load_stack_len(&mut builder, vm_val);
                            let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                            let offset_from_top =
                                builder.ins().iconst(types::I64, (arg_count as i64 + 1) as i64);
                            let receiver_slot = builder.ins().isub(stack_len, offset_from_top);
                            let receiver_off = builder.ins().imul(receiver_slot, value_size);
                            let receiver_ptr = builder.ins().iadd(stack_ptr, receiver_off);
                            let flags = MemFlags::trusted();

                            let check_def_block = builder.create_block();
                            let hit_block = builder.create_block();
                            let miss_block = builder.create_block();
                            let ok_block = builder.create_block();
                            let err_block = builder.create_block();
                            builder.append_block_param(err_block, types::I32);

                            let tag = builder.ins().load(types::I8, flags, receiver_ptr, 0);
                            let struct_tag =
                                builder.ins().iconst(types::I8, VALUE_TAG_STRUCT_INSTANCE as i64);
                            let is_struct =
                                builder.ins().icmp(IntCC::Equal, tag, struct_tag);
                            builder
                                .ins()
                                .brif(is_struct, check_def_block, &[], miss_block, &[]);

                            builder.switch_to_block(check_def_block);
                            let receiver_raw = builder.ins().load(
                                ptr_ty,
                                flags,
                                receiver_ptr,
                                VALUE_INT_PAYLOAD_OFFSET as i32,
                            );
                            let inst_ptr = builder.ins().iadd_imm(receiver_raw, RC_VALUE_OFFSET as i64);
                            let inst_def_raw = builder.ins().load(
                                ptr_ty,
                                flags,
                                inst_ptr,
                                STRUCT_INSTANCE_DEF_OFFSET as i32,
                            );
                            let cached_def_raw = builder.ins().load(
                                ptr_ty,
                                flags,
                                cache_val,
                                MethodCacheEntry::OFFSET_STRUCT_DEF_RAW,
                            );
                            let def_matches =
                                builder.ins().icmp(IntCC::Equal, inst_def_raw, cached_def_raw);

                            // Inline-expansion path: if the cache's
                            // `inline_kind` is set, skip the JitFrame push
                            // + thunk dispatch and emit the method body's
                            // peephole operation directly. The full path
                            // (hit_block) remains for non-inline callees
                            // and as a safety fallback.
                            let inline_dispatch_block = builder.create_block();
                            builder.ins().brif(
                                def_matches,
                                inline_dispatch_block,
                                &[],
                                miss_block,
                                &[],
                            );

                            // Emit the inline expansion for whichever arity
                            // this call site has. The `inline_kind` in the
                            // cache is only populated for arity that
                            // matches (FieldAddConst → 0 args,
                            // FieldAddLocal → 1 arg) so a stale or
                            // mismatched kind can't fire here.
                            builder.switch_to_block(inline_dispatch_block);
                            let inline_kind_byte = builder.ins().load(
                                types::I8,
                                flags,
                                cache_val,
                                MethodCacheEntry::OFFSET_INLINE_KIND,
                            );
                            let none_kind = builder.ins().iconst(types::I8, 0);
                            let is_none_kind = builder
                                .ins()
                                .icmp(IntCC::Equal, inline_kind_byte, none_kind);

                            let target_kind: i64 = match arg_count {
                                0 => 1, // FieldAddConst
                                1 => 2, // FieldAddLocal
                                _ => 0, // no inline for other arities
                            };
                            let inline_body_block = builder.create_block();
                            // On inline_kind == 0 go to the full path;
                            // otherwise check the expected kind for this
                            // arity and fall through to the inline body or
                            // the full path on mismatch.
                            let kind_ok_block = builder.create_block();
                            builder.ins().brif(
                                is_none_kind,
                                hit_block,
                                &[],
                                kind_ok_block,
                                &[],
                            );

                            builder.switch_to_block(kind_ok_block);
                            let expected_kind =
                                builder.ins().iconst(types::I8, target_kind);
                            let matches_expected = builder
                                .ins()
                                .icmp(IntCC::Equal, inline_kind_byte, expected_kind);
                            builder.ins().brif(
                                matches_expected,
                                inline_body_block,
                                &[],
                                hit_block,
                                &[],
                            );

                            // ── Inline body ─────────────────────────────
                            builder.switch_to_block(inline_body_block);
                            // Receiver's RcBox pointer is `receiver_raw`
                            // (the Value payload at offset 8, == Rc bit
                            // pattern). `inst_ptr` = RcBox + RC_VALUE_OFFSET
                            // — already computed above.
                            // Guard: receiver Rc strong count > 1 so that
                            // decrementing it in-place doesn't drop the
                            // instance (the full path handles drop).
                            let receiver_rcbox = receiver_raw;
                            let strong =
                                builder.ins().load(types::I64, flags, receiver_rcbox, 0);
                            let one_i64 = builder.ins().iconst(types::I64, 1);
                            let strong_is_one = builder
                                .ins()
                                .icmp(IntCC::Equal, strong, one_i64);
                            let after_strong_block = builder.create_block();
                            builder.ins().brif(
                                strong_is_one,
                                miss_block,
                                &[],
                                after_strong_block,
                                &[],
                            );
                            builder.switch_to_block(after_strong_block);

                            // Compute the field slot pointer from the
                            // cache's layout index.
                            let fields_ptr = builder.ins().load(
                                ptr_ty,
                                flags,
                                inst_ptr,
                                STRUCT_INSTANCE_FIELDS_PTR_OFFSET as i32,
                            );
                            let field_idx_val = builder.ins().load(
                                types::I32,
                                flags,
                                cache_val,
                                MethodCacheEntry::OFFSET_INLINE_FIELD_INDEX,
                            );
                            let field_idx_i64 =
                                builder.ins().uextend(types::I64, field_idx_val);
                            let field_slot_off =
                                builder.ins().imul(field_idx_i64, value_size);
                            let field_slot_ptr =
                                builder.ins().iadd(fields_ptr, field_slot_off);

                            // Guard: current field value is Integer.
                            let field_tag =
                                builder.ins().load(types::I8, flags, field_slot_ptr, 0);
                            let int_tag = builder
                                .ins()
                                .iconst(types::I8, VALUE_TAG_INTEGER as i64);
                            let field_is_int = builder
                                .ins()
                                .icmp(IntCC::Equal, field_tag, int_tag);
                            let after_field_int_block = builder.create_block();
                            builder.ins().brif(
                                field_is_int,
                                after_field_int_block,
                                &[],
                                miss_block,
                                &[],
                            );
                            builder.switch_to_block(after_field_int_block);

                            // Load the operand (addend for Const, arg for
                            // Local). For Local, also tag-guard the arg.
                            let rhs = if arg_count == 0 {
                                builder.ins().load(
                                    types::I64,
                                    flags,
                                    cache_val,
                                    MethodCacheEntry::OFFSET_INLINE_ADDEND,
                                )
                            } else {
                                // Arg sits at stack[len - 1].
                                let arg_slot =
                                    builder.ins().isub(stack_len, one_i64);
                                let arg_off = builder.ins().imul(arg_slot, value_size);
                                let arg_ptr = builder.ins().iadd(stack_ptr, arg_off);
                                let arg_tag =
                                    builder.ins().load(types::I8, flags, arg_ptr, 0);
                                let arg_is_int = builder
                                    .ins()
                                    .icmp(IntCC::Equal, arg_tag, int_tag);
                                let after_arg_int_block = builder.create_block();
                                builder.ins().brif(
                                    arg_is_int,
                                    after_arg_int_block,
                                    &[],
                                    miss_block,
                                    &[],
                                );
                                builder.switch_to_block(after_arg_int_block);
                                builder.ins().load(
                                    types::I64,
                                    flags,
                                    arg_ptr,
                                    VALUE_INT_PAYLOAD_OFFSET as i32,
                                )
                            };

                            // Update the field payload in place. Tag is
                            // already Integer so we don't rewrite it.
                            let cur = builder.ins().load(
                                types::I64,
                                flags,
                                field_slot_ptr,
                                VALUE_INT_PAYLOAD_OFFSET as i32,
                            );
                            let sum = builder.ins().iadd(cur, rhs);
                            builder.ins().store(
                                flags,
                                sum,
                                field_slot_ptr,
                                VALUE_INT_PAYLOAD_OFFSET as i32,
                            );

                            // Decrement receiver Rc strong count (balances
                            // the bump that happened when the receiver was
                            // pushed onto the stack). Guarded above so
                            // strong > 1 here, which means we can
                            // decrement without having to run Drop.
                            let strong_dec =
                                builder.ins().iadd_imm(strong, -1);
                            builder.ins().store(flags, strong_dec, receiver_rcbox, 0);

                            // Overwrite the receiver slot with Value::None
                            // (tag-only write; the stale payload bytes are
                            // irrelevant because `Value::None`'s Drop is a
                            // no-op and the JIT only reads the tag to
                            // decide what to do with a slot).
                            let none_tag = builder
                                .ins()
                                .iconst(types::I8, VALUE_TAG_NONE as i64);
                            builder.ins().store(flags, none_tag, receiver_ptr, 0);

                            // For the Local variant, pop the arg slot —
                            // its contents were guarded to Integer so
                            // there's no Drop to run.
                            if arg_count == 1 {
                                emit_inline_stack_pop_one(&mut builder, vm_val);
                            }
                            builder.ins().jump(ok_block, &[]);

                            // ── Fall-through to full path below ────────
                            builder.switch_to_block(hit_block);
                            // `closure_raw_box` is the raw `NonNull<RcBox<T>>`
                            // bit pattern (same layout as the payload of
                            // a `Value::Closure`). `closure_raw_t` is the
                            // `*const ObjClosure` for direct-field access
                            // (matches `Rc::as_ptr` / `active_closure()`).
                            // We need BOTH: the RcBox pointer for writing
                            // to the stack as a live `Value::Closure`, and
                            // the T pointer for `JitFrame.closure_raw`.
                            let closure_raw_box = builder.ins().load(
                                ptr_ty,
                                flags,
                                cache_val,
                                MethodCacheEntry::OFFSET_CLOSURE_RAW,
                            );
                            // Bump Rc<ObjClosure> strong count. The
                            // `emit_write_closure_value` below stamps a
                            // new live `Value::Closure` whose payload is
                            // this same `RcBox` pointer. When stack
                            // teardown (normal Return or error path)
                            // eventually drops that Value, the Drop impl
                            // will decrement `strong`. We increment here
                            // so the pair balances — otherwise the count
                            // underflows, the RcBox gets freed while the
                            // cache's `_keeper: Rc<ObjClosure>` still
                            // points at it, and the next call through
                            // this cache site reads freed memory.
                            //
                            // `Rc` is `!Send`/`!Sync` and the VM is
                            // single-threaded, so a non-atomic load/add/
                            // store is equivalent to what `Rc::clone`
                            // itself does. `Cell<usize>` is
                            // repr-transparent over `usize`, and `RcBox`
                            // layout is `{ strong@0, weak@8, value@16
                            // (= RC_VALUE_OFFSET) }` — so `strong` is at
                            // RcBox offset 0. This invariant is pinned
                            // by `rc_strong_count_lives_at_rcbox_offset_zero`
                            // in `core/src/vm/value.rs`.
                            let strong =
                                builder.ins().load(types::I64, flags, closure_raw_box, 0);
                            let strong_inc = builder.ins().iadd_imm(strong, 1);
                            builder.ins().store(flags, strong_inc, closure_raw_box, 0);
                            let closure_raw_t = builder
                                .ins()
                                .iadd_imm(closure_raw_box, RC_VALUE_OFFSET as i64);
                            let thunk_raw = builder.ins().load(
                                ptr_ty,
                                flags,
                                cache_val,
                                MethodCacheEntry::OFFSET_THUNK_RAW,
                            );
                            let line_val = builder.ins().iconst(types::I32, line as i64);

                            if arg_count == 0 {
                                let dst_ptr = builder.ins().iadd(receiver_ptr, value_size);
                                emit_copy_value(&mut builder, receiver_ptr, dst_ptr);
                            } else {
                                let arg_ptr = builder.ins().iadd(receiver_ptr, value_size);
                                let dst_arg_ptr = builder.ins().iadd(arg_ptr, value_size);
                                emit_copy_value(&mut builder, arg_ptr, dst_arg_ptr);
                                emit_copy_value(&mut builder, receiver_ptr, arg_ptr);
                            }
                            // Write a fresh `Value::Closure` at the
                            // receiver slot. The payload is the raw
                            // RcBox pointer — same as what pops out of a
                            // normal `Value::Closure`.
                            emit_write_closure_value(&mut builder, receiver_ptr, closure_raw_box);
                            let new_stack_len = builder.ins().iadd_imm(stack_len, 1);
                            // Commit the logical stack length to the
                            // backing `Vec<Value>` so bounds-checked
                            // helpers (e.g. `jit_get_local`'s slow path
                            // using `self.stack[idx]`) see the new slots.
                            // A direct store to `stack_view.len` alone
                            // desyncs `Vec::len` and causes OOB panics
                            // on the next non-inline local access.
                            builder
                                .ins()
                                .call(refs.stack_commit_len, &[vm_val, new_stack_len]);

                            let jit_frames_ptr = builder.ins().load(
                                ptr_ty,
                                flags,
                                vm_val,
                                vm_jit_frame_view_ptr_offset(),
                            );
                            let jit_frames_len = builder.ins().load(
                                types::I64,
                                flags,
                                vm_val,
                                vm_jit_frame_view_len_offset(),
                            );
                            let frame_size =
                                builder.ins().iconst(types::I64, std::mem::size_of::<JitFrame>() as i64);
                            let frame_off = builder.ins().imul(jit_frames_len, frame_size);
                            let new_frame_ptr = builder.ins().iadd(jit_frames_ptr, frame_off);
                            let caller_frame_ptr = emit_load_top_jit_frame_ptr(&mut builder, vm_val);
                            let module_globals = builder.ins().load(
                                ptr_ty,
                                flags,
                                caller_frame_ptr,
                                JitFrame::OFFSET_MODULE_GLOBALS,
                            );
                            // The JIT frame stores a *const ObjClosure* —
                            // i.e., the T pointer, not the RcBox pointer.
                            builder.ins().store(
                                flags,
                                closure_raw_t,
                                new_frame_ptr,
                                JitFrame::OFFSET_CLOSURE_RAW,
                            );
                            builder.ins().store(
                                flags,
                                receiver_slot,
                                new_frame_ptr,
                                JitFrame::OFFSET_SLOT_OFFSET,
                            );
                            builder.ins().store(
                                flags,
                                module_globals,
                                new_frame_ptr,
                                JitFrame::OFFSET_MODULE_GLOBALS,
                            );
                            builder.ins().store(
                                flags,
                                line_val,
                                new_frame_ptr,
                                JitFrame::OFFSET_LINE,
                            );
                            let new_jit_len = builder.ins().iadd_imm(jit_frames_len, 1);
                            builder.ins().store(
                                flags,
                                new_jit_len,
                                vm_val,
                                vm_jit_frame_view_len_offset(),
                            );
                            let hit_call =
                                builder.ins().call_indirect(thunk_sig, thunk_raw, &[vm_val]);
                            let hit_status = builder.inst_results(hit_call)[0];
                            let restore_err_block = builder.create_block();
                            builder.append_block_param(restore_err_block, types::I32);
                            builder.ins().brif(
                                hit_status,
                                restore_err_block,
                                &[hit_status],
                                ok_block,
                                &[],
                            );
                            builder.switch_to_block(restore_err_block);
                            let err_status = builder.block_params(restore_err_block)[0];
                            // Roll back the JitFrame we pushed so the
                            // outer thunk's `jit_frame_top()` is correct
                            // on error propagation. Mirrors the Call IR
                            // guard's restore_err_block handling.
                            // `stack_view.len` / `Vec::len` are
                            // deliberately NOT rolled back — the
                            // synthetic `Value::Closure` and moved-
                            // receiver Values are bit-valid; they will
                            // Drop correctly during outer teardown and
                            // the strong-count bump above balances the
                            // Drop of the synthetic `Value::Closure`.
                            builder.ins().store(
                                flags,
                                jit_frames_len,
                                vm_val,
                                vm_jit_frame_view_len_offset(),
                            );
                            builder.ins().jump(err_block, &[err_status]);

                            builder.switch_to_block(miss_block);
                            let call = builder.ins().call(
                                refs.op_method_call_ic,
                                &[vm_val, mi_val, ac_val, cache_val],
                            );
                            let status = builder.inst_results(call)[0];
                            builder
                                .ins()
                                .brif(status, err_block, &[status], ok_block, &[]);

                            builder.switch_to_block(err_block);
                            let err_status = builder.block_params(err_block)[0];
                            builder.ins().return_(&[err_status]);

                            builder.switch_to_block(ok_block);
                        } else {
                            let mi_val = builder.ins().iconst(types::I32, method_idx as i64);
                            let ac_val = builder.ins().iconst(types::I32, arg_count as i64);
                            let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);
                            let call = builder.ins().call(
                                refs.op_method_call_ic,
                                &[vm_val, mi_val, ac_val, cache_val],
                            );
                            let status = builder.inst_results(call)[0];
                            emit_early_exit_on_err(&mut builder, status);
                        }
                        ip += 4;
                    }

                    OpCode::Return => {
                        builder.ins().call(refs.op_return, &[vm_val]);
                        let zero = builder.ins().iconst(types::I32, 0);
                        builder.ins().return_(&[zero]);
                        terminated = true;
                        ip += 1;
                    }

                    // Scan should have rejected these — defensive.
                    _ => return Err(()),
                }
            }

            // If the last block fell off the end without terminating,
            // something's wrong with the bytecode (every path should end
            // in Return). Defensive: emit a runtime bailout.
            if !terminated {
                let two = builder.ins().iconst(types::I32, 2);
                builder.ins().return_(&[two]);
            }

            builder.seal_all_blocks();
            builder.finalize();
        }

        self.module
            .define_function(thunk_id, &mut self.ctx)
            .map_err(|e| {
                if std::env::var("OXIGEN_JIT_DEBUG").is_ok() {
                    eprintln!("[jit] define_function failed: {:?}", e);
                }
            })?;
        self.module.clear_context(&mut self.ctx);

        // A2: if the function is specialized-eligible, emit the
        // specialized entry body. v1 uses a "forward trampoline" —
        // the body boxes each i64 arg as Value::Integer onto the VM
        // stack, pushes a JitFrame matching the generic convention,
        // calls the generic thunk directly, unboxes the top-of-stack
        // result, and returns (i64, u32). Correct by construction
        // (generic body unchanged) and gives A3 a specialized
        // dispatch target. A later stage can upgrade this to a full
        // register-native body for more per-call savings.
        if let (Some(spec_id), Some(spec_seq_id)) = (spec_thunk_id, spec_seq_id) {
            self.emit_specialized_trampoline(
                func,
                thunk_id,
                spec_id,
                spec_seq_id,
                ptr_ty,
            )?;
        }

        self.module.finalize_definitions().map_err(|_| ())?;

        let generic_ptr = self.module.get_finalized_function(thunk_id);
        let generic: CompiledThunk = unsafe { std::mem::transmute(generic_ptr) };

        let (specialized, specialized_arity, specialized_kind) =
            if let Some(sid) = spec_thunk_id {
                let raw = self.module.get_finalized_function(sid);
                // Today the only specialized body emitter is the A2
                // forward trampoline. A later commit swaps this to
                // NativeIntBody once the real body is generated.
                (
                    Some(raw as SpecializedThunkRaw),
                    func.arity,
                    Some(SpecializedEntryKind::ForwardTrampoline),
                )
            } else {
                (None, 0, None)
            };

        if specialized.is_some() {
            self.counters
                .specialized_entry_compiled
                .set(self.counters.specialized_entry_compiled.get() + 1);
        }

        Ok(CompiledEntries {
            generic,
            specialized,
            specialized_arity,
            specialized_kind,
        })
    }

    /// Emit the forward-trampoline specialized entry body.
    ///
    /// The body receives `(vm, i64, i64, ..., i64)` in registers per the
    /// C ABI and must produce `(i64, u32)` for the caller. The v1
    /// strategy is to route back through the generic thunk so correctness
    /// is trivially provided by the unchanged generic body:
    ///
    /// 1. For each i64 arg, push `Value::Integer(arg)` onto the VM stack.
    /// 2. Push a `JitFrame` whose slot_offset points one below the
    ///    just-pushed args (so the first arg sits at `slot_offset + 1`,
    ///    matching the generic ABI).
    /// 3. `call_indirect` the generic thunk with `(vm)`.
    /// 4. If status != 0, return `(0, status)` — generic already cleaned
    ///    up its frame on the way out.
    /// 5. On status 0, the top of the VM stack holds the boxed return
    ///    value. We ONLY emit a specialized entry when the function's
    ///    return is Int (A1 eligibility), so load the i64 payload from
    ///    the top and return `(payload, 0)`.
    fn emit_specialized_trampoline(
        &mut self,
        func: &Rc<Function>,
        generic_id: FuncId,
        spec_id: FuncId,
        spec_seq_id: u32,
        ptr_ty: types::Type,
    ) -> Result<(), ()> {
        use cranelift_codegen::ir::MemFlags;
        use cranelift_codegen::ir::condcodes::IntCC;

        let arity = func.arity as usize;

        let mut spec_sig = self.module.make_signature();
        spec_sig.params.push(AbiParam::new(ptr_ty));
        for _ in 0..arity {
            spec_sig.params.push(AbiParam::new(types::I64));
        }
        spec_sig.returns.push(AbiParam::new(types::I64));
        spec_sig.returns.push(AbiParam::new(types::I32));

        self.ctx.func.signature = spec_sig.clone();
        self.ctx.func.name = UserFuncName::user(0, spec_seq_id);

        let counters_ptr_opt: Option<*const JitCounters> =
            if std::env::var("OXIGEN_JIT_STATS").is_ok() {
                Some(self.counters.as_ref() as *const _)
            } else {
                None
            };

        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.fbc);

            // Import the generic thunk into this function so we can
            // call it directly.
            let generic_sig = {
                let mut gs = self.module.make_signature();
                gs.params.push(AbiParam::new(ptr_ty));
                gs.returns.push(AbiParam::new(types::I32));
                builder.import_signature(gs)
            };
            let generic_fref = self.module.declare_func_in_func(generic_id, builder.func);

            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);

            let vm_val = builder.block_params(entry_block)[0];
            let args: Vec<cranelift_codegen::ir::Value> = (0..arity)
                .map(|i| builder.block_params(entry_block)[i + 1])
                .collect();

            // Counter: bump specialized_entry_called at the very top.
            if let Some(cp) = counters_ptr_opt {
                emit_counter_bump(
                    &mut builder,
                    cp,
                    counter_offsets::SPECIALIZED_ENTRY_CALLED,
                );
            }

            // (1) Push each i64 arg onto the VM stack as
            // Value::Integer(arg). emit_inline_push_integer bumps
            // stack_view.len, matching what the generic ABI caller
            // would have done.
            for a in &args {
                emit_inline_push_integer(&mut builder, vm_val, *a);
            }

            // (2) Push the JitFrame for the callee. slot_offset is the
            // stack-view length BEFORE our arg pushes — that's where the
            // caller-side closure would have sat in the generic path.
            // Our trampoline is entered AFTER the caller's specialized-
            // call IR pushed a JitFrame; we reuse that frame's
            // slot_offset by loading it from the top JitFrame. The
            // closure pointer and line are likewise already set by the
            // caller. So: nothing to do here — the caller has already
            // done step (2) for us.
            //
            // Behavior alignment note: in the current pre-A3 state,
            // NO caller emits a specialized-call path, so the
            // specialized trampoline is reachable only if A3/A5 later
            // direct-calls it with a frame already pushed. We still
            // emit the correct body now.

            let flags = MemFlags::trusted();

            // (3) Call generic.
            let call = builder.ins().call(generic_fref, &[vm_val]);
            let _ = generic_sig;
            let status = builder.inst_results(call)[0];

            let ok_block = builder.create_block();
            let err_block = builder.create_block();
            builder.append_block_param(err_block, types::I32);
            builder.ins().brif(status, err_block, &[status], ok_block, &[]);

            // (4) Error / bailout passthrough: return (0, status).
            builder.switch_to_block(err_block);
            let err_status = builder.block_params(err_block)[0];
            let zero_i64 = builder.ins().iconst(types::I64, 0);
            builder.ins().return_(&[zero_i64, err_status]);

            // (5) Success: generic pushed a Value::Integer(result) at
            // stack top (which is what op_return does). Load the i64
            // payload from there, pop it, return (payload, 0).
            builder.switch_to_block(ok_block);
            let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
            let stack_len = emit_load_stack_len(&mut builder, vm_val);
            let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
            let one = builder.ins().iconst(types::I64, 1);
            let top_idx = builder.ins().isub(stack_len, one);
            let byte_off = builder.ins().imul(top_idx, value_size);
            let top_addr = builder.ins().iadd(stack_ptr, byte_off);
            let payload = builder.ins().load(
                types::I64,
                flags,
                top_addr,
                VALUE_INT_PAYLOAD_OFFSET as i32,
            );
            // Pop the Value from stack (decrement stack_view.len). The
            // caller is responsible for whatever cleanup follows.
            emit_inline_stack_pop_one(&mut builder, vm_val);

            // Defensive: also verify tag is Integer; if not, we signal
            // bailout. Should never fire because A1 only marks a
            // function eligible when every Return IP has Int64 on top.
            let tag = builder.ins().load(types::I8, flags, top_addr, 0);
            let integer_tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
            let is_int = builder.ins().icmp(IntCC::Equal, tag, integer_tag);
            let return_ok_block = builder.create_block();
            let bailout_block = builder.create_block();
            builder.ins().brif(is_int, return_ok_block, &[], bailout_block, &[]);

            builder.switch_to_block(bailout_block);
            let two = builder.ins().iconst(types::I32, 2);
            let zero_i64_b = builder.ins().iconst(types::I64, 0);
            builder.ins().return_(&[zero_i64_b, two]);

            builder.switch_to_block(return_ok_block);
            let zero_status = builder.ins().iconst(types::I32, 0);
            builder.ins().return_(&[payload, zero_status]);

            builder.seal_all_blocks();
            builder.finalize();
        }

        self.module
            .define_function(spec_id, &mut self.ctx)
            .map_err(|e| {
                if std::env::var("OXIGEN_JIT_DEBUG").is_ok() {
                    eprintln!("[jit] specialized define_function failed: {:?}", e);
                }
            })?;
        self.module.clear_context(&mut self.ctx);
        Ok(())
    }
}

impl Drop for JitInner {
    fn drop(&mut self) {
        if std::env::var("OXIGEN_JIT_STATS").is_ok() {
            self.counters.dump();
        }
    }
}

// ── Helper registration & declaration ──────────────────────────────────

fn register_helpers(builder: &mut JITBuilder) {
    macro_rules! reg {
        ($name:literal, $fn:path) => {
            builder.symbol($name, $fn as *const u8);
        };
    }

    reg!("jit_run_via_interpreter", runtime::jit_run_via_interpreter);
    reg!("jit_push_constant", runtime::jit_push_constant);
    reg!("jit_push_integer_inline", runtime::jit_push_integer_inline);
    reg!("jit_push_float_inline", runtime::jit_push_float_inline);
    reg!("jit_push_none", runtime::jit_push_none);
    reg!("jit_push_true", runtime::jit_push_true);
    reg!("jit_push_false", runtime::jit_push_false);
    reg!("jit_pop", runtime::jit_pop);
    reg!("jit_dup", runtime::jit_dup);
    reg!("jit_build_array", runtime::jit_build_array);
    reg!(
        "jit_op_index_fast_array_int",
        runtime::jit_op_index_fast_array_int
    );
    reg!("jit_type_wrap", runtime::jit_type_wrap);
    reg!(
        "jit_local_add_array_mod_index",
        runtime::jit_local_add_array_mod_index
    );
    reg!(
        "jit_struct_field_add_const",
        runtime::jit_struct_field_add_const
    );
    reg!(
        "jit_struct_field_add_local",
        runtime::jit_struct_field_add_local
    );
    reg!("jit_get_local", runtime::jit_get_local);
    reg!("jit_set_local", runtime::jit_set_local);
    reg!("jit_op_add", runtime::jit_op_add);
    reg!("jit_op_sub", runtime::jit_op_sub);
    reg!("jit_op_mul", runtime::jit_op_mul);
    reg!("jit_op_div", runtime::jit_op_div);
    reg!("jit_op_mod", runtime::jit_op_mod);
    reg!("jit_op_eq", runtime::jit_op_eq);
    reg!("jit_op_ne", runtime::jit_op_ne);
    reg!("jit_op_lt", runtime::jit_op_lt);
    reg!("jit_op_le", runtime::jit_op_le);
    reg!("jit_op_gt", runtime::jit_op_gt);
    reg!("jit_op_ge", runtime::jit_op_ge);
    reg!("jit_op_not", runtime::jit_op_not);
    reg!("jit_op_negate", runtime::jit_op_negate);
    reg!("jit_peek_truthy", runtime::jit_peek_truthy);
    reg!("jit_pop_truthy", runtime::jit_pop_truthy);
    reg!("jit_op_return", runtime::jit_op_return);
    reg!("jit_op_call", runtime::jit_op_call);
    reg!("jit_op_call_hit", runtime::jit_op_call_hit);
    reg!("jit_op_call_miss", runtime::jit_op_call_miss);
    reg!("jit_get_global", runtime::jit_get_global);
    reg!("jit_set_global", runtime::jit_set_global);
    reg!("jit_define_global", runtime::jit_define_global);
    reg!("jit_define_global_typed", runtime::jit_define_global_typed);
    reg!("jit_get_upvalue", runtime::jit_get_upvalue);
    reg!("jit_set_upvalue", runtime::jit_set_upvalue);
    reg!("jit_close_upvalue", runtime::jit_close_upvalue);
    reg!("jit_op_closure", runtime::jit_op_closure);
    reg!("jit_op_struct_def", runtime::jit_op_struct_def);
    reg!("jit_op_struct_literal", runtime::jit_op_struct_literal);
    reg!("jit_op_get_field", runtime::jit_op_get_field);
    reg!("jit_op_set_field", runtime::jit_op_set_field);
    reg!("jit_op_get_field_ic_miss", runtime::jit_op_get_field_ic_miss);
    reg!("jit_op_set_field_ic_miss", runtime::jit_op_set_field_ic_miss);
    reg!("jit_op_define_method", runtime::jit_op_define_method);
    reg!("jit_op_method_call", runtime::jit_op_method_call);
    reg!("jit_op_method_call_ic", runtime::jit_op_method_call_ic);
    reg!("jit_stack_as_mut_ptr", runtime::jit_stack_as_mut_ptr);
    reg!("jit_stack_len", runtime::jit_stack_len);
    reg!("jit_stack_pop_one", runtime::jit_stack_pop_one);
    reg!("jit_stack_pop_n", runtime::jit_stack_pop_n);
    reg!("jit_stack_commit_len", runtime::jit_stack_commit_len);
    reg!("jit_stack_truncate", runtime::jit_stack_truncate);
    reg!(
        "jit_replace_top2_with_bool",
        runtime::jit_replace_top2_with_bool
    );
    reg!("jit_current_slot_offset", runtime::jit_current_slot_offset);
    reg!("jit_get_global_ic", runtime::jit_get_global_ic);
}

fn declare_helpers(module: &mut JITModule) -> HelperIds {
    let ptr_ty = module.target_config().pointer_type();

    let mut sig_vm_only = module.make_signature();
    sig_vm_only.params.push(AbiParam::new(ptr_ty));

    let mut sig_vm_u32 = module.make_signature();
    sig_vm_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_u32.params.push(AbiParam::new(types::I32));

    // Signature for inline primitive pushes: fn(*mut VM, i64).
    // Floats are passed as their u64 bit pattern reinterpreted as i64.
    let mut sig_vm_i64 = module.make_signature();
    sig_vm_i64.params.push(AbiParam::new(ptr_ty));
    sig_vm_i64.params.push(AbiParam::new(types::I64));

    let mut sig_vm_to_u32 = module.make_signature();
    sig_vm_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_to_u32.returns.push(AbiParam::new(types::I32));

    // Signature for jit_op_call: fn(*mut VM, u32) -> u32
    let mut sig_vm_u32_to_u32 = module.make_signature();
    sig_vm_u32_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_u32_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_u32_to_u32.returns.push(AbiParam::new(types::I32));

    // Signature for jit_op_closure: fn(*mut VM, u32, u32) -> u32
    let mut sig_vm_u32_u32_to_u32 = module.make_signature();
    sig_vm_u32_u32_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_u32_u32_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_u32_u32_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_u32_u32_to_u32
        .returns
        .push(AbiParam::new(types::I32));

    // Signature for jit_define_global_typed: fn(*mut VM, u32, u32, u32) -> u32
    let mut sig_vm_3u32_to_u32 = module.make_signature();
    sig_vm_3u32_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_3u32_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_3u32_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_3u32_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_3u32_to_u32.returns.push(AbiParam::new(types::I32));

    // Signature for jit_struct_field_add_const:
    // fn(*mut VM, u32 self_slot, u32 field_idx, i64 addend,
    //    *mut FieldCacheEntry) -> u32
    let mut sig_vm_u32_u32_i64_ptr_to_u32 = module.make_signature();
    sig_vm_u32_u32_i64_ptr_to_u32
        .params
        .push(AbiParam::new(ptr_ty));
    sig_vm_u32_u32_i64_ptr_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_u32_u32_i64_ptr_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_u32_u32_i64_ptr_to_u32
        .params
        .push(AbiParam::new(types::I64));
    sig_vm_u32_u32_i64_ptr_to_u32
        .params
        .push(AbiParam::new(ptr_ty));
    sig_vm_u32_u32_i64_ptr_to_u32
        .returns
        .push(AbiParam::new(types::I32));

    // Signature for jit_struct_field_add_local:
    // fn(*mut VM, u32 self_slot, u32 field_idx, u32 rhs_slot,
    //    *mut FieldCacheEntry) -> u32
    let mut sig_vm_3u32_ptr_to_u32 = module.make_signature();
    sig_vm_3u32_ptr_to_u32
        .params
        .push(AbiParam::new(ptr_ty));
    sig_vm_3u32_ptr_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_3u32_ptr_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_3u32_ptr_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_3u32_ptr_to_u32
        .params
        .push(AbiParam::new(ptr_ty));
    sig_vm_3u32_ptr_to_u32
        .returns
        .push(AbiParam::new(types::I32));

    // Signature for jit_op_method_call_ic: fn(*mut VM, u32, u32, *mut ptr) -> u32
    let mut sig_vm_u32_u32_ptr_to_u32 = module.make_signature();
    sig_vm_u32_u32_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_u32_u32_ptr_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_u32_u32_ptr_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_u32_u32_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_u32_u32_ptr_to_u32
        .returns
        .push(AbiParam::new(types::I32));

    // Signature for jit_local_add_array_mod_index:
    // fn(*mut VM, u32, u32, u32, i64, u32) -> u32
    let mut sig_vm_3u32_i64_u32_to_u32 = module.make_signature();
    sig_vm_3u32_i64_u32_to_u32
        .params
        .push(AbiParam::new(ptr_ty));
    sig_vm_3u32_i64_u32_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_3u32_i64_u32_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_3u32_i64_u32_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_3u32_i64_u32_to_u32
        .params
        .push(AbiParam::new(types::I64));
    sig_vm_3u32_i64_u32_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_3u32_i64_u32_to_u32
        .returns
        .push(AbiParam::new(types::I32));

    // Signature for fallible with one u32 operand: fn(*mut VM, u32) -> u32
    // (same as sig_vm_u32_to_u32)
    let sig_vm_u32_fallible = &sig_vm_u32_to_u32;

    // Signature: fn(*mut VM) -> *mut T (pointer-sized return)
    let mut sig_vm_to_ptr = module.make_signature();
    sig_vm_to_ptr.params.push(AbiParam::new(ptr_ty));
    sig_vm_to_ptr.returns.push(AbiParam::new(ptr_ty));

    // Signature: fn(*mut VM) -> i64
    let mut sig_vm_to_i64 = module.make_signature();
    sig_vm_to_i64.params.push(AbiParam::new(ptr_ty));
    sig_vm_to_i64.returns.push(AbiParam::new(types::I64));

    // Signature: fn(*mut VM, *mut GlobalCacheEntry, u32) -> u32
    // Signature for jit_op_call_hit: fn(*mut VM, *mut ptr) -> u32
    let mut sig_vm_ptr_to_u32 = module.make_signature();
    sig_vm_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_ptr_to_u32.returns.push(AbiParam::new(types::I32));

    // Signature for jit_op_call_miss: fn(*mut VM, u32, *mut ptr) -> u32
    let mut sig_vm_u32_ptr_to_u32 = module.make_signature();
    sig_vm_u32_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_u32_ptr_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_u32_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_u32_ptr_to_u32
        .returns
        .push(AbiParam::new(types::I32));

    let mut sig_vm_ptr_u32_to_u32 = module.make_signature();
    sig_vm_ptr_u32_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_ptr_u32_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_ptr_u32_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_ptr_u32_to_u32
        .returns
        .push(AbiParam::new(types::I32));

    fn decl(module: &mut JITModule, name: &str, sig: &Signature) -> FuncId {
        module
            .declare_function(name, Linkage::Import, sig)
            .expect("helper declaration should succeed")
    }

    HelperIds {
        push_constant: decl(module, "jit_push_constant", &sig_vm_u32),
        push_integer_inline: decl(module, "jit_push_integer_inline", &sig_vm_i64),
        push_float_inline: decl(module, "jit_push_float_inline", &sig_vm_i64),
        push_none: decl(module, "jit_push_none", &sig_vm_only),
        push_true: decl(module, "jit_push_true", &sig_vm_only),
        push_false: decl(module, "jit_push_false", &sig_vm_only),
        pop: decl(module, "jit_pop", &sig_vm_only),
        dup: decl(module, "jit_dup", &sig_vm_only),
        build_array: decl(module, "jit_build_array", &sig_vm_u32),
        index_fast_array_int: decl(module, "jit_op_index_fast_array_int", &sig_vm_to_u32),
        type_wrap: decl(module, "jit_type_wrap", sig_vm_u32_fallible),
        local_add_array_mod_index: decl(
            module,
            "jit_local_add_array_mod_index",
            &sig_vm_3u32_i64_u32_to_u32,
        ),
        struct_field_add_const: decl(
            module,
            "jit_struct_field_add_const",
            &sig_vm_u32_u32_i64_ptr_to_u32,
        ),
        struct_field_add_local: decl(
            module,
            "jit_struct_field_add_local",
            &sig_vm_3u32_ptr_to_u32,
        ),
        get_local: decl(module, "jit_get_local", &sig_vm_u32),
        set_local: decl(module, "jit_set_local", &sig_vm_u32),
        add: decl(module, "jit_op_add", &sig_vm_to_u32),
        sub: decl(module, "jit_op_sub", &sig_vm_to_u32),
        mul: decl(module, "jit_op_mul", &sig_vm_to_u32),
        div: decl(module, "jit_op_div", &sig_vm_to_u32),
        modu: decl(module, "jit_op_mod", &sig_vm_to_u32),
        eq: decl(module, "jit_op_eq", &sig_vm_only),
        ne: decl(module, "jit_op_ne", &sig_vm_only),
        lt: decl(module, "jit_op_lt", &sig_vm_to_u32),
        le: decl(module, "jit_op_le", &sig_vm_to_u32),
        gt: decl(module, "jit_op_gt", &sig_vm_to_u32),
        ge: decl(module, "jit_op_ge", &sig_vm_to_u32),
        not: decl(module, "jit_op_not", &sig_vm_only),
        negate: decl(module, "jit_op_negate", &sig_vm_to_u32),
        peek_truthy: decl(module, "jit_peek_truthy", &sig_vm_to_u32),
        pop_truthy: decl(module, "jit_pop_truthy", &sig_vm_to_u32),
        op_return: decl(module, "jit_op_return", &sig_vm_only),
        op_call: decl(module, "jit_op_call", &sig_vm_u32_to_u32),
        op_call_hit: decl(module, "jit_op_call_hit", &sig_vm_ptr_to_u32),
        op_call_miss: decl(module, "jit_op_call_miss", &sig_vm_u32_ptr_to_u32),
        get_global: decl(module, "jit_get_global", sig_vm_u32_fallible),
        get_global_ic: decl(module, "jit_get_global_ic", &sig_vm_ptr_u32_to_u32),
        set_global: decl(module, "jit_set_global", sig_vm_u32_fallible),
        define_global: decl(module, "jit_define_global", sig_vm_u32_fallible),
        define_global_typed: decl(module, "jit_define_global_typed", &sig_vm_3u32_to_u32),
        get_upvalue: decl(module, "jit_get_upvalue", &sig_vm_u32),
        set_upvalue: decl(module, "jit_set_upvalue", &sig_vm_u32),
        close_upvalue: decl(module, "jit_close_upvalue", &sig_vm_only),
        op_closure: decl(module, "jit_op_closure", &sig_vm_u32_u32_to_u32),
        op_struct_def: decl(module, "jit_op_struct_def", &sig_vm_u32),
        op_struct_literal: decl(module, "jit_op_struct_literal", &sig_vm_u32_u32_to_u32),
        op_get_field_ic_miss: decl(module, "jit_op_get_field_ic_miss", &sig_vm_u32_ptr_to_u32),
        op_set_field_ic_miss: decl(module, "jit_op_set_field_ic_miss", &sig_vm_u32_ptr_to_u32),
        op_define_method: decl(module, "jit_op_define_method", &sig_vm_u32_u32_to_u32),
        op_method_call: decl(module, "jit_op_method_call", &sig_vm_u32_u32_to_u32),
        op_method_call_ic: decl(
            module,
            "jit_op_method_call_ic",
            &sig_vm_u32_u32_ptr_to_u32,
        ),
        stack_as_mut_ptr: decl(module, "jit_stack_as_mut_ptr", &sig_vm_to_ptr),
        stack_len: decl(module, "jit_stack_len", &sig_vm_to_i64),
        stack_pop_one: decl(module, "jit_stack_pop_one", &sig_vm_only),
        stack_pop_n: decl(module, "jit_stack_pop_n", &sig_vm_u32),
        stack_commit_len: decl(module, "jit_stack_commit_len", &sig_vm_i64),
        stack_truncate: decl(module, "jit_stack_truncate", &sig_vm_i64),
        replace_top2_with_bool: decl(module, "jit_replace_top2_with_bool", &sig_vm_u32),
        current_slot_offset: decl(module, "jit_current_slot_offset", &sig_vm_to_i64),
    }
}

fn declare_helper_refs(
    ids: &HelperIds,
    module: &mut JITModule,
    builder: &mut FunctionBuilder<'_>,
) -> HelperRefs {
    let mut r = |id| module.declare_func_in_func(id, builder.func);
    HelperRefs {
        push_constant: r(ids.push_constant),
        push_integer_inline: r(ids.push_integer_inline),
        push_float_inline: r(ids.push_float_inline),
        push_none: r(ids.push_none),
        push_true: r(ids.push_true),
        push_false: r(ids.push_false),
        pop: r(ids.pop),
        dup: r(ids.dup),
        build_array: r(ids.build_array),
        index_fast_array_int: r(ids.index_fast_array_int),
        type_wrap: r(ids.type_wrap),
        local_add_array_mod_index: r(ids.local_add_array_mod_index),
        struct_field_add_const: r(ids.struct_field_add_const),
        struct_field_add_local: r(ids.struct_field_add_local),
        get_local: r(ids.get_local),
        set_local: r(ids.set_local),
        add: r(ids.add),
        sub: r(ids.sub),
        mul: r(ids.mul),
        div: r(ids.div),
        modu: r(ids.modu),
        eq: r(ids.eq),
        ne: r(ids.ne),
        lt: r(ids.lt),
        le: r(ids.le),
        gt: r(ids.gt),
        ge: r(ids.ge),
        not: r(ids.not),
        negate: r(ids.negate),
        peek_truthy: r(ids.peek_truthy),
        pop_truthy: r(ids.pop_truthy),
        op_return: r(ids.op_return),
        op_call: r(ids.op_call),
        op_call_hit: r(ids.op_call_hit),
        op_call_miss: r(ids.op_call_miss),
        get_global: r(ids.get_global),
        get_global_ic: r(ids.get_global_ic),
        set_global: r(ids.set_global),
        define_global: r(ids.define_global),
        define_global_typed: r(ids.define_global_typed),
        get_upvalue: r(ids.get_upvalue),
        set_upvalue: r(ids.set_upvalue),
        close_upvalue: r(ids.close_upvalue),
        op_closure: r(ids.op_closure),
        op_struct_def: r(ids.op_struct_def),
        op_struct_literal: r(ids.op_struct_literal),
        op_get_field_ic_miss: r(ids.op_get_field_ic_miss),
        op_set_field_ic_miss: r(ids.op_set_field_ic_miss),
        op_define_method: r(ids.op_define_method),
        op_method_call: r(ids.op_method_call),
        op_method_call_ic: r(ids.op_method_call_ic),
        stack_as_mut_ptr: r(ids.stack_as_mut_ptr),
        stack_len: r(ids.stack_len),
        stack_pop_one: r(ids.stack_pop_one),
        stack_pop_n: r(ids.stack_pop_n),
        stack_commit_len: r(ids.stack_commit_len),
        stack_truncate: r(ids.stack_truncate),
        replace_top2_with_bool: r(ids.replace_top2_with_bool),
        current_slot_offset: r(ids.current_slot_offset),
    }
}

// ── Fallible-helper emission ───────────────────────────────────────────

/// Emit a call to a fallible helper, then a check: if status != 0, return
/// early from the thunk with that status. Otherwise continue in a fresh
/// "ok" block.
fn emit_fallible(
    builder: &mut FunctionBuilder<'_>,
    helper: FuncRef,
    vm_val: cranelift_codegen::ir::Value,
) {
    let call = builder.ins().call(helper, &[vm_val]);
    let status = builder.inst_results(call)[0];
    emit_early_exit_on_err(builder, status);
}

/// Given a u32 status produced by a helper call, branch on status != 0:
/// non-zero returns the status from the thunk; zero continues in a fresh
/// "ok" block.
fn emit_early_exit_on_err(builder: &mut FunctionBuilder<'_>, status: cranelift_codegen::ir::Value) {
    let err_block = builder.create_block();
    let ok_block = builder.create_block();
    builder.ins().brif(status, err_block, &[], ok_block, &[]);
    builder.switch_to_block(err_block);
    builder.ins().return_(&[status]);
    builder.switch_to_block(ok_block);
}

// ── Bytecode helpers ───────────────────────────────────────────────────

fn read_u16(code: &[u8], offset: usize) -> u16 {
    ((code[offset] as u16) << 8) | (code[offset + 1] as u16)
}

/// Count `GetGlobal` opcodes in a chunk so the translator can
/// pre-allocate one inline-cache slot each. We walk the stream using
/// the same length table as the scanner; the scan pass has already
/// validated every opcode is in the allow-list, so we can assume
/// fixed-length instructions here — except `Closure`, which is
/// variable-length and needs the constants pool to resolve its length.
fn count_ic_sites(chunk: &crate::compiler::opcode::Chunk) -> (usize, usize, usize, usize) {
    let code = &chunk.code;
    let mut get_globals = 0usize;
    let mut calls = 0usize;
    let mut method_calls = 0usize;
    let mut field_ops = 0usize;
    let mut ip = 0;
    while ip < code.len() {
        let Some(op) = OpCode::from_byte(code[ip]) else {
            break;
        };
        match op {
            OpCode::GetGlobal => get_globals += 1,
            OpCode::Call => calls += 1,
            OpCode::MethodCall => method_calls += 1,
            OpCode::GetField | OpCode::SetField => field_ops += 1,
            _ => {}
        }
        ip += match op {
            // 1-byte opcodes
            OpCode::None
            | OpCode::True
            | OpCode::False
            | OpCode::Pop
            | OpCode::Dup
            | OpCode::Add
            | OpCode::Subtract
            | OpCode::Multiply
            | OpCode::Divide
            | OpCode::Modulo
            | OpCode::Equal
            | OpCode::NotEqual
            | OpCode::Less
            | OpCode::LessEqual
            | OpCode::Greater
            | OpCode::GreaterEqual
            | OpCode::Not
            | OpCode::Negate
            | OpCode::Index
            | OpCode::CloseUpvalue
            | OpCode::Return => 1,
            // u8 operand
            OpCode::Call => 2,
            // u16 operand
            OpCode::Constant
            | OpCode::BuildArray
            | OpCode::TypeWrap
            | OpCode::GetLocal
            | OpCode::SetLocal
            | OpCode::GetGlobal
            | OpCode::SetGlobal
            | OpCode::DefineGlobal
            | OpCode::GetUpvalue
            | OpCode::SetUpvalue
            | OpCode::Jump
            | OpCode::JumpIfFalse
            | OpCode::JumpIfTrue
            | OpCode::Loop
            | OpCode::PopJumpIfFalse
            | OpCode::StructDef
            | OpCode::GetField
            | OpCode::SetField => 3,
            // u16 + u8
            OpCode::StructLiteral | OpCode::DefineMethod | OpCode::MethodCall => 4,
            // u16 + u8 + u16
            OpCode::DefineGlobalTyped => 6,
            // Variable-length
            OpCode::Closure => {
                if ip + 2 >= code.len() {
                    break;
                }
                let fn_idx = ((code[ip + 1] as u16) << 8) | (code[ip + 2] as u16);
                let uv_count = match chunk.constants.get(fn_idx as usize) {
                    Some(crate::vm::value::Value::Closure(t)) => t.function.upvalue_count as usize,
                    _ => break,
                };
                3 + 3 * uv_count
            }
            // Anything else shouldn't reach here — scan has rejected
            // the function if we hit a non-allow-listed opcode.
            _ => break,
        };
    }
    (get_globals, calls, method_calls, field_ops)
}

struct LocalConstArithUpdate {
    slot: u16,
    constant: i64,
    op: IntArithOp,
    pop_after: bool,
    len: usize,
}

struct LocalScaledArithUpdate {
    dst_slot: u16,
    rhs_slot: u16,
    constant: i64,
    op: IntArithOp,
    pop_after: bool,
    len: usize,
}

struct LocalArithUpdate {
    dst_slot: u16,
    rhs_slot: u16,
    op: IntArithOp,
    pop_after: bool,
    len: usize,
}

#[derive(Debug)]
enum StructFieldAddShape {
    Const { addend: i64 },
    Local { rhs_slot: u16 },
}

/// Result of inspecting a struct method's bytecode for an inline-expandable
/// shape. Populated at JIT compile time (for the `detect_inline_method_info`
/// export) and consumed by `jit_op_method_call_ic` on first MethodCall miss.
#[derive(Debug)]
pub(crate) struct DetectedInlineMethod {
    pub kind: MethodInlineKind,
    pub field_name: std::rc::Rc<String>,
    pub addend: i64,
}

/// Detect whether `func`'s bytecode is a single field-add peephole body
/// (`self.f = self.f + <const|arg>; return`), and if so return the kind
/// plus the field name and addend so the caller can stamp it into the
/// MethodCacheEntry for inline expansion at the call site.
pub(crate) fn detect_inline_method_info(
    func: &Function,
) -> Option<DetectedInlineMethod> {
    use crate::vm::value::Value;

    let code = &func.chunk.code;
    // Need at least the 16-byte peephole + a 1-byte trailing `Return`.
    // (The compiler emits an implicit None/Return tail when the method
    // body has no explicit return value; we accept either shape.)
    if code.len() < 17 {
        return None;
    }

    // Match with an empty blocks map — the peephole must cover the whole
    // body so there can't be any branch targets inside it.
    let empty_blocks: HashMap<usize, Block> = HashMap::new();
    let m = match_struct_field_add_update(code, &func.chunk, 0, &empty_blocks)?;

    // Everything after the peephole must be a trivial return tail:
    //   - `Return` alone (1 byte)
    //   - `None Return` (2 bytes, push None then return)
    let tail_ok = match OpCode::from_byte(code[16]) {
        Some(OpCode::Return) => code.len() == 17,
        Some(OpCode::None) => {
            code.len() == 18 && OpCode::from_byte(code[17]) == Some(OpCode::Return)
        }
        _ => false,
    };
    if !tail_ok {
        return None;
    }

    // Resolve the field name from the callee's constant pool.
    let field_name_val = func.chunk.constants.get(m.field_idx as usize)?;
    let field_name: std::rc::Rc<String> = match field_name_val {
        Value::String(s) => std::rc::Rc::clone(s),
        _ => return None,
    };

    // `self` is always the first user-visible param (slot 1 — slot 0 is the
    // closure stack-frame marker), so arity must match the shape:
    //   - FieldAddConst → 0 user args → arity 1
    //   - FieldAddLocal → 1 user arg → arity 2
    match m.shape {
        StructFieldAddShape::Const { addend } => {
            if func.arity != 1 {
                return None;
            }
            Some(DetectedInlineMethod {
                kind: MethodInlineKind::FieldAddConst,
                field_name,
                addend,
            })
        }
        StructFieldAddShape::Local { rhs_slot } => {
            if func.arity != 2 || rhs_slot != 2 {
                return None;
            }
            Some(DetectedInlineMethod {
                kind: MethodInlineKind::FieldAddLocal,
                field_name,
                addend: 0,
            })
        }
    }
}

struct StructFieldAddPeephole {
    self_slot: u16,
    field_idx: u16,
    shape: StructFieldAddShape,
    len: usize,
}

fn match_local_arith_update(
    code: &[u8],
    ip: usize,
    blocks: &HashMap<usize, Block>,
) -> Option<LocalArithUpdate> {
    if ip + 10 > code.len() {
        return None;
    }

    let dst_slot = read_u16(code, ip + 1);
    let rhs_ip = ip + 3;
    let arith_ip = ip + 6;
    let set_ip = ip + 7;

    if blocks.contains_key(&rhs_ip)
        || blocks.contains_key(&arith_ip)
        || blocks.contains_key(&set_ip)
    {
        return None;
    }

    if OpCode::from_byte(code[rhs_ip]) != Some(OpCode::GetLocal) {
        return None;
    }
    let rhs_slot = read_u16(code, rhs_ip + 1);

    let op = match OpCode::from_byte(code[arith_ip]) {
        Some(OpCode::Add) => IntArithOp::Add,
        Some(OpCode::Subtract) => IntArithOp::Sub,
        _ => return None,
    };

    if OpCode::from_byte(code[set_ip]) != Some(OpCode::SetLocal) {
        return None;
    }
    let set_slot = read_u16(code, set_ip + 1);
    if set_slot != dst_slot {
        return None;
    }

    let pop_ip = ip + 10;
    let pop_after = pop_ip < code.len()
        && OpCode::from_byte(code[pop_ip]) == Some(OpCode::Pop)
        && !blocks.contains_key(&pop_ip);
    let len = if pop_after { 11 } else { 10 };

    Some(LocalArithUpdate {
        dst_slot,
        rhs_slot,
        op,
        pop_after,
        len,
    })
}

fn match_struct_field_add_update(
    code: &[u8],
    chunk: &Chunk,
    ip: usize,
    blocks: &HashMap<usize, Block>,
) -> Option<StructFieldAddPeephole> {
    if ip + 16 > code.len() {
        return None;
    }

    let self_slot = read_u16(code, ip + 1);
    let self2_ip = ip + 3;
    let get_field_ip = ip + 6;
    let rhs_ip = ip + 9;
    let add_ip = ip + 12;
    let set_field_ip = ip + 13;

    if blocks.contains_key(&self2_ip)
        || blocks.contains_key(&get_field_ip)
        || blocks.contains_key(&rhs_ip)
        || blocks.contains_key(&add_ip)
        || blocks.contains_key(&set_field_ip)
    {
        return None;
    }

    if OpCode::from_byte(code[ip]) != Some(OpCode::GetLocal)
        || OpCode::from_byte(code[self2_ip]) != Some(OpCode::GetLocal)
        || OpCode::from_byte(code[get_field_ip]) != Some(OpCode::GetField)
        || OpCode::from_byte(code[add_ip]) != Some(OpCode::Add)
        || OpCode::from_byte(code[set_field_ip]) != Some(OpCode::SetField)
    {
        return None;
    }

    let self_slot_2 = read_u16(code, self2_ip + 1);
    if self_slot != self_slot_2 {
        return None;
    }

    // GetField and SetField carry constant-pool indices for the field
    // *name*. The parser may emit two separate `Value::String("val")`
    // constants for two source-level occurrences of the same identifier,
    // so compare resolved names rather than raw indices. The helper and
    // inline IR then use the GetField-side index for both load and store:
    // the runtime resolves it against the instance's `FieldLayout` which
    // is keyed on the string name.
    let field_idx = read_u16(code, get_field_ip + 1);
    let set_fidx = read_u16(code, set_field_ip + 1);
    if field_idx != set_fidx {
        let get_name = chunk.constants.get(field_idx as usize);
        let set_name = chunk.constants.get(set_fidx as usize);
        match (get_name, set_name) {
            (
                Some(crate::vm::value::Value::String(a)),
                Some(crate::vm::value::Value::String(b)),
            ) if a.as_ref() == b.as_ref() => {}
            _ => return None,
        }
    }

    let shape = match OpCode::from_byte(code[rhs_ip]) {
        Some(OpCode::Constant) => {
            let const_idx = read_u16(code, rhs_ip + 1);
            let addend = match chunk.constants.get(const_idx as usize) {
                Some(crate::vm::value::Value::Integer(n)) => *n,
                _ => return None,
            };
            StructFieldAddShape::Const { addend }
        }
        Some(OpCode::GetLocal) => StructFieldAddShape::Local {
            rhs_slot: read_u16(code, rhs_ip + 1),
        },
        _ => return None,
    };

    Some(StructFieldAddPeephole {
        self_slot,
        field_idx,
        shape,
        len: 16,
    })
}

struct LocalArrayModIndexAddUpdate {
    dst_slot: u16,
    array_slot: u16,
    index_slot: u16,
    modulus: i64,
    pop_after: bool,
    len: usize,
}

fn match_local_array_mod_index_add_update(
    code: &[u8],
    chunk: &Chunk,
    ip: usize,
    blocks: &HashMap<usize, Block>,
) -> Option<LocalArrayModIndexAddUpdate> {
    if ip + 18 > code.len() {
        return None;
    }

    let dst_slot = read_u16(code, ip + 1);
    let array_ip = ip + 3;
    let index_ip = ip + 6;
    let modulus_ip = ip + 9;
    let modulo_ip = ip + 12;
    let index_op_ip = ip + 13;
    let add_ip = ip + 14;
    let set_ip = ip + 15;

    if blocks.contains_key(&array_ip)
        || blocks.contains_key(&index_ip)
        || blocks.contains_key(&modulus_ip)
        || blocks.contains_key(&modulo_ip)
        || blocks.contains_key(&index_op_ip)
        || blocks.contains_key(&add_ip)
        || blocks.contains_key(&set_ip)
    {
        return None;
    }

    if OpCode::from_byte(code[array_ip]) != Some(OpCode::GetLocal) {
        return None;
    }
    let array_slot = read_u16(code, array_ip + 1);

    if OpCode::from_byte(code[index_ip]) != Some(OpCode::GetLocal) {
        return None;
    }
    let index_slot = read_u16(code, index_ip + 1);

    if OpCode::from_byte(code[modulus_ip]) != Some(OpCode::Constant) {
        return None;
    }
    let modulus_idx = read_u16(code, modulus_ip + 1);
    let modulus = match chunk.constants.get(modulus_idx as usize) {
        Some(crate::vm::value::Value::Integer(n)) => *n,
        _ => return None,
    };

    if OpCode::from_byte(code[modulo_ip]) != Some(OpCode::Modulo)
        || OpCode::from_byte(code[index_op_ip]) != Some(OpCode::Index)
        || OpCode::from_byte(code[add_ip]) != Some(OpCode::Add)
        || OpCode::from_byte(code[set_ip]) != Some(OpCode::SetLocal)
    {
        return None;
    }

    let set_slot = read_u16(code, set_ip + 1);
    if set_slot != dst_slot {
        return None;
    }

    let pop_ip = ip + 18;
    let pop_after = pop_ip < code.len()
        && OpCode::from_byte(code[pop_ip]) == Some(OpCode::Pop)
        && !blocks.contains_key(&pop_ip);
    let len = if pop_after { 19 } else { 18 };

    Some(LocalArrayModIndexAddUpdate {
        dst_slot,
        array_slot,
        index_slot,
        modulus,
        pop_after,
        len,
    })
}

fn match_local_scaled_arith_update(
    code: &[u8],
    chunk: &Chunk,
    ip: usize,
    blocks: &HashMap<usize, Block>,
) -> Option<LocalScaledArithUpdate> {
    if ip + 14 > code.len() {
        return None;
    }

    let dst_slot = read_u16(code, ip + 1);
    let rhs_ip = ip + 3;
    let constant_ip = ip + 6;
    let multiply_ip = ip + 9;
    let arith_ip = ip + 10;
    let set_ip = ip + 11;

    // Do not fuse across block boundaries. A jump into the middle of the
    // sequence expects the stack effects of the original instructions.
    if blocks.contains_key(&rhs_ip)
        || blocks.contains_key(&constant_ip)
        || blocks.contains_key(&multiply_ip)
        || blocks.contains_key(&arith_ip)
        || blocks.contains_key(&set_ip)
    {
        return None;
    }

    if OpCode::from_byte(code[rhs_ip]) != Some(OpCode::GetLocal) {
        return None;
    }
    let rhs_slot = read_u16(code, rhs_ip + 1);

    if OpCode::from_byte(code[constant_ip]) != Some(OpCode::Constant) {
        return None;
    }
    let constant_idx = read_u16(code, constant_ip + 1);
    let constant = match chunk.constants.get(constant_idx as usize) {
        Some(crate::vm::value::Value::Integer(n)) => *n,
        _ => return None,
    };

    if OpCode::from_byte(code[multiply_ip]) != Some(OpCode::Multiply) {
        return None;
    }

    let op = match OpCode::from_byte(code[arith_ip]) {
        Some(OpCode::Add) => IntArithOp::Add,
        Some(OpCode::Subtract) => IntArithOp::Sub,
        _ => return None,
    };

    if OpCode::from_byte(code[set_ip]) != Some(OpCode::SetLocal) {
        return None;
    }
    let set_slot = read_u16(code, set_ip + 1);
    if set_slot != dst_slot {
        return None;
    }

    let pop_ip = ip + 14;
    let pop_after = pop_ip < code.len()
        && OpCode::from_byte(code[pop_ip]) == Some(OpCode::Pop)
        && !blocks.contains_key(&pop_ip);
    let len = if pop_after { 15 } else { 14 };

    Some(LocalScaledArithUpdate {
        dst_slot,
        rhs_slot,
        constant,
        op,
        pop_after,
        len,
    })
}

fn match_local_const_arith_update(
    code: &[u8],
    chunk: &Chunk,
    ip: usize,
    blocks: &HashMap<usize, Block>,
) -> Option<LocalConstArithUpdate> {
    if ip + 10 > code.len() {
        return None;
    }

    let local_slot = read_u16(code, ip + 1);
    let constant_ip = ip + 3;
    let arith_ip = ip + 6;
    let set_ip = ip + 7;

    // Do not fuse across block boundaries. A jump into the middle of the
    // sequence expects the stack effects of the original instructions.
    if blocks.contains_key(&constant_ip)
        || blocks.contains_key(&arith_ip)
        || blocks.contains_key(&set_ip)
    {
        return None;
    }

    if OpCode::from_byte(code[constant_ip]) != Some(OpCode::Constant) {
        return None;
    }
    let constant_idx = read_u16(code, constant_ip + 1);
    let constant = match chunk.constants.get(constant_idx as usize) {
        Some(crate::vm::value::Value::Integer(n)) => *n,
        _ => return None,
    };

    let op = match OpCode::from_byte(code[arith_ip]) {
        Some(OpCode::Add) => IntArithOp::Add,
        Some(OpCode::Subtract) => IntArithOp::Sub,
        Some(OpCode::Multiply) => IntArithOp::Mul,
        _ => return None,
    };

    if OpCode::from_byte(code[set_ip]) != Some(OpCode::SetLocal) {
        return None;
    }
    let set_slot = read_u16(code, set_ip + 1);
    if set_slot != local_slot {
        return None;
    }

    let pop_ip = ip + 10;
    let pop_after = pop_ip < code.len()
        && OpCode::from_byte(code[pop_ip]) == Some(OpCode::Pop)
        && !blocks.contains_key(&pop_ip);
    let len = if pop_after { 11 } else { 10 };

    Some(LocalConstArithUpdate {
        slot: local_slot,
        constant,
        op,
        pop_after,
        len,
    })
}

// ── Inline stack-view reads (no FFI) ──────────────────────────────────
//
// We target 64-bit ABI exclusively; pointers are I64 and `usize` is I64.
// The JIT's fast paths used to call `jit_stack_as_mut_ptr` / `jit_stack_len`
// as FFI helpers just to read two words of VM state — each FFI crossing
// is ~3-5ns. Since the stack is pre-allocated to `STACK_MAX` in
// `VM::new()`, its backing pointer is stable for the VM's lifetime, and
// `stack_view.len` is synced after every mutation via the VM's
// `push`/`pop`/`stack_truncate`/`stack_drain_from`/`stack_insert`
// methods. Reading `stack_view.{ptr, len}` directly is therefore always
// safe and much faster.

/// Emit IR that loads `vm.stack_view.ptr` at its pinned offset.
#[inline]
fn emit_load_stack_ptr(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    use cranelift_codegen::ir::MemFlags;
    builder
        .ins()
        .load(types::I64, MemFlags::trusted(), vm_val, vm_stack_view_ptr_offset())
}

/// Emit IR that loads `vm.stack_view.len` at its pinned offset.
#[inline]
fn emit_load_stack_len(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    use cranelift_codegen::ir::MemFlags;
    builder
        .ins()
        .load(types::I64, MemFlags::trusted(), vm_val, vm_stack_view_len_offset())
}

#[allow(dead_code)]
#[inline]
fn emit_load_top_jit_frame_ptr(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    use cranelift_codegen::ir::MemFlags;

    let base = builder
        .ins()
        .load(types::I64, MemFlags::trusted(), vm_val, vm_jit_frame_view_ptr_offset());
    let len = builder
        .ins()
        .load(types::I64, MemFlags::trusted(), vm_val, vm_jit_frame_view_len_offset());
    let one = builder.ins().iconst(types::I64, 1);
    let frame_size = builder.ins().iconst(types::I64, std::mem::size_of::<JitFrame>() as i64);
    let top_ix = builder.ins().isub(len, one);
    let top_off = builder.ins().imul(top_ix, frame_size);
    builder.ins().iadd(base, top_off)
}

#[inline]
fn emit_load_top_jit_frame_slot_offset(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    use cranelift_codegen::ir::MemFlags;

    let frame_ptr = emit_load_top_jit_frame_ptr(builder, vm_val);
    builder
        .ins()
        .load(types::I64, MemFlags::trusted(), frame_ptr, JitFrame::OFFSET_SLOT_OFFSET)
}

#[inline]
fn emit_copy_value(
    builder: &mut FunctionBuilder<'_>,
    src_ptr: cranelift_codegen::ir::Value,
    dst_ptr: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlags;

    let flags = MemFlags::trusted();
    for off in (0..VALUE_SIZE).step_by(8) {
        let word = builder
            .ins()
            .load(types::I64, flags, src_ptr, off as i32);
        builder.ins().store(flags, word, dst_ptr, off as i32);
    }
}

#[inline]
fn emit_write_closure_value(
    builder: &mut FunctionBuilder<'_>,
    dst_ptr: cranelift_codegen::ir::Value,
    closure_raw: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlags;

    let flags = MemFlags::trusted();
    let closure_tag = builder.ins().iconst(types::I8, VALUE_TAG_CLOSURE as i64);
    builder.ins().store(flags, closure_tag, dst_ptr, 0);
    builder.ins().store(
        flags,
        closure_raw,
        dst_ptr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
}

#[allow(dead_code)]
#[inline]
fn emit_store_current_line(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
    line: u32,
) {
    use cranelift_codegen::ir::MemFlags;

    let frame_ptr = emit_load_top_jit_frame_ptr(builder, vm_val);
    let line_val = builder.ins().iconst(types::I32, line as i64);
    builder
        .ins()
        .store(MemFlags::trusted(), line_val, frame_ptr, JitFrame::OFFSET_LINE);
}

// ── Inline int fast path ───────────────────────────────────────────────

#[derive(Clone, Copy)]
enum IntArithOp {
    Add,
    Sub,
    Mul,
}

/// Emit IR for an arithmetic opcode that has an inline int+int fast
/// path. The emitted sequence checks the top-two values' tag bytes; if
/// both are `Value::Integer`, it performs `op` inline on their i64
/// payloads (wrapping semantics) and writes the result back in-place,
/// then calls the `stack_pop_one` helper to drop the now-duplicate top.
/// Otherwise it falls through to `slow_helper`.
fn emit_int_fast_arith(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    op: IntArithOp,
    slow_helper: FuncRef,
) {
    use cranelift_codegen::ir::MemFlags;

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let stack_len = emit_load_stack_len(builder, vm_val);

    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let one = builder.ins().iconst(types::I64, 1);
    let two = builder.ins().iconst(types::I64, 2);
    let len_minus_1 = builder.ins().isub(stack_len, one);
    let len_minus_2 = builder.ins().isub(stack_len, two);
    let off_b = builder.ins().imul(len_minus_1, value_size);
    let off_a = builder.ins().imul(len_minus_2, value_size);
    let ptr_ty = builder.func.dfg.value_type(stack_ptr);
    let val_b_ptr = builder.ins().iadd(stack_ptr, off_b);
    let val_a_ptr = builder.ins().iadd(stack_ptr, off_a);

    let flags = MemFlags::trusted();
    let tag_b = builder.ins().load(types::I8, flags, val_b_ptr, 0);
    let tag_a = builder.ins().load(types::I8, flags, val_a_ptr, 0);
    let int_tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
    let a_is_int = builder.ins().icmp(
        cranelift_codegen::ir::condcodes::IntCC::Equal,
        tag_a,
        int_tag,
    );
    let b_is_int = builder.ins().icmp(
        cranelift_codegen::ir::condcodes::IntCC::Equal,
        tag_b,
        int_tag,
    );
    let both_int = builder.ins().band(a_is_int, b_is_int);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let continue_block = builder.create_block();

    builder
        .ins()
        .brif(both_int, fast_block, &[], slow_block, &[]);

    // ── Fast path ──
    builder.switch_to_block(fast_block);
    let payload_a = builder.ins().load(
        types::I64,
        flags,
        val_a_ptr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    let payload_b = builder.ins().load(
        types::I64,
        flags,
        val_b_ptr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    let result = match op {
        IntArithOp::Add => builder.ins().iadd(payload_a, payload_b),
        IntArithOp::Sub => builder.ins().isub(payload_a, payload_b),
        IntArithOp::Mul => builder.ins().imul(payload_a, payload_b),
    };
    // Write result back into val_a's payload slot. Its tag is already
    // `VALUE_TAG_INTEGER`, so we don't need to touch it.
    builder
        .ins()
        .store(flags, result, val_a_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
    // Inline the pop. Safe because the fast block gates on
    // VALUE_TAG_INTEGER (top value has no Drop side-effect).
    emit_inline_stack_pop_one(builder, vm_val);
    builder.ins().jump(continue_block, &[]);

    // ── Slow path ──
    builder.switch_to_block(slow_block);
    let call = builder.ins().call(slow_helper, &[vm_val]);
    let status = builder.inst_results(call)[0];
    let err_block = builder.create_block();
    builder
        .ins()
        .brif(status, err_block, &[], continue_block, &[]);
    builder.switch_to_block(err_block);
    builder.ins().return_(&[status]);

    builder.switch_to_block(continue_block);

    // Suppress the unused-warning on ptr_ty — it's the pointer type we
    // expect from `stack_as_mut_ptr` and `iadd` would verify consistency.
    let _ = ptr_ty;
}

/// Inline IR for the struct-field-add peephole (`self.f = self.f + k` /
/// `self.f = self.f + rhs_local`). On IC hit — receiver is a
/// `StructInstance` whose `struct_def_raw` matches the cached pointer
/// AND the current field value is an Integer — we update the i64 payload
/// in place. On miss, fall through to the existing runtime helper which
/// populates the cache for the next trip.
///
/// Guard order, matching the runtime helper's fast-path shape:
///   1. Receiver slot tag == `VALUE_TAG_STRUCT_INSTANCE`.
///   2. `inst.def` raw pointer == `cache.struct_def_raw` (nonzero).
///   3. (Local variant only) rhs slot tag == `VALUE_TAG_INTEGER`.
///   4. Current field slot tag == `VALUE_TAG_INTEGER`.
///
/// Any guard failure branches to the miss path so the helper can do the
/// full dispatch (and repopulate the cache if the shape has changed).
fn emit_inline_struct_field_add(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    ptr_ty: types::Type,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    self_slot: u16,
    field_idx: u16,
    cache_ptr: *mut FieldCacheEntry,
    shape: StructFieldAddShape,
) {
    use cranelift_codegen::ir::MemFlags;
    use cranelift_codegen::ir::condcodes::IntCC;

    let flags = MemFlags::trusted();
    let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);

    let check_def_block = builder.create_block();
    let check_rhs_block = builder.create_block();
    let check_val_block = builder.create_block();
    let fast_block = builder.create_block();
    let miss_block = builder.create_block();
    let ok_block = builder.create_block();
    let err_block = builder.create_block();
    builder.append_block_param(err_block, types::I32);

    // 1. Receiver slot address on the VM stack.
    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let self_slot_off = builder.ins().iconst(types::I64, self_slot as i64);
    let self_slot_abs = builder.ins().iadd(slot_offset_val, self_slot_off);
    let self_byte_off = builder.ins().imul(self_slot_abs, value_size);
    let self_val_ptr = builder.ins().iadd(stack_ptr, self_byte_off);

    // 2. Tag guard: StructInstance.
    let tag = builder.ins().load(types::I8, flags, self_val_ptr, 0);
    let struct_tag = builder
        .ins()
        .iconst(types::I8, VALUE_TAG_STRUCT_INSTANCE as i64);
    let is_struct = builder.ins().icmp(IntCC::Equal, tag, struct_tag);
    builder
        .ins()
        .brif(is_struct, check_def_block, &[], miss_block, &[]);

    // 3. Load `inst_ptr` (past RcBox header) and compare its `def` raw
    // pointer against the cached one. An unpopulated cache has `struct_def_raw
    // == null`, so the comparison naturally misses on the first call.
    builder.switch_to_block(check_def_block);
    let rcbox = builder
        .ins()
        .load(ptr_ty, flags, self_val_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
    let inst_ptr = builder.ins().iadd_imm(rcbox, RC_VALUE_OFFSET as i64);
    let inst_def_raw = builder
        .ins()
        .load(ptr_ty, flags, inst_ptr, STRUCT_INSTANCE_DEF_OFFSET as i32);
    let cached_def_raw =
        builder
            .ins()
            .load(ptr_ty, flags, cache_val, FieldCacheEntry::OFFSET_STRUCT_DEF_RAW);
    let def_matches = builder.ins().icmp(IntCC::Equal, inst_def_raw, cached_def_raw);
    builder
        .ins()
        .brif(def_matches, check_rhs_block, &[], miss_block, &[]);

    // 4. For the Local variant, guard the rhs slot tag too. The Const
    // variant skips this block by jumping straight through.
    builder.switch_to_block(check_rhs_block);
    let rhs_payload = match shape {
        StructFieldAddShape::Const { addend } => {
            builder.ins().jump(check_val_block, &[]);
            builder.switch_to_block(check_val_block);
            builder.ins().iconst(types::I64, addend)
        }
        StructFieldAddShape::Local { rhs_slot } => {
            let rhs_slot_off = builder.ins().iconst(types::I64, rhs_slot as i64);
            let rhs_slot_abs = builder.ins().iadd(slot_offset_val, rhs_slot_off);
            let rhs_byte_off = builder.ins().imul(rhs_slot_abs, value_size);
            let rhs_val_ptr = builder.ins().iadd(stack_ptr, rhs_byte_off);
            let rhs_tag = builder.ins().load(types::I8, flags, rhs_val_ptr, 0);
            let int_tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
            let rhs_is_int = builder.ins().icmp(IntCC::Equal, rhs_tag, int_tag);
            builder
                .ins()
                .brif(rhs_is_int, check_val_block, &[], miss_block, &[]);
            builder.switch_to_block(check_val_block);
            builder
                .ins()
                .load(types::I64, flags, rhs_val_ptr, VALUE_INT_PAYLOAD_OFFSET as i32)
        }
    };

    // 5. Load fields_ptr + field_index, compute slot_ptr, guard Integer tag.
    let fields_ptr = builder
        .ins()
        .load(ptr_ty, flags, inst_ptr, STRUCT_INSTANCE_FIELDS_PTR_OFFSET as i32);
    let field_index = builder
        .ins()
        .load(types::I32, flags, cache_val, FieldCacheEntry::OFFSET_FIELD_INDEX);
    let field_index_i64 = builder.ins().uextend(types::I64, field_index);
    let slot_byte_off = builder.ins().imul(field_index_i64, value_size);
    let slot_ptr = builder.ins().iadd(fields_ptr, slot_byte_off);
    let cur_tag = builder.ins().load(types::I8, flags, slot_ptr, 0);
    let int_tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
    let cur_is_int = builder.ins().icmp(IntCC::Equal, cur_tag, int_tag);
    builder
        .ins()
        .brif(cur_is_int, fast_block, &[], miss_block, &[]);

    // 6. Fast path — update the i64 payload in place.
    builder.switch_to_block(fast_block);
    let cur_payload =
        builder
            .ins()
            .load(types::I64, flags, slot_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
    let new_payload = builder.ins().iadd(cur_payload, rhs_payload);
    builder
        .ins()
        .store(flags, new_payload, slot_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
    builder.ins().jump(ok_block, &[]);

    // 7. Miss path — call the runtime helper (which falls back to the
    // interpreter's `handle_get_field`/`handle_set_field` and repopulates
    // the IC). The helper signature now takes `cache_ptr` as its last arg.
    builder.switch_to_block(miss_block);
    let self_slot_v = builder.ins().iconst(types::I32, self_slot as i64);
    let field_idx_v = builder.ins().iconst(types::I32, field_idx as i64);
    let status = match shape {
        StructFieldAddShape::Const { addend } => {
            let addend_v = builder.ins().iconst(types::I64, addend);
            let call = builder.ins().call(
                refs.struct_field_add_const,
                &[vm_val, self_slot_v, field_idx_v, addend_v, cache_val],
            );
            builder.inst_results(call)[0]
        }
        StructFieldAddShape::Local { rhs_slot } => {
            let rhs_slot_v = builder.ins().iconst(types::I32, rhs_slot as i64);
            let call = builder.ins().call(
                refs.struct_field_add_local,
                &[vm_val, self_slot_v, field_idx_v, rhs_slot_v, cache_val],
            );
            builder.inst_results(call)[0]
        }
    };
    builder
        .ins()
        .brif(status, err_block, &[status], ok_block, &[]);

    builder.switch_to_block(err_block);
    let err_status = builder.block_params(err_block)[0];
    builder.ins().return_(&[err_status]);

    builder.switch_to_block(ok_block);
}

/// Inline `jit_stack_pop_one`: decrement `stack_view.len`. Safe only
/// when the popped slot is guaranteed to hold a primitive-tag value
/// (no `Drop` side-effect); callers must enforce this gating.
#[inline]
fn emit_inline_stack_pop_one(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlags;
    let flags = MemFlags::trusted();
    let top = emit_load_stack_len(builder, vm_val);
    let one = builder.ins().iconst(types::I64, 1);
    let new_top = builder.ins().isub(top, one);
    builder
        .ins()
        .store(flags, new_top, vm_val, vm_stack_view_len_offset());
}

/// Inline `jit_push_integer_inline`: write Value::Integer(payload) at
/// the current stack top and bump the length. Replaces a full ABI-cost
/// FFI call with ~9 Cranelift instructions.
///
/// Layout guarded by `value_size_is_pinned_at_40` and
/// `value_integer_layout_is_pinned` tests in vm/value.rs.
#[inline]
fn emit_inline_push_integer(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
    payload: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlags;
    let flags = MemFlags::trusted();
    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let top = emit_load_stack_len(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let byte_off = builder.ins().imul(top, value_size);
    let slot_ptr = builder.ins().iadd(stack_ptr, byte_off);
    let tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
    builder.ins().store(flags, tag, slot_ptr, 0);
    builder
        .ins()
        .store(flags, payload, slot_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
    let one = builder.ins().iconst(types::I64, 1);
    let new_top = builder.ins().iadd(top, one);
    builder
        .ins()
        .store(flags, new_top, vm_val, vm_stack_view_len_offset());
}

// ── B2.1 flush helpers ────────────────────────────────────────────────
//
// The following helpers maintain the core B2.1 invariant: while
// virtualized Int64 locals live authoritatively in Cranelift
// Variables, the backing VM stack slot is always a valid (possibly
// stale) copy. Before any runtime code can observe the stack, these
// helpers write the Variable's current value back into the slot.
//
// `flush_all` is the boundary-op full flush; `spill_live_int_locals`
// and `flush_expr_stack_to_stack_view` are the two pieces. Consumers
// (B2.1c+) pick the right level of flushing based on what's about to
// happen. B2.1b defines the helpers; B2.1c is the first consumer.

/// Store an `i64` value into the VM stack slot for `slot`, keeping the
/// slot's existing tag as Integer. Exact-slot store — DOES NOT push,
/// DOES NOT modify `stack_view.len` or `Vec::len`.
///
/// Used by `spill_live_int_locals` to write a Variable's current value
/// back to its backing slot when runtime code is about to observe the
/// stack.
#[allow(dead_code)] // consumed by B2.1c
fn emit_store_stack_slot_integer(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    slot: u16,
    value: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlags;
    let flags = MemFlags::trusted();
    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let slot_const = builder.ins().iconst(types::I64, slot as i64);
    let abs_slot = builder.ins().iadd(slot_offset_val, slot_const);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let byte_off = builder.ins().imul(abs_slot, value_size);
    let slot_ptr = builder.ins().iadd(stack_ptr, byte_off);
    // Rewrite tag defensively in case the slot carried something else
    // before (shouldn't happen for virtualizable slots, but the cost
    // is 1 byte store — cheap insurance).
    let tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
    builder.ins().store(flags, tag, slot_ptr, 0);
    builder
        .ins()
        .store(flags, value, slot_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
}

/// Spill every currently-live virtualized Int64 local back to its
/// backing VM stack slot. v1 is intentionally over-conservative:
/// spills ALL live slots, not only those written since the last
/// spill. The dirty-set refinement is deferred to B2.1f.
///
/// Does NOT modify `stack_view.len` — the backing slots already exist
/// (they were materialized at initialization), we just update their
/// contents to reflect the Variables' current values.
///
/// Iterates `live_int_slots` in sorted order for deterministic IR
/// output (easier to diff across runs).
#[allow(dead_code)] // consumed by B2.1c
fn spill_live_int_locals(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    int_locals: &HashMap<u16, Variable>,
    live_int_slots: &HashSet<u16>,
) {
    let mut slots: Vec<u16> = live_int_slots.iter().copied().collect();
    slots.sort_unstable();
    for slot in slots {
        if let Some(&var) = int_locals.get(&slot) {
            let val = builder.use_var(var);
            emit_store_stack_slot_integer(builder, vm_val, slot_offset_val, slot, val);
        }
    }
}

/// Materialize every staged expression-stack temporary onto the real VM
/// stack via `emit_inline_push_integer`. Drains `expr_stack` (leaves it
/// empty). Each push bumps `stack_view.len`, so the VM's view matches
/// the logical depth after the call.
///
/// Does NOT sync `Vec::len` — that's `commit_stack_len`'s job. See
/// `flush_all`.
#[allow(dead_code)] // consumed by B2.1c
fn flush_expr_stack_to_stack_view(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
    expr_stack: &mut Vec<cranelift_codegen::ir::Value>,
) {
    for val in expr_stack.drain(..) {
        emit_inline_push_integer(builder, vm_val, val);
    }
}

/// Full flush: spill live Int64 locals to their backing slots,
/// materialize expr-stack temporaries onto the VM stack, and commit
/// `Vec::len` so runtime helpers see a consistent backing store.
///
/// Call before any opcode that might let generic runtime code
/// (helpers, interpreter takeover, error-handling paths, etc.)
/// observe the stack. After this call, every virtualized value
/// lives only on the VM stack; Cranelift `Variable`s may be stale
/// (caller is free to re-def_var after the helper returns if the
/// call doesn't invalidate them).
#[allow(dead_code)] // consumed by B2.1c
fn flush_all(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    int_locals: &HashMap<u16, Variable>,
    live_int_slots: &HashSet<u16>,
    expr_stack: &mut Vec<cranelift_codegen::ir::Value>,
) {
    spill_live_int_locals(builder, vm_val, slot_offset_val, int_locals, live_int_slots);
    flush_expr_stack_to_stack_view(builder, vm_val, expr_stack);
    // Sync Vec::len to stack_view.len. The existing stack_commit_len
    // helper handles this — same path MethodCall's inline IC uses.
    let new_len = emit_load_stack_len(builder, vm_val);
    builder.ins().call(refs.stack_commit_len, &[vm_val, new_len]);
}

/// Inline the no-upvalue fast path of `jit_op_return`. Only safe when
/// `scan_info.may_capture_upvalues == false` — the function body does
/// not contain `OpCode::Closure`, so no upvalue ever points into this
/// frame's slot range and `close_upvalues` is provably a no-op.
///
/// Currently unused: the helper-based return path is faster because
/// `jit_stack_truncate` must still walk-and-drop each local slot, and
/// that drop loop adds up. Kept for future work: if scan becomes
/// precise enough to prove "all locals are primitives for this
/// function," we can swap in a drop-free truncate and activate this.
#[inline]
#[allow(dead_code)]
fn emit_inline_op_return(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlags;
    let flags = MemFlags::trusted();

    // Load slot_offset of the current (about-to-pop) JitFrame.
    let slot_off = emit_load_top_jit_frame_slot_offset(builder, vm_val);

    // Load the return Value's source pointer (stack[top-1]).
    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let top = emit_load_stack_len(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let one = builder.ins().iconst(types::I64, 1);
    let top_idx = builder.ins().isub(top, one);
    let src_byte = builder.ins().imul(top_idx, value_size);
    let src_ptr = builder.ins().iadd(stack_ptr, src_byte);

    // Read the return Value into VALUE_SIZE-worth of i64s (on the
    // Cranelift side they live as SSA values and get spilled if needed).
    let mut words = Vec::with_capacity(VALUE_SIZE / 8);
    for off in (0..VALUE_SIZE).step_by(8) {
        words.push(
            builder
                .ins()
                .load(types::I64, flags, src_ptr, off as i32),
        );
    }

    // Pop the JitFrame.
    let jf_len = builder
        .ins()
        .load(types::I64, flags, vm_val, vm_jit_frame_view_len_offset());
    let new_jf_len = builder.ins().isub(jf_len, one);
    builder
        .ins()
        .store(flags, new_jf_len, vm_val, vm_jit_frame_view_len_offset());

    // Truncate the stack to slot_offset via a helper so Rc-bearing locals
    // get their Drop. This is the one FFI we keep in the inline path,
    // for correctness.
    builder
        .ins()
        .call(refs.stack_truncate, &[vm_val, slot_off]);

    // After stack_truncate, Vec::len == slot_off and stack_view.len ==
    // slot_off. Write the return Value at stack[slot_off] via raw
    // stores, then bump stack_view.len by 1.
    let dst_byte = builder.ins().imul(slot_off, value_size);
    let dst_ptr = builder.ins().iadd(stack_ptr, dst_byte);
    for (i, w) in words.iter().enumerate() {
        builder
            .ins()
            .store(flags, *w, dst_ptr, (i * 8) as i32);
    }
    let new_top = builder.ins().iadd(slot_off, one);
    builder
        .ins()
        .store(flags, new_top, vm_val, vm_stack_view_len_offset());
}

/// Inline `jit_replace_top2_with_bool` for the integer fast path.
/// Callers are inside int-fast-cmp fast blocks, so the top two values
/// are guaranteed Integer (no Drop side-effect). Writes Value::Boolean
/// at stack[len-2], then decrements len by 1.
///
/// Layout: Bool tag = 3 (pinned by `value_bool_tag_and_payload_are_pinned`
/// in vm/value.rs), payload byte at offset 1.
#[inline]
fn emit_inline_replace_top2_with_bool(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
    bool_val: cranelift_codegen::ir::Value, // i32, nonzero => true
) {
    use cranelift_codegen::ir::MemFlags;
    let flags = MemFlags::trusted();
    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let top = emit_load_stack_len(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let two = builder.ins().iconst(types::I64, 2);
    let target_idx = builder.ins().isub(top, two);
    let target_byte = builder.ins().imul(target_idx, value_size);
    let target_ptr = builder.ins().iadd(stack_ptr, target_byte);
    let bool_tag = builder.ins().iconst(types::I8, 3);
    builder.ins().store(flags, bool_tag, target_ptr, 0);
    let bool_i8 = builder.ins().ireduce(types::I8, bool_val);
    builder.ins().store(flags, bool_i8, target_ptr, 1);
    let one = builder.ins().iconst(types::I64, 1);
    let new_top = builder.ins().isub(top, one);
    builder
        .ins()
        .store(flags, new_top, vm_val, vm_stack_view_len_offset());
}

/// Inline `jit_pop` for the common case where the top-of-stack holds
/// a primitive-tag value (no `Drop` side-effect). Reads the top tag
/// byte, and if it's in the primitive range (0..=6), decrements
/// `stack_view.len` inline. Otherwise falls back to the helper so
/// Rc-bearing values get their Drop run correctly.
#[inline]
fn emit_inline_pop_tag_gated(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlags;
    use cranelift_codegen::ir::condcodes::IntCC;
    let flags = MemFlags::trusted();
    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let top = emit_load_stack_len(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let one = builder.ins().iconst(types::I64, 1);
    let top_idx = builder.ins().isub(top, one);
    let byte_off = builder.ins().imul(top_idx, value_size);
    let slot_ptr = builder.ins().iadd(stack_ptr, byte_off);
    let tag = builder.ins().load(types::I8, flags, slot_ptr, 0);
    // Primitive tags are 0..=6 (Integer..None); heap-backed start at 7.
    let six = builder.ins().iconst(types::I8, 6);
    let is_primitive = builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, tag, six);
    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let cont_block = builder.create_block();
    builder
        .ins()
        .brif(is_primitive, fast_block, &[], slow_block, &[]);

    builder.switch_to_block(fast_block);
    builder
        .ins()
        .store(flags, top_idx, vm_val, vm_stack_view_len_offset());
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(slow_block);
    builder.ins().call(refs.pop, &[vm_val]);
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(cont_block);
}

/// Inline `jit_push_float_inline`: same shape as the integer version
/// with `VALUE_TAG_FLOAT` and the f64 bit pattern at payload offset.
#[inline]
fn emit_inline_push_float(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
    bits: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlags;
    let flags = MemFlags::trusted();
    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let top = emit_load_stack_len(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let byte_off = builder.ins().imul(top, value_size);
    let slot_ptr = builder.ins().iadd(stack_ptr, byte_off);
    let tag = builder.ins().iconst(types::I8, VALUE_TAG_FLOAT as i64);
    builder.ins().store(flags, tag, slot_ptr, 0);
    builder
        .ins()
        .store(flags, bits, slot_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
    let one = builder.ins().iconst(types::I64, 1);
    let new_top = builder.ins().iadd(top, one);
    builder
        .ins()
        .store(flags, new_top, vm_val, vm_stack_view_len_offset());
}

fn emit_int_fast_divmod(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    is_modulo: bool,
    slow_helper: FuncRef,
) {
    use cranelift_codegen::ir::MemFlags;
    use cranelift_codegen::ir::condcodes::IntCC;

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let stack_len = emit_load_stack_len(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let one = builder.ins().iconst(types::I64, 1);
    let two = builder.ins().iconst(types::I64, 2);
    let len_minus_1 = builder.ins().isub(stack_len, one);
    let len_minus_2 = builder.ins().isub(stack_len, two);
    let off_b = builder.ins().imul(len_minus_1, value_size);
    let off_a = builder.ins().imul(len_minus_2, value_size);
    let val_b_ptr = builder.ins().iadd(stack_ptr, off_b);
    let val_a_ptr = builder.ins().iadd(stack_ptr, off_a);

    let flags = MemFlags::trusted();
    let tag_b = builder.ins().load(types::I8, flags, val_b_ptr, 0);
    let tag_a = builder.ins().load(types::I8, flags, val_a_ptr, 0);
    let int_tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
    let a_is_int = builder.ins().icmp(IntCC::Equal, tag_a, int_tag);
    let b_is_int = builder.ins().icmp(IntCC::Equal, tag_b, int_tag);
    let both_int = builder.ins().band(a_is_int, b_is_int);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let continue_block = builder.create_block();
    builder
        .ins()
        .brif(both_int, fast_block, &[], slow_block, &[]);

    builder.switch_to_block(fast_block);
    let lhs = builder.ins().load(
        types::I64,
        flags,
        val_a_ptr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    let rhs = builder.ins().load(
        types::I64,
        flags,
        val_b_ptr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    let rhs_nonzero = builder
        .ins()
        .icmp_imm(IntCC::NotEqual, rhs, 0);
    let nonzero_block = builder.create_block();
    let fast_math_block = builder.create_block();
    builder
        .ins()
        .brif(rhs_nonzero, nonzero_block, &[], slow_block, &[]);

    builder.switch_to_block(nonzero_block);
    let lhs_min = builder.ins().icmp_imm(IntCC::Equal, lhs, i64::MIN);
    let rhs_neg1 = builder.ins().icmp_imm(IntCC::Equal, rhs, -1);
    let overflow = builder.ins().band(lhs_min, rhs_neg1);
    builder
        .ins()
        .brif(overflow, slow_block, &[], fast_math_block, &[]);

    builder.switch_to_block(fast_math_block);
    let result = if is_modulo {
        builder.ins().srem(lhs, rhs)
    } else {
        builder.ins().sdiv(lhs, rhs)
    };
    builder
        .ins()
        .store(flags, result, val_a_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
    // Inline pop — same rationale as emit_int_fast_arith.
    emit_inline_stack_pop_one(builder, vm_val);
    builder.ins().jump(continue_block, &[]);

    builder.switch_to_block(slow_block);
    let call = builder.ins().call(slow_helper, &[vm_val]);
    let status = builder.inst_results(call)[0];
    let err_block = builder.create_block();
    builder
        .ins()
        .brif(status, err_block, &[], continue_block, &[]);
    builder.switch_to_block(err_block);
    builder.ins().return_(&[status]);

    builder.switch_to_block(continue_block);
}

/// Emit IR that increments a `Cell<u64>` counter at a fixed address.
/// The address is baked in as an immediate; since `JitCounters` lives in
/// a `Box` owned by `JitInner`, the address is stable for the JIT's
/// lifetime. Emits load + iadd_imm + store — 3 ops.
#[inline]
fn emit_counter_bump(
    builder: &mut FunctionBuilder<'_>,
    counters_ptr: *const JitCounters,
    offset: isize,
) {
    use cranelift_codegen::ir::MemFlags;
    let addr = unsafe { (counters_ptr as *const u8).offset(offset) } as i64;
    let ptr = builder.ins().iconst(types::I64, addr);
    let flags = MemFlags::trusted();
    let cur = builder.ins().load(types::I64, flags, ptr, 0);
    let next = builder.ins().iadd_imm(cur, 1);
    builder.ins().store(flags, next, ptr, 0);
}

/// Byte offset of each `JitCounters` field from the struct base.
/// The struct is `#[repr(C)]` so these are stable across Rust versions.
/// Fields are `Cell<u64>` which is 8 bytes and identical to `u64`.
#[allow(dead_code)] // consumed by later Plan A stages (A2/A3/A5)
mod counter_offsets {
    use super::JitCounters;
    use std::mem::offset_of;
    pub const SPECIALIZED_ENTRY_COMPILED: isize =
        offset_of!(JitCounters, specialized_entry_compiled) as isize;
    pub const SPECIALIZED_ENTRY_CALLED: isize =
        offset_of!(JitCounters, specialized_entry_called) as isize;
    pub const SPECIALIZED_CALL_IC_HIT: isize =
        offset_of!(JitCounters, specialized_call_ic_hit) as isize;
    pub const SPECIALIZED_CALL_IC_MISS: isize =
        offset_of!(JitCounters, specialized_call_ic_miss) as isize;
    pub const SPECIALIZED_CALL_NO_ENTRY: isize =
        offset_of!(JitCounters, specialized_call_no_entry) as isize;
    pub const SPECIALIZED_CALL_ARITY_MISMATCH: isize =
        offset_of!(JitCounters, specialized_call_arity_mismatch) as isize;
    pub const SPECIALIZED_CALL_BAILOUT: isize =
        offset_of!(JitCounters, specialized_call_bailout) as isize;
    pub const SELF_RECURSION_DIRECT_CALL: isize =
        offset_of!(JitCounters, self_recursion_direct_call) as isize;
    pub const VIRT_DIVMOD_FAST: isize = offset_of!(JitCounters, virt_divmod_fast) as isize;
    pub const VIRT_DIVMOD_SLOW_ZERO: isize =
        offset_of!(JitCounters, virt_divmod_slow_zero) as isize;
    pub const VIRT_DIVMOD_SLOW_OVERFLOW: isize =
        offset_of!(JitCounters, virt_divmod_slow_overflow) as isize;
}

/// Load the `i64` payload from the Value currently at stack[stack_view.len - 1].
/// Used by `emit_int_virt_divmod`'s slow path to harvest the helper's
/// pushed result without going through a helper call of its own.
#[inline]
fn emit_load_stack_top_integer_payload(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    use cranelift_codegen::ir::MemFlags;
    let flags = MemFlags::trusted();
    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let stack_len = emit_load_stack_len(builder, vm_val);
    let one = builder.ins().iconst(types::I64, 1);
    let top_idx = builder.ins().isub(stack_len, one);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let top_off = builder.ins().imul(top_idx, value_size);
    let top_addr = builder.ins().iadd(stack_ptr, top_off);
    builder
        .ins()
        .load(types::I64, flags, top_addr, VALUE_INT_PAYLOAD_OFFSET as i32)
}

/// Virtual Int Divide / Modulo: operands are register-resident Cranelift
/// SSA values (never loaded from the VM stack). Emits zero + i64::MIN/-1
/// guards on the fast path; falls back to the existing runtime helper via
/// a slow path that first re-boxes both operands onto the VM stack.
///
/// Mirrors `emit_int_fast_divmod`'s guard shape but accepts operands in
/// registers so the common case (rhs != 0 and !(i64::MIN / -1)) does zero
/// memory traffic.
fn emit_int_virt_divmod(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
    is_modulo: bool,
    lhs: cranelift_codegen::ir::Value,
    rhs: cranelift_codegen::ir::Value,
    slow_helper: FuncRef,
    counters_ptr: Option<*const JitCounters>,
) -> cranelift_codegen::ir::Value {
    use cranelift_codegen::ir::condcodes::IntCC;

    let rhs_nonzero = builder.ins().icmp_imm(IntCC::NotEqual, rhs, 0);
    let nonzero_block = builder.create_block();
    let zero_slow_block = builder.create_block();
    let overflow_slow_block = builder.create_block();
    let slow_block = builder.create_block();
    let fast_block = builder.create_block();
    let cont_block = builder.create_block();
    builder.append_block_param(cont_block, types::I64);

    builder
        .ins()
        .brif(rhs_nonzero, nonzero_block, &[], zero_slow_block, &[]);

    // Zero-divisor pre-slow block: bump counter + jump to shared slow.
    builder.switch_to_block(zero_slow_block);
    if let Some(cp) = counters_ptr {
        emit_counter_bump(builder, cp, counter_offsets::VIRT_DIVMOD_SLOW_ZERO);
    }
    builder.ins().jump(slow_block, &[]);

    // i64::MIN / -1 overflow guard.
    builder.switch_to_block(nonzero_block);
    let lhs_min = builder.ins().icmp_imm(IntCC::Equal, lhs, i64::MIN);
    let rhs_neg1 = builder.ins().icmp_imm(IntCC::Equal, rhs, -1);
    let overflow = builder.ins().band(lhs_min, rhs_neg1);
    builder
        .ins()
        .brif(overflow, overflow_slow_block, &[], fast_block, &[]);

    // Overflow pre-slow block: bump counter + jump to shared slow.
    builder.switch_to_block(overflow_slow_block);
    if let Some(cp) = counters_ptr {
        emit_counter_bump(builder, cp, counter_offsets::VIRT_DIVMOD_SLOW_OVERFLOW);
    }
    builder.ins().jump(slow_block, &[]);

    // Fast path: register sdiv / srem.
    builder.switch_to_block(fast_block);
    if let Some(cp) = counters_ptr {
        emit_counter_bump(builder, cp, counter_offsets::VIRT_DIVMOD_FAST);
    }
    let result = if is_modulo {
        builder.ins().srem(lhs, rhs)
    } else {
        builder.ins().sdiv(lhs, rhs)
    };
    builder.ins().jump(cont_block, &[result]);

    // Slow path: re-box both operands as Value::Integer onto the VM
    // stack, call the existing helper (which handles div-by-zero and
    // overflow errors), then read the result back as i64 payload and
    // pop the helper-pushed result Value. Shared entry from both
    // zero_slow_block and overflow_slow_block.
    builder.switch_to_block(slow_block);
    emit_inline_push_integer(builder, vm_val, lhs);
    emit_inline_push_integer(builder, vm_val, rhs);
    let call = builder.ins().call(slow_helper, &[vm_val]);
    let status = builder.inst_results(call)[0];
    let err_block = builder.create_block();
    let slow_ok_block = builder.create_block();
    builder
        .ins()
        .brif(status, err_block, &[], slow_ok_block, &[]);
    builder.switch_to_block(err_block);
    builder.ins().return_(&[status]);
    builder.switch_to_block(slow_ok_block);
    let payload = emit_load_stack_top_integer_payload(builder, vm_val);
    emit_inline_stack_pop_one(builder, vm_val);
    builder.ins().jump(cont_block, &[payload]);

    builder.switch_to_block(cont_block);
    builder.block_params(cont_block)[0]
}

/// Peephole fast path for `GetLocal(slot); Constant(int); Arith;
/// SetLocal(slot); [Pop]`. On integer locals this updates the local
/// payload in place and skips the temporary stack traffic. Non-integer
/// locals replay the original helper sequence so mixed numeric/string
/// semantics stay unchanged.
fn emit_inline_local_const_arith_update(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    slot: u16,
    constant: i64,
    op: IntArithOp,
    pop_after: bool,
) {
    use cranelift_codegen::ir::MemFlags;
    use cranelift_codegen::ir::condcodes::IntCC;

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);

    let slot_const = builder.ins().iconst(types::I64, slot as i64);
    let abs_slot = builder.ins().iadd(slot_offset_val, slot_const);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let byte_off = builder.ins().imul(abs_slot, value_size);
    let addr_slot = builder.ins().iadd(stack_ptr, byte_off);

    let flags = MemFlags::trusted();
    let tag = builder.ins().load(types::I8, flags, addr_slot, 0);
    let int_tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
    let is_int = builder.ins().icmp(IntCC::Equal, tag, int_tag);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let cont_block = builder.create_block();

    builder.ins().brif(is_int, fast_block, &[], slow_block, &[]);

    builder.switch_to_block(fast_block);
    let old_payload = builder.ins().load(
        types::I64,
        flags,
        addr_slot,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    let rhs = builder.ins().iconst(types::I64, constant);
    let result = match op {
        IntArithOp::Add => builder.ins().iadd(old_payload, rhs),
        IntArithOp::Sub => builder.ins().isub(old_payload, rhs),
        IntArithOp::Mul => builder.ins().imul(old_payload, rhs),
    };
    builder
        .ins()
        .store(flags, result, addr_slot, VALUE_INT_PAYLOAD_OFFSET as i32);
    if !pop_after {
        emit_inline_push_integer(builder, vm_val, result);
    }
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(slow_block);
    let slot_val = builder.ins().iconst(types::I32, slot as i64);
    builder.ins().call(refs.get_local, &[vm_val, slot_val]);
    let rhs = builder.ins().iconst(types::I64, constant);
    emit_inline_push_integer(builder, vm_val, rhs);
    let slow_helper = match op {
        IntArithOp::Add => refs.add,
        IntArithOp::Sub => refs.sub,
        IntArithOp::Mul => refs.mul,
    };
    let call = builder.ins().call(slow_helper, &[vm_val]);
    let status = builder.inst_results(call)[0];
    let err_block = builder.create_block();
    let set_block = builder.create_block();
    builder.ins().brif(status, err_block, &[], set_block, &[]);
    builder.switch_to_block(err_block);
    builder.ins().return_(&[status]);

    builder.switch_to_block(set_block);
    builder.ins().call(refs.set_local, &[vm_val, slot_val]);
    if pop_after {
        builder.ins().call(refs.pop, &[vm_val]);
    }
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(cont_block);
}

/// Peephole fast path for `GetLocal(dst); GetLocal(rhs); Constant(int);
/// Multiply; Add/Subtract; SetLocal(dst); [Pop]`. This covers tight-loop
/// updates like `total := total + i * 2` without building the temporary
/// stack values on the integer path.
fn emit_inline_local_scaled_arith_update(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    dst_slot: u16,
    rhs_slot: u16,
    constant: i64,
    op: IntArithOp,
    pop_after: bool,
) {
    use cranelift_codegen::ir::MemFlags;
    use cranelift_codegen::ir::condcodes::IntCC;

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);

    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let dst_slot_const = builder.ins().iconst(types::I64, dst_slot as i64);
    let rhs_slot_const = builder.ins().iconst(types::I64, rhs_slot as i64);

    let abs_dst = builder.ins().iadd(slot_offset_val, dst_slot_const);
    let dst_off = builder.ins().imul(abs_dst, value_size);
    let addr_dst = builder.ins().iadd(stack_ptr, dst_off);

    let abs_rhs = builder.ins().iadd(slot_offset_val, rhs_slot_const);
    let rhs_off = builder.ins().imul(abs_rhs, value_size);
    let addr_rhs = builder.ins().iadd(stack_ptr, rhs_off);

    let flags = MemFlags::trusted();
    let dst_tag = builder.ins().load(types::I8, flags, addr_dst, 0);
    let rhs_tag = builder.ins().load(types::I8, flags, addr_rhs, 0);
    let int_tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
    let dst_is_int = builder.ins().icmp(IntCC::Equal, dst_tag, int_tag);
    let rhs_is_int = builder.ins().icmp(IntCC::Equal, rhs_tag, int_tag);
    let both_int = builder.ins().band(dst_is_int, rhs_is_int);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let cont_block = builder.create_block();

    builder
        .ins()
        .brif(both_int, fast_block, &[], slow_block, &[]);

    builder.switch_to_block(fast_block);
    let dst_payload =
        builder
            .ins()
            .load(types::I64, flags, addr_dst, VALUE_INT_PAYLOAD_OFFSET as i32);
    let rhs_payload =
        builder
            .ins()
            .load(types::I64, flags, addr_rhs, VALUE_INT_PAYLOAD_OFFSET as i32);
    let scale = builder.ins().iconst(types::I64, constant);
    let scaled_rhs = builder.ins().imul(rhs_payload, scale);
    let result = match op {
        IntArithOp::Add => builder.ins().iadd(dst_payload, scaled_rhs),
        IntArithOp::Sub => builder.ins().isub(dst_payload, scaled_rhs),
        IntArithOp::Mul => unreachable!("scaled update only supports Add/Subtract"),
    };
    builder
        .ins()
        .store(flags, result, addr_dst, VALUE_INT_PAYLOAD_OFFSET as i32);
    if !pop_after {
        emit_inline_push_integer(builder, vm_val, result);
    }
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(slow_block);
    let dst_slot_val = builder.ins().iconst(types::I32, dst_slot as i64);
    let rhs_slot_val = builder.ins().iconst(types::I32, rhs_slot as i64);
    builder.ins().call(refs.get_local, &[vm_val, dst_slot_val]);
    builder.ins().call(refs.get_local, &[vm_val, rhs_slot_val]);
    let scale = builder.ins().iconst(types::I64, constant);
    emit_inline_push_integer(builder, vm_val, scale);

    let mul_call = builder.ins().call(refs.mul, &[vm_val]);
    let mul_status = builder.inst_results(mul_call)[0];
    let mul_err_block = builder.create_block();
    let arith_block = builder.create_block();
    builder
        .ins()
        .brif(mul_status, mul_err_block, &[], arith_block, &[]);
    builder.switch_to_block(mul_err_block);
    builder.ins().return_(&[mul_status]);

    builder.switch_to_block(arith_block);
    let slow_helper = match op {
        IntArithOp::Add => refs.add,
        IntArithOp::Sub => refs.sub,
        IntArithOp::Mul => unreachable!("scaled update only supports Add/Subtract"),
    };
    let arith_call = builder.ins().call(slow_helper, &[vm_val]);
    let arith_status = builder.inst_results(arith_call)[0];
    let arith_err_block = builder.create_block();
    let set_block = builder.create_block();
    builder
        .ins()
        .brif(arith_status, arith_err_block, &[], set_block, &[]);
    builder.switch_to_block(arith_err_block);
    builder.ins().return_(&[arith_status]);

    builder.switch_to_block(set_block);
    builder.ins().call(refs.set_local, &[vm_val, dst_slot_val]);
    if pop_after {
        builder.ins().call(refs.pop, &[vm_val]);
    }
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(cont_block);
}

/// Emit IR for a comparison opcode that has an inline int+int fast
/// path. Mirrors `emit_int_fast_arith`: both top-of-stack tags are
/// checked for `VALUE_TAG_INTEGER`, and if the fast path is taken we
/// compare the two i64 payloads with `cc` and push back a boolean via
/// `jit_replace_top2_with_bool`. Otherwise we fall through to
/// `slow_helper`.
fn emit_int_fast_cmp(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    cc: cranelift_codegen::ir::condcodes::IntCC,
    slow_helper: FuncRef,
) {
    use cranelift_codegen::ir::MemFlags;

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let stack_len = emit_load_stack_len(builder, vm_val);

    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let one = builder.ins().iconst(types::I64, 1);
    let two = builder.ins().iconst(types::I64, 2);
    let len_minus_1 = builder.ins().isub(stack_len, one);
    let len_minus_2 = builder.ins().isub(stack_len, two);
    let off_b = builder.ins().imul(len_minus_1, value_size);
    let off_a = builder.ins().imul(len_minus_2, value_size);
    let val_b_ptr = builder.ins().iadd(stack_ptr, off_b);
    let val_a_ptr = builder.ins().iadd(stack_ptr, off_a);

    let flags = MemFlags::trusted();
    let tag_b = builder.ins().load(types::I8, flags, val_b_ptr, 0);
    let tag_a = builder.ins().load(types::I8, flags, val_a_ptr, 0);
    let int_tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
    let a_is_int = builder.ins().icmp(
        cranelift_codegen::ir::condcodes::IntCC::Equal,
        tag_a,
        int_tag,
    );
    let b_is_int = builder.ins().icmp(
        cranelift_codegen::ir::condcodes::IntCC::Equal,
        tag_b,
        int_tag,
    );
    let both_int = builder.ins().band(a_is_int, b_is_int);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let continue_block = builder.create_block();

    builder
        .ins()
        .brif(both_int, fast_block, &[], slow_block, &[]);

    // ── Fast path ──
    builder.switch_to_block(fast_block);
    let payload_a = builder.ins().load(
        types::I64,
        flags,
        val_a_ptr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    let payload_b = builder.ins().load(
        types::I64,
        flags,
        val_b_ptr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    let cmp = builder.ins().icmp(cc, payload_a, payload_b);
    // `icmp` returns i8 (0 or 1); widen to i32 for the helper's u32 param.
    let cmp_u32 = builder.ins().uextend(types::I32, cmp);
    emit_inline_replace_top2_with_bool(builder, vm_val, cmp_u32);
    builder.ins().jump(continue_block, &[]);

    // ── Slow path ──
    builder.switch_to_block(slow_block);
    let call = builder.ins().call(slow_helper, &[vm_val]);
    let status = builder.inst_results(call)[0];
    let err_block = builder.create_block();
    builder
        .ins()
        .brif(status, err_block, &[], continue_block, &[]);
    builder.switch_to_block(err_block);
    builder.ins().return_(&[status]);

    builder.switch_to_block(continue_block);
}

fn emit_int_fast_eq(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    is_equal: bool,
) {
    use cranelift_codegen::ir::MemFlags;
    use cranelift_codegen::ir::condcodes::IntCC;

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let stack_len = emit_load_stack_len(builder, vm_val);

    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let one = builder.ins().iconst(types::I64, 1);
    let two = builder.ins().iconst(types::I64, 2);
    let len_minus_1 = builder.ins().isub(stack_len, one);
    let len_minus_2 = builder.ins().isub(stack_len, two);
    let off_b = builder.ins().imul(len_minus_1, value_size);
    let off_a = builder.ins().imul(len_minus_2, value_size);
    let val_b_ptr = builder.ins().iadd(stack_ptr, off_b);
    let val_a_ptr = builder.ins().iadd(stack_ptr, off_a);

    let flags = MemFlags::trusted();
    let tag_b = builder.ins().load(types::I8, flags, val_b_ptr, 0);
    let tag_a = builder.ins().load(types::I8, flags, val_a_ptr, 0);
    let int_tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
    let a_is_int = builder.ins().icmp(IntCC::Equal, tag_a, int_tag);
    let b_is_int = builder.ins().icmp(IntCC::Equal, tag_b, int_tag);
    let both_int = builder.ins().band(a_is_int, b_is_int);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let continue_block = builder.create_block();
    builder
        .ins()
        .brif(both_int, fast_block, &[], slow_block, &[]);

    builder.switch_to_block(fast_block);
    let payload_a = builder.ins().load(
        types::I64,
        flags,
        val_a_ptr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    let payload_b = builder.ins().load(
        types::I64,
        flags,
        val_b_ptr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    let cc = if is_equal {
        IntCC::Equal
    } else {
        IntCC::NotEqual
    };
    let cmp = builder.ins().icmp(cc, payload_a, payload_b);
    let cmp_u32 = builder.ins().uextend(types::I32, cmp);
    emit_inline_replace_top2_with_bool(builder, vm_val, cmp_u32);
    builder.ins().jump(continue_block, &[]);

    builder.switch_to_block(slow_block);
    let helper = if is_equal { refs.eq } else { refs.ne };
    builder.ins().call(helper, &[vm_val]);
    builder.ins().jump(continue_block, &[]);

    builder.switch_to_block(continue_block);
}

/// Emit IR for a fused comparison + conditional-branch sequence. On the
/// int+int fast path the top two values are compared directly with `icmp`
/// and then branched on in a single `brif`, without ever materialising the
/// intermediate `Value::Boolean` or calling `jit_peek_truthy` /
/// `jit_pop_truthy`. On the slow path we fall back to the normal
/// comparison helper plus the truthy-check helper that the un-fused code
/// would have used.
///
/// Both paths finish by branching to either `target_block` (jump taken) or
/// `fall_block` (jump not taken).
fn emit_fused_int_cmp_branch(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    cc: cranelift_codegen::ir::condcodes::IntCC,
    slow_helper: FuncRef,
    branch_op: OpCode,
    target_block: Block,
    fall_block: Block,
) {
    use cranelift_codegen::ir::MemFlags;

    debug_assert!(matches!(
        branch_op,
        OpCode::JumpIfFalse | OpCode::JumpIfTrue | OpCode::PopJumpIfFalse
    ));

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let stack_len = emit_load_stack_len(builder, vm_val);

    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let one = builder.ins().iconst(types::I64, 1);
    let two = builder.ins().iconst(types::I64, 2);
    let len_minus_1 = builder.ins().isub(stack_len, one);
    let len_minus_2 = builder.ins().isub(stack_len, two);
    let off_b = builder.ins().imul(len_minus_1, value_size);
    let off_a = builder.ins().imul(len_minus_2, value_size);
    let val_b_ptr = builder.ins().iadd(stack_ptr, off_b);
    let val_a_ptr = builder.ins().iadd(stack_ptr, off_a);

    let flags = MemFlags::trusted();
    let tag_b = builder.ins().load(types::I8, flags, val_b_ptr, 0);
    let tag_a = builder.ins().load(types::I8, flags, val_a_ptr, 0);
    let int_tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
    let a_is_int = builder.ins().icmp(
        cranelift_codegen::ir::condcodes::IntCC::Equal,
        tag_a,
        int_tag,
    );
    let b_is_int = builder.ins().icmp(
        cranelift_codegen::ir::condcodes::IntCC::Equal,
        tag_b,
        int_tag,
    );
    let both_int = builder.ins().band(a_is_int, b_is_int);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();

    builder
        .ins()
        .brif(both_int, fast_block, &[], slow_block, &[]);

    // ── Fast path: inline icmp + brif ──
    //
    // Stack bookkeeping depends on the branch opcode, mirroring the
    // non-fused code's net effect:
    //
    // * `JumpIfFalse`/`JumpIfTrue` do NOT pop the Boolean — the compiler
    //   emits an explicit `Pop` on the fall-through side and again on
    //   the jump target. We therefore must leave a `Boolean(cmp)` on
    //   the stack by calling `jit_replace_top2_with_bool`.
    // * `PopJumpIfFalse` pops its Boolean itself. Since the fast path
    //   never materialised one, we can just drop both integer operands.
    builder.switch_to_block(fast_block);
    let payload_a = builder.ins().load(
        types::I64,
        flags,
        val_a_ptr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    let payload_b = builder.ins().load(
        types::I64,
        flags,
        val_b_ptr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    let cmp = builder.ins().icmp(cc, payload_a, payload_b);
    match branch_op {
        OpCode::JumpIfFalse | OpCode::JumpIfTrue => {
            let cmp_u32 = builder.ins().uextend(types::I32, cmp);
            emit_inline_replace_top2_with_bool(builder, vm_val, cmp_u32);
        }
        OpCode::PopJumpIfFalse => {
            let two_u32 = builder.ins().iconst(types::I32, 2);
            builder.ins().call(refs.stack_pop_n, &[vm_val, two_u32]);
        }
        _ => unreachable!(),
    }
    match branch_op {
        OpCode::JumpIfFalse | OpCode::PopJumpIfFalse => {
            // `cmp` is the boolean result; jump to target when it's false.
            builder.ins().brif(cmp, fall_block, &[], target_block, &[]);
        }
        OpCode::JumpIfTrue => {
            builder.ins().brif(cmp, target_block, &[], fall_block, &[]);
        }
        _ => unreachable!(),
    }

    // ── Slow path: comparison helper, then truthy helper, then brif ──
    builder.switch_to_block(slow_block);
    let call = builder.ins().call(slow_helper, &[vm_val]);
    let status = builder.inst_results(call)[0];
    let err_block = builder.create_block();
    let cont_block = builder.create_block();
    builder.ins().brif(status, err_block, &[], cont_block, &[]);
    builder.switch_to_block(err_block);
    builder.ins().return_(&[status]);

    builder.switch_to_block(cont_block);
    // `slow_helper` pushed a `Boolean`; now replicate the branch opcode's
    // own behavior on top of it.
    let truthy_helper = match branch_op {
        OpCode::JumpIfFalse | OpCode::JumpIfTrue => refs.peek_truthy,
        OpCode::PopJumpIfFalse => refs.pop_truthy,
        _ => unreachable!(),
    };
    let tcall = builder.ins().call(truthy_helper, &[vm_val]);
    let truthy = builder.inst_results(tcall)[0];
    match branch_op {
        OpCode::JumpIfFalse | OpCode::PopJumpIfFalse => {
            builder
                .ins()
                .brif(truthy, fall_block, &[], target_block, &[]);
        }
        OpCode::JumpIfTrue => {
            builder
                .ins()
                .brif(truthy, target_block, &[], fall_block, &[]);
        }
        _ => unreachable!(),
    }
}

/// Inline fast path for `GetLocal(slot)`. Reads the tag byte of the
/// local directly from stack memory and, for `Value::Integer` /
/// `Value::Float`, bypasses the generic clone-and-push helper by
/// calling the primitive-specific push helpers (no Rc, no variant
/// dispatch). Falls through to `jit_get_local` for any other tag so
/// correct `Clone` semantics (refcount bumps) keep working for
/// strings, closures, arrays, etc.
fn emit_inline_get_local(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    slot: u16,
) {
    use cranelift_codegen::ir::MemFlags;
    use cranelift_codegen::ir::condcodes::IntCC;

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);

    let slot_const = builder.ins().iconst(types::I64, slot as i64);
    let abs_slot = builder.ins().iadd(slot_offset_val, slot_const);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let byte_off = builder.ins().imul(abs_slot, value_size);
    let addr = builder.ins().iadd(stack_ptr, byte_off);

    let flags = MemFlags::trusted();
    let tag = builder.ins().load(types::I8, flags, addr, 0);
    let int_tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
    let float_tag = builder.ins().iconst(types::I8, VALUE_TAG_FLOAT as i64);

    let int_block = builder.create_block();
    let check_float_block = builder.create_block();
    let float_block = builder.create_block();
    let slow_block = builder.create_block();
    let cont_block = builder.create_block();

    let is_int = builder.ins().icmp(IntCC::Equal, tag, int_tag);
    builder
        .ins()
        .brif(is_int, int_block, &[], check_float_block, &[]);

    builder.switch_to_block(int_block);
    let payload = builder
        .ins()
        .load(types::I64, flags, addr, VALUE_INT_PAYLOAD_OFFSET as i32);
    emit_inline_push_integer(builder, vm_val, payload);
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(check_float_block);
    let is_float = builder.ins().icmp(IntCC::Equal, tag, float_tag);
    builder
        .ins()
        .brif(is_float, float_block, &[], slow_block, &[]);

    builder.switch_to_block(float_block);
    let bits = builder
        .ins()
        .load(types::I64, flags, addr, VALUE_INT_PAYLOAD_OFFSET as i32);
    emit_inline_push_float(builder, vm_val, bits);
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(slow_block);
    let slot_val = builder.ins().iconst(types::I32, slot as i64);
    builder.ins().call(refs.get_local, &[vm_val, slot_val]);
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(cont_block);
}

/// Inline fast path for `SetLocal(slot)`. Mirrors the interpreter's
/// semantics (clone the top-of-stack into the slot, leaving top intact)
/// but bypasses the helper call in the common case where BOTH the new
/// value (top) and the existing slot value are primitive tags (<= 6 =
/// None/Boolean/Byte/Char/Uint/Float/Integer — all have empty `Drop`).
/// That lets us safely bitwise-copy `VALUE_SIZE` bytes from top to slot
/// with no Rc refcount work.
///
/// Falls through to `jit_set_local` when either tag is non-primitive so
/// that `Drop` / `Clone` semantics remain correct for Rc-containing
/// variants.
fn emit_inline_set_local(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    slot: u16,
) {
    use cranelift_codegen::ir::MemFlags;
    use cranelift_codegen::ir::condcodes::IntCC;

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let stack_len = emit_load_stack_len(builder, vm_val);

    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let one = builder.ins().iconst(types::I64, 1);

    // addr_top = stack_ptr + (len - 1) * VALUE_SIZE
    let top_idx = builder.ins().isub(stack_len, one);
    let top_off = builder.ins().imul(top_idx, value_size);
    let addr_top = builder.ins().iadd(stack_ptr, top_off);

    // addr_slot = stack_ptr + (slot_offset + slot) * VALUE_SIZE
    let slot_const = builder.ins().iconst(types::I64, slot as i64);
    let abs_slot = builder.ins().iadd(slot_offset_val, slot_const);
    let slot_off = builder.ins().imul(abs_slot, value_size);
    let addr_slot = builder.ins().iadd(stack_ptr, slot_off);

    let flags = MemFlags::trusted();
    let top_tag = builder.ins().load(types::I8, flags, addr_top, 0);
    let old_tag = builder.ins().load(types::I8, flags, addr_slot, 0);
    // "Primitive" = tags 0..=6. Anything that holds an `Rc` or `RefCell`
    // is >= 7.
    let max_primitive = builder.ins().iconst(types::I8, 6);
    let top_prim = builder
        .ins()
        .icmp(IntCC::UnsignedLessThanOrEqual, top_tag, max_primitive);
    let old_prim = builder
        .ins()
        .icmp(IntCC::UnsignedLessThanOrEqual, old_tag, max_primitive);
    let both_prim = builder.ins().band(top_prim, old_prim);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let cont_block = builder.create_block();

    builder
        .ins()
        .brif(both_prim, fast_block, &[], slow_block, &[]);

    // Fast path: bitwise-copy VALUE_SIZE bytes from top to slot. No
    // Rc/Drop interaction possible because both sides are primitives.
    builder.switch_to_block(fast_block);
    let words = VALUE_SIZE.div_ceil(8);
    for i in 0..words {
        let off = (i * 8) as i32;
        let v = builder.ins().load(types::I64, flags, addr_top, off);
        builder.ins().store(flags, v, addr_slot, off);
    }
    builder.ins().jump(cont_block, &[]);

    // Slow path: generic helper handles `Clone` of the top-of-stack
    // value and `Drop` of whatever was in the slot.
    builder.switch_to_block(slow_block);
    let slot_val = builder.ins().iconst(types::I32, slot as i64);
    builder.ins().call(refs.set_local, &[vm_val, slot_val]);
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(cont_block);
}
