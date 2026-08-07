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
use crate::vm::value::{
    Function, RC_VALUE_OFFSET, STRUCT_INSTANCE_DEF_OFFSET, STRUCT_INSTANCE_FIELDS_PTR_OFFSET,
    VALUE_INT_PAYLOAD_OFFSET, VALUE_SIZE, VALUE_TAG_CLOSURE, VALUE_TAG_FLOAT, VALUE_TAG_INTEGER,
    VALUE_TAG_NONE, VALUE_TAG_STRUCT_INSTANCE,
};
use crate::vm::JitFrame;

use super::scan;
use super::virt_stack::{VirtConst, VirtSlot, VirtStack};
use super::{CompiledEntries, CompiledThunk, SpecializedThunkRaw};

mod cache;
mod counters;
mod defs;
mod helpers;
pub(crate) use cache::{
    CallCacheEntry, FieldCacheEntry, FieldCacheKind, GlobalCacheEntry, MethodCacheEntry,
    MethodInlineKind,
};
pub(crate) use counters::{HelperCounter, JitCounters};
use counters::{counter_offsets, emit_counter_bump};
use defs::{
    Entry, obj_closure_module_globals_offset, vm_jit_frame_view_len_offset,
    vm_jit_frame_view_ptr_offset, vm_stack_view_len_offset, vm_stack_view_ptr_offset,
};
pub(crate) use defs::{EntryKind, InvokeOutcome, SpecializedEntryKind};
use helpers::{HelperIds, HelperRefs, declare_helper_refs, declare_helpers, register_helpers};

// vec_box: IC-cache Box addresses are baked into emitted IR and must not move.
#[allow(clippy::vec_box)]
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

    /// Per-global-name version cells for the GetGlobal ICs. Boxed so the
    /// address is stable (stored in `GlobalCacheEntry.version_cell` and
    /// read from emitted IR). `SetGlobal`/`DefineGlobal` bump only the
    /// written name's cell, so unrelated ICs stay hot.
    global_version_cells: HashMap<String, Box<u64>>,
}


impl JitInner {
    pub fn new() -> Self {
        use cranelift_codegen::settings::Configurable;
        let mut flag_builder = settings::builder();
        // opt_level=speed: matches or beats `none` on every loop bench; other 0.131 knobs regressed.
        flag_builder
            .set("opt_level", "speed")
            .expect("Cranelift accepts opt_level=speed");
        let flags = settings::Flags::new(flag_builder);

        let isa_builder =
            cranelift_native::builder().expect("cranelift-native should detect host ISA");
        let isa = isa_builder
            .finish(flags)
            .expect("ISA builder should finalize");

        let mut jit_builder = JITBuilder::with_isa(isa, default_libcall_names());
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
            global_version_cells: HashMap::new(),
        }
    }

    /// Stable pointer to `name`'s version cell, creating it (at version 0)
    /// on first use. Called by `jit_get_global_ic` when it populates a
    /// cache entry.
    pub(crate) fn global_version_cell(&mut self, name: &str) -> *mut u64 {
        let b = self
            .global_version_cells
            .entry(name.to_string())
            .or_insert_with(|| Box::new(0));
        b.as_mut() as *mut u64
    }

    /// Invalidate every IC caching `name`. Names no IC has ever cached
    /// have no cell and cost only the lookup.
    pub(crate) fn bump_global_version(&mut self, name: &str) {
        if let Some(cell) = self.global_version_cells.get_mut(name) {
            **cell = cell.wrapping_add(1);
        }
    }

    /// Stable raw pointer to the counters block, safe to bake into
    /// emitted IR. The `Box` keeps the allocation pinned for the
    /// JitInner's lifetime.
    pub(crate) fn counters_ptr(&self) -> *const JitCounters {
        self.counters.as_ref() as *const _
    }

    /// Allocate a fresh inline-cache slot for a GetGlobal call site.
    /// Returns a raw pointer whose address is stable for the JitInner's
    /// lifetime (because the Box's heap allocation doesn't move).
    pub(crate) fn alloc_global_cache(&mut self) -> *mut GlobalCacheEntry {
        self.global_caches.push(Box::new(GlobalCacheEntry {
            version: u64::MAX,
            value: crate::vm::value::Value::None,
            version_cell: &cache::GLOBAL_VERSION_SENTINEL,
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
            specialized_kind: 0,
            _pad: [0; 6],
            specialized_thunk: std::ptr::null(),
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
        // Read pending_error volatilely: thunk(vm) aliases self through vm.jit.inner, so LLVM caches a stale None.
        let self_ptr: *mut JitInner = self as *mut JitInner;
        let pending_ptr = unsafe { &raw mut (*self_ptr).pending_error };
        let stop_depth_ptr = unsafe { &raw mut (*self_ptr).current_stop_depth };

        unsafe {
            stop_depth_ptr.write_volatile(stop_depth);
            pending_ptr.write_volatile(None);
        }

        // Save/restore so nested invoke_thunk reads the thunk's frame, not a paused interpreter one.
        let vm_ref = unsafe { &*vm };
        let prev_jit_exec = vm_ref.jit_executing.replace(true);

        let status = unsafe { thunk(vm) };

        vm_ref.jit_executing.set(prev_jit_exec);
        unsafe {
            stop_depth_ptr.write_volatile(0);
        }

        // Inline returns update stack_view.len only, leaving Vec::len stale by +1.
        unsafe {
            (*vm).sync_stack_from_view();
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
                    code: crate::diagnostics::registry::RUNTIME_ERROR,
                    tag: None,
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

        let counters_ptr_opt: Option<*const JitCounters> =
            if std::env::var("OXIGEN_JIT_STATS").is_ok() {
                Some(self.counters.as_ref() as *const _)
            } else {
                None
            };

        // Runs before any IR is emitted so the analysis is stable across the compile.
        let slot_types = crate::compiler::slot_types::analyze(func);
        #[cfg(debug_assertions)]
        {
            if let Err(msg) = crate::compiler::slot_types::certify(func, &slot_types) {
                // Certifier failure is soft: skip virtualization, slot stays on the Value path.
                eprintln!(
                    "[jit] slot_types certify failed for {:?}: {}",
                    func.name.as_deref().unwrap_or("<anonymous>"),
                    msg,
                );
            }
        }

        // Boxes give stable heap addresses to bake into IR.
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
        let mut generic_sig = self.module.make_signature();
        generic_sig.params.push(AbiParam::new(ptr_ty));
        generic_sig.returns.push(AbiParam::new(types::I32));

        self.next_id = self.next_id.wrapping_add(1);
        let generic_seq_id = self.next_id;
        let thunk_name = format!("oxigen_jit_thunk_{}_{:p}", generic_seq_id, Rc::as_ptr(func));
        let thunk_id = self
            .module
            .declare_function(&thunk_name, Linkage::Local, &generic_sig)
            .map_err(|_| ())?;

        // Bumped once per compile, so the count is functions-rejected-for-reason-X.
        {
            use crate::compiler::slot_types::SpecEligibilityOutcome::*;
            let c = &self.counters;
            let cell = match slot_types.specialized_entry_outcome {
                Eligible => &c.spec_entry_eligible,
                RejectedZeroArity => &c.spec_entry_rejected_zero_arity,
                RejectedParamNotInt => &c.spec_entry_rejected_param_not_int,
                RejectedParamCaptured => &c.spec_entry_rejected_param_captured,
                RejectedHasClosureOp => &c.spec_entry_rejected_has_closure_op,
                RejectedHasUpvalueOp => &c.spec_entry_rejected_has_upvalue_op,
                RejectedNoReturn => &c.spec_entry_rejected_no_return,
                RejectedNoCall => &c.spec_entry_rejected_no_call,
                RejectedReturnUnreachable => &c.spec_entry_rejected_return_unreachable,
            };
            cell.set(cell.get() + 1);
        }

        // fn(*mut VM, i64...) -> (status, payload); payload valid only when status == 0.
        let spec_wants_closure_arg = slot_types.wants_closure_arg;
        let (spec_thunk_id, spec_seq_id, spec_sig_opt): (
            Option<FuncId>,
            Option<u32>,
            Option<Signature>,
        ) = if slot_types.specialized_entry_eligible {
            let arity = func.arity as usize;
            let mut spec_sig = self.module.make_signature();
            spec_sig.params.push(AbiParam::new(ptr_ty));
            if spec_wants_closure_arg {
                spec_sig.params.push(AbiParam::new(ptr_ty));
            }
            for _ in 0..arity {
                spec_sig.params.push(AbiParam::new(types::I64));
            }
            spec_sig.returns.push(AbiParam::new(types::I32));
            spec_sig.returns.push(AbiParam::new(types::I64));
            self.next_id = self.next_id.wrapping_add(1);
            let ssid = self.next_id;
            let spec_name = format!("oxigen_jit_specialized_{}_{:p}", ssid, Rc::as_ptr(func));
            let sid = self
                .module
                .declare_function(&spec_name, Linkage::Local, &spec_sig)
                .map_err(|_| ())?;
            (Some(sid), Some(ssid), Some(spec_sig))
        } else {
            (None, None, None)
        };

        struct EntryJob {
            kind: EntryKind,
            func_id: FuncId,
            seq_id: u32,
            sig: Signature,
        }

        let mut entries_to_compile: Vec<EntryJob> = Vec::new();
        entries_to_compile.push(EntryJob {
            kind: EntryKind::Generic,
            func_id: thunk_id,
            seq_id: generic_seq_id,
            sig: generic_sig.clone(),
        });
        if let (Some(sid), Some(ssid), Some(ssig)) = (spec_thunk_id, spec_seq_id, spec_sig_opt) {
            entries_to_compile.push(EntryJob {
                kind: EntryKind::IntSpecialized,
                func_id: sid,
                seq_id: ssid,
                sig: ssig,
            });
        }

        for job in entries_to_compile {
            let kind = job.kind;
            self.ctx.func.signature = job.sig;
            self.ctx.func.name = UserFuncName::user(0, job.seq_id);

            {
                let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.fbc);

                let refs = declare_helper_refs(&self.helpers, &mut self.module, &mut builder);

                let entry_block = builder.create_block();
                builder.append_block_params_for_function_params(entry_block);

                // Shared exit block; payload is meaningful only on specialized success-Return.
                let exit_block = builder.create_block();
                builder.append_block_param(exit_block, types::I32);
                builder.append_block_param(exit_block, types::I64);

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

                // This activation's own JitFrame address, computed once. Same rationale
                // as slot_offset below: jit_frames is preallocated to FRAMES_MAX and
                // never reallocates, and every callee teardown and bailout/error rollback
                // restores jit_frame_view.len before this function's next instruction —
                // so `ptr + (len-1)*size` is fixed for the whole activation. Every
                // consumer wants *this* frame (its closure, its slot offset, its current
                // line), never whatever happens to be on top, so recomputing it from a
                // mutable len bought nothing but ~5 instructions a site.
                let own_frame_ptr = emit_load_top_jit_frame_ptr(&mut builder, vm_val);

                // The slot offset is immutable for the lifetime of this activation.
                let slot_offset_val = builder.ins().load(
                    types::I64,
                    cranelift_codegen::ir::MemFlags::trusted(),
                    own_frame_ptr,
                    JitFrame::OFFSET_SLOT_OFFSET,
                );

                // Backing slot is still populated so stack shape stays valid; flush_all spills before generic ops.
                let mut int_locals: HashMap<u16, Variable> = HashMap::new();
                let mut live_int_slots: HashSet<u16> = HashSet::new();
                // IntSpecialized prologue def_var's the closure register arg for inline GetUpvalue.
                let closure_arg_var: Option<Variable> =
                    if matches!(kind, EntryKind::IntSpecialized) && spec_wants_closure_arg {
                        Some(builder.declare_var(ptr_ty))
                    } else {
                        None
                    };
                // Any opcode letting runtime code observe vm.stack must flush_to_memory first.
                let mut virt_stack = VirtStack::new();
                // Pops whose virtual compare+branch never materialized a Boolean have nothing to pop.
                let mut virt_branch_elided_pops: HashSet<usize> = HashSet::new();


                // Cranelift 0.131+ owns Variable ids; declare_var returns a fresh one.
                {
                    let mut needs_var: Vec<u16> = Vec::new();
                    for &slot in slot_types.local_init_result_ip.values() {
                        if slot_types.is_virtualizable(slot) && !int_locals.contains_key(&slot) {
                            needs_var.push(slot);
                        }
                    }
                    // Sort so two identical compilations produce identical IR.
                    needs_var.sort_unstable();
                    needs_var.dedup();
                    for slot in needs_var {
                        let var = builder.declare_var(types::I64);
                        int_locals.insert(slot, var);
                    }
                }

                // One-shot tag guard per Int param; bail to the interpreter on a non-Integer tag.
                let mut effective_entry_block = entry_block;

                // (1) Read-only Value-typed param mirrors (B2.2a).
                let mut param_mirrors: HashMap<u16, Variable> = HashMap::new();
                if !slot_types.int_mirror_param_slots.is_empty() {
                    let mut eligible: Vec<u16> =
                        slot_types.int_mirror_param_slots.iter().copied().collect();
                    eligible.sort_unstable();
                    for slot in eligible {
                        let var = builder.declare_var(types::I64);
                        param_mirrors.insert(slot, var);
                    }
                }

                // Writable params go in int_locals so SetLocal's def_var path sees them.
                let mut int_typed_param_slots: Vec<u16> = Vec::new();
                for slot in 1..=(func.arity as u16) {
                    if slot_types.is_virtualizable(slot) && !int_locals.contains_key(&slot) {
                        let var = builder.declare_var(types::I64);
                        int_locals.insert(slot, var);
                        int_typed_param_slots.push(slot);
                    }
                }
                int_typed_param_slots.sort_unstable();

                // Sort for deterministic IR; the two slot lists are disjoint by construction.
                let mut prologue_slots: Vec<u16> = param_mirrors
                    .keys()
                    .copied()
                    .chain(int_typed_param_slots.iter().copied())
                    .collect();
                prologue_slots.sort_unstable();

                // Generic tag-guards each param; IntSpecialized trusts the caller's contract.
                match kind {
                    EntryKind::IntSpecialized => {
                        use cranelift_codegen::ir::MemFlags;
                        let flags = MemFlags::trusted();
                        let arity = func.arity as usize;

                        // Closure-aware layout is [vm, closure_ptr, args..]; otherwise [vm, args..].
                        let arg_block_param_offset =
                            if spec_wants_closure_arg { 2 } else { 1 };
                        if let Some(var) = closure_arg_var {
                            let closure_ptr_val = builder.block_params(entry_block)[1];
                            builder.def_var(var, closure_ptr_val);
                        }

                        // Slot `i` lives at slot_offset + i.
                        for slot in 1..=arity as u16 {
                            let arg_val = builder.block_params(entry_block)
                                [arg_block_param_offset + (slot as usize - 1)];
                            emit_store_stack_slot_integer(
                                &mut builder,
                                vm_val,
                                slot_offset_val,
                                slot,
                                arg_val,
                            );
                            // Route to whichever map owns this slot.
                            if let Some(&var) = int_locals.get(&slot) {
                                builder.def_var(var, arg_val);
                                live_int_slots.insert(slot);
                            } else if let Some(&var) = param_mirrors.get(&slot) {
                                builder.def_var(var, arg_val);
                            }
                        }

                        let stack_len = emit_load_stack_len(&mut builder, vm_val);
                        let arity_plus_closure =
                            builder.ins().iconst(types::I64, (arity + 1) as i64);
                        // new_len = stack_len + arity: the caller left the closure pushed, so stack_len is slot_offset + 1.
                        let arity_val = builder.ins().iconst(types::I64, arity as i64);
                        let new_len = builder.ins().iadd(stack_len, arity_val);
                        let _ = arity_plus_closure; // computed-but-unused comment anchor
                        builder
                            .ins()
                            .store(flags, new_len, vm_val, vm_stack_view_len_offset());

                        // Counter: specialized entry was actually entered.
                        if let Some(cp) = counters_ptr_opt {
                            emit_counter_bump(
                                &mut builder,
                                cp,
                                counter_offsets::SPECIALIZED_ENTRY_CALLED,
                            );
                        }
                    }
                    EntryKind::Generic => {
                        if !prologue_slots.is_empty() {
                            use cranelift_codegen::ir::MemFlags;
                            let flags = MemFlags::trusted();

                            let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                            let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                            let integer_tag =
                                builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);

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
                                if let Some(&var) = param_mirrors.get(slot) {
                                    builder.def_var(var, payload);
                                } else if let Some(&var) = int_locals.get(slot) {
                                    builder.def_var(var, payload);
                                    live_int_slots.insert(*slot);
                                }
                            }

                            let prologue_done = builder.create_block();
                            builder.ins().jump(prologue_done, &[]);

                            builder.switch_to_block(bailout_block);
                            let two = builder.ins().iconst(types::I32, 2);
                            let zero64 = builder.ins().iconst(types::I64, 0);
                            builder.ins().jump(exit_block, &[two.into(), zero64.into()]);

                            builder.switch_to_block(prologue_done);
                            effective_entry_block = prologue_done;
                            blocks.insert(0, prologue_done);
                        }
                    }
                }

                let mut current_block = effective_entry_block;
                let mut terminated = false;
                let mut ip: usize = 0;
                // Skip the line store when unchanged, but always emit at block boundaries.
                let mut last_emitted_loc: Option<(u32, u32)> = None;
                let mut ic_ix: usize = 0;
                // Same, but for Call opcodes.
                let mut call_ic_ix: usize = 0;
                // Same, but for MethodCall opcodes.
                let mut method_ic_ix: usize = 0;
                let mut field_ic_ix: usize = 0;

                while ip < code.len() {
                    if let Some(&block) = blocks.get(&ip)
                        && block != current_block {
                            if !terminated {
                                // Fall-through is a control-flow edge: staged values must live in memory at the merge.
                                virt_stack.flush_to_memory(&mut builder, vm_val);
                                builder.ins().jump(block, &[]);
                            }
                            builder.switch_to_block(block);
                            current_block = block;
                            terminated = false;
                            // A merge block starts with an empty abstract operand stack.
                            virt_stack = VirtStack::new();
                            // A predecessor could have written any line, so the first opcode must re-emit.
                            last_emitted_loc = None;
                        }

                    // Cranelift forbids emitting into a filled block; byte-step to the next real block start.
                    if terminated {
                        ip += 1;
                        continue;
                    }

                    let op = OpCode::from_byte(code[ip]).ok_or(())?;
                    let line = chunk.lines.get(ip).copied().unwrap_or(0);
                    let col = chunk.columns.get(ip).copied().unwrap_or(0);
                    // Only emit loc stores for opcodes that can surface an error attributed to this IP.
                    if opcode_always_needs_line(op) && last_emitted_loc != Some((line, col)) {
                        emit_store_current_loc(&mut builder, own_frame_ptr, line, col);
                        last_emitted_loc = Some((line, col));
                    }

                    // Drains staged temps only; virtualized locals stay live in SSA.
                    let handles_own_flush = matches!(
                        op,
                        OpCode::GetLocal
                            | OpCode::SetLocal
                            | OpCode::Pop
                            | OpCode::Constant
                            | OpCode::None
                            | OpCode::True
                            | OpCode::False
                            | OpCode::Add
                            | OpCode::Subtract
                            | OpCode::Multiply
                            | OpCode::Divide
                            | OpCode::Modulo
                            | OpCode::Less
                            | OpCode::LessEqual
                            | OpCode::Greater
                            | OpCode::GreaterEqual
                            | OpCode::Equal
                            | OpCode::NotEqual
                            | OpCode::Call
                            | OpCode::TypeWrap
                    );
                    if !handles_own_flush && !virt_stack.is_empty() {
                        virt_stack.flush_to_memory(&mut builder, vm_val);
                    }

                    match op {
                        OpCode::Constant => {
                            let idx = read_u16(code, ip + 1);
                            let init_slot = slot_types.local_init_result_ip.get(&ip).copied();
                            match chunk.constants[idx as usize].repr() {
                                crate::vm::value::ValueRepr::Integer(n) => {
                                    let v = builder.ins().iconst(types::I64, n);

                                    if let Some(slot) = init_slot {
                                        // Materialize the backing slot so stack shape stays valid, then def_var.
                                        emit_inline_push_integer(&mut builder, vm_val, v);
                                        if let Some(&var) = int_locals.get(&slot) {
                                            builder.def_var(var, v);
                                            live_int_slots.insert(slot);
                                        }
                                    } else {
                                        virt_stack.push_int_ssa(v);
                                    }
                                }
                                crate::vm::value::ValueRepr::Float(f) => {
                                    let bits = f.to_bits() as i64;
                                    let v = builder.ins().iconst(types::I64, bits);
                                    if init_slot.is_some() {
                                        // Must materialize so the scaled-arith peephole sees the slot in memory.
                                        emit_inline_push_float(&mut builder, vm_val, v);
                                    } else {
                                        virt_stack.push_float_ssa(v);
                                    }
                                }
                                _ => {
                                    if !virt_stack.is_empty() {
                                        virt_stack.flush_to_memory(&mut builder, vm_val);
                                    }
                                    let idx_val = builder.ins().iconst(types::I32, idx as i64);
                                    builder.ins().call(refs.push_constant, &[vm_val, idx_val]);
                                }
                            }
                            ip += 3;
                        }
                        OpCode::None => {
                            // Deferring lets a following Pop delete the whole push/pop pair.
                            virt_stack.push_const(VirtConst::None);
                            ip += 1;
                        }
                        OpCode::True => {
                            virt_stack.push_const(VirtConst::True);
                            ip += 1;
                        }
                        OpCode::False => {
                            virt_stack.push_const(VirtConst::False);
                            ip += 1;
                        }
                        OpCode::Pop => {
                            // Four cases: elided virtual branch, scope teardown, expression cleanup, ordinary Pop.
                            if virt_branch_elided_pops.contains(&ip) {
                                // Virtual no-op — no mem op, no expr_stack change.
                            } else if let Some(slot) = slot_types.scope_pop_for(ip) {
                                live_int_slots.remove(&slot);
                                emit_inline_pop_tag_gated(&mut builder, &refs, vm_val);
                            } else if !virt_stack.is_empty() {
                                // No memory op: the virt slot was never materialised.
                                virt_stack.pop();
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
                            emit_early_exit_on_err(&mut builder, exit_block, status);
                            ip += 1;
                        }
                        OpCode::IndexAssign => {
                            // The helper pops from vm.stack, so flush staged temps first.
                            if !virt_stack.is_empty() {
                                virt_stack.flush_to_memory(&mut builder, vm_val);
                            }
                            let call = builder.ins().call(refs.index_assign, &[vm_val]);
                            let status = builder.inst_results(call)[0];
                            emit_early_exit_on_err(&mut builder, exit_block, status);
                            ip += 1;
                        }
                        OpCode::IterLen | OpCode::IterGet | OpCode::IterEntry => {
                            // The helpers pop from vm.stack, so flush staged temps first.
                            if !virt_stack.is_empty() {
                                virt_stack.flush_to_memory(&mut builder, vm_val);
                            }
                            let func = match op {
                                OpCode::IterLen => refs.iter_len,
                                OpCode::IterGet => refs.iter_get,
                                _ => refs.iter_entry,
                            };
                            let call = builder.ins().call(func, &[vm_val]);
                            let status = builder.inst_results(call)[0];
                            emit_early_exit_on_err(&mut builder, exit_block, status);
                            ip += 1;
                        }
                        OpCode::TypeWrap => {
                            // slot_types proved this TypeWrap is identity; skip the FFI entirely.
                            if slot_types.noop_type_wrap_ips.contains(&ip) {
                                ip += 3;
                                continue;
                            }
                            // Real conversion: the helper pops from vm.stack, so flush first.
                            if !virt_stack.is_empty() {
                                virt_stack.flush_to_memory(&mut builder, vm_val);
                            }
                            let idx = read_u16(code, ip + 1);
                            let idx_val = builder.ins().iconst(types::I32, idx as i64);
                            let call = builder.ins().call(refs.type_wrap, &[vm_val, idx_val]);
                            let status = builder.inst_results(call)[0];
                            emit_early_exit_on_err(&mut builder, exit_block, status);
                            ip += 3;
                        }
                        OpCode::GetLocal => {
                            // Disabled inside loops: Cranelift hoists the field load across iterations and loses the accumulation.
                            let sf_in_loop =
                                info.loop_ranges.iter().any(|&(t, p)| ip >= t && ip < p);
                            if let Some(m) = if sf_in_loop {
                                None
                            } else {
                                match_struct_field_add_update(code, chunk, ip, &blocks)
                            } {
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }
                                let cache_ptr = field_cache_ptrs[field_ic_ix];
                                field_ic_ix += 2;
                                emit_inline_struct_field_add(
                                    &mut builder,
                                    exit_block,
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
                            } else if let Some(m) =
                                match_local_array_mod_index_add_update(code, chunk, ip, &blocks)
                                    .filter(|m| {
                                        // The helper reads dst, array and index from the VM stack; any virtualized slot's backing is stale.
                                        !int_locals.contains_key(&m.dst_slot)
                                            && !int_locals.contains_key(&m.array_slot)
                                            && !int_locals.contains_key(&m.index_slot)
                                    })
                            {
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }
                                let dst = builder.ins().iconst(types::I32, m.dst_slot as i64);
                                let array = builder.ins().iconst(types::I32, m.array_slot as i64);
                                let index = builder.ins().iconst(types::I32, m.index_slot as i64);
                                let modulus = builder.ins().iconst(types::I64, m.modulus);
                                let pop_after = builder
                                    .ins()
                                    .iconst(types::I32, if m.pop_after { 1 } else { 0 });
                                let call = builder.ins().call(
                                    refs.local_add_array_mod_index,
                                    &[vm_val, dst, array, index, modulus, pop_after],
                                );
                                let status = builder.inst_results(call)[0];
                                emit_early_exit_on_err(&mut builder, exit_block, status);
                                ip += m.len;
                            } else if let Some(m) = match_local_arith_update(code, ip, &blocks)
                                .filter(|m| {
                                    !int_locals.contains_key(&m.dst_slot)
                                        && !int_locals.contains_key(&m.rhs_slot)
                                })
                            {
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }
                                emit_inline_local_scaled_arith_update(
                                    &mut builder,
                                    exit_block,
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
                            } else if let Some(m) = match_local_scaled_arith_update(
                                code, chunk, ip, &blocks,
                            )
                            .filter(|m| {
                                !int_locals.contains_key(&m.dst_slot)
                                    && !int_locals.contains_key(&m.rhs_slot)
                            }) {
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }
                                emit_inline_local_scaled_arith_update(
                                    &mut builder,
                                    exit_block,
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
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }
                                emit_inline_local_const_arith_update(
                                    &mut builder,
                                    exit_block,
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
                                // Peephole guards above ensure no other path writes a virtualized slot without def_var.
                                let slot = read_u16(code, ip + 1);
                                if let Some(&var) = int_locals.get(&slot)
                                    && live_int_slots.contains(&slot) {
                                        let val = builder.use_var(var);
                                        virt_stack.push_int_ssa(val);
                                        ip += 3;
                                        continue;
                                    }
                                // Read-only Value params that passed the tag guard live in a Variable for the invocation.
                                if let Some(&var) = param_mirrors.get(&slot) {
                                    let val = builder.use_var(var);
                                    virt_stack.push_int_ssa(val);
                                    ip += 3;
                                    continue;
                                }
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
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
                            // SetLocal peeks rather than pops (VM semantics), so the following Pop drains expr_stack.
                            let slot = read_u16(code, ip + 1);
                            if let (Some(&var), Some(top)) =
                                (int_locals.get(&slot), virt_stack.peek_int_ssa())
                            {
                                builder.def_var(var, top);
                                live_int_slots.insert(slot);
                                ip += 3;
                            } else {
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }
                                // Peek before the store: without this re-sync the Variable stays pinned at its Constant-init def forever.
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
                                let constrained = func
                                    .locals
                                    .get(slot as usize)
                                    .map(|li| li.type_constraint.is_some())
                                    .unwrap_or(false);
                                let int_constraint = func
                                    .locals
                                    .get(slot as usize)
                                    .and_then(|li| li.type_constraint.as_deref())
                                    == Some("INTEGER");
                                if constrained && int_constraint {
                                    // Inline the int type-lock check; only a real violation crosses into the checked helper.
                                    use cranelift_codegen::ir::MemFlags;
                                    use cranelift_codegen::ir::condcodes::IntCC;
                                    let flags = MemFlags::trusted();
                                    let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                                    let stack_len = emit_load_stack_len(&mut builder, vm_val);
                                    let value_size =
                                        builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                                    let one = builder.ins().iconst(types::I64, 1);
                                    let top_idx = builder.ins().isub(stack_len, one);
                                    let top_off = builder.ins().imul(top_idx, value_size);
                                    let addr_top = builder.ins().iadd(stack_ptr, top_off);
                                    let top_tag = builder.ins().load(types::I8, flags, addr_top, 0);
                                    let int_tag =
                                        builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
                                    let is_int = builder.ins().icmp(IntCC::Equal, top_tag, int_tag);
                                    let ok_block = builder.create_block();
                                    let err_block = builder.create_block();
                                    builder.ins().brif(is_int, ok_block, &[], err_block, &[]);

                                    // Rare violation: the helper raises the exact lock error and returns 1.
                                    builder.switch_to_block(err_block);
                                    maybe_emit_current_line(
                                        &mut builder,
                                        own_frame_ptr,
                                        line,
                                        col,
                                        &mut last_emitted_loc,
                                    );
                                    let slot_arg = builder.ins().iconst(types::I32, slot as i64);
                                    let call = builder
                                        .ins()
                                        .call(refs.set_local_checked, &[vm_val, slot_arg]);
                                    let status = builder.inst_results(call)[0];
                                    let zero64 = builder.ins().iconst(types::I64, 0);
                                    builder
                                        .ins()
                                        .jump(exit_block, &[status.into(), zero64.into()]);

                                    builder.switch_to_block(ok_block);
                                    emit_inline_set_local(
                                        &mut builder,
                                        &refs,
                                        vm_val,
                                        slot_offset_val,
                                        slot,
                                    );
                                } else if constrained {
                                    // Non-int type lock: enforce via the fallible checked helper.
                                    maybe_emit_current_line(
                                        &mut builder,
                                        own_frame_ptr,
                                        line,
                                        col,
                                        &mut last_emitted_loc,
                                    );
                                    let slot_arg =
                                        builder.ins().iconst(types::I32, slot as i64);
                                    let call = builder
                                        .ins()
                                        .call(refs.set_local_checked, &[vm_val, slot_arg]);
                                    let status = builder.inst_results(call)[0];
                                    emit_early_exit_on_err(&mut builder, exit_block, status);
                                } else {
                                    emit_inline_set_local(
                                        &mut builder,
                                        &refs,
                                        vm_val,
                                        slot_offset_val,
                                        slot,
                                    );
                                }
                                if let (Some(&var), Some(payload)) =
                                    (int_locals.get(&slot), top_payload)
                                {
                                    builder.def_var(var, payload);
                                    live_int_slots.insert(slot);
                                }
                                ip += 3;
                            }
                        }

                        // Add/Sub/Mul get an inline int+int fast path via a direct tag check on the stack.
                        OpCode::Add | OpCode::Subtract | OpCode::Multiply => {
                            // Both operands staged as Int: fold into a register iadd/isub/imul.
                            if virt_stack.top_n_are_int_ssa(2) {
                                let rhs = virt_stack.pop_int_ssa().unwrap();
                                let lhs = virt_stack.pop_int_ssa().unwrap();
                                let result = match op {
                                    OpCode::Add => builder.ins().iadd(lhs, rhs),
                                    OpCode::Subtract => builder.ins().isub(lhs, rhs),
                                    OpCode::Multiply => builder.ins().imul(lhs, rhs),
                                    _ => unreachable!(),
                                };
                                virt_stack.push_int_ssa(result);
                            } else if virt_stack.pending_depth() == 1
                                && virt_stack.peek_int_ssa().is_some()
                            {
                                // Mixed-mode: memory lhs, register rhs.
                                maybe_emit_current_line(
                                    &mut builder,
                                    own_frame_ptr,
                                    line,
                                    col,
                                    &mut last_emitted_loc,
                                );
                                let rhs = virt_stack.peek_int_ssa().unwrap();
                                emit_mixed_mode_int_arith(
                                    &mut builder,
                                    exit_block,
                                    &refs,
                                    vm_val,
                                    rhs,
                                    op,
                                );
                                let _ = virt_stack.pop_int_ssa().unwrap();
                            } else {
                                // The slow path can call the fallible helper, so stamp the line first.
                                maybe_emit_current_line(
                                    &mut builder,
                                    own_frame_ptr,
                                    line,
                                    col,
                                    &mut last_emitted_loc,
                                );
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }
                                let (arith_op, slow) = match op {
                                    OpCode::Add => (IntArithOp::Add, refs.add),
                                    OpCode::Subtract => (IntArithOp::Sub, refs.sub),
                                    OpCode::Multiply => (IntArithOp::Mul, refs.mul),
                                    _ => unreachable!(),
                                };
                                emit_int_fast_arith(
                                    &mut builder,
                                    exit_block,
                                    &refs,
                                    vm_val,
                                    arith_op,
                                    slow,
                                );
                            }
                            ip += 1;
                        }
                        OpCode::BitAnd
                        | OpCode::BitOr
                        | OpCode::BitXor
                        | OpCode::ShiftLeft
                        | OpCode::ShiftRight => {
                            // Shift count masked to low 6 bits, matching vm::binary_shl/shr.
                            if virt_stack.top_n_are_int_ssa(2) {
                                let rhs = virt_stack.pop_int_ssa().unwrap();
                                let lhs = virt_stack.pop_int_ssa().unwrap();
                                let result = match op {
                                    OpCode::BitAnd => builder.ins().band(lhs, rhs),
                                    OpCode::BitOr => builder.ins().bor(lhs, rhs),
                                    OpCode::BitXor => builder.ins().bxor(lhs, rhs),
                                    OpCode::ShiftLeft => builder.ins().ishl(lhs, rhs),
                                    OpCode::ShiftRight => builder.ins().sshr(lhs, rhs),
                                    _ => unreachable!(),
                                };
                                virt_stack.push_int_ssa(result);
                            } else {
                                maybe_emit_current_line(
                                    &mut builder,
                                    own_frame_ptr,
                                    line,
                                    col,
                                    &mut last_emitted_loc,
                                );
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }
                                let (arith_op, slow) = match op {
                                    OpCode::BitAnd => (IntArithOp::BitAnd, refs.op_band),
                                    OpCode::BitOr => (IntArithOp::BitOr, refs.op_bor),
                                    OpCode::BitXor => (IntArithOp::BitXor, refs.op_bxor),
                                    OpCode::ShiftLeft => (IntArithOp::Shl, refs.op_shl),
                                    OpCode::ShiftRight => (IntArithOp::Shr, refs.op_shr),
                                    _ => unreachable!(),
                                };
                                emit_int_fast_arith(
                                    &mut builder,
                                    exit_block,
                                    &refs,
                                    vm_val,
                                    arith_op,
                                    slow,
                                );
                            }
                            ip += 1;
                        }
                        OpCode::Divide | OpCode::Modulo => {
                            let is_mod = matches!(op, OpCode::Modulo);

                            // Retained after the 0.131 bump: removing it regressed bench_collatz 42.6 -> 49.9 ms.
                            if is_mod
                                && let Some(next_ip) = try_emit_parity_branch_peephole(
                                    &mut builder,
                                    chunk,
                                    code,
                                    ip,
                                    &mut virt_stack,
                                    &mut virt_branch_elided_pops,
                                    &mut blocks,
                                    &slot_types,
                                    counters_ptr_opt,
                                ) {
                                    terminated = true;
                                    ip = next_ip;
                                    continue;
                                }

                            // Bias-and-shift instead of Cranelift's ~20-cycle idiv; required at opt_level=none.

                            // Register sdiv/srem with zero and INT_MIN/-1 guards, falling back to the helper.
                            if virt_stack.top_n_are_int_ssa(2) {
                                let rhs = virt_stack.pop_int_ssa().unwrap();
                                let lhs = virt_stack.pop_int_ssa().unwrap();
                                if !is_mod
                                    && let Some(q) =
                                        try_emit_sdiv_pow2_peephole(&mut builder, lhs, rhs)
                                    {
                                        if let Some(cp) = counters_ptr_opt {
                                            emit_counter_bump(
                                                &mut builder,
                                                cp,
                                                counter_offsets::VIRT_DIV_POW2_LOWERED,
                                            );
                                        }
                                        virt_stack.push_int_ssa(q);
                                        ip += 1;
                                        continue;
                                    }
                                // Both paths can raise (zero divisor, INT_MIN/-1), so stamp the line.
                                maybe_emit_current_line(
                                    &mut builder,
                                    own_frame_ptr,
                                    line,
                                    col,
                                    &mut last_emitted_loc,
                                );
                                let slow_helper = if is_mod { refs.modu } else { refs.div };
                                let result = emit_int_virt_divmod(
                                    &mut builder,
                                    exit_block,
                                    vm_val,
                                    is_mod,
                                    lhs,
                                    rhs,
                                    slow_helper,
                                    counters_ptr_opt,
                                );
                                virt_stack.push_int_ssa(result);
                            } else {
                                maybe_emit_current_line(
                                    &mut builder,
                                    own_frame_ptr,
                                    line,
                                    col,
                                    &mut last_emitted_loc,
                                );
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }
                                let slow = if is_mod { refs.modu } else { refs.div };
                                emit_int_fast_divmod(
                                    &mut builder,
                                    exit_block,
                                    &refs,
                                    vm_val,
                                    is_mod,
                                    slow,
                                );
                            }
                            ip += 1;
                        }
                        OpCode::Less
                        | OpCode::LessEqual
                        | OpCode::Greater
                        | OpCode::GreaterEqual
                        | OpCode::Equal
                        | OpCode::NotEqual => {
                            use cranelift_codegen::ir::condcodes::IntCC;
                            // lt/le/gt/ge return a status; eq/ne return nothing, so inst_results would panic.
                            let (cc, slow_helper, slow_helper_fallible) = match op {
                                OpCode::Less => (IntCC::SignedLessThan, refs.lt, true),
                                OpCode::LessEqual => (IntCC::SignedLessThanOrEqual, refs.le, true),
                                OpCode::Greater => (IntCC::SignedGreaterThan, refs.gt, true),
                                OpCode::GreaterEqual => {
                                    (IntCC::SignedGreaterThanOrEqual, refs.ge, true)
                                }
                                OpCode::Equal => (IntCC::Equal, refs.eq, false),
                                OpCode::NotEqual => (IntCC::NotEqual, refs.ne, false),
                                _ => unreachable!(),
                            };

                            // A safe peek needs the next byte in range and not itself a branch target.
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

                                // Bool-less path needs PopJumpIfFalse, or cleanup Pops classified so they can be elided.
                                let virt_eligible = virt_stack.top_n_are_int_ssa(2)
                                    && match branch_op {
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
                                    if matches!(op, OpCode::Equal | OpCode::NotEqual)
                                        && let Some(cp) = counters_ptr_opt {
                                            emit_counter_bump(
                                                &mut builder,
                                                cp,
                                                counter_offsets::VIRT_BRANCH_EQ_HIT,
                                            );
                                        }
                                    let rhs = virt_stack.pop_int_ssa().unwrap();
                                    let lhs = virt_stack.pop_int_ssa().unwrap();
                                    let pred = builder.ins().icmp(cc, lhs, rhs);
                                    // Flush pending virt slots BELOW the operands: successors reset the virt stack and would lose them.
                                    if !virt_stack.is_empty() {
                                        virt_stack.flush_to_memory(&mut builder, vm_val);
                                    }
                                    let (true_block, false_block) = match branch_op {
                                        OpCode::JumpIfFalse | OpCode::PopJumpIfFalse => {
                                            (fall_block, target_block)
                                        }
                                        // JumpIfTrue: branch taken when TRUE.
                                        OpCode::JumpIfTrue => (target_block, fall_block),
                                        _ => unreachable!(),
                                    };
                                    builder.ins().brif(pred, true_block, &[], false_block, &[]);
                                    if matches!(branch_op, OpCode::JumpIfFalse | OpCode::JumpIfTrue)
                                    {
                                        // The Pop handler will virtual-no-op these.
                                        virt_branch_elided_pops.insert(next_ip);
                                        virt_branch_elided_pops.insert(target_ip);
                                    }
                                } else {
                                    // Slow path: helper may type-error.
                                    maybe_emit_current_line(
                                        &mut builder,
                                        own_frame_ptr,
                                        line,
                                        col,
                                    &mut last_emitted_loc,
                                    );
                                    if !virt_stack.is_empty() {
                                        virt_stack.flush_to_memory(&mut builder, vm_val);
                                    }
                                    emit_fused_int_cmp_branch(
                                        &mut builder,
                                        exit_block,
                                        &refs,
                                        vm_val,
                                        cc,
                                        slow_helper,
                                        slow_helper_fallible,
                                        branch_op,
                                        target_block,
                                        fall_block,
                                    );
                                }
                                terminated = true;
                                ip = branch_ip + 3;
                            } else {
                                // Standalone comparison always touches the helper, so stamp the line.
                                maybe_emit_current_line(
                                    &mut builder,
                                    own_frame_ptr,
                                    line,
                                    col,
                                    &mut last_emitted_loc,
                                );
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }
                                // Standalone Eq/Ne uses the equality helper for mixed-type and String cases.
                                if matches!(op, OpCode::Equal | OpCode::NotEqual) {
                                    emit_int_fast_eq(
                                        &mut builder,
                                        &refs,
                                        vm_val,
                                        matches!(op, OpCode::Equal),
                                    );
                                } else {
                                    emit_int_fast_cmp(
                                        &mut builder,
                                        exit_block,
                                        &refs,
                                        vm_val,
                                        cc,
                                        slow_helper,
                                    );
                                }
                                ip += 1;
                            }
                        }

                        OpCode::Negate => {
                            emit_fallible(&mut builder, exit_block, refs.negate, vm_val);
                            ip += 1;
                        }

                        OpCode::BitNot => {
                            // BitNot is monomorphic on Int, so the virt-int fast path is always safe.
                            if let Some(payload) = virt_stack.pop_int_ssa() {
                                let result = builder.ins().bnot(payload);
                                virt_stack.push_int_ssa(result);
                            } else {
                                maybe_emit_current_line(
                                    &mut builder,
                                    own_frame_ptr,
                                    line,
                                    col,
                                    &mut last_emitted_loc,
                                );
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }
                                emit_fallible(&mut builder, exit_block, refs.op_bnot, vm_val);
                            }
                            ip += 1;
                        }

                        OpCode::Log => {
                            maybe_emit_current_line(
                                &mut builder,
                                own_frame_ptr,
                                line,
                                col,
                                    &mut last_emitted_loc,
                            );
                            if !virt_stack.is_empty() {
                                virt_stack.flush_to_memory(&mut builder, vm_val);
                            }
                            let flags = code[ip + 1];
                            let flags_val = builder.ins().iconst(types::I32, flags as i64);
                            let call = builder.ins().call(refs.op_log, &[vm_val, flags_val]);
                            let status = builder.inst_results(call)[0];
                            emit_early_exit_on_err(&mut builder, exit_block, status);
                            ip += 2;
                        }

                        // Equal/NotEqual are handled in the comparison arm above so they share JumpIf* fusion.
                        OpCode::Not => {
                            builder.ins().call(refs.not, &[vm_val]);
                            ip += 1;
                        }

                        // Control flow
                        OpCode::Jump => {
                            let off = read_u16(code, ip + 1) as usize;
                            let target_ip = ip + 3 + off;
                            let target = blocks[&target_ip];
                            virt_stack.flush_to_memory(&mut builder, vm_val);
                            builder.ins().jump(target, &[]);
                            terminated = true;
                            ip += 3;
                        }
                        OpCode::Loop => {
                            let off = read_u16(code, ip + 1) as usize;
                            let target_ip = (ip + 3) - off;
                            let target = blocks[&target_ip];
                            virt_stack.flush_to_memory(&mut builder, vm_val);
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

                            // Both successors are block edges and must see a consistent memory stack.
                            virt_stack.flush_to_memory(&mut builder, vm_val);
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

                            virt_stack.flush_to_memory(&mut builder, vm_val);
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

                            virt_stack.flush_to_memory(&mut builder, vm_val);
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
                            // Version check, then tag-gated Rc bump and copy; miss falls through to the helper.
                            use cranelift_codegen::ir::MemFlags;
                            use cranelift_codegen::ir::condcodes::IntCC;
                            let flags = MemFlags::trusted();
                            let cache_ver = builder.ins().load(types::I64, flags, cache_val, 0);
                            // Per-name version cell trades one dependent load for the VM-wide globals_version read.
                            let cell_ptr = builder.ins().load(
                                ptr_ty,
                                flags,
                                cache_val,
                                GlobalCacheEntry::OFFSET_VERSION_CELL,
                            );
                            let vm_ver = builder.ins().load(types::I64, flags, cell_ptr, 0);
                            let is_hit = builder.ins().icmp(IntCC::Equal, cache_ver, vm_ver);
                            let hit_block = builder.create_block();
                            let miss_block = builder.create_block();
                            let cont_block = builder.create_block();
                            builder.ins().brif(is_hit, hit_block, &[], miss_block, &[]);

                            // ── Hit ──
                            builder.switch_to_block(hit_block);
                            // cache.value sits at byte offset 8 of the cache.
                            let cache_value_ptr = builder.ins().iadd_imm(cache_val, 8);
                            // Heap-backed means tag > 6 AND tag != 13: Builtin's payload is a fn pointer, and bumping it SIGBUSes.
                            let tag = builder.ins().load(types::I8, flags, cache_value_ptr, 0);
                            let six = builder.ins().iconst(types::I8, 6);
                            let is_heap = builder.ins().icmp(IntCC::UnsignedGreaterThan, tag, six);
                            let check_builtin = builder.create_block();
                            let bump_block = builder.create_block();
                            let post_bump = builder.create_block();
                            builder
                                .ins()
                                .brif(is_heap, check_builtin, &[], post_bump, &[]);

                            builder.switch_to_block(check_builtin);
                            let builtin_tag = builder.ins().iconst(types::I8, 13);
                            let is_builtin = builder.ins().icmp(IntCC::Equal, tag, builtin_tag);
                            builder
                                .ins()
                                .brif(is_builtin, post_bump, &[], bump_block, &[]);

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
                            builder
                                .ins()
                                .store(flags, new_top, vm_val, vm_stack_view_len_offset());
                            builder.ins().jump(cont_block, &[]);

                            // ── Miss ──
                            builder.switch_to_block(miss_block);
                            let call = builder
                                .ins()
                                .call(refs.get_global_ic, &[vm_val, cache_val, idx_val]);
                            let status = builder.inst_results(call)[0];
                            let err_block = builder.create_block();
                            builder.ins().brif(status, err_block, &[], cont_block, &[]);
                            builder.switch_to_block(err_block);
                            let zero64 = builder.ins().iconst(types::I64, 0);
                            builder
                                .ins()
                                .jump(exit_block, &[status.into(), zero64.into()]);

                            builder.switch_to_block(cont_block);
                            ip += 3;
                        }
                        OpCode::SetGlobal => {
                            let idx = read_u16(code, ip + 1);
                            let idx_val = builder.ins().iconst(types::I32, idx as i64);
                            let call = builder.ins().call(refs.set_global, &[vm_val, idx_val]);
                            let status = builder.inst_results(call)[0];
                            emit_early_exit_on_err(&mut builder, exit_block, status);
                            ip += 3;
                        }
                        OpCode::DefineGlobal => {
                            let idx = read_u16(code, ip + 1);
                            let idx_val = builder.ins().iconst(types::I32, idx as i64);
                            let call = builder.ins().call(refs.define_global, &[vm_val, idx_val]);
                            let status = builder.inst_results(call)[0];
                            emit_early_exit_on_err(&mut builder, exit_block, status);
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
                            emit_early_exit_on_err(&mut builder, exit_block, status);
                            ip += 6;
                        }

                        // ── Upvalues ────────────────────────────────────
                        OpCode::GetUpvalue => {
                            // Read kinds[idx] from the closure's cache; the helper populates it so later runs hit the fast path.
                            use cranelift_codegen::ir::MemFlags;
                            use cranelift_codegen::ir::condcodes::IntCC;
                            let idx = read_u16(code, ip + 1);
                            let idx_val = builder.ins().iconst(types::I32, idx as i64);
                            let flags = MemFlags::trusted();

                            let closure_ptr = if let Some(var) = closure_arg_var {
                                builder.use_var(var)
                            } else {
                                let frame_ptr = own_frame_ptr;
                                builder.ins().load(
                                    ptr_ty,
                                    flags,
                                    frame_ptr,
                                    JitFrame::OFFSET_CLOSURE_RAW,
                                )
                            };
                            // Pinned by obj_closure_upvalue_caches_layout: each field's first usize is the data pointer.
                            let kinds_box_off = std::mem::offset_of!(
                                crate::vm::value::ObjClosure,
                                upvalue_int_kinds
                            ) as i32;
                            let values_box_off = std::mem::offset_of!(
                                crate::vm::value::ObjClosure,
                                upvalue_int_values
                            ) as i32;
                            let kinds_data =
                                builder
                                    .ins()
                                    .load(ptr_ty, flags, closure_ptr, kinds_box_off);
                            let kind_byte =
                                builder.ins().load(types::I8, flags, kinds_data, idx as i32);
                            let is_int = builder.ins().icmp_imm(IntCC::Equal, kind_byte, 1);

                            let fast_block = builder.create_block();
                            let fallback_block = builder.create_block();
                            let cont_block = builder.create_block();
                            builder
                                .ins()
                                .brif(is_int, fast_block, &[], fallback_block, &[]);

                            // ── Fast block: cache hit ──
                            builder.switch_to_block(fast_block);
                            let values_data =
                                builder
                                    .ins()
                                    .load(ptr_ty, flags, closure_ptr, values_box_off);
                            let int_val = builder.ins().load(
                                types::I64,
                                flags,
                                values_data,
                                (idx as i32) * 8,
                            );
                            emit_inline_push_integer(&mut builder, vm_val, int_val);
                            if let Some(cp) = counters_ptr_opt {
                                emit_counter_bump(
                                    &mut builder,
                                    cp,
                                    counter_offsets::GET_UPVALUE_INLINE_HIT,
                                );
                            }
                            builder.ins().jump(cont_block, &[]);

                            // ── Fallback block: helper ──
                            builder.switch_to_block(fallback_block);
                            builder.ins().call(refs.get_upvalue, &[vm_val, idx_val]);
                            builder.ins().jump(cont_block, &[]);

                            builder.switch_to_block(cont_block);
                            builder.seal_block(fast_block);
                            builder.seal_block(fallback_block);
                            builder.seal_block(cont_block);
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
                            let upvalue_count = chunk.constants[fn_idx as usize]
                                .as_closure()
                                .map(|t| t.function.upvalue_count as usize)
                                .ok_or(())?;
                            let descriptors_offset = ip + 3;
                            let fn_val = builder.ins().iconst(types::I32, fn_idx as i64);
                            let off_val =
                                builder.ins().iconst(types::I32, descriptors_offset as i64);
                            let call = builder
                                .ins()
                                .call(refs.op_closure, &[vm_val, fn_val, off_val]);
                            let status = builder.inst_results(call)[0];
                            emit_early_exit_on_err(&mut builder, exit_block, status);
                            ip = descriptors_offset + 3 * upvalue_count;
                        }

                        OpCode::Call => {
                            let arg_count = code[ip + 1];
                            let cache_ptr = call_cache_ptrs[call_ic_ix];
                            call_ic_ix += 1;

                            // Emitted inside the IC's hit block to share its tag and RC checks; separate emission ran bench_arith 254% slower.
                            let mut shared_post_call_block: Option<Block> = None;


                            // Direct self-recursion: guard on callee_rc == our own closure_raw, else fall through to the IC.
                            //
                            // Excludes the closure-aware shape: that signature leads with a
                            // *const ObjClosure the direct call below never passes, so emitting it
                            // fails the Cranelift verifier and takes the whole function down with
                            // it — a nested named fn reaches itself through GetUpvalue, so it lands
                            // here, and every such function silently ran interpreted.
                            //
                            // Also skipped where no GetGlobal names this function, since the
                            // guard below can then never match: it cost bench_closure a dead
                            // compare plus a snapshot and re-flush on all 12M calls.
                            let a3_eligible = slot_types.specialized_entry_eligible
                                && !spec_wants_closure_arg
                                && slot_types.self_reachable_by_global_name
                                && spec_thunk_id.is_some()
                                && arg_count as usize == func.arity as usize
                                && virt_stack.top_n_are_int_ssa(arg_count as usize);

                            // Fuse the call with a following int arith op. Both register-carrying
                            // success paths (A3, closure-aware) currently box their i64 result onto
                            // the value stack for the very next opcode to tag-check and reload —
                            // fusing hands it straight over in a register instead.
                            //
                            // Requires a path that can actually carry a register, and that the arith
                            // op is reached ONLY by falling out of this call: if it is a branch
                            // target, some other predecessor expects the result in memory.
                            let fuse_op: Option<OpCode> = if (a3_eligible || arg_count == 1)
                                && ip + 2 < code.len()
                                && !blocks.contains_key(&(ip + 2))
                            {
                                match OpCode::from_byte(code[ip + 2]) {
                                    Some(o @ (OpCode::Add | OpCode::Subtract | OpCode::Multiply)) => {
                                        Some(o)
                                    }
                                    _ => None,
                                }
                            } else {
                                None
                            };
                            // One I64 param carries the un-boxed result to the fused arithmetic.
                            let fast_join: Option<Block> = fuse_op.map(|_| {
                                let b = builder.create_block();
                                builder.append_block_param(b, types::I64);
                                b
                            });

                            // Snapshot virt_stack: A3's fallback never runs its pops, so the IC would otherwise flush an empty stack.
                            let virt_snap_pre_a3: Option<Vec<VirtSlot>> = if a3_eligible {
                                Some(virt_stack.snapshot())
                            } else {
                                None
                            };

                            let a3_post_call_block: Option<Block> = if a3_eligible {
                                use cranelift_codegen::ir::MemFlags;
                                use cranelift_codegen::ir::condcodes::IntCC;

                                let flags = MemFlags::trusted();
                                let spec_id = spec_thunk_id.unwrap();

                                // Args are still on expr_stack, so the callee is at stack top, not stack_top - arg_count.
                                let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                                let stack_len = emit_load_stack_len(&mut builder, vm_val);
                                let value_size =
                                    builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                                let one = builder.ins().iconst(types::I64, 1);
                                let callee_slot = builder.ins().isub(stack_len, one);
                                let callee_off = builder.ins().imul(callee_slot, value_size);
                                let callee_ptr = builder.ins().iadd(stack_ptr, callee_off);

                                // Guard: tag == Closure.
                                let tag = builder.ins().load(types::I8, flags, callee_ptr, 0);
                                let closure_tag =
                                    builder.ins().iconst(types::I8, VALUE_TAG_CLOSURE as i64);
                                let is_closure = builder.ins().icmp(IntCC::Equal, tag, closure_tag);

                                let check_rc_block = builder.create_block();
                                let direct_call_block = builder.create_block();
                                let fallback_block = builder.create_block();
                                // Share post_call_block with CA so the IC's tail jump need not multiplex.
                                let post_call_block = match shared_post_call_block {
                                    Some(b) => b,
                                    None => {
                                        let b = builder.create_block();
                                        shared_post_call_block = Some(b);
                                        b
                                    }
                                };

                                builder.ins().brif(
                                    is_closure,
                                    check_rc_block,
                                    &[],
                                    fallback_block,
                                    &[],
                                );

                                builder.switch_to_block(check_rc_block);
                                let curr_rc_raw = builder.ins().load(
                                    ptr_ty,
                                    flags,
                                    callee_ptr,
                                    VALUE_INT_PAYLOAD_OFFSET as i32,
                                );
                                // curr_rc_raw is NonNull<RcBox<ObjClosure>>; JitFrame.closure_raw is adjusted by RC_VALUE_OFFSET.
                                let closure_ptr =
                                    builder.ins().iadd_imm(curr_rc_raw, RC_VALUE_OFFSET as i64);
                                let caller_frame_ptr = own_frame_ptr;
                                let current_closure_raw = builder.ins().load(
                                    ptr_ty,
                                    flags,
                                    caller_frame_ptr,
                                    JitFrame::OFFSET_CLOSURE_RAW,
                                );
                                let rc_matches = builder.ins().icmp(
                                    IntCC::Equal,
                                    closure_ptr,
                                    current_closure_raw,
                                );
                                builder.ins().brif(
                                    rc_matches,
                                    direct_call_block,
                                    &[],
                                    fallback_block,
                                    &[],
                                );

                                // ── Direct-call block ──
                                builder.switch_to_block(direct_call_block);
                                if let Some(cp) = counters_ptr_opt {
                                    emit_counter_bump(
                                        &mut builder,
                                        cp,
                                        counter_offsets::SELF_RECURSION_DIRECT_CALL,
                                    );
                                }


                                // Replicate the VM's FRAMES_MAX guard: without it deep recursion overruns jit_frames and SIGSEGVs.
                                emit_inline_recursion_guard(
                                    &mut builder,
                                    exit_block,
                                    &refs,
                                    vm_val,
                                );

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
                                let frame_size = builder
                                    .ins()
                                    .iconst(types::I64, std::mem::size_of::<JitFrame>() as i64);
                                let frame_off = builder.ins().imul(jit_frames_len, frame_size);
                                let new_frame_ptr = builder.ins().iadd(jit_frames_ptr, frame_off);
                                // The callee's own module, never the caller's — a frame's scope
                                // follows the function that was defined there.
                                let module_globals = builder.ins().load(
                                    ptr_ty,
                                    flags,
                                    closure_ptr,
                                    obj_closure_module_globals_offset(),
                                );
                                let line_val = builder.ins().iconst(types::I32, line as i64);
                                builder.ins().store(
                                    flags,
                                    closure_ptr,
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
                                let new_fl = builder.ins().iadd(jit_frames_len, one64);
                                builder.ins().store(
                                    flags,
                                    new_fl,
                                    vm_val,
                                    vm_jit_frame_view_len_offset(),
                                );

                                // The specialized prologue leaves slot 0 alone; the callee's Return truncates to slot_offset.

                                // Pop in reverse (top is rightmost) then reverse for lhs-first order.
                                let mut call_args: Vec<cranelift_codegen::ir::Value> =
                                    Vec::with_capacity(arg_count as usize + 1);
                                call_args.push(vm_val);
                                let mut popped: Vec<cranelift_codegen::ir::Value> =
                                    Vec::with_capacity(arg_count as usize);
                                for _ in 0..arg_count {
                                    popped.push(virt_stack.pop_int_ssa().unwrap());
                                }
                                popped.reverse();
                                call_args.extend(popped);

                                // A mismatch here fails the verifier and silently drops the whole
                                // function to the interpreter, so catch it in tests rather than in
                                // a benchmark. Not assert!: a release panic beats no fast path by
                                // nothing at all.
                                debug_assert_eq!(
                                    call_args.len(),
                                    1 + func.arity as usize,
                                    "A3 direct call must match the non-closure-aware specialized signature"
                                );

                                let spec_fref =
                                    self.module.declare_func_in_func(spec_id, builder.func);
                                let call = builder.ins().call(spec_fref, &call_args);
                                let results = builder.inst_results(call);
                                let status = results[0];
                                let payload = results[1];

                                // Status: 0 = i64 payload, 3 = boxed result already on the stack, 1/2 = error/bailout.
                                let three_a3 = builder.ins().iconst(types::I32, 3);
                                let is_boxed_a3 = builder.ins().icmp(
                                    cranelift_codegen::ir::condcodes::IntCC::Equal,
                                    status,
                                    three_a3,
                                );
                                let dispatch_a3 = builder.create_block();
                                builder
                                    .ins()
                                    .brif(is_boxed_a3, post_call_block, &[], dispatch_a3, &[]);
                                builder.switch_to_block(dispatch_a3);

                                // Error / bailout propagate.
                                let ok_block_a3 = builder.create_block();
                                let err_block_a3 = builder.create_block();
                                builder.append_block_param(err_block_a3, types::I32);
                                builder.ins().brif(
                                    status,
                                    err_block_a3,
                                    &[status.into()],
                                    ok_block_a3,
                                    &[],
                                );

                                builder.switch_to_block(err_block_a3);
                                let eb_status = builder.block_params(err_block_a3)[0];
                                let zero64 = builder.ins().iconst(types::I64, 0);
                                builder
                                    .ins()
                                    .jump(exit_block, &[eb_status.into(), zero64.into()]);

                                // Callee already truncated the stack to slot_offset, dropping closure and args.
                                builder.switch_to_block(ok_block_a3);
                                match fast_join {
                                    Some(fj) => {
                                        builder.ins().jump(fj, &[payload.into()]);
                                    }
                                    None => {
                                        emit_inline_push_integer(&mut builder, vm_val, payload);
                                        builder.ins().jump(post_call_block, &[]);
                                    }
                                }

                                builder.switch_to_block(fallback_block);
                                // Restore the pre-A3 snapshot: the fallback path never runs those pops, so the IC would read garbage.
                                if let Some(snap) = virt_snap_pre_a3.clone() {
                                    virt_stack.restore(snap);
                                }
                                // Flush so the IC sees args boxed on the VM stack; Call is in handles_own_flush.
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }

                                Some(post_call_block)
                            } else {
                                // A3 ineligible: the IC needs args on the VM stack.
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }
                                None
                            };

                            let _ = a3_post_call_block; // used after IC below

                            // Guard reads vm.stack_view directly: tag == CLOSURE and Rc == cache.closure_raw, else jit_op_call_miss.
                            use cranelift_codegen::ir::MemFlags;
                            use cranelift_codegen::ir::condcodes::IntCC;

                            let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                            let stack_len = emit_load_stack_len(&mut builder, vm_val);

                            let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                            let offset_from_top = builder
                                .ins()
                                .iconst(types::I64, arg_count as i64 + 1);
                            let callee_slot = builder.ins().isub(stack_len, offset_from_top);
                            let callee_off = builder.ins().imul(callee_slot, value_size);
                            let callee_ptr = builder.ins().iadd(stack_ptr, callee_off);

                            let flags = MemFlags::trusted();
                            let tag = builder.ins().load(types::I8, flags, callee_ptr, 0);
                            let closure_tag =
                                builder.ins().iconst(types::I8, VALUE_TAG_CLOSURE as i64);
                            let is_closure = builder.ins().icmp(IntCC::Equal, tag, closure_tag);
                            // Used in both successor paths — define in entry.
                            let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);

                            let check_rc_block = builder.create_block();
                            let hit_block = builder.create_block();
                            let miss_block = builder.create_block();
                            let ok_block = builder.create_block();
                            let err_block = builder.create_block();
                            builder.append_block_param(err_block, types::I32);

                            builder
                                .ins()
                                .brif(is_closure, check_rc_block, &[], miss_block, &[]);

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
                            let rc_matches = builder.ins().icmp(IntCC::Equal, curr_rc, cached_rc);
                            builder
                                .ins()
                                .brif(rc_matches, hit_block, &[], miss_block, &[]);

                            // Hit — cached closure matches.
                            builder.switch_to_block(hit_block);
                            if let Some(cp) = counters_ptr_opt {
                                emit_counter_bump(&mut builder, cp, counter_offsets::CALL_IC_HIT);
                                // A4.0 probe: count cached callees with a NativeIntBody specialized entry.
                                let probe_closure_ptr =
                                    builder.ins().iadd_imm(curr_rc, RC_VALUE_OFFSET as i64);
                                let spec_kind_off = std::mem::offset_of!(
                                    crate::vm::value::ObjClosure,
                                    specialized_kind
                                ) as i32;
                                let spec_kind = builder.ins().load(
                                    types::I8,
                                    flags,
                                    probe_closure_ptr,
                                    spec_kind_off,
                                );
                                let is_native = builder.ins().icmp_imm(
                                    IntCC::Equal,
                                    spec_kind,
                                    crate::vm::value::SPECIALIZED_KIND_NATIVE_INT_BODY as i64,
                                );
                                let probe_bump_block = builder.create_block();
                                let probe_after_block = builder.create_block();
                                builder.ins().brif(
                                    is_native,
                                    probe_bump_block,
                                    &[],
                                    probe_after_block,
                                    &[],
                                );
                                builder.switch_to_block(probe_bump_block);
                                emit_counter_bump(
                                    &mut builder,
                                    cp,
                                    counter_offsets::IC_CALLEE_HAS_SPEC_ENTRY,
                                );
                                builder.ins().jump(probe_after_block, &[]);
                                builder.switch_to_block(probe_after_block);
                                builder.seal_block(probe_bump_block);
                                builder.seal_block(probe_after_block);
                            }

                            // Fires inside the IC hit block after tag+RC match; non-CA cost is 1 kind load + 1 brif.
                            let post_ca_dispatch_block = if arg_count == 1 {
                                // Compute closure_obj_ptr once.
                                let closure_obj_ptr = builder
                                    .ins()
                                    .iadd_imm(curr_rc, RC_VALUE_OFFSET as i64);

                                // Locate the arg slot up front so it can be tag-checked alongside kind/arity.
                                let stack_ptr_ca = emit_load_stack_ptr(&mut builder, vm_val);
                                let stack_len_ca = emit_load_stack_len(&mut builder, vm_val);
                                let value_size_ca =
                                    builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                                let one64_ca = builder.ins().iconst(types::I64, 1);
                                let arg_slot_ca = builder.ins().isub(stack_len_ca, one64_ca);
                                let arg_off_ca = builder.ins().imul(arg_slot_ca, value_size_ca);
                                let arg_addr_ca = builder.ins().iadd(stack_ptr_ca, arg_off_ca);

                                // cache_val is an iconst, so Cranelift can hoist these loads aggressively.
                                let kind_byte = builder.ins().load(
                                    types::I8,
                                    flags,
                                    cache_val,
                                    CallCacheEntry::OFFSET_SPECIALIZED_KIND,
                                );
                                let arity_byte = builder.ins().load(
                                    types::I8,
                                    flags,
                                    cache_val,
                                    16, // CallCacheEntry::OFFSET_ARITY = 16
                                );
                                let arg_tag = builder.ins().load(types::I8, flags, arg_addr_ca, 0);

                                let is_ca_kind = builder.ins().icmp_imm(
                                    IntCC::Equal,
                                    kind_byte,
                                    crate::vm::value::SPECIALIZED_KIND_NATIVE_INT_BODY_WITH_CLOSURE as i64,
                                );
                                let arity_match = builder.ins().icmp_imm(
                                    IntCC::Equal,
                                    arity_byte,
                                    arg_count as i64,
                                );
                                let arg_is_int = builder.ins().icmp_imm(
                                    IntCC::Equal,
                                    arg_tag,
                                    VALUE_TAG_INTEGER as i64,
                                );
                                // Fuse kind + arity + arg_tag into one brif; saves 2 brifs and 2 blocks per site.
                                let kind_and_arity = builder.ins().band(is_ca_kind, arity_match);
                                let all_ok = builder.ins().band(kind_and_arity, arg_is_int);

                                let ca_dispatch_block = builder.create_block();
                                let post_ca_block = builder.create_block();
                                builder.ins().brif(
                                    all_ok,
                                    ca_dispatch_block,
                                    &[],
                                    post_ca_block,
                                    &[],
                                );

                                // ── ca_dispatch_block: take the call ──
                                builder.switch_to_block(ca_dispatch_block);

                                // FRAMES_MAX guard before any mutation; the 256 MB driver stack means this fires before the native stack dies.
                                emit_inline_recursion_guard(
                                    &mut builder,
                                    exit_block,
                                    &refs,
                                    vm_val,
                                );

                                let arg_payload = builder.ins().load(
                                    types::I64,
                                    flags,
                                    arg_addr_ca,
                                    VALUE_INT_PAYLOAD_OFFSET as i32,
                                );

                                // Drop the arg, leave the closure on top; the spec prologue re-bumps len by arity.
                                builder.ins().store(
                                    flags,
                                    arg_slot_ca,
                                    vm_val,
                                    vm_stack_view_len_offset(),
                                );

                                // Callee is at stack[stack_len - 1] after the truncate.
                                let jit_frames_ptr_ca = builder.ins().load(
                                    ptr_ty,
                                    flags,
                                    vm_val,
                                    vm_jit_frame_view_ptr_offset(),
                                );
                                let jit_frames_len_ca = builder.ins().load(
                                    types::I64,
                                    flags,
                                    vm_val,
                                    vm_jit_frame_view_len_offset(),
                                );
                                let frame_size_ca = builder.ins().iconst(
                                    types::I64,
                                    std::mem::size_of::<JitFrame>() as i64,
                                );
                                let frame_off_ca =
                                    builder.ins().imul(jit_frames_len_ca, frame_size_ca);
                                let new_frame_ptr_ca =
                                    builder.ins().iadd(jit_frames_ptr_ca, frame_off_ca);
                                let module_globals_ca = builder.ins().load(
                                    ptr_ty,
                                    flags,
                                    closure_obj_ptr,
                                    obj_closure_module_globals_offset(),
                                );
                                let line_val_ca =
                                    builder.ins().iconst(types::I32, line as i64);
                                builder.ins().store(
                                    flags,
                                    closure_obj_ptr,
                                    new_frame_ptr_ca,
                                    JitFrame::OFFSET_CLOSURE_RAW,
                                );
                                // The spec entry's slot_offset is the closure's position, arg_slot_ca - 1.
                                let callee_slot_ca = builder.ins().iadd_imm(arg_slot_ca, -1);
                                builder.ins().store(
                                    flags,
                                    callee_slot_ca,
                                    new_frame_ptr_ca,
                                    JitFrame::OFFSET_SLOT_OFFSET,
                                );
                                builder.ins().store(
                                    flags,
                                    module_globals_ca,
                                    new_frame_ptr_ca,
                                    JitFrame::OFFSET_MODULE_GLOBALS,
                                );
                                builder.ins().store(
                                    flags,
                                    line_val_ca,
                                    new_frame_ptr_ca,
                                    JitFrame::OFFSET_LINE,
                                );
                                let one_inc_ca = builder.ins().iconst(types::I64, 1);
                                let new_fl_ca =
                                    builder.ins().iadd(jit_frames_len_ca, one_inc_ca);
                                builder.ins().store(
                                    flags,
                                    new_fl_ca,
                                    vm_val,
                                    vm_jit_frame_view_len_offset(),
                                );

                                let mut ca_sig = self.module.make_signature();
                                ca_sig.params.push(AbiParam::new(ptr_ty));
                                ca_sig.params.push(AbiParam::new(ptr_ty));
                                ca_sig.params.push(AbiParam::new(types::I64));
                                ca_sig.returns.push(AbiParam::new(types::I32));
                                ca_sig.returns.push(AbiParam::new(types::I64));
                                let ca_sig_ref = builder.import_signature(ca_sig);

                                // Read spec_thunk from the IC cache: a constant address, no folding ambiguity.
                                let spec_thunk = builder.ins().load(
                                    ptr_ty,
                                    flags,
                                    cache_val,
                                    CallCacheEntry::OFFSET_SPECIALIZED_THUNK,
                                );

                                let ca_call_inst = builder.ins().call_indirect(
                                    ca_sig_ref,
                                    spec_thunk,
                                    &[vm_val, closure_obj_ptr, arg_payload],
                                );
                                let ca_results = builder.inst_results(ca_call_inst);
                                let ca_status = ca_results[0];
                                let ca_payload = ca_results[1];

                                let ca_pcb = match shared_post_call_block {
                                    Some(b) => b,
                                    None => {
                                        let b = builder.create_block();
                                        shared_post_call_block = Some(b);
                                        b
                                    }
                                };
                                // Status 3 = boxed result on the stack; the callee already tore down its frame.
                                let three_ca = builder.ins().iconst(types::I32, 3);
                                let is_boxed_ca = builder.ins().icmp(
                                    cranelift_codegen::ir::condcodes::IntCC::Equal,
                                    ca_status,
                                    three_ca,
                                );
                                let ca_dispatch = builder.create_block();
                                builder.ins().brif(is_boxed_ca, ca_pcb, &[], ca_dispatch, &[]);
                                builder.switch_to_block(ca_dispatch);

                                let ca_ok_block = builder.create_block();
                                let ca_err_block = builder.create_block();
                                builder.append_block_param(ca_err_block, types::I32);
                                builder.ins().brif(
                                    ca_status,
                                    ca_err_block,
                                    &[ca_status.into()],
                                    ca_ok_block,
                                    &[],
                                );

                                // ── ca_err_block: rollback frame, propagate ──
                                builder.switch_to_block(ca_err_block);
                                let ca_err_status = builder.block_params(ca_err_block)[0];
                                builder.ins().store(
                                    flags,
                                    jit_frames_len_ca,
                                    vm_val,
                                    vm_jit_frame_view_len_offset(),
                                );
                                let zero64_ca = builder.ins().iconst(types::I64, 0);
                                builder.ins().jump(
                                    exit_block,
                                    &[ca_err_status.into(), zero64_ca.into()],
                                );

                                // ── ca_ok_block: push int payload, jump pcb ──
                                builder.switch_to_block(ca_ok_block);
                                if let Some(cp) = counters_ptr_opt {
                                    emit_counter_bump(
                                        &mut builder,
                                        cp,
                                        counter_offsets::CLOSURE_AWARE_CALL_DISPATCH,
                                    );
                                }
                                match fast_join {
                                    Some(fj) => {
                                        builder.ins().jump(fj, &[ca_payload.into()]);
                                    }
                                    None => {
                                        emit_inline_push_integer(&mut builder, vm_val, ca_payload);
                                        builder.ins().jump(ca_pcb, &[]);
                                    }
                                }

                                builder.switch_to_block(post_ca_block);
                                Some(post_ca_block)
                            } else {
                                None
                            };
                            let _ = post_ca_dispatch_block;

                            // FRAMES_MAX guard before the push; the 256 MB driver stack means this fires before the native stack dies.
                            emit_inline_recursion_guard(
                                &mut builder,
                                exit_block,
                                &refs,
                                vm_val,
                            );

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
                            let frame_size = builder
                                .ins()
                                .iconst(types::I64, std::mem::size_of::<JitFrame>() as i64);
                            let frame_off = builder.ins().imul(jit_frames_len, frame_size);
                            let new_frame_ptr = builder.ins().iadd(jit_frames_ptr, frame_off);
                            let thunk_raw = builder.ins().load(
                                ptr_ty,
                                flags,
                                cache_val,
                                CallCacheEntry::OFFSET_THUNK_RAW,
                            );
                            let line_val = builder.ins().iconst(types::I32, line as i64);
                            // Adjust by RC_VALUE_OFFSET so closure_raw points at ObjClosure, not the RcBox header.
                            let closure_t_ptr =
                                builder.ins().iadd_imm(curr_rc, RC_VALUE_OFFSET as i64);
                            let module_globals = builder.ins().load(
                                ptr_ty,
                                flags,
                                closure_t_ptr,
                                obj_closure_module_globals_offset(),
                            );
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
                            // Status 2 is the callee's entry-guard bailout, not an error: propagating it re-ran the caller mid-loop.
                            let hit_nonzero_block = builder.create_block();
                            builder.ins().brif(
                                hit_status,
                                hit_nonzero_block,
                                &[],
                                ok_block,
                                &[],
                            );

                            builder.switch_to_block(hit_nonzero_block);
                            let bail_block = builder.create_block();
                            let is_bailout = builder.ins().icmp_imm(
                                cranelift_codegen::ir::condcodes::IntCC::Equal,
                                hit_status,
                                2,
                            );
                            builder.ins().brif(
                                is_bailout,
                                bail_block,
                                &[],
                                restore_err_block,
                                &[hit_status.into()],
                            );

                            builder.switch_to_block(bail_block);
                            builder.ins().store(
                                flags,
                                jit_frames_len,
                                vm_val,
                                vm_jit_frame_view_len_offset(),
                            );
                            builder.ins().jump(miss_block, &[]);

                            builder.switch_to_block(restore_err_block);
                            let err_status = builder.block_params(restore_err_block)[0];
                            builder.ins().store(
                                flags,
                                jit_frames_len,
                                vm_val,
                                vm_jit_frame_view_len_offset(),
                            );
                            builder.ins().jump(err_block, &[err_status.into()]);

                            // Miss — fallback + populate cache.
                            builder.switch_to_block(miss_block);
                            if let Some(cp) = counters_ptr_opt {
                                emit_counter_bump(&mut builder, cp, counter_offsets::CALL_IC_MISS);
                            }
                            let ac_val = builder.ins().iconst(types::I32, arg_count as i64);
                            let miss_call = builder
                                .ins()
                                .call(refs.op_call_miss, &[vm_val, ac_val, cache_val]);
                            let miss_status = builder.inst_results(miss_call)[0];
                            builder.ins().brif(
                                miss_status,
                                err_block,
                                &[miss_status.into()],
                                ok_block,
                                &[],
                            );

                            // Error exit.
                            builder.switch_to_block(err_block);
                            let err_status = builder.block_params(err_block)[0];
                            let zero64 = builder.ins().iconst(types::I64, 0);
                            builder
                                .ins()
                                .jump(exit_block, &[err_status.into(), zero64.into()]);

                            builder.switch_to_block(ok_block);

                            // Converge A3, closure-aware and generic IC success into one continuation.
                            let _ = a3_post_call_block;
                            if let Some(pcb) = shared_post_call_block {
                                builder.ins().jump(pcb, &[]);
                                builder.switch_to_block(pcb);
                            }

                            match (fuse_op, fast_join) {
                                (Some(fop), Some(fj)) => {
                                    // Emit the fused arithmetic twice — once per result location.
                                    // Both sides end with the result at the same memory slot and an
                                    // empty virt stack, so exactly one opcode needs duplicating.
                                    let fip = ip + 2;
                                    let fline = chunk.lines.get(fip).copied().unwrap_or(0);
                                    let fcol = chunk.columns.get(fip).copied().unwrap_or(0);
                                    let after_fuse = builder.create_block();

                                    // Slow side: result was boxed onto the stack, so both operands
                                    // are in memory — byte-for-byte the pre-fusion emission.
                                    emit_store_current_loc(
                                        &mut builder,
                                        own_frame_ptr,
                                        fline,
                                        fcol,
                                    );
                                    let (arith_op, slow_helper) = match fop {
                                        OpCode::Add => (IntArithOp::Add, refs.add),
                                        OpCode::Subtract => (IntArithOp::Sub, refs.sub),
                                        OpCode::Multiply => (IntArithOp::Mul, refs.mul),
                                        _ => unreachable!(),
                                    };
                                    emit_int_fast_arith(
                                        &mut builder,
                                        exit_block,
                                        &refs,
                                        vm_val,
                                        arith_op,
                                        slow_helper,
                                    );
                                    builder.ins().jump(after_fuse, &[]);

                                    // Fast side: the result never reached memory, so the stack top
                                    // is still the lhs and the register is the rhs — the same
                                    // operand roles the mixed-mode path already handles.
                                    builder.switch_to_block(fj);
                                    let reg_result = builder.block_params(fj)[0];
                                    emit_store_current_loc(
                                        &mut builder,
                                        own_frame_ptr,
                                        fline,
                                        fcol,
                                    );
                                    emit_mixed_mode_int_arith(
                                        &mut builder,
                                        exit_block,
                                        &refs,
                                        vm_val,
                                        reg_result,
                                        fop,
                                    );
                                    builder.ins().jump(after_fuse, &[]);

                                    builder.switch_to_block(after_fuse);
                                    builder.seal_block(fj);
                                    builder.seal_block(after_fuse);
                                    virt_stack = VirtStack::new();
                                    last_emitted_loc = Some((fline, fcol));
                                    ip += 3;
                                }
                                _ => {
                                    ip += 2;
                                }
                            }
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
                            emit_early_exit_on_err(&mut builder, exit_block, status);
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
                                .brif(status, err_block, &[status.into()], ok_block, &[]);

                            builder.switch_to_block(err_block);
                            let err_status = builder.block_params(err_block)[0];
                            let zero64 = builder.ins().iconst(types::I64, 0);
                            builder
                                .ins()
                                .jump(exit_block, &[err_status.into(), zero64.into()]);
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
                                .brif(status, err_block, &[status.into()], ok_block, &[]);

                            builder.switch_to_block(err_block);
                            let err_status = builder.block_params(err_block)[0];
                            let zero64 = builder.ins().iconst(types::I64, 0);
                            builder
                                .ins()
                                .jump(exit_block, &[err_status.into(), zero64.into()]);
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
                            emit_early_exit_on_err(&mut builder, exit_block, status);
                            ip += 4;
                        }
                        OpCode::MethodCall => {
                            let method_idx = read_u16(code, ip + 1);
                            let arg_count = code[ip + 3];
                            let cache_ptr = method_cache_ptrs[method_ic_ix];
                            method_ic_ix += 1;

                            // Bump the method closure's strong count, commit Vec::len after raw stores, roll back the frame on error.
                            if arg_count <= 1 {
                                use cranelift_codegen::ir::MemFlags;
                                use cranelift_codegen::ir::condcodes::IntCC;

                                let mi_val = builder.ins().iconst(types::I32, method_idx as i64);
                                let ac_val = builder.ins().iconst(types::I32, arg_count as i64);
                                let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);
                                let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                                let stack_len = emit_load_stack_len(&mut builder, vm_val);
                                let value_size =
                                    builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                                let offset_from_top = builder
                                    .ins()
                                    .iconst(types::I64, arg_count as i64 + 1);
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
                                let struct_tag = builder
                                    .ins()
                                    .iconst(types::I8, VALUE_TAG_STRUCT_INSTANCE as i64);
                                let is_struct = builder.ins().icmp(IntCC::Equal, tag, struct_tag);
                                builder.ins().brif(
                                    is_struct,
                                    check_def_block,
                                    &[],
                                    miss_block,
                                    &[],
                                );

                                builder.switch_to_block(check_def_block);
                                let receiver_raw = builder.ins().load(
                                    ptr_ty,
                                    flags,
                                    receiver_ptr,
                                    VALUE_INT_PAYLOAD_OFFSET as i32,
                                );
                                let inst_ptr =
                                    builder.ins().iadd_imm(receiver_raw, RC_VALUE_OFFSET as i64);
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
                                    builder
                                        .ins()
                                        .icmp(IntCC::Equal, inst_def_raw, cached_def_raw);

                                // Skip the JitFrame push and emit the method body's peephole directly.
                                let inline_dispatch_block = builder.create_block();
                                builder.ins().brif(
                                    def_matches,
                                    inline_dispatch_block,
                                    &[],
                                    miss_block,
                                    &[],
                                );

                                // inline_kind is only populated for a matching arity, so a stale kind cannot fire.
                                builder.switch_to_block(inline_dispatch_block);
                                let inline_kind_byte = builder.ins().load(
                                    types::I8,
                                    flags,
                                    cache_val,
                                    MethodCacheEntry::OFFSET_INLINE_KIND,
                                );
                                let none_kind = builder.ins().iconst(types::I8, 0);
                                let is_none_kind =
                                    builder
                                        .ins()
                                        .icmp(IntCC::Equal, inline_kind_byte, none_kind);

                                let target_kind: i64 = match arg_count {
                                    0 => 1, // FieldAddConst
                                    1 => 2, // FieldAddLocal
                                    _ => 0, // no inline for other arities
                                };
                                let inline_body_block = builder.create_block();
                                let kind_ok_block = builder.create_block();
                                builder.ins().brif(
                                    is_none_kind,
                                    hit_block,
                                    &[],
                                    kind_ok_block,
                                    &[],
                                );

                                builder.switch_to_block(kind_ok_block);
                                let expected_kind = builder.ins().iconst(types::I8, target_kind);
                                let matches_expected = builder.ins().icmp(
                                    IntCC::Equal,
                                    inline_kind_byte,
                                    expected_kind,
                                );
                                builder.ins().brif(
                                    matches_expected,
                                    inline_body_block,
                                    &[],
                                    hit_block,
                                    &[],
                                );

                                // ── Inline body ─────────────────────────────
                                builder.switch_to_block(inline_body_block);
                                // Guard strong > 1 so the in-place decrement cannot drop the instance.
                                let receiver_rcbox = receiver_raw;
                                let strong =
                                    builder.ins().load(types::I64, flags, receiver_rcbox, 0);
                                let one_i64 = builder.ins().iconst(types::I64, 1);
                                let strong_is_one =
                                    builder.ins().icmp(IntCC::Equal, strong, one_i64);
                                let after_strong_block = builder.create_block();
                                builder.ins().brif(
                                    strong_is_one,
                                    miss_block,
                                    &[],
                                    after_strong_block,
                                    &[],
                                );
                                builder.switch_to_block(after_strong_block);

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
                                let field_slot_off = builder.ins().imul(field_idx_i64, value_size);
                                let field_slot_ptr = builder.ins().iadd(fields_ptr, field_slot_off);

                                // Guard: current field value is Integer.
                                let field_tag =
                                    builder.ins().load(types::I8, flags, field_slot_ptr, 0);
                                let int_tag =
                                    builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
                                let field_is_int =
                                    builder.ins().icmp(IntCC::Equal, field_tag, int_tag);
                                let after_field_int_block = builder.create_block();
                                builder.ins().brif(
                                    field_is_int,
                                    after_field_int_block,
                                    &[],
                                    miss_block,
                                    &[],
                                );
                                builder.switch_to_block(after_field_int_block);

                                // For Local, also tag-guard the arg.
                                let rhs = if arg_count == 0 {
                                    builder.ins().load(
                                        types::I64,
                                        flags,
                                        cache_val,
                                        MethodCacheEntry::OFFSET_INLINE_ADDEND,
                                    )
                                } else {
                                    // Arg sits at stack[len - 1].
                                    let arg_slot = builder.ins().isub(stack_len, one_i64);
                                    let arg_off = builder.ins().imul(arg_slot, value_size);
                                    let arg_ptr = builder.ins().iadd(stack_ptr, arg_off);
                                    let arg_tag = builder.ins().load(types::I8, flags, arg_ptr, 0);
                                    let arg_is_int =
                                        builder.ins().icmp(IntCC::Equal, arg_tag, int_tag);
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

                                // Tag is already Integer, so only the payload needs rewriting.
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

                                // Guarded strong > 1 above, so this decrement never has to run Drop.
                                let strong_dec = builder.ins().iadd_imm(strong, -1);
                                builder.ins().store(flags, strong_dec, receiver_rcbox, 0);

                                // Tag-only write: Value::None has no Drop and the JIT only reads the tag.
                                let none_tag =
                                    builder.ins().iconst(types::I8, VALUE_TAG_NONE as i64);
                                builder.ins().store(flags, none_tag, receiver_ptr, 0);

                                // The arg was guarded to Integer, so there is no Drop to run.
                                if arg_count == 1 {
                                    emit_inline_stack_pop_one(&mut builder, vm_val);
                                }
                                builder.ins().jump(ok_block, &[]);

                                // ── Fall-through to full path below ────────
                                builder.switch_to_block(hit_block);
                                // Need both pointers: RcBox for the stack Value, T for JitFrame.closure_raw. FRAMES_MAX checked before any mutation.
                                emit_inline_recursion_guard(
                                    &mut builder,
                                    exit_block,
                                    &refs,
                                    vm_val,
                                );
                                let closure_raw_box = builder.ins().load(
                                    ptr_ty,
                                    flags,
                                    cache_val,
                                    MethodCacheEntry::OFFSET_CLOSURE_RAW,
                                );
                                // Balance the Drop that stack teardown will run; without it the RcBox is freed while the cache keeper still points at it.
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
                                emit_write_closure_value(
                                    &mut builder,
                                    receiver_ptr,
                                    closure_raw_box,
                                );
                                let new_stack_len = builder.ins().iadd_imm(stack_len, 1);
                                // Commit Vec::len or bounds-checked helpers OOB-panic on the next non-inline local access.
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
                                let frame_size = builder
                                    .ins()
                                    .iconst(types::I64, std::mem::size_of::<JitFrame>() as i64);
                                let frame_off = builder.ins().imul(jit_frames_len, frame_size);
                                let new_frame_ptr = builder.ins().iadd(jit_frames_ptr, frame_off);
                                let module_globals = builder.ins().load(
                                    ptr_ty,
                                    flags,
                                    closure_raw_t,
                                    obj_closure_module_globals_offset(),
                                );
                                // JitFrame stores the T pointer, not the RcBox pointer.
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
                                    &[hit_status.into()],
                                    ok_block,
                                    &[],
                                );
                                builder.switch_to_block(restore_err_block);
                                let err_status = builder.block_params(restore_err_block)[0];
                                // stack_view.len is deliberately not rolled back: those Values are bit-valid and Drop correctly during outer teardown.
                                builder.ins().store(
                                    flags,
                                    jit_frames_len,
                                    vm_val,
                                    vm_jit_frame_view_len_offset(),
                                );
                                builder.ins().jump(err_block, &[err_status.into()]);

                                builder.switch_to_block(miss_block);
                                let call = builder.ins().call(
                                    refs.op_method_call_ic,
                                    &[vm_val, mi_val, ac_val, cache_val],
                                );
                                let status = builder.inst_results(call)[0];
                                builder.ins().brif(
                                    status,
                                    err_block,
                                    &[status.into()],
                                    ok_block,
                                    &[],
                                );

                                builder.switch_to_block(err_block);
                                let err_status = builder.block_params(err_block)[0];
                                let zero64 = builder.ins().iconst(types::I64, 0);
                                builder
                                    .ins()
                                    .jump(exit_block, &[err_status.into(), zero64.into()]);

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
                                emit_early_exit_on_err(&mut builder, exit_block, status);
                            }
                            ip += 4;
                        }

                        OpCode::Return => {
                            match kind {
                                EntryKind::Generic => {
                                    // Inline teardown when no upvalue capture, all slots are primitives, and the closure's strong count is > 1.
                                    let inline_eligible = !info.may_capture_upvalues
                                        && return_slots_safe_to_truncate(
                                            &slot_types,
                                            &param_mirrors,
                                            func,
                                        );

                                    if inline_eligible {
                                        emit_inline_generic_return(
                                            &mut builder,
                                            exit_block,
                                            &refs,
                                            vm_val,
                                            slot_offset_val,
                                            &mut virt_stack,
                                        );
                                    } else {
                                        // Needed when locals hold Rc-bearing Values or a nested closure captured this frame's slots.
                                        builder.ins().call(refs.op_return, &[vm_val]);
                                        let zero32 = builder.ins().iconst(types::I32, 0);
                                        let zero64 = builder.ins().iconst(types::I64, 0);
                                        builder
                                            .ins()
                                            .jump(exit_block, &[zero32.into(), zero64.into()]);
                                    }
                                }
                                EntryKind::IntSpecialized => {
                                    // Raw i64 payload only when the return is provably Integer; otherwise status 3 leaves it boxed on the stack.
                                    use cranelift_codegen::ir::MemFlags;
                                    use cranelift_codegen::ir::condcodes::IntCC;
                                    let flags = MemFlags::trusted();

                                    macro_rules! emit_intspec_teardown {
                                        () => {{
                                            builder.ins().store(
                                                flags,
                                                slot_offset_val,
                                                vm_val,
                                                vm_stack_view_len_offset(),
                                            );
                                            let fl = builder.ins().load(
                                                types::I64,
                                                flags,
                                                vm_val,
                                                vm_jit_frame_view_len_offset(),
                                            );
                                            let one = builder.ins().iconst(types::I64, 1);
                                            let new_fl = builder.ins().isub(fl, one);
                                            builder.ins().store(
                                                flags,
                                                new_fl,
                                                vm_val,
                                                vm_jit_frame_view_len_offset(),
                                            );
                                        }};
                                    }

                                    if let Some(top) = virt_stack.pop_int_ssa() {
                                        // Provably Integer — fast i64 return.
                                        emit_intspec_teardown!();
                                        let zero32 = builder.ins().iconst(types::I32, 0);
                                        builder
                                            .ins()
                                            .jump(exit_block, &[zero32.into(), top.into()]);
                                    } else {
                                        // Return value is on the VM stack; tag-check.
                                        let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                                        let stack_len = emit_load_stack_len(&mut builder, vm_val);
                                        let value_size =
                                            builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                                        let one = builder.ins().iconst(types::I64, 1);
                                        let top_idx = builder.ins().isub(stack_len, one);
                                        let byte_off = builder.ins().imul(top_idx, value_size);
                                        let top_addr = builder.ins().iadd(stack_ptr, byte_off);
                                        let tag = builder.ins().load(types::I8, flags, top_addr, 0);
                                        let int_tag = builder
                                            .ins()
                                            .iconst(types::I8, VALUE_TAG_INTEGER as i64);
                                        let is_int =
                                            builder.ins().icmp(IntCC::Equal, tag, int_tag);
                                        let int_block = builder.create_block();
                                        let boxed_block = builder.create_block();
                                        builder
                                            .ins()
                                            .brif(is_int, int_block, &[], boxed_block, &[]);

                                        // Integer → fast i64 return.
                                        builder.switch_to_block(int_block);
                                        let payload = builder.ins().load(
                                            types::I64,
                                            flags,
                                            top_addr,
                                            VALUE_INT_PAYLOAD_OFFSET as i32,
                                        );
                                        emit_intspec_teardown!();
                                        let zero32 = builder.ins().iconst(types::I32, 0);
                                        builder
                                            .ins()
                                            .jump(exit_block, &[zero32.into(), payload.into()]);

                                        // Non-Integer: return generically and report status 3 = result on stack.
                                        builder.switch_to_block(boxed_block);
                                        builder.ins().call(refs.op_return, &[vm_val]);
                                        let three = builder.ins().iconst(types::I32, 3);
                                        let zero64 = builder.ins().iconst(types::I64, 0);
                                        builder
                                            .ins()
                                            .jump(exit_block, &[three.into(), zero64.into()]);
                                    }
                                }
                            }
                            terminated = true;
                            ip += 1;
                        }

                        // Scan should have rejected these — defensive.
                        _ => return Err(()),
                    }
                }

                // Defensive: every path should end in Return, so bail out via the shared exit block.
                if !terminated {
                    let two = builder.ins().iconst(types::I32, 2);
                    let zero64 = builder.ins().iconst(types::I64, 0);
                    builder.ins().jump(exit_block, &[two.into(), zero64.into()]);
                }

                // Generic returns a status; IntSpecialized returns (status, payload).
                builder.switch_to_block(exit_block);
                let exit_status = builder.block_params(exit_block)[0];
                let exit_payload = builder.block_params(exit_block)[1];
                match kind {
                    EntryKind::Generic => {
                        let _ = exit_payload;
                        builder.ins().return_(&[exit_status]);
                    }
                    EntryKind::IntSpecialized => {
                        builder.ins().return_(&[exit_status, exit_payload]);
                    }
                }

                builder.seal_all_blocks();
                builder.finalize();
            }

            // OXIGEN_JIT_DISASM=<name> dumps that function's disasm; ALL dumps every one.
            let disasm_target = std::env::var("OXIGEN_JIT_DISASM").ok();
            let want_disasm = match disasm_target.as_deref() {
                Some("ALL") => true,
                Some(name) => name == func.name.as_deref().unwrap_or(""),
                None => false,
            };
            if want_disasm {
                self.ctx.want_disasm = true;
            }

            self.module
                .define_function(job.func_id, &mut self.ctx)
                .map_err(|e| {
                    if std::env::var("OXIGEN_JIT_DEBUG").is_ok() {
                        eprintln!("[jit] define_function failed for {:?}: {:?}", kind, e,);
                    }
                })?;

            if want_disasm
                && let Some(code) = self.ctx.compiled_code()
                    && let Some(vcode) = code.vcode.as_ref() {
                        eprintln!(
                            "=== disasm fn={} kind={:?} ===\n{}",
                            func.name.as_deref().unwrap_or("<anon>"),
                            kind,
                            vcode
                        );
                    }

            self.module.clear_context(&mut self.ctx);
        }

        self.module.finalize_definitions().map_err(|_| ())?;

        let generic_ptr = self.module.get_finalized_function(thunk_id);
        let generic: CompiledThunk = unsafe { std::mem::transmute(generic_ptr) };

        let (specialized, specialized_arity, specialized_kind) = if let Some(sid) = spec_thunk_id {
            let raw = self.module.get_finalized_function(sid);
            // The caller IC gates on this kind to pass the closure pointer in the second register arg.
            let kind = if spec_wants_closure_arg {
                SpecializedEntryKind::NativeIntBodyWithClosure
            } else {
                SpecializedEntryKind::NativeIntBody
            };
            (Some(raw as SpecializedThunkRaw), func.arity, Some(kind))
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
}

impl Drop for JitInner {
    fn drop(&mut self) {
        if std::env::var("OXIGEN_JIT_STATS").is_ok() {
            self.counters.dump();
        }
    }
}

// ── Helper registration & declaration ──────────────────────────────────

// ── Fallible-helper emission ───────────────────────────────────────────

/// Emit a call to a fallible helper, then a check: if status != 0, return
/// early from the thunk with that status. Otherwise continue in a fresh
/// "ok" block.
fn emit_fallible(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    helper: FuncRef,
    vm_val: cranelift_codegen::ir::Value,
) {
    let call = builder.ins().call(helper, &[vm_val]);
    let status = builder.inst_results(call)[0];
    emit_early_exit_on_err(builder, exit_block, status);
}

/// Given a u32 status produced by a helper call, branch on status != 0:
/// non-zero returns the status from the thunk; zero continues in a fresh
/// "ok" block.
fn emit_early_exit_on_err(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    status: cranelift_codegen::ir::Value,
) {
    let err_block = builder.create_block();
    let ok_block = builder.create_block();
    builder.ins().brif(status, err_block, &[], ok_block, &[]);
    builder.switch_to_block(err_block);
    let zero64 = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[status.into(), zero64.into()]);
    builder.switch_to_block(ok_block);
}

/// Inline replacement for the per-call `check_recursion_depth` FFI guard.
/// The inline JIT call paths push a `JitFrame` + operands directly into the
/// pre-allocated `jit_frames` (cap `FRAMES_MAX`) and value-stack (cap
/// `STACK_MAX`) buffers, so each recursive call must bounds-check before the
/// push or it overruns the buffer and SIGSEGVs (the V7 guard). The old code
/// crossed into an FFI helper for that check on EVERY call — millions of
/// crossings on a recursive workload (fib(30) ≈ 2.7M). Here the common path
/// is two loads + two compares + a branch; only the rare overflow crosses
/// into the helper to stash the exact graceful "stack overflow" error and
/// exit. The inline bound omits the (constant, ≥0) interpreter-frame count
/// the helper adds, so it triggers no earlier than the buffers fill — it
/// still can't overrun (no SIGSEGV) and still raises `Err` on unbounded
/// recursion, matching the guard's contract. (`fix_jit_recursion_guard`.)
fn emit_inline_recursion_guard(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlags;
    use cranelift_codegen::ir::condcodes::IntCC;
    let flags = MemFlags::trusted();
    let jfl = builder
        .ins()
        .load(types::I64, flags, vm_val, vm_jit_frame_view_len_offset());
    let sl = builder
        .ins()
        .load(types::I64, flags, vm_val, vm_stack_view_len_offset());
    let frame_max = builder
        .ins()
        .iconst(types::I64, crate::vm::FRAMES_MAX as i64);
    let stack_lim = builder
        .ins()
        .iconst(types::I64, (crate::vm::STACK_MAX - 8192) as i64);
    let frame_of = builder
        .ins()
        .icmp(IntCC::UnsignedGreaterThanOrEqual, jfl, frame_max);
    let stack_of = builder
        .ins()
        .icmp(IntCC::UnsignedGreaterThanOrEqual, sl, stack_lim);
    let overflow = builder.ins().bor(frame_of, stack_of);
    let ovf_block = builder.create_block();
    let ok_block = builder.create_block();
    builder.ins().brif(overflow, ovf_block, &[], ok_block, &[]);
    // Rare overflow: the helper re-checks and stashes the graceful error.
    builder.switch_to_block(ovf_block);
    let call = builder.ins().call(refs.check_recursion_depth, &[vm_val]);
    let status = builder.inst_results(call)[0];
    let zero64 = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[status.into(), zero64.into()]);
    builder.switch_to_block(ok_block);
}

// ── Bytecode helpers ───────────────────────────────────────────────────

fn read_u16(code: &[u8], offset: usize) -> u16 {
    ((code[offset] as u16) << 8) | (code[offset + 1] as u16)
}

/// Count inline-cache sites so the translator can pre-allocate their slots.
/// The scan pass has already validated the bytecode, and instruction
/// boundaries come from the canonical decoder in `compiler::opcode`.
fn count_ic_sites(chunk: &crate::compiler::opcode::Chunk) -> (usize, usize, usize, usize) {
    let code = &chunk.code;
    let mut get_globals = 0usize;
    let mut calls = 0usize;
    let mut method_calls = 0usize;
    let mut field_ops = 0usize;
    let mut ip = 0;
    while ip < code.len() {
        let op = OpCode::from_byte(code[ip]).expect("scan accepted an invalid opcode");
        match op {
            OpCode::GetGlobal => get_globals += 1,
            OpCode::Call => calls += 1,
            OpCode::MethodCall => method_calls += 1,
            OpCode::GetField | OpCode::SetField => field_ops += 1,
            _ => {}
        }
        ip += chunk
            .instruction_len(ip)
            .expect("scan accepted malformed bytecode");
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
pub(crate) fn detect_inline_method_info(func: &Function) -> Option<DetectedInlineMethod> {
    let code = &func.chunk.code;
    // Accept either shape: the compiler emits an implicit None/Return tail when the body has no return value.
    if code.len() < 17 {
        return None;
    }

    // Empty blocks map: the peephole must cover the whole body, so no branch targets inside it.
    let empty_blocks: HashMap<usize, Block> = HashMap::new();
    let m = match_struct_field_add_update(code, &func.chunk, 0, &empty_blocks)?;

    // The tail must be `Return` alone or `None Return`.
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
    let field_name: std::rc::Rc<String> = std::rc::Rc::clone(field_name_val.as_string()?);

    // `self` is slot 1 (slot 0 is the closure marker), so FieldAddConst is arity 1 and FieldAddLocal arity 2.
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

    // Compare resolved field names, not constant indices: the parser can emit two constants for one identifier.
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

    // Do not fuse across blocks: a jump into the middle expects the original stack effects.
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

    // Do not fuse across blocks: a jump into the middle expects the original stack effects.
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

// 64-bit ABI only: pointers and usize are I64. Reading stack state inline avoids a ~3-5ns FFI crossing.

/// Emit IR that loads `vm.stack_view.ptr` at its pinned offset.
#[inline]
fn emit_load_stack_ptr(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    use cranelift_codegen::ir::MemFlags;
    builder.ins().load(
        types::I64,
        MemFlags::trusted(),
        vm_val,
        vm_stack_view_ptr_offset(),
    )
}

/// Emit IR that loads `vm.stack_view.len` at its pinned offset.
#[inline]
fn emit_load_stack_len(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    use cranelift_codegen::ir::MemFlags;
    builder.ins().load(
        types::I64,
        MemFlags::trusted(),
        vm_val,
        vm_stack_view_len_offset(),
    )
}

#[allow(dead_code)]
#[inline]
fn emit_load_top_jit_frame_ptr(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    use cranelift_codegen::ir::MemFlags;

    let base = builder.ins().load(
        types::I64,
        MemFlags::trusted(),
        vm_val,
        vm_jit_frame_view_ptr_offset(),
    );
    let len = builder.ins().load(
        types::I64,
        MemFlags::trusted(),
        vm_val,
        vm_jit_frame_view_len_offset(),
    );
    let one = builder.ins().iconst(types::I64, 1);
    let frame_size = builder
        .ins()
        .iconst(types::I64, std::mem::size_of::<JitFrame>() as i64);
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
    builder.ins().load(
        types::I64,
        MemFlags::trusted(),
        frame_ptr,
        JitFrame::OFFSET_SLOT_OFFSET,
    )
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
        let word = builder.ins().load(types::I64, flags, src_ptr, off as i32);
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
    builder
        .ins()
        .store(flags, closure_raw, dst_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
}

/// Records `(line, col)` in this activation's frame for error attribution.
///
/// `frame_ptr` is the caller's hoisted `own_frame_ptr` — this always writes the
/// *running* function's frame, never whatever is on top at the moment.
///
/// `line` and `column` are adjacent `u32`s at offsets 24 and 28 of a `#[repr(C)]`
/// `JitFrame` whose total size is 32, so one 8-byte store covers both exactly and
/// cannot overrun. Little-endian puts `line` in the low half.
#[allow(dead_code)]
#[inline]
fn emit_store_current_loc(
    builder: &mut FunctionBuilder<'_>,
    frame_ptr: cranelift_codegen::ir::Value,
    line: u32,
    col: u32,
) {
    use cranelift_codegen::ir::MemFlags;

    const _: () = assert!(
        JitFrame::OFFSET_COLUMN == JitFrame::OFFSET_LINE + 4
            && std::mem::size_of::<JitFrame>() == JitFrame::OFFSET_COLUMN as usize + 4,
        "packed loc store requires line/column adjacent and ending the struct"
    );
    #[cfg(not(target_endian = "little"))]
    compile_error!("packed loc store assumes little-endian field order");

    let packed = (line as u64) | ((col as u64) << 32);
    let packed_val = builder.ins().iconst(types::I64, packed as i64);
    builder.ins().store(
        MemFlags::trusted(),
        packed_val,
        frame_ptr,
        JitFrame::OFFSET_LINE,
    );
}

/// Conditional line-store helper used by per-opcode arms whose fault
/// path needs the line written, but whose fast path doesn't. Updates
/// the `last_emitted_line` cache so subsequent same-line ops (within
/// the same block) skip the store.
#[inline]
fn maybe_emit_current_line(
    builder: &mut FunctionBuilder<'_>,
    frame_ptr: cranelift_codegen::ir::Value,
    line: u32,
    col: u32,
    last_emitted_loc: &mut Option<(u32, u32)>,
) {
    if *last_emitted_loc != Some((line, col)) {
        emit_store_current_loc(builder, frame_ptr, line, col);
        *last_emitted_loc = Some((line, col));
    }
}

/// True iff this opcode's emission path always reaches a runtime
/// helper that may stash an error attributed to *this* IP. Arms that
/// return `false` here are responsible for calling
/// `maybe_emit_current_line` themselves at any internal slow-path
/// emission point that can fault.
///
/// The classification mirrors what `runtime.rs` actually does:
/// helpers that call `vm.jit.stash_error()` (or that re-enter the
/// interpreter via `execute_until`) need a line; helpers that are
/// infallible (`jit_get_local`, `jit_set_local`, `jit_pop`, the
/// stack-len shrink helpers, the truthy-check helpers) do not.
///
/// `OpCode::Add | Subtract | Multiply | Divide | Modulo | Less |
/// LessEqual | Greater | GreaterEqual | Equal | NotEqual` are
/// **conditionally** faulting — when both operands are virt-int the
/// emitted IR is purely register arithmetic with no helper call.
/// Returning `false` here lets the all-Int hot path (the common case
/// for typed loops) skip the line store entirely; the slow-path emit
/// sites inside each arm are responsible for re-emitting.
fn opcode_always_needs_line(op: OpCode) -> bool {
    use OpCode::*;
    match op {
        // Pure stack/arith/control-flow with no fault path.
        Constant | None | True | False | Pop | Dup
        | GetLocal | SetLocal
        | GetUpvalue | SetUpvalue | CloseUpvalue
        | StructDef
        | Jump | Loop | JumpIfFalse | JumpIfTrue | PopJumpIfFalse
        | Not
        // Conditionally-faulting — slow path emits its own line store.
        | Add | Subtract | Multiply | Divide | Modulo
        | Less | LessEqual | Greater | GreaterEqual
        | Equal | NotEqual => false,
        // Always-faulting / always re-enter user code.
        Negate
        | Index | TypeWrap
        | BuildArray
        | GetGlobal | SetGlobal | DefineGlobal | DefineGlobalTyped
        | Closure
        | Call
        | StructLiteral
        | GetField | SetField
        | DefineMethod | MethodCall
        | Return => true,
        // Anything not in the JIT allow-list. Default safe.
        _ => true,
    }
}

// ── Inline int fast path ───────────────────────────────────────────────

#[derive(Clone, Copy)]
enum IntArithOp {
    Add,
    Sub,
    Mul,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

/// Emit IR for an arithmetic opcode that has an inline int+int fast
/// path. The emitted sequence checks the top-two values' tag bytes; if
/// both are `Value::Integer`, it performs `op` inline on their i64
/// payloads (wrapping semantics) and writes the result back in-place,
/// then calls the `stack_pop_one` helper to drop the now-duplicate top.
/// Otherwise it falls through to `slow_helper`.
/// Arithmetic where the lhs is on the value stack and the rhs is already in a
/// register: tag-check the memory lhs, do register arithmetic and overwrite the
/// payload in place on a hit, otherwise materialize `rhs` and fall back to the
/// generic helper.
///
/// The fast path never touches `stack_view.len` — it rewrites the payload of a
/// slot whose tag it has just proven is Integer, so nothing is abandoned and no
/// Rc is dropped. Both paths leave the result at the same memory top, so the
/// continuation sees one consistent stack shape.
///
/// `rhs` is passed in rather than read from the virt stack so a call site that
/// has the value in a register but not staged (a call result) can reuse this.
/// The caller owns stamping the line and popping the staged rhs.
#[inline]
fn emit_mixed_mode_int_arith(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    rhs: cranelift_codegen::ir::Value,
    op: OpCode,
) {
    use cranelift_codegen::ir::MemFlags;
    use cranelift_codegen::ir::condcodes::IntCC;
    let flags = MemFlags::trusted();

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let stack_len = emit_load_stack_len(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let one = builder.ins().iconst(types::I64, 1);
    let mem_top_idx = builder.ins().isub(stack_len, one);
    let mem_top_off = builder.ins().imul(mem_top_idx, value_size);
    let mem_top_addr = builder.ins().iadd(stack_ptr, mem_top_off);

    let lhs_tag = builder.ins().load(types::I8, flags, mem_top_addr, 0);
    let int_tag_const = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
    let is_int = builder.ins().icmp(IntCC::Equal, lhs_tag, int_tag_const);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let cont_block = builder.create_block();
    builder
        .ins()
        .brif(is_int, fast_block, &[], slow_block, &[]);

    // Tag byte is already Integer, so only the payload needs overwriting.
    builder.switch_to_block(fast_block);
    let lhs_payload = builder.ins().load(
        types::I64,
        flags,
        mem_top_addr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    let result_fast = match op {
        OpCode::Add => builder.ins().iadd(lhs_payload, rhs),
        OpCode::Subtract => builder.ins().isub(lhs_payload, rhs),
        OpCode::Multiply => builder.ins().imul(lhs_payload, rhs),
        _ => unreachable!(),
    };
    builder.ins().store(
        flags,
        result_fast,
        mem_top_addr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    builder.ins().jump(cont_block, &[]);

    // Materialize the register rhs and let int_fast_arith handle type dispatch.
    builder.switch_to_block(slow_block);
    {
        let stack_ptr2 = emit_load_stack_ptr(builder, vm_val);
        let top2 = emit_load_stack_len(builder, vm_val);
        let value_size2 = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
        let byte_off2 = builder.ins().imul(top2, value_size2);
        let slot_ptr2 = builder.ins().iadd(stack_ptr2, byte_off2);
        let int_tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
        builder.ins().store(flags, int_tag, slot_ptr2, 0);
        builder
            .ins()
            .store(flags, rhs, slot_ptr2, VALUE_INT_PAYLOAD_OFFSET as i32);
        let one2 = builder.ins().iconst(types::I64, 1);
        let new_top = builder.ins().iadd(top2, one2);
        builder
            .ins()
            .store(flags, new_top, vm_val, vm_stack_view_len_offset());
    }
    let (arith_op, slow_helper) = match op {
        OpCode::Add => (IntArithOp::Add, refs.add),
        OpCode::Subtract => (IntArithOp::Sub, refs.sub),
        OpCode::Multiply => (IntArithOp::Mul, refs.mul),
        _ => unreachable!(),
    };
    emit_int_fast_arith(builder, exit_block, refs, vm_val, arith_op, slow_helper);
    builder.ins().jump(cont_block, &[]);

    // Both branches left the result on memory top.
    builder.switch_to_block(cont_block);
    builder.seal_block(fast_block);
    builder.seal_block(slow_block);
    builder.seal_block(cont_block);
}

fn emit_int_fast_arith(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    _refs: &HelperRefs,
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
        IntArithOp::BitAnd => builder.ins().band(payload_a, payload_b),
        IntArithOp::BitOr => builder.ins().bor(payload_a, payload_b),
        IntArithOp::BitXor => builder.ins().bxor(payload_a, payload_b),
        // Shift count masked to low 6 bits by x86 and Cranelift alike, matching vm::binary_shl/shr.
        IntArithOp::Shl => builder.ins().ishl(payload_a, payload_b),
        IntArithOp::Shr => builder.ins().sshr(payload_a, payload_b),
    };
    // Tag is already VALUE_TAG_INTEGER, so only the payload needs writing.
    builder
        .ins()
        .store(flags, result, val_a_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
    // Safe to inline: the fast block gates on VALUE_TAG_INTEGER, which has no Drop.
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
    let zero64 = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[status.into(), zero64.into()]);

    builder.switch_to_block(continue_block);

    // Suppress the unused warning; ptr_ty is what stack_as_mut_ptr and iadd would verify.
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
#[allow(clippy::too_many_arguments)]
fn emit_inline_struct_field_add(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
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

    // An unpopulated cache has struct_def_raw == null, so the first call misses naturally.
    builder.switch_to_block(check_def_block);
    let rcbox = builder
        .ins()
        .load(ptr_ty, flags, self_val_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
    let inst_ptr = builder.ins().iadd_imm(rcbox, RC_VALUE_OFFSET as i64);
    let inst_def_raw =
        builder
            .ins()
            .load(ptr_ty, flags, inst_ptr, STRUCT_INSTANCE_DEF_OFFSET as i32);
    let cached_def_raw = builder.ins().load(
        ptr_ty,
        flags,
        cache_val,
        FieldCacheEntry::OFFSET_STRUCT_DEF_RAW,
    );
    let def_matches = builder
        .ins()
        .icmp(IntCC::Equal, inst_def_raw, cached_def_raw);
    builder
        .ins()
        .brif(def_matches, check_rhs_block, &[], miss_block, &[]);

    // The Const variant jumps straight through this rhs tag guard.
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
            builder.ins().load(
                types::I64,
                flags,
                rhs_val_ptr,
                VALUE_INT_PAYLOAD_OFFSET as i32,
            )
        }
    };

    // 5. Load fields_ptr + field_index, compute slot_ptr, guard Integer tag.
    let fields_ptr = builder.ins().load(
        ptr_ty,
        flags,
        inst_ptr,
        STRUCT_INSTANCE_FIELDS_PTR_OFFSET as i32,
    );
    let field_index = builder.ins().load(
        types::I32,
        flags,
        cache_val,
        FieldCacheEntry::OFFSET_FIELD_INDEX,
    );
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
    builder.ins().store(
        flags,
        new_payload,
        slot_ptr,
        VALUE_INT_PAYLOAD_OFFSET as i32,
    );
    builder.ins().jump(ok_block, &[]);

    // Miss: the helper falls back to handle_get_field/handle_set_field and repopulates the IC.
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
        .brif(status, err_block, &[status.into()], ok_block, &[]);

    builder.switch_to_block(err_block);
    let err_status = builder.block_params(err_block)[0];
    let zero64 = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[err_status.into(), zero64.into()]);

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

// B2.1 invariant: a virtualized local's backing slot stays a valid (possibly stale) copy.

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
    // Rewrite the tag defensively; one byte store is cheap insurance.
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

// Legacy expr_stack flush helpers, superseded by VirtStack::flush_to_memory.

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

    // Cranelift keeps these as SSA values and spills if needed.
    let mut words = Vec::with_capacity(VALUE_SIZE / 8);
    for off in (0..VALUE_SIZE).step_by(8) {
        words.push(builder.ins().load(types::I64, flags, src_ptr, off as i32));
    }

    // Pop the JitFrame.
    let jf_len = builder
        .ins()
        .load(types::I64, flags, vm_val, vm_jit_frame_view_len_offset());
    let new_jf_len = builder.ins().isub(jf_len, one);
    builder
        .ins()
        .store(flags, new_jf_len, vm_val, vm_jit_frame_view_len_offset());

    // The one FFI kept inline: truncation must run Drop for Rc-bearing locals.
    builder.ins().call(refs.stack_truncate, &[vm_val, slot_off]);

    // After truncate both lengths equal slot_off; write the result there, then bump by 1.
    let dst_byte = builder.ins().imul(slot_off, value_size);
    let dst_ptr = builder.ins().iadd(stack_ptr, dst_byte);
    for (i, w) in words.iter().enumerate() {
        builder.ins().store(flags, *w, dst_ptr, (i * 8) as i32);
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
    exit_block: Block,
    _refs: &HelperRefs,
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
    let rhs_nonzero = builder.ins().icmp_imm(IntCC::NotEqual, rhs, 0);
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
    let zero64 = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[status.into(), zero64.into()]);

    builder.switch_to_block(continue_block);
}

/// True iff `val` is the result of a Cranelift `iconst` with the
/// given immediate. Used by the parity-branch peephole to recognise
/// the constants `2` (Modulo RHS) and `0` (Equal/NotEqual operand)
/// without relying on bytecode position. Returns false for any value
/// that wasn't materialised by a recent iconst — which is also the
/// safe-fallback answer for the peephole (we just don't fire it).
#[inline]
fn is_iconst_imm(
    builder: &FunctionBuilder<'_>,
    val: cranelift_codegen::ir::Value,
    imm: i64,
) -> bool {
    use cranelift_codegen::ir::{InstructionData, Opcode, ValueDef};
    let dfg = &builder.func.dfg;
    match dfg.value_def(val) {
        ValueDef::Result(inst, _) => match dfg.insts[inst] {
            InstructionData::UnaryImm {
                opcode: Opcode::Iconst,
                imm: i,
            } => i64::from(i) == imm,
            _ => false,
        },
        _ => false,
    }
}

/// Extract the i64 immediate of a Cranelift Value if it's the result
/// of an `iconst`. Returns None if `val` was produced by anything else.
#[inline]
fn iconst_imm(builder: &FunctionBuilder<'_>, val: cranelift_codegen::ir::Value) -> Option<i64> {
    use cranelift_codegen::ir::{InstructionData, Opcode, ValueDef};
    let dfg = &builder.func.dfg;
    match dfg.value_def(val) {
        ValueDef::Result(inst, _) => match dfg.insts[inst] {
            InstructionData::UnaryImm {
                opcode: Opcode::Iconst,
                imm,
            } => Some(i64::from(imm)),
            _ => None,
        },
        _ => None,
    }
}

/// B2.1g parity-branch peephole. Detects the four shapes
///
/// ```text
/// (x % 2) == 0      —— pattern A, evaluating "is even"
/// (x % 2) != 0      —— pattern A, "is odd"
/// 0 == (x % 2)      —— pattern B, "is even" (commuted)
/// 0 != (x % 2)      —— pattern B, "is odd"  (commuted)
/// ```
///
/// when followed immediately by `JumpIfFalse` / `JumpIfTrue` /
/// `PopJumpIfFalse`. On match, lowers to `band_imm + icmp_imm + brif`
/// and returns `Some(next_ip)` for the dispatch loop to skip past all
/// consumed bytecode opcodes. On any mismatch returns None and the
/// caller proceeds with the existing virt-Modulo path (which still
/// emits `srem`).
///
/// Caller must invoke this BEFORE emitting any IR for the Modulo
/// itself — otherwise the rewrite would be too late and `srem` would
/// stay in the IR.
#[allow(clippy::too_many_arguments)]
fn try_emit_parity_branch_peephole(
    builder: &mut FunctionBuilder<'_>,
    chunk: &Chunk,
    code: &[u8],
    ip: usize,
    virt_stack: &mut VirtStack,
    virt_branch_elided_pops: &mut HashSet<usize>,
    blocks: &mut HashMap<usize, Block>,
    slot_types: &crate::compiler::slot_types::FunctionSlotTypes,
    counters_ptr_opt: Option<*const JitCounters>,
) -> Option<usize> {
    use cranelift_codegen::ir::condcodes::IntCC;

    // Need at least (x, 2) on the virt stack — both must be virt Ints.
    if virt_stack.pending_depth() < 2 {
        return None;
    }

    // Top of the virt stack is the Modulo RHS and must be iconst 2.
    let rhs = virt_stack.peek_int_ssa()?;
    if !is_iconst_imm(builder, rhs, 2) {
        return None;
    }

    // Pattern A: Constant(0); Equal|NotEqual; JumpIf*. Pattern B: the 0 is already under x.
    let after_mod = ip + 1;

    let try_constant_zero = |start: usize| -> Option<usize> {
        if OpCode::from_byte(*code.get(start)?)? != OpCode::Constant {
            return None;
        }
        let idx = read_u16(code, start + 1) as usize;
        match chunk.constants.get(idx) {
            Some(crate::vm::value::Value::Integer(0)) => Some(start + 3),
            _ => None,
        }
    };

    // Decide A vs B by trying A first (Constant(0) then Equal/NotEqual).
    let (cmp_ip, is_pattern_b) = match try_constant_zero(after_mod) {
        Some(post_const_ip) => (post_const_ip, false),
        None => (after_mod, true),
    };

    let cmp_op = OpCode::from_byte(*code.get(cmp_ip)?)?;
    let is_equal = match cmp_op {
        OpCode::Equal => true,
        OpCode::NotEqual => false,
        _ => return None,
    };

    let branch_ip = cmp_ip + 1;
    let branch_op = OpCode::from_byte(*code.get(branch_ip)?)?;
    if !matches!(
        branch_op,
        OpCode::JumpIfFalse | OpCode::JumpIfTrue | OpCode::PopJumpIfFalse
    ) {
        return None;
    }
    // Reject if the byte after Modulo is a branch target: another block expects to emit there.
    if blocks.contains_key(&after_mod) {
        return None;
    }

    let off = read_u16(code, branch_ip + 1) as usize;
    let target_ip = branch_ip + 3 + off;
    let next_ip = branch_ip + 3;

    // Cleanup-Pop classification (mirrors B2.1f).
    let cleanup_ok = match branch_op {
        OpCode::PopJumpIfFalse => true,
        OpCode::JumpIfFalse | OpCode::JumpIfTrue => {
            slot_types.condition_cleanup_pop_ips.contains(&next_ip)
                && slot_types.condition_cleanup_pop_ips.contains(&target_ip)
        }
        _ => return None,
    };
    if !cleanup_ok {
        return None;
    }

    // For pattern B, virt_stack[-3] must be `iconst 0`.
    if is_pattern_b {
        let zero_val = virt_stack.peek_int_ssa_at(2)?;
        if !is_iconst_imm(builder, zero_val, 0) {
            return None;
        }
    }

    // ── Match — emit. ──────────────────────────────────────────────
    if let Some(cp) = counters_ptr_opt {
        emit_counter_bump(builder, cp, counter_offsets::VIRT_BRANCH_PARITY_HIT);
    }

    // Pop rhs and lhs; pattern B also pops the leading 0.
    virt_stack.pop_int_ssa().unwrap(); // rhs (2) — discarded; we use band_imm
    let lhs = virt_stack.pop_int_ssa().unwrap();
    if is_pattern_b {
        virt_stack.pop_int_ssa().unwrap(); // 0 — already proven via is_iconst_imm
    }

    let parity = builder.ins().band_imm(lhs, 1);
    let cc = if is_equal {
        IntCC::Equal
    } else {
        IntCC::NotEqual
    };
    let pred = builder.ins().icmp_imm(cc, parity, 0);

    let target_block = blocks[&target_ip];
    let fall_block = *blocks
        .entry(next_ip)
        .or_insert_with(|| builder.create_block());

    let (true_block, false_block) = match branch_op {
        OpCode::JumpIfFalse | OpCode::PopJumpIfFalse => (fall_block, target_block),
        OpCode::JumpIfTrue => (target_block, fall_block),
        _ => unreachable!(),
    };
    builder.ins().brif(pred, true_block, &[], false_block, &[]);

    if matches!(branch_op, OpCode::JumpIfFalse | OpCode::JumpIfTrue) {
        virt_branch_elided_pops.insert(next_ip);
        virt_branch_elided_pops.insert(target_ip);
    }

    // branch_ip + 3 is the byte after the JumpIf*'s opcode + offset.
    Some(branch_ip + 3)
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
///
/// B2.1h signed-div-by-power-of-two peephole. Tries to lower
/// `OpCode::Divide` whose RHS is a constant power-of-two integer to a
/// bias-and-shift sequence, replacing Cranelift's ~20-cycle `idiv`
/// with ~5 cycles of `sshr_imm + band_imm + iadd + sshr_imm`.
///
/// Trunc-toward-zero semantics for signed integers require a sign-bias
/// before the right shift:
///
/// ```text
/// q = (x + ((x >> 63) & (|d| - 1))) >> log2(|d|)
/// if d < 0 { q = -q }
/// ```
///
/// For known non-negative `x`, the cheaper form `q = x >> log2(|d|)`
/// is correct. v1 doesn't have non-negative facts yet, so we always
/// emit the bias form. The bias is two extra ops (~2 cycles) — still
/// far cheaper than `idiv`.
///
/// Bailouts (return None, fall through to virt-divmod path's existing
/// `idiv`-with-guards behavior):
///
///   * RHS not a recognized iconst at compile time.
///   * d == 0   (must preserve div-by-zero error).
///   * d == -1  (must preserve i64::MIN / -1 overflow trap).
///   * |d| not a power of two.
///
/// Returns `Some(quotient)` on match; the caller pops both operands
/// from the virt stack and pushes the returned value.
fn try_emit_sdiv_pow2_peephole(
    builder: &mut FunctionBuilder<'_>,
    lhs: cranelift_codegen::ir::Value,
    rhs: cranelift_codegen::ir::Value,
) -> Option<cranelift_codegen::ir::Value> {
    let d = iconst_imm(builder, rhs)?;
    if d == 0 || d == -1 {
        return None;
    }
    let abs = d.checked_abs()?;
    if (abs as u64).count_ones() != 1 {
        return None;
    }
    let k = (abs as u64).trailing_zeros() as i64;

    let q = if k == 0 {
        // |d| == 1: x / 1 == x; x / -1 was rejected above.
        lhs
    } else {
        let sign = builder.ins().sshr_imm(lhs, 63);
        let bias_mask = abs - 1;
        let bias = builder.ins().band_imm(sign, bias_mask);
        let adjusted = builder.ins().iadd(lhs, bias);
        builder.ins().sshr_imm(adjusted, k)
    };

    let q = if d < 0 { builder.ins().ineg(q) } else { q };
    Some(q)
}

#[allow(clippy::too_many_arguments)]
fn emit_int_virt_divmod(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
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
    builder.ins().jump(cont_block, &[result.into()]);

    // Re-box both operands, call the helper (which handles zero and overflow), then read the payload back.
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
    let zero64 = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[status.into(), zero64.into()]);
    builder.switch_to_block(slow_ok_block);
    let payload = emit_load_stack_top_integer_payload(builder, vm_val);
    emit_inline_stack_pop_one(builder, vm_val);
    builder.ins().jump(cont_block, &[payload.into()]);

    builder.switch_to_block(cont_block);
    builder.block_params(cont_block)[0]
}

/// Peephole fast path for `GetLocal(slot); Constant(int); Arith;
/// SetLocal(slot); [Pop]`. On integer locals this updates the local
/// payload in place and skips the temporary stack traffic. Non-integer
/// locals replay the original helper sequence so mixed numeric/string
/// semantics stay unchanged.
#[allow(clippy::too_many_arguments)]
fn emit_inline_local_const_arith_update(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
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
        IntArithOp::BitAnd
        | IntArithOp::BitOr
        | IntArithOp::BitXor
        | IntArithOp::Shl
        | IntArithOp::Shr => unreachable!("local-const peephole only emitted for Add/Sub/Mul"),
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
        IntArithOp::BitAnd
        | IntArithOp::BitOr
        | IntArithOp::BitXor
        | IntArithOp::Shl
        | IntArithOp::Shr => unreachable!("local-const peephole only emitted for Add/Sub/Mul"),
    };
    let call = builder.ins().call(slow_helper, &[vm_val]);
    let status = builder.inst_results(call)[0];
    let err_block = builder.create_block();
    let set_block = builder.create_block();
    builder.ins().brif(status, err_block, &[], set_block, &[]);
    builder.switch_to_block(err_block);
    let zero64 = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[status.into(), zero64.into()]);

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
#[allow(clippy::too_many_arguments)]
fn emit_inline_local_scaled_arith_update(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
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
        IntArithOp::Mul
        | IntArithOp::BitAnd
        | IntArithOp::BitOr
        | IntArithOp::BitXor
        | IntArithOp::Shl
        | IntArithOp::Shr => unreachable!("scaled update only supports Add/Sub"),
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
    let zero64_a = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[mul_status.into(), zero64_a.into()]);

    builder.switch_to_block(arith_block);
    let slow_helper = match op {
        IntArithOp::Add => refs.add,
        IntArithOp::Sub => refs.sub,
        IntArithOp::Mul
        | IntArithOp::BitAnd
        | IntArithOp::BitOr
        | IntArithOp::BitXor
        | IntArithOp::Shl
        | IntArithOp::Shr => unreachable!("scaled update only supports Add/Sub"),
    };
    let arith_call = builder.ins().call(slow_helper, &[vm_val]);
    let arith_status = builder.inst_results(arith_call)[0];
    let arith_err_block = builder.create_block();
    let set_block = builder.create_block();
    builder
        .ins()
        .brif(arith_status, arith_err_block, &[], set_block, &[]);
    builder.switch_to_block(arith_err_block);
    let zero64_b = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[arith_status.into(), zero64_b.into()]);

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
    exit_block: Block,
    _refs: &HelperRefs,
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
    let zero64 = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[status.into(), zero64.into()]);

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
// True when slow_helper returns a status; eq/ne return nothing, so the status read is skipped.
#[allow(clippy::too_many_arguments)]
fn emit_fused_int_cmp_branch(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    cc: cranelift_codegen::ir::condcodes::IntCC,
    slow_helper: FuncRef,
    slow_helper_fallible: bool,
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

    // JumpIfFalse/JumpIfTrue do not pop the Boolean; the compiler emits explicit Pops on both sides.
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
    let cont_block = builder.create_block();
    if slow_helper_fallible {
        // lt/le/gt/ge return a status; forward non-zero to exit_block as a bailout.
        let status = builder.inst_results(call)[0];
        let err_block = builder.create_block();
        builder.ins().brif(status, err_block, &[], cont_block, &[]);
        builder.switch_to_block(err_block);
        let zero64 = builder.ins().iconst(types::I64, 0);
        builder
            .ins()
            .jump(exit_block, &[status.into(), zero64.into()]);
    } else {
        // eq/ne return nothing, so inst_results is empty and indexing it panics.
        builder.ins().jump(cont_block, &[]);
    }

    builder.switch_to_block(cont_block);
    // slow_helper pushed a Boolean; replicate the branch opcode's own behaviour on it.
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

// Primitive tags 0..=6 clone by 16-byte memcpy; no Rc traffic, no variant dispatch.

/// Predicate: is it safe to inline the Generic-entry Return as a
/// raw `stack_view.len = slot_offset + 1` truncation? Returns true
/// when every slot in `1..num_slots` is provably non-Rc at Return
/// time:
///
/// * **Int64** (per `slot_types`): primitive, no Drop work.
/// * **Bottom**: never written on any path; the slot's bytes are
///   uninitialized junk that the truncation drops without touching.
/// * **Value-typed param in `param_mirrors`**: the entry tag-guard
///   already proved the param's runtime tag is Integer when this
///   thunk was entered, so its actual content is also primitive.
///
/// Anything else (a `Value`-classified slot that wasn't gated as
/// an int-mirror) could be holding a Closure / Array / String /
/// etc., and a raw len-truncation would leak its `Rc` refcount.
/// Caller must fall back to `jit_op_return` in that case.
///
/// Slot 0 is special — it's always the closure marker
/// (`Value::Closure(Rc<ObjClosure>)`), and the inline-Return path
/// emits an explicit Rc dec for it (with a slow-helper fallback
/// when `strong == 1`).
fn return_slots_safe_to_truncate(
    slot_types: &crate::compiler::slot_types::FunctionSlotTypes,
    param_mirrors: &HashMap<u16, cranelift_frontend::Variable>,
    func: &Function,
) -> bool {
    use crate::compiler::slot_types::SlotType;
    let num_slots = func.locals.len().max(func.arity as usize + 1);
    for slot in 1..num_slots as u16 {
        match slot_types.get(slot as usize) {
            SlotType::Int64 | SlotType::Bottom => {}
            SlotType::Value => {
                // Only OK if it's a verified-Int param mirror.
                if !param_mirrors.contains_key(&slot) {
                    return false;
                }
            }
        }
    }
    true
}

/// Emit the inline Generic-entry Return teardown. Replaces the
/// `jit_op_return` helper FFI with:
///
/// 1. Source the result `Value` (16 bytes) — from a virt-int SSA
///    that the previous opcode pushed, or from `stack[len-1]` when
///    the result is already memory-resident.
/// 2. Decrement the Rc strong count on the closure marker at
///    `stack[slot_offset]`. Fast path is `strong > 1`; the
///    `strong == 1` slow path falls through to `jit_op_return` to
///    do the full Drop chain (stack truncate calls Drop on every
///    Value, which can recursively drop the inner T).
/// 3. Write the result Value at `stack[slot_offset]` (overwriting
///    the just-decremented closure marker bits).
/// 4. Set `stack_view.len = slot_offset + 1`.
/// 5. Decrement `jit_frame_view.len`.
/// 6. Exit through `exit_block` with `(0, 0)`.
///
/// Safety: the caller must have verified
/// `return_slots_safe_to_truncate` so the implicit drop of slots
/// `slot_offset+1..old_len` doesn't leak any `Rc` refcount.
fn emit_inline_generic_return(
    builder: &mut FunctionBuilder<'_>,
    exit_block: cranelift_codegen::ir::Block,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    virt_stack: &mut VirtStack,
) {
    use cranelift_codegen::ir::MemFlags;
    use cranelift_codegen::ir::condcodes::IntCC;

    let flags = MemFlags::trusted();

    // Int SSA on top builds {tag, payload} directly; otherwise flush and read 16 bytes from memory.
    let result_ssa: Option<cranelift_codegen::ir::Value> = virt_stack.pop_int_ssa();
    if !virt_stack.is_empty() {
        // Staged values sit below the extracted result and are dropped by the truncation.
        virt_stack.flush_to_memory(builder, vm_val);
    }

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);

    // result_lo / result_hi (16 bytes total) — sourced from virt or memory.
    let (result_lo, result_hi) = if let Some(payload) = result_ssa {
        // For an Integer the tag is 0 and the rest of the first word is don't-care.
        let tag_word = builder.ins().iconst(types::I64, VALUE_TAG_INTEGER as i64);
        (tag_word, payload)
    } else {
        // Memory-source.
        let stack_len = emit_load_stack_len(builder, vm_val);
        let one = builder.ins().iconst(types::I64, 1);
        let top_idx = builder.ins().isub(stack_len, one);
        let top_off = builder.ins().imul(top_idx, value_size);
        let top_addr = builder.ins().iadd(stack_ptr, top_off);
        let lo = builder.ins().load(types::I64, flags, top_addr, 0);
        let hi = builder
            .ins()
            .load(types::I64, flags, top_addr, VALUE_INT_PAYLOAD_OFFSET as i32);
        (lo, hi)
    };

    // slot_offset is a slot index, not a byte offset.
    let cm_byte_off = builder.ins().imul(slot_offset_val, value_size);
    let cm_addr = builder.ins().iadd(stack_ptr, cm_byte_off);

    // Read the RcBox pointer from the closure marker's payload.
    let cm_rc = builder
        .ins()
        .load(types::I64, flags, cm_addr, VALUE_INT_PAYLOAD_OFFSET as i32);
    let strong = builder.ins().load(types::I64, flags, cm_rc, 0);

    // ── Branch on strong > 1 ──
    let one_i64 = builder.ins().iconst(types::I64, 1);
    let strong_gt_1 = builder
        .ins()
        .icmp(IntCC::UnsignedGreaterThan, strong, one_i64);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    builder
        .ins()
        .brif(strong_gt_1, fast_block, &[], slow_block, &[]);

    // strong == 1: fall back to the helper for the proper Drop chain.
    builder.switch_to_block(slow_block);
    // A virt-int result leaves a stale value at the memory top, which the helper's pop would read.
    if result_ssa.is_some() {
        // Materialize the virt-int result at a new top slot so the helper's pop reads it.
        emit_inline_push_integer(builder, vm_val, result_hi);
    }
    builder.ins().call(refs.op_return, &[vm_val]);
    let zero32_s = builder.ins().iconst(types::I32, 0);
    let zero64_s = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[zero32_s.into(), zero64_s.into()]);

    // ── Fast path: strong > 1. Inline dec + truncate + push.
    builder.switch_to_block(fast_block);
    let strong_dec = builder.ins().iadd_imm(strong, -1);
    builder.ins().store(flags, strong_dec, cm_rc, 0);

    // The dec'd closure marker's Rc was already adjusted, so overwriting it is safe.
    builder.ins().store(flags, result_lo, cm_addr, 0);
    builder
        .ins()
        .store(flags, result_hi, cm_addr, VALUE_INT_PAYLOAD_OFFSET as i32);

    let new_stack_len = builder.ins().iadd(slot_offset_val, one_i64);
    builder
        .ins()
        .store(flags, new_stack_len, vm_val, vm_stack_view_len_offset());

    let jfv_len = builder
        .ins()
        .load(types::I64, flags, vm_val, vm_jit_frame_view_len_offset());
    let new_jfv_len = builder.ins().isub(jfv_len, one_i64);
    builder
        .ins()
        .store(flags, new_jfv_len, vm_val, vm_jit_frame_view_len_offset());

    let zero32_f = builder.ins().iconst(types::I32, 0);
    let zero64_f = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[zero32_f.into(), zero64_f.into()]);
}

fn emit_inline_get_local(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    slot: u16,
) {
    use cranelift_codegen::ir::MemFlags;
    use cranelift_codegen::ir::condcodes::IntCC;

    let _ = refs; // helper-table no longer consulted on the hot path

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);

    let slot_const = builder.ins().iconst(types::I64, slot as i64);
    let abs_slot = builder.ins().iadd(slot_offset_val, slot_const);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let byte_off = builder.ins().imul(abs_slot, value_size);
    let src_addr = builder.ins().iadd(stack_ptr, byte_off);

    let flags = MemFlags::trusted();
    let tag = builder.ins().load(types::I8, flags, src_addr, 0);

    // Compute destination address (stack[len] before increment).
    let stack_len = emit_load_stack_len(builder, vm_val);
    let dst_off = builder.ins().imul(stack_len, value_size);
    let dst_addr = builder.ins().iadd(stack_ptr, dst_off);

    emit_copy_value(builder, src_addr, dst_addr);

    // Bump only when tag > 6 && tag != 13; two icmps are shorter than the fused range check.
    let six = builder.ins().iconst(types::I8, 6);
    let is_heap = builder.ins().icmp(IntCC::UnsignedGreaterThan, tag, six);

    let check_builtin_block = builder.create_block();
    let post_bump_block = builder.create_block();
    builder
        .ins()
        .brif(is_heap, check_builtin_block, &[], post_bump_block, &[]);

    builder.switch_to_block(check_builtin_block);
    // Tag 13 = Builtin: the payload is a fn pointer, and treating it as an Rc dereferences code memory.
    let builtin_tag = builder.ins().iconst(types::I8, 13);
    let is_builtin = builder.ins().icmp(IntCC::Equal, tag, builtin_tag);
    let bump_block = builder.create_block();
    builder
        .ins()
        .brif(is_builtin, post_bump_block, &[], bump_block, &[]);

    builder.switch_to_block(bump_block);
    // Strong count is at RcBox offset 0, pinned by rc_strong_count_lives_at_rcbox_offset_zero.
    let rc_ptr = builder
        .ins()
        .load(types::I64, flags, src_addr, VALUE_INT_PAYLOAD_OFFSET as i32);
    let strong = builder.ins().load(types::I64, flags, rc_ptr, 0);
    let strong_new = builder.ins().iadd_imm(strong, 1);
    builder.ins().store(flags, strong_new, rc_ptr, 0);
    builder.ins().jump(post_bump_block, &[]);

    builder.switch_to_block(post_bump_block);
    let one = builder.ins().iconst(types::I64, 1);
    let new_len = builder.ins().iadd(stack_len, one);
    builder
        .ins()
        .store(flags, new_len, vm_val, vm_stack_view_len_offset());
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

    let top_idx = builder.ins().isub(stack_len, one);
    let top_off = builder.ins().imul(top_idx, value_size);
    let addr_top = builder.ins().iadd(stack_ptr, top_off);

    let slot_const = builder.ins().iconst(types::I64, slot as i64);
    let abs_slot = builder.ins().iadd(slot_offset_val, slot_const);
    let slot_off = builder.ins().imul(abs_slot, value_size);
    let addr_slot = builder.ins().iadd(stack_ptr, slot_off);

    let flags = MemFlags::trusted();
    let top_tag = builder.ins().load(types::I8, flags, addr_top, 0);
    let old_tag = builder.ins().load(types::I8, flags, addr_slot, 0);
    // Primitive = tags 0..=6; anything holding an Rc or RefCell is >= 7.
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

    // Both sides are primitives, so no Rc or Drop interaction is possible.
    builder.switch_to_block(fast_block);
    let words = VALUE_SIZE.div_ceil(8);
    for i in 0..words {
        let off = (i * 8) as i32;
        let v = builder.ins().load(types::I64, flags, addr_top, off);
        builder.ins().store(flags, v, addr_slot, off);
    }
    builder.ins().jump(cont_block, &[]);

    // The helper clones the top and drops whatever was in the slot.
    builder.switch_to_block(slow_block);
    let slot_val = builder.ins().iconst(types::I32, slot as i64);
    builder.ins().call(refs.set_local, &[vm_val, slot_val]);
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(cont_block);
}
