//! Cranelift-backed JIT engine internals. Only compiled when the `jit`
//! feature is enabled.
//!
//! Translates bytecode for control flow, locals and arithmetic. Every opcode
//! becomes one `call` to an `extern "C"` runtime helper plus any branching.

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
use crate::vm::JitFrame;
use crate::vm::VM;
use crate::vm::VMError;
use crate::vm::value::{
    Function, RC_VALUE_OFFSET, STRUCT_INSTANCE_DEF_OFFSET, STRUCT_INSTANCE_FIELDS_PTR_OFFSET,
    STRUCT_INSTANCE_LOCKED_OFFSET, VALUE_INT_PAYLOAD_OFFSET, VALUE_SIZE, VALUE_TAG_CLOSURE,
    VALUE_TAG_FLOAT, VALUE_TAG_INTEGER, VALUE_TAG_NONE, VALUE_TAG_STRUCT_INSTANCE,
};

use super::scan;
use super::virt_stack::{VirtConst, VirtSlot, VirtStack};
use super::{CompiledEntries, CompiledThunk, SpecializedThunkRaw};

mod cache;
mod counters;
mod defs;
mod helpers;
mod lean;
pub(crate) use cache::ClosureInlineKind;
pub(crate) use cache::{
    CallCacheEntry, FieldCacheEntry, FieldCacheKind, GlobalCacheEntry, MethodCacheEntry,
    MethodInlineKind,
};
pub(crate) use counters::{HelperCounter, JitCounters};
use counters::{counter_offsets, emit_counter_bump};
use defs::{
    Entry, obj_closure_module_globals_offset, vm_jit_frame_view_len_offset,
    vm_jit_frame_view_ptr_offset, vm_limits_armed_offset, vm_loop_countdown_offset,
    vm_stack_view_len_offset, vm_stack_view_ptr_offset,
};
pub(crate) use defs::{EntryKind, InvokeOutcome, SpecializedEntryKind};
use helpers::{HelperIds, HelperRefs, declare_helper_refs, declare_helpers, register_helpers};

#[allow(clippy::vec_box)]
pub(super) struct JitInner {
    module: JITModule,
    ctx: Context,
    fbc: FunctionBuilderContext,
    compiled: HashMap<*const Function, Entry>,
    /// Keeps `Rc<Function>`s alive so the pointer keys remain valid.
    retained: HashMap<*const Function, Rc<Function>>,

    /// Cached helper `FuncId`s, registered once at module init.
    helpers: HelperIds,
    lean_cold_id: cranelift_module::FuncId,

    /// Pre-call frame depth, stashed by `invoke` before jumping to compiled
    /// code and read by runtime helpers through the VM pointer.
    current_stop_depth: usize,
    pending_error: Option<VMError>,

    /// Counter for unique thunk names.
    next_id: u32,

    /// Instrumentation counters, held in a stable heap allocation so emitted IR
    /// can bake the raw pointer.
    pub(crate) counters: Box<JitCounters>,

    /// Per-call-site inline caches for `GetGlobal`. Each compiled GetGlobal has its
    /// own `Box<GlobalCacheEntry>` whose raw pointer is baked into emitted IR.
    global_caches: Vec<Box<GlobalCacheEntry>>,

    /// Per-call-site inline caches for `Call`, one `Box<CallCacheEntry>` per
    /// compiled Call op.
    call_caches: Vec<Box<CallCacheEntry>>,

    /// Per-call-site inline caches for `MethodCall`, one `Box<MethodCacheEntry>`
    /// per compiled MethodCall op.
    method_caches: Vec<Box<MethodCacheEntry>>,

    /// Per-field-op inline caches for `GetField`/`SetField`.
    field_caches: Vec<Box<FieldCacheEntry>>,

    /// Per-global-name version cells for the GetGlobal ICs, stored in
    /// `GlobalCacheEntry.version_cell` and read from emitted IR.
    /// `SetGlobal`/`DefineGlobal` bump only the written name's cell.
    global_version_cells: HashMap<String, Box<u64>>,
    /// Address of the VM's armed byte, or 0 to load it through the VM.
    limits_armed: usize,
}

impl JitInner {
    pub fn new(limits_armed: usize) -> Self {
        use cranelift_codegen::settings::Configurable;
        let mut flag_builder = settings::builder();
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

        let lean_cold_id = {
            let mut sig = module.make_signature();
            sig.returns.push(AbiParam::new(types::I64));
            module
                .declare_function("oxigen_lean_overflow", Linkage::Import, &sig)
                .expect("declare lean overflow helper")
        };

        Self {
            limits_armed,
            module,
            ctx: Context::new(),
            fbc: FunctionBuilderContext::new(),
            compiled: HashMap::new(),
            retained: HashMap::new(),
            helpers,
            lean_cold_id,
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

    /// Stable pointer to `name`'s version cell, creating it (at version 0) on
    /// first use.
    pub(crate) fn global_version_cell(&mut self, name: &str) -> *mut u64 {
        let b = self
            .global_version_cells
            .entry(name.to_string())
            .or_insert_with(|| Box::new(0));
        b.as_mut() as *mut u64
    }

    /// Invalidates every IC caching `name`.
    pub(crate) fn bump_global_version(&mut self, name: &str) {
        if let Some(cell) = self.global_version_cells.get_mut(name) {
            **cell = cell.wrapping_add(1);
        }
    }

    /// Makes every method IC miss once by clearing the identity the emitted guards
    /// compare. The keepers stay until the miss path refills the entry.
    pub(crate) fn invalidate_method_caches(&mut self) {
        for m in &mut self.method_caches {
            m.struct_def_raw = std::ptr::null();
            m.inline_kind = MethodInlineKind::None;
        }
        for f in &mut self.field_caches {
            if matches!(f.kind, FieldCacheKind::DefMethod) {
                f.struct_def_raw = std::ptr::null();
                f.kind = FieldCacheKind::Invalid;
            }
        }
    }

    /// Stable raw pointer to the counters block, safe to bake into emitted IR.
    pub(crate) fn counters_ptr(&self) -> *const JitCounters {
        self.counters.as_ref() as *const _
    }

    /// Allocates an inline-cache slot for a GetGlobal call site and returns a raw
    /// pointer to it, stable for the JitInner's lifetime.
    pub(crate) fn alloc_global_cache(&mut self) -> *mut GlobalCacheEntry {
        self.global_caches.push(Box::new(GlobalCacheEntry {
            version: u64::MAX,
            value: crate::vm::value::Value::None,
            version_cell: &cache::GLOBAL_VERSION_SENTINEL,
        }));
        let b = self.global_caches.last_mut().unwrap();
        b.as_mut() as *mut _
    }

    /// Allocates an inline-cache slot for a Call site and returns a raw pointer to
    /// it, stable for the JitInner's lifetime.
    pub(crate) fn alloc_call_cache(&mut self) -> *mut CallCacheEntry {
        self.call_caches.push(Box::new(CallCacheEntry {
            closure_raw: std::ptr::null(),
            thunk_raw: std::ptr::null(),
            arity: 0,
            specialized_kind: 0,
            inline_kind: cache::ClosureInlineKind::None,
            _pad: [0; 1],
            inline_upvalue_index: 0,
            specialized_thunk: std::ptr::null(),
            _keeper: None,
        }));
        let b = self.call_caches.last_mut().unwrap();
        b.as_mut() as *mut _
    }

    /// Allocates an inline-cache slot for a MethodCall site.
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

    /// Primary JIT-compile hook. Returns the generic thunk (`fn(*mut VM) -> u32`)
    /// on success. The specialized entry, when generated, is available via
    /// `maybe_compile_entries`.
    pub fn maybe_compile_thunk(&mut self, func: &Rc<Function>) -> Option<CompiledThunk> {
        self.maybe_compile_entries(func).map(|e| e.generic)
    }

    /// Like `maybe_compile_thunk` but returns the full `CompiledEntries`.
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
        let self_ptr: *mut JitInner = self as *mut JitInner;
        let pending_ptr = unsafe { &raw mut (*self_ptr).pending_error };
        let stop_depth_ptr = unsafe { &raw mut (*self_ptr).current_stop_depth };

        unsafe {
            stop_depth_ptr.write_volatile(stop_depth);
            pending_ptr.write_volatile(None);
        }

        let vm_ref = unsafe { &*vm };
        let prev_jit_exec = vm_ref.jit_executing.replace(true);

        let status = unsafe { thunk(vm) };

        vm_ref.jit_executing.set(prev_jit_exec);
        unsafe {
            stop_depth_ptr.write_volatile(0);
        }

        unsafe {
            (*vm).sync_stack_from_view();
        }
        match status {
            0 => InvokeOutcome::Returned,
            1 => {
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
        let limits_armed = self.limits_armed;
        if func.chunk.stack_bound() > crate::vm::STACK_GUARD_MARGIN {
            return Err(());
        }
        let chunk = &func.chunk;
        let code = &chunk.code;

        let info = scan::scan(chunk).map_err(|_| ())?;

        let counters_ptr_opt: Option<*const JitCounters> =
            if std::env::var("OXIGEN_JIT_STATS").is_ok() {
                Some(self.counters.as_ref() as *const _)
            } else {
                None
            };

        let slot_types = crate::compiler::slot_types::analyze(func);
        #[cfg(debug_assertions)]
        {
            if let Err(msg) = crate::compiler::slot_types::certify(func, &slot_types) {
                eprintln!(
                    "[jit] slot_types certify failed for {:?}: {}",
                    func.name.as_deref().unwrap_or("<anonymous>"),
                    msg,
                );
            }
        }

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

                let own_frame_ptr = emit_load_top_jit_frame_ptr(&mut builder, vm_val);

                let slot_offset_val = builder.ins().load(
                    types::I64,
                    cranelift_codegen::ir::MemFlagsData::trusted(),
                    own_frame_ptr,
                    JitFrame::OFFSET_SLOT_OFFSET,
                );

                let mut int_locals: HashMap<u16, Variable> = HashMap::new();
                let mut live_int_slots: HashSet<u16> = HashSet::new();
                let closure_arg_var: Option<Variable> =
                    if matches!(kind, EntryKind::IntSpecialized) && spec_wants_closure_arg {
                        Some(builder.declare_var(ptr_ty))
                    } else {
                        None
                    };
                let mut virt_stack = VirtStack::new();
                let mut virt_branch_elided_pops: HashSet<usize> = HashSet::new();

                {
                    let mut needs_var: Vec<u16> = Vec::new();
                    for &slot in slot_types.local_init_result_ip.values() {
                        if slot_types.is_virtualizable(slot) && !int_locals.contains_key(&slot) {
                            needs_var.push(slot);
                        }
                    }
                    needs_var.sort_unstable();
                    needs_var.dedup();
                    for slot in needs_var {
                        let var = builder.declare_var(types::I64);
                        int_locals.insert(slot, var);
                    }
                }

                let mut effective_entry_block = entry_block;

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

                let mut int_typed_param_slots: Vec<u16> = Vec::new();
                for slot in 1..=(func.arity as u16) {
                    if slot_types.is_virtualizable(slot) && !int_locals.contains_key(&slot) {
                        let var = builder.declare_var(types::I64);
                        int_locals.insert(slot, var);
                        int_typed_param_slots.push(slot);
                    }
                }
                int_typed_param_slots.sort_unstable();

                let mut prologue_slots: Vec<u16> = param_mirrors
                    .keys()
                    .copied()
                    .chain(int_typed_param_slots.iter().copied())
                    .collect();
                prologue_slots.sort_unstable();

                match kind {
                    EntryKind::IntSpecialized => {
                        use cranelift_codegen::ir::MemFlagsData;
                        let flags = MemFlagsData::trusted();
                        let arity = func.arity as usize;

                        let arg_block_param_offset = if spec_wants_closure_arg { 2 } else { 1 };
                        if let Some(var) = closure_arg_var {
                            let closure_ptr_val = builder.block_params(entry_block)[1];
                            builder.def_var(var, closure_ptr_val);
                        }

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
                        let arity_val = builder.ins().iconst(types::I64, arity as i64);
                        let new_len = builder.ins().iadd(stack_len, arity_val);
                        let _ = arity_plus_closure;
                        builder
                            .ins()
                            .store(flags, new_len, vm_val, vm_stack_view_len_offset());

                        if let Some(cp) = counters_ptr_opt {
                            emit_counter_bump(
                                &mut builder,
                                cp,
                                counter_offsets::SPECIALIZED_ENTRY_CALLED,
                            );
                        }
                    }
                    EntryKind::Generic => {
                        if func.params.iter().any(|p| p.type_ann.is_some()) {
                            let guarded = emit_param_type_guard(
                                &mut builder,
                                exit_block,
                                &refs,
                                vm_val,
                                slot_offset_val,
                                &func.params,
                            );
                            effective_entry_block = guarded;
                            blocks.insert(0, guarded);
                        }
                        if !prologue_slots.is_empty() {
                            use cranelift_codegen::ir::MemFlagsData;
                            let flags = MemFlagsData::trusted();

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
                let mut last_emitted_loc: Option<(u32, u32)> = None;
                let mut ic_ix: usize = 0;
                let mut call_ic_ix: usize = 0;
                let mut method_ic_ix: usize = 0;
                let mut field_ic_ix: usize = 0;

                while ip < code.len() {
                    if let Some(&block) = blocks.get(&ip)
                        && block != current_block
                    {
                        if !terminated {
                            virt_stack.flush_to_memory(&mut builder, vm_val);
                            builder.ins().jump(block, &[]);
                        }
                        builder.switch_to_block(block);
                        current_block = block;
                        terminated = false;
                        virt_stack = VirtStack::new();
                        last_emitted_loc = None;
                    }

                    if terminated {
                        ip += 1;
                        continue;
                    }

                    let op = OpCode::from_byte(code[ip]).ok_or(())?;
                    let line = chunk.lines.get(ip).copied().unwrap_or(0);
                    let col = chunk.columns.get(ip).copied().unwrap_or(0);
                    if opcode_always_needs_line(op) && last_emitted_loc != Some((line, col)) {
                        emit_store_current_loc(&mut builder, own_frame_ptr, line, col);
                        last_emitted_loc = Some((line, col));
                    }

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
                            | OpCode::RangeLimit
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
                                        if !virt_stack.is_empty() {
                                            virt_stack.flush_to_memory(&mut builder, vm_val);
                                        }
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
                                        if !virt_stack.is_empty() {
                                            virt_stack.flush_to_memory(&mut builder, vm_val);
                                        }
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
                            if virt_branch_elided_pops.contains(&ip) {
                            } else if let Some(slot) = slot_types.scope_pop_for(ip) {
                                live_int_slots.remove(&slot);
                                if virt_stack.is_empty() {
                                    emit_inline_pop_tag_gated(&mut builder, &refs, vm_val);
                                } else {
                                    virt_stack.pop();
                                }
                            } else if !virt_stack.is_empty() {
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
                        OpCode::Lock => {
                            if !virt_stack.is_empty() {
                                virt_stack.flush_to_memory(&mut builder, vm_val);
                            }
                            builder.ins().call(refs.lock, &[vm_val]);
                            ip += 1;
                        }
                        OpCode::IndexAssign | OpCode::IndexExisting => {
                            if !virt_stack.is_empty() {
                                virt_stack.flush_to_memory(&mut builder, vm_val);
                            }
                            let func = if op == OpCode::IndexAssign {
                                refs.index_assign
                            } else {
                                refs.index_existing
                            };
                            let call = builder.ins().call(func, &[vm_val]);
                            let status = builder.inst_results(call)[0];
                            emit_early_exit_on_err(&mut builder, exit_block, status);
                            ip += 1;
                        }
                        OpCode::IterLen | OpCode::IterGet | OpCode::IterEntry => {
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
                            if slot_types.noop_type_wrap_ips.contains(&ip) {
                                ip += 3;
                                continue;
                            }
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
                        OpCode::CheckReturn => {
                            if slot_types.noop_check_return_ips.contains(&ip) {
                                ip += 3;
                                continue;
                            }
                            maybe_emit_current_line(
                                &mut builder,
                                own_frame_ptr,
                                line,
                                col,
                                &mut last_emitted_loc,
                            );
                            let idx = read_u16(code, ip + 1);
                            let idx_val = builder.ins().iconst(types::I32, idx as i64);
                            let call = builder.ins().call(refs.check_return, &[vm_val, idx_val]);
                            let status = builder.inst_results(call)[0];
                            emit_early_exit_on_err(&mut builder, exit_block, status);
                            ip += 3;
                        }
                        OpCode::RangeLimit => {
                            let idx = read_u16(code, ip + 1);
                            let step = match chunk.constants.get(idx as usize) {
                                Some(crate::vm::value::Value::Integer(step)) if *step != 0 => *step,
                                _ => return Err(()),
                            };
                            let init_var = slot_types
                                .local_init_result_ip
                                .get(&ip)
                                .and_then(|slot| int_locals.get(slot).map(|&var| (*slot, var)));
                            if virt_stack.top_n_are_int_ssa(2) {
                                let end = virt_stack.pop_int_ssa().unwrap();
                                let _start = virt_stack.pop_int_ssa().unwrap();
                                use cranelift_codegen::ir::condcodes::IntCC;
                                let step_val = builder.ins().iconst(types::I64, step);
                                let diff = builder.ins().isub(end, step_val);
                                let (cc, edge, sat) = if step > 0 {
                                    (IntCC::SignedLessThan, i64::MIN + step, i64::MIN)
                                } else {
                                    (IntCC::SignedGreaterThan, i64::MAX + step, i64::MAX)
                                };
                                let edge_val = builder.ins().iconst(types::I64, edge);
                                let overflows = builder.ins().icmp(cc, end, edge_val);
                                let sat_val = builder.ins().iconst(types::I64, sat);
                                let limit = builder.ins().select(overflows, sat_val, diff);
                                if let Some((slot, var)) = init_var {
                                    if !virt_stack.is_empty() {
                                        virt_stack.flush_to_memory(&mut builder, vm_val);
                                    }
                                    emit_inline_push_integer(&mut builder, vm_val, limit);
                                    builder.def_var(var, limit);
                                    live_int_slots.insert(slot);
                                } else {
                                    virt_stack.push_int_ssa(limit);
                                }
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
                                let idx_val = builder.ins().iconst(types::I32, idx as i64);
                                let call = builder.ins().call(refs.range_limit, &[vm_val, idx_val]);
                                let status = builder.inst_results(call)[0];
                                emit_early_exit_on_err(&mut builder, exit_block, status);
                                if let Some((slot, var)) = init_var {
                                    let limit =
                                        emit_load_stack_top_integer_payload(&mut builder, vm_val);
                                    builder.def_var(var, limit);
                                    live_int_slots.insert(slot);
                                }
                            }
                            ip += 3;
                        }
                        OpCode::GetLocal => {
                            let sf_in_loop =
                                info.loop_ranges.iter().any(|&(t, p)| ip >= t && ip < p);
                            if let Some(m) = if sf_in_loop {
                                None
                            } else {
                                match_struct_field_add_update(code, chunk, ip, &blocks)
                            }
                            .filter(|m| match m.shape {
                                StructFieldAddShape::Local { rhs_slot } => {
                                    !int_locals.contains_key(&rhs_slot)
                                }
                                StructFieldAddShape::Const { .. } => true,
                            }) {
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
                                    own_frame_ptr,
                                    chunk_loc(chunk, ip + 6),
                                );
                                ip += m.len;
                            } else if let Some(m) =
                                match_local_array_mod_index_add_update(code, chunk, ip, &blocks)
                                    .filter(|m| {
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
                                let arith_ip = ip + 6;
                                emit_inline_local_scaled_arith_update(
                                    &mut builder,
                                    exit_block,
                                    &refs,
                                    vm_val,
                                    slot_offset_val,
                                    m.dst_slot,
                                    m.rhs_slot,
                                    None,
                                    m.op,
                                    m.pop_after,
                                    PeepholeLocs {
                                        frame_ptr: own_frame_ptr,
                                        mul: (0, 0),
                                        arith: chunk_loc(chunk, arith_ip),
                                    },
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
                                    Some(m.constant),
                                    m.op,
                                    m.pop_after,
                                    PeepholeLocs {
                                        frame_ptr: own_frame_ptr,
                                        mul: chunk_loc(chunk, ip + 9),
                                        arith: chunk_loc(chunk, ip + 10),
                                    },
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
                                    PeepholeLocs {
                                        frame_ptr: own_frame_ptr,
                                        mul: (0, 0),
                                        arith: chunk_loc(chunk, ip + 6),
                                    },
                                );
                                ip += m.len;
                            } else {
                                let slot = read_u16(code, ip + 1);
                                if let Some(&var) = int_locals.get(&slot)
                                    && live_int_slots.contains(&slot)
                                {
                                    let val = builder.use_var(var);
                                    virt_stack.push_int_ssa(val);
                                    ip += 3;
                                    continue;
                                }
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
                                let top_payload = int_locals.get(&slot).map(|_| {
                                    use cranelift_codegen::ir::MemFlagsData;
                                    let flags = MemFlagsData::trusted();
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
                                    use cranelift_codegen::ir::MemFlagsData;
                                    use cranelift_codegen::ir::condcodes::IntCC;
                                    let flags = MemFlagsData::trusted();
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

                        OpCode::Add | OpCode::Subtract | OpCode::Multiply => {
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
                                )
                            {
                                terminated = true;
                                ip = next_ip;
                                continue;
                            }

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
                                        && let Some(cp) = counters_ptr_opt
                                    {
                                        emit_counter_bump(
                                            &mut builder,
                                            cp,
                                            counter_offsets::VIRT_BRANCH_EQ_HIT,
                                        );
                                    }
                                    let rhs = virt_stack.pop_int_ssa().unwrap();
                                    let lhs = virt_stack.pop_int_ssa().unwrap();
                                    let pred = builder.ins().icmp(cc, lhs, rhs);
                                    if !virt_stack.is_empty() {
                                        virt_stack.flush_to_memory(&mut builder, vm_val);
                                    }
                                    let (true_block, false_block) = match branch_op {
                                        OpCode::JumpIfFalse | OpCode::PopJumpIfFalse => {
                                            (fall_block, target_block)
                                        }
                                        OpCode::JumpIfTrue => (target_block, fall_block),
                                        _ => unreachable!(),
                                    };
                                    builder.ins().brif(pred, true_block, &[], false_block, &[]);
                                    if matches!(branch_op, OpCode::JumpIfFalse | OpCode::JumpIfTrue)
                                    {
                                        virt_branch_elided_pops.insert(next_ip);
                                        virt_branch_elided_pops.insert(target_ip);
                                    }
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

                        OpCode::Not => {
                            builder.ins().call(refs.not, &[vm_val]);
                            ip += 1;
                        }

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
                            emit_loop_poll(&mut builder, exit_block, &refs, vm_val, limits_armed);
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

                            virt_stack.flush_to_memory(&mut builder, vm_val);
                            let call = builder.ins().call(refs.peek_truthy, &[vm_val]);
                            let truthy = builder.inst_results(call)[0];
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

                        OpCode::GetGlobal => {
                            let idx = read_u16(code, ip + 1);
                            let cache_ptr = global_cache_ptrs[ic_ix];
                            ic_ix += 1;
                            let idx_val = builder.ins().iconst(types::I32, idx as i64);
                            let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);
                            use cranelift_codegen::ir::MemFlagsData;
                            use cranelift_codegen::ir::condcodes::IntCC;
                            let flags = MemFlagsData::trusted();
                            let cache_ver = builder.ins().load(types::I64, flags, cache_val, 0);
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

                            builder.switch_to_block(hit_block);
                            let cache_value_ptr = builder.ins().iadd_imm_s(cache_val, 8);
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
                            let strong_new = builder.ins().iadd_imm_s(strong, 1);
                            builder.ins().store(flags, strong_new, rc_ptr, 0);
                            builder.ins().jump(post_bump, &[]);

                            builder.switch_to_block(post_bump);
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

                        OpCode::GetUpvalue => {
                            use cranelift_codegen::ir::MemFlagsData;
                            use cranelift_codegen::ir::condcodes::IntCC;
                            let idx = read_u16(code, ip + 1);
                            let idx_val = builder.ins().iconst(types::I32, idx as i64);
                            let flags = MemFlagsData::trusted();

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
                            let kind_is_int = builder.ins().icmp_imm_s(IntCC::Equal, kind_byte, 1);
                            let values_data =
                                builder
                                    .ins()
                                    .load(ptr_ty, flags, closure_ptr, values_box_off);
                            let fresh = emit_upvalue_cache_is_fresh(&mut builder, values_data);
                            let is_int = builder.ins().band(kind_is_int, fresh);

                            let fast_block = builder.create_block();
                            let fallback_block = builder.create_block();
                            let cont_block = builder.create_block();
                            builder
                                .ins()
                                .brif(is_int, fast_block, &[], fallback_block, &[]);

                            builder.switch_to_block(fast_block);
                            let int_val = builder.ins().load(
                                types::I64,
                                flags,
                                values_data,
                                (idx as i32 + 1) * 8,
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

                            let mut shared_post_call_block: Option<Block> = None;

                            let a3_eligible = slot_types.specialized_entry_eligible
                                && !spec_wants_closure_arg
                                && slot_types.self_reachable_by_global_name
                                && spec_thunk_id.is_some()
                                && arg_count as usize == func.arity as usize
                                && virt_stack.top_n_are_int_ssa(arg_count as usize);

                            let fuse_op: Option<OpCode> = if (a3_eligible || arg_count == 1)
                                && ip + 2 < code.len()
                                && !blocks.contains_key(&(ip + 2))
                            {
                                match OpCode::from_byte(code[ip + 2]) {
                                    Some(
                                        o @ (OpCode::Add | OpCode::Subtract | OpCode::Multiply),
                                    ) => Some(o),
                                    _ => None,
                                }
                            } else {
                                None
                            };
                            let fast_join: Option<Block> = fuse_op.map(|_| {
                                let b = builder.create_block();
                                builder.append_block_param(b, types::I64);
                                b
                            });

                            let virt_snap_pre_a3: Option<Vec<VirtSlot>> = if a3_eligible {
                                Some(virt_stack.snapshot())
                            } else {
                                None
                            };

                            let a3_post_call_block: Option<Block> = if a3_eligible {
                                use cranelift_codegen::ir::MemFlagsData;
                                use cranelift_codegen::ir::condcodes::IntCC;

                                let flags = MemFlagsData::trusted();
                                let spec_id = spec_thunk_id.unwrap();

                                let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                                let stack_len = emit_load_stack_len(&mut builder, vm_val);
                                let value_size =
                                    builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                                let one = builder.ins().iconst(types::I64, 1);
                                let callee_slot = builder.ins().isub(stack_len, one);
                                let callee_off = builder.ins().imul(callee_slot, value_size);
                                let callee_ptr = builder.ins().iadd(stack_ptr, callee_off);

                                let tag = builder.ins().load(types::I8, flags, callee_ptr, 0);
                                let closure_tag =
                                    builder.ins().iconst(types::I8, VALUE_TAG_CLOSURE as i64);
                                let is_closure = builder.ins().icmp(IntCC::Equal, tag, closure_tag);

                                let check_rc_block = builder.create_block();
                                let direct_call_block = builder.create_block();
                                let fallback_block = builder.create_block();
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
                                let closure_ptr = builder
                                    .ins()
                                    .iadd_imm_s(curr_rc_raw, RC_VALUE_OFFSET as i64);
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

                                builder.switch_to_block(direct_call_block);
                                if let Some(cp) = counters_ptr_opt {
                                    emit_counter_bump(
                                        &mut builder,
                                        cp,
                                        counter_offsets::SELF_RECURSION_DIRECT_CALL,
                                    );
                                }

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

                                let three_a3 = builder.ins().iconst(types::I32, 3);
                                let is_boxed_a3 = builder.ins().icmp(
                                    cranelift_codegen::ir::condcodes::IntCC::Equal,
                                    status,
                                    three_a3,
                                );
                                let dispatch_a3 = builder.create_block();
                                builder.ins().brif(
                                    is_boxed_a3,
                                    post_call_block,
                                    &[],
                                    dispatch_a3,
                                    &[],
                                );
                                builder.switch_to_block(dispatch_a3);

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
                                if let Some(snap) = virt_snap_pre_a3.clone() {
                                    virt_stack.restore(snap);
                                }
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }

                                Some(post_call_block)
                            } else {
                                if !virt_stack.is_empty() {
                                    virt_stack.flush_to_memory(&mut builder, vm_val);
                                }
                                None
                            };

                            let _ = a3_post_call_block;

                            use cranelift_codegen::ir::MemFlagsData;
                            use cranelift_codegen::ir::condcodes::IntCC;

                            let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                            let stack_len = emit_load_stack_len(&mut builder, vm_val);

                            let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                            let offset_from_top =
                                builder.ins().iconst(types::I64, arg_count as i64 + 1);
                            let callee_slot = builder.ins().isub(stack_len, offset_from_top);
                            let callee_off = builder.ins().imul(callee_slot, value_size);
                            let callee_ptr = builder.ins().iadd(stack_ptr, callee_off);

                            let flags = MemFlagsData::trusted();
                            let tag = builder.ins().load(types::I8, flags, callee_ptr, 0);
                            let closure_tag =
                                builder.ins().iconst(types::I8, VALUE_TAG_CLOSURE as i64);
                            let is_closure = builder.ins().icmp(IntCC::Equal, tag, closure_tag);
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

                            builder.switch_to_block(hit_block);
                            if let Some(cp) = counters_ptr_opt {
                                emit_counter_bump(&mut builder, cp, counter_offsets::CALL_IC_HIT);
                                let probe_closure_ptr =
                                    builder.ins().iadd_imm_s(curr_rc, RC_VALUE_OFFSET as i64);
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
                                let is_native = builder.ins().icmp_imm_s(
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

                            if arg_count == 1
                                && matches!(kind, EntryKind::Generic | EntryKind::IntSpecialized)
                            {
                                use cranelift_codegen::ir::MemFlagsData;
                                use cranelift_codegen::ir::condcodes::IntCC;
                                let f = MemFlagsData::trusted();

                                let inline_after_block = builder.create_block();
                                let try_inline_block = builder.create_block();

                                let ik = builder.ins().load(
                                    types::I8,
                                    f,
                                    cache_val,
                                    CallCacheEntry::OFFSET_INLINE_KIND,
                                );
                                let is_add_upv = builder.ins().icmp_imm_s(
                                    IntCC::Equal,
                                    ik,
                                    ClosureInlineKind::AddUpvalueArg as i64,
                                );
                                builder.ins().brif(
                                    is_add_upv,
                                    try_inline_block,
                                    &[],
                                    inline_after_block,
                                    &[],
                                );

                                builder.switch_to_block(try_inline_block);
                                let inline_obj_ptr =
                                    builder.ins().iadd_imm_s(curr_rc, RC_VALUE_OFFSET as i64);

                                let sp_i = emit_load_stack_ptr(&mut builder, vm_val);
                                let sl_i = emit_load_stack_len(&mut builder, vm_val);
                                let vsz_i = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                                let one_i = builder.ins().iconst(types::I64, 1);
                                let arg_slot_i = builder.ins().isub(sl_i, one_i);
                                let arg_addr_i = {
                                    let off = builder.ins().imul(arg_slot_i, vsz_i);
                                    builder.ins().iadd(sp_i, off)
                                };

                                let arg_tag_i = builder.ins().load(types::I8, f, arg_addr_i, 0);
                                let arg_is_int = builder.ins().icmp_imm_s(
                                    IntCC::Equal,
                                    arg_tag_i,
                                    VALUE_TAG_INTEGER as i64,
                                );

                                let upv_idx = builder.ins().load(
                                    types::I32,
                                    f,
                                    cache_val,
                                    CallCacheEntry::OFFSET_INLINE_UPVALUE_INDEX,
                                );
                                let upv_idx64 = builder.ins().uextend(types::I64, upv_idx);
                                let kinds_off = std::mem::offset_of!(
                                    crate::vm::value::ObjClosure,
                                    upvalue_int_kinds
                                ) as i32;
                                let vals_off = std::mem::offset_of!(
                                    crate::vm::value::ObjClosure,
                                    upvalue_int_values
                                ) as i32;
                                let kinds_data =
                                    builder.ins().load(ptr_ty, f, inline_obj_ptr, kinds_off);
                                let kind_addr = builder.ins().iadd(kinds_data, upv_idx64);
                                let upv_kind = builder.ins().load(types::I8, f, kind_addr, 0);
                                let upv_is_int =
                                    builder.ins().icmp_imm_s(IntCC::Equal, upv_kind, 1);

                                let both_ok = builder.ins().band(arg_is_int, upv_is_int);
                                let fresh_check_block = builder.create_block();
                                let do_inline_block = builder.create_block();
                                let guard_miss_block = builder.create_block();
                                builder.ins().brif(
                                    both_ok,
                                    fresh_check_block,
                                    &[],
                                    guard_miss_block,
                                    &[],
                                );

                                builder.switch_to_block(fresh_check_block);
                                let vals_data =
                                    builder.ins().load(ptr_ty, f, inline_obj_ptr, vals_off);
                                let upv_fresh =
                                    emit_upvalue_cache_is_fresh(&mut builder, vals_data);
                                builder.ins().brif(
                                    upv_fresh,
                                    do_inline_block,
                                    &[],
                                    guard_miss_block,
                                    &[],
                                );

                                builder.switch_to_block(guard_miss_block);
                                if let Some(cp) = counters_ptr_opt {
                                    emit_counter_bump(
                                        &mut builder,
                                        cp,
                                        counter_offsets::CLOSURE_INLINE_GUARD_MISS,
                                    );
                                }
                                builder.ins().jump(inline_after_block, &[]);

                                builder.switch_to_block(do_inline_block);
                                let eight_i = builder.ins().iconst(types::I64, 8);
                                let val_off = builder.ins().imul(upv_idx64, eight_i);
                                let val_addr = builder.ins().iadd(vals_data, val_off);
                                let upv_val = builder.ins().load(types::I64, f, val_addr, 8);
                                let arg_val = builder.ins().load(
                                    types::I64,
                                    f,
                                    arg_addr_i,
                                    VALUE_INT_PAYLOAD_OFFSET as i32,
                                );
                                let inline_result = builder.ins().iadd(upv_val, arg_val);

                                let callee_slot_i = builder.ins().isub(arg_slot_i, one_i);
                                let callee_addr_i = {
                                    let off = builder.ins().imul(callee_slot_i, vsz_i);
                                    builder.ins().iadd(sp_i, off)
                                };
                                let cm_rc_i = builder.ins().load(
                                    types::I64,
                                    f,
                                    callee_addr_i,
                                    VALUE_INT_PAYLOAD_OFFSET as i32,
                                );
                                let strong_i = builder.ins().load(types::I64, f, cm_rc_i, 0);
                                let strong_gt1_i =
                                    builder
                                        .ins()
                                        .icmp(IntCC::UnsignedGreaterThan, strong_i, one_i);
                                let commit_block = builder.create_block();
                                builder.ins().brif(
                                    strong_gt1_i,
                                    commit_block,
                                    &[],
                                    guard_miss_block,
                                    &[],
                                );

                                builder.switch_to_block(commit_block);
                                let dec_i = builder.ins().iadd_imm_s(strong_i, -1);
                                builder.ins().store(f, dec_i, cm_rc_i, 0);
                                builder.ins().store(
                                    f,
                                    callee_slot_i,
                                    vm_val,
                                    vm_stack_view_len_offset(),
                                );
                                if let Some(cp) = counters_ptr_opt {
                                    emit_counter_bump(
                                        &mut builder,
                                        cp,
                                        counter_offsets::CLOSURE_INLINE_HIT,
                                    );
                                }
                                match fast_join {
                                    Some(fj) => {
                                        builder.ins().jump(fj, &[inline_result.into()]);
                                    }
                                    None => {
                                        emit_inline_push_integer(
                                            &mut builder,
                                            vm_val,
                                            inline_result,
                                        );
                                        let pcb = match shared_post_call_block {
                                            Some(b) => b,
                                            None => {
                                                let b = builder.create_block();
                                                shared_post_call_block = Some(b);
                                                b
                                            }
                                        };
                                        builder.ins().jump(pcb, &[]);
                                    }
                                }

                                builder.switch_to_block(inline_after_block);
                                builder.seal_block(try_inline_block);
                                builder.seal_block(fresh_check_block);
                                builder.seal_block(do_inline_block);
                                builder.seal_block(guard_miss_block);
                                builder.seal_block(commit_block);
                                builder.seal_block(inline_after_block);
                            }

                            let post_ca_dispatch_block = if arg_count == 1 {
                                let closure_obj_ptr =
                                    builder.ins().iadd_imm_s(curr_rc, RC_VALUE_OFFSET as i64);

                                let stack_ptr_ca = emit_load_stack_ptr(&mut builder, vm_val);
                                let stack_len_ca = emit_load_stack_len(&mut builder, vm_val);
                                let value_size_ca =
                                    builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                                let one64_ca = builder.ins().iconst(types::I64, 1);
                                let arg_slot_ca = builder.ins().isub(stack_len_ca, one64_ca);
                                let arg_off_ca = builder.ins().imul(arg_slot_ca, value_size_ca);
                                let arg_addr_ca = builder.ins().iadd(stack_ptr_ca, arg_off_ca);

                                let kind_byte = builder.ins().load(
                                    types::I8,
                                    flags,
                                    cache_val,
                                    CallCacheEntry::OFFSET_SPECIALIZED_KIND,
                                );
                                let arity_byte =
                                    builder.ins().load(types::I8, flags, cache_val, 16);
                                let arg_tag = builder.ins().load(types::I8, flags, arg_addr_ca, 0);

                                let is_ca_kind = builder.ins().icmp_imm_s(
                                    IntCC::Equal,
                                    kind_byte,
                                    crate::vm::value::SPECIALIZED_KIND_NATIVE_INT_BODY_WITH_CLOSURE
                                        as i64,
                                );
                                let arity_match = builder.ins().icmp_imm_s(
                                    IntCC::Equal,
                                    arity_byte,
                                    arg_count as i64,
                                );
                                let arg_is_int = builder.ins().icmp_imm_s(
                                    IntCC::Equal,
                                    arg_tag,
                                    VALUE_TAG_INTEGER as i64,
                                );
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

                                builder.switch_to_block(ca_dispatch_block);

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

                                builder.ins().store(
                                    flags,
                                    arg_slot_ca,
                                    vm_val,
                                    vm_stack_view_len_offset(),
                                );

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
                                let frame_size_ca = builder
                                    .ins()
                                    .iconst(types::I64, std::mem::size_of::<JitFrame>() as i64);
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
                                let line_val_ca = builder.ins().iconst(types::I32, line as i64);
                                builder.ins().store(
                                    flags,
                                    closure_obj_ptr,
                                    new_frame_ptr_ca,
                                    JitFrame::OFFSET_CLOSURE_RAW,
                                );
                                let callee_slot_ca = builder.ins().iadd_imm_s(arg_slot_ca, -1);
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
                                let new_fl_ca = builder.ins().iadd(jit_frames_len_ca, one_inc_ca);
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
                                let three_ca = builder.ins().iconst(types::I32, 3);
                                let is_boxed_ca = builder.ins().icmp(
                                    cranelift_codegen::ir::condcodes::IntCC::Equal,
                                    ca_status,
                                    three_ca,
                                );
                                let ca_dispatch = builder.create_block();
                                builder
                                    .ins()
                                    .brif(is_boxed_ca, ca_pcb, &[], ca_dispatch, &[]);
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

                                builder.switch_to_block(ca_err_block);
                                let ca_err_status = builder.block_params(ca_err_block)[0];
                                builder.ins().store(
                                    flags,
                                    jit_frames_len_ca,
                                    vm_val,
                                    vm_jit_frame_view_len_offset(),
                                );
                                let zero64_ca = builder.ins().iconst(types::I64, 0);
                                builder
                                    .ins()
                                    .jump(exit_block, &[ca_err_status.into(), zero64_ca.into()]);

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

                            emit_inline_recursion_guard(&mut builder, exit_block, &refs, vm_val);

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
                            let closure_t_ptr =
                                builder.ins().iadd_imm_s(curr_rc, RC_VALUE_OFFSET as i64);
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
                            let hit_nonzero_block = builder.create_block();
                            builder
                                .ins()
                                .brif(hit_status, hit_nonzero_block, &[], ok_block, &[]);

                            builder.switch_to_block(hit_nonzero_block);
                            let bail_block = builder.create_block();
                            let is_bailout = builder.ins().icmp_imm_s(
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

                            builder.switch_to_block(err_block);
                            let err_status = builder.block_params(err_block)[0];
                            let zero64 = builder.ins().iconst(types::I64, 0);
                            builder
                                .ins()
                                .jump(exit_block, &[err_status.into(), zero64.into()]);

                            builder.switch_to_block(ok_block);

                            let _ = a3_post_call_block;
                            if let Some(pcb) = shared_post_call_block {
                                builder.ins().jump(pcb, &[]);
                                builder.switch_to_block(pcb);
                            }

                            match (fuse_op, fast_join) {
                                (Some(fop), Some(fj)) => {
                                    let fip = ip + 2;
                                    let fline = chunk.lines.get(fip).copied().unwrap_or(0);
                                    let fcol = chunk.columns.get(fip).copied().unwrap_or(0);
                                    let after_fuse = builder.create_block();

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

                            if arg_count <= 1 {
                                use cranelift_codegen::ir::MemFlagsData;
                                use cranelift_codegen::ir::condcodes::IntCC;

                                let mi_val = builder.ins().iconst(types::I32, method_idx as i64);
                                let ac_val = builder.ins().iconst(types::I32, arg_count as i64);
                                let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);
                                let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                                let stack_len = emit_load_stack_len(&mut builder, vm_val);
                                let value_size =
                                    builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                                let offset_from_top =
                                    builder.ins().iconst(types::I64, arg_count as i64 + 1);
                                let receiver_slot = builder.ins().isub(stack_len, offset_from_top);
                                let receiver_off = builder.ins().imul(receiver_slot, value_size);
                                let receiver_ptr = builder.ins().iadd(stack_ptr, receiver_off);
                                let flags = MemFlagsData::trusted();

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
                                let inst_ptr = builder
                                    .ins()
                                    .iadd_imm_s(receiver_raw, RC_VALUE_OFFSET as i64);
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

                                let inline_dispatch_block = builder.create_block();
                                builder.ins().brif(
                                    def_matches,
                                    inline_dispatch_block,
                                    &[],
                                    miss_block,
                                    &[],
                                );

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
                                    0 => 1,
                                    1 => 2,
                                    _ => 0,
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

                                builder.switch_to_block(inline_body_block);
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

                                let locked_byte = builder.ins().load(
                                    types::I8,
                                    flags,
                                    inst_ptr,
                                    STRUCT_INSTANCE_LOCKED_OFFSET as i32,
                                );
                                let after_locked_block = builder.create_block();
                                builder.ins().brif(
                                    locked_byte,
                                    miss_block,
                                    &[],
                                    after_locked_block,
                                    &[],
                                );
                                builder.switch_to_block(after_locked_block);

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

                                let rhs = if arg_count == 0 {
                                    builder.ins().load(
                                        types::I64,
                                        flags,
                                        cache_val,
                                        MethodCacheEntry::OFFSET_INLINE_ADDEND,
                                    )
                                } else {
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

                                let strong_dec = builder.ins().iadd_imm_s(strong, -1);
                                builder.ins().store(flags, strong_dec, receiver_rcbox, 0);

                                let none_tag =
                                    builder.ins().iconst(types::I8, VALUE_TAG_NONE as i64);
                                builder.ins().store(flags, none_tag, receiver_ptr, 0);

                                if arg_count == 1 {
                                    emit_inline_stack_pop_one(&mut builder, vm_val);
                                }
                                builder.ins().jump(ok_block, &[]);

                                builder.switch_to_block(hit_block);
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
                                let strong =
                                    builder.ins().load(types::I64, flags, closure_raw_box, 0);
                                let strong_inc = builder.ins().iadd_imm_s(strong, 1);
                                builder.ins().store(flags, strong_inc, closure_raw_box, 0);
                                let closure_raw_t = builder
                                    .ins()
                                    .iadd_imm_s(closure_raw_box, RC_VALUE_OFFSET as i64);
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
                                let new_stack_len = builder.ins().iadd_imm_s(stack_len, 1);
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
                                let closure_mg = builder.ins().load(
                                    ptr_ty,
                                    flags,
                                    closure_raw_t,
                                    obj_closure_module_globals_offset(),
                                );
                                let def_t = builder
                                    .ins()
                                    .iadd_imm_s(inst_def_raw, RC_VALUE_OFFSET as i64);
                                let def_mg = builder.ins().load(
                                    ptr_ty,
                                    flags,
                                    def_t,
                                    std::mem::offset_of!(
                                        crate::vm::value::ObjStructDef,
                                        module_globals
                                    ) as i32,
                                );
                                let module_globals =
                                    builder.ins().select(def_mg, def_mg, closure_mg);
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
                                let new_jit_len = builder.ins().iadd_imm_s(jit_frames_len, 1);
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
                                        builder.ins().call(refs.op_return, &[vm_val]);
                                        let zero32 = builder.ins().iconst(types::I32, 0);
                                        let zero64 = builder.ins().iconst(types::I64, 0);
                                        builder
                                            .ins()
                                            .jump(exit_block, &[zero32.into(), zero64.into()]);
                                    }
                                }
                                EntryKind::IntSpecialized => {
                                    use cranelift_codegen::ir::MemFlagsData;
                                    use cranelift_codegen::ir::condcodes::IntCC;
                                    let flags = MemFlagsData::trusted();

                                    macro_rules! emit_intspec_teardown {
                                        ($slow_result:expr) => {{
                                            let stack_ptr_td =
                                                emit_load_stack_ptr(&mut builder, vm_val);
                                            let vsize_td =
                                                builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                                            let cm_off_td =
                                                builder.ins().imul(slot_offset_val, vsize_td);
                                            let cm_addr_td =
                                                builder.ins().iadd(stack_ptr_td, cm_off_td);
                                            let cm_rc_td = builder.ins().load(
                                                types::I64,
                                                flags,
                                                cm_addr_td,
                                                VALUE_INT_PAYLOAD_OFFSET as i32,
                                            );
                                            let strong_td =
                                                builder.ins().load(types::I64, flags, cm_rc_td, 0);
                                            let one_td = builder.ins().iconst(types::I64, 1);
                                            let gt1_td = builder.ins().icmp(
                                                IntCC::UnsignedGreaterThan,
                                                strong_td,
                                                one_td,
                                            );
                                            let fast_td = builder.create_block();
                                            let slow_td = builder.create_block();
                                            builder.ins().brif(gt1_td, fast_td, &[], slow_td, &[]);

                                            builder.switch_to_block(slow_td);
                                            if let Some(v) = $slow_result {
                                                emit_inline_push_integer(&mut builder, vm_val, v);
                                            }
                                            builder.ins().call(refs.op_return, &[vm_val]);
                                            let three_td = builder.ins().iconst(types::I32, 3);
                                            let zero64_td = builder.ins().iconst(types::I64, 0);
                                            builder.ins().jump(
                                                exit_block,
                                                &[three_td.into(), zero64_td.into()],
                                            );

                                            builder.switch_to_block(fast_td);
                                            builder.seal_block(fast_td);
                                            builder.seal_block(slow_td);
                                            let dec_td = builder.ins().iadd_imm_s(strong_td, -1);
                                            builder.ins().store(flags, dec_td, cm_rc_td, 0);

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

                                    let spec_inline_eligible = !info.may_capture_upvalues
                                        && return_slots_safe_to_truncate(
                                            &slot_types,
                                            &param_mirrors,
                                            func,
                                        );

                                    if !spec_inline_eligible {
                                        if !virt_stack.is_empty() {
                                            virt_stack.flush_to_memory(&mut builder, vm_val);
                                        }
                                        builder.ins().call(refs.op_return, &[vm_val]);
                                        let three = builder.ins().iconst(types::I32, 3);
                                        let zero64 = builder.ins().iconst(types::I64, 0);
                                        builder
                                            .ins()
                                            .jump(exit_block, &[three.into(), zero64.into()]);
                                    } else if let Some(top) = virt_stack.pop_int_ssa() {
                                        emit_intspec_teardown!(Some(top));
                                        let zero32 = builder.ins().iconst(types::I32, 0);
                                        builder
                                            .ins()
                                            .jump(exit_block, &[zero32.into(), top.into()]);
                                    } else {
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
                                        let is_int = builder.ins().icmp(IntCC::Equal, tag, int_tag);
                                        let int_block = builder.create_block();
                                        let boxed_block = builder.create_block();
                                        builder.ins().brif(
                                            is_int,
                                            int_block,
                                            &[],
                                            boxed_block,
                                            &[],
                                        );

                                        builder.switch_to_block(int_block);
                                        let payload = builder.ins().load(
                                            types::I64,
                                            flags,
                                            top_addr,
                                            VALUE_INT_PAYLOAD_OFFSET as i32,
                                        );
                                        emit_intspec_teardown!(
                                            None::<cranelift_codegen::ir::Value>
                                        );
                                        let zero32 = builder.ins().iconst(types::I32, 0);
                                        builder
                                            .ins()
                                            .jump(exit_block, &[zero32.into(), payload.into()]);

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

                        _ => return Err(()),
                    }
                }

                if !terminated {
                    let two = builder.ins().iconst(types::I32, 2);
                    let zero64 = builder.ins().iconst(types::I64, 0);
                    builder.ins().jump(exit_block, &[two.into(), zero64.into()]);
                }

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
                builder.finalize(self.module.target_config());
            }

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
                && let Some(vcode) = code.vcode.as_ref()
            {
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

        let (lean, lean_arity) = if slot_types.lean_entry_eligible {
            let cold_id = self.lean_cold_id;
            self.next_id = self.next_id.wrapping_add(1);
            let seq = self.next_id;
            match lean::compile_lean_entry(
                &mut self.module,
                &mut self.ctx,
                &mut self.fbc,
                func,
                cold_id,
                seq,
            ) {
                Some(id) => {
                    self.module.finalize_definitions().map_err(|_| ())?;
                    let p = self.module.get_finalized_function(id);
                    (Some(p as *const ()), func.arity)
                }
                None => (None, 0),
            }
        } else {
            (None, 0)
        };

        Ok(CompiledEntries {
            generic,
            specialized,
            specialized_arity,
            specialized_kind,
            lean,
            lean_arity,
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

/// Emits a call to a fallible helper, then a check: if status != 0, returns
/// early from the thunk with that status. Otherwise continues in a fresh "ok"
/// block.
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

/// True (as an `i8` flag) when a closure's upvalue int cache was filled at the
/// current `UPVALUE_WRITE_EPOCH`, i.e. no closed upvalue cell has been written
/// since. `values_data` is the data pointer of `upvalue_int_values`, whose
/// first element is the fill epoch.
fn emit_upvalue_cache_is_fresh(
    builder: &mut FunctionBuilder<'_>,
    values_data: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    use cranelift_codegen::ir::MemFlagsData;
    use cranelift_codegen::ir::condcodes::IntCC;
    let flags = MemFlagsData::trusted();
    let epoch_addr = builder.ins().iconst(
        types::I64,
        &crate::jit::UPVALUE_WRITE_EPOCH as *const std::sync::atomic::AtomicI64 as i64,
    );
    let current = builder.ins().load(types::I64, flags, epoch_addr, 0);
    let filled_at = builder.ins().load(types::I64, flags, values_data, 0);
    builder.ins().icmp(IntCC::Equal, current, filled_at)
}

/// Typed-parameter check at a compiled function's generic entry, returning the
/// block the body starts in.
///
/// An Integer tag in an `<int>` parameter is accepted inline; anything else asks
/// `jit_check_param_types`, which raises the interpreter's error. A value it
/// accepts that is not an Integer (an optional parameter's None, a struct named
/// INTEGER) bails out to the interpreter. Other annotations always go through
/// the helper.
fn emit_param_type_guard(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    params: &[crate::vm::value::ParamInfo],
) -> Block {
    use cranelift_codegen::ir::MemFlagsData;
    use cranelift_codegen::ir::condcodes::IntCC;
    let flags = MemFlagsData::trusted();
    let is_int_ann = |t: &str| matches!(t, "int" | "INTEGER");

    let done_block = builder.create_block();
    let all_int = params
        .iter()
        .all(|p| p.type_ann.as_deref().is_none_or(is_int_ann));
    if !all_int {
        let call = builder.ins().call(refs.check_param_types, &[vm_val]);
        let status = builder.inst_results(call)[0];
        emit_early_exit_on_err(builder, exit_block, status);
    }

    let slow_block = builder.create_block();
    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    for (i, param) in params.iter().enumerate() {
        if !param.type_ann.as_deref().is_some_and(is_int_ann) {
            continue;
        }
        let slot = builder.ins().iconst(types::I64, i as i64 + 1);
        let abs_slot = builder.ins().iadd(slot_offset_val, slot);
        let byte_off = builder.ins().imul(abs_slot, value_size);
        let slot_ptr = builder.ins().iadd(stack_ptr, byte_off);
        let tag = builder.ins().load(types::I8, flags, slot_ptr, 0);
        let is_int = builder
            .ins()
            .icmp_imm_s(IntCC::Equal, tag, VALUE_TAG_INTEGER as i64);
        let next = builder.create_block();
        builder.ins().brif(is_int, next, &[], slow_block, &[]);
        builder.switch_to_block(next);
    }
    builder.ins().jump(done_block, &[]);

    builder.switch_to_block(slow_block);
    let call = builder.ins().call(refs.check_param_types, &[vm_val]);
    let status = builder.inst_results(call)[0];
    emit_early_exit_on_err(builder, exit_block, status);
    let two = builder.ins().iconst(types::I32, 2);
    let zero64 = builder.ins().iconst(types::I64, 0);
    builder.ins().jump(exit_block, &[two.into(), zero64.into()]);

    builder.switch_to_block(done_block);
    done_block
}

/// Branches on a u32 status produced by a helper call: non-zero returns the
/// status from the thunk; zero continues in a fresh "ok" block.
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

/// Inline recursion-depth guard for the JIT call paths. Bounds-checks the
/// `jit_frames` (cap `FRAMES_MAX`) and value-stack (cap `STACK_MAX`) buffers
/// before a `JitFrame` and operands are pushed. The common path is two loads,
/// two compares and a branch; on overflow it calls into the helper, which stashes
/// the "stack overflow" error and exits.
fn emit_inline_recursion_guard(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlagsData;
    use cranelift_codegen::ir::condcodes::IntCC;
    let flags = MemFlagsData::trusted();
    let jfl = builder
        .ins()
        .load(types::I64, flags, vm_val, vm_jit_frame_view_len_offset());
    let sl = builder
        .ins()
        .load(types::I64, flags, vm_val, vm_stack_view_len_offset());
    let frame_max = builder
        .ins()
        .iconst(types::I64, crate::vm::FRAMES_MAX as i64);
    let stack_lim = builder.ins().iconst(
        types::I64,
        (crate::vm::STACK_MAX - crate::vm::STACK_GUARD_MARGIN) as i64,
    );
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
    builder.switch_to_block(ovf_block);
    let call = builder.ins().call(refs.check_recursion_depth, &[vm_val]);
    let status = builder.inst_results(call)[0];
    let zero64 = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[status.into(), zero64.into()]);
    builder.switch_to_block(ok_block);
}

/// A loop back-edge's share of the host's limits (`vm::limits`). The common
/// path loads the armed byte and branches past. While armed it counts the
/// iteration against `loop_countdown` inline, and on an interrupt or a spent
/// budget calls `loop_stop` and leaves the function.
fn emit_loop_poll(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    limits_armed: usize,
) {
    use cranelift_codegen::ir::MemFlagsData;
    let flags = MemFlagsData::trusted();
    let armed_ptr = if limits_armed != 0 {
        builder.ins().iconst(types::I64, limits_armed as i64)
    } else {
        builder
            .ins()
            .load(types::I64, flags, vm_val, vm_limits_armed_offset())
    };
    let armed = builder.ins().load(types::I8, flags, armed_ptr, 0);
    let armed_block = builder.create_block();
    let budget_block = builder.create_block();
    let count_block = builder.create_block();
    let stop_block = builder.create_block();
    let done_block = builder.create_block();
    for cold in [armed_block, budget_block, count_block, stop_block] {
        builder.set_cold_block(cold);
    }
    builder.ins().brif(armed, armed_block, &[], done_block, &[]);

    builder.switch_to_block(armed_block);
    let interrupt = builder
        .ins()
        .band_imm_s(armed, i64::from(crate::vm::limits::INTERRUPT));
    builder
        .ins()
        .brif(interrupt, stop_block, &[], budget_block, &[]);

    builder.switch_to_block(budget_block);
    let offset = vm_loop_countdown_offset();
    let left = builder.ins().load(types::I64, flags, vm_val, offset);
    builder.ins().brif(left, count_block, &[], stop_block, &[]);

    builder.switch_to_block(count_block);
    let left = builder.ins().iadd_imm_s(left, -1);
    builder.ins().store(flags, left, vm_val, offset);
    builder.ins().jump(done_block, &[]);

    builder.switch_to_block(stop_block);
    let call = builder.ins().call(refs.loop_stop, &[vm_val]);
    let status = builder.inst_results(call)[0];
    let zero64 = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[status.into(), zero64.into()]);

    builder.switch_to_block(done_block);
}

fn read_u16(code: &[u8], offset: usize) -> u16 {
    ((code[offset] as u16) << 8) | (code[offset + 1] as u16)
}

/// Counts inline-cache sites so the translator can pre-allocate their slots.
/// Instruction boundaries come from the canonical decoder in
/// `compiler::opcode`.
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

/// Whether every parameter accepts any Integer: parameters are untyped or
/// `<int>`.
fn params_admit_any_int(func: &Function) -> bool {
    func.params.iter().all(|p| {
        p.type_ann
            .as_deref()
            .is_none_or(|t| matches!(t, "int" | "INTEGER"))
    })
}

/// Detects whether `func` is exactly `fun(y) { x + y }` over one immutable
/// captured integer, returning the upvalue slot it reads.
///
/// The whole body must be these 8 bytes:
///
/// ```text
/// GetUpvalue(k)   3
/// GetLocal(1)     3     slot 0 is the closure marker, so 1 is the parameter
/// Add             1
/// Return          1
/// ```
///
/// Whether the upvalue holds an integer is checked by a guard at the call site.
pub(crate) fn detect_inline_closure_upvalue_add(func: &Function) -> Option<u16> {
    if func.arity != 1 || !params_admit_any_int(func) {
        return None;
    }
    let code = &func.chunk.code;
    if code.len() != 8 {
        return None;
    }
    if OpCode::from_byte(code[0]) != Some(OpCode::GetUpvalue)
        || OpCode::from_byte(code[3]) != Some(OpCode::GetLocal)
        || OpCode::from_byte(code[6]) != Some(OpCode::Add)
        || OpCode::from_byte(code[7]) != Some(OpCode::Return)
    {
        return None;
    }
    if read_u16(code, 4) != 1 {
        return None;
    }
    Some(read_u16(code, 1))
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

/// Detects whether `func`'s bytecode is a single field-add peephole body
/// (`self.f = self.f + <const|arg>; return`), and if so returns the kind plus
/// the field name and addend for stamping into the MethodCacheEntry.
pub(crate) fn detect_inline_method_info(func: &Function) -> Option<DetectedInlineMethod> {
    if !params_admit_any_int(func) {
        return None;
    }
    let code = &func.chunk.code;
    if code.len() < 17 {
        return None;
    }

    let empty_blocks: HashMap<usize, Block> = HashMap::new();
    let m = match_struct_field_add_update(code, &func.chunk, 0, &empty_blocks)?;

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

    let field_name_val = func.chunk.constants.get(m.field_idx as usize)?;
    let field_name: std::rc::Rc<String> = std::rc::Rc::clone(field_name_val.as_string()?);

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

/// Emits IR that loads `vm.stack_view.ptr` at its pinned offset.
#[inline]
fn emit_load_stack_ptr(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    use cranelift_codegen::ir::MemFlagsData;
    builder.ins().load(
        types::I64,
        MemFlagsData::trusted(),
        vm_val,
        vm_stack_view_ptr_offset(),
    )
}

/// Emits IR that loads `vm.stack_view.len` at its pinned offset.
#[inline]
fn emit_load_stack_len(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    use cranelift_codegen::ir::MemFlagsData;
    builder.ins().load(
        types::I64,
        MemFlagsData::trusted(),
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
    use cranelift_codegen::ir::MemFlagsData;

    let base = builder.ins().load(
        types::I64,
        MemFlagsData::trusted(),
        vm_val,
        vm_jit_frame_view_ptr_offset(),
    );
    let len = builder.ins().load(
        types::I64,
        MemFlagsData::trusted(),
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
    use cranelift_codegen::ir::MemFlagsData;

    let frame_ptr = emit_load_top_jit_frame_ptr(builder, vm_val);
    builder.ins().load(
        types::I64,
        MemFlagsData::trusted(),
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
    use cranelift_codegen::ir::MemFlagsData;

    let flags = MemFlagsData::trusted();
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
    use cranelift_codegen::ir::MemFlagsData;

    let flags = MemFlagsData::trusted();
    let closure_tag = builder.ins().iconst(types::I8, VALUE_TAG_CLOSURE as i64);
    builder.ins().store(flags, closure_tag, dst_ptr, 0);
    builder
        .ins()
        .store(flags, closure_raw, dst_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
}

/// Records `(line, col)` in this activation's frame for error attribution.
///
/// `frame_ptr` is the caller's hoisted `own_frame_ptr`; the running function's
/// frame is always the one written. `line` and `column` are adjacent `u32`s at
/// offsets 24 and 28 of a `#[repr(C)]` `JitFrame` of size 32, written with one
/// 8-byte store.
#[allow(dead_code)]
#[inline]
fn emit_store_current_loc(
    builder: &mut FunctionBuilder<'_>,
    frame_ptr: cranelift_codegen::ir::Value,
    line: u32,
    col: u32,
) {
    use cranelift_codegen::ir::MemFlagsData;

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
        MemFlagsData::trusted(),
        packed_val,
        frame_ptr,
        JitFrame::OFFSET_LINE,
    );
}

/// Conditional line-store helper for per-opcode arms whose fault path needs the
/// line written but whose fast path doesn't. Updates the `last_emitted_line`
/// cache so later same-line ops in the same block skip the store.
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

/// True iff this opcode's emission path always reaches a runtime helper that
/// may stash an error attributed to this IP. Arms that return `false` call
/// `maybe_emit_current_line` themselves at any internal slow-path emission
/// point that can fault.
///
/// `OpCode::Add | Subtract | Multiply | Divide | Modulo | Less | LessEqual |
/// Greater | GreaterEqual | Equal | NotEqual` are conditionally faulting: when
/// both operands are virt-int the emitted IR is purely register arithmetic with
/// no helper call, so these return `false`.
fn opcode_always_needs_line(op: OpCode) -> bool {
    use OpCode::*;
    match op {
        Constant | None | True | False | Pop | Dup | GetLocal | SetLocal | GetUpvalue
        | SetUpvalue | CloseUpvalue | StructDef | Jump | Loop | JumpIfFalse | JumpIfTrue
        | PopJumpIfFalse | Not | Add | Subtract | Multiply | Divide | Modulo | Less | LessEqual
        | Greater | GreaterEqual | Equal | NotEqual | CheckReturn => false,
        Negate | Index | IndexExisting | TypeWrap | BuildArray | GetGlobal | SetGlobal
        | DefineGlobal | DefineGlobalTyped | Closure | Call | StructLiteral | GetField
        | SetField | DefineMethod | MethodCall | Return => true,
        _ => true,
    }
}

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

/// Arithmetic where the lhs is on the value stack and the rhs is already in a
/// register: tag-checks the memory lhs, does register arithmetic and overwrites
/// the payload in place on a hit, otherwise materializes `rhs` and falls back to
/// the generic helper. Both paths leave the result at the same memory top. The
/// caller stamps the line and pops the staged rhs.
#[inline]
fn emit_mixed_mode_int_arith(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    rhs: cranelift_codegen::ir::Value,
    op: OpCode,
) {
    use cranelift_codegen::ir::MemFlagsData;
    use cranelift_codegen::ir::condcodes::IntCC;
    let flags = MemFlagsData::trusted();

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
    builder.ins().brif(is_int, fast_block, &[], slow_block, &[]);

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

    builder.switch_to_block(cont_block);
    builder.seal_block(fast_block);
    builder.seal_block(slow_block);
    builder.seal_block(cont_block);
}

/// Emits IR for an arithmetic opcode that has an inline int+int fast path. The
/// sequence checks the top-two values' tag bytes; if both are `Value::Integer`,
/// it performs `op` inline on their i64 payloads (wrapping semantics), writes
/// the result back in place, then calls the `stack_pop_one` helper to drop the
/// duplicate top. Otherwise it falls through to `slow_helper`.
fn emit_int_fast_arith(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    _refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    op: IntArithOp,
    slow_helper: FuncRef,
) {
    use cranelift_codegen::ir::MemFlagsData;

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

    let flags = MemFlagsData::trusted();
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
        IntArithOp::Shl => builder.ins().ishl(payload_a, payload_b),
        IntArithOp::Shr => builder.ins().sshr(payload_a, payload_b),
    };
    builder
        .ins()
        .store(flags, result, val_a_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
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

    let _ = ptr_ty;
}

/// Inline IR for the struct-field-add peephole (`self.f = self.f + k` /
/// `self.f = self.f + rhs_local`). On IC hit, the integer field's i64 payload is
/// updated in place. On miss, it falls through to the runtime helper.
///
/// Guards, in order:
///   1. Receiver slot tag == `VALUE_TAG_STRUCT_INSTANCE`.
///   2. `inst.def` raw pointer == `cache.struct_def_raw` (nonzero).
///   3. (Local variant only) rhs slot tag == `VALUE_TAG_INTEGER`.
///   4. Current field slot tag == `VALUE_TAG_INTEGER`.
///
/// Any guard failure branches to the miss path.
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
    frame_ptr: cranelift_codegen::ir::Value,
    get_field_loc: (u32, u32),
) {
    use cranelift_codegen::ir::MemFlagsData;
    use cranelift_codegen::ir::condcodes::IntCC;

    let flags = MemFlagsData::trusted();
    let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);

    let check_def_block = builder.create_block();
    let check_rhs_block = builder.create_block();
    let check_val_block = builder.create_block();
    let fast_block = builder.create_block();
    let miss_block = builder.create_block();
    let ok_block = builder.create_block();
    let err_block = builder.create_block();
    builder.append_block_param(err_block, types::I32);

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let self_slot_off = builder.ins().iconst(types::I64, self_slot as i64);
    let self_slot_abs = builder.ins().iadd(slot_offset_val, self_slot_off);
    let self_byte_off = builder.ins().imul(self_slot_abs, value_size);
    let self_val_ptr = builder.ins().iadd(stack_ptr, self_byte_off);

    let tag = builder.ins().load(types::I8, flags, self_val_ptr, 0);
    let struct_tag = builder
        .ins()
        .iconst(types::I8, VALUE_TAG_STRUCT_INSTANCE as i64);
    let is_struct = builder.ins().icmp(IntCC::Equal, tag, struct_tag);
    builder
        .ins()
        .brif(is_struct, check_def_block, &[], miss_block, &[]);

    builder.switch_to_block(check_def_block);
    let rcbox = builder
        .ins()
        .load(ptr_ty, flags, self_val_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
    let inst_ptr = builder.ins().iadd_imm_s(rcbox, RC_VALUE_OFFSET as i64);
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
    let check_locked_block = builder.create_block();
    builder
        .ins()
        .brif(def_matches, check_locked_block, &[], miss_block, &[]);

    builder.switch_to_block(check_locked_block);
    let locked_byte = builder.ins().load(
        types::I8,
        flags,
        inst_ptr,
        STRUCT_INSTANCE_LOCKED_OFFSET as i32,
    );
    builder
        .ins()
        .brif(locked_byte, miss_block, &[], check_rhs_block, &[]);

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

    builder.switch_to_block(miss_block);
    emit_store_current_loc(builder, frame_ptr, get_field_loc.0, get_field_loc.1);
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

/// Inline `jit_stack_pop_one`: decrements `stack_view.len`. Only valid when the
/// popped slot holds a primitive-tag value (no `Drop` side-effect).
#[inline]
fn emit_inline_stack_pop_one(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlagsData;
    let flags = MemFlagsData::trusted();
    let top = emit_load_stack_len(builder, vm_val);
    let one = builder.ins().iconst(types::I64, 1);
    let new_top = builder.ins().isub(top, one);
    builder
        .ins()
        .store(flags, new_top, vm_val, vm_stack_view_len_offset());
}

/// Inline `jit_push_integer_inline`: writes Value::Integer(payload) at the
/// current stack top and bumps the length.
///
/// Layout is pinned by `value_size_is_pinned_at_40` and
/// `value_integer_layout_is_pinned` in vm/value.rs.
#[inline]
fn emit_inline_push_integer(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
    payload: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlagsData;
    let flags = MemFlagsData::trusted();
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

/// Stores an `i64` into the VM stack slot for `slot`, keeping the slot's tag as
/// Integer. Exact-slot store: does not push and does not modify
/// `stack_view.len` or `Vec::len`.
#[allow(dead_code)]
fn emit_store_stack_slot_integer(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    slot: u16,
    value: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlagsData;
    let flags = MemFlagsData::trusted();
    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let slot_const = builder.ins().iconst(types::I64, slot as i64);
    let abs_slot = builder.ins().iadd(slot_offset_val, slot_const);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let byte_off = builder.ins().imul(abs_slot, value_size);
    let slot_ptr = builder.ins().iadd(stack_ptr, byte_off);
    let tag = builder.ins().iconst(types::I8, VALUE_TAG_INTEGER as i64);
    builder.ins().store(flags, tag, slot_ptr, 0);
    builder
        .ins()
        .store(flags, value, slot_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
}

/// Spills every currently-live virtualized Int64 local back to its backing VM
/// stack slot, in sorted slot order.
///
/// Does not modify `stack_view.len`.
#[allow(dead_code)]
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

/// Inline no-upvalue fast path of `jit_op_return`. Only valid when
/// `scan_info.may_capture_upvalues == false`, i.e. the function body contains no
/// `OpCode::Closure`.
#[inline]
#[allow(dead_code)]
fn emit_inline_op_return(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlagsData;
    let flags = MemFlagsData::trusted();

    let slot_off = emit_load_top_jit_frame_slot_offset(builder, vm_val);

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let top = emit_load_stack_len(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let one = builder.ins().iconst(types::I64, 1);
    let top_idx = builder.ins().isub(top, one);
    let src_byte = builder.ins().imul(top_idx, value_size);
    let src_ptr = builder.ins().iadd(stack_ptr, src_byte);

    let mut words = Vec::with_capacity(VALUE_SIZE / 8);
    for off in (0..VALUE_SIZE).step_by(8) {
        words.push(builder.ins().load(types::I64, flags, src_ptr, off as i32));
    }

    let jf_len = builder
        .ins()
        .load(types::I64, flags, vm_val, vm_jit_frame_view_len_offset());
    let new_jf_len = builder.ins().isub(jf_len, one);
    builder
        .ins()
        .store(flags, new_jf_len, vm_val, vm_jit_frame_view_len_offset());

    builder.ins().call(refs.stack_truncate, &[vm_val, slot_off]);

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

/// Inline `jit_replace_top2_with_bool` for the integer fast path. The top two
/// values are Integer. Writes Value::Boolean at stack[len-2], then decrements
/// len by 1.
///
/// Layout: Bool tag = 3 (pinned by `value_bool_tag_and_payload_are_pinned` in
/// vm/value.rs), payload byte at offset 1.
#[inline]
fn emit_inline_replace_top2_with_bool(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
    bool_val: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlagsData;
    let flags = MemFlagsData::trusted();
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

/// Inline `jit_pop` for a top-of-stack primitive-tag value. Reads the top tag
/// byte, and if it is in the primitive range (0..=6), decrements
/// `stack_view.len`. Otherwise falls back to the helper so Rc-bearing values are
/// dropped correctly.
#[inline]
fn emit_inline_pop_tag_gated(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlagsData;
    use cranelift_codegen::ir::condcodes::IntCC;
    let flags = MemFlagsData::trusted();
    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let top = emit_load_stack_len(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let one = builder.ins().iconst(types::I64, 1);
    let top_idx = builder.ins().isub(top, one);
    let byte_off = builder.ins().imul(top_idx, value_size);
    let slot_ptr = builder.ins().iadd(stack_ptr, byte_off);
    let tag = builder.ins().load(types::I8, flags, slot_ptr, 0);
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

/// Inline `jit_push_float_inline`: like the integer version with
/// `VALUE_TAG_FLOAT` and the f64 bit pattern at the payload offset.
#[inline]
fn emit_inline_push_float(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
    bits: cranelift_codegen::ir::Value,
) {
    use cranelift_codegen::ir::MemFlagsData;
    let flags = MemFlagsData::trusted();
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
    use cranelift_codegen::ir::MemFlagsData;
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

    let flags = MemFlagsData::trusted();
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
    let rhs_nonzero = builder.ins().icmp_imm_s(IntCC::NotEqual, rhs, 0);
    let nonzero_block = builder.create_block();
    let fast_math_block = builder.create_block();
    builder
        .ins()
        .brif(rhs_nonzero, nonzero_block, &[], slow_block, &[]);

    builder.switch_to_block(nonzero_block);
    let lhs_min = builder.ins().icmp_imm_s(IntCC::Equal, lhs, i64::MIN);
    let rhs_neg1 = builder.ins().icmp_imm_s(IntCC::Equal, rhs, -1);
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

/// True iff `val` is the result of a Cranelift `iconst` with the given
/// immediate.
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

/// Extracts the i64 immediate of a Cranelift Value if it is the result of an
/// `iconst`, else None.
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

/// Parity-branch peephole. Detects the four shapes
///
/// ```text
/// (x % 2) == 0      pattern A, "is even"
/// (x % 2) != 0      pattern A, "is odd"
/// 0 == (x % 2)      pattern B, "is even" (commuted)
/// 0 != (x % 2)      pattern B, "is odd"  (commuted)
/// ```
///
/// when followed immediately by `JumpIfFalse` / `JumpIfTrue` /
/// `PopJumpIfFalse`. On match, lowers to `band_imm + icmp_imm + brif` and
/// returns `Some(next_ip)`, the ip after all consumed opcodes. On mismatch
/// returns None. Must be called before emitting any IR for the Modulo itself.
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

    if virt_stack.pending_depth() < 2 {
        return None;
    }

    let rhs = virt_stack.peek_int_ssa()?;
    if !is_iconst_imm(builder, rhs, 2) {
        return None;
    }

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
    if blocks.contains_key(&after_mod) {
        return None;
    }

    let off = read_u16(code, branch_ip + 1) as usize;
    let target_ip = branch_ip + 3 + off;
    let next_ip = branch_ip + 3;

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

    if is_pattern_b {
        let zero_val = virt_stack.peek_int_ssa_at(2)?;
        if !is_iconst_imm(builder, zero_val, 0) {
            return None;
        }
    }

    if let Some(cp) = counters_ptr_opt {
        emit_counter_bump(builder, cp, counter_offsets::VIRT_BRANCH_PARITY_HIT);
    }

    virt_stack.pop_int_ssa().unwrap();
    let lhs = virt_stack.pop_int_ssa().unwrap();
    if is_pattern_b {
        virt_stack.pop_int_ssa().unwrap();
    }

    let parity = builder.ins().band_imm_u(lhs, 1);
    let cc = if is_equal {
        IntCC::Equal
    } else {
        IntCC::NotEqual
    };
    let pred = builder.ins().icmp_imm_s(cc, parity, 0);

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

    Some(branch_ip + 3)
}

/// Loads the `i64` payload from the Value at stack[stack_view.len - 1].
#[inline]
fn emit_load_stack_top_integer_payload(
    builder: &mut FunctionBuilder<'_>,
    vm_val: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    use cranelift_codegen::ir::MemFlagsData;
    let flags = MemFlagsData::trusted();
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

/// Signed-div-by-power-of-two peephole. Tries to lower `OpCode::Divide` whose
/// RHS is a constant power-of-two integer to a bias-and-shift sequence:
///
/// ```text
/// q = (x + ((x >> 63) & (|d| - 1))) >> log2(|d|)
/// if d < 0 { q = -q }
/// ```
///
/// Returns None when:
///
///   * RHS is not a recognized iconst at compile time.
///   * d == 0.
///   * d == -1.
///   * |d| is not a power of two.
///
/// Returns `Some(quotient)` on match; the caller pops both operands from the
/// virt stack and pushes the returned value.
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
        lhs
    } else {
        let sign = builder.ins().sshr_imm_u(lhs, 63);
        let bias_mask = abs - 1;
        let bias = builder.ins().band_imm_u(sign, bias_mask);
        let adjusted = builder.ins().iadd(lhs, bias);
        builder.ins().sshr_imm_u(adjusted, k)
    };

    let q = if d < 0 { builder.ins().ineg(q) } else { q };
    Some(q)
}

/// Emits integer divide or modulo for operands held in registers. The fast
/// path guards against a zero divisor and `i64::MIN / -1`; the slow path boxes
/// both operands onto the VM stack and calls the runtime helper.
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

    let rhs_nonzero = builder.ins().icmp_imm_s(IntCC::NotEqual, rhs, 0);
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

    builder.switch_to_block(zero_slow_block);
    if let Some(cp) = counters_ptr {
        emit_counter_bump(builder, cp, counter_offsets::VIRT_DIVMOD_SLOW_ZERO);
    }
    builder.ins().jump(slow_block, &[]);

    builder.switch_to_block(nonzero_block);
    let lhs_min = builder.ins().icmp_imm_s(IntCC::Equal, lhs, i64::MIN);
    let rhs_neg1 = builder.ins().icmp_imm_s(IntCC::Equal, rhs, -1);
    let overflow = builder.ins().band(lhs_min, rhs_neg1);
    builder
        .ins()
        .brif(overflow, overflow_slow_block, &[], fast_block, &[]);

    builder.switch_to_block(overflow_slow_block);
    if let Some(cp) = counters_ptr {
        emit_counter_bump(builder, cp, counter_offsets::VIRT_DIVMOD_SLOW_OVERFLOW);
    }
    builder.ins().jump(slow_block, &[]);

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

/// Source locations a local-update peephole stamps before each helper call that
/// can fault.
#[derive(Clone, Copy)]
struct PeepholeLocs {
    frame_ptr: cranelift_codegen::ir::Value,
    /// The `Multiply` of a scaled update; unused otherwise.
    mul: (u32, u32),
    /// The `Add`/`Subtract`/`Multiply` that writes the local.
    arith: (u32, u32),
}

fn chunk_loc(chunk: &Chunk, ip: usize) -> (u32, u32) {
    (
        chunk.lines.get(ip).copied().unwrap_or(0),
        chunk.columns.get(ip).copied().unwrap_or(0),
    )
}

/// Slow-path store of the stack top into a local, through the checked helper so
/// a type-locked local rejects a non-matching result like `SetLocal` does.
fn emit_slow_set_local_checked(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    slot_val: cranelift_codegen::ir::Value,
) {
    let call = builder
        .ins()
        .call(refs.set_local_checked, &[vm_val, slot_val]);
    let status = builder.inst_results(call)[0];
    emit_early_exit_on_err(builder, exit_block, status);
}

/// Peephole fast path for `GetLocal(slot); Constant(int); Arith; SetLocal(slot);
/// [Pop]`. On integer locals it updates the local payload in place. Non-integer
/// locals replay the original helper sequence.
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
    locs: PeepholeLocs,
) {
    use cranelift_codegen::ir::MemFlagsData;
    use cranelift_codegen::ir::condcodes::IntCC;

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);

    let slot_const = builder.ins().iconst(types::I64, slot as i64);
    let abs_slot = builder.ins().iadd(slot_offset_val, slot_const);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let byte_off = builder.ins().imul(abs_slot, value_size);
    let addr_slot = builder.ins().iadd(stack_ptr, byte_off);

    let flags = MemFlagsData::trusted();
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
    emit_store_current_loc(builder, locs.frame_ptr, locs.arith.0, locs.arith.1);
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
    emit_slow_set_local_checked(builder, exit_block, refs, vm_val, slot_val);
    if pop_after {
        builder.ins().call(refs.pop, &[vm_val]);
    }
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(cont_block);
}

/// Peephole fast path for `GetLocal(dst); GetLocal(rhs); Constant(int);
/// Multiply; Add/Subtract; SetLocal(dst); [Pop]`, e.g. `total := total + i * 2`.
///
/// `constant` is `None` for the unscaled `GetLocal(dst); GetLocal(rhs);
/// Add/Subtract; SetLocal(dst)` shape, whose slow path does not multiply.
#[allow(clippy::too_many_arguments)]
fn emit_inline_local_scaled_arith_update(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    dst_slot: u16,
    rhs_slot: u16,
    constant: Option<i64>,
    op: IntArithOp,
    pop_after: bool,
    locs: PeepholeLocs,
) {
    use cranelift_codegen::ir::MemFlagsData;
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

    let flags = MemFlagsData::trusted();
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
    let scaled_rhs = match constant {
        Some(c) => {
            let scale = builder.ins().iconst(types::I64, c);
            builder.ins().imul(rhs_payload, scale)
        }
        None => rhs_payload,
    };
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
    if let Some(c) = constant {
        let scale = builder.ins().iconst(types::I64, c);
        emit_inline_push_integer(builder, vm_val, scale);

        emit_store_current_loc(builder, locs.frame_ptr, locs.mul.0, locs.mul.1);
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
    }
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
    emit_store_current_loc(builder, locs.frame_ptr, locs.arith.0, locs.arith.1);
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
    emit_slow_set_local_checked(builder, exit_block, refs, vm_val, dst_slot_val);
    if pop_after {
        builder.ins().call(refs.pop, &[vm_val]);
    }
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(cont_block);
}

/// Emits IR for a comparison opcode that has an inline int+int fast path. Both
/// top-of-stack tags are checked for `VALUE_TAG_INTEGER`; on the fast path the
/// two i64 payloads are compared with `cc` and a boolean is pushed via
/// `jit_replace_top2_with_bool`. Otherwise it falls through to `slow_helper`.
fn emit_int_fast_cmp(
    builder: &mut FunctionBuilder<'_>,
    exit_block: Block,
    _refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    cc: cranelift_codegen::ir::condcodes::IntCC,
    slow_helper: FuncRef,
) {
    use cranelift_codegen::ir::MemFlagsData;

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

    let flags = MemFlagsData::trusted();
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
    let cmp_u32 = builder.ins().uextend(types::I32, cmp);
    emit_inline_replace_top2_with_bool(builder, vm_val, cmp_u32);
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

fn emit_int_fast_eq(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    is_equal: bool,
) {
    use cranelift_codegen::ir::MemFlagsData;
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

    let flags = MemFlagsData::trusted();
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

/// Emits IR for a fused comparison + conditional-branch sequence. On the int+int
/// fast path the top two values are compared with `icmp` and branched on with a
/// single `brif`. On the slow path it uses the normal comparison helper plus the
/// truthy-check helper.
///
/// Both paths finish by branching to either `target_block` (jump taken) or
/// `fall_block` (jump not taken).
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
    use cranelift_codegen::ir::MemFlagsData;

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

    let flags = MemFlagsData::trusted();
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
            builder.ins().brif(cmp, fall_block, &[], target_block, &[]);
        }
        OpCode::JumpIfTrue => {
            builder.ins().brif(cmp, target_block, &[], fall_block, &[]);
        }
        _ => unreachable!(),
    }

    builder.switch_to_block(slow_block);
    let call = builder.ins().call(slow_helper, &[vm_val]);
    let cont_block = builder.create_block();
    if slow_helper_fallible {
        let status = builder.inst_results(call)[0];
        let err_block = builder.create_block();
        builder.ins().brif(status, err_block, &[], cont_block, &[]);
        builder.switch_to_block(err_block);
        let zero64 = builder.ins().iconst(types::I64, 0);
        builder
            .ins()
            .jump(exit_block, &[status.into(), zero64.into()]);
    } else {
        builder.ins().jump(cont_block, &[]);
    }

    builder.switch_to_block(cont_block);
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

/// True iff the Generic-entry Return can be inlined as a raw
/// `stack_view.len = slot_offset + 1` truncation, i.e. every slot in
/// `1..num_slots` is provably non-Rc at Return time:
///
/// * **Int64** (per `slot_types`): primitive, no Drop work.
/// * **Bottom**: never written on any path.
/// * **Value-typed param in `param_mirrors`**: the entry tag-guard proved its
///   runtime tag is Integer.
///
/// Any other slot may hold an Rc value, in which case the caller must use
/// `jit_op_return`.
///
/// Slot 0 is the closure marker (`Value::Closure(Rc<ObjClosure>)`); the inline
/// Return path emits an explicit Rc dec for it.
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
                if !param_mirrors.contains_key(&slot) {
                    return false;
                }
            }
        }
    }
    true
}

/// Emits the inline Generic-entry Return teardown:
///
/// 1. Source the result `Value` (16 bytes) from a virt-int SSA or from
///    `stack[len-1]`.
/// 2. Decrement the Rc strong count on the closure marker at
///    `stack[slot_offset]`. When `strong == 1` it falls through to
///    `jit_op_return` for the full Drop chain.
/// 3. Write the result Value at `stack[slot_offset]`.
/// 4. Set `stack_view.len = slot_offset + 1`.
/// 5. Decrement `jit_frame_view.len`.
/// 6. Exit through `exit_block` with `(0, 0)`.
///
/// The caller must have verified `return_slots_safe_to_truncate`.
fn emit_inline_generic_return(
    builder: &mut FunctionBuilder<'_>,
    exit_block: cranelift_codegen::ir::Block,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    virt_stack: &mut VirtStack,
) {
    use cranelift_codegen::ir::MemFlagsData;
    use cranelift_codegen::ir::condcodes::IntCC;

    let flags = MemFlagsData::trusted();

    let result_ssa: Option<cranelift_codegen::ir::Value> = virt_stack.pop_int_ssa();
    if !virt_stack.is_empty() {
        virt_stack.flush_to_memory(builder, vm_val);
    }

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);

    let (result_lo, result_hi) = if let Some(payload) = result_ssa {
        let tag_word = builder.ins().iconst(types::I64, VALUE_TAG_INTEGER as i64);
        (tag_word, payload)
    } else {
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

    let cm_byte_off = builder.ins().imul(slot_offset_val, value_size);
    let cm_addr = builder.ins().iadd(stack_ptr, cm_byte_off);

    let cm_rc = builder
        .ins()
        .load(types::I64, flags, cm_addr, VALUE_INT_PAYLOAD_OFFSET as i32);
    let strong = builder.ins().load(types::I64, flags, cm_rc, 0);

    let one_i64 = builder.ins().iconst(types::I64, 1);
    let strong_gt_1 = builder
        .ins()
        .icmp(IntCC::UnsignedGreaterThan, strong, one_i64);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    builder
        .ins()
        .brif(strong_gt_1, fast_block, &[], slow_block, &[]);

    builder.switch_to_block(slow_block);
    if result_ssa.is_some() {
        emit_inline_push_integer(builder, vm_val, result_hi);
    }
    builder.ins().call(refs.op_return, &[vm_val]);
    let zero32_s = builder.ins().iconst(types::I32, 0);
    let zero64_s = builder.ins().iconst(types::I64, 0);
    builder
        .ins()
        .jump(exit_block, &[zero32_s.into(), zero64_s.into()]);

    builder.switch_to_block(fast_block);
    let strong_dec = builder.ins().iadd_imm_s(strong, -1);
    builder.ins().store(flags, strong_dec, cm_rc, 0);

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
    use cranelift_codegen::ir::MemFlagsData;
    use cranelift_codegen::ir::condcodes::IntCC;

    let _ = refs;

    let stack_ptr = emit_load_stack_ptr(builder, vm_val);

    let slot_const = builder.ins().iconst(types::I64, slot as i64);
    let abs_slot = builder.ins().iadd(slot_offset_val, slot_const);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let byte_off = builder.ins().imul(abs_slot, value_size);
    let src_addr = builder.ins().iadd(stack_ptr, byte_off);

    let flags = MemFlagsData::trusted();
    let tag = builder.ins().load(types::I8, flags, src_addr, 0);

    let stack_len = emit_load_stack_len(builder, vm_val);
    let dst_off = builder.ins().imul(stack_len, value_size);
    let dst_addr = builder.ins().iadd(stack_ptr, dst_off);

    emit_copy_value(builder, src_addr, dst_addr);

    let six = builder.ins().iconst(types::I8, 6);
    let is_heap = builder.ins().icmp(IntCC::UnsignedGreaterThan, tag, six);

    let check_builtin_block = builder.create_block();
    let post_bump_block = builder.create_block();
    builder
        .ins()
        .brif(is_heap, check_builtin_block, &[], post_bump_block, &[]);

    builder.switch_to_block(check_builtin_block);
    let builtin_tag = builder.ins().iconst(types::I8, 13);
    let is_builtin = builder.ins().icmp(IntCC::Equal, tag, builtin_tag);
    let bump_block = builder.create_block();
    builder
        .ins()
        .brif(is_builtin, post_bump_block, &[], bump_block, &[]);

    builder.switch_to_block(bump_block);
    let rc_ptr = builder
        .ins()
        .load(types::I64, flags, src_addr, VALUE_INT_PAYLOAD_OFFSET as i32);
    let strong = builder.ins().load(types::I64, flags, rc_ptr, 0);
    let strong_new = builder.ins().iadd_imm_s(strong, 1);
    builder.ins().store(flags, strong_new, rc_ptr, 0);
    builder.ins().jump(post_bump_block, &[]);

    builder.switch_to_block(post_bump_block);
    let one = builder.ins().iconst(types::I64, 1);
    let new_len = builder.ins().iadd(stack_len, one);
    builder
        .ins()
        .store(flags, new_len, vm_val, vm_stack_view_len_offset());
}

/// Inline fast path for `SetLocal(slot)`. Clones the top-of-stack into the slot,
/// leaving the top intact. When both the new value and the existing slot value
/// have primitive tags (<= 6: None/Boolean/Byte/Char/Uint/Float/Integer), it
/// bitwise-copies `VALUE_SIZE` bytes from top to slot. Falls through to
/// `jit_set_local` when either tag is non-primitive.
fn emit_inline_set_local(
    builder: &mut FunctionBuilder<'_>,
    refs: &HelperRefs,
    vm_val: cranelift_codegen::ir::Value,
    slot_offset_val: cranelift_codegen::ir::Value,
    slot: u16,
) {
    use cranelift_codegen::ir::MemFlagsData;
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

    let flags = MemFlagsData::trusted();
    let top_tag = builder.ins().load(types::I8, flags, addr_top, 0);
    let old_tag = builder.ins().load(types::I8, flags, addr_slot, 0);
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

    builder.switch_to_block(fast_block);
    let words = VALUE_SIZE.div_ceil(8);
    for i in 0..words {
        let off = (i * 8) as i32;
        let v = builder.ins().load(types::I64, flags, addr_top, off);
        builder.ins().store(flags, v, addr_slot, off);
    }
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(slow_block);
    let slot_val = builder.ins().iconst(types::I32, slot as i64);
    builder.ins().call(refs.set_local, &[vm_val, slot_val]);
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(cont_block);
}
