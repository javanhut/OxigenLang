//! Cranelift-backed JIT engine internals. Only compiled when the `jit`
//! feature is enabled.
//!
//! Milestone 1, Steps 3+4: real bytecode translation for control-flow,
//! locals, and arithmetic. Every opcode becomes exactly one `call` to an
//! `extern "C"` runtime helper, plus any branching. Call/Return-chain
//! mechanics are still handled by the runtime helpers; no inline fast
//! paths yet.

use std::collections::HashMap;
use std::rc::Rc;

use cranelift_codegen::Context;
use cranelift_codegen::ir::{
    AbiParam, Block, FuncRef, InstBuilder, Signature, UserFuncName, types,
};
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module, default_libcall_names};

use crate::compiler::opcode::{Chunk, OpCode};
use crate::vm::VM;
use crate::vm::VMError;
use crate::vm::{JitFrame, JitFrameView, StackView};
use crate::vm::value::{
    Function, RC_VALUE_OFFSET, REF_CELL_VALUE_OFFSET, STRUCT_INSTANCE_DEF_OFFSET,
    STRUCT_INSTANCE_FIELDS_OFFSET, VALUE_INT_PAYLOAD_OFFSET, VALUE_SIZE, VALUE_TAG_CLOSURE,
    VALUE_TAG_FLOAT, VALUE_TAG_INTEGER, VALUE_TAG_STRUCT_INSTANCE,
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

use super::CompiledThunk;
use super::runtime;
use super::scan;

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

/// Per-MethodCall-site cache. Stores the struct def pointer (identity-
/// compared, held alive by the `Rc`) and the resolved method closure so
/// that a hit skips `find_struct_method`'s two HashMap lookups entirely.
#[repr(C)]
pub(crate) struct MethodCacheEntry {
    pub struct_def_raw: *const crate::vm::value::ObjStructDef,
    pub closure_raw: *const crate::vm::value::ObjClosure,
    pub thunk_raw: *const (),
    pub arity: u8,
    _pad: [u8; 7],
    pub struct_def: Option<std::rc::Rc<crate::vm::value::ObjStructDef>>,
    pub closure: Option<std::rc::Rc<crate::vm::value::ObjClosure>>,
}

impl MethodCacheEntry {
    pub(crate) const OFFSET_STRUCT_DEF_RAW: i32 = 0;
    pub(crate) const OFFSET_CLOSURE_RAW: i32 = 8;
    pub(crate) const OFFSET_THUNK_RAW: i32 = 16;
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

impl FieldCacheEntry {
    pub(crate) const OFFSET_STRUCT_DEF_RAW: i32 = 0;
    pub(crate) const OFFSET_FIELD_INDEX: i32 = 8;
    pub(crate) const OFFSET_KIND: i32 = 12;
    pub(crate) const OFFSET_CLOSURE_RAW: i32 = 16;
}

enum Entry {
    Compiled { thunk: CompiledThunk },
    Failed,
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
    replace_top2_with_bool: FuncId, // (vm, u32)
    current_slot_offset: FuncId,    // (vm) -> i64
}

/// FuncRefs for the current function's context — the in-function "import
/// table" of declared helpers.
struct HelperRefs {
    push_constant: FuncRef,
    push_integer_inline: FuncRef,
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
    stack_pop_one: FuncRef,
    stack_pop_n: FuncRef,
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
            global_caches: Vec::new(),
            call_caches: Vec::new(),
            method_caches: Vec::new(),
            field_caches: Vec::new(),
        }
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
            _pad: [0; 7],
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

    pub fn maybe_compile_thunk(&mut self, func: &Rc<Function>) -> Option<CompiledThunk> {
        let key = Rc::as_ptr(func);
        match self.compiled.get(&key) {
            Some(Entry::Compiled { thunk }) => return Some(*thunk),
            Some(Entry::Failed) => return None,
            None => {}
        }

        if scan::scan(&func.chunk).is_err() {
            self.compiled.insert(key, Entry::Failed);
            return None;
        }

        match self.compile_function(func) {
            Ok(thunk) => {
                self.retained.insert(key, Rc::clone(func));
                self.compiled.insert(key, Entry::Compiled { thunk });
                Some(thunk)
            }
            Err(_) => {
                self.compiled.insert(key, Entry::Failed);
                None
            }
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
            Some(Entry::Compiled { thunk }) => *thunk,
            _ => return InvokeOutcome::Bailout,
        };

        unsafe { self.invoke_thunk(vm, thunk, stop_depth) }
    }

    #[inline(always)]
    pub unsafe fn invoke_thunk(
        &mut self,
        vm: *mut VM,
        thunk: CompiledThunk,
        stop_depth: usize,
    ) -> InvokeOutcome {
        self.current_stop_depth = stop_depth;
        self.pending_error = None;

        let status = unsafe { thunk(vm) };

        self.current_stop_depth = 0;
        match status {
            0 => InvokeOutcome::Returned,
            1 => {
                let err = self.pending_error.take().unwrap_or_else(|| VMError {
                    message: "JIT runtime error with no stashed detail".to_string(),
                    line: 0,
                });
                InvokeOutcome::RuntimeError(err)
            }
            _ => InvokeOutcome::Bailout,
        }
    }

    fn compile_function(&mut self, func: &Rc<Function>) -> Result<CompiledThunk, ()> {
        let chunk = &func.chunk;
        let code = &chunk.code;

        let info = scan::scan(chunk).map_err(|_| ())?;

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

        // Thunk signature: fn(*mut VM) -> i32
        let ptr_ty = self.module.target_config().pointer_type();
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(ptr_ty));
        sig.returns.push(AbiParam::new(types::I32));

        self.next_id = self.next_id.wrapping_add(1);
        let thunk_name = format!("oxigen_jit_thunk_{}_{:p}", self.next_id, Rc::as_ptr(func));
        let thunk_id = self
            .module
            .declare_function(&thunk_name, Linkage::Local, &sig)
            .map_err(|_| ())?;

        self.ctx.func.signature = sig;
        self.ctx.func.name = UserFuncName::user(0, self.next_id);

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

            let mut current_block = entry_block;
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
                match op {
                    OpCode::Constant => {
                        let idx = read_u16(code, ip + 1);
                        match &chunk.constants[idx as usize] {
                            crate::vm::value::Value::Integer(n) => {
                                let v = builder.ins().iconst(types::I64, *n);
                                builder.ins().call(refs.push_integer_inline, &[vm_val, v]);
                            }
                            crate::vm::value::Value::Float(f) => {
                                let bits = f.to_bits() as i64;
                                let v = builder.ins().iconst(types::I64, bits);
                                builder.ins().call(refs.push_float_inline, &[vm_val, v]);
                            }
                            _ => {
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
                        builder.ins().call(refs.pop, &[vm_val]);
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
                        if let Some(m) =
                            match_struct_field_add_update(code, chunk, ip, &blocks)
                        {
                            let self_slot =
                                builder.ins().iconst(types::I32, m.self_slot as i64);
                            let field_idx =
                                builder.ins().iconst(types::I32, m.field_idx as i64);
                            let call = match m.shape {
                                StructFieldAddShape::Const { addend } => {
                                    let addend_val = builder.ins().iconst(types::I64, addend);
                                    builder.ins().call(
                                        refs.struct_field_add_const,
                                        &[vm_val, self_slot, field_idx, addend_val],
                                    )
                                }
                                StructFieldAddShape::Local { rhs_slot } => {
                                    let rhs_slot_val =
                                        builder.ins().iconst(types::I32, rhs_slot as i64);
                                    builder.ins().call(
                                        refs.struct_field_add_local,
                                        &[vm_val, self_slot, field_idx, rhs_slot_val],
                                    )
                                }
                            };
                            let status = builder.inst_results(call)[0];
                            emit_early_exit_on_err(&mut builder, status);
                            ip += m.len;
                        } else if let Some(m) =
                            match_local_array_mod_index_add_update(code, chunk, ip, &blocks)
                        {
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
                        } else if let Some(m) = match_local_arith_update(code, ip, &blocks) {
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
                        {
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
                        {
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
                            let slot = read_u16(code, ip + 1);
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
                        emit_inline_set_local(&mut builder, &refs, vm_val, slot_offset_val, slot);
                        ip += 3;
                    }

                    // Fallible arithmetic/comparison: call helper, return
                    // early with the helper's error status if non-zero.
                    //
                    // Add/Sub/Mul get an inline int+int fast path via a
                    // direct tag check on the stack — skips the full
                    // dispatch in `binary_add` etc. when both operands
                    // are `Value::Integer`.
                    OpCode::Add => {
                        emit_int_fast_arith(&mut builder, &refs, vm_val, IntArithOp::Add, refs.add);
                        ip += 1;
                    }
                    OpCode::Subtract => {
                        emit_int_fast_arith(&mut builder, &refs, vm_val, IntArithOp::Sub, refs.sub);
                        ip += 1;
                    }
                    OpCode::Multiply => {
                        emit_int_fast_arith(&mut builder, &refs, vm_val, IntArithOp::Mul, refs.mul);
                        ip += 1;
                    }
                    OpCode::Divide => {
                        emit_int_fast_divmod(&mut builder, &refs, vm_val, false, refs.div);
                        ip += 1;
                    }
                    OpCode::Modulo => {
                        emit_int_fast_divmod(&mut builder, &refs, vm_val, true, refs.modu);
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
                            terminated = true;
                            ip = branch_ip + 3;
                        } else {
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
                        let call = builder
                            .ins()
                            .call(refs.get_global_ic, &[vm_val, cache_val, idx_val]);
                        let status = builder.inst_results(call)[0];
                        emit_early_exit_on_err(&mut builder, status);
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
                        builder.ins().store(
                            flags,
                            curr_rc,
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
                        use cranelift_codegen::ir::MemFlags;
                        use cranelift_codegen::ir::condcodes::IntCC;
                        let idx_val = builder.ins().iconst(types::I32, idx as i64);
                        let cache_ptr = field_cache_ptrs[field_ic_ix];
                        field_ic_ix += 1;
                        let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);
                        let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                        let stack_len = emit_load_stack_len(&mut builder, vm_val);
                        let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                        let one = builder.ins().iconst(types::I64, 1);
                        let top_slot = builder.ins().isub(stack_len, one);
                        let top_off = builder.ins().imul(top_slot, value_size);
                        let top_ptr = builder.ins().iadd(stack_ptr, top_off);
                        let flags = MemFlags::trusted();
                        let check_def_block = builder.create_block();
                        let instance_field_block = builder.create_block();
                        let def_method_block = builder.create_block();
                        let write_method_block = builder.create_block();
                        let miss_block = builder.create_block();
                        let ok_block = builder.create_block();
                        let err_block = builder.create_block();
                        builder.append_block_param(err_block, types::I32);

                        let tag = builder.ins().load(types::I8, flags, top_ptr, 0);
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
                            top_ptr,
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
                            FieldCacheEntry::OFFSET_STRUCT_DEF_RAW,
                        );
                        let def_matches =
                            builder.ins().icmp(IntCC::Equal, inst_def_raw, cached_def_raw);
                        let kind = builder.ins().load(
                            types::I8,
                            flags,
                            cache_val,
                            FieldCacheEntry::OFFSET_KIND,
                        );
                        let kind_instance = builder
                            .ins()
                            .iconst(types::I8, FieldCacheKind::InstanceField as i64);
                        let kind_method = builder
                            .ins()
                            .iconst(types::I8, FieldCacheKind::DefMethod as i64);
                        let kind_is_instance =
                            builder.ins().icmp(IntCC::Equal, kind, kind_instance);
                        let kind_is_method =
                            builder.ins().icmp(IntCC::Equal, kind, kind_method);
                        let kind_block = builder.create_block();
                        builder
                            .ins()
                            .brif(def_matches, kind_block, &[], miss_block, &[]);
                        builder.switch_to_block(kind_block);
                        builder.ins().brif(
                            kind_is_instance,
                            instance_field_block,
                            &[],
                            def_method_block,
                            &[],
                        );

                        builder.switch_to_block(instance_field_block);
                        let field_index = builder.ins().load(
                            types::I32,
                            flags,
                            cache_val,
                            FieldCacheEntry::OFFSET_FIELD_INDEX,
                        );
                        let fields_ptr = builder
                            .ins()
                            .iadd_imm(inst_ptr, STRUCT_INSTANCE_FIELDS_OFFSET as i64);
                        let vec_ptr = builder
                            .ins()
                            .load(ptr_ty, flags, fields_ptr, REF_CELL_VALUE_OFFSET as i32);
                        let field_index64 = builder.ins().uextend(types::I64, field_index);
                        let field_off = builder.ins().imul(field_index64, value_size);
                        let field_ptr = builder.ins().iadd(vec_ptr, field_off);
                        emit_copy_value(&mut builder, field_ptr, top_ptr);
                        builder.ins().jump(ok_block, &[]);

                        builder.switch_to_block(def_method_block);
                        builder.ins().brif(
                            kind_is_method,
                            write_method_block,
                            &[],
                            miss_block,
                            &[],
                        );
                        builder.switch_to_block(write_method_block);
                        let closure_raw = builder.ins().load(
                            ptr_ty,
                            flags,
                            cache_val,
                            FieldCacheEntry::OFFSET_CLOSURE_RAW,
                        );
                        emit_write_closure_value(&mut builder, top_ptr, closure_raw);
                        builder.ins().jump(ok_block, &[]);

                        builder.switch_to_block(miss_block);
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
                        use cranelift_codegen::ir::MemFlags;
                        use cranelift_codegen::ir::condcodes::IntCC;
                        let idx_val = builder.ins().iconst(types::I32, idx as i64);
                        let cache_ptr = field_cache_ptrs[field_ic_ix];
                        field_ic_ix += 1;
                        let cache_val = builder.ins().iconst(ptr_ty, cache_ptr as i64);
                        let stack_ptr = emit_load_stack_ptr(&mut builder, vm_val);
                        let stack_len = emit_load_stack_len(&mut builder, vm_val);
                        let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
                        let one = builder.ins().iconst(types::I64, 1);
                        let two64 = builder.ins().iconst(types::I64, 2);
                        let obj_slot = builder.ins().isub(stack_len, two64);
                        let value_slot = builder.ins().isub(stack_len, one);
                        let obj_off = builder.ins().imul(obj_slot, value_size);
                        let value_off = builder.ins().imul(value_slot, value_size);
                        let obj_ptr = builder.ins().iadd(stack_ptr, obj_off);
                        let value_ptr = builder.ins().iadd(stack_ptr, value_off);
                        let flags = MemFlags::trusted();
                        let check_def_block = builder.create_block();
                        let hit_block = builder.create_block();
                        let miss_block = builder.create_block();
                        let ok_block = builder.create_block();
                        let err_block = builder.create_block();
                        builder.append_block_param(err_block, types::I32);
                        let tag = builder.ins().load(types::I8, flags, obj_ptr, 0);
                        let struct_tag =
                            builder.ins().iconst(types::I8, VALUE_TAG_STRUCT_INSTANCE as i64);
                        let is_struct =
                            builder.ins().icmp(IntCC::Equal, tag, struct_tag);
                        builder
                            .ins()
                            .brif(is_struct, check_def_block, &[], miss_block, &[]);
                        builder.switch_to_block(check_def_block);
                        let obj_raw = builder.ins().load(
                            ptr_ty,
                            flags,
                            obj_ptr,
                            VALUE_INT_PAYLOAD_OFFSET as i32,
                        );
                        let inst_ptr = builder.ins().iadd_imm(obj_raw, RC_VALUE_OFFSET as i64);
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
                            FieldCacheEntry::OFFSET_STRUCT_DEF_RAW,
                        );
                        let def_matches =
                            builder.ins().icmp(IntCC::Equal, inst_def_raw, cached_def_raw);
                        let kind = builder.ins().load(
                            types::I8,
                            flags,
                            cache_val,
                            FieldCacheEntry::OFFSET_KIND,
                        );
                        let kind_instance = builder
                            .ins()
                            .iconst(types::I8, FieldCacheKind::InstanceField as i64);
                        let kind_matches =
                            builder.ins().icmp(IntCC::Equal, kind, kind_instance);
                        let kind_block = builder.create_block();
                        builder
                            .ins()
                            .brif(def_matches, kind_block, &[], miss_block, &[]);
                        builder.switch_to_block(kind_block);
                        builder
                            .ins()
                            .brif(kind_matches, hit_block, &[], miss_block, &[]);
                        builder.switch_to_block(hit_block);
                        let field_index = builder.ins().load(
                            types::I32,
                            flags,
                            cache_val,
                            FieldCacheEntry::OFFSET_FIELD_INDEX,
                        );
                        let fields_ptr = builder
                            .ins()
                            .iadd_imm(inst_ptr, STRUCT_INSTANCE_FIELDS_OFFSET as i64);
                        let vec_ptr = builder
                            .ins()
                            .load(ptr_ty, flags, fields_ptr, REF_CELL_VALUE_OFFSET as i32);
                        let field_index64 = builder.ins().uextend(types::I64, field_index);
                        let field_off = builder.ins().imul(field_index64, value_size);
                        let field_ptr = builder.ins().iadd(vec_ptr, field_off);
                        emit_copy_value(&mut builder, value_ptr, field_ptr);
                        let two = builder.ins().iconst(types::I32, 2);
                        builder.ins().call(refs.stack_pop_n, &[vm_val, two]);
                        builder.ins().jump(ok_block, &[]);

                        builder.switch_to_block(miss_block);
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
                            builder
                                .ins()
                                .brif(def_matches, hit_block, &[], miss_block, &[]);

                            builder.switch_to_block(hit_block);
                            let closure_raw = builder.ins().load(
                                ptr_ty,
                                flags,
                                cache_val,
                                MethodCacheEntry::OFFSET_CLOSURE_RAW,
                            );
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
                            emit_write_closure_value(&mut builder, receiver_ptr, closure_raw);
                            let new_stack_len = builder.ins().iadd_imm(stack_len, 1);
                            builder.ins().store(
                                flags,
                                new_stack_len,
                                vm_val,
                                vm_stack_view_len_offset(),
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
                            builder.ins().store(
                                flags,
                                closure_raw,
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
        self.module.finalize_definitions().map_err(|_| ())?;

        let raw_ptr = self.module.get_finalized_function(thunk_id);
        let thunk: CompiledThunk = unsafe { std::mem::transmute(raw_ptr) };
        Ok(thunk)
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

    let mut sig_vm_u32_u32_i64_to_u32 = module.make_signature();
    sig_vm_u32_u32_i64_to_u32
        .params
        .push(AbiParam::new(ptr_ty));
    sig_vm_u32_u32_i64_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_u32_u32_i64_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_u32_u32_i64_to_u32
        .params
        .push(AbiParam::new(types::I64));
    sig_vm_u32_u32_i64_to_u32
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
            &sig_vm_u32_u32_i64_to_u32,
        ),
        struct_field_add_local: decl(module, "jit_struct_field_add_local", &sig_vm_3u32_to_u32),
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

enum StructFieldAddShape {
    Const { addend: i64 },
    Local { rhs_slot: u16 },
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
    if field_idx != read_u16(code, set_field_ip + 1) {
        return None;
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
    // Drop the now-duplicate top slot. Integer has no Drop so the call
    // is cheap.
    builder.ins().call(refs.stack_pop_one, &[vm_val]);
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
    builder.ins().call(refs.stack_pop_one, &[vm_val]);
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
        builder
            .ins()
            .call(refs.push_integer_inline, &[vm_val, result]);
    }
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(slow_block);
    let slot_val = builder.ins().iconst(types::I32, slot as i64);
    builder.ins().call(refs.get_local, &[vm_val, slot_val]);
    let rhs = builder.ins().iconst(types::I64, constant);
    builder.ins().call(refs.push_integer_inline, &[vm_val, rhs]);
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
        builder
            .ins()
            .call(refs.push_integer_inline, &[vm_val, result]);
    }
    builder.ins().jump(cont_block, &[]);

    builder.switch_to_block(slow_block);
    let dst_slot_val = builder.ins().iconst(types::I32, dst_slot as i64);
    let rhs_slot_val = builder.ins().iconst(types::I32, rhs_slot as i64);
    builder.ins().call(refs.get_local, &[vm_val, dst_slot_val]);
    builder.ins().call(refs.get_local, &[vm_val, rhs_slot_val]);
    let scale = builder.ins().iconst(types::I64, constant);
    builder
        .ins()
        .call(refs.push_integer_inline, &[vm_val, scale]);

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
    builder
        .ins()
        .call(refs.replace_top2_with_bool, &[vm_val, cmp_u32]);
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
    builder
        .ins()
        .call(refs.replace_top2_with_bool, &[vm_val, cmp_u32]);
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
            builder
                .ins()
                .call(refs.replace_top2_with_bool, &[vm_val, cmp_u32]);
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
    builder
        .ins()
        .call(refs.push_integer_inline, &[vm_val, payload]);
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
    builder.ins().call(refs.push_float_inline, &[vm_val, bits]);
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
