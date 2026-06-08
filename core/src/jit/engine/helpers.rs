//! Cranelift helper-function tables: `HelperIds` (per-module FuncIds),
//! `HelperRefs` (per-function FuncRefs), and the bootstrap routines
//! that register and declare them.
//!
//! Pulled out of the original monolithic `engine.rs`. Both struct
//! definitions are large field-by-field listings of every `extern "C"`
//! runtime helper the JIT might call; co-locating them with their
//! registration / declaration code makes the helper plumbing a single
//! browseable file.

#![cfg(feature = "jit")]

use cranelift_codegen::ir::{AbiParam, FuncRef, Signature, types};
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use super::super::runtime;

/// Per-`JITModule` table of FuncIds for every runtime helper the
/// translator might call. Created once at engine init via
/// `declare_helpers`; converted to a per-function `HelperRefs` table
/// inside each `compile_function` invocation via `declare_helper_refs`.
pub(super) struct HelperIds {
    // Push
    pub push_constant: FuncId,       // (vm, u32)
    pub push_integer_inline: FuncId, // (vm, i64)
    pub push_float_inline: FuncId,   // (vm, i64 bits-of-f64)
    // Stack manip
    pub pop: FuncId, // (vm)
    pub dup: FuncId, // (vm)

    // Collections
    pub build_array: FuncId,               // (vm, u32)
    pub index_fast_array_int: FuncId,      // (vm) -> u32
    pub type_wrap: FuncId,                 // (vm, u32) -> u32
    pub local_add_array_mod_index: FuncId, // (vm, u32, u32, u32, i64, u32) -> u32
    pub struct_field_add_const: FuncId,    // (vm, u32, u32, i64) -> u32
    pub struct_field_add_local: FuncId,    // (vm, u32, u32, u32) -> u32

    // Locals
    pub get_local: FuncId, // (vm, u32)
    pub set_local: FuncId, // (vm, u32)

    // Arithmetic (fallible)
    pub add: FuncId,
    pub sub: FuncId,
    pub mul: FuncId,
    pub div: FuncId,
    pub modu: FuncId,

    // Comparison
    pub eq: FuncId, // infallible
    pub ne: FuncId, // infallible
    pub lt: FuncId,
    pub le: FuncId,
    pub gt: FuncId,
    pub ge: FuncId,

    // Logical / unary
    pub not: FuncId,    // infallible
    pub negate: FuncId, // fallible

    // Bitwise (all fallible: type-error on non-int operands)
    pub op_band: FuncId,
    pub op_bor: FuncId,
    pub op_bxor: FuncId,
    pub op_bnot: FuncId,
    pub op_shl: FuncId,
    pub op_shr: FuncId,

    // Logging
    pub op_log: FuncId, // (vm, u32 flags) -> u32

    // B2.2.f debug invariant helper: (vm, *cache, *closure, phase, slot_offset, jit_frame_len, arg_count) -> u32
    pub dbg_check_spec_call: FuncId,

    // Branching truthy checks
    pub peek_truthy: FuncId, // (vm) -> u32
    pub pop_truthy: FuncId,  // (vm) -> u32

    // Return
    pub op_return: FuncId, // (vm)

    // Call (fallible, takes arg_count)
    pub op_call: FuncId,      // (vm, u32) -> u32
    pub op_call_hit: FuncId,  // (vm, *mut CallCacheEntry) -> u32
    pub op_call_miss: FuncId, // (vm, u32, *mut CallCacheEntry) -> u32

    // Globals
    pub get_global: FuncId,          // (vm, u32) -> u32
    pub get_global_ic: FuncId,       // (vm, *mut GlobalCacheEntry, u32) -> u32
    pub set_global: FuncId,          // (vm, u32) -> u32
    pub define_global: FuncId,       // (vm, u32) -> u32
    pub define_global_typed: FuncId, // (vm, u32, u32, u32) -> u32

    // Upvalues
    pub get_upvalue: FuncId,   // (vm, u32)
    pub set_upvalue: FuncId,   // (vm, u32)
    pub close_upvalue: FuncId, // (vm)

    // Closure
    pub op_closure: FuncId, // (vm, u32, u32) -> u32

    // Struct ops
    pub op_struct_def: FuncId,        // (vm, u32)
    pub op_struct_literal: FuncId,    // (vm, u32, u32) -> u32
    pub op_get_field_ic_miss: FuncId, // (vm, u32, *mut FieldCacheEntry) -> u32
    pub op_set_field_ic_miss: FuncId, // (vm, u32, *mut FieldCacheEntry) -> u32
    pub op_define_method: FuncId,     // (vm, u32, u32) -> u32
    pub op_method_call: FuncId,       // (vm, u32, u32) -> u32
    pub op_method_call_ic: FuncId,    // (vm, u32, u32, *mut MethodCacheEntry) -> u32

    // Inline int fast-path support
    pub stack_as_mut_ptr: FuncId, // (vm) -> *mut Value (pointer-sized)
    pub stack_len: FuncId,        // (vm) -> i64
    pub stack_pop_one: FuncId,    // (vm)
    pub stack_pop_n: FuncId,      // (vm, u32)
    pub stack_commit_len: FuncId, // (vm, u64)
    pub stack_truncate: FuncId,   // (vm, i64)
    pub replace_top2_with_bool: FuncId, // (vm, u32)
    pub current_slot_offset: FuncId, // (vm) -> i64
}

/// FuncRefs for the current function's context — the in-function
/// "import table" of declared helpers.
pub(super) struct HelperRefs {
    pub push_constant: FuncRef,
    #[allow(dead_code)]
    pub push_integer_inline: FuncRef,
    #[allow(dead_code)]
    pub push_float_inline: FuncRef,
    pub pop: FuncRef,
    pub dup: FuncRef,
    pub build_array: FuncRef,
    pub index_fast_array_int: FuncRef,
    pub type_wrap: FuncRef,
    pub local_add_array_mod_index: FuncRef,
    pub struct_field_add_const: FuncRef,
    pub struct_field_add_local: FuncRef,
    pub get_local: FuncRef,
    pub set_local: FuncRef,
    pub add: FuncRef,
    pub sub: FuncRef,
    pub mul: FuncRef,
    pub div: FuncRef,
    pub modu: FuncRef,
    pub eq: FuncRef,
    pub ne: FuncRef,
    pub lt: FuncRef,
    pub le: FuncRef,
    pub gt: FuncRef,
    pub ge: FuncRef,
    pub not: FuncRef,
    pub negate: FuncRef,
    pub op_band: FuncRef,
    pub op_bor: FuncRef,
    pub op_bxor: FuncRef,
    pub op_bnot: FuncRef,
    pub op_shl: FuncRef,
    pub op_shr: FuncRef,
    pub op_log: FuncRef,
    pub dbg_check_spec_call: FuncRef,
    pub peek_truthy: FuncRef,
    pub pop_truthy: FuncRef,
    pub op_return: FuncRef,
    #[allow(dead_code)]
    pub op_call: FuncRef,
    #[allow(dead_code)]
    pub op_call_hit: FuncRef,
    pub op_call_miss: FuncRef,
    #[allow(dead_code)]
    pub get_global: FuncRef,
    pub get_global_ic: FuncRef,
    pub set_global: FuncRef,
    pub define_global: FuncRef,
    pub define_global_typed: FuncRef,
    pub get_upvalue: FuncRef,
    pub set_upvalue: FuncRef,
    pub close_upvalue: FuncRef,
    pub op_closure: FuncRef,
    pub op_struct_def: FuncRef,
    pub op_struct_literal: FuncRef,
    pub op_get_field_ic_miss: FuncRef,
    pub op_set_field_ic_miss: FuncRef,
    pub op_define_method: FuncRef,
    #[allow(dead_code)]
    pub op_method_call: FuncRef,
    pub op_method_call_ic: FuncRef,
    // `stack_as_mut_ptr` and `stack_len` are still registered as
    // runtime helpers for backwards compatibility, but the hot JIT
    // paths now read `vm.stack_view.{ptr, len}` directly via
    // `emit_load_stack_*` — no FFI crossing.
    #[allow(dead_code)]
    pub stack_as_mut_ptr: FuncRef,
    #[allow(dead_code)]
    pub stack_len: FuncRef,
    #[allow(dead_code)]
    pub stack_pop_one: FuncRef,
    pub stack_pop_n: FuncRef,
    pub stack_commit_len: FuncRef,
    pub stack_truncate: FuncRef,
    pub replace_top2_with_bool: FuncRef,
    #[allow(dead_code)]
    pub current_slot_offset: FuncRef,
}

pub(super) fn register_helpers(builder: &mut JITBuilder) {
    macro_rules! reg {
        ($name:literal, $fn:path) => {
            builder.symbol($name, $fn as *const u8);
        };
    }

    reg!("jit_run_via_interpreter", runtime::jit_run_via_interpreter);
    reg!("jit_push_constant", runtime::jit_push_constant);
    reg!("jit_push_integer_inline", runtime::jit_push_integer_inline);
    reg!("jit_push_float_inline", runtime::jit_push_float_inline);
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
    reg!("jit_op_band", runtime::jit_op_band);
    reg!("jit_op_bor", runtime::jit_op_bor);
    reg!("jit_op_bxor", runtime::jit_op_bxor);
    reg!("jit_op_bnot", runtime::jit_op_bnot);
    reg!("jit_op_shl", runtime::jit_op_shl);
    reg!("jit_op_shr", runtime::jit_op_shr);
    reg!("jit_op_log", runtime::jit_op_log);
    reg!("jit_dbg_check_spec_call", runtime::jit_dbg_check_spec_call);
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
    reg!(
        "jit_op_get_field_ic_miss",
        runtime::jit_op_get_field_ic_miss
    );
    reg!(
        "jit_op_set_field_ic_miss",
        runtime::jit_op_set_field_ic_miss
    );
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

pub(super) fn declare_helpers(module: &mut JITModule) -> HelperIds {
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

    let mut sig_vm_u32_to_u32 = module.make_signature();
    sig_vm_u32_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_u32_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_u32_to_u32.returns.push(AbiParam::new(types::I32));

    // B2.2.f debug helper: (vm, cache_ptr, closure_ptr, phase: u32,
    // expected_slot_offset: i64, expected_jit_frame_len: i64,
    // arg_count: u32) -> u32
    let mut sig_dbg_spec_call = module.make_signature();
    sig_dbg_spec_call.params.push(AbiParam::new(ptr_ty));
    sig_dbg_spec_call.params.push(AbiParam::new(ptr_ty));
    sig_dbg_spec_call.params.push(AbiParam::new(ptr_ty));
    sig_dbg_spec_call.params.push(AbiParam::new(types::I32));
    sig_dbg_spec_call.params.push(AbiParam::new(types::I64));
    sig_dbg_spec_call.params.push(AbiParam::new(types::I64));
    sig_dbg_spec_call.params.push(AbiParam::new(types::I32));
    sig_dbg_spec_call.returns.push(AbiParam::new(types::I32));

    let mut sig_vm_u32_u32_to_u32 = module.make_signature();
    sig_vm_u32_u32_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_u32_u32_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_u32_u32_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_u32_u32_to_u32
        .returns
        .push(AbiParam::new(types::I32));

    let mut sig_vm_3u32_to_u32 = module.make_signature();
    sig_vm_3u32_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_3u32_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_3u32_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_3u32_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_3u32_to_u32.returns.push(AbiParam::new(types::I32));

    // jit_struct_field_add_const: fn(*mut VM, u32, u32, i64, *mut FieldCacheEntry) -> u32
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

    // jit_struct_field_add_local: fn(*mut VM, u32, u32, u32, *mut FieldCacheEntry) -> u32
    let mut sig_vm_3u32_ptr_to_u32 = module.make_signature();
    sig_vm_3u32_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_3u32_ptr_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_3u32_ptr_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_3u32_ptr_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_3u32_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_3u32_ptr_to_u32
        .returns
        .push(AbiParam::new(types::I32));

    // jit_op_method_call_ic: fn(*mut VM, u32, u32, *mut MethodCacheEntry) -> u32
    let mut sig_vm_u32_u32_ptr_to_u32 = module.make_signature();
    sig_vm_u32_u32_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_u32_u32_ptr_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_u32_u32_ptr_to_u32
        .params
        .push(AbiParam::new(types::I32));
    sig_vm_u32_u32_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_u32_u32_ptr_to_u32
        .returns
        .push(AbiParam::new(types::I32));

    // jit_local_add_array_mod_index: fn(*mut VM, u32, u32, u32, i64, u32) -> u32
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

    let sig_vm_u32_fallible = &sig_vm_u32_to_u32;

    let mut sig_vm_to_ptr = module.make_signature();
    sig_vm_to_ptr.params.push(AbiParam::new(ptr_ty));
    sig_vm_to_ptr.returns.push(AbiParam::new(ptr_ty));

    let mut sig_vm_to_i64 = module.make_signature();
    sig_vm_to_i64.params.push(AbiParam::new(ptr_ty));
    sig_vm_to_i64.returns.push(AbiParam::new(types::I64));

    // jit_op_call_hit: fn(*mut VM, *mut CallCacheEntry) -> u32
    let mut sig_vm_ptr_to_u32 = module.make_signature();
    sig_vm_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_ptr_to_u32.returns.push(AbiParam::new(types::I32));

    // jit_op_call_miss: fn(*mut VM, u32, *mut CallCacheEntry) -> u32
    let mut sig_vm_u32_ptr_to_u32 = module.make_signature();
    sig_vm_u32_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_u32_ptr_to_u32.params.push(AbiParam::new(types::I32));
    sig_vm_u32_ptr_to_u32.params.push(AbiParam::new(ptr_ty));
    sig_vm_u32_ptr_to_u32
        .returns
        .push(AbiParam::new(types::I32));

    // jit_get_global_ic: fn(*mut VM, *mut GlobalCacheEntry, u32) -> u32
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
        op_band: decl(module, "jit_op_band", &sig_vm_to_u32),
        op_bor: decl(module, "jit_op_bor", &sig_vm_to_u32),
        op_bxor: decl(module, "jit_op_bxor", &sig_vm_to_u32),
        op_bnot: decl(module, "jit_op_bnot", &sig_vm_to_u32),
        op_shl: decl(module, "jit_op_shl", &sig_vm_to_u32),
        op_shr: decl(module, "jit_op_shr", &sig_vm_to_u32),
        op_log: decl(module, "jit_op_log", &sig_vm_u32_to_u32),
        dbg_check_spec_call: decl(module, "jit_dbg_check_spec_call", &sig_dbg_spec_call),
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
        op_method_call_ic: decl(module, "jit_op_method_call_ic", &sig_vm_u32_u32_ptr_to_u32),
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

pub(super) fn declare_helper_refs(
    ids: &HelperIds,
    module: &mut JITModule,
    builder: &mut FunctionBuilder<'_>,
) -> HelperRefs {
    let mut r = |id| module.declare_func_in_func(id, builder.func);
    HelperRefs {
        push_constant: r(ids.push_constant),
        push_integer_inline: r(ids.push_integer_inline),
        push_float_inline: r(ids.push_float_inline),
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
        op_band: r(ids.op_band),
        op_bor: r(ids.op_bor),
        op_bxor: r(ids.op_bxor),
        op_bnot: r(ids.op_bnot),
        op_shl: r(ids.op_shl),
        op_shr: r(ids.op_shr),
        op_log: r(ids.op_log),
        dbg_check_spec_call: r(ids.dbg_check_spec_call),
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
