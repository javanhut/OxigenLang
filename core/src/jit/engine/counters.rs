//! JIT instrumentation counters and the IR-side `emit_counter_bump` helper
//! used to bump them from emitted code.

#![cfg(feature = "jit")]

use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir::types;
use cranelift_frontend::FunctionBuilder;

/// Discriminant for the per-helper call counter array on `JitCounters`. One
/// slot per `extern "C"` runtime helper in `runtime.rs`, bumped from the
/// helper's entry. `HELPER_NAMES` follows this enum's order.
#[repr(usize)]
#[derive(Copy, Clone, Debug)]
pub(crate) enum HelperCounter {
    PushConstant = 0,
    PushIntegerInline,
    PushFloatInline,
    Pop,
    Dup,
    BuildArray,
    IndexFastArrayInt,
    TypeWrap,
    LocalAddArrayModIndex,
    StructFieldAddConst,
    StructFieldAddLocal,
    GetLocal,
    SetLocal,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Not,
    Negate,
    PeekTruthy,
    PopTruthy,
    OpReturn,
    OpCall,
    OpCallHit,
    OpCallMiss,
    GetGlobal,
    GetGlobalIc,
    SetGlobal,
    DefineGlobal,
    DefineGlobalTyped,
    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,
    OpClosure,
    OpStructDef,
    OpStructLiteral,
    OpGetField,
    OpSetField,
    OpGetFieldIcMiss,
    OpSetFieldIcMiss,
    OpDefineMethod,
    OpMethodCall,
    OpMethodCallIc,
    StackAsMutPtr,
    StackLen,
    StackPopOne,
    StackPopN,
    StackCommitLen,
    StackTruncate,
    ReplaceTop2WithBool,
    CurrentSlotOffset,
    OpBand,
    OpBor,
    OpBxor,
    OpBnot,
    OpShl,
    OpShr,
    OpLog,
    DbgCheckSpecCall,
    RunViaInterpreter,
}

impl HelperCounter {
    pub(crate) const COUNT: usize = HelperCounter::RunViaInterpreter as usize + 1;
}

pub(crate) const HELPER_NAMES: [&str; HelperCounter::COUNT] = [
    "push_constant",
    "push_integer_inline",
    "push_float_inline",
    "pop",
    "dup",
    "build_array",
    "index_fast_array_int",
    "type_wrap",
    "local_add_array_mod_index",
    "struct_field_add_const",
    "struct_field_add_local",
    "get_local",
    "set_local",
    "add",
    "sub",
    "mul",
    "div",
    "mod",
    "eq",
    "ne",
    "lt",
    "le",
    "gt",
    "ge",
    "not",
    "negate",
    "peek_truthy",
    "pop_truthy",
    "op_return",
    "op_call",
    "op_call_hit",
    "op_call_miss",
    "get_global",
    "get_global_ic",
    "set_global",
    "define_global",
    "define_global_typed",
    "get_upvalue",
    "set_upvalue",
    "close_upvalue",
    "op_closure",
    "op_struct_def",
    "op_struct_literal",
    "op_get_field",
    "op_set_field",
    "op_get_field_ic_miss",
    "op_set_field_ic_miss",
    "op_define_method",
    "op_method_call",
    "op_method_call_ic",
    "stack_as_mut_ptr",
    "stack_len",
    "stack_pop_one",
    "stack_pop_n",
    "stack_commit_len",
    "stack_truncate",
    "replace_top2_with_bool",
    "current_slot_offset",
    "op_band",
    "op_bor",
    "op_bxor",
    "op_bnot",
    "op_shl",
    "op_shr",
    "op_log",
    "dbg_check_spec_call",
    "run_via_interpreter",
];

/// Lightweight JIT instrumentation counters. Each field is a `Cell<u64>`. On
/// process exit, `JitInner::drop` dumps the counters to stderr when the env
/// var `OXIGEN_JIT_STATS` is set.
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
    /// Incremented every time the signed-div-by-power-of-two peephole replaces
    /// a Cranelift `sdiv x, pow2` with the bias-and-shift sequence.
    pub virt_div_pow2_lowered: std::cell::Cell<u64>,
    /// Incremented every time the fused `Equal` / `NotEqual` + `JumpIf*` path
    /// fires with both operands staged as virt Ints.
    pub virt_branch_eq_hit: std::cell::Cell<u64>,
    /// Incremented when the parity branch peephole fires: `(x % 2) == 0` /
    /// `!= 0` (and the commuted forms) lowered to `band_imm + icmp_imm + brif`.
    pub virt_branch_parity_hit: std::cell::Cell<u64>,
    /// Raw count of `jit_get_upvalue` FFI crossings.
    pub get_upvalue_calls: std::cell::Cell<u64>,
    /// Subset of `get_upvalue_calls` where the upvalue was
    /// `Closed(Value::Integer(_))`.
    pub get_upvalue_closed_integer_hit: std::cell::Cell<u64>,
    /// Subset of `get_upvalue_calls` that were Open, or Closed non-Integer.
    pub get_upvalue_fallback: std::cell::Cell<u64>,
    /// Raw count of `jit_set_upvalue` FFI crossings.
    pub set_upvalue_calls: std::cell::Cell<u64>,
    /// Raw count of `jit_op_return` FFI crossings.
    pub jit_op_return_calls: std::cell::Cell<u64>,
    /// Inline Call IC hits (cache matches expected closure). Bumped from
    /// emitted IR at the hit-block entry.
    pub call_ic_hit: std::cell::Cell<u64>,
    /// Inline Call IC misses (cache miss or non-closure tag). Bumped from
    /// emitted IR at the miss-block entry.
    pub call_ic_miss: std::cell::Cell<u64>,
    /// Incremented when the inline GetUpvalue closed-integer fast path fires:
    /// the cached i64 is loaded directly from `ObjClosure.upvalue_int_values`
    /// and pushed as a `Value::Integer`. Bumped from IR at the fast-block
    /// entry.
    pub get_upvalue_inline_hit: std::cell::Cell<u64>,
    /// Subset of `call_ic_hit` where the callee closure has
    /// `specialized_kind == NATIVE_INT_BODY`. Bumped from IR inside the IC hit
    /// block; does not change behavior.
    pub ic_callee_has_spec_entry: std::cell::Cell<u64>,
    /// Subset of `call_ic_hit` where the callee closure has
    /// `specialized_kind == NATIVE_INT_BODY_WITH_CLOSURE`. Bumped at the
    /// runtime check site, before the dispatch commits.
    pub ic_callee_has_closure_aware_spec: std::cell::Cell<u64>,
    /// Closure-aware specialized calls dispatched: args and closure pointer
    /// passed in registers. Bumped from emitted IR at the call_indirect site on
    /// the success branch. Subset of `ic_callee_has_closure_aware_spec`.
    pub closure_aware_call_dispatch: std::cell::Cell<u64>,

    /// Phase 1: calls whose callee body was evaluated inline at the call site —
    /// no JitFrame, no operand-stack argument, no dispatch.
    pub closure_inline_hit: std::cell::Cell<u64>,
    /// Calls where the cache recorded an inlinable body but a runtime guard
    /// (non-integer argument, or an upvalue not cached as a closed integer)
    /// sent it down the normal path anyway.
    pub closure_inline_guard_miss: std::cell::Cell<u64>,

    /// Per-helper FFI call counts, indexed by `HelperCounter` and bumped from
    /// each helper's entry.
    pub helper_calls: [std::cell::Cell<u64>; HelperCounter::COUNT],

    /// Count of functions the spec-entry eligibility analysis returned
    /// `Eligible` for.
    pub spec_entry_eligible: std::cell::Cell<u64>,
    /// Rejected because the function takes 0 args.
    pub spec_entry_rejected_zero_arity: std::cell::Cell<u64>,
    /// Rejected because at least one param wasn't Int64-typed and wasn't an
    /// int-mirror candidate.
    pub spec_entry_rejected_param_not_int: std::cell::Cell<u64>,
    /// Rejected because at least one param was captured by an upvalue.
    pub spec_entry_rejected_param_captured: std::cell::Cell<u64>,
    /// Rejected because the body contains a `Closure` op.
    pub spec_entry_rejected_has_closure_op: std::cell::Cell<u64>,
    /// Rejected because the body contains `GetUpvalue` / `SetUpvalue` /
    /// `CloseUpvalue`.
    pub spec_entry_rejected_has_upvalue_op: std::cell::Cell<u64>,
    /// Rejected because the function has no Return.
    pub spec_entry_rejected_no_return: std::cell::Cell<u64>,
    /// Rejected because the function has no Call.
    pub spec_entry_rejected_no_call: std::cell::Cell<u64>,
    /// Rejected because at least one Return IP was unreachable per the abstract
    /// interpretation.
    pub spec_entry_rejected_return_unreachable: std::cell::Cell<u64>,
}

impl JitCounters {
    pub(super) fn new() -> Self {
        let helper_calls: [std::cell::Cell<u64>; HelperCounter::COUNT] =
            std::array::from_fn(|_| std::cell::Cell::new(0));
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
            virt_div_pow2_lowered: std::cell::Cell::new(0),
            virt_branch_eq_hit: std::cell::Cell::new(0),
            virt_branch_parity_hit: std::cell::Cell::new(0),
            get_upvalue_calls: std::cell::Cell::new(0),
            get_upvalue_closed_integer_hit: std::cell::Cell::new(0),
            get_upvalue_fallback: std::cell::Cell::new(0),
            set_upvalue_calls: std::cell::Cell::new(0),
            jit_op_return_calls: std::cell::Cell::new(0),
            call_ic_hit: std::cell::Cell::new(0),
            call_ic_miss: std::cell::Cell::new(0),
            get_upvalue_inline_hit: std::cell::Cell::new(0),
            ic_callee_has_spec_entry: std::cell::Cell::new(0),
            ic_callee_has_closure_aware_spec: std::cell::Cell::new(0),
            closure_aware_call_dispatch: std::cell::Cell::new(0),
            closure_inline_hit: std::cell::Cell::new(0),
            closure_inline_guard_miss: std::cell::Cell::new(0),
            helper_calls,
            spec_entry_eligible: std::cell::Cell::new(0),
            spec_entry_rejected_zero_arity: std::cell::Cell::new(0),
            spec_entry_rejected_param_not_int: std::cell::Cell::new(0),
            spec_entry_rejected_param_captured: std::cell::Cell::new(0),
            spec_entry_rejected_has_closure_op: std::cell::Cell::new(0),
            spec_entry_rejected_has_upvalue_op: std::cell::Cell::new(0),
            spec_entry_rejected_no_return: std::cell::Cell::new(0),
            spec_entry_rejected_no_call: std::cell::Cell::new(0),
            spec_entry_rejected_return_unreachable: std::cell::Cell::new(0),
        }
    }

    /// Per-helper bump. Called from every `extern "C"` runtime helper at entry;
    /// does nothing when counters are absent.
    #[inline]
    pub(crate) fn bump_helper(&self, h: HelperCounter) {
        let cell = &self.helper_calls[h as usize];
        cell.set(cell.get() + 1);
    }

    pub(super) fn dump(&self) {
        eprintln!("[jit stats]");
        eprintln!(
            "  specialized_entry_compiled:        {}",
            self.specialized_entry_compiled.get()
        );
        eprintln!(
            "  specialized_entry_called:          {}",
            self.specialized_entry_called.get()
        );
        eprintln!(
            "  specialized_call_ic_hit:           {}",
            self.specialized_call_ic_hit.get()
        );
        eprintln!(
            "  specialized_call_ic_miss:          {}",
            self.specialized_call_ic_miss.get()
        );
        eprintln!(
            "  specialized_call_no_entry:         {}",
            self.specialized_call_no_entry.get()
        );
        eprintln!(
            "  specialized_call_arity_mismatch:   {}",
            self.specialized_call_arity_mismatch.get()
        );
        eprintln!(
            "  specialized_call_bailout:          {}",
            self.specialized_call_bailout.get()
        );
        eprintln!(
            "  self_recursion_direct_call:        {}",
            self.self_recursion_direct_call.get()
        );
        eprintln!(
            "  virt_divmod_fast:                  {}",
            self.virt_divmod_fast.get()
        );
        eprintln!(
            "  virt_divmod_slow_zero:             {}",
            self.virt_divmod_slow_zero.get()
        );
        eprintln!(
            "  virt_divmod_slow_overflow:         {}",
            self.virt_divmod_slow_overflow.get()
        );
        eprintln!(
            "  virt_div_pow2_lowered:             {}",
            self.virt_div_pow2_lowered.get()
        );
        eprintln!(
            "  virt_branch_eq_hit:                {}",
            self.virt_branch_eq_hit.get()
        );
        eprintln!(
            "  virt_branch_parity_hit:            {}",
            self.virt_branch_parity_hit.get()
        );
        eprintln!(
            "  get_upvalue_calls:                 {}",
            self.get_upvalue_calls.get()
        );
        eprintln!(
            "  get_upvalue_closed_integer_hit:    {}",
            self.get_upvalue_closed_integer_hit.get()
        );
        eprintln!(
            "  get_upvalue_fallback:              {}",
            self.get_upvalue_fallback.get()
        );
        eprintln!(
            "  set_upvalue_calls:                 {}",
            self.set_upvalue_calls.get()
        );
        eprintln!(
            "  jit_op_return_calls:               {}",
            self.jit_op_return_calls.get()
        );
        eprintln!(
            "  call_ic_hit:                       {}",
            self.call_ic_hit.get()
        );
        eprintln!(
            "  call_ic_miss:                      {}",
            self.call_ic_miss.get()
        );
        eprintln!(
            "  get_upvalue_inline_hit:            {}",
            self.get_upvalue_inline_hit.get()
        );
        eprintln!(
            "  ic_callee_has_spec_entry:          {}",
            self.ic_callee_has_spec_entry.get()
        );
        eprintln!(
            "  ic_callee_has_closure_aware_spec:  {}",
            self.ic_callee_has_closure_aware_spec.get()
        );
        eprintln!(
            "  closure_aware_call_dispatch:       {}",
            self.closure_aware_call_dispatch.get()
        );
        eprintln!(
            "  closure_inline_hit:                {}",
            self.closure_inline_hit.get()
        );
        eprintln!(
            "  closure_inline_guard_miss:         {}",
            self.closure_inline_guard_miss.get()
        );

        eprintln!("[jit stats] spec-entry eligibility");
        eprintln!(
            "  spec_entry_eligible:                       {}",
            self.spec_entry_eligible.get()
        );
        eprintln!(
            "  spec_entry_rejected_zero_arity:            {}",
            self.spec_entry_rejected_zero_arity.get()
        );
        eprintln!(
            "  spec_entry_rejected_param_not_int:         {}",
            self.spec_entry_rejected_param_not_int.get()
        );
        eprintln!(
            "  spec_entry_rejected_param_captured:        {}",
            self.spec_entry_rejected_param_captured.get()
        );
        eprintln!(
            "  spec_entry_rejected_has_closure_op:        {}",
            self.spec_entry_rejected_has_closure_op.get()
        );
        eprintln!(
            "  spec_entry_rejected_has_upvalue_op:        {}",
            self.spec_entry_rejected_has_upvalue_op.get()
        );
        eprintln!(
            "  spec_entry_rejected_no_return:             {}",
            self.spec_entry_rejected_no_return.get()
        );
        eprintln!(
            "  spec_entry_rejected_no_call:               {}",
            self.spec_entry_rejected_no_call.get()
        );
        eprintln!(
            "  spec_entry_rejected_return_unreachable:    {}",
            self.spec_entry_rejected_return_unreachable.get()
        );

        eprintln!("[jit stats] helper FFI calls (non-zero)");
        for (i, &name) in HELPER_NAMES.iter().enumerate() {
            let n = self.helper_calls[i].get();
            if n > 0 {
                eprintln!("  {:<32} {}", name, n);
            }
        }
    }
}

/// Emit IR that increments a `Cell<u64>` counter at a fixed address, baked in
/// as an immediate. Emits load + iadd_imm + store.
#[inline]
pub(super) fn emit_counter_bump(
    builder: &mut FunctionBuilder<'_>,
    counters_ptr: *const JitCounters,
    offset: isize,
) {
    let addr = unsafe { (counters_ptr as *const u8).offset(offset) } as i64;
    let ptr = builder.ins().iconst(types::I64, addr);
    let flags = cranelift_codegen::ir::MemFlagsData::trusted();
    let cur = builder.ins().load(types::I64, flags, ptr, 0);
    let next = builder.ins().iadd_imm_s(cur, 1);
    builder.ins().store(flags, next, ptr, 0);
}

/// Byte offset of each `JitCounters` field from the struct base. The struct is
/// `#[repr(C)]` and the fields are `Cell<u64>`.
#[allow(dead_code)]
pub(super) mod counter_offsets {
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
    pub const VIRT_DIV_POW2_LOWERED: isize =
        offset_of!(JitCounters, virt_div_pow2_lowered) as isize;
    pub const VIRT_BRANCH_EQ_HIT: isize = offset_of!(JitCounters, virt_branch_eq_hit) as isize;
    pub const VIRT_BRANCH_PARITY_HIT: isize =
        offset_of!(JitCounters, virt_branch_parity_hit) as isize;
    pub const CALL_IC_HIT: isize = offset_of!(JitCounters, call_ic_hit) as isize;
    pub const CALL_IC_MISS: isize = offset_of!(JitCounters, call_ic_miss) as isize;
    pub const GET_UPVALUE_INLINE_HIT: isize =
        offset_of!(JitCounters, get_upvalue_inline_hit) as isize;
    pub const IC_CALLEE_HAS_SPEC_ENTRY: isize =
        offset_of!(JitCounters, ic_callee_has_spec_entry) as isize;
    pub const IC_CALLEE_HAS_CLOSURE_AWARE_SPEC: isize =
        offset_of!(JitCounters, ic_callee_has_closure_aware_spec) as isize;
    pub const CLOSURE_AWARE_CALL_DISPATCH: isize =
        offset_of!(JitCounters, closure_aware_call_dispatch) as isize;
    pub const CLOSURE_INLINE_HIT: isize = offset_of!(JitCounters, closure_inline_hit) as isize;
    pub const CLOSURE_INLINE_GUARD_MISS: isize =
        offset_of!(JitCounters, closure_inline_guard_miss) as isize;
}
