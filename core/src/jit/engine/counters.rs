//! JIT instrumentation counters + the IR-side `emit_counter_bump`
//! helper that consumers use to wire bumps into emitted code.
//!
//! Pulled out of the original monolithic `engine.rs`. Two reasons it
//! lives in its own module:
//!   1. The `JitCounters` struct + its `dump()` impl is ~200 lines and
//!      mostly noise from the perspective of the dispatch loop.
//!   2. `counter_offsets` baking — it relies on `#[repr(C)]` field
//!      offsets that should be co-located with the struct definition,
//!      not jammed together with the IR emitters that consume them.

#![cfg(feature = "jit")]

use cranelift_codegen::ir;
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir::types;
use cranelift_frontend::FunctionBuilder;

/// Step 0 attribution: discriminant for the per-helper call counter
/// array on `JitCounters`. One slot per `extern "C"` runtime helper in
/// `runtime.rs`. Bumped from the helper's entry so the cost model is
/// "what fraction of this benchmark's time went into which helper".
///
/// `HELPER_NAMES` below must stay in lockstep with this enum order.
/// Adding a helper: add the variant, add the name, bump the counter
/// at the helper's entry.
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

/// Lightweight JIT instrumentation counters. Each field is a
/// `Cell<u64>` so increments compile to plain load + inc + store in
/// the JIT IR. On process exit, `JitInner::drop` dumps the counters
/// to stderr when the env var `OXIGEN_JIT_STATS` is set. Off by
/// default — zero text output.
///
/// Added in stage A0 ahead of the Plan A calling-convention work so a
/// weak benchmark result is debuggable ("did the specialized path
/// never get generated? never get hit? get hit but bail?").
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
    /// B2.1h: incremented every time the signed-div-by-power-of-two
    /// peephole replaces a Cranelift `sdiv x, pow2` with the bias-
    /// and-shift sequence (`sshr_imm + band_imm + iadd + sshr_imm`),
    /// avoiding the ~20-cycle `idiv` lowering Cranelift emits at
    /// `opt_level=none`.
    pub virt_div_pow2_lowered: std::cell::Cell<u64>,
    /// B2.1f: incremented every time the fused `Equal` / `NotEqual`
    /// + `JumpIf*` path fires with both operands staged as virt Ints.
    /// Measures how often the fast branch-fusion replaces a boxed-Bool
    /// round trip through `refs.eq`/`refs.ne`.
    pub virt_branch_eq_hit: std::cell::Cell<u64>,
    /// B2.1g: incremented when the parity branch peephole fires —
    /// `(x % 2) == 0` / `!= 0` (and the commuted `0 == x % 2` /
    /// `0 != x % 2`) lowered to `band_imm + icmp_imm + brif`,
    /// avoiding `srem` entirely. Retained post-Cranelift-0.131
    /// because the egraph mid-end's `srem`-by-2 lowering still
    /// emits a comparison sequence longer than this peephole's
    /// single bit-test.
    pub virt_branch_parity_hit: std::cell::Cell<u64>,
    /// Probe (bench_closure): raw count of `jit_get_upvalue` FFI
    /// crossings. Bumped from the Rust helper. Nonzero count here with
    /// zero coverage from a fast path indicates GetUpvalue is a
    /// candidate for inline lowering.
    pub get_upvalue_calls: std::cell::Cell<u64>,
    /// Probe: subset of `get_upvalue_calls` where the upvalue was
    /// `Closed(Value::Integer(_))`. If this ≈ `get_upvalue_calls` for
    /// a hot bench, an inline Closed+Integer fast path would cover
    /// nearly all traffic.
    pub get_upvalue_closed_integer_hit: std::cell::Cell<u64>,
    /// Probe: subset of `get_upvalue_calls` that were Open, or Closed
    /// non-Integer. Measures how often a fast path would have to fall
    /// back to the generic helper.
    pub get_upvalue_fallback: std::cell::Cell<u64>,
    /// Probe: raw count of `jit_set_upvalue` FFI crossings. Mutable
    /// upvalues disqualify simple caching strategies.
    pub set_upvalue_calls: std::cell::Cell<u64>,
    /// Probe: raw count of `jit_op_return` FFI crossings. Large values
    /// indicate return-path FFI is a significant fixed cost per
    /// function invocation.
    pub jit_op_return_calls: std::cell::Cell<u64>,
    /// Probe: inline Call IC hits (cache matches expected closure).
    /// Bumped from emitted IR at the hit-block entry.
    pub call_ic_hit: std::cell::Cell<u64>,
    /// Probe: inline Call IC misses (cache miss or non-closure tag).
    /// Bumped from emitted IR at the miss-block entry.
    pub call_ic_miss: std::cell::Cell<u64>,
    /// B2.2: incremented when the inline GetUpvalue closed-integer
    /// fast path fires — `kinds[idx] == 1` lets the JIT load the
    /// cached i64 directly from `ObjClosure.upvalue_int_values` and
    /// push it as a `Value::Integer` without crossing into Rust.
    /// Bumped from IR at the fast-block entry. The first GetUpvalue
    /// against any given closure always misses (cache empty) and goes
    /// through `jit_get_upvalue`, which populates the cache;
    /// subsequent reads hit. So
    /// `inline_hit + get_upvalue_calls ≈ total dynamic GetUpvalue
    /// count`, and the first-call miss counts toward
    /// `get_upvalue_calls`.
    pub get_upvalue_inline_hit: std::cell::Cell<u64>,
    /// A4.0 probe: subset of `call_ic_hit` where the callee closure
    /// has `specialized_kind == NATIVE_INT_BODY` (its specialized
    /// entry exists and would be invocable via direct i64 dispatch).
    /// Measures the upper bound on how often A4 (Call IC routes to
    /// specialized entry) would fire if we built that path. Bumped
    /// from IR inside the IC hit block — does NOT change behavior;
    /// the call still dispatches to the generic thunk.
    pub ic_callee_has_spec_entry: std::cell::Cell<u64>,
    /// B2.2.f probe: subset of `call_ic_hit` where the callee closure
    /// has `specialized_kind == NATIVE_INT_BODY_WITH_CLOSURE`.
    /// Measures the upper bound on closure-aware spec dispatch
    /// coverage. Bumped at the runtime check site, before the
    /// dispatch commits — counts callees that *could* be reached via
    /// the new ABI for this call site.
    pub ic_callee_has_closure_aware_spec: std::cell::Cell<u64>,
    /// B2.2.f: closure-aware specialized call dispatched. Args passed
    /// in registers, closure pointer in a register, no JitFrame walk
    /// for upvalue access, inline Return without `jit_op_return` FFI.
    /// Bumped from emitted IR at the call_indirect site, on the
    /// success branch (after the all-checks-passed gate). Strict
    /// subset of `ic_callee_has_closure_aware_spec` — the gap is
    /// arity / thunk-non-null / RC fail tail.
    pub closure_aware_call_dispatch: std::cell::Cell<u64>,

    /// Step 0 attribution: per-helper FFI call counts. Indexed by
    /// `HelperCounter`; bumped from each helper's entry. Tells us
    /// "this benchmark spent N FFI crossings into op_call_hit, M into
    /// get_local, ..." without re-instrumenting per investigation.
    pub helper_calls: [std::cell::Cell<u64>; HelperCounter::COUNT],

    /// Step 0 attribution: count of functions that the spec-entry
    /// eligibility analysis returned `Eligible` for.
    pub spec_entry_eligible: std::cell::Cell<u64>,
    /// Step 0 attribution: rejected because the function takes 0 args.
    pub spec_entry_rejected_zero_arity: std::cell::Cell<u64>,
    /// Step 0 attribution: rejected because at least one param wasn't
    /// Int64-typed and wasn't an int-mirror candidate.
    pub spec_entry_rejected_param_not_int: std::cell::Cell<u64>,
    /// Step 0 attribution: rejected because at least one param was
    /// captured by an upvalue.
    pub spec_entry_rejected_param_captured: std::cell::Cell<u64>,
    /// Step 0 attribution: rejected because the body contains a
    /// `Closure` op (allocates a new closure on the heap).
    pub spec_entry_rejected_has_closure_op: std::cell::Cell<u64>,
    /// Step 0 attribution: rejected because the body contains
    /// `GetUpvalue` / `SetUpvalue` / `CloseUpvalue`. Step 4 will lift
    /// this restriction (closure-aware specialized ABI); the counter
    /// is the bound on how much that step can unlock.
    pub spec_entry_rejected_has_upvalue_op: std::cell::Cell<u64>,
    /// Step 0 attribution: rejected because the function has no
    /// Return.
    pub spec_entry_rejected_no_return: std::cell::Cell<u64>,
    /// Step 0 attribution: rejected because the function has no
    /// Call — emitting a specialized body would be pure compile-time
    /// overhead.
    pub spec_entry_rejected_no_call: std::cell::Cell<u64>,
    /// Step 0 attribution: rejected because at least one Return IP was
    /// unreachable per the abstract interpretation.
    pub spec_entry_rejected_return_unreachable: std::cell::Cell<u64>,
}

impl JitCounters {
    pub(super) fn new() -> Self {
        // [Cell<u64>; N] doesn't impl Default in older toolchains;
        // build it explicitly with std::array::from_fn so
        // HelperCounter::COUNT can grow without touching this site.
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

    /// Step 0 attribution: per-helper bump. Called from every
    /// `extern "C"` runtime helper at entry. Branch on whether
    /// counters are present (None when JIT is built but the run is
    /// non-JIT) so the helper-side cost is one nullable-load + cmp +
    /// load + inc + store on the hot path.
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

        // Step 0 spec-entry eligibility outcome breakdown.
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

        // Step 0 helper-call counts. Print only non-zero entries to
        // keep the dump signal-dense; widths are aligned with the
        // longest helper name in HELPER_NAMES.
        eprintln!("[jit stats] helper FFI calls (non-zero)");
        for (i, &name) in HELPER_NAMES.iter().enumerate() {
            let n = self.helper_calls[i].get();
            if n > 0 {
                eprintln!("  {:<32} {}", name, n);
            }
        }
    }
}

/// Emit IR that increments a `Cell<u64>` counter at a fixed address.
/// The address is baked in as an immediate; since `JitCounters` lives
/// in a `Box` owned by `JitInner`, the address is stable for the JIT's
/// lifetime. Emits load + iadd_imm + store — 3 ops.
#[inline]
pub(super) fn emit_counter_bump(
    builder: &mut FunctionBuilder<'_>,
    counters_ptr: *const JitCounters,
    offset: isize,
) {
    use ir::MemFlags;
    let addr = unsafe { (counters_ptr as *const u8).offset(offset) } as i64;
    let ptr = builder.ins().iconst(types::I64, addr);
    let flags = MemFlags::trusted();
    let cur = builder.ins().load(types::I64, flags, ptr, 0);
    let next = builder.ins().iadd_imm(cur, 1);
    builder.ins().store(flags, next, ptr, 0);
}

/// Byte offset of each `JitCounters` field from the struct base.
/// The struct is `#[repr(C)]` so these are stable across Rust
/// versions. Fields are `Cell<u64>` which is 8 bytes and identical
/// to `u64`.
#[allow(dead_code)] // consumed by later Plan A stages (A2/A3/A5)
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
}
