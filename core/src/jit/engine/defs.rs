//! Small enums + VM-offset helpers shared across the engine submodules.
//!
//! Pulled out of the original monolithic `engine.rs` so the orchestration
//! file can focus on the giant `compile_function` dispatch loop. Nothing
//! here depends on Cranelift IR construction; it's pure constants and
//! small enums.

#![cfg(feature = "jit")]

use crate::vm::{JitFrameView, StackView, VM, VMError};

use super::super::CompiledEntries;

// ── VM struct field offsets baked into emitted IR ──────────────────────

/// Byte offset of the `stack_view.ptr` field from the base of `VM`.
/// Computed once and baked into emitted IR as a constant.
#[inline(always)]
pub(super) fn vm_stack_view_ptr_offset() -> i32 {
    (VM::stack_view_offset() + StackView::OFFSET_PTR as usize) as i32
}

/// Byte offset of the `stack_view.len` field from the base of `VM`.
#[inline(always)]
pub(super) fn vm_stack_view_len_offset() -> i32 {
    (VM::stack_view_offset() + StackView::OFFSET_LEN as usize) as i32
}

#[allow(dead_code)]
#[inline(always)]
pub(super) fn vm_jit_frame_view_ptr_offset() -> i32 {
    (VM::jit_frame_view_offset() + JitFrameView::OFFSET_PTR as usize) as i32
}

#[allow(dead_code)]
#[inline(always)]
pub(super) fn vm_jit_frame_view_len_offset() -> i32 {
    (VM::jit_frame_view_offset() + JitFrameView::OFFSET_LEN as usize) as i32
}

// ── Compile-function state machine ─────────────────────────────────────

/// Cache slot for `JitInner::compiled` — either a successful compile
/// or a recorded failure so we don't keep retrying broken functions.
pub(super) enum Entry {
    Compiled { entries: CompiledEntries },
    Failed,
}

/// Which entry point the compile_function IR emitter is currently
/// producing. Used as a compile-time branch variable at three
/// divergence points (entry block params, prologue, OpCode::Return
/// success path) — everywhere else the emission logic is shared.
///
/// The ABI shape per entry kind.
///   Generic:        fn(*mut VM) -> u32
///   IntSpecialized: fn(*mut VM, i64, ..., i64) -> (u32, i64)
///
/// Every emit site within `compile_function` funnels its return through
/// a single `exit_block` with two block params (status: I32, payload:
/// I64). The post-dispatch tail of the entry then issues
/// `return_(&[status])` for Generic (dropping payload) or
/// `return_(&[status, payload])` for IntSpecialized. Early-exit /
/// bailout sites pass `(status, iconst(0))` since payload is only
/// meaningful on status == 0 success-Return.
#[allow(dead_code)] // Generic variant not yet routed through this enum
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum EntryKind {
    /// `fn(*mut VM) -> u32`. Caller pushes args onto the VM stack as
    /// `Value`. Entry reads them through `stack_view`, optionally
    /// through B2.2a tag guards for int mirrors. Successful
    /// `OpCode::Return` calls the `op_return` helper.
    Generic,
    /// `fn(*mut VM, i64, ..., i64) -> (u32, i64)`. Caller passes args
    /// in registers; status returns in the first ABI return slot,
    /// payload (valid iff status == 0) in the second. Entry writes
    /// each i64 back to the backing stack slot for helper compat +
    /// `def_var`s the mirror Variable directly — no tag guard (args
    /// are trusted Int by the caller's contract). Successful
    /// `OpCode::Return` jumps to `exit_block` with `(0, payload)`;
    /// the post-dispatch tail issues the multi-value return. Early
    /// error/bailout returns flow through the same exit_block with
    /// `(status, 0)` — the payload slot is ignored by the caller on
    /// non-zero status.
    IntSpecialized,
}

/// What kind of specialized entry a `CompiledEntries` carries.
/// Observable on the VM side (ObjClosure) so call sites can gate A3's
/// direct-call path on having a real body, not a trampoline.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum SpecializedEntryKind {
    /// A2's adapter: boxes i64 args → calls generic → unboxes result.
    /// Correctness-preserving but pays more than it saves vs the IC
    /// path. A3 must NOT direct-call trampolines.
    ForwardTrampoline,
    /// The real specialized body: runs the function's opcodes with
    /// i64-resident args, no box/unbox round trip. Safe A3 target.
    NativeIntBody,
    /// B2.2: closure-aware specialized body. ABI passes the
    /// `*const ObjClosure` as a register arg in addition to the i64
    /// args, so `GetUpvalue` reads through the register instead of
    /// walking the JitFrame to find `closure_raw`. Eligibility (per
    /// `compute_specialized_entry_eligibility` in slot_types.rs):
    /// body uses `GetUpvalue` only (no `SetUpvalue` / `CloseUpvalue`),
    /// no nested `Closure` op, all the usual int-typed-param /
    /// has-Return / has-Call rules. Closure-aware Return inlines the
    /// stack truncate + multi-return without `jit_op_return`'s FFI
    /// crossing — the path that pays the most for `bench_closure`'s
    /// 500k invocations.
    NativeIntBodyWithClosure,
}

/// Outcome of invoking a JIT-compiled function. Surfaced to the
/// `JitEngine` facade in `core/src/jit/mod.rs`.
pub(crate) enum InvokeOutcome {
    Returned,
    Bailout,
    RuntimeError(VMError),
}
