//! Small enums and VM-offset helpers shared across the engine submodules.

#![cfg(feature = "jit")]

use crate::vm::{JitFrameView, StackView, VM, VMError};

use super::super::CompiledEntries;

/// Byte offset of the `stack_view.ptr` field from the base of `VM`.
#[inline(always)]
pub(super) fn vm_stack_view_ptr_offset() -> i32 {
    (VM::stack_view_offset() + StackView::OFFSET_PTR as usize) as i32
}

/// Byte offset of the `stack_view.len` field from the base of `VM`.
#[inline(always)]
pub(super) fn vm_stack_view_len_offset() -> i32 {
    (VM::stack_view_offset() + StackView::OFFSET_LEN as usize) as i32
}

/// Byte offset of `limits_armed` from the base of `VM`.
#[inline(always)]
pub(super) fn vm_limits_armed_offset() -> i32 {
    VM::limits_armed_offset() as i32
}

/// Byte offset of `loop_countdown` from the base of `VM`.
#[inline(always)]
pub(super) fn vm_loop_countdown_offset() -> i32 {
    VM::loop_countdown_offset() as i32
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

/// Byte offset of a closure's owning-module globals from the base of
/// `ObjClosure`. Loads as a plain pointer, null for a closure defined in the
/// main script.
#[allow(dead_code)]
#[inline(always)]
pub(super) fn obj_closure_module_globals_offset() -> i32 {
    std::mem::offset_of!(crate::vm::value::ObjClosure, module_globals) as i32
}

/// Cache slot for `JitInner::compiled`: either a successful compile or a
/// recorded failure.
pub(super) enum Entry {
    Compiled { entries: CompiledEntries },
    Failed,
}

/// Which entry point the compile_function IR emitter is producing.
///
/// The ABI shape per entry kind:
///   Generic:        fn(*mut VM) -> u32
///   IntSpecialized: fn(*mut VM, i64, ..., i64) -> (u32, i64)
#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum EntryKind {
    /// `fn(*mut VM) -> u32`. Caller pushes args onto the VM stack as `Value`;
    /// the entry reads them through `stack_view`. Successful `OpCode::Return`
    /// calls the `op_return` helper.
    Generic,
    /// `fn(*mut VM, i64, ..., i64) -> (u32, i64)`. Caller passes args in
    /// registers; status returns in the first return slot and the payload
    /// (valid iff status == 0) in the second. Args are trusted to be Int by
    /// the caller's contract.
    IntSpecialized,
}

/// What kind of specialized entry a `CompiledEntries` carries. Observable on
/// the VM side (ObjClosure).
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum SpecializedEntryKind {
    /// The real specialized body: runs the function's opcodes with
    /// i64-resident args, no box/unbox round trip.
    NativeIntBody,
    /// Closure-aware specialized body: the `*const ObjClosure` is passed as a
    /// register arg so `GetUpvalue` reads through it. Requires the body to use
    /// `GetUpvalue` only (no `SetUpvalue` / `CloseUpvalue`) and no nested
    /// `Closure` op.
    NativeIntBodyWithClosure,
}

/// Outcome of invoking a JIT-compiled function. Surfaced to the
/// `JitEngine` facade in `core/src/jit/mod.rs`.
pub(crate) enum InvokeOutcome {
    Returned,
    Bailout,
    RuntimeError(VMError),
}
