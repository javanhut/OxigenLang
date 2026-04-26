//! Virtual operand-stack layer for the Cranelift-backed JIT.
//!
//! Replaces direct memory writes-then-reads of `vm.stack` with a
//! compile-time stack of pending Cranelift SSA values and constants.
//! Each entry represents one operand-stack position **above** the
//! materialised tail of `vm.stack` (whose authoritative depth is
//! `vm.stack_view.len`). Pushing extends the virt stack without any
//! IR. The next opcode that pops can either consume the virt slot
//! directly (no memory load) or call `flush_to_memory` to spill
//! everything to `vm.stack` before reading.
//!
//! Invariants:
//!   * The virt stack only ever holds **pending** values. After
//!     `flush_to_memory` it is empty and `vm.stack_view.len`
//!     matches the logical depth.
//!   * Memory-resident slots (everything below the virt stack) are
//!     not tracked here — the outer JIT manages `stack_view.len`
//!     directly.
//!   * Any opcode that lets runtime code observe `vm.stack` (helper
//!     calls, the interpreter, GC) MUST flush first. That contract
//!     is enforced one level up in `engine.rs`'s opcode dispatch
//!     loop, not here.
//!
//! Why this matters with Cranelift: Cranelift is an SSA codegen, so
//! it optimizes by tracking `Value`s flowing through pure operations
//! and degrades when values get round-tripped through memory. Pushing
//! a `Constant(5)` straight to `vm.stack` and immediately reloading
//! it from memory at the next `Add` is a pessimization Cranelift's
//! alias analysis cannot always see through; even when it can, you
//! pay for a tag-byte write that no consumer needs. Staging values as
//! SSA lets Cranelift constant-fold across opcodes, allocate
//! registers, drop dead pushes entirely, and fuse `iconst+iadd` into
//! `iadd_imm`.

#![cfg(feature = "jit")]

use cranelift_codegen::ir;
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir::types;
use cranelift_frontend::FunctionBuilder;

use crate::vm::value::{
    VALUE_INT_PAYLOAD_OFFSET, VALUE_SIZE, VALUE_TAG_FLOAT, VALUE_TAG_INTEGER, VALUE_TAG_NONE,
};

/// One pending operand-stack position. All variants are virt-only —
/// memory-resident slots are not tracked by this stack.
#[derive(Copy, Clone, Debug)]
pub(crate) enum VirtSlot {
    /// Raw `i64` payload. Materialises as `Value::Integer(payload)`.
    IntSsa(ir::Value),
    /// Raw `f64` bits packed in an `i64`. Materialises as
    /// `Value::Float(f64::from_bits(bits))`.
    FloatSsa(ir::Value),
    /// One of the zero-payload primitives (`None`, `True`, `False`).
    Const(VirtConst),
}

/// Zero-payload virt constants. Tracked separately from `IntSsa` so
/// future folding (e.g. `if true`) can recognise them statically and
/// so each variant's tag byte is known at codegen time without
/// emitting a runtime tag check.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum VirtConst {
    None,
    True,
    False,
}

/// Compile-time stack of pending virt-only operand-stack positions.
///
/// `slots[len-1]` is the current top. Pushing a Mem-resident value to
/// `vm.stack` (e.g. via a runtime helper) does NOT add an entry here;
/// memory-resident slots are tracked implicitly by `vm.stack_view.len`.
///
/// To consume a value from `vm.stack` while the virt stack is
/// non-empty, you must `flush_to_memory` first — otherwise the
/// memory and virt depths disagree on which slot is at which depth.
pub(crate) struct VirtStack {
    slots: Vec<VirtSlot>,
}

impl VirtStack {
    pub(crate) fn new() -> Self {
        Self {
            slots: Vec::with_capacity(16),
        }
    }

    /// True iff there are no pending virt slots — safe to consume
    /// from `vm.stack` directly.
    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    /// Number of pending virt slots above the memory-resident region.
    /// Existing call sites that compare `expr_stack.len() >= N` map
    /// to `virt_stack.pending_depth() >= N`.
    #[inline]
    pub(crate) fn pending_depth(&self) -> usize {
        self.slots.len()
    }

    /// Push a virt-only Int64 SSA value.
    #[inline]
    pub(crate) fn push_int_ssa(&mut self, v: ir::Value) {
        self.slots.push(VirtSlot::IntSsa(v));
    }

    /// Push a virt-only Float SSA value (raw bits).
    #[inline]
    pub(crate) fn push_float_ssa(&mut self, bits: ir::Value) {
        self.slots.push(VirtSlot::FloatSsa(bits));
    }

    /// Push a virt-only zero-payload constant.
    #[inline]
    pub(crate) fn push_const(&mut self, k: VirtConst) {
        self.slots.push(VirtSlot::Const(k));
    }

    /// Pop the top virt slot. Returns `None` if the stack is empty
    /// (in which case the caller should consume from `vm.stack`
    /// instead).
    #[inline]
    pub(crate) fn pop(&mut self) -> Option<VirtSlot> {
        self.slots.pop()
    }

    /// Pop the top virt slot iff it is `IntSsa`. Returns `None` if
    /// the stack is empty or the top is not an `IntSsa` (caller must
    /// then flush + consume from memory). Convenience for arith fast
    /// paths that only care about the int case.
    #[inline]
    pub(crate) fn pop_int_ssa(&mut self) -> Option<ir::Value> {
        match self.slots.last() {
            Some(VirtSlot::IntSsa(_)) => match self.slots.pop() {
                Some(VirtSlot::IntSsa(v)) => Some(v),
                _ => unreachable!(),
            },
            _ => None,
        }
    }

    /// Peek at the top slot without consuming.
    #[inline]
    pub(crate) fn peek(&self) -> Option<VirtSlot> {
        self.slots.last().copied()
    }

    /// Peek at slot at depth `from_top` (0 = top). Mirrors the
    /// existing `expr_stack[len - 1 - from_top]` indexing pattern.
    #[inline]
    pub(crate) fn peek_at(&self, from_top: usize) -> Option<VirtSlot> {
        let len = self.slots.len();
        if from_top >= len {
            None
        } else {
            Some(self.slots[len - 1 - from_top])
        }
    }

    /// Convenience: peek at the top iff it is `IntSsa`.
    #[inline]
    pub(crate) fn peek_int_ssa(&self) -> Option<ir::Value> {
        match self.slots.last() {
            Some(VirtSlot::IntSsa(v)) => Some(*v),
            _ => None,
        }
    }

    /// Convenience: peek at slot at `from_top` iff it is `IntSsa`.
    /// Mirrors the existing `expr_stack[len - 1 - from_top]` access
    /// pattern that assumed every slot was an int SSA value.
    #[inline]
    pub(crate) fn peek_int_ssa_at(&self, from_top: usize) -> Option<ir::Value> {
        match self.peek_at(from_top) {
            Some(VirtSlot::IntSsa(v)) => Some(v),
            _ => None,
        }
    }

    /// True iff the top `n` slots are all `IntSsa`. Replaces the
    /// previous `expr_stack.len() >= n` check, which trivially held
    /// because the legacy `expr_stack` only ever stored ints.
    /// Now that the stack can hold non-int variants, callers that
    /// want to take an int-only fast path must verify the relevant
    /// slots are `IntSsa` before popping.
    #[inline]
    pub(crate) fn top_n_are_int_ssa(&self, n: usize) -> bool {
        let len = self.slots.len();
        if n > len {
            return false;
        }
        self.slots[len - n..]
            .iter()
            .all(|s| matches!(s, VirtSlot::IntSsa(_)))
    }

    /// Materialise every pending virt slot into `vm.stack[]` and bump
    /// `stack_view.len` accordingly. After this call the virt stack
    /// is empty and `vm.stack` reflects the full logical depth.
    /// Idempotent — safe to call when already empty.
    pub(crate) fn flush_to_memory(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        vm_val: ir::Value,
    ) {
        if self.slots.is_empty() {
            return;
        }
        // Pushes are processed in stack order so writes land at
        // sequentially increasing `stack_view.len`.
        for slot in self.slots.drain(..) {
            match slot {
                VirtSlot::IntSsa(payload) => {
                    emit_inline_push_integer(builder, vm_val, payload);
                }
                VirtSlot::FloatSsa(bits) => {
                    emit_inline_push_float(builder, vm_val, bits);
                }
                VirtSlot::Const(k) => {
                    emit_inline_push_const(builder, vm_val, k);
                }
            }
        }
    }
}

/// Inline `jit_push_integer_inline` — copy of the engine.rs helper,
/// duplicated here so the virt-stack module doesn't reach into engine
/// private functions. Layout pinned by `value_size_is_pinned_at_16`
/// and `value_integer_layout_is_pinned` tests in `vm/value.rs`.
fn emit_inline_push_integer(
    builder: &mut FunctionBuilder<'_>,
    vm_val: ir::Value,
    payload: ir::Value,
) {
    let flags = ir::MemFlags::trusted();
    let stack_ptr = load_stack_ptr(builder, vm_val);
    let top = load_stack_len(builder, vm_val);
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

/// Inline `jit_push_float_inline`.
fn emit_inline_push_float(
    builder: &mut FunctionBuilder<'_>,
    vm_val: ir::Value,
    bits: ir::Value,
) {
    let flags = ir::MemFlags::trusted();
    let stack_ptr = load_stack_ptr(builder, vm_val);
    let top = load_stack_len(builder, vm_val);
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

/// Inline push of a zero-payload constant. Layout invariants pinned
/// by `value_bool_tag_and_payload_are_pinned` and
/// `value_none_layout_is_pinned` tests in `vm/value.rs`.
fn emit_inline_push_const(
    builder: &mut FunctionBuilder<'_>,
    vm_val: ir::Value,
    k: VirtConst,
) {
    let flags = ir::MemFlags::trusted();
    let stack_ptr = load_stack_ptr(builder, vm_val);
    let top = load_stack_len(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let byte_off = builder.ins().imul(top, value_size);
    let slot_ptr = builder.ins().iadd(stack_ptr, byte_off);

    match k {
        VirtConst::None => {
            let tag = builder.ins().iconst(types::I8, VALUE_TAG_NONE as i64);
            builder.ins().store(flags, tag, slot_ptr, 0);
            // Zero the payload word for a clean bit pattern (matches
            // Value::None's freshly constructed shape; defensive for
            // any whole-slot byte compare, of which there are none
            // today but future code may add).
            let zero64 = builder.ins().iconst(types::I64, 0);
            builder
                .ins()
                .store(flags, zero64, slot_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
        }
        VirtConst::True | VirtConst::False => {
            // Boolean tag is 3 (4th variant); payload byte at offset 1.
            // Pinned by `value_bool_tag_and_payload_are_pinned`.
            const VALUE_TAG_BOOLEAN: u8 = 3;
            let tag = builder.ins().iconst(types::I8, VALUE_TAG_BOOLEAN as i64);
            builder.ins().store(flags, tag, slot_ptr, 0);
            let payload_byte = if matches!(k, VirtConst::True) { 1 } else { 0 };
            let pb = builder.ins().iconst(types::I8, payload_byte);
            builder.ins().store(flags, pb, slot_ptr, 1);
        }
    }

    let one = builder.ins().iconst(types::I64, 1);
    let new_top = builder.ins().iadd(top, one);
    builder
        .ins()
        .store(flags, new_top, vm_val, vm_stack_view_len_offset());
}

fn load_stack_ptr(builder: &mut FunctionBuilder<'_>, vm_val: ir::Value) -> ir::Value {
    let flags = ir::MemFlags::trusted();
    builder
        .ins()
        .load(types::I64, flags, vm_val, vm_stack_view_ptr_offset())
}

fn load_stack_len(builder: &mut FunctionBuilder<'_>, vm_val: ir::Value) -> ir::Value {
    let flags = ir::MemFlags::trusted();
    builder
        .ins()
        .load(types::I64, flags, vm_val, vm_stack_view_len_offset())
}

fn vm_stack_view_ptr_offset() -> i32 {
    use crate::vm::{StackView, VM};
    (VM::stack_view_offset() + StackView::OFFSET_PTR as usize) as i32
}

fn vm_stack_view_len_offset() -> i32 {
    use crate::vm::{StackView, VM};
    (VM::stack_view_offset() + StackView::OFFSET_LEN as usize) as i32
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_stack() {
        let s = VirtStack::new();
        assert!(s.is_empty());
        assert_eq!(s.pending_depth(), 0);
    }

    // The pop variants and IR emission are exercised end-to-end by
    // the JIT integration tests in core/tests/jit_fallback.rs and by
    // the bench suite. Constructing a Cranelift FunctionBuilder for
    // a unit test would be heavier than the test surface justifies.
}
