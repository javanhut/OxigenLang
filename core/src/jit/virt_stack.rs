//! Virtual operand-stack layer for the Cranelift-backed JIT.
//!
//! Holds a compile-time stack of pending Cranelift SSA values and constants.
//! Each entry represents one operand-stack position **above** the
//! materialised tail of `vm.stack` (whose authoritative depth is
//! `vm.stack_view.len`). Pushing extends the virt stack without any
//! IR. The next opcode that pops can either consume the virt slot
//! directly or call `flush_to_memory` to spill everything to `vm.stack`
//! before reading.
//!
//! Invariants:
//!   * The virt stack only ever holds **pending** values. After
//!     `flush_to_memory` it is empty and `vm.stack_view.len`
//!     matches the logical depth.
//!   * Memory-resident slots (everything below the virt stack) are
//!     not tracked here.
//!   * Any opcode that lets runtime code observe `vm.stack` (helper
//!     calls, the interpreter, GC) must flush first.

#![cfg(feature = "jit")]

use cranelift_codegen::ir;
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir::types;
use cranelift_frontend::FunctionBuilder;

use crate::vm::value::{
    VALUE_INT_PAYLOAD_OFFSET, VALUE_SIZE, VALUE_TAG_FLOAT, VALUE_TAG_INTEGER, VALUE_TAG_NONE,
};

/// One pending operand-stack position. All variants are virt-only;
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

/// Zero-payload virt constants, each with a tag byte known at codegen time.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum VirtConst {
    None,
    True,
    False,
}

/// Compile-time stack of pending virt-only operand-stack positions.
///
/// `slots[len-1]` is the current top. Memory-resident slots are tracked
/// implicitly by `vm.stack_view.len`.
///
/// To consume a value from `vm.stack` while the virt stack is
/// non-empty, `flush_to_memory` must be called first.
pub(crate) struct VirtStack {
    slots: Vec<VirtSlot>,
}

impl VirtStack {
    pub(crate) fn new() -> Self {
        Self {
            slots: Vec::with_capacity(16),
        }
    }

    /// True iff there are no pending virt slots, so values can be consumed
    /// from `vm.stack` directly.
    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    /// Number of pending virt slots above the memory-resident region.
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

    /// Pop the top virt slot. Returns `None` if the stack is empty.
    #[inline]
    pub(crate) fn pop(&mut self) -> Option<VirtSlot> {
        self.slots.pop()
    }

    /// Pop the top virt slot iff it is `IntSsa`. Returns `None` if
    /// the stack is empty or the top is not an `IntSsa`.
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

    /// Peek at slot at depth `from_top` (0 = top).
    #[inline]
    pub(crate) fn peek_at(&self, from_top: usize) -> Option<VirtSlot> {
        let len = self.slots.len();
        if from_top >= len {
            None
        } else {
            Some(self.slots[len - 1 - from_top])
        }
    }

    /// Peek at the top iff it is `IntSsa`.
    #[inline]
    pub(crate) fn peek_int_ssa(&self) -> Option<ir::Value> {
        match self.slots.last() {
            Some(VirtSlot::IntSsa(v)) => Some(*v),
            _ => None,
        }
    }

    /// Peek at slot at `from_top` iff it is `IntSsa`.
    #[inline]
    pub(crate) fn peek_int_ssa_at(&self, from_top: usize) -> Option<ir::Value> {
        match self.peek_at(from_top) {
            Some(VirtSlot::IntSsa(v)) => Some(v),
            _ => None,
        }
    }

    /// True iff the top `n` slots are all `IntSsa`.
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

    /// Capture the current virt-stack contents so a sibling emission can mutate
    /// them without disturbing what later code expects to see. Restoring only
    /// rewinds the compile-time tracking; Cranelift IR already emitted is
    /// untouched.
    #[inline]
    pub(crate) fn snapshot(&self) -> Vec<VirtSlot> {
        self.slots.clone()
    }

    /// Pair to `snapshot`. Resets the compile-time tracking to `snap`.
    #[inline]
    pub(crate) fn restore(&mut self, snap: Vec<VirtSlot>) {
        self.slots = snap;
    }

    /// Materialise every pending virt slot into `vm.stack[]` and bump
    /// `stack_view.len` accordingly. After this call the virt stack
    /// is empty and `vm.stack` reflects the full logical depth.
    /// Idempotent.
    pub(crate) fn flush_to_memory(&mut self, builder: &mut FunctionBuilder<'_>, vm_val: ir::Value) {
        if self.slots.is_empty() {
            return;
        }
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

/// Inline `jit_push_integer_inline`: pushes an integer value onto `vm.stack`.
fn emit_inline_push_integer(
    builder: &mut FunctionBuilder<'_>,
    vm_val: ir::Value,
    payload: ir::Value,
) {
    let flags = ir::MemFlagsData::trusted();
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
fn emit_inline_push_float(builder: &mut FunctionBuilder<'_>, vm_val: ir::Value, bits: ir::Value) {
    let flags = ir::MemFlagsData::trusted();
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

/// Inline push of a zero-payload constant.
fn emit_inline_push_const(builder: &mut FunctionBuilder<'_>, vm_val: ir::Value, k: VirtConst) {
    let flags = ir::MemFlagsData::trusted();
    let stack_ptr = load_stack_ptr(builder, vm_val);
    let top = load_stack_len(builder, vm_val);
    let value_size = builder.ins().iconst(types::I64, VALUE_SIZE as i64);
    let byte_off = builder.ins().imul(top, value_size);
    let slot_ptr = builder.ins().iadd(stack_ptr, byte_off);

    match k {
        VirtConst::None => {
            let tag = builder.ins().iconst(types::I8, VALUE_TAG_NONE as i64);
            builder.ins().store(flags, tag, slot_ptr, 0);
            let zero64 = builder.ins().iconst(types::I64, 0);
            builder
                .ins()
                .store(flags, zero64, slot_ptr, VALUE_INT_PAYLOAD_OFFSET as i32);
        }
        VirtConst::True | VirtConst::False => {
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
    let flags = ir::MemFlagsData::trusted();
    builder
        .ins()
        .load(types::I64, flags, vm_val, vm_stack_view_ptr_offset())
}

fn load_stack_len(builder: &mut FunctionBuilder<'_>, vm_val: ir::Value) -> ir::Value {
    let flags = ir::MemFlagsData::trusted();
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
}
