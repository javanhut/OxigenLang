//! `extern "C"` runtime helpers invoked by JIT-compiled code.
//!
//! The ABI is uniform: every helper takes `*mut VM` as its first argument
//! plus any per-op immediates; erroring helpers return `u32` status codes
//! (0 = ok, 1 = runtime error) so the baseline JIT never needs to unwind
//! across the FFI boundary. Non-erroring helpers return no value.
//!
//! Implementation strategy: each helper is a thin wrapper around the
//! existing interpreter logic in `core/src/vm/mod.rs`. We deliberately do
//! *not* duplicate the big type-dispatch match arms — we call the
//! interpreter's own `binary_add`, `compare_less`, etc.

#![cfg(feature = "jit")]

use crate::vm::VM;
use crate::vm::value::{Upvalue, Value};
use std::cell::RefCell;
use std::rc::Rc;

// ── Fallback harness (Step 2, still used for functions the translator
//    itself rejects — currently none, but kept as a safety valve) ────────

/// Drive the interpreter for the just-pushed frame until it returns.
/// See the Milestone-1 plan for the full contract.
pub unsafe extern "C" fn jit_run_via_interpreter(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    let stop_depth = vm.jit.current_stop_depth();
    match vm.execute_until(stop_depth) {
        Ok(()) => 0,
        Err(err) => {
            vm.jit.stash_error(err);
            1
        }
    }
}

// ── Stack push helpers ─────────────────────────────────────────────────

pub unsafe extern "C" fn jit_push_constant(vm: *mut VM, idx: u32) {
    let vm = unsafe { &mut *vm };
    let value = vm.current_constant(idx as u16);
    vm.push(value);
}

pub unsafe extern "C" fn jit_push_integer_inline(vm: *mut VM, value: i64) {
    let vm = unsafe { &mut *vm };
    vm.push(Value::Integer(value));
}

pub unsafe extern "C" fn jit_push_float_inline(vm: *mut VM, bits: u64) {
    let vm = unsafe { &mut *vm };
    vm.push(Value::Float(f64::from_bits(bits)));
}

pub unsafe extern "C" fn jit_push_none(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.push(Value::None);
}

pub unsafe extern "C" fn jit_push_true(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.push(Value::Boolean(true));
}

pub unsafe extern "C" fn jit_push_false(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.push(Value::Boolean(false));
}

// ── Stack manipulation ─────────────────────────────────────────────────

pub unsafe extern "C" fn jit_pop(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.pop();
}

pub unsafe extern "C" fn jit_dup(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    let top = vm.peek(0).clone();
    vm.push(top);
}

// ── Collections ───────────────────────────────────────────────────────

pub unsafe extern "C" fn jit_build_array(vm: *mut VM, count: u32) {
    let vm = unsafe { &mut *vm };
    let count = count as usize;
    let mut elements = Vec::with_capacity(count);
    for _ in 0..count {
        elements.push(vm.pop());
    }
    elements.reverse();
    vm.push(Value::Array(Rc::new(RefCell::new(elements))));
}

pub unsafe extern "C" fn jit_op_index_fast_array_int(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    let index = vm.pop();
    let collection = vm.pop();

    if let (Value::Array(arr), Value::Integer(i)) = (&collection, &index) {
        let borrowed = arr.borrow();
        let idx = if *i < 0 {
            (borrowed.len() as i64 + i) as usize
        } else {
            *i as usize
        };
        vm.push(borrowed.get(idx).cloned().unwrap_or(Value::None));
        vm.jit.record_array_index_fast_hit();
        return 0;
    }

    vm.jit.record_array_index_fast_miss();
    match vm.eval_index(collection, index) {
        Ok(value) => {
            vm.push(value);
            0
        }
        Err(err) => {
            vm.jit.stash_error(err);
            1
        }
    }
}

pub unsafe extern "C" fn jit_type_wrap(vm: *mut VM, type_idx: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    let type_name = vm.current_constant(type_idx as u16);
    let value = vm.pop();

    let Value::String(target) = type_name else {
        vm.jit.stash_error(vm.runtime_error("invalid type name in TypeWrap"));
        return 1;
    };

    match vm.type_wrap(&target, value) {
        Ok(value) => {
            vm.push(value);
            0
        }
        Err(err) => {
            vm.jit.stash_error(err);
            1
        }
    }
}

pub unsafe extern "C" fn jit_local_add_array_mod_index(
    vm: *mut VM,
    dst_slot: u32,
    array_slot: u32,
    index_slot: u32,
    modulus: i64,
    pop_after: u32,
) -> u32 {
    let vm = unsafe { &mut *vm };
    if modulus == 0 {
        vm.jit.stash_error(vm.runtime_error("modulo by zero"));
        return 1;
    }

    let base = vm.current_slot_offset();
    let dst_abs = base + dst_slot as usize;
    let array_abs = base + array_slot as usize;
    let index_abs = base + index_slot as usize;

    let fast_result = match (
        vm.stack_slot(dst_abs),
        vm.stack_slot(array_abs),
        vm.stack_slot(index_abs),
    ) {
        (Value::Integer(total), Value::Array(arr), Value::Integer(index)) => {
            let indexed = index % modulus;
            let borrowed = arr.borrow();
            let idx = if indexed < 0 {
                (borrowed.len() as i64 + indexed) as usize
            } else {
                indexed as usize
            };
            match borrowed.get(idx) {
                Some(Value::Integer(value)) => Some(Value::Integer(total.wrapping_add(*value))),
                Some(_) | None => None,
            }
        }
        _ => None,
    };

    let result = if let Some(result) = fast_result {
        result
    } else {
        let total = vm.stack_slot(dst_abs).clone();
        let array = vm.stack_slot(array_abs).clone();
        let index = vm.stack_slot(index_abs).clone();
        let indexed = match vm.binary_mod(index, Value::Integer(modulus)) {
            Ok(value) => value,
            Err(err) => {
                vm.jit.stash_error(err);
                return 1;
            }
        };
        let value = match vm.eval_index(array, indexed) {
            Ok(value) => value,
            Err(err) => {
                vm.jit.stash_error(err);
                return 1;
            }
        };
        match vm.binary_add(total, value) {
            Ok(value) => value,
            Err(err) => {
                vm.jit.stash_error(err);
                return 1;
            }
        }
    };

    vm.set_stack_slot(dst_abs, result.clone());
    if pop_after == 0 {
        vm.push(result);
    }
    0
}

// ── Locals ─────────────────────────────────────────────────────────────

/// Return the current frame's `slot_offset` for the JIT's inline
/// GetLocal / SetLocal fast path to cache at thunk entry. Stable for the
/// lifetime of a thunk activation — our frame never has its slot_offset
/// rewritten once pushed.
pub unsafe extern "C" fn jit_current_slot_offset(vm: *mut VM) -> i64 {
    let vm = unsafe { &mut *vm };
    vm.current_slot_offset() as i64
}

pub unsafe extern "C" fn jit_get_local(vm: *mut VM, slot: u32) {
    let vm = unsafe { &mut *vm };
    let base = vm.current_slot_offset();
    let value = vm.stack_slot(base + slot as usize).clone();
    vm.push(value);
}

pub unsafe extern "C" fn jit_set_local(vm: *mut VM, slot: u32) {
    let vm = unsafe { &mut *vm };
    let base = vm.current_slot_offset();
    let value = vm.peek(0).clone();
    vm.set_stack_slot(base + slot as usize, value);
}

// ── Arithmetic — return 0 on ok, 1 on runtime error ────────────────────

macro_rules! binop_fallible {
    ($name:ident, $method:ident) => {
        pub unsafe extern "C" fn $name(vm: *mut VM) -> u32 {
            let vm = unsafe { &mut *vm };
            let b = vm.pop();
            let a = vm.pop();
            match vm.$method(a, b) {
                Ok(v) => {
                    vm.push(v);
                    0
                }
                Err(e) => {
                    vm.jit.stash_error(e);
                    1
                }
            }
        }
    };
}

binop_fallible!(jit_op_add, binary_add);
binop_fallible!(jit_op_sub, binary_sub);
binop_fallible!(jit_op_mul, binary_mul);
binop_fallible!(jit_op_div, binary_div);
binop_fallible!(jit_op_mod, binary_mod);
binop_fallible!(jit_op_lt, compare_less);
binop_fallible!(jit_op_le, compare_less_equal);
binop_fallible!(jit_op_gt, compare_greater);
binop_fallible!(jit_op_ge, compare_greater_equal);

// ── Comparison that can't fail (PartialEq over Value) ─────────────────

pub unsafe extern "C" fn jit_op_eq(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    let b = vm.pop();
    let a = vm.pop();
    vm.push(Value::Boolean(a == b));
}

pub unsafe extern "C" fn jit_op_ne(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    let b = vm.pop();
    let a = vm.pop();
    vm.push(Value::Boolean(a != b));
}

// ── Logical / unary ────────────────────────────────────────────────────

pub unsafe extern "C" fn jit_op_not(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    let v = vm.pop();
    vm.push(Value::Boolean(!v.is_truthy()));
}

pub unsafe extern "C" fn jit_op_negate(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    let v = vm.pop();
    match v {
        Value::Integer(n) => {
            vm.push(Value::Integer(-n));
            0
        }
        Value::Float(f) => {
            vm.push(Value::Float(-f));
            0
        }
        other => {
            let err = vm.runtime_error_hint(
                &format!(
                    "negation is only supported for numbers, got {}",
                    other.type_name()
                ),
                "only <int> and <float> values can be negated",
            );
            vm.jit.stash_error(err);
            1
        }
    }
}

// ── Branching truthy checks ────────────────────────────────────────────

/// Peek at top-of-stack (does NOT pop). Returns 1 if truthy, 0 if not.
/// Matches the semantics of `JumpIfFalse`/`JumpIfTrue`.
pub unsafe extern "C" fn jit_peek_truthy(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    if vm.peek(0).is_truthy() { 1 } else { 0 }
}

/// Pop top-of-stack, return 1 if truthy, 0 if not. Matches
/// `PopJumpIfFalse`.
pub unsafe extern "C" fn jit_pop_truthy(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    let v = vm.pop();
    if v.is_truthy() { 1 } else { 0 }
}

// ── Globals ────────────────────────────────────────────────────────────

pub unsafe extern "C" fn jit_get_global(vm: *mut VM, name_idx: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    match vm.handle_get_global(name_idx as u16) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

/// GetGlobal with a per-call-site inline cache. When the cache's
/// recorded `globals_version` matches the VM's current version, we push
/// a clone of the cached value and skip the HashMap lookup entirely —
/// one atomic Rc-inc for `Closure`/`String` variants, or a trivial
/// memcpy for primitives. On miss we fall through to the generic lookup
/// and repopulate the cache.
///
/// `cache_ptr` was allocated by `JitInner::alloc_global_cache()` and has
/// a stable address for the JIT engine's lifetime.
pub unsafe extern "C" fn jit_get_global_ic(
    vm: *mut VM,
    cache_ptr: *mut super::engine::GlobalCacheEntry,
    name_idx: u32,
) -> u32 {
    let vm = unsafe { &mut *vm };
    let cache = unsafe { &mut *cache_ptr };
    if cache.version == vm.globals_version {
        vm.push(cache.value.clone());
        return 0;
    }
    match vm.handle_get_global(name_idx as u16) {
        Ok(()) => {
            cache.value = vm.peek(0).clone();
            cache.version = vm.globals_version;
            0
        }
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

pub unsafe extern "C" fn jit_set_global(vm: *mut VM, name_idx: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    match vm.handle_set_global(name_idx as u16) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

pub unsafe extern "C" fn jit_define_global(vm: *mut VM, name_idx: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    match vm.handle_define_global(name_idx as u16) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

pub unsafe extern "C" fn jit_define_global_typed(
    vm: *mut VM,
    name_idx: u32,
    mutable: u32,
    type_idx: u32,
) -> u32 {
    let vm = unsafe { &mut *vm };
    match vm.handle_define_global_typed(name_idx as u16, mutable != 0, type_idx as u16) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

// ── Upvalues ───────────────────────────────────────────────────────────

pub unsafe extern "C" fn jit_get_upvalue(vm: *mut VM, idx: u32) {
    let vm = unsafe { &mut *vm };
    // handle_get_upvalue returns Result but can't actually fail today;
    // fold any future error into a stashed error rather than propagating.
    if let Err(e) = vm.handle_get_upvalue(idx as u16) {
        vm.jit.stash_error(e);
    }
}

pub unsafe extern "C" fn jit_set_upvalue(vm: *mut VM, idx: u32) {
    let vm = unsafe { &mut *vm };
    if let Err(e) = vm.handle_set_upvalue(idx as u16) {
        vm.jit.stash_error(e);
    }
}

pub unsafe extern "C" fn jit_close_upvalue(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.handle_close_upvalue();
}

// ── Closure ────────────────────────────────────────────────────────────

/// Executes the `Closure` opcode. `fn_idx` is the function-constant
/// index; `descriptors_offset` is the absolute bytecode offset of the
/// first upvalue descriptor.
pub unsafe extern "C" fn jit_op_closure(vm: *mut VM, fn_idx: u32, descriptors_offset: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    match vm.handle_closure(fn_idx as u16, descriptors_offset as usize) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

// ── Struct ops ─────────────────────────────────────────────────────────

pub unsafe extern "C" fn jit_op_struct_def(_vm: *mut VM, _const_idx: u32) {
    // `StructDef` is currently a no-op in the interpreter (structs are
    // loaded via `Constant`), so we mirror that.
}

pub unsafe extern "C" fn jit_op_struct_literal(
    vm: *mut VM,
    name_idx: u32,
    field_count: u32,
) -> u32 {
    let vm = unsafe { &mut *vm };
    match vm.handle_struct_literal(name_idx as u16, field_count as u8) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

pub unsafe extern "C" fn jit_op_get_field(vm: *mut VM, field_idx: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    match vm.handle_get_field(field_idx as u16) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

pub unsafe extern "C" fn jit_op_set_field(vm: *mut VM, field_idx: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    match vm.handle_set_field(field_idx as u16) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

pub unsafe extern "C" fn jit_op_define_method(
    vm: *mut VM,
    struct_name_idx: u32,
    method_count: u32,
) -> u32 {
    let vm = unsafe { &mut *vm };
    match vm.handle_define_method(struct_name_idx as u16, method_count as u8) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

/// MethodCall — like `call_value`, if a user frame was pushed we must
/// drive the interpreter until it returns.
pub unsafe extern "C" fn jit_op_method_call(vm: *mut VM, method_idx: u32, arg_count: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    let before_depth = vm.frames_len();
    match vm.handle_method_call(method_idx as u16, arg_count as u8) {
        Ok(()) => {
            if vm.frames_len() > before_depth {
                match vm.execute_until(before_depth) {
                    Ok(()) => 0,
                    Err(e) => {
                        vm.jit.stash_error(e);
                        1
                    }
                }
            } else {
                0
            }
        }
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

// ── Call — dispatch to `call_value` and, if a user frame was pushed,
//    drive the interpreter / JIT until that frame returns. ───────────────

/// Handle the `Call` opcode from JIT-compiled code.
///
/// Stack must be laid out as `[..., callee, arg0, ..., argN-1]`, exactly
/// what the interpreter's Call arm expects. On return the callee's result
/// is on top of the stack (consuming the callee + args).
///
/// Returns 0 on success, 1 on runtime error.
pub unsafe extern "C" fn jit_op_call(vm: *mut VM, arg_count: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    let before_depth = vm.frames_len();

    let call_result = match vm.call_closure_from_stack(arg_count as usize) {
        Ok(true) => Ok(()),
        Ok(false) => vm.call_value(arg_count as usize, &[]),
        Err(e) => Err(e),
    };

    match call_result {
        Ok(()) => {
            // `call_value` → `call_closure` pushes a frame for a user-defined
            // function. If the JIT didn't already run it to completion, the
            // frame is still on top and we drive the interpreter until it
            // returns. For builtin calls no frame was pushed and we're done.
            if vm.frames_len() > before_depth {
                match vm.execute_until(before_depth) {
                    Ok(()) => 0,
                    Err(e) => {
                        vm.jit.stash_error(e);
                        1
                    }
                }
            } else {
                0
            }
        }
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

// ── Return — mirrors the interpreter's `Return` opcode handler ────────

/// Pop the return value, pop the frame, close upvalues, truncate stack,
/// push the result back. After this runs, the stack has its previous
/// content plus the result on top, and `frames.len()` has decreased by 1.
pub unsafe extern "C" fn jit_op_return(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    let result = vm.pop();
    let frame = vm.frames_pop().expect("jit_op_return called with no frame");
    vm.close_upvalues(frame.slot_offset);
    vm.stack_truncate(frame.slot_offset);
    vm.push(result);
}

// ── Inline int fast-path support ───────────────────────────────────────

/// Return a raw pointer to the first `Value` on the VM stack. The JIT's
/// inline int fast path reads the top two values' tag bytes through this
/// pointer. Valid only for the duration of one opcode emission — any
/// other runtime helper that might reallocate the stack invalidates it.
pub unsafe extern "C" fn jit_stack_as_mut_ptr(vm: *mut VM) -> *mut crate::vm::value::Value {
    let vm = unsafe { &mut *vm };
    vm.stack_as_mut_ptr()
}

pub unsafe extern "C" fn jit_stack_len(vm: *mut VM) -> u64 {
    let vm = unsafe { &mut *vm };
    vm.stack_len() as u64
}

/// Commit the inline int fast path: pop the top Value (which the fast
/// path already overwrote with the result at the `stack_len - 2` slot
/// via direct memory write) and truncate the stack by one.
pub unsafe extern "C" fn jit_stack_pop_one(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.stack_shrink(1);
}

/// Shrink the stack by `n`. Used by the fused compare+branch fast path to
/// drop both operands in a single helper call after the integer icmp.
pub unsafe extern "C" fn jit_stack_pop_n(vm: *mut VM, n: u32) {
    let vm = unsafe { &mut *vm };
    vm.stack_shrink(n as usize);
}

/// Commit the inline int fast path for comparison opcodes: pop the top
/// two Values and push a `Value::Boolean(result != 0)`. Drop handling
/// is done in Rust so refcount semantics stay correct even for the
/// (never-taken on this fast path) case where the popped values were
/// heap-backed.
pub unsafe extern "C" fn jit_replace_top2_with_bool(vm: *mut VM, result: u32) {
    let vm = unsafe { &mut *vm };
    vm.pop();
    vm.pop();
    vm.push(Value::Boolean(result != 0));
}

// ── Upvalue type compile-time sanity (avoid unused import warning) ─────

#[allow(dead_code)]
fn _unused_upvalue_reference() -> Option<Upvalue> {
    None
}
