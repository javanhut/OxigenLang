//! `extern "C"` runtime helpers invoked by JIT-compiled code.
//!
//! Every helper takes `*mut VM` as its first argument plus any per-op
//! immediates. Erroring helpers return `u32` status codes (0 = ok, 1 = runtime
//! error); non-erroring helpers return no value. Each helper wraps the
//! interpreter logic in `core/src/vm/mod.rs`.

#![cfg(feature = "jit")]

use crate::vm::VM;
use crate::vm::value::{Upvalue, Value};
use std::cell::RefCell;
use std::rc::Rc;

use super::engine::HelperCounter;

/// Debug check of a specialized call site's frame state.
/// Args:
/// - `cache_ptr`: the per-call-site `CallCacheEntry` raw pointer.
/// - `closure_ptr`: the `*const ObjClosure` passed as the spec entry's closure
///   register argument.
/// - `phase`: 0 = pre-call, 1 = post-success.
/// - `expected_slot_offset`: the JitFrame slot_offset the spec body should
///   observe.
/// - `expected_jit_frame_len`: phase 0 — caller's `jit_frame_view.len` after
///   pushing the callee frame; phase 1 — its value after the callee popped its
///   own frame.
/// - `arg_count`: number of i64 args the caller passed in registers.
pub unsafe extern "C" fn jit_dbg_check_spec_call(
    vm: *mut VM,
    cache_ptr: *const super::engine::CallCacheEntry,
    closure_ptr: *const crate::vm::value::ObjClosure,
    phase: u32,
    expected_slot_offset: i64,
    expected_jit_frame_len: i64,
    arg_count: u32,
) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::DbgCheckSpecCall);

    macro_rules! fail {
        ($($arg:tt)*) => {{
            eprintln!("[spec_call_dbg phase={}] {}", phase, format!($($arg)*));
            return 1;
        }};
    }

    if cache_ptr.is_null() {
        fail!("cache_ptr is null");
    }
    if closure_ptr.is_null() {
        fail!("closure_ptr is null");
    }

    let cache = unsafe { &*cache_ptr };
    let cache_closure_raw = cache.closure_raw as *const u8;
    let derived = closure_ptr as *const u8;
    let expected_obj = unsafe { cache_closure_raw.add(crate::vm::value::RC_VALUE_OFFSET) }
        as *const crate::vm::value::ObjClosure;
    if derived != expected_obj as *const u8 {
        fail!(
            "closure_ptr ({:p}) != cache.closure_raw + RC_VALUE_OFFSET ({:p})",
            derived,
            expected_obj
        );
    }

    let closure = unsafe { &*closure_ptr };

    if phase == 0 {
        let kind = closure.specialized_kind.get();
        if kind != crate::vm::value::SPECIALIZED_KIND_NATIVE_INT_BODY_WITH_CLOSURE {
            fail!(
                "closure.specialized_kind = {} (expected {} = NATIVE_INT_BODY_WITH_CLOSURE)",
                kind,
                crate::vm::value::SPECIALIZED_KIND_NATIVE_INT_BODY_WITH_CLOSURE
            );
        }
        if closure.specialized_thunk.get().is_none() {
            fail!("closure.specialized_thunk is None despite kind=3");
        }
        let cached_arity = closure.specialized_arity.get();
        if cached_arity as u32 != arg_count {
            fail!(
                "closure.specialized_arity = {} but caller arg_count = {}",
                cached_arity,
                arg_count
            );
        }

        let stack_len = vm.stack_view.len as i64;
        if stack_len != expected_slot_offset + 1 {
            fail!(
                "stack_view.len = {} but expected slot_offset + 1 = {}",
                stack_len,
                expected_slot_offset + 1
            );
        }

        let slot_idx = expected_slot_offset as usize;
        vm.sync_stack_from_view();
        if slot_idx >= vm.stack_len() {
            fail!(
                "expected_slot_offset {} out of bounds (stack_view.len {})",
                slot_idx,
                vm.stack_view.len
            );
        }
        let stack_val = vm.stack_at(slot_idx);
        if let Some(c) = stack_val.as_closure() {
            let c_ref: *const Rc<crate::vm::value::ObjClosure> = c;
            let raw_rc = unsafe { *(c_ref as *const *const crate::vm::value::ObjClosure) };
            if raw_rc != cache.closure_raw {
                fail!(
                    "stack[{}] closure raw ({:p}) != cache.closure_raw ({:p})",
                    slot_idx,
                    raw_rc,
                    cache.closure_raw
                );
            }
        } else {
            fail!(
                "stack[{}] is not Value::Closure: tag={:?}",
                slot_idx,
                std::mem::discriminant(stack_val)
            );
        }

        let jit_len = vm.jit_frame_view.len as i64;
        if jit_len != expected_jit_frame_len {
            fail!(
                "jit_frame_view.len = {} but expected {}",
                jit_len,
                expected_jit_frame_len
            );
        }
    } else if phase == 1 {
        let stack_len = vm.stack_view.len as i64;
        if stack_len != expected_slot_offset {
            fail!(
                "post-call stack_view.len = {} but expected slot_offset = {} (callee Return should truncate to slot_offset)",
                stack_len,
                expected_slot_offset
            );
        }
        let jit_len = vm.jit_frame_view.len as i64;
        if jit_len != expected_jit_frame_len {
            fail!(
                "post-call jit_frame_view.len = {} but expected {} (callee Return should pop the frame I pushed)",
                jit_len,
                expected_jit_frame_len
            );
        }
    } else {
        fail!("unknown phase {}", phase);
    }

    0
}

/// Drive the interpreter for the just-pushed frame until it returns.
pub unsafe extern "C" fn jit_run_via_interpreter(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::RunViaInterpreter);
    vm.sync_stack_from_view();
    let stop_depth = vm.jit.current_stop_depth();
    match vm.execute_until(stop_depth) {
        Ok(()) => 0,
        Err(err) => {
            vm.jit.stash_error(err);
            1
        }
    }
}

pub unsafe extern "C" fn jit_push_constant(vm: *mut VM, idx: u32) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::PushConstant);
    vm.sync_stack_from_view();
    let value = vm.current_constant(idx as u16);
    vm.push(value);
}

pub unsafe extern "C" fn jit_push_integer_inline(vm: *mut VM, value: i64) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::PushIntegerInline);
    vm.sync_stack_from_view();
    vm.push(Value::Integer(value));
}

pub unsafe extern "C" fn jit_push_float_inline(vm: *mut VM, bits: u64) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::PushFloatInline);
    vm.sync_stack_from_view();
    vm.push(Value::Float(f64::from_bits(bits)));
}

pub unsafe extern "C" fn jit_pop(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::Pop);
    vm.sync_stack_from_view();
    vm.pop();
}

pub unsafe extern "C" fn jit_dup(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::Dup);
    vm.sync_stack_from_view();
    let top = vm.peek(0).clone();
    vm.push(top);
}

pub unsafe extern "C" fn jit_build_array(vm: *mut VM, count: u32) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::BuildArray);
    vm.sync_stack_from_view();
    let count = count as usize;
    let mut elements = Vec::with_capacity(count);
    for _ in 0..count {
        elements.push(vm.pop());
    }
    elements.reverse();
    vm.push(Value::Array(Rc::new(RefCell::new(elements.into()))));
}

pub unsafe extern "C" fn jit_op_index_fast_array_int(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::IndexFastArrayInt);
    vm.sync_stack_from_view();
    let index = vm.pop();
    let collection = vm.pop();

    if let (Value::Array(arr), Value::Integer(i)) = (&collection, &index) {
        let borrowed = arr.borrow();
        let len = borrowed.len() as i64;
        let idx = if *i < 0 {
            len.checked_add(*i)
        } else {
            Some(*i)
        };
        if let Some(idx) = idx
            && (0..len).contains(&idx)
        {
            let value = borrowed[idx as usize].clone();
            drop(borrowed);
            vm.push(value);
            vm.jit.record_array_index_fast_hit();
            return 0;
        }
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

/// `each`-loop length: pops the iterable, pushes its length. `[it] -> len`.
/// Mirrors the `IterLen` interpreter arm via the shared `VM::iter_len`.
pub unsafe extern "C" fn jit_op_iter_len(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.sync_stack_from_view();
    let iterable = vm.pop();
    match vm.iter_len(&iterable) {
        Ok(len) => {
            vm.push(Value::Integer(len));
            0
        }
        Err(err) => {
            vm.jit.stash_error(err);
            1
        }
    }
}

/// `lock`: `[value] -> [locked copy]`. Mirrors the `Lock` interpreter arm.
pub unsafe extern "C" fn jit_op_lock(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.sync_stack_from_view();
    let value = vm.pop();
    let locked = vm.lock_value(&value);
    vm.push(locked);
    0
}

/// `m[k]` read for `m[k]++`: like `Index`, but a missing map key is an error.
/// `[coll, idx] -> [value]`. Mirrors the `IndexExisting` interpreter arm.
pub unsafe extern "C" fn jit_op_index_existing(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.sync_stack_from_view();
    let index = vm.pop();
    let collection = vm.pop();
    match vm.eval_index_existing(collection, index) {
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

/// `arr[i] = v` / `map[k] = v`: pops value+index+collection, assigns in
/// place, pushes nothing. `[coll, idx, val] -> []`. Mirrors the
/// `IndexAssign` interpreter arm via the shared `VM::eval_index_assign`.
pub unsafe extern "C" fn jit_op_index_assign(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.sync_stack_from_view();
    let value = vm.pop();
    let index = vm.pop();
    let collection = vm.pop();
    match vm.eval_index_assign(collection, index, value) {
        Ok(()) => 0,
        Err(err) => {
            vm.jit.stash_error(err);
            1
        }
    }
}

/// `each`-loop element: pops index+iterable, pushes `iterable[index]`.
/// `[it, idx] -> elem`. Mirrors `IterGet` via the shared `VM::iter_get`.
pub unsafe extern "C" fn jit_op_iter_get(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.sync_stack_from_view();
    let index = vm.pop();
    let iterable = vm.pop();
    match vm.iter_get(&iterable, &index) {
        Ok(val) => {
            vm.push(val);
            0
        }
        Err(err) => {
            vm.jit.stash_error(err);
            1
        }
    }
}

/// `each k, v` step: pops index+iterable, pushes key then value.
/// `[it, idx] -> [key, val]`. Mirrors `IterEntry` via `VM::iter_entry`.
pub unsafe extern "C" fn jit_op_iter_entry(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.sync_stack_from_view();
    let index = vm.pop();
    let iterable = vm.pop();
    match vm.iter_entry(&iterable, &index) {
        Ok((key, val)) => {
            vm.push(key);
            vm.push(val);
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
    vm.jit.bump_helper(HelperCounter::TypeWrap);
    vm.sync_stack_from_view();
    let type_name = vm.current_constant(type_idx as u16);
    let value = vm.pop();

    let Some(target) = type_name.as_string().cloned() else {
        vm.jit
            .stash_error(vm.runtime_error("invalid type name in TypeWrap"));
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

/// `CheckReturn`: the VM stack's top against the declared return type. The
/// value stays on the stack for the `Return` that follows.
pub unsafe extern "C" fn jit_check_return(vm: *mut VM, type_idx: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.sync_stack_from_view();
    let type_name = vm.current_constant(type_idx as u16);
    let Some(expected) = type_name.as_string() else {
        vm.jit
            .stash_error(vm.runtime_error("invalid type name in CheckReturn"));
        return 1;
    };
    match vm.check_return_type(expected) {
        Ok(()) => 0,
        Err(err) => {
            vm.jit.stash_error(err);
            1
        }
    }
}

/// `RangeLimit` when its bounds aren't both staged as ints: [start, end] ->
/// `[limit]` on the VM stack, or the `range` builtin's error.
pub unsafe extern "C" fn jit_range_limit(vm: *mut VM, step_idx: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.sync_stack_from_view();
    let step = match vm.current_constant(step_idx as u16) {
        Value::Integer(step) => step,
        _ => {
            vm.jit
                .stash_error(vm.runtime_error("invalid step constant in RangeLimit"));
            return 1;
        }
    };
    match vm.handle_range_limit(step) {
        Ok(()) => 0,
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
    vm.jit.bump_helper(HelperCounter::LocalAddArrayModIndex);
    vm.sync_stack_from_view();
    if modulus == 0 {
        vm.jit.stash_error(vm.runtime_error_hint(
            "modulo by zero",
            "ensure the divisor is not zero before using %",
        ));
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

pub unsafe extern "C" fn jit_struct_field_add_const(
    vm: *mut VM,
    self_slot: u32,
    field_idx: u32,
    addend: i64,
    cache_ptr: *mut super::engine::FieldCacheEntry,
) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::StructFieldAddConst);
    vm.sync_stack_from_view();
    let base = vm.current_slot_offset();
    let self_val = vm.stack_slot(base + self_slot as usize).clone();

    if let Some(inst) = self_val.as_struct_instance() {
        let field_name = vm.current_constant(field_idx as u16);
        if let Some(fname) = field_name.as_string()
            && let Some(&idx) = inst.layout.indices.get(fname.as_ref())
            && (!inst.layout.slots[idx].2 || vm.hidden_field_access_allowed(inst))
            && !inst.locked.get()
        {
            unsafe {
                let slot = inst.fields.ptr.add(idx);
                if let Some(n) = (*slot).as_integer() {
                    let new = n.wrapping_add(addend);
                    let payload_ptr = (slot as *mut u8)
                        .add(crate::vm::value::VALUE_INT_PAYLOAD_OFFSET)
                        as *mut i64;
                    *payload_ptr = new;
                    populate_field_cache(cache_ptr, inst, idx);
                    return 0;
                }
            }
        }
    }

    match vm.handle_get_field_from_value(self_val.clone(), field_idx as u16) {
        Ok(current) => match vm.binary_add(current, Value::Integer(addend)) {
            Ok(sum) => match vm.handle_set_field_on_value(self_val, field_idx as u16, sum) {
                Ok(()) => 0,
                Err(err) => {
                    vm.jit.stash_error(err);
                    1
                }
            },
            Err(err) => {
                vm.jit.stash_error(err);
                1
            }
        },
        Err(err) => {
            vm.jit.stash_error(err);
            1
        }
    }
}

/// Populate the peephole's `FieldCacheEntry` with the struct-def identity and
/// resolved layout index for the JIT's inline hit path. Invoked from the
/// Integer fast-path branch of `jit_struct_field_add_const` / `_local`.
#[inline]
unsafe fn populate_field_cache(
    cache_ptr: *mut super::engine::FieldCacheEntry,
    inst: &std::rc::Rc<crate::vm::value::ObjStructInstance>,
    field_index: usize,
) {
    if cache_ptr.is_null() {
        return;
    }
    let cache = unsafe { &mut *cache_ptr };
    let def_raw: *const crate::vm::value::ObjStructDef = unsafe {
        *(&inst.def as *const std::rc::Rc<crate::vm::value::ObjStructDef>
            as *const *const crate::vm::value::ObjStructDef)
    };
    if cache.struct_def_raw == def_raw
        && matches!(cache.kind, super::engine::FieldCacheKind::InstanceField)
    {
        return;
    }
    cache.struct_def_raw = def_raw;
    cache.field_index = field_index as u32;
    cache.kind = super::engine::FieldCacheKind::InstanceField;
    cache.closure_raw = std::ptr::null();
    cache.thunk_raw = std::ptr::null();
    cache.struct_def = Some(std::rc::Rc::clone(&inst.def));
    cache.closure = None;
}

pub unsafe extern "C" fn jit_struct_field_add_local(
    vm: *mut VM,
    self_slot: u32,
    field_idx: u32,
    rhs_slot: u32,
    cache_ptr: *mut super::engine::FieldCacheEntry,
) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::StructFieldAddLocal);
    vm.sync_stack_from_view();
    let base = vm.current_slot_offset();
    let self_val = vm.stack_slot(base + self_slot as usize).clone();
    let rhs_val = vm.stack_slot(base + rhs_slot as usize).clone();

    if let (Some(inst), Some(rhs)) = (self_val.as_struct_instance(), rhs_val.as_integer()) {
        let field_name = vm.current_constant(field_idx as u16);
        if let Some(fname) = field_name.as_string()
            && let Some(&idx) = inst.layout.indices.get(fname.as_ref())
            && (!inst.layout.slots[idx].2 || vm.hidden_field_access_allowed(inst))
            && !inst.locked.get()
        {
            unsafe {
                let slot = inst.fields.ptr.add(idx);
                if let Some(n) = (*slot).as_integer() {
                    let new = n.wrapping_add(rhs);
                    let payload_ptr = (slot as *mut u8)
                        .add(crate::vm::value::VALUE_INT_PAYLOAD_OFFSET)
                        as *mut i64;
                    *payload_ptr = new;
                    populate_field_cache(cache_ptr, inst, idx);
                    return 0;
                }
            }
        }
    }

    match vm.handle_get_field_from_value(self_val.clone(), field_idx as u16) {
        Ok(current) => match vm.binary_add(current, rhs_val) {
            Ok(sum) => match vm.handle_set_field_on_value(self_val, field_idx as u16, sum) {
                Ok(()) => 0,
                Err(err) => {
                    vm.jit.stash_error(err);
                    1
                }
            },
            Err(err) => {
                vm.jit.stash_error(err);
                1
            }
        },
        Err(err) => {
            vm.jit.stash_error(err);
            1
        }
    }
}

/// Return the current frame's `slot_offset`. Stable for the lifetime of a
/// thunk activation.
pub unsafe extern "C" fn jit_current_slot_offset(vm: *mut VM) -> i64 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::CurrentSlotOffset);
    vm.sync_stack_from_view();
    vm.current_slot_offset() as i64
}

pub unsafe extern "C" fn jit_get_local(vm: *mut VM, slot: u32) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::GetLocal);
    vm.sync_stack_from_view();
    let base = vm.current_slot_offset();
    let value = vm.stack_slot(base + slot as usize).clone();
    vm.push(value);
}

pub unsafe extern "C" fn jit_set_local(vm: *mut VM, slot: u32) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::SetLocal);
    vm.sync_stack_from_view();
    let base = vm.current_slot_offset();
    let value = vm.peek(0).clone();
    vm.set_stack_slot(base + slot as usize, value);
}

/// Type-lock-enforcing variant of `jit_set_local`. Returns 0 on success, 1 if
/// the stored value violates the lock (the error is stashed for the thunk to
/// surface).
pub unsafe extern "C" fn jit_set_local_checked(vm: *mut VM, slot: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::SetLocal);
    vm.sync_stack_from_view();
    if let Err(e) = vm.check_local_lock(&vm.active_closure().function, slot as usize) {
        vm.jit.stash_error(e);
        return 1;
    }
    let base = vm.current_slot_offset();
    let value = vm.peek(0).clone();
    vm.set_stack_slot(base + slot as usize, value);
    0
}

/// Recursion-depth guard for JIT call paths that push a `JitFrame` inline.
/// Must be called immediately before the inline frame push. Returns 0 when
/// there is room for one more frame, 1 (with the error stashed for the thunk
/// to surface) when the call would overflow the frame limit.
pub unsafe extern "C" fn jit_check_recursion_depth(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    let frame_overflow =
        vm.frames_len().saturating_add(vm.jit_frame_len()) >= crate::vm::FRAMES_MAX;
    let stack_overflow = vm.stack_view.len >= crate::vm::STACK_MAX - crate::vm::STACK_GUARD_MARGIN;
    if frame_overflow || stack_overflow {
        let e = vm.runtime_error_hint(
            "stack overflow",
            "check for infinite recursion or deeply nested calls",
        );
        vm.jit.stash_error(e);
        return 1;
    }
    0
}

/// Called by a compiled loop back-edge that has to stop: an interrupt is
/// pending, or the budget is spent. Stashes the error and returns 1; the
/// back-edge never resumes the loop after calling this.
pub unsafe extern "C" fn jit_loop_stop(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    let err = match vm.loop_limit_step() {
        Err(e) => e,
        Ok(()) => vm.runtime_error("internal error: a loop stopped with no limit reached"),
    };
    vm.jit.stash_error(err);
    1
}

macro_rules! binop_fallible {
    ($name:ident, $method:ident, $counter:ident) => {
        pub unsafe extern "C" fn $name(vm: *mut VM) -> u32 {
            let vm = unsafe { &mut *vm };
            vm.jit.bump_helper(HelperCounter::$counter);
            vm.sync_stack_from_view();
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

binop_fallible!(jit_op_add, binary_add, Add);
binop_fallible!(jit_op_sub, binary_sub, Sub);
binop_fallible!(jit_op_mul, binary_mul, Mul);
binop_fallible!(jit_op_div, binary_div, Div);
binop_fallible!(jit_op_mod, binary_mod, Mod);
binop_fallible!(jit_op_lt, compare_less, Lt);
binop_fallible!(jit_op_le, compare_less_equal, Le);
binop_fallible!(jit_op_gt, compare_greater, Gt);
binop_fallible!(jit_op_ge, compare_greater_equal, Ge);
binop_fallible!(jit_op_band, binary_band, OpBand);
binop_fallible!(jit_op_bor, binary_bor, OpBor);
binop_fallible!(jit_op_bxor, binary_bxor, OpBxor);
binop_fallible!(jit_op_shl, binary_shl, OpShl);
binop_fallible!(jit_op_shr, binary_shr, OpShr);

pub unsafe extern "C" fn jit_op_bnot(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpBnot);
    vm.sync_stack_from_view();
    let v = vm.pop();
    match vm.unary_bnot(v) {
        Ok(result) => {
            vm.push(result);
            0
        }
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

pub unsafe extern "C" fn jit_op_log(vm: *mut VM, flags: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpLog);
    vm.sync_stack_from_view();
    vm.handle_log(flags as u8);
    0
}

pub unsafe extern "C" fn jit_op_eq(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::Eq);
    vm.sync_stack_from_view();
    let b = vm.pop();
    let a = vm.pop();
    let eq = vm.values_equal_forced(a, b);
    vm.push(Value::Boolean(eq));
}

pub unsafe extern "C" fn jit_op_ne(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::Ne);
    vm.sync_stack_from_view();
    let b = vm.pop();
    let a = vm.pop();
    let eq = vm.values_equal_forced(a, b);
    vm.push(Value::Boolean(!eq));
}

pub unsafe extern "C" fn jit_op_not(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::Not);
    vm.sync_stack_from_view();
    let v = vm.pop();
    let v = vm.force(v);
    vm.push(Value::Boolean(!v.is_truthy()));
}

pub unsafe extern "C" fn jit_op_negate(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::Negate);
    vm.sync_stack_from_view();
    let v = vm.pop();
    match vm.unary_negate(v) {
        Ok(negated) => {
            vm.push(negated);
            0
        }
        Err(err) => {
            vm.jit.stash_error(err);
            1
        }
    }
}

/// Peek at top-of-stack (does NOT pop). Returns 1 if truthy, 0 if not.
/// Matches the semantics of `JumpIfFalse`/`JumpIfTrue`.
pub unsafe extern "C" fn jit_peek_truthy(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::PeekTruthy);
    vm.sync_stack_from_view();
    if vm.peek(0).is_truthy() { 1 } else { 0 }
}

/// Pop top-of-stack, return 1 if truthy, 0 if not. Matches
/// `PopJumpIfFalse`.
pub unsafe extern "C" fn jit_pop_truthy(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::PopTruthy);
    vm.sync_stack_from_view();
    let v = vm.pop();
    if v.is_truthy() { 1 } else { 0 }
}

pub unsafe extern "C" fn jit_get_global(vm: *mut VM, name_idx: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::GetGlobal);
    vm.sync_stack_from_view();
    match vm.handle_get_global(name_idx as u16) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

/// GetGlobal with a per-call-site inline cache. When the cache's recorded
/// `globals_version` matches the VM's current version, pushes a clone of the
/// cached value; otherwise performs the generic lookup and repopulates the
/// cache.
///
/// `cache_ptr` is allocated by `JitInner::alloc_global_cache()`.
pub unsafe extern "C" fn jit_get_global_ic(
    vm: *mut VM,
    cache_ptr: *mut super::engine::GlobalCacheEntry,
    name_idx: u32,
) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::GetGlobalIc);
    vm.sync_stack_from_view();
    let cache = unsafe { &mut *cache_ptr };
    if vm.has_active_module_globals() {
        return match vm.handle_get_global(name_idx as u16) {
            Ok(()) => 0,
            Err(e) => {
                vm.jit.stash_error(e);
                1
            }
        };
    }
    if cache.version == unsafe { *cache.version_cell } {
        vm.push(cache.value.clone());
        return 0;
    }
    match vm.handle_get_global(name_idx as u16) {
        Ok(()) => {
            cache.value = vm.peek(0).clone();
            if let Ok(name) = vm.name_constant(name_idx as u16) {
                let cell = vm.jit.global_version_cell(name.as_str());
                cache.version_cell = cell;
                cache.version = unsafe { *cell };
            }
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
    vm.jit.bump_helper(HelperCounter::SetGlobal);
    vm.sync_stack_from_view();
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
    vm.jit.bump_helper(HelperCounter::DefineGlobal);
    vm.sync_stack_from_view();
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
    vm.jit.bump_helper(HelperCounter::DefineGlobalTyped);
    vm.sync_stack_from_view();
    match vm.handle_define_global_typed(name_idx as u16, mutable as u8, type_idx as u16) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

pub unsafe extern "C" fn jit_get_upvalue(vm: *mut VM, idx: u32) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::GetUpvalue);
    vm.sync_stack_from_view();
    let mut closed_int_value: Option<i64> = None;
    {
        let upvalue = &vm.active_closure().upvalues[idx as usize];
        if let crate::vm::value::Upvalue::Closed(crate::vm::value::Value::Integer(n)) =
            &*upvalue.borrow()
        {
            closed_int_value = Some(*n);
        }
    }
    arm_upvalue_int_cache(vm.active_closure(), idx as usize, closed_int_value);
    if let Some(c) = vm.jit.counters_ptr_opt() {
        let c = unsafe { &*c };
        c.get_upvalue_calls.set(c.get_upvalue_calls.get() + 1);
        if closed_int_value.is_some() {
            c.get_upvalue_closed_integer_hit
                .set(c.get_upvalue_closed_integer_hit.get() + 1);
        } else {
            c.get_upvalue_fallback.set(c.get_upvalue_fallback.get() + 1);
        }
    }
    if let Err(e) = vm.handle_get_upvalue(idx as u16) {
        vm.jit.stash_error(e);
    }
}

pub unsafe extern "C" fn jit_set_upvalue(vm: *mut VM, idx: u32) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::SetUpvalue);
    vm.sync_stack_from_view();
    if let Some(c) = vm.jit.counters_ptr_opt() {
        let c = unsafe { &*c };
        c.set_upvalue_calls.set(c.set_upvalue_calls.get() + 1);
    }
    if let Err(e) = vm.handle_set_upvalue(idx as u16) {
        vm.jit.stash_error(e);
        return;
    }
    let mut closed_int_value: Option<i64> = None;
    {
        let upvalue = &vm.active_closure().upvalues[idx as usize];
        if let crate::vm::value::Upvalue::Closed(crate::vm::value::Value::Integer(n)) =
            &*upvalue.borrow()
        {
            closed_int_value = Some(*n);
        }
    }
    arm_upvalue_int_cache(vm.active_closure(), idx as usize, closed_int_value);
}

/// Record upvalue `idx`'s current shape in `closure`'s int cache. A cache
/// filled at an older write epoch is cleared first.
fn arm_upvalue_int_cache(
    closure: &crate::vm::value::ObjClosure,
    idx: usize,
    closed_int_value: Option<i64>,
) {
    if idx >= closure.upvalue_int_kinds.len() {
        return;
    }
    let epoch = crate::jit::UPVALUE_WRITE_EPOCH.load(std::sync::atomic::Ordering::Relaxed);
    let values = &closure.upvalue_int_values;
    if values[0].get() != epoch {
        for kind in closure.upvalue_int_kinds.iter() {
            kind.set(0);
        }
        values[0].set(epoch);
    }
    if let Some(n) = closed_int_value {
        values[idx + 1].set(n);
        closure.upvalue_int_kinds[idx].set(1);
    } else {
        closure.upvalue_int_kinds[idx].set(0);
    }
}

/// Typed-parameter check run by a compiled function's generic entry. Applies
/// the interpreter's rule and message; the error is reported from the caller.
pub unsafe extern "C" fn jit_check_param_types(vm: *mut VM) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.sync_stack_from_view();
    let function = Rc::clone(&vm.active_closure().function);
    let base = vm.current_slot_offset() + 1;
    for (i, param) in function.params.iter().enumerate() {
        let Some(expected_ty) = param.type_ann.as_deref() else {
            continue;
        };
        let val = vm.stack_slot(base + i);
        if (param.optional || param.has_default) && matches!(val, Value::None) {
            continue;
        }
        if !val.effective_type_is(expected_ty)
            && !crate::vm::type_matches(expected_ty, val.type_name())
        {
            let actual = val.effective_type_name();
            let e = vm.error_in_jit_caller(
                &format!(
                    "type mismatch for parameter '{}': expected {}, got {}",
                    param.name, expected_ty, actual
                ),
                &format!("parameter '{}' requires type {}", param.name, expected_ty),
            );
            vm.jit.stash_error(e);
            return 1;
        }
    }
    0
}

pub unsafe extern "C" fn jit_close_upvalue(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::CloseUpvalue);
    vm.sync_stack_from_view();
    vm.handle_close_upvalue();
}

/// Executes the `Closure` opcode. `fn_idx` is the function-constant
/// index; `descriptors_offset` is the absolute bytecode offset of the
/// first upvalue descriptor.
pub unsafe extern "C" fn jit_op_closure(vm: *mut VM, fn_idx: u32, descriptors_offset: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpClosure);
    vm.sync_stack_from_view();
    match vm.handle_closure(fn_idx as u16, descriptors_offset as usize) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

pub unsafe extern "C" fn jit_op_struct_def(vm: *mut VM, _const_idx: u32) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpStructDef);
}

pub unsafe extern "C" fn jit_op_struct_literal(
    vm: *mut VM,
    name_idx: u32,
    field_count: u32,
) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpStructLiteral);
    vm.sync_stack_from_view();
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
    vm.jit.bump_helper(HelperCounter::OpGetField);
    vm.sync_stack_from_view();
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
    vm.jit.bump_helper(HelperCounter::OpSetField);
    vm.sync_stack_from_view();
    match vm.handle_set_field(field_idx as u16) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

pub unsafe extern "C" fn jit_op_get_field_ic_miss(
    vm: *mut VM,
    field_idx: u32,
    cache_ptr: *mut super::engine::FieldCacheEntry,
) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpGetFieldIcMiss);
    vm.sync_stack_from_view();
    let cache = unsafe { &mut *cache_ptr };
    let object = vm.peek(0).clone();
    let field_name = vm.current_constant(field_idx as u16);
    let status = match vm.handle_get_field(field_idx as u16) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    };
    if status != 0 {
        return status;
    }

    cache.kind = super::engine::FieldCacheKind::Invalid;
    cache.struct_def_raw = std::ptr::null();
    cache.closure_raw = std::ptr::null();
    cache.thunk_raw = std::ptr::null();
    cache.struct_def = None;
    cache.closure = None;

    let Some(fname) = field_name.as_string().cloned() else {
        return 0;
    };
    if let Some(inst) = object.as_struct_instance() {
        let def_raw: *const crate::vm::value::ObjStructDef = unsafe {
            *(&inst.def as *const Rc<crate::vm::value::ObjStructDef>
                as *const *const crate::vm::value::ObjStructDef)
        };
        cache.struct_def_raw = def_raw;
        cache.struct_def = Some(Rc::clone(&inst.def));
        if let Some(&idx) = inst.layout.indices.get(fname.as_ref()) {
            cache.field_index = idx as u32;
            cache.kind = super::engine::FieldCacheKind::InstanceField;
        } else if let Ok(method) = vm.find_struct_method(&inst.struct_name, &fname)
            && let Some(closure) = method.as_closure().cloned()
        {
            let closure_raw: *const crate::vm::value::ObjClosure = unsafe {
                *(&closure as *const Rc<crate::vm::value::ObjClosure>
                    as *const *const crate::vm::value::ObjClosure)
            };
            cache.kind = super::engine::FieldCacheKind::DefMethod;
            cache.closure_raw = closure_raw;
            cache.thunk_raw = closure
                .jit_thunk
                .get()
                .map(|thunk| thunk as *const ())
                .unwrap_or(std::ptr::null());
            cache.closure = Some(closure);
        }
    }
    0
}

pub unsafe extern "C" fn jit_op_set_field_ic_miss(
    vm: *mut VM,
    field_idx: u32,
    cache_ptr: *mut super::engine::FieldCacheEntry,
) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpSetFieldIcMiss);
    vm.sync_stack_from_view();
    let cache = unsafe { &mut *cache_ptr };
    let object = vm.stack_at(vm.stack_len() - 2).clone();
    let field_name = vm.current_constant(field_idx as u16);
    let status = match vm.handle_set_field(field_idx as u16) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    };
    if status != 0 {
        return status;
    }

    cache.kind = super::engine::FieldCacheKind::Invalid;
    cache.struct_def_raw = std::ptr::null();
    cache.struct_def = None;

    let Some(fname) = field_name.as_string().cloned() else {
        return 0;
    };
    if let Some(inst) = object.as_struct_instance()
        && let Some(&idx) = inst.layout.indices.get(fname.as_ref())
    {
        let def_raw: *const crate::vm::value::ObjStructDef = unsafe {
            *(&inst.def as *const Rc<crate::vm::value::ObjStructDef>
                as *const *const crate::vm::value::ObjStructDef)
        };
        cache.struct_def_raw = def_raw;
        cache.field_index = idx as u32;
        cache.kind = super::engine::FieldCacheKind::InstanceField;
        cache.struct_def = Some(Rc::clone(&inst.def));
    }
    0
}

pub unsafe extern "C" fn jit_op_define_method(
    vm: *mut VM,
    struct_name_idx: u32,
    method_count: u32,
) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpDefineMethod);
    vm.sync_stack_from_view();
    match vm.handle_define_method(struct_name_idx as u16, method_count as u8) {
        Ok(()) => 0,
        Err(e) => {
            vm.jit.stash_error(e);
            1
        }
    }
}

/// MethodCall with a per-call-site inline cache. On a hit (the instance's
/// struct def `Rc` matches the cached def) it dispatches directly to the
/// cached method closure. On a miss it resolves the method, populates the
/// cache, and dispatches.
#[inline(always)]
pub unsafe extern "C" fn jit_op_method_call_ic(
    vm: *mut VM,
    method_idx: u32,
    arg_count: u32,
    cache_ptr: *mut super::engine::MethodCacheEntry,
) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpMethodCallIc);
    vm.sync_stack_from_view();
    let cache = unsafe { &mut *cache_ptr };
    let ac = arg_count as usize;

    let instance_idx = vm.stack_len() - 1 - ac;

    let (struct_def, struct_name_owned) = match vm.stack_at(instance_idx).as_struct_instance() {
        Some(inst) => (Rc::clone(&inst.def), None::<String>),
        None => {
            return unsafe { jit_op_method_call(vm as *mut VM, method_idx, arg_count) };
        }
    };

    if let (Some(cached_def), Some(cached_closure)) =
        (cache.struct_def.as_ref(), cache.closure.as_ref())
        && Rc::ptr_eq(cached_def, &struct_def)
        && !cache.struct_def_raw.is_null()
    {
        let closure = Rc::clone(cached_closure);
        let owning_mg = struct_def.module_globals.get();
        let before_depth = vm.frames_len();
        return match vm.call_struct_method_with_closure(closure, ac, owning_mg) {
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
        };
    }

    let method_name_val = vm.current_constant(method_idx as u16);
    let Some(mname_rc) = method_name_val.as_string().cloned() else {
        return unsafe { jit_op_method_call(vm as *mut VM, method_idx, arg_count) };
    };
    let mname: String = mname_rc.to_string();
    let struct_name = struct_name_owned.unwrap_or_else(|| struct_def.name.clone());

    match vm.find_struct_method(&struct_name, &mname) {
        Ok(method) if method.as_closure().is_some() => {
            let closure = method.as_closure().unwrap().clone();
            let Some(thunk) = closure.jit_thunk.get() else {
                return unsafe { jit_op_method_call(vm as *mut VM, method_idx, arg_count) };
            };
            let def_raw: *const crate::vm::value::ObjStructDef = unsafe {
                *(&struct_def as *const Rc<crate::vm::value::ObjStructDef>
                    as *const *const crate::vm::value::ObjStructDef)
            };
            let closure_raw: *const crate::vm::value::ObjClosure = unsafe {
                *(&closure as *const Rc<crate::vm::value::ObjClosure>
                    as *const *const crate::vm::value::ObjClosure)
            };
            cache.struct_def_raw = def_raw;
            cache.closure_raw = closure_raw;
            cache.thunk_raw = thunk as *const ();
            cache.arity = closure.function.arity;
            cache.inline_kind = super::engine::MethodInlineKind::None;
            cache.inline_field_index = 0;
            cache.inline_addend = 0;
            if arg_count <= 1
                && let Some(info) = super::engine::detect_inline_method_info(&closure.function)
            {
                let expected_arity: u8 = match info.kind {
                    super::engine::MethodInlineKind::FieldAddConst => 1,
                    super::engine::MethodInlineKind::FieldAddLocal => 2,
                    super::engine::MethodInlineKind::None => u8::MAX,
                };
                if closure.function.arity == expected_arity
                    && let Some(inst) = vm.stack_at(instance_idx).as_struct_instance()
                    && let Some(&layout_idx) = inst.layout.indices.get(info.field_name.as_ref())
                {
                    cache.inline_kind = info.kind;
                    cache.inline_field_index = layout_idx as u32;
                    cache.inline_addend = info.addend;
                }
            }
            cache.struct_def = Some(Rc::clone(&struct_def));
            cache.closure = Some(Rc::clone(&closure));
            let owning_mg = struct_def.module_globals.get();
            let before_depth = vm.frames_len();
            match vm.call_struct_method_with_closure(closure, ac, owning_mg) {
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
        _ => unsafe { jit_op_method_call(vm as *mut VM, method_idx, arg_count) },
    }
}

/// MethodCall — like `call_value`, if a user frame was pushed the interpreter
/// is driven until it returns.
pub unsafe extern "C" fn jit_op_method_call(vm: *mut VM, method_idx: u32, arg_count: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpMethodCall);
    vm.sync_stack_from_view();
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

/// Handle the `Call` opcode from JIT-compiled code.
///
/// Stack must be laid out as `[..., callee, arg0, ..., argN-1]`, exactly
/// what the interpreter's Call arm expects. On return the callee's result
/// is on top of the stack (consuming the callee + args).
///
/// Returns 0 on success, 1 on runtime error.
#[inline(always)]
pub unsafe extern "C" fn jit_op_call(vm: *mut VM, arg_count: u32) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpCall);
    vm.sync_stack_from_view();
    let before_depth = vm.frames_len();

    let call_result = match vm.call_closure_from_stack(arg_count as usize) {
        Ok(true) => Ok(()),
        Ok(false) => vm.call_value(arg_count as usize, &[]),
        Err(e) => Err(e),
    };

    match call_result {
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

/// Hit path of the inline-IR Call guard. Pre-conditions (verified by the
/// caller's IR): the callee on stack is a Closure whose Rc pointer equals
/// `cache.closure_raw`, and `cache.thunk_raw` is a valid `CompiledThunk`
/// pointer. Pushes a frame and invokes the cached thunk.
#[inline(always)]
pub unsafe extern "C" fn jit_op_call_hit(
    vm: *mut VM,
    cache_ptr: *mut super::engine::CallCacheEntry,
) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpCallHit);
    vm.sync_stack_from_view();
    let cache = unsafe { &mut *cache_ptr };

    let Some(cached_rc) = cache._keeper.as_ref().map(Rc::clone) else {
        return unsafe { jit_op_call(vm as *mut VM, cache.arity as u32) };
    };

    let before_depth = vm.frames_len();
    match vm.call_closure_fast_path(&cached_rc) {
        Some(Ok(())) => {
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
        Some(Err(e)) => {
            vm.jit.stash_error(e);
            1
        }
        None => {
            cache.closure_raw = std::ptr::null();
            cache.thunk_raw = std::ptr::null();
            cache._keeper = None;
            unsafe { jit_op_call(vm as *mut VM, cache.arity as u32) }
        }
    }
}

/// Miss path of the inline-IR Call guard. Runs the generic `jit_op_call`
/// flow, then — on success and if the callee is a JIT-compiled closure —
/// populates the cache so subsequent calls hit the IR fast path.
#[inline(always)]
pub unsafe extern "C" fn jit_op_call_miss(
    vm: *mut VM,
    arg_count: u32,
    cache_ptr: *mut super::engine::CallCacheEntry,
) -> u32 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpCallMiss);
    vm.sync_stack_from_view();
    let cache = unsafe { &mut *cache_ptr };
    let ac = arg_count as usize;

    let before_depth = vm.frames_len();
    let callee_idx = vm.stack_len() - 1 - ac;
    let callee_candidate = vm.stack_at(callee_idx).as_closure().map(Rc::clone);

    let call_result = match vm.call_closure_from_stack(ac) {
        Ok(true) => Ok(()),
        Ok(false) => vm.call_value(ac, &[]),
        Err(e) => Err(e),
    };

    let status = match call_result {
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
    };

    if status == 0
        && let Some(c) = callee_candidate
        && c.jit_state.get() == 1
        && c.function.arity as usize == ac
        && let Some(thunk) = c.jit_thunk.get()
    {
        let rc_raw: *const crate::vm::value::ObjClosure = unsafe {
            *(&c as *const Rc<crate::vm::value::ObjClosure>
                as *const *const crate::vm::value::ObjClosure)
        };
        cache.closure_raw = rc_raw;
        cache.thunk_raw = thunk as *const ();
        cache.arity = c.function.arity;
        cache.specialized_kind = c.specialized_kind.get();
        cache.specialized_thunk = c.specialized_thunk.get().unwrap_or(std::ptr::null());
        match super::engine::detect_inline_closure_upvalue_add(&c.function) {
            Some(idx) => {
                cache.inline_kind = super::engine::ClosureInlineKind::AddUpvalueArg;
                cache.inline_upvalue_index = idx as u32;
            }
            None => {
                cache.inline_kind = super::engine::ClosureInlineKind::None;
                cache.inline_upvalue_index = 0;
            }
        }
        cache._keeper = Some(c);
    }

    status
}

/// Truncate the VM stack to `new_len`, dropping values above it.
/// Truncate the VM stack to `new_len`, dropping values above it.
pub unsafe extern "C" fn jit_stack_truncate(vm: *mut VM, new_len: i64) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::StackTruncate);
    vm.sync_stack_from_view();
    vm.stack_truncate(new_len as usize);
}

/// Pop the return value, pop the frame, close upvalues, truncate stack,
/// push the result back. After this runs, the stack has its previous
/// content plus the result on top, and `frames.len()` has decreased by 1.
pub unsafe extern "C" fn jit_op_return(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::OpReturn);
    vm.sync_stack_from_view();
    if let Some(c) = vm.jit.counters_ptr_opt() {
        let c = unsafe { &*c };
        c.jit_op_return_calls.set(c.jit_op_return_calls.get() + 1);
    }
    let result = vm.pop();
    let frame = vm
        .jit_frame_pop_raw()
        .expect("jit_op_return called with no jit frame");
    vm.close_upvalues(frame.slot_offset);
    vm.stack_truncate(frame.slot_offset);
    vm.push(result);
}

/// Return a raw pointer to the first `Value` on the VM stack. Valid only until
/// another runtime helper reallocates the stack.
pub unsafe extern "C" fn jit_stack_as_mut_ptr(vm: *mut VM) -> *mut crate::vm::value::Value {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::StackAsMutPtr);
    vm.sync_stack_from_view();
    vm.stack_as_mut_ptr()
}

pub unsafe extern "C" fn jit_stack_len(vm: *mut VM) -> u64 {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::StackLen);
    vm.sync_stack_from_view();
    vm.stack_len() as u64
}

/// Commit the inline int fast path: pop the top Value and truncate the stack
/// by one.
pub unsafe extern "C" fn jit_stack_pop_one(vm: *mut VM) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::StackPopOne);
    vm.sync_stack_from_view();
    vm.stack_shrink(1);
}

/// Shrink the stack by `n`.
pub unsafe extern "C" fn jit_stack_pop_n(vm: *mut VM, n: u32) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::StackPopN);
    vm.sync_stack_from_view();
    vm.stack_shrink(n as usize);
}

/// Commit a raw-IR stack rearrangement to the `Vec<Value>` backing store.
///
/// Extends the `Vec`'s logical length to cover `Value` slots written by the
/// inline MethodCall IR. Safety is delegated to `VM::stack_set_len_raw`; see
/// that method's doc comment for the invariants the caller must uphold.
#[inline(always)]
pub unsafe extern "C" fn jit_stack_commit_len(vm: *mut VM, new_len: u64) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::StackCommitLen);
    vm.sync_stack_from_view();
    unsafe { vm.stack_set_len_raw(new_len as usize) };
}

/// Commit the inline int fast path for comparison opcodes: pop the top two
/// Values and push a `Value::Boolean(result != 0)`.
pub unsafe extern "C" fn jit_replace_top2_with_bool(vm: *mut VM, result: u32) {
    let vm = unsafe { &mut *vm };
    vm.jit.bump_helper(HelperCounter::ReplaceTop2WithBool);
    vm.sync_stack_from_view();
    vm.pop();
    vm.pop();
    vm.push(Value::Boolean(result != 0));
}

#[allow(dead_code)]
fn _unused_upvalue_reference() -> Option<Upvalue> {
    None
}

/// Why a lean entry did not produce a value.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LeanExit {
    /// Recursion budget exhausted — report as a stack overflow.
    Overflow,
    /// This entry cannot be dispatched (arity outside the supported set). The
    /// caller must fall through, NOT report an error.
    Unsupported,
}

/// Where a lean entry's cold exit lands.
/// Where a lean entry's cold exit lands. Thread-local.
#[repr(C, align(16))]
pub struct LeanJmpBuf([u64; 64]);

unsafe extern "C" {
    #[link_name = "setjmp"]
    fn lean_setjmp(env: *mut LeanJmpBuf) -> i32;
    #[link_name = "longjmp"]
    fn lean_longjmp(env: *mut LeanJmpBuf, val: i32) -> !;
}

thread_local! {
    static LEAN_UNWIND: std::cell::UnsafeCell<LeanJmpBuf> =
        const { std::cell::UnsafeCell::new(LeanJmpBuf([0; 64])) };
}

/// Raw pointer to this thread's unwind target.
fn lean_unwind_target() -> *mut LeanJmpBuf {
    LEAN_UNWIND.with(|c| c.get())
}

/// Called from a lean entry when the recursion budget is exhausted. Never
/// returns.
pub unsafe extern "C" fn jit_lean_overflow() -> i64 {
    unsafe { lean_longjmp(lean_unwind_target(), 1) }
}

/// Invokes a lean entry with integer arguments.
///
/// `Ok(v)` on success. `Err(LeanExit::Overflow)` when the budget ran out, which
/// the caller reports as a "stack overflow". `Err(LeanExit::Unsupported)` means
/// this entry cannot be dispatched and the caller must fall through to a normal
/// entry.
///
/// The cold path leaves VM state unchanged: a lean entry never touches
/// `stack_view.len`, `jit_frame_view.len` or any `Rc`.
/// # Safety
/// `entry` must be a finalized lean entry of arity `args.len()`.
pub unsafe fn invoke_lean(entry: *const (), args: &[i64], budget: i64) -> Result<i64, LeanExit> {
    let rc = unsafe { lean_setjmp(lean_unwind_target()) };
    if rc != 0 {
        return Err(LeanExit::Overflow);
    }
    let v = unsafe {
        match args.len() {
            1 => {
                let f: extern "C" fn(i64, i64) -> i64 = std::mem::transmute(entry);
                f(args[0], budget)
            }
            2 => {
                let f: extern "C" fn(i64, i64, i64) -> i64 = std::mem::transmute(entry);
                f(args[0], args[1], budget)
            }
            3 => {
                let f: extern "C" fn(i64, i64, i64, i64) -> i64 = std::mem::transmute(entry);
                f(args[0], args[1], args[2], budget)
            }
            4 => {
                let f: extern "C" fn(i64, i64, i64, i64, i64) -> i64 = std::mem::transmute(entry);
                f(args[0], args[1], args[2], args[3], budget)
            }
            5 => {
                let f: extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64 =
                    std::mem::transmute(entry);
                f(args[0], args[1], args[2], args[3], args[4], budget)
            }
            6 => {
                let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
                    std::mem::transmute(entry);
                f(args[0], args[1], args[2], args[3], args[4], args[5], budget)
            }
            7 => {
                let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> i64 =
                    std::mem::transmute(entry);
                f(
                    args[0], args[1], args[2], args[3], args[4], args[5], args[6], budget,
                )
            }
            8 => {
                let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64, i64) -> i64 =
                    std::mem::transmute(entry);
                f(
                    args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], budget,
                )
            }
            _ => return Err(LeanExit::Unsupported),
        }
    };
    Ok(v)
}
