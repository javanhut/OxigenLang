# Closure Specialization Design Note

> **Status: design proposal.** Not yet implemented. Captures the
> remaining hot-path costs in `bench_closure` after the line-elision
> + slot-types fix + inline-GetLocal-Rc-bump optimization round.

## Why bench_closure is now bottlenecked on `op_return` + `get_upvalue` cache hit

After steps 1–4 (line-store elision, slot_types Pop-clear fix,
proven-int virt-int folding, inline GetLocal Rc-bump), the
`bench_closure` JIT stats look like:

```
specialized_entry_compiled:        1
specialized_entry_called:          0
self_recursion_direct_call:        0
get_upvalue_calls:                 1     ← cold-path cache populate
get_upvalue_inline_hit:            499,999
jit_op_return_calls:               500,003
call_ic_hit:                       499,999
call_ic_miss:                      3
spec_entry_rejected_param_not_int:        1
spec_entry_rejected_has_upvalue_op:       1

helper FFI calls (non-zero):
  op_return                  500,003
  op_call_miss                     3
  get_global_ic                    2
  define_global                    2
  type_wrap                        2
  get_upvalue                      1
  op_closure                       3
```

`get_local` is **0** (was 500,000 before step 4).

The remaining cost is dominated by:

1. **`op_return` × 500,003.** Every closure invocation ends with a
   Generic `Return`, which calls `jit_op_return` to pop the result,
   pop the JitFrame, run `close_upvalues`, truncate the stack, then
   re-push the result. That is the single largest helper crossing
   today: ~500k FFI hops per `run(500_000)` call.

2. **`call_ic_hit` × 499,999 — but no helper FFI crossing per hit.**
   The Call IC's hit path is fully inline IR (guard + JitFrame push +
   `call_indirect` to the cached thunk). The 500K count is just the
   IR-side bump counter. Each call still pays:
   * The inline guard (`closure_raw == cache.closure_raw`).
   * The JitFrame push (4 store instructions).
   * The indirect call.
   * The callee thunk's prologue (saves callee-saves + reads
     `slot_offset` from the JitFrame).
   * The callee body.
   * The callee `Return` → 1 `jit_op_return` FFI hop (item 1 above).

3. **`get_upvalue_inline_hit` × 499,999.** All cache-hits, no FFI.
   The closed-Int kind cache reads the i64 directly from
   `ObjClosure.upvalue_int_values` and pushes it via
   `emit_inline_push_integer`.

The hot loop body of `<anon>(y) { x + y }` is just **GetUpvalue 0;
GetLocal 1; Add; Return**. The first two are inline (no FFI), the
`Add` virt-folds when both operands are virt-int, and `Return`
crosses FFI exactly once per call.

## Why the closure can't take the existing specialized entry today

The `specialized_entry_eligible` analysis in
`core/src/compiler/slot_types.rs` rejects this closure for two
independent reasons in the current run:

```
spec_entry_rejected_param_not_int:   1   ← y is untyped (Value param,
                                            no <int> annotation)
spec_entry_rejected_has_upvalue_op:  1   ← uses GetUpvalue
```

A closure that captures any upvalue is **always** rejected today,
because the current specialized ABI is `fn(*mut VM, i64, ..., i64)
-> (u32, i64)` — pure-Int args, no provision for a captured-state
pointer. The body's `GetUpvalue 0` would have nowhere to read `x`
from in that ABI.

## Proposed design: closure-aware specialized entry

A two-pronged change to lift the upvalue restriction.

### A. Specialized ABI extension

Add a second specialized variant whose signature carries the
caller's `*const ObjClosure`:

```
fn(vm: *mut VM, closure: *const ObjClosure, a0: i64, ..., aN: i64)
    -> (u32, i64)
```

The `closure` pointer is what the caller would otherwise have written
to `JitFrame.closure_raw`; passing it as a register avoids the load
through the frame struct on every `GetUpvalue` and removes the need
for the callee to push a `JitFrame` for itself.

`SpecializedEntryKind` grows a third variant
`NativeIntBodyWithClosure` so the call sites can dispatch on the
closure-bearing signature when both eligible and the callee has
captured upvalues.

### B. `slot_types` eligibility relaxation

Lift the `RejectedHasUpvalueOp` rejection when:

1. Every `GetUpvalue idx` has `upvalue_int_kinds[idx] == 1` after the
   first call (i.e., the closed-Int cache is hot for every captured
   slot).
2. No `SetUpvalue` opcode appears in the body — captured ints are
   read-only from the JIT's perspective. (`SetUpvalue` would require
   write-through to the live `Rc<RefCell<Upvalue>>`; out of scope for
   v1.)
3. The body has no `Closure` op — i.e., the closure doesn't itself
   capture into a nested closure.

Conditions 1 and 3 are already in the analyzer. Condition 2 is the
new gate. When all hold, emit the `NativeIntBodyWithClosure` entry.

### C. Inline GetUpvalue in the specialized body

In the specialized body, the captured-state pointer is in a register
(per the new ABI). `GetUpvalue idx` lowers to:

```
upvalue_int_kinds_data = closure.upvalue_int_kinds.ptr
kind = upvalue_int_kinds_data[idx]
if kind == 1 {
    push virt_int(closure.upvalue_int_values.ptr[idx])
} else {
    bail to interpreter (status 2)
}
```

No JitFrame walk, no helper. Identical to the current Generic-entry
inline IR but reading through the closure-pointer register instead
of `vm.jit_frame_view[top].closure_raw`.

### D. Inline `Return` in the specialized body

The `IntSpecialized` Return path already inlines stack truncation +
JitFrame pop and exits with `(0, payload)`. The
`NativeIntBodyWithClosure` Return is identical: the caller doesn't
push a JitFrame for the callee in the new ABI (the closure pointer
is in a register, no frame needed), so the Return becomes:

```
stack_view.len = caller_slot_offset_at_call_time
return (0, payload)
```

Two stores instead of `jit_op_return`'s ~10-instruction sequence
plus its FFI overhead.

### E. Caller-side IC routing

`OpCode::Call`'s inline IC hit path checks `cache.specialized_kind`.
When `specialized_kind == NATIVE_INT_BODY_WITH_CLOSURE`:

* Verify `arg_count == cache.arity` (already done).
* Verify all `arg_count` arg operands on the virt stack are int SSA
  (already done in the A3 self-recursion direct-call path).
* `call_indirect` to the cached specialized thunk with the caller's
  cached closure pointer + the i64 args.
* On `(0, payload)` success: push `Value::Integer(payload)` onto the
  VM stack via `emit_inline_push_integer`.
* On `(1, _)` runtime error: jump to the caller's `exit_block` with
  status 1 (existing pattern).
* On `(2, _)` bailout: re-do the call through the IC miss path
  (which goes via `jit_op_call_miss` and the interpreter), letting
  `op_call_miss` re-prime the cache.

### Expected impact on bench_closure

Removing both `op_return` (500k FFI hops) and the JitFrame push for
the callee leaves the hot path at:

* IC guard hit (~3 inline ops).
* Specialized direct call (no JitFrame, no helper).
* Callee body: GetUpvalue (inline, register-based), GetLocal (virt
  int via param mirror), Add (virt-int fold), Return (2 stores).
* Caller resumes: push Value::Integer(payload) on stack, continue.

Estimated drop from ~22 ms (current step1-4 result) to ~6–10 ms,
matching the structure of `bench_nested_loop_big`'s post-optimization
cost shape.

## Why this is a separate change from steps 1–4

Steps 1–4 were purely about the JIT *codegen* (what IR to emit for a
given opcode) and a single fix to the *type analysis* (slot_types Pop
clearing). Each was small, mechanical, and locally verifiable: a
runtime-error-line test, a helper count diff, a disasm count.

Closure specialization touches:

* The specialized ABI (a new `extern "C"` function shape).
* The eligibility analysis (a new acceptance rule).
* The Call IC's hit-path branching (a new dispatch arm).
* The Closure / Upvalue runtime contracts (preserving the
  `Rc<RefCell<Upvalue>>` invariant under the new register-passed
  closure pointer).

Each of these has its own correctness surface and its own test
matrix (closures with captured ints, closures with captured non-ints
that bail, mixed nested closures, etc.). It's a coherent piece of
work but distinctly larger and more test-intensive than steps 1–4.
Doing it in a separate change keeps the bench-1–4 win uncoupled from
its slower-to-stabilize follow-up.

## Out of scope for v1

* `SetUpvalue` virtualization (would require write-through into the
  shared `Upvalue` cell; non-trivial under the existing closed-Int
  cache).
* Closures that capture non-Int upvalues. These keep going through
  the Generic entry; the kind cache miss already routes them to the
  helper.
* Deep nesting (a closure that itself defines a closure). The body's
  `Closure` op forces the Generic path.
* Multi-return closures. The current specialized ABI returns one
  `(u32, i64)` pair.

## File touchpoints

When implementing:

* `core/src/jit/engine/defs.rs` — add `NativeIntBodyWithClosure`
  variant to `SpecializedEntryKind`; bump
  `SPECIALIZED_KIND_NATIVE_INT_BODY_WITH_CLOSURE`.
* `core/src/jit/engine/mod.rs` — add the new specialized signature,
  the prologue that reads upvalue caches via the closure register
  param, the IC routing in `OpCode::Call`.
* `core/src/compiler/slot_types.rs` — relax
  `RejectedHasUpvalueOp` per the conditions in §B.
* `core/src/jit/runtime.rs` — no changes (the new path is
  helper-free).
* `core/tests/jit_fallback.rs` — tests for: closure with captured
  Int returns correct sum, closure with non-Int upvalue bails to
  interpreter cleanly, nested-closure-with-Closure-op stays on
  Generic.
