# Known JIT bugs (pre-existing, tracked)

Two correctness bugs in the JIT predate the current branch — both reproduce
identically on `main` and are **not** regressions from recent work. They are
documented here with minimal reproductions and the analysis gathered so far.
Both are interpreter-correct (`--no-jit` always produces the right answer);
they only manifest under JIT-compiled code.

Status: **open.** Root-causing them needs interactive-debugger inspection of
the emitted machine code, which the current sandboxed environment blocks
(macOS SIP refuses `lldb` attach). They are recorded so a debugger-capable
environment can pick them up.

---

## Bug 1 — SIGBUS: builtin call inside a JIT-compiled loop

### Severity
High (process crash, `SIGBUS`/exit 138). Default mode (`oxigen script.oxi`),
release builds only.

### Minimal reproduction
```oxi
fun go() {
    s <int>
    repeat unless s >= 3 {
        str(7)        // any builtin call: str(), len(), push(), ...
        s = s + 1
    }
    s
}
println(go())
```
```
oxigen script.oxi      # crashes: Bus error (138)
oxigen --no-jit script.oxi   # prints 3 (correct)
```
Real programs hit by it: `example/Array.oxi`, `example/linked_list.oxi`
(both call `len()`/`push()`/`last()` inside method loops).

### What is established
- **Release-only.** A debug build does not crash (so it's optimization-
  sensitive — UB exposed under release, or a Cranelift codegen interaction).
- **Builtin-specific.** A *closure*/user-function call in the same loop works
  (`fun h(x){x+1}` called in the loop is fine); only **builtin** callees
  crash. Builtins route through `jit_op_call_miss` → `call_value`'s builtin
  branch (no interpreter frame pushed); closures push a frame and run via
  `execute_until`.
- **Accumulation, not first-hit.** A loop that runs the body **once** does not
  crash; **3+** iterations do. Each builtin call inside the loop appears to
  leave the stack ~1 slot off; after a few iterations the frame is corrupted
  and a pointer load faults.
- **Fault site.** The macOS crash report puts the fault *inside go's JIT'd
  thunk* (frame 0 = JIT code, frame 1 = `invoke_thunk`). In the
  `emit_inline_generic_return` fast path the faulting load is the closure-
  marker `Rc` deref (`ldr x6, [x5]` where `x5` came from `stack[slot_offset]`)
  — i.e. the closure marker slot was clobbered to a non-pointer. But forcing
  the `op_return` *helper* path (by adding a `Closure` op so
  `may_capture_upvalues` is set) still crashes, so the corruption happens
  **during the loop**, with the return-path deref just one place it surfaces.

### Hypotheses tried and **disproven**
- *Virtualized-local spill/reload collision.* Disabling local virtualization
  for loop+call functions (skipping `int_locals` population) — **still
  crashed**. So virtualization is not the cause.
- *Conservative compile-refusal* (`func.has_loop && call_count > 0` → don't
  compile) — fixes the crash but **regresses `bench_closure` 4.6×→0.65× and
  `bench_struct_method` 8.3×→0.34×** vs CPython, because those call
  closures/methods in loops (which work). Builtin-vs-user callee is a runtime
  distinction, not knowable at compile time, so the guard can't be narrowed to
  only the crashing case. Reverted.

### Where to look next (with a debugger)
- `core/src/jit/runtime.rs` — `jit_op_call_miss` (builtin branch via
  `call_value`) and its `sync_stack_from_view` boundary.
- `core/src/jit/engine/mod.rs` — the `OpCode::Call` miss-path emission
  (`miss_block` → `op_call_miss` → `ok_block`) and whether a stack length /
  pointer cached in an SSA value before the call is reused (stale) after it,
  inside the loop body. The "+1 per iteration" accumulation points at a
  stale post-call stack-height computation specific to the no-frame (builtin)
  return from `op_call_miss`.
- Dump the thunk with `OXIGEN_JIT_DISASM=go` and single-step the loop body in
  a debugger to catch the iteration where `stack[slot_offset]` is overwritten.

---

## Bug 2 — `--jit` eager mode: comparison/`option` takes the wrong branch

### Severity
Medium (wrong result, no crash). `--jit` (eager, threshold=1) only; default
mode is unaffected because the function isn't hot enough to compile.

### Minimal reproduction
```oxi
fun mn(a, b) { option { a <= b -> a, b } }
println(mn(3, 7))
```
```
oxigen --jit script.oxi    # prints 7  (WRONG — a<=b is true, should return a=3)
oxigen script.oxi          # prints 3  (correct; mn isn't eager-compiled)
oxigen --no-jit script.oxi # prints 3  (correct)
```
Real program hit by it: `example/import_test.oxi` under `--jit`
(`math.min(3,7)` returns 7).

### What is established
- `--jit`-only (eager compilation of a small, non-loop function with int
  params). The `a <= b` comparison feeding the `option`'s branch selects the
  *false* arm when the condition is true — an inverted-polarity or
  wrong-condition-code bug in the fused compare+branch for this shape.
- Pre-existing on `main`.

### Where to look next
- `core/src/jit/engine/mod.rs` — the fused compare+branch emission for
  `LessEqual`/`Less`/… immediately followed by `JumpIfFalse`/`PopJumpIfFalse`
  (the `option` lowering), specifically the `IntCC` condition code and the
  brif successor ordering, for int-param functions in the eager path.

---

## Cross-cutting note
Neither bug blocks merging the current branch to `main`: both are present on
`main` already, so the branch does not regress them. The one true regression
this branch had — `import_test` failing in **default** mode — was a missing
`Vec`/`stack_view` length sync in `invoke_thunk` and is fixed (see the
"sync the backing Vec's length" comment in
`core/src/jit/engine/mod.rs::invoke_thunk`).
