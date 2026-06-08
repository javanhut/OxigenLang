# JIT correctness bugs — fixed (root-cause record)

Three JIT correctness bugs were found and fixed in 2026-06. All were
release-mode codegen/runtime defects; the interpreter (`--no-jit`) was
always correct. Regression tests live in `core/tests/jit_fallback.rs`
(`regression_*`). This file records the root causes so the patterns aren't
reintroduced.

---

## 1. `import_test` regression — stale `Vec` length after inline return
**Symptom:** default mode, `math.pow(2,10)` → "cannot call INTEGER".
**Cause:** `invoke_thunk` ran the thunk but didn't sync `vm.stack`'s `Vec`
length from `stack_view.len`. The inline return path updates only
`stack_view.len` (raw store); the interpreter then indexed `vm.stack` with
a stale length (+1) → wrong callee slot.
**Fix:** `sync_stack_from_view()` after the thunk returns in `invoke_thunk`
(commit "sync Vec length from stack_view after thunk returns").

## 2. SIGBUS — builtin call inside a JIT-compiled loop
**Symptom:** `SIGBUS`/exit 138 in default mode (`Array.oxi`,
`linked_list.oxi`; minimal: `len()`/`str()` in a `repeat`).
**Cause:** the GetGlobal inline-cache *hit* path bumped the Rc strong count
for any value with `tag > 6`. `Value::Builtin` (tag 13) is > 6 but holds a
**function pointer, not an `Rc`** — the bump did `*[fnptr] += 1`, writing
into a read-only code page. First access missed (helper path, correct);
the 2nd+ (cache hit, e.g. a builtin called each loop iteration) crashed.
**Fix:** exclude tag 13 from the bump in the GetGlobal IC, mirroring the
existing guard in the value-clone helper (commit "fixed the sigbus issue").

## 3. `option`/ternary arms return the wrong value (e.g. `min`/`max`)
**Symptom:** `min(a,b) = option { a <= b -> a, b }` returned `b`
unconditionally once JIT-compiled — in **default** mode too (any hot
min/max-style function), not just eager `--jit`.
**Cause:** the compile-time operand stack (`virt_stack`) was never flushed
or reset at basic-block boundaries. Each `option` arm pushed its result
onto the single shared `virt_stack`; at the merge (`Return`) only the
last-emitted arm's SSA value remained, and it was live on both paths, so
every arm yielded the same (second) value.
**Fix:** flush `virt_stack` to memory before every block terminator
(fall-through edge, `Jump`, `Loop`, `JumpIfFalse`/`JumpIfTrue`/
`PopJumpIfFalse`) and reset it to empty at each block start, so merges read
from memory. No-op when `virt_stack` is empty (tight numeric loops), so the
benchmark suite is unchanged (commit "flush virt_stack at block
boundaries").

---

## Note on debugging method
These were release-only and (for #2/#3) memory-corruption-adjacent, so
runtime instrumentation perturbed layout and masked them (heisenbugs), and
`lldb` attach is blocked by macOS SIP in the sandbox. They were root-caused
by **static analysis of the emitted machine code** via
`OXIGEN_JIT_DISASM=<fn>` cross-referenced with the macOS `.ips` crash
report's faulting address — useful tools for any future JIT codegen bug.
