# Oxigen JIT Optimization Roadmap

**Goal:** beat CPython, PyPy, and Bun/V8 on the Oxigen benchmark suite.

This document tracks the long-running effort. Each task is isolated, measurable, and independently reversible — a failed attempt is a branch that never merges.

---

## Current state

| Target | Status | Benchmark geomean (lower ratio = Oxigen is faster) |
| --- | --- | --- |
| **CPython 3.14** | ✅ beaten 8/8 | 0.41× (Oxigen is 2.4× faster) |
| **PyPy** | ⚠️ not measured | — |
| **Bun (V8 JIT)** | ❌ losing 3–6× | ~4.5× (we're 4.5× slower) |

Last measured: `benchmark_reports/latest-native.md`. Re-run with `scripts/bench.sh`.

### Baseline already in place
- Cranelift-backed single-tier baseline JIT (`--jit` flag, feature-gated)
- Inline ICs: `GetGlobal`, `Call`, `MethodCall` (arg ≤ 1), `GetField`/`SetField` helper sites
- Inline IR: int arithmetic fast path, int comparison fast path
- Peephole: struct-field-add fused into one helper call
- **Tier-2 (committed):** inline IR for struct-field-add peephole, monomorphic method-call inline expansion for field-add-shaped bodies
- `ObjStructInstance` is `#[repr(C)]` with a raw field-buffer pointer

---

## Methodology

Every task follows this lifecycle:

1. **Branch** from `oxigen-jit` (or current tip): `git checkout -b opt/<task-id>`.
2. **Implement** behind the existing `jit` cargo feature. No new user-facing flags unless the task is explicitly runtime-switchable.
3. **Correctness gate (blocking):**
   - `cargo test -p oxigen-core --lib --release --features jit` — all tests pass.
   - Every `example/bench_*.oxi` produces the same result as `main` (compare `--jit` vs `--no-jit`).
   - Any new JIT invariant gets a `#[test]` pinning it (e.g., layout offsets, tag constants).
4. **Perf gate (blocking):** measure against `main` using `scripts/bench.sh` (≥8 warmups, ≥15 runs, per-variant isolated). The task's "acceptance gate" below specifies the required delta.
5. **Merge** only if both gates pass. Otherwise either iterate on the branch or abandon it with a note in the Decisions Log.
6. **Rollback** if a regression surfaces post-merge: `git revert <merge-commit>`. Each task's scope is small enough for a clean revert.

### Commit discipline

- Each task becomes one merge commit on `oxigen-jit` (or whatever the integration branch is).
- Sub-steps get their own commits within the branch so bisection works.
- Commit messages document the observed perf delta, not just the intent.

---

## Task table

Legend: ☐ not started · ◐ in progress · ✅ landed · ❌ attempted & abandoned

| ID | Task | Status | Prereqs | Target gain | Scope |
| --- | --- | --- | --- | --- | --- |
| **Phase A — Representation (foundational)** | | | | | |
| A1 | NaN-box Value (full 8 B migration) | ◐ partial (A1.1b landed; A1.2 deferred — see Decisions Log 2026-04-23 pivot) | — | 30–50% geomean | vm/value.rs, jit/engine.rs, all VM consumers |
| A2 | Small-integer tagging (SMI) | ☐ | A1 | 20–30% on int-heavy benches | vm/value.rs, jit arith paths |
| A3 | Feedback vectors (record-only) | ☐ | — | 0% standalone — enables B3 | jit/engine.rs, jit/runtime.rs |
| **Phase B — Typed JIT frames + unboxed execution** (revised 2026-04-23) | | | | | |
| B2.0 | Typed slot analysis pass | ☐ | — | 0% standalone — enables B2.1–B2.4 | new `core/src/compiler/slot_types.rs` (or similar) |
| B2.1 | Unboxed int locals in JIT | ☐ | B2.0 | 15–25% on int-arith benches | jit/engine.rs |
| B2.2 | Unboxed int calling convention (args + return) | ☐ | B2.0, B2.1 | 30–50% on self-recursive int funcs (bench_fib) | jit/engine.rs, jit/runtime.rs |
| B2.3 | Boxed-boundary adapters | ☐ | B2.2 | enabler (correctness) | jit/engine.rs |
| B2.4 | Guards + generic-path fallback | ☐ | B2.2 | enabler (correctness) | jit/engine.rs |
| B3 | Tier-2 speculative JIT + deopt | ☐ | A1, A3, B2.0 | 2–3× on monomorphic code | new tier2/ module, runtime deopt support |
| **Phase C — Eliminate allocation** | | | | | |
| C1 | Escape analysis + SRoA | ☐ | B3 | 2–10× on alloc-heavy code | jit optimizer pass |
| C2 | Generational nursery GC | ☐ | A1 | 20–50% on alloc-heavy code | replaces Rc throughout VM |
| **Phase D — Final gap closers** | | | | | |
| D1 | Hidden classes + polymorphic ICs | ☐ | A1 | 15–30% on struct-heavy code | vm/value.rs (struct layout), jit ICs |
| D2 | On-stack replacement (OSR) | ☐ | B3 | 2–3× on script-style code | jit/engine.rs, runtime |
| D3 | Trace JIT for hot loops | ☐ | B3, D2 | 1.5–3× on tight loops | new trace/ module |
| D4 | Register-based bytecode | ☐ | — (disruptive) | 10–20% JIT, 2× interpreter | compiler/, vm/, jit/ |
| **Phase E — Interpreter wins (orthogonal)** | | | | | |
| E1 | Direct-threaded / computed-goto dispatch | ☐ | — | 10–20% interpreter-only | vm/mod.rs dispatch |
| E2 | Superinstructions | ☐ | — | 5–15% interpreter-only | compiler/, vm/ |
| E3 | Quickening (self-rewriting bytecode) | ☐ | — | 10–30% interpreter-only | vm/mod.rs |

---

## Task details

Each entry specifies the concrete change, what test to add, the perf gate, and the rollback strategy. Update status inline as work progresses.

### A1 · NaN-box Value

**Goal:** shrink `Value` from 40 B to 8 B. Every push/pop, every local access, every stack slot read becomes register-sized. Prerequisite for every tier-2 specialization because a tagged 40-byte struct can't ride in a register.

**Approach:** IEEE-754 double-NaN payload encoding. 48-bit pointer space lives inside quiet-NaN bits; the remaining encoding slots hold Bool / None / Char / the SMI range (A2). Doubles that aren't NaN are their own bit pattern.

**Concrete layout (match SpiderMonkey, adjust as needed):**
```
- Non-NaN double: bit-for-bit IEEE-754
- NaN with top bits 0xFFF8_0000_0000_0000 + kind nibble + payload:
    0xFFF8_...  Int32   (payload: low 32 bits)
    0xFFF9_...  Bool    (payload byte 0/1)
    0xFFFA_...  None
    0xFFFB_...  String  (Rc pointer in low 48 bits)
    0xFFFC_...  StructInstance  (Rc pointer)
    ...
```

**Files touched:**
- `core/src/vm/value.rs` — full rewrite of `Value`, Clone/Drop/Display/PartialEq, layout constants
- `core/src/jit/engine.rs` — every `VALUE_INT_PAYLOAD_OFFSET` / `VALUE_TAG_*` usage
- `core/src/jit/runtime.rs` — all helper signatures that accept/return `Value`
- `core/src/vm/mod.rs` — stack push/pop, local access
- All `match` arms on `Value::...` throughout `core/src/`

**Testing:**
- New tests in `vm/value.rs::layout_tests`:
  - `nanbox_integer_round_trips`
  - `nanbox_pointer_round_trips`
  - `nanbox_float_round_trips` (including `f64::NAN`, `±∞`, subnormals, `-0.0`)
  - `nanbox_none_bool_round_trip`
  - `nanbox_rc_refcount_preserved_across_clone_drop`
- All 375+ existing tests pass unchanged.
- Run every `example/bench_*.oxi` with `--jit` and `--no-jit`; results must match.
- Fuzz: generate random sequences of Value operations (construct, clone, drop, compare) and assert consistency vs. an oracle implementation.

**Acceptance gate:**
- Geomean improves by ≥ 20% across the full bench suite.
- No individual bench regresses by ≥ 3%.
- At least one bench improves by ≥ 30%.
- `value_size_is_pinned_at_40` test is updated to `value_size_is_pinned_at_8` (or similar).

**Rollback:** never merge the branch. `Value` on main stays at 40 bytes.

**Reference implementations to read:**
- SpiderMonkey `js/src/vm/JSValue.h` (MPL-2.0)
- LuaJIT `src/lj_obj.h` (MIT)
- Moonshine `src/value.rs` (a Rust Lua VM, smaller and simpler to read)

**Notes / gotchas:**
- The JIT assumes stack stride = `VALUE_SIZE = 40`. Every `imul(slot, 40)` becomes `imul(slot, 8)` (or `shl 3`). Update `emit_inline_stack_pop_one`, `emit_copy_value`, every stack-offset computation.
- `f64::NAN` bit pattern is non-canonical on most platforms; normalize all NaNs to a single sentinel or accept the ambiguity.
- Drop semantics: cloning a pointer-bearing Value must still bump Rc strong; decoding must re-cast the 48-bit pointer back to `Rc<T>` through `Rc::from_raw`. Document the invariant.

---

### A2 · Small-integer tagging (SMI)

**Goal:** skip allocation and boxing for integers fitting in 31 or 48 bits. Loop counters, array indices, and the vast majority of real-world integers become tag-free.

**Approach:** within the NaN-boxed representation (A1), reserve one kind for `Int32` (or `Int48`) with the value embedded directly. Any integer outside the range escapes to a boxed representation (still via the NaN-box, just as a heap pointer).

**Files touched:**
- `core/src/vm/value.rs` — SMI encode/decode helpers, `Value::Integer(i64)` case routes to SMI when in range
- `core/src/jit/engine.rs` — integer fast-path checks (currently `VALUE_TAG_INTEGER`) become SMI checks; arithmetic becomes direct on the tagged word (with overflow deopt)

**Testing:**
- `smi_boundary_values_round_trip` (MIN, MAX, MIN-1, MAX+1, 0, -1)
- `smi_arithmetic_overflow_promotes_to_boxed` — add that overflows SMI range produces correct boxed result
- `bench_fib.oxi` produces identical result (fib grows large, will cross SMI boundary)

**Acceptance gate:**
- Integer-heavy benches (`bench_arith`, `bench_fib`, `bench_loop`, `bench_nested_loop*`) improve by ≥ 15% on top of A1.
- No bench regresses.

**Rollback:** revert the branch; NaN-boxed non-SMI representation from A1 continues to work.

---

### A3 · Feedback vectors (record-only)

**Goal:** instrument the baseline JIT to record observed operand types at key sites. No behavior change yet — this is pure data collection to feed B3.

**Approach:** V8's `FeedbackVector` pattern. One slot per type-variable IR site (arithmetic, field access, method call). Each slot tracks observed types as a bitmask. Baseline IR writes to the slot on every execution.

**Files touched:**
- `core/src/jit/engine.rs` — add `FeedbackSlot` struct, allocate one per instrumented opcode, insert tiny IR snippets to update them
- `core/src/jit/runtime.rs` — helpers for non-trivial feedback updates

**Testing:**
- Instrumentation correctness: run a program with known types; assert the feedback vector matches.
- Perf-neutrality: ≤ 3% regression across the suite (the writes are cheap but non-zero).

**Acceptance gate:**
- All benches within ±3% of main. This task standalone doesn't pay off; its value unlocks B3.
- Feedback contents verified by a new test that inspects them after running `bench_arith`.

**Rollback:** revert. Baseline JIT returns to no feedback collection.

---

### Phase B (revised) — Typed JIT frames for int-stable functions

**Guiding principle:** hot integer code should not traffic in `Value` at all.
Earlier framing ("unboxed integer locals") would have under-delivered because
each function boundary re-boxes — catastrophic for recursion-heavy code
(`bench_fib`). The revised goal is to lift integer execution out of the
`Value` representation end-to-end: locals, params, return, arithmetic, and
direct recursive calls all use native `i64`. `Value` is reified only at the
boundary with the generic interpreter / unknown call targets / error paths.

SlotKind lattice (narrow on purpose — we add more kinds later):

```
⊥           uninitialized
Int64       provably always an integer in this slot
Value       may hold anything — existing generic path
```

Sub-tasks are sequenced so each lands independently with its own perf test:

### B2.0 · Typed slot analysis pass

**Goal:** classify each local slot of each JIT-compiled function as `Int64`
or `Value`. Conservative — err toward `Value` when in doubt. No codegen
change yet; this task only produces the analysis output consumed by B2.1+.

**Acceptance cases (must be inferred as `Int64` to enable downstream wins):**
- Integer literals via `Constant(Value::Integer(_))`.
- Arithmetic results: `Add`, `Sub`, `Mul`, `Div`, `Mod`, `Negate`, bitwise ops — when both operands are already `Int64`.
- Integer comparison results (`<`, `<=`, `==`, etc.) — when both operands are `Int64`. Result type is `Bool`, but booleans are out of scope for v1.
- `GetLocal` of an `Int64` slot propagates `Int64`.
- Function parameters annotated `<int>` in source (e.g., `fib(n <int>)`) — start as `Int64` at slot entry.

**Non-goals for v1:** unions, flow-sensitive narrowing, inferring int types across ad-hoc call sites, generic function specialization.

**Files:** new `core/src/compiler/slot_types.rs`. Emits `FunctionSlotTypes`.
The JIT engine imports the analysis and stashes it alongside compiled entries.

**Testing:** unit tests per opcode transfer function; snapshot tests per `example/bench_*.oxi` that assert which slots are `Int64`.

**Acceptance gate:** all of `bench_arith`, `bench_fib`, `bench_loop`, `bench_nested_loop`, `bench_nested_loop_big` have their loop counter and accumulator slots classified as `Int64`. Perf ±2% (no codegen change).

**Rollback:** revert module; B2.1+ blocked on this anyway.

---

### B2.1 · Unboxed int locals in JIT

**Goal:** emit raw `i64` SSA values for `Int64` slots instead of going through the stack `Vec<Value>` on every `GetLocal`/`SetLocal`. Inside the function body, arithmetic on `Int64` slots uses native `i64` ops with no tag check and no payload load/store.

**Approach:** at function entry, allocate a Cranelift `Variable` per `Int64` slot. `GetLocal` reads via `builder.use_var`. `SetLocal` writes via `builder.def_var`. The stack `Vec<Value>` slot for that local still exists (initialized to `Value::Integer(0)` or equivalent) but is only touched at sync points.

**Sync points — when to flush `Variable` → stack slot:**
- Before any `call` to a helper that takes `*mut VM` (helpers may inspect stack).
- Before `Return` (the return value reification is the boundary; see B2.2/B2.3).
- Before error exits (the interpreter's error handler walks frames).
- Before crossing a `Guard` (user-level error-propagation opcode).

**Re-hydration — when to load stack slot → `Variable`:**
- After any sync-point call that might have mutated the slot (rare for JIT helpers; mostly for builtin calls that get passed arguments).

**Files:** `core/src/jit/engine.rs` — GetLocal/SetLocal/Arithmetic handlers pick the typed path when the slot is classified `Int64`. Helper: `flush_int_locals_to_stack(dirty_set)`.

**Testing:** every bench still produces identical output. Stress test: function mixing `Int64` and `Value` locals with helper calls between them.

**Acceptance gate:** `bench_arith`, `bench_loop`, `bench_nested_loop*` improve by ≥ 15%. No bench regresses ≥ 3%.

**Rollback:** revert; baseline JIT keeps boxing.

---

### B2.2 · Unboxed int calling convention

**Goal:** self-recursive and direct monomorphic calls between int-stable JIT-compiled functions pass arguments and returns as native `i64`, never materializing a `Value` across the call boundary. **This is the unlock for `bench_fib`**.

**Approach:** a JIT-compiled function whose B2.0 analysis says "all params are `Int64` and return is `Int64`" gets **two entry points**:
- **Generic entry:** takes `*mut VM`, reads args from the stack as `Value`, unboxes each via a tag check, populates the `Int64` `Variable`s, runs the body, boxes the return value, pushes to stack. This is the existing thunk signature, extended with an unbox/box prelude/epilogue.
- **Specialized entry:** takes `*mut VM, i64, i64, ...` (one `i64` per int param) and returns `i64`. No stack manipulation for args, no box on return. This is a plain C-ABI function.

**Call-site emission:** at `Call` / `MethodCall` opcodes:
1. If the callee's IC has stabilized on a specialized target with matching arity and our argument slots are all `Int64`, emit a direct `call_indirect` to the specialized entry with `i64` args in registers.
2. Otherwise fall through to the existing generic call path (boxes args to the stack, calls generic entry).

**Self-recursion fast path:** when a function calls itself (same `FunctionId`) with int args, we know the specialized entry exists — no IC needed. Direct call.

**Files:** `core/src/jit/engine.rs` — two-entry-point emission, specialized call-site dispatch. `core/src/jit/runtime.rs` — the generic entry's unbox/box adapters.

**Testing:** `bench_fib` must produce the same result. Stress test: mutually recursive int functions; the callee has specialized entry, the caller uses it.

**Acceptance gate:** `bench_fib` improves by ≥ 30% on top of B2.1. No bench regresses ≥ 3%.

**Rollback:** revert. B2.1 (unboxed locals) remains intact.

---

### B2.3 · Boxed-boundary adapters

**Goal:** correctness infrastructure to make B2.2 safe. Make sure every transition between specialized-int code and generic-Value code boxes/unboxes exactly once, and only when needed.

**Approach:** formal adapter shapes:
- **Generic caller → specialized callee:** pops Value args from stack, tag-checks each as `Int`, extracts payload, calls specialized entry, boxes `i64` return, pushes to stack. Lives on the generic entry's prologue.
- **Specialized caller → generic callee:** boxes its `i64` args back to `Value`, pushes to stack, calls generic entry, pops `Value` return, tag-checks as `Int`, extracts payload. Emitted inline at call sites in specialized functions when the target isn't specialized.
- **Specialized callee → helper:** sync `Int64` `Variable`s to stack slots, call helper, re-hydrate. Already covered by B2.1's sync points; no new infrastructure.

**Testing:** adversarial test — a generic function calls a specialized function with a non-integer arg; must gracefully bail to a typed error, not UB. Covered by B2.4's guards.

**Acceptance gate:** no correctness regressions; all 391+ existing tests pass.

**Rollback:** revert B2.2+B2.3 together.

---

### B2.4 · Guards + generic-path fallback

**Goal:** safety net. Specialized code assumes slots remain `Int64`. If that assumption ever breaks (e.g., a caller passes a non-int from the generic world and the generic-entry tag check is skipped due to compiler bug), we must bail gracefully rather than crash.

**Approach:** the generic entry's tag checks are the primary guard — they run on every call from the generic world. The specialized entry itself assumes pre-checked args and does not re-verify.

For v1 we rely on B2.0's conservative analysis never classifying a slot as `Int64` unless we can prove it. If a bug in the analysis ever produces a false positive, the generic entry's guards still catch the mismatch on entry, but code inside the specialized body could have already corrupted the slot. Acceptable for v1; refine with B3's deopt machinery.

**Testing:** deliberately craft a program that B2.0 *does* classify as `Int64` but that injects a non-int value via a builtin (if any such path exists). Verify the guard fires.

**Acceptance gate:** guards fire correctly on the adversarial test.

**Rollback:** revert B2.2–B2.4.

---

### B3 · Tier-2 speculative JIT + deopt

**Goal:** second JIT tier that reads A3's feedback, compiles assuming observed types are stable, and inserts guards that deoptimize back to the baseline-JIT on violation.

**Approach:** V8 TurboFan / HotSpot C2 recipe.
- Triggered at higher call-count threshold than baseline (e.g., 5000 vs 50).
- Lowers bytecode to a typed IR using A3 feedback.
- Cranelift emits specialized code with deopt points (Cranelift has `stack_switch` / stack maps).
- Deopt restores the interpreter's or baseline-JIT's view and continues.

**Files touched:**
- New module: `core/src/jit/tier2/` (IR, lowering, deopt)
- `core/src/jit/mod.rs` — tier-2 entry point, tiering policy
- `core/src/vm/mod.rs` — deopt entry point in the VM

**Testing:**
- Deopt correctness: every deopt point has a test that forces the guard to fail and verifies execution continues correctly.
- Tier-down loop: synthetic program that rapidly changes types should tier-up to T2, deopt, fall back to baseline, and eventually stabilize.

**Acceptance gate:**
- `bench_struct_method` improves by ≥ 30% on top of A+B1+B2 (ideally breaks 15 ms median).
- `bench_arith` / `bench_fib` improve by ≥ 20%.
- Deopt overhead on adversarial type-changing code is ≤ 2× interpreter.

**Rollback:** revert. Baseline JIT continues as the sole tier.

**Reference implementations:**
- V8: `src/compiler/pipeline.cc` (TurboFan), `src/deoptimizer/` (deopt machinery)
- HotSpot: `hotspot/share/opto/` (C2)
- Winch (a Wasm tier-2 JIT built on Cranelift): has a deopt design doc that's directly applicable.

---

### C1 · Escape analysis + SRoA

**Goal:** for objects that don't escape their allocating function, replace the heap allocation with register-resident fields (scalar replacement of aggregates).

**Approach:** over tier-2 IR. Classic Choi et al. 1999 algorithm. For each allocation site, compute escape set (fields that cross a function boundary). If empty, break the object into its constituent SSA values.

**Files touched:**
- `core/src/jit/tier2/escape.rs` (new)
- Tier-2 pipeline wires it in.

**Testing:**
- Snapshot test: a function that allocates a short-lived Counter emits no allocation in tier-2 IR.
- Correctness: every bench matches output.

**Acceptance gate:**
- `bench_struct_method` with inline-allocated `Counter(0)` shows no heap allocation during the hot loop (verify via allocation counter).
- ≥ 30% improvement on any bench where allocation was the bottleneck.

**Rollback:** revert. Tier-2 continues without SRoA.

---

### C2 · Generational nursery GC

**Goal:** replace `Rc` with a bump-pointer nursery + copying major GC. Allocation becomes free. Ref-count bumps/drops disappear.

**Approach:** standard generational collector.
- Young gen: bump allocator in a fixed-size nursery (~1 MB).
- Allocations happen by `ptr += size`; overflow triggers minor GC.
- Minor GC copies live objects to the old gen; updates forwarding pointers.
- Major GC is mark-and-sweep over the old gen, rarely triggered.
- Stack maps (Cranelift supports these) identify roots.

**Files touched:**
- New module: `core/src/gc/`
- `core/src/vm/value.rs` — `Value::StructInstance(Gc<ObjStructInstance>)` replaces `Rc`
- Every `Rc::clone` / `Rc::drop` call site

**Testing:**
- Stress: allocate-churn benchmarks that force minor GCs; correctness must hold.
- Fuzz: interleaved allocation, mutation, and GC triggering.
- Allocation counter: nursery allocation should be ≤ 2 instructions in the emitted code.

**Acceptance gate:**
- Allocation-heavy benches (simulate via new bench that creates structs in a loop) improve by ≥ 2×.
- No bench regresses (minor GC pauses amortize acceptably).
- GC pause times visible in a new `--gc-log` flag.

**Rollback:** revert. Rc returns to being the ownership model. Note: this is the biggest single-task risk on the roadmap; plan for 4–6 weeks and reserve budget.

---

### D1 · Hidden classes + polymorphic ICs

**Goal:** `GetField`/`SetField`/`MethodCall` inline caches become 2-instruction (shape-compare + offset-load) regardless of struct identity. PIC extends to 2–4 shapes per site.

**Approach:** replace per-def `FieldLayout` with a shape-chain. Struct creation, field add/remove all produce shape transitions. Call sites cache `(shape, offset)` pairs; a 4-entry MRU cache handles polymorphism.

**Files touched:**
- `core/src/vm/value.rs` — `ObjStructInstance` gets a shape pointer; `ObjStructDef` maintains its shape chain
- `core/src/jit/engine.rs` — field-access ICs rewrite to shape-compare-offset-load
- `core/src/jit/runtime.rs` — shape transition handling

**Testing:**
- Morphism tests: programs that use the same struct with different field orders produce correct results.
- PIC tests: call site that sees 3 shapes dispatches correctly to all 3.

**Acceptance gate:**
- `bench_struct_method` improves further (target: < 15 ms).
- Polymorphic struct code (new bench) improves by ≥ 3× over a non-PIC baseline.

**Rollback:** revert. `FieldLayout` stays.

---

### D2 · On-stack replacement (OSR)

**Goal:** compile hot loops independent of whether their enclosing function has tiered up. Critical for scripts where all work happens in one `run()` function with a long loop.

**Approach:** when a loop backedge count exceeds threshold and the enclosing function is still interpreting, compile just the loop (parameterized by the current interpreter state). Transfer control from the interpreter into the compiled code mid-execution, restoring live state.

**Files touched:**
- `core/src/jit/engine.rs` — OSR entry points
- `core/src/vm/mod.rs` — loop-backedge threshold check, OSR transfer

**Testing:**
- Synthetic: function that interprets for a long time, then hits a hot loop. Verify OSR fires and produces correct result.
- Every bench with an outer loop (`run(N)` pattern) should transfer into compiled code within the first ~10k iterations.

**Acceptance gate:**
- Cold-run benches (the first execution of each bench in a process) see ≥ 2× speedup for script-style workloads.

**Rollback:** revert. Tier-2 continues compiling only at function boundaries.

---

### D3 · Trace JIT for hot loops

**Goal:** PyPy / LuaJIT-style trace recording for the hottest loops. Specializes every op on actually-observed types, produces linear specialized code that outperforms even tier-2 method JIT on regular loops.

**Approach:** record a trace starting at a loop backedge hit N times. Each opcode emits specialized IR based on observed operands. Trace ends at an outer loop backedge (success) or at a hot side-exit (extend into a tree). Guards deopt via the B3 infrastructure.

**Files touched:**
- New module: `core/src/jit/trace/`
- Tier-2 cooperates: a trace can embed into a tier-2 compilation as a superblock.

**Testing:**
- Traces on `bench_loop` and `bench_nested_loop` produce identical output.
- Side-exit handling: a loop that takes a different branch periodically extends the trace correctly.

**Acceptance gate:**
- Tight integer loops match or beat Bun on `bench_arith`, `bench_loop`, `bench_nested_loop_big`.

**Rollback:** revert. Method JIT (B3) continues.

**Reference:** LuaJIT `src/lj_trace.c`, `src/lj_record.c`. Read it cover-to-cover before starting.

---

### D4 · Register-based bytecode

**Goal:** eliminate stack push/pop overhead in both the interpreter and the JIT. Bytecode becomes 3-address; operands are explicit register indices.

**Approach:** migrate `compiler/opcode.rs` to register-based ops. VM interprets them directly. JIT codegen becomes simpler because operands are named.

**Files touched:**
- `core/src/compiler/opcode.rs` — new op set
- `core/src/compiler/` — all bytecode emission
- `core/src/vm/mod.rs` — new dispatch
- `core/src/jit/engine.rs` — all IR emission

**Testing:**
- Massive: every test must pass. Bytecode snapshot tests need updating.
- Correctness over a month of use before trusting it.

**Acceptance gate:**
- Interpreter benches (`--no-jit`) improve by ≥ 1.5×.
- JIT benches improve by ≥ 10% (code size goes down, less IR to emit).

**Rollback:** revert. This one is the most painful to roll back because it touches everything; budget for that.

**Order consideration:** this is "whenever you're willing to eat the disruption." It can happen after A1 or be deferred to the end. No hard dependency.

---

### E1 · Direct-threaded / computed-goto dispatch

**Goal:** make the interpreter 10–20% faster without touching the JIT. Matters when the JIT hasn't kicked in yet.

**Approach:** replace the `match` dispatch loop with computed-goto (via a custom `match` table or `#[cold]`-hinted inline asm). CPython 3.11's approach.

**Files touched:**
- `core/src/vm/mod.rs` — one function rewrite

**Acceptance gate:**
- `--no-jit` benches improve by ≥ 10%.
- `--jit` benches unchanged.

**Rollback:** revert.

---

### E2 · Superinstructions

**Goal:** fuse common bytecode sequences into single opcodes. CPython 3.11 auto-generates these from observed frequencies.

**Approach:** profile bytecode bigrams/trigrams across the bench suite and the stdlib. Emit the top-N as new opcodes. The compiler rewrites on emission.

**Files touched:**
- `core/src/compiler/opcode.rs` — new opcodes
- `core/src/vm/mod.rs` — handlers
- `core/src/jit/engine.rs` — handlers (usually trivial — lower to same IR as the component opcodes)

**Acceptance gate:**
- ≥ 5% across the suite on `--no-jit`.
- No JIT regression.

**Rollback:** revert.

---

### E3 · Quickening (self-rewriting bytecode)

**Goal:** interpreter halfway to JIT: opcodes rewrite themselves into specialized variants after first execution.

**Approach:** every "maybe specializable" opcode starts as a generic version that, on first execution, rewrites itself to a specialized form (e.g., `ADD` → `ADD_INT_INT`). On type change, rewrite back. CPython 3.11 design.

**Files touched:**
- `core/src/compiler/opcode.rs`
- `core/src/vm/mod.rs`

**Acceptance gate:**
- `--no-jit` improves by ≥ 15%.

**Rollback:** revert.

---

## Decisions Log

Record each task's outcome here. Keep terse.

| Date | Task | Outcome | Notes |
| --- | --- | --- | --- |
| 2026-04-23 | Tier-2 Phase 4 (pre-roadmap) | ✅ landed | bench_struct_method 186 → 22 ms, 8.6× win. Merged as `cde4235` + `ca1ab10` + `86fdfd0` on `oxigen-jit`. |
| 2026-04-23 | A1.0 NaN-box encoding module | ✅ landed | Foundation only — 8 B `NanValue` type + 16 tests, not yet wired in. Committed `e90032f` on `opt/a1-nanbox`. |
| 2026-04-23 | A1.1a Box `ErrorValue` (40 → 24 B Value) | ✅ landed (intermediate) | Small net positive: fib −9%, closure −7%, arith −7% on controlled runs; bench_loop +4% regression (microarch, not code); struct_method unchanged. Geomean ~3% win. Not the full A1 gate, but a stepping stone toward the 16 B and 8 B targets — the 32 B `ErrorValue{msg,tag}` inline variant was the single biggest size driver. |
| 2026-04-23 | A1.1b `Rc<str>` → `Rc<String>` (24 → 16 B Value) | ✅ landed (intermediate) | Wins compound with A1.1a but diminishing: fib −10.2%, closure −6.6%, arith −6.3% vs. 40 B baseline. bench_loop regression (+4%) persists unchanged from A1.1a. bench_struct_method now +1.8% vs baseline (regressed mildly). Geomean ~3–4% win total across A1.1a+b. Replaced `Rc<str>` (fat pointer, 16 B) with `Rc<String>` (thin pointer, 8 B) in `String` and `Error` variants and in `ErrorValueData`. Required ~250 `.into()` → `rc_str(...)` rewrites across builtins/vm/compiler (Python-regex driven, paren-balanced). Prerequisite for any NaN-box migration since NaN-box needs 8-byte aligned pointers. |
| 2026-04-23 | **Pivot decision: A1.2 → B2 (typed JIT frames)** | ☐ (recording) | After A1.1a+b diminishing returns on struct_method (flat) and persistent bench_loop regression, user refocused on typed JIT frames rather than continuing Value-size shrinkage. Key insight: "hot integer code shouldn't traffic in `Value` at all" — unboxing locals alone under-delivers because recursive calls re-box. Phase B revised to 5 sub-tasks (B2.0–B2.4); see Phase B section above. A1.2 (full NaN-box) deferred — may be revisited later as foundation for B3 tier-2 + D1 hidden classes. A1 marked `◐ partial` in task table. |

---

## Reading list

Before starting any task, read the relevant papers and reference code.

- **NaN-boxing:** Gudeman 1993; SpiderMonkey `JSValue.h`; LuaJIT `lj_obj.h`.
- **SMI:** V8 design docs, `include/v8-internal.h`.
- **Tier-2 + feedback:** V8 TurboFan pipeline docs; HotSpot C1/C2 papers by Cliff Click.
- **Deopt:** Hölzle/Chambers/Ungar 1992 "Debugging Optimized Code with Dynamic Deoptimization."
- **Escape analysis:** Choi et al. 1999 "Escape Analysis for Java."
- **Trace JIT:** LuaJIT source; Bolz et al. 2009 "Tracing the Meta-Level."
- **Hidden classes:** Hölzle/Chambers/Ungar 1991 "Optimizing Dynamically-Typed Object-Oriented Languages with Polymorphic Inline Caches."
- **Register-based bytecode:** "Virtual Machine Showdown: Stack vs. Registers" (Shi et al. 2008).
- **Quickening:** CPython PEP 659.
- **Generational GC:** Appel 1988; Jones & Lins "The Garbage Collection Handbook."

---

## How to update this document

- Mark status inline in the task table when starting/finishing.
- Append to the Decisions Log on every merge or abandon.
- Revise target gains after a task lands, informed by the actual measurement.
- If a task's approach changes significantly, rewrite its section — don't let the doc drift from reality.
