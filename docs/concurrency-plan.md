# Oxigen Concurrency Plan

**Goal:** true multi-core parallelism and async I/O, with a dead-simple front end and **zero regression** to single-thread JIT performance.

This document tracks the concurrency effort. It is greenfield — there is no existing multithreaded code to preserve. Each phase is isolated, measurable, and independently reversible; a failed phase is a branch that never merges.

---

## Design principle

Data races come from exactly one thing: **shared mutable state**. Locks, atomics, and GILs are all patches applied *after* deciding to share it. Rust's borrow checker is the formalization of "no shared mutable state without synchronization" (`Send`/`Sync`).

So we do not make the heap safe to share — we **never share it**. Mutable state stays thread-local; data crosses threads only by **ownership transfer** (`Send`). Parallelism is then lock-free and GC-free, and the Rust compiler proves it correct at compile time.

Consequence for the existing runtime:

- **Inside a thread:** `Rc` + non-atomic refcounts + the JIT stay exactly as they are. This *is* the performance, and it is left untouched.
- **Across threads:** never share — move ownership. A worker is just the existing `VM` running on its own thread with its own heap. We run *N copies* of the VM we already have.

This was chosen over the two alternatives deliberately:

- **`Arc` + `Mutex` shared heap** — atomic refcounts are ~10–20× slower in exactly the hot loops the JIT optimizes; rejected on performance.
- **Tracing/concurrent GC + shared heap** (the `optimization-roadmap.md` Phase C "generational GC" item) — enables fine-grained sharing but adds write barriers that slow JIT stores, risks single-thread regression, and bypasses the borrow checker. Deferred indefinitely; only revisit if a workload genuinely needs large mutable state shared *live* across cores.

---

## The rules we plan under

- **Reuse the whole synchronous VM unchanged.** Not making it re-entrant, not touching JIT internals, not making `Value` thread-safe.
- **One new idea on the heap:** a `Task` handle. Everything else is plumbing in isolated files under `core/src/concurrent/`.
- **Copy at the boundary, move later.** v1 transfers data by a deep walk into an owned `Send` form. The boundary is crossed once per task, not per op; zero-copy is deferred until a benchmark says it matters.
- **Blocking I/O first, reactor later.** v1 tasks block their worker on I/O — that already gives N-wide I/O concurrency (parallel fetches) for free. Coroutines + reactor (10k-connections-per-core) wait for Phase 2.
- **VM backend only.** The tree-walker stays single-threaded; it is not the perf path and duplicating into it is YAGNI.

---

## Architecture (v1)

```
main thread (existing VM)
   │  spawn compute(0, 1_000_000)
   ▼
 [ global MPMC task queue ]   ◄── crossbeam channel, the only shared thing
   │        │        │
   ▼        ▼        ▼
worker 1  worker 2  worker N      ← each = a full VM, own heap, own globals
 (core)    (core)    (core)          tasks run start-to-finish, no shared mutable state
```

A task names a top-level function and carries its arguments as plain owned data (`Sendable`). A worker looks the name up in *its own* globals, rebuilds args into *its own* heap, runs the existing `execute` loop, and ships the result back the same way. No `Arc`, no `Mutex`, no atomics in any hot path. `Send` is the only safety mechanism and the compiler enforces it.

**Net new surface on existing hot code:** one `Value` variant + two opcodes + two `VM` helper methods. Everything else is new files.

---

## Methodology

Same lifecycle as `optimization-roadmap.md`:

1. **Branch** from `threading`: `git checkout -b conc/<task-id>`.
2. **Implement** behind a `concurrent` cargo feature (so a build can exclude it entirely while it stabilizes).
3. **Correctness gate (blocking):**
   - `cargo test -p oxigen-core --lib --release --features "jit,concurrent"` — all pass.
   - Existing single-thread benches/examples produce identical results with the feature on and off (concurrency must be inert until `spawn` is used).
   - Every new invariant gets a `#[test]` (transfer round-trips, `Value::Task` layout, error propagation).
4. **Perf gate (blocking):**
   - **No single-thread regression:** existing `benchmark_reports` geomean unchanged within noise with `--features concurrent`.
   - Phase-specific scaling gate (below).
5. **Merge** only if both gates pass; otherwise iterate or abandon with a Decisions Log note.
6. **Rollback** via `git revert <merge-commit>` — each phase is a clean, small revert.

---

## Phases

Legend: ☐ not started · ◐ in progress · ✅ landed · ❌ attempted & abandoned

| ID | Task | Status | Prereqs | Scope |
| --- | --- | --- | --- | --- |
| **Phase 0 — Plumbing** | | | | |
| P0.1 | `Sendable` + `detach`/`attach` (Int/Float/Bool/Str/Arr/Unit subset) | ✅ | — | `core/src/concurrent.rs` |
| P0.2 | Pool + worker loop + queue (std `mpsc` + `Arc<Mutex<Receiver>>`) | ✅ | P0.1 | `core/src/concurrent.rs` |
| P0.3 | Load defs without `main` | ◐ | — | `core/src/vm/mod.rs` |
| P0.4 | `VM::call_with_args` + `VM::get_global` | ✅ | P0.1 | `core/src/vm/mod.rs` |
| **Phase 1 — Parallel-compute win (go/no-go)** | | | | |
| P1.1 | Task handle — `Value::Uint(id)` + thread-local table (no new `Value` variant) | ✅ | P0.* | `core/src/concurrent.rs` |
| P1.2 | `spawn` / `join` as **builtins** (no opcodes — see log) | ✅ | P1.1 | `core/src/concurrent.rs`, `vm/builtins.rs` |
| P1.3 | Parser/lowering | n/a | — | builtins → `spawn(fn, args…)` is a plain call |
| P1.4 | Drain outstanding tasks at program exit | ☐ | P0.2 | `core/src/concurrent.rs` |
| P1.5 | Benchmark prime-count seq vs parallel + report | ✅ | P1.3 | scratchpad → move into `benchmarks/` |
| **Phase 2 — True async I/O** | | | | |
| P2.1 | Per-worker reactor (`mio`/`polling`) | ☐ | P1 validated | `core/src/concurrent/` |
| P2.2 | Stackful coroutines per task (`corosensei`) | ☐ | P2.1 | `core/src/concurrent/` |
| P2.3 | Blocking builtins → register-fd-and-yield | ☐ | P2.2 | `vm/builtins.rs`, `netres/` |
| P2.4 | `Sendable::Socket(TcpStream)` migration | ☐ | P2.3 | `transfer.rs`, `netres/` |
| P2.5 | Lift "main-thread-only join" (yield on join) | ☐ | P2.2 | `vm/mod.rs` |
| **Phase 3 — Polish (gated on demand)** | | | | |
| P3.1 | Auto-join-on-use sugar | ☐ | P1 | compiler |
| P3.2 | Closure/function-value transfer (enables `pmap`, parallel-`each`) | ☐ | P1 | `transfer.rs`, compiler |
| P3.3 | Per-core work-stealing deques | ☐ | profiling | `core/src/concurrent/` |
| P3.4 | Zero-copy move-transfer for unique graphs | ☐ | profiling | `transfer.rs` (encapsulated `unsafe`) |
| P3.5 | Biased reference counting | ☐ | profiling | `vm/value.rs` |
| P3.6 | Structured-concurrency scope, cancellation, timeouts | ☐ | demand | language + runtime |

### Spike results (landed on `threading`, not committed)

A time-boxed YAGNI spike proved the architecture end-to-end:

- **~5.1× speedup** on 8 `spawn`/`join` tasks (prime-counting, trial division) on a 14-core box: sequential ~5006 ms → parallel ~984 ms (debug build). Totals match exactly — correctness verified.
- **Footprint:** 265 lines in `core/src/concurrent.rs` + ~29 lines across existing files (`lib.rs`, `vm/builtins.rs`, `vm/mod.rs`, cli `main.rs`). **No new dependency** (std `mpsc` + `Arc<Mutex<Receiver>>`); no `Value` variant, no opcode, no parser change.
- **Known sharp edge (→ finish P0.3):** `spawn`/`join` driver code must live inside `main { … }`. Workers build their VM by re-running the source with `is_main_context = false`, which skips the `main` body (`Main` opcode, `vm/mod.rs:2137`) but **not** free-standing top-level statements — so a top-level `spawn` re-executes during worker init and recurses until the worker stack overflows. Idiomatic Oxigen already puts driver code in `main`, so this is a documented constraint for now; the proper fix is P0.3 done right (register `fun`/`struct`/`pattern`/`introduce` declarations only, skip all top-level execution).

---

## Phase 0 — Plumbing

**Goal:** a task runs on another thread and returns a correct value, driven from a Rust test. No language surface yet.

`core/src/concurrent/transfer.rs` — the single place that crosses the `Send` boundary, used in **both** directions (DRY):

```rust
// owned, Send mirror of Value — primitives + plain data only
enum Sendable {
    Unit, Int(i64), Float(f64), Bool(bool), Char(char), Byte(u8), Uint(u64),
    Str(String), Arr(Vec<Sendable>), Tuple(Vec<Sendable>),
    Map(Vec<(Sendable, Sendable)>), Set(Vec<Sendable>),
    Struct { name: String, fields: Vec<Sendable> },
    Enum   { name: String, variant: String, payload: Vec<Sendable> },
    Err(String),
}
fn detach(v: &Value) -> Result<Sendable, String>;   // Closure/Module/Def/Task → Err (v1)
fn attach(s: Sendable, vm: &mut VM) -> Value;        // rebuild in the local heap
```

`core/src/concurrent/mod.rs` — pool + worker + task:

```rust
struct Task { func: String, args: Vec<Sendable>, reply: Sender<Result<Sendable, String>> }
struct Pool { tx: Sender<Task>, outstanding: Arc<AtomicUsize> }
static POOL: OnceLock<Pool> = OnceLock::new();          // lazily started on first spawn

fn worker_main(src: String, rx: Receiver<Task>) {
    let mut vm = VM::load_definitions(&src);            // compile + run top-level decls, SKIP main
    while let Ok(t) = rx.recv() {
        let _ = t.reply.send(vm.run_named(&t.func, t.args));
        POOL.get().unwrap().outstanding.fetch_sub(1, Relaxed);
    }
}
```

Two reuse-helpers on `VM` (`core/src/vm/mod.rs`):

- `VM::load_definitions(src)` — existing compile path, stops before invoking `main`. **One semantic decision:** top-level side effects run once on the main thread; workers load *definitions only*. Documented constraint: "put side effects in `main`." Re-running imports/decls per worker is idempotent. This avoids making the compiled program `Send`/`Arc`-shared (deferred — see P3 / Decisions Log).
- `VM::run_named(name, args)` — `attach` args, look up name in globals, call the existing `call_closure`, `detach` the result. Pure reuse of the existing call path.

Workers default to `available_parallelism()`, override via `OXIGEN_WORKERS`. Spawn them with the same large-stack pattern the CLI already uses for the main thread.

**Skipped here:** work-stealing deques (shared MPMC queue load-balances fine), `Arc<Program>` sharing, any syntax.

**Acceptance gate:** a Rust test enqueues a `Task` calling a named function with int/string/array/struct args and gets the correct `Sendable` back; `detach`→`attach` round-trips every `Sendable` variant.

---

## Phase 1 — Parallel-compute win (go/no-go)

**Goal:** near-linear speedup on CPU-bound work. This is the number that decides whether we commit.

- `Value::Task(Rc<TaskHandle>)` in `core/src/vm/value.rs`:
  ```rust
  struct TaskHandle { reply: Receiver<Result<Sendable, String>>, memo: RefCell<Option<Value>> }
  ```
  Another 8-byte `Rc` heap variant — fits the pinned 16-byte layout. Follow the existing Rc-variant pattern for `Clone`/`Drop`/print/eq. The JIT treats it as an opaque heap tag (no fast path), like any other boxed value.

- Two opcodes in `core/src/compiler/opcode.rs` — **both need `&mut VM`**, so they cannot be ordinary `fn(&[Value]) -> Value` builtins:
  - **`Spawn { argc }`:** pop args + callee; require `callee.function.name` is `Some` and resolves to a top-level global (else runtime error: *"spawn target must be a named top-level function"*); `detach` args; create reply channel; send `Task`; `outstanding += 1`; push `Value::Task`.
  - **`Join`:** pop handle; return memo if present; else `reply.recv()` (blocks), `attach` result (or raise the transferred error through the existing error machinery), memoize, push.

- Parser + compiler lowering for placeholder `spawn expr` / `join(expr)`. **Surface syntax is TBD and decided later** — the backend does not care.

- `VM` exits drain `outstanding` so fire-and-forget tasks finish cleanly.

- Benchmark `benchmarks/parallel_primes.oxi` (or mandelbrot): a **named** worker function over integer ranges — the case that fits v1 exactly:
  ```
  h1 := spawn count_primes(0, 2_500_000)
  h2 := spawn count_primes(2_500_000, 5_000_000)
  ...
  total := join(h1) + join(h2) + ...
  ```

**Hard v1 boundary:** spawn takes a **named top-level function with plain-data arguments**. No closures-as-arguments, no anonymous task bodies — those need function-value/upvalue transfer (P3.2). Generic `pmap(f, xs)` and parallel-`each` with an arbitrary inline body are **not** v1.

**Deadlock dodge (KISS):** only the main thread joins; tasks are leaves that do not spawn-and-join. This eliminates pool-join starvation outright and still covers parallel-compute and fan-out. Nested concurrency falls out in Phase 2 once tasks yield instead of block.

**Acceptance gate:** `count_primes` across 1 vs 8 workers lands **~6–8× on 8 cores**, with single-thread throughput **unchanged** vs `threading` tip. If it holds, the architecture is validated and we commit. If not, we found out cheaply, before building anything complex.

---

## Phase 2 — True async I/O

**Goal:** "10k connections on one core" and `spawn handle(conn)` — the original async ask — without a thread per connection. Only started after Phase 1 validates.

- Per-worker **reactor** (`mio` or `polling`). **Not tokio** — its shared-heap, `Send + 'static` model fights `Rc` and would be the "grab a conventional library" move we are avoiding.
- Each task runs inside a **stackful coroutine** (`corosensei`; single-thread, never needs `Send`). Existing blocking builtins (`net.*`, `io.*`, `time.sleep`) change at one layer only: instead of blocking the syscall, register the fd with the reactor and **yield**. Stack-swapping works *through JIT'd frames* (it only swaps the stack pointer), so the JIT stays untouched. The `.oxi` stdlib wrappers do not change — this stays colorless.
- **Socket migration:** `std::net::TcpStream` is already `Send`. Add `Sendable::Socket(TcpStream)` so an accepted connection moves into a task and re-registers on its worker. Unlocks the accept-loop server pattern (the current `netres` registry is thread-local, which is why socket-passing waited for here).
- **Nested concurrency for free:** a joining task now yields instead of blocking its worker, so the Phase-1 "main-thread-only join" restriction lifts.

---

## Phase 3 — Polish (each item gated on real demand)

Only if profiling or usage asks: auto-join-on-use sugar; closure transfer (function-value via shared code + upvalue snapshot, enabling `pmap` / parallel-`each` with arbitrary bodies); per-core work-stealing deques replacing the global queue (if queue contention shows); zero-copy move-transfer for unique (refcount==1) graphs and biased refcounting (if the boundary copy shows); structured-concurrency scopes; cancellation/timeouts.

---

## Gotchas and chosen resolutions

| Gotcha | v1 resolution |
| --- | --- |
| Worker globals / double side effects | `load_definitions` skips `main`; "side effects go in `main`" (documented). `Arc<Program>` sharing deferred. |
| Pool-join deadlock | v1: only main joins, tasks are leaves. Fixed in P2 by yielding joins. |
| Thread-local `netres` socket registry | No socket passing in v1; self-contained I/O per task. Migration is P2.4. |
| Thread-local RNG / string interner | Per-worker is fine — independent RNG streams, independent constant interning. No correctness issue. |
| JIT duplicate compilation per worker | Accept it (each worker JITs its own hot functions). Share a read-only code cache later if startup cost shows. |
| Worker stack size | Spawn workers with the large stack the CLI already gives main; N× virtual address space is free on 64-bit. |
| `Value::Task` in JIT | Opaque heap tag, no fast path; verify the JIT never assumes a closed tag set on the slow path. |

---

## Decisions log

- **Share-nothing + ownership transfer** chosen over `Arc`/`Mutex` (perf) and tracing GC (complexity, single-thread regression risk, bypasses borrow checker). See Design Principle.
- **Recompile-defs-per-worker** chosen over `Arc<Program>` sharing for v1 — avoids making the compiled program `Send`. Revisit only if worker startup cost shows in profiling.
- **Global MPMC queue** chosen over per-core work-stealing deques for v1 — simplest thing that load-balances. Revisit on queue-contention evidence (P3.3).
- **Copy-transfer** chosen over move-transfer for v1 — boundary crossed once per task; encapsulated-`unsafe` zero-copy deferred (P3.4).
- **`spawn` keyword is a placeholder.** Final surface syntax undecided; backend is syntax-agnostic. The spike ships them as builtins, so `spawn(fn, args…)` is a plain call today.
- **Spike used builtins + `Value::Uint` handles, not opcodes + `Value::Task`.** Faster to land and arguably more YAGNI (zero hot-path churn). Revisit only if we want auto-join-on-use or stronger handle typing (P3.1).
- **`call_with_args` reuses the existing `call_value`/`execute_until` path** rather than duplicating call machinery (DRY).
- **P0.3 is only partially done (◐):** worker VM skips the `main` body but not free-standing top-level statements. Finish before relying on top-level driver code (see Spike results).
