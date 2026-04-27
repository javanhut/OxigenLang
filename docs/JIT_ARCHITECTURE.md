# OxigenLang JIT Architecture

> **Status: experimental.** The baseline JIT is off by default. It's
> feature-gated and ships as an opt-in build. It already outperforms
> CPython on a subset of workloads (tight integer loops, closures) but
> still trails on call-heavy workloads (recursion, struct methods). It
> will stay experimental until every bench in the suite at
> `example/bench_*.oxi` beats CPython 3.14.

This document covers:

- [Status and goals](#status-and-goals)
- [Enabling the JIT](#enabling-the-jit)
- [Runtime modes](#runtime-modes)
- [Architecture overview](#architecture-overview)
- [Supported opcodes](#supported-opcodes)
- [Inline caches and fast paths](#inline-caches-and-fast-paths)
- [Safety invariants](#safety-invariants)
- [Benchmark status](#benchmark-status)
- [Troubleshooting](#troubleshooting)

## Status and goals

**What the JIT is.** A Cranelift-backed baseline JIT that compiles
Oxigen bytecode chunks to native code and installs them as "thunks"
on hot `ObjClosure`s. Every allow-listed opcode becomes a call to an
extern `"C"` runtime helper that reuses the interpreter's logic, plus
a handful of inline fast paths (int arithmetic, compare-branch, local
access, the `Call` guard) that skip the helper for the common case.

**What it is NOT.** Not an optimizing JIT. No inlining, no escape
analysis, no type specialization beyond tag-check fast paths. No LLVM
backend. No concurrent compilation. No deoptimization to a separate
tier — just a clean bailout back to the interpreter.

**Goals.**

1. Be **memory-safe** under Rust's ownership model. Every shortcut the
   JIT takes past Rust's `Rc` / borrow-checker rules is audited and
   documented.
2. Be **correct** on every program the interpreter runs correctly. All
   424+ unit tests and every `.oxi` in `example/` pass in `--jit`,
   `default`, and `--no-jit` modes.
3. Beat CPython 3.14 on every bench in the suite before we drop the
   "experimental" label. Currently we beat it on 3/7; see
   [Benchmark status](#benchmark-status).

## Enabling the JIT

### Build with the `jit` feature

The `jit` feature is off by default because Cranelift pulls in a heavy
dependency tree. To enable:

```bash
# Cargo (dev)
cargo build --release --features jit -p oxigen
cargo run --release --features jit -p oxigen -- path/to/script.oxi --jit

# Makefile install
make build-with-jit
sudo make install-with-jit

# Test with the JIT
cargo test --release --features jit
```

The feature lives on two crates. `oxigen-core` owns the JIT module
(gated by `feature = "jit"`); `oxigen` (the CLI crate) re-exports the
feature flag and surfaces the `--jit` / `--no-jit` CLI switches:

```toml
# crates/oxigen-cli/Cargo.toml
[features]
default = []
jit = ["oxigen-core/jit"]

# core/Cargo.toml
[features]
default = []
jit = [
    "dep:cranelift-jit",
    "dep:cranelift-module",
    "dep:cranelift-codegen",
    "dep:cranelift-frontend",
    "dep:cranelift-native",
]
```

Without `--features jit` the binary still builds and runs — the `--jit`
flag is just unrecognized and the JIT engine is a ZST stub that always
takes the interpreter path.

### Verify

```bash
./target/release/oxigen --jit example/bench_loop.oxi
# Should return quickly (~25 ms; interpreter is ~160 ms).
```

If the binary was built without the feature, `--jit` is silently
ignored and the bench runs at interpreter speed.

## Runtime modes

Once the JIT is compiled in, three tiering modes are available:

| Mode | Threshold | Behavior |
|------|-----------|----------|
| **`--jit`** | 1 | Compile every function on first call. Fastest for one-shot scripts and benchmarks. Also readable via `OXIGEN_JIT=1`. |
| **default** | 50 | Compile a function after 50 calls. Functions with a backward-branch opcode (`has_loop == true`) compile eagerly on first call via a loop-entry thunk, so long-running single-call scripts still benefit. |
| **`--no-jit`** | off | Pure bytecode interpreter. Always correct, always the slowest. Useful for reproducing bugs that vanish under the JIT. |

All three modes pass the full test suite and produce bit-identical
output for every program.

```bash
oxigen --jit script.oxi      # eager
oxigen       script.oxi      # default (threshold-based)
OXIGEN_JIT=1 oxigen script.oxi  # equivalent to --jit
oxigen --no-jit script.oxi   # interpreter only
```

## Architecture overview

```
                          ┌──────────────────────┐
                          │    VM::run(fn)       │
                          └──────────┬───────────┘
                                     │
                                     ▼
                          ┌──────────────────────┐
                          │   call_closure       │
                          │   (slow path)        │
                          └──────────┬───────────┘
                                     │
                   state==0  ┌───────┴────────┐  state==1
                    (cold)   │                │  (compiled)
                             ▼                ▼
                ┌────────────────┐    ┌──────────────────┐
                │ maybe_compile  │    │ call_closure_    │
                │ + push frame   │    │ fast_path        │
                │ + invoke_thunk │    └─────────┬────────┘
                └────────┬───────┘              │
                         │                      ▼
                         ▼            ┌──────────────────┐
                  invoke_thunk ──────▶│  Thunk runs      │
                                      │  (native code)   │
                                      └────────┬─────────┘
                                               │
                  ┌────────────────────────────┤
                  │                            │
            status=0 (OK)                status=1 (err)   status=2 (bail)
                  │                            │              │
                  ▼                            ▼              ▼
          frame popped by            pending_error.take() falls back to
          jit_op_return              returned as VMError   interpreter
```

### Key data structures

**`JitEngine` (in `core/src/jit/mod.rs`)** — the outer facade. Always
present on `VM` regardless of the feature flag. When the feature is
off it's a ZST and every method is a no-op.

**`JitInner` (in `core/src/jit/engine.rs`)** — the actual Cranelift
module + compiled-thunk cache. Holds:
- `module: JITModule` — Cranelift JIT module.
- `compiled: HashMap<*const Function, Entry>` — thunk cache keyed by
  `Rc::as_ptr(&Function)`.
- `retained: HashMap<*const Function, Rc<Function>>` — keeps
  `Function`s alive so the pointer keys stay valid.
- `pending_error: Option<VMError>` — see [Safety
  invariants](#safety-invariants); accessed across the thunk-call
  boundary with volatile pointer reads/writes.
- `current_stop_depth: usize` — the frame depth the currently-running
  thunk should execute until.
- `global_caches`, `call_caches`, `method_caches`, `field_caches` —
  per-call-site inline caches (see next section).

**`JitFrame` and `JitFrameView` (in `core/src/vm/mod.rs`)** —
repr(C) frame records the JIT pushes instead of `CallFrame`. Read
directly from Cranelift IR at fixed offsets. See
[`StackView` and `JitFrameView`](#stackview-and-jitframeview).

**`VM.jit_executing: Cell<bool>`** — flipped at `invoke_thunk` entry
(true) and `execute_until` entry (false). Determines whether
`current_constant` / `active_closure` / `current_slot_offset` /
`current_line` / `active_module_globals` resolve against the JIT
frame stack or the interpreter frame stack. See [Safety
invariants](#safety-invariants).

### StackView and JitFrameView

Both are `#[repr(C)]` so the JIT can load `ptr`/`len` at fixed
byte offsets without any FFI:

```rust
#[repr(C)]
pub struct StackView { pub ptr: *mut Value, pub len: usize }

#[repr(C)]
pub(crate) struct JitFrameView { pub ptr: *mut JitFrame, pub len: usize }
```

**Pre-allocation invariant.** `VM::new()` pre-allocates
`stack: Vec<Value>` to `STACK_MAX` (262 144 slots) and `jit_frames:
Vec<JitFrame>` to `FRAMES_MAX` (16 384 frames). This guarantees no
reallocation ever happens under a JIT pointer, so the cached `ptr` in
each view is stable for the VM's lifetime. Only `len` needs syncing
on each mutation.

**`len` sync.** Every stack-mutating method on `VM` (`push`, `pop`,
`stack_truncate`, `stack_shrink`, `stack_drain_from`, `stack_insert`)
updates `stack_view.len`. The JIT's inline IR fast paths that grow
the stack (today: only the MethodCall IC hit path) call
`jit_stack_commit_len(vm, new_len)` to keep `Vec::len` in sync too —
otherwise bounds-checked helpers like `vm.stack[idx]` would see stale
length and panic.

### Opcode dispatch

Every compiled thunk has the signature `extern "C" fn(vm: *mut VM) ->
u32`. Status codes:

- `0` — Returned normally. The thunk has already popped its own frame
  and left the return value on top of the stack.
- `1` — Runtime error. `JitInner::pending_error` holds the `VMError`.
- `2` — Bailout. The thunk can't continue (e.g. hit an unsupported
  opcode encoding). The frame is still on top; the caller drives
  `execute_until(stop_depth)` to let the interpreter finish.

## Supported opcodes

The JIT's scan (in `core/src/jit/scan.rs`) rejects any function that
contains an opcode outside the allow-list. Current allow-list:

**Constants / values.** `Constant`, `None`, `True`, `False`, `Pop`,
`Dup`, `BuildArray` (via helper).

**Arithmetic.** `Add`, `Subtract`, `Multiply`, `Divide`, `Modulo` —
inline int+int fast path for Add/Sub/Mul, fallible helper for the
rest.

**Comparison.** `Equal`, `NotEqual`, `Less`, `LessEqual`, `Greater`,
`GreaterEqual` — inline int fast path, fused with `JumpIfFalse` /
`JumpIfTrue` when followed.

**Logic.** `Not`, `Negate`.

**Locals.** `GetLocal`, `SetLocal` — inline fast path for
Integer/Float, slow-path helper for everything else. Peephole fusions
for `GetLocal+Const+Arith+SetLocal`, `GetLocal+GetLocal+Arith+SetLocal`,
`GetLocal+Array[Mod]+SetLocal`.

**Globals.** `GetGlobal` (inline cache), `SetGlobal`, `DefineGlobal`,
`DefineGlobalTyped`.

**Upvalues.** `GetUpvalue`, `SetUpvalue`, `CloseUpvalue`, `Closure`.

**Control flow.** `Jump`, `JumpIfFalse`, `JumpIfTrue`, `Loop`,
`PopJumpIfFalse`.

**Calls.** `Call` (inline IR guard + `jit_op_call_hit` /
`jit_op_call_miss`), `Return` (`jit_op_return`).

**Structs.** `StructDef`, `StructLiteral`, `GetField` (field IC),
`SetField` (field IC), `DefineMethod`, `MethodCall` (inline IR guard
for `arg_count <= 1` + `jit_op_method_call_ic`).

**Not supported (yet).** `Log` / `println`, `BuildTuple` /
`BuildMap` / `BuildSet`, `Index` / `IndexAssign` / `Slice`, `Import` /
`GetModuleField`, `IsMut` / `IsType` / `IsTypeMut`, `TypeWrap`,
`ErrorConstruct` / `Guard` / `Fail` / `ValueConstruct`,
`DefinePattern` / `TestPattern`, `StringInterp`, `Unpack`, `Main`,
`Unless`, `IterLen` / `IterGet`, bitwise ops, `CallNamed`.
Functions that contain any of these are kept on the interpreter.

## Inline caches and fast paths

### Per-call-site caches

Each compiled Call / MethodCall / GetField / SetField / GetGlobal site
gets its own `Box<CacheEntry>`. The box address is baked into the
emitted IR as an `iconst`, so hits are a single indirect load.

| Cache | Stored | Invalidation |
|-------|--------|--------------|
| `GlobalCacheEntry` | `version: u64`, `value: Value` | `vm.globals_version` bump on any `DefineGlobal`/`SetGlobal` |
| `CallCacheEntry` | `closure_raw: *const ObjClosure` (raw RcBox ptr), `thunk_raw: *const ()`, `arity: u8`, `_keeper: Option<Rc<ObjClosure>>` | `Rc::ptr_eq`-style raw-pointer compare on each call |
| `MethodCacheEntry` | `struct_def_raw`, `closure_raw`, `thunk_raw`, `arity`, plus `_keeper` Rcs | Struct-def pointer compare |
| `FieldCacheEntry` | `struct_def_raw`, `field_index: u32`, `kind: FieldCacheKind` (Invalid / InstanceField / DefMethod), plus `_keeper` Rcs | Struct-def pointer compare |

### Inline IR fast paths

- **Int+int arithmetic.** `Add`, `Sub`, `Mul` check both operands'
  tag bytes at `stack_ptr + (len - N) * VALUE_SIZE` (using
  `stack_view`, no FFI). If both are `VALUE_TAG_INTEGER`, the
  arithmetic happens in registers and the result is stored
  in-place.
- **Fused compare+branch.** `Less`/`LessEqual`/`Greater`/`GreaterEqual`
  followed by `JumpIf*` becomes a single `icmp` + `brif`, skipping
  the Boolean round-trip.
- **Int constant specialization.** `Constant` loads of
  `Value::Integer` / `Value::Float` go through `jit_push_integer_inline`
  / `jit_push_float_inline` to skip the constants-pool lookup.
- **Local fast path.** `GetLocal` / `SetLocal` check the slot's tag
  byte and bypass the generic push/pop for primitives. The parent
  frame's `slot_offset` is cached in an SSA var at thunk entry (one
  helper call per activation).
- **GetGlobal IC.** Version-checked single pointer compare + Rc clone
  on hit, HashMap lookup on miss (which also repopulates the cache).
- **Call IC.** At each Call site the IR reads the callee's tag byte
  and `Rc<ObjClosure>` bit pattern directly from `stack_view`,
  compares against `cache.closure_raw`, and on hit calls
  `jit_op_call_hit` (tight frame push + thunk invoke). On miss it
  calls `jit_op_call_miss` (generic path + cache populate).
- **MethodCall IC hit path (experimental).** For `arg_count <= 1` the
  IR rearranges the stack via raw memcpy of `Value` words, stamps a
  fresh `Value::Closure` at the receiver slot, bumps the method
  closure's `RcBox` strong count inline (3-instruction
  `load/iadd/store` at RcBox+0), calls `jit_stack_commit_len` to sync
  `Vec::len`, pushes a `JitFrame`, and `call_indirect`s the cached
  thunk. See [Safety invariants](#safety-invariants) for the
  memory-ownership accounting.

## Safety invariants

The JIT takes several shortcuts that bypass Rust's usual guarantees.
Each one is only safe because of a specific invariant. If you
introduce a new IR path that touches any of these, verify the
invariant still holds.

### 1. Stack backing storage is stable

`VM::new()` pre-allocates `Vec::with_capacity(STACK_MAX)` for the
stack. As long as nothing does `self.stack.shrink_to_fit()` or
replaces the `Vec`, the backing allocation pointer captured in
`stack_view.ptr` remains valid. No helper currently shrinks or
reallocates the stack.

### 2. `stack_view.len` and `Vec::len` must stay in sync

The JIT reads `stack_view.len` via raw load. Some helpers use
`self.stack[idx]` (bounds-checked via `Vec::len`). If IR grows the
stack via raw stores past `Vec::len`, it must call
`jit_stack_commit_len(vm, new_len)` before invoking any helper that
uses Vec indexing (notably `jit_get_local`'s non-primitive fallback
path).

### 3. Raw Rc pointers require refcount accounting

The JIT caches `Rc<T>` identity as the raw `NonNull<RcBox<T>>` bit
pattern (not `Rc::as_ptr`, which points to T — offset 16 past in
practice). When IR creates a *new* `Value` that holds a cached Rc
(e.g. `emit_write_closure_value` in the MethodCall hit path), it
**must** bump the `RcBox` strong count inline:

```cranelift
strong     = load.i64 rcbox_ptr, 0
strong_inc = iadd_imm strong, 1
store      flags, strong_inc, rcbox_ptr, 0
```

Rc strong count lives at `RcBox` offset 0 (before `weak` at 8 and
`value` at `RC_VALUE_OFFSET = 16`). This layout is pinned by the
`rc_strong_count_lives_at_rcbox_offset_zero` test in
`core/src/vm/value.rs`. Non-atomic stores are correct: `Rc` is
`!Send`/`!Sync` and the VM is single-threaded.

Moves (where source is overwritten immediately after) don't need a
bump — they transfer ownership, not clone it.

### 4. Frame context: JIT vs interpreter

`VM.jit_executing: Cell<bool>` distinguishes which frame stack is
"live":

- `invoke_thunk` saves the current flag, sets it to `true`, runs the
  thunk, restores on exit.
- `execute_until` saves, sets to `false`, runs the interpreter,
  restores on exit.

Every "active frame" accessor (`current_constant`, `active_closure`,
`current_slot_offset`, `current_line`, `active_module_globals`)
checks this flag to decide between `jit_frame_top()` and
`frames.last()`. Without this, an interpreter call running *under*
a paused JIT frame reads constants from the JIT's chunk → "expected
closure constant" errors or silent corruption.

### 5. `pending_error` must not be cached across thunk calls

`invoke_thunk` writes `pending_error = None` before the thunk and
reads it after. The thunk, through the `vm` pointer, reaches back to
the same `JitInner` via `vm.jit.inner.as_mut()` and calls
`stash_error`. LLVM can't see that these two paths alias the same
memory, so by default it caches the pre-call `None` across the
opaque `thunk(vm)` call — the stashed error is silently lost and
`"JIT runtime error with no stashed detail"` surfaces.

Fix: `invoke_thunk` accesses `pending_error` and `current_stop_depth`
through **`write_volatile`** and **`read_volatile`** on raw pointers.
Volatile forces the compiler to treat each access as an observable
side effect that cannot be reordered across the opaque call. This is
the minimum ceremony that lets the JIT and Rust safely share the
`pending_error` slot.

### 6. Call-site ICs trust `Rc::ptr_eq` identity

`CallCacheEntry` / `MethodCacheEntry` / `FieldCacheEntry` each hold
an `Option<Rc<T>>` keeper that prevents the cached allocation from
being freed. The IR compares the keeper's raw bit pattern against
the current callee's raw Rc, which is `Rc::ptr_eq`-equivalent
because `Rc` is a `NonNull<RcBox<T>>`. When the keeper is `None`
the cache is considered empty (raw pointer is null).

## Benchmark status

Reproduce with:

```bash
cargo build --release --features jit
OXIGEN_BENCH_CPU=5 python3 scripts/bench.py --python --runs 10 --warmups 3 \
    example/bench_fib.oxi \
    example/bench_arith.oxi \
    example/bench_loop.oxi \
    example/bench_nested_loop.oxi \
    example/bench_collatz.oxi \
    example/bench_closure.oxi \
    example/bench_struct_method.oxi
```

Latest measurements (CPython 3.14, Linux x86-64, best-of-10,
CPU-pinned):

| Bench | Oxigen `--jit` | Python 3.14 | Ratio | Winner |
|---|---|---|---|---|
| `bench_loop` (tight int loop) | 24.9 ms | 74.0 ms | **2.97×** | Oxigen ✓ |
| `bench_nested_loop` (nested int loops) | 13.3 ms | 26.7 ms | **2.02×** | Oxigen ✓ |
| `bench_closure` (upvalue hot call) | 39.3 ms | 51.7 ms | **1.31×** | Oxigen ✓ |
| `bench_arith` (recursive mixed arithmetic) | 67.2 ms | 56.5 ms | 0.84× | Python |
| `bench_fib` (recursive `fib(30)`) | 147.0 ms | 104.4 ms | 0.71× | Python |
| `bench_collatz` (mod/div loop) | 436.7 ms | 323.8 ms | 0.74× | Python |
| `bench_struct_method` (hot method loop) | 191.2 ms | 69.7 ms | 0.36× | Python |

**Where the JIT wins.** Tight numeric loops (int arithmetic fast path,
fused compare+branch, local-access peephole). Closures (upvalue
access stays in native code once both the outer and inner function
are tiered up).

**Where it loses.** Call-heavy and method-heavy code. Each call pays
for:

- Two FFI crossings per Call (`jit_op_call_{hit,miss}` + `invoke_thunk`
  for nested invocations).
- A `JitFrame` push through the `jit_frame_view` ptr + Rc bookkeeping.
- The `jit_executing` flag save/restore.
- `pending_error` volatile writes.

CPython's hand-optimized C call path is ~38 ns/call; Oxigen's is
~50-60 ns/call. Closing that on a baseline JIT is the remaining work.

## Troubleshooting

**"JIT runtime error with no stashed detail"** — means a fallible
helper returned status 1 without stashing a proper `VMError`.
Usually indicates a missed `vm.jit.stash_error(e)` call on a new
runtime helper, or a new code path that writes `pending_error`
across an opaque boundary without `write_volatile`. See
[Safety invariant 5](#safety-invariants).

**Panics with `stack_slot` out of bounds** — `stack_view.len` and
`Vec::len` have desynced. New IR path that grows the stack via raw
stores must call `jit_stack_commit_len` before invoking any
bounds-checked helper. See [invariant 2](#safety-invariants).

**Segfaults under `--jit` but not `--no-jit`** — most likely a new IR
path that creates or copies `Value` bit patterns without
`Rc::clone`. Every new IR write site that synthesizes a heap-backed
`Value` must either route through Rust code that performs the clone
or emit an inline `strong += 1` on the `RcBox`. See
[invariant 3](#safety-invariants).

**"expected closure constant"** — `current_constant` read the wrong
frame's chunk. Check that whoever called it honors the
`jit_executing` flag, and that `invoke_thunk` / `execute_until`
correctly save and restore it across nesting. See
[invariant 4](#safety-invariants).

**Cranelift verifier errors** (e.g. `"uses value v23 from
non-dominating inst29"`) — an SSA value is used in a block that
isn't dominated by its definition. Move the definition to the
common entry block, or pass it through a block parameter. Enable
with `OXIGEN_JIT_DEBUG=1` (prints `define_function` errors to
stderr).

## See also

- `core/src/jit/engine.rs` — Cranelift IR emission and helper tables.
- `core/src/jit/runtime.rs` — `extern "C"` helpers called from
  compiled code.
- `core/src/jit/scan.rs` — opcode allow-list and fixed-length table.
- `core/src/vm/mod.rs` — `VM`, `CallFrame`, `JitFrame`, `StackView`,
  `JitFrameView`, `invoke_thunk` call path.
- `core/tests/jit_fallback.rs` — regression tests that run every
  supported opcode through both interpreter and JIT and assert
  matching output.
- `scripts/bench.py` — benchmark harness (supports `--python` flag
  for side-by-side CPython comparison).
