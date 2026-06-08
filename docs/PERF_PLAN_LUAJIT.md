# LuaJIT-Inspired Performance Plan for OxigenLang

> **Status: proposal / awaiting approval.** No source changes have been made.
> This document is the implementation plan. Phases 1–2 contain turnkey,
> copy-paste-ready code. Phases 3–4 are anchored designs with code
> skeletons; their final diffs require a focused read of the named
> emission functions in `core/src/jit/engine/mod.rs` (336 KB) before they
> are written, and that read is called out explicitly in each phase.

## Why these four phases

OxigenLang's own `docs/JIT_ARCHITECTURE.md` benchmark table shows the JIT
**wins** on tight loops and **loses** on call/method/recursion-heavy code
(`bench_fib` 0.71×, `bench_collatz` 0.74×, `bench_struct_method` 0.36× vs
CPython). LuaJIT's speed comes from two coupled facts: it is a *tracing*
JIT (inlining across calls is free) and its *deoptimization is cheap* (so
it can speculate and strip guards). OxigenLang is a per-function method
JIT that can only bail at function entry — that single constraint is why
calls are its bottleneck.

The phases below are ordered by ROI / risk:

| Phase | Change | Targets | Architecture change? | Effort |
|------:|--------|---------|----------------------|--------|
| 1 | Hash-backed `Map`/`Set` | map/set workloads | No | S |
| 2 | Bounded string interning | map keys, equality, alloc churn | No | S–M |
| 3 | JIT→JIT direct calls + self-recursion | `fib`, `arith`, all calls | No (extends existing spec entries) | M |
| 4 | Mid-function deopt (snapshots) | everything; unlocks speculation | **Yes** (the strategic bet) | L |

Phases 1–2 are interpreter-level and independent of the `jit` feature.
Phase 3 builds directly on the existing `SpecializedEntryKind::NativeIntBody`
work. Phase 4 is the gateway to LuaJIT-class optimization (narrowing, LICM,
allocation sinking) — none of which pay off before deopt exists.

---

## Phase 1 — Hash-backed `Map` and `Set`

### Problem (verified in source)

`core/src/vm/value.rs:348-349`:
```rust
Map(Rc<RefCell<Vec<(Value, Value)>>>),
Set(Rc<RefCell<Vec<Value>>>),
```

Every map lookup is a linear scan — `core/src/vm/mod.rs:3127-3134`:
```rust
(Value::Map(m), _) => {
    let borrowed = m.borrow();
    for (k, v) in borrowed.iter() {
        if k == &index {
            return Ok(v.clone());
        }
    }
    Ok(Value::None)
}
```
`eval_index_assign` (`mod.rs:3162-3171`) and `BuildSet` dedup
(`mod.rs:1694-1700`) are likewise O(n). A map/set with N entries costs
O(N) per access and O(N²) to build.

### Design

Introduce two ordered, hash-indexed container types in a new module
`core/src/vm/collections.rs`. They preserve **insertion order** (current
behavior that iteration/printing depend on) while giving O(1) average
lookup via a side index keyed by a hashable projection of `Value`.

Non-hashable keys (`Array`, `Map`, `Set`, `StructInstance`, …) and the
±0.0 / NaN float edge cases fall back to a linear scan over `entries`, so
**equality semantics are preserved exactly** for the common path and only
the documented float-key edge changes (see note).

### New file: `core/src/vm/collections.rs`

```rust
//! Ordered, hash-indexed Map/Set backing stores.
//!
//! Replaces the previous `Vec<(Value, Value)>` / `Vec<Value>` linear-scan
//! representations. Insertion order is preserved (iteration and Display
//! depend on it); lookups are O(1) average via `index`, with a linear
//! fallback for keys that have no hashable projection (`HashKey::None`
//! arm is a real key; "no projection" means `try_hash_key` returned
//! `None`).

use std::collections::HashMap;
use std::rc::Rc;

use super::value::Value;

/// Hashable projection of a `Value` usable as a map/set key.
///
/// Mirrors `Value`'s `PartialEq` for the hashable kinds. `Integer` and
/// `Float` are deliberately distinct (matching `Value::eq`, where
/// `Integer(1) != Float(1.0)`).
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum HashKey {
    Int(i64),
    /// Canonicalized f64 bits: `-0.0` folds to `+0.0`, all NaNs fold to a
    /// single quiet-NaN pattern. See `f64_key_bits` and the note below.
    Float(u64),
    Bool(bool),
    Char(char),
    Byte(u8),
    Uint(u64),
    /// Content-hashed/compared via `Rc<String>`'s `Hash`/`Eq` (delegates
    /// to `String`), so distinct `Rc`s with equal contents are one key.
    Str(Rc<String>),
    None,
    /// Tuple of hashable elements; `None` if any element is non-hashable.
    Tuple(Vec<HashKey>),
}

#[inline]
fn f64_key_bits(f: f64) -> u64 {
    if f == 0.0 {
        0 // unify +0.0 and -0.0 (they are `==`, so must be one key)
    } else if f.is_nan() {
        0x7ff8_0000_0000_0000 // canonical qNaN
    } else {
        f.to_bits()
    }
}

impl Value {
    /// Hashable projection for use as a Map/Set key. `None` means the
    /// value is not hashable and callers must use a linear scan.
    pub fn try_hash_key(&self) -> Option<HashKey> {
        Some(match self {
            Value::Integer(i) => HashKey::Int(*i),
            Value::Float(f) => HashKey::Float(f64_key_bits(*f)),
            Value::Boolean(b) => HashKey::Bool(*b),
            Value::Char(c) => HashKey::Char(*c),
            Value::Byte(b) => HashKey::Byte(*b),
            Value::Uint(u) => HashKey::Uint(*u),
            Value::String(s) => HashKey::Str(s.clone()),
            Value::None => HashKey::None,
            Value::Tuple(t) => {
                let mut ks = Vec::with_capacity(t.len());
                for e in t.iter() {
                    ks.push(e.try_hash_key()?);
                }
                HashKey::Tuple(ks)
            }
            _ => return None, // Array, Map, Set, StructInstance, closures…
        })
    }
}

/// Insertion-ordered map with O(1) average lookup for hashable keys.
#[derive(Debug, Clone, Default)]
pub struct OxMap {
    entries: Vec<(Value, Value)>,
    index: HashMap<HashKey, usize>,
}

impl OxMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Borrow the ordered entries (for iteration / Display / builtins).
    pub fn entries(&self) -> &[(Value, Value)] {
        &self.entries
    }
    pub fn iter(&self) -> std::slice::Iter<'_, (Value, Value)> {
        self.entries.iter()
    }

    pub fn get(&self, key: &Value) -> Option<&Value> {
        if let Some(hk) = key.try_hash_key() {
            self.index.get(&hk).map(|&i| &self.entries[i].1)
        } else {
            self.entries.iter().find(|(k, _)| k == key).map(|(_, v)| v)
        }
    }

    pub fn contains_key(&self, key: &Value) -> bool {
        if let Some(hk) = key.try_hash_key() {
            self.index.contains_key(&hk)
        } else {
            self.entries.iter().any(|(k, _)| k == key)
        }
    }

    /// Insert or overwrite, preserving insertion order on overwrite.
    pub fn insert(&mut self, key: Value, val: Value) {
        if let Some(hk) = key.try_hash_key() {
            if let Some(&i) = self.index.get(&hk) {
                self.entries[i].1 = val;
            } else {
                let i = self.entries.len();
                self.entries.push((key, val));
                self.index.insert(hk, i);
            }
        } else if let Some(e) = self.entries.iter_mut().find(|(k, _)| *k == key) {
            e.1 = val;
        } else {
            self.entries.push((key, val));
        }
    }

    /// Remove by key, preserving order. Returns the removed value.
    /// O(n) (entry shift + index rebuild); deletes are rare.
    pub fn remove(&mut self, key: &Value) -> Option<Value> {
        let pos = if let Some(hk) = key.try_hash_key() {
            self.index.get(&hk).copied()
        } else {
            self.entries.iter().position(|(k, _)| k == key)
        }?;
        let (_, v) = self.entries.remove(pos);
        self.reindex();
        Some(v)
    }

    fn reindex(&mut self) {
        self.index.clear();
        for (i, (k, _)) in self.entries.iter().enumerate() {
            if let Some(hk) = k.try_hash_key() {
                self.index.insert(hk, i);
            }
        }
    }

    pub fn keys(&self) -> impl Iterator<Item = &Value> {
        self.entries.iter().map(|(k, _)| k)
    }
    pub fn values(&self) -> impl Iterator<Item = &Value> {
        self.entries.iter().map(|(_, v)| v)
    }

    pub fn from_pairs(pairs: Vec<(Value, Value)>) -> Self {
        let mut m = Self::new();
        for (k, v) in pairs {
            m.insert(k, v);
        }
        m
    }
}

impl PartialEq for OxMap {
    fn eq(&self, other: &Self) -> bool {
        // Order-insensitive, matching the old Vec equality which compared
        // `*a.borrow() == *b.borrow()` only when build order matched. We
        // upgrade to order-insensitive (key-based) which is the correct
        // map semantics; see migration note for the behavior delta.
        if self.entries.len() != other.entries.len() {
            return false;
        }
        self.entries.iter().all(|(k, v)| other.get(k) == Some(v))
    }
}

/// Insertion-ordered set with O(1) average membership for hashable elems.
#[derive(Debug, Clone, Default)]
pub struct OxSet {
    items: Vec<Value>,
    index: HashMap<HashKey, usize>,
}

impl OxSet {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn len(&self) -> usize {
        self.items.len()
    }
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
    pub fn items(&self) -> &[Value] {
        &self.items
    }
    pub fn iter(&self) -> std::slice::Iter<'_, Value> {
        self.items.iter()
    }

    pub fn contains(&self, v: &Value) -> bool {
        if let Some(hk) = v.try_hash_key() {
            self.index.contains_key(&hk)
        } else {
            self.items.iter().any(|i| i == v)
        }
    }

    /// Insert if absent. Returns true if newly inserted.
    pub fn insert(&mut self, v: Value) -> bool {
        if self.contains(&v) {
            return false;
        }
        if let Some(hk) = v.try_hash_key() {
            self.index.insert(hk, self.items.len());
        }
        self.items.push(v);
        true
    }

    pub fn from_iter_dedup(vals: impl IntoIterator<Item = Value>) -> Self {
        let mut s = Self::new();
        for v in vals {
            s.insert(v);
        }
        s
    }
}

impl PartialEq for OxSet {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.items.iter().all(|i| other.contains(i))
    }
}
```

### Edits to existing files

**`core/src/vm/mod.rs`** — register the module near the other `mod`
declarations (top of the vm module):
```rust
pub(crate) mod collections;
```

**`core/src/vm/value.rs:348-349`** — change the variant payloads:
```rust
    Map(Rc<RefCell<crate::vm::collections::OxMap>>),
    Set(Rc<RefCell<crate::vm::collections::OxSet>>),
```
And the matching `ValueRepr` borrows at `value.rs:411-412`:
```rust
    Map(&'a Rc<RefCell<crate::vm::collections::OxMap>>),
    Set(&'a Rc<RefCell<crate::vm::collections::OxSet>>),
```
And `as_map`/`as_set` return types (`value.rs:801-805`):
```rust
    #[inline] pub fn as_map(&self) -> Option<&Rc<RefCell<crate::vm::collections::OxMap>>> {
        if let Value::Map(m) = self { Some(m) } else { None }
    }
    #[inline] pub fn as_set(&self) -> Option<&Rc<RefCell<crate::vm::collections::OxSet>>> {
        if let Value::Set(s) = self { Some(s) } else { None }
    }
```

**`value.rs:1082-1087`** — equality now delegates to the container impls
(no behavior change beyond the documented order-insensitivity for maps):
```rust
            (Value::Map(a), Value::Map(b)) => *a.borrow() == *b.borrow(),
            (Value::Set(a), Value::Set(b)) => *a.borrow() == *b.borrow(),
```
(The map arm is unchanged textually; `OxMap::PartialEq` provides the new
semantics. The set arm replaces the old inline all/any loop.)

**`core/src/vm/mod.rs` — `eval_index` (3127-3135):**
```rust
            (Value::Map(m), _) => Ok(m.borrow().get(&index).cloned().unwrap_or(Value::None)),
```

**`mod.rs` — `eval_index_assign` (3162-3172):**
```rust
            (Value::Map(m), _) => {
                m.borrow_mut().insert(index, value);
                Ok(())
            }
```

**`mod.rs` — `BuildMap` (1680-1689):**
```rust
                OpCode::BuildMap => {
                    let count = self.frames[frame_idx].read_u16() as usize;
                    let start = self.stack.len() - count * 2;
                    let flat: Vec<Value> = self.stack_drain_from(start);
                    let mut map = crate::vm::collections::OxMap::new();
                    for pair in flat.chunks(2) {
                        map.insert(pair[0].clone(), pair[1].clone());
                    }
                    self.push(Value::Map(Rc::new(RefCell::new(map))));
                }
```

**`mod.rs` — `BuildSet` (1690-1701):**
```rust
                OpCode::BuildSet => {
                    let count = self.frames[frame_idx].read_u16() as usize;
                    let start = self.stack.len() - count;
                    let elements: Vec<Value> = self.stack_drain_from(start);
                    let set = crate::vm::collections::OxSet::from_iter_dedup(elements);
                    self.push(Value::Set(Rc::new(RefCell::new(set))));
                }
```

**`core/src/vm/builtins.rs`** — mechanical migration of the ~12 borrow
sites. The pattern is identical at each:

| Old | New |
|-----|-----|
| `m.borrow().len()` | unchanged (`OxMap::len`) |
| `m.borrow().iter()` | unchanged (`OxMap::iter`) |
| `entries.push((k, v))` after building a `Vec` | build `OxMap`/`OxSet` and `insert` |
| `Value::Map(Rc::new(RefCell::new(entries)))` where `entries: Vec<(Value,Value)>` | `Value::Map(Rc::new(RefCell::new(OxMap::from_pairs(entries))))` |
| `Value::Set(Rc::new(RefCell::new(items)))` | `Value::Set(Rc::new(RefCell::new(OxSet::from_iter_dedup(items))))` |
| a map `get`/`contains`/`set` builtin doing a manual scan | call `OxMap::get` / `contains_key` / `insert` |

Exact sites (from grep): `builtins.rs` 194-195, 381, 394, 407, 419, 435,
440, 454, 458, 478, 895, 942, 1475, 1500, 1546. Each is a 1–3 line change;
the construction sites (`478`, `895`, `942`, `1475`, `1500`) use the
`from_pairs` / `from_iter_dedup` constructors above.

**`core/src/vm/nanvalue.rs`** — this is the abandoned NaN-box experiment
(`OldValue::Map`/`OldValue::Set` at 849-983, 1554-1605). It is gated/dead
relative to the live `Value`. Confirm with `cargo build` whether it
compiles into the default build; if it does, apply the same payload swap,
otherwise leave untouched (it is not on the hot path). **Do not** spend
effort optimizing it.

### Note on the only behavior delta

1. **Float keys `±0.0` / `NaN`.** `-0.0` and `+0.0` become one key
   (they are already `==`, so this is *more* correct). `NaN` becomes a
   single stable key instead of being un-findable (old linear scan used
   `==`, and `NaN != NaN`, so a `NaN` key could never be read back and
   could be inserted repeatedly). This is a strict improvement and matches
   common language semantics, but it is a behavior change — call it out in
   the changelog.
2. **Map equality is now order-insensitive** (correct map semantics). The
   old `Vec` equality was order-*sensitive*. If any test asserts that two
   maps built in different orders are unequal, it was asserting an
   implementation artifact; update it.

### Tests for Phase 1

Add `core/tests/collections.rs`:
- `map_insert_get_overwrite_preserves_order`
- `map_int_vs_float_keys_distinct` (`m[1]` and `m[1.0]` are different)
- `map_nan_key_is_stable` (insert NaN, read back returns the value)
- `map_non_hashable_key_falls_back` (Array key still works via linear path)
- `set_dedup_and_membership`
- `map_eq_order_insensitive`
- A perf smoke test: build a 100k-entry map and assert wall-clock is
  sub-linear vs the old behavior (guard against regressing to O(n)).

Run: `cargo test -p oxigen-core collections` and the full suite
(`cargo test`) in default, `--features jit`, and `--no-jit` modes per the
JIT doc's correctness contract.

---

## Phase 2 — Bounded string interning

### Goal and the constraint that shapes it

LuaJIT interns *every* string and relies on its GC to evict them. OxigenLang
uses `Rc<String>` with **no GC**, so interning every transient runtime
string (concatenations, user input, formatted output) would grow the intern
table unboundedly — a leak. Therefore interning here is **bounded to
symbol-like strings**: constant-pool string literals, identifiers, field /
method / map-key names. These are finite (bounded by program text) and are
exactly the strings used as keys and in equality checks.

Today struct fields/methods are already `HashMap<String, _>`-keyed
(`def.methods` at `mod.rs:622-623`, `inst.layout.indices` at `mod.rs:2657`),
so the field/method *dispatch* path is already O(1) and JIT-cached. The
concrete wins from interning are therefore narrower than a naive reading
suggests:
- O(1) pointer equality for string **map keys** (compounds with Phase 1:
  `HashKey::Str` could become `HashKey::Symbol(u32)`).
- Reduced allocation for repeated literal materialization.

### Design

A process-local interner keyed by content, returning canonical
`Rc<String>` (so identity == equality). Single-threaded VM ⇒ no locking
beyond a `thread_local! RefCell`.

New file `core/src/vm/intern.rs`:
```rust
//! Bounded string interner for symbol-like strings (literals, identifiers,
//! field/method/key names). NOT for arbitrary runtime strings — there is
//! no GC to evict them, so only intern strings whose count is bounded by
//! the program text.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

thread_local! {
    static INTERN: RefCell<HashMap<Box<str>, Rc<String>>> = RefCell::new(HashMap::new());
}

/// Return the canonical `Rc<String>` for `s`, interning it if new.
/// Use ONLY for symbol-like strings (see module docs).
pub fn intern(s: &str) -> Rc<String> {
    INTERN.with(|t| {
        let mut t = t.borrow_mut();
        if let Some(rc) = t.get(s) {
            return rc.clone();
        }
        let rc = Rc::new(s.to_owned());
        t.insert(s.into(), rc.clone());
        rc
    })
}
```

### Where to call `intern`

1. **Compiler constant pool** (`core/src/compiler/mod.rs`): when adding a
   string constant that originates from an identifier, field name, method
   name, or string literal used as a key, route it through `intern`
   instead of `rc_str`. Anchor: every `Chunk::add_constant(Value::String(...))`
   that comes from a name token.
2. **`Value::String` equality fast path** (`value.rs:1075`): add a pointer
   short-circuit before the content compare:
   ```rust
   (Value::String(a), Value::String(b)) => Rc::ptr_eq(a, b) || a == b,
   ```
   This is always correct (pointer-equal ⇒ content-equal) and makes the
   common interned-key case a single pointer compare.

### Optional follow-on (defer until measured)

Replace `HashKey::Str(Rc<String>)` from Phase 1 with a `Symbol(u32)` id
once interning assigns stable ids, turning string-keyed map lookups into
integer-keyed lookups. Only do this if profiling shows string hashing in
map-heavy code is hot; otherwise the `Rc<String>` content hash is fine.

### Risk / decision required

Interning is a **smaller, more diffuse win** than Phase 1 and carries a
correctness obligation (never intern unbounded runtime strings). Recommend
implementing the `Rc::ptr_eq` fast path (trivial, always safe) immediately,
and gating the compiler-side interning behind a measured map/field
benchmark. **This phase is optional and lower priority than Phase 3.**

---

## Phase 3 — JIT→JIT direct calls and self-recursion

> **Implementation note:** the exact diff requires reading the `Call`
> opcode emitter and the call-site IC code in `core/src/jit/engine/mod.rs`
> (the `compile_function` dispatch loop) plus `core/src/jit/runtime.rs`
> (`jit_op_call_hit` / `jit_op_call_miss`). The integration points and the
> data already in place are identified precisely below; the skeleton shows
> the shape of the change. Final IR-emission code is produced after that
> read, with approval.

### The cost being removed (from `docs/JIT_ARCHITECTURE.md`)

Each `Call` today pays: the Call IC compare, `jit_op_call_hit`/`miss`
(FFI), `invoke_thunk` (FFI) for nested invocation, a `JitFrame` push + Rc
bookkeeping, the `jit_executing` flag save/restore, and `pending_error`
volatile writes — ~50–60 ns vs LuaJIT's ~handful of instructions.

### What is already in place

- `CompiledEntries { generic, specialized, specialized_arity, specialized_kind }`
  (`core/src/jit/mod.rs:54-59`).
- `SpecializedEntryKind::NativeIntBody` / `NativeIntBodyWithClosure`
  (`core/src/jit/engine/defs.rs:91-112`) — a real `fn(*mut VM, i64, …, i64)
  -> (u32 status, i64 payload)` body with args in registers, no box/unbox.
- `JitEngine::maybe_compile_entries_for` already returns the specialized
  pointer + kind to the VM (`mod.rs:209-233`), and the comment at
  `defs.rs:92-99` states the explicit goal: "A3 must NOT direct-call
  trampolines" — i.e. the direct-call path is the planned next step and
  the eligibility gating already exists.

### Design

At a `Call` site inside compiled code, when the Call IC is monomorphic and
the cached callee has a `specialized` entry whose kind is
`NativeIntBody`(`WithClosure`) **and** the statically-known argument types
at the call site are all int (available from `slot_types.rs`), emit a
direct `call_indirect` to the specialized entry:

- arguments passed in registers (already materialized as SSA i64s on the
  int fast path) — **no** VM-stack marshaling;
- result returned in a register — pushed directly;
- **skip** `jit_op_call_hit`, `invoke_thunk`, the `jit_executing`
  save/restore, and the `pending_error` volatile dance, because no FFI
  boundary is crossed (both sides are native, same `*mut VM`);
- on IC identity-guard miss, fall through to the existing
  `jit_op_call_miss` slow path (unchanged).

Self-recursion is the special case where the cached callee *is* the
function currently being compiled: the specialized `FuncId` is known at
emit time, so the recursive call needs no IC at all — emit a direct
`call` to our own specialized entry. This is the `bench_fib` / `bench_arith`
win.

### Skeleton (shape; final code after the emitter read)

In the `OpCode::Call` arm of `compile_function`, at the point where the IC
hit branch is emitted:
```rust
// PSEUDO-ANCHORED: inside the Call-site emitter, IC-hit block.
// Preconditions established earlier in this arm:
//   - `callee_cache: *const CallCacheEntry` baked as iconst (exists today)
//   - int fast path proved arg SSA values `arg_vals: Vec<ir::Value>` are i64
//   - `self_func_id: Option<FuncId>` set when the callee is this function
if let Some(spec) = callee_specialized_entry(/* from cache or self */) {
    // Build the specialized signature: (i64 vm, i64×arity) -> (i32, i64)
    let mut sig = self.module.make_signature();
    sig.params.push(AbiParam::new(types::I64)); // vm
    for _ in 0..arity {
        sig.params.push(AbiParam::new(types::I64));
    }
    sig.returns.push(AbiParam::new(types::I32)); // status
    sig.returns.push(AbiParam::new(types::I64)); // payload
    let sigref = builder.import_signature(sig);

    let mut args = Vec::with_capacity(arity as usize + 1);
    args.push(vm_ptr);
    args.extend_from_slice(&arg_vals);

    let call = match self_func_id {
        Some(fid) => {
            // Self-recursion: direct call to our own specialized entry.
            let fref = self.module.declare_func_in_func(fid, builder.func);
            builder.ins().call(fref, &args)
        }
        None => {
            // Monomorphic callee: indirect call to cached spec pointer.
            let callee = builder.ins().iconst(types::I64, spec as i64);
            builder.ins().call_indirect(sigref, callee, &args)
        }
    };
    let results = builder.inst_results(call);
    let (status, payload) = (results[0], results[1]);
    // status != 0 → branch to the shared exit_block with (status, 0)
    // status == 0 → push `payload` as the int result and continue
    // (reuses the existing exit_block plumbing described in defs.rs)
}
```

### Correctness obligations

- The specialized ABI contract (args are trusted-int per
  `defs.rs:85` / `EntryKind::IntSpecialized`) must be guaranteed by the
  call-site type proof; if any arg is not statically int, do **not** take
  the direct path — fall to the IC/generic path.
- Frame accounting: the specialized callee pushes/pops its own `JitFrame`;
  the direct call must preserve the same `current_stop_depth` invariant the
  generic path maintains (verify against `invoke_thunk` in `mod.rs`).
- Re-entrancy of `pending_error`: a direct native→native call does not
  cross the opaque thunk boundary, so the volatile dance is unnecessary —
  but confirm no helper reached transitively reads `pending_error` without
  the volatile (Safety invariant 5 in the JIT doc).

### Tests

- Extend `core/tests/jit_fallback.rs`: assert `bench_fib`-shaped recursion
  and a 2-function mutual-call produce identical output under `--jit`,
  default, and `--no-jit`.
- Add a counter assertion: with direct calls enabled, `jit_op_call_hit`
  FFI count (already tracked in `engine/counters.rs`, `OpCallHit`) drops to
  ~0 on the recursive benchmark.
- Re-run `scripts/bench.py bench_fib bench_arith` and record the new ratios
  in the JIT doc's benchmark table.

---

## Phase 4 — Mid-function deoptimization (the strategic bet)

> **Largest effort; gateway to LuaJIT-class optimization.** This is a
> design sketch, not turnkey code — it is included so the architecture can
> be approved before any of the speculative-optimization work
> (narrowing / LICM / allocation sinking) is attempted, since all of it
> depends on this primitive.

### Why

OxigenLang can only bail at function entry, so every uncertain op needs a
full generic fallback path *inside* compiled code. LuaJIT can place a guard
anywhere and, on failure, restore interpreter state from a lightweight
snapshot and resume (`lj_snap.c`, `lj_trace_exit`). Cheap deopt is what
lets it *speculate* (assume int, assume callee, assume no overflow) and
delete the fallback paths entirely.

### Primitive to add: snapshot + exit

At selected guard points the JIT records a **snapshot**: enough state to
rebuild an interpreter `CallFrame` and resume at a specific bytecode IP.
On guard failure, control jumps to a deopt stub that:
1. materializes interpreter stack slots from the snapshot (SSA values that
   live in registers/spill slots → `Value`s on `VM.stack`);
2. constructs/repairs the `CallFrame` for this function at the right IP;
3. sets `jit_executing = false` and resumes `execute_until` at that IP.

### What already exists to build on

- `slot_types.rs` already does the abstract interpretation (per-slot types,
  init sites, captured slots) that a snapshot needs to know which slots are
  live and what type each holds.
- `JitFrame` / `JitFrameView` / `StackView` (`vm/mod.rs`) give the exact
  `#[repr(C)]` layout the deopt stub writes into.
- `scan.rs` already computes `touches_heap_values` / `may_capture_upvalues`,
  which bound what a snapshot must reconstruct.
- The `exit_block` plumbing in `defs.rs` (every return funnels through one
  block with `(status, payload)`) is the natural place to add a third
  outcome: `status = 3 (deopt)` carrying a snapshot id.

### Sketch of the data structures

```rust
/// One snapshot per guard that may deopt. Built at emit time.
struct Snapshot {
    bytecode_ip: u32,             // where the interpreter resumes
    stack_depth: u16,             // logical stack height at this point
    // For each live slot: where its current value lives and its type, so
    // the stub can write a correct `Value` back onto VM.stack.
    slots: Vec<SnapSlot>,
}
struct SnapSlot {
    target: u16,                  // VM stack index to write
    src: SnapSrc,                 // SSA reg/spill/const at this guard
    ty: SlotType,                 // from slot_types abstract interp
}
enum SnapSrc { Reg(/* cranelift value */), Const(Value), StackSlot(u16) }
```

The deopt stub is emitted once per function; the guard passes a snapshot
index. On the int-speculation path, instead of emitting a generic
arithmetic fallback, the guard becomes: `is_int(operand)` → continue,
else → `deopt(snapshot_i)`.

### Staged rollout (each independently testable)

1. **Infrastructure only:** add the snapshot table, the deopt stub, and a
   single *artificial* always-true guard that deopts; assert identical
   output and that the interpreter resumes correctly. No speculation yet.
2. **First real speculation:** replace one generic fallback (int `Add`
   overflow / non-int operand) with guard-or-deopt; measure code-size and
   speed on `bench_arith`.
3. **Expand opcode coverage:** opcodes currently on the `scan.rs` reject
   list become emittable *speculatively* (guard the assumption, deopt on
   miss), shrinking the interpreter-only surface.
4. **Then, and only then:** the LuaJIT optimizer passes become applicable —
   integer narrowing (`lj_opt_narrow`), loop-invariant hoisting across the
   now-removable guards (`lj_opt_loop`), and allocation sinking
   (`lj_opt_sink`) for short-lived boxed values. Each is its own follow-up
   plan; none are in scope until step 1–3 land.

### Tests

- A dedicated `core/tests/jit_deopt.rs` that forces each guard to fail and
  asserts the interpreter resumes with bit-identical state/output vs
  `--no-jit`.
- Fuzz: run every `example/*.oxi` with an env knob that makes every
  speculative guard deopt on its first hit, proving the deopt path is
  exercised and correct everywhere.

---

## Suggested execution order

1. **Phase 1** (hash Map/Set) — standalone, high-confidence, fixes a real
   complexity defect. Land first.
2. **Phase 2, `Rc::ptr_eq` equality fast path only** — one trivial, always-
   safe line; defer the compiler-side interning until a map/field benchmark
   justifies it.
3. **Phase 3** (JIT→JIT direct calls + self-recursion) — attacks the
   measured `fib`/`arith` losses; builds on existing specialized entries.
   Requires the targeted emitter read noted in the phase.
4. **Phase 4** (deopt) — schedule as the strategic investment once 1–3 are
   in; it is the prerequisite for every further LuaJIT-class optimization.

## Validation contract (all phases)

Per `docs/JIT_ARCHITECTURE.md`, every change must keep the full suite green
in **all three** modes and produce bit-identical program output:
```bash
cargo test                      # default tiering
cargo test --features jit       # JIT on
# plus --no-jit runs of example/*.oxi
```
Benchmarks before/after with `python3 scripts/bench.py --python`, updating
the benchmark table in `docs/JIT_ARCHITECTURE.md` for any phase that moves
a number.
