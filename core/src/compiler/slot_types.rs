//! B2.0 of the optimization roadmap: typed slot analysis.
//!
//! For each JIT-compilable function this pass produces a
//! `FunctionSlotTypes` map that classifies each local slot as one of:
//!
//!   - `Int64` — provably always an `i64` in this function
//!   - `Value` — generic fallback (anything else, or couldn't prove int)
//!
//! The pass is forward abstract interpretation on the bytecode with a
//! 3-element lattice (`⊥`, `Int64`, `Value`). It is deliberately
//! conservative: when in doubt, a slot is `Value`. Downstream codegen
//! (B2.1 unboxed locals, B2.2 unboxed calling convention) consumes the
//! output and emits native `i64` SSA for the `Int64` slots while
//! leaving the `Value` slots on the generic stack path.
//!
//! Scope for v1:
//!   - Only local slots are classified. Stack positions are tracked
//!     *internally* during the walk (required to determine `SetLocal`
//!     types) but not surfaced in the output.
//!   - Booleans, floats, and other non-`Int64` scalars collapse to
//!     `Value`. Broadening the lattice to track `Bool`, `Float`, etc.
//!     is deferred until B2.1 has shown the basic shape pays off.
//!   - Control flow is handled via a worklist: at each control-flow
//!     merge the incoming lattices are joined; if the merged state
//!     changes the merged block's successors are re-queued. This
//!     converges because the lattice has no infinite chain.
//!
//! See `docs/optimization-roadmap.md` section "Phase B (revised)" for
//! the broader picture.

use std::collections::{HashMap, HashSet};

use crate::compiler::opcode::{Chunk, OpCode};
use crate::vm::value::{Function, Value};

// ── Lattice ───────────────────────────────────────────────────────────

/// Abstract type for a slot or stack position.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum SlotType {
    /// Unreachable / uninitialized. Joins with anything to that other
    /// thing (⊥ ⊔ X = X).
    Bottom,
    /// Provably an `i64` payload at this point in the function.
    Int64,
    /// Any runtime value (fallback). Joins with anything to `Value`.
    Value,
}

impl SlotType {
    #[inline]
    pub fn join(self, other: SlotType) -> SlotType {
        match (self, other) {
            (SlotType::Bottom, x) | (x, SlotType::Bottom) => x,
            (SlotType::Int64, SlotType::Int64) => SlotType::Int64,
            _ => SlotType::Value,
        }
    }
}

// ── Specialized-entry eligibility outcome ─────────────────────────────

/// Per-function classification of why specialized-entry eligibility
/// passed or failed. Step 0 attribution: each outcome maps to a
/// JitCounter so we can attribute "bench_closure has 0 spec dispatches"
/// to the specific rule that excluded the callee.
///
/// Order follows the rejection cascade in
/// `compute_specialized_entry_eligibility` — each variant corresponds
/// to the first rule that rejected the function. A function reaching
/// the end of the cascade is `Eligible`.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SpecEligibilityOutcome {
    Eligible,
    /// Function takes no args; specialized entry's reason for existing
    /// (passing args in registers) doesn't apply.
    RejectedZeroArity,
    /// At least one param is neither Int64-typed nor an int-mirror
    /// candidate. Cannot pass the arg as raw i64 safely.
    RejectedParamNotInt,
    /// At least one param is captured by an upvalue. The specialized
    /// entry would need to materialize a heap slot for the capture,
    /// which the current ABI doesn't do.
    RejectedParamCaptured,
    /// Body contains a `Closure` opcode (creates a new closure).
    /// Heap effect; defer to a later step that handles allocation.
    RejectedHasClosureOp,
    /// Body contains `GetUpvalue` / `SetUpvalue` / `CloseUpvalue`.
    /// Step 4a will lift this restriction by adding a closure pointer
    /// to the specialized ABI; until then these are unsafe to run
    /// without the surrounding generic frame.
    RejectedHasUpvalueOp,
    /// Function has no `Return` opcode (e.g., always errors out or
    /// loops forever). Specialized return slot would never fire.
    RejectedNoReturn,
    /// Function has no `Call` opcode. Without a call there's no
    /// self-recursion, and the IC never observes this callee — pure
    /// compile-time overhead to emit a specialized body.
    RejectedNoCall,
    /// At least one Return IP is unreachable per the abstract
    /// interpretation (Bottom on top-of-stack at that IP).
    RejectedReturnUnreachable,
}

impl SpecEligibilityOutcome {
    #[inline]
    pub fn is_eligible(self) -> bool {
        matches!(self, SpecEligibilityOutcome::Eligible)
    }
}

// ── Per-function result ───────────────────────────────────────────────

/// Per-slot classification for one JIT-compiled function, plus the
/// bytecode metadata that B2.1 codegen needs.
///
/// Indexed by local slot number. A slot that was never assigned stays
/// `Bottom` (callers can treat this as `Value` — it means the slot is
/// unused).
///
/// The additional maps beyond `slots` are the product of the same
/// abstract-interpretation walk and are consumed by B2.1+ codegen
/// (`core/src/jit/engine.rs`) to decide, per IP, whether a `Constant`
/// is initializing a virtualizable local, whether a `Pop` is an
/// expression cleanup / condition cleanup / scope teardown, and
/// which slots are off-limits because they're captured by a nested
/// closure.
#[derive(Debug, Clone)]
pub struct FunctionSlotTypes {
    /// Per-slot classification. `slots[N]` is the join of every type
    /// ever written to slot N during the function.
    pub slots: Vec<SlotType>,

    /// IP → slot. "The opcode at this IP first initializes this local
    /// slot." For v1 B2.1 only recognizes Constant(Integer) initializers
    /// for virtualization; the map itself is general (the initializer
    /// may be any expression-producing opcode). Param slots are NOT in
    /// this map — they're initialized at function entry, before any
    /// bytecode runs. See `init_sites` for the unified "is slot
    /// initialized somewhere?" question.
    pub local_init_result_ip: HashMap<usize, u16>,

    /// All slots that are known to be initialized somewhere — either at
    /// function entry (params, slots 1..=arity) or at a specific
    /// bytecode IP (values of `local_init_result_ip`). B2.1's
    /// virtualizability test checks this; consumers should not need to
    /// special-case param vs. local.
    pub init_sites: HashSet<u16>,

    /// IPs of `Pop` opcodes that exist solely to remove a conditional
    /// branch's condition value from the stack. These are the `Pop`s
    /// the bytecode compiler emits immediately after a JumpIfFalse /
    /// JumpIfTrue / Unless fallthrough, AND at the branch target. B2.1e
    /// suppresses these when a virtual Bool was consumed at the branch.
    pub condition_cleanup_pop_ips: HashSet<usize>,

    /// IP → slot. "The opcode at this IP pops a local slot at end-of-
    /// scope." B2.1b+ uses this to remove the slot from the live set
    /// so subsequent `flush_all` calls don't store into a dead slot.
    pub scope_pop_slot_ip: HashMap<usize, u16>,

    /// Slots referenced as `is_local = 1` in any nested `Closure`
    /// opcode's upvalue descriptors (i.e., captured by a closure
    /// defined inside this function). Virtualizing a captured slot
    /// would make the capture see a stale backing slot whenever the
    /// variable lives in a Cranelift `Variable`. B2.1 never
    /// virtualizes captured slots.
    pub captured_slots: HashSet<u16>,

    /// Param slots eligible for an Int64 register mirror (B2.2a).
    /// Selection criteria:
    ///   - In the param range (1..=arity).
    ///   - Classified `Value` — already-`Int64` params don't need a
    ///     mirror since B2.1 virtualizes them directly.
    ///   - Never written (no `SetLocal` targeting this slot) — a
    ///     read-only slot's mirror never goes stale, which lets us
    ///     skip a per-write resync.
    ///   - Not captured — same reason as other virtualization paths.
    ///   - Used by at least one `GetLocal` in the function body —
    ///     otherwise the mirror is pure overhead.
    ///
    /// Engine emits a one-shot tag guard at function entry: if the
    /// param's runtime tag is `Integer`, extract the payload and
    /// def_var the mirror; if not, bail the thunk out and let the
    /// interpreter handle this invocation.
    pub int_mirror_param_slots: HashSet<u16>,

    /// True iff the function qualifies for a second, specialized
    /// calling-convention entry point `fn(*mut VM, i64, ..., i64) -> (i64, u32)`
    /// with args passed in registers and result returned as raw i64.
    /// See `collect_specialized_entry_eligibility` for the full rule set.
    /// Not written here — consumers check `specialized_param_slots`
    /// and the `Return` IPs separately if they need per-IP info.
    pub specialized_entry_eligible: bool,

    /// Step 0 attribution: per-rejection reason for `specialized_entry_eligible`.
    /// `Eligible` when the function qualifies; otherwise the first rule
    /// that rejected it. Consumed by the JIT to bump per-reason counters
    /// so we can attribute "why didn't this benchmark take the spec
    /// dispatch path" without re-running the analysis.
    pub specialized_entry_outcome: SpecEligibilityOutcome,

    /// Param slots that the specialized entry would receive as i64
    /// (1..=arity when eligible; empty otherwise). Distinct from
    /// `int_mirror_param_slots`: specialized slots include BOTH the
    /// read-only Int-demand Value-typed params AND the Int64-typed
    /// params (whether writable or not). Used by the codegen stage to
    /// know the specialized signature arity and which slots to
    /// materialize from register args.
    pub specialized_param_slots: Vec<u16>,
}

impl FunctionSlotTypes {
    /// Return the inferred type for slot `i`, or `Value` if the index
    /// is out of range (defensive default).
    pub fn get(&self, i: usize) -> SlotType {
        self.slots.get(i).copied().unwrap_or(SlotType::Value)
    }

    /// Count of slots classified as `Int64`. Useful for tests and
    /// quick diagnostics.
    pub fn int64_count(&self) -> usize {
        self.slots
            .iter()
            .filter(|s| matches!(**s, SlotType::Int64))
            .count()
    }

    /// True iff slot `i` is initialized somewhere — at function entry
    /// (param) or at a specific bytecode IP (local with recognized
    /// initializer). Equivalent to `init_sites.contains(slot)`.
    pub fn has_known_initializer(&self, slot: u16) -> bool {
        self.init_sites.contains(&slot)
    }

    /// Eligibility rule for B2.1 virtualization. See plan §
    /// "Virtualization eligibility".
    ///
    /// This helper is the single source of truth for the codegen pass;
    /// every Variable-emitting code path should gate on this.
    pub fn is_virtualizable(&self, slot: u16) -> bool {
        self.get(slot as usize) == SlotType::Int64
            && !self.captured_slots.contains(&slot)
            && self.has_known_initializer(slot)
    }

    /// Return Some(slot) if the Pop at `ip` is recognized as a
    /// scope-ending teardown for a local slot, else None.
    pub fn scope_pop_for(&self, ip: usize) -> Option<u16> {
        self.scope_pop_slot_ip.get(&ip).copied()
    }

    /// True iff the Pop at `ip` is recognized as a conditional-branch
    /// cleanup (virtual branches suppress these when firing).
    pub fn is_condition_cleanup_pop(&self, ip: usize) -> bool {
        self.condition_cleanup_pop_ips.contains(&ip)
    }
}

// ── Abstract state during the walk ────────────────────────────────────

/// The analysis state at a program point. Oxigen's locals are just
/// positions on the VM stack: slot N is `stack[N]`. There's no
/// separate "locals" array — a `Constant` opcode that initializes a
/// variable simply pushes at the slot's position. Temporaries (loop
/// conditions, arithmetic intermediates) live at positions ABOVE
/// `num_slots`.
///
/// We track two things:
///   - `stack`: the per-position abstract type at this IP (used during
///     transfer to compute operand types).
///   - `slot_types`: the slot's classification, updated only on WRITES
///     that target a local position (initialization pushes, explicit
///     `SetLocal`, `Increment`/`Decrement`). Not touched by transients.
///
/// Two separate fields avoids the "temporary at stack[slot] poisons
/// the slot's type" trap: comparisons and intermediate values live on
/// the stack but never flow into `slot_types`.
#[derive(Clone, Debug, PartialEq, Eq)]
struct AbstractState {
    stack: Vec<SlotType>,
    slot_types: Vec<SlotType>,
}

impl AbstractState {
    fn new(num_slots: usize) -> Self {
        AbstractState {
            stack: Vec::new(),
            slot_types: vec![SlotType::Bottom; num_slots],
        }
    }

    /// Pointwise join of both stack AND slot_types. Pads shorter
    /// sides with `Bottom` (the lattice identity).
    fn join(&mut self, other: &AbstractState) -> bool {
        let mut changed = false;

        let stack_len = self.stack.len().max(other.stack.len());
        if self.stack.len() < stack_len {
            self.stack.resize(stack_len, SlotType::Bottom);
            changed = true;
        }
        for i in 0..stack_len {
            let a = self.stack.get(i).copied().unwrap_or(SlotType::Bottom);
            let b = other.stack.get(i).copied().unwrap_or(SlotType::Bottom);
            let joined = a.join(b);
            if joined != self.stack[i] {
                self.stack[i] = joined;
                changed = true;
            }
        }

        let slot_len = self.slot_types.len().max(other.slot_types.len());
        if self.slot_types.len() < slot_len {
            self.slot_types.resize(slot_len, SlotType::Bottom);
            changed = true;
        }
        for i in 0..slot_len {
            let a = self.slot_types.get(i).copied().unwrap_or(SlotType::Bottom);
            let b = other.slot_types.get(i).copied().unwrap_or(SlotType::Bottom);
            let joined = a.join(b);
            if joined != self.slot_types[i] {
                self.slot_types[i] = joined;
                changed = true;
            }
        }

        changed
    }

    /// Record a write to local slot `idx` with type `ty`. Writes from
    /// outside the local range (transients) are ignored.
    fn write_slot(&mut self, idx: usize, ty: SlotType) {
        if idx < self.slot_types.len() {
            self.slot_types[idx] = self.slot_types[idx].join(ty);
        }
    }
}

// ── Entry point ───────────────────────────────────────────────────────

/// Run the analysis on one function and return its slot classifications.
///
/// `num_slots` is max(locals_len, arity + 1). Param slots (1..=arity)
/// are typed from `params[*].type_ann`. Slot 0 is the closure marker.
/// Additional slots beyond params are populated by the body's
/// stack-push initializers (`Constant`, arithmetic, `GetLocal`) —
/// this is Oxigen's calling convention: locals live at stack
/// positions equal to their slot index.
///
/// For each stack position we track the abstract type across all
/// reachable program points and report the join as the slot's
/// classification. This works whether the local is initialized via
/// `Constant+(implicit)SetLocal` (the common path — Oxigen doesn't
/// emit an explicit `SetLocal` at initialization) or via explicit
/// `SetLocal` on reassignment.
pub fn analyze(func: &Function) -> FunctionSlotTypes {
    let num_slots = func.locals.len().max(func.arity as usize + 1);
    let chunk = &func.chunk;
    let code = &chunk.code;

    // Entry state: stack holds the closure marker at position 0 and
    // param types at positions 1..=arity. These are the first slot
    // WRITES — we record them in slot_types too.
    let mut entry = AbstractState::new(num_slots);
    entry.stack.push(SlotType::Value);
    entry.write_slot(0, SlotType::Value);
    for i in 0..func.arity as usize {
        let ty = match func.params.get(i).and_then(|p| p.type_ann.as_deref()) {
            Some("int") | Some("INTEGER") => SlotType::Int64,
            _ => SlotType::Value,
        };
        entry.stack.push(ty);
        entry.write_slot(i + 1, ty);
    }

    // Metadata populated during the walk (B2.1a). `first_init_ip`
    // records the FIRST IP at which each slot is initialized — we
    // only want the initializer, not subsequent re-writes.
    let mut first_init_ip: HashMap<u16, usize> = HashMap::new();
    let mut scope_pop_slot_ip: HashMap<usize, u16> = HashMap::new();

    // IP → state-on-entry. We maintain a worklist of IPs whose
    // on-entry state has changed and whose post-state needs
    // recomputing.
    let mut states: HashMap<usize, AbstractState> = HashMap::new();
    states.insert(0, entry.clone());
    let mut worklist: Vec<usize> = vec![0];

    while let Some(ip) = worklist.pop() {
        // Linearly walk forward from `ip` applying transfer functions,
        // until we hit a terminating op or a point whose on-entry
        // state is already recorded (= a known join point or loop
        // head — let the merge/worklist carry the recomputation).
        let mut state = states[&ip].clone();
        let mut cursor = ip;

        loop {
            if cursor >= code.len() {
                break;
            }
            let op = match OpCode::from_byte(code[cursor]) {
                Some(o) => o,
                None => break,
            };
            let depth_before = state.stack.len();
            let (mut next_state, terminates, targets) = transfer(op, cursor, code, chunk, &state);
            let fallthrough = cursor + opcode_len(op, code, cursor, chunk);

            // Initialization-push detection: if the stack grew by one
            // and the new top lands at a position within the local
            // range, that push is initializing that local slot. This
            // is how Oxigen initializes `total := 0` — no SetLocal
            // emitted, just a Constant at the slot's stack position.
            //
            // IMPORTANT: transient expression pushes (e.g., a GetLocal
            // in an outer loop condition that lands at the stack
            // position of a not-yet-declared inner-scope local) must
            // not be misclassified as slot inits. In v1 B2.1 only
            // virtualizes slots with Constant initializers, so gate
            // init detection on `op == Constant`. Non-Constant
            // initializers (`x := y + 1`, `x := foo()`, etc.) will
            // simply not be virtualized — safe and consistent with
            // the v1 eligibility rule.
            let depth_after = next_state.stack.len();
            if matches!(op, OpCode::Constant) && depth_after == depth_before + 1 {
                let new_pos = depth_after - 1;
                if new_pos < num_slots {
                    let ty = next_state
                        .stack
                        .last()
                        .copied()
                        .unwrap_or(SlotType::Bottom);
                    next_state.write_slot(new_pos, ty);
                    // B2.1a metadata: record the earliest IP at which
                    // this slot appears to be initialized. Ordering by
                    // IP value isn't dominator-correct in general, but
                    // for Oxigen's locals (declared/initialized before
                    // use on every reaching path) it matches the
                    // declaration site. The certifier below verifies
                    // use-after-init.
                    let slot = new_pos as u16;
                    let prev = first_init_ip.get(&slot).copied();
                    if prev.map_or(true, |p| cursor < p) {
                        first_init_ip.insert(slot, cursor);
                    }
                }
            }

            // Step 0 bugfix: any op that LEAVES a value at a position
            // within the local range — not just Constant — should join
            // its pushed type into slot_types[pos]. Without this, the
            // slot_types only sees the FIRST push at that position
            // (recorded by the Constant block above), and subsequent
            // ops that consume those constants and push a different
            // type at the same position (`arr := [10, 20, 30]` pops
            // three Int64 transients via BuildArray and pushes an
            // Array at the slot's position) leave slot_types[N] stuck
            // at the transient's Int64. The JIT then virtualizes the
            // slot as Int64 and reads the cached Constant integer,
            // skipping the actual Array — `arr[1]` becomes `10[1]`,
            // and the Index helper errors with "cannot index INTEGER
            // with INTEGER". Joining on every push converges
            // `slot_types[N]` to the lattice top (Value) whenever any
            // non-Int64 type ever lands at that position, correctly
            // disqualifying the slot from virtualization.
            //
            // Init-IP / `init_sites` are NOT extended here: those
            // gate the *virtualization* fast path that synthesises
            // values from a Constant initializer, and that mechanism
            // only handles Constant initializers today (per the
            // comment block above). Joining slot_types is sufficient
            // because is_virtualizable also checks `slot is Int64`.
            if depth_after >= 1 && !matches!(op, OpCode::Constant) {
                let new_pos = depth_after - 1;
                if new_pos < num_slots {
                    // Bugfix: only join the pushed type into
                    // `slot_types[new_pos]` when that slot has
                    // ALREADY been initialized on this path. A
                    // push that lands at `position N` before slot
                    // N is declared is a transient — e.g., the
                    // inner-loop comparison `j <= n` in
                    // `nested_sum` lands a Bool at stack position
                    // 4 (the eventual `j` slot) BEFORE `j := 1`
                    // initializes it at a later IP. Joining the
                    // transient type would poison `slot_types[4]`
                    // to `Value`, defeating Int64 virtualization.
                    //
                    // Once `j` is initialized (state.slot_types[4]
                    // != Bottom), any subsequent write *at slot
                    // 4's position* is a real reassignment and
                    // should be joined — that's how the
                    // `arr := [10, 20, 30]` case (BuildArray
                    // overwriting the transient Constants at the
                    // slot's position) gets the correct type
                    // join. The pre-init Bottom check leaves
                    // that case untouched.
                    let already_init = state
                        .slot_types
                        .get(new_pos)
                        .copied()
                        .unwrap_or(SlotType::Bottom)
                        != SlotType::Bottom;
                    if already_init {
                        let ty = next_state
                            .stack
                            .last()
                            .copied()
                            .unwrap_or(SlotType::Bottom);
                        next_state.write_slot(new_pos, ty);
                    }
                }
            }

            // Scope-pop detection: a `Pop` whose depth_after lands on
            // a local slot's position (i.e., the thing being popped
            // occupied slot N). Only record when that slot has been
            // initialized — otherwise we'd "destroy" a not-yet-live
            // slot. Per plan: default is still to record and let the
            // downstream handler do a real pop; missing a scope pop
            // is safer than claiming a spurious one.
            if matches!(op, OpCode::Pop) {
                let popped_pos = depth_before - 1;
                if popped_pos < num_slots {
                    // Consult the CURRENT state (before this Pop) to
                    // see if the slot was live.
                    let was_init = state
                        .slot_types
                        .get(popped_pos)
                        .copied()
                        .unwrap_or(SlotType::Bottom)
                        != SlotType::Bottom;
                    if was_init {
                        scope_pop_slot_ip.insert(cursor, popped_pos as u16);
                    }
                }
            }

            // Explicit non-fall-through branches (jumps, conditional
            // targets, etc.). Merge our post-op state into each target
            // and enqueue it if the join changed anything.
            for tgt in &targets {
                let entry = states
                    .entry(*tgt)
                    .or_insert_with(|| AbstractState::new(num_slots));
                let changed = entry.join(&next_state);
                if changed && !worklist.contains(tgt) {
                    worklist.push(*tgt);
                }
            }

            if terminates {
                break;
            }
            if fallthrough >= code.len() {
                break;
            }

            // Is the fall-through IP already a known join point (e.g.
            // a loop back-edge target we've been here before)? If so,
            // merge into its recorded state and break — the worklist
            // will re-process if our contribution changed the merge.
            if let Some(existing) = states.get_mut(&fallthrough) {
                let changed = existing.join(&next_state);
                if changed && !worklist.contains(&fallthrough) {
                    worklist.push(fallthrough);
                }
                break;
            }

            // Fresh IP — record it (so future joiners can find us) and
            // continue linearly.
            states.insert(fallthrough, next_state.clone());
            state = next_state;
            cursor = fallthrough;
        }
    }

    // Project `slot_types` (which tracks only WRITES into local
    // positions — initialization pushes, explicit SetLocal, and
    // Increment/Decrement) across all states. We deliberately do NOT
    // look at `stack` here — a stack position within the local range
    // can hold a transient value (an arithmetic intermediate at a
    // position that happens to coincide with a not-yet-live local's
    // slot). `slot_types` only records real writes, so joining it
    // across states gives the true "what's the most general type slot
    // N ever holds?" answer.
    let mut result = vec![SlotType::Bottom; num_slots];
    for state in states.values() {
        for i in 0..num_slots {
            let ty = state
                .slot_types
                .get(i)
                .copied()
                .unwrap_or(SlotType::Bottom);
            result[i] = result[i].join(ty);
        }
    }

    // local_init_result_ip: invert first_init_ip (slot → IP) into
    // (IP → slot). B2.1 codegen wants to dispatch by IP.
    let local_init_result_ip: HashMap<usize, u16> = first_init_ip
        .into_iter()
        .map(|(slot, ip)| (ip, slot))
        .collect();

    // init_sites: union of param slots (1..=arity — initialized at
    // function entry, not at any bytecode IP) and all slots that
    // appear in local_init_result_ip.
    let mut init_sites: HashSet<u16> = HashSet::new();
    for i in 1..=func.arity as u16 {
        init_sites.insert(i);
    }
    for &slot in local_init_result_ip.values() {
        init_sites.insert(slot);
    }

    // Post-pass 1: condition_cleanup_pop_ips. For every conditional
    // branch, check whether both the fall-through IP and the branch
    // target IP carry a `Pop`. When both match, they're the condition-
    // cleanup pair B2.1e wants to suppress.
    let condition_cleanup_pop_ips = collect_condition_cleanup_pops(code, chunk);

    // Post-pass 2: captured_slots. Walk every Closure opcode and parse
    // its upvalue descriptors; any descriptor with `is_local = 1`
    // captures a slot of this function by reference.
    let captured_slots = collect_captured_slots(code, chunk);

    // Post-pass 3 (B2.2a): param mirror eligibility. Scan the bytecode
    // once to find SetLocal-written slots and GetLocal-read slots,
    // then cross-check against the param range + captured_slots.
    let int_mirror_param_slots =
        collect_int_mirror_param_slots(code, chunk, func.arity as u16, &captured_slots, &result);

    // Post-pass 4 (A1): specialized-entry eligibility.
    //
    // Subtlety: the primary `states` map was built with untyped params
    // classified as Value. A function like fib(n) that returns `n` in
    // its base case would then have Value on top of the stack at the
    // Return IP, incorrectly disqualifying it.
    //
    // Fix: re-run the abstract interpretation with any param in
    // `int_mirror_param_slots` treated as Int64 (the B2.2a mirror
    // makes that true at runtime). The eligibility check uses this
    // LIFTED state map, not the primary one.
    let lifted_states = if int_mirror_param_slots.is_empty() {
        None
    } else {
        Some(analyze_states_with_lifted_params(
            func,
            chunk,
            &int_mirror_param_slots,
        ))
    };
    let eligibility_states = lifted_states.as_ref().unwrap_or(&states);
    let (specialized_entry_outcome, specialized_param_slots) = compute_specialized_entry_eligibility(
        code,
        chunk,
        func.arity as u16,
        &result,
        &captured_slots,
        &int_mirror_param_slots,
        eligibility_states,
    );
    let specialized_entry_eligible = specialized_entry_outcome.is_eligible();

    FunctionSlotTypes {
        slots: result,
        local_init_result_ip,
        init_sites,
        condition_cleanup_pop_ips,
        scope_pop_slot_ip,
        captured_slots,
        int_mirror_param_slots,
        specialized_entry_eligible,
        specialized_entry_outcome,
        specialized_param_slots,
    }
}

/// Second-pass abstract interpretation with a subset of param slots
/// lifted from `Value` to `Int64` in the initial entry state. Used only
/// by A1 eligibility — we don't replace the primary `states` with this
/// because the rest of the JIT (B2.1, B2.2a) depends on the unlifted
/// classification.
fn analyze_states_with_lifted_params(
    func: &Function,
    chunk: &Chunk,
    lift_to_int: &HashSet<u16>,
) -> HashMap<usize, AbstractState> {
    let num_slots = func.locals.len().max(func.arity as usize + 1);
    let code = &chunk.code;

    let mut entry = AbstractState::new(num_slots);
    entry.stack.push(SlotType::Value);
    entry.write_slot(0, SlotType::Value);
    for i in 0..func.arity as usize {
        let slot = (i + 1) as u16;
        let ty = if lift_to_int.contains(&slot) {
            SlotType::Int64
        } else {
            match func.params.get(i).and_then(|p| p.type_ann.as_deref()) {
                Some("int") | Some("INTEGER") => SlotType::Int64,
                _ => SlotType::Value,
            }
        };
        entry.stack.push(ty);
        entry.write_slot(i + 1, ty);
    }

    let mut states: HashMap<usize, AbstractState> = HashMap::new();
    states.insert(0, entry.clone());
    let mut worklist: Vec<usize> = vec![0];

    while let Some(ip) = worklist.pop() {
        let mut state = states[&ip].clone();
        let mut cursor = ip;

        loop {
            if cursor >= code.len() {
                break;
            }
            let op = match OpCode::from_byte(code[cursor]) {
                Some(o) => o,
                None => break,
            };
            let (next_state, terminates, targets) = transfer(op, cursor, code, chunk, &state);
            let fallthrough = cursor + opcode_len(op, code, cursor, chunk);

            for tgt in &targets {
                let entry = states
                    .entry(*tgt)
                    .or_insert_with(|| AbstractState::new(num_slots));
                let changed = entry.join(&next_state);
                if changed && !worklist.contains(tgt) {
                    worklist.push(*tgt);
                }
            }

            if terminates {
                break;
            }
            if fallthrough >= code.len() {
                break;
            }

            if let Some(existing) = states.get_mut(&fallthrough) {
                let changed = existing.join(&next_state);
                if changed && !worklist.contains(&fallthrough) {
                    worklist.push(fallthrough);
                }
                break;
            }

            states.insert(fallthrough, next_state.clone());
            state = next_state;
            cursor = fallthrough;
        }
    }

    states
}

/// Determine whether the function qualifies for a specialized i64-ABI
/// entry point. Returns `(outcome, param_slots)` — the second value is
/// empty when ineligible, or the full `(1..=arity)` list otherwise.
/// `outcome` carries the specific rule that rejected the function so
/// the JIT can attribute "why didn't this take the spec dispatch path"
/// without re-running the analysis.
fn compute_specialized_entry_eligibility(
    code: &[u8],
    chunk: &Chunk,
    arity: u16,
    slot_types: &[SlotType],
    captured_slots: &HashSet<u16>,
    int_mirror_param_slots: &HashSet<u16>,
    states: &HashMap<usize, AbstractState>,
) -> (SpecEligibilityOutcome, Vec<u16>) {
    if arity == 0 {
        return (SpecEligibilityOutcome::RejectedZeroArity, Vec::new());
    }

    // Every param slot must be Int-stable AND not captured.
    for slot in 1..=arity {
        let is_int_typed = matches!(
            slot_types.get(slot as usize).copied().unwrap_or(SlotType::Value),
            SlotType::Int64
        );
        let is_int_demand_mirror = int_mirror_param_slots.contains(&slot);
        if !(is_int_typed || is_int_demand_mirror) {
            return (SpecEligibilityOutcome::RejectedParamNotInt, Vec::new());
        }
        if captured_slots.contains(&slot) {
            return (SpecEligibilityOutcome::RejectedParamCaptured, Vec::new());
        }
    }

    // The body must not contain opcodes we don't yet know how to
    // handle in the specialized entry: closures, upvalues, nonlocal
    // control flow that might invalidate our frame assumptions.
    //
    // Also require at least one `Call` opcode: A2.5 emits a
    // specialized body ONLY to be directly-called by A3 (from a
    // self-recursive Call site). A function with no Calls can never
    // self-recurse, so emitting a specialized body is pure compile-
    // time overhead. Benches with tight loops-without-calls (e.g.,
    // bench_nested_loop) would regress from the extra codegen work
    // without this gate.
    let mut has_return = false;
    let mut has_call = false;
    let mut return_ips: Vec<usize> = Vec::new();
    let mut ip = 0;
    while ip < code.len() {
        let op = match OpCode::from_byte(code[ip]) {
            Some(o) => o,
            None => break,
        };
        let op_len = opcode_len(op, code, ip, chunk);

        match op {
            OpCode::Closure => {
                return (SpecEligibilityOutcome::RejectedHasClosureOp, Vec::new());
            }
            OpCode::GetUpvalue | OpCode::SetUpvalue | OpCode::CloseUpvalue => {
                return (SpecEligibilityOutcome::RejectedHasUpvalueOp, Vec::new());
            }
            OpCode::Return => {
                has_return = true;
                return_ips.push(ip);
            }
            OpCode::Call | OpCode::CallNamed => {
                has_call = true;
            }
            _ => {}
        }

        ip += op_len;
    }

    if !has_return {
        return (SpecEligibilityOutcome::RejectedNoReturn, Vec::new());
    }
    if !has_call {
        return (SpecEligibilityOutcome::RejectedNoCall, Vec::new());
    }

    // Every Return IP must have a non-Bottom top (reachable) and must
    // be classifiable as Int64 OR Value — Value is acceptable because
    // the specialized trampoline tag-checks the top-of-stack at
    // runtime and bails gracefully on non-Integer. The ONLY
    // disqualifier at this level is dead/unreachable Return or a
    // statically-proven non-numeric (e.g., Bottom unreachable state).
    for &rip in &return_ips {
        let state = match states.get(&rip) {
            Some(s) => s,
            None => {
                return (SpecEligibilityOutcome::RejectedReturnUnreachable, Vec::new());
            }
        };
        let top_ty = state.stack.last().copied().unwrap_or(SlotType::Bottom);
        // Bottom means unreachable → still reject (can't trust).
        if matches!(top_ty, SlotType::Bottom) {
            return (SpecEligibilityOutcome::RejectedReturnUnreachable, Vec::new());
        }
        // Value and Int64 both ok: the trampoline's runtime tag check
        // takes care of correctness.
    }

    let param_slots: Vec<u16> = (1..=arity).collect();
    (SpecEligibilityOutcome::Eligible, param_slots)
}

/// Walk the bytecode and pick out param slots that qualify for an
/// Int64 register mirror (B2.2a). See the field doc on
/// `FunctionSlotTypes::int_mirror_param_slots` for the full criteria.
fn collect_int_mirror_param_slots(
    code: &[u8],
    chunk: &Chunk,
    arity: u16,
    captured_slots: &HashSet<u16>,
    slot_types: &[SlotType],
) -> HashSet<u16> {
    if arity == 0 {
        return HashSet::new();
    }

    let mut written: HashSet<u16> = HashSet::new();
    // For each param slot: track whether any GetLocal is immediately
    // followed by an int-consumer op (evidence the param is used as
    // Int) vs. a non-int-consumer op (evidence the param is Value-
    // shaped — a struct, array, closure, etc.). A param with a
    // non-int consumer anywhere is not eligible; a tag guard at entry
    // would bail the thunk out at runtime for this perfectly normal
    // calling convention.
    let mut has_int_demand: HashSet<u16> = HashSet::new();
    let mut has_non_int_demand: HashSet<u16> = HashSet::new();

    let mut ip = 0;
    while ip < code.len() {
        let op = match OpCode::from_byte(code[ip]) {
            Some(o) => o,
            None => break,
        };
        let op_len = opcode_len(op, code, ip, chunk);

        match op {
            OpCode::SetLocal => {
                let slot = read_u16(code, ip + 1);
                written.insert(slot);
            }
            OpCode::GetLocal => {
                let slot = read_u16(code, ip + 1);
                // Scan forward from ip + op_len, skipping a BOUNDED
                // number of "push-one-int" opcodes (Constant(Integer)
                // is the common case — `n - 1`, `n < 2`). The param
                // stays at stack[top-1]; when the first real classifier
                // op fires, it either uses the param directly (unary
                // op) or as lhs of a binary (top-1 after one push).
                // We classify based on the classifier op.
                let mut cursor = ip + op_len;
                let mut skipped = 0;
                let classifier_op = loop {
                    if skipped > 4 || cursor >= code.len() {
                        break None;
                    }
                    let cop = OpCode::from_byte(code[cursor]);
                    match cop {
                        // Neutral "push a constant" — the param is
                        // still on the stack, just below the new top.
                        // Skip past it and keep scanning.
                        Some(OpCode::Constant)
                        | Some(OpCode::None)
                        | Some(OpCode::True)
                        | Some(OpCode::False) => {
                            let step = opcode_len(cop.unwrap(), code, cursor, chunk);
                            cursor += step;
                            skipped += 1;
                            continue;
                        }
                        _ => break cop,
                    }
                };
                match classifier_op {
                    Some(
                        OpCode::Add
                        | OpCode::Subtract
                        | OpCode::Multiply
                        | OpCode::Divide
                        | OpCode::Modulo
                        | OpCode::Negate
                        | OpCode::Less
                        | OpCode::LessEqual
                        | OpCode::Greater
                        | OpCode::GreaterEqual,
                    ) => {
                        has_int_demand.insert(slot);
                    }
                    Some(
                        OpCode::GetField
                        | OpCode::SetField
                        | OpCode::MethodCall
                        | OpCode::MethodCallNamed
                        | OpCode::Index
                        | OpCode::IndexAssign
                        | OpCode::BuildArray
                        | OpCode::BuildTuple
                        | OpCode::BuildMap
                        | OpCode::BuildSet
                        | OpCode::StringInterp
                        | OpCode::StructLiteral
                        | OpCode::IterLen
                        | OpCode::IterGet
                        | OpCode::Closure,
                    ) => {
                        has_non_int_demand.insert(slot);
                    }
                    _ => {
                        // Neutral / ambiguous / unknown — no classify.
                    }
                }
            }
            _ => {}
        }

        ip += op_len;
    }

    let mut out = HashSet::new();
    for slot in 1..=arity {
        if written.contains(&slot) {
            continue;
        }
        if !has_int_demand.contains(&slot) {
            // No evidence the param is used as int — the tag guard
            // would be pure overhead.
            continue;
        }
        if has_non_int_demand.contains(&slot) {
            // Evidence the param isn't always int — skip, or the
            // guard would bail out for every Value-shaped invocation
            // and permanently un-JIT the function.
            continue;
        }
        if captured_slots.contains(&slot) {
            continue;
        }
        // Already-Int64 params don't need a mirror — B2.1 covers them.
        if slot_types
            .get(slot as usize)
            .copied()
            .map_or(false, |t| matches!(t, SlotType::Int64))
        {
            continue;
        }
        out.insert(slot);
    }
    out
}

/// Walk the bytecode and find every conditional-branch-cleanup `Pop`
/// pair. A pair consists of:
///   - a `Pop` at IP `fall = branch_ip + branch_len`
///   - a `Pop` at IP `target = branch_ip + branch_len + offset`
/// If either IP isn't a `Pop`, the pair isn't recognized and we record
/// neither (v1: be conservative — B2.1e falls back to the materialized-
/// Bool path on unrecognized shapes).
fn collect_condition_cleanup_pops(code: &[u8], chunk: &Chunk) -> HashSet<usize> {
    let mut out = HashSet::new();
    let mut ip = 0;
    while ip < code.len() {
        let op = match OpCode::from_byte(code[ip]) {
            Some(o) => o,
            None => break,
        };
        let op_len = opcode_len(op, code, ip, chunk);

        match op {
            OpCode::JumpIfFalse | OpCode::JumpIfTrue | OpCode::Unless => {
                // 1-byte opcode + u16 forward offset.
                let off = read_u16(code, ip + 1) as usize;
                let fall_ip = ip + op_len;
                let target_ip = ip + op_len + off;
                let fall_is_pop = code.get(fall_ip).and_then(|&b| OpCode::from_byte(b))
                    == Some(OpCode::Pop);
                let target_is_pop = code.get(target_ip).and_then(|&b| OpCode::from_byte(b))
                    == Some(OpCode::Pop);
                if fall_is_pop && target_is_pop {
                    out.insert(fall_ip);
                    out.insert(target_ip);
                }
            }
            // PopJumpIfFalse already consumes its condition — no
            // cleanup Pop involved.
            _ => {}
        }

        ip += op_len;
    }
    out
}

/// Walk every `Closure` opcode and collect slots captured via
/// `is_local = 1` upvalue descriptors.
///
/// Each `Closure` is laid out as:
///   - 1 byte opcode
///   - u16 constant index (pointing to a `Value::Closure` whose
///     inner `Function.upvalue_count` tells us how many descriptors
///     follow)
///   - N descriptors of (u8 is_local, u16 index) each
///
/// If we can't parse the constant (not a closure, or the function
/// reference is missing), we conservatively return an empty set —
/// that's fine because B2.1's other gates (e.g. slot type == Int64,
/// known initializer) still apply.
fn collect_captured_slots(code: &[u8], chunk: &Chunk) -> HashSet<u16> {
    let mut out = HashSet::new();
    let mut ip = 0;
    while ip < code.len() {
        let op = match OpCode::from_byte(code[ip]) {
            Some(o) => o,
            None => break,
        };

        if matches!(op, OpCode::Closure) {
            if ip + 3 > code.len() {
                break;
            }
            let const_idx = read_u16(code, ip + 1) as usize;
            let upvalue_count = match chunk.constants.get(const_idx) {
                Some(Value::Closure(cl)) => cl.function.upvalue_count as usize,
                _ => {
                    // Malformed — treat as no upvalues and continue.
                    0
                }
            };
            // Parse N descriptors following the opcode + u16.
            let desc_start = ip + 3;
            for d in 0..upvalue_count {
                let base = desc_start + d * 3;
                if base + 3 > code.len() {
                    break;
                }
                let is_local = code[base];
                let index = read_u16(code, base + 1);
                if is_local == 1 {
                    out.insert(index);
                }
            }
            // Advance past the full Closure opcode + descriptors.
            ip = desc_start + upvalue_count * 3;
        } else {
            ip += opcode_len(op, code, ip, chunk);
        }
    }
    out
}

// ── Transfer function ────────────────────────────────────────────────

/// Given an opcode and the state just before it, compute the state just
/// after (for the fall-through) and the list of non-fall-through branch
/// targets. `terminates` = true means no fall-through (e.g. `Return`).
fn transfer(
    op: OpCode,
    ip: usize,
    code: &[u8],
    chunk: &Chunk,
    state: &AbstractState,
) -> (AbstractState, bool, Vec<usize>) {
    let mut next = state.clone();
    let mut terminates = false;
    let mut targets: Vec<usize> = Vec::new();

    match op {
        // Push a constant — type depends on the constant's variant.
        OpCode::Constant => {
            let idx = read_u16(code, ip + 1) as usize;
            let ty = match chunk.constants.get(idx) {
                Some(Value::Integer(_)) => SlotType::Int64,
                _ => SlotType::Value,
            };
            next.stack.push(ty);
        }

        // None / True / False → Value for v1.
        OpCode::None | OpCode::True | OpCode::False => {
            next.stack.push(SlotType::Value);
        }

        OpCode::Pop => {
            // Clear slot_types[N] when Pop destroys position N. This
            // matters for slots whose lifetime is bounded by a scope
            // (e.g., `j := 1` inside an outer-loop body). When the
            // scope-end Pop fires, we want subsequent transient
            // pushes at the same stack position (intermediate values
            // of an enclosing expression) to *not* be classified as
            // slot N's content.
            //
            // Without this clear, after `j` is popped at the inner-
            // loop scope teardown, slot_types[4] stays Int64 forever.
            // On the back-edge re-entry to the outer loop top, the
            // outer condition's `i <= n` lands a Bool transient at
            // stack position 4 — and the "step 0 bugfix" join in
            // `analyze` would then poison slot_types[4] to Value,
            // disqualifying `j` from Int64 virtualization.
            //
            // Clearing on Pop ensures slot_types[N] reflects only the
            // type while the slot is alive on this path. Joins across
            // back-edges still correctly yield the type when alive
            // (Bottom ∨ Int64 = Int64), but the in-state Bottom gates
            // the transient-poisoning join in `analyze`.
            let pos_before = next.stack.len();
            next.stack.pop();
            if pos_before > 0 {
                let popped_pos = pos_before - 1;
                if popped_pos < next.slot_types.len() {
                    next.slot_types[popped_pos] = SlotType::Bottom;
                }
            }
        }
        OpCode::Dup => {
            let top = next.stack.last().copied().unwrap_or(SlotType::Value);
            next.stack.push(top);
        }

        // Binary int arith — Int × Int → Int, else Value.
        OpCode::Add
        | OpCode::Subtract
        | OpCode::Multiply
        | OpCode::Divide
        | OpCode::Modulo
        | OpCode::BitAnd
        | OpCode::BitOr
        | OpCode::BitXor
        | OpCode::ShiftLeft
        | OpCode::ShiftRight => {
            let b = next.stack.pop().unwrap_or(SlotType::Value);
            let a = next.stack.pop().unwrap_or(SlotType::Value);
            let result = if a == SlotType::Int64 && b == SlotType::Int64 {
                SlotType::Int64
            } else {
                SlotType::Value
            };
            next.stack.push(result);
        }

        // Comparison → Bool, which for v1 is Value (we don't track Bool
        // separately yet).
        OpCode::Equal
        | OpCode::NotEqual
        | OpCode::Greater
        | OpCode::GreaterEqual
        | OpCode::Less
        | OpCode::LessEqual => {
            next.stack.pop();
            next.stack.pop();
            next.stack.push(SlotType::Value);
        }

        OpCode::Not => {
            next.stack.pop();
            next.stack.push(SlotType::Value);
        }

        OpCode::BitNot | OpCode::Negate => {
            let a = next.stack.pop().unwrap_or(SlotType::Value);
            let result = if a == SlotType::Int64 {
                SlotType::Int64
            } else {
                SlotType::Value
            };
            next.stack.push(result);
        }

        OpCode::Increment | OpCode::Decrement => {
            let slot = read_u16(code, ip + 1) as usize;
            // Stays Int64 only if the slot already is Int64.
            if slot < next.stack.len() {
                next.stack[slot] = match next.stack[slot] {
                    SlotType::Int64 => SlotType::Int64,
                    _ => SlotType::Value,
                };
            }
            let cur = next
                .stack
                .get(slot)
                .copied()
                .unwrap_or(SlotType::Value);
            next.write_slot(slot, cur);
        }

        OpCode::GetLocal => {
            let slot = read_u16(code, ip + 1) as usize;
            let ty = next.stack.get(slot).copied().unwrap_or(SlotType::Value);
            next.stack.push(ty);
        }
        OpCode::SetLocal => {
            let slot = read_u16(code, ip + 1) as usize;
            // SetLocal PEEKS the top and copies it to stack[slot];
            // the top is NOT popped (see `vm/mod.rs`: it uses
            // `self.peek(0).clone()`). Subsequent `Pop` opcodes remove
            // the value from the top as needed.
            let top = next.stack.last().copied().unwrap_or(SlotType::Value);
            if slot >= next.stack.len() {
                next.stack.resize(slot + 1, SlotType::Bottom);
            }
            next.stack[slot] = next.stack[slot].join(top);
            next.write_slot(slot, top);
        }

        OpCode::GetGlobal | OpCode::GetUpvalue => {
            next.stack.push(SlotType::Value);
        }
        OpCode::SetGlobal | OpCode::DefineGlobal | OpCode::SetUpvalue => {
            next.stack.pop();
        }
        OpCode::DefineGlobalTyped => {
            next.stack.pop();
        }
        OpCode::CloseUpvalue => {}

        // Control flow.
        OpCode::Jump => {
            let off = read_u16(code, ip + 1) as usize;
            targets.push(ip + 3 + off);
            terminates = true;
        }
        OpCode::Loop => {
            let off = read_u16(code, ip + 1) as usize;
            // Backward jump — target is (ip + 3) - off.
            let base = ip + 3;
            if off <= base {
                targets.push(base - off);
            }
            terminates = true;
        }
        OpCode::JumpIfFalse | OpCode::JumpIfTrue => {
            // Peeks top, does not pop — both branches share the same
            // stack.
            let off = read_u16(code, ip + 1) as usize;
            targets.push(ip + 3 + off);
            // Fall-through also possible — caller adds it.
        }
        OpCode::PopJumpIfFalse => {
            let off = read_u16(code, ip + 1) as usize;
            next.stack.pop();
            targets.push(ip + 3 + off);
        }
        OpCode::Unless => {
            let off = read_u16(code, ip + 1) as usize;
            // Unless: if condition (already on stack) is truthy, jump
            // to alternative.
            next.stack.pop();
            targets.push(ip + 3 + off);
        }

        OpCode::Return => {
            next.stack.pop();
            terminates = true;
        }

        // Function calls — pop args + callee, push Value. v1 does not
        // track callee return types.
        OpCode::Call => {
            let argc = code[ip + 1] as usize;
            for _ in 0..=argc {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }
        OpCode::CallNamed => {
            let pos = code[ip + 1] as usize;
            let named = code[ip + 2] as usize;
            // callee + pos args + (name, value) pairs
            for _ in 0..(1 + pos + 2 * named) {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }
        OpCode::MethodCall => {
            let argc = code[ip + 3] as usize;
            for _ in 0..=argc {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }
        OpCode::MethodCallNamed => {
            let pos = code[ip + 3] as usize;
            let named = code[ip + 4] as usize;
            for _ in 0..(1 + pos + 2 * named) {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }

        // Closure: push Value. The operand stream includes upvalue
        // descriptors we need to skip — handled in `opcode_len`.
        OpCode::Closure => {
            next.stack.push(SlotType::Value);
        }

        // Collections / structs / enums — all push `Value`.
        OpCode::BuildArray | OpCode::BuildSet => {
            let count = read_u16(code, ip + 1) as usize;
            for _ in 0..count {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }
        OpCode::BuildTuple => {
            let count = read_u16(code, ip + 1) as usize;
            for _ in 0..count {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }
        OpCode::BuildMap => {
            let pairs = read_u16(code, ip + 1) as usize;
            for _ in 0..(2 * pairs) {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }
        OpCode::Index => {
            next.stack.pop();
            next.stack.pop();
            next.stack.push(SlotType::Value);
        }
        OpCode::IndexAssign => {
            next.stack.pop();
            next.stack.pop();
            next.stack.pop();
        }
        OpCode::Slice => {
            let flags = code[ip + 1];
            let has_start = flags & 1 != 0;
            let has_end = flags & 2 != 0;
            let pops = 1 + has_start as usize + has_end as usize;
            for _ in 0..pops {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }

        OpCode::StructDef | OpCode::EnumDef | OpCode::DefineMethod => {
            // All consume and/or push; for v1 we just push `Value` or
            // leave stack alone. DefineMethod consumes (name, closure)
            // pairs from the stack.
            if matches!(op, OpCode::DefineMethod) {
                let count = code[ip + 3] as usize;
                for _ in 0..(2 * count) {
                    next.stack.pop();
                }
            }
            // StructDef / EnumDef don't consume from the stack here —
            // they register in the globals table.
        }
        OpCode::StructLiteral => {
            let count = read_u16(code, ip + 3) as usize;
            for _ in 0..(2 * count) {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }
        OpCode::GetField | OpCode::GetModuleField => {
            next.stack.pop();
            next.stack.push(SlotType::Value);
        }
        OpCode::SetField => {
            next.stack.pop();
            next.stack.pop();
        }
        OpCode::MakeEnumVariantUnit => {
            next.stack.pop();
            next.stack.push(SlotType::Value);
        }
        OpCode::MakeEnumVariantTuple => {
            let argc = code[ip + 3] as usize;
            for _ in 0..(1 + argc) {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }
        OpCode::MakeEnumVariantStruct => {
            let fieldc = code[ip + 3] as usize;
            for _ in 0..(1 + 2 * fieldc) {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }

        // Error / pattern / misc — conservative.
        OpCode::ErrorConstruct => {
            let has_tag = code[ip + 1] != 0;
            for _ in 0..(1 + has_tag as usize) {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }
        OpCode::ValueConstruct => {
            next.stack.pop();
            next.stack.push(SlotType::Value);
        }
        OpCode::Guard => {
            // Binds top of stack (peek, not pop) and may jump on error.
            let off = read_u16(code, ip + 3) as usize;
            targets.push(ip + 5 + off);
        }
        OpCode::Fail => {
            next.stack.pop();
            terminates = true;
        }

        OpCode::Import => {
            let has_selective = code[ip + 3] != 0;
            // Import doesn't leave anything on the stack for v1.
            let _ = has_selective;
        }

        OpCode::StringInterp => {
            let count = read_u16(code, ip + 1) as usize;
            for _ in 0..count {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }
        OpCode::Log => {
            let flags = code[ip + 1];
            let has_tag = flags & 1 != 0;
            let has_sub = flags & 2 != 0;
            let has_msg = flags & 4 != 0;
            let pops =
                has_tag as usize + has_sub as usize + has_msg as usize + 1; /* the expression */
            for _ in 0..pops {
                next.stack.pop();
            }
        }
        OpCode::Unpack => {
            let count = code[ip + 1] as usize;
            next.stack.pop();
            for _ in 0..count {
                next.stack.push(SlotType::Value);
            }
        }
        OpCode::Main => {
            // No stack effect — just emits a jump.
            let off = read_u16(code, ip + 1) as usize;
            targets.push(ip + 3 + off);
        }
        OpCode::IterLen | OpCode::IterGet => {
            // IterLen: [it] → [it, len]. len is int-in-range but we don't
            // know if it'll be used as Int; be conservative → Value.
            // IterGet: [it, idx] → [elem].
            if matches!(op, OpCode::IterLen) {
                next.stack.push(SlotType::Value);
            } else {
                next.stack.pop();
                next.stack.pop();
                next.stack.push(SlotType::Value);
            }
        }
        OpCode::TypeWrap => {
            next.stack.pop();
            next.stack.push(SlotType::Value);
        }
        OpCode::IsMut | OpCode::IsType | OpCode::IsTypeMut => {
            if matches!(op, OpCode::IsType) {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
        }
        OpCode::DefinePattern => {
            // No stack effect — registers a pattern globally.
        }
        OpCode::TestPattern => {
            // [value] → [value, bool] — peeks.
            next.stack.push(SlotType::Value);
        }
    }

    (next, terminates, targets)
}

/// Length in bytes of opcode `op` starting at `ip`. `Closure` is
/// variable-length (has a trailing run of upvalue descriptors).
fn opcode_len(op: OpCode, code: &[u8], ip: usize, chunk: &Chunk) -> usize {
    match op {
        // 1-byte opcodes
        OpCode::None
        | OpCode::True
        | OpCode::False
        | OpCode::Pop
        | OpCode::Dup
        | OpCode::Add
        | OpCode::Subtract
        | OpCode::Multiply
        | OpCode::Divide
        | OpCode::Modulo
        | OpCode::Equal
        | OpCode::NotEqual
        | OpCode::Greater
        | OpCode::GreaterEqual
        | OpCode::Less
        | OpCode::LessEqual
        | OpCode::Not
        | OpCode::BitAnd
        | OpCode::BitOr
        | OpCode::BitXor
        | OpCode::BitNot
        | OpCode::ShiftLeft
        | OpCode::ShiftRight
        | OpCode::Negate
        | OpCode::CloseUpvalue
        | OpCode::Return
        | OpCode::Index
        | OpCode::IndexAssign
        | OpCode::IterLen
        | OpCode::IterGet
        | OpCode::ValueConstruct
        | OpCode::Fail => 1,

        // 1-byte opcode + 1-byte operand
        OpCode::Call | OpCode::ErrorConstruct | OpCode::Slice | OpCode::Log | OpCode::Unpack => 2,
        OpCode::CallNamed => 3,

        // 1-byte opcode + u16 operand
        OpCode::Constant
        | OpCode::Increment
        | OpCode::Decrement
        | OpCode::GetLocal
        | OpCode::SetLocal
        | OpCode::GetGlobal
        | OpCode::SetGlobal
        | OpCode::DefineGlobal
        | OpCode::GetUpvalue
        | OpCode::SetUpvalue
        | OpCode::Jump
        | OpCode::JumpIfFalse
        | OpCode::JumpIfTrue
        | OpCode::Loop
        | OpCode::PopJumpIfFalse
        | OpCode::BuildArray
        | OpCode::BuildTuple
        | OpCode::BuildMap
        | OpCode::BuildSet
        | OpCode::GetField
        | OpCode::SetField
        | OpCode::StructDef
        | OpCode::EnumDef
        | OpCode::MakeEnumVariantUnit
        | OpCode::DefinePattern
        | OpCode::GetModuleField
        | OpCode::StringInterp
        | OpCode::Main
        | OpCode::Unless
        | OpCode::TypeWrap
        | OpCode::IsMut
        | OpCode::IsType
        | OpCode::IsTypeMut => 3,

        // 1-byte opcode + u16 + u8
        OpCode::StructLiteral
        | OpCode::DefineMethod
        | OpCode::MakeEnumVariantTuple
        | OpCode::MakeEnumVariantStruct
        | OpCode::MethodCall
        | OpCode::Import => 4,
        // 1-byte opcode + 2*u16
        OpCode::TestPattern | OpCode::Guard => 5,

        // MethodCallNamed: u16 + u8 + u8
        OpCode::MethodCallNamed => 5,

        // DefineGlobalTyped: u16 name + u8 mutable + u16 type
        OpCode::DefineGlobalTyped => 6,

        // Closure is variable-length: u16 constant + N×(u8, u16).
        // Look up the target function's `upvalue_count` in the chunk
        // to compute the full length. If the constant isn't a closure
        // (malformed bytecode), fall back to 3 and let the caller
        // cope — at worst we step into descriptor bytes and interpret
        // them as opcodes, which the subsequent analysis handles
        // conservatively (everything touched becomes `Value`).
        OpCode::Closure => {
            let idx = read_u16(code, ip + 1) as usize;
            let upv = match chunk.constants.get(idx) {
                Some(Value::Closure(cl)) => cl.function.upvalue_count as usize,
                _ => 0,
            };
            3 + upv * 3
        }
    }
}

#[inline]
fn read_u16(code: &[u8], offset: usize) -> u16 {
    ((code[offset] as u16) << 8) | (code[offset + 1] as u16)
}

// ──────────────────────────────────────────────────────────────────────
// Certifier — debug-only invariant checks for B2.1+ consumers
// ──────────────────────────────────────────────────────────────────────

/// Structured result of the certifier. Returning a `Result` with a
/// descriptive string keeps invariant failures out of the panic path so
/// the JIT can log + bail gracefully in release builds (or the caller
/// can `.unwrap()` in tests to get a clean failure message).
pub type CertifyResult = Result<(), String>;

/// Debug-only invariant checks across a function's bytecode + its
/// computed `FunctionSlotTypes`. Intended to be called from
/// `debug_assertions`-guarded code paths in B2.1 codegen, and from
/// tests that want to assert the metadata is self-consistent.
///
/// Invariants verified:
///
/// 1. **No captured slot is virtualizable.** `captured_slots`
///    disqualifies slots from virtualization; if a slot is both
///    Int64-classified *and* captured, `is_virtualizable` must return
///    false. (This is a check on the helper, not the data — catches
///    future refactors that might forget the captured rule.)
///
/// 2. **Every virtualizable slot has a recognized initializer IP.**
///    B2.1c's `GetLocal` fast path would `use_var` an undefined
///    Cranelift variable otherwise.
///
/// 3. **Every `SetLocal` targeting a virtualizable slot is immediately
///    followed by a `Pop`.** Matches Oxigen's peek-not-pop discipline
///    (B2.0 already relied on this; we re-check so future opcode
///    emission changes don't silently break B2.1).
///
/// 4. **No `scope_pop_slot_ip` entry points at a slot that isn't a
///    real local.** Defensive — the walker shouldn't produce these.
///
/// 5. **No `local_init_result_ip` IP points to a `Closure` opcode.**
///    B2.1 only virtualizes `Constant(Integer)` initializers in v1;
///    a Closure-initialized slot should stay on the Value path and
///    never appear in this map for a virtualizable slot.
pub fn certify(func: &Function, types: &FunctionSlotTypes) -> CertifyResult {
    // 1. Captured-slot rule.
    for slot in 0..types.slots.len() as u16 {
        if types.slots.get(slot as usize).copied() == Some(SlotType::Int64)
            && types.captured_slots.contains(&slot)
            && types.is_virtualizable(slot)
        {
            return Err(format!(
                "slot {} is Int64 and captured but is_virtualizable returned true",
                slot
            ));
        }
    }

    // 2. Every virtualizable slot has a known initializer IP.
    for slot in 0..types.slots.len() as u16 {
        if types.is_virtualizable(slot) && !types.has_known_initializer(slot) {
            return Err(format!(
                "virtualizable slot {} has no entry in local_init_result_ip",
                slot
            ));
        }
    }

    // 3. Every SetLocal targeting a virtualizable slot is followed by a Pop.
    let code = &func.chunk.code;
    let mut ip = 0usize;
    while ip < code.len() {
        let Some(op) = OpCode::from_byte(code[ip]) else {
            break;
        };
        if matches!(op, OpCode::SetLocal) {
            let slot = read_u16(code, ip + 1);
            if types.is_virtualizable(slot) {
                let after = ip + 3;
                let next_is_pop = code.get(after).and_then(|&b| OpCode::from_byte(b))
                    == Some(OpCode::Pop);
                if !next_is_pop {
                    return Err(format!(
                        "SetLocal on virtualizable slot {} at ip {} is not followed by Pop \
                         (Oxigen's peek-not-pop discipline broken)",
                        slot, ip
                    ));
                }
            }
        }
        ip += opcode_len(op, code, ip, &func.chunk);
    }

    // 4. scope_pop_slot_ip only references real local slots.
    for (ip, &slot) in &types.scope_pop_slot_ip {
        if (slot as usize) >= types.slots.len() {
            return Err(format!(
                "scope_pop_slot_ip[{}] = {} is out of range (num_slots={})",
                ip,
                slot,
                types.slots.len()
            ));
        }
    }

    // 5. No local_init_result_ip entry points at a Closure opcode
    //    (or any non-Constant opcode, for v1).
    for (&ip, &slot) in &types.local_init_result_ip {
        if !types.is_virtualizable(slot) {
            continue; // only virtualizable slots need the Constant guard
        }
        let Some(op) = code.get(ip).copied().and_then(OpCode::from_byte) else {
            continue;
        };
        if !matches!(op, OpCode::Constant) {
            return Err(format!(
                "local_init_result_ip[{}] = slot {} but opcode at that IP is {:?}, \
                 not Constant — B2.1 v1 only supports Constant(Integer) initializers",
                ip, slot, op
            ));
        }
    }

    Ok(())
}

// ──────────────────────────────────────────────────────────────────────
// Tests
// ──────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::value::{LocalInfo, ParamInfo};

    fn make_fn(name: &str, arity: u8, code: Vec<u8>, constants: Vec<Value>) -> Function {
        let mut func = Function::new(Some(name.to_string()), arity);
        func.chunk.code = code;
        func.chunk.constants = constants;
        // Reserve locals for params + a reasonable buffer for locals.
        for _ in 0..(arity as usize + 8) {
            func.locals.push(LocalInfo::default());
        }
        for i in 0..arity {
            func.params.push(ParamInfo {
                name: format!("p{}", i),
                has_default: false,
                optional: false,
                type_ann: None,
            });
        }
        func
    }

    // ── Lattice ───────────────────────────────────────────────────────

    #[test]
    fn lattice_join_is_commutative_and_conservative() {
        use SlotType::*;
        assert_eq!(Bottom.join(Int64), Int64);
        assert_eq!(Int64.join(Bottom), Int64);
        assert_eq!(Int64.join(Int64), Int64);
        assert_eq!(Int64.join(Value), Value);
        assert_eq!(Value.join(Int64), Value);
        assert_eq!(Bottom.join(Bottom), Bottom);
    }

    // ── Single-opcode transfer functions ─────────────────────────────

    #[test]
    fn constant_int_pushes_int64() {
        let mut f = make_fn(
            "f",
            0,
            vec![OpCode::Constant as u8, 0, 0, OpCode::Return as u8],
            vec![Value::Integer(42)],
        );
        // slot 0 is the closure marker; we use slot 1 to stash a value.
        f.chunk.code = vec![
            OpCode::Constant as u8, 0, 0,
            OpCode::SetLocal as u8, 0, 1,
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Int64);
    }

    #[test]
    fn constant_string_pushes_value() {
        let mut f = make_fn(
            "f",
            0,
            vec![],
            vec![Value::String(crate::vm::value::rc_str("hi"))],
        );
        f.chunk.code = vec![
            OpCode::Constant as u8, 0, 0,
            OpCode::SetLocal as u8, 0, 1,
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Value);
    }

    #[test]
    fn int_plus_int_is_int() {
        // locals[1] = 3 + 5  → int + int → int
        let mut f = make_fn(
            "f",
            0,
            vec![],
            vec![Value::Integer(3), Value::Integer(5)],
        );
        f.chunk.code = vec![
            OpCode::Constant as u8, 0, 0,
            OpCode::Constant as u8, 0, 1,
            OpCode::Add as u8,
            OpCode::SetLocal as u8, 0, 1,
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Int64);
    }

    #[test]
    fn int_plus_string_is_value() {
        let mut f = make_fn(
            "f",
            0,
            vec![],
            vec![
                Value::Integer(3),
                Value::String(crate::vm::value::rc_str("x")),
            ],
        );
        f.chunk.code = vec![
            OpCode::Constant as u8, 0, 0,
            OpCode::Constant as u8, 0, 1,
            OpCode::Add as u8,
            OpCode::SetLocal as u8, 0, 1,
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Value);
    }

    #[test]
    fn typed_int_param_is_int_on_entry() {
        let mut f = make_fn("f", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetLocal as u8, 0, 1,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Int64);
    }

    #[test]
    fn slot_reassigned_to_value_merges_to_value() {
        // slot 1 set first to Int, then to Value → merge Value.
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(7)]);
        f.chunk.code = vec![
            OpCode::Constant as u8, 0, 0,
            OpCode::SetLocal as u8, 0, 1,
            OpCode::None as u8,
            OpCode::SetLocal as u8, 0, 1,
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Value);
    }

    // ── Control flow ──────────────────────────────────────────────────

    #[test]
    fn int_through_unconditional_jump() {
        // slot 1 = 1; jump to end; at end slot 1 still int.
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(1)]);
        f.chunk.code = vec![
            OpCode::Constant as u8, 0, 0,     // ip=0  push 1
            OpCode::SetLocal as u8, 0, 1,     // ip=3  set slot 1
            OpCode::Jump as u8, 0, 2,         // ip=6  jump +2 → ip=11
            OpCode::None as u8,               // ip=9  (dead)
            OpCode::None as u8,               // ip=10 (dead)
            OpCode::None as u8,               // ip=11
            OpCode::Return as u8,             // ip=12
        ];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Int64);
    }

    // ── Realistic shape: the bench_loop pattern ──────────────────────

    #[test]
    fn loop_counter_stays_int() {
        // loop_sum(n): total := 0; i := 1; while i <= n: total += i*2; i += 1; return total
        // Slots: 0=closure, 1=n, 2=total, 3=i
        let mut f = make_fn("loop_sum", 1, vec![], vec![Value::Integer(0), Value::Integer(1), Value::Integer(2)]);
        f.params[0].type_ann = Some("int".to_string());
        // Hand-assembled bytecode. Offsets are approximate — this test
        // is about the fixed-point converging on Int64 for slots 2/3.
        f.chunk.code = vec![
            // total := 0
            OpCode::Constant as u8, 0, 0,    // 0..=2 push 0
            OpCode::SetLocal as u8, 0, 2,    // 3..=5 slot 2 := 0
            // i := 1
            OpCode::Constant as u8, 0, 1,    // 6..=8 push 1
            OpCode::SetLocal as u8, 0, 3,    // 9..=11 slot 3 := 1
            // loop_start: (ip=12)
            // total := total + i * 2
            OpCode::GetLocal as u8, 0, 2,    // 12..=14 push total
            OpCode::GetLocal as u8, 0, 3,    // 15..=17 push i
            OpCode::Constant as u8, 0, 2,    // 18..=20 push 2
            OpCode::Multiply as u8,          // 21 i*2
            OpCode::Add as u8,               // 22 total+(i*2)
            OpCode::SetLocal as u8, 0, 2,    // 23..=25 slot 2 := result
            // i := i + 1
            OpCode::GetLocal as u8, 0, 3,    // 26..=28
            OpCode::Constant as u8, 0, 1,    // 29..=31 push 1
            OpCode::Add as u8,               // 32
            OpCode::SetLocal as u8, 0, 3,    // 33..=35
            // Return total
            OpCode::GetLocal as u8, 0, 2,    // 36..=38
            OpCode::Return as u8,            // 39
        ];
        let r = analyze(&f);
        assert_eq!(r.get(2), SlotType::Int64, "total should stay Int64");
        assert_eq!(r.get(3), SlotType::Int64, "i should stay Int64");
    }

    // ── Integration: analyze real compiled bench code ────────────────
    //
    // These are the B2.0 acceptance-gate tests — they compile a small
    // Oxigen program (matching each benchmark's hot loop shape) and
    // assert that the loop counter / accumulator slots come out as
    // Int64. This is what gates downstream B2.1+.

    /// Lex + parse + compile a source string. Returns the top-level
    /// function, plus — when `inner_name` is provided — the nested
    /// function with that name from the constant pool.
    fn compile_for_analysis(source: &str, inner_name: Option<&str>) -> Function {
        use crate::compiler::Compiler;
        use crate::lexer::Lexer;
        use crate::parser::Parser;

        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer, source);
        let program = parser.parse_program();
        if !parser.errors().is_empty() {
            panic!("Parser errors:\n{}", parser.format_errors());
        }
        let compiler = Compiler::new();
        let top = compiler
            .compile(&program)
            .unwrap_or_else(|errors| panic!("Compile errors: {:?}", errors));

        match inner_name {
            None => top,
            Some(want) => {
                // Walk constants for a Function (wrapped in a closure
                // that we haven't built yet, so the compiler embeds the
                // raw `Function` as a constant).
                for c in &top.chunk.constants {
                    if let Value::Closure(closure) = c {
                        if closure.function.name.as_deref() == Some(want) {
                            return (*closure.function).clone();
                        }
                    }
                }
                // Some compiler versions embed the `Function` directly
                // via a different path; recursively look inside nested
                // functions too.
                for c in &top.chunk.constants {
                    if let Value::Closure(closure) = c {
                        for inner in &closure.function.chunk.constants {
                            if let Value::Closure(ic) = inner {
                                if ic.function.name.as_deref() == Some(want) {
                                    return (*ic.function).clone();
                                }
                            }
                        }
                    }
                }
                panic!(
                    "could not find function {:?} in compiled output. Top-level constants: {:?}",
                    want,
                    top.chunk
                        .constants
                        .iter()
                        .map(|c| format!("{:?}", c))
                        .collect::<Vec<_>>()
                );
            }
        }
    }

    /// Helper: count Int64 slots. Includes param slot 0 which is the
    /// closure marker (always Value), so a function with `k` int
    /// locals/params returns at least `k` here.
    fn count_int64(tys: &FunctionSlotTypes) -> usize {
        tys.int64_count()
    }

    #[test]
    fn annotated_int_param_classifies_as_int() {
        // With an explicit `<int>` param annotation, the param slot is
        // Int64 at entry. This is the B2.2 trigger for the unboxed
        // calling convention.
        let src = r#"
            fun loop_sum(n <int>) {
                total := 0
                i := 1
                repeat when i <= n {
                    total := total + i
                    i := i + 1
                }
                total
            }
            loop_sum(100)
        "#;
        let f = compile_for_analysis(src, Some("loop_sum"));
        let tys = analyze(&f);
        // slot 1 is `n` (slot 0 = closure marker).
        assert_eq!(
            tys.get(1),
            SlotType::Int64,
            "annotated int param must be Int64. Full slots: {:?}",
            tys.slots
        );
        assert!(
            count_int64(&tys) >= 3,
            "expected ≥3 Int64 slots (n, total, i); got {:?}",
            tys.slots
        );
    }

    #[test]
    fn int_constants_propagate_through_loop_even_without_param_annotation() {
        // This mirrors the actual `example/bench_loop.oxi` which does
        // NOT type-annotate its parameter. Even so, locals initialized
        // from integer constants should flow as Int64 through the
        // loop. (The parameter `n` stays Value because it's unannotated
        // — bench_loop wins are on the locals, not the param.)
        let src = r#"
            fun loop_sum(n) {
                total := 0
                i := 1
                repeat when i <= n {
                    total := total + i * 2
                    i := i + 1
                }
                total
            }
            loop_sum(100)
        "#;
        let f = compile_for_analysis(src, Some("loop_sum"));
        let tys = analyze(&f);

        // `total` and `i` should be Int64 even though `n` isn't.
        assert!(
            count_int64(&tys) >= 2,
            "expected ≥2 Int64 locals (total, i); got {:?}",
            tys.slots
        );
    }

    #[test]
    fn nested_loop_locals_stay_int() {
        // Actual bench_nested_loop.oxi shape.
        let src = r#"
            fun nested_sum(n) {
                total := 0
                i := 1
                repeat when i <= n {
                    j := 1
                    repeat when j <= n {
                        total := total + i * j
                        j := j + 1
                    }
                    i := i + 1
                }
                total
            }
            nested_sum(10)
        "#;
        let f = compile_for_analysis(src, Some("nested_sum"));
        let tys = analyze(&f);
        assert!(
            count_int64(&tys) >= 3,
            "expected ≥3 Int64 locals (total, i, j); got {:?}",
            tys.slots
        );
    }

    #[test]
    fn bench_fib_unannotated_param_stays_value() {
        // Documents the current B2.0 limitation: bench_fib's `n` has
        // no annotation, so flow-forward analysis can't prove it's an
        // int — bench_fib won't benefit from B2.2 until A3 feedback
        // vectors (or a backward type inference) is wired in. This
        // test will be inverted once that works.
        let src = r#"
            fun fib(n) {
                option { n < 2 -> n, fib(n - 1) + fib(n - 2) }
            }
            fib(10)
        "#;
        let f = compile_for_analysis(src, Some("fib"));
        let tys = analyze(&f);
        assert_eq!(
            tys.get(1),
            SlotType::Value,
            "unannotated fib param is currently Value (B2.0 limitation); \
             invert this test when A3 feedback-driven typing lands. \
             Full slots: {:?}",
            tys.slots
        );
    }

    #[test]
    fn fib_with_typed_param_classifies_as_int() {
        // Proof that the analysis CAN type fib's param if the user
        // writes the annotation. B2.2 will unbox this call site.
        let src = r#"
            fun fib(n <int>) {
                option { n < 2 -> n, fib(n - 1) + fib(n - 2) }
            }
            fib(10)
        "#;
        let f = compile_for_analysis(src, Some("fib"));
        let tys = analyze(&f);
        assert_eq!(
            tys.get(1),
            SlotType::Int64,
            "typed fib(n <int>) param must classify as Int64. \
             Full slots: {:?}",
            tys.slots
        );
    }

    // ── Guard correctness ────────────────────────────────────────────

    #[test]
    fn call_result_is_value_even_if_args_are_int() {
        // slot 1 = f(int_arg) → Value (callee return type unknown)
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(1)]);
        f.chunk.code = vec![
            OpCode::None as u8,                  // closure
            OpCode::Constant as u8, 0, 0,        // int arg
            OpCode::Call as u8, 1,               // call with 1 arg
            OpCode::SetLocal as u8, 0, 1,        // slot 1 := result
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Value);
    }

    // ── B2.1a: new metadata tests ─────────────────────────────────────

    #[test]
    fn local_init_result_ip_recorded_for_constant_int_init() {
        // With arity=0, slot 1 is the first non-closure-marker local,
        // and the first Constant push at ip=0 initializes it.
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(42)]);
        f.chunk.code = vec![
            OpCode::Constant as u8, 0, 0,        // ip=0 push 42 → slot 1 init
            OpCode::None as u8,                  // ip=3 push None
            OpCode::Return as u8,                // ip=4
        ];
        let r = analyze(&f);
        assert_eq!(r.local_init_result_ip.get(&0), Some(&1));
        assert!(r.is_virtualizable(1));
    }

    #[test]
    fn local_init_result_ip_picks_first_when_reassigned() {
        // slot 1 := 1; slot 1 := 2. First init is ip=0 (the first
        // Constant push that lands at position 1), not the later
        // SetLocal which only rewrites the slot.
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(1), Value::Integer(2)]);
        f.chunk.code = vec![
            OpCode::Constant as u8, 0, 0,        // ip=0 push 1 → slot 1 init
            OpCode::Constant as u8, 0, 1,        // ip=3 push 2 (temp at slot 2)
            OpCode::SetLocal as u8, 0, 1,        // ip=6 slot 1 := 2
            OpCode::Pop as u8,                   // ip=9
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert_eq!(
            r.local_init_result_ip.get(&0),
            Some(&1),
            "first initializer should be at ip=0, not the later SetLocal"
        );
    }

    #[test]
    fn condition_cleanup_pops_detected_on_loop_pattern() {
        // Compile a real loop and check the condition-cleanup pops are
        // collected. Uses the already-proven loop_sum shape.
        let src = r#"
            fun loop_sum(n <int>) {
                total := 0
                i := 1
                repeat when i <= n {
                    total := total + i
                    i := i + 1
                }
                total
            }
            loop_sum(10)
        "#;
        let f = compile_for_analysis(src, Some("loop_sum"));
        let r = analyze(&f);
        // There should be at least two condition-cleanup pops: one at
        // the loop body entry (fall-through after JumpIfFalse) and one
        // at the loop exit (the branch target). Exact IPs depend on
        // the compiler, but we can check the set is non-empty and that
        // each claimed IP IS actually a Pop in the bytecode.
        assert!(
            !r.condition_cleanup_pop_ips.is_empty(),
            "expected at least one condition-cleanup Pop; got {:?}",
            r.condition_cleanup_pop_ips
        );
        for &ip in &r.condition_cleanup_pop_ips {
            let op = OpCode::from_byte(f.chunk.code[ip]);
            assert_eq!(
                op,
                Some(OpCode::Pop),
                "claimed condition-cleanup Pop at ip={} is actually {:?}",
                ip,
                op
            );
        }
    }

    #[test]
    fn captured_slots_empty_for_flat_function() {
        // loop_sum has no nested closures, so captured_slots should be
        // empty.
        let src = r#"
            fun loop_sum(n <int>) {
                total := 0
                i := 1
                repeat when i <= n {
                    total := total + i
                    i := i + 1
                }
                total
            }
            loop_sum(10)
        "#;
        let f = compile_for_analysis(src, Some("loop_sum"));
        let r = analyze(&f);
        assert!(
            r.captured_slots.is_empty(),
            "no nested closures should mean no captured slots; got {:?}",
            r.captured_slots
        );
        // All the int locals (n, total, i) should be virtualizable.
        for slot in 1..=3u16 {
            assert!(
                r.is_virtualizable(slot),
                "slot {} should be virtualizable (Int64 + not captured + has init)",
                slot
            );
        }
    }

    #[test]
    fn certify_accepts_loop_sum() {
        let src = r#"
            fun loop_sum(n <int>) {
                total := 0
                i := 1
                repeat when i <= n {
                    total := total + i
                    i := i + 1
                }
                total
            }
            loop_sum(10)
        "#;
        let f = compile_for_analysis(src, Some("loop_sum"));
        let r = analyze(&f);
        certify(&f, &r).expect("loop_sum should certify");
    }

    #[test]
    fn is_virtualizable_requires_all_three_conditions() {
        // Int64 slot with an initializer that isn't captured: OK.
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(0)]);
        f.chunk.code = vec![
            OpCode::Constant as u8, 0, 0,        // ip=0: slot 1 init
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        let mut r = analyze(&f);
        assert!(r.is_virtualizable(1));

        // Pretend it's captured → no longer virtualizable.
        r.captured_slots.insert(1);
        assert!(!r.is_virtualizable(1));
        r.captured_slots.remove(&1);

        // Remove the initializer record → no longer virtualizable.
        r.local_init_result_ip.clear();
        r.init_sites.remove(&1);
        assert!(!r.is_virtualizable(1));
    }

    #[test]
    fn certify_rejects_non_constant_initializer_for_virtualizable_slot() {
        // Real init at ip=0 (Constant) — legitimate. Manufacture a
        // spurious local_init_result_ip entry pointing at ip=3 where
        // the opcode is None (non-Constant). Certifier rule 5 catches.
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(7)]);
        f.chunk.code = vec![
            OpCode::Constant as u8, 0, 0,       // ip=0
            OpCode::None as u8,                 // ip=3 (not a Constant)
            OpCode::Return as u8,               // ip=4
        ];
        let mut r = analyze(&f);
        // Point slot 1's initializer at the None at ip=3.
        r.local_init_result_ip.clear();
        r.local_init_result_ip.insert(3, 1);
        assert!(r.is_virtualizable(1));
        let err = certify(&f, &r).expect_err("certifier should reject");
        assert!(
            err.contains("not Constant"),
            "expected message about non-Constant initializer, got: {}",
            err
        );
    }

    #[test]
    fn int_param_is_virtualizable_via_init_sites() {
        // Param slot is in init_sites even though it's not in
        // local_init_result_ip (params are initialized by the caller,
        // before any bytecode runs).
        let mut f = make_fn("f", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetLocal as u8, 0, 1,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Int64);
        assert!(r.init_sites.contains(&1));
        assert!(r.is_virtualizable(1));
        assert!(
            !r.local_init_result_ip.values().any(|&s| s == 1),
            "param slot should NOT have a local_init_result_ip entry"
        );
    }

    // ── A1: specialized-entry eligibility ────────────────────────────

    #[test]
    fn specialized_eligible_typed_int_param_returning_int() {
        // fun f(n <int>) { f(0); n }  — minimal self-call to satisfy
        // the has_call eligibility gate (A2.5 commit 5 requires at
        // least one Call since the specialized body is only useful
        // as a direct-call target).
        let mut f = make_fn("f", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetLocal as u8, 0, 1, // push f (self)
            OpCode::Constant as u8, 0, 0, // push 0
            OpCode::Call as u8, 1,        // call (synthetic arity 1)
            OpCode::Pop as u8,            // discard result
            OpCode::GetLocal as u8, 0, 1, // push n
            OpCode::Return as u8,
        ];
        f.chunk.constants = vec![Value::Integer(0)];
        let r = analyze(&f);
        assert!(r.specialized_entry_eligible);
        assert_eq!(r.specialized_param_slots, vec![1]);
    }

    #[test]
    fn specialized_ineligible_zero_arity() {
        // fun f() { 42 } — nothing to unbox, not worth a second entry.
        let f = make_fn("f", 0, vec![
            OpCode::Constant as u8, 0, 0,
            OpCode::Return as u8,
        ], vec![Value::Integer(42)]);
        let r = analyze(&f);
        assert!(!r.specialized_entry_eligible);
        assert!(r.specialized_param_slots.is_empty());
    }

    #[test]
    fn specialized_eligible_even_with_value_return() {
        // fun f(n <int>) { f(0); "hello" } — return is a String; still
        // eligible because the specialized body's Return path will
        // bail via status 2 on non-Integer at runtime. has_call
        // satisfied via the self-call.
        let mut f = make_fn("f", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetLocal as u8, 0, 1,
            OpCode::Constant as u8, 0, 0,
            OpCode::Call as u8, 1,
            OpCode::Pop as u8,
            OpCode::Constant as u8, 0, 1,   // push "hello"
            OpCode::Return as u8,
        ];
        f.chunk.constants = vec![
            Value::Integer(0),
            Value::String(crate::vm::value::rc_str("hello")),
        ];
        let r = analyze(&f);
        assert!(r.specialized_entry_eligible);
    }

    #[test]
    fn specialized_ineligible_untyped_value_param_with_no_int_demand() {
        // fun f(n) { n } — `n` is Value-typed (no annotation) AND has
        // no int-demand use (Return is neutral). int_mirror won't
        // pick it up. So the param isn't Int-stable → ineligible.
        let mut f = make_fn("f", 1, vec![], vec![]);
        // no type_ann — defaults to Value
        f.chunk.code = vec![
            OpCode::GetLocal as u8, 0, 1,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert!(!r.specialized_entry_eligible);
    }

    #[test]
    fn specialized_eligible_int_param_with_arith_and_return() {
        // fun f(n <int>) { f(0); n + 1 } — has_call satisfied, arith
        // return eligible.
        let mut f = make_fn("f", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetLocal as u8, 0, 1,
            OpCode::Constant as u8, 0, 0,
            OpCode::Call as u8, 1,
            OpCode::Pop as u8,
            OpCode::GetLocal as u8, 0, 1,
            OpCode::Constant as u8, 0, 1,
            OpCode::Add as u8,
            OpCode::Return as u8,
        ];
        f.chunk.constants = vec![Value::Integer(0), Value::Integer(1)];
        let r = analyze(&f);
        assert!(r.specialized_entry_eligible);
        assert_eq!(r.specialized_param_slots, vec![1]);
    }
}
