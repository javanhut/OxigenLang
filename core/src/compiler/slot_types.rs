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
use crate::vm::value::Function;
#[cfg(test)]
use crate::vm::value::Value;

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

    /// B2.2: true iff the function is `specialized_entry_eligible` AND
    /// its body contains at least one `GetUpvalue`. The JIT engine
    /// uses this to pick `SpecializedEntryKind::NativeIntBodyWithClosure`
    /// (closure pointer in a register) over the plain `NativeIntBody`.
    /// False for functions that don't read upvalues — those continue
    /// to use the original int-only specialized ABI.
    pub wants_closure_arg: bool,

    /// IPs of `OpCode::TypeWrap` whose runtime effect is provably
    /// identity: target constant is the string `"INTEGER"` AND the
    /// abstract stack top at entry is `Int64`. The JIT skips the
    /// `jit_type_wrap` FFI call at these IPs entirely — `convert_to_type`
    /// for `INTEGER` on a `Value::Integer` is just `value.clone()`,
    /// which is a bit-copy with no Rc. Eliminates ~50k FFI hops per
    /// `bench_collatz` run (one per `steps <int> := 0` reinit per
    /// `collatz_steps` call) plus the auto-flush that the helper-call
    /// dispatch path would otherwise force.
    pub noop_type_wrap_ips: std::collections::HashSet<usize>,
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

    // Closure marker at 0, params at 1..=arity; these are the first slot writes.
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

    // first_init_ip records the FIRST init IP per slot, not later re-writes.
    let mut first_init_ip: HashMap<u16, usize> = HashMap::new();
    let mut scope_pop_slot_ip: HashMap<usize, u16> = HashMap::new();

    // Worklist of IPs whose on-entry state changed and whose post-state needs recomputing.
    let mut states: HashMap<usize, AbstractState> = HashMap::new();
    states.insert(0, entry.clone());
    let mut worklist: Vec<usize> = vec![0];

    while let Some(ip) = worklist.pop() {
        // Stop at a terminator or an already-recorded IP; the worklist carries the recomputation.
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
            let fallthrough = cursor
                + chunk
                    .instruction_len(cursor)
                    .expect("compiler produced malformed bytecode");

            // A push growing the stack into the local range initializes that slot; Oxigen emits no SetLocal for `total := 0`.
            let depth_after = next_state.stack.len();

            // A vacated position's first_init_ip is stale: a transient can share a stack slot with a later local.
            if matches!(
                op,
                OpCode::BuildArray
                    | OpCode::BuildTuple
                    | OpCode::BuildSet
                    | OpCode::BuildMap
                    | OpCode::StructLiteral
                    | OpCode::Call
                    | OpCode::CallNamed
                    | OpCode::MethodCall
                    | OpCode::MethodCallNamed
            ) && depth_after < depth_before
            {
                for pos in depth_after..depth_before {
                    first_init_ip.remove(&(pos as u16));
                }
            }

            if matches!(op, OpCode::Constant) && depth_after == depth_before + 1 {
                let new_pos = depth_after - 1;
                if new_pos < num_slots {
                    let ty = next_state.stack.last().copied().unwrap_or(SlotType::Bottom);
                    next_state.write_slot(new_pos, ty);
                    // A Constant initializes a slot only if the next op leaves it in place, not if it consumes the top.
                    let next_op = code.get(fallthrough).and_then(|&b| OpCode::from_byte(b));
                    let next_consumes_top = matches!(
                        next_op,
                        Some(OpCode::Pop)
                            | Some(OpCode::Call)
                            | Some(OpCode::CallNamed)
                            | Some(OpCode::MethodCall)
                            | Some(OpCode::Add)
                            | Some(OpCode::Subtract)
                            | Some(OpCode::Multiply)
                            | Some(OpCode::Divide)
                            | Some(OpCode::Modulo)
                            | Some(OpCode::Equal)
                            | Some(OpCode::NotEqual)
                            | Some(OpCode::Less)
                            | Some(OpCode::LessEqual)
                            | Some(OpCode::Greater)
                            | Some(OpCode::GreaterEqual)
                            | Some(OpCode::BitAnd)
                            | Some(OpCode::BitOr)
                            | Some(OpCode::BitXor)
                            | Some(OpCode::ShiftLeft)
                            | Some(OpCode::ShiftRight)
                            | Some(OpCode::Negate)
                            | Some(OpCode::Not)
                            | Some(OpCode::BitNot)
                            | Some(OpCode::JumpIfFalse)
                            | Some(OpCode::JumpIfTrue)
                            | Some(OpCode::PopJumpIfFalse)
                            | Some(OpCode::Unless)
                            | Some(OpCode::SetLocal)
                            | Some(OpCode::SetGlobal)
                            | Some(OpCode::DefineGlobal)
                            | Some(OpCode::DefineGlobalTyped)
                            | Some(OpCode::SetUpvalue)
                            | Some(OpCode::Increment)
                            | Some(OpCode::Decrement)
                            | Some(OpCode::Index)
                            | Some(OpCode::IndexAssign)
                            | Some(OpCode::IsType)
                            | Some(OpCode::IsMut)
                            | Some(OpCode::IsTypeMut)
                            | Some(OpCode::BuildArray)
                            | Some(OpCode::BuildSet)
                            | Some(OpCode::BuildTuple)
                            | Some(OpCode::BuildMap)
                            | Some(OpCode::StructLiteral)
                            | Some(OpCode::Closure)
                            | Some(OpCode::Return)
                    );
                    if !next_consumes_top {
                        let slot = new_pos as u16;
                        let prev = first_init_ip.get(&slot).copied();
                        // Latest wins: earlier Constants at a position are usually array-literal elements popped by BuildArray.
                        if prev.is_none_or(|p| cursor > p) {
                            first_init_ip.insert(slot, cursor);
                        }
                    }
                }
            }

            // Any op leaving a value in the local range must join its type, not just Constant.
            if depth_after >= 1 && !matches!(op, OpCode::Constant) {
                let new_pos = depth_after - 1;
                if new_pos < num_slots {
                    // Only join when the slot is already initialized on this path; an earlier push there is a transient.
                    let already_init = state
                        .slot_types
                        .get(new_pos)
                        .copied()
                        .unwrap_or(SlotType::Bottom)
                        != SlotType::Bottom;
                    if already_init {
                        let ty = next_state.stack.last().copied().unwrap_or(SlotType::Bottom);
                        next_state.write_slot(new_pos, ty);
                    }
                }
            }

            // A Pop whose depth_after lands on a live slot's position destroys that slot.
            if matches!(op, OpCode::Pop) && depth_before > 0 {
                let popped_pos = depth_before - 1;
                if popped_pos < num_slots {
                    // Consult the state before this Pop to see whether the slot was live.
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

            // Merge our post-op state into each branch target and enqueue if the join changed.
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

            // A known join point merges and breaks; the worklist re-processes if our contribution changed it.
            if let Some(existing) = states.get_mut(&fallthrough) {
                let changed = existing.join(&next_state);
                if changed && !worklist.contains(&fallthrough) {
                    worklist.push(fallthrough);
                }
                break;
            }

            // Fresh IP: record it so future joiners find us, then continue linearly.
            states.insert(fallthrough, next_state.clone());
            state = next_state;
            cursor = fallthrough;
        }
    }

    // Deliberately ignores `stack`: a position in the local range can hold a transient.
    let mut result = vec![SlotType::Bottom; num_slots];
    for state in states.values() {
        for (i, slot) in result.iter_mut().enumerate() {
            let ty = state.slot_types.get(i).copied().unwrap_or(SlotType::Bottom);
            *slot = slot.join(ty);
        }
    }

    // Invert first_init_ip to IP -> slot; B2.1 codegen dispatches by IP.
    let local_init_result_ip: HashMap<usize, u16> = first_init_ip
        .into_iter()
        .map(|(slot, ip)| (ip, slot))
        .collect();

    // Params are initialized at entry, not at any bytecode IP.
    let mut init_sites: HashSet<u16> = HashSet::new();
    for i in 1..=func.arity as u16 {
        init_sites.insert(i);
    }
    for &slot in local_init_result_ip.values() {
        init_sites.insert(slot);
    }

    // A conditional branch whose fall-through and target both Pop is the cleanup pair to suppress.
    let condition_cleanup_pop_ips = collect_condition_cleanup_pops(code, chunk);

    // A Closure upvalue descriptor with is_local = 1 captures one of this function's slots by reference.
    let captured_slots = collect_captured_slots(code, chunk);

    // Cross-check SetLocal-written and GetLocal-read slots against the param range and captured_slots.
    let int_mirror_param_slots = collect_int_mirror_param_slots(
        code,
        chunk,
        func.arity as u16,
        &captured_slots,
        &result,
        &func.params,
    );

    // Re-run with typed params: classifying them as Value would disqualify fib(n) returning n.
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
    let (specialized_entry_outcome, specialized_param_slots, wants_closure_arg) =
        compute_specialized_entry_eligibility(
            code,
            chunk,
            func.arity as u16,
            &result,
            &captured_slots,
            &int_mirror_param_slots,
            eligibility_states,
        );
    let specialized_entry_eligible = specialized_entry_outcome.is_eligible();

    // A TypeWrap to INTEGER whose stack top is already Int64 is identity, so the JIT can skip the FFI.
    let mut noop_type_wrap_ips: HashSet<usize> = HashSet::new();
    for (&ip, st) in eligibility_states.iter() {
        if ip + 2 >= code.len() {
            continue;
        }
        let op = match OpCode::from_byte(code[ip]) {
            Some(o) => o,
            None => continue,
        };
        if op != OpCode::TypeWrap {
            continue;
        }
        let target_idx = read_u16(code, ip + 1) as usize;
        let target_is_int = chunk
            .constants
            .get(target_idx)
            .and_then(|v| v.as_string())
            .is_some_and(|s| s.as_str() == "INTEGER");
        if !target_is_int {
            continue;
        }
        let top = st.stack.last().copied().unwrap_or(SlotType::Value);
        if top == SlotType::Int64 {
            noop_type_wrap_ips.insert(ip);
        }
    }

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
        wants_closure_arg,
        noop_type_wrap_ips,
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
            let fallthrough = cursor
                + chunk
                    .instruction_len(cursor)
                    .expect("compiler produced malformed bytecode");

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
) -> (SpecEligibilityOutcome, Vec<u16>, bool) {
    // True iff a GetUpvalue was seen and the function is otherwise eligible; picks the closure-aware ABI.
    if arity == 0 {
        return (SpecEligibilityOutcome::RejectedZeroArity, Vec::new(), false);
    }

    // Every param slot must be Int-stable AND not captured.
    for slot in 1..=arity {
        let is_int_typed = matches!(
            slot_types
                .get(slot as usize)
                .copied()
                .unwrap_or(SlotType::Value),
            SlotType::Int64
        );
        let is_int_demand_mirror = int_mirror_param_slots.contains(&slot);
        if !(is_int_typed || is_int_demand_mirror) {
            return (SpecEligibilityOutcome::RejectedParamNotInt, Vec::new(), false);
        }
        if captured_slots.contains(&slot) {
            return (SpecEligibilityOutcome::RejectedParamCaptured, Vec::new(), false);
        }
    }

    // Closure and SetUpvalue/CloseUpvalue still bail; GetUpvalue alone is acceptable.
    let mut has_return = false;
    let mut has_call = false;
    let mut has_get_upvalue = false;
    let mut return_ips: Vec<usize> = Vec::new();
    let mut ip = 0;
    while ip < code.len() {
        let op = match OpCode::from_byte(code[ip]) {
            Some(o) => o,
            None => break,
        };
        let op_len = chunk
            .instruction_len(ip)
            .expect("compiler produced malformed bytecode");

        match op {
            OpCode::Closure => {
                return (SpecEligibilityOutcome::RejectedHasClosureOp, Vec::new(), false);
            }
            // SetUpvalue needs write-through into the live Rc<RefCell<Upvalue>>; CloseUpvalue keeps the helper path.
            OpCode::SetUpvalue | OpCode::CloseUpvalue => {
                return (SpecEligibilityOutcome::RejectedHasUpvalueOp, Vec::new(), false);
            }
            OpCode::GetUpvalue => {
                has_get_upvalue = true;
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
        return (SpecEligibilityOutcome::RejectedNoReturn, Vec::new(), false);
    }
    // Relaxed for GetUpvalue closures: without a Call the IC still dispatches through the specialized entry.
    if !has_call && !has_get_upvalue {
        return (SpecEligibilityOutcome::RejectedNoCall, Vec::new(), false);
    }

    // Every Return top must be non-Bottom; Value is fine since the trampoline tag-checks at runtime.
    for &rip in &return_ips {
        let state = match states.get(&rip) {
            Some(s) => s,
            None => {
                return (SpecEligibilityOutcome::RejectedReturnUnreachable, Vec::new(), false);
            }
        };
        let top_ty = state.stack.last().copied().unwrap_or(SlotType::Bottom);
        // Bottom means unreachable → still reject (can't trust).
        if matches!(top_ty, SlotType::Bottom) {
            return (SpecEligibilityOutcome::RejectedReturnUnreachable, Vec::new(), false);
        }
        // Value and Int64 are both fine: the trampoline's runtime tag check handles correctness.
    }

    let param_slots: Vec<u16> = (1..=arity).collect();
    (SpecEligibilityOutcome::Eligible, param_slots, has_get_upvalue)
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
    params: &[crate::vm::value::ParamInfo],
) -> HashSet<u16> {
    if arity == 0 {
        return HashSet::new();
    }

    let mut written: HashSet<u16> = HashSet::new();
    // A param with any non-int consumer is ineligible; int-consumers are evidence it is Int-shaped.
    let mut has_int_demand: HashSet<u16> = HashSet::new();
    let mut has_non_int_demand: HashSet<u16> = HashSet::new();

    let mut ip = 0;
    while ip < code.len() {
        let op = match OpCode::from_byte(code[ip]) {
            Some(o) => o,
            None => break,
        };
        let op_len = chunk
            .instruction_len(ip)
            .expect("compiler produced malformed bytecode");

        match op {
            OpCode::SetLocal => {
                let slot = read_u16(code, ip + 1);
                written.insert(slot);
            }
            OpCode::GetLocal => {
                let slot = read_u16(code, ip + 1);
                // Skip a bounded run of push-one-int opcodes; the param stays at stack[top-1] until a classifier fires.
                let mut cursor = ip + op_len;
                let mut skipped = 0;
                let classifier_op = loop {
                    if skipped > 4 || cursor >= code.len() {
                        break None;
                    }
                    let cop = OpCode::from_byte(code[cursor]);
                    match cop {
                        // Neutral push: the param is still on the stack, just below the new top.
                        Some(OpCode::Constant) => {
                            // Only an integer constant is neutral; a float one makes the following arithmetic float.
                            let idx = read_u16(code, cursor + 1) as usize;
                            let is_int_const = chunk
                                .constants
                                .get(idx)
                                .and_then(|v| v.as_integer())
                                .is_some();
                            if !is_int_const {
                                break Some(OpCode::Constant);
                            }
                            let step = chunk
                                .instruction_len(cursor)
                                .expect("compiler produced malformed bytecode");
                            cursor += step;
                            skipped += 1;
                            continue;
                        }
                        Some(OpCode::None) | Some(OpCode::True) | Some(OpCode::False) => {
                            let step = chunk
                                .instruction_len(cursor)
                                .expect("compiler produced malformed bytecode");
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
                        OpCode::Constant
                        | OpCode::GetField
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
                        | OpCode::IterEntry
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
        // A <float> param mirrored as int fails the tag guard on every call, and the bail corrupts the caller's locals.
        if params
            .get(slot as usize - 1)
            .and_then(|p| p.type_ann.as_deref())
            .is_some_and(|t| t.eq_ignore_ascii_case("float"))
        {
            continue;
        }
        if !has_int_demand.contains(&slot) {
            // No evidence of int use, so the tag guard would be pure overhead.
            continue;
        }
        if has_non_int_demand.contains(&slot) {
            // Not always int: the guard would bail every Value-shaped call and permanently un-JIT the function.
            continue;
        }
        if captured_slots.contains(&slot) {
            continue;
        }
        // Already-Int64 params don't need a mirror — B2.1 covers them.
        if slot_types
            .get(slot as usize)
            .copied()
            .is_some_and(|t| matches!(t, SlotType::Int64))
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
///
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
        let op_len = chunk
            .instruction_len(ip)
            .expect("compiler produced malformed bytecode");

        match op {
            OpCode::JumpIfFalse | OpCode::JumpIfTrue | OpCode::Unless => {
                // 1-byte opcode + u16 forward offset.
                let off = read_u16(code, ip + 1) as usize;
                let fall_ip = ip + op_len;
                let target_ip = ip + op_len + off;
                let fall_is_pop =
                    code.get(fall_ip).and_then(|&b| OpCode::from_byte(b)) == Some(OpCode::Pop);
                let target_is_pop =
                    code.get(target_ip).and_then(|&b| OpCode::from_byte(b)) == Some(OpCode::Pop);
                if fall_is_pop && target_is_pop {
                    out.insert(fall_ip);
                    out.insert(target_ip);
                }
            }
            // PopJumpIfFalse consumes its own condition, so there is no cleanup Pop.
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
            let upvalue_count = match chunk.constants.get(const_idx).and_then(|v| v.as_closure()) {
                Some(cl) => cl.function.upvalue_count as usize,
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
            ip += chunk
                .instruction_len(ip)
                .expect("compiler produced malformed bytecode");
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
            let ty = if chunk.constants.get(idx).and_then(|v| v.as_integer()).is_some() {
                SlotType::Int64
            } else {
                SlotType::Value
            };
            next.stack.push(ty);
        }

        // None / True / False → Value for v1.
        OpCode::None | OpCode::True | OpCode::False => {
            next.stack.push(SlotType::Value);
        }

        OpCode::Pop => {
            // Clearing on scope-end Pop stops later transients at the same position from being read as the dead slot.
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

        // Comparison yields Bool, tracked as Value for now.
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
            let cur = next.stack.get(slot).copied().unwrap_or(SlotType::Value);
            next.write_slot(slot, cur);
        }

        OpCode::GetLocal => {
            let slot = read_u16(code, ip + 1) as usize;
            let ty = next.stack.get(slot).copied().unwrap_or(SlotType::Value);
            next.stack.push(ty);
        }
        OpCode::SetLocal => {
            let slot = read_u16(code, ip + 1) as usize;
            // SetLocal peeks and copies; the top is not popped (vm/mod.rs uses peek(0).clone()).
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
        // Treating CloseUpvalue as stack-neutral makes a captured loop local gain a slot per backedge and never converge.
        OpCode::CloseUpvalue => {
            next.stack.pop();
        }
        // No stack effect; unreachable in practice since the JIT scan rejects such functions.
        OpCode::CloseUpvalueAt => {}

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
            // Peeks top without popping, so both branches share the same stack.
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
            // Truthy condition jumps to the alternative.
            next.stack.pop();
            targets.push(ip + 3 + off);
        }

        OpCode::Return => {
            next.stack.pop();
            terminates = true;
        }

        // Pops args and callee, pushes Value; callee return types are not tracked.
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

        // The operand stream carries upvalue descriptors; opcode_len skips them.
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
            // DefineMethod consumes (name, closure) pairs; the rest just push Value.
            if matches!(op, OpCode::DefineMethod) {
                let count = code[ip + 3] as usize;
                for _ in 0..(2 * count) {
                    next.stack.pop();
                }
            }
            // StructDef / EnumDef register in globals rather than consuming from the stack.
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
            // Pops one slot per set flag (tag/sub/msg) and pushes None; see vm::handle_log.
            let flags = code[ip + 1];
            let has_tag = flags & 1 != 0;
            let has_sub = flags & 2 != 0;
            let has_msg = flags & 4 != 0;
            let pops = has_tag as usize + has_sub as usize + has_msg as usize;
            for _ in 0..pops {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
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
        OpCode::IterLen | OpCode::IterGet | OpCode::IterEntry => {
            // Must match the VM dispatch arms exactly or the JIT virtualizes the wrong slots (silent infinite loops).
            next.stack.pop();
            if matches!(op, OpCode::IterGet | OpCode::IterEntry) {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
            if matches!(op, OpCode::IterEntry) {
                next.stack.push(SlotType::Value);
            }
        }
        OpCode::TypeWrap => {
            // TypeWrap to INTEGER is identity, so preserve Int64 and keep typed-int locals virtualizable.
            let top = next.stack.pop().unwrap_or(SlotType::Value);
            let target_idx = read_u16(code, ip + 1) as usize;
            let target_is_int = chunk
                .constants
                .get(target_idx)
                .and_then(|v| v.as_string())
                .is_some_and(|s| s.as_str() == "INTEGER");
            if target_is_int && top == SlotType::Int64 {
                next.stack.push(SlotType::Int64);
            } else {
                next.stack.push(SlotType::Value);
            }
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
        // No operand-stack effect; unreachable since scan rejects these functions.
        OpCode::PushHandler | OpCode::PopHandler => {}
    }

    (next, terminates, targets)
}

#[inline]
fn read_u16(code: &[u8], offset: usize) -> u16 {
    ((code[offset] as u16) << 8) | (code[offset + 1] as u16)
}

// Certifier: debug-only invariant checks for B2.1+ consumers.

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
                let next_is_pop =
                    code.get(after).and_then(|&b| OpCode::from_byte(b)) == Some(OpCode::Pop);
                if !next_is_pop {
                    return Err(format!(
                        "SetLocal on virtualizable slot {} at ip {} is not followed by Pop \
                         (Oxigen's peek-not-pop discipline broken)",
                        slot, ip
                    ));
                }
            }
        }
        ip += func
            .chunk
            .instruction_len(ip)
            .map_err(|err| format!("invalid bytecode at ip {}: {:?}", ip, err))?;
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

    // 5. No local_init_result_ip entry points at a non-Constant opcode.
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

// Tests

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
            OpCode::Constant as u8,
            0,
            0,
            OpCode::SetLocal as u8,
            0,
            1,
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
            OpCode::Constant as u8,
            0,
            0,
            OpCode::SetLocal as u8,
            0,
            1,
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Value);
    }

    #[test]
    fn int_plus_int_is_int() {
        // locals[1] = 3 + 5  → int + int → int
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(3), Value::Integer(5)]);
        f.chunk.code = vec![
            OpCode::Constant as u8,
            0,
            0,
            OpCode::Constant as u8,
            0,
            1,
            OpCode::Add as u8,
            OpCode::SetLocal as u8,
            0,
            1,
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
            OpCode::Constant as u8,
            0,
            0,
            OpCode::Constant as u8,
            0,
            1,
            OpCode::Add as u8,
            OpCode::SetLocal as u8,
            0,
            1,
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
        f.chunk.code = vec![OpCode::GetLocal as u8, 0, 1, OpCode::Return as u8];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Int64);
    }

    #[test]
    fn slot_reassigned_to_value_merges_to_value() {
        // slot 1 set first to Int, then to Value → merge Value.
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(7)]);
        f.chunk.code = vec![
            OpCode::Constant as u8,
            0,
            0,
            OpCode::SetLocal as u8,
            0,
            1,
            OpCode::None as u8,
            OpCode::SetLocal as u8,
            0,
            1,
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
            OpCode::Constant as u8,
            0,
            0, // ip=0  push 1
            OpCode::SetLocal as u8,
            0,
            1, // ip=3  set slot 1
            OpCode::Jump as u8,
            0,
            2,                    // ip=6  jump +2 → ip=11
            OpCode::None as u8,   // ip=9  (dead)
            OpCode::None as u8,   // ip=10 (dead)
            OpCode::None as u8,   // ip=11
            OpCode::Return as u8, // ip=12
        ];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Int64);
    }

    // ── Realistic shape: the bench_loop pattern ──────────────────────

    #[test]
    fn loop_counter_stays_int() {
        // loop_sum(n). Slots: 0=closure, 1=n, 2=total, 3=i.
        let mut f = make_fn(
            "loop_sum",
            1,
            vec![],
            vec![Value::Integer(0), Value::Integer(1), Value::Integer(2)],
        );
        f.params[0].type_ann = Some("int".to_string());
        // Offsets are approximate; this test is about the fixed point converging on Int64 for slots 2/3.
        f.chunk.code = vec![
            // total := 0
            OpCode::Constant as u8,
            0,
            0, // 0..=2 push 0
            OpCode::SetLocal as u8,
            0,
            2, // 3..=5 slot 2 := 0
            // i := 1
            OpCode::Constant as u8,
            0,
            1, // 6..=8 push 1
            OpCode::SetLocal as u8,
            0,
            3, // 9..=11 slot 3 := 1
            // loop_start (ip=12): total := total + i * 2.
            OpCode::GetLocal as u8,
            0,
            2, // 12..=14 push total
            OpCode::GetLocal as u8,
            0,
            3, // 15..=17 push i
            OpCode::Constant as u8,
            0,
            2,                      // 18..=20 push 2
            OpCode::Multiply as u8, // 21 i*2
            OpCode::Add as u8,      // 22 total+(i*2)
            OpCode::SetLocal as u8,
            0,
            2, // 23..=25 slot 2 := result
            // i := i + 1
            OpCode::GetLocal as u8,
            0,
            3, // 26..=28
            OpCode::Constant as u8,
            0,
            1,                 // 29..=31 push 1
            OpCode::Add as u8, // 32
            OpCode::SetLocal as u8,
            0,
            3, // 33..=35
            // Return total
            OpCode::GetLocal as u8,
            0,
            2,                    // 36..=38
            OpCode::Return as u8, // 39
        ];
        let r = analyze(&f);
        assert_eq!(r.get(2), SlotType::Int64, "total should stay Int64");
        assert_eq!(r.get(3), SlotType::Int64, "i should stay Int64");
    }

    // B2.0 acceptance gate: compile each bench's hot-loop shape and assert the counter/accumulator are Int64.

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
                // The compiler embeds the raw Function as a constant since the closure isn't built yet.
                for c in &top.chunk.constants {
                    if let Some(closure) = c.as_closure()
                        && closure.function.name.as_deref() == Some(want)
                    {
                        return (*closure.function).clone();
                    }
                }
                // Some compiler versions embed the Function via a different path, so recurse into nested functions.
                for c in &top.chunk.constants {
                    if let Some(closure) = c.as_closure() {
                        for inner in &closure.function.chunk.constants {
                            if let Some(ic) = inner.as_closure()
                                && ic.function.name.as_deref() == Some(want)
                            {
                                return (*ic.function).clone();
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
        // An explicit <int> param is Int64 at entry, which is the B2.2 unboxed-calling-convention trigger.
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
        // Mirrors example/bench_loop.oxi, which does not annotate its parameter.
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
        // Current B2.0 limit: fib's unannotated `n` can't be proven int; invert this test once feedback vectors land.
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
        // With the annotation the analysis types fib's param, so B2.2 unboxes the call site.
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
            OpCode::None as u8, // closure
            OpCode::Constant as u8,
            0,
            0, // int arg
            OpCode::Call as u8,
            1, // call with 1 arg
            OpCode::SetLocal as u8,
            0,
            1, // slot 1 := result
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Value);
    }

    // ── B2.1a: new metadata tests ─────────────────────────────────────

    #[test]
    fn local_init_result_ip_recorded_for_constant_int_init() {
        // With arity=0 slot 1 is the first local, and the following None doesn't consume the top.
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(42)]);
        f.chunk.code = vec![
            OpCode::Constant as u8,
            0,
            0,                    // ip=0 push 42 → slot 1 init
            OpCode::None as u8,   // ip=3 push None
            OpCode::Return as u8, // ip=4
        ];
        let r = analyze(&f);
        assert_eq!(r.local_init_result_ip.get(&0), Some(&1));
        assert!(r.is_virtualizable(1));
    }

    #[test]
    fn local_init_result_ip_picks_first_when_reassigned() {
        // First init is the ip=0 Constant push, not the later SetLocal that only rewrites the slot.
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(1), Value::Integer(2)]);
        f.chunk.code = vec![
            OpCode::Constant as u8,
            0,
            0, // ip=0 push 1 → slot 1 init
            OpCode::Constant as u8,
            0,
            1, // ip=3 push 2 (temp at slot 2)
            OpCode::SetLocal as u8,
            0,
            1,                 // ip=6 slot 1 := 2
            OpCode::Pop as u8, // ip=9
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
        // Compile a real loop and check condition-cleanup pops are collected.
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
        // At least two: the body entry (fall-through) and the loop exit (branch target).
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
        // loop_sum has no nested closures, so captured_slots is empty.
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
            OpCode::Constant as u8,
            0,
            0, // ip=0: slot 1 init
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
        // Manufacture a local_init_result_ip pointing at a non-Constant opcode; certifier rule 5 catches it.
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(7)]);
        f.chunk.code = vec![
            OpCode::Constant as u8,
            0,
            0,                    // ip=0
            OpCode::None as u8,   // ip=3 (not a Constant)
            OpCode::Return as u8, // ip=4
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
        // Params are in init_sites but not local_init_result_ip: the caller initializes them.
        let mut f = make_fn("f", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![OpCode::GetLocal as u8, 0, 1, OpCode::Return as u8];
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
        // Minimal self-call to satisfy has_call; the specialized body is only useful as a direct-call target.
        let mut f = make_fn("f", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetLocal as u8,
            0,
            1, // push f (self)
            OpCode::Constant as u8,
            0,
            0, // push 0
            OpCode::Call as u8,
            1,                 // call (synthetic arity 1)
            OpCode::Pop as u8, // discard result
            OpCode::GetLocal as u8,
            0,
            1, // push n
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
        let f = make_fn(
            "f",
            0,
            vec![OpCode::Constant as u8, 0, 0, OpCode::Return as u8],
            vec![Value::Integer(42)],
        );
        let r = analyze(&f);
        assert!(!r.specialized_entry_eligible);
        assert!(r.specialized_param_slots.is_empty());
    }

    #[test]
    fn specialized_eligible_even_with_value_return() {
        // String return is still eligible: the Return path bails via status 2 on non-Integer at runtime.
        let mut f = make_fn("f", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetLocal as u8,
            0,
            1,
            OpCode::Constant as u8,
            0,
            0,
            OpCode::Call as u8,
            1,
            OpCode::Pop as u8,
            OpCode::Constant as u8,
            0,
            1, // push "hello"
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
        // Unannotated `n` with no int-demand use, so int_mirror skips it and the param isn't Int-stable.
        let mut f = make_fn("f", 1, vec![], vec![]);
        // no type_ann — defaults to Value
        f.chunk.code = vec![OpCode::GetLocal as u8, 0, 1, OpCode::Return as u8];
        let r = analyze(&f);
        assert!(!r.specialized_entry_eligible);
    }

    #[test]
    fn specialized_eligible_int_param_with_arith_and_return() {
        // has_call satisfied, arithmetic return eligible.
        let mut f = make_fn("f", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetLocal as u8,
            0,
            1,
            OpCode::Constant as u8,
            0,
            0,
            OpCode::Call as u8,
            1,
            OpCode::Pop as u8,
            OpCode::GetLocal as u8,
            0,
            1,
            OpCode::Constant as u8,
            0,
            1,
            OpCode::Add as u8,
            OpCode::Return as u8,
        ];
        f.chunk.constants = vec![Value::Integer(0), Value::Integer(1)];
        let r = analyze(&f);
        assert!(r.specialized_entry_eligible);
        assert_eq!(r.specialized_param_slots, vec![1]);
        // No GetUpvalue, so plain NativeIntBody rather than the closure-aware variant.
        assert!(!r.wants_closure_arg);
    }

    // ── B2.2: closure-aware specialized entry eligibility ──────────────

    #[test]
    fn specialized_eligible_closure_with_int_upvalue() {
        // The closure reads upvalue 0 and adds its int param: eligible and wants_closure_arg.
        let mut f = make_fn("closure", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetLocal as u8, 0, 1,    // push self (for synthetic call)
            OpCode::Constant as u8, 0, 0,
            OpCode::Call as u8, 1,
            OpCode::Pop as u8,
            OpCode::GetUpvalue as u8, 0, 0,  // push upvalue 0 (x)
            OpCode::GetLocal as u8, 0, 1,    // push y (param)
            OpCode::Add as u8,
            OpCode::Return as u8,
        ];
        f.chunk.constants = vec![Value::Integer(0)];
        let r = analyze(&f);
        assert!(r.specialized_entry_eligible);
        assert!(r.wants_closure_arg);
        assert_eq!(r.specialized_param_slots, vec![1]);
    }

    #[test]
    fn specialized_ineligible_closure_with_set_upvalue() {
        // SetUpvalue is rejected; write-through is out of scope for v1.
        let mut f = make_fn("mut_closure", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetLocal as u8, 0, 1,
            OpCode::Constant as u8, 0, 0,
            OpCode::Call as u8, 1,
            OpCode::Pop as u8,
            OpCode::GetLocal as u8, 0, 1,
            OpCode::SetUpvalue as u8, 0, 0,  // mutate upvalue 0
            OpCode::Pop as u8,
            OpCode::GetLocal as u8, 0, 1,
            OpCode::Return as u8,
        ];
        f.chunk.constants = vec![Value::Integer(0)];
        let r = analyze(&f);
        assert!(!r.specialized_entry_eligible);
        assert!(matches!(
            r.specialized_entry_outcome,
            SpecEligibilityOutcome::RejectedHasUpvalueOp
        ));
        assert!(!r.wants_closure_arg);
    }

    #[test]
    fn specialized_ineligible_closure_with_close_upvalue() {
        // CloseUpvalue is rejected: the v1 specialized return path never runs close_upvalues.
        let mut f = make_fn("closing", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetLocal as u8, 0, 1,
            OpCode::Constant as u8, 0, 0,
            OpCode::Call as u8, 1,
            OpCode::Pop as u8,
            // A real CloseUpvalue has the captured local on top and pops it; keep this bytecode stack-valid.
            OpCode::Constant as u8, 0, 0,
            OpCode::CloseUpvalue as u8,
            OpCode::GetLocal as u8, 0, 1,
            OpCode::Return as u8,
        ];
        f.chunk.constants = vec![Value::Integer(0)];
        let r = analyze(&f);
        assert!(!r.specialized_entry_eligible);
        assert!(matches!(
            r.specialized_entry_outcome,
            SpecEligibilityOutcome::RejectedHasUpvalueOp
        ));
        assert!(!r.wants_closure_arg);
    }

    #[test]
    fn captured_local_loop_analysis_converges() {
        // If CloseUpvalue were stack-neutral the abstract stack would grow per pass and this test would hang.
        let src = r#"
            fun f() {
                out := []
                each i in [1, 2] {
                    x := i
                    g := fun() { x }
                    out = push(out, g)
                }
                out
            }
            f()
        "#;
        let f = compile_for_analysis(src, Some("f"));
        let result = analyze(&f);
        assert!(
            !result.captured_slots.is_empty(),
            "the nested closure should mark its captured local"
        );
        assert!(certify(&f, &result).is_ok());
    }

    #[test]
    fn specialized_eligible_closure_no_call_with_upvalue() {
        // The bench_closure inner-closure shape: no Call, but closure-aware dispatch still reaches it.
        let mut f = make_fn("inner_closure", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetUpvalue as u8, 0, 0,  // push upvalue 0 (x)
            OpCode::GetLocal as u8, 0, 1,    // push y (param)
            OpCode::Add as u8,
            OpCode::Return as u8,
        ];
        f.chunk.constants = vec![];
        let r = analyze(&f);
        assert!(r.specialized_entry_eligible);
        assert!(r.wants_closure_arg);
        assert_eq!(r.specialized_param_slots, vec![1]);
    }

    #[test]
    fn specialized_ineligible_no_call_no_upvalue() {
        // No Call and no GetUpvalue means no path reaches a specialized entry, so reject as RejectedNoCall.
        let mut f = make_fn("identity", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetLocal as u8, 0, 1,
            OpCode::Return as u8,
        ];
        f.chunk.constants = vec![];
        let r = analyze(&f);
        assert!(!r.specialized_entry_eligible);
        assert!(matches!(
            r.specialized_entry_outcome,
            SpecEligibilityOutcome::RejectedNoCall
        ));
        assert!(!r.wants_closure_arg);
    }

    // OpCode::Closure always rejects first, so the nested-closure case never reaches has_get_upvalue.
}
