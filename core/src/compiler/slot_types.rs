//! Typed slot analysis.
//!
//! For each JIT-compilable function this pass produces a
//! `FunctionSlotTypes` map that classifies each local slot as one of:
//!
//!   - `Int64` — provably always an `i64` in this function
//!   - `Value` — generic fallback (anything else, or not provably an int)
//!
//! The pass is forward abstract interpretation on the bytecode with a
//! 3-element lattice (`⊥`, `Int64`, `Value`). When in doubt, a slot is `Value`.
//! Codegen consumes the output and emits native `i64` SSA for the `Int64` slots
//! while leaving the `Value` slots on the generic stack path.
//!
//! Only local slots are classified. Booleans, floats, and other non-`Int64`
//! scalars collapse to `Value`. Control flow is handled via a worklist: at each
//! control-flow merge the incoming lattices are joined, and if the merged state
//! changes the block's successors are re-queued.

use std::collections::{HashMap, HashSet};

use crate::compiler::opcode::{Chunk, OpCode};
use crate::vm::value::Function;
#[cfg(test)]
use crate::vm::value::Value;

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

/// Per-function classification of why specialized-entry eligibility
/// passed or failed. Each variant is the first rule that rejected the function
/// (in the order of `compute_specialized_entry_eligibility`); a function that
/// passes every rule is `Eligible`.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SpecEligibilityOutcome {
    Eligible,
    /// Function takes no args.
    RejectedZeroArity,
    /// At least one param is neither Int64-typed nor an int-mirror
    /// candidate.
    RejectedParamNotInt,
    /// At least one param is captured by an upvalue.
    RejectedParamCaptured,
    /// Body contains a `Closure` opcode (creates a new closure).
    RejectedHasClosureOp,
    /// Body contains `GetUpvalue` / `SetUpvalue` / `CloseUpvalue`.
    RejectedHasUpvalueOp,
    /// Function has no `Return` opcode (e.g., always errors out or
    /// loops forever).
    RejectedNoReturn,
    /// Function has no `Call` opcode.
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

/// Per-slot classification for one JIT-compiled function, plus the
/// bytecode metadata that codegen needs.
///
/// Indexed by local slot number. A slot that was never assigned stays
/// `Bottom` (callers can treat this as `Value`; the slot is unused).
#[derive(Debug, Clone)]
pub struct FunctionSlotTypes {
    /// Per-slot classification. `slots[N]` is the join of every type
    /// ever written to slot N during the function.
    pub slots: Vec<SlotType>,

    /// IP → slot. "The opcode at this IP first initializes this local
    /// slot." The map is general (the initializer may be any expression-producing
    /// opcode). Param slots are not in this map; they are initialized at function
    /// entry. See `init_sites` for the unified "is slot initialized somewhere?"
    /// question.
    pub local_init_result_ip: HashMap<usize, u16>,

    /// All slots that are known to be initialized somewhere — either at
    /// function entry (params, slots 1..=arity) or at a specific bytecode IP
    /// (values of `local_init_result_ip`).
    pub init_sites: HashSet<u16>,

    /// IPs of `Pop` opcodes that exist solely to remove a conditional
    /// branch's condition value from the stack. These are the `Pop`s
    /// emitted immediately after a JumpIfFalse / JumpIfTrue / Unless
    /// fallthrough, and at the branch target.
    pub condition_cleanup_pop_ips: HashSet<usize>,

    /// IP → slot. "The opcode at this IP pops a local slot at end-of-
    /// scope."
    pub scope_pop_slot_ip: HashMap<usize, u16>,

    /// Slots referenced as `is_local = 1` in any nested `Closure`
    /// opcode's upvalue descriptors (i.e., captured by a closure
    /// defined inside this function).
    pub captured_slots: HashSet<u16>,

    /// Param slots eligible for an Int64 register mirror.
    /// Selection criteria:
    ///   - In the param range (1..=arity).
    ///   - Classified `Value`.
    ///   - Never written (no `SetLocal` targeting this slot).
    ///   - Not captured.
    ///   - Used by at least one `GetLocal` in the function body.
    pub int_mirror_param_slots: HashSet<u16>,

    /// True iff the function qualifies for a second, specialized
    /// calling-convention entry point `fn(*mut VM, i64, ..., i64) -> (i64, u32)`
    /// with args passed in registers and result returned as raw i64.
    /// See `collect_specialized_entry_eligibility` for the full rule set.
    pub specialized_entry_eligible: bool,

    /// Per-rejection reason for `specialized_entry_eligible`.
    /// `Eligible` when the function qualifies; otherwise the first rule
    /// that rejected it.
    pub specialized_entry_outcome: SpecEligibilityOutcome,

    /// Param slots that the specialized entry receives as i64
    /// (1..=arity when eligible; empty otherwise). Distinct from
    /// `int_mirror_param_slots`: specialized slots include both the
    /// read-only Int-demand Value-typed params and the Int64-typed
    /// params (whether writable or not).
    pub specialized_param_slots: Vec<u16>,

    /// True iff the function is `specialized_entry_eligible` and
    /// its body contains at least one `GetUpvalue`, so the specialized entry
    /// takes a closure pointer in a register
    /// (`SpecializedEntryKind::NativeIntBodyWithClosure`).
    pub wants_closure_arg: bool,

    /// IPs of `OpCode::TypeWrap` whose runtime effect is provably
    /// identity: target constant is the string `"INTEGER"` and the
    /// abstract stack top at entry is `Int64`.
    pub noop_type_wrap_ips: std::collections::HashSet<usize>,

    /// IPs of `OpCode::CheckReturn` that provably pass: the abstract stack
    /// top is `Int64` and the declared return type accepts an int.
    pub noop_check_return_ips: std::collections::HashSet<usize>,

    /// True iff this chunk contains a `GetGlobal` naming this very function.
    ///
    /// A top-level named function reaches itself only through `GetGlobal`. When
    /// this is false the function is anonymous or reaches itself through an
    /// upvalue.
    pub self_reachable_by_global_name: bool,

    /// Whether the function's whole body stays inside the integer world, so
    /// a lean entry could run it without materializing VM state.
    ///
    /// Requires every reachable operation to be an integer constant, a
    /// virtualizable integer local, wrapping integer arithmetic, an integer
    /// comparison or branch, or a return, with no captured slot, no upvalue,
    /// and no heap-bearing constant.
    ///
    /// `Call` is permitted only where the callee can be the function itself
    /// (`self_reachable_by_global_name`) with matching arity.
    ///
    /// This is an eligibility gate only; emitting a lean entry also needs a
    /// per-bailout reconstruction description.
    pub lean_entry_eligible: bool,
}

impl FunctionSlotTypes {
    /// Return the inferred type for slot `i`, or `Value` if the index
    /// is out of range.
    pub fn get(&self, i: usize) -> SlotType {
        self.slots.get(i).copied().unwrap_or(SlotType::Value)
    }

    /// Count of slots classified as `Int64`.
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

    /// Whether slot `i` is eligible for virtualization into a native register
    /// variable.
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

/// The analysis state at a program point. Oxigen's locals are positions on
/// the VM stack: slot N is `stack[N]`. Temporaries (loop conditions,
/// arithmetic intermediates) live at positions above `num_slots`.
///
/// Two things are tracked:
///   - `stack`: the per-position abstract type at this IP.
///   - `slot_types`: the slot's classification, updated only on writes
///     that target a local position (initialization pushes, explicit
///     `SetLocal`, `Increment`/`Decrement`).
#[derive(Clone, Debug, PartialEq, Eq)]
struct AbstractState {
    stack: Vec<SlotType>,
    slot_types: Vec<SlotType>,
    /// Positions holding a declared local that is still in scope: set at its
    /// declaration point, cleared when a `Pop` removes it. Joined with OR.
    live_locals: Vec<bool>,
}

impl AbstractState {
    fn new(num_slots: usize) -> Self {
        AbstractState {
            stack: Vec::new(),
            slot_types: vec![SlotType::Bottom; num_slots],
            live_locals: vec![false; num_slots],
        }
    }

    /// Pointwise join of both stack and slot_types. Pads shorter
    /// sides with `Bottom`.
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

        if self.live_locals.len() < other.live_locals.len() {
            self.live_locals.resize(other.live_locals.len(), false);
            changed = true;
        }
        for (mine, theirs) in self.live_locals.iter_mut().zip(&other.live_locals) {
            if *theirs && !*mine {
                *mine = true;
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

/// Map each local's initializing `Constant` to its slot, from the compiler's
/// declaration points.
///
/// A declaration is a simple init when the instruction ending at its
/// declaration point is a `Constant` that left the value exactly at the slot,
/// and no forward jump also arrives there.
///
/// The returned `init_sites` holds the params plus every slot whose
/// declarations are all simple inits. A slot with any other declaration, or
/// whose local was consumed straight from the stack while still in scope, is
/// left out.
fn collect_local_inits(
    func: &Function,
    code: &[u8],
    chunk: &Chunk,
    states: &HashMap<usize, AbstractState>,
    forward_targets: &HashSet<usize>,
    consumed_live: &HashSet<u16>,
) -> (HashMap<usize, u16>, HashSet<u16>) {
    let mut start_of_end: HashMap<usize, usize> = HashMap::new();
    let mut ip = 0;
    while ip < code.len() {
        let Ok(len) = chunk.instruction_len(ip) else {
            break;
        };
        start_of_end.insert(ip + len, ip);
        ip += len;
    }

    let mut local_init_result_ip: HashMap<usize, u16> = HashMap::new();
    let mut complex_slots: HashSet<u16> = HashSet::new();
    let mut simple_slots: HashSet<u16> = HashSet::new();
    for &(decl_ip, slot) in &func.local_decls {
        if (1..=func.arity as u16).contains(&slot) {
            continue;
        }
        let last = start_of_end.get(&decl_ip).copied();
        let producer = match last {
            Some(p) if OpCode::from_byte(code[p]) == Some(OpCode::TypeWrap) => {
                let wraps_to_int = chunk
                    .constants
                    .get(read_u16(code, p + 1) as usize)
                    .and_then(|v| v.as_string())
                    .is_some_and(|t| t.as_str() == "INTEGER");
                start_of_end.get(&p).copied().filter(|&c| {
                    wraps_to_int
                        && !forward_targets.contains(&p)
                        && OpCode::from_byte(code[c]) == Some(OpCode::Constant)
                        && matches!(
                            chunk.constants.get(read_u16(code, c + 1) as usize),
                            Some(crate::vm::value::Value::Integer(_))
                        )
                })
            }
            other => other,
        };
        let init_ip = producer.filter(|&p| {
            matches!(
                OpCode::from_byte(code[p]),
                Some(OpCode::Constant | OpCode::RangeLimit)
            ) && !forward_targets.contains(&decl_ip)
                && states
                    .get(&decl_ip)
                    .is_some_and(|st| st.stack.len() == slot as usize + 1)
        });
        match init_ip {
            Some(p) => {
                local_init_result_ip.insert(p, slot);
                simple_slots.insert(slot);
            }
            None => {
                complex_slots.insert(slot);
            }
        }
    }

    let mut init_sites: HashSet<u16> = (1..=func.arity as u16).collect();
    init_sites.extend(
        simple_slots
            .difference(&complex_slots)
            .filter(|slot| !consumed_live.contains(slot))
            .copied(),
    );
    (local_init_result_ip, init_sites)
}

/// Run the analysis on one function and return its slot classifications.
///
/// `num_slots` is max(locals_len, arity + 1). Param slots (1..=arity)
/// are typed from `params[*].type_ann`. Slot 0 is the closure marker.
/// Additional slots beyond params are populated by the body's
/// stack-push initializers (`Constant`, arithmetic, `GetLocal`).
///
/// For each stack position the abstract type is tracked across all
/// reachable program points and the join is reported as the slot's
/// classification.
pub fn analyze(func: &Function) -> FunctionSlotTypes {
    let num_slots = func.locals.len().max(func.arity as usize + 1);
    let chunk = &func.chunk;
    let code = &chunk.code;

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

    let mut forward_targets: HashSet<usize> = HashSet::new();
    let mut decls_at: HashMap<usize, Vec<u16>> = HashMap::new();
    for &(ip, slot) in &func.local_decls {
        decls_at.entry(ip).or_default().push(slot);
    }
    let mut consumed_live: HashSet<u16> = HashSet::new();
    let mut scope_pop_slot_ip: HashMap<usize, u16> = HashMap::new();

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
            if let Some(slots) = decls_at.get(&cursor) {
                for &slot in slots {
                    if let Some(live) = state.live_locals.get_mut(slot as usize) {
                        *live = true;
                    }
                }
            }
            let depth_before = state.stack.len();
            let (mut next_state, terminates, targets) = transfer(op, cursor, code, chunk, &state);
            let fallthrough = cursor
                + chunk
                    .instruction_len(cursor)
                    .expect("compiler produced malformed bytecode");

            let depth_after = next_state.stack.len();

            if depth_after < depth_before {
                for pos in depth_after..depth_before.min(num_slots) {
                    if next_state.live_locals.get(pos).copied().unwrap_or(false) {
                        if !matches!(op, OpCode::Pop | OpCode::CloseUpvalue | OpCode::Return) {
                            consumed_live.insert(pos as u16);
                        }
                        next_state.live_locals[pos] = false;
                    }
                }
            }

            if matches!(op, OpCode::Constant) && depth_after == depth_before + 1 {
                let new_pos = depth_after - 1;
                if new_pos < num_slots {
                    let ty = next_state.stack.last().copied().unwrap_or(SlotType::Bottom);
                    next_state.write_slot(new_pos, ty);
                }
            }

            if matches!(op, OpCode::RangeLimit) && depth_after >= 1 {
                let new_pos = depth_after - 1;
                if new_pos < num_slots {
                    next_state.write_slot(new_pos, SlotType::Int64);
                }
            }

            if depth_after >= 1 && !matches!(op, OpCode::Constant | OpCode::RangeLimit) {
                let new_pos = depth_after - 1;
                if new_pos < num_slots {
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

            if matches!(op, OpCode::Pop) && depth_before > 0 {
                let popped_pos = depth_before - 1;
                if popped_pos < num_slots {
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

            for tgt in &targets {
                if *tgt > cursor {
                    forward_targets.insert(*tgt);
                }
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

    let mut result = vec![SlotType::Bottom; num_slots];
    for state in states.values() {
        for (i, slot) in result.iter_mut().enumerate() {
            let ty = state.slot_types.get(i).copied().unwrap_or(SlotType::Bottom);
            *slot = slot.join(ty);
        }
    }

    let (local_init_result_ip, init_sites) =
        collect_local_inits(func, code, chunk, &states, &forward_targets, &consumed_live);

    let condition_cleanup_pop_ips = collect_condition_cleanup_pops(code, chunk);

    let captured_slots = collect_captured_slots(code, chunk);

    let int_mirror_param_slots = collect_int_mirror_param_slots(
        code,
        chunk,
        func.arity as u16,
        &captured_slots,
        &result,
        &func.params,
    );

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
        if captured_slots.is_empty() && top == SlotType::Int64 {
            noop_type_wrap_ips.insert(ip);
        }
    }

    let mut noop_check_return_ips: HashSet<usize> = HashSet::new();
    for (&ip, st) in eligibility_states.iter() {
        if ip + 2 >= code.len() || OpCode::from_byte(code[ip]) != Some(OpCode::CheckReturn) {
            continue;
        }
        let top = st.stack.last().copied().unwrap_or(SlotType::Value);
        if captured_slots.is_empty()
            && top == SlotType::Int64
            && check_return_accepts_int(chunk, read_u16(code, ip + 1))
        {
            noop_check_return_ips.insert(ip);
        }
    }

    let self_reachable_by_global_name = chunk_reads_global_named(chunk, func.name.as_deref());
    let lean_entry_eligible = compute_lean_entry_eligible(
        func,
        &result,
        &captured_slots,
        self_reachable_by_global_name,
    );

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
        noop_check_return_ips,
        self_reachable_by_global_name,
        lean_entry_eligible,
    }
}

/// Second-pass abstract interpretation with a subset of param slots
/// lifted from `Value` to `Int64` in the initial entry state.
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
/// `outcome` carries the specific rule that rejected the function.
fn compute_specialized_entry_eligibility(
    code: &[u8],
    chunk: &Chunk,
    arity: u16,
    slot_types: &[SlotType],
    captured_slots: &HashSet<u16>,
    int_mirror_param_slots: &HashSet<u16>,
    states: &HashMap<usize, AbstractState>,
) -> (SpecEligibilityOutcome, Vec<u16>, bool) {
    if arity == 0 {
        return (SpecEligibilityOutcome::RejectedZeroArity, Vec::new(), false);
    }

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
            return (
                SpecEligibilityOutcome::RejectedParamNotInt,
                Vec::new(),
                false,
            );
        }
        if captured_slots.contains(&slot) {
            return (
                SpecEligibilityOutcome::RejectedParamCaptured,
                Vec::new(),
                false,
            );
        }
    }

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
                return (
                    SpecEligibilityOutcome::RejectedHasClosureOp,
                    Vec::new(),
                    false,
                );
            }
            OpCode::SetUpvalue | OpCode::CloseUpvalue => {
                return (
                    SpecEligibilityOutcome::RejectedHasUpvalueOp,
                    Vec::new(),
                    false,
                );
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
    if !has_call && !has_get_upvalue {
        return (SpecEligibilityOutcome::RejectedNoCall, Vec::new(), false);
    }

    for &rip in &return_ips {
        let state = match states.get(&rip) {
            Some(s) => s,
            None => {
                return (
                    SpecEligibilityOutcome::RejectedReturnUnreachable,
                    Vec::new(),
                    false,
                );
            }
        };
        let top_ty = state.stack.last().copied().unwrap_or(SlotType::Bottom);
        if matches!(top_ty, SlotType::Bottom) {
            return (
                SpecEligibilityOutcome::RejectedReturnUnreachable,
                Vec::new(),
                false,
            );
        }
    }

    let param_slots: Vec<u16> = (1..=arity).collect();
    (
        SpecEligibilityOutcome::Eligible,
        param_slots,
        has_get_upvalue,
    )
}

/// Walk the bytecode and pick out param slots that qualify for an
/// Int64 register mirror. See the field doc on
/// `FunctionSlotTypes::int_mirror_param_slots` for the criteria.
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
                let mut cursor = ip + op_len;
                let mut skipped = 0;
                let classifier_op = loop {
                    if skipped > 4 || cursor >= code.len() {
                        break None;
                    }
                    let cop = OpCode::from_byte(code[cursor]);
                    match cop {
                        Some(OpCode::Constant) => {
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
                        | OpCode::IndexExisting
                        | OpCode::IndexAssign
                        | OpCode::Lock
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
                    _ => {}
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
        if params
            .get(slot as usize - 1)
            .and_then(|p| p.type_ann.as_deref())
            .is_some_and(|t| t.eq_ignore_ascii_case("float"))
        {
            continue;
        }
        if !has_int_demand.contains(&slot) {
            continue;
        }
        if has_non_int_demand.contains(&slot) {
            continue;
        }
        if captured_slots.contains(&slot) {
            continue;
        }
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
/// If either IP isn't a `Pop`, neither is recorded.
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
///     inner `Function.upvalue_count` gives the number of descriptors)
///   - N descriptors of (u8 is_local, u16 index) each
///
/// If the constant cannot be parsed (not a closure, or the function
/// reference is missing), the result is an empty set.
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
                _ => 0,
            };
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
            ip = desc_start + upvalue_count * 3;
        } else {
            ip += chunk
                .instruction_len(ip)
                .expect("compiler produced malformed bytecode");
        }
    }
    out
}

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
        OpCode::Constant => {
            let idx = read_u16(code, ip + 1) as usize;
            let ty = if chunk
                .constants
                .get(idx)
                .and_then(|v| v.as_integer())
                .is_some()
            {
                SlotType::Int64
            } else {
                SlotType::Value
            };
            next.stack.push(ty);
        }

        OpCode::None | OpCode::True | OpCode::False => {
            next.stack.push(SlotType::Value);
        }

        OpCode::Pop => {
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

        OpCode::RangeLimit => {
            next.stack.pop();
            next.stack.pop();
            next.stack.push(SlotType::Int64);
        }

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
        OpCode::CloseUpvalue => {
            next.stack.pop();
        }
        OpCode::CloseUpvalueAt => {}

        OpCode::Jump => {
            let off = read_u16(code, ip + 1) as usize;
            targets.push(ip + 3 + off);
            terminates = true;
        }
        OpCode::Loop => {
            let off = read_u16(code, ip + 1) as usize;
            let base = ip + 3;
            if off <= base {
                targets.push(base - off);
            }
            terminates = true;
        }
        OpCode::JumpIfFalse | OpCode::JumpIfTrue => {
            let off = read_u16(code, ip + 1) as usize;
            targets.push(ip + 3 + off);
        }
        OpCode::PopJumpIfFalse => {
            let off = read_u16(code, ip + 1) as usize;
            next.stack.pop();
            targets.push(ip + 3 + off);
        }
        OpCode::Unless => {
            let off = read_u16(code, ip + 1) as usize;
            next.stack.pop();
            targets.push(ip + 3 + off);
        }

        OpCode::Return => {
            next.stack.pop();
            terminates = true;
        }

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

        OpCode::Closure => {
            next.stack.push(SlotType::Value);
        }

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
        OpCode::Index | OpCode::IndexExisting => {
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
            if matches!(op, OpCode::DefineMethod) {
                let count = code[ip + 3] as usize;
                for _ in 0..(2 * count) {
                    next.stack.pop();
                }
            }
        }
        OpCode::StructLiteral => {
            let count = code[ip + 3] as usize;
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
            let off = read_u16(code, ip + 3) as usize;
            targets.push(ip + 5 + off);
        }
        OpCode::Fail => {
            next.stack.pop();
            terminates = true;
        }

        OpCode::Import => {
            let has_selective = code[ip + 3] != 0;
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
            let off = read_u16(code, ip + 1) as usize;
            targets.push(ip + 3 + off);
        }
        OpCode::IterLen | OpCode::IterGet | OpCode::IterEntry => {
            next.stack.pop();
            if matches!(op, OpCode::IterGet | OpCode::IterEntry) {
                next.stack.pop();
            }
            next.stack.push(SlotType::Value);
            if matches!(op, OpCode::IterEntry) {
                next.stack.push(SlotType::Value);
            }
        }
        OpCode::Lock => {
            next.stack.pop();
            next.stack.push(SlotType::Value);
        }
        OpCode::TypeWrap => {
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
        OpCode::DefinePattern => {}
        OpCode::TestPattern => {
            next.stack.push(SlotType::Value);
        }
        OpCode::PushHandler | OpCode::PopHandler | OpCode::CheckType | OpCode::SetLocalLocked => {}
        OpCode::CheckReturn => {}
    }

    (next, terminates, targets)
}

#[inline]
fn read_u16(code: &[u8], offset: usize) -> u16 {
    ((code[offset] as u16) << 8) | (code[offset + 1] as u16)
}

/// Whether every reachable operation stays inside the integer world.
fn compute_lean_entry_eligible(
    func: &Function,
    _slots: &[SlotType],
    captured: &HashSet<u16>,
    self_reachable: bool,
) -> bool {
    if func.arity == 0 {
        return false;
    }
    if !captured.is_empty() {
        return false;
    }

    let chunk = &func.chunk;
    let code = &chunk.code;
    let mut ip = 0usize;
    while ip < code.len() {
        let Some(op) = OpCode::from_byte(code[ip]) else {
            return false;
        };
        let Ok(len) = chunk.instruction_len(ip) else {
            return false;
        };
        match op {
            OpCode::Constant => {
                let idx = read_u16(code, ip + 1) as usize;
                match chunk.constants.get(idx).map(|v| v.repr()) {
                    Some(crate::vm::value::ValueRepr::Integer(_)) => {}
                    _ => return false,
                }
            }
            OpCode::GetLocal | OpCode::SetLocal => {}
            OpCode::Add
            | OpCode::Subtract
            | OpCode::Multiply
            | OpCode::Less
            | OpCode::LessEqual
            | OpCode::Greater
            | OpCode::GreaterEqual
            | OpCode::Equal
            | OpCode::NotEqual
            | OpCode::Jump
            | OpCode::JumpIfFalse
            | OpCode::JumpIfTrue
            | OpCode::Pop
            | OpCode::Return => {}
            OpCode::CheckReturn => {
                if !check_return_accepts_int(chunk, read_u16(code, ip + 1)) {
                    return false;
                }
            }
            OpCode::Call => {
                if !self_reachable || code[ip + 1] != func.arity {
                    return false;
                }
            }
            OpCode::GetGlobal => {
                if !self_reachable {
                    return false;
                }
                let idx = read_u16(code, ip + 1) as usize;
                let names_self = chunk
                    .constants
                    .get(idx)
                    .and_then(|v| v.as_string())
                    .zip(func.name.as_deref())
                    .is_some_and(|(g, own)| g.as_str() == own);
                if !names_self {
                    return false;
                }
            }
            _ => return false,
        }
        ip += len.max(1);
    }
    lean_value_kinds_ok(func)
}

/// Whether a `CheckReturn` whose type-name constant is at `idx` accepts an int.
fn check_return_accepts_int(chunk: &crate::compiler::opcode::Chunk, idx: u16) -> bool {
    chunk
        .constants
        .get(idx as usize)
        .and_then(|v| v.as_string())
        .is_some_and(|s| s.split(" || ").any(|t| t == "INTEGER" || t == "GENERIC"))
}

/// What a lean entry's raw `i64` stands for at one stack position.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum LeanKind {
    Int,
    /// A comparison result: 0 or 1 in the lean entry, but a BOOLEAN to the
    /// interpreter.
    Bool,
    /// The function value a self-call pops, or anything the walk cannot name.
    Other,
}

/// Whether the lean entry, which carries every value as a raw `i64`, computes
/// exactly what the interpreter does.
///
/// This holds as long as comparison results only steer branches. Walks the
/// bytecode with a kind per stack position (locals are stack positions), and
/// refuses on any other use of a comparison result or on a merge it cannot
/// reconcile.
fn lean_value_kinds_ok(func: &Function) -> bool {
    let chunk = &func.chunk;
    let code = &chunk.code;
    let mut entry: Vec<LeanKind> = vec![LeanKind::Other];
    entry.extend(std::iter::repeat_n(LeanKind::Int, func.arity as usize));

    let mut states: HashMap<usize, Vec<LeanKind>> = HashMap::new();
    states.insert(0, entry);
    let mut worklist = vec![0usize];
    let merge = |states: &mut HashMap<usize, Vec<LeanKind>>,
                 worklist: &mut Vec<usize>,
                 at: usize,
                 st: &Vec<LeanKind>|
     -> bool {
        match states.get_mut(&at) {
            None => {
                states.insert(at, st.clone());
                worklist.push(at);
                true
            }
            Some(existing) => {
                if existing.len() != st.len() {
                    return false;
                }
                let mut changed = false;
                for (e, n) in existing.iter_mut().zip(st) {
                    if *e != *n && *e != LeanKind::Other {
                        *e = LeanKind::Other;
                        changed = true;
                    }
                }
                if changed {
                    worklist.push(at);
                }
                true
            }
        }
    };

    while let Some(ip) = worklist.pop() {
        let mut st = states[&ip].clone();
        let Some(op) = code.get(ip).and_then(|&b| OpCode::from_byte(b)) else {
            return false;
        };
        let Ok(len) = chunk.instruction_len(ip) else {
            return false;
        };
        let next = ip + len;
        let pop_ints = |st: &mut Vec<LeanKind>, n: usize| -> bool {
            (0..n).all(|_| st.pop() == Some(LeanKind::Int))
        };
        match op {
            OpCode::Constant => st.push(LeanKind::Int),
            OpCode::GetLocal => {
                let slot = read_u16(code, ip + 1) as usize;
                match st.get(slot).copied() {
                    Some(k) => st.push(k),
                    None => return false,
                }
            }
            OpCode::SetLocal => {
                let slot = read_u16(code, ip + 1) as usize;
                match (st.last().copied(), slot < st.len()) {
                    (Some(k), true) => st[slot] = k,
                    _ => return false,
                }
            }
            OpCode::Add | OpCode::Subtract | OpCode::Multiply => {
                if !pop_ints(&mut st, 2) {
                    return false;
                }
                st.push(LeanKind::Int);
            }
            OpCode::Less
            | OpCode::LessEqual
            | OpCode::Greater
            | OpCode::GreaterEqual
            | OpCode::Equal
            | OpCode::NotEqual => {
                if !pop_ints(&mut st, 2) {
                    return false;
                }
                st.push(LeanKind::Bool);
            }
            OpCode::Pop => {
                if st.pop().is_none() {
                    return false;
                }
            }
            OpCode::GetGlobal => st.push(LeanKind::Other),
            OpCode::Call => {
                let argc = code[ip + 1] as usize;
                if !pop_ints(&mut st, argc) || st.pop().is_none() {
                    return false;
                }
                st.push(LeanKind::Int);
            }
            OpCode::Return => {
                if st.last() != Some(&LeanKind::Int) {
                    return false;
                }
                continue;
            }
            OpCode::CheckReturn => {
                if st.last() != Some(&LeanKind::Int) {
                    return false;
                }
            }
            OpCode::Jump => {
                let target = ip + 3 + read_u16(code, ip + 1) as usize;
                if !merge(&mut states, &mut worklist, target, &st) {
                    return false;
                }
                continue;
            }
            OpCode::JumpIfFalse | OpCode::JumpIfTrue => {
                if !matches!(st.last(), Some(LeanKind::Int) | Some(LeanKind::Bool)) {
                    return false;
                }
                let target = ip + 3 + read_u16(code, ip + 1) as usize;
                if !merge(&mut states, &mut worklist, target, &st) {
                    return false;
                }
            }
            _ => return false,
        }
        if next >= code.len() || !merge(&mut states, &mut worklist, next, &st) {
            return false;
        }
    }
    true
}

/// True iff `chunk` contains a `GetGlobal` whose name constant is `name`.
///
/// An undecodable chunk answers `true`.
fn chunk_reads_global_named(chunk: &Chunk, name: Option<&str>) -> bool {
    let Some(name) = name else {
        return false;
    };
    let code = &chunk.code;
    let mut ip = 0usize;
    while ip < code.len() {
        let Ok(len) = chunk.instruction_len(ip) else {
            return true;
        };
        if matches!(OpCode::from_byte(code[ip]), Some(OpCode::GetGlobal)) && ip + 2 < code.len() {
            let idx = read_u16(code, ip + 1) as usize;
            if chunk
                .constants
                .get(idx)
                .and_then(|v| v.as_string())
                .is_some_and(|s| s.as_str() == name)
            {
                return true;
            }
        }
        ip += len.max(1);
    }
    false
}

/// Structured result of the certifier: `Ok` or a descriptive error string.
pub type CertifyResult = Result<(), String>;

/// Debug-only invariant checks across a function's bytecode and its
/// computed `FunctionSlotTypes`.
///
/// Invariants verified:
///
/// 1. **No captured slot is virtualizable.** If a slot is both Int64-classified
///    and captured, `is_virtualizable` must return false.
///
/// 2. **Every virtualizable slot has a recognized initializer IP.**
///
/// 3. **Every `SetLocal` targeting a virtualizable slot is immediately
///    followed by a `Pop`.**
///
/// 4. **No `scope_pop_slot_ip` entry points at a slot that isn't a
///    real local.**
///
/// 5. **No `local_init_result_ip` IP points to a `Closure` opcode.**
pub fn certify(func: &Function, types: &FunctionSlotTypes) -> CertifyResult {
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

    for slot in 0..types.slots.len() as u16 {
        if types.is_virtualizable(slot) && !types.has_known_initializer(slot) {
            return Err(format!(
                "virtualizable slot {} has no entry in local_init_result_ip",
                slot
            ));
        }
    }

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

    for (&ip, &slot) in &types.local_init_result_ip {
        if !types.is_virtualizable(slot) {
            continue;
        }
        let Some(op) = code.get(ip).copied().and_then(OpCode::from_byte) else {
            continue;
        };
        if !matches!(op, OpCode::Constant | OpCode::RangeLimit) {
            return Err(format!(
                "local_init_result_ip[{}] = slot {} but opcode at that IP is {:?}, \
                 not Constant — B2.1 only supports Constant(Integer) and RangeLimit initializers",
                ip, slot, op
            ));
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::value::{LocalInfo, ParamInfo};

    fn make_fn(name: &str, arity: u8, code: Vec<u8>, constants: Vec<Value>) -> Function {
        let mut func = Function::new(Some(name.to_string()), arity);
        func.chunk.code = code;
        func.chunk.constants = constants;
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

    #[test]
    fn constant_int_pushes_int64() {
        let mut f = make_fn(
            "f",
            0,
            vec![OpCode::Constant as u8, 0, 0, OpCode::Return as u8],
            vec![Value::Integer(42)],
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
    fn struct_literal_pops_only_its_fields() {
        let mut f = make_fn(
            "f",
            0,
            vec![],
            vec![
                Value::Integer(3),
                Value::Integer(5),
                Value::String(crate::vm::value::rc_str("Pt")),
            ],
        );
        f.chunk.code = vec![
            OpCode::Constant as u8,
            0,
            0,
            OpCode::Constant as u8,
            0,
            1,
            OpCode::StructLiteral as u8,
            0,
            2,
            0,
            OpCode::Pop as u8,
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

    #[test]
    fn int_through_unconditional_jump() {
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(1)]);
        f.chunk.code = vec![
            OpCode::Constant as u8,
            0,
            0,
            OpCode::SetLocal as u8,
            0,
            1,
            OpCode::Jump as u8,
            0,
            2,
            OpCode::None as u8,
            OpCode::None as u8,
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert_eq!(r.get(1), SlotType::Int64);
    }

    #[test]
    fn loop_counter_stays_int() {
        let mut f = make_fn(
            "loop_sum",
            1,
            vec![],
            vec![Value::Integer(0), Value::Integer(1), Value::Integer(2)],
        );
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::Constant as u8,
            0,
            0,
            OpCode::SetLocal as u8,
            0,
            2,
            OpCode::Constant as u8,
            0,
            1,
            OpCode::SetLocal as u8,
            0,
            3,
            OpCode::GetLocal as u8,
            0,
            2,
            OpCode::GetLocal as u8,
            0,
            3,
            OpCode::Constant as u8,
            0,
            2,
            OpCode::Multiply as u8,
            OpCode::Add as u8,
            OpCode::SetLocal as u8,
            0,
            2,
            OpCode::GetLocal as u8,
            0,
            3,
            OpCode::Constant as u8,
            0,
            1,
            OpCode::Add as u8,
            OpCode::SetLocal as u8,
            0,
            3,
            OpCode::GetLocal as u8,
            0,
            2,
            OpCode::Return as u8,
        ];
        let r = analyze(&f);
        assert_eq!(r.get(2), SlotType::Int64, "total should stay Int64");
        assert_eq!(r.get(3), SlotType::Int64, "i should stay Int64");
    }

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
                for c in &top.chunk.constants {
                    if let Some(closure) = c.as_closure()
                        && closure.function.name.as_deref() == Some(want)
                    {
                        return (*closure.function).clone();
                    }
                }
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

    /// Count Int64 slots. Includes param slot 0 which is the
    /// closure marker (always Value).
    fn count_int64(tys: &FunctionSlotTypes) -> usize {
        tys.int64_count()
    }

    #[test]
    fn annotated_int_param_classifies_as_int() {
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

        assert!(
            count_int64(&tys) >= 2,
            "expected ≥2 Int64 locals (total, i); got {:?}",
            tys.slots
        );
    }

    #[test]
    fn nested_loop_locals_stay_int() {
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

    #[test]
    fn call_result_is_value_even_if_args_are_int() {
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(1)]);
        f.chunk.code = vec![
            OpCode::None as u8,
            OpCode::Constant as u8,
            0,
            0,
            OpCode::Call as u8,
            1,
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
    fn local_init_result_ip_recorded_for_constant_int_init() {
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(42)]);
        f.chunk.code = vec![
            OpCode::Constant as u8,
            0,
            0,
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        f.local_decls = vec![(3, 1)];
        let r = analyze(&f);
        assert_eq!(r.local_init_result_ip.get(&0), Some(&1));
        assert!(r.is_virtualizable(1));
    }

    #[test]
    fn local_init_result_ip_picks_first_when_reassigned() {
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(1), Value::Integer(2)]);
        f.chunk.code = vec![
            OpCode::Constant as u8,
            0,
            0,
            OpCode::Constant as u8,
            0,
            1,
            OpCode::SetLocal as u8,
            0,
            1,
            OpCode::Pop as u8,
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        f.local_decls = vec![(3, 1)];
        let r = analyze(&f);
        assert_eq!(
            r.local_init_result_ip.get(&0),
            Some(&1),
            "first initializer should be at ip=0, not the later SetLocal"
        );
    }

    #[test]
    fn condition_cleanup_pops_detected_on_loop_pattern() {
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
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(0)]);
        f.chunk.code = vec![
            OpCode::Constant as u8,
            0,
            0,
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        f.local_decls = vec![(3, 1)];
        let mut r = analyze(&f);
        assert!(r.is_virtualizable(1));

        r.captured_slots.insert(1);
        assert!(!r.is_virtualizable(1));
        r.captured_slots.remove(&1);

        r.local_init_result_ip.clear();
        r.init_sites.remove(&1);
        assert!(!r.is_virtualizable(1));
    }

    #[test]
    fn certify_rejects_non_constant_initializer_for_virtualizable_slot() {
        let mut f = make_fn("f", 0, vec![], vec![Value::Integer(7)]);
        f.chunk.code = vec![
            OpCode::Constant as u8,
            0,
            0,
            OpCode::None as u8,
            OpCode::Return as u8,
        ];
        f.local_decls = vec![(3, 1)];
        let mut r = analyze(&f);
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
    fn init_sites_come_from_declarations_not_stack_shape() {
        let src = r#"
            fun f(b) {
                s := 0
                each i in range(3) { s = s + i }
                t := m(1, 2, 3)
                u := 0
                [s, t, u]
            }
            fun m(a, b, c) { a }
        "#;
        let f = compile_for_analysis(src, Some("f"));
        let r = analyze(&f);
        for &ip in r.local_init_result_ip.keys() {
            assert!(matches!(
                OpCode::from_byte(f.chunk.code[ip]),
                Some(OpCode::Constant | OpCode::RangeLimit)
            ));
        }
        let decl_slots: HashSet<u16> = f.local_decls.iter().map(|&(_, s)| s).collect();
        for slot in r.local_init_result_ip.values() {
            assert!(decl_slots.contains(slot));
        }
        let t_slot = f
            .local_decls
            .iter()
            .filter(|&&(ip, _)| {
                ip >= 3 && OpCode::from_byte(f.chunk.code[ip - 2]) == Some(OpCode::Call)
            })
            .map(|&(_, s)| s)
            .next()
            .expect("t is declared right after its call");
        assert!(!r.is_virtualizable(t_slot));
    }

    #[test]
    fn int_param_is_virtualizable_via_init_sites() {
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

    #[test]
    fn specialized_eligible_typed_int_param_returning_int() {
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
            OpCode::Return as u8,
        ];
        f.chunk.constants = vec![Value::Integer(0)];
        let r = analyze(&f);
        assert!(r.specialized_entry_eligible);
        assert_eq!(r.specialized_param_slots, vec![1]);
    }

    #[test]
    fn specialized_ineligible_zero_arity() {
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
            1,
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
        let mut f = make_fn("f", 1, vec![], vec![]);
        f.chunk.code = vec![OpCode::GetLocal as u8, 0, 1, OpCode::Return as u8];
        let r = analyze(&f);
        assert!(!r.specialized_entry_eligible);
    }

    #[test]
    fn specialized_eligible_int_param_with_arith_and_return() {
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
        assert!(!r.wants_closure_arg);
    }

    #[test]
    fn specialized_eligible_closure_with_int_upvalue() {
        let mut f = make_fn("closure", 1, vec![], vec![]);
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
            OpCode::GetUpvalue as u8,
            0,
            0,
            OpCode::GetLocal as u8,
            0,
            1,
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
        let mut f = make_fn("mut_closure", 1, vec![], vec![]);
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
            OpCode::SetUpvalue as u8,
            0,
            0,
            OpCode::Pop as u8,
            OpCode::GetLocal as u8,
            0,
            1,
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
        let mut f = make_fn("closing", 1, vec![], vec![]);
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
            0,
            OpCode::CloseUpvalue as u8,
            OpCode::GetLocal as u8,
            0,
            1,
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
        let mut f = make_fn("inner_closure", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![
            OpCode::GetUpvalue as u8,
            0,
            0,
            OpCode::GetLocal as u8,
            0,
            1,
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
        let mut f = make_fn("identity", 1, vec![], vec![]);
        f.params[0].type_ann = Some("int".to_string());
        f.chunk.code = vec![OpCode::GetLocal as u8, 0, 1, OpCode::Return as u8];
        f.chunk.constants = vec![];
        let r = analyze(&f);
        assert!(!r.specialized_entry_eligible);
        assert!(matches!(
            r.specialized_entry_outcome,
            SpecEligibilityOutcome::RejectedNoCall
        ));
        assert!(!r.wants_closure_arg);
    }

    #[test]
    fn lean_entry_accepts_typed_int_self_recursion() {
        let f = compile_for_analysis(
            r#"
fun work(n <int>) {
    option {
        n < 2 -> { n }
        { work(n - 1) + work(n - 2) * 3 - n }
    }
}
work(10)
"#,
            Some("work"),
        );
        let r = analyze(&f);
        assert!(r.lean_entry_eligible, "pure integer recursion must qualify");
    }

    #[test]
    fn lean_entry_rejects_string_valued_body() {
        let f = compile_for_analysis(
            r#"
fun label(n <int>) {
    give "small" when n < 2
    "big"
}
label(3)
"#,
            Some("label"),
        );
        let r = analyze(&f);
        assert!(!r.lean_entry_eligible, "a string constant must disqualify");
    }

    #[test]
    fn lean_entry_rejects_upvalue_reader() {
        let f = compile_for_analysis(
            "fun mk(x) { fun(y <int>) { x + y } }
mk(1)",
            None,
        );
        let r = analyze(&f);
        assert!(!r.lean_entry_eligible);
    }

    #[test]
    fn lean_entry_rejects_zero_arity() {
        let f = compile_for_analysis(
            "fun c() { 1 + 2 }
c()",
            Some("c"),
        );
        let r = analyze(&f);
        assert!(!r.lean_entry_eligible);
    }

    #[test]
    fn lean_entry_accepts_untyped_param_because_the_caller_guards() {
        let f = compile_for_analysis(
            r#"
fun work(n) {
    option {
        n < 2 -> { n }
        { work(n - 1) + work(n - 2) * 3 - n }
    }
}
work(10)
"#,
            Some("work"),
        );
        let r = analyze(&f);
        assert!(
            r.lean_entry_eligible,
            "an untyped int-only body qualifies; the caller supplies the guard"
        );
    }

    #[test]
    fn self_reachable_by_global_name_true_for_top_level_recursion() {
        let f = compile_for_analysis(
            r#"
fun fib(n <int>) {
    give n when n < 2
    fib(n - 1) + fib(n - 2)
}
fib(10)
"#,
            Some("fib"),
        );
        let r = analyze(&f);
        assert!(r.self_reachable_by_global_name);
        assert!(r.specialized_entry_eligible);
    }

    #[test]
    fn self_reachable_by_global_name_false_for_local_callee() {
        let f = compile_for_analysis(
            r#"
fun make_adder(x) { fun(y) { x + y } }
fun run(n <int>) {
    add5 := make_adder(5)
    total <int> := 0
    i <int> := 0
    repeat when i < n {
        total = total + add5(i)
        i = i + 1
    }
    total
}
run(10)
"#,
            Some("run"),
        );
        let r = analyze(&f);
        assert!(
            !r.self_reachable_by_global_name,
            "no GetGlobal names `run`, so the self-recursion guard can never match"
        );
        assert!(
            r.specialized_entry_eligible,
            "suppressing the guard must not suppress the specialized entry"
        );
    }

    #[test]
    fn self_reachable_by_global_name_false_for_mutual_recursion() {
        let f = compile_for_analysis(
            r#"
fun a(n <int>) {
    give 0 when n <= 0
    b(n - 1)
}
fun b(n <int>) { a(n - 1) + 1 }
a(4)
"#,
            Some("a"),
        );
        let r = analyze(&f);
        assert!(!r.self_reachable_by_global_name);
    }

    #[test]
    fn nested_named_self_recursion_is_eligible_and_closure_aware() {
        let f = compile_for_analysis(
            r#"
fun outer() {
    base <int> := 100
    fun rec(n <int>) {
        give base when n <= 0
        rec(n - 1) + 1
    }
    rec(10)
}
"#,
            Some("rec"),
        );
        let r = analyze(&f);
        assert!(
            r.specialized_entry_eligible,
            "rec must stay specialized-entry eligible: {:?}",
            r.specialized_entry_outcome
        );
        assert!(
            r.wants_closure_arg,
            "rec reaches its own name through GetUpvalue, so it is closure-aware"
        );
    }
}
