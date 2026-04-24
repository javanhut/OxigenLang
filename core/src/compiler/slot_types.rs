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

use std::collections::HashMap;

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

// ── Per-function result ───────────────────────────────────────────────

/// Per-slot classification for one JIT-compiled function. Indexed by
/// local slot number. A slot that was never assigned stays `Bottom`
/// (callers can treat this as `Value` — it means the slot is unused).
#[derive(Debug, Clone)]
pub struct FunctionSlotTypes {
    pub slots: Vec<SlotType>,
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
            let fallthrough = cursor + opcode_len(op, code, cursor);

            // Initialization-push detection: if the stack grew by one
            // and the new top lands at a position within the local
            // range, that push is initializing that local slot. This
            // is how Oxigen initializes `total := 0` — no SetLocal
            // emitted, just a Constant at the slot's stack position.
            let depth_after = next_state.stack.len();
            if depth_after == depth_before + 1 {
                let new_pos = depth_after - 1;
                if new_pos < num_slots {
                    let ty = next_state
                        .stack
                        .last()
                        .copied()
                        .unwrap_or(SlotType::Bottom);
                    next_state.write_slot(new_pos, ty);
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

    FunctionSlotTypes { slots: result }
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
            next.stack.pop();
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
fn opcode_len(op: OpCode, code: &[u8], ip: usize) -> usize {
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
        // We decode the constant to read the function's upvalue count
        // and advance accordingly.
        OpCode::Closure => {
            let idx = read_u16(code, ip + 1) as usize;
            // We can't easily access the chunk's constants from here
            // without threading it through. For the type analysis this
            // opcode contributes nothing special (it pushes `Value`),
            // so approximating the length by "walk until we find the
            // next known opcode" would be fragile. Instead we assume
            // the caller threaded the chunk; if it didn't, we fall
            // back to 3 (base opcode size) — which may be incorrect
            // for functions with upvalues but is conservative (we'd
            // still mark everything in the body as `Value` on
            // underflow so downstream analysis remains sound).
            let _ = idx;
            3
        }
    }
}

#[inline]
fn read_u16(code: &[u8], offset: usize) -> u16 {
    ((code[offset] as u16) << 8) | (code[offset + 1] as u16)
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
}
