//! Bytecode pre-scan: verifies every opcode is in the translator's
//! allow-list and collects absolute branch-target offsets so the
//! translator can preallocate one Cranelift block per target.
//!
//! The allow-list is intentionally conservative: any opcode outside of it
//! causes compilation to be refused, so the function keeps running in the
//! interpreter. This is the "no mid-execution deopt" guarantee from the
//! plan.

use crate::compiler::opcode::{Chunk, ControlFlow, JitSupport, OpCode};
use crate::vm::value::ValueRepr;

/// Result of a successful scan. The branch-target list is sorted and
/// deduplicated.
#[derive(Debug, Default)]
pub struct ScanInfo {
    /// Absolute bytecode offsets that are branch targets.
    pub branch_targets: Vec<usize>,
    /// Loop body ranges `[target, loop_op_pos)` — one per `OpCode::Loop`
    /// back-edge (`target` is the back-edge destination, `loop_op_pos` is the
    /// `Loop` opcode's own offset). An ip within any range executes inside a
    /// loop, which the translator uses to disable peephole optimizations whose
    /// inline memory loads would be hoisted/CSE'd across iterations (e.g. the
    /// struct-field-add peephole, which otherwise reuses a stale field value).
    pub loop_ranges: Vec<(usize, usize)>,
    /// True if the function body contains `OpCode::Closure`, i.e. can
    /// create closures that capture locals of the current frame. When
    /// false, `VM::close_upvalues(frame.slot_offset)` at Return time is
    /// provably a no-op (only `handle_closure` extends `open_upvalues`
    /// with entries pointing into the current frame's slots), which
    /// lets the JIT inline the return path.
    pub may_capture_upvalues: bool,
    /// True if the function body produces or consumes any heap-backed
    /// (Rc-bearing) Value on its stack. When false, the stack's values
    /// inside this frame are guaranteed to be primitive tags
    /// (Integer/Float/Bool/etc.) with no Drop side-effect, so Return
    /// can skip the stack_truncate drop loop entirely.
    ///
    /// Conservative: set true by any opcode that can place a heap-
    /// backed Value on the stack or touch one already there (args or
    /// upvalues we can't track statically).
    pub touches_heap_values: bool,
}

#[derive(Debug)]
pub enum ScanError {
    UnsupportedOpcode { offset: usize, byte: u8 },
    InvalidBytecode { offset: usize },
}

fn read_u16(code: &[u8], offset: usize) -> Option<u16> {
    if offset + 1 >= code.len() {
        return None;
    }
    Some(((code[offset] as u16) << 8) | (code[offset + 1] as u16))
}

pub fn scan(chunk: &Chunk) -> Result<ScanInfo, ScanError> {
    let code = &chunk.code;
    let mut info = ScanInfo::default();
    let mut ip = 0;

    while ip < code.len() {
        let byte = code[ip];
        let op = OpCode::from_byte(byte).ok_or(ScanError::InvalidBytecode { offset: ip })?;
        let op_info = op.info();

        if op_info.jit != JitSupport::Native {
            return Err(ScanError::UnsupportedOpcode { offset: ip, byte });
        }

        // Branch targets: jumps use u16 offsets right after the opcode
        // byte.
        match op_info.control_flow {
            ControlFlow::ForwardJump | ControlFlow::ConditionalJump => {
                let off =
                    read_u16(code, ip + 1).ok_or(ScanError::InvalidBytecode { offset: ip })?;
                let target = ip + 3 + off as usize;
                info.branch_targets.push(target);
            }
            ControlFlow::BackwardJump => {
                let off =
                    read_u16(code, ip + 1).ok_or(ScanError::InvalidBytecode { offset: ip })?;
                let after = ip + 3;
                if (off as usize) > after {
                    return Err(ScanError::InvalidBytecode { offset: ip });
                }
                let target = after - off as usize;
                info.branch_targets.push(target);
                info.loop_ranges.push((target, ip));
            }
            ControlFlow::Fallthrough | ControlFlow::Return | ControlFlow::ErrorHandler => {}
        }

        // Heap-value detection. Conservative: any opcode that can put a
        // heap-backed (Rc-bearing) Value on the stack sets the flag.
        // Also any Constant pointing to an Rc-backed constant. Opcodes
        // that only touch primitives (arithmetic, comparison, jumps,
        // local/global primitive ops) don't set the flag.
        if op_info.properties.touches_heap {
            info.touches_heap_values = true;
        } else if matches!(op, OpCode::Constant) {
            let idx = read_u16(code, ip + 1)
                .ok_or(ScanError::InvalidBytecode { offset: ip })?;
            if let Some(v) = chunk.constants.get(idx as usize)
                && !matches!(
                    v.repr(),
                    ValueRepr::Integer(_)
                        | ValueRepr::Float(_)
                        | ValueRepr::Boolean(_)
                        | ValueRepr::Byte(_)
                        | ValueRepr::Uint(_)
                        | ValueRepr::Char(_)
                        | ValueRepr::None
                ) {
                    info.touches_heap_values = true;
                }
        }

        // Advance using the canonical bytecode-width decoder. It also
        // validates fixed operands and Closure descriptors.
        let instruction_len = chunk
            .instruction_len(ip)
            .map_err(|_| ScanError::InvalidBytecode { offset: ip })?;
        if op_info.properties.captures {
            // Closure opcode means this function may capture locals into
            // upvalues of the inner closure; Return must therefore call
            // close_upvalues. Record that fact so the JIT can NOT inline
            // op_return for this function.
            info.may_capture_upvalues = true;
        }
        ip += instruction_len;
    }

    info.branch_targets.sort_unstable();
    info.branch_targets.dedup();
    Ok(info)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn close_upvalue_requires_interpreter_fallback() {
        let mut chunk = Chunk::new();
        chunk.code.push(OpCode::CloseUpvalue as u8);
        assert!(matches!(
            scan(&chunk),
            Err(ScanError::UnsupportedOpcode { offset: 0, .. })
        ));
    }
}
