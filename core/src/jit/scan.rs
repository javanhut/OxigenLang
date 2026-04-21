//! Bytecode pre-scan: verifies every opcode is in the translator's
//! allow-list and collects absolute branch-target offsets so the
//! translator can preallocate one Cranelift block per target.
//!
//! The allow-list is intentionally conservative: any opcode outside of it
//! causes compilation to be refused, so the function keeps running in the
//! interpreter. This is the "no mid-execution deopt" guarantee from the
//! plan.

use crate::compiler::opcode::{Chunk, OpCode};
use crate::vm::value::Value;

/// Result of a successful scan. The branch-target list is sorted and
/// deduplicated.
#[derive(Debug, Default)]
pub struct ScanInfo {
    /// Absolute bytecode offsets that are branch targets.
    pub branch_targets: Vec<usize>,
}

#[derive(Debug)]
pub enum ScanError {
    UnsupportedOpcode { offset: usize, byte: u8 },
    InvalidBytecode { offset: usize },
}

/// Return `true` if the baseline JIT's translator can currently emit
/// native code for this opcode.
fn is_supported(op: OpCode) -> bool {
    matches!(
        op,
        // Basic stack / constants
        OpCode::Constant
            | OpCode::None
            | OpCode::True
            | OpCode::False
            | OpCode::Pop
            | OpCode::Dup
            | OpCode::BuildArray
            | OpCode::Index
            | OpCode::TypeWrap
            // Arithmetic
            | OpCode::Add
            | OpCode::Subtract
            | OpCode::Multiply
            | OpCode::Divide
            | OpCode::Modulo
            // Comparison
            | OpCode::Equal
            | OpCode::NotEqual
            | OpCode::Less
            | OpCode::LessEqual
            | OpCode::Greater
            | OpCode::GreaterEqual
            // Logical / unary
            | OpCode::Not
            | OpCode::Negate
            // Locals
            | OpCode::GetLocal
            | OpCode::SetLocal
            // Globals
            | OpCode::GetGlobal
            | OpCode::SetGlobal
            | OpCode::DefineGlobal
            | OpCode::DefineGlobalTyped
            // Upvalues / closures
            | OpCode::GetUpvalue
            | OpCode::SetUpvalue
            | OpCode::CloseUpvalue
            | OpCode::Closure
            // Control flow
            | OpCode::Jump
            | OpCode::JumpIfFalse
            | OpCode::JumpIfTrue
            | OpCode::Loop
            | OpCode::PopJumpIfFalse
            // Functions
            | OpCode::Call
            | OpCode::Return
            // Structs
            | OpCode::StructDef
            | OpCode::StructLiteral
            | OpCode::GetField
            | OpCode::SetField
            | OpCode::DefineMethod
            | OpCode::MethodCall
    )
}

/// Length of this instruction in bytes, including its immediate operands.
/// `Closure` has runtime-determined length (depends on the captured
/// function's upvalue count), so it's handled specially in `scan`.
fn instr_fixed_len(op: OpCode) -> usize {
    match op {
        // 1-byte (no operand)
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
        | OpCode::Less
        | OpCode::LessEqual
        | OpCode::Greater
        | OpCode::GreaterEqual
        | OpCode::Not
        | OpCode::Negate
        | OpCode::Index
        | OpCode::CloseUpvalue
        | OpCode::Return => 1,

        // u8 operand (2 bytes)
        OpCode::Call => 2,

        // u16 operand (3 bytes)
        OpCode::Constant
        | OpCode::GetLocal
            | OpCode::SetLocal
            | OpCode::BuildArray
            | OpCode::TypeWrap
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
        | OpCode::StructDef
        | OpCode::GetField
        | OpCode::SetField => 3,

        // u16 + u8 (4 bytes)
        OpCode::StructLiteral | OpCode::DefineMethod | OpCode::MethodCall => 4,

        // u16 + u8 + u16 (6 bytes)
        OpCode::DefineGlobalTyped => 6,

        // Variable — Closure handled outside this function.
        _ => unreachable!("instr_fixed_len called for unsupported opcode {:?}", op),
    }
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

        if !is_supported(op) {
            return Err(ScanError::UnsupportedOpcode { offset: ip, byte });
        }

        // Branch targets: jumps use u16 offsets right after the opcode
        // byte.
        match op {
            OpCode::Jump | OpCode::JumpIfFalse | OpCode::JumpIfTrue | OpCode::PopJumpIfFalse => {
                let off =
                    read_u16(code, ip + 1).ok_or(ScanError::InvalidBytecode { offset: ip })?;
                let target = ip + 3 + off as usize;
                info.branch_targets.push(target);
            }
            OpCode::Loop => {
                let off =
                    read_u16(code, ip + 1).ok_or(ScanError::InvalidBytecode { offset: ip })?;
                let after = ip + 3;
                if (off as usize) > after {
                    return Err(ScanError::InvalidBytecode { offset: ip });
                }
                let target = after - off as usize;
                info.branch_targets.push(target);
            }
            _ => {}
        }

        // Advance ip.
        if matches!(op, OpCode::Closure) {
            // Closure is variable-length: 3 bytes for fn_const_idx, then
            // 3 bytes per upvalue descriptor (u8 is_local + u16 index).
            // We need the constants pool to resolve the upvalue count.
            let fn_idx = read_u16(code, ip + 1).ok_or(ScanError::InvalidBytecode { offset: ip })?;
            let upvalue_count = match chunk.constants.get(fn_idx as usize) {
                Some(Value::Closure(t)) => t.function.upvalue_count as usize,
                _ => return Err(ScanError::InvalidBytecode { offset: ip }),
            };
            ip += 3 + 3 * upvalue_count;
        } else {
            ip += instr_fixed_len(op);
        }
    }

    info.branch_targets.sort_unstable();
    info.branch_targets.dedup();
    Ok(info)
}
