//! Ad-hoc bytecode dumper for hot benchmarks. Invoked by hand via
//! `cargo test --release --features jit -p oxigen-core --test bytecode_dump
//!  -- --nocapture nested_loop_big`.
//!
//! Not a real test (no asserts that fail) — exists only so the JIT
//! review work can read the exact bytecode the compiler emits without
//! adding a CLI flag.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::compiler::opcode::{Chunk, OpCode};
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::value::Value;

fn parse_compile(source: &str) -> oxigen_core::vm::value::Function {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(parser.errors().is_empty(), "{}", parser.format_errors());
    Compiler::new().compile(&program).expect("compile")
}

fn fmt_const(v: &Value) -> String {
    match v {
        Value::Integer(n) => format!("Int({})", n),
        Value::Float(f) => format!("Float({})", f),
        Value::String(s) => format!("Str({:?})", s.as_ref()),
        Value::Closure(c) => format!(
            "Closure({:?} arity={})",
            c.function.name.as_deref().unwrap_or("<anon>"),
            c.function.arity
        ),
        Value::None => "None".into(),
        Value::Boolean(b) => format!("Bool({})", b),
        _ => format!("<other>"),
    }
}

fn r16(c: &[u8], off: usize) -> u16 {
    ((c[off] as u16) << 8) | (c[off + 1] as u16)
}

fn dump_chunk(name: &str, chunk: &Chunk) {
    println!("\n=== {} ===", name);
    println!("constants:");
    for (i, c) in chunk.constants.iter().enumerate() {
        println!("  {:>3}: {}", i, fmt_const(c));
    }
    println!("code:");
    let code = &chunk.code;
    let mut ip = 0;
    while ip < code.len() {
        let op = match OpCode::from_byte(code[ip]) {
            Some(o) => o,
            None => {
                println!("  {:>4}  <bad byte 0x{:02x}>", ip, code[ip]);
                ip += 1;
                continue;
            }
        };
        let line = chunk.lines.get(ip).copied().unwrap_or(0);
        let prefix = format!("  {:>4} L{:<3}", ip, line);
        let len = match op {
            // 1-byte
            OpCode::None | OpCode::True | OpCode::False | OpCode::Pop | OpCode::Dup
            | OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide
            | OpCode::Modulo | OpCode::Equal | OpCode::NotEqual | OpCode::Less
            | OpCode::LessEqual | OpCode::Greater | OpCode::GreaterEqual
            | OpCode::Not | OpCode::Negate | OpCode::Index | OpCode::CloseUpvalue
            | OpCode::Return => {
                println!("{} {:?}", prefix, op);
                1
            }
            // u8
            OpCode::Call => {
                let ac = code[ip + 1];
                println!("{} Call ac={}", prefix, ac);
                2
            }
            // u16
            OpCode::Constant | OpCode::BuildArray | OpCode::TypeWrap | OpCode::GetLocal
            | OpCode::SetLocal | OpCode::GetGlobal | OpCode::SetGlobal | OpCode::DefineGlobal
            | OpCode::GetUpvalue | OpCode::SetUpvalue | OpCode::Jump | OpCode::JumpIfFalse
            | OpCode::JumpIfTrue | OpCode::Loop | OpCode::PopJumpIfFalse | OpCode::StructDef
            | OpCode::GetField | OpCode::SetField => {
                let v = r16(code, ip + 1);
                let extra = match op {
                    OpCode::Constant | OpCode::GetGlobal | OpCode::SetGlobal
                    | OpCode::DefineGlobal | OpCode::GetField | OpCode::SetField
                    | OpCode::TypeWrap => {
                        chunk.constants.get(v as usize).map(|c| format!(" → {}", fmt_const(c))).unwrap_or_default()
                    }
                    OpCode::Jump | OpCode::JumpIfFalse | OpCode::JumpIfTrue
                    | OpCode::PopJumpIfFalse => format!(" → ip={}", ip + 3 + v as usize),
                    OpCode::Loop => format!(" → ip={}", (ip + 3).saturating_sub(v as usize)),
                    _ => String::new(),
                };
                println!("{} {:?} {}{}", prefix, op, v, extra);
                3
            }
            // u16+u8
            OpCode::StructLiteral | OpCode::DefineMethod | OpCode::MethodCall => {
                let v = r16(code, ip + 1);
                let n = code[ip + 3];
                println!("{} {:?} {} {}", prefix, op, v, n);
                4
            }
            // u16+u8+u16
            OpCode::DefineGlobalTyped => {
                let v = r16(code, ip + 1);
                let m = code[ip + 3];
                let t = r16(code, ip + 4);
                println!("{} {:?} name_idx={} mut={} type_idx={}", prefix, op, v, m, t);
                6
            }
            OpCode::Closure => {
                let fn_idx = r16(code, ip + 1);
                let uv_count = match chunk.constants.get(fn_idx as usize) {
                    Some(Value::Closure(c)) => c.function.upvalue_count as usize,
                    _ => 0,
                };
                println!(
                    "{} Closure fn_idx={} (uv_count={})",
                    prefix, fn_idx, uv_count
                );
                3 + 3 * uv_count
            }
            _ => {
                println!("{} {:?} (unsupported in dumper)", prefix, op);
                1
            }
        };
        ip += len;
    }
}

fn walk(name: &str, func: &oxigen_core::vm::value::Function) {
    dump_chunk(&format!("{} (top-level)", name), &func.chunk);
    for (i, c) in func.chunk.constants.iter().enumerate() {
        if let Value::Closure(closure) = c {
            let fname = closure
                .function
                .name
                .clone()
                .unwrap_or_else(|| format!("<anon@const{}>", i));
            dump_chunk(&fname, &closure.function.chunk);
            // recurse
            for (j, c2) in closure.function.chunk.constants.iter().enumerate() {
                if let Value::Closure(inner) = c2 {
                    let n2 = inner
                        .function
                        .name
                        .clone()
                        .unwrap_or_else(|| format!("<anon@{}.const{}>", fname, j));
                    dump_chunk(&n2, &inner.function.chunk);
                }
            }
        }
    }
}

#[test]
fn dump_nested_loop_big() {
    let src = std::fs::read_to_string("../example/bench_nested_loop_big.oxi").unwrap();
    let f = parse_compile(&src);
    walk("nested_loop_big", &f);
}

#[test]
fn dump_fib() {
    let src = std::fs::read_to_string("../example/bench_fib.oxi").unwrap();
    let f = parse_compile(&src);
    walk("fib", &f);
}

#[test]
fn dump_struct_method() {
    let src = std::fs::read_to_string("../example/bench_struct_method.oxi").unwrap();
    let f = parse_compile(&src);
    walk("struct_method", &f);
}

#[test]
fn dump_closure() {
    let src = std::fs::read_to_string("../example/bench_closure.oxi").unwrap();
    let f = parse_compile(&src);
    walk("closure", &f);
}
