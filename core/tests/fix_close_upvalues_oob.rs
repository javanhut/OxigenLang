//! Tests that a closure capturing a new block-local declared inside an
//! `option`-arm block runs without an out-of-bounds read in `close_upvalues`:
//!
//!   n := 0
//!   option { n <= 0 -> { data := 5
//!   c := fun(){ data }
//!   println(c()) }, 0 }

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_result(source: &str, jit_threshold: Option<u32>) -> Result<String, String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    let function = Compiler::new()
        .compile(&program)
        .expect("compile should succeed");
    let mut vm = VM::new();
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    vm.run(function)
        .map(|v| format!("{}", v))
        .map_err(|e| e.message)
}

const REPRO: &str = r#"
n := 0
option { n <= 0 -> { data := 5
c := fun(){ data }
println(c()) }, 0 }
"#;

const REPRO_VALUE: &str = r#"
n := 0
r := option { n <= 0 -> { data := 5
c := fun(){ data }
c() }, 0 }
r
"#;

#[test]
fn close_upvalues_oob_no_panic_interpreter() {
    let out = run_result(REPRO, None);
    assert!(out.is_ok(), "interpreter raised an error: {:?}", out);
}

#[test]
fn close_upvalues_oob_no_panic_jit() {
    let out = run_result(REPRO, Some(1));
    assert!(out.is_ok(), "jit raised an error: {:?}", out);
}

#[test]
fn close_upvalues_captured_value_is_five_interpreter() {
    let out = run_result(REPRO_VALUE, None);
    assert_eq!(out.as_deref(), Ok("5"), "interpreter: {:?}", out);
}

#[test]
fn close_upvalues_captured_value_is_five_jit() {
    let out = run_result(REPRO_VALUE, Some(1));
    assert_eq!(out.as_deref(), Ok("5"), "jit: {:?}", out);
}
