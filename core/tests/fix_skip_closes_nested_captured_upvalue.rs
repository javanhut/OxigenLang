//! Tests that captured locals in block-as-expression arms keep the arm's value
//! and are closed correctly, across the VM, the JIT, and the third backend.
//!
//! Covers `end_scope_keeping_value` for value-producing option/choose arms that
//! declare locals (including captured non-floor locals), and the
//! `CloseUpvalueAt` opcode for a captured floor local.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Run a program on the bytecode VM. `jit_threshold == Some(1)` forces the JIT
/// path; `None` keeps it on the interpreter.
fn run_vm(source: &str, jit_threshold: Option<u32>) -> Result<String, String> {
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
        .map_err(|errs| format!("{:?}", errs))?;
    let mut vm = if jit_threshold.is_some() {
        VM::new_tiered()
    } else {
        VM::new_interpreter()
    };
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    vm.run(function)
        .map(|v| format!("{}", v))
        .map_err(|e| e.message)
}

/// Assert VM (interp) == VM (JIT), and both equal to `expected`.
fn assert_parity(source: &str, expected: &str) {
    let interp = run_vm(source, None).expect("VM interp should succeed");
    let jit = run_vm(source, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, expected, "VM interp mismatch");
    assert_eq!(jit, expected, "VM JIT mismatch");
    assert_eq!(interp, jit, "VM interp != VM JIT");
}

#[test]
fn option_arm_value_with_single_local() {
    assert_parity("r := option { True -> {\n y := 5\n y + 1\n } }\nr", "6");
}

#[test]
fn option_arm_value_with_two_locals() {
    assert_parity(
        "r := option { True -> {\n y := 5\n z := 7\n y + z\n } }\nr",
        "12",
    );
}

#[test]
fn option_arm_value_with_three_locals() {
    assert_parity(
        "r := option { True -> {\n a := 1\n b := 2\n c := 3\n a + b + c\n } }\nr",
        "6",
    );
}

#[test]
fn nested_option_arm_value_with_local_per_iteration() {
    let src = "\
out := []
each i in [1,2] {
  r := option { True -> {
    y := i * 100
    option { True -> { y + 1 } }
  } }
  out = push(out, r)
}
out";
    assert_parity(src, "[101, 201]");
}

#[test]
fn captured_non_floor_local_in_value_block_per_iteration() {
    let src = "\
acc := []
each i in [1,2] {
  option { True -> {
    pad := 0
    x := i * 100
    g := fun(){ x }
    option { True -> { acc = push(acc, g) } }
  } }
}
out := []
each f in acc { out = push(out, f()) }
out";
    assert_parity(src, "[100, 200]");
}

#[test]
fn skip_path_closes_captured_local_from_nested_option_arm() {
    let src = "\
acc := []
each i in [1,2] {
  x := i * 100
  g := fun(){ x }
  option { i == 2 -> { skip }, True -> { acc = push(acc, g) } }
}
out := []
each f in acc { out = push(out, f()) }
out";
    assert_parity(src, "[100]");
}

#[test]
fn skip_in_loop_with_captured_body_local_then_skip() {
    let src = "\
acc := []
each i in [1,2] {
  x := i * 100
  g := fun(){ x }
  acc = push(acc, g)
  option { i == 1 -> { skip }, True -> {} }
}
out := []
each f in acc { out = push(out, f()) }
out";
    assert_parity(src, "[100, 200]");
}

#[test]
fn floor_captured_local_skip_bug_a() {
    let src = "\
acc := []
each i in [1,2] {
  option { True -> {
    x := i * 100
    g := fun(){ x }
    option { i == 2 -> { skip }, True -> { acc = push(acc, g) } }
  } }
}
out := []
each f in acc { out = push(out, f()) }
out";
    assert_parity(src, "[100]");
}

#[test]
fn floor_captured_local_value_block_no_skip() {
    let src = "\
acc := []
each i in [1,2] {
  option { True -> {
    x := i * 100
    g := fun(){ x }
    option { True -> { acc = push(acc, g) } }
  } }
}
out := []
each f in acc { out = push(out, f()) }
out";
    assert_parity(src, "[100, 200]");
}

#[test]
fn function_returning_captured_floor_value_block() {
    let src = "\
fun f(n) {
  r := option { True -> {
    x := n * 2
    g := fun(){ x }
    g()
  } }
  give r
}
acc := 0
acc = f(10)
acc = f(20)
acc = f(30)
acc";
    assert_parity(src, "60");
}
