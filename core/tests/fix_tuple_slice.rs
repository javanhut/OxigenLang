//! Tests tuple slicing (`(1,2,3,4)[1:3]` is `(2, 3)`) gives the same result on
//! the interpreter and the JIT, including open, negative and empty/over-range
//! bounds, and that `x <int> = 5; x++` errors on every backend.

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
    let mut vm = VM::new();
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    vm.run(function)
        .map(|v| format!("{}", v))
        .map_err(|e| e.message)
}

/// Assert VM (interp) == VM (JIT) == `expected`.
fn assert_parity(source: &str, expected: &str) {
    let interp = run_vm(source, None).expect("VM interp should succeed");
    let jit = run_vm(source, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, expected, "VM interp mismatch for: {source}");
    assert_eq!(jit, expected, "VM JIT mismatch for: {source}");
    assert_eq!(interp, jit, "VM interp != VM JIT for: {source}");
}

#[test]
fn tuple_slice_basic() {
    assert_parity("(1,2,3,4)[1:3]", "(2, 3)");
}

#[test]
fn tuple_slice_open_start() {
    assert_parity("(1,2,3)[:2]", "(1, 2)");
}

#[test]
fn tuple_slice_open_end() {
    assert_parity("(1,2,3)[1:]", "(2, 3)");
}

#[test]
fn tuple_slice_open_both() {
    assert_parity("(1,2,3)[:]", "(1, 2, 3)");
}

#[test]
fn tuple_slice_negative_start() {
    assert_parity("(1,2,3,4)[-2:]", "(3, 4)");
}

#[test]
fn tuple_slice_negative_end() {
    assert_parity("(1,2,3,4)[:-1]", "(1, 2, 3)");
}

#[test]
fn tuple_slice_negative_both() {
    assert_parity("(1,2,3,4,5)[-3:-1]", "(3, 4)");
}

#[test]
fn tuple_slice_empty_inverted() {
    assert_parity("(1,2,3)[2:1]", "()");
}

#[test]
fn tuple_slice_over_range_end() {
    assert_parity("(1,2,3)[1:99]", "(2, 3)");
}

#[test]
fn tuple_slice_start_at_len() {
    assert_parity("(1,2,3)[3:]", "()");
}

#[test]
fn tuple_slice_full_explicit() {
    assert_parity("(10,20,30)[0:3]", "(10, 20, 30)");
}

#[test]
fn tuple_slice_mixed_elements() {
    assert_parity("(1, \"a\", True, 4)[1:3]", "(a, True)");
}

const INCR_IMMUT_SRC: &str = "x <int> = 5\nx++\n";

#[test]
fn incr_immut_vm_errors() {
    let interp = run_vm(INCR_IMMUT_SRC, None);
    assert!(
        interp.is_err(),
        "VM interp should error on incrementing an immutable, got: {interp:?}"
    );
    let jit = run_vm(INCR_IMMUT_SRC, Some(1));
    assert!(
        jit.is_err(),
        "VM JIT should error on incrementing an immutable, got: {jit:?}"
    );
    let msg = interp.unwrap_err();
    assert!(
        msg.contains("immutable variable `x`"),
        "VM message should mention the immutable variable, got: {msg}"
    );
}

#[test]
fn incr_immut_wording() {
    let msg = run_vm(INCR_IMMUT_SRC, None).unwrap_err();
    assert!(
        msg.contains("cannot mutate immutable variable `x` with `++`"),
        "compile-time wording drifted, got: {msg}"
    );
}

#[test]
fn incr_immut_runtime_wording() {
    let src = "fun f() {\n    x++\n}\nx <int> = 5\nf()\n";
    let msg = run_vm(src, None).unwrap_err();
    assert!(
        msg.contains("cannot assign to immutable variable `x`"),
        "VM wording drifted, got: {msg}"
    );
}
