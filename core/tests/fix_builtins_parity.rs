//! Builtin-permissiveness regression: the bytecode VM and JIT must agree on
//! the result of several builtins that were previously inconsistent:
//!
//!   - range(start, end)  — range(n) and range(start, end) both supported.
//!   - str([1, 2])        — uses the Display form.
//!   - str(None)          — "None".
//!   - rest([])           — [] (empty array).
//!   - int(" 5 ")         — trims surrounding whitespace.
//!   - float(True/False)  — Boolean arm.
//!
//! Plus a few cases to prove no regression.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Run a program on the bytecode VM. `jit_threshold == Some(1)` forces the
/// JIT path; `None` keeps it on the interpreter.
fn run_vm(source: &str, jit_threshold: Option<u32>) -> String {
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
        .map_err(|errs| format!("{:?}", errs))
        .expect("compile should succeed");
    let mut vm = VM::new();
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    vm.run(function)
        .map(|v| format!("{}", v))
        .unwrap_or_else(|e| format!("ERR: {}", e.message))
}

/// Assert VM (interp) == VM (JIT) for the given source.
fn assert_parity(source: &str) {
    let interp = run_vm(source, None);
    let jit = run_vm(source, Some(1));
    assert_eq!(interp, jit, "VM interp != VM JIT for `{}`", source);
}

#[test]
fn range_two_arg() {
    assert_parity("range(2, 5)");
}

#[test]
fn range_one_arg_no_regression() {
    assert_parity("range(3)");
}

#[test]
fn str_of_array() {
    assert_parity("str([1, 2])");
}

#[test]
fn str_of_none() {
    assert_parity("str(None)");
}

#[test]
fn str_of_int_no_regression() {
    assert_parity("str(42)");
}

#[test]
fn rest_of_empty() {
    assert_parity("rest([])");
}

#[test]
fn int_with_surrounding_whitespace() {
    assert_parity("int(\" 5 \")");
}

#[test]
fn int_plain_no_regression() {
    assert_parity("int(\"5\")");
}

#[test]
fn float_of_true() {
    assert_parity("float(True)");
}

#[test]
fn float_of_false() {
    assert_parity("float(False)");
}

/// Concrete expected values, so the parity assertions aren't trivially
/// satisfied by interp and JIT sharing the same wrong answer.
#[test]
fn concrete_expected_values() {
    assert_eq!(run_vm("range(2, 5)", None), "[2, 3, 4]");
    assert_eq!(run_vm("str([1, 2])", None), "[1, 2]");
    assert_eq!(run_vm("str(None)", None), "None");
    assert_eq!(run_vm("rest([])", None), "[]");
    assert_eq!(run_vm("int(\" 5 \")", None), "5");
    assert_eq!(run_vm("float(True)", None), "1");
    assert_eq!(run_vm("float(False)", None), "0");
}
