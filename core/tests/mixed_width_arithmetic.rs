//! Arithmetic between different numeric types follows one promotion rule for
//! every operator, the same one `==` already uses:
//!
//! - `byte` or `uint` with `int` is done as `int`;
//! - `byte` with `uint` is done as `uint`;
//! - anything with `float` is done as `float`;
//! - `byte` with `byte` is done as `int`.
//!
//! The promoted pair then behaves exactly like that type with itself:
//! `int` wraps, `uint` subtraction below zero is an error, and `/` and `%`
//! reject a zero divisor. Before this, `+` and `-` accepted some mixes while
//! `*`, `/` and `%` rejected most of them as type mismatches.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_on(mut vm: VM, source: &str) -> Result<String, String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(parser.errors().is_empty(), "{}", parser.format_errors());
    let function = Compiler::new()
        .compile(&program)
        .unwrap_or_else(|errs| panic!("compile errors: {errs:?}"));
    vm.run(function)
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

fn backends() -> [(&'static str, VM); 2] {
    [
        ("interpreter", VM::new_interpreter()),
        ("jit", VM::new_eager_jit()),
    ]
}

/// `expr` evaluates to `value`, of type `type_name`.
fn assert_gives(expr: &str, value: &str, type_name: &str) {
    for (name, vm) in backends() {
        let got = run_on(vm, &format!("r := {expr}\nstr(r) + \" \" + type(r)"))
            .unwrap_or_else(|e| panic!("{name} failed on {expr}: {e}"));
        assert_eq!(got, format!("{value} {type_name}"), "{name}: {expr}");
    }
}

fn assert_rejects(expr: &str, needle: &str) {
    for (name, vm) in backends() {
        let err = run_on(vm, expr).expect_err(&format!("{name} should reject {expr}"));
        assert!(
            err.contains(needle),
            "{name}: expected {needle:?} in: {err}"
        );
    }
}

#[test]
fn byte_and_uint_mix_with_int_as_int() {
    for (op, result) in [("*", "12"), ("+", "7"), ("-", "1"), ("/", "1"), ("%", "1")] {
        assert_gives(&format!("4 {op} byte(3)"), result, "INTEGER");
        assert_gives(&format!("byte(4) {op} 3"), result, "INTEGER");
        assert_gives(&format!("uint(4) {op} 3"), result, "INTEGER");
        assert_gives(&format!("4 {op} uint(3)"), result, "INTEGER");
    }
}

#[test]
fn byte_and_uint_mix_as_uint() {
    for (op, result) in [("*", "12"), ("+", "7"), ("-", "1"), ("/", "1"), ("%", "1")] {
        assert_gives(&format!("uint(4) {op} byte(3)"), result, "UINT");
        assert_gives(&format!("byte(4) {op} uint(3)"), result, "UINT");
    }
}

#[test]
fn bytes_with_bytes_divide_as_int() {
    assert_gives("byte(7) / byte(2)", "3", "INTEGER");
    assert_gives("byte(7) % byte(2)", "1", "INTEGER");
}

#[test]
fn anything_with_a_float_is_a_float() {
    assert_gives("byte(2) - 0.5", "1.5", "FLOAT");
    assert_gives("uint(3) * 0.5", "1.5", "FLOAT");
    assert_gives("2.0 / byte(4)", "0.5", "FLOAT");
    assert_gives("uint(7) % 2.0", "1", "FLOAT");
    assert_gives("0.5 * uint(4)", "2", "FLOAT");
}

#[test]
fn promoted_int_arithmetic_wraps() {
    assert_gives("9223372036854775807 * byte(2)", "-2", "INTEGER");
    assert_gives("uint(2) * 9223372036854775807", "-2", "INTEGER");
}

#[test]
fn promoted_uint_subtraction_below_zero_is_an_error() {
    assert_rejects("uint(1) - byte(2)", "unsigned integer underflow");
}

#[test]
fn a_zero_divisor_is_still_an_error() {
    assert_rejects("5 / byte(0)", "division by zero");
    assert_rejects("uint(5) % byte(0)", "modulo by zero");
    assert_rejects("byte(5) / byte(0)", "division by zero");
}

#[test]
fn non_numbers_still_do_not_mix() {
    assert_rejects("byte(1) * \"a\"", "type mismatch: BYTE * STRING");
    assert_rejects("uint(1) / True", "type mismatch: UINT / BOOLEAN");
}
