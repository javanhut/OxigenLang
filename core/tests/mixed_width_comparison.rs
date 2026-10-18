//! `<`, `<=`, `>` and `>=` accept the same operands, and compare numbers of
//! different kinds by value:
//!
//! - integers of any width (`int`, `uint`, `byte`) compare exactly, so a
//!   `uint` above the largest `int` is still greater than every `int`;
//! - anything with a `float` compares as floats;
//! - strings compare with strings and chars with chars.
//!
//! Before, only `int` with `float` mixed, and strings and bytes supported
//! `<` but none of the other three.

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

/// `smaller` and `larger` order that way under all four operators, and each
/// equals itself under `<=` and `>=` but not `<` and `>`.
fn assert_ordered(smaller: &str, larger: &str) {
    let cases = [
        (format!("{smaller} < {larger}"), "True"),
        (format!("{smaller} <= {larger}"), "True"),
        (format!("{smaller} > {larger}"), "False"),
        (format!("{smaller} >= {larger}"), "False"),
        (format!("{larger} < {smaller}"), "False"),
        (format!("{larger} <= {smaller}"), "False"),
        (format!("{larger} > {smaller}"), "True"),
        (format!("{larger} >= {smaller}"), "True"),
        (format!("{smaller} <= {smaller}"), "True"),
        (format!("{smaller} >= {smaller}"), "True"),
        (format!("{smaller} < {smaller}"), "False"),
        (format!("{smaller} > {smaller}"), "False"),
    ];
    for (name, _) in backends() {
        for (expr, expected) in &cases {
            let vm = match name {
                "interpreter" => VM::new_interpreter(),
                _ => VM::new_eager_jit(),
            };
            let got = run_on(vm, &format!("{UMAX}{expr}"))
                .unwrap_or_else(|e| panic!("{name} failed on {expr}: {e}"));
            assert_eq!(&got, expected, "{name}: {expr}");
        }
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

/// The largest `uint`, which no `int` can hold.
const UMAX: &str = "umax := uint(9223372036854775807) * uint(2) + uint(1)\n";

#[test]
fn bytes_and_uints_compare_with_ints() {
    assert_ordered("byte(3)", "4");
    assert_ordered("3", "byte(4)");
    assert_ordered("uint(3)", "4");
    assert_ordered("3", "uint(4)");
}

#[test]
fn bytes_compare_with_uints_and_bytes() {
    assert_ordered("byte(3)", "uint(4)");
    assert_ordered("uint(3)", "byte(4)");
    assert_ordered("byte(3)", "byte(4)");
}

#[test]
fn integers_of_any_width_compare_exactly() {
    // As an int, umax would wrap to -1.
    assert_ordered("0", "umax");
    assert_ordered("9223372036854775807", "umax");
    assert_ordered("-1", "uint(0)");
    assert_ordered("-1", "byte(0)");
}

#[test]
fn anything_with_a_float_compares_as_floats() {
    assert_ordered("byte(2)", "2.5");
    assert_ordered("2.5", "uint(3)");
    assert_ordered("1.5", "2");
}

#[test]
fn strings_and_chars_support_every_operator() {
    assert_ordered("\"apple\"", "\"banana\"");
    assert_ordered("`a`", "`b`");
}

#[test]
fn unrelated_kinds_still_do_not_compare() {
    assert_rejects("1 < \"a\"", "cannot compare INTEGER < STRING");
    assert_rejects("\"a\" >= `a`", "cannot compare STRING >= CHAR");
    assert_rejects("vec2(1, 2) <= vec2(1, 2)", "cannot compare VEC2 <= VEC2");
}
