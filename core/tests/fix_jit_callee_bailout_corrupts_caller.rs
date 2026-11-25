//! Tests that a JIT callee's entry-guard bailout is not reported as the caller's:
//! a float passed to a parameter the analyzer guessed was an int must not change
//! the caller's result. Every test compares the two backends.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn compile(source: &str) -> oxigen_core::vm::value::Function {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    Compiler::new()
        .compile(&program)
        .unwrap_or_else(|errs| panic!("compile errors: {errs:?}"))
}

fn run_on(mut vm: VM, source: &str) -> String {
    vm.run(compile(source))
        .map(|v| format!("{v}"))
        .unwrap_or_else(|e| e.message)
}

/// Asserts both backends agree, and that the answer is `expected`.
fn assert_backends_agree(source: &str, expected: &str) {
    let interp = run_on(VM::new_interpreter(), source);
    let jit = run_on(VM::new_eager_jit(), source);
    assert_eq!(interp, jit, "jit disagrees with interpreter for:\n{source}");
    assert_eq!(interp, expected, "wrong result for:\n{source}");
}

#[test]
fn loop_variable_survives_a_calls_bailout() {
    assert_backends_agree(
        "fun d(a <float>) { a / 4.0 }\nlog := []\neach i in range(4) { x := d(10.0)\n log = push(log, i) }\nlog",
        "[0, 1, 2, 3]",
    );
}

#[test]
fn loop_variable_is_correct_for_any_range_shape() {
    assert_backends_agree(
        "fun d(a <float>) { a / 4.0 }\nlog := []\neach i in range(3, 7) { x := d(10.0)\n log = push(log, i) }\nlog",
        "[3, 4, 5, 6]",
    );
    assert_backends_agree(
        "fun d(a <float>) { a / 4.0 }\nlog := []\neach i in range(0, 6, 2) { x := d(10.0)\n log = push(log, i) }\nlog",
        "[0, 2, 4]",
    );
}

#[test]
fn no_iteration_is_lost_when_accumulating_a_call_result() {
    assert_backends_agree(
        "fun d(a <float>) { a / 4.0 }\nt <float> := 0.0\neach i in range(4) { t = t + d(10.0) }\nt",
        "10",
    );
    assert_backends_agree(
        "fun d(a <float>) { a / 4.0 }\nt <float> := 0.0\nn <int> := 0\neach i in range(200) { t = t + d(10.0)\n n = n + 1 }\nn",
        "200",
    );
}

#[test]
fn untyped_params_handed_floats_are_correct() {
    assert_backends_agree(
        "fun d(a, b) { a + b }\nt <float> := 0.0\neach i in range(4) { t = t + d(10.0, 4.0) }\nt",
        "56",
    );
    assert_backends_agree(
        "fun d(a, b) { a + b }\nt <float> := 0.0\neach i in range(200) { t = t + d(10.0, 4.0) }\nt",
        "2800",
    );
}

#[test]
fn float_arithmetic_flavours_all_agree() {
    for (body, expected) in [
        ("a / 4.0", "10"),
        ("a * 0.25", "10"),
        ("a - 7.5", "10"),
        ("a % 4.0", "8"),
    ] {
        assert_backends_agree(
            &format!(
                "fun d(a <float>) {{ {body} }}\nt <float> := 0.0\neach i in range(4) {{ t = t + d(10.0) }}\nt"
            ),
            expected,
        );
    }
}

#[test]
fn a_bailing_callee_still_returns_the_right_value() {
    assert_backends_agree("fun d(a, b) { a + b }\nd(10.0, 4.0)", "14");
    assert_backends_agree("fun d(a <float>) { a / 4.0 }\nd(10.0)", "2.5");
}

#[test]
fn int_callees_are_unaffected() {
    assert_backends_agree(
        "fun d(a, b) { a + b }\nt <int> := 0\neach i in range(4) { t = t + d(10, 4) }\nt",
        "56",
    );
    assert_backends_agree(
        "fun work(n) {\n  option { n < 2 -> n, work(n - 1) + work(n - 2) }\n}\nwork(20)",
        "6765",
    );
}

#[test]
fn alternating_int_and_float_calls_agree() {
    assert_backends_agree(
        "fun d(a, b) { a + b }\nt <float> := 0.0\neach i in range(50) { t = t + d(1, 2)\n t = t + d(1.5, 2.5) }\nt",
        "350",
    );
}
