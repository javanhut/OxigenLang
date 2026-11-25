//! Division and modulo by zero are an error for every numeric type, on both
//! the interpreter and the JIT.

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

fn run_on(mut vm: VM, source: &str) -> Result<String, String> {
    vm.run(compile(source))
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

/// Asserts both backends reject `source` with the same message containing `needle`.
fn assert_both_reject(source: &str, needle: &str) {
    let interp = run_on(VM::new_interpreter(), source).unwrap_or_else(|e| e);
    let jit = run_on(VM::new_eager_jit(), source).unwrap_or_else(|e| e);
    assert_eq!(interp, jit, "backends disagree for: {source}");
    assert!(
        interp.contains(needle),
        "expected {needle:?} for {source}, got: {interp}"
    );
}

fn assert_both_yield(source: &str, expected: &str) {
    for (name, vm) in [
        ("interpreter", VM::new_interpreter()),
        ("jit", VM::new_eager_jit()),
    ] {
        let got = run_on(vm, source).unwrap_or_else(|e| panic!("{name} failed: {e}"));
        assert_eq!(got, expected, "{name} mismatch for: {source}");
    }
}

#[test]
fn float_division_by_zero_is_an_error() {
    for source in ["1.0 / 0.0", "-1.0 / 0.0", "0.0 / 0.0", "1.0 / 0", "1 / 0.0"] {
        assert_both_reject(source, "division by zero");
    }
}

#[test]
fn float_modulo_by_zero_is_an_error() {
    for source in ["1.0 % 0.0", "1.0 % 0", "1 % 0.0", "0.0 % 0.0"] {
        assert_both_reject(source, "modulo by zero");
    }
}

#[test]
fn negative_zero_is_still_zero() {
    assert_both_reject("z := -0.0\n1.0 / z", "division by zero");
    assert_both_reject("z := -0.0\n1.0 % z", "modulo by zero");
}

#[test]
fn integer_and_uint_division_by_zero_still_errors() {
    assert_both_reject("1 / 0", "division by zero");
    assert_both_reject("1 % 0", "modulo by zero");
}

#[test]
fn the_message_is_the_same_for_ints_and_floats() {
    let headline = |source: &str| {
        run_on(VM::new_interpreter(), source)
            .unwrap_err()
            .lines()
            .next()
            .unwrap_or_default()
            .to_string()
    };
    assert_eq!(headline("1 / 0"), headline("1.0 / 0.0"));
    assert_eq!(headline("1 % 0"), headline("1.0 % 0.0"));
}

#[test]
fn ordinary_division_is_unchanged() {
    assert_both_yield("7.0 / 2.0", "3.5");
    assert_both_yield("7 / 2", "3");
    assert_both_yield("1.0 / 0.5", "2");
    assert_both_yield("7.5 % 2.0", "1.5");
    assert_both_yield("7 % 2", "1");
    assert_both_yield("-7.0 / 2.0", "-3.5");
}

#[test]
fn a_hot_loop_agrees_across_backends() {
    assert_both_yield(
        "fun d(a <float>, b <float>) { a / b }\nlast <float> := 0.0\neach i in range(200) { last = d(10.0, 4.0) }\nlast",
        "2.5",
    );
    assert_both_reject(
        "fun d(a <float>, b <float>) { a / b }\nt <float> := 0.0\neach i in range(200) { t = t + d(10.0, 4.0) }\nd(1.0, 0.0)",
        "division by zero",
    );
}
