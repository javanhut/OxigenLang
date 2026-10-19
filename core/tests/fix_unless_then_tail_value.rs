//! Regression: a two-armed `x unless cond then y` produces a value when it is
//! the last statement of a function or block.
//!
//! The statement parses to an `If` with an alternative, which the tail-value
//! path marked as value-producing, but each arm was compiled as a plain
//! statement whose value was popped. Nothing was left for the `Return`, so
//! `fun g() { a unless True then -a }` returned the function itself
//! (`fun g()`), and the same tail inside an `option` arm block gave `None`.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_on(mut vm: VM, source: &str) -> Result<String, String> {
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
        .map_err(|errs| format!("{errs:?}"))?;
    vm.run(function)
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

fn assert_all_ok(source: &str, expected: &str) {
    for (name, make) in [
        ("interpreter", VM::new_interpreter as fn() -> VM),
        ("tiered", VM::new_tiered),
        ("eager_jit", VM::new_eager_jit),
    ] {
        let got = run_on(make(), source).unwrap_or_else(|e| panic!("{name} failed: {e}\n{source}"));
        assert_eq!(got, expected, "{name} mismatch for:\n{source}");
    }
}

#[test]
fn function_tail_takes_the_then_branch() {
    assert_all_ok("a := 1\nfun g() { a unless True then -a }\ng()", "-1");
}

#[test]
fn function_tail_takes_the_main_branch() {
    assert_all_ok("a := 1\nfun h() { a unless False then -a }\nh()", "1");
}

#[test]
fn function_tail_with_locals_in_scope() {
    assert_all_ok(
        "fun k(n) {\n  m := n * 2\n  m unless n > 3 then m + 100\n}\n[k(1), k(5)]",
        "[2, 110]",
    );
}

#[test]
fn option_arm_block_tail() {
    assert_all_ok(
        "a := 1\nx := option { False -> { 5 } { a unless True then -a } }\nx",
        "-1",
    );
}
