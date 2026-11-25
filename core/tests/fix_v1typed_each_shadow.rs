//! Tests that a typed re-declaration (`x <int> := ...` / `x <int> = ...`) of an
//! existing untyped global updates the global inside a `repeat`/plain block and
//! shadows it inside an `each` body, including nested combinations.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_vm(source: &str) -> String {
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
    vm.run(function)
        .map(|v| format!("{}", v))
        .unwrap_or_else(|e| format!("ERR: {}", e.message))
}

/// Assert the VM's final program value equals `expected`.
fn assert_parity_value(source: &str, expected: &str) {
    let vm = run_vm(source);
    assert_eq!(vm, expected, "unexpected value for:\n{source}");
}

#[test]
fn each_body_typed_redecl_of_untyped_global_shadows() {
    assert_parity_value("x := 0\neach i in [1,2,3] { x <int> := x + 1 }\nx\n", "0");
    assert_parity_value("x := 0\neach i in [1,2,3] { x <int> = x + 1 }\nx\n", "0");
}

#[test]
fn repeat_body_typed_redecl_of_untyped_global_updates() {
    assert_parity_value("x := 0\nrepeat when x <= 5 { x <int> := x + 1 }\nx\n", "6");
}

#[test]
fn typed_outer_each_shadow_is_preserved() {
    assert_parity_value(
        "x <int> = 10\neach i in [1] { x <str> = \"hi\" }\nx\n",
        "10",
    );
}

#[test]
fn repeat_inside_each_shadows() {
    assert_parity_value(
        "x := 0\neach i in [1,2] { repeat when False { x <int> := 9 } }\nx\n",
        "0",
    );
}

#[test]
fn each_inside_repeat_updates_outer_via_repeat() {
    assert_parity_value(
        "x := 0\nrepeat when x < 3 { x <int> := x + 1\neach i in [1] { x <int> := 99 } }\nx\n",
        "3",
    );
}
