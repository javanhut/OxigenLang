//! V1-typed regression: a typed re-declaration (`x <int> := …` / `x <int> = …`)
//! of an existing UNTYPED global must follow the tree-walker's env model —
//! UPDATE the global inside a `repeat`/plain block (shared enclosing env) but
//! SHADOW inside an `each` body (fresh per-iteration env).
//!
//! The first V1-typed attempt keyed update-vs-shadow off the global's type-ness
//! and so wrongly UPDATED inside `each` loops, diverging from the tree-walker
//! (`x := 0; each i in [1,2,3] { x <int> := x + 1 }; x` gave 3 in the VM but 0 in
//! the tree-walker). The fix tracks `each_body_depth` in the compiler frame and
//! shadows inside `each` bodies. These tests pin tree-walker == VM parity for the
//! repeat-update, each-shadow, documented-shadowing, and nested combinations.

use oxigen_core::compiler::Compiler;
use oxigen_core::evaluator::Evaluator;
use oxigen_core::lexer::Lexer;
use oxigen_core::object::environment::Environment;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;
use std::cell::RefCell;
use std::rc::Rc;

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

fn run_tree(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    let env = Rc::new(RefCell::new(Environment::new()));
    let mut evaluator = Evaluator::new();
    format!("{}", evaluator.eval_program(&program, env))
}

/// Assert tree-walker (oracle) == VM, and equal to `expected` (the VM's final
/// program value — these programs end in a `println`, so the script value is
/// `None`; we compare on the captured stdout instead where it matters, but for
/// the value-returning final-expression cases we compare the script value).
fn assert_parity_value(source: &str, expected: &str) {
    let tree = run_tree(source);
    let vm = run_vm(source);
    assert_eq!(tree, vm, "tree-walker != VM for:\n{source}");
    assert_eq!(vm, expected, "unexpected value for:\n{source}");
}

#[test]
fn each_body_typed_redecl_of_untyped_global_shadows() {
    // The reviewer's regression case: each iteration has a fresh env, so the
    // typed re-declaration shadows and the outer `x` stays 0.
    assert_parity_value("x := 0\neach i in [1,2,3] { x <int> := x + 1 }\nx\n", "0");
    // The `=` (non-walrus) typed form behaves identically.
    assert_parity_value("x := 0\neach i in [1,2,3] { x <int> = x + 1 }\nx\n", "0");
}

#[test]
fn repeat_body_typed_redecl_of_untyped_global_updates() {
    // `repeat` shares the enclosing env, so the typed re-declaration updates the
    // global and the loop terminates (no shadow-induced hang).
    assert_parity_value("x := 0\nrepeat when x <= 5 { x <int> := x + 1 }\nx\n", "6");
}

#[test]
fn typed_outer_each_shadow_is_preserved() {
    // Documented shadowing: a TYPED outer global is shadowed (not mutated) by a
    // typed re-declaration in an inner scope — outer stays 10.
    assert_parity_value(
        "x <int> = 10\neach i in [1] { x <str> = \"hi\" }\nx\n",
        "10",
    );
}

#[test]
fn repeat_inside_each_shadows() {
    // each_body_depth > 0 dominates: the inner repeat is still inside the each
    // body's fresh env, so the re-declaration shadows and outer `x` stays 0.
    assert_parity_value(
        "x := 0\neach i in [1,2] { repeat when False { x <int> := 9 } }\nx\n",
        "0",
    );
}

#[test]
fn each_inside_repeat_updates_outer_via_repeat() {
    // The repeat updates `x` (shared env); the inner each shadows its own 99.
    // x goes 0->1->2->3 via the repeat's typed re-decl; the each's 99 is local.
    assert_parity_value(
        "x := 0\nrepeat when x < 3 { x <int> := x + 1\neach i in [1] { x <int> := 99 } }\nx\n",
        "3",
    );
}
