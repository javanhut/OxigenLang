//! `oxigen test` now runs each `<test>` block on the bytecode VM (not the
//! tree-walker), so tests observe the same semantics as `oxigen file.oxi` —
//! notably IN-PLACE `push`/`insert`. These tests cover the VM test runner
//! (`oxigen_core::test_runner::run_vm_tests`):
//!   * pass/fail detection (a body that raises an error fails),
//!   * push mutating in place (the whole point of the switch),
//!   * the auto-imported `expect` + its `Expectation` struct/methods resolving
//!     across the module boundary (the VM module-globals fix), and
//!   * files with no `<test>` blocks yielding nothing.

#![cfg(feature = "jit")]

use oxigen_core::evaluator::TestOutcome;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::test_runner::run_vm_tests;

fn run(src: &str) -> Vec<TestOutcome> {
    let lexer = Lexer::new(src);
    let mut parser = Parser::new(lexer, src);
    let program = parser.parse_program();
    assert!(parser.errors().is_empty(), "parse errors:\n{}", parser.format_errors());
    run_vm_tests(&program, src, None)
}

#[test]
fn runner_reports_pass_and_fail() {
    let outcomes = run(
        "<test>(\"passes\") { x := 1 + 1\n x }\n\
         <test>(\"fails on runtime error\") { arr := [1, 2]\n arr[10][0] }\n",
    );
    assert_eq!(outcomes.len(), 2);
    assert_eq!(outcomes[0].name, "passes");
    assert!(outcomes[0].passed, "a clean body must pass");
    assert_eq!(outcomes[1].name, "fails on runtime error");
    assert!(!outcomes[1].passed, "a body that raises must fail");
}

#[test]
fn push_mutates_in_place_under_runner() {
    // In `oxigen test` (VM) push mutates the original. If it did NOT (the old
    // tree-walker behavior), `a[2]` would be None and `+ 0` would error,
    // failing the test.
    let outcomes = run("<test>(\"push\") { a := [1, 2]\n push(a, 3)\n a[2] + 0 }\n");
    assert_eq!(outcomes.len(), 1);
    assert!(
        outcomes[0].passed,
        "push must mutate in place under the VM runner; got: {:?}",
        outcomes[0].message
    );
}

#[test]
fn expect_import_and_module_struct_methods_resolve() {
    // Exercises the VM module-globals fix: `expect` (imported from the `test`
    // module) returns an `Expectation`, and `.eq(..)` resolves on it even though
    // neither `expect` nor `Expectation` is in the test file's own globals.
    let outcomes = run(
        "<test>(\"expect pass\") { expect(2 + 3).eq(5) }\n\
         <test>(\"expect fail\") { expect(1).eq(2) }\n",
    );
    assert_eq!(outcomes.len(), 2);
    assert!(outcomes[0].passed, "expect(5).eq(5) should pass");
    assert!(!outcomes[1].passed, "expect(1).eq(2) should fail");
    assert!(
        outcomes[1].message.as_deref().unwrap_or("").contains("expected"),
        "failure message should carry the matcher text, got: {:?}",
        outcomes[1].message
    );
}

#[test]
fn setup_functions_are_visible_to_tests() {
    // Top-level `fun` definitions (setup) are included before each test body.
    let outcomes = run(
        "fun double(n) { n * 2 }\n\
         <test>(\"uses setup\") { expect(double(21)).eq(42) }\n",
    );
    assert_eq!(outcomes.len(), 1);
    assert!(outcomes[0].passed, "setup fn must be callable from the test; {:?}", outcomes[0].message);
}

#[test]
fn no_test_blocks_yields_empty() {
    let outcomes = run("fun f(x) { x + 1 }\nprintln(f(2))\n");
    assert!(outcomes.is_empty(), "a file with no <test> blocks yields no outcomes");
}
