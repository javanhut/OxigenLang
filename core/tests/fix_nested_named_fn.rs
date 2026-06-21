//! V3 regression: a nested NAMED function must be able to reference itself
//! (recursive self-call) from inside its own body.
//!
//! Before the fix, the VM/JIT reported `undefined variable: rec` because a
//! named function declared inside another function did not bind its own name
//! in the enclosing scope before its body was compiled. The tree-walker (the
//! maturity oracle) recurses fine, so VM == JIT == tree-walker is the bar.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::evaluator::Evaluator;
use oxigen_core::lexer::Lexer;
use oxigen_core::object::environment::Environment;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;
use std::cell::RefCell;
use std::rc::Rc;

/// Run a program on the bytecode VM. `jit_threshold == Some(1)` forces the
/// JIT path; `None` keeps it on the interpreter.
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

/// Run a program on the tree-walking interpreter (the reference oracle).
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

/// Assert VM (interp) == VM (JIT) == tree-walker, and equal to `expected`.
fn assert_parity(source: &str, expected: &str) {
    let interp = run_vm(source, None).expect("VM interp should succeed");
    let jit = run_vm(source, Some(1)).expect("VM JIT should succeed");
    let tree = run_tree(source);
    assert_eq!(interp, expected, "VM interp mismatch");
    assert_eq!(jit, expected, "VM JIT mismatch");
    assert_eq!(tree, expected, "tree-walker mismatch");
    assert_eq!(interp, jit, "VM interp != VM JIT");
    assert_eq!(interp, tree, "VM interp != tree-walker");
}

#[test]
fn nested_named_fn_can_self_recurse() {
    // The canonical V3 repro: factorial via a nested named function `rec`
    // that calls itself.
    let src = r#"
fun outer(){
  fun rec(n){
    give 1 when n <= 1
    n * rec(n - 1)
  }
  rec(5)
}
outer()
"#;
    assert_parity(src, "120");
}

#[test]
fn nested_named_fn_no_recursion_still_works() {
    // A nested named fn that does NOT recurse must be unaffected by the fix.
    let src = r#"
fun outer(){
  fun helper(n){
    n + 1
  }
  helper(41)
}
outer()
"#;
    assert_parity(src, "42");
}

#[test]
fn deeper_nested_named_fn_self_recurse() {
    // Two levels of nesting; the innermost named fn recurses on itself.
    let src = r#"
fun a(){
  fun b(){
    fun sum(n){
      give 0 when n <= 0
      n + sum(n - 1)
    }
    sum(4)
  }
  b()
}
a()
"#;
    // 4 + 3 + 2 + 1 = 10
    assert_parity(src, "10");
}

#[test]
fn nested_named_fn_captures_outer_local_and_recurses() {
    // The recursive nested fn ALSO captures an outer local (`base`) as an
    // upvalue, exercising self-capture alongside ordinary capture.
    let src = r#"
fun outer(){
  base := 100
  fun rec(n){
    give base when n <= 0
    rec(n - 1) + 1
  }
  rec(3)
}
outer()
"#;
    // base(100) + 1 + 1 + 1 = 103
    assert_parity(src, "103");
}

#[test]
fn two_nested_named_fns_each_recurse() {
    // Two sibling nested named fns in the same body, each self-recursive.
    let src = r#"
fun outer(){
  fun fac(n){
    give 1 when n <= 1
    n * fac(n - 1)
  }
  fun cnt(n){
    give 0 when n <= 0
    1 + cnt(n - 1)
  }
  fac(4) + cnt(7)
}
outer()
"#;
    // 24 + 7 = 31
    assert_parity(src, "31");
}

#[test]
fn nested_named_fn_used_after_recursion() {
    // A local declared AFTER the nested fn must still get the right slot
    // (the pre-declared fn slot must not corrupt subsequent locals).
    let src = r#"
fun outer(){
  fun rec(n){
    give 1 when n <= 1
    n * rec(n - 1)
  }
  extra := 7
  rec(4) + extra
}
outer()
"#;
    // 24 + 7 = 31
    assert_parity(src, "31");
}
