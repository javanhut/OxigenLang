//! Tests that a nested named function can reference itself (recursive
//! self-call) from inside its own body, with VM (interp) == VM (JIT).

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

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

/// Assert VM (interp) == VM (JIT) == `expected`.
fn assert_parity(source: &str, expected: &str) {
    let interp = run_vm(source, None).expect("VM interp should succeed");
    let jit = run_vm(source, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, expected, "VM interp mismatch");
    assert_eq!(jit, expected, "VM JIT mismatch");
    assert_eq!(interp, jit, "VM interp != VM JIT");
}

#[test]
fn nested_named_fn_can_self_recurse() {
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
    assert_parity(src, "10");
}

#[test]
fn nested_named_fn_captures_outer_local_and_recurses() {
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
    assert_parity(src, "103");
}

#[test]
fn two_nested_named_fns_each_recurse() {
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
    assert_parity(src, "31");
}

#[test]
fn nested_named_fn_used_after_recursion() {
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
    assert_parity(src, "31");
}

#[test]
fn typed_nested_named_fn_capturing_an_outer_local_compiles_under_jit() {
    let src = r#"
fun outer(){
  base <int> := 100
  fun rec(n <int>){
    give base when n <= 0
    rec(n - 1) + 1
  }
  rec(10)
}
outer()
"#;
    assert_parity(src, "110");
}
