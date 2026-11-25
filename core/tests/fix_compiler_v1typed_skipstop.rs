//! Compiler tests.
//!
//! (A) skip/stop as a value: using `skip`/`stop` in a value-consumed position
//!     is a compile error, while `skip when ...` / `stop when ...` inside a
//!     loop body compiles and runs.
//!
//! (B) Typed update-vs-shadow: a typed walrus `x <int> := ...` of an existing
//!     untyped global from inside a nested `repeat` body updates the global,
//!     while a typed re-declaration of an already-typed global inside an
//!     `each` body shadows it.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn parse(source: &str) -> oxigen_core::ast::Program {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    program
}

fn compiles(source: &str) -> bool {
    Compiler::new().compile(&parse(source)).is_ok()
}

fn run_vm(source: &str, jit_threshold: Option<u32>) -> Result<String, String> {
    let program = parse(source);
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

#[test]
fn skip_as_value_in_option_arm_is_compile_error() {
    let src = r#"a := 1 + option { 1 <= 0 -> "x", <Error> -> { skip } }
a"#;
    assert!(
        !compiles(src),
        "skip used as a value should be a COMPILE error"
    );
}

#[test]
fn stop_as_value_in_block_is_compile_error() {
    let src = r#"a := option { 1 <= 0 -> "x", 1 >= 0 -> { stop } }
a"#;
    assert!(
        !compiles(src),
        "stop used as a value should be a COMPILE error"
    );
}

#[test]
fn skip_as_control_flow_still_compiles_and_runs() {
    let src = r#"sum := 0
each i in [1, 2, 3, 4] {
  skip when i == 2
  sum = sum + i
}
sum"#;
    assert!(compiles(src), "skip-as-control-flow must still compile");
    assert_eq!(run_vm(src, None).unwrap(), "8");
    assert_eq!(run_vm(src, Some(1)).unwrap(), "8");
}

#[test]
fn stop_as_control_flow_still_compiles_and_runs() {
    let src = r#"sum := 0
each i in [1, 2, 3, 4] {
  stop when i == 3
  sum = sum + i
}
sum"#;
    assert!(compiles(src), "stop-as-control-flow must still compile");
    assert_eq!(run_vm(src, None).unwrap(), "3");
    assert_eq!(run_vm(src, Some(1)).unwrap(), "3");
}

#[test]
fn typed_walrus_updates_untyped_global_in_repeat() {
    let src = r#"x := 0
repeat when x <= 5 {
  x <int> := x + 1
}
x"#;
    let interp = run_vm(src, None).expect("VM interp should terminate");
    let jit = run_vm(src, Some(1)).expect("VM JIT should terminate");
    assert_eq!(interp, "6", "VM interp should reach 6");
    assert_eq!(jit, "6", "VM JIT should reach 6");
    assert_eq!(interp, jit, "VM interp != VM JIT");
}

#[test]
fn typed_redeclare_of_typed_global_shadows_in_each() {
    let src = r#"x <int> = 10
each i in [1] {
  x <str> = "hi"
  println(x)
}
println(x)"#;
    let final_src = r#"x <int> = 10
each i in [1] {
  x <str> = "hi"
}
x"#;
    assert!(compiles(src), "shadowing program must compile");
    let interp = run_vm(final_src, None).unwrap();
    let jit = run_vm(final_src, Some(1)).unwrap();
    assert_eq!(interp, "10", "VM interp: outer x must remain 10");
    assert_eq!(jit, "10", "VM JIT: outer x must remain 10");
    assert_eq!(interp, jit, "VM interp != VM JIT");
}
