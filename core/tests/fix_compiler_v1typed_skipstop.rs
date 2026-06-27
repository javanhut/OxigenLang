//! Lane "compiler" regressions.
//!
//! (A) skip/stop as a VALUE: using `skip`/`stop` in a value-consumed position
//!     (e.g. the body of an option/choose arm, or the last expression of a
//!     block used as a value) must be a COMPILE error rather than the VM
//!     silently producing a value (`INTEGER + SKIP`). Legitimate control-flow
//!     `skip when ...` / `stop when ...` inside a loop body must still compile
//!     and run.
//!
//! (B) V1-typed update-vs-shadow: a typed walrus `x <int> := ...` of an existing
//!     UNTYPED global from inside a nested `repeat` body must UPDATE the global
//!     (so the loop terminates), while a typed re-declaration of an already-typed
//!     global inside an `each` body must SHADOW (outer value untouched).

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

// --- (A) skip/stop as a value ---------------------------------------------

#[test]
fn skip_as_value_in_option_arm_is_compile_error() {
    // The canonical repro: `1 + option { ... <Error> -> { skip } }`. The
    // tree-walker rejects `INTEGER + SKIP`; the compiler must reject it too
    // rather than silently producing a bogus value.
    let src = r#"a := 1 + option { 1 <= 0 -> "x", <Error> -> { skip } }
a"#;
    assert!(
        !compiles(src),
        "skip used as a value should be a COMPILE error"
    );
}

#[test]
fn stop_as_value_in_block_is_compile_error() {
    // A bare `stop` as the value-producing last statement of a block used as a
    // value is equally invalid.
    let src = r#"a := option { 1 <= 0 -> "x", 1 >= 0 -> { stop } }
a"#;
    assert!(
        !compiles(src),
        "stop used as a value should be a COMPILE error"
    );
}

#[test]
fn skip_as_control_flow_still_compiles_and_runs() {
    // `skip when ...` inside a loop body is legitimate control flow and must
    // NOT be rejected.
    let src = r#"sum := 0
each i in [1, 2, 3, 4] {
  skip when i == 2
  sum = sum + i
}
sum"#;
    assert!(compiles(src), "skip-as-control-flow must still compile");
    // 1 + 3 + 4 = 8 (2 skipped)
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
    // 1 + 2 = 3 (loop stops at i == 3)
    assert_eq!(run_vm(src, None).unwrap(), "3");
    assert_eq!(run_vm(src, Some(1)).unwrap(), "3");
}

// --- (B) V1-typed update-vs-shadow ----------------------------------------

#[test]
fn typed_walrus_updates_untyped_global_in_repeat() {
    // The hang repro: the inner typed walrus must UPDATE the outer untyped
    // global `x`, so the loop terminates instead of forever shadowing.
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
    // The documented shadowing example: the inner typed re-declaration of an
    // already-typed global must SHADOW and NOT alter the outer value.
    let src = r#"x <int> = 10
each i in [1] {
  x <str> = "hi"
  println(x)
}
println(x)"#;
    // We can't capture println output through the value, so assert parity on a
    // value-returning variant: the program's final value reflects the outer x.
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
