//! R3b: a `skip`/`stop` whose value is CONSUMED by an enclosing expression
//! (operator operand, assignment/Let RHS, call argument, give/return value)
//! must be a COMPILE error rather than the VM/JIT silently producing a value
//! (e.g. `1 + skip` => `type mismatch: INTEGER + SKIP`).
//!
//! The rejection must fire ONLY in value-CONSUMED position. A `skip`/`stop`
//! in an `option`/`choose` arm whose RESULT IS DISCARDED (the construct is a
//! bare expression-statement) is legitimate loop control flow and MUST keep
//! compiling and running — that is the `stop_does_not_leak_loop_locals` shape.

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

/// Whether the program compiles (bytecode VM path). `true` == compiles.
fn compiles(source: &str) -> bool {
    Compiler::new().compile(&parse(source)).is_ok()
}

/// Run a program on the bytecode VM. `jit_threshold == Some(1)` forces the JIT.
fn run_vm(source: &str, jit_threshold: Option<u32>) -> Result<String, String> {
    let function = Compiler::new()
        .compile(&parse(source))
        .map_err(|errs| format!("{:?}", errs))?;
    let mut vm = VM::new();
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    vm.run(function)
        .map(|v| format!("{}", v))
        .map_err(|e| e.message)
}

// ── CONSUMED skip/stop => COMPILE error (the R3b repro) ─────────────────────

#[test]
fn skip_consumed_by_infix_is_compile_error() {
    // The canonical repro: the option's value (whose recovered <Error> arm is
    // `{ skip }`) is consumed by `1 + ...`. The tree-walker errors at runtime
    // on `INTEGER + SKIP`; the compiler must reject it up front.
    let src = r#"fun boom(){ <fail>("x") }
each i in [1] { x := 1 + option { boom() -> 5, <Error> -> { skip } }
  println(x) }
"#;
    assert!(
        !compiles(src),
        "skip consumed by `1 + option{{...}}` must be a COMPILE error"
    );
    // And the VM rejects the same program (at compile time).
    assert!(
        run_vm(src, None).is_err(),
        "VM should reject consumed skip; got {:?}",
        run_vm(src, None)
    );
}

#[test]
fn stop_consumed_by_infix_is_compile_error() {
    let src = r#"each i in [1] { x := 1 + option { false -> 5, true -> { stop } }
  println(x) }
"#;
    assert!(
        !compiles(src),
        "stop consumed by `1 + option{{...}}` must be a COMPILE error"
    );
}

#[test]
fn skip_consumed_by_let_rhs_is_compile_error() {
    // option whose tail arm is `{ skip }`, directly the RHS of a `:=`.
    let src = r#"each i in [1] { x := option { true -> { skip }, false -> 5 }
  println(x) }
"#;
    assert!(
        !compiles(src),
        "skip as a Let RHS value must be a COMPILE error"
    );
}

#[test]
fn skip_consumed_by_call_arg_is_compile_error() {
    let src = r#"each i in [1] { println(option { true -> { skip }, false -> 5 }) }
"#;
    assert!(
        !compiles(src),
        "skip as a call argument must be a COMPILE error"
    );
}

// ── DISCARDED skip/stop => still compiles AND runs (no regression) ──────────

#[test]
fn discarded_nested_loop_stop_still_compiles_and_runs() {
    // The exact `stop_does_not_leak_loop_locals` C4 shape: the option is a bare
    // expression-statement (its value is discarded), so the `<Error> -> { stop }`
    // recovered arm is legitimate loop control flow. Must compile AND run.
    let src = r#"fun f(){ each i in range(2){ each k in range(3){ option { k == 1 -> <fail>("x"), <Error> -> { stop }, k } } }
"done" }
f()"#;
    assert!(compiles(src), "discarded-stop option must still compile");
    assert_eq!(run_vm(src, None).unwrap(), "done", "VM interp");
    assert_eq!(run_vm(src, Some(1)).unwrap(), "done", "VM JIT");
}

#[test]
fn discarded_skip_arm_still_compiles_and_runs() {
    // An option that is a bare expression-statement whose recovered <Error> arm
    // is `{ skip }`. The value is discarded, so `skip` is legitimate `each`
    // control flow (the `stop_does_not_leak` shape, with skip instead of stop).
    let src = r#"fun f(){ s := 0
each i in range(3){ option { i == 1 -> <fail>("x"), <Error> -> { skip }, i }
  s = s + 1 }
s }
f()"#;
    assert!(compiles(src), "discarded-skip option must still compile");
    // The option value is discarded; the trailing `s = s + 1` runs every
    // iteration regardless of the <Error> arm's `skip`.
    assert_eq!(
        run_vm(src, None).unwrap(),
        run_vm(src, Some(1)).unwrap(),
        "VM interp == JIT"
    );
}

// ── Plain `skip when`/`stop when` control flow stays unaffected ─────────────

#[test]
fn plain_skip_when_still_compiles_and_runs() {
    let src = r#"s := 0
each i in range(5){ skip when i == 2
  s = s + i }
s"#;
    assert!(compiles(src), "plain `skip when` must compile");
    // 0 + 1 + 3 + 4 = 8 (i == 2 skipped)
    assert_eq!(run_vm(src, None).unwrap(), "8");
    assert_eq!(run_vm(src, Some(1)).unwrap(), "8");
}

#[test]
fn plain_stop_when_still_compiles_and_runs() {
    let src = r#"s := 0
each i in range(5){ stop when i == 3
  s = s + i }
s"#;
    assert!(compiles(src), "plain `stop when` must compile");
    // 0 + 1 + 2 = 3 (stop at i == 3)
    assert_eq!(run_vm(src, None).unwrap(), "3");
    assert_eq!(run_vm(src, Some(1)).unwrap(), "3");
}

#[test]
fn discarded_bare_option_with_skip_tail_compiles() {
    // An option whose LAST arm tail is `{ skip }`, used as a bare statement
    // (value discarded). Still legitimate; must compile.
    let src = r#"each i in range(2){ option { true -> { skip } } }
"ok""#;
    assert!(
        compiles(src),
        "bare (discarded) option with skip tail must compile"
    );
}
