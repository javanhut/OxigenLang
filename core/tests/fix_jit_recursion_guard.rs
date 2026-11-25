//! Tests that the JIT self-recursion and closure-aware inline frame-push paths
//! honor the `FRAMES_MAX` recursion-depth guard.
//!
//! The tests force the JIT path (threshold = 1) and assert:
//!   1. unbounded self-recursion returns `Err` with a "stack overflow" message
//!      for both a string-returning and an int-returning recursive function, and
//!   2. a moderate-depth recursive call still returns the correct value.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_result(source: &str, jit_threshold: Option<u32>) -> Result<String, String> {
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
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    vm.run(function)
        .map(|v| format!("{}", v))
        .map_err(|e| e.message)
}

/// Self-recursion that exhausts the frame stack produces `Err("stack overflow")`
/// under the JIT, matching the VM. Uses a string return.
#[test]
fn jit_self_recursion_string_return_overflows_gracefully() {
    let src = r#"
fun f(n) {
    option {
        n <= 0 -> "ok",
        f(n - 1)
    }
}
println(f(200000))
"#;
    let err = run_result(src, Some(1)).expect_err("deep recursion must return Err, not abort");
    assert!(
        err.to_lowercase().contains("stack overflow"),
        "expected a stack-overflow error, got: {err:?}"
    );
}

/// The same guard fires on the int-return recursion path.
#[test]
fn jit_self_recursion_int_return_overflows_gracefully() {
    let src = r#"
fun f(n) {
    option {
        n <= 0 -> 0,
        f(n - 1)
    }
}
println(f(200000))
"#;
    let err = run_result(src, Some(1)).expect_err("deep recursion must return Err, not abort");
    assert!(
        err.to_lowercase().contains("stack overflow"),
        "expected a stack-overflow error, got: {err:?}"
    );
}

/// Like `run_result`, but on a 256 MB-stack thread.
fn run_result_big_stack(source: &str, jit_threshold: Option<u32>) -> Result<String, String> {
    let source = source.to_string();
    std::thread::Builder::new()
        .stack_size(256 << 20)
        .spawn(move || run_result(&source, jit_threshold))
        .expect("spawn 256MB thread")
        .join()
        .expect("execution thread must not abort (a SIGSEGV here is the bug)")
}

/// A recursive function with many locals pushes many operand-stack slots per
/// frame, so deep recursion fills the value stack (STACK_MAX) before FRAMES_MAX
/// frames; the guard returns a graceful Err on every backend.
#[test]
fn jit_many_locals_recursion_overflows_value_stack_gracefully() {
    let src = r#"
fun rec(a, b, c, d, e, f, g, h) {
    p := a + b + c + d + e + f + g + h
    q := p * 2
    r := q - a
    s := r + b
    t := s * c
    u := t - d
    v := u + e
    w := v * f
    option { a <= 0 -> 0, rec(a - 1, b, c, d, e, f, g, h) + w }
}
println(rec(200000, 1, 2, 3, 4, 5, 6, 7))
"#;
    let jit =
        run_result_big_stack(src, Some(1)).expect_err("deep many-local recursion must return Err");
    assert!(
        jit.to_lowercase().contains("stack overflow"),
        "JIT: expected a stack-overflow error, got: {jit:?}"
    );
    let vm = run_result_big_stack(src, None)
        .expect_err("deep many-local recursion must return Err on VM");
    assert!(
        vm.to_lowercase().contains("stack overflow"),
        "VM: expected a stack-overflow error, got: {vm:?}"
    );
}

/// A moderate-depth recursion well under `FRAMES_MAX` computes the correct
/// result under the JIT. `f(n)` returns `n`.
#[test]
fn jit_moderate_depth_recursion_still_correct() {
    let src = r#"
fun f(n) {
    option {
        n <= 0 -> 0,
        1 + f(n - 1)
    }
}
f(1000)
"#;
    let out = run_result(src, Some(1)).expect("moderate-depth recursion must succeed");
    assert_eq!(out, "1000", "moderate recursion returned the wrong value");
}
