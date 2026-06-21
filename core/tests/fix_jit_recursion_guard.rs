//! V7 regression: the JIT self-recursion / closure-aware inline frame-push
//! paths must honor the same `FRAMES_MAX` recursion-depth guard the VM's
//! `call_closure` enforces. Before the fix, deep self-recursion under the JIT
//! ran PAST the limit and SIGSEGV'd by overrunning the pre-allocated
//! `jit_frames` buffer; the VM (`--no-jit`) returned a graceful
//! `stack overflow` error.
//!
//! These tests force the JIT path (threshold = 1) and assert:
//!   1. unbounded self-recursion returns `Err` with a "stack overflow"
//!      message (a graceful Result, NOT a process abort), for both a
//!      string-returning and an int-returning recursive function, and
//!   2. a moderate-depth recursive call still returns the correct value
//!      (the guard is a bound check, not a behavior change on legal depths).

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

/// Self-recursion that blows the frame stack must produce a graceful
/// `Err("stack overflow")` under the JIT, matching the VM — NOT a SIGSEGV.
/// String return exercises the V2 non-int-return path that lets recursion
/// actually reach deep depth.
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

/// Same guard must fire on the int-return recursion path (the bug is
/// general — it reproduces regardless of return type).
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

/// The guard is a pure bound check: a moderate-depth recursion that stays
/// well under `FRAMES_MAX` must still compute the correct result under the
/// JIT. `f(n)` returns `n` (sums 1 a thousand times down from 1000).
#[test]
fn jit_moderate_depth_recursion_still_correct() {
    // NOTE: assert on the *script value* (a bare trailing expression), not on
    // `println(...)` — a trailing `println` statement yields the script value
    // `None`, which would make this test check the wrong quantity. The printed
    // output and the bare-expression value are both verified to be 1000 across
    // tree-walk / --no-jit / --jit.
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
