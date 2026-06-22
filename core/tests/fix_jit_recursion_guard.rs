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

/// Like `run_result`, but on a 256 MB-stack thread (mirroring the CLI's
/// `run_file_vm`). Deep JIT self-recursion consumes a native call frame per
/// level; the default test-harness thread stack overflows natively at ~16k
/// frames before the VALUE-stack guard can trip, so the value-stack overflow
/// path must be exercised with the same large stack the CLI uses.
fn run_result_big_stack(source: &str, jit_threshold: Option<u32>) -> Result<String, String> {
    let source = source.to_string();
    std::thread::Builder::new()
        .stack_size(256 << 20)
        .spawn(move || run_result(&source, jit_threshold))
        .expect("spawn 256MB thread")
        .join()
        .expect("execution thread must not abort (a SIGSEGV here is the bug)")
}

/// A SECOND overflow axis (the value stack, not the frame count): a recursive
/// function with MANY LOCALS pushes many operand-stack slots per frame, so deep
/// recursion fills the pre-allocated value stack (STACK_MAX) long before
/// FRAMES_MAX frames. Before the fix the JIT's inline operand pushes had no
/// STACK_MAX bound, so this overran the value-stack buffer on the heap and
/// SIGSEGV'd (an 8-local function died near ~16k frames while the 1-local
/// functions above reach FRAMES_MAX cleanly). The recursion-depth guard now
/// also bounds `stack_view.len`, so this returns a graceful Err on every
/// backend instead of aborting.
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
    // JIT path: must be a graceful Err (value-stack guard), NOT a SIGSEGV.
    // Run on a 256 MB stack so the value stack fills before the native stack.
    let jit =
        run_result_big_stack(src, Some(1)).expect_err("deep many-local recursion must return Err");
    assert!(
        jit.to_lowercase().contains("stack overflow"),
        "JIT: expected a stack-overflow error, got: {jit:?}"
    );
    // VM (interpreter) path: same graceful Err via call_closure's STACK_MAX guard.
    let vm =
        run_result_big_stack(src, None).expect_err("deep many-local recursion must return Err on VM");
    assert!(
        vm.to_lowercase().contains("stack overflow"),
        "VM: expected a stack-overflow error, got: {vm:?}"
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
