//! V8 regression: deep MUTUAL/indirect recursion under `--jit` must end
//! GRACEFULLY (a "stack overflow" `Err`, rc=1) like the VM — NOT a process
//! abort (SIGABRT rc=134).
//!
//! Root cause: the JIT indirect-call paths (generic IC / CA dispatch / method
//! IC) NATIVELY call the cached thunk, so every nested call in a mutually
//! recursive chain (`a -> b -> a -> …`) consumes one real machine-stack frame.
//! The default 8 MB native stack overflows at ~11k deep — BELOW `FRAMES_MAX`
//! (16384) — so the V7 graceful `FRAMES_MAX` heap-buffer guard never gets a
//! chance to fire, and the process SIGABRTs instead.
//!
//! Fix (V8, option (a)): drive program execution on a thread with a LARGE
//! native stack (256 MB) so that all 16384 `JitFrame`s fit within the native
//! stack, and the V7 `FRAMES_MAX` heap guard becomes the real, GRACEFUL limit.
//! The thread spawn lives in the CLI entry (`crates/oxigen-cli/src/main.rs`,
//! cross-file). The in-crate JIT V7 guard (`jit_check_recursion_depth`, emitted
//! before every inline `JitFrame` push) is unchanged and is what turns the
//! native-stack overflow into a graceful `Err` once the native stack is big
//! enough to reach the heap bound.
//!
//! This test mirrors the CLI fix by running the VM on a 256 MB-stack thread, so
//! it can verify the deep case in-process. It asserts:
//!   1. deep mutual recursion under JIT returns a graceful `Err` ("stack
//!      overflow"), NOT a process abort, and that VM(interp) == VM(jit);
//!   2. shallow mutual recursion still returns the correct value.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Native stack size used to drive execution — matches the CLI's V8 fix so the
/// V7 `FRAMES_MAX` heap guard (16384 frames) is reached before the native stack
/// overflows.
const BIG_STACK: usize = 256 << 20; // 256 MB

/// Run a program on the bytecode VM. `jit_threshold == Some(1)` forces the JIT
/// path; `None` keeps it on the interpreter.
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

/// Run `run_vm` on a 256 MB-stack thread (the V8 fix), so deep recursion can
/// reach the graceful `FRAMES_MAX` heap bound instead of overrunning the native
/// stack. The thread JOINs, so a panic/abort inside would propagate as a join
/// error rather than crashing the whole test process ungracefully — but with
/// the fix the deep case returns a normal `Err` via the V7 guard.
fn run_vm_big_stack(source: &'static str, jit_threshold: Option<u32>) -> Result<String, String> {
    std::thread::Builder::new()
        .stack_size(BIG_STACK)
        .spawn(move || run_vm(source, jit_threshold))
        .expect("spawn big-stack thread")
        .join()
        .expect("execution thread should not abort/panic (V8: must be graceful)")
}

const MUTUAL_DEEP: &str = r#"
fun a(n){ option { n <= 0 -> 0, b(n - 1) } }
fun b(n){ option { n <= 0 -> 0, a(n - 1) } }
println(a(40000))
"#;

const MUTUAL_SHALLOW: &str = r#"
fun a(n){ option { n <= 0 -> 0, b(n - 1) } }
fun b(n){ option { n <= 0 -> 0, a(n - 1) } }
a(10)
"#;

/// Deep mutual recursion under JIT must end with a graceful "stack overflow"
/// `Err` (rc=1 in the CLI), NOT a process abort. Driving it on the big-stack
/// thread lets the native stack hold all 16384 `JitFrame`s so the V7 heap guard
/// fires gracefully. A depth (40000) safely beyond `FRAMES_MAX` (16384)
/// guarantees the bound is hit regardless of tail-call behavior.
#[test]
fn deep_mutual_recursion_jit_is_graceful_not_abort() {
    let jit = run_vm_big_stack(MUTUAL_DEEP, Some(1));
    assert!(
        jit.is_err(),
        "deep mutual recursion under JIT must return graceful Err, got Ok: {:?}",
        jit
    );
    let msg = jit.unwrap_err().to_lowercase();
    assert!(
        msg.contains("stack overflow") || msg.contains("overflow") || msg.contains("recursion"),
        "expected a graceful stack-overflow error, got: {}",
        msg
    );
}

/// Parity: the interpreter must ALSO end gracefully on the same deep input,
/// and the JIT must agree with it (both `Err` "stack overflow") — VM == JIT.
#[test]
fn deep_mutual_recursion_vm_equals_jit() {
    let interp = run_vm_big_stack(MUTUAL_DEEP, None);
    let jit = run_vm_big_stack(MUTUAL_DEEP, Some(1));
    assert!(
        interp.is_err(),
        "interp deep mutual recursion must be graceful Err, got: {:?}",
        interp
    );
    assert!(
        jit.is_err(),
        "jit deep mutual recursion must be graceful Err, got: {:?}",
        jit
    );
    // Both must report the same graceful stack-overflow condition.
    let i = interp.unwrap_err().to_lowercase();
    let j = jit.unwrap_err().to_lowercase();
    assert!(
        i.contains("overflow") && j.contains("overflow"),
        "VM != JIT graceful-overflow message: interp={:?} jit={:?}",
        i,
        j
    );
}

/// Shallow mutual recursion still computes the right value (the option arms
/// return 0 at the base case) under interp AND JIT — the fix must not perturb
/// correct, non-overflowing mutual recursion.
#[test]
fn shallow_mutual_recursion_still_correct() {
    let interp = run_vm_big_stack(MUTUAL_SHALLOW, None).expect("interp shallow should succeed");
    let jit = run_vm_big_stack(MUTUAL_SHALLOW, Some(1)).expect("jit shallow should succeed");
    assert_eq!(interp, "0", "interp shallow mutual recursion wrong value");
    assert_eq!(jit, "0", "jit shallow mutual recursion wrong value");
    assert_eq!(interp, jit, "VM interp != VM JIT (shallow mutual recursion)");
}
