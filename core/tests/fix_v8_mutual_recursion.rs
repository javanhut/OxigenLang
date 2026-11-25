//! Deep mutual recursion under `--jit` ends with a graceful "stack overflow"
//! `Err` like the VM, not a process abort.
//!
//! Execution runs on a thread with a 256 MB native stack. The test asserts:
//!   1. deep mutual recursion under JIT returns a graceful `Err` ("stack
//!      overflow"), and VM(interp) == VM(jit);
//!   2. shallow mutual recursion still returns the correct value.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Native stack size used to drive execution.
const BIG_STACK: usize = 256 << 20;

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

/// Run `run_vm` on a 256 MB-stack thread.
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

/// Deep mutual recursion under JIT ends with a graceful "stack overflow"
/// `Err`. The depth (40000) is beyond `FRAMES_MAX` (16384).
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

/// The interpreter also ends gracefully on the same deep input, and the JIT
/// agrees with it (both `Err` "stack overflow").
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
    let i = interp.unwrap_err().to_lowercase();
    let j = jit.unwrap_err().to_lowercase();
    assert!(
        i.contains("overflow") && j.contains("overflow"),
        "VM != JIT graceful-overflow message: interp={:?} jit={:?}",
        i,
        j
    );
}

/// Shallow mutual recursion computes the right value under interp and JIT.
#[test]
fn shallow_mutual_recursion_still_correct() {
    let interp = run_vm_big_stack(MUTUAL_SHALLOW, None).expect("interp shallow should succeed");
    let jit = run_vm_big_stack(MUTUAL_SHALLOW, Some(1)).expect("jit shallow should succeed");
    assert_eq!(interp, "0", "interp shallow mutual recursion wrong value");
    assert_eq!(jit, "0", "jit shallow mutual recursion wrong value");
    assert_eq!(
        interp, jit,
        "VM interp != VM JIT (shallow mutual recursion)"
    );
}
