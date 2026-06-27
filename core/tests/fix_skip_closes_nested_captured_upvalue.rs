//! BUG A investigation + block-as-expression value-preservation fix.
//!
//! The original BUG A repro (a `skip` from a nested option-arm leaving a
//! captured loop-scoped local with a dangling open upvalue) turned out NOT to
//! be a defect in the `skip`/`stop` stack-cleanup itself — that loop already
//! emits `CloseUpvalue` for every captured local above the loop floor. The true
//! root cause is the BLOCK-AS-EXPRESSION cleanup in the compiler:
//! `compile_block_as_expression` produced the arm's RESULT VALUE on top of the
//! block's locals, then called `end_scope`, whose `Pop`/`CloseUpvalue` both act
//! on the TOP of the operand stack — so they discarded/closed the RESULT instead
//! of the locals. That corrupts the value of EVERY value-producing option/choose
//! arm that declares a local, e.g. `r := option { True -> { y := 5  y + 1 } }`
//! evaluated to `5` (the local), not `6` (the result), under VM/JIT.
//!
//! The fix adds `end_scope_keeping_value` (compiler/mod.rs): it stashes the
//! result into the floor slot with `SetLocal`, drops the duplicate, then
//! closes/pops the remaining body locals top-down. This makes VM == JIT for
//! value-producing blocks with locals, including captured NON-floor locals.
//!
//! The remaining case — when the block's FLOOR local is itself captured by a
//! surviving closure — is now closed too. The value-preserving stash would
//! overwrite that slot before its upvalue could be closed, and no top-of-stack
//! op can close a buried slot while keeping the result on top. So the compiler
//! emits a slot-indexed `CloseUpvalueAt floor` (opcode.rs) BEFORE the stash; it
//! snapshots and closes the buried floor upvalue in place. The VM interpreter
//! implements it (`close_upvalue_at_slot`); the JIT scan does NOT allow-list
//! `CloseUpvalueAt`, so a hot function containing it falls back to the
//! interpreter (the same way `BuildArray`/`Index` functions do). The original
//! BUG A repro shape (`x` is the floor local of the outer option arm) therefore
//! agrees across all three backends — see the `floor_captured_local_*` tests
//! below (run under JIT threshold 1, exercising that fallback).

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

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

/// Assert VM (interp) == VM (JIT), and both equal to `expected`.
fn assert_parity(source: &str, expected: &str) {
    let interp = run_vm(source, None).expect("VM interp should succeed");
    let jit = run_vm(source, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, expected, "VM interp mismatch");
    assert_eq!(jit, expected, "VM JIT mismatch");
    assert_eq!(interp, jit, "VM interp != VM JIT");
}

// ── Block-as-expression value preservation (the real root cause) ────────────

#[test]
fn option_arm_value_with_single_local() {
    // The minimal repro of the corruption: a value-producing option arm that
    // declares a local. Must yield the RESULT (6), not the local (5).
    assert_parity("r := option { True -> { y := 5  y + 1 } }\nr", "6");
}

#[test]
fn option_arm_value_with_two_locals() {
    assert_parity("r := option { True -> { y := 5  z := 7  y + z } }\nr", "12");
}

#[test]
fn option_arm_value_with_three_locals() {
    assert_parity(
        "r := option { True -> { a := 1  b := 2  c := 3  a + b + c } }\nr",
        "6",
    );
}

#[test]
fn nested_option_arm_value_with_local_per_iteration() {
    // A non-captured local under a value-producing inner-option tail, once per
    // loop iteration. Previously the VM kept the local instead of the result.
    // Returns a list of the per-iteration results so the harness can observe
    // the value (it cannot see `print` stdout).
    let src = "\
out := []
each i in [1,2] {
  r := option { True -> {
    y := i * 100
    option { True -> { y + 1 } }
  } }
  out = push(out, r)
}
out";
    assert_parity(src, "[101, 201]");
}

// ── Captured NON-floor locals in value blocks (now fixed) ───────────────────

#[test]
fn captured_non_floor_local_in_value_block_per_iteration() {
    // `pad` is the floor local (not captured); the captured local `x` sits
    // above it, so the value-preserving stash closes `x` correctly. Each
    // iteration's closure must observe its own `x` (100 then 200).
    let src = "\
acc := []
each i in [1,2] {
  option { True -> {
    pad := 0
    x := i * 100
    g := fun(){ x }
    option { True -> { acc = push(acc, g) } }
  } }
}
out := []
each f in acc { out = push(out, f()) }
out";
    assert_parity(src, "[100, 200]");
}

// ── skip / stop stack cleanup still correct (no regression) ─────────────────

#[test]
fn skip_path_closes_captured_local_from_nested_option_arm() {
    // The surviving closure is captured on an iteration that then SKIPs. The
    // skip cleanup closes its captured local correctly (the captured local is
    // declared directly in the loop body, so it is a body local above the loop
    // floor — not a value-block floor local).
    let src = "\
acc := []
each i in [1,2] {
  x := i * 100
  g := fun(){ x }
  option { i == 2 -> { skip }, True -> { acc = push(acc, g) } }
}
out := []
each f in acc { out = push(out, f()) }
out";
    // i=1 captured (100), i=2 skipped.
    assert_parity(src, "[100]");
}

#[test]
fn skip_in_loop_with_captured_body_local_then_skip() {
    // Capture happens BEFORE the skip in the same iteration: i=1 captures then
    // skips (closure snapshots 100), i=2 captures via the non-skip path (200).
    let src = "\
acc := []
each i in [1,2] {
  x := i * 100
  g := fun(){ x }
  acc = push(acc, g)
  option { i == 1 -> { skip }, True -> {} }
}
out := []
each f in acc { out = push(out, f()) }
out";
    assert_parity(src, "[100, 200]");
}

// ── Captured FLOOR local in a value block (the original BUG A, now fixed) ────
//
// These encode the original BUG A and its no-skip variant. The complete fix is
// the slot-indexed `CloseUpvalueAt` opcode emitted by `end_scope_keeping_value`
// and implemented in both the VM interpreter and the JIT; both backends now
// agree on the asserted values.

#[test]
fn floor_captured_local_skip_bug_a() {
    let src = "\
acc := []
each i in [1,2] {
  option { True -> {
    x := i * 100
    g := fun(){ x }
    option { i == 2 -> { skip }, True -> { acc = push(acc, g) } }
  } }
}
out := []
each f in acc { out = push(out, f()) }
out";
    assert_parity(src, "[100]");
}

#[test]
fn floor_captured_local_value_block_no_skip() {
    // No skip at all — pure value-block corruption with a captured FLOOR local.
    let src = "\
acc := []
each i in [1,2] {
  option { True -> {
    x := i * 100
    g := fun(){ x }
    option { True -> { acc = push(acc, g) } }
  } }
}
out := []
each f in acc { out = push(out, f()) }
out";
    assert_parity(src, "[100, 200]");
}

// ── Function-scoped captured FLOOR local in a value block ────────────────────
//
// A different shape from the loop cases above: the captured-floor value block is
// the body of a RETURNING function, with no arrays or iteration. This is the
// only otherwise-JIT-eligible carrier of `CloseUpvalueAt` (arrays/`each` already
// force interpreter fallback). Because the JIT scan does not allow-list
// `CloseUpvalueAt`, calling `f` hot under threshold 1 falls back to the
// interpreter rather than running `slot_types` virtualization over the
// value-block cleanup — and all three backends agree.

#[test]
fn function_returning_captured_floor_value_block() {
    // `x` is the option arm's floor local, captured by `g`; the arm's value is
    // `g()` (== x). `f(n)` must return `n * 2` on every backend, including when
    // the JIT threshold makes `f` a hot call.
    let src = "\
fun f(n) {
  r := option { True -> {
    x := n * 2
    g := fun(){ x }
    g()
  } }
  give r
}
acc := 0
acc = f(10)
acc = f(20)
acc = f(30)
acc";
    assert_parity(src, "60");
}
