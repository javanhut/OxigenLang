//! TS1: Tuple slicing VM regression.
//!
//! `(1,2,3,4)[1:3]` must evaluate to `(2, 3)`, but the VM/JIT previously
//! errored with `cannot slice TUPLE` even though its own hint advertised that
//! slicing works on tuples. This test pins VM (interp) == VM (JIT) for a spread
//! of tuple slices, including open and negative bounds, empty/over-range
//! results.
//!
//! INCR_IMMUT: `x <int> = 5; x++` must error on every backend (`=` declares an
//! immutable typed var; `++` cannot mutate it).

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

/// Assert VM (interp) == VM (JIT) == `expected`.
fn assert_parity(source: &str, expected: &str) {
    let interp = run_vm(source, None).expect("VM interp should succeed");
    let jit = run_vm(source, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, expected, "VM interp mismatch for: {source}");
    assert_eq!(jit, expected, "VM JIT mismatch for: {source}");
    assert_eq!(interp, jit, "VM interp != VM JIT for: {source}");
}

#[test]
fn tuple_slice_basic() {
    assert_parity("(1,2,3,4)[1:3]", "(2, 3)");
}

#[test]
fn tuple_slice_open_start() {
    // `[:2]` -> first two elements.
    assert_parity("(1,2,3)[:2]", "(1, 2)");
}

#[test]
fn tuple_slice_open_end() {
    // `[1:]` -> from index 1 to the end.
    assert_parity("(1,2,3)[1:]", "(2, 3)");
}

#[test]
fn tuple_slice_open_both() {
    // `[:]` -> a copy of the whole tuple.
    assert_parity("(1,2,3)[:]", "(1, 2, 3)");
}

#[test]
fn tuple_slice_negative_start() {
    // The VM converts the bound with `n as usize` BEFORE clamping, so a
    // negative start wraps to a huge index and yields an empty tuple (it does
    // NOT count from the end).
    assert_parity("(1,2,3,4)[-2:]", "()");
}

#[test]
fn tuple_slice_negative_end() {
    // A negative end wraps to a huge index, clamps to len, leaving the whole
    // tuple from `start` (here start defaults to 0).
    assert_parity("(1,2,3,4)[:-1]", "(1, 2, 3, 4)");
}

#[test]
fn tuple_slice_negative_both() {
    // Negative start wraps huge; `s > e` => empty.
    assert_parity("(1,2,3,4,5)[-3:-1]", "()");
}

#[test]
fn tuple_slice_empty_inverted() {
    // start > end yields an empty tuple, just like arrays.
    assert_parity("(1,2,3)[2:1]", "()");
}

#[test]
fn tuple_slice_over_range_end() {
    // end past the length is clamped to the length.
    assert_parity("(1,2,3)[1:99]", "(2, 3)");
}

#[test]
fn tuple_slice_start_at_len() {
    // start at the length yields an empty tuple.
    assert_parity("(1,2,3)[3:]", "()");
}

#[test]
fn tuple_slice_full_explicit() {
    assert_parity("(10,20,30)[0:3]", "(10, 20, 30)");
}

#[test]
fn tuple_slice_mixed_elements() {
    // Slicing preserves heterogeneous element values/types.
    assert_parity("(1, \"a\", True, 4)[1:3]", "(a, True)");
}

// ── INCR_IMMUT ──────────────────────────────────────────────────────────────

const INCR_IMMUT_SRC: &str = "x <int> = 5\nx++\n";

#[test]
fn incr_immut_vm_errors() {
    // `=` declares an immutable typed binding; `++` cannot mutate it. The VM
    // (and JIT, which inherits the store op) must reject it.
    let interp = run_vm(INCR_IMMUT_SRC, None);
    assert!(
        interp.is_err(),
        "VM interp should error on incrementing an immutable, got: {interp:?}"
    );
    let jit = run_vm(INCR_IMMUT_SRC, Some(1));
    assert!(
        jit.is_err(),
        "VM JIT should error on incrementing an immutable, got: {jit:?}"
    );
    let msg = interp.unwrap_err();
    assert!(
        msg.contains("immutable variable 'x'"),
        "VM message should mention the immutable variable, got: {msg}"
    );
}

#[test]
fn incr_immut_wording() {
    // The VM's `handle_set_global` path is shared with plain `=` reassignment,
    // so the increment-on-immutable error reads `cannot reassign immutable
    // variable '...'`.
    let vm_msg = run_vm(INCR_IMMUT_SRC, None).unwrap_err();
    assert!(
        vm_msg.contains("cannot reassign immutable variable 'x'"),
        "VM wording drifted, got: {vm_msg}"
    );
}
