//! Typed zero-init heap defaults are fresh per initialization, and runtime
//! errors carry hints.
//!
//! - `arr <array>` declared with a type and no initializer yields a fresh
//!   collection per initialization, so VM(interp) == VM(JIT).
//! - Divide-by-zero and modulo-by-zero runtime errors carry the `  = hint:` line.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Run a program on the bytecode VM. `jit_threshold == Some(1)` forces the
/// JIT path; `None` keeps it on the interpreter.
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

/// Assert VM(interp) == VM(JIT), both equal to `expected`.
fn assert_parity(source: &str, expected: &str) {
    let interp = run_vm(source, None).expect("VM interp should succeed");
    let jit = run_vm(source, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, expected, "VM interp mismatch");
    assert_eq!(jit, expected, "VM JIT mismatch");
    assert_eq!(interp, jit, "VM interp != VM JIT");
}

#[test]
fn typed_array_default_is_fresh_per_call() {
    let src = r#"
fun b(){
  arr <array>
  arr = push(arr, 1)
  len(arr)
}
b() + b() + b()
"#;
    assert_parity(src, "3");
}

#[test]
fn typed_map_default_is_fresh_per_call() {
    let src = r#"
fun b(){
  m <map>
  m = insert(m, "k", 1)
  len(m)
}
b() + b() + b()
"#;
    assert_parity(src, "3");
}

#[test]
fn typed_set_default_is_fresh_per_call() {
    let src = r#"
fun b(){
  s <set>
  s = insert(s, 7)
  len(s)
}
b() + b() + b()
"#;
    let interp = run_vm(src, None).expect("VM interp should succeed");
    let jit = run_vm(src, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, "3", "VM interp: set default aliased across calls");
    assert_eq!(jit, "3", "VM JIT: set default aliased across calls");
}

#[test]
fn typed_array_default_fresh_under_jit_longer_chain() {
    let src = r#"
fun b(){
  arr <array>
  arr = push(arr, 1)
  len(arr)
}
b() + b() + b() + b() + b()
"#;
    let interp = run_vm(src, None).expect("VM interp should succeed");
    let jit = run_vm(src, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, "5", "VM interp: array default aliased across calls");
    assert_eq!(jit, "5", "VM JIT: array default aliased across calls");
    assert_eq!(interp, jit, "VM interp != VM JIT");
}

#[test]
fn divide_by_zero_hint_matches_tree_walker() {
    let src = "1 / 0\n";
    let vm_err = run_vm(src, None).expect_err("1/0 must error on the VM");
    let jit_err = run_vm(src, Some(1)).expect_err("1/0 must error under JIT");

    for (label, msg) in [("interp", &vm_err), ("jit", &jit_err)] {
        assert!(
            msg.contains("division by zero"),
            "VM {label} dropped the divide-by-zero message: {msg:?}"
        );
        assert!(
            msg.contains("ensure the divisor is not zero before dividing"),
            "VM {label} dropped the divide-by-zero hint: {msg:?}"
        );
    }
}

#[test]
fn modulo_by_zero_hint_matches_tree_walker() {
    let src = "5 % 0\n";
    let vm_err = run_vm(src, None).expect_err("5%0 must error on the VM");
    let jit_err = run_vm(src, Some(1)).expect_err("5%0 must error under JIT");

    for (label, msg) in [("interp", &vm_err), ("jit", &jit_err)] {
        assert!(
            msg.contains("modulo by zero"),
            "VM {label} dropped the modulo-by-zero message: {msg:?}"
        );
        assert!(
            msg.contains("ensure the divisor is not zero before using %"),
            "VM {label} dropped the modulo-by-zero hint: {msg:?}"
        );
    }
}

#[test]
fn operator_type_mismatch_hint_matches_tree_walker() {
    let src = "5 + \"x\"\n";
    let vm_err = run_vm(src, None).expect_err("5 + string must error on the VM");

    assert!(
        vm_err.contains("operands must be the same type for this operator"),
        "VM operator-mismatch hint missing: {vm_err:?}"
    );
}
