//! V5 regression + runtime-error-hint parity.
//!
//! V5: a typed zero-init heap default — `arr <array>` declared with a type but
//! NO initializer — used to be produced once by the compiler and parked in the
//! function's constant pool. A plain `clone()` of that constant on every call
//! shared the SAME `Rc<RefCell<Vec<Value>>>`, so an `arr = push(arr, 1)` in one
//! call leaked into the next: `b()` called three times printed `1, 2, 3`
//! instead of `1, 1, 1`. The fix materializes a FRESH collection per
//! initialization (`Value::fresh_clone` at the `Constant` opcode and in
//! `current_constant`, which the JIT's `jit_push_constant` helper routes
//! through). The tree-walker (the maturity oracle) prints `1, 1, 1`, so
//! VM(interp) == VM(JIT) == tree-walker is the bar.
//!
//! Error-hint parity: the VM used to drop the `  = hint:` line on
//! divide-by-zero / modulo-by-zero runtime errors. It must now carry the same
//! hint text the tree-walker produces so file-vs-REPL output agrees.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::evaluator::Evaluator;
use oxigen_core::lexer::Lexer;
use oxigen_core::object::environment::Environment;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;
use std::cell::RefCell;
use std::rc::Rc;

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

/// Run a program on the tree-walking interpreter (the reference oracle),
/// returning the final value as a string.
fn run_tree(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    let env = Rc::new(RefCell::new(Environment::new()));
    let mut evaluator = Evaluator::new();
    format!("{}", evaluator.eval_program(&program, env))
}

/// Run a program on the tree-walker and return its error string (the program
/// is expected to fail at runtime). Panics if it unexpectedly succeeds.
fn run_tree_err(source: &str) -> String {
    use oxigen_core::object::Object;
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    let env = Rc::new(RefCell::new(Environment::new()));
    let mut evaluator = Evaluator::new();
    let result = evaluator.eval_program(&program, env);
    match result.as_ref() {
        Object::Error(msg) => msg.clone(),
        other => panic!("expected a tree-walker runtime error, got {:?}", other),
    }
}

/// Assert VM(interp) == VM(JIT) == tree-walker, all equal to `expected`.
fn assert_parity(source: &str, expected: &str) {
    let interp = run_vm(source, None).expect("VM interp should succeed");
    let jit = run_vm(source, Some(1)).expect("VM JIT should succeed");
    let tree = run_tree(source);
    assert_eq!(interp, expected, "VM interp mismatch");
    assert_eq!(jit, expected, "VM JIT mismatch");
    assert_eq!(tree, expected, "tree-walker mismatch");
    assert_eq!(interp, jit, "VM interp != VM JIT");
    assert_eq!(interp, tree, "VM interp != tree-walker");
}

// ── V5: typed zero-init heap defaults are fresh per initialization ──────────
//
// IMPORTANT (test shape): the V5 functions below use a FALL-THROUGH return
// (the last expression is the result) rather than an early `give`. An early
// `give` inside a JIT-compiled function trips a SEPARATE, pre-existing JIT
// codegen abort ("you cannot add an instruction to a block already filled" in
// `jit::engine::emit_store_current_loc`) that reproduces with a plain `clone()`
// — i.e. WITHOUT this V5 fix — so it is unrelated to V5 and lives in a JIT
// engine file this lane does not own. The fall-through shape JIT-compiles and
// runs cleanly under a forced threshold, so it lets us assert the V5 fix on
// BOTH the interpreter and the JIT (`current_constant`/`jit_push_constant`
// fresh-clone) without entangling that JIT bug.

#[test]
fn typed_array_default_is_fresh_per_call() {
    // The canonical V5 repro: `arr <array>` declared with no initializer, then
    // mutated. Each call must start from an EMPTY array. Returning
    // `b() + b() + b()` is `1 + 1 + 1 = 3` when fresh, `1 + 2 + 3 = 6` when
    // the heap default is aliased across calls. `assert_parity` runs this on
    // the interpreter, on a forced-JIT threshold, and on the tree-walker
    // oracle.
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
    // Same aliasing hazard for a typed `<map>` default.
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
    // Same aliasing hazard for a typed `<set>` default. The VM's `insert`
    // builtin is 3-arg (map-shaped) and accepts a set as its first argument
    // (the value arg is ignored for sets); the tree-walker's `insert` is
    // map-only, so this is a VM(interp) == VM(JIT) check rather than a
    // tree-walker parity check. A fresh set per call means each `len(s)` is 1,
    // so `b() + b() + b() == 3`; an aliased set default would give `1 + 2 + 3`.
    let src = r#"
fun b(){
  s <set>
  s = insert(s, 7, 7)
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
    // A longer (5-call) chain under a forced JIT threshold so the JIT path's
    // fresh-clone of the typed-array constant is exercised across several
    // compiled invocations. Fresh array per call ⇒ `len == 1` ⇒ chain == 5;
    // an aliased heap default would give `1 + 2 + 3 + 4 + 5 = 15`.
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
    let tree = run_tree(src);
    assert_eq!(interp, "5", "VM interp: array default aliased across calls");
    assert_eq!(jit, "5", "VM JIT: array default aliased across calls");
    assert_eq!(tree, "5", "tree-walker (oracle) should also give 5");
    assert_eq!(interp, jit, "VM interp != VM JIT");
}

// ── Error-hint parity: VM hint text must match the tree-walker ──────────────

#[test]
fn divide_by_zero_hint_matches_tree_walker() {
    let src = "1 / 0\n";
    let vm_err = run_vm(src, None).expect_err("1/0 must error on the VM");
    let jit_err = run_vm(src, Some(1)).expect_err("1/0 must error under JIT");
    let tree_err = run_tree_err(src);

    // The tree-walker's canonical message + hint for divide-by-zero.
    assert!(
        tree_err.contains("division by zero"),
        "tree-walker message changed: {tree_err:?}"
    );
    assert!(
        tree_err.contains("ensure the divisor is not zero before dividing"),
        "tree-walker hint changed: {tree_err:?}"
    );

    // The VM (interp + JIT) must now carry the SAME message and hint text.
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
    let tree_err = run_tree_err(src);

    assert!(
        tree_err.contains("modulo by zero"),
        "tree-walker message changed: {tree_err:?}"
    );
    assert!(
        tree_err.contains("ensure the divisor is not zero before using %"),
        "tree-walker hint changed: {tree_err:?}"
    );

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
    // INTEGER + STRING is a type mismatch in both backends. The hint wording
    // must agree so file vs REPL output is consistent.
    let src = "5 + \"x\"\n";
    let vm_err = run_vm(src, None).expect_err("5 + string must error on the VM");
    let tree_err = run_tree_err(src);

    assert!(
        tree_err.contains("operands must be the same type for this operator"),
        "tree-walker hint changed: {tree_err:?}"
    );
    assert!(
        vm_err.contains("operands must be the same type for this operator"),
        "VM operator-mismatch hint does not match tree-walker: {vm_err:?}"
    );
}
