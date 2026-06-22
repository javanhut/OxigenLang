//! Residual tree-walker parity fixes (R1/R2/R3a). For each, the VM/JIT are the
//! correct reference and the tree-walker was behind:
//!
//!   R1  A method parameter that SHADOWS a field name must not clobber the
//!       field via the post-method field write-back. The param wins inside the
//!       method body, but the underlying field is left untouched (VM/JIT give
//!       the original field value; the tree-walker used to write the param's
//!       final value back).
//!   R2  Deep recursion under the tree-walker must return a graceful runtime
//!       error ("stack overflow") instead of SIGABRT-ing the native stack.
//!   R3a A `skip`/`stop` that escapes every enclosing loop into a value context
//!       (Let RHS / top-level program) must become a runtime error, matching
//!       the VM/JIT which reject `skip`/`stop` used outside a loop.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::evaluator::Evaluator;
use oxigen_core::lexer::Lexer;
use oxigen_core::object::environment::Environment;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;
use std::cell::RefCell;
use std::rc::Rc;

/// Run a program on the bytecode VM. `jit_threshold == Some(1)` forces the JIT
/// path; `None` keeps it on the interpreter. Returns Ok(output) on success or
/// Err(message) on a runtime/compile error.
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

/// Run a program on the tree-walking interpreter. Returns the stringified
/// result object (errors stringify to their message text).
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

// ── R1: param shadowing a field must not clobber the field ──────────────────

const R1_SHADOW_SRC: &str = r#"
struct P {
  radius <int>
}
P includes {
  fun touch(radius <int>) {
    radius = radius + 1
  }
}
p := P(10)
p.touch(100)
p.radius
"#;

#[test]
fn r1_param_shadowing_field_does_not_clobber() {
    // Param `radius` wins inside the method; the field is untouched -> 10.
    let interp = run_vm(R1_SHADOW_SRC, None).expect("VM interp should succeed");
    let jit = run_vm(R1_SHADOW_SRC, Some(1)).expect("VM JIT should succeed");
    let tree = run_tree(R1_SHADOW_SRC);
    assert_eq!(interp, "10", "VM interp: field should stay 10");
    assert_eq!(jit, "10", "VM JIT: field should stay 10");
    assert_eq!(tree, "10", "tree-walker: field should stay 10 (not 101)");
    assert_eq!(interp, jit);
    assert_eq!(interp, tree, "VM interp != tree-walker");
}

const R1_MUTATOR_SRC: &str = r#"
struct P {
  radius <int>
}
P includes {
  fun scale(f <int>) {
    radius = radius * f
  }
}
p := P(10)
p.scale(3)
p.radius
"#;

#[test]
fn r1_non_shadowing_bare_name_mutator_still_persists() {
    // A bare-name field write that does NOT shadow a param must still persist.
    let interp = run_vm(R1_MUTATOR_SRC, None).expect("VM interp should succeed");
    let jit = run_vm(R1_MUTATOR_SRC, Some(1)).expect("VM JIT should succeed");
    let tree = run_tree(R1_MUTATOR_SRC);
    assert_eq!(interp, "30", "VM interp: field should update to 30");
    assert_eq!(jit, "30", "VM JIT: field should update to 30");
    assert_eq!(tree, "30", "tree-walker: bare-name mutator must still update");
    assert_eq!(interp, tree, "VM interp != tree-walker");
}

const R1_SELF_MUTATOR_SRC: &str = r#"
struct P {
  radius <int>
}
P includes {
  fun bump() {
    self.radius = self.radius + 5
  }
}
p := P(10)
p.bump()
p.radius
"#;

#[test]
fn r1_self_field_mutation_still_persists() {
    // `self.field = ...` must still persist (the fix only skips param-shadowed
    // names, not legitimate field writes).
    let tree = run_tree(R1_SELF_MUTATOR_SRC);
    assert_eq!(tree, "15", "tree-walker: self.field mutation must persist");
}

// ── R2: deep recursion -> graceful "stack overflow", not SIGABRT ────────────

const R2_DEEP_SRC: &str = r#"
fun f(n) {
  option {
    n <= 0 -> 0,
    f(n - 1)
  }
}
println(f(200000))
"#;

const R2_MODERATE_SRC: &str = r#"
fun f(n) {
  option {
    n <= 0 -> 0,
    f(n - 1)
  }
}
f(1000)
"#;

#[test]
fn r2_deep_recursion_errors_gracefully() {
    // Run on a large native stack (256 MB), mirroring the CLI's tree-walk
    // entry, so the recursion-DEPTH guard is the limit that fires (a graceful
    // "stack overflow" error) rather than the native stack overrunning first.
    // The default test-thread stack is far too small to reach the guard's
    // bound, so we spawn our own big-stack thread here; Integrate additionally
    // verifies rc=1 (not rc=134) via the CLI binary.
    let out = std::thread::Builder::new()
        .stack_size(256 << 20)
        .spawn(|| run_tree(R2_DEEP_SRC))
        .expect("failed to spawn big-stack thread")
        .join()
        .expect("tree-walk recursion thread should NOT abort (it must return a graceful error)");
    assert!(
        out.contains("stack overflow"),
        "deep recursion should yield a graceful 'stack overflow' error, got: {out}"
    );
}

#[test]
fn r2_moderate_recursion_still_correct() {
    // A moderate recursion well below the guard must still compute correctly on
    // all backends.
    let interp = run_vm(R2_MODERATE_SRC, None).expect("VM interp should succeed");
    let jit = run_vm(R2_MODERATE_SRC, Some(1)).expect("VM JIT should succeed");
    let tree = std::thread::Builder::new()
        .stack_size(256 << 20)
        .spawn(|| run_tree(R2_MODERATE_SRC))
        .expect("failed to spawn thread")
        .join()
        .expect("moderate recursion should succeed");
    assert_eq!(interp, "0");
    assert_eq!(jit, "0");
    assert_eq!(tree, "0", "tree-walker: moderate recursion must still work");
}

// ── R3a: skip/stop escaping a loop into a value context is an error ─────────

const R3A_STOP_VALUE_SRC: &str = r#"
a := option { 1 >= 0 -> { stop } }
println(a)
"#;

#[test]
fn r3a_stop_as_value_errors_on_all_backends() {
    // VM/JIT reject `stop` outside a loop at compile time; the tree-walker must
    // now also error (it used to print "stop").
    let interp = run_vm(R3A_STOP_VALUE_SRC, None);
    let jit = run_vm(R3A_STOP_VALUE_SRC, Some(1));
    assert!(
        interp.is_err(),
        "VM interp should reject stop-as-value, got Ok({:?})",
        interp.ok()
    );
    assert!(
        jit.is_err(),
        "VM JIT should reject stop-as-value, got Ok({:?})",
        jit.ok()
    );
    let tree = run_tree(R3A_STOP_VALUE_SRC);
    // The tree-walker now rejects with the SAME static skip/stop-as-value
    // diagnostic the bytecode compiler uses, so all three backends agree.
    assert!(
        tree.contains("cannot be used as a value"),
        "tree-walker should reject stop-as-value, got: {tree}"
    );
}

const R3A_SKIP_VALUE_SRC: &str = r#"
a := option { 1 >= 0 -> { skip } }
println(a)
"#;

#[test]
fn r3a_skip_as_value_errors_in_tree_walker() {
    let tree = run_tree(R3A_SKIP_VALUE_SRC);
    assert!(
        tree.contains("cannot be used as a value"),
        "tree-walker should reject skip-as-value, got: {tree}"
    );
}

const R3A_NORMAL_SKIP_LOOP_SRC: &str = r#"
each i in range(6) {
  skip when i % 2 == 0
  println(i)
}
"#;

#[test]
fn r3a_normal_skip_loop_still_works() {
    // skip INSIDE a loop must remain valid control flow on all backends.
    let interp = run_vm(R3A_NORMAL_SKIP_LOOP_SRC, None).expect("VM interp should succeed");
    let jit = run_vm(R3A_NORMAL_SKIP_LOOP_SRC, Some(1)).expect("VM JIT should succeed");
    let tree = run_tree(R3A_NORMAL_SKIP_LOOP_SRC);
    // No error in any backend.
    assert!(!tree.to_lowercase().contains("outside of loop"), "tree: {tree}");
    assert_eq!(interp, jit, "VM interp != VM JIT for normal skip loop");
}

const R3A_NORMAL_STOP_LOOP_SRC: &str = r#"
each i in range(10) {
  stop when i >= 3
  println(i)
}
"#;

#[test]
fn r3a_normal_stop_loop_still_works() {
    // stop INSIDE a loop must remain valid control flow on all backends.
    let interp = run_vm(R3A_NORMAL_STOP_LOOP_SRC, None).expect("VM interp should succeed");
    let jit = run_vm(R3A_NORMAL_STOP_LOOP_SRC, Some(1)).expect("VM JIT should succeed");
    let tree = run_tree(R3A_NORMAL_STOP_LOOP_SRC);
    assert!(!tree.to_lowercase().contains("outside of loop"), "tree: {tree}");
    assert_eq!(interp, jit, "VM interp != VM JIT for normal stop loop");
}
