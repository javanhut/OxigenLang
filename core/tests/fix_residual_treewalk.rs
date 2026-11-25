//! VM/JIT tests for:
//!
//!   - A method parameter that shadows a field name does not clobber the
//!     field via the post-method field write-back.
//!   - Deep recursion returns a graceful runtime error ("stack overflow").
//!   - A `skip`/`stop` that escapes every enclosing loop into a value context
//!     is a compile/runtime error.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

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
    let interp = run_vm(R1_SHADOW_SRC, None).expect("VM interp should succeed");
    let jit = run_vm(R1_SHADOW_SRC, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, "10", "VM interp: field should stay 10");
    assert_eq!(jit, "10", "VM JIT: field should stay 10");
    assert_eq!(interp, jit);
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
    let interp = run_vm(R1_MUTATOR_SRC, None).expect("VM interp should succeed");
    let jit = run_vm(R1_MUTATOR_SRC, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, "30", "VM interp: field should update to 30");
    assert_eq!(jit, "30", "VM JIT: field should update to 30");
    assert_eq!(interp, jit);
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
    let interp = run_vm(R1_SELF_MUTATOR_SRC, None).expect("VM interp should succeed");
    assert_eq!(interp, "15", "VM interp: self.field mutation must persist");
}

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
    let out = std::thread::Builder::new()
        .stack_size(256 << 20)
        .spawn(|| run_vm(R2_DEEP_SRC, None))
        .expect("failed to spawn big-stack thread")
        .join()
        .expect("VM recursion thread should NOT abort (it must return a graceful error)");
    assert!(
        out.as_ref().is_err_and(|e| e.contains("stack overflow")),
        "deep recursion should yield a graceful 'stack overflow' error, got: {out:?}"
    );
}

#[test]
fn r2_moderate_recursion_still_correct() {
    let interp = run_vm(R2_MODERATE_SRC, None).expect("VM interp should succeed");
    let jit = run_vm(R2_MODERATE_SRC, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, "0");
    assert_eq!(jit, "0");
}

const R3A_STOP_VALUE_SRC: &str = r#"
a := option { 1 >= 0 -> { stop } }
println(a)
"#;

#[test]
fn r3a_stop_as_value_errors_on_all_backends() {
    assert!(
        run_vm(R3A_STOP_VALUE_SRC, None)
            .unwrap_err()
            .contains("E0016"),
        "VM interp should reject stop-as-value, got: {:?}",
        run_vm(R3A_STOP_VALUE_SRC, None)
    );
    assert!(
        run_vm(R3A_STOP_VALUE_SRC, Some(1)).is_err(),
        "VM JIT should reject stop-as-value, got Ok({:?})",
        run_vm(R3A_STOP_VALUE_SRC, Some(1)).ok()
    );
}

const R3A_SKIP_VALUE_SRC: &str = r#"
a := option { 1 >= 0 -> { skip } }
println(a)
"#;

#[test]
fn r3a_skip_as_value_errors_on_vm() {
    assert!(
        run_vm(R3A_SKIP_VALUE_SRC, None)
            .unwrap_err()
            .contains("E0016"),
        "VM should reject skip-as-value, got: {:?}",
        run_vm(R3A_SKIP_VALUE_SRC, None)
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
    let interp = run_vm(R3A_NORMAL_SKIP_LOOP_SRC, None).expect("VM interp should succeed");
    let jit = run_vm(R3A_NORMAL_SKIP_LOOP_SRC, Some(1)).expect("VM JIT should succeed");
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
    let interp = run_vm(R3A_NORMAL_STOP_LOOP_SRC, None).expect("VM interp should succeed");
    let jit = run_vm(R3A_NORMAL_STOP_LOOP_SRC, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, jit, "VM interp != VM JIT for normal stop loop");
}
