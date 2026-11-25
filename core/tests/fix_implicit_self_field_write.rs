//! Tests that a bare-name assignment inside a method, where the name is a
//! field of the enclosing struct, resolves to `self.field = ...`. VM
//! (interp), VM (JIT) and the expected value must agree.

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

#[test]
fn implicit_self_write_scale_then_area() {
    let src = r#"
struct C { radius <float> }
C includes {
  fun area(){ 3.14 * radius * radius }
  fun scale(f <float>){ radius = radius * f }
}
c := C(2.0)
c.scale(2.0)
c.area()
"#;
    assert_parity(src, "50.24");
}

#[test]
fn implicit_self_write_loop_increments_field() {
    let src = r#"
struct Ctr { v <int> }
Ctr includes {
  fun inc(){ v = v + 1 }
  fun get(){ v }
}
c := Ctr(0)
c.inc()
c.inc()
c.inc()
c.inc()
c.inc()
c.get()
"#;
    assert_parity(src, "5");
}

#[test]
fn implicit_self_write_getter_setter_pair() {
    let src = r#"
struct Box { val <int> }
Box includes {
  fun setv(n <int>){ val = n }
  fun getv(){ val }
}
b := Box(1)
b.setv(99)
b.getv()
"#;
    assert_parity(src, "99");
}

#[test]
fn implicit_self_write_param_shadows_field() {
    let src = r#"
struct P { radius <int> }
P includes {
  fun touch(radius <int>){ radius = radius + 1 }
  fun get(){ radius }
}
p := P(10)
p.touch(100)
p.get()
"#;
    let interp = run_vm(src, None).expect("VM interp should succeed");
    let jit = run_vm(src, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, "10", "VM interp: param must shadow field on write");
    assert_eq!(jit, "10", "VM JIT: param must shadow field on write");
    assert_eq!(interp, jit, "VM interp != VM JIT (param-shadow write)");
}

#[test]
fn implicit_self_write_compound_with_outer_local() {
    let src = r#"
struct Acc { x <int> }
Acc includes {
  fun bump(){ x = x + 1 }
  fun val(){ x }
}
a := Acc(41)
a.bump()
a.val()
"#;
    assert_parity(src, "42");
}

#[test]
fn implicit_self_write_does_not_clobber_global() {
    let src = r#"
g := 1
struct C { r <int> }
C includes {
  fun set(){ g = 5 }
  fun rr(){ r }
}
c := C(7)
c.set()
g
"#;
    assert_parity(src, "5");
}

#[test]
fn implicit_self_write_field_wins_over_same_named_global() {
    let src = r#"
n := 7
struct S { n <int> }
S includes {
  fun set(){ n = 50 }
  fun real(){ n }
}
s := S(1)
s.set()
s.real() + n
"#;
    assert_parity(src, "57");
}
