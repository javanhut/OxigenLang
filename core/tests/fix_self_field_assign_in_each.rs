//! Tests that an explicit `self.<field> = ...` mutation inside an `each` loop in
//! a struct method persists as seen by the caller, on the interpreter and the
//! JIT.

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

/// Assert VM (interp) == VM (JIT) == `expected`.
fn assert_parity(source: &str, expected: &str) {
    let interp = run_vm(source, None).expect("VM interp should succeed");
    let jit = run_vm(source, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, expected, "VM interp mismatch");
    assert_eq!(jit, expected, "VM JIT mismatch");
    assert_eq!(interp, jit, "VM interp != VM JIT");
}

#[test]
fn self_field_assign_in_each_persists() {
    let src = r#"
struct W { n <int> }
W includes { fun tick(){ each i in [5] { self.n = self.n + i } } }
w <W> := W(0)
w.tick()
w.n
"#;
    assert_parity(src, "5");
}

#[test]
fn self_field_assign_in_each_multiple_iterations() {
    let src = r#"
struct W { n <int> }
W includes { fun tick(){ each i in [1, 2, 3, 4] { self.n = self.n + i } } }
w <W> := W(0)
w.tick()
w.n
"#;
    assert_parity(src, "10");
}

#[test]
fn self_field_assign_in_repeat_still_persists() {
    let src = r#"
struct C { n <int> }
C includes { fun run(){ repeat when self.n < 3 { self.n = self.n + 1 } } }
c <C> := C(0)
c.run()
c.n
"#;
    assert_parity(src, "3");
}

#[test]
fn bare_name_field_mutation_in_each_still_persists() {
    let src = r#"
struct B { n <int> }
B includes { fun tick(){ each i in [2, 3] { n = n + i } } }
b <B> := B(0)
b.tick()
b.n
"#;
    assert_parity(src, "5");
}

#[test]
fn param_shadowed_field_left_untouched_in_each() {
    let src = r#"
struct S { n <int> }
S includes { fun bump(n <int>){ each i in [10] { n = n + i } } }
s <S> := S(7)
s.bump(100)
s.n
"#;
    assert_parity(src, "7");
}

#[test]
fn self_field_assign_outside_loop_still_persists() {
    let src = r#"
struct P { n <int> }
P includes { fun set(){ self.n = 42 } }
p <P> := P(0)
p.set()
p.n
"#;
    assert_parity(src, "42");
}

#[test]
fn self_field_assign_in_each_two_fields() {
    let src = r#"
struct T {
  a <int>
  b <int>
}
T includes {
  fun go(){
    each i in [1, 2] {
      self.a = self.a + i
      self.b = self.b + (i * 10)
    }
  }
}
t <T> := T(0, 0)
t.go()
[t.a, t.b]
"#;
    assert_parity(src, "[3, 30]");
}
