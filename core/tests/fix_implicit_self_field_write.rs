//! B12-write regression: a bare-name ASSIGNMENT inside a method whose name is
//! a field of the enclosing method's struct must resolve to `self.field = ...`
//! (implicit self).
//!
//! Before the fix, the implicit-self READ resolved (B12) but a bare-name WRITE
//! fell through to a global SetGlobal, so a mutating method left the struct
//! field untouched. The bar is VM(interp) == VM(JIT) == expected.

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
    // The canonical differential repro: `scale` writes the field via a bare
    // name (`radius = radius * f`), then `area` reads it. After scaling a
    // radius-2 circle by 2, area = 3.14 * 4 * 4 = 50.24.
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
    // struct_method_loop: a field-mutating loop method `inc(){ v = v + 1 }`
    // called 5 times leaves v == 5.
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
    // A getter/setter pair: setter writes the field via a bare name, getter
    // reads it back.
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
    // Resolution order: a method PARAMETER named like a field shadows the
    // field on the assignment TARGET, so `radius = radius + 1` writes the LOCAL
    // param (SetLocal), NOT the field (no implicit-self SetField). This is the
    // load-bearing B12-write property: a param wins over a field for the write,
    // exactly as it wins for the read (compile_identifier_inner).
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
    // The param wins on the write: the field is never assigned, so the getter
    // (an implicit-self READ) still sees the original 10 under VM and JIT.
    let interp = run_vm(src, None).expect("VM interp should succeed");
    let jit = run_vm(src, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, "10", "VM interp: param must shadow field on write");
    assert_eq!(jit, "10", "VM JIT: param must shadow field on write");
    assert_eq!(interp, jit, "VM interp != VM JIT (param-shadow write)");
}

#[test]
fn implicit_self_write_compound_with_outer_local() {
    // A compound `self.x = x + 1` mix where `x` on the RHS is the field (a
    // bare-name read), incremented and written back via a bare-name write.
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
    // A bare name that is a DECLARED GLOBAL (not a field) must still resolve to
    // the global on assignment — the implicit-self write only fires for fields.
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
    // When a GLOBAL shares the field's name, a bare-name WRITE in a method must
    // resolve to the FIELD (not the global), mirroring the implicit-self READ
    // order (local -> field -> upvalue -> global). `set()` writes the field, so
    // `real()` (the field) becomes 50 while the global `n` stays 7.
    // `real() + n` => 50 + 7 = 57 (a stale write-to-global bug would give 1+50=51).
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
