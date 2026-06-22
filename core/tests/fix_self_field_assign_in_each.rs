//! Bug B regression: an explicit `self.<field> = ...` mutation inside an `each`
//! loop in a struct method must persist as seen by the caller.
//!
//! The tree-walker was the BUGGY backend here (VM/JIT were already correct).
//! The BoundMethod machinery binds each instance field as a method-env
//! variable and, after the body runs, writes each field's method-env value
//! back onto the instance. An `each` loop runs its body in a fresh
//! per-iteration env, so an explicit `self.field = ...` that previously did
//! `env.set(field, ..)` wrote into that throwaway env, leaving the method-env
//! binding stale; the write-back then clobbered the instance's mutated value.
//!
//! After the fix, `self.field = ...` updates the field's actual binding in the
//! method env (via scope-chain `update`), so the write-back is consistent.
//! The bar is tree-walker == VM(interp) == VM(JIT) == expected.

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

/// Run a program on the tree-walking interpreter.
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

/// Assert tree-walker == VM (interp) == VM (JIT) == `expected`.
fn assert_parity(source: &str, expected: &str) {
    let interp = run_vm(source, None).expect("VM interp should succeed");
    let jit = run_vm(source, Some(1)).expect("VM JIT should succeed");
    let tree = run_tree(source);
    assert_eq!(tree, expected, "tree-walker mismatch");
    assert_eq!(interp, expected, "VM interp mismatch");
    assert_eq!(jit, expected, "VM JIT mismatch");
    assert_eq!(tree, interp, "tree-walker != VM interp");
    assert_eq!(interp, jit, "VM interp != VM JIT");
}

#[test]
fn self_field_assign_in_each_persists() {
    // The canonical repro: `self.n = self.n + i` inside an `each` loop must
    // persist. Expected 5 (0 + 5); the tree-walker used to drop the mutation.
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
    // Multiple iterations accumulate: 1 + 2 + 3 + 4 = 10.
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
    // The `repeat`-loop variant must remain correct (it always was). Counts
    // up to 3.
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
    // The bare-name field write form (`n = ...` without `self.`) must keep
    // persisting through an `each` loop — this is the R1 fix, not to regress.
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
    // A parameter whose name shadows a field must NOT participate in the
    // write-back: assigning to it inside the method leaves the underlying
    // field untouched. Here `n` is a param shadowing field `n`; the field
    // keeps its original value (7).
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
    // The non-loop `self.field = ...` form must remain correct.
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
    // Two distinct fields each mutated via `self.field = ...` inside `each`.
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
    // a = 1 + 2 = 3, b = 10 + 20 = 30
    assert_parity(src, "[3, 30]");
}
