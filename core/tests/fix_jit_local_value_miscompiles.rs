//! Tests that JIT-compiled functions return the same value as the interpreter
//! for:
//!
//! 1. `self.f = self.f + x` directly inside a loop body.
//! 2. A local initialized from a non-constant (`val := n + 100`) and read from a
//!    conditional `option`/`choose` arm.
//!
//! All assertions check VM(interp) == VM(JIT, threshold 1).

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_vm(source: &str, jit_threshold: Option<u32>) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    let function = Compiler::new().compile(&program).expect("compile");
    let mut vm = VM::new();
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    format!("{}", vm.run(function).expect("run"))
}

/// VM(interp) == VM(JIT), both equal to `expected`.
fn assert_parity(src: &str, expected: &str) {
    let interp = run_vm(src, None);
    let jit = run_vm(src, Some(1));
    assert_eq!(interp, expected, "VM interp mismatch");
    assert_eq!(jit, expected, "VM JIT mismatch (the miscompile)");
}

#[test]
fn struct_field_add_accumulate_in_loop() {
    assert_parity(
        "struct Sum { acc <int> }\n\
         Sum includes { fun run(n <int>) { i <int> := 0\n\
           repeat when i < n { self.acc = self.acc + i\n i = i + 1 }\n self.acc } }\n\
         s <Sum> := Sum(0)\n s.run(10)",
        "45",
    );
}

#[test]
fn struct_field_add_in_loop_with_inheritance() {
    assert_parity(
        "struct A { a <int> }\nstruct B(A) { b <int> }\n\
         B includes { fun run(n <int>) { i <int> := 0\n\
           repeat when i < n { self.a = self.a + i\n i = i + 1 }\n self.a } }\n\
         x <B> := B(0, 0)\n x.run(10)",
        "45",
    );
}

#[test]
fn walrus_local_returned_from_conditional_option_arm() {
    assert_parity(
        "fun f(n) { val := n + 100\n option { n <= 0 -> val, 999 } }\n f(0)",
        "100",
    );
    assert_parity(
        "fun f(n) { val := n + 100\n option { n > 0 -> 999, val } }\n f(0)",
        "100",
    );
    assert_parity(
        "fun f(n) { a := n + 1\n b := n + 100\n option { n <= 0 -> b, 999 } }\n f(0)",
        "100",
    );
}

#[test]
fn recursion_with_a_local_combined_after_call() {
    assert_parity(
        "fun rec(n) { val := n\n option { n <= 0 -> 0, rec(n - 1) + val } }\n rec(10)",
        "55",
    );
    assert_parity(
        "fun a(n) { val := n\n option { n <= 0 -> 0, b(n - 1) + val } }\n\
         fun b(n) { val := n\n option { n <= 0 -> 0, a(n - 1) + val } }\n a(10)",
        "55",
    );
}

#[test]
fn non_loop_field_add_and_unconditional_arm_still_correct() {
    assert_parity(
        "struct S { v <int> }\n\
         S includes { fun run() { self.v = self.v + 1\n self.v = self.v + 2\n self.v } }\n\
         s <S> := S(0)\n s.run()",
        "3",
    );
    assert_parity(
        "fun f(n) { val := n + 100\n option { True -> val } }\n f(7)",
        "107",
    );
}
