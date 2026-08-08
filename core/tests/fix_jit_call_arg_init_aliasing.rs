//! Regression tests for a JIT miscompile that swapped a call's first two
//! arguments.
//!
//! `slot_types` records "the `Constant` at ip N initializes local slot S" so
//! the JIT can virtualize that local. A call's arguments are transients, not
//! initializers — but the guard that rejects them only inspects the
//! *immediately following* opcode, and in
//!
//!     Constant 1; Constant 2; Constant 3; Call 3
//!
//! only the last argument is followed by the `Call`. Arguments 1 and 2 are each
//! followed by another `Constant`, so both were recorded as initializers for
//! whatever locals later occupied those stack positions. When the same chunk
//! also declared locals there — which an `each` loop does, via its
//! `__iterable__`/`__index__` slots — the JIT materialized the wrong values and
//! the call received its first two arguments transposed.
//!
//! Silent wrong answers, no error and no bailout, so these tests compare the
//! two backends directly rather than asserting on the JIT alone.
//!
//! The fix drops stale `first_init_ip` entries for positions vacated by a call,
//! matching what `BuildArray`/`BuildTuple`/... already did.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Runs `source` on both backends and asserts they agree. Returns the value.
fn assert_backends_agree(source: &str) -> String {
    let compile = || {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer, source);
        let program = parser.parse_program();
        assert!(
            parser.errors().is_empty(),
            "parser errors:\n{}",
            parser.format_errors()
        );
        Compiler::new()
            .compile(&program)
            .unwrap_or_else(|errs| panic!("compile errors: {errs:?}"))
    };
    let interp = VM::new_interpreter()
        .run(compile())
        .map(|v| format!("{v}"))
        .unwrap_or_else(|e| e.message);
    let jit = VM::new_eager_jit()
        .run(compile())
        .map(|v| format!("{v}"))
        .unwrap_or_else(|e| e.message);
    assert_eq!(interp, jit, "backends disagree for:\n{source}");
    interp
}

#[test]
fn call_args_keep_their_order_when_the_chunk_has_an_each_loop() {
    // The original repro. The `each` loop never touches `v` — merely declaring
    // locals at the argument positions was enough to corrupt the call.
    let out = assert_backends_agree(
        "fun m(a, b, c) { [a, b, c] }\nv := m(1, 2, 3)\neach z in [0] { }\nv",
    );
    assert_eq!(out, "[1, 2, 3]");
}

#[test]
fn every_parameter_receives_its_own_argument() {
    // Pins the whole mapping, not just "the array looks right": the defect
    // exchanged the *values* of parameters 1 and 2 wherever they were read.
    for (returns, expected) in [("a", "1"), ("b", "2"), ("c", "3"), ("d", "4")] {
        let out = assert_backends_agree(&format!(
            "fun m(a, b, c, d) {{ {returns} }}\nv := m(1, 2, 3, 4)\neach z in [0] {{ }}\nv"
        ));
        assert_eq!(out, expected, "parameter {returns} got the wrong argument");
    }
}

#[test]
fn argument_order_holds_across_arities() {
    // Two arguments never tripped it (the second is followed by the Call, so
    // the old guard caught it); three or more did.
    for argc in 2..=6 {
        let params: Vec<String> = (0..argc).map(|i| format!("p{i}")).collect();
        let args: Vec<String> = (0..argc).map(|i| (i + 1).to_string()).collect();
        let out = assert_backends_agree(&format!(
            "fun m({}) {{ [{}] }}\nv := m({})\neach z in [0] {{ }}\nv",
            params.join(", "),
            params.join(", "),
            args.join(", ")
        ));
        assert_eq!(out, format!("[{}]", args.join(", ")), "arity {argc}");
    }
}

#[test]
fn builtin_calls_keep_their_argument_order() {
    // The defect hit builtins too: `range(0, 3, s)` was invoked as
    // `range(3, 0, 1)`, yielding an empty array and a loop body that never ran.
    let out = assert_backends_agree(
        "s := 1\nacc <str> := \"\"\neach i in range(0, 3, s) { acc = acc + \"{i},\" }\nacc",
    );
    assert_eq!(out, "0,1,2,");

    let down = assert_backends_agree(
        "s := -1\nacc <str> := \"\"\neach i in range(3, 0, s) { acc = acc + \"{i},\" }\nacc",
    );
    assert_eq!(down, "3,2,1,");
}

#[test]
fn nested_and_repeated_calls_keep_their_argument_order() {
    let out = assert_backends_agree(
        "fun add3(a, b, c) { a + b * 10 + c * 100 }\nv := add3(1, 2, 3) + add3(4, 5, 6)\neach z in [0] { }\nv",
    );
    // 1 + 20 + 300 = 321, 4 + 50 + 600 = 654
    assert_eq!(out, "975");
}

#[test]
fn method_calls_keep_their_argument_order() {
    let out = assert_backends_agree(
        "struct S { n <int> }\nS includes {\n  fun pick(a, b, c) { [a, b, c] }\n}\ns := S(1)\nv := s.pick(1, 2, 3)\neach z in [0] { }\nv",
    );
    assert_eq!(out, "[1, 2, 3]");
}

#[test]
fn genuine_constant_locals_are_still_virtualized() {
    // The fix drops stale init records; it must not drop real ones, which is
    // what makes int locals virtualizable. A hot counting loop over constant
    // -initialized locals must still compute correctly on both backends.
    let out = assert_backends_agree(
        "fun run() {\n  total <int> := 0\n  step <int> := 3\n  each i in range(1000) { total = total + i * step }\n  total\n}\nrun()",
    );
    assert_eq!(out, "1498500");
}
