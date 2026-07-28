//! A callee's entry-guard bailout must not be reported as the caller's.
//!
//! The JIT mirrors a parameter into a register when the bytecode suggests it is
//! used as an int, and guards that at entry: if the runtime tag isn't Integer,
//! the thunk bails (status 2) and the VM re-runs that call in the interpreter.
//! Bailouts are only safe because they happen at ip 0, before side effects.
//!
//! The inline call-IC dispatch treated *any* nonzero status from the callee's
//! thunk as an error and propagated it outward, so the callee's bailout became
//! the CALLER's exit status. The VM then re-ran the caller from ip 0 — in the
//! middle of its loop, after side effects — replaying part of the loop and
//! leaving its locals reading a stale counter. Passing a float to a parameter
//! the analyzer guessed was an int (`fun d(a, b) { a + b }` called with floats)
//! was enough to trigger it.
//!
//! Silent wrong numbers, so every test compares the two backends.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn compile(source: &str) -> oxigen_core::vm::value::Function {
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
}

fn run_on(mut vm: VM, source: &str) -> String {
    vm.run(compile(source))
        .map(|v| format!("{v}"))
        .unwrap_or_else(|e| e.message)
}

/// Asserts both backends agree, and that the answer is `expected`.
fn assert_backends_agree(source: &str, expected: &str) {
    let interp = run_on(VM::new_interpreter(), source);
    let jit = run_on(VM::new_eager_jit(), source);
    assert_eq!(interp, jit, "jit disagrees with interpreter for:\n{source}");
    assert_eq!(interp, expected, "wrong result for:\n{source}");
}

#[test]
fn loop_variable_survives_a_calls_bailout() {
    // The clearest symptom: `i` froze at start+step and an iteration vanished.
    assert_backends_agree(
        "fun d(a <float>) { a / 4.0 }\nlog := []\neach i in range(4) { x := d(10.0)\n log = push(log, i) }\nlog",
        "[0, 1, 2, 3]",
    );
}

#[test]
fn loop_variable_is_correct_for_any_range_shape() {
    // The stale value was always `start + step`, so vary both.
    assert_backends_agree(
        "fun d(a <float>) { a / 4.0 }\nlog := []\neach i in range(3, 7) { x := d(10.0)\n log = push(log, i) }\nlog",
        "[3, 4, 5, 6]",
    );
    assert_backends_agree(
        "fun d(a <float>) { a / 4.0 }\nlog := []\neach i in range(0, 6, 2) { x := d(10.0)\n log = push(log, i) }\nlog",
        "[0, 2, 4]",
    );
}

#[test]
fn no_iteration_is_lost_when_accumulating_a_call_result() {
    assert_backends_agree(
        "fun d(a <float>) { a / 4.0 }\nt <float> := 0.0\neach i in range(4) { t = t + d(10.0) }\nt",
        "10",
    );
    assert_backends_agree(
        "fun d(a <float>) { a / 4.0 }\nt <float> := 0.0\nn <int> := 0\neach i in range(200) { t = t + d(10.0)\n n = n + 1 }\nn",
        "200",
    );
}

#[test]
fn untyped_params_handed_floats_are_correct() {
    // The analyzer guesses `a`/`b` are ints from `a + b`; the runtime tag guard
    // is what catches the float. That bailout must stay the callee's problem.
    assert_backends_agree(
        "fun d(a, b) { a + b }\nt <float> := 0.0\neach i in range(4) { t = t + d(10.0, 4.0) }\nt",
        "56",
    );
    assert_backends_agree(
        "fun d(a, b) { a + b }\nt <float> := 0.0\neach i in range(200) { t = t + d(10.0, 4.0) }\nt",
        "2800",
    );
}

#[test]
fn float_arithmetic_flavours_all_agree() {
    for (body, expected) in [
        ("a / 4.0", "10"),
        ("a * 0.25", "10"),
        ("a - 7.5", "10"),
        ("a % 4.0", "8"),
    ] {
        assert_backends_agree(
            &format!(
                "fun d(a <float>) {{ {body} }}\nt <float> := 0.0\neach i in range(4) {{ t = t + d(10.0) }}\nt"
            ),
            expected,
        );
    }
}

#[test]
fn a_bailing_callee_still_returns_the_right_value() {
    // Independent of any loop: the call itself must produce the real result.
    assert_backends_agree("fun d(a, b) { a + b }\nd(10.0, 4.0)", "14");
    assert_backends_agree("fun d(a <float>) { a / 4.0 }\nd(10.0)", "2.5");
}

#[test]
fn int_callees_are_unaffected() {
    // The mirror optimization must still work where it is valid — otherwise
    // this "fix" would just be turning the optimization off.
    assert_backends_agree(
        "fun d(a, b) { a + b }\nt <int> := 0\neach i in range(4) { t = t + d(10, 4) }\nt",
        "56",
    );
    assert_backends_agree(
        "fun work(n) {\n  option { n < 2 -> n, work(n - 1) + work(n - 2) }\n}\nwork(20)",
        "6765",
    );
}

#[test]
fn alternating_int_and_float_calls_agree() {
    // Forces repeated bail / re-enter transitions on the same callee.
    assert_backends_agree(
        "fun d(a, b) { a + b }\nt <float> := 0.0\neach i in range(50) { t = t + d(1, 2)\n t = t + d(1.5, 2.5) }\nt",
        "350",
    );
}
