//! Follow-up VM/JIT regression tests for the skip/stop-as-value (R3) work,
//! covering two gaps the adversarial review found:
//!
//!  1. A NESTED *discarded* option/choose whose inner tail is `skip`/`stop` was
//!     wrongly compile-rejected by the VM/JIT (the discarded `value_consumed`
//!     context was dropped one nesting level down in
//!     `compile_last_statement_as_value`). It must compile + run.
//!  2. A `skip`/`stop` escaping to a function's RETURN value (via `give` or an
//!     implicit tail) must be rejected by the VM/JIT.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Compile + run on the VM; compile errors and runtime errors both surface as
/// `Err`. `jit_threshold == Some(1)` forces the JIT path.
fn run_vm(source: &str, jit_threshold: Option<u32>) -> Result<String, String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(parser.errors().is_empty(), "parser errors:\n{}", parser.format_errors());
    let function = Compiler::new()
        .compile(&program)
        .map_err(|errs| format!("compile error: {errs:?}"))?;
    let mut vm = VM::new();
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    vm.run(function).map(|v| format!("{v}")).map_err(|e| e.message)
}

#[test]
fn nested_discarded_skip_compiles_and_runs() {
    // The over-rejection case: a discarded outer construct (an expression-
    // statement loop body) whose INNER option/choose tail is `skip`. The skip is
    // legitimate loop control flow; all three backends must run it cleanly.
    for src in [
        "each i in range(2){ choose i { else -> { option { True -> { skip } } } } }\n\"ok\"\n",
        "each i in range(2){ option { False -> 1, option { True -> { skip } } } }\n\"ok\"\n",
        "each i in range(2){ option { True -> (option { True -> { skip } }) } }\n\"ok\"\n",
    ] {
        // Its final value is the string "ok".
        assert_eq!(run_vm(src, None).unwrap(), "ok", "VM interp should run nested-discarded skip:\n{src}");
        assert!(run_vm(src, Some(1)).is_ok(), "VM JIT should run nested-discarded skip:\n{src}");
    }
}

#[test]
fn skip_escaping_to_function_return_is_rejected_everywhere() {
    // `give option{… -> {skip}}` returns a skip sentinel — not a value. The
    // bytecode compiler rejects it with a static skip/stop-as-value analysis
    // ("'skip' cannot be used as a value").
    let give = "fun g(){ give option { 1 >= 0 -> { skip } } }\nprintln(g())\n";
    assert!(
        run_vm(give, None).unwrap_err().contains("cannot be used as a value"),
        "VM should reject skip as a give value, got: {:?}",
        run_vm(give, None)
    );
    assert!(run_vm(give, Some(1)).is_err(), "JIT should reject skip as a give value");

    // Same via an implicit (tail) return.
    let implicit = "fun g(){ option { 1 >= 0 -> { stop } } }\nprintln(g())\n";
    assert!(
        run_vm(implicit, None).unwrap_err().contains("cannot be used as a value"),
        "VM should reject stop as an implicit return, got: {:?}",
        run_vm(implicit, None)
    );
}

#[test]
fn consumed_skip_still_errors_and_normal_loops_unaffected() {
    // A genuinely consumed skip is rejected statically by the VM/JIT compiler...
    let consumed = "each i in [1] { x := 1 + option { i == 1 -> { skip }, 5 }\nprintln(x) }\n";
    assert!(run_vm(consumed, None).unwrap_err().contains("cannot be used as a value"),
        "VM should reject a consumed skip, got: {:?}", run_vm(consumed, None));

    // ...while ordinary in-loop skip/stop control flow is untouched.
    let normal = "each i in range(6){ skip when i % 2 == 0\ni }\n";
    assert!(run_vm(normal, None).is_ok(), "normal skip-in-loop must still run");
    assert!(run_vm(normal, Some(1)).is_ok(), "normal skip-in-loop must still run under JIT");
}

#[test]
fn dead_skip_arm_is_rejected_on_all_backends() {
    // The last residual: a consumed `skip` arm that is DYNAMICALLY UNREACHED.
    // `c(5)` selects the `n>0` arm so the `skip` arm never runs, but the VM/JIT
    // reject it statically (compile-error) regardless of which arm runs.
    let src = "fun c(n){ option { n > 0 -> \"p\", True -> { skip } } }\nprintln(c(5))\n";
    assert!(
        run_vm(src, None).unwrap_err().contains("cannot be used as a value"),
        "VM must reject a dead consumed skip arm, got: {:?}",
        run_vm(src, None)
    );
    assert!(run_vm(src, Some(1)).is_err(), "JIT must reject a dead consumed skip arm");

    // A function whose option has a stop arm that IS structurally a value but is
    // never selected at the call — same static rejection.
    let stop_src = "fun pick(n){ option { n < 0 -> -1, n == 0 -> 0, True -> { stop } } }\npick(0)\n";
    assert!(
        run_vm(stop_src, None).unwrap_err().contains("cannot be used as a value"),
        "VM must reject a dead consumed stop arm, got: {:?}",
        run_vm(stop_src, None)
    );
}

#[test]
fn skip_stop_outside_loop_in_uncalled_function_rejected_everywhere() {
    // CLASS A (parity hunt): `skip`/`stop` lexically outside any loop, in a
    // function/closure/method that is NEVER called. The bytecode compiler rejects
    // it eagerly ("used outside of loop").
    for src in [
        "f := fun(n){ skip when n == 2 }\nprintln(\"ok\")\n",
        "f := fun(n){ stop when n == 2 }\nprintln(\"ok\")\n",
        "f := fun(n){ skip\nn }\nprintln(\"ok\")\n",
        "struct Foo { n <int> }\nFoo includes { fun bar(){ skip when self.n == 2 } }\nprintln(\"ok\")\n",
    ] {
        assert!(
            run_vm(src, None).unwrap_err().contains("used outside of loop"),
            "VM must reject skip/stop outside a loop, got: {:?}\n{src}",
            run_vm(src, None)
        );
        assert!(run_vm(src, Some(1)).is_err(), "JIT must reject skip/stop outside a loop:\n{src}");
    }
}

#[test]
fn non_tail_skip_stop_in_consumed_block_rejected_everywhere() {
    // CLASS B (parity hunt): a NON-tail bare `skip`/`stop` in an option arm block
    // whose value is consumed makes the block's value ill-defined. The VM/JIT
    // reject it ("cannot be used as a value").
    for src in [
        "each i in [1,2,3] {\n  println(option { i == 2 -> { skip\n99 }, i })\n}\n",
        "each i in [1,2,3] {\n  println(option { i == 2 -> { stop\n99 }, i })\n}\n",
        "each i in [1,2,3] {\n  x := 100 + option { i == 2 -> { skip\n5 }, i }\n  println(x)\n}\n",
    ] {
        assert!(
            run_vm(src, None).unwrap_err().contains("cannot be used as a value"),
            "VM must reject non-tail consumed skip/stop, got: {:?}\n{src}",
            run_vm(src, None)
        );
        assert!(run_vm(src, Some(1)).is_err(), "JIT must reject non-tail consumed skip/stop:\n{src}");
    }

    // ...but a non-tail skip/stop in a DISCARDED block is still legitimate loop
    // control flow and runs on all three backends.
    let discarded = "each i in [1,2,3] {\n  option { i == 2 -> { skip\n99 } }\n}\n\"done\"\n";
    assert!(run_vm(discarded, None).is_ok(), "discarded non-tail skip must still run");
    assert!(run_vm(discarded, Some(1)).is_ok(), "discarded non-tail skip must still run under JIT");
}
