//! Follow-up regressions for the skip/stop-as-value (R3) work, covering two
//! gaps the adversarial review found:
//!
//!  1. A NESTED *discarded* option/choose whose inner tail is `skip`/`stop` was
//!     wrongly compile-rejected by the VM/JIT (the discarded `value_consumed`
//!     context was dropped one nesting level down in
//!     `compile_last_statement_as_value`). It must compile + run, like the
//!     tree-walker.
//!  2. A `skip`/`stop` escaping to a function's RETURN value (via `give` or an
//!     implicit tail) was accepted by the tree-walker (printed `skip`) but
//!     rejected by the VM/JIT. The tree-walker now rejects it too.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::evaluator::Evaluator;
use oxigen_core::lexer::Lexer;
use oxigen_core::object::environment::Environment;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;
use std::cell::RefCell;
use std::rc::Rc;

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

/// Run on the tree-walker; the formatted result string carries `error: …` text
/// for runtime errors.
fn run_tree(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(parser.errors().is_empty(), "parser errors:\n{}", parser.format_errors());
    let env = Rc::new(RefCell::new(Environment::new()));
    let mut evaluator = Evaluator::new();
    format!("{}", evaluator.eval_program(&program, env))
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
        assert!(run_vm(src, None).is_ok(), "VM interp should run nested-discarded skip:\n{src}");
        assert!(run_vm(src, Some(1)).is_ok(), "VM JIT should run nested-discarded skip:\n{src}");
        // tree-walker runs it (its final value is the string "ok").
        assert_eq!(run_tree(src), "ok", "tree-walker should run nested-discarded skip:\n{src}");
    }
}

#[test]
fn skip_escaping_to_function_return_is_rejected_everywhere() {
    // `give option{… -> {skip}}` returns a skip sentinel — not a value. The
    // tree-walker now rejects it with the SAME static skip/stop-as-value analysis
    // the bytecode compiler uses ("'skip' cannot be used as a value"), so all
    // three backends reject identically.
    let give = "fun g(){ give option { 1 >= 0 -> { skip } } }\nprintln(g())\n";
    assert!(run_vm(give, None).is_err(), "VM should reject skip as a give value");
    assert!(run_vm(give, Some(1)).is_err(), "JIT should reject skip as a give value");
    assert!(
        run_tree(give).contains("cannot be used as a value"),
        "tree-walker should reject skip as a give value, got: {}",
        run_tree(give)
    );

    // Same via an implicit (tail) return.
    let implicit = "fun g(){ option { 1 >= 0 -> { stop } } }\nprintln(g())\n";
    assert!(run_vm(implicit, None).is_err(), "VM should reject stop as an implicit return");
    assert!(
        run_tree(implicit).contains("cannot be used as a value"),
        "tree-walker should reject stop as an implicit return, got: {}",
        run_tree(implicit)
    );
}

#[test]
fn consumed_skip_still_errors_and_normal_loops_unaffected() {
    // A genuinely consumed skip is still rejected on all backends (the
    // tree-walker now rejects it statically, like VM/JIT, rather than only at
    // runtime if the arm is reached)...
    let consumed = "each i in [1] { x := 1 + option { i == 1 -> { skip }, 5 }\nprintln(x) }\n";
    assert!(run_vm(consumed, None).is_err(), "VM should reject a consumed skip");
    assert!(run_tree(consumed).contains("cannot be used as a value"),
        "tree-walker should reject a consumed skip, got: {}", run_tree(consumed));

    // ...while ordinary in-loop skip/stop control flow is untouched.
    let normal = "each i in range(6){ skip when i % 2 == 0\ni }\n";
    assert!(run_vm(normal, None).is_ok(), "normal skip-in-loop must still run");
    assert!(run_vm(normal, Some(1)).is_ok(), "normal skip-in-loop must still run under JIT");
}

#[test]
fn dead_skip_arm_is_rejected_on_all_backends() {
    // The last residual: a consumed `skip` arm that is DYNAMICALLY UNREACHED.
    // `c(5)` selects the `n>0` arm so the `skip` arm never runs — the tree-walker
    // used to run this (rc=0) while VM/JIT compile-error. The tree-walker now runs
    // the same static skip/stop-as-value check the compiler does, so all three
    // backends reject it identically (closing the compile-time-vs-runtime gap).
    let src = "fun c(n){ option { n > 0 -> \"p\", True -> { skip } } }\nprintln(c(5))\n";
    assert!(run_vm(src, None).is_err(), "VM must reject a dead consumed skip arm");
    assert!(run_vm(src, Some(1)).is_err(), "JIT must reject a dead consumed skip arm");
    assert!(
        run_tree(src).contains("cannot be used as a value"),
        "tree-walker must now reject a dead consumed skip arm, got: {}",
        run_tree(src)
    );

    // A function whose option has a stop arm that IS structurally a value but is
    // never selected at the call — same static rejection everywhere.
    let stop_src = "fun pick(n){ option { n < 0 -> -1, n == 0 -> 0, True -> { stop } } }\npick(0)\n";
    assert!(run_vm(stop_src, None).is_err(), "VM must reject a dead consumed stop arm");
    assert!(
        run_tree(stop_src).contains("cannot be used as a value"),
        "tree-walker must reject a dead consumed stop arm, got: {}",
        run_tree(stop_src)
    );
}

#[test]
fn skip_stop_outside_loop_in_uncalled_function_rejected_everywhere() {
    // CLASS A (parity hunt): `skip`/`stop` lexically outside any loop, in a
    // function/closure/method that is NEVER called. The bytecode compiler rejects
    // it eagerly ("used outside of loop"); the tree-walker now runs the same
    // static check via the eval_program gate instead of lazily ignoring the dead
    // code. All three backends reject identically.
    for src in [
        "f := fun(n){ skip when n == 2 }\nprintln(\"ok\")\n",
        "f := fun(n){ stop when n == 2 }\nprintln(\"ok\")\n",
        "f := fun(n){ skip\nn }\nprintln(\"ok\")\n",
        "struct Foo { n <int> }\nFoo includes { fun bar(){ skip when self.n == 2 } }\nprintln(\"ok\")\n",
    ] {
        assert!(run_vm(src, None).is_err(), "VM must reject skip/stop outside a loop:\n{src}");
        assert!(run_vm(src, Some(1)).is_err(), "JIT must reject skip/stop outside a loop:\n{src}");
        assert!(
            run_tree(src).contains("used outside of loop"),
            "tree-walker must reject skip/stop outside a loop, got: {}\n{src}",
            run_tree(src)
        );
    }
}

#[test]
fn non_tail_skip_stop_in_consumed_block_rejected_everywhere() {
    // CLASS B (parity hunt): a NON-tail bare `skip`/`stop` in an option arm block
    // whose value is consumed makes the block's value ill-defined. All three
    // backends reject it ("cannot be used as a value"); previously the tree-walker
    // printed the sentinel while VM/JIT corrupted the loop.
    for src in [
        "each i in [1,2,3] {\n  println(option { i == 2 -> { skip\n99 }, i })\n}\n",
        "each i in [1,2,3] {\n  println(option { i == 2 -> { stop\n99 }, i })\n}\n",
        "each i in [1,2,3] {\n  x := 100 + option { i == 2 -> { skip\n5 }, i }\n  println(x)\n}\n",
    ] {
        assert!(run_vm(src, None).is_err(), "VM must reject non-tail consumed skip/stop:\n{src}");
        assert!(run_vm(src, Some(1)).is_err(), "JIT must reject non-tail consumed skip/stop:\n{src}");
        assert!(
            run_tree(src).contains("cannot be used as a value"),
            "tree-walker must reject non-tail consumed skip/stop, got: {}\n{src}",
            run_tree(src)
        );
    }

    // ...but a non-tail skip/stop in a DISCARDED block is still legitimate loop
    // control flow and runs on all three backends.
    let discarded = "each i in [1,2,3] {\n  option { i == 2 -> { skip\n99 } }\n}\n\"done\"\n";
    assert!(run_vm(discarded, None).is_ok(), "discarded non-tail skip must still run");
    assert!(run_vm(discarded, Some(1)).is_ok(), "discarded non-tail skip must still run under JIT");
}
