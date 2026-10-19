//! Regression: named arguments must never misalign the callee's frame.
//!
//! `call_closure`'s named-args branch skipped the arity check whenever the
//! callee had an optional parameter, then placed its frame at
//! `len - arity`. Surplus positional args were left BELOW the frame, so the
//! callee read the wrong slots (`f(1, 2, 3, 4, b=9)` returned `[fun f(), 1]`,
//! a method saw `self` as an INTEGER) and every such call leaked stack slots
//! into the caller. Missing required parameters were silently filled with
//! `None` and unknown names were dropped.
//!
//! The leak was also the way in to a memory-safety bug: once a script had
//! leaked the stack up to `STACK_MAX`, the receiver insert of a method call
//! (`stack_insert`) reallocated the stack `Vec` and left the JIT's cached
//! `StackView.ptr` dangling — a segfault in default and `--jit` modes.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_on(mut vm: VM, source: &str) -> Result<String, String> {
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
        .map_err(|errs| format!("{errs:?}"))?;
    vm.run(function)
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

fn backends() -> [(&'static str, fn() -> VM); 3] {
    [
        ("interpreter", VM::new_interpreter),
        ("tiered", VM::new_tiered),
        ("eager_jit", VM::new_eager_jit),
    ]
}

fn assert_all_ok(source: &str, expected: &str) {
    for (name, make) in backends() {
        let got = run_on(make(), source).unwrap_or_else(|e| panic!("{name} failed: {e}\n{source}"));
        assert_eq!(got, expected, "{name} mismatch for:\n{source}");
    }
}

fn assert_all_err(source: &str, needle: &str) {
    for (name, make) in backends() {
        match run_on(make(), source) {
            Ok(v) => panic!("{name}: expected an error containing {needle:?}, got Ok({v})\n{source}"),
            Err(e) => assert!(
                e.contains(needle),
                "{name}: error {e:?} does not mention {needle:?}\n{source}"
            ),
        }
    }
}

const F: &str = "fun f(a, b?) { [a, b] }\n";

#[test]
fn surplus_positional_args_with_a_named_arg_are_an_arity_error() {
    assert_all_err(&format!("{F}f(1, 2, 3, 4, b=9)"), "argument");
}

#[test]
fn caller_frame_survives_a_rejected_named_call() {
    // The rejected call must not leave its surplus args on the caller's stack.
    let src = format!(
        "{F}fun g() {{\n  r := <type<Error || Value>>(f(1, 2, 3, 4, b=9))\n  z := \"zed\"\n  [is_error(r), z]\n}}\ng()"
    );
    assert_all_ok(&src, "[True, zed]");
}

#[test]
fn missing_required_param_with_named_args_is_an_error() {
    assert_all_err("fun f(a, b = 5) { [a, b] }\nf(b=2)", "a");
}

#[test]
fn unknown_named_arg_is_an_error_on_indirect_calls() {
    assert_all_err("fun f(a, b = 5) { [a, b] }\nh := f\nh(1, zzz=2)", "zzz");
}

#[test]
fn named_arg_repeating_a_positional_is_an_error() {
    assert_all_err("fun f(a, b = 5) { [a, b] }\nh := f\nh(1, a=2)", "a");
}

#[test]
fn valid_named_calls_still_work() {
    assert_all_ok("fun f(a, b = 5) { [a, b] }\nf(1, b=7)", "[1, 7]");
    assert_all_ok("fun f(a, b = 5) { [a, b] }\nf(a=3)", "[3, 5]");
    assert_all_ok("fun k(a, b?) { [a, b] }\nk(b=2, a=1)", "[1, 2]");
    assert_all_ok("fun k(a, b?) { [a, b] }\nh := k\nh(1, b=4)", "[1, 4]");
}

#[test]
fn method_named_args_keep_self_intact() {
    let s = "struct S { v <int> }\nS includes { fun m(a, b?) { [self.v, a, b] } }\ns := S(4)\n";
    assert_all_ok(&format!("{s}s.m(1, b=2)"), "[4, 1, 2]");
    assert_all_err(&format!("{s}s.m(1, 2, 3, 4, b=9)"), "argument");
}

/// The original reproducer: leak the stack with rejected named calls, then
/// make a method call whose receiver insert lands at `STACK_MAX`. Every
/// backend must finish (or raise a clean error) — never crash.
#[test]
fn leaked_stack_cannot_reach_the_receiver_insert() {
    let elems = vec!["z"; 8199].join(", ");
    let src = format!(
        "struct S {{ v <int> }}\n\
         S includes {{ fun m() {{ 1 }} }}\n\
         fun f(a, b?) {{ a }}\n\
         fun hot(n) {{\n  s := 0\n  each i in range(n) {{ s = s + i }}\n  s\n}}\n\
         o := S(1)\n\
         out := []\n\
         each z in [0] {{\n\
           each i in range(140000) {{\n\
             r := <type<Error || Value>>(f(1, 2, 3, 4, b=9))\n\
           }}\n\
           q := <type<Error || Value>>([{elems}, o.m()])\n\
           out.push(hot(10))\n\
         }}\n\
         out.push(hot(1000))\n\
         out"
    );
    for (name, make) in backends() {
        match run_on(make(), &src) {
            Ok(v) => assert_eq!(v, "[45, 499500]", "{name}"),
            Err(e) => assert!(e.contains("stack overflow"), "{name}: unexpected error {e}"),
        }
    }
}
