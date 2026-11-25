//! Specialized-call `Rc` balance in the JIT.
//!
//! Holds a probe reference to the callee closure and compares its strong
//! count after a small iteration count against a large one. The count must
//! not depend on how many times the function was called.

#![cfg(feature = "jit")]

use std::rc::Rc;

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;
use oxigen_core::vm::value::Value;

/// Runs `source` and returns the strong count of the global closure `name`.
fn closure_strong_count(source: &str, name: &str, eager_jit: bool) -> usize {
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
        .expect("compile should succeed");

    let mut vm = if eager_jit {
        VM::new_eager_jit()
    } else {
        VM::new_interpreter()
    };
    let _ = vm.run(function);

    match vm.get_global(name) {
        Some(Value::Closure(rc)) => Rc::strong_count(&rc),
        other => panic!("global `{name}` should be a closure, got {other:?}"),
    }
}

/// Like `closure_strong_count`, but observes the closure the program returns.
/// The program must end by evaluating to it.
fn result_closure_strong_count(source: &str, eager_jit: bool) -> usize {
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
        .expect("compile should succeed");

    let mut vm = if eager_jit {
        VM::new_eager_jit()
    } else {
        VM::new_interpreter()
    };
    match vm.run(function) {
        Ok(Value::Closure(rc)) => Rc::strong_count(&rc),
        other => panic!("program should evaluate to a closure, got {other:?}"),
    }
}

/// Asserts the callee's strong count is independent of the call count, on both
/// backends. `src_for(n)` builds the same program with `n` iterations.
fn assert_balanced_across_call_counts(label: &str, callee: &str, src_for: impl Fn(u32) -> String) {
    for (backend, eager) in [("interpreter", false), ("eager-jit", true)] {
        let few = closure_strong_count(&src_for(2_000), callee, eager);
        let many = closure_strong_count(&src_for(20_000), callee, eager);
        assert_eq!(
            few,
            many,
            "{label} on {backend}: `{callee}` strong count depends on call count \
             ({few} after 2k calls, {many} after 20k) — each call leaks {} reference(s)",
            (many as i64 - few as i64) as f64 / 18_000.0
        );
    }
}

#[test]
fn direct_self_recursion_is_refcount_balanced() {
    assert_balanced_across_call_counts("direct self-recursion", "fib", |n| {
        format!(
            "fun fib(k <int>) {{ give k when k < 2\nfib(k-1) + fib(k-2) }}\n\
             fun run(n <int>) {{ t <int> := 0\ni <int> := 0\n\
             repeat when i < n {{ t = t + fib(8)\ni = i + 1 }}\nt }}\nrun({n})"
        )
    });
}

#[test]
fn closure_aware_dispatch_is_refcount_balanced() {
    assert_balanced_across_call_counts("closure-aware dispatch", "add5", |n| {
        format!(
            "fun make_adder(x) {{ fun(y) {{ x + y }} }}\n\
             add5 := make_adder(5)\n\
             fun run(n <int>) {{ t <int> := 0\ni <int> := 0\n\
             repeat when i < n {{ t = t + add5(i)\ni = i + 1 }}\nt }}\nrun({n})"
        )
    });
}

#[test]
fn generic_global_call_is_refcount_balanced() {
    assert_balanced_across_call_counts("generic global call", "add2", |n| {
        format!(
            "fun add2(a <int>, b <int>) {{ a + b }}\n\
             fun run(n <int>) {{ t <int> := 0\ni <int> := 0\n\
             repeat when i < n {{ t = t + add2(i, 1)\ni = i + 1 }}\nt }}\nrun({n})"
        )
    });
}

#[test]
fn local_closure_call_is_refcount_balanced() {
    let src_for = |n: u32| {
        format!(
            "fun pick() {{ fun(y <int>) {{ y + 1 }} }}\n\
             fun run(n <int>) {{ f := pick()\nt <int> := 0\ni <int> := 0\n\
             repeat when i < n {{ t = t + f(i)\ni = i + 1 }}\nf }}\nrun({n})"
        )
    };
    for (backend, eager) in [("interpreter", false), ("eager-jit", true)] {
        let few = result_closure_strong_count(&src_for(2_000), eager);
        let many = result_closure_strong_count(&src_for(20_000), eager);
        assert_eq!(
            few, many,
            "local closure call on {backend}: strong count depends on call count \
             ({few} after 2k calls, {many} after 20k)"
        );
    }
}

#[test]
fn callee_bailout_is_refcount_balanced() {
    assert_balanced_across_call_counts("callee bailout", "d", |n| {
        format!(
            "fun d(a, b) {{ a + b }}\n\
             fun run(n <int>) {{ t := 0.0\ni <int> := 0\n\
             repeat when i < n {{ t = t + d(1.5, 2.5)\ni = i + 1 }}\nt }}\nrun({n})"
        )
    });
}

#[test]
fn caught_runtime_error_is_refcount_balanced() {
    assert_balanced_across_call_counts("caught runtime error", "boom", |n| {
        format!(
            "fun boom(k <int>) {{ <fail>(\"e\") }}\n\
             fun run(n <int>) {{ t <int> := 0\ni <int> := 0\n\
             repeat when i < n {{ x := boom(i) <guard>(7)\nt = t + x\ni = i + 1 }}\nt }}\nrun({n})"
        )
    });
}

#[test]
fn alternating_bailout_and_success_is_refcount_balanced() {
    assert_balanced_across_call_counts("alternating bailout", "d", |n| {
        format!(
            "fun d(a, b) {{ a + b }}\n\
             fun run(n <int>) {{ t := 0.0\ni <int> := 0\n\
             repeat when i < n {{ t = t + d(1, 2)\nt = t + d(1.5, 2.5)\ni = i + 1 }}\nt }}\nrun({n})"
        )
    });
}
