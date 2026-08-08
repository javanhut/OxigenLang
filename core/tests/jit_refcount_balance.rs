//! Phase 0.2: specialized-call `Rc` balance.
//!
//! The JIT's call paths copy, borrow and abandon `Value::Closure` handles while
//! bypassing `Vec`'s own bookkeeping — `sync_stack_from_view` is a bare
//! `set_len` that runs no `Drop`. An imbalance there is invisible: a leak just
//! grows RSS, and an over-release is a use-after-free that only bites under a
//! specific interleaving. Neither shows up as a wrong answer, so output-parity
//! tests cannot find them. (The existing `*_is_refcount_safe` tests in
//! `jit_fallback.rs` compare output only; they do not observe a refcount.)
//!
//! Method: hold a probe reference to the callee closure and compare its strong
//! count after a small iteration count against a large one. Any per-call
//! imbalance scales with the number of calls, so `count(1) == count(N)` for a
//! large N is a direct assertion that each call is balanced — and it needs no
//! access to VM internals, only the public `get_global`.
//!
//! Absolute counts are deliberately not asserted: they depend on how many
//! places legitimately hold the closure (globals map, IC keeper, probe clone),
//! which is an implementation detail. The *invariant* is that the count does
//! not depend on how many times the function was called.

#![cfg(feature = "jit")]

use std::rc::Rc;

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;
use oxigen_core::vm::value::Value;

/// Runs `source` and returns the strong count of the global closure `name`.
///
/// The returned `Value` from `get_global` is itself a clone, so the count
/// includes a constant +1 — harmless, since every call here is compared against
/// another measured the same way.
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
    // Errors are expected in the failure-path tests; the count is what matters.
    let _ = vm.run(function);

    match vm.get_global(name) {
        Some(Value::Closure(rc)) => Rc::strong_count(&rc),
        other => panic!("global `{name}` should be a closure, got {other:?}"),
    }
}

/// Like `closure_strong_count`, but observes the closure the program *returns*.
///
/// Needed for callees that never land in a global — an anonymous closure held
/// in a local is exactly the shape the closure-aware dispatch path serves, and
/// `get_global` cannot see it. The program must end by evaluating to it.
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
/// backends. `src_for(n)` must build the same program with `n` iterations.
fn assert_balanced_across_call_counts(
    label: &str,
    callee: &str,
    src_for: impl Fn(u32) -> String,
) {
    for (backend, eager) in [("interpreter", false), ("eager-jit", true)] {
        // Both points are past the compile threshold on purpose. Comparing an
        // uncompiled run against a compiled one measures one-time installation
        // (the compiled entry retains the closure) rather than per-call balance,
        // and reads as a fixed +2 "leak" that does not scale.
        let few = closure_strong_count(&src_for(2_000), callee, eager);
        let many = closure_strong_count(&src_for(20_000), callee, eager);
        assert_eq!(
            few, many,
            "{label} on {backend}: `{callee}` strong count depends on call count \
             ({few} after 2k calls, {many} after 20k) — each call leaks {} reference(s)",
            (many as i64 - few as i64) as f64 / 18_000.0
        );
    }
}

// ── success paths ───────────────────────────────────────────────────────────

#[test]
fn direct_self_recursion_is_refcount_balanced() {
    // A3: top-level self-recursive int fn, reached via GetGlobal.
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
    // The bench_closure shape: arity-1 closure with an int upvalue, called in a
    // hot loop through the closure-aware specialized dispatch path.
    //
    // `add5` is bound at top level so it IS the global under observation — the
    // hot callee itself, not the factory that produced it. Watching `make_adder`
    // here would prove nothing: it runs once no matter what `n` is.
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
    // GetGlobal closure copy + generic IC dispatch (arity 2 skips the
    // closure-aware path, which is arity-1 only).
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
    // GetLocal closure copy rather than GetGlobal: the callee lives in a local
    // slot, so the call path copies it out of the frame each iteration.
    //
    // The callee never becomes a global, so it is observed as the program's
    // result instead — `run` calls it n times and then hands it back.
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

// ── failure paths ───────────────────────────────────────────────────────────

#[test]
fn callee_bailout_is_refcount_balanced() {
    // Untyped params the analyzer guesses as int, handed floats — every call
    // trips the entry tag guard and bails to the interpreter. The bailout
    // rollback must not leak the closure it was about to call.
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
    // A callee that fails every iteration, recovered by <guard>. The unwind
    // runs per call, so a leak in the error rollback scales with n.
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
    // Forces repeated bail/re-enter on one callee, exercising the consecutive-
    // bailout deopt counter and its reset alongside the refcount bookkeeping.
    assert_balanced_across_call_counts("alternating bailout", "d", |n| {
        format!(
            "fun d(a, b) {{ a + b }}\n\
             fun run(n <int>) {{ t := 0.0\ni <int> := 0\n\
             repeat when i < n {{ t = t + d(1, 2)\nt = t + d(1.5, 2.5)\ni = i + 1 }}\nt }}\nrun({n})"
        )
    });
}
