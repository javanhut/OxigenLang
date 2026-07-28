//! Regression tests for two `each`/`range` additions:
//!
//!   * `range(start, end, step)` — a negative step counts down, so the bound
//!     stays exclusive from whichever side it is approached. A *literal* step
//!     also keeps the compiler's counting-loop lowering (no array is
//!     materialised); a computed step falls back to the builtin. Both paths
//!     must produce identical results, which is what the `_literal` /
//!     `_computed` pairs below pin.
//!
//!   * `each k, v in coll` — the leading name binds a map's key or a
//!     sequence's ordinal index. The one-name form is unchanged, and for maps
//!     still binds the whole `(k, v)` tuple.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Runs a program and returns its final value, formatted.
fn run(source: &str) -> String {
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
        .unwrap_or_else(|errs| panic!("compile errors: {errs:?}"));
    let mut vm = VM::new();
    match vm.run(function) {
        Ok(v) => format!("{v}"),
        Err(e) => e.message,
    }
}

/// Runs an `each` loop, appending `entry` per iteration, and returns the
/// accumulated string. `head` is the loop's `each ...` clause.
fn collect(head: &str, entry: &str) -> String {
    run(&format!(
        "acc <str> := \"\"\n{head} {{ acc = acc + \"{entry},\" }}\nacc"
    ))
}

/// Collects `each i in <range expr>` into a comma-joined string.
fn range_yields(range_expr: &str) -> String {
    collect(&format!("each i in {range_expr}"), "{i}")
}

// ── range: step and direction ───────────────────────────────────────────────

#[test]
fn range_counts_down_with_a_negative_step() {
    assert_eq!(range_yields("range(5, 0, -1)"), "5,4,3,2,1,");
    assert_eq!(range_yields("range(10, 0, -3)"), "10,7,4,1,");
}

#[test]
fn range_skips_with_a_positive_step() {
    assert_eq!(range_yields("range(0, 10, 3)"), "0,3,6,9,");
    assert_eq!(range_yields("range(2, 3, 5)"), "2,");
}

#[test]
fn range_bound_stays_exclusive_in_both_directions() {
    // The endpoint is never produced, counting up or down.
    assert_eq!(range_yields("range(0, 3, 1)"), "0,1,2,");
    assert_eq!(range_yields("range(3, 0, -1)"), "3,2,1,");
}

#[test]
fn range_is_empty_when_the_step_points_away_from_the_bound() {
    assert_eq!(range_yields("range(0, 5, -1)"), "");
    assert_eq!(range_yields("range(5, 0, 1)"), "");
    assert_eq!(range_yields("range(5, 5, 1)"), "");
}

#[test]
fn range_one_and_two_argument_forms_are_unchanged() {
    assert_eq!(range_yields("range(3)"), "0,1,2,");
    assert_eq!(range_yields("range(2, 5)"), "2,3,4,");
    assert_eq!(range_yields("range(0)"), "");
    // Descending without an explicit step is still empty, not auto-reversed.
    assert_eq!(range_yields("range(5, 0)"), "");
}

#[test]
fn literal_and_computed_steps_agree() {
    // A literal step takes the counting-loop lowering; a computed one falls
    // back to the builtin. Same answers either way.
    for (literal, step) in [("range(9, 0, -2)", -2), ("range(0, 9, 2)", 2)] {
        let via_local = run(&format!(
            "st := {step}\nacc <str> := \"\"\neach i in range({}, {}, st) {{ acc = acc + \"{{i}},\" }}\nacc",
            if step < 0 { 9 } else { 0 },
            if step < 0 { 0 } else { 9 },
        ));
        assert_eq!(
            range_yields(literal),
            via_local,
            "literal vs computed step disagree for {literal}"
        );
    }
}

#[test]
fn range_rejects_a_zero_step() {
    // Guards against a loop that never advances.
    let out = run("range(0, 5, 0)");
    assert!(out.contains("step"), "expected a step error, got: {out}");
}

// ── each k, v ───────────────────────────────────────────────────────────────

#[test]
fn two_names_over_a_sequence_bind_index_and_element() {
    assert_eq!(
        collect("each i, v in [\"a\", \"b\", \"c\"]", "{i}={v}"),
        "0=a,1=b,2=c,"
    );
}

#[test]
fn two_names_over_a_map_bind_key_and_value() {
    assert_eq!(
        collect("each k, v in {\"x\": 1, \"y\": 2}", "{k}->{v}"),
        "x->1,y->2,"
    );
}

#[test]
fn two_names_work_over_strings_and_tuples() {
    assert_eq!(collect("each i, c in \"hi\"", "{i}{c}"), "0h,1i,");
    assert_eq!(collect("each i, t in (10, 20)", "{i}:{t}"), "0:10,1:20,");
}

#[test]
fn one_name_over_a_map_still_binds_the_pair() {
    // The existing single-name behaviour must not shift under the new form.
    assert_eq!(collect("each pair in {\"z\": 9}", "{pair}"), "(z, 9),");
}

#[test]
fn two_names_over_a_range_number_the_elements() {
    // Ranges fall back to the array path here, so the first name is a real
    // ordinal index rather than a copy of the counter.
    assert_eq!(collect("each i, v in range(10, 0, -5)", "{i}:{v}"), "0:10,1:5,");
}

#[test]
fn skip_and_stop_balance_both_loop_bindings() {
    // Both names are pushed per iteration, so `skip`/`stop` have to unwind two
    // locals; a mismatch corrupts the stack rather than failing loudly.
    assert_eq!(
        run("acc <str> := \"\"\neach i, v in [\"a\", \"b\", \"c\", \"d\"] {\n  skip when i == 1\n  acc = acc + \"{i}{v},\"\n}\nacc"),
        "0a,2c,3d,"
    );
    assert_eq!(
        run("acc <str> := \"\"\neach i, v in [\"a\", \"b\", \"c\"] {\n  stop when i == 2\n  acc = acc + \"{i}{v},\"\n}\nacc"),
        "0a,1b,"
    );
}

#[test]
fn closures_capture_each_iterations_own_pair() {
    assert_eq!(
        run("fs := []\neach i, v in [\"p\", \"q\"] { fs = push(fs, fun() { \"{i}{v}\" }) }\nacc <str> := \"\"\neach f in fs { acc = acc + f() + \",\" }\nacc"),
        "0p,1q,"
    );
}

#[test]
fn two_name_loops_nest() {
    assert_eq!(
        run("acc <str> := \"\"\neach i, row in [[1, 2], [3, 4]] {\n  each j, cell in row { acc = acc + \"{i},{j}={cell};\" }\n}\nacc"),
        "0,0=1;0,1=2;1,0=3;1,1=4;"
    );
}

#[test]
fn two_name_loop_in_a_function_is_reentrant() {
    // Repeated calls exercise the loop's per-iteration cleanup on a warm
    // (JIT-eligible) function.
    assert_eq!(
        run("fun total(m) {\n  acc <int> := 0\n  each k, v in m { acc = acc + v }\n  acc\n}\nsum <int> := 0\neach n in range(3) { sum = sum + total({\"a\": 1, \"b\": 2}) }\nsum"),
        "9"
    );
}

// ── interpreter/JIT parity ──────────────────────────────────────────────────
// A stepped `each` loop changes the comparison direction and the increment
// constant the compiler emits, and the two-name form adds `IterEntry`. All of
// it has to mean the same thing on both backends.
//
// Computed steps are covered too. They fall back to materialising an array,
// which used to hit a JIT miscompile that transposed a call's first two
// arguments (`range(0, 3, s)` was invoked as `range(3, 0, 1)`); see
// `fix_jit_call_arg_init_aliasing.rs`.

/// Runs `source` on the eager-JIT and interpreter backends, asserting both
/// produce the same final value, and returns it.
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
fn stepped_ranges_agree_across_backends() {
    for (expr, expected) in [
        ("range(5, 0, -1)", "5,4,3,2,1,"),
        ("range(10, 0, -3)", "10,7,4,1,"),
        ("range(0, 10, 3)", "0,3,6,9,"),
        ("range(-1, -4, -1)", "-1,-2,-3,"),
        ("range(3)", "0,1,2,"),
    ] {
        let out = assert_backends_agree(&format!(
            "acc <str> := \"\"\neach i in {expr} {{ acc = acc + \"{{i}},\" }}\nacc"
        ));
        assert_eq!(out, expected, "wrong result for {expr}");
    }
}

#[test]
fn computed_step_ranges_agree_across_backends() {
    for (step, expected) in [(-1, "3,2,1,"), (2, "0,2,"), (-2, "3,1,")] {
        let out = assert_backends_agree(&format!(
            "s := {step}\nacc <str> := \"\"\neach i in range({}, {}, s) {{ acc = acc + \"{{i}},\" }}\nacc",
            if step < 0 { 3 } else { 0 },
            if step < 0 { 0 } else { 3 },
        ));
        assert_eq!(out, expected, "wrong result for computed step {step}");
    }
}

#[test]
fn two_name_loops_agree_across_backends() {
    for (head, entry, expected) in [
        ("each i, v in [\"a\", \"b\"]", "{i}={v}", "0=a,1=b,"),
        ("each k, v in {\"x\": 1, \"y\": 2}", "{k}{v}", "x1,y2,"),
        ("each i, c in \"hi\"", "{i}{c}", "0h,1i,"),
    ] {
        let out = assert_backends_agree(&format!(
            "acc <str> := \"\"\n{head} {{ acc = acc + \"{entry},\" }}\nacc"
        ));
        assert_eq!(out, expected, "wrong result for {head}");
    }
}

#[test]
fn hot_two_name_loop_agrees_across_backends() {
    // Enough iterations to tier the function up and run the loop as native code.
    let out = assert_backends_agree(
        "fun weigh(a) {\n  t <int> := 0\n  each i, v in a { t = t + i * v }\n  t\n}\nsum <int> := 0\neach n in range(200) { sum = sum + weigh([1, 2, 3, 4]) }\nsum",
    );
    assert_eq!(out, "4000");
}
