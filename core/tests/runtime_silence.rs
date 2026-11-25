//! Runtime operations that must fail loudly instead of producing a value: a call padded with `None`, a struct with zero-filled holes, a truncated JSON document, a `cancel` that cancels nothing.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Runs on both backends, asserting they agree; returns the value or the error.
fn run(source: &str) -> String {
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
            .unwrap_or_else(|e| panic!("compile errors: {e:?}"))
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

fn assert_rejected(source: &str, code: &str) -> String {
    let out = run(source);
    assert!(
        out.contains(code),
        "expected {code} for:\n{source}\ngot: {out}"
    );
    out
}

#[test]
fn too_few_arguments_are_reported_at_the_call() {
    let out = assert_rejected("fun add(a, b) { a + b }\nadd(1)", "E0032");
    assert!(
        out.contains("takes 2 arguments but 1 was supplied"),
        "{out}"
    );
    assert!(out.contains("missing `b`"), "{out}");
    assert!(
        !out.contains("INTEGER + NONE"),
        "still failing inside the callee:\n{out}"
    );
}

#[test]
fn too_many_arguments_are_reported_rather_than_corrupting_the_frame() {
    assert_rejected("fun add(a, b) { a + b }\nadd(1, 2, 3)", "E0032");
    let out = assert_rejected("fun one(a) { a }\none(1, 2, 3)", "E0032");
    assert!(
        !out.contains("cannot call INTEGER"),
        "frame still misaligned:\n{out}"
    );
}

#[test]
fn defaults_and_optionals_may_still_be_omitted() {
    assert_eq!(run("fun f(a, b = 2) { a + b }\nf(1)"), "3");
    assert_eq!(run("fun f(a, b = 2) { a + b }\nf(1, 5)"), "6");
    assert_eq!(run("fun g(a, b?) { a }\ng(1)"), "1");
    assert_eq!(run("fun add(a, b) { a + b }\nadd(1, 2)"), "3");
}

#[test]
fn the_arity_message_names_the_function_and_counts() {
    let out = run("fun solo(a) { a }\nsolo()");
    assert!(out.contains("`solo`"), "{out}");
    assert!(out.contains("1 argument"), "singular form: {out}");
}

#[test]
fn a_struct_missing_fields_is_rejected() {
    let out = assert_rejected("struct P { a <int> b <int> }\nP(1)", "E0033");
    assert!(out.contains("missing field `b`"), "{out}");
}

#[test]
fn a_struct_given_too_many_values_is_rejected() {
    assert_rejected("struct P { a <int> }\nP(1, 2)", "E0033");
}

#[test]
fn zero_value_initialization_is_still_allowed() {
    assert_eq!(run("struct P { a <int> b <int> }\np <P>\np.b"), "0");
    assert_eq!(run("struct P { a <int> }\nP{}.a"), "0");
}

#[test]
fn partial_named_construction_is_rejected_too() {
    assert_rejected("struct P { a <int> b <int> }\nP{a: 1}", "E0033");
}

#[test]
fn a_field_that_admits_none_may_be_omitted() {
    assert_eq!(run("struct N { d <int>\n nx <N> || <None> }\nN(1).d"), "1");
}

#[test]
fn an_explicitly_supplied_none_is_not_treated_as_missing() {
    assert_eq!(run("struct P { a <generic> }\nP{a: None}.a"), "None");
}

#[test]
fn complete_struct_construction_is_unaffected() {
    assert_eq!(run("struct P { a <int> b <int> }\nP(1, 2).b"), "2");
    assert_eq!(run("struct P { a <int> b <int> }\nP{a: 1, b: 3}.b"), "3");
}

#[test]
fn trailing_content_after_a_json_value_is_rejected() {
    let out = run("introduce json\njson.parse(\"1 2\")");
    assert!(out.contains("trailing content"), "{out}");
}

#[test]
fn well_formed_json_still_parses() {
    assert_eq!(run("introduce json\njson.parse(\"42\")"), "42");
    assert_eq!(run("introduce json\njson.parse(\"  [1, 2]  \")"), "[1, 2]");
}

#[test]
fn cancel_on_a_non_task_is_rejected() {
    let out = run("cancel(42)");
    assert!(out.contains("requires a task"), "{out}");
}

#[test]
fn cancel_on_a_real_task_still_works() {
    let source = "t := diverge { 1 }\ncancel(t)\n\"ok\"";
    for mut vm in [VM::new_interpreter(), VM::new_eager_jit()] {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer, source);
        let program = parser.parse_program();
        let function = Compiler::new()
            .compile(&program)
            .unwrap_or_else(|e| panic!("compile errors: {e:?}"));
        oxigen_core::concurrent::register_program(&function, &program, source, None, false);
        let out = vm
            .run(function)
            .map(|v| format!("{v}"))
            .unwrap_or_else(|e| e.message);
        assert_eq!(out, "ok");
    }
}

#[test]
fn spawning_from_an_unregistered_program_is_reported() {
    let out = run("t := diverge { 1 }\n\"ok\"");
    assert!(out.contains("not registered"), "{out}");
}

#[test]
fn every_runtime_error_now_carries_a_code() {
    for src in [
        "a := [1]\na[9]",
        "1 / 0",
        "fun f(a, b) { a + b }\nf(1)",
        "undefined_name",
    ] {
        let out = run(src);
        assert!(out.contains("error[E"), "no code in:\n{src}\n{out}");
    }
}
