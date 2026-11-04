//! Declared return types: `fun area(w <int>, h <int>) <int> { w * h }`.
//!
//! Every value a function returns, from its last expression or from a
//! `give`, is checked against the type written after its parameter list.
//! Each program must give the same value — or the same error — under the
//! interpreter, the tiered JIT and the eager JIT, including after warm-up,
//! where the JIT skips checks it can prove.

#![cfg(feature = "jit")]

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
    Compiler::new().compile(&program).expect("compile")
}

fn run_in(mut vm: VM, source: &str) -> String {
    vm.set_source(source);
    match vm.run(compile(source)) {
        Ok(v) => format!("{v}"),
        Err(e) => format!("error: {}", e.message),
    }
}

fn outcomes(source: &str) -> [(&'static str, String); 3] {
    [
        ("interpreter", run_in(VM::new_interpreter(), source)),
        ("tiered", run_in(VM::new_tiered(), source)),
        ("eager", run_in(VM::new_eager_jit(), source)),
    ]
}

/// All three backends agree, and the value is `expected`.
fn assert_all(source: &str, expected: &str) {
    for (name, got) in outcomes(source) {
        assert_eq!(got, expected, "{name} mismatch for:\n{source}");
    }
}

/// All three backends fail with the same error, which contains `needle`.
fn assert_all_fail(source: &str, needle: &str) {
    let all = outcomes(source);
    let interp = &all[0].1;
    assert!(
        interp.starts_with("error: ") && interp.contains(needle),
        "interpreter: {interp}"
    );
    for (name, got) in &all[1..] {
        assert_eq!(
            got, interp,
            "{name} differs from the interpreter for:\n{source}"
        );
    }
}

// ── Values that match ──────────────────────────────────────────────────

#[test]
fn matching_return_passes() {
    assert_all(
        "fun area(w <int>, h <int>) <int> { w * h }\narea(3, 4)",
        "12",
    );
}

#[test]
fn unannotated_functions_are_unchanged() {
    assert_all("fun f(x) { x }\n[f(1), f(\"a\")]", "[1, a]");
}

#[test]
fn generic_accepts_anything() {
    assert_all("fun id(x) <generic> { x }\n[id(1), id(\"a\")]", "[1, a]");
}

#[test]
fn union_with_none_accepts_both() {
    assert_all(
        "fun find(n <int>) <int || None> {\n    give None when n < 0\n    n\n}\n[find(-1), find(2)]",
        "[None, 2]",
    );
}

#[test]
fn struct_return_type() {
    assert_all(
        "struct P {\n    x <int>\n}\nfun make(v <int>) <P> { P(v) }\nmake(7).x",
        "7",
    );
}

#[test]
fn method_return_type() {
    assert_all(
        "struct C {\n    r <float>\n}\nC includes {\n    fun double() <float> { self.r * 2.0 }\n}\nc <C> := C(1.5)\nc.double()",
        "3",
    );
}

#[test]
fn plain_error_member_accepts_tagged_errors() {
    assert_all(
        "fun check(n <int>) <int || Error> {\n    option {\n        n < 0 -> <Error<negative>>(\"below zero\"),\n        n\n    }\n}\n[is_error(check(-1)), check(5)]",
        "[True, 5]",
    );
}

#[test]
fn indent_mode_signature() {
    assert_all(
        "#[indent]\nfun area(w <int>, h <int>) <int>:\n    w * h\narea(2, 3)",
        "6",
    );
}

#[test]
fn bool_return_from_comparison() {
    assert_all(
        "fun small(n <int>) <bool> { n < 2 }\nfun drive() {\n    i := 0\n    t := 0\n    repeat when i < 200 {\n        t = t + option { small(i) -> 1, 0 }\n        i = i + 1\n    }\n    t\n}\ndrive()",
        "2",
    );
}

// ── Hot functions: the JIT's skipped checks must still be right ────────

#[test]
fn hot_int_function() {
    assert_all(
        "fun sq(n <int>) <int> { n * n }\nfun drive() {\n    i := 0\n    t := 0\n    repeat when i < 200 {\n        t = t + sq(i)\n        i = i + 1\n    }\n    t\n}\ndrive()",
        "2646700",
    );
}

#[test]
fn recursive_int_function() {
    assert_all(
        "fun fib(n <int>) <int> {\n    option {\n        n < 2 -> n,\n        fib(n - 1) + fib(n - 2)\n    }\n}\nfib(20)",
        "6765",
    );
}

#[test]
fn mismatch_after_warmup() {
    assert_all_fail(
        "fun pick(n) <int> {\n    option {\n        n < 150 -> n,\n        \"big\"\n    }\n}\nfun drive() {\n    i := 0\n    t := 0\n    repeat when i < 200 {\n        t = t + pick(i)\n        i = i + 1\n    }\n    t\n}\ndrive()",
        "return type mismatch in 'pick': expected INTEGER, got STRING",
    );
}

#[test]
fn comparison_is_not_an_int_return() {
    assert_all_fail(
        "fun small(n <int>) <int> { n < 2 }\nfun drive() {\n    i := 0\n    repeat when i < 200 {\n        small(i)\n        i = i + 1\n    }\n}\ndrive()",
        "return type mismatch in 'small': expected INTEGER, got BOOLEAN",
    );
}

// ── Values that don't ──────────────────────────────────────────────────

#[test]
fn implicit_return_is_checked_without_conversion() {
    assert_all_fail(
        "fun half(n <int>) <int> { n / 2.0 }\nhalf(3)",
        "return type mismatch in 'half': expected INTEGER, got FLOAT",
    );
}

#[test]
fn give_is_checked() {
    assert_all_fail(
        "fun label(n <int>) <str> {\n    give 0 when n < 0\n    \"ok\"\n}\nlabel(-1)",
        "return type mismatch in 'label': expected STRING, got INTEGER",
    );
}

#[test]
fn none_needs_to_be_declared() {
    assert_all_fail(
        "fun find(n <int>) <int> {\n    give None when n < 0\n    n\n}\nfind(-1)",
        "expected INTEGER, got NONE",
    );
}

#[test]
fn error_value_needs_to_be_declared() {
    assert_all_fail(
        "fun parse() <int> { <Error>(\"bad\") }\nparse()",
        "return type mismatch in 'parse': expected INTEGER, got ERROR",
    );
}

#[test]
fn fail_passes_through_the_check() {
    assert_all_fail(
        "fun parse() <int> { <fail>(\"bad input\") }\nparse()",
        "bad input",
    );
}

#[test]
fn anonymous_function_is_checked() {
    assert_all_fail(
        "fun apply(g) { g(1) }\napply(fun(x <int>) <str> { x })",
        "return type mismatch in anonymous function: expected STRING, got INTEGER",
    );
}

#[test]
fn method_mismatch_names_the_method() {
    assert_all_fail(
        "struct C {\n    r <float>\n}\nC includes {\n    fun size() <int> { self.r }\n}\nc <C> := C(1.5)\nc.size()",
        "return type mismatch in 'size': expected INTEGER, got FLOAT",
    );
}
