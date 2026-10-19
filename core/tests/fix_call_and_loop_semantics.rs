//! Regression: assorted call, loop and scoping checks that one path enforced
//! and another skipped.
//!
//! - A JIT-compiled callee skipped its parameter type check: once
//!   `fun sq(n <int>)` had tiered up, `sq(1.5)` returned 2.25.
//! - The lowered `each ... in range(...)` loop wrapped past i64::MAX and
//!   accepted float/string bounds the `range` builtin rejects.
//! - The loop's synthetic locals (`__index__`, `__iterable__`, ...) were
//!   ordinary names, so user code could read and overwrite them and end or
//!   redirect the loop.
//! - A bare field name inside a lambda nested in a method resolved to a
//!   global instead of `self`'s field.
//! - Enum payload annotations were never checked.
//! - Arity errors for methods counted the implicit `self`.

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
    let function = Compiler::new().compile(&program).map_err(|errs| {
        errs.iter()
            .map(|e| e.message.clone())
            .collect::<Vec<_>>()
            .join("; ")
    })?;
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

#[test]
fn typed_parameter_is_checked_after_the_callee_tiers_up() {
    let src = "fun sq(n <int>) { n * n }\n\
               fun drive() {\n  t := 0\n  each i in range(300) { t = t + sq(i) }\n  t\n}\n\
               drive()\ndrive()\nsq(1.5)";
    assert_all_err(src, "type mismatch for parameter 'n'");
}

#[test]
fn typed_method_parameter_is_checked_after_tier_up() {
    let src = "struct C { k <int> }\nC includes { fun twice(x <int>) { x * 2 } }\nc := C(1)\n\
               fun drive() {\n  t := 0\n  each i in range(300) { t = t + c.twice(i) }\n  t\n}\n\
               drive()\ndrive()\nc.twice(2.5)";
    assert_all_err(src, "type mismatch for parameter 'x'");
}

#[test]
fn lowered_range_loop_does_not_wrap_past_the_end() {
    let src = "n := 0\neach i in range(9223372036854775800, 9223372036854775807, 5) {\n  n = n + 1\n  stop when n == 20\n}\nn";
    assert_all_ok(src, "2");
}

#[test]
fn lowered_range_loop_rejects_what_the_builtin_rejects() {
    assert_all_err("n := 0\neach i in range(0.5, 3) { n = n + 1 }\nn", "range");
    assert_all_err("n := 0\neach i in range(\"a\", \"c\") { n = n + 1 }\nn", "range");
}

#[test]
fn loop_internals_are_not_user_visible() {
    for (name, make) in backends() {
        let res = run_on(
            make(),
            "fun f() {\n  n := 0\n  each x in [1, 2, 3] {\n    __index__ = 100\n    n = n + 1\n  }\n  n\n}\nf()",
        );
        match res {
            Ok(v) => assert_eq!(v, "3", "{name}: writing __index__ changed the loop"),
            Err(e) => assert!(e.contains("__index__"), "{name}: {e}"),
        }
    }
}

#[test]
fn bare_field_name_in_a_lambda_inside_a_method_is_the_field() {
    let src = "count := 100\nstruct C { count <int> }\n\
               C includes {\n  fun w() {\n    g := fun() { count = 7 }\n    g()\n    self.count\n  }\n  fun r() {\n    h := fun() { count + 1 }\n    h()\n  }\n}\n\
               c := C(5)\n[c.w(), count, c.r()]";
    assert_all_ok(src, "[7, 100, 8]");
}

#[test]
fn enum_payload_annotations_are_checked() {
    let e = "enum Shape {\n  Circle(radius <float>)\n  Rect { w <int>, h <int> }\n}\n";
    assert_all_err(&format!("{e}Shape.Circle(\"not a float\")"), "type mismatch");
    assert_all_err(&format!("{e}Shape.Rect {{ w: \"x\", h: 2 }}"), "type mismatch");
    assert_all_ok(&format!("{e}c := Shape.Circle(1.5)\nr := Shape.Rect {{ w: 3, h: 4 }}\n[c.radius, r.w * r.h]"), "[1.5, 12]");
}

#[test]
fn method_arity_errors_do_not_count_self() {
    let src = "struct C { k <int> }\nC includes { fun one(x) { x } }\nc := C(1)\nc.one()";
    assert_all_err(src, "takes 1 argument but 0 were supplied");
}
