//! `oxigen check` beyond syntax: names nothing defines, logic that compiles but
//! reads differently from what it does, and keywords from other languages.

use oxigen_core::compiler::Compiler;
use oxigen_core::diagnostics::{Diagnostic, Severity, registry};
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;

fn check(src: &str) -> Vec<Diagnostic> {
    let mut parser = Parser::new(Lexer::new(src), src);
    let program = parser.parse_program();
    assert!(parser.errors().is_empty(), "parse errors: {:?}", parser.errors());
    let mut builtins = std::collections::HashMap::new();
    oxigen_core::vm::builtins::register_builtins(&mut builtins);
    Compiler::new().check(&program, &|n: &str| builtins.contains_key(n))
}

fn codes(src: &str) -> Vec<(&'static str, Severity)> {
    check(src).iter().map(|d| (d.code.0, d.severity)).collect()
}

#[test]
fn an_undefined_name_is_an_error() {
    let src = "fun reply(raw) {\n    data := raw\n    object.message\n}\n";
    let found = check(src);
    assert_eq!(found.len(), 1, "{found:?}");
    assert_eq!(found[0].code, registry::UNDEFINED_NAME);
    assert_eq!(found[0].severity, Severity::Error);
    assert!(found[0].message.contains("`object`"));
}

#[test]
fn defined_names_are_not_reported() {
    let src = r#"
introduce json
introduce {join} from strings
struct Point { x <int> }
enum Color { red, green }
fun later() { helper() + count }
fun helper() { 1 }
count := 2
println(json.stringify([1]), join(["a"], ","), Point(1), Color.red, later(), len("x"))
each i in range(3) { println(i) }
"#;
    assert_eq!(codes(src), vec![]);
}

#[test]
fn each_undefined_name_is_reported_once() {
    assert_eq!(codes("println(nope)\nprintln(nope)\n").len(), 1);
}

#[test]
fn a_literal_after_a_comparison_is_a_warning() {
    let found = codes("cmd := \"x\"\nprintln(cmd == \"quit\" or \"exit\")\n");
    assert_eq!(found, vec![("E0039", Severity::Warning)]);
}

#[test]
fn a_literal_on_the_left_of_or_is_a_warning() {
    assert_eq!(codes("t := (\"quit\" or \"exit\")\nprintln(t)\n"), vec![("E0039", Severity::Warning)]);
}

#[test]
fn not_equal_to_a_distributed_or_is_a_warning() {
    assert_eq!(codes("c := 1\nprintln(c != (1 or 2))\n"), vec![("E0039", Severity::Warning)]);
}

#[test]
fn intended_forms_are_quiet() {
    let src = "cmd := \"x\"\nname := None\nprintln(cmd == (\"quit\" or \"exit\"), name or \"anonymous\", cmd == \"a\" or cmd == \"b\")\n";
    assert_eq!(codes(src), vec![]);
}

#[test]
fn return_explains_give() {
    let src = "fun f(reply) {\n    return reply\n}\n";
    let mut parser = Parser::new(Lexer::new(src), src);
    parser.parse_program();
    let first = &parser.errors()[0];
    assert!(first.message.contains("`return` is not an Oxigen keyword"), "{}", first.message);
    let text = parser.format_errors();
    assert!(text.contains("give value"), "{text}");
}
