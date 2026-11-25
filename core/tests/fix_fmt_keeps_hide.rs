//! Tests that `oxigen fmt` keeps `hide` on a private top-level `fun`.

use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;

fn format_source(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    Formatter::format(&program)
}

#[test]
fn hide_survives_formatting() {
    let out = format_source("hide fun __helper() { 42 }\n");
    assert!(
        out.contains("hide fun __helper"),
        "formatter dropped `hide`, publishing the function:\n{out}"
    );
}

#[test]
fn only_the_hidden_function_keeps_the_keyword() {
    let out = format_source(
        "hide fun __private() { 1 }\n\nfun public_one() { 2 }\n\nhide fun __also() { 3 }\n",
    );
    assert!(out.contains("hide fun __private"), "{out}");
    assert!(out.contains("hide fun __also"), "{out}");
    assert!(
        out.contains("\nfun public_one") || out.starts_with("fun public_one"),
        "public function was marked hidden:\n{out}"
    );
}

#[test]
fn formatting_is_idempotent() {
    let once = format_source("hide fun __helper() { 42 }\n\nfun api() { __helper() }\n");
    let twice = format_source(&once);
    assert_eq!(once, twice, "second format changed the file");
}

#[test]
fn a_struct_fields_hide_is_still_preserved() {
    let out = format_source("struct Account {\n    hide balance <int>\n    owner <str>\n}\n");
    assert!(out.contains("hide balance"), "{out}");
}
