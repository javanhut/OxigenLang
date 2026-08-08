//! `oxigen fmt` must not turn working source into source that no longer parses.
//!
//! `<type<T>>(expr)` came back out as `expr as <T>` — a syntax the language has
//! never had. The result failed at the *parser* stage, so a single `oxigen fmt`
//! over a project silently broke every file using the two-tier error model.
//!
//! The guard is round-tripping rather than exact output: format, re-parse, and
//! require the result to still be valid. That is the property that was lost, and
//! it holds whichever spelling the formatter settles on for a union — both
//! `<A || B>` and `<A> || <B>` parse, and the stdlib uses the second.

use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;

fn format_source(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors in input:\n{}",
        parser.format_errors()
    );
    Formatter::format(&program)
}

/// Format, then parse the output. Panics with the formatter's own output if the
/// round trip does not survive.
fn reformat_and_reparse(source: &str) -> String {
    let formatted = format_source(source);
    let lexer = Lexer::new(&formatted);
    let mut parser = Parser::new(lexer, &formatted);
    let _ = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "formatter emitted source that no longer parses:\n{}\nerrors:\n{}",
        formatted,
        parser.format_errors()
    );
    formatted
}

#[test]
fn expected_result_normalization_survives_formatting() {
    let out = reformat_and_reparse(
        r#"
fun risky() { 1 }

main {
    result := <type<Error || Value>>(risky())
    println(result)
}
"#,
    );
    assert!(
        out.contains("<type<"),
        "the angle form was rewritten into something else:\n{}",
        out
    );
    assert!(
        !out.contains(" as <"),
        "the `as` syntax, which does not exist, reappeared:\n{}",
        out
    );
}

#[test]
fn tagged_and_plain_type_wraps_survive_formatting() {
    let out = reformat_and_reparse(
        r#"
main {
    tagged := <type<Error<non_integer> || Value>>(int("x"))
    plain := <type<int>>("7")
    println([tagged, plain])
}
"#,
    );
    // The tag is the whole point of the tagged form — losing it would silently
    // change which errors a `guard` filters on.
    assert!(
        out.contains("Error<non_integer>"),
        "the error tag was dropped:\n{}",
        out
    );
    assert!(
        out.contains("<type<int>>(\"7\")"),
        "single-type wrap lost its shape:\n{}",
        out
    );
}

#[test]
fn union_annotations_survive_formatting() {
    let out = reformat_and_reparse(
        r#"
fun risky() { 1 }

main {
    outcome <Error || Value> := <type<Error || Value>>(risky())
    println(outcome)
}
"#,
    );
    assert!(
        out.contains("Error") && out.contains("Value"),
        "a union member was dropped from the annotation:\n{}",
        out
    );
}

/// The stdlib writes unions as `<int> || <float>`; formatting must leave that
/// spelling alone rather than churn every signature in math.oxi.
#[test]
fn split_bracket_unions_are_left_alone() {
    let out = reformat_and_reparse("fun abs(x <int> || <float>) { x }\n");
    assert!(
        out.contains("x <int> || <float>"),
        "the stdlib's union spelling was rewritten:\n{}",
        out
    );
}

#[test]
fn formatting_is_idempotent_for_angle_forms() {
    let source = r#"
fun risky() { 1 }

main {
    outcome <Error || Value> := <type<Error || Value>>(risky())
    println(outcome)
}
"#;
    let once = reformat_and_reparse(source);
    let twice = reformat_and_reparse(&once);
    assert_eq!(once, twice, "formatting angle forms is not idempotent");
}
