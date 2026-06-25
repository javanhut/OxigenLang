use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use std::path::PathBuf;

fn example_source(name: &str) -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("example")
        .join(name);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|err| panic!("failed to read {}: {}", path.display(), err))
}

fn format_source(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "Parse errors: {:?}",
        parser.errors()
    );
    Formatter::format(&program)
}

/// Verify formatter output re-parses without errors
fn assert_reparseable(source: &str, label: &str) {
    let formatted = format_source(source);
    let lexer = Lexer::new(&formatted);
    let mut parser = Parser::new(lexer, &formatted);
    let _program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "{}: Formatted output has parse errors:\n{}\n---\nFormatted:\n{}",
        label,
        parser.format_errors(),
        formatted
    );
}

/// Verify formatter is idempotent (formatting twice gives same result)
fn assert_idempotent(source: &str, label: &str) {
    let first = format_source(source);
    let second = format_source(&first);
    assert_eq!(
        first, second,
        "{}: Formatter is not idempotent.\nFirst:\n{}\nSecond:\n{}",
        label, first, second
    );
}

#[test]
fn test_hello_world_roundtrip() {
    let source = example_source("hello_world.oxi");
    assert_reparseable(&source, "hello_world");
    assert_idempotent(&source, "hello_world");
}

#[test]
fn test_struct_example_roundtrip() {
    let source = example_source("struct_example.oxi");
    assert_reparseable(&source, "struct_example");
    assert_idempotent(&source, "struct_example");
}

#[test]
fn test_generic_example_roundtrip() {
    let source = example_source("generic_example.oxi");
    assert_reparseable(&source, "generic_example");
    assert_idempotent(&source, "generic_example");
}

#[test]
fn test_options_roundtrip() {
    let source = example_source("options.oxi");
    assert_reparseable(&source, "options");
    assert_idempotent(&source, "options");
}

#[test]
fn test_import_test_roundtrip() {
    let source = example_source("import_test.oxi");
    assert_reparseable(&source, "import_test");
    assert_idempotent(&source, "import_test");
}

#[test]
fn test_multiline_strings_roundtrip() {
    let source = example_source("multiline_strings.oxi");
    assert_reparseable(&source, "multiline_strings");
    assert_idempotent(&source, "multiline_strings");
}

#[test]
fn test_multiline_string_keeps_triple_quotes_and_raw_newlines() {
    // A triple-quoted string must format back to triple quotes with its raw
    // newlines intact — not collapse to a single-line `"...\n..."`.
    let source = "x := \"\"\"\nline one\nline two\n\"\"\"\n";
    let formatted = format_source(source);

    assert!(
        formatted.contains("\"\"\""),
        "triple quotes not preserved:\n{}",
        formatted
    );
    assert!(
        formatted.contains("line one\nline two"),
        "raw newlines not preserved:\n{}",
        formatted
    );
    assert!(
        !formatted.contains("\\n"),
        "newlines inside a triple-quoted string must stay raw:\n{}",
        formatted
    );
    assert_reparseable(source, "multiline_decl");
    assert_idempotent(source, "multiline_decl");
}

#[test]
fn test_single_line_string_is_not_promoted_to_triple() {
    // Regression guard: a single-line string written with `\n` escapes must
    // stay single-line and escaped — only triple-quoted sources format as
    // multi-line strings.
    let source = "x := \"a\\nb\"\n";
    let formatted = format_source(source);

    assert!(
        !formatted.contains("\"\"\""),
        "single-line string must not become triple-quoted:\n{}",
        formatted
    );
    assert!(
        formatted.contains("\"a\\nb\""),
        "escaped single-line form not preserved:\n{}",
        formatted
    );
}

#[test]
fn test_diverge_converge_keep_high_level_syntax() {
    // Regression guard: the formatter must round-trip `diverge`/`converge`
    // surface syntax, NOT lower it to the `__spawn`/`__join_task` builtins.
    let source = "main {\n    \
        sq := diverge each n in nums {\n        n * n\n    }\n    \
        work := diverge {\n        slow()\n    }\n    \
        r := converge work within 500\n    \
        all := converge [work]\n}\n";
    let formatted = format_source(source);

    for kw in ["diverge each ", "diverge {", "converge work", "within 500"] {
        assert!(
            formatted.contains(kw),
            "high-level concurrency syntax `{}` not preserved:\n{}",
            kw,
            formatted
        );
    }
    assert!(
        !formatted.contains("__spawn") && !formatted.contains("__join_task"),
        "formatter leaked the lowered builtins:\n{}",
        formatted
    );
    assert_reparseable(source, "diverge_converge");
    assert_idempotent(source, "diverge_converge");
}

#[test]
fn test_multiline_string_with_interpolation_roundtrip() {
    // Interpolation inside a triple-quoted string round-trips, keeping the
    // triple quotes and the literal newlines around the `{...}` expression.
    let source = "msg := \"\"\"\nHello, {name}!\nBye.\n\"\"\"\n";
    let formatted = format_source(source);

    assert!(
        formatted.contains("\"\"\""),
        "triple quotes not preserved for interpolated string:\n{}",
        formatted
    );
    assert!(
        formatted.contains("{name}"),
        "interpolation not preserved:\n{}",
        formatted
    );
    assert_reparseable(source, "multiline_interp");
    assert_idempotent(source, "multiline_interp");
}
