use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;

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
    let source = std::fs::read_to_string("../../example/hello_world.oxi").unwrap();
    assert_reparseable(&source, "hello_world");
    assert_idempotent(&source, "hello_world");
}

#[test]
fn test_struct_example_roundtrip() {
    let source = std::fs::read_to_string("../../example/struct_example.oxi").unwrap();
    assert_reparseable(&source, "struct_example");
    assert_idempotent(&source, "struct_example");
}

#[test]
fn test_generic_example_roundtrip() {
    let source = std::fs::read_to_string("../../example/generic_example.oxi").unwrap();
    assert_reparseable(&source, "generic_example");
    assert_idempotent(&source, "generic_example");
}

#[test]
fn test_options_roundtrip() {
    let source = std::fs::read_to_string("../../example/options.oxi").unwrap();
    assert_reparseable(&source, "options");
    assert_idempotent(&source, "options");
}

#[test]
fn test_import_test_roundtrip() {
    let source = std::fs::read_to_string("../../example/import_test.oxi").unwrap();
    assert_reparseable(&source, "import_test");
    assert_idempotent(&source, "import_test");
}

#[test]
fn test_simple_auth_roundtrip() {
    let source = std::fs::read_to_string("../../example/simple_auth.oxi").unwrap();
    assert_reparseable(&source, "simple_auth");
    assert_idempotent(&source, "simple_auth");
}
