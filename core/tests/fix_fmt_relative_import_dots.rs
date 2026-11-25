//! `oxigen fmt` keeps which module an `introduce` resolves to, including
//! leading dots of relative imports.

use oxigen_core::ast::{ModulePath, Statement};
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

/// The module path every `introduce` in `source` resolves to, in order.
fn imported_paths(source: &str) -> Vec<ModulePath> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    program
        .statements
        .iter()
        .filter_map(|s| match s {
            Statement::Introduce { path, .. } => Some(path.clone()),
            _ => None,
        })
        .collect()
}

/// Formatting must be a no-op on already-formatted source, and must never change
/// which module is being imported.
fn assert_import_survives(source: &str) {
    let formatted = format_source(source);
    assert_eq!(
        formatted.trim(),
        source.trim(),
        "formatter rewrote the import"
    );
    assert_eq!(
        imported_paths(&formatted),
        imported_paths(source),
        "formatted source resolves to a different module"
    );
}

#[test]
fn relative_whole_module_imports_keep_their_dots() {
    assert_import_survives("introduce .mylib");
    assert_import_survives("introduce .utils.strings");
    assert_import_survives("introduce ..up.mod");
    assert_import_survives("introduce ...far.mod");
}

#[test]
fn relative_selective_imports_keep_their_dots() {
    assert_import_survives("introduce {a, b} from .other");
    assert_import_survives("introduce {parse} from ..up.mod");
}

#[test]
fn absolute_imports_gain_no_dots() {
    assert_import_survives("introduce math");
    assert_import_survives("introduce {sqrt} from math");
}

#[test]
fn a_local_module_is_never_rewritten_into_the_stdlib_one() {
    let formatted = format_source("introduce .json");
    assert!(
        formatted.contains(".json"),
        "leading dot dropped: {formatted}"
    );
    let path = &imported_paths(&formatted)[0];
    assert!(path.is_relative, "local import became a stdlib import");
    assert_eq!(path.parent_levels, 0);
    assert_eq!(path.segments, vec!["json".to_string()]);
}

#[test]
fn the_cli_path_keeps_the_dots_too() {
    let source = "// header\nintroduce ..up.mod\n";
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(parser.errors().is_empty(), "{}", parser.format_errors());
    let formatted = Formatter::format_source(&program, parser.comments(), false);
    assert!(
        formatted.contains("introduce ..up.mod"),
        "dots lost on the CLI path: {formatted}"
    );
}

#[test]
fn formatting_is_idempotent_for_relative_imports() {
    let once = format_source("introduce ..up.mod");
    let twice = format_source(&once);
    assert_eq!(once, twice);
}
