// Regression tests for the parser lane (P1 struct inheritance, P2 hidden fields).
//
// These assert PARSING + AST shape only. Full runtime inheritance / access-control
// semantics live in the compiler + vm and are intentionally deferred;
// see docs/VM_PARITY_FIXES.md for the cross-file spec.

use oxigen_core::ast::{Program, Statement};
use oxigen_core::{lexer::Lexer, parser::Parser};

fn parse_ok(src: &str) -> Program {
    let lexer = Lexer::new(src);
    let mut parser = Parser::new(lexer, src);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "expected no parser errors for {src:?}, got: {:?}",
        parser.errors()
    );
    program
}

fn first_struct(program: &Program) -> (&Option<oxigen_core::ast::Identifier>, &Vec<oxigen_core::ast::StructField>) {
    match &program.statements[0] {
        Statement::StructDef { parent, fields, .. } => (parent, fields),
        other => panic!("expected StructDef as first statement, got {other:?}"),
    }
}

// ---------- P1: struct inheritance via `includes ParentName` ----------

#[test]
fn struct_includes_parent_parses_with_parent_in_ast() {
    let program = parse_ok("struct Dog includes Animal { name <str> }");
    let (parent, fields) = first_struct(&program);
    let parent = parent.as_ref().expect("parent should be recorded in AST");
    assert_eq!(parent.value, "Animal");
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name.value, "name");
}

#[test]
fn struct_includes_parent_multiline_and_multifield() {
    let src = "struct Dog includes Animal {\n  name <str>\n  age <int>\n}";
    let program = parse_ok(src);
    let (parent, fields) = first_struct(&program);
    assert_eq!(parent.as_ref().unwrap().value, "Animal");
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name.value, "name");
    assert_eq!(fields[1].name.value, "age");
}

#[test]
fn struct_parenthesized_parent_still_parses() {
    // The pre-existing `struct Child(Parent)` form must keep working.
    let program = parse_ok("struct American(Person) { age <int> }");
    let (parent, _fields) = first_struct(&program);
    assert_eq!(parent.as_ref().unwrap().value, "Person");
}

// ---------- P2: hidden fields ----------

#[test]
fn struct_hidden_field_keyword_parses_with_flag() {
    let program = parse_ok("struct S { hidden secret <int>\n public <int> }");
    let (_parent, fields) = first_struct(&program);
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name.value, "secret");
    assert!(fields[0].hidden, "field marked `hidden` should set hidden flag");
    assert_eq!(fields[1].name.value, "public");
    assert!(!fields[1].hidden, "unmarked field should not be hidden");
}

#[test]
fn struct_hide_keyword_still_marks_hidden() {
    // The pre-existing `hide` keyword form must keep working.
    let program = parse_ok("struct Person {\n  hide name <str>\n  age <int>\n}");
    let (_parent, fields) = first_struct(&program);
    assert!(fields[0].hidden);
    assert!(!fields[1].hidden);
}

#[test]
fn field_literally_named_hidden_is_not_a_modifier() {
    // `hidden <int>` is a field NAMED `hidden`, not a visibility modifier.
    let program = parse_ok("struct H { hidden <int> }");
    let (_parent, fields) = first_struct(&program);
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name.value, "hidden");
    assert!(!fields[0].hidden);
}

#[test]
fn struct_inheritance_and_hidden_combined() {
    let program = parse_ok("struct Dog includes Animal { hidden chip <int>\n name <str> }");
    let (parent, fields) = first_struct(&program);
    assert_eq!(parent.as_ref().unwrap().value, "Animal");
    assert_eq!(fields.len(), 2);
    assert!(fields[0].hidden);
    assert_eq!(fields[0].name.value, "chip");
    assert!(!fields[1].hidden);
    assert_eq!(fields[1].name.value, "name");
}

// ---------- regressions: untouched forms ----------

#[test]
fn plain_struct_still_parses() {
    let program = parse_ok("struct P { x <int> }");
    let (parent, fields) = first_struct(&program);
    assert!(parent.is_none());
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name.value, "x");
    assert!(!fields[0].hidden);
}

#[test]
fn method_include_block_still_parses() {
    // `Name includes { fun ... }` is a separate IncludesDef statement, NOT a
    // struct-inheritance parent. The struct decl and the method block must both
    // parse as distinct statements.
    let program = parse_ok("struct C { r <int> }\nC includes { fun area(){ r } }");
    assert!(matches!(program.statements[0], Statement::StructDef { .. }));
    assert!(matches!(
        program.statements[1],
        Statement::IncludesDef { .. }
    ));
}
