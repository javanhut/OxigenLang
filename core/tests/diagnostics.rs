//! The unified diagnostic type, its registry, and its renderers.
//!
//! Before this, four stages reported problems in four incompatible shapes, a
//! diagnostic could only point at one character, and nothing had a stable
//! identity — so tests had to assert on prose, which made improving a message a
//! breaking change.

use oxigen_core::diagnostics::registry;
use oxigen_core::diagnostics::{
    Applicability, Code, Diagnostic, DiagnosticSink, Edit, SourceFile, render,
};
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::token::{Pos, Span};

fn span(line: usize, col: usize, start: usize, end: usize) -> Span {
    Span::range(Pos::new(line, col, start), Pos::new(line, col + (end - start), end))
}

/// Parses and returns the rendered diagnostics.
fn render_parse_errors(src: &str) -> String {
    let lexer = Lexer::new(src);
    let mut parser = Parser::with_file(lexer, src, "test.oxi");
    parser.parse_program();
    parser.format_errors()
}

// ── registry ────────────────────────────────────────────────────────────────

#[test]
fn every_registered_code_is_unique() {
    // Codes are permanent identifiers; a duplicate would make `explain`
    // ambiguous and suppression unpredictable.
    let mut seen = std::collections::HashSet::new();
    for entry in registry::all() {
        assert!(
            seen.insert(entry.code),
            "duplicate code in registry: {}",
            entry.code
        );
    }
}

#[test]
fn every_registered_code_has_a_title_and_explanation() {
    for entry in registry::all() {
        assert!(!entry.title.is_empty(), "{} has no title", entry.code);
        assert!(
            entry.explanation.trim().len() > 40,
            "{} needs a real explanation, not a stub",
            entry.code
        );
    }
}

#[test]
fn explain_finds_a_code_case_insensitively() {
    let text = render::explain("e0003").expect("E0003 should be explicable");
    assert!(text.contains("E0003"), "{text}");
    assert!(render::explain("E9999").is_none());
}

// ── rendering ───────────────────────────────────────────────────────────────

#[test]
fn a_range_is_underlined_across_its_width() {
    // The whole point of widening spans: previously this was a single caret
    // regardless of how much text was wrong.
    let src = SourceFile::named("t.oxi", "value := 1234\n");
    let d = Diagnostic::error(Code("E0003"), span(1, 10, 9, 13), "bad literal");
    let out = render::render_human(&d, &src);
    assert!(out.contains("^^^^"), "expected a 4-wide underline:\n{out}");
}

#[test]
fn a_point_span_still_renders_one_caret() {
    let src = SourceFile::named("t.oxi", "value := 1\n");
    let d = Diagnostic::error(Code("E0003"), Span::new(1, 7), "here");
    let out = render::render_human(&d, &src);
    assert!(out.contains("^"), "{out}");
    assert!(!out.contains("^^"), "point span should be one caret:\n{out}");
}

#[test]
fn the_filename_is_named_so_terminals_can_link_it() {
    let src = SourceFile::named("app.oxi", "x := 1\n");
    let d = Diagnostic::error(Code("E0003"), Span::new(1, 1), "nope");
    assert!(render::render_human(&d, &src).contains("--> app.oxi:1:1"));

    // Without a name, fall back to the old form rather than inventing one.
    let anon = SourceFile::new("x := 1\n");
    assert!(render::render_human(&d, &anon).contains("--> line 1:1"));
}

#[test]
fn secondary_labels_notes_and_helps_all_render() {
    let text = "PI <float> = 3.14159\nPI = 3.0\n";
    let src = SourceFile::named("c.oxi", text);
    let d = Diagnostic::error(Code("E0015"), span(2, 1, 21, 29), "cannot assign to `PI`")
        .label("cannot assign")
        .secondary(span(1, 1, 0, 20), "declared immutable here with `=`")
        .note("`=` binds immutably; `:=` binds mutably")
        .help("rebind with `:=`");
    let out = render::render_human(&d, &src);

    assert!(out.contains("declared immutable here"), "{out}");
    assert!(out.contains("cannot assign"), "{out}");
    assert!(out.contains("= note: `=` binds immutably"), "{out}");
    assert!(out.contains("= help: rebind with `:=`"), "{out}");
    // The secondary is marked with dashes so it reads as context, not a second
    // error.
    assert!(out.contains("---"), "secondary should underline with dashes:\n{out}");
}

#[test]
fn a_suggestion_shows_the_fixed_line() {
    let text = "PI = 3.0\n";
    let src = SourceFile::named("c.oxi", text);
    let d = Diagnostic::error(Code("E0015"), span(1, 1, 0, 8), "cannot assign").suggest(
        "rebind with `:=`",
        vec![Edit {
            span: span(1, 4, 3, 4),
            replacement: ":=".into(),
        }],
        Applicability::MaybeIncorrect,
    );
    let out = render::render_human(&d, &src);
    assert!(out.contains("PI := 3.0"), "expected the fixed line:\n{out}");
}

#[test]
fn an_unknown_location_does_not_invent_a_source_line() {
    // A site that only knows "something went wrong" must not print line 1's
    // text under a `0:0` header, which is what a naive renderer does.
    let src = SourceFile::named("t.oxi", "first line\nsecond line\n");
    let d = Diagnostic::error(Code("E0021"), Span::new(0, 0), "no location");
    let out = render::render_human(&d, &src);
    assert!(!out.contains("first line"), "invented a source line:\n{out}");
    assert!(!out.contains("0:0"), "{out}");
}

#[test]
fn short_form_is_greppable() {
    let src = SourceFile::named("app.oxi", "x := 1\n");
    let d = Diagnostic::error(Code("E0003"), Span::new(4, 9), "boom");
    assert_eq!(
        render::render_short(&d, &src),
        "app.oxi:4:9: error[E0003]: boom"
    );
}

#[test]
fn json_keeps_the_legacy_fields_and_adds_the_new_ones() {
    // The LSP reads the old shape; it must keep working while it migrates.
    let src = SourceFile::named("t.oxi", "x := 1\n");
    let d = Diagnostic::error(Code("E0003"), span(1, 1, 0, 6), "boom").help("try this");
    let v = render::render_json(&d, &src);

    for legacy in ["line", "column", "message", "suggestion", "severity"] {
        assert!(v.get(legacy).is_some(), "missing legacy field {legacy}");
    }
    assert_eq!(v["code"], "E0003");
    assert_eq!(v["primary"]["start"]["offset"], 0);
    assert_eq!(v["primary"]["end"]["offset"], 6);
    assert_eq!(v["suggestion"], "try this");
}

// ── sink ────────────────────────────────────────────────────────────────────

#[test]
fn the_sink_reports_whether_anything_is_fatal() {
    let mut sink = DiagnosticSink::new();
    assert!(!sink.has_errors());
    sink.push(Diagnostic::warning(Code("W0001"), Span::new(1, 1), "unused"));
    assert!(!sink.has_errors(), "a warning alone is not fatal");
    sink.push(Diagnostic::error(Code("E0003"), Span::new(1, 1), "bad"));
    assert!(sink.has_errors());
}

#[test]
fn deduplicate_drops_same_line_repeats_but_keeps_distinct_errors() {
    let mut sink = DiagnosticSink::new();
    // Two diagnostics inside the first one's span, same line — cascade noise.
    sink.push(Diagnostic::error(Code("E0003"), span(1, 1, 0, 10), "outer"));
    sink.push(Diagnostic::error(Code("E0003"), span(1, 3, 2, 4), "inner"));
    // A genuinely separate error further down must survive.
    sink.push(Diagnostic::error(Code("E0003"), span(5, 1, 40, 44), "elsewhere"));
    sink.deduplicate();

    let messages: Vec<&str> = sink.diagnostics().iter().map(|d| d.message.as_str()).collect();
    assert_eq!(messages, vec!["outer", "elsewhere"]);
}

// ── parser integration ──────────────────────────────────────────────────────

#[test]
fn parse_errors_carry_a_code_and_an_explain_pointer() {
    let out = render_parse_errors("main {\n  y :=\n}\n");
    assert!(out.contains("error[E"), "expected a code in:\n{out}");
    assert!(out.contains("oxigen explain"), "{out}");
    assert!(out.contains("--> test.oxi:"), "{out}");
}

#[test]
fn distinct_problems_get_distinct_codes() {
    // Codes are the stable interface, so different mistakes must be
    // distinguishable without reading the prose.
    let type_err = render_parse_errors("x <> := 1\n");
    let interp_err = render_parse_errors("println(\"{ }\")\n");
    let extract = |s: &str| {
        s.split("error[")
            .nth(1)
            .and_then(|r| r.split(']').next())
            .unwrap_or("")
            .to_string()
    };
    let a = extract(&type_err);
    let b = extract(&interp_err);
    assert!(!a.is_empty() && !b.is_empty(), "{type_err}\n---\n{interp_err}");
}

#[test]
fn all_errors_are_shown_rather_than_truncated_at_three() {
    // The old renderer capped output at three and appended `... and N more`,
    // which hid independent errors behind cascade noise.
    let src = "fun a(\nfun b(\nfun c(\nfun d(\nfun e(\n";
    let out = render_parse_errors(src);
    assert!(
        !out.contains("and more error"),
        "output should not be truncated:\n{out}"
    );
}

// ── lexical diagnostics ─────────────────────────────────────────────────────
// The lexer used to smuggle its error text through `TokenType::Illegal`'s
// `literal` field — the message stored where a token's *text* belongs — so the
// parser could not tell a smuggled message from a stray character, and every
// lexical problem surfaced under the generic "unexpected token" code.

/// The code of the first rendered diagnostic, e.g. `E0002`.
fn first_code(src: &str) -> String {
    let out = render_parse_errors(src);
    out.split("error[")
        .nth(1)
        .and_then(|r| r.split(']').next())
        .unwrap_or_default()
        .to_string()
}

#[test]
fn lexical_problems_get_lexical_codes_not_the_generic_one() {
    assert_eq!(first_code("x := 1;\n"), "E0002", "semicolon");
    assert_eq!(first_code("name := \"ada\n"), "E0001", "unterminated string");
    assert_eq!(first_code("c := ``\n"), "E0022", "empty char literal");
}

#[test]
fn a_lexical_diagnostic_is_reported_exactly_once() {
    // The lexer records it and the parser folds it in; if the parser also
    // reported the `Illegal` token it would appear twice.
    let out = render_parse_errors("x := 1;\n");
    // Count headers, not mentions — the explain footer names the code too.
    assert_eq!(
        out.matches("error[E0002]").count(),
        1,
        "reported more than once:\n{out}"
    );
}

#[test]
fn the_semicolon_error_explains_the_fix() {
    let out = render_parse_errors("x := 1;\n");
    assert!(out.contains("statement terminator"), "{out}");
    assert!(out.contains("newline"), "{out}");
}

#[test]
fn a_question_mark_is_not_a_lexical_error() {
    // `?` lexes as `Illegal` but is a live marker for optional parameters, so
    // it must not produce a diagnostic.
    let src = "fun f(a?) { a }\nf(1)\n";
    let lexer = Lexer::new(src);
    let mut parser = Parser::with_file(lexer, src, "t.oxi");
    parser.parse_program();
    let out = parser.format_errors();
    assert!(!out.contains("E0002"), "`?` reported as illegal:\n{out}");
}

#[test]
fn compiler_errors_that_have_a_span_underline_it() {
    // Not every compiler site has a span yet; the ones that do must render a
    // caret rather than pointing at column 0.
    let src = "main {\n    skip\n}\n";
    let lexer = Lexer::new(src);
    let mut parser = Parser::with_file(lexer, src, "t.oxi");
    let program = parser.parse_program();
    let errors = oxigen_core::compiler::Compiler::new()
        .compile(&program)
        .expect_err("`skip` outside a loop should fail to compile");
    let source = SourceFile::named("t.oxi", src);
    let out = render::render_human(&errors.into_iter().next().unwrap().into_diagnostic(), &source);
    assert!(out.contains("^^^^"), "expected `skip` underlined:\n{out}");
    assert!(!out.contains(":0"), "column should not be 0:\n{out}");
}
