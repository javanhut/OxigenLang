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

// ── skip / stop misuse ──────────────────────────────────────────────────────
// `skip` and `stop` are lexically scoped to the loop they are written inside: a
// function called from a loop must not be able to continue it, or the
// function's control flow would depend on its call site.
//
// The rule was already enforced, but *which* error you got was decided by where
// the keyword happened to sit. `fun f() { skip }` reported "cannot be used as a
// value" (because a function body's last statement is its return value) while
// `fun f() { skip\n 0 }` reported "used outside of loop" — one mistake, two
// errors. The message is now derived from the fact: no enclosing loop.

/// Compiles and returns the first diagnostic's code, or "" if it compiled.
fn compile_code(src: &str) -> String {
    let lexer = Lexer::new(src);
    let mut parser = Parser::with_file(lexer, src, "t.oxi");
    let program = parser.parse_program();
    if let Some(d) = parser.errors().first() {
        return d.code.0.to_string();
    }
    match oxigen_core::compiler::Compiler::new().compile(&program) {
        Ok(_) => String::new(),
        Err(errs) => errs[0].code.0.to_string(),
    }
}

#[test]
fn no_enclosing_loop_reports_the_same_code_whatever_the_position() {
    // Identical mistake; `skip` is last in one and not in the other.
    assert_eq!(compile_code("fun f() { skip }\neach i in range(2) { f() }"), "E0016");
    assert_eq!(compile_code("fun f() { skip\n 0 }\neach i in range(2) { f() }"), "E0016");
}

#[test]
fn a_closure_or_thread_body_is_not_inside_the_loop() {
    assert_eq!(compile_code("each i in range(2) { g := fun() { skip }\n g() }"), "E0016");
    assert_eq!(compile_code("each i in range(2) { diverge { skip } }"), "E0016");
}

#[test]
fn skip_and_stop_outside_any_loop_are_rejected() {
    assert_eq!(compile_code("main { skip }"), "E0016");
    assert_eq!(compile_code("main { stop }"), "E0016");
}

#[test]
fn used_as_a_value_survives_where_it_is_the_real_reason() {
    // With a loop present and the value genuinely consumed, "cannot be used as
    // a value" IS the correct diagnosis — so E0016 must not swallow it.
    assert_eq!(
        compile_code("each i in range(3) { x := option { i == 1 -> skip, 0 }\n println(x) }"),
        "E0017"
    );
}

#[test]
fn legitimate_loop_control_still_compiles() {
    assert_eq!(compile_code("each i in range(3) { skip when i == 1\n println(i) }"), "");
    assert_eq!(compile_code("each i in range(3) { stop when i == 1\n println(i) }"), "");
    // Value discarded — this is ordinary loop control flow, not a value use.
    assert_eq!(
        compile_code("each i in range(3) { option { i == 1 -> skip, 0 }\n println(i) }"),
        ""
    );
    assert_eq!(
        compile_code("each i in range(2) { each j in range(2) { skip when j == 0\n println(j) } }"),
        ""
    );
}

#[test]
fn the_outside_loop_error_explains_lexical_scoping() {
    let src = "fun f() { skip }\neach i in range(2) { f() }";
    let lexer = Lexer::new(src);
    let mut parser = Parser::with_file(lexer, src, "t.oxi");
    let program = parser.parse_program();
    let errs = oxigen_core::compiler::Compiler::new()
        .compile(&program)
        .expect_err("should not compile");
    let out = render::render_human(
        &errs.into_iter().next().unwrap().into_diagnostic(),
        &SourceFile::named("t.oxi", src),
    );
    assert!(out.contains("no loop for `skip` to control"), "{out}");
    assert!(out.contains("cannot control it"), "{out}");
    assert!(out.contains("^^^^"), "should underline the keyword:\n{out}");
}

// ── pattern arity ───────────────────────────────────────────────────────────
// A pattern is always invoked with exactly one argument — the value being
// matched (`Call, 1` at every use site) — so no syntax can supply a second.
// Declaring more used to parse: unreferenced extras were dead syntax, but
// referencing one failed at run time inside the synthesized `__pattern__`
// (`cannot compare INTEGER > NONE`) or, from an arm body, as
// `undefined variable`. Declaring none only worked because a surplus argument
// is silently discarded.

#[test]
fn a_pattern_must_declare_exactly_one_parameter() {
    assert_eq!(compile_code("pattern cmp(a, b) when a > b\n1"), "E0024");
    assert_eq!(compile_code("pattern cmp(a, b, c) when a > b\n1"), "E0024");
    assert_eq!(compile_code("pattern always() when True\n1"), "E0024");
}

#[test]
fn the_rule_applies_to_inline_patterns_too() {
    assert_eq!(
        compile_code("each i in [1] { choose i { pattern cmp(a, b) when a > b -> 1, else -> 2 } }"),
        "E0024"
    );
    assert_eq!(
        compile_code("each i in [1] { choose i { pattern p() when True -> 1, else -> 2 } }"),
        "E0024"
    );
}

#[test]
fn it_is_rejected_even_when_the_pattern_is_never_used() {
    // The declaration can never work, so waiting for a use site would let a
    // broken pattern sit in a file indefinitely.
    assert_eq!(compile_code("pattern cmp(a, b) when a > b\nprintln(\"unused\")"), "E0024");
}

#[test]
fn single_parameter_patterns_still_compile() {
    assert_eq!(compile_code("pattern big(n) when n > 3\n1"), "");
    assert_eq!(
        compile_code("pattern big(n) when n > 3\neach i in [1] { choose i { big -> 1, else -> 2 } }"),
        ""
    );
    assert_eq!(
        compile_code("each i in [1] { choose i { pattern big(n) when n > 3 -> 1, else -> 2 } }"),
        ""
    );
    // Comparing against a captured value is the supported alternative.
    assert_eq!(compile_code("limit := 3\npattern big(n) when n > limit\n1"), "");
}

#[test]
fn the_error_names_the_unbindable_parameters() {
    let src = "pattern cmp(a, b) when a > b\n1";
    let lexer = Lexer::new(src);
    let mut parser = Parser::with_file(lexer, src, "t.oxi");
    let program = parser.parse_program();
    let errs = oxigen_core::compiler::Compiler::new()
        .compile(&program)
        .expect_err("should not compile");
    let out = render::render_human(
        &errs.into_iter().next().unwrap().into_diagnostic(),
        &SourceFile::named("t.oxi", src),
    );
    assert!(out.contains("`b` never bound"), "{out}");
    assert!(out.contains("value being matched"), "{out}");
}

// ── binding provenance ──────────────────────────────────────────────────────
// A local recorded only `mutable: bool`, so the reporting site knew a write was
// forbidden but not why — and guessed the advice. A loop variable was told to
// "use `:=` to override", which produces a rebinding discarded at the end of
// the iteration; the `i++` site had no advice at all; and `:=` bypassed the
// check entirely. `BindingKind` carries the reason and the span that
// established it, so one function derives all three.

/// Renders the first compile diagnostic for `src`.
fn compile_render(src: &str) -> String {
    let lexer = Lexer::new(src);
    let mut parser = Parser::with_file(lexer, src, "t.oxi");
    let program = parser.parse_program();
    let errs = oxigen_core::compiler::Compiler::new()
        .compile(&program)
        .expect_err("expected a compile error");
    render::render_human(
        &errs.into_iter().next().unwrap().into_diagnostic(),
        &SourceFile::named("t.oxi", src),
    )
}

#[test]
fn a_loop_variable_is_not_told_to_use_walrus() {
    // The original wrong hint: `:=` here lasts one iteration.
    let out = compile_render("fun f() {\n  each i in range(3) { i = i * 2 }\n}\nf()");
    assert!(out.contains("loop variable"), "{out}");
    assert!(
        !out.contains("use := to override"),
        "still gives the misleading advice:\n{out}"
    );
    assert!(out.contains("does not change which values are iterated"), "{out}");
}

#[test]
fn the_loop_header_is_shown_as_a_secondary_label() {
    // Impossible while spans were points — this is the payoff of widening them.
    let out = compile_render("fun f() {\n  each i in range(3) { i = 1 }\n}\nf()");
    assert!(out.contains("rebound on each iteration"), "{out}");
    assert!(out.contains("---"), "expected a secondary underline:\n{out}");
}

#[test]
fn the_increment_site_now_has_advice_too() {
    // `i++` is the more common mistake and previously carried no hint at all.
    let out = compile_render("fun f() {\n  each i in range(3) { i++ }\n}\nf()");
    assert!(out.contains("cannot mutate loop variable `i` with `++`"), "{out}");
    assert!(out.contains("= help:"), "no advice:\n{out}");
}

#[test]
fn a_constant_points_at_its_declaration() {
    let out = compile_render("fun f() {\n  PI <float> = 3.14\n  PI = 3.0\n  PI\n}\nf()");
    assert!(out.contains("declared immutable here"), "{out}");
    assert!(out.contains("`=` binds immutably"), "{out}");
}

#[test]
fn walrus_no_longer_bypasses_the_check_on_a_loop_variable() {
    // `i = 6` errored while `i := 6` silently wrote the same slot.
    let out = compile_render("fun f() {\n  each i in range(3) { i := i * 10 }\n}\nf()");
    assert!(out.contains("E0025"), "{out}");
    assert!(out.contains("lasts one iteration"), "{out}");
    // The message must not claim the write does nothing: it IS visible for the
    // rest of the iteration, just discarded before the next one.
    assert!(
        !out.contains("no effect"),
        "claims something untrue about the rebinding:\n{out}"
    );
}

#[test]
fn walrus_still_overrides_a_constant() {
    // Documented behaviour: `:=` is how you override an immutable binding.
    assert_eq!(compile_code("fun f() {\n  PI <float> = 3.14\n  PI := 3.0\n  PI\n}\nf()"), "");
}

#[test]
fn mutable_locals_are_unaffected() {
    assert_eq!(compile_code("fun f() {\n  n := 1\n  n = 2\n  n++\n  n\n}\nf()"), "");
    assert_eq!(compile_code("fun f(p) {\n  p = 2\n  p\n}\nf(1)"), "");
}

// ── silence is not shippable ────────────────────────────────────────────────
// A parse function returning `Option` could not distinguish "I reported an
// error" from "I gave up quietly", and `parse_program` only synchronised when
// the error count rose — so a quiet `None` dropped the statement and carried
// on. The file compiled and ran, minus a line nobody was told about.

#[test]
fn an_out_of_range_integer_is_reported_not_dropped() {
    // Was: the binding silently never happened and the first symptom was
    // `undefined variable` on the next, perfectly correct line.
    let out = render_parse_errors("x := 99999999999999999999999999\nprintln(x)\n");
    assert!(out.contains("E0027"), "{out}");
    assert!(out.contains("64-bit"), "{out}");
    assert!(
        !out.contains("undefined variable"),
        "should fail at the literal, not downstream:\n{out}"
    );
}

#[test]
fn an_unterminated_block_comment_is_reported() {
    // Was: swallowed the rest of the file, exited 0, and `check` said clean.
    let out = render_parse_errors("println(\"before\")\n/* oops\nprintln(\"after\")\n");
    assert!(out.contains("E0029"), "{out}");
    assert!(out.contains("never closed"), "{out}");
}

#[test]
fn a_second_decimal_point_is_a_number_error_not_field_access() {
    // Was: `1.2.3` lexed as `1.2` then `.3`, reported as
    // `cannot access field '3' on FLOAT`.
    let out = render_parse_errors("x := 1.2.3\n");
    assert!(out.contains("E0028"), "{out}");
    assert!(out.contains("decimal point"), "{out}");
}

#[test]
fn a_valid_number_is_still_a_number() {
    // The malformed-literal check must not catch ordinary floats or the
    // field access that genuinely follows one.
    assert_eq!(compile_code("x := 1.25\nx"), "");
    assert_eq!(compile_code("x := 9223372036854775807\nx"), "");
    assert_eq!(compile_code("x := 1.0\ny := [x]\ny[0]"), "");
}

#[test]
fn the_net_catches_a_parse_failure_that_reports_nothing() {
    // The guarantee itself: whatever the parser does, it cannot drop a
    // statement without saying so. Every rejected program must produce at
    // least one diagnostic — E0030 is the backstop if a path forgets.
    for src in [
        "x := 99999999999999999999999999\n",
        "/* unclosed\n",
        "x := 1.2.3\n",
        "enum E {\n 123\n}\n",
        "x := \"unterminated\n",
        "x := 1;\n",
    ] {
        let out = render_parse_errors(src);
        assert!(
            out.contains("error["),
            "parsed with no diagnostic at all:\n{src}"
        );
    }
}

#[test]
fn valid_programs_do_not_trip_the_net() {
    // E0030 means "the parser gave up without saying why" — it must never fire
    // on code that parses.
    for src in [
        "x := 1\nprintln(x)\n",
        "fun f(a, b) { a + b }\nf(1, 2)\n",
        "each i in range(3) { println(i) }\n",
        "m := {\"k\": 1}\nprintln(m.k)\n",
        "struct P { n <int> }\np := P(1)\np.n\n",
    ] {
        let out = render_parse_errors(src);
        assert!(out.is_empty(), "valid program produced diagnostics:\n{src}\n{out}");
    }
}
