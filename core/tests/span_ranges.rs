//! Tests that `Span` is a range.
//!
//! A token's span covers the text it consumed, and `offset` is a byte index into
//! the original source even when the lexer stripped a shebang or `#[...]` header
//! first.

use oxigen_core::lexer::Lexer;
use oxigen_core::token::TokenType;

/// Lexes to EOF, returning every token.
fn tokens(src: &str) -> Vec<oxigen_core::token::Token> {
    let mut lexer = Lexer::new(src);
    let mut out = Vec::new();
    loop {
        let t = lexer.next_token();
        let end = t.token_type == TokenType::Eof;
        out.push(t);
        if end {
            break;
        }
    }
    out
}

/// Tokens with `Newline`s dropped.
fn code_tokens(src: &str) -> Vec<oxigen_core::token::Token> {
    tokens(src)
        .into_iter()
        .filter(|t| t.token_type != TokenType::Newline)
        .collect()
}

/// The source text a span points at, sliced by byte offset.
fn slice<'a>(src: &'a str, tok: &oxigen_core::token::Token) -> &'a str {
    &src[tok.span.start.offset..tok.span.end.offset]
}

#[test]
fn a_tokens_span_covers_the_text_it_consumed() {
    let src = "value := 1234";
    let toks = code_tokens(src);

    assert_eq!(slice(src, &toks[0]), "value");
    assert_eq!(slice(src, &toks[1]), ":=");
    assert_eq!(slice(src, &toks[2]), "1234");
}

#[test]
fn spans_are_byte_offsets_into_the_original_source() {
    let src = "alpha + beta";
    let toks = code_tokens(src);
    assert_eq!(toks[0].span.start.offset, 0);
    assert_eq!(toks[0].span.end.offset, 5);
    assert_eq!(slice(src, &toks[2]), "beta");
}

#[test]
fn offsets_survive_a_stripped_header() {
    let src = "#!/usr/bin/env oxigen\n#[indent]\nvalue := 1\n";
    let toks = code_tokens(src);
    assert_eq!(toks[0].literal, "value");
    assert_eq!(
        slice(src, &toks[0]),
        "value",
        "offset was not rebased past the stripped header"
    );
}

#[test]
fn offsets_are_bytes_not_chars_for_multibyte_source() {
    let src = "greeting := \"café\"\ntail := 1";
    let toks = code_tokens(src);
    let tail = toks
        .iter()
        .find(|t| t.literal == "tail")
        .expect("expected the `tail` token");
    assert_eq!(
        slice(src, tail),
        "tail",
        "byte offsets drifted across a multibyte literal"
    );
    assert_eq!(tail.span.start.line, 2);
    assert_eq!(tail.span.start.column, 1);
}

#[test]
fn a_string_literals_span_covers_its_quotes() {
    let src = "greeting := \"hello\"";
    let toks = code_tokens(src);
    let string_tok = toks
        .iter()
        .find(|t| t.token_type == TokenType::String)
        .expect("expected a string token");
    assert!(
        slice(src, string_tok).contains("hello"),
        "got {:?}",
        slice(src, string_tok)
    );
}

#[test]
fn line_and_column_accessors_still_report_the_start() {
    let src = "one\n  two";
    let toks = code_tokens(src);
    assert_eq!((toks[0].span.line(), toks[0].span.column()), (1, 1));
    assert_eq!((toks[1].span.line(), toks[1].span.column()), (2, 3));
}

#[test]
fn spans_combine_to_cover_a_composite_range() {
    let src = "a + b * c";
    let toks = code_tokens(src);
    let whole = toks[0].span.to(toks[4].span);
    assert_eq!(&src[whole.start.offset..whole.end.offset], "a + b * c");
}

#[test]
fn a_point_span_reports_zero_width() {
    use oxigen_core::token::{Pos, Span};
    let p = Pos::new(3, 7, 42);
    let s = Span::point(p);
    assert!(s.is_point());
    assert_eq!(s.len_bytes(), 0);
    assert_eq!((s.line(), s.column()), (3, 7));
}
