use super::*;

fn collect_tokens(input: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    loop {
        let tok = lexer.next_token();
        let is_eof = tok.token_type == TokenType::Eof;
        tokens.push(tok);
        if is_eof {
            break;
        }
    }
    tokens
}

#[test]
fn test_brace_mode_unchanged() {
    let input = "x := 5\nif y {\n  print(x)\n}";
    let tokens = collect_tokens(input);

    // Should have LBrace and RBrace as literals
    let has_lbrace = tokens
        .iter()
        .any(|t| t.token_type == TokenType::LBrace && t.literal == "{");
    let has_rbrace = tokens
        .iter()
        .any(|t| t.token_type == TokenType::RBrace && t.literal == "}");
    assert!(has_lbrace, "Should have literal LBrace");
    assert!(has_rbrace, "Should have literal RBrace");
}

#[test]
fn test_indent_mode_colon_becomes_lbrace() {
    let input = "#[indent]\neach num in x:\n  print(num)\n";
    let tokens = collect_tokens(input);

    // Colon at EOL should become LBrace
    let has_lbrace = tokens.iter().any(|t| t.token_type == TokenType::LBrace);
    assert!(
        has_lbrace,
        "Colon at EOL should become LBrace in indent mode"
    );
}

#[test]
fn test_indent_mode_dedent_becomes_rbrace() {
    let input = "#[indent]\neach num in x:\n  print(num)\ny := 5\n";
    let tokens = collect_tokens(input);

    // Dedent should produce RBrace
    let has_rbrace = tokens.iter().any(|t| t.token_type == TokenType::RBrace);
    assert!(has_rbrace, "Dedent should produce RBrace in indent mode");
}

#[test]
fn test_indent_mode_nested_blocks() {
    let input = "#[indent]\nouter:\n  inner:\n    x\n";
    let tokens = collect_tokens(input);

    let lbrace_count = tokens
        .iter()
        .filter(|t| t.token_type == TokenType::LBrace)
        .count();
    let rbrace_count = tokens
        .iter()
        .filter(|t| t.token_type == TokenType::RBrace)
        .count();

    assert_eq!(
        lbrace_count, 2,
        "Should have 2 LBrace tokens for nested blocks"
    );
    assert_eq!(
        rbrace_count, 2,
        "Should have 2 RBrace tokens for nested blocks"
    );
}

#[test]
fn test_indent_directive_stripped() {
    let input = "#[indent]\nx := 5";
    let tokens = collect_tokens(input);

    // Should not have Hash or the indent directive tokens
    let has_hash_indent = tokens
        .iter()
        .any(|t| t.literal == "#" || t.literal == "indent");
    assert!(!has_hash_indent, "#[indent] directive should be stripped");
}

#[test]
fn test_location_directive_is_stripped() {
    let input = "#[location=/usr/local/bin/oxigen]\nx := 5";
    let tokens = collect_tokens(input);

    let x_tok = tokens.iter().find(|t| t.literal == "x").unwrap();
    assert_eq!(x_tok.span.line, 2);

    let has_location_tokens = tokens
        .iter()
        .any(|t| t.literal == "#" || t.literal == "location");
    assert!(
        !has_location_tokens,
        "#[location=...] directive should be stripped"
    );
}

#[test]
fn test_indent_and_brace_produce_same_structure() {
    // Simple equivalent programs
    let indent_input = "#[indent]\neach x:\n  print(x)\n";
    let brace_input = "each x {\n  print(x)\n}";

    let indent_tokens = collect_tokens(indent_input);
    let brace_tokens = collect_tokens(brace_input);

    // Extract just the token types (ignore literals and whitespace differences)
    let indent_types: Vec<_> = indent_tokens
        .iter()
        .map(|t| &t.token_type)
        .filter(|t| **t != TokenType::Newline)
        .collect();

    let brace_types: Vec<_> = brace_tokens
        .iter()
        .map(|t| &t.token_type)
        .filter(|t| **t != TokenType::Newline)
        .collect();

    assert_eq!(
        indent_types, brace_types,
        "Indent and brace syntax should produce same token types"
    );
}

#[test]
fn test_shebang_line_is_stripped() {
    let input = "#!/usr/local/bin/oxigen\nx := 5";
    let tokens = collect_tokens(input);

    assert_eq!(tokens[0].literal, "x");
    assert_eq!(tokens[0].span.line, 2);
    assert!(
        tokens
            .iter()
            .all(|t| t.literal != "#" && t.literal != "!" && t.literal != "/usr/local/bin/oxigen")
    );
}

#[test]
fn test_shebang_then_indent_directive_still_enables_indent_mode() {
    let input = "#!/usr/local/bin/oxigen\n#[indent]\neach num in x:\n  print(num)\n";
    let tokens = collect_tokens(input);

    let has_lbrace = tokens.iter().any(|t| t.token_type == TokenType::LBrace);
    let has_rbrace = tokens.iter().any(|t| t.token_type == TokenType::RBrace);

    assert!(
        has_lbrace,
        "Indent mode should still open blocks after a shebang"
    );
    assert!(
        has_rbrace,
        "Indent mode should still close blocks after a shebang"
    );
}

#[test]
fn test_shebang_location_and_indent_headers_can_coexist() {
    let input = "#!/usr/local/bin/oxigen\n#[location=/usr/local/bin/oxigen]\n#[indent]\neach num in x:\n  print(num)\n";
    let tokens = collect_tokens(input);

    let has_lbrace = tokens.iter().any(|t| t.token_type == TokenType::LBrace);
    let has_rbrace = tokens.iter().any(|t| t.token_type == TokenType::RBrace);

    assert!(
        has_lbrace,
        "Indent mode should survive multiple header lines"
    );
    assert!(
        has_rbrace,
        "Dedent handling should survive multiple header lines"
    );
}

#[test]
fn test_span_tracking() {
    let input = "x := 5\ny := 10";
    let tokens = collect_tokens(input);

    // 'x' should be at line 1, col 1
    assert_eq!(tokens[0].span.line, 1);
    assert_eq!(tokens[0].span.column, 1);

    // 'y' should be at line 2, col 1
    let y_tok = tokens.iter().find(|t| t.literal == "y").unwrap();
    assert_eq!(y_tok.span.line, 2);
    assert_eq!(y_tok.span.column, 1);
}

#[test]
fn test_span_column_tracking() {
    let input = "abc := 42";
    let tokens = collect_tokens(input);

    // 'abc' at col 1
    assert_eq!(tokens[0].literal, "abc");
    assert_eq!(tokens[0].span.column, 1);

    // ':=' at col 5
    assert_eq!(tokens[1].literal, ":=");
    assert_eq!(tokens[1].span.column, 5);

    // '42' at col 8
    assert_eq!(tokens[2].literal, "42");
    assert_eq!(tokens[2].span.column, 8);
}

#[test]
fn test_string_escape_sequences_include_ansi_escape() {
    let tokens = collect_tokens(r#""\e[31mred\e[0m" "\x1b[32mgreen\x1B[0m""#);

    assert_eq!(tokens[0].token_type, TokenType::String);
    assert_eq!(tokens[0].literal, "\x1b[31mred\x1b[0m");
    assert_eq!(tokens[1].token_type, TokenType::String);
    assert_eq!(tokens[1].literal, "\x1b[32mgreen\x1b[0m");
}

#[test]
fn test_interpolated_string_escape_sequences_include_ansi_escape() {
    let tokens = collect_tokens(r#""\e[31m{name}\x1b[0m""#);

    assert_eq!(tokens[0].token_type, TokenType::InterpStart);
    assert_eq!(tokens[1].token_type, TokenType::String);
    assert_eq!(tokens[1].literal, "\x1b[31m");
    assert_eq!(tokens[5].token_type, TokenType::String);
    assert_eq!(tokens[5].literal, "\x1b[0m");
}

#[test]
fn test_unterminated_string_at_eof_is_illegal() {
    // No closing quote, end of input: must report an unterminated string,
    // not silently produce a String token.
    let tokens = collect_tokens(r#"x := "hello"#);

    let illegal = tokens
        .iter()
        .find(|t| t.token_type == TokenType::Illegal)
        .expect("unterminated string should emit an Illegal token");
    assert!(
        illegal.literal.contains("unterminated"),
        "Illegal literal should describe the problem, got {:?}",
        illegal.literal
    );
    // No bogus String token for the unterminated literal.
    assert!(
        !tokens.iter().any(|t| t.token_type == TokenType::String),
        "unterminated string must not produce a String token"
    );
}

#[test]
fn test_unterminated_string_anchored_at_opening_quote() {
    // The opening quote is on line 1 at column 6 (1-based). The Illegal token
    // must point THERE, not at the next line where lexing recovers.
    let src = "x := \"hello\ny := 5\n";
    let tokens = collect_tokens(src);

    let illegal = tokens
        .iter()
        .find(|t| t.token_type == TokenType::Illegal)
        .expect("unterminated string should emit an Illegal token");
    assert_eq!(
        illegal.span.line, 1,
        "error should be anchored at the opening quote's line"
    );
    assert_eq!(
        illegal.span.column, 6,
        "error should be anchored at the opening quote's column"
    );
}

#[test]
fn test_unterminated_string_does_not_swallow_following_line() {
    // After the unterminated string the lexer must keep making progress and
    // still tokenize the next line, so we should see a Newline then `y`.
    let src = "x := \"hello\ny := 5\n";
    let tokens = collect_tokens(src);

    assert!(
        tokens
            .iter()
            .any(|t| t.token_type == TokenType::Ident && t.literal == "y"),
        "the line after an unterminated string must still be tokenized"
    );
    // And the run terminates cleanly with Eof (no infinite loop / hang).
    assert_eq!(tokens.last().unwrap().token_type, TokenType::Eof);
}

#[test]
fn test_unterminated_single_quote_string_is_illegal() {
    let tokens = collect_tokens("x := 'oops\n");
    assert!(
        tokens.iter().any(|t| t.token_type == TokenType::Illegal
            && t.literal.contains("unterminated")),
        "unterminated single-quoted string should also be reported"
    );
}

#[test]
fn test_unterminated_interpolated_string_is_illegal() {
    // Interpolation that never closes its outer quote (raw newline before the
    // closing delimiter). Should be one clean unterminated error anchored at
    // the opening quote, not a cascade of interpolation part tokens.
    let src = "msg := \"hi {name}\nx := 1\n";
    let tokens = collect_tokens(src);

    let illegal = tokens
        .iter()
        .find(|t| t.token_type == TokenType::Illegal)
        .expect("unterminated interpolated string should emit an Illegal token");
    assert!(illegal.literal.contains("unterminated"));
    assert_eq!(illegal.span.line, 1);
    // No interpolation part tokens should leak out for the broken string.
    assert!(
        !tokens
            .iter()
            .any(|t| t.token_type == TokenType::InterpStart),
        "broken interpolated string must not leak InterpStart"
    );
    // Recovery: the next line is still tokenized.
    assert!(
        tokens
            .iter()
            .any(|t| t.token_type == TokenType::Ident && t.literal == "x")
    );
}

#[test]
fn test_triple_quoted_string_spans_lines() {
    // A `"""` string spans raw newlines and produces a single MultilineString
    // token whose literal contains the embedded newlines verbatim.
    let tokens = collect_tokens("\"\"\"line1\nline2\nline3\"\"\"");

    assert_eq!(tokens[0].token_type, TokenType::MultilineString);
    assert_eq!(tokens[0].literal, "line1\nline2\nline3");
    assert_eq!(tokens[1].token_type, TokenType::Eof);
}

#[test]
fn test_triple_single_quoted_string_spans_lines() {
    // `'''` is the single-quote flavor of the same multi-line string.
    let tokens = collect_tokens("'''a\nb'''");

    assert_eq!(tokens[0].token_type, TokenType::MultilineString);
    assert_eq!(tokens[0].literal, "a\nb");
}

#[test]
fn test_triple_quoted_string_interpolation() {
    // Interpolation works inside a multi-line string exactly as in a
    // single-line one; literal parts keep their embedded newlines.
    let tokens = collect_tokens("\"\"\"\nHello {name}\n\"\"\"");

    assert_eq!(tokens[0].token_type, TokenType::MultilineInterpStart);
    // Leading literal part carries the newline after the opening fence.
    assert_eq!(tokens[1].token_type, TokenType::String);
    assert_eq!(tokens[1].literal, "\nHello ");
    assert_eq!(tokens[2].token_type, TokenType::InterpExprStart);
    assert_eq!(tokens[3].token_type, TokenType::Ident);
    assert_eq!(tokens[3].literal, "name");
    assert_eq!(tokens[4].token_type, TokenType::InterpExprEnd);
    assert_eq!(tokens[5].token_type, TokenType::String);
    assert_eq!(tokens[5].literal, "\n");
    assert_eq!(tokens[6].token_type, TokenType::InterpEnd);
}

#[test]
fn test_triple_quoted_interpolation_expression_spans_lines() {
    // An interpolation expression inside a `"""` string may itself span
    // multiple physical lines; the inner tokens are lexed normally.
    let tokens = collect_tokens("\"\"\"sum {\n  x +\n  y\n}\"\"\"");

    assert_eq!(tokens[0].token_type, TokenType::MultilineInterpStart);
    assert!(
        tokens
            .iter()
            .any(|t| t.token_type == TokenType::Ident && t.literal == "x"),
        "x should be lexed inside the multi-line interpolation"
    );
    assert!(tokens.iter().any(|t| t.token_type == TokenType::Plus));
    assert!(
        tokens
            .iter()
            .any(|t| t.token_type == TokenType::Ident && t.literal == "y"),
        "y should be lexed inside the multi-line interpolation"
    );
    assert!(tokens.iter().any(|t| t.token_type == TokenType::InterpEnd));
    // No Illegal tokens: the newlines inside `{ }` must be skipped, not
    // tokenized as stray characters.
    assert!(
        !tokens.iter().any(|t| t.token_type == TokenType::Illegal),
        "newlines inside a multi-line interpolation must not become Illegal tokens"
    );
}

#[test]
fn test_empty_triple_quoted_string() {
    // Six quotes in a row is an empty multi-line string, not an unterminated one.
    let tokens = collect_tokens("\"\"\"\"\"\"");

    assert_eq!(tokens[0].token_type, TokenType::MultilineString);
    assert_eq!(tokens[0].literal, "");
    assert_eq!(tokens[1].token_type, TokenType::Eof);
}

#[test]
fn test_empty_single_line_string_is_not_triple() {
    // `""` is an empty single-line string and must NOT be mistaken for the
    // start of a triple-quoted string.
    let tokens = collect_tokens("\"\" \"after\"");

    assert_eq!(tokens[0].token_type, TokenType::String);
    assert_eq!(tokens[0].literal, "");
    assert_eq!(tokens[1].token_type, TokenType::String);
    assert_eq!(tokens[1].literal, "after");
}

#[test]
fn test_escape_sequences_work_in_triple_quoted_string() {
    // Escapes are still processed inside a multi-line string, including an
    // escaped quote that does not close the fence.
    let tokens = collect_tokens("\"\"\"tab\\tnext\nquote \\\" done\"\"\"");

    assert_eq!(tokens[0].token_type, TokenType::MultilineString);
    assert_eq!(tokens[0].literal, "tab\tnext\nquote \" done");
}

#[test]
fn test_unterminated_triple_quoted_string_is_illegal() {
    // A `"""` opened but never closed before EOF is unterminated and reported
    // as an Illegal token anchored at the opening fence — no String token.
    let tokens = collect_tokens("x := \"\"\"hello\nworld\n");

    let illegal = tokens
        .iter()
        .find(|t| t.token_type == TokenType::Illegal)
        .expect("unterminated triple-quoted string should emit an Illegal token");
    assert!(illegal.literal.contains("unterminated"));
    assert_eq!(
        illegal.span.line, 1,
        "error should be anchored at the opening fence's line"
    );
    assert_eq!(
        illegal.span.column, 6,
        "error should be anchored at the opening fence's column"
    );
    assert!(
        !tokens.iter().any(
            |t| t.token_type == TokenType::String || t.token_type == TokenType::MultilineString
        ),
        "unterminated triple-quoted string must not produce a string token"
    );
}

#[test]
fn test_well_terminated_strings_still_lex() {
    // Regression guard: normal strings, escaped quotes, escaped newlines, and
    // interpolation must all keep working unchanged.
    let tokens = collect_tokens(r#""hello world""#);
    assert_eq!(tokens[0].token_type, TokenType::String);
    assert_eq!(tokens[0].literal, "hello world");

    let tokens = collect_tokens(r#""line\nbreak""#);
    assert_eq!(tokens[0].token_type, TokenType::String);
    assert_eq!(tokens[0].literal, "line\nbreak");

    let tokens = collect_tokens(r#""say \"hi\"""#);
    assert_eq!(tokens[0].token_type, TokenType::String);
    assert_eq!(tokens[0].literal, "say \"hi\"");

    let tokens = collect_tokens(r#""hi {name}!""#);
    assert_eq!(tokens[0].token_type, TokenType::InterpStart);
    assert!(
        !tokens.iter().any(|t| t.token_type == TokenType::Illegal),
        "a well-formed interpolated string must not emit Illegal"
    );
}
