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
