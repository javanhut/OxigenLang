use crate::diagnostics::registry as codes;
use crate::diagnostics::Diagnostic;
use crate::token::{Pos, Span, Token, TokenType, token_map};
use std::collections::HashMap;
use std::collections::VecDeque;

/// A comment seen while lexing.
///
/// Comments carry no semantics, so they never become tokens and never reach the
/// AST — but `fmt` reprints from the AST, so without this record it would drop
/// every comment in the file. `line`/`column` are where the comment started and
/// `own_line` distinguishes a comment sitting alone on its line from one
/// trailing code, which is all the formatter needs to put it back where the
/// author wrote it. `column` is what separates a comment indented inside a
/// block from one written past the block's closing delimiter — a distinction
/// the AST cannot make, since it stores no span for a `}`.
#[derive(Debug, Clone, PartialEq)]
pub struct Comment {
    pub line: usize,
    pub column: usize,
    pub text: String,
    pub own_line: bool,
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
    keywords: HashMap<&'static str, TokenType>,
    // Indent mode support
    indent_mode: bool,
    indent_stack: Vec<usize>,
    pending_tokens: VecDeque<Token>,
    at_line_start: bool,
    // Source location tracking
    line: usize,
    column: usize,
    /// Byte offset of each char in `input`, plus a final entry for EOF, so a
    /// position can be reported as a byte index without rescanning. `input` is
    /// `Vec<char>`, and diagnostics/edits need byte offsets into the original
    /// source text.
    char_byte_offsets: Vec<usize>,
    /// Bytes `preprocess_input` stripped (shebang, `#[...]` directives). Added
    /// to every offset so they index the ORIGINAL source, matching the
    /// line numbers, which are already original-relative via `start_line`.
    base_offset: usize,
    comments: Vec<Comment>,
    /// Real diagnostics for lexical problems.
    ///
    /// These used to be smuggled through `TokenType::Illegal`'s `literal`
    /// field — the error text stored where a token's *text* belongs — leaving
    /// the parser to guess whether a literal was a message or a stray
    /// character. `Illegal` is now only a recovery placeholder.
    diagnostics: Vec<Diagnostic>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let (effective_input, start_line, indent_mode) = Self::preprocess_input(input);

        // preprocess_input returns a suffix, so the byte-length delta is exactly what it stripped.
        let base_offset = input.len() - effective_input.len();
        let mut char_byte_offsets = Vec::with_capacity(effective_input.len() + 1);
        let mut running = 0usize;
        for c in effective_input.chars() {
            char_byte_offsets.push(running);
            running += c.len_utf8();
        }
        char_byte_offsets.push(running); // EOF

        let mut l = Self {
            input: effective_input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: '\0',
            keywords: token_map(),
            indent_mode,
            indent_stack: vec![0], // Start with base indentation level 0
            pending_tokens: VecDeque::new(),
            at_line_start: true, // We start at the beginning of input
            line: start_line,
            column: 0,
            char_byte_offsets,
            base_offset,
            comments: Vec::new(),
            diagnostics: Vec::new(),
        };
        l.read_char();
        l
    }

    /// Lexical diagnostics collected so far. Complete once driven to EOF.
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Comments collected so far, in source order. Only complete once the lexer
    /// has been driven to EOF (which `Parser::parse_program` does).
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    fn preprocess_input(input: &str) -> (&str, usize, bool) {
        let mut effective_input = input;
        let mut start_line = 1;
        let mut indent_mode = false;

        // Allow executable Oxigen scripts to start with a Unix shebang line.
        if effective_input.starts_with("#!") {
            if let Some(newline) = effective_input.find('\n') {
                effective_input = &effective_input[newline + 1..];
                start_line += 1;
            } else {
                effective_input = "";
            }
        }

        while let Some((rest, newlines, enables_indent)) =
            Self::strip_top_level_directive(effective_input)
        {
            effective_input = rest;
            start_line += newlines;
            indent_mode |= enables_indent;
        }

        (effective_input, start_line, indent_mode)
    }

    fn strip_top_level_directive(input: &str) -> Option<(&str, usize, bool)> {
        let trimmed = input.trim_start();
        if !trimmed.starts_with("#[") {
            return None;
        }

        let offset = input.len() - trimmed.len();
        let end = trimmed.find(']')?;
        let directive = &trimmed[..=end];

        let enables_indent = directive == "#[indent]";
        let is_location =
            directive.starts_with("#[location=") && directive.len() > "#[location=]".len();

        if !enables_indent && !is_location {
            return None;
        }

        let start = offset + directive.len();
        let skipped = &input[..start];
        let newlines = skipped.chars().filter(|&c| c == '\n').count();

        Some((&input[start..], newlines, enables_indent))
    }

    /// Current source position, including the byte offset into the original
    /// source.
    fn pos(&self) -> Pos {
        let offset = self
            .char_byte_offsets
            .get(self.position)
            .copied()
            .unwrap_or_else(|| self.char_byte_offsets.last().copied().unwrap_or(0));
        Pos::new(self.line, self.column, self.base_offset + offset)
    }

    /// Zero-width span at the character being processed. `next_token` widens
    /// the returned token's span to the text it actually consumed.
    fn span(&self) -> Span {
        Span::point(self.pos())
    }

    pub fn next_token(&mut self) -> Token {
        // Return any pending tokens first (from dedent handling)
        if let Some(tok) = self.pending_tokens.pop_front() {
            return tok;
        }

        // Handle indentation at line start (indent mode only)
        if self.indent_mode && self.at_line_start {
            self.at_line_start = false;
            let indent_level = self.measure_indentation();
            self.handle_indentation(indent_level);

            // Check if we queued any dedent tokens
            if let Some(tok) = self.pending_tokens.pop_front() {
                return tok;
            }
        }

        self.skip_whitespace_except_newline();

        if self.ch == '\n' {
            let span = self.span();
            self.read_char();
            self.at_line_start = true;
            return Token {
                token_type: TokenType::Newline,
                literal: "\n".into(),
                span,
            };
        }

        let span = self.span();
        let mut token = self.lex_token_at(span);
        // read_string anchors an unterminated string at its opening quote, so only the end moves.
        token.span.end = self.pos();
        token
    }

    /// Lexes exactly one token starting at the current character.
    ///
    /// Split out of `next_token` so string interpolation can reuse the real
    /// lexer for the expression inside `{ ... }`. That used to be a separate
    /// hand-written scanner accepting only a whitelist of characters
    /// (identifiers, numbers, strings, and `( ) , + - * / . [ ]`), which made
    /// `"{a == b}"`, `"{x % 2}"` and every other operator a syntax error while
    /// the parser behind it was perfectly capable of handling them.
    ///
    /// Callers are responsible for the parts of `next_token` NOT included here:
    /// the pending-token queue, indent-mode bookkeeping, leading whitespace and
    /// the newline token.
    fn lex_token_at(&mut self, span: Span) -> Token {
        match self.ch {
            '\0' => {
                // At EOF in indent mode, emit RBrace for any remaining open blocks
                if self.indent_mode && self.indent_stack.len() > 1 {
                    self.indent_stack.pop();
                    // Queue remaining closes
                    while self.indent_stack.len() > 1 {
                        self.indent_stack.pop();
                        self.pending_tokens.push_back(Token {
                            token_type: TokenType::RBrace,
                            literal: "}".into(),
                            span,
                        });
                    }
                    // Queue the final EOF
                    self.pending_tokens.push_back(Token {
                        token_type: TokenType::Eof,
                        literal: "".into(),
                        span,
                    });
                    return Token {
                        token_type: TokenType::RBrace,
                        literal: "}".into(),
                        span,
                    };
                }
                Token {
                    token_type: TokenType::Eof,
                    literal: "".into(),
                    span,
                }
            }
            ':' if self.peek_char() == '=' => {
                self.read_char();
                self.read_char();
                Token {
                    token_type: TokenType::Walrus,
                    literal: ":=".into(),
                    span,
                }
            }
            '-' if self.peek_char() == '>' => {
                self.read_char();
                self.read_char();
                Token {
                    token_type: TokenType::Arrow,
                    literal: "->".into(),
                    span,
                }
            }
            '>' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::Gte,
                        literal: ">=".into(),
                        span,
                    }
                }
                '>' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::RShift,
                        literal: ">>".into(),
                        span,
                    }
                }
                _ => self.single_with_span(TokenType::Gt, span),
            },
            '<' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::Lte,
                        literal: "<=".into(),
                        span,
                    }
                }
                '<' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::LShift,
                        literal: "<<".into(),
                        span,
                    }
                }
                _ => self.single_with_span(TokenType::Lt, span),
            },
            '-' => match self.peek_char() {
                '-' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::Decrement,
                        literal: "--".into(),
                        span,
                    }
                }
                _ => self.single_with_span(TokenType::Minus, span),
            },
            '+' => match self.peek_char() {
                '+' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::Increment,
                        literal: "++".into(),
                        span,
                    }
                }
                _ => self.single_with_span(TokenType::Plus, span),
            },
            ':' => {
                // In indent mode, colon at end of line becomes LBrace
                if self.indent_mode && self.is_colon_at_eol() {
                    self.read_char(); // consume ':'
                    Token {
                        token_type: TokenType::LBrace,
                        literal: "{".into(),
                        span,
                    }
                } else {
                    self.single_with_span(TokenType::Colon, span)
                }
            }
            '=' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::Eq,
                        literal: "==".into(),
                        span,
                    }
                }
                _ => self.single_with_span(TokenType::Assign, span),
            },
            '!' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::NotEq,
                        literal: "!=".into(),
                        span,
                    }
                }
                _ => self.single_with_span(TokenType::Shebang, span),
            },
            ',' => self.single_with_span(TokenType::Comma, span),
            '$' => self.single_with_span(TokenType::DollarSign, span),
            '#' => self.single_with_span(TokenType::Hash, span),
            '@' => self.single_with_span(TokenType::At, span),
            '%' => self.single_with_span(TokenType::Mod, span),
            '&' => self.single_with_span(TokenType::Ampersand, span),
            '.' => self.single_with_span(TokenType::FullStop, span),
            '|' => match self.peek_char() {
                '|' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::DoublePipe,
                        literal: "||".into(),
                        span,
                    }
                }
                _ => self.single_with_span(TokenType::Pipe, span),
            },
            // Oxigen has no statement terminator; Illegal carries its own message so the parser reports the fix.
            ';' => {
                self.read_char();
                let span = Span::range(span.start, self.pos());
                self.diagnostics.push(
                    Diagnostic::error(
                        codes::ILLEGAL_TOKEN,
                        span,
                        "`;` is not an Oxigen statement terminator",
                    )
                    .label("remove this")
                    .note("Oxigen ends a statement at the newline"),
                );
                self.illegal(";", span)
            }
            '^' => self.single_with_span(TokenType::Caret, span),
            '~' => self.single_with_span(TokenType::Tilde, span),
            '[' => self.single_with_span(TokenType::LBracket, span),
            ']' => self.single_with_span(TokenType::RBracket, span),
            '{' => self.single_with_span(TokenType::LBrace, span),
            '}' => self.single_with_span(TokenType::RBrace, span),
            '(' => self.single_with_span(TokenType::LParen, span),
            ')' => self.single_with_span(TokenType::RParen, span),
            '*' => self.single_with_span(TokenType::Asterisk, span),
            '/' => match self.peek_char() {
                '/' => {
                    self.skip_line_comment();
                    self.next_token()
                }
                '*' => {
                    self.skip_block_comment();
                    self.next_token()
                }
                _ => self.single_with_span(TokenType::FSlash, span),
            },
            '\\' => self.single_with_span(TokenType::BSlash, span),
            '"' => {
                let triple = self.at_triple_quote('"');
                self.read_string('"', triple, span)
            }
            '\'' => {
                let triple = self.at_triple_quote('\'');
                self.read_string('\'', triple, span)
            }
            '`' => self.read_char_literal(span),
            c if c.is_ascii_digit() => self.read_number(span),
            c if is_ident_start(c) => self.read_ident(span),
            _ => {
                let lit = self.ch.to_string();
                self.read_char();
                let span = Span::range(span.start, self.pos());
                // `?` marks optional parameters and is consumed by the parser, so it is not an error.
                if lit != "?" {
                    self.diagnostics.push(Diagnostic::error(
                        codes::ILLEGAL_TOKEN,
                        span,
                        format!("unexpected character {lit:?}"),
                    ));
                }
                self.illegal(&lit, span)
            }
        }
    }

    fn single_with_span(&mut self, tt: TokenType, span: Span) -> Token {
        let lit = self.ch.to_string();
        self.read_char();
        Token {
            token_type: tt,
            literal: lit,
            span,
        }
    }

    fn read_char(&mut self) {
        if self.ch == '\n' {
            self.line += 1;
            self.column = 0;
        }
        self.ch = if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        };
        self.position = self.read_position;
        self.read_position += 1;
        if self.ch != '\0' {
            self.column += 1;
        }
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }

    fn skip_whitespace_except_newline(&mut self) {
        while self.ch.is_whitespace() && self.ch != '\n' {
            self.read_char();
        }
    }

    fn skip_line_comment(&mut self) {
        let (span, own_line, start) = self.comment_start();
        self.read_char(); // skip first '/'
        self.read_char(); // skip second '/'
        while self.ch != '\n' && self.ch != '\0' {
            self.read_char();
        }
        self.record_comment(span, own_line, start);
        // Leave self.ch at '\n' or '\0' so next_token handles it
    }

    fn skip_block_comment(&mut self) {
        let (span, own_line, start) = self.comment_start();
        self.read_char(); // skip '/'
        self.read_char(); // skip '*'
        let mut closed = false;
        loop {
            if self.ch == '\0' {
                break;
            }
            if self.ch == '*' && self.peek_char() == '/' {
                self.read_char(); // skip '*'
                self.read_char(); // skip '/'
                closed = true;
                break;
            }
            self.read_char();
        }
        if !closed {
            // Running to EOF used to be silent: everything after `/*` was swallowed and the file exited 0.
            self.diagnostics.push(
                Diagnostic::error(
                    codes::UNTERMINATED_BLOCK_COMMENT,
                    span,
                    "unterminated block comment",
                )
                .label("this comment is never closed")
                .note("everything after this point was treated as a comment")
                .help("close it with `*/`"),
            );
        }
        self.record_comment(span, own_line, start);
    }

    /// Snapshot taken before a comment is consumed: its span, whether anything
    /// but whitespace precedes it on that line, and where its text begins.
    fn comment_start(&self) -> (Span, bool, usize) {
        let mut i = self.position;
        let own_line = loop {
            if i == 0 {
                break true;
            }
            i -= 1;
            match self.input[i] {
                '\n' => break true,
                c if c.is_whitespace() => {}
                _ => break false,
            }
        };
        (self.span(), own_line, self.position)
    }

    fn record_comment(&mut self, span: Span, own_line: bool, start: usize) {
        let text: String = self.input[start..self.position].iter().collect();
        self.comments.push(Comment {
            line: span.line(),
            column: span.column(),
            text: text.trim_end().to_string(),
            own_line,
        });
    }

    fn read_ident(&mut self, span: Span) -> Token {
        let start = self.position;
        while is_ident_continue(self.ch) {
            self.read_char();
        }
        let literal: String = self.input[start..self.position].iter().collect();
        let tt = self
            .keywords
            .get(literal.as_str())
            .cloned()
            .unwrap_or(TokenType::Ident);
        Token {
            token_type: tt,
            literal,
            span,
        }
    }

    fn read_number(&mut self, span: Span) -> Token {
        let start = self.position;
        let mut is_float = false;

        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        // Check for decimal point followed by digits
        if self.ch == '.' && self.peek_char().is_ascii_digit() {
            is_float = true;
            self.read_char(); // consume '.'
            while self.ch.is_ascii_digit() {
                self.read_char();
            }
        }

        // `1.2.3` used to lex as `1.2` then `.3`, surfacing as a field access on FLOAT.
        if is_float && self.ch == '.' && self.peek_char().is_ascii_digit() {
            while self.ch == '.' || self.ch.is_ascii_digit() {
                self.read_char();
            }
            let literal: String = self.input[start..self.position].iter().collect();
            let span = Span::range(span.start, self.pos());
            self.diagnostics.push(
                Diagnostic::error(
                    codes::MALFORMED_NUMBER,
                    span,
                    format!("malformed number literal `{literal}`"),
                )
                .label("more than one decimal point")
                .help("a number may contain at most one `.`"),
            );
            return self.illegal(&literal, span);
        }

        let literal: String = self.input[start..self.position].iter().collect();
        Token {
            token_type: if is_float {
                TokenType::Float
            } else {
                TokenType::Integer
            },
            literal,
            span,
        }
    }

    /// True when the scanner sits at a run of three `delimiter` chars — the
    /// opening or closing fence of a triple-quoted string (`"""` or `'''`).
    fn at_triple_quote(&self, delimiter: char) -> bool {
        self.ch == delimiter
            && self.peek_char() == delimiter
            && self.input.get(self.position + 2).copied() == Some(delimiter)
    }

    /// True when the scanner is positioned at the delimiter run that closes a
    /// string of this kind: one quote for a single-line string, three for a
    /// triple-quoted one.
    fn at_closing_delimiter(&self, delimiter: char, triple: bool) -> bool {
        if triple {
            self.at_triple_quote(delimiter)
        } else {
            self.ch == delimiter
        }
    }

    /// True when scanning of the string body should stop: at the closing
    /// delimiter, at EOF, or — for single-line strings only — at a raw newline
    /// (which makes the string unterminated). Triple-quoted strings span
    /// newlines and only end at their closing fence or EOF.
    fn at_string_body_end(&self, delimiter: char, triple: bool) -> bool {
        self.ch == '\0'
            || self.at_closing_delimiter(delimiter, triple)
            || (!triple && self.ch == '\n')
    }

    /// Consume the run of delimiter chars that opens or closes a string: one
    /// quote normally, three for a triple-quoted string.
    fn consume_quote_fence(&mut self, triple: bool) {
        self.read_char();
        if triple {
            self.read_char();
            self.read_char();
        }
    }

    fn read_string(&mut self, delimiter: char, triple: bool, span: Span) -> Token {
        self.consume_quote_fence(triple); // opening quote(s)

        // Check if this string contains interpolation
        let has_interp = self.string_has_interpolation(delimiter, triple);

        if !has_interp {
            // A single-line string stops at the delimiter, EOF, or a raw newline.
            let mut literal = std::string::String::new();
            while !self.at_string_body_end(delimiter, triple) {
                if self.ch == '\\' {
                    self.push_escape_sequence(&mut literal, delimiter);
                } else {
                    literal.push(self.ch);
                    self.read_char();
                }
            }
            if !self.at_closing_delimiter(delimiter, triple) {
                // Do not consume the sentinel, so the rest of the line still tokenizes without a cascade.
                return self.unterminated_string_token(span);
            }
            self.consume_quote_fence(triple); // closing quote(s)
            return Token {
                token_type: if triple {
                    TokenType::MultilineString
                } else {
                    TokenType::String
                },
                literal,
                span,
            };
        }

        // Remember the pending-token count so the queue can roll back cleanly.
        let pending_mark = self.pending_tokens.len();
        let mut literal_buf = std::string::String::new();

        // Stop at the closing fence, EOF, or a raw newline for single-line strings.
        while !self.at_string_body_end(delimiter, triple) {
            if self.ch == '{' {
                // Emit any accumulated literal as a String token
                if !literal_buf.is_empty() {
                    self.pending_tokens.push_back(Token {
                        token_type: TokenType::String,
                        literal: literal_buf.clone(),
                        span,
                    });
                    literal_buf.clear();
                }

                let expr_span = self.span();
                // Emit InterpExprStart
                self.pending_tokens.push_back(Token {
                    token_type: TokenType::InterpExprStart,
                    literal: "{".into(),
                    span: expr_span,
                });

                self.read_char(); // skip '{'

                // Lex tokens inside {} using a brace depth counter
                let mut brace_depth = 1;
                loop {
                    // Newlines are only valid here inside a triple-quoted string.
                    loop {
                        self.skip_whitespace_except_newline();
                        if triple && self.ch == '\n' {
                            self.read_char();
                            continue;
                        }
                        break;
                    }

                    if self.ch == '\0' {
                        break;
                    }

                    // Depth counts from the first significant character so a nested map brace is not read as the end.
                    if self.ch == '}' {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            break; // caller consumes the closing brace
                        }
                    } else if self.ch == '{' {
                        brace_depth += 1;
                    }

                    // The comment arms restart via next_token, which would pop the interpolation tokens queued below.
                    if self.ch == '/' && matches!(self.peek_char(), '/' | '*') {
                        if self.peek_char() == '/' {
                            self.skip_line_comment();
                        } else {
                            self.skip_block_comment();
                        }
                        continue;
                    }

                    // Use the real lexer so `{ ... }` supports the whole language, not a hand-maintained subset.
                    let inner_span = self.span();
                    let inner_tok = self.lex_token_at(inner_span);
                    self.pending_tokens.push_back(inner_tok);
                }

                let end_span = self.span();
                // Emit InterpExprEnd
                self.pending_tokens.push_back(Token {
                    token_type: TokenType::InterpExprEnd,
                    literal: "}".into(),
                    span: end_span,
                });

                self.read_char(); // skip closing '}'
            } else if self.ch == '\\' {
                self.push_escape_sequence(&mut literal_buf, delimiter);
            } else {
                literal_buf.push(self.ch);
                self.read_char();
            }
        }

        if !self.at_closing_delimiter(delimiter, triple) {
            // Discard the queued part tokens and report one error at the opening quote.
            self.pending_tokens.truncate(pending_mark);
            return self.unterminated_string_token(span);
        }

        // Emit any trailing literal
        if !literal_buf.is_empty() {
            self.pending_tokens.push_back(Token {
                token_type: TokenType::String,
                literal: literal_buf,
                span,
            });
        }

        // Emit InterpEnd
        self.pending_tokens.push_back(Token {
            token_type: TokenType::InterpEnd,
            literal: "".into(),
            span: self.span(),
        });

        self.consume_quote_fence(triple); // closing quote(s)

        // A multi-line variant when triple-quoted, so the formatter can round-trip it.
        Token {
            token_type: if triple {
                TokenType::MultilineInterpStart
            } else {
                TokenType::InterpStart
            },
            literal: "".into(),
            span,
        }
    }

    /// Build the diagnostic token for a string literal that was never closed.
    ///
    /// A single-line Oxigen string is closed by a matching quote on the same
    /// line (newlines are written with the `\n` escape), so reaching
    /// end-of-line or end-of-input first is a missing-quote typo. A
    /// triple-quoted string (`"""`/`'''`) may span lines, so only end-of-input
    /// before its closing fence is unterminated. Either way we surface this as
    /// a `TokenType::Illegal` token whose `literal` is the human-readable
    /// message and whose `span` points at the OPENING quote — the actual
    /// location of the mistake — instead of letting the lexer swallow the rest
    /// of the file and produce a misleading cascade of parser errors. The
    /// parser turns this `Illegal` token into a reported diagnostic.
    fn unterminated_string_token(&mut self, span: Span) -> Token {
        self.diagnostics.push(
            Diagnostic::error(codes::UNTERMINATED_STRING, span, "unterminated string literal")
                .label("this string is never closed")
                .help("close it on the same line, or use a triple-quoted string to span lines"),
        );
        self.illegal("unterminated string literal", span)
    }

    /// A recovery placeholder. The diagnostic is recorded separately.
    fn illegal(&self, literal: &str, span: Span) -> Token {
        Token {
            token_type: TokenType::Illegal,
            literal: literal.to_string(),
            span,
        }
    }

    fn push_escape_sequence(&mut self, literal: &mut std::string::String, delimiter: char) {
        self.read_char(); // consume '\'
        match self.ch {
            'n' => {
                literal.push('\n');
                self.read_char();
            }
            't' => {
                literal.push('\t');
                self.read_char();
            }
            'r' => {
                literal.push('\r');
                self.read_char();
            }
            'e' => {
                literal.push('\x1b');
                self.read_char();
            }
            'x' if self.peek_hex_byte().is_some() => {
                let value = self.peek_hex_byte().unwrap();
                literal.push(value as char);
                self.read_char(); // consume 'x'
                self.read_char(); // consume first hex digit
                self.read_char(); // consume second hex digit
            }
            '\\' => {
                literal.push('\\');
                self.read_char();
            }
            '0' => {
                literal.push('\0');
                self.read_char();
            }
            c if c == delimiter => {
                literal.push(c);
                self.read_char();
            }
            // `\{` and `\}` are the only way to write a literal brace; a bare `{` starts an interpolation.
            '{' | '}' => {
                literal.push(self.ch);
                self.read_char();
            }
            other => {
                literal.push('\\');
                literal.push(other);
                self.read_char();
            }
        }
    }

    fn peek_hex_byte(&self) -> Option<u8> {
        let first = *self.input.get(self.position + 1)?;
        let second = *self.input.get(self.position + 2)?;
        let high = first.to_digit(16)?;
        let low = second.to_digit(16)?;
        Some(((high << 4) | low) as u8)
    }

    /// Look ahead to check if a string contains `{` before its closing fence.
    ///
    /// For a single-line string the scan stops at the delimiter, EOF, or a raw
    /// newline (which makes the string unterminated). For a triple-quoted
    /// string the scan spans newlines and stops only at the closing `"""`/`'''`
    /// fence or EOF. In both cases a non-interpolated result routes the string
    /// to the simpler escape-only branch of `read_string`.
    fn string_has_interpolation(&self, delimiter: char, triple: bool) -> bool {
        let mut pos = self.position;
        while pos < self.input.len() {
            let c = self.input[pos];
            if c == '\0' {
                return false;
            }
            if triple {
                // Closing fence: three delimiter chars in a row.
                if c == delimiter
                    && self.input.get(pos + 1).copied() == Some(delimiter)
                    && self.input.get(pos + 2).copied() == Some(delimiter)
                {
                    return false;
                }
            } else if c == delimiter || c == '\n' {
                return false;
            }
            if c == '\\' {
                pos += 2; // skip escaped character
                continue;
            }
            if c == '{' {
                return true;
            }
            pos += 1;
        }
        false
    }

    fn read_char_literal(&mut self, span: Span) -> Token {
        self.read_char(); // opening `
        let start = self.position;
        while self.ch != '`' && self.ch != '\0' {
            self.read_char();
        }
        let literal: String = self.input[start..self.position].iter().collect();
        self.read_char(); // closing `

        // Validate it's exactly one character
        if literal.chars().count() != 1 {
            let span = Span::range(span.start, self.pos());
            self.diagnostics.push(
                Diagnostic::error(
                    codes::INVALID_CHAR_LITERAL,
                    span,
                    format!("invalid character literal `{literal}`"),
                )
                .note("a character literal holds exactly one character")
                .help("use a string literal for zero or more than one character"),
            );
            return self.illegal(&format!("invalid char literal: `{}`", literal), span);
        }

        Token {
            token_type: TokenType::Char,
            literal,
            span,
        }
    }

    // === Indent mode helpers ===

    /// Check if the colon is at end of line (only whitespace until newline/EOF)
    fn is_colon_at_eol(&self) -> bool {
        let mut pos = self.read_position;
        while pos < self.input.len() {
            let c = self.input[pos];
            if c == '\n' || c == '\0' {
                return true;
            }
            // A // comment counts as end of line
            if c == '/' && pos + 1 < self.input.len() && self.input[pos + 1] == '/' {
                return true;
            }
            if !c.is_whitespace() {
                return false;
            }
            pos += 1;
        }
        true // EOF counts as end of line
    }

    /// Measure the indentation level at current position (count spaces/tabs)
    /// Called at the start of a line, before skipping whitespace
    fn measure_indentation(&self) -> usize {
        let mut indent = 0;
        let mut pos = self.position;

        // If we're past a newline, start from read_position
        if self.ch == '\n' {
            pos = self.read_position;
        }

        while pos < self.input.len() {
            match self.input[pos] {
                ' ' => indent += 1,
                '\t' => indent += 4, // Treat tab as 4 spaces
                '\n' => {
                    // Empty line - reset and continue to next line
                    indent = 0;
                    pos += 1;
                    continue;
                }
                _ => break,
            }
            pos += 1;
        }
        indent
    }

    /// Handle indentation changes - emit RBrace tokens for dedents
    fn handle_indentation(&mut self, indent_level: usize) {
        let current_indent = *self.indent_stack.last().unwrap_or(&0);
        let span = self.span();

        if indent_level > current_indent {
            // Indent increased - push new level (LBrace was already emitted by colon)
            self.indent_stack.push(indent_level);
        } else if indent_level < current_indent {
            // Dedent - pop levels and emit RBrace for each
            while let Some(&top) = self.indent_stack.last() {
                if top <= indent_level {
                    break;
                }
                self.indent_stack.pop();
                self.pending_tokens.push_back(Token {
                    token_type: TokenType::RBrace,
                    literal: "}".into(),
                    span,
                });
            }
        }
        // If equal, no action needed
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests;
