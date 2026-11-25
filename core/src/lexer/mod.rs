use crate::diagnostics::Diagnostic;
use crate::diagnostics::registry as codes;
use crate::token::{Pos, Span, Token, TokenType, token_map};
use std::collections::HashMap;
use std::collections::VecDeque;

/// A comment seen while lexing. Comments never become tokens or reach the AST;
/// the lexer records them so the formatter can reprint them. `line`/`column`
/// are where the comment started, and `own_line` distinguishes a comment on
/// its own line from one trailing code.
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
    indent_mode: bool,
    indent_stack: Vec<usize>,
    pending_tokens: VecDeque<Token>,
    at_line_start: bool,
    line: usize,
    column: usize,
    /// Byte offset of each char in `input`, plus a final entry for EOF.
    char_byte_offsets: Vec<usize>,
    /// Bytes `preprocess_input` stripped (shebang, `#[...]` directives). Added
    /// to every offset so they index the original source.
    base_offset: usize,
    comments: Vec<Comment>,
    /// Open `(`, `[` and written `{` brackets. Indentation inside them is
    /// layout, not structure.
    bracket_depth: usize,
    /// Strings currently being lexed inside interpolations.
    interp_depth: usize,
    /// Diagnostics for lexical problems.
    diagnostics: Vec<Diagnostic>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let (effective_input, start_line, indent_mode) = Self::preprocess_input(input);

        let base_offset = input.len() - effective_input.len();
        let mut char_byte_offsets = Vec::with_capacity(effective_input.len() + 1);
        let mut running = 0usize;
        for c in effective_input.chars() {
            char_byte_offsets.push(running);
            running += c.len_utf8();
        }
        char_byte_offsets.push(running);

        let mut l = Self {
            input: effective_input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: '\0',
            keywords: token_map(),
            indent_mode,
            indent_stack: vec![0],
            pending_tokens: VecDeque::new(),
            at_line_start: true,
            line: start_line,
            column: 0,
            char_byte_offsets,
            base_offset,
            comments: Vec::new(),
            bracket_depth: 0,
            interp_depth: 0,
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

        if let Some(rest) = effective_input.strip_prefix('\u{feff}') {
            effective_input = rest;
        }

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

    /// The shebang and top-level directives (`#[indent]`, `#[location=...]`)
    /// that are stripped before any code is read, one per line, and whether
    /// they switch on indent mode.
    pub fn header_directives(input: &str) -> (Vec<String>, bool) {
        let (rest, _, indent_mode) = Self::preprocess_input(input);
        let header = &input[..input.len() - rest.len()];
        let header = header.strip_prefix('\u{feff}').unwrap_or(header);
        let directives = header
            .lines()
            .map(str::trim)
            .filter(|line| !line.is_empty())
            .map(str::to_string)
            .collect();
        (directives, indent_mode)
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
        loop {
            if let Some(tok) = self.pending_tokens.pop_front() {
                return tok;
            }

            if self.at_line_start {
                self.at_line_start = false;
                if self.indent_mode && self.bracket_depth == 0 {
                    let indent_level = self.measure_indentation();
                    self.handle_indentation(indent_level);
                    continue;
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

            if self.ch == '/' && matches!(self.peek_char(), '/' | '*') {
                if self.peek_char() == '/' {
                    self.skip_line_comment();
                } else {
                    self.skip_block_comment();
                }
                continue;
            }

            let span = self.span();
            let mut token = self.lex_token_at(span);
            token.span.end = self.pos();
            return token;
        }
    }

    /// True once every character has been consumed.
    fn at_eof(&self) -> bool {
        self.position >= self.input.len()
    }

    /// Lexes exactly one token starting at the current character.
    ///
    /// Does not handle the pending-token queue, indent-mode bookkeeping,
    /// leading whitespace or the newline token; those are the caller's
    /// responsibility.
    fn lex_token_at(&mut self, span: Span) -> Token {
        match self.ch {
            '\0' if !self.at_eof() => {
                self.read_char();
                let span = Span::range(span.start, self.pos());
                self.diagnostics.push(
                    Diagnostic::error(codes::ILLEGAL_TOKEN, span, "unexpected NUL byte")
                        .note("a NUL byte is not valid in Oxigen source")
                        .help("remove it, or write `\\0` inside a string"),
                );
                self.illegal("\0", span)
            }
            '\0' => {
                if self.indent_mode && self.indent_stack.len() > 1 {
                    self.indent_stack.pop();
                    while self.indent_stack.len() > 1 {
                        self.indent_stack.pop();
                        self.pending_tokens.push_back(Token {
                            token_type: TokenType::RBrace,
                            literal: "}".into(),
                            span,
                        });
                    }
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
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::MinusAssign,
                        literal: "-=".into(),
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
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::PlusAssign,
                        literal: "+=".into(),
                        span,
                    }
                }
                _ => self.single_with_span(TokenType::Plus, span),
            },
            ':' => {
                if self.indent_mode && self.bracket_depth == 0 && self.is_colon_at_eol() {
                    self.read_char();
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
            '[' | '{' | '(' => {
                self.bracket_depth += 1;
                let tt = match self.ch {
                    '[' => TokenType::LBracket,
                    '{' => TokenType::LBrace,
                    _ => TokenType::LParen,
                };
                self.single_with_span(tt, span)
            }
            ']' | '}' | ')' => {
                self.bracket_depth = self.bracket_depth.saturating_sub(1);
                let tt = match self.ch {
                    ']' => TokenType::RBracket,
                    '}' => TokenType::RBrace,
                    _ => TokenType::RParen,
                };
                self.single_with_span(tt, span)
            }
            '*' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::AsteriskAssign,
                        literal: "*=".into(),
                        span,
                    }
                }
                _ => self.single_with_span(TokenType::Asterisk, span),
            },
            '/' => match self.peek_char() {
                '/' => {
                    self.skip_line_comment();
                    self.next_token()
                }
                '*' => {
                    self.skip_block_comment();
                    self.next_token()
                }
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::SlashAssign,
                        literal: "/=".into(),
                        span,
                    }
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
        if !self.at_eof() {
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
        self.read_char();
        self.read_char();
        while self.ch != '\n' && !self.at_eof() {
            self.read_char();
        }
        self.record_comment(span, own_line, start);
    }

    fn skip_block_comment(&mut self) {
        let (span, own_line, start) = self.comment_start();
        self.read_char();
        self.read_char();
        let mut closed = false;
        loop {
            if self.at_eof() {
                break;
            }
            if self.ch == '*' && self.peek_char() == '/' {
                self.read_char();
                self.read_char();
                closed = true;
                break;
            }
            self.read_char();
        }
        if !closed {
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

        if self.ch == '.' && self.peek_char().is_ascii_digit() {
            is_float = true;
            self.read_char();
            while self.ch.is_ascii_digit() {
                self.read_char();
            }
        }

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

        if matches!(self.ch, 'e' | 'E') {
            let sign = matches!(self.peek_char(), '+' | '-');
            let digit_at = if sign {
                self.position + 2
            } else {
                self.position + 1
            };
            if self.input.get(digit_at).is_some_and(|c| c.is_ascii_digit()) {
                is_float = true;
                self.read_char();
                if sign {
                    self.read_char();
                }
                while self.ch.is_ascii_digit() {
                    self.read_char();
                }
            }
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
        self.at_eof()
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
        self.consume_quote_fence(triple);

        let has_interp = self.string_has_interpolation(delimiter, triple);

        if has_interp && self.interp_depth >= MAX_INTERP_DEPTH {
            while !self.at_string_body_end(delimiter, triple) {
                self.read_char();
            }
            if self.at_closing_delimiter(delimiter, triple) {
                self.consume_quote_fence(triple);
            }
            let span = Span::range(span.start, self.pos());
            self.diagnostics.push(Diagnostic::error(
                codes::BAD_STRING_INTERPOLATION,
                span,
                format!("string interpolation nested more than {MAX_INTERP_DEPTH} levels deep"),
            ));
            return self.illegal("nested interpolation", span);
        }

        if !has_interp {
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
                return self.unterminated_string_token(span);
            }
            self.consume_quote_fence(triple);
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

        let pending_mark = self.pending_tokens.len();
        let depth_mark = self.bracket_depth;
        let mut literal_buf = std::string::String::new();

        while !self.at_string_body_end(delimiter, triple) {
            if self.ch == '{' {
                if !literal_buf.is_empty() {
                    self.pending_tokens.push_back(Token {
                        token_type: TokenType::String,
                        literal: literal_buf.clone(),
                        span,
                    });
                    literal_buf.clear();
                }

                let expr_span = self.span();
                self.pending_tokens.push_back(Token {
                    token_type: TokenType::InterpExprStart,
                    literal: "{".into(),
                    span: expr_span,
                });

                self.read_char();

                let mut brace_depth = 1;
                loop {
                    loop {
                        self.skip_whitespace_except_newline();
                        if triple && self.ch == '\n' {
                            self.read_char();
                            continue;
                        }
                        break;
                    }

                    if self.at_eof() {
                        break;
                    }

                    if self.ch == '}' {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            break;
                        }
                    } else if self.ch == '{' {
                        brace_depth += 1;
                    }

                    if self.ch == '/' && matches!(self.peek_char(), '/' | '*') {
                        if self.peek_char() == '/' {
                            self.skip_line_comment();
                        } else {
                            self.skip_block_comment();
                        }
                        continue;
                    }

                    let inner_span = self.span();
                    let mark = self.pending_tokens.len();
                    self.interp_depth += 1;
                    let inner_tok = self.lex_token_at(inner_span);
                    self.interp_depth -= 1;
                    self.pending_tokens.insert(mark, inner_tok);
                }

                let end_span = self.span();
                self.pending_tokens.push_back(Token {
                    token_type: TokenType::InterpExprEnd,
                    literal: "}".into(),
                    span: end_span,
                });

                self.read_char();
            } else if self.ch == '\\' {
                self.push_escape_sequence(&mut literal_buf, delimiter);
            } else {
                literal_buf.push(self.ch);
                self.read_char();
            }
        }

        self.bracket_depth = depth_mark;
        if !self.at_closing_delimiter(delimiter, triple) {
            self.pending_tokens.truncate(pending_mark);
            return self.unterminated_string_token(span);
        }

        if !literal_buf.is_empty() {
            self.pending_tokens.push_back(Token {
                token_type: TokenType::String,
                literal: literal_buf,
                span,
            });
        }

        self.pending_tokens.push_back(Token {
            token_type: TokenType::InterpEnd,
            literal: "".into(),
            span: self.span(),
        });

        self.consume_quote_fence(triple);

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
    /// A single-line string is unterminated at end-of-line or end-of-input; a
    /// triple-quoted string only at end-of-input. The result is a
    /// `TokenType::Illegal` token whose `literal` is the message and whose
    /// `span` points at the opening quote.
    fn unterminated_string_token(&mut self, span: Span) -> Token {
        self.diagnostics.push(
            Diagnostic::error(
                codes::UNTERMINATED_STRING,
                span,
                "unterminated string literal",
            )
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
        self.read_char();
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
                self.read_char();
                self.read_char();
                self.read_char();
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
    /// newline. For a triple-quoted string it stops only at the closing
    /// `"""`/`'''` fence or EOF.
    fn string_has_interpolation(&self, delimiter: char, triple: bool) -> bool {
        let mut pos = self.position;
        while pos < self.input.len() {
            let c = self.input[pos];
            if triple {
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
                pos += 2;
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
        self.read_char();
        let start = self.position;
        while self.ch != '`' && !self.at_eof() {
            self.read_char();
        }
        let literal: String = self.input[start..self.position].iter().collect();
        self.read_char();

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

    /// Check if the colon is at end of line (only whitespace until newline/EOF)
    fn is_colon_at_eol(&self) -> bool {
        let mut pos = self.read_position;
        while pos < self.input.len() {
            let c = self.input[pos];
            if c == '\n' {
                return true;
            }
            if c == '/' && pos + 1 < self.input.len() && self.input[pos + 1] == '/' {
                return true;
            }
            if !c.is_whitespace() {
                return false;
            }
            pos += 1;
        }
        true
    }

    /// Indentation of the next line that holds code, called at the start of a
    /// line. Blank lines and lines holding only a comment are consumed (their
    /// comments still recorded).
    fn measure_indentation(&mut self) -> usize {
        loop {
            let mut indent = 0;
            let mut pos = self.position;
            while let Some(&c) = self.input.get(pos) {
                match c {
                    ' ' => indent += 1,
                    '\t' => indent += 4,
                    '\r' => {}
                    _ => break,
                }
                pos += 1;
            }
            let next = self.input.get(pos).copied();
            let comment = next == Some('/') && matches!(self.input.get(pos + 1), Some('/' | '*'));
            if next != Some('\n') && !comment {
                return indent;
            }
            while self.position < pos {
                self.read_char();
            }
            if comment {
                if self.peek_char() == '/' {
                    self.skip_line_comment();
                } else {
                    self.skip_block_comment();
                }
                self.skip_whitespace_except_newline();
                if self.ch != '\n' && !self.at_eof() {
                    return indent;
                }
            }
            if self.at_eof() {
                return indent;
            }
            self.read_char();
        }
    }

    /// Handle indentation changes - emit RBrace tokens for dedents
    fn handle_indentation(&mut self, indent_level: usize) {
        let current_indent = *self.indent_stack.last().unwrap_or(&0);
        let span = self.span();

        if indent_level > current_indent {
            self.indent_stack.push(indent_level);
        } else if indent_level < current_indent {
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
                self.pending_tokens.push_back(Token {
                    token_type: TokenType::Newline,
                    literal: "\n".into(),
                    span,
                });
            }
            if self
                .indent_stack
                .last()
                .is_some_and(|&top| top != indent_level)
            {
                self.diagnostics.push(
                    Diagnostic::error(
                        codes::ILLEGAL_TOKEN,
                        span,
                        "unindent does not match any outer indentation level",
                    )
                    .help("indent this line to the same column as the block it belongs to"),
                );
                self.indent_stack.push(indent_level);
            }
        }
    }
}

/// Interpolated strings nested inside one another's `{...}` beyond this are
/// reported rather than lexed.
const MAX_INTERP_DEPTH: usize = 64;

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests;
