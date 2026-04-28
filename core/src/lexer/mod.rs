use crate::token::{Span, Token, TokenType, token_map};
use std::collections::HashMap;
use std::collections::VecDeque;

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
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let (effective_input, start_line, indent_mode) = Self::preprocess_input(input);

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
        };
        l.read_char();
        l
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

    /// Returns the current span (line, column) for the character being processed.
    fn span(&self) -> Span {
        Span::new(self.line, self.column)
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

        let tok = match self.ch {
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
            '^' => self.single_with_span(TokenType::Caret, span),
            '~' => self.single_with_span(TokenType::Tilde, span),
            ';' => self.single_with_span(TokenType::Semicolon, span),
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
                    return self.next_token();
                }
                '*' => {
                    self.skip_block_comment();
                    return self.next_token();
                }
                _ => self.single_with_span(TokenType::FSlash, span),
            },
            '\\' => self.single_with_span(TokenType::BSlash, span),
            '"' => self.read_string('"', span),
            '\'' => self.read_string('\'', span),
            '`' => self.read_char_literal(span),
            c if c.is_ascii_digit() => return self.read_number(span),
            c if is_ident_start(c) => return self.read_ident(span),
            _ => {
                let lit = self.ch.to_string();
                self.read_char();
                Token {
                    token_type: TokenType::Illegal,
                    literal: lit,
                    span,
                }
            }
        };
        tok
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
        self.read_char(); // skip first '/'
        self.read_char(); // skip second '/'
        while self.ch != '\n' && self.ch != '\0' {
            self.read_char();
        }
        // Leave self.ch at '\n' or '\0' so next_token handles it
    }

    fn skip_block_comment(&mut self) {
        self.read_char(); // skip '/'
        self.read_char(); // skip '*'
        loop {
            if self.ch == '\0' {
                break;
            }
            if self.ch == '*' && self.peek_char() == '/' {
                self.read_char(); // skip '*'
                self.read_char(); // skip '/'
                break;
            }
            self.read_char();
        }
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

    fn read_string(&mut self, delimiter: char, span: Span) -> Token {
        self.read_char(); // opening quote

        // Check if this string contains interpolation
        let has_interp = self.string_has_interpolation(delimiter);

        if !has_interp {
            // Simple string, no interpolation — process escape sequences
            let mut literal = std::string::String::new();
            while self.ch != delimiter && self.ch != '\0' {
                if self.ch == '\\' {
                    self.push_escape_sequence(&mut literal, delimiter);
                } else {
                    literal.push(self.ch);
                    self.read_char();
                }
            }
            self.read_char(); // closing quote
            return Token {
                token_type: TokenType::String,
                literal,
                span,
            };
        }

        // String has interpolation — emit InterpStart, then queue all parts
        // Collect literal parts and expression tokens
        let mut literal_buf = std::string::String::new();

        while self.ch != delimiter && self.ch != '\0' {
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
                while brace_depth > 0 && self.ch != '\0' {
                    if self.ch == '{' {
                        brace_depth += 1;
                    } else if self.ch == '}' {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            break;
                        }
                    }

                    // Skip whitespace inside interpolation
                    self.skip_whitespace_except_newline();
                    if self.ch == '}' {
                        continue;
                    }

                    let inner_span = self.span();
                    // Lex one token from inside the interpolation
                    let inner_tok = match self.ch {
                        c if c.is_ascii_digit() => self.read_number(inner_span),
                        c if is_ident_start(c) => self.read_ident(inner_span),
                        '"' => self.read_string('"', inner_span),
                        '\'' => self.read_string('\'', inner_span),
                        '(' => self.single_with_span(TokenType::LParen, inner_span),
                        ')' => self.single_with_span(TokenType::RParen, inner_span),
                        ',' => self.single_with_span(TokenType::Comma, inner_span),
                        '+' => self.single_with_span(TokenType::Plus, inner_span),
                        '-' => self.single_with_span(TokenType::Minus, inner_span),
                        '*' => self.single_with_span(TokenType::Asterisk, inner_span),
                        '/' => self.single_with_span(TokenType::FSlash, inner_span),
                        '.' => self.single_with_span(TokenType::FullStop, inner_span),
                        '[' => self.single_with_span(TokenType::LBracket, inner_span),
                        ']' => self.single_with_span(TokenType::RBracket, inner_span),
                        _ => {
                            let lit = self.ch.to_string();
                            self.read_char();
                            Token {
                                token_type: TokenType::Illegal,
                                literal: lit,
                                span: inner_span,
                            }
                        }
                    };
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

        self.read_char(); // closing quote

        // Return InterpStart as the first token
        Token {
            token_type: TokenType::InterpStart,
            literal: "".into(),
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

    /// Look ahead to check if a string contains `{` before its closing delimiter
    fn string_has_interpolation(&self, delimiter: char) -> bool {
        let mut pos = self.position;
        while pos < self.input.len() {
            let c = self.input[pos];
            if c == delimiter || c == '\0' {
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
            return Token {
                token_type: TokenType::Illegal,
                literal: format!("invalid char literal: `{}`", literal),
                span,
            };
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
