use crate::token::{Token, TokenType, token_map};
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
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let indent_directive = "#[indent]";
        let indent_mode = input.trim_start().starts_with(indent_directive);

        // If indent mode, skip past the directive
        let effective_input = if indent_mode {
            let start = input.find(indent_directive).unwrap() + indent_directive.len();
            &input[start..]
        } else {
            input
        };

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
        };
        l.read_char();
        l
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
            self.read_char();
            self.at_line_start = true;
            return Token {
                token_type: TokenType::Newline,
                literal: "\n".into(),
            };
        }

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
                        });
                    }
                    // Queue the final EOF
                    self.pending_tokens.push_back(Token {
                        token_type: TokenType::Eof,
                        literal: "".into(),
                    });
                    return Token {
                        token_type: TokenType::RBrace,
                        literal: "}".into(),
                    };
                }
                Token {
                    token_type: TokenType::Eof,
                    literal: "".into(),
                }
            }
            ':' if self.peek_char() == '=' => {
                self.read_char();
                self.read_char();
                Token {
                    token_type: TokenType::Walrus,
                    literal: ":=".into(),
                }
            }
            '-' if self.peek_char() == '>' => {
                self.read_char();
                self.read_char();
                Token {
                    token_type: TokenType::Arrow,
                    literal: "->".into(),
                }
            }
            '>' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::Gte,
                        literal: ">=".into(),
                    }
                }
                '>' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::RShift,
                        literal: ">>".into(),
                    }
                }
                _ => self.single(TokenType::Gt),
            },
            '<' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::Lte,
                        literal: "<=".into(),
                    }
                }
                '<' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::LShift,
                        literal: "<<".into(),
                    }
                }
                _ => self.single(TokenType::Lt),
            },
            '-' => match self.peek_char() {
                '-' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::Decrement, literal: "--".into()
                    }
                }
                _ => self.single(TokenType::Minus),
            },
            '+' => match self.peek_char() {
                '+' => {
                    self.read_char();
                    self.read_char();
                    Token {
                        token_type: TokenType::Increment, literal: "++".into()
                    }
                }
                _ => self.single(TokenType::Plus),
            },
            ':' => {
                // In indent mode, colon at end of line becomes LBrace
                if self.indent_mode && self.is_colon_at_eol() {
                    self.read_char(); // consume ':'
                    Token {
                        token_type: TokenType::LBrace,
                        literal: "{".into(),
                    }
                } else {
                    self.single(TokenType::Colon)
                }
            },
            '=' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token {token_type: TokenType::Eq, literal: "==".into()}
                }
                _ => self.single(TokenType::Assign)
            }
            '!' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token { 
                        token_type: TokenType::NotEq,
                        literal: "!=".into()
                    }
                }
                _ => self.single(TokenType::Shebang)
            },
            ',' => self.single(TokenType::Comma),
            '$' => self.single(TokenType::DollarSign),
            '#' => self.single(TokenType::Hash),
            '@' => self.single(TokenType::At),
            '%' => self.single(TokenType::Mod),
            '&' => self.single(TokenType::Ampersand),
            '.' => self.single(TokenType::FullStop),
            '|' => self.single(TokenType::Pipe),
            '^' => self.single(TokenType::Caret),
            '~' => self.single(TokenType::Tilde),
            ';' => self.single(TokenType::Semicolon),
            '[' => self.single(TokenType::LBracket),
            ']' => self.single(TokenType::RBracket),
            '{' => self.single(TokenType::LBrace),
            '}' => self.single(TokenType::RBrace),
            '(' => self.single(TokenType::LParen),
            ')' => self.single(TokenType::RParen),
            '*' => self.single(TokenType::Asterisk),
            '/' => self.single(TokenType::FSlash),
            '\\' => self.single(TokenType::BSlash),
            '"' => self.read_string('"'),
            '\'' => self.read_string('\''),
            '`' => self.read_char_literal(),
            c if c.is_ascii_digit() => return self.read_number(),
            c if is_ident_start(c) => return self.read_ident(),
            _ => {
                let lit = self.ch.to_string();
                self.read_char();
                Token {
                    token_type: TokenType::Illegal,
                    literal: lit,
                }
            }
        };
        tok
    }

    fn single(&mut self, tt: TokenType) -> Token {
        let lit = self.ch.to_string();
        self.read_char();
        Token {
            token_type: tt,
            literal: lit,
        }
    }

    fn read_char(&mut self) {
        self.ch = if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        };
        self.position = self.read_position;
        self.read_position += 1;
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

    fn read_ident(&mut self) -> Token {
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
        }
    }

    fn read_number(&mut self) -> Token {
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
            token_type: if is_float { TokenType::Float } else { TokenType::Integer },
            literal,
        }
    }

    fn read_string(&mut self, delimiter: char) -> Token {
        self.read_char(); // opening quote
        let start = self.position;
        while self.ch != delimiter && self.ch != '\0' {
            self.read_char();
        }
        let literal: String = self.input[start..self.position].iter().collect();
        self.read_char(); // closing quote
        Token {
            token_type: TokenType::String,
            literal,
        }
    }

    fn read_char_literal(&mut self) -> Token {
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
            };
        }

        Token {
            token_type: TokenType::Char,
            literal,
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
mod tests {
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
        let has_lbrace = tokens.iter().any(|t| t.token_type == TokenType::LBrace && t.literal == "{");
        let has_rbrace = tokens.iter().any(|t| t.token_type == TokenType::RBrace && t.literal == "}");
        assert!(has_lbrace, "Should have literal LBrace");
        assert!(has_rbrace, "Should have literal RBrace");
    }

    #[test]
    fn test_indent_mode_colon_becomes_lbrace() {
        let input = "#[indent]\neach num in x:\n  print(num)\n";
        let tokens = collect_tokens(input);

        // Colon at EOL should become LBrace
        let has_lbrace = tokens.iter().any(|t| t.token_type == TokenType::LBrace);
        assert!(has_lbrace, "Colon at EOL should become LBrace in indent mode");
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

        let lbrace_count = tokens.iter().filter(|t| t.token_type == TokenType::LBrace).count();
        let rbrace_count = tokens.iter().filter(|t| t.token_type == TokenType::RBrace).count();

        assert_eq!(lbrace_count, 2, "Should have 2 LBrace tokens for nested blocks");
        assert_eq!(rbrace_count, 2, "Should have 2 RBrace tokens for nested blocks");
    }

    #[test]
    fn test_indent_directive_stripped() {
        let input = "#[indent]\nx := 5";
        let tokens = collect_tokens(input);

        // Should not have Hash or the indent directive tokens
        let has_hash_indent = tokens.iter().any(|t| t.literal == "#" || t.literal == "indent");
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
}
