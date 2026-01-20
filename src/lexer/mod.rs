use std::collections::HashMap;
use crate::token::{Token, TokenType, token_map};

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
    keywords: HashMap<&'static str, TokenType>,
}


impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut l = Self {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: '\0',
            keywords: token_map(),
        };
        l.read_char();
        l
    }
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace_except_newline();
        if self.ch == '\n' {
            self.read_char();
            return Token {
                token_type: TokenType::Newline,
                literal: "\n".into(),
            };
        }
        let tok = match self.ch {
            '\0' => Token { token_type: TokenType::Eof, literal: "".into() },
            ':' if self.peek_char() == '=' => {
                self.read_char();
                self.read_char();
                Token {token_type: TokenType::Walrus, literal: ":=".into()}
            }
            '-' if self.peek_char() == '>' => {
                self.read_char();
                self.read_char();
                Token {token_type: TokenType::Arrow, literal: "->".into()}
            }
            ':' => self.single(TokenType::Colon),
            '"' => self.read_string(),
            c if c.is_ascii_digit() => return self.read_number(),
            c if is_ident_start(c) => return self.read_ident(),
            _ => {
                let lit = self.ch.to_string();
                self.read_char();
                Token {token_type: TokenType::Illegal, literal: lit}
            }
        };
        tok
    }

    fn single(&mut self, tt: TokenType) -> Token {
        let lit = self.ch.to_string();
        self.read_char();
        Token {token_type: tt, literal: lit}
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
        while self.ch.is_whitespace() && self.ch != '\n'{
            self.read_char();
        }
    }

     fn read_ident(&mut self) -> Token {
        let start = self.position;
        while is_ident_continue(self.ch) {
            self.read_char();
        }
        let literal: String = self.input[start..self.position].iter().collect();
        let tt = self.keywords.get(literal.as_str()).cloned().unwrap_or(TokenType::Ident);
        Token { token_type: tt, literal }
    }

    fn read_number(&mut self) -> Token {
        let start = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        let literal: String = self.input[start..self.position].iter().collect();
        Token { token_type: TokenType::Integer, literal }
    }

      fn read_string(&mut self) -> Token {
        self.read_char(); // opening "
        let start = self.position;
        while self.ch != '"' && self.ch != '\0' {
            self.read_char();
        }
        let literal: String = self.input[start..self.position].iter().collect();
        self.read_char(); // closing "
        Token { token_type: TokenType::String, literal }
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}



