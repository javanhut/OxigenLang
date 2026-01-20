use std::env;
use std::fs;
mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

use lexer::Lexer;
use token::TokenType;

fn main() {
    let args: Vec<String> = env::args().collect();
    if let Some(file_path) = args.get(1)
        && file_path.ends_with(".oxi")
    {
        let contents =
            fs::read_to_string(file_path).expect("Should have been able to read this file");

        println!("=== Lexing: {} ===\n", file_path);

        let mut lexer = Lexer::new(&contents);
        loop {
            let tok = lexer.next_token();
            let is_eof = tok.token_type == TokenType::Eof;

            // Skip newlines for cleaner output
            if tok.token_type != TokenType::Newline {
                println!("{:?}", tok);
            }

            if is_eof {
                break;
            }
        }
    } else {
        repl::run_repl();
    }
}
