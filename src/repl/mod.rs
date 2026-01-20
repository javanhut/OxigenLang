use crate::lexer::Lexer;
use crate::token::TokenType;
use std::io::{self, Write};

const OXI_VERSION: &str = "0.1.0";
pub fn run_repl() {
    let line_prefix: &str = ">> ";
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    loop {
        print!("{}",line_prefix);
        stdout.flush().unwrap();

        let mut input = String::new();
        if stdin.read_line(&mut input).is_err(){
            break;
        }
        let line = input.trim();
        if line == "exit" || line == "quit" {
            break;
        }
        if line.is_empty() {
            continue;
        }

        if line == "version" {
            println!("Oxi Version {OXI_VERSION}");
        }

        let mut lexer = Lexer::new(line);
        loop {
            let tok = lexer.next_token();
            println!("{:?}", tok);
            if tok.token_type ==  TokenType::Eof {
                break;
            }
        }
        

    }
}
