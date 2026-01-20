use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, Write};

const OXI_VERSION: &str = "0.1.0";
pub fn run_repl() {
    let line_prefix: &str = ">> ";
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    loop {
        print!("{}", line_prefix);
        stdout.flush().unwrap();

        let mut input = String::new();
        if stdin.read_line(&mut input).is_err() {
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

        let lexer = Lexer::new(line);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let errors = parser.error();
        if !errors.is_empty() {
            for err in errors {
                println!("Error: {}", err);
            }
        } else {
            for stmt in &program.statements {
                println!("{:?}", stmt);
            }
        }
    }
}
