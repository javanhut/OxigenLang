use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::object::environment::Environment;
use crate::object::Object;
use crate::parser::Parser;
use std::cell::RefCell;
use std::io::{self, Write};
use std::rc::Rc;

const OXI_VERSION: &str = "0.1.0";

pub fn run_repl() {
    let line_prefix: &str = ">> ";
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    // Create persistent environment and evaluator for the REPL session
    let env = Rc::new(RefCell::new(Environment::new()));
    let mut evaluator = Evaluator::new();

    println!("Oxigen REPL v{}", OXI_VERSION);
    println!("Type 'exit' or 'quit' to exit, 'version' for version info");

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
            continue;
        }

        let lexer = Lexer::new(line);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let errors = parser.error();
        if !errors.is_empty() {
            for err in errors {
                println!("Error: {}", err);
            }
            continue;
        }

        let result = evaluator.eval_program(&program, Rc::clone(&env));

        // Print result unless it's None
        match result.as_ref() {
            Object::None => {}
            Object::Error(msg) => println!("Error: {}", msg),
            _ => println!("{}", result),
        }
    }
}
