use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::object::environment::Environment;
use crate::object::Object;
use crate::parser::Parser;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::cell::RefCell;
use std::rc::Rc;

const OXI_VERSION: &str = "0.1.0";

pub fn run_repl() {
    let prompt: &str = ">> ";

    let mut rl = DefaultEditor::new().expect("Failed to initialize line editor");

    // Create persistent environment and evaluator for the REPL session
    let env = Rc::new(RefCell::new(Environment::new()));
    let mut evaluator = Evaluator::new();

    println!("Oxigen REPL v{}", OXI_VERSION);
    println!("Type 'exit' or 'quit' to exit, 'version' for version info");

    loop {
        match rl.readline(prompt) {
            Ok(input) => {
                let line = input.trim();
                if line.is_empty() {
                    continue;
                }

                rl.add_history_entry(&input).ok();

                if line == "exit" || line == "quit" {
                    break;
                }

                if line == "version" {
                    println!("Oxi Version {OXI_VERSION}");
                    continue;
                }

                let lexer = Lexer::new(line);
                let mut parser = Parser::new(lexer, line);
                let program = parser.parse_program();

                let errors = parser.errors();
                if !errors.is_empty() {
                    println!("{}", parser.format_errors());
                    continue;
                }

                evaluator.set_source(line);
                let result = evaluator.eval_program(&program, Rc::clone(&env));

                // Print result unless it's None
                match result.as_ref() {
                    Object::None => {}
                    Object::Error(msg) => println!("{}", msg),
                    Object::ErrorValue { .. } => println!("{}", result),
                    _ => println!("{}", result),
                }
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                eprintln!("Error: {}", err);
                break;
            }
        }
    }
}
