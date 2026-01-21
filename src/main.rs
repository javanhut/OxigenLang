use std::cell::RefCell;
use std::env;
use std::fs;
use std::rc::Rc;

mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

use evaluator::Evaluator;
use lexer::Lexer;
use object::environment::Environment;
use parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if let Some(file_path) = args.get(1)
        && file_path.ends_with(".oxi")
    {
        let contents =
            fs::read_to_string(file_path).expect("Should have been able to read this file");

        let lexer = Lexer::new(&contents);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let errors = parser.error();
        if !errors.is_empty() {
            eprintln!("Parser errors:");
            for err in errors {
                eprintln!("  {}", err);
            }
            std::process::exit(1);
        }

        let env = Rc::new(RefCell::new(Environment::new()));
        let mut evaluator = Evaluator::new();
        let result = evaluator.eval_program(&program, env);

        // Print result if it's not None and not an error
        match result.as_ref() {
            object::Object::None => {}
            object::Object::Error(msg) => {
                eprintln!("Error: {}", msg);
                std::process::exit(1);
            }
            _ => println!("{}", result),
        }
    } else {
        repl::run_repl();
    }
}
