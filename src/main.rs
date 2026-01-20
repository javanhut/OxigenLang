use std::fs;
use std::env;
mod repl;
mod lexer;
mod token;



fn main(){
    let args: Vec<String> = env::args().collect();
    if let Some(file_path) = args.get(1) && file_path.ends_with(".oxi") {
        let contents = fs::read_to_string(file_path).expect("Should have been able to read this file");
        println!("Contents: {contents}");
    } else {
        repl::run_repl();
    }
} 


