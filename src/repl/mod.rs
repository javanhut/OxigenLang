use std::io::{self, Write};

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
            println!("Oxi Version 0.1.0");
        }
        println!("{line}");
        

    }
}
