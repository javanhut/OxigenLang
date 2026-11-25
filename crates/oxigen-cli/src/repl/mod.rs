use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;
use oxigen_core::vm::value::Value;
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;

pub fn run_repl() {
    const STACK_SIZE: usize = 256 << 20;
    std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(repl_loop)
        .expect("failed to spawn REPL thread")
        .join()
        .expect("REPL thread panicked");
}

fn repl_loop() {
    let prompt = ">> ";
    let mut rl = DefaultEditor::new().expect("Failed to initialize line editor");

    let mut vm = VM::new();
    vm.set_script_args(&[]);

    println!("Oxigen REPL");
    crate::print_version();
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
                    crate::print_version();
                    continue;
                }

                eval_line(&mut vm, line);
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("Error: {}", err);
                break;
            }
        }
    }
}

/// Compile and run a single line on the persistent VM, printing the result unless it is None.
/// Parse, compile, and runtime errors are reported inline and do not end the session.
fn eval_line(vm: &mut VM, line: &str) {
    let lexer = Lexer::new(line);
    let mut parser = Parser::new(lexer, line);
    let program = parser.parse_program();

    if !parser.errors().is_empty() {
        println!("{}", parser.format_errors());
        return;
    }

    let function = match Compiler::new().compile(&program) {
        Ok(f) => f,
        Err(errors) => {
            for err in &errors {
                println!("{}", err);
            }
            return;
        }
    };

    vm.set_source(line);
    let unit = oxigen_core::concurrent::unit_of(&function);
    oxigen_core::concurrent::register_program(&function, &program, line, None, true);
    match vm.run(function) {
        Ok(Value::None) => {}
        Ok(Value::Error(msg)) => println!("{}", msg),
        Ok(result) => println!("{}", result),
        Err(err) => {
            println!("{}", err.message);
            vm.reset_exec_state();
            oxigen_core::concurrent::unregister_program(unit);
        }
    }
}
