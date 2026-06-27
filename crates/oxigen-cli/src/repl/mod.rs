use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;
use oxigen_core::vm::value::Value;
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;

const OXI_VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn run_repl() {
    // Run the session on a large-stack thread so a function that tiers up to
    // the JIT has room for its native frames (mirrors run_file_vm). rustyline
    // reads stdin fine from a spawned thread, and the Rc-backed VM is created
    // inside the closure so nothing non-Send crosses the boundary.
    const STACK_SIZE: usize = 256 << 20; // 256 MB
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

    // One persistent VM for the whole session. Top-level `var`/`fun`/`struct`/
    // `enum` declarations compile to globals, so they carry across lines.
    let mut vm = VM::new();

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

/// Compile and run a single line on the persistent VM, printing the result
/// unless it is None. Parse, compile, and runtime errors are reported inline
/// and do not end the session; after a runtime error the VM's transient state
/// is reset so the next line runs against a balanced stack.
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
    match vm.run(function) {
        Ok(Value::None) => {}
        Ok(Value::Error(msg)) => println!("{}", msg),
        Ok(result) => println!("{}", result),
        Err(err) => {
            println!("{}", err.message);
            vm.reset_exec_state();
        }
    }
}
