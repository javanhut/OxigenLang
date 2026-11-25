//! Tests that a `lock`ed value stays locked when it crosses to a `diverge`
//! worker and back.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(parser.errors().is_empty(), "{}", parser.format_errors());
    let function = Compiler::new()
        .compile(&program)
        .expect("compile should succeed");
    oxigen_core::concurrent::set_src(source.to_string());
    VM::new()
        .run(function)
        .map(|v| format!("{}", v))
        .unwrap_or_else(|e| e.message)
}

#[test]
fn lock_survives_the_trip_to_a_worker_and_back() {
    let out = run(r#"
lock xs = [1, 2]
going := converge diverge { push(xs, 3) }
t := diverge {
    lock m = {"a": [1]}
    m
}
back := converge t
(going, converge diverge { push(back["a"], 2) })
"#);
    assert_eq!(
        out.matches("push() cannot change a locked array").count(),
        2,
        "{out}"
    );
}
