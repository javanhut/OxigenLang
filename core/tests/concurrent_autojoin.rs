//! Auto-join: a task handle resolves to its value when consumed by an operator,
//! without an explicit `converge`. Covers arithmetic, comparison, equality,
//! unary, and indexing.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run(source: &str) -> String {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parse errors:\n{}",
        parser.format_errors()
    );
    let function = Compiler::new()
        .compile(&program)
        .map_err(|e| format!("{:?}", e))
        .expect("compile should succeed");
    oxigen_core::concurrent::set_src(source.to_string());
    let mut vm = VM::new();
    vm.run(function)
        .map(|v| format!("{}", v))
        .unwrap_or_else(|e| format!("ERR: {}", e.message))
}

#[test]
fn auto_join_resolves_task_at_every_operator_site() {
    let out = run("fun w(x) { x * x }\n\
         a := diverge { w(6) }\n\
         b := diverge { w(5) }\n\
         c := diverge { w(4) }\n\
         d := diverge { w(3) }\n\
         e := diverge { w(2) }\n\
         i := diverge { w(1) }\n\
         [a * 2, b - 5, c == 16, d > 8, -e, [10, 20, 30][i]]\n");
    assert_eq!(out, "[72, 20, True, True, -4, 20]");
}
