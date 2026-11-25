//! Tests that a block declaring locals works anywhere an expression may
//! appear, including after values already pushed for the enclosing expression
//! (call arguments, array elements, operands).

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_on(mut vm: VM, source: &str) -> Result<String, String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    let function = Compiler::new()
        .compile(&program)
        .map_err(|errs| format!("{errs:?}"))?;
    vm.run(function)
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

fn assert_all_ok(source: &str, expected: &str) {
    for (name, make) in [
        ("interpreter", VM::new_interpreter as fn() -> VM),
        ("tiered", VM::new_tiered),
        ("eager_jit", VM::new_eager_jit),
    ] {
        let got = run_on(make(), source).unwrap_or_else(|e| panic!("{name} failed: {e}\n{source}"));
        assert_eq!(got, expected, "{name} mismatch for:\n{source}");
    }
}

#[test]
fn block_local_inside_call_arguments() {
    let src = "fun pair(a, b, c) { [a, b, c] }\n\
               fun f() {\n  a := 10\n  pair(1, 2, option { True -> {\n    y := 5\n    y + a\n  } })\n}\nf()";
    assert_all_ok(src, "[1, 2, 15]");
}

#[test]
fn block_local_inside_an_array_literal() {
    let src = "fun f() {\n  a := 10\n  [a, option { True -> {\n    y := 5\n    z := y * 2\n    z + a\n  } }, a]\n}\nf()";
    assert_all_ok(src, "[10, 20, 10]");
}

const RIGHT_OPERAND: &str =
    "fun f() {\n  a := 10\n  a + option { a > 1 -> {\n    y := 5\n    y + 1\n  }, 0 }\n}\nf()";

#[test]
fn block_local_as_right_operand() {
    let got = run_on(VM::new_interpreter(), RIGHT_OPERAND).expect("interpreter");
    assert_eq!(got, "16");
}

#[test]
fn block_local_as_right_operand_jit() {
    assert_all_ok(RIGHT_OPERAND, "16");
}

#[test]
fn block_local_at_top_level_inside_call_arguments() {
    let src =
        "fun pair(a, b) { [a, b] }\nk := 3\npair(k, option { True -> {\n  y := 4\n  y * k\n} })";
    assert_all_ok(src, "[3, 12]");
}

#[test]
fn captured_block_local_inside_call_arguments() {
    let src = "fun call(g) { g() }\nfun pair(a, b) { [a, b] }\n\
               fun f() {\n  pair(1, option { True -> {\n    y := 7\n    call(fun() { y + 1 })\n  } })\n}\nf()";
    assert_all_ok(src, "[1, 8]");
}
