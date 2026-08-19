//! Regression: type metadata belongs to a physical local slot, which may be
//! reused by bindings in disjoint scopes. A typed binding in one `option` arm
//! must not impose its lock on an `each` loop's hidden integer index in another.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_vm(source: &str) -> Result<String, String> {
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
        .map_err(|errors| format!("{errors:?}"))?;
    VM::new()
        .run(function)
        .map(|value| format!("{value}"))
        .map_err(|error| error.message)
}

#[test]
fn later_typed_arm_does_not_lock_each_index_slot() {
    let source = r#"
fun collect(flag <bool>) {
    count <int> := 0
    option {
        flag -> {
            each item in ["a", "b"] {
                count = count + 1
            }
        }
        {
            first <str> := "x"
            second <str> := "y"
        }
    }
    count
}
collect(True)
"#;

    assert_eq!(run_vm(source).as_deref(), Ok("2"));
}

#[test]
fn earlier_typed_arm_does_not_lock_each_index_slot() {
    let source = r#"
fun collect(flag <bool>) {
    count <int> := 0
    option {
        flag -> {
            first <str> := "x"
            second <str> := "y"
        }
        {
            each item in ["a", "b"] {
                count = count + 1
            }
        }
    }
    count
}
collect(False)
"#;

    assert_eq!(run_vm(source).as_deref(), Ok("2"));
}
