//! Struct constructors bind named (field=value) arguments. The expectation is
//! VM(interp) == VM(JIT) == expected.

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_vm(source: &str, jit_threshold: Option<u32>) -> Result<String, String> {
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
        .map_err(|errs| format!("{:?}", errs))?;
    let mut vm = VM::new();
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    vm.run(function)
        .map(|v| format!("{}", v))
        .map_err(|e| e.message)
}

/// Assert VM (interp) == VM (JIT) == `expected`.
fn assert_parity(source: &str, expected: &str) {
    let interp = run_vm(source, None).expect("VM interp should succeed");
    let jit = run_vm(source, Some(1)).expect("VM JIT should succeed");
    assert_eq!(interp, expected, "VM interp mismatch");
    assert_eq!(jit, expected, "VM JIT mismatch");
    assert_eq!(interp, jit, "VM interp != VM JIT");
}

#[test]
fn named_ctor_arg_binds_value() {
    let src = r#"
struct S { x <str> }
s := S(x="hello")
s.x
"#;
    assert_parity(src, "hello");
}

#[test]
fn all_named_ctor_args_bind() {
    let src = r#"
struct ChatMessage { model <str>  messages <array> }
m := ChatMessage(model="gemma4:latest", messages=[])
m.model
"#;
    assert_parity(src, "gemma4:latest");
}

#[test]
fn named_ctor_args_bind_out_of_order() {
    let src = r#"
struct ChatMessage { model <str>  messages <array> }
m := ChatMessage(messages=[], model="gemma4:latest")
m.model
"#;
    assert_parity(src, "gemma4:latest");
}

#[test]
fn mixed_positional_then_named_ctor_args() {
    let src = r#"
struct ChatMessage { model <str>  messages <array> }
m := ChatMessage("gemma4:latest", messages=[])
m.model
"#;
    assert_parity(src, "gemma4:latest");
}
