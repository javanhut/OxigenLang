//! Regression: typed conversions and call encodings reject values they cannot
//! represent instead of wrapping silently.
//!
//! - `x <uint> := -1` gave 18446744073709551615 and `-1.5` gave 0, while
//!   `docs/type_system.md` limits `uint` conversion to values `>= 0` (and the
//!   `uint()` builtin already errors).
//! - `x <int> := 1e30` saturated to i64::MAX and NaN became 0.
//! - Argument counts and arities are encoded in one byte. A function with
//!   256 parameters reported "takes 256 to 0 arguments", and a call with 258
//!   arguments called the wrong callee and corrupted the stack.
//! - Parameter and variable type checks compare type NAMES, so an instance
//!   of a user struct called `INTEGER` passed an `<int>` check; such names
//!   are now reserved.

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
    let function = Compiler::new().compile(&program).map_err(|errs| {
        errs.iter()
            .map(|e| e.message.clone())
            .collect::<Vec<_>>()
            .join("; ")
    })?;
    vm.run(function)
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

fn backends() -> [(&'static str, fn() -> VM); 3] {
    [
        ("interpreter", VM::new_interpreter),
        ("tiered", VM::new_tiered),
        ("eager_jit", VM::new_eager_jit),
    ]
}

fn assert_all_ok(source: &str, expected: &str) {
    for (name, make) in backends() {
        let got = run_on(make(), source).unwrap_or_else(|e| panic!("{name} failed: {e}\n{source}"));
        assert_eq!(got, expected, "{name} mismatch for:\n{source}");
    }
}

fn assert_all_err(source: &str, needle: &str) {
    for (name, make) in backends() {
        match run_on(make(), source) {
            Ok(v) => panic!("{name}: expected an error containing {needle:?}, got Ok({v})\n{source}"),
            Err(e) => assert!(
                e.contains(needle),
                "{name}: error {e:?} does not mention {needle:?}\n{source}"
            ),
        }
    }
}

#[test]
fn uint_declaration_rejects_negative_values() {
    assert_all_err("x <uint> := -1\nx", "UINT");
    assert_all_err("x <uint> := -1.5\nx", "UINT");
    assert_all_err("fun f() {\n  x <uint> := -3\n  x\n}\nf()", "UINT");
}

#[test]
fn uint_declaration_still_converts_valid_values() {
    assert_all_ok("x <uint> := 3.7\nx", "3");
    assert_all_ok("x <uint> := 42\nx", "42");
}

#[test]
fn int_declaration_rejects_unrepresentable_floats() {
    assert_all_err("x <int> := 1000000000000000000000000000000.0\nx", "INTEGER");
    assert_all_err("x <int> := float(\"nan\")\nx", "INTEGER");
    assert_all_ok("x <int> := -3.9\nx", "-3");
}

fn params(n: usize) -> String {
    (0..n).map(|i| format!("p{i}")).collect::<Vec<_>>().join(", ")
}

#[test]
fn more_than_255_parameters_is_a_compile_error() {
    let src = format!("fun h({}) {{ 1 }}\nh", params(256));
    assert_all_err(&src, "255");
}

#[test]
fn more_than_255_arguments_is_a_compile_error() {
    let args = (0..258).map(|i| i.to_string()).collect::<Vec<_>>().join(", ");
    assert_all_err(&format!("print({args})"), "255");
    assert_all_err(&format!("fun f(a) {{ a }}\nf({args})"), "255");
}

#[test]
fn a_user_struct_named_like_a_builtin_type_does_not_pass_its_check() {
    assert_all_err(
        "struct INTEGER { v <int> }\nfun f(n <int>) { n }\nf(INTEGER(1))",
        "reserved for a built-in type",
    );
}
