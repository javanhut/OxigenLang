//! Regression: struct field annotations are enforced, as `docs/structs.md`
//! documents ("Field mutation is type-checked").
//!
//! Neither the constructor nor `SetField` looked at the field's declared
//! type: `P("x")` built a `P` whose `n <int>` held a STRING, `P { n: 2.5 }`
//! was accepted, and `p.n = "x"` / `self.n = "x"` succeeded.

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

/// A backend's name and constructor.
type Backend = (&'static str, fn() -> VM);

fn backends() -> [Backend; 3] {
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
            Ok(v) => {
                panic!("{name}: expected an error containing {needle:?}, got Ok({v})\n{source}")
            }
            Err(e) => assert!(
                e.contains(needle),
                "{name}: error {e:?} does not mention {needle:?}\n{source}"
            ),
        }
    }
}

const P: &str = "struct P {\n  n <int>\n  label <str>\n}\n";

#[test]
fn positional_constructor_checks_field_types() {
    assert_all_err(&format!("{P}P(\"x\", \"a\")"), "type mismatch");
    assert_all_err(&format!("{P}P(1, 2)"), "type mismatch");
}

#[test]
fn literal_constructor_checks_field_types() {
    assert_all_err(&format!("{P}P {{ n: 2.5, label: \"a\" }}"), "type mismatch");
}

#[test]
fn field_assignment_checks_the_declared_type() {
    assert_all_err(
        &format!("{P}p := P(1, \"a\")\np.n = \"x\"\np.n"),
        "type mismatch",
    );
}

#[test]
fn field_assignment_from_a_method_checks_the_declared_type() {
    let src = format!(
        "{P}P includes {{\n  fun bad() {{ self.n = \"s\" }}\n  fun bare() {{ n = \"s\" }}\n}}\np := P(1, \"a\")\n"
    );
    assert_all_err(&format!("{src}p.bad()\np.n"), "type mismatch");
    assert_all_err(&format!("{src}p.bare()\np.n"), "type mismatch");
}

#[test]
fn well_typed_construction_and_mutation_still_work() {
    assert_all_ok(
        &format!("{P}p := P(1, \"a\")\np.n = 5\np.label = \"b\"\n[p.n, p.label]"),
        "[5, b]",
    );
    assert_all_ok(&format!("{P}p := P {{ n: 3, label: \"z\" }}\np.n"), "3");
    // Zero-valued construction keeps working.
    assert_all_ok(&format!("{P}p <P>\n[p.n, p.label]"), "[0, ]");
}

#[test]
fn hot_field_writes_stay_checked_after_tier_up() {
    // The JIT's SetField path must agree with the interpreter.
    let src = format!(
        "{P}p := P(0, \"a\")\nfun bump(p, v) {{ p.n = v }}\neach i in range(200) {{ bump(p, i) }}\nbump(p, \"x\")\np.n"
    );
    assert_all_err(&src, "type mismatch");
}

#[test]
fn struct_and_untyped_fields_accept_any_matching_value() {
    let src = "struct Node {\n  v <int>\n  next <Node>\n  tag <generic>\n}\n\
               a := Node(1, None, \"t\")\n\
               b := Node(2, a, 5)\n\
               b.next = None\n\
               b.tag = [1]\n\
               [b.v, b.next, b.tag]";
    assert_all_ok(src, "[2, None, [1]]");
}
