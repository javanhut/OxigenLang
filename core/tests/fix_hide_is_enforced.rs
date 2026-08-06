//! `hide` is a rule, not a comment.
//!
//! The keyword parsed, the flag rode all the way into `ObjStructDef.fields` as
//! the third tuple element — and nothing ever read it. A field marked `hide`
//! could be read, written, and supplied to a constructor from anywhere, while
//! docs/structs.md promised it was "accessible inside methods but not from
//! outside the struct" and docs/conventions.md used it to guard a bank balance
//! and a PIN. Encapsulation that only holds in code review is worse than none,
//! because it reads as enforced.
//!
//! Access is permitted from code compiled as part of the instance's own struct
//! chain: a method of the struct, a method it inherited, or a closure nested in
//! one of those. Everything else — top-level code, a plain function, another
//! struct's method — is refused.
//!
//! Every case runs on both backends. The interpreter reads the executing frame
//! from `frames.last()`, but a method that tiers up executes from a JIT frame,
//! where that answers for the *caller* — so a hot method was refused its own
//! `self.field` until the check moved to `active_closure()`.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn compile(source: &str) -> oxigen_core::vm::value::Function {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(
        parser.errors().is_empty(),
        "parser errors:\n{}",
        parser.format_errors()
    );
    Compiler::new()
        .compile(&program)
        .unwrap_or_else(|errs| panic!("compile errors: {errs:?}"))
}

fn run_on(mut vm: VM, source: &str) -> Result<String, String> {
    vm.run(compile(source))
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

/// Asserts both backends accept `source`, and returns the agreed value.
fn eval(source: &str) -> String {
    let interp =
        run_on(VM::new_interpreter(), source).unwrap_or_else(|e| panic!("interpreter: {e}"));
    let jit = run_on(VM::new_eager_jit(), source).unwrap_or_else(|e| panic!("jit: {e}"));
    assert_eq!(interp, jit, "backends disagree for:\n{source}");
    interp
}

/// Asserts both backends reject `source`, and returns the agreed message.
fn eval_err(source: &str) -> String {
    let interp = run_on(VM::new_interpreter(), source).expect_err("interpreter should reject");
    let jit = run_on(VM::new_eager_jit(), source).expect_err("jit should reject");
    assert_eq!(interp, jit, "backends disagree for:\n{source}");
    interp
}

const ACCOUNT: &str = r#"
struct Account {
    hide balance <int>
    owner <str>
}

Account includes {
    fun open(o <str>, b <int>) {
        self.owner = o
        self.balance = b
    }
    fun total() { self.balance }
    fun richer_than(other <Account>) { self.balance > other.balance }
    fun clone_with(b <int>) { Account(balance=b, owner=self.owner) }
}

fun make() {
    a <Account>
    a.open("ada", 100)
    a
}
"#;

#[test]
fn a_method_reaches_its_own_hidden_field() {
    assert_eq!(eval(&format!("{ACCOUNT}\nmake().total()")), "100");
}

#[test]
fn a_method_reaches_another_instance_of_the_same_struct() {
    let src = format!(
        "{ACCOUNT}
b <Account>
b.open(\"bo\", 10)
make().richer_than(b)"
    );
    assert_eq!(eval(&src), "True");
}

#[test]
fn a_closure_nested_in_a_method_still_counts_as_the_struct() {
    // The lambda is a separate function; it inherits `method_of` from the
    // method it was written inside, or `self.balance` would be refused here.
    let src = format!(
        "{ACCOUNT}
Account includes {{
    fun scaled(f <int>) {{ array.map([f], fun(x <int>) {{ self.balance * x }}) }}
}}
introduce array
make().scaled(2)"
    );
    assert_eq!(eval(&src), "[200]");
}

#[test]
fn a_subclass_method_reaches_an_inherited_hidden_field() {
    let src = format!(
        "{ACCOUNT}
struct Savings(Account) {{
    rate <int>
}}
Savings includes {{
    fun projected() {{ self.balance * self.rate }}
}}
s <Savings>
s.open(\"ada\", 50)
s.rate = 2
s.projected()"
    );
    assert_eq!(eval(&src), "100");
}

#[test]
fn a_hot_method_keeps_its_access_after_tiering_up() {
    // The regression the JIT frame lookup fixes: the loop makes `grind` a JIT
    // candidate, and reading the executing frame from `frames.last()` there
    // answers for the caller — top-level code, which owns no struct.
    let src = format!(
        "{ACCOUNT}
Account includes {{
    fun grind() {{
        i <int>
        total <int>
        repeat when i < 100000 {{
            total = total + self.balance
            i++
        }}
        total
    }}
}}
make().grind()"
    );
    assert_eq!(eval(&src), "10000000");
}

#[test]
fn reading_a_hidden_field_from_outside_is_refused() {
    let msg = eval_err(&format!("{ACCOUNT}\nmake().balance"));
    assert!(
        msg.contains("hidden on Account"),
        "unexpected message: {msg}"
    );
}

#[test]
fn writing_a_hidden_field_from_outside_is_refused() {
    let msg = eval_err(&format!(
        "{ACCOUNT}
fun poke(a <Account>) {{ a.balance = 5 }}
poke(make())"
    ));
    assert!(
        msg.contains("hidden on Account"),
        "unexpected message: {msg}"
    );
}

#[test]
fn an_unrelated_structs_method_is_refused() {
    // Being *a* method is not enough — it has to be a method of this struct.
    let msg = eval_err(&format!(
        "{ACCOUNT}
struct Intruder {{ x <int> }}
Intruder includes {{
    fun peek(a <Account>) {{ a.balance }}
}}
i <Intruder>
i.peek(make())"
    ));
    assert!(
        msg.contains("hidden on Account"),
        "unexpected message: {msg}"
    );
}

#[test]
fn construction_from_outside_cannot_supply_a_hidden_field() {
    // Blocking reads while leaving the initial value open would be half a rule.
    let msg = eval_err(&format!("{ACCOUNT}\nAccount(balance=1, owner=\"x\").owner"));
    assert!(
        msg.contains("hidden on Account"),
        "unexpected message: {msg}"
    );
}

#[test]
fn a_factory_inside_the_struct_may_still_build_one() {
    // Otherwise a struct with a hidden field could never be built with a value.
    assert_eq!(eval(&format!("{ACCOUNT}\nmake().clone_with(7).total()")), "7");
}

#[test]
fn zero_value_construction_is_untouched() {
    let src = format!("{ACCOUNT}\nc <Account>\nc.total()");
    assert_eq!(eval(&src), "0");
}

#[test]
fn public_fields_on_the_same_struct_are_unaffected() {
    assert_eq!(eval(&format!("{ACCOUNT}\nmake().owner")), "ada");
}
