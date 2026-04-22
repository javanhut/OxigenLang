//! Milestone 1 fallback-harness tests.
//!
//! These exercise the full JIT plumbing end-to-end: the Cranelift module
//! emits a thunk, the thunk calls `jit_run_via_interpreter`, and the helper
//! drives the interpreter for one activation. Program output must match
//! the interpreter-only baseline byte-for-byte.
//!
//! No actual bytecode translation happens yet (that's Step 3).

#![cfg(feature = "jit")]

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run(source: &str, jit_threshold: Option<u32>) -> (String, usize, usize) {
    run_with_thresholds(source, jit_threshold, None)
}

fn run_result(source: &str, jit_threshold: Option<u32>) -> Result<String, String> {
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
        .expect("compile should succeed");

    let mut vm = VM::new();
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    vm.run(function)
        .map(|v| format!("{}", v))
        .map_err(|e| e.message)
}

fn run_with_thresholds(
    source: &str,
    jit_threshold: Option<u32>,
    loop_threshold: Option<u32>,
) -> (String, usize, usize) {
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
        .expect("compile should succeed");

    let mut vm = VM::new();
    if let Some(t) = jit_threshold {
        vm.jit.set_threshold(t);
    }
    if let Some(t) = loop_threshold {
        vm.jit.set_loop_threshold(t);
    }
    let result = vm.run(function).expect("run should succeed");
    (
        format!("{}", result),
        vm.jit.compiled_ok_count(),
        vm.jit.compile_failed_count(),
    )
}

fn run_disabled(source: &str) -> (String, usize, usize) {
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
        .expect("compile should succeed");

    let mut vm = VM::new();
    vm.jit.disable();
    let result = vm.run(function).expect("run should succeed");
    (
        format!("{}", result),
        vm.jit.compiled_ok_count(),
        vm.jit.compile_failed_count(),
    )
}

fn compile_function(source: &str) -> oxigen_core::vm::value::Function {
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
        .expect("compile should succeed")
}

#[test]
fn fallback_matches_interpreter_simple_add() {
    // The nested fn `compute` has a body of just `Constant Constant Add
    // Return` — every opcode is in the M1 allow-list, so scan passes and
    // the fallback thunk gets installed.
    let source = r#"
fun compute() {
    1 + 2
}
compute()
"#;

    let (baseline, b_ok, b_failed) = run(source, None);
    let (jitted, j_ok, j_failed) = run(source, Some(1));

    assert_eq!(
        baseline, jitted,
        "program output must match across backends"
    );
    assert_eq!(
        b_ok, 0,
        "with JIT disabled (default threshold), nothing should compile"
    );
    assert_eq!(b_failed, 0);
    assert!(
        j_ok >= 1,
        "JIT should have compiled at least `compute` (got {} ok, {} failed)",
        j_ok,
        j_failed
    );
}

#[test]
fn fallback_matches_interpreter_arith_via_args() {
    // The function body is pure arithmetic on a parameter — exercises
    // GetLocal + Constant + Add + Multiply + Return, all in the M1
    // allow-list. Type annotations trigger `TypeWrap`, which is NOT in
    // the allow-list, so we deliberately leave the parameter untyped.
    let source = r#"
fun poly(x) {
    x * x + x + 1
}
poly(5)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, j_failed) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(
        j_ok >= 1,
        "expected a compile; got {} ok, {} failed",
        j_ok,
        j_failed
    );
}

#[test]
fn fallback_matches_interpreter_conditional_branch() {
    // Exercises JumpIfFalse + Jump + Pop: the `option` construct compiles
    // to a classic then/else branch.
    let source = r#"
fun max2(a, b) {
    option { a > b -> a, b }
}
max2(3, 7)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "max2 should compile");
}

#[test]
fn fallback_matches_interpreter_recursive_fib() {
    // True recursive fib — requires GetGlobal (fib references itself via
    // the enclosing scope). Exercises JIT-to-JIT recursion with globals.
    let source = r#"
fun fib(n) {
    option { n < 2 -> n, fib(n - 1) + fib(n - 2) }
}
fib(10)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "fib should compile; got {} ok", j_ok);
}

#[test]
fn fallback_matches_interpreter_recursive_globals_other_than_self() {
    let source = r#"
step := 3

fun sumdown(n) {
    option { n < 1 -> 0, step + sumdown(n - 1) }
}

sumdown(5)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "15");
    assert!(j_ok >= 1, "sumdown should compile; got {} ok", j_ok);
}

#[test]
fn fallback_matches_interpreter_compiled_recursion_with_interpreted_helper() {
    let source = r#"
fun helper(x) {
    (x,)
}

fun walk(n) {
    option { n < 1 -> 0, helper(walk(n - 1) + 1)[0] }
}

walk(6)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, j_failed) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "6");
    assert!(
        j_ok >= 1,
        "walk should compile even though helper stays interpreted; got {} ok, {} failed",
        j_ok,
        j_failed
    );
}

#[test]
fn fallback_matches_interpreter_closure_with_upvalue() {
    // Verifies Closure + GetUpvalue. `make_adder` returns a closure that
    // captures `n` as an upvalue; the inner closure reads it via
    // GetUpvalue and adds it to its argument.
    let source = r#"
fun make_adder(n) {
    fun(x) { x + n }
}
add5 := make_adder(5)
add5(10)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(
        j_ok >= 2,
        "make_adder and the inner closure should both compile; got {} ok",
        j_ok
    );
}

#[test]
fn fallback_matches_interpreter_div_mod_and_eq() {
    let source = r#"
fun collatzish(n) {
    option {
        n % 2 == 0 -> n / 2,
        n * 3 + 1
    }
}
collatzish(10) + collatzish(9)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "expected collatzish to compile");
}

#[test]
fn fallback_matches_interpreter_divide_by_zero_error() {
    let source = r#"
fun boom(n) {
    n / 0
}
boom(10)
"#;

    let baseline = run_result(source, None).expect_err("baseline should error");
    let jitted = run_result(source, Some(1)).expect_err("jitted run should error");

    assert_eq!(baseline, jitted);
}

#[test]
fn fallback_matches_interpreter_jit_to_jit_call() {
    // Exercises the Call opcode from inside JIT code. `apply` receives the
    // callee as an argument (a local) rather than a global, so its body
    // stays inside the M1 allow-list (GetLocal / Call / Return). When
    // `apply` invokes `double`, both call sides run JIT-compiled code.
    //
    // (A classic recursive fib can't be used here: fib references itself
    // via `GetGlobal`, which is intentionally outside M1.)
    let source = r#"
fun double(x) {
    x * 2
}

fun apply(f, x) {
    f(x)
}

apply(double, 5)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(
        j_ok >= 2,
        "both `double` and `apply` should compile; got {}",
        j_ok
    );
}

#[test]
fn fallback_matches_interpreter_struct_methods() {
    // Exercises StructLiteral / GetField / MethodCall all inside JIT
    // code.
    let source = r#"
struct Point {
    x <int>
    y <int>
}

Point contains {
    fun sum_xy() { self.x + self.y }
}

p <Point> := Point(3, 4)
p.sum_xy()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "expected some compile; got {}", j_ok);
}

#[test]
fn fallback_matches_interpreter_struct_method_mutation_shapes() {
    let source = r#"
struct Counter {
    val <int>
}

Counter contains {
    fun inc() { self.val = self.val + 1 }
    fun add(amount) { self.val = self.val + amount }
}

fun run() {
    c <Counter> := Counter(0)
    c.inc()
    c.add(4)
    c.val
}

run()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "5");
    assert!(j_ok >= 2, "expected method bodies to compile; got {}", j_ok);
}

#[test]
fn fallback_matches_interpreter_mixed_numeric_add() {
    // Integer+Float takes the slow path in the JIT (tag check fails on
    // the right operand). Make sure the slow-path fallback still
    // produces the correct promoted-float result.
    let source = r#"
fun addem(n) {
    n + 1.5
}
addem(2)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1);
}

#[test]
fn jit_local_const_update_peephole_matches_interpreter() {
    let source = r#"
fun update(n) {
    x := 1
    x := x + 4
    x := x * 3
    x := x - 2
    x := x - n
    x
}
update(2)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "11");
    assert!(j_ok >= 1);
}

#[test]
fn jit_local_const_update_peephole_slow_path_keeps_float_semantics() {
    let source = r#"
fun update() {
    x := 1.5
    x := x + 2
    x
}
update()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "3.5");
    assert!(j_ok >= 1);
}

#[test]
fn jit_local_const_update_peephole_requires_same_destination_slot() {
    let source = r#"
fun update() {
    a := 1
    b := 10
    a := b + 1
    a * 100 + b
}
update()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "1110");
    assert!(j_ok >= 1);
}

#[test]
fn jit_local_scaled_update_peephole_matches_loop_shape() {
    let source = r#"
fun update(n) {
    total := 0
    i := 1
    repeat when i <= n {
        total := total + i * 2
        i := i + 1
    }
    total
}
update(5)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "30");
    assert!(j_ok >= 1);
}

#[test]
fn jit_local_scaled_update_peephole_supports_subtract() {
    let source = r#"
fun update() {
    total := 100
    i := 3
    total := total - i * 4
    total
}
update()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "88");
    assert!(j_ok >= 1);
}

#[test]
fn jit_local_scaled_update_peephole_slow_path_keeps_float_semantics() {
    let source = r#"
fun update() {
    total := 1.5
    i := 3
    total := total + i * 2
    total
}
update()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "7.5");
    assert!(j_ok >= 1);
}

#[test]
fn default_jit_compiles_single_call_hot_loop_by_backedge_count() {
    let source = r#"
fun update(n) {
    total := 0
    i := 1
    repeat when i <= n {
        total := total + i * 2
        i := i + 1
    }
    total
}
update(20)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run_with_thresholds(source, None, Some(3));

    assert_eq!(baseline, jitted);
    assert_eq!(jitted, "420");
    assert!(
        j_ok >= 1,
        "loop-hot function should compile even though it is called once"
    );
}

#[test]
fn no_jit_disables_loop_hot_compilation() {
    let source = r#"
fun update(n) {
    total := 0
    i := 1
    repeat when i <= n {
        total := total + i * 2
        i := i + 1
    }
    total
}
update(20)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, j_failed) = run_disabled(source);

    assert_eq!(baseline, jitted);
    assert_eq!(j_ok, 0);
    assert_eq!(j_failed, 0);
}

#[test]
fn unsupported_opcodes_bail_out_cleanly() {
    // Map literals are still outside the JIT allow-list, so the top-level
    // scan must fail and the interpreter must take over without disruption.
    let source = r#"
m := {"x": 7}
m["x"]
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, _, j_failed) = run(source, Some(1));

    assert_eq!(baseline, jitted);
    assert!(
        j_failed >= 1,
        "top-level with map literal should have been rejected by scan"
    );
}

#[test]
fn typed_local_metadata_preserves_type_lock() {
    let function = compile_function(
        r#"
fun typed(a <int>, b <array>) {
    c <float> := 1.5
    b[0] + a
}
typed(2, [40])
"#,
    );

    let closure = function
        .chunk
        .constants
        .iter()
        .find_map(|v| match v {
            oxigen_core::vm::value::Value::Closure(c) => Some(c),
            _ => None,
        })
        .expect("expected closure constant");
    let locals = &closure.function.locals;
    assert_eq!(locals[1].type_constraint.as_deref(), Some("INTEGER"));
    assert_eq!(locals[2].type_constraint.as_deref(), Some("ARRAY"));
    assert_eq!(locals[3].type_constraint.as_deref(), Some("FLOAT"));
    assert!(locals[1].mutable);
    assert!(locals[2].mutable);
}

#[test]
fn jit_compiles_array_indexing() {
    let source = r#"
fun pick() {
    arr <array> := [10, 20, 30]
    arr[1] + arr[-1]
}
pick()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, "50");
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "array indexing function should compile");
}

#[test]
fn jit_array_index_out_of_range_matches_interpreter() {
    let source = r#"
fun pick() {
    arr <array> := [10, 20, 30]
    arr[99]
}
pick()
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run(source, Some(1));

    assert_eq!(baseline, "None");
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "array indexing function should compile");
}

#[test]
fn jit_fuses_local_array_mod_index_sum_loop() {
    let source = r#"
fun array_index_sum(n <int>) {
    arr <array> := [1, 2, 3, 4]
    i <int> := 0
    total <int> := 0
    repeat unless i >= n {
        total = total + arr[i % 4]
        i = i + 1
    }
    total
}
array_index_sum(20)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, _) = run_with_thresholds(source, Some(1), Some(1));

    assert_eq!(baseline, "50");
    assert_eq!(baseline, jitted);
    assert!(j_ok >= 1, "array sum loop should compile");
}

#[test]
fn jit_fuses_local_to_local_sum_loop() {
    let source = r#"
fun loop_sum(n <int>) {
    i <int> := 0
    total <int> := 0
    repeat unless i >= n {
        total = total + i
        i = i + 1
    }
    total
}
loop_sum(20)
"#;

    let (baseline, _, _) = run(source, None);
    let (jitted, j_ok, j_failed) = run_with_thresholds(source, Some(1), Some(1));

    assert_eq!(baseline, "190");
    assert_eq!(baseline, jitted);
    assert!(
        j_ok >= 2,
        "top-level and local-to-local sum loop should compile (ok={j_ok}, failed={j_failed})"
    );
}
