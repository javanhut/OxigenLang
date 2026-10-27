//! Regression tests for JIT miscompiles found by the adversarial review.
//! Each program must give the same value — or the same error, message,
//! line and stack trace included — under the interpreter, the tiered JIT
//! (default thresholds) and the eager JIT.
//!
//! 1. `x = x + y` on two locals went through the scaled-update peephole
//!    with a scale of 1, whose slow path multiplied `y` by 1 first: string
//!    concatenation failed and `uint + uint` came back INTEGER.
//! 2. The per-closure upvalue int cache went stale when another closure
//!    wrote the shared upvalue cell.
//! 3. The struct-field-add peephole read a register-held int local from its
//!    stale stack slot.
//! 4. The lean entry returned comparison results as INTEGER, not BOOLEAN.
//! 5. A JIT method-call hit ran the method against the caller's module
//!    globals instead of the defining module's.
//! 6. Compiled call paths skipped typed-parameter checks, so a STRING in an
//!    `<int>` parameter was read as a raw pointer.
//! 7. The lean self-call kept calling the original function after its
//!    global name was rebound.
//! 8. Method inline caches survived an `includes` block redefining methods.
//! 9. The field-add peephole wrote `hide` fields from outside the struct.
//! 10. Error traces dropped JIT frames when an interpreted frame was on top.
//! 11. Local-arith peephole slow paths reported the wrong error line.
//! 12. The slot-type pass mistook a reused slot's earlier initializer, or an
//!     expression temporary, for a local's initializer.
//! 13. A block local whose initializer stayed in registers was popped from
//!     memory at its scope end, dropping the value beneath it.
//! 14. Inlined callees (`fun(y) { x + y }`, `self.f = self.f + d`) skipped the
//!     callee's typed-parameter check.

#![cfg(feature = "jit")]

use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

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
    Compiler::new().compile(&program).expect("compile")
}

fn run_in(mut vm: VM, source: &str, file: Option<&Path>) -> String {
    vm.set_source(source);
    if let Some(f) = file {
        vm.set_file(f.to_path_buf());
    }
    match vm.run(compile(source)) {
        Ok(v) => format!("{v}"),
        Err(e) => format!("error: {}", e.message),
    }
}

fn outcomes(source: &str, file: Option<&Path>) -> [(&'static str, String); 3] {
    [
        ("interpreter", run_in(VM::new_interpreter(), source, file)),
        ("tiered", run_in(VM::new_tiered(), source, file)),
        ("eager", run_in(VM::new_eager_jit(), source, file)),
    ]
}

/// All three backends agree, and the value is `expected`.
fn assert_all(source: &str, expected: &str) {
    for (name, got) in outcomes(source, None) {
        assert_eq!(got, expected, "{name} mismatch for:\n{source}");
    }
}

/// All three backends fail with the same rendered error, which contains `needle`.
fn assert_all_fail(source: &str, needle: &str) {
    let all = outcomes(source, None);
    let interp = &all[0].1;
    assert!(
        interp.starts_with("error: ") && interp.contains(needle),
        "interpreter: {interp}"
    );
    for (name, got) in &all[1..] {
        assert_eq!(
            got, interp,
            "{name} differs from the interpreter for:\n{source}"
        );
    }
}

fn temp_dir(label: &str) -> PathBuf {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("oxigen-jitparity-{label}-{unique}"));
    fs::create_dir_all(&dir).unwrap();
    dir
}

// 1 ------------------------------------------------------------------------

#[test]
fn local_plus_local_concatenates_strings() {
    assert_all(
        "fun join(words) {\n    out := \"\"\n    each w in words {\n        out = out + w\n    }\n    out\n}\njoin([\"a\", \"b\", \"c\"])",
        "abc",
    );
}

#[test]
fn local_plus_local_keeps_uint() {
    assert_all(
        "fun f() {\n    a := uint(5)\n    b := uint(3)\n    a = a + b\n    type(a)\n}\nf()",
        "UINT",
    );
}

#[test]
fn local_minus_local_on_floats() {
    assert_all(
        "fun f(xs) {\n    t := 100.5\n    each x in xs {\n        t = t - x\n    }\n    t\n}\nf([0.25, 0.25])",
        "100",
    );
}

// 2 ------------------------------------------------------------------------

#[test]
fn shared_upvalue_written_by_sibling_closure() {
    assert_all(
        "fun mk() {\n    x := 1\n    get := fun() { x }\n    set := fun(v) { x = v }\n    [get, set]\n}\n\
         p := mk()\ng := p[0]\ns := p[1]\na := g()\ns(2)\nb := g()\ns(\"str\")\nc := g()\n[a, b, c]",
        "[1, 2, str]",
    );
}

#[test]
fn shared_upvalue_getter_setter_in_hot_loop() {
    assert_all(
        "fun mk() {\n    x := 0\n    get := fun() { x }\n    set := fun(v) { x = v }\n    [get, set]\n}\n\
         p := mk()\ng := p[0]\ns := p[1]\n\
         fun run(g, s) {\n    t := 0\n    i := 0\n    repeat when i < 100 {\n        s(i)\n        t = t + g()\n        i = i + 1\n    }\n    t\n}\nrun(g, s)",
        "4950",
    );
}

#[test]
fn closure_upvalue_add_sees_outer_write() {
    assert_all(
        "fun mk() {\n    k := 1\n    add := fun(v) { v + k }\n    bump := fun() { k = k + 10 }\n    [add, bump]\n}\n\
         p := mk()\nadd := p[0]\nbump := p[1]\n\
         fun run(f) {\n    t := 0\n    i := 0\n    repeat when i < 60 {\n        t = f(t)\n        i = i + 1\n    }\n    t\n}\n\
         a := run(add)\nbump()\nb := run(add)\n[a, b]",
        "[60, 660]",
    );
}

// 3 ------------------------------------------------------------------------

#[test]
fn struct_field_add_from_register_local() {
    assert_all(
        "struct Acc {\n    total <int>\n}\n\
         fun add_scaled(a, x <int>) {\n    k := 0\n    k = x * 10\n    a.total = a.total + k\n}\n\
         a := Acc(0)\ni := 0\nrepeat when i < 100 {\n    add_scaled(a, 1)\n    i = i + 1\n}\na.total",
        "1000",
    );
}

// 4 ------------------------------------------------------------------------

#[test]
fn lean_comparison_returns_boolean() {
    assert_all(
        "fun lt(a, b) { a < b }\n\
         fun drive() {\n    i := 0\n    repeat when i < 60 {\n        lt(i, 100)\n        i++\n    }\n\
             j := 0\n    repeat when j < 5 {\n        lt(1.5, 2.5)\n        j++\n    }\n\
             [lt(1, 2), type(lt(1, 2)), lt(1, 2) == True]\n}\ndrive()",
        "[True, BOOLEAN, True]",
    );
}

#[test]
fn lean_equality_returns_boolean() {
    assert_all(
        "fun same(a, b) { a == b }\n\
         fun drive() {\n    i := 0\n    r := False\n    repeat when i < 80 {\n        r = same(i, i)\n        i++\n    }\n    [r, type(r)]\n}\ndrive()",
        "[True, BOOLEAN]",
    );
}

// 5 ------------------------------------------------------------------------

const VEC_MODULE: &str = "fun scale(v) { v * 2 }\n\
    X := \"geo-X\"\n\
    struct Box {\n    v <int>\n}\n\
    Box includes {\n    fun doubled() { scale(self.v) }\n    fun tag() { X }\n}\n";

#[test]
fn method_ic_uses_defining_module_globals() {
    let dir = temp_dir("modglobals");
    fs::write(dir.join("vec.oxi"), VEC_MODULE).unwrap();
    let main = "introduce {Box} from .vec\n\
        fun sum(b, n) {\n    i := 0\n    t := 0\n    repeat when i < n {\n        t = t + b.doubled()\n        i = i + 1\n    }\n    t\n}\n\
        sum(Box(3), 10)";
    for (name, got) in outcomes(main, Some(&dir.join("main.oxi"))) {
        assert_eq!(got, "60", "{name}");
    }
}

#[test]
fn method_ic_ignores_callers_same_named_global() {
    let dir = temp_dir("modshadow");
    fs::write(dir.join("vec.oxi"), VEC_MODULE).unwrap();
    let main = "introduce {Box} from .vec\n\
        fun scale(v) { 1000 }\n\
        X := \"main-X\"\n\
        fun sum(b, n) {\n    i := 0\n    t := 0\n    s := \"\"\n    repeat when i < n {\n        t = t + b.doubled()\n        s = b.tag()\n        i = i + 1\n    }\n    [t, s]\n}\n\
        sum(Box(3), 60)";
    for (name, got) in outcomes(main, Some(&dir.join("main.oxi"))) {
        assert_eq!(got, "[360, geo-X]", "{name}");
    }
}

// 6 ------------------------------------------------------------------------

#[test]
fn typed_param_checked_after_warmup() {
    assert_all_fail(
        "fun sq(n <int>) { n * n }\n\
         fun drive(x) {\n    i := 0\n    t := 0\n    repeat when i < 100 {\n        t = t + sq(i)\n        i = i + 1\n    }\n    sq(x)\n}\ndrive(1.5)",
        "type mismatch for parameter 'n'",
    );
}

#[test]
fn typed_param_string_is_not_read_as_int() {
    assert_all_fail(
        "fun f(p <int>) {\n    g := fun() { p }\n    k := 0\n    k = p + p\n    k\n}\nf(1)\nf(\"ab\")",
        "type mismatch for parameter 'p'",
    );
}

#[test]
fn typed_param_float_is_not_read_as_int() {
    assert_all_fail(
        "fun f(p <int>) {\n    g := fun() { p }\n    k := 0\n    k = p + p\n    k\n}\nf(1)\nf(2.5)",
        "type mismatch for parameter 'p'",
    );
}

#[test]
fn typed_method_param_checked_after_warmup() {
    assert_all_fail(
        "struct C {\n    v <int>\n}\nC includes {\n    fun twice(x <int>) { x * 2 }\n}\n\
         fun drive(c, y) {\n    i := 0\n    t := 0\n    repeat when i < 100 {\n        t = t + c.twice(i)\n        i = i + 1\n    }\n    c.twice(y)\n}\ndrive(C(1), 2.5)",
        "type mismatch for parameter 'x'",
    );
}

// 7 ------------------------------------------------------------------------

#[test]
fn lean_self_call_follows_rebound_global() {
    assert_all(
        "fun fib(n) {\n    option {\n        n < 2 -> n\n        fib(n - 1) + fib(n - 2)\n    }\n}\n\
         orig := fib\nfib = fun(n) { 1000 }\norig(15)",
        "2000",
    );
}

#[test]
fn lean_self_call_rebound_after_warmup() {
    assert_all(
        "fun fib(n) {\n    option {\n        n < 2 -> n\n        fib(n - 1) + fib(n - 2)\n    }\n}\n\
         a := fib(15)\norig := fib\nfib = fun(n) { 1000 }\n[a, orig(15)]",
        "[610, 2000]",
    );
}

// 8 ------------------------------------------------------------------------

#[test]
fn method_ic_invalidated_by_includes_redefinition() {
    assert_all(
        "struct C {\n    n <int>\n}\n\
         C includes {\n    fun inc() { 1 }\n    fun name() { \"one\" }\n}\n\
         fun run(c) {\n    t := 0\n    s := \"\"\n    i := 0\n    repeat when i < 100 {\n        t = t + c.inc()\n        s = c.name()\n        i = i + 1\n    }\n    [t, s]\n}\n\
         c := C(0)\na := run(c)\n\
         C includes {\n    fun inc() { 100 }\n    fun name() { \"two\" }\n}\n\
         b := run(c)\n[a[0] + b[0], b[1]]",
        "[10100, two]",
    );
}

// 9 ------------------------------------------------------------------------

#[test]
fn field_add_peephole_respects_hide() {
    assert_all_fail(
        "struct C {\n    hide val <int>\n}\nC includes {\n    fun get() { self.val }\n}\n\
         fun bump(c, flag) {\n    option { flag -> { c.val = c.val + 1 } }\n    0\n}\n\
         c <C>\nk := 0\nrepeat when k < 100 {\n    bump(c, k > 80)\n    k = k + 1\n}\nc.get()",
        "hidden",
    );
}

#[test]
fn field_add_local_peephole_respects_hide() {
    assert_all_fail(
        "struct C {\n    hide val <int>\n}\n\
         fun bump(c, flag, d) {\n    option { flag -> { c.val = c.val + d } }\n    0\n}\n\
         c <C>\nk := 0\nrepeat when k < 100 {\n    bump(c, k > 80, k)\n    k = k + 1\n}\nk",
        "hidden",
    );
}

// 10 -----------------------------------------------------------------------

#[test]
fn trace_keeps_jit_frames_below_interpreted_frame() {
    assert_all_fail(
        "fun g(a, b) { a / b }\nfun h(a, b) { g(a, b) }\nh(7.0, 0.0)",
        "stack trace",
    );
}

// 11 -----------------------------------------------------------------------

#[test]
fn local_arith_slow_path_reports_its_own_line() {
    assert_all_fail(
        "fun f() {\n    x := 1\n    x = \"s\"\n    x = x + 1\n    x\n}\nf()",
        "type mismatch",
    );
}

// 12 -----------------------------------------------------------------------

#[test]
fn slot_reused_after_loop_keeps_first_local() {
    assert_all(
        "fun f() {\n    s := 0\n    each i in range(3) {\n        s = s + i\n    }\n    t := 0\n    [s, t]\n}\nf()",
        "[3, 0]",
    );
}

#[test]
fn expression_temporary_is_not_a_local_init() {
    assert_all(
        "fun f(b) {\n    acc := []\n    acc.push(16 + (2 * b))\n    each i in [7] {\n        each j in range(2, 3) { }\n    }\n    acc\n}\nf(4)",
        "[24]",
    );
}

#[test]
fn block_local_in_option_arm_does_not_clobber() {
    assert_all(
        "fun f(a) {\n    acc := [1]\n    option { a > 0 -> { v0 := 3 } }\n    acc\n}\n\
         fun drive() {\n    i := 0\n    r := []\n    repeat when i < 6000 {\n        r = f(i)\n        i = i + 1\n    }\n    r\n}\ndrive()",
        "[1]",
    );
}

#[test]
fn float_param_rejects_int_on_specialized_path() {
    assert_all_fail(
        "fun g(x <float>) {\n    option {\n        x < 1 -> 0\n        g(x - 1) + 1\n    }\n}\n\
         fun drive(v) {\n    i := 0\n    t := 0\n    repeat when i < 100 {\n        t = t + g(5.0)\n        i = i + 1\n    }\n    g(v)\n}\ndrive(3)",
        "type mismatch for parameter 'x'",
    );
}

#[test]
fn float_param_rejects_int_on_inlined_closure_add() {
    assert_all_fail(
        "fun mk() {\n    x := 10\n    fun(y <float>) { x + y }\n}\nadd := mk()\n\
         fun run(f, v) {\n    t := 0.0\n    i := 0\n    repeat when i < 100 {\n        t = t + f(1.5)\n        i = i + 1\n    }\n    f(v)\n}\nrun(add, 2)",
        "type mismatch for parameter 'y'",
    );
}

#[test]
fn guarded_counter_after_two_loops_reusing_slots() {
    // Reported by the stdlib work as "5 under the interpreter, 1 under the
    // JIT": three `each` loops in one chunk reuse the same hidden slots.
    assert_all(
        "h := []\neach i in range(6) { h = push(h, i + 40) }\neach x in h { str(x) }\n\
         adjacent := 0\n\
         each i in range(1, len(h)) { adjacent = adjacent + 1 when h[i] - h[i - 1] == 1 or h[i - 1] - h[i] == 1 }\n\
         adjacent",
        "5",
    );
}

#[test]
fn block_local_from_staged_arith_is_popped_virtually() {
    // `v0`'s initializer is register arithmetic that never reaches memory; its
    // scope-end Pop used to pop memory instead, dropping `acc` beneath it.
    assert_all(
        "fun f(a, b) {\n  acc := []\n  option {\n    a > -5 -> {\n      v0 := (((-5 * b) - (a - a)) * a)\n    }\n  }\n  acc.push(24 - (b * a) + b)\n  acc\n}\n[f(3, 4), f(1, 7)]",
        "[[16], [24]]",
    );
}

#[test]
fn float_param_rejects_int_at_a_warm_inlined_call_site() {
    // The same call site first sees floats (so its IC records the inlinable
    // `x + y` callee), then an int, which `<float>` rejects.
    assert_all_fail(
        "fun mk() {\n    x := 10\n    fun(y <float>) { x + y }\n}\nadd := mk()\n\
         fun run(f) {\n    t := 0.0\n    i := 0\n    repeat when i < 100 {\n        v := option { i < 99 -> 1.5, 2 }\n        t = t + f(v)\n        i = i + 1\n    }\n    t\n}\nrun(add)",
        "type mismatch for parameter 'y'",
    );
}

#[test]
fn float_param_rejects_int_in_an_inlined_method() {
    assert_all_fail(
        "struct C {\n    n <float>\n}\nC includes {\n    fun bump(d <float>) { self.n = self.n + d }\n}\n\
         fun run(c) {\n    i := 0\n    repeat when i < 100 {\n        v := option { i < 99 -> 1.5, 2 }\n        c.bump(v)\n        i = i + 1\n    }\n    c.n\n}\nrun(C(0))",
        "type mismatch for parameter 'd'",
    );
}
