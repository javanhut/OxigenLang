//! `vec2`, `vec3` and `vec4`: small vectors of `f32` components, the
//! currency of game maths. Scripts that model them as structs pay for a
//! struct per value and per intermediate result; these are a single value.
//!
//! Vectors are values: operators make new vectors, and components are read,
//! never assigned.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

fn run_on(mut vm: VM, source: &str) -> Result<String, String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, source);
    let program = parser.parse_program();
    assert!(parser.errors().is_empty(), "{}", parser.format_errors());
    let function = Compiler::new()
        .compile(&program)
        .unwrap_or_else(|errs| panic!("compile errors: {errs:?}"));
    vm.run(function)
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

fn backends() -> [(&'static str, VM); 2] {
    [
        ("interpreter", VM::new_interpreter()),
        ("jit", VM::new_eager_jit()),
    ]
}

fn assert_yields(source: &str, expected: &str) {
    for (name, vm) in backends() {
        let got = run_on(vm, source).unwrap_or_else(|e| panic!("{name} failed on {source}: {e}"));
        assert_eq!(got, expected, "{name}: {source}");
    }
}

fn assert_true(source: &str) {
    assert_yields(source, "True");
}

fn assert_rejects(source: &str, needle: &str) {
    for (name, vm) in backends() {
        let err = run_on(vm, source).expect_err(&format!("{name} should reject: {source}"));
        assert!(
            err.contains(needle),
            "{name}: expected {needle:?} in: {err}"
        );
    }
}

// ── Construction and display ───────────────────────────────────────────

#[test]
fn vectors_display_their_components() {
    assert_yields("vec2(1, 2)", "vec2(1, 2)");
    assert_yields("vec3(1, 2.5, -3)", "vec3(1, 2.5, -3)");
    assert_yields("vec4(0, 0.25, 0, 1)", "vec4(0, 0.25, 0, 1)");
}

#[test]
fn vectors_report_their_type() {
    assert_yields("type(vec2(0, 0))", "VEC2");
    assert_yields("type(vec3(0, 0, 0))", "VEC3");
    assert_yields("type(vec4(0, 0, 0, 0))", "VEC4");
}

#[test]
fn constructors_need_the_right_count_of_numbers() {
    assert_rejects("vec3(1, 2)", "vec3 takes 3 numbers");
    assert_rejects("vec2(1, 2, 3)", "vec2 takes 2 numbers");
    assert_rejects("vec3(1, \"two\", 3)", "vec3 takes 3 numbers");
}

// ── Components ─────────────────────────────────────────────────────────

#[test]
fn components_read_back_as_floats() {
    assert_true("v := vec3(1, 2, 3)\nv.x == 1.0 and v.y == 2.0 and v.z == 3.0");
    assert_true("v := vec4(1, 2, 3, 4)\nv.w == 4.0");
    assert_yields("type(vec2(1, 2).x)", "FLOAT");
}

#[test]
fn a_missing_component_is_an_error() {
    assert_rejects("vec3(1, 2, 3).w", "vec3 has no component w");
    assert_rejects("vec2(1, 2).z", "vec2 has no component z");
    assert_rejects("vec2(1, 2).length", "vec2 has no component length");
}

#[test]
fn components_cannot_be_assigned() {
    assert_rejects("v := vec2(1, 2)\nv.x = 5\nv", "vectors are values");
}

// ── Operators ──────────────────────────────────────────────────────────

#[test]
fn vectors_add_and_subtract_componentwise() {
    assert_true("vec2(1, 2) + vec2(3, 4) == vec2(4, 6)");
    assert_true("vec3(5, 5, 5) - vec3(1, 2, 3) == vec3(4, 3, 2)");
    assert_true("vec4(1, 1, 1, 1) + vec4(0, 1, 2, 3) == vec4(1, 2, 3, 4)");
}

#[test]
fn vectors_scale_by_a_number_on_either_side() {
    assert_true("vec3(1, 2, 3) * 2 == vec3(2, 4, 6)");
    assert_true("2 * vec3(1, 2, 3) == vec3(2, 4, 6)");
    assert_true("vec2(1, 2) * 0.5 == vec2(0.5, 1)");
    assert_true("vec2(2, 4) / 2 == vec2(1, 2)");
}

#[test]
fn vectors_multiply_componentwise() {
    assert_true("vec3(1, 2, 3) * vec3(4, 5, 6) == vec3(4, 10, 18)");
}

#[test]
fn a_vector_negates() {
    assert_true("-vec2(1, -2) == vec2(-1, 2)");
}

#[test]
fn equality_needs_the_same_size_and_components() {
    assert_true("vec2(1, 2) == vec2(1, 2)");
    assert_true("vec2(1, 2) != vec2(2, 1)");
    assert_true("vec2(0, 0) != vec3(0, 0, 0)");
}

#[test]
fn mixing_sizes_is_an_error() {
    assert_rejects("vec2(1, 2) + vec3(1, 2, 3)", "VEC2 + VEC3");
    assert_rejects("vec2(1, 2) * vec4(1, 2, 3, 4)", "VEC2 * VEC4");
}

#[test]
fn dividing_a_number_by_a_vector_is_an_error() {
    assert_rejects("2 / vec2(1, 2)", "VEC2");
}

#[test]
fn vector_maths_in_a_hot_loop() {
    let src = "p := vec2(0, 0)\nv := vec2(1, 2)\ni := 0\nrepeat when i < 1000 {\n    p = p + v * 0.5\n    i++\n}\np == vec2(500, 1000)";
    assert_true(src);
}

// ── math functions ─────────────────────────────────────────────────────

#[test]
fn math_dot_and_cross() {
    assert_true("introduce math\nmath.dot(vec3(1, 2, 3), vec3(4, 5, 6)) == 32.0");
    assert_true("introduce math\nmath.dot(vec2(1, 0), vec2(0, 1)) == 0.0");
    assert_true("introduce math\nmath.cross(vec3(1, 0, 0), vec3(0, 1, 0)) == vec3(0, 0, 1)");
    assert_rejects(
        "introduce math\nmath.cross(vec2(1, 0), vec2(0, 1))",
        "cross needs two vec3",
    );
}

#[test]
fn math_length_distance_and_normalize() {
    assert_true("introduce math\nmath.length(vec2(3, 4)) == 5.0");
    assert_true("introduce math\nmath.distance(vec3(1, 1, 1), vec3(1, 4, 5)) == 5.0");
    assert_true("introduce math\nmath.normalize(vec2(3, 4)) == vec2(0.6, 0.8)");
    assert_rejects("introduce math\nmath.normalize(vec2(0, 0))", "zero-length");
}

#[test]
fn math_lerp_blends_vectors() {
    assert_true("introduce math\nmath.lerp(vec2(0, 0), vec2(10, 20), 0.5) == vec2(5, 10)");
}
