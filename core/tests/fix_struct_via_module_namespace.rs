//! Regression: a struct is constructible through its module's namespace.
//!
//! `introduce .lib` then `lib.Pt(1, 2)` evaluated `lib.Pt` to the struct def,
//! but the constructor dropped the def and looked it up again by name with
//! `resolve_field_layout`, which only sees the active scope. The importer
//! never bound `Pt`, so the call died with "struct 'Pt' is not defined in this
//! scope" while `introduce {Pt} from .lib` worked.
//!
//! Construction through the namespace must behave exactly like construction
//! through a selective import: same fields, same checks, same values.
//!
//! The same goes for annotations: `<lib.Pt>` checks as `Pt`, and `p <lib.Pt>`
//! builds Pt's zero value through the module, refusing anything that isn't a
//! struct instead of calling it.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

const LIB: &str = "\
struct Pt {\n\
    x <int>\n\
    y <int>\n\
}\n\
\n\
Pt includes {\n\
    fun sum() { x + y }\n\
    fun moved(d <int>) { Pt(x + d, y + d) }\n\
}\n\
\n\
struct P3(Pt) {\n\
    z <int>\n\
}\n\
\n\
struct Secret {\n\
    hide v <int>\n\
}\n\
\n\
fun make() { Pt(1, 2) }\n\
\n\
enum Light { Red, Green }\n\
\n\
hide fun __helper() { 1 }\n";

fn run_with_lib(main_src: &str, tag: &str) -> String {
    let dir = std::env::temp_dir().join(format!("oxi_ns_struct_{}_{}", std::process::id(), tag));
    std::fs::create_dir_all(&dir).expect("mk tempdir");
    std::fs::write(dir.join("lib.oxi"), LIB).expect("write module");

    let source = main_src.to_string();
    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer, &source);
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

    let mut vm = VM::new();
    vm.set_source(&source);
    vm.set_file(dir.join("main.oxi"));
    let out = vm
        .run(function)
        .map(|v| format!("{}", v))
        .unwrap_or_else(|e| format!("ERR: {}", e.message));
    let _ = std::fs::remove_dir_all(&dir);
    out
}

#[test]
fn positional_and_named_construction_through_namespace() {
    let out = run_with_lib("introduce .lib\nlib.Pt(1, 2)\n", "positional");
    assert_eq!(out, "Pt { x: 1, y: 2 }");
    let out = run_with_lib("introduce .lib\nlib.Pt(y=5, x=1).sum()\n", "named");
    assert_eq!(out, "6");
}

#[test]
fn methods_on_a_namespace_built_instance_construct_their_own_struct() {
    let out = run_with_lib("introduce .lib\nlib.Pt(1, 2).moved(1)\n", "method");
    assert_eq!(out, "Pt { x: 2, y: 3 }");
}

#[test]
fn namespace_and_selective_construction_agree() {
    let out = run_with_lib(
        "introduce .lib\nintroduce {Pt} from .lib\nlib.Pt(1, 2) == Pt(1, 2) and lib.make() == lib.Pt(1, 2)\n",
        "agree",
    );
    assert_eq!(out, "True");
}

#[test]
fn child_built_through_namespace_inherits_parent_fields() {
    let out = run_with_lib("introduce .lib\nlib.P3(1, 2, 3).sum()\n", "child");
    assert_eq!(out, "3");
}

#[test]
fn field_checks_still_apply_through_namespace() {
    let out = run_with_lib("introduce .lib\nlib.Pt(1)\n", "missing");
    assert!(out.contains("Pt is missing field `y`"), "got: {out}");
    let out = run_with_lib("introduce .lib\nlib.Pt(\"a\", 2)\n", "type");
    assert!(out.contains("field 'x' of Pt"), "got: {out}");
    let out = run_with_lib("introduce .lib\nlib.Secret(v=1)\n", "hidden");
    assert!(
        out.contains("field 'v' is hidden on Secret and cannot be set from outside"),
        "got: {out}"
    );
}

#[test]
fn qualified_zero_value_declaration() {
    let out = run_with_lib("introduce .lib\np <lib.Pt>\np.x = 4\np.sum()\n", "zero");
    assert_eq!(out, "4");
    let out = run_with_lib("introduce .lib\np <lib.P3>\np\n", "zero_child");
    assert_eq!(out, "P3 { x: 0, y: 0, z: 0 }");
    let out = run_with_lib(
        "introduce .lib\nfun mk() {\n    q <lib.Pt>\n    q\n}\nmk()\n",
        "zero_local",
    );
    assert_eq!(out, "Pt { x: 0, y: 0 }");
}

#[test]
fn qualified_annotations_check_by_name() {
    let out = run_with_lib(
        "introduce .lib\nfun f(p <lib.Pt>) { p.sum() }\nf(lib.Pt(3, 4))\n",
        "param",
    );
    assert_eq!(out, "7");
    let out = run_with_lib(
        "introduce .lib\nfun f(p <lib.Pt>) { p }\nf(5)\n",
        "param_bad",
    );
    assert!(out.contains("expected Pt, got INTEGER"), "got: {out}");
    let out = run_with_lib(
        "introduce .lib\nfun f(p <lib.Pt || int>) { p }\nf(5)\n",
        "union",
    );
    assert_eq!(out, "5");
    let out = run_with_lib(
        "introduce .lib\nstruct Line {\n    a <lib.Pt>\n}\nLine(5)\n",
        "field",
    );
    assert!(out.contains("field 'a' of Line is Pt"), "got: {out}");
    let out = run_with_lib(
        "introduce .lib\nl <lib.Light> := lib.Light.Green\nl\n",
        "enum",
    );
    assert_eq!(out, "Light.Green");
}

#[test]
fn qualified_zero_value_refuses_non_structs() {
    let cases = [
        ("l <lib.Light>", "lib.Light is an enum, not a struct"),
        ("f <lib.make>", "lib.make is a function, not a struct"),
        ("f <lib.Nope>", "has no member 'Nope'"),
        ("f <lib.__helper>", "'__helper' is hidden inside module"),
        (
            "p <other.Pt>",
            "'other' in <other.Pt> is not an imported module",
        ),
    ];
    for (i, (decl, expected)) in cases.iter().enumerate() {
        let out = run_with_lib(&format!("introduce .lib\n{decl}\n"), &format!("refuse{i}"));
        assert!(out.contains(expected), "{decl}: got {out}");
    }
}

#[test]
fn bare_zero_value_refuses_non_structs() {
    // `p <make>` used to call make() and bind its result.
    let cases = [
        (
            "fun make() { 5 }\np <make>\np\n",
            "make is a function, not a struct",
        ),
        (
            "enum Light { Red }\nl <Light>\n",
            "Light is an enum, not a struct",
        ),
        ("x := 3\np <x>\n", "x is an integer, not a struct"),
        ("p <Nope>\n", "struct 'Nope' is not defined in this scope"),
    ];
    for (i, (src, expected)) in cases.iter().enumerate() {
        let out = run_with_lib(src, &format!("bare_refuse{i}"));
        assert!(out.contains(expected), "{src}: got {out}");
    }
}

#[test]
fn bare_zero_value_still_builds_structs() {
    let out = run_with_lib(
        "introduce {P3} from .lib\nstruct Q {\n    a <int>\n}\nfun f() {\n    q <Q>\n    q.a = 2\n    q\n}\nc <P3>\n[f(), c]\n",
        "bare_ok",
    );
    assert_eq!(out, "[Q { a: 2 }, P3 { x: 0, y: 0, z: 0 }]");
}

#[test]
fn brace_literal_through_namespace() {
    // `lib.Pt { ... }` parses like an enum struct variant; the VM builds the
    // struct when `lib` turns out to be a module.
    let out = run_with_lib(
        "introduce .lib\n[lib.Pt { y: 2, x: 1 }, lib.Pt {}, lib.P3 { x: 1, y: 2, z: 3 }]\n",
        "brace",
    );
    assert_eq!(
        out,
        "[Pt { x: 1, y: 2 }, Pt { x: 0, y: 0 }, P3 { x: 1, y: 2, z: 3 }]"
    );
    let out = run_with_lib(
        "introduce .lib\nintroduce {Pt} from .lib\nlib.Pt { x: 1, y: 2 } == Pt { x: 1, y: 2 }\n",
        "brace_agree",
    );
    assert_eq!(out, "True");
}

#[test]
fn brace_literal_through_namespace_keeps_its_checks() {
    let cases = [
        ("lib.Pt { x: 1 }", "Pt is missing field `y`"),
        ("lib.Pt { x: \"a\", y: 2 }", "field 'x' of Pt"),
        (
            "lib.Pt { x: 1, y: 2, w: 3 }",
            "field 'w' not declared on Pt",
        ),
        (
            "lib.Secret { v: 1 }",
            "field 'v' is hidden on Secret and cannot be set in a literal",
        ),
        ("lib.make { a: 1 }", "lib.make is a function, not a struct"),
        ("lib.Light { a: 1 }", "lib.Light is an enum, not a struct"),
    ];
    for (i, (expr, expected)) in cases.iter().enumerate() {
        let out = run_with_lib(
            &format!("introduce .lib\n{expr}\n"),
            &format!("brace_err{i}"),
        );
        assert!(out.contains(expected), "{expr}: got {out}");
    }
}
