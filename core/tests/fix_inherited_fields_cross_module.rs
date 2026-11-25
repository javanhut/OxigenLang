//! Tests that a struct imported from another module inherits its parent's
//! fields, not just its methods.
//!
//! The parent resolves through the child's own module, and an unresolvable
//! parent errors at instantiation.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Base owns the field; Child adds none of its own.
const LIB: &str = "\
struct Base {\n\
    hide x <array>\n\
}\n\
\n\
Base includes {\n\
    fun count() { len(self.x) }\n\
}\n\
\n\
struct Child(Base) {\n\
}\n\
\n\
Child includes {\n\
    fun add(v) { self.x = push(self.x, v) }\n\
}\n";

fn run_with_lib(main_src: &str, tag: &str) -> String {
    let dir = std::env::temp_dir().join(format!("oxi_inherit_{}_{}", std::process::id(), tag));
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
fn child_imported_without_parent_still_has_parent_fields() {
    let out = run_with_lib(
        "introduce {Child} from .lib\n\
         c <Child>\n\
         c.add(\"a\")\n\
         c.add(\"b\")\n\
         c.count()\n",
        "child_only",
    );
    assert_eq!(
        out, "2",
        "an imported child must inherit its parent's fields even when the importer never names the parent"
    );
}

#[test]
fn importing_the_parent_too_still_works() {
    let out = run_with_lib(
        "introduce {Base, Child} from .lib\n\
         c <Child>\n\
         c.add(\"a\")\n\
         c.count()\n",
        "both",
    );
    assert_eq!(out, "1", "naming the parent as well must keep working");
}

#[test]
fn unresolvable_parent_errors_at_instantiation() {
    let out = run_with_lib(
        "struct Orphan(NoSuchBase) {\n\
             y <str>\n\
         }\n\
         o <Orphan>\n\
         o\n",
        "orphan",
    );
    assert!(
        out.contains("parent struct 'NoSuchBase' of 'Orphan' is not defined"),
        "an unresolvable parent must fail loudly at instantiation, got: {out}"
    );
}
