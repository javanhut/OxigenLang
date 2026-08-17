//! Regression: a struct imported from another module inherits its parent's
//! FIELDS, not just its methods.
//!
//! `resolve_field_layout` walked the inheritance chain with `get_struct_def`,
//! which only sees the active scope. `introduce {Child} from .lib` binds Child
//! but not Base, so at instantiation the parent lookup returned None, the chain
//! stopped at Child, and the instance got a layout with none of Base's fields.
//! Methods still resolved (`find_method_on_def` consults the DEFINING module's
//! globals), so the failure surfaced far from its cause: an inherited method
//! touching an inherited field died with "field 'x' not found on Child".
//!
//! Both halves are asserted: the parent resolves through the child's own module,
//! and a genuinely unresolvable parent errors at instantiation instead of
//! silently producing a struct missing every inherited field.

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::VM;

/// Base owns the field; Child adds none of its own — so an empty layout and a
/// correct one differ by exactly the inherited slot.
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
    // The exact failing shape: only Child is named in the import.
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
    // The workaround path must not regress now that the module lookup wins.
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
    // Dropping the parent silently yields an instance missing every inherited
    // field; the error has to land here, not at the first field access.
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
