//! The interface a host (a game engine) uses to embed Oxigen.
//!
//! - **Native functions** are Rust closures, so they can reach the host's
//!   state. Builtins are plain `fn` pointers and cannot.
//! - **Host values** carry a Rust value (an entity handle, a texture id)
//!   through a script untouched, for natives to read back.
//! - **Capabilities** let the host decide what a script may reach outside
//!   the VM. A restricted VM refuses the builtins behind the missing ones,
//!   including inside the modules the script imports.
//! - **Module invalidation** drops a changed module, and every module that
//!   imported it, from the cache, so running the script again picks up the
//!   new code (hot reload).

use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use oxigen_core::compiler::Compiler;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::Parser;
use oxigen_core::vm::value::Value;
use oxigen_core::vm::{Capability, VM};

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

fn run_on(vm: &mut VM, source: &str) -> Result<String, String> {
    vm.run(compile(source))
        .map(|v| format!("{v}"))
        .map_err(|e| e.message)
}

fn backends() -> [(&'static str, VM); 2] {
    [
        ("interpreter", VM::new_interpreter()),
        ("jit", VM::new_eager_jit()),
    ]
}

/// A fresh, empty directory for one test's module files.
fn scratch_dir(test: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("oxigen-embed-{test}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn write(dir: &Path, file: &str, source: &str) {
    std::fs::write(dir.join(file), source).unwrap();
}

// ── Native functions ───────────────────────────────────────────────────

#[test]
fn a_native_function_can_change_host_state() {
    for (name, mut vm) in backends() {
        let score = Rc::new(RefCell::new(0i64));
        let seen = Rc::clone(&score);
        vm.register_native("add_score", move |args| match args {
            [Value::Integer(n)] => {
                *seen.borrow_mut() += n;
                Ok(Value::None)
            }
            _ => Err("add_score takes one int".to_string()),
        });
        run_on(&mut vm, "add_score(5)\nadd_score(7)\nNone")
            .unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(*score.borrow(), 12, "{name}");
    }
}

#[test]
fn a_native_function_returns_a_value() {
    for (name, mut vm) in backends() {
        vm.register_native("twice", |args| match args {
            [Value::Integer(n)] => Ok(Value::Integer(n * 2)),
            _ => Err("twice takes one int".to_string()),
        });
        let got = run_on(&mut vm, "twice(21)").unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(got, "42", "{name}");
    }
}

#[test]
fn a_native_function_is_called_from_a_hot_loop() {
    for (name, mut vm) in backends() {
        let calls = Rc::new(RefCell::new(0));
        let count = Rc::clone(&calls);
        vm.register_native("tick", move |_| {
            *count.borrow_mut() += 1;
            Ok(Value::None)
        });
        run_on(
            &mut vm,
            "i := 0\nrepeat when i < 1000 {\n    tick()\n    i++\n}\ni",
        )
        .unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(*calls.borrow(), 1000, "{name}");
    }
}

#[test]
fn a_native_error_is_a_script_error_the_script_can_catch() {
    for (name, mut vm) in backends() {
        vm.register_native("deny", |_| Err("the host said no".to_string()));
        let err = run_on(&mut vm, "deny()").expect_err(name);
        assert!(err.contains("the host said no"), "{name}: got {err}");
        let got = run_on(&mut vm, "x := deny() <guard>(\"fallback\")\nx")
            .unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(got, "fallback", "{name}");
    }
}

#[test]
fn a_native_function_can_be_passed_around_like_any_function() {
    for (name, mut vm) in backends() {
        vm.register_native("inc", |args| match args {
            [Value::Integer(n)] => Ok(Value::Integer(n + 1)),
            _ => Err("inc takes one int".to_string()),
        });
        let got = run_on(&mut vm, "fun apply(f, x) { f(x) }\napply(inc, 1)")
            .unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(got, "2", "{name}");
        let got = run_on(&mut vm, "type(inc)").unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(got, "BUILTIN", "{name}");
    }
}

#[test]
fn imported_modules_see_native_functions() {
    let dir = scratch_dir("native-module");
    write(&dir, "helper.oxi", "fun doubled(x) { twice(x) }\n");
    for (name, mut vm) in backends() {
        vm.set_file(dir.join("main.oxi"));
        vm.register_native("twice", |args| match args {
            [Value::Integer(n)] => Ok(Value::Integer(n * 2)),
            _ => Err("twice takes one int".to_string()),
        });
        let got = run_on(&mut vm, "introduce .helper\nhelper.doubled(4)")
            .unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(got, "8", "{name}");
    }
}

// ── Host values ────────────────────────────────────────────────────────

#[derive(Debug, PartialEq)]
struct Entity(u32);

fn entity_natives(vm: &mut VM) {
    vm.register_native("spawn", |args| match args {
        [Value::Integer(id)] => Ok(Value::host("Entity", Entity(*id as u32))),
        _ => Err("spawn takes one int".to_string()),
    });
    vm.register_native("entity_id", |args| match args {
        [v] => match v.host_ref::<Entity>() {
            Some(e) => Ok(Value::Integer(e.0 as i64)),
            None => Err("not an Entity".to_string()),
        },
        _ => Err("entity_id takes one entity".to_string()),
    });
}

#[test]
fn a_host_value_round_trips_through_a_script() {
    for (name, mut vm) in backends() {
        entity_natives(&mut vm);
        let got = run_on(&mut vm, "e := spawn(7)\nentity_id(e)")
            .unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(got, "7", "{name}");
    }
}

#[test]
fn a_host_value_survives_storage_in_collections_and_structs() {
    for (name, mut vm) in backends() {
        entity_natives(&mut vm);
        let src = "struct Holder {\n    thing <generic>\n}\nh := Holder { thing: spawn(3) }\nlist := [spawn(1), h.thing]\nentity_id(list[1])";
        let got = run_on(&mut vm, src).unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(got, "3", "{name}");
    }
}

#[test]
fn a_host_value_reports_its_type_name() {
    for (name, mut vm) in backends() {
        entity_natives(&mut vm);
        let got = run_on(&mut vm, "type(spawn(1))").unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(got, "Entity", "{name}");
    }
}

#[test]
fn host_values_compare_by_their_rust_value() {
    for (name, mut vm) in backends() {
        entity_natives(&mut vm);
        let got = run_on(&mut vm, "spawn(4) == spawn(4)").unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(got, "True", "{name}");
        let got = run_on(&mut vm, "spawn(4) == spawn(5)").unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(got, "False", "{name}");
    }
}

#[test]
fn a_native_rejects_a_host_value_of_another_type() {
    #[derive(PartialEq)]
    struct Texture;
    for (name, mut vm) in backends() {
        entity_natives(&mut vm);
        vm.register_native("texture", |_| Ok(Value::host("Texture", Texture)));
        let err = run_on(&mut vm, "entity_id(texture())").expect_err(name);
        assert!(err.contains("not an Entity"), "{name}: got {err}");
    }
}

#[test]
fn the_host_reads_a_host_value_the_script_returns() {
    let mut vm = VM::new_interpreter();
    entity_natives(&mut vm);
    let value = vm.run(compile("spawn(9)")).unwrap();
    assert_eq!(value.host_ref::<Entity>(), Some(&Entity(9)));
}

// ── Calling into the script ────────────────────────────────────────────

#[test]
fn the_host_calls_a_script_function_by_name_every_frame() {
    for (name, mut vm) in backends() {
        run_on(&mut vm, "state := {\"total\": 0}\nfun on_update(dt) {\n    state[\"total\"] = state[\"total\"] + dt\n    state[\"total\"]\n}\nNone")
            .unwrap_or_else(|e| panic!("{name}: {e}"));
        let on_update = vm.get_global("on_update").expect("on_update is defined");
        let mut last = Value::None;
        for _ in 0..100 {
            last = vm
                .call_with_args(on_update.clone(), vec![Value::Integer(2)])
                .unwrap_or_else(|e| panic!("{name}: {}", e.message));
        }
        assert_eq!(format!("{last}"), "200", "{name}");
    }
}

#[test]
fn a_failed_call_leaves_the_vm_usable() {
    for (name, mut vm) in backends() {
        run_on(&mut vm, "fun broken() { 1 / 0 }\nfun fine() { 5 }\nNone")
            .unwrap_or_else(|e| panic!("{name}: {e}"));
        let broken = vm.get_global("broken").unwrap();
        let fine = vm.get_global("fine").unwrap();
        assert!(vm.call_with_args(broken, vec![]).is_err(), "{name}");
        let got = vm
            .call_with_args(fine, vec![])
            .unwrap_or_else(|e| panic!("{name}: {}", e.message));
        assert_eq!(format!("{got}"), "5", "{name}");
    }
}

// ── Capabilities ───────────────────────────────────────────────────────

fn restricted(name: &str, allowed: &[Capability]) -> VM {
    let mut vm = match name {
        "interpreter" => VM::new_interpreter(),
        _ => VM::new_eager_jit(),
    };
    vm.restrict(allowed);
    vm
}

fn assert_refused(vm: &mut VM, source: &str, capability: &str) {
    let err = run_on(vm, source).expect_err(&format!("should be refused: {source}"));
    assert!(
        err.contains(capability),
        "expected the {capability:?} capability in: {err}"
    );
}

#[test]
fn a_restricted_vm_refuses_the_filesystem() {
    for name in ["interpreter", "jit"] {
        let mut vm = restricted(name, &[]);
        assert_refused(
            &mut vm,
            "introduce io\nio.read_file(\"/etc/hostname\")",
            "filesystem",
        );
        assert_refused(
            &mut vm,
            "introduce io\nio.write_file(\"/tmp/x\", \"y\")",
            "filesystem",
        );
        assert_refused(&mut vm, "introduce os\nos.list_dir(\"/\")", "filesystem");
    }
}

#[test]
fn a_restricted_vm_refuses_the_network() {
    for name in ["interpreter", "jit"] {
        let mut vm = restricted(name, &[]);
        assert_refused(&mut vm, "__net_tcp_connect(\"127.0.0.1\", 9)", "network");
        assert_refused(
            &mut vm,
            "__http_request(\"GET\", \"http://127.0.0.1:9/\", {}, \"\")",
            "network",
        );
    }
}

#[test]
fn a_restricted_vm_refuses_processes_and_the_environment() {
    for name in ["interpreter", "jit"] {
        let mut vm = restricted(name, &[]);
        assert_refused(&mut vm, "__exec(\"true\")", "process");
        assert_refused(&mut vm, "__env_get(\"HOME\")", "process");
        assert_refused(&mut vm, "__chdir(\"/\")", "process");
        // `__exit` would end the host; it must never run.
        assert_refused(&mut vm, "__exit(3)", "process");
    }
}

#[test]
fn a_restricted_vm_refuses_threads_stdin_and_sleep() {
    for name in ["interpreter", "jit"] {
        let mut vm = restricted(name, &[]);
        assert_refused(&mut vm, "__spawn(len, [1])", "threads");
        assert_refused(&mut vm, "__read_line()", "stdin");
        assert_refused(&mut vm, "__time_sleep(1)", "sleep");
    }
}

#[test]
fn a_restricted_vm_keeps_pure_builtins_and_stdlib() {
    for name in ["interpreter", "jit"] {
        let mut vm = restricted(name, &[]);
        let got = run_on(&mut vm, "introduce math\nintroduce strings\nlen([1, 2, 3]) + math.floor(2.5) + len(strings.upper(\"ab\"))")
            .unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(got, "7", "{name}");
    }
}

#[test]
fn a_capability_that_is_allowed_still_works() {
    for name in ["interpreter", "jit"] {
        let mut vm = restricted(name, &[Capability::FileSystem]);
        let got = run_on(&mut vm, "introduce io\nio.file_exists(\"/\")")
            .unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(got, "True", "{name}");
        assert_refused(&mut vm, "__env_get(\"HOME\")", "process");
    }
}

#[test]
fn an_imported_module_cannot_escape_the_restriction() {
    let dir = scratch_dir("restricted-module");
    write(&dir, "sneaky.oxi", "stolen := __env_get(\"HOME\")\n");
    for name in ["interpreter", "jit"] {
        let mut vm = restricted(name, &[]);
        vm.set_file(dir.join("main.oxi"));
        assert_refused(&mut vm, "introduce .sneaky\nsneaky.stolen", "process");
    }
}

#[test]
fn the_script_cannot_rebind_a_refused_builtin_to_get_it_back() {
    for name in ["interpreter", "jit"] {
        let mut vm = restricted(name, &[]);
        assert_refused(&mut vm, "f := __env_get\nf(\"HOME\")", "process");
    }
}

// ── Hot reload ─────────────────────────────────────────────────────────

const MAIN: &str = "introduce .m\nm.value()";

#[test]
fn a_cached_module_is_not_reread() {
    let dir = scratch_dir("cached");
    write(&dir, "m.oxi", "fun value() { 1 }\n");
    for (name, mut vm) in backends() {
        vm.set_file(dir.join("main.oxi"));
        assert_eq!(run_on(&mut vm, MAIN).unwrap(), "1", "{name}");
        write(&dir, "m.oxi", "fun value() { 2 }\n");
        assert_eq!(
            run_on(&mut vm, MAIN).unwrap(),
            "1",
            "{name}: the cache is in use"
        );
        write(&dir, "m.oxi", "fun value() { 1 }\n");
    }
}

#[test]
fn invalidating_a_module_reloads_it() {
    let dir = scratch_dir("reload");
    for (name, mut vm) in backends() {
        write(&dir, "m.oxi", "fun value() { 1 }\n");
        vm.set_file(dir.join("main.oxi"));
        assert_eq!(run_on(&mut vm, MAIN).unwrap(), "1", "{name}");

        write(&dir, "m.oxi", "fun value() { 2 }\n");
        assert_eq!(vm.invalidate_module(&dir.join("m.oxi")), 1, "{name}");
        assert_eq!(run_on(&mut vm, MAIN).unwrap(), "2", "{name}");
    }
}

#[test]
fn invalidating_a_module_reloads_the_modules_that_imported_it() {
    let dir = scratch_dir("reload-chain");
    for (name, mut vm) in backends() {
        write(&dir, "b.oxi", "fun value() { 1 }\n");
        write(
            &dir,
            "a.oxi",
            "introduce .b\nfun value() { b.value() * 10 }\n",
        );
        vm.set_file(dir.join("main.oxi"));
        let main = "introduce .a\na.value()";
        assert_eq!(run_on(&mut vm, main).unwrap(), "10", "{name}");

        write(&dir, "b.oxi", "fun value() { 2 }\n");
        assert_eq!(
            vm.invalidate_module(&dir.join("b.oxi")),
            1,
            "{name}: a.oxi goes too"
        );
        assert_eq!(run_on(&mut vm, main).unwrap(), "20", "{name}");
    }
}

#[test]
fn invalidating_a_module_that_was_never_loaded_does_nothing() {
    let dir = scratch_dir("never");
    let mut vm = VM::new_interpreter();
    assert_eq!(vm.invalidate_module(&dir.join("nothing.oxi")), 0);
}

#[test]
fn a_reload_with_a_syntax_error_keeps_the_vm_usable() {
    let dir = scratch_dir("reload-broken");
    for (name, mut vm) in backends() {
        write(&dir, "m.oxi", "fun value() { 1 }\n");
        vm.set_file(dir.join("main.oxi"));
        assert_eq!(run_on(&mut vm, MAIN).unwrap(), "1", "{name}");

        write(&dir, "m.oxi", "fun value( { 2 }\n");
        vm.invalidate_module(&dir.join("m.oxi"));
        assert!(
            run_on(&mut vm, MAIN).is_err(),
            "{name}: the broken module fails"
        );

        write(&dir, "m.oxi", "fun value() { 3 }\n");
        vm.invalidate_module(&dir.join("m.oxi"));
        assert_eq!(
            run_on(&mut vm, MAIN).unwrap(),
            "3",
            "{name}: and recovers once fixed"
        );
    }
}
