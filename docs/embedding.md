# Embedding Oxigen

Oxigen can run inside a Rust program, such as a game engine running gameplay
scripts. The host compiles a script, runs it on a `VM`, and then calls the
script's functions whenever it needs to: once per frame, on an event, and so
on. This page covers what the host can do with the VM.

```rust
use oxigen_core::{compiler::Compiler, lexer::Lexer, parser::Parser};
use oxigen_core::vm::{value::Value, VM};

let source = std::fs::read_to_string("game.oxi")?;
let mut parser = Parser::new(Lexer::new(&source), &source);
let program = parser.parse_program();
let function = Compiler::new().compile(&program).expect("compiles");

let mut vm = VM::new();
vm.set_file("game.oxi".into()); // so `introduce .module` resolves
vm.run(function)?;               // defines the script's functions

let on_update = vm.get_global("on_update").expect("defined");
vm.call_with_args(on_update, vec![Value::Float(1.0 / 60.0)])?;
```

A failed `run` or `call_with_args` returns the error and leaves the VM ready
for the next call.

## Native functions

`register_native` makes a Rust closure callable from scripts. Unlike the
built-in functions, a native is a closure, so it can hold the host's state:

```rust
let score = Rc::new(RefCell::new(0));
let seen = Rc::clone(&score);
vm.register_native("add_score", move |args| match args {
    [Value::Integer(n)] => {
        *seen.borrow_mut() += n;
        Ok(Value::None)
    }
    _ => Err("add_score takes one int".to_string()),
});
```

The script calls it like any function: `add_score(10)`. An `Err` becomes a
runtime error in the script, which the script can catch with `<guard>`.
Modules the script imports can call natives too.

To return a collection, build it with `Value::array(vec![...])` or
`Value::tuple(vec![...])`. Their elements are held in a `ValueVec`, which
derefs to `Vec<Value>` (so `a.borrow().len()` and `a.borrow_mut().push(v)`
work as before), and which frees deeply nested values without overflowing
your thread's stack. A raw `Vec<Value>` converts with `.into()`.

## Host values

`Value::host` wraps any Rust value that implements `PartialEq`, such as an
entity handle, so it can pass through a script untouched:

```rust
#[derive(PartialEq)]
struct Entity(u32);

vm.register_native("spawn", |args| match args {
    [Value::Integer(id)] => Ok(Value::host("Entity", Entity(*id as u32))),
    _ => Err("spawn takes one int".to_string()),
});
vm.register_native("entity_id", |args| match args.first().and_then(|v| v.host_ref::<Entity>()) {
    Some(e) => Ok(Value::Integer(e.0 as i64)),
    None => Err("not an Entity".to_string()),
});
```

A script can store a host value in variables, collections and struct fields,
and compare two with `==`, which compares the Rust values. `type()` reports
the name given to `Value::host`. Only the host can look inside, with
`host_ref::<T>()`.

## Capabilities

A new VM can do everything the `oxigen` command can, including reading files,
opening sockets and running programs. A host that runs scripts it does not
trust, a game loading mods for example, restricts it:

```rust
use oxigen_core::vm::Capability;

vm.restrict(&[]);                         // nothing outside the VM
vm.restrict(&[Capability::FileSystem]);   // files, but nothing else
```

| Capability | What it allows |
|---|---|
| `FileSystem` | Reading and writing files and directories (`io`, most of `os`) |
| `Network` | Sockets and HTTP (`net`, `api`); `net.download`, `net.upload` and `net.send_file` touch local files, so they need `FileSystem` too |
| `Process` | Running programs (`os.exec`, `os.spawn`), signals (`os.signals`), environment variables, the working directory, exiting |
| `Stdin` | Reading from standard input, which blocks until the user types |
| `Threads` | `diverge`/`converge`, and `channel` |
| `Sleep` | `time.sleep`, which blocks the thread running the script |
| `Ffi` | Loading C libraries and calling them (`ffi`). C code can do anything the process can, so never grant it to a script you would not run natively |

A script that calls `os.spawn` or `os.signals` installs signal handlers for
SIGINT, SIGTERM and SIGHUP (and any signal it subscribes to) in the host
process, plus an `atexit` hook that stops the children it started. A host
that handles those signals itself should leave `Process` out.

A script with `Ffi` can call any C function in the process, including
everything the other capabilities guard, so allowing it amounts to allowing
everything. A host that wants scripts to reach a particular C library safely
should wrap that library in `register_native` functions instead, and leave
`Ffi` out. See [FFI architecture](FFI_ARCHITECTURE.md#safety-and-sandboxing).

The language, the pure built-ins and the stdlib modules built on them keep
working. A call that needs a missing capability fails with an error naming
it. Modules the script imports run under the same restriction, and so do the
tasks it spawns, when `Threads` is allowed.

## Limits

A script with an endless loop would freeze the host. Two limits stop one, and
neither can be caught by the script:

- `vm.set_loop_budget(Some(n))` allows `n` more loop iterations, across runs
  and calls, until it is set again. Running out is a `loop budget exhausted`
  error. A host calling a script every frame sets the budget before each
  call. `None` removes the limit.
- `vm.interrupt_handle()` returns a handle that can be sent to another thread.
  Calling `interrupt()` on it stops the running script with an `interrupted`
  error at its next loop iteration.

Both apply to code the JIT has compiled and to imported modules. Unbounded
recursion needs neither: it ends in a `stack overflow` error.

## Hot reload

Imported modules are cached: importing one again reuses it. When a module's
file changes, invalidate it and run the script again:

```rust
vm.invalidate_module(Path::new("scripts/enemy.oxi"));
vm.run(compile(&main_source))?;   // imports enemy.oxi afresh
```

`invalidate_module` also drops every cached module that imported the changed
file, directly or not, and returns how many it dropped. Look functions up
again with `get_global` after a reload, since the old values still run the
old code. Keep game state in the host (or in a module that is not reloaded),
and a reload has nothing to migrate.

## Threads

A `VM` belongs to the thread that made it: its values are reference counted
without atomics, so it cannot be sent to another thread. Use one VM per
thread. The interrupt handle is the exception and can be sent anywhere.

`diverge`/`converge` run tasks on a worker pool shared by the whole process.
Each task runs on a fresh VM built from the declarations of the program that
spawned it, so tell the pool which program that is before running it:
`oxigen_core::concurrent::register_program(&function, &program, source, file,
false)` (pass `true` to keep the previously registered program's declarations
too, as the REPL does line by line). The last program registered is the one
new tasks run against, process-wide, so register and run one program at a
time; functions from imported modules find their module on their own. `set_src` still works for a host with one program per process. Host
natives and host values are not available inside spawned tasks.
