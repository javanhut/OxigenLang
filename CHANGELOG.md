# Changelog

## Unreleased

### Added: adding, removing and searching in collections

Arrays, maps and sets now share one set of builtins, all of which change the
collection in place:

- `push(set, x)` adds to a set, as it already appended to an array.
- `insert(set, x)` takes two arguments, and `insert(arr, index, x)` puts an
  element at a position (negative indices count from the end).
- `remove(arr, x)` removes the first element equal to `x`.
- `pop` removes one entry and returns it: `pop(arr)` the last element,
  `pop(arr, index)` the one at `index`, `pop(map, key)` the key's value
  (`None` if absent), and `pop(set)` the most recently added value.
- `extend(arr|set, items)` adds every element of an array, tuple or set;
  `extend(map, other)` copies in another map's entries.
- `clear(collection)` empties an array, map or set.
- `copy(collection)` makes a separate (shallow) copy, since `b := a` shares
  one collection between two names.
- `has(str, sub)` finds a substring or character.
- `set(arr)` makes a set of the array's elements.

A new `sets` module does set algebra: `union`, `intersection`, `difference`,
`symmetric_difference`, `is_subset`, `is_superset`, `is_disjoint`,
`to_array`, `filter` and `map`.

### Added: the `matrix` module

`matrix` builds 2-D matrices with `full`, `zeros`, `ones`, `identity`,
`rand_int`, `rand_float` and `from_array`. A `Matrix` has `get`, `set`, `row`,
`col`, `to_array`, `map`, `add`, `sub`, `scale`, `mul` (the matrix product),
`transpose` and `print`. `matrix.Matrix(rows, cols)` makes one from its shape
alone (its `data` is optional and reads as zeros), and `m.full`, `m.zeros`,
`m.ones`, `m.rand_int` and `m.rand_float` build a new one of the same shape.
A negative size or mismatched shapes fail with `<Error<shape>>`.

### Changed

- `insert(set, value, extra)` is an error. It used to need the third argument
  and ignore it; write `insert(set, value)` or `push(set, value)`.
- The docs for `insert` and `remove` said they returned a new map. They have
  always changed the map in place, and the docs now say so.
- A struct can be built through its module's namespace: after
  `introduce parse_args`, `parse_args.Parser()` works. It used to fail with
  *struct 'Parser' is not defined in this scope* unless the struct was also
  imported by name.
- The brace form works through a module too: `shapes.Pt { x: 1, y: 2 }`.
- A loop or `unless` with an empty body, `each x in items {}` or
  `repeat unless b.ok {}`, parses. The `{}` used to be read as an empty struct
  literal, which failed with *expected LBrace*.
- Type annotations can name a struct or enum through its module:
  `p <parse_args.Parser>`, `fun f(r <api.Res>)`. A declaration like
  `x <random.rand_int>` is refused with *random.rand_int is a function, not a
  struct*.
- `p <make>`, where `make` is a function, is an error: *make is a function,
  not a struct*. It used to call `make()` and bind the result. `l <Light>` on
  an enum now says *Light is an enum, not a struct* instead of *cannot call
  ENUM_DEF*.

### Added: background processes, signals and channels

- `os.spawn(prog, argv, capture)` starts a program and returns a handle at
  once. Talk to it with `os.write_stdin`, `os.close_stdin`,
  `os.read_output_line` and `os.read_output`; control it with `os.wait`
  (with an optional timeout), `os.kill`, `os.is_running` and
  `os.process_pid`. Children still running when the script ends, whether
  normally, through `os.exit`, or on Ctrl-C, SIGTERM or SIGHUP, are stopped.
- `os.signals(["INT", "TERM"])` delivers signals as messages on a channel.
  A subscribed signal no longer ends the program, so it can clean up first.
- `os.exec_argv(prog, argv, stdin)` sends text to the program's standard
  input, with no shell and no temporary file.
- A new `channel` module passes messages between `diverge` tasks in order:
  `new`, `send`, `recv` (with an optional timeout), `try_recv`, `close`,
  `len` and `is_closed`. Channels need the `threads` capability.

### Added: binary data

- `net.download` takes `method`, `headers` and `body`, so the response to a
  POST (audio from a text-to-speech API, say) streams to a file byte for
  byte. `net.post_to_file` is the POST shorthand.
- `io.read_bytes` and `io.write_bytes` read and write files as arrays of
  bytes.

### Added: tooling

- `oxigen check` now finds names nothing defines (`E0038`, an error) and
  logic that compiles but is almost certainly wrong (`E0039`, a warning):
  `x == "a" or "b"`, `("a" or "b")`, and `x != ("a" or "b")`. At a terminal it
  prints readable diagnostics; piped, or with `--json`, it prints JSON as
  before.
- `oxigen doc` shows the reference docs in the terminal: the module list,
  one module (`oxigen doc os`), one function (`oxigen doc os.exec_argv`, or
  just `oxigen doc exec_argv`), or `oxigen doc builtins`.
- Writing `return x`, `let x`, `def f`, `break`, `while` and other keywords
  from other languages now says what Oxigen calls them, instead of
  "expected a new line".
- `strings.index_of(s, sub, start)` searches from a start index.

### Added: calling C (`ffi`)

A new standard library module, `ffi`, calls C libraries from scripts. Declare
functions with their C signatures and call them like Oxigen functions:

```oxi
introduce ffi
libm := ffi.open("m", {"pow": "double(double x, double y)"})
libm.pow(2.0, 10.0)   // 1024
```

- Libraries by bare name (`"m"`, `"sqlite3"`), file name, path, or `None` for
  the C runtime.
- C type spellings in signatures (`const char *`, `size_t`, `unsigned long`,
  `enum X`, `T *`), plus short names (`i32`, `f64`, `ptr`).
- Variadic functions (`printf`, `snprintf`), structs by value
  (`ffi.cstruct`), arrays and maps passed to `T *` and copied back
  (out-parameters), and callbacks (`ffi.callback`) that can re-enter the VM.
- Memory helpers: `alloc`, `new`, `free`, `read`, `write`, `sizeof`, `string`
  and `cstring`.
- A new capability, `Capability::Ffi`. A host that calls `vm.restrict` must
  list it for scripts to use `ffi`.

Building from source now needs a C compiler and `make`, for the bundled
libffi. The prebuilt Linux binaries are statically linked against musl and
cannot load shared libraries, so `ffi.open` fails there; build from source to
use it on Linux. See [docs/stdlib/ffi.md](docs/stdlib/ffi.md) and
[docs/FFI_ARCHITECTURE.md](docs/FFI_ARCHITECTURE.md).

## 0.2.0

A correctness release. An adversarial review of the whole toolchain (front
end, compiler and VM, JIT, stdlib, concurrency, CLI, LSP and build scripts)
turned up about 100 confirmed defects, and this release fixes them. Every fix
comes with a regression test that was seen failing first.

Several fixes make OxigenLang reject code it used to accept, or change what a
stdlib function returns, so this is a minor release rather than a patch. Read
**Upgrading** before moving a project over.

### Upgrading

Code that ran on 0.1.4 can now stop with an error, or behave differently, in
these cases:

- **Two statements on one line need a separator.** `1.5e3` used to parse as
  `1.5` followed by a separate statement `e3`; it is now one number, `1500`.
- **Strings count characters, not bytes.** `len`, indexing and slicing agree
  with `each c in s`, so `len("héllo")` is `5`.
- **Mismatched unpacking is an error.** `a, b := 1, 2, 3`, `a, b, c := 1, 2`
  and `x, y := [1, 2, 3]` used to drop or pad values silently. `a, b = b, a`
  now swaps.
- **Types are enforced everywhere.** Struct field types, enum payload types,
  and writes through closures and multi-name assignment all check type locks
  and immutability. A lock belongs to the binding, so it can't be lost when
  scopes reuse a stack slot.
- **`range()` takes integers only**, in loops as well as the builtin. A float
  or string bound is an error, and an array built by `range()` is capped at
  100 million elements.
- **`uint(-1)` is an error.**
- **Circular and self imports raise `circular import`** instead of
  overflowing the stack. A module imported from several places runs once per
  program and shares its state.
- **`random.rand_int(min, max)` includes `max`.**
- **`api.serve` binds `127.0.0.1` by default.** Pass `host = "0.0.0.0"` to
  accept connections from other machines.
- **`api.file` and `static_dir` return a streamed file response**, so binary
  files and large files are served correctly.
- **The network builtins need `FileSystem` too.** In a restricted engine,
  `__net_download` and `__net_upload` require both the network and the
  `FileSystem` capability.

For hosts embedding `oxigen-core`:

- Call `concurrent::register_program` before running a compiled program that
  spawns tasks.
- Array and tuple contents are now `ValueVec`, which derefs to `Vec<Value>`.
  Build them with `Value::array(vec)`, `Value::tuple(vec)` or `vec.into()`.
- Release builds use `panic = "unwind"`. The test runner and the worker pool
  catch a panic and report it as a failed test or task instead of the process
  exiting.

### Fixed: crashes and miscompiles

- A plain script could segfault under the JIT. A named-argument call with
  surplus arguments misaligned the callee's frame, and the VM stack could be
  reallocated under a pointer the JIT held.
- JIT miscompiles:
  - string `out = out + w` raised `STRING * INTEGER`
  - comparisons returned `1` instead of `True`
  - shared upvalues went stale across closures
  - method caches used the wrong module's globals
  - the field-add fast path bypassed `hide`
  - typed parameters were unchecked, so a string could print as a heap address
- `diverge` inside a module could run the wrong function.
- Freeing a deeply nested value (`x = [x]` in a loop, a long struct linked
  list, an enum `Cons` chain) overflowed the stack. Drops past 64 levels now
  use a work list.
- `each i in range(...)` with a step past the largest integer wrapped around
  and looped practically forever. It now stops like the builtin does.
- Panics that killed the process:
  - slicing non-ASCII strings
  - sorting mixed types
  - a bad `datetime.format` pattern
  - an invalid `env_set` key
  - printing or comparing cyclic values
  - non-UTF-8 arguments
  - a few thousand nested `(`
- `oxigen fmt` could change what a program does or write a file that no
  longer parsed:
  - a single-statement `unless`/`when` block was folded into a postfix guard
  - a mid-line block expression in an `#[indent]` file swallowed the code
    after it
  - a NUL byte truncated the file
  - several other patterns

### Fixed: security and robustness

- `api` servers:
  - a JSON body of a few million `[` overflowed a worker and killed the server
  - `\r\n` in a query parameter could inject response headers
  - a client sending one byte at a time could hold a worker forever
  - chunked request bodies were mishandled
- Worker VMs are restricted before they run any module code, and network
  handle ids are no longer sequential.
- `restrict()` no longer leaves a module cached from before the restriction
  usable with its old builtins.

### Fixed: tools and runtime

- `oxigen test`: `os.exit(0)` inside a test no longer counts as a pass, and
  nested tests are no longer silently dropped.
- Concurrency:
  - each task runs on its own VM, so state no longer leaks between tasks
  - `cancel()` stops JIT-compiled loops and loops with no calls
  - deeply nested `diverge` no longer overflows the stack
  - `converge t within ms` remembers a failed task's error
- LSP: diagnostics are tracked per file, and `Content-Length` is bounded.
- The updater parses versions correctly and builds the new version before
  uninstalling the old one.
- Front end: `#[indent]` edge cases (comments, CRLF line endings, dedenting to
  an unknown level), float exponents, nested interpolation, a UTF-8 byte order
  mark, and a nesting-depth limit.
- JSON: surrogate pairs and malformed numbers.
- Smaller fixes in `parse_args`, the secrets file reader and `rand_int`.

### Other changes

- New website at [www.oxigenlang.org](https://www.oxigenlang.org) with a
  download page and a one-line installer
  (`curl -fsSL https://www.oxigenlang.org/install.sh | sh`).
- OxigenLang is dual-licensed under MIT or Apache-2.0.
- Releases are cut only from commits that pass the test suite, the install
  and uninstall scripts are safer, and the Docker image runs as a non-root
  user.

### Performance

The `example/bench_*` programs run within noise of 0.1.4. Under the JIT, a
range loop whose start is a variable (`range(a, b)`) is 12–15% slower, and a
microbenchmark that only allocates and drops nested containers is 4% slower.

### Known limitations

- Binary request bodies in `api` are still decoded as lossy UTF-8.
- End-of-file diagnostics still report column 0.
- Third-party CI actions are not yet pinned to commit SHAs.
