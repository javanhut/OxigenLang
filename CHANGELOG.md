# Changelog

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
