# How the C FFI works

This page explains what happens inside Oxigen when a script calls C through
the [`ffi`](stdlib/ffi.md) module: how a signature string becomes a callable
function, how values cross the boundary, and how C can call back into a
script. Read it before changing `core/src/vm/ffi.rs`. To *use* the module,
the [ffi reference](stdlib/ffi.md) is all you need.

- [The layers](#the-layers)
- [Loading a library](#loading-a-library)
- [From a signature to a function](#from-a-signature-to-a-function)
- [One call, step by step](#one-call-step-by-step)
- [Structs](#structs)
- [Callbacks](#callbacks)
- [Safety and sandboxing](#safety-and-sandboxing)
- [Threads](#threads)
- [Platforms](#platforms)
- [Changing the FFI](#changing-the-ffi)

## The layers

```
script            libm.cos(0.0)
                      │  method call on a module
stdlib/ffi.oxi    ffi.open(lib, decls)  →  __ffi_open(lib, decls)
                      │
core/src/vm/ffi.rs    parse signatures, load the library, build natives
                      │  Value::Native closures inside a Value::Module
libffi                Cif::call_return_into / Closure  (the C calling convention)
                      │
C                 cos(0.0)
```

`stdlib/ffi.oxi` is a thin wrapper. Each public function forwards to an
`__ffi_*` builtin, apart from `ffi.new`, which is written in Oxigen on top of
`alloc`, `sizeof` and `write`. The builtins are registered by
`ffi::register`, which `register_builtins` calls. They are ordinary
`fn(&[Value]) -> Value` builtins, and they return an error value rather than
panicking when an argument is wrong.

The functions a script gets back from `ffi.open` and `ffi.fn` are not
builtins. They are `Value::Native` closures, the same kind a Rust host makes
with `vm.register_native`, so each one can carry its own signature and
function address. `ffi.open` puts them in a `Value::Module`. That is why
`libm.cos(0.0)` works: the VM's module method call looks `cos` up in the
module's globals and calls it. A map would not work, because `m.f(x)` on a
map is parsed as a call to a map method.

All the real work is in one file, `core/src/vm/ffi.rs`, behind
`#[cfg(all(not(target_arch = "wasm32"), any(unix, windows)))]`. On any other
target a stub module registers the same builtin names, and every one fails
with "ffi is not supported on this platform". Scripts and the capability
list therefore look the same everywhere.

## Loading a library

`library()` turns the script's library argument into a handle:

| Argument | What is loaded |
|----------|----------------|
| `None` | the C runtime: the running program on Unix (`dlopen(NULL)`), `ucrtbase.dll` on Windows |
| a bare name (`"m"`, no `/`, `\`, `.so`, `.dylib` or `.dll`) | each name from `os::candidates`, in order, until one opens |
| anything else | that file name or path, as given |

The candidates are `lib{name}.dylib`, then the Homebrew and `/usr/local`
copies, then `{name}.framework/{name}` on macOS; `lib{name}.so` and then
`.so.6`, `.so.1`, `.so.0` ... on Linux; and `{name}.dll` and
`lib{name}.dll` on Windows. Linux needs the versioned names because
`libm.so` is often a linker script, which the loader cannot open.

Unix uses `dlopen(RTLD_NOW | RTLD_LOCAL)`. `RTLD_NOW` makes a library with
unresolved symbols fail at `open`, not halfway through a call. Windows uses
`LoadLibraryA` and `GetProcAddress`, declared directly instead of pulling in
a Windows bindings crate.

Handles are cached per thread in `LIBS`, keyed by the argument as written, and
are never closed. A script that opens the same library in a loop costs one
hash lookup per call after the first. Unloading is left out on purpose: a
native bound to a library would turn into a dangling pointer the moment it
was closed.

## From a signature to a function

A signature string is parsed into a `Sig`:

```rust
struct Sig {
    name: Option<String>,   // "pow" in "double pow(double, double)"
    ret: CType,
    args: Vec<CType>,
    variadic: bool,         // the parameters end in "..."
}
```

`parse_sig` splits on the outer parentheses. `parse_decl` reads the text before
them, and each comma-separated parameter, as a type with an optional name:
it first tries the whole text as a type, and only if that fails does it treat
the last identifier as a name. That is how `unsigned int` stays a type while
`double x` becomes `double` named `x`. `parse_type` then maps the C spelling
to a `CType`:

```rust
enum CType {
    Void, Bool, I8, U8, I16, U16, I32, U32, I64, U64, F32, F64,
    Str,                        // char *
    Ptr(Option<Rc<CType>>),     // T *; None for void * and function pointers
    Struct(Rc<StructDef>),
}
```

A few rules worth knowing:

- `const`, `volatile`, `struct` and `union` are dropped before the lookup.
- `char *` becomes `Str`. Other pointers become `Ptr`, which keeps its pointee
  type (`int *` is `Ptr(Some(I32))`). The pointee type is what lets an Oxigen
  array be copied into, and back out of, a `T *` parameter.
- `long` and `unsigned long` follow `size_of::<c_long>()`: 8 bytes on 64-bit
  Unix, 4 on Windows.
- `enum X` is `I32`, as C compilers lay it out by default.
- A name that is not a built-in type is looked up among the structs declared
  with `ffi.cstruct` (see [Structs](#structs)). An unknown name is an error.

`native(addr, sig)` makes the callable. For a fixed-arity signature it builds
the libffi `Cif` (the call interface: argument and return types for this
platform's ABI) once, at bind time. The `Cif` is moved into the closure, so a
call does no type work at all. A variadic signature cannot be prepared in
advance, because the types of the extra arguments depend on each call, so its
`Cif` is built per call with `Cif::new_variadic`.

`ffi.open` resolves every symbol while it builds the module. A missing
function or a bad signature is reported there, naming the key, instead of on
the first call.

## One call, step by step

`call(addr, sig, cached_cif, args)` runs for every C call:

1. **Arity.** A fixed signature needs exactly `sig.args.len()` arguments, and
   a variadic one at least that many.
2. **Variadic types.** Each extra argument gets C's default promotion from its
   Oxigen type: `float` to `double`, `str` to `char *`, `uint` to `uint64_t`,
   and every other integer-like value to `long long`. On the 64-bit ABIs
   supported, a `long long` in an `int` slot is read correctly, because the
   callee reads the low 32 bits, so `%d` and `%lld` both work.
3. **Marshal.** Each argument is written into its own zeroed buffer, made of
   `u64` words so it is 8-byte aligned, by `write_value`. Memory that must
   outlive this step goes in `Temps`:
   - `strings`: the `CString` copy of every `str` passed as `char *` or
     `void *`.
   - `back`: for every array or map passed to a `T *`, the C buffer it was
     copied into, and the script value it came from.
4. **Call.** `cif.call_return_into` does the call. The return buffer is at
   least 16 bytes, because libffi widens integer returns smaller than a
   register to a full `ffi_arg`.
5. **Callback errors.** If a callback failed during the call, it left its error
   in `PENDING`, and the call returns that error now (see
   [Callbacks](#callbacks)).
6. **Write back.** Each `Temps::back` buffer is read element by element, back
   into the array or map that was passed. This is what makes
   `frexp(8.0, exp)` update `exp[0]`.
7. **Return.** `read_value` converts the return buffer. A NULL `char *` or
   pointer becomes `None`, and a struct becomes a map.

`Temps` is dropped at the end of `call`, so the string copies and the
write-back buffers live exactly as long as the C call. A C function that keeps
a `char *` argument after it returns needs memory from `ffi.cstring`, which
lasts until `ffi.free`.

`write_value` and `read_value` are the only two places that know how a
`CType` looks in memory, so arguments, returns, struct fields, `ffi.read`,
`ffi.write` and callbacks all convert values the same way.

## Structs

`ffi.cstruct("Vec2", "double x; double y")` parses each field with
`parse_decl` and builds `Type::structure` from the field types. It then asks
libffi for the layout with `struct_offsets`. Offsets, size and alignment
therefore come from libffi's model of the platform ABI, the same model that
passes the struct in registers or on the stack, and are never computed by
hand. The result is a `StructDef`:

```rust
struct StructDef {
    name: String,
    fields: Vec<(String, CType, usize)>,   // name, type, byte offset
    size: usize,
    align: usize,
    ffi: Type,                             // what a Cif uses to pass it by value
}
```

The definition is registered by name in the per-thread `STRUCTS` table. Later
signatures can then use `Vec2` and `Vec2 *` like any other type, and so can
later structs, which is how nesting works. Declaring a name again replaces it
for signatures parsed afterwards. Functions bound earlier keep the old layout,
because they hold an `Rc` to it.

When a struct is written, a map sets its fields by name (missing ones stay
zero) and an array or tuple sets them in order. When one is read, it always
comes back as a map, with keys in field order.

The one-string form, `ffi.cstruct("typedef struct \{ int a; \} Item;")`, is
only another way to spell the name and fields. It takes the name from before
the brace, or after it for a typedef.

## Callbacks

A callback is the reverse direction: C calls a function pointer, and an Oxigen
function runs. Two problems make this the subtle part of the FFI.

**Making the pointer.** `ffi.callback(sig, func)` builds a `Cif` for the
signature and a libffi `Closure`. That is a small piece of executable code
which, when C calls it, calls the `trampoline` function with the C arguments
and a pointer to its `CallbackData` (the `Sig` and the Oxigen function). The
`CallbackData` is leaked to get the `'static` lifetime the closure needs, and
the `Closure` is kept in the per-thread `CALLBACKS` list. A callback is
therefore never freed. That is a known ceiling, marked with a `ponytail:`
comment: a script that makes callbacks in a loop grows memory, and the fix is
an `ffi.free_callback`.

**Finding the VM.** The trampoline is a plain `extern "C"` function. C passes
it no VM, and a builtin never has one either. The VM supplies it instead: in
`VM::call_value`, the branch that calls a `Value::Native` stores
`self as *mut VM` in the thread-local `ffi::CURRENT_VM` for the duration of
the call, and restores the previous value afterwards. Every C call a script
makes goes through that branch, so while C is running, `CURRENT_VM` points at
the VM that called it. Calls nest naturally: a callback that calls C that
calls another callback sets and restores the same pointer.

The trampoline then:

1. converts each C argument with `read_value`;
2. calls the function with `VM::call_reentrant`;
3. writes the result with `write_return`, which widens small integers to a
   full register as libffi requires.

`call_reentrant` is `call_with_args` plus unwinding. It records the frame,
stack, JIT-frame and error-handler depths, and if the call fails it truncates
all four back and closes the upvalues above the old stack top. The script that
made the outer C call then continues from a consistent VM, as if the C
function had returned an error.

**Reporting errors.** A callback cannot return an error through C, and a
Rust panic must not unwind into C frames. The trampoline runs everything
inside `catch_unwind`. On failure it zeroes the return value, stores the first
error message in the thread-local `PENDING`, and returns normally. When the
outer C call comes back, `call` sees `PENDING` and fails with that message,
which the script can catch like any other error.

**The stack invariant.** While a callback runs, the VM's operand stack can
grow and reallocate. The `args` slice the native received points into that
stack. `call` must therefore finish reading `args` before the C call starts,
and never read it after; the comment at that point in `call` says so. Values
that write-back needs afterwards are kept as cloned `Value`s in `Temps`, not
as references into `args`. Any change to `call` must keep this.

**What callbacks cannot do.** A callback needs `CURRENT_VM`, which is only set
on the script's thread while one of its C calls is running. That covers
sorting, iteration, `sqlite3_exec`-style row callbacks and event polling,
where C calls back from inside a call. A callback that C calls later, on its
own thread, finds a null `CURRENT_VM` and fails. Its error goes into that
thread's `PENDING`, where nothing will report it, so the C side just sees a
zero return.

## Safety and sandboxing

Nothing on the C side can be checked. A wrong signature, a dangling pointer
or a too-small buffer is undefined behaviour and can crash the process, just
as in C. The FFI does validate everything it can see: argument counts, value
types against declared types, NUL bytes in strings, NULL pointers passed to
`ffi.read` and `ffi.write`, and unknown type names. `ffi.write` builds the
value in a scratch buffer first, so a bad field fails before any memory is
touched.

Since C code can do anything the process can, the FFI is behind its own
capability, `Capability::Ffi`. Every `__ffi_*` builtin is listed in
`GATED_BUILTINS` in `core/src/vm/capabilities.rs`. When a host calls
`vm.restrict` without `Ffi`, those builtins are replaced with functions that
fail, naming the capability. Bound C functions can only be made through
`__ffi_open` and `__ffi_fn`, so gating the builtins gates every C call. The
`every_builtin_is_either_gated_or_known_to_be_pure` test fails if a new
`__ffi_*` builtin is added without being gated.

## Threads

All FFI state is per thread: the library cache, the struct table, the
callbacks and `CURRENT_VM`. That matches the VM, which belongs to one thread.

A `diverge` task runs on a fresh VM on another thread, and only values that
can be copied cross into it. A module of natives cannot, so a library opened
at the top level is undefined inside a task. Open it inside the task instead,
usually through a function, which every task can call:

```oxi
introduce ffi

fun libc() { ffi.open(None, {"labs": "long(long)"}) }

println(diverge each n in [-1, -2, -3] { libc().labs(n) })   // [1, 2, 3]
```

Each worker thread pays for the `dlopen` and the symbol lookups once. After
that, `ffi.open` hits that thread's library cache and only rebuilds the
natives. Structs are per thread too, so a task must call `ffi.cstruct` before
it uses a struct.

## Platforms

| Platform | Status |
|----------|--------|
| macOS, Apple Silicon | tested: the full test suite, sqlite3, and about 100,000 callbacks re-entering JIT-compiled code |
| Linux and macOS, x86_64; Linux, aarch64 | expected to work: the same libffi and `dlopen` code paths, not yet tested |
| Linux, static musl (the prebuilt release binaries) | compiles, but cannot load libraries: static musl has no dynamic loader, so `dlopen` always fails |
| Windows | written, not yet tested |
| WebAssembly (the browser playground) | not available: libffi does not target `wasm32` |

libffi comes from the `libffi` crate, which builds its bundled copy of libffi
with the system C compiler. It is a non-wasm32 target dependency in
`core/Cargo.toml`, so the playground build never compiles it. Building it
needs a C compiler, `sh` and `make`; on Windows MSVC it uses the compiler and
assembler that come with Visual Studio.

For `ffi` to work on the prebuilt Linux binaries, the release workflow would
have to build for `*-unknown-linux-gnu` (dynamically linked against glibc)
instead of `*-unknown-linux-musl`, or ship both.

## Changing the FFI

**A new C type spelling** (another alias for an existing type, such as
`DWORD`): add it to the `match` in `parse_type`.

**A new kind of C type** (a fixed-size array field, say): add a `CType`
variant and handle it in `ffi()`, `size()`, `align()`, `write_value` and
`read_value`. Those five are the whole description of a type, and every path
(arguments, returns, fields, `read`/`write`, callbacks) goes through them.

**A new builtin**:

1. Write the function in the `imp` module and add it to `BUILTINS`.
2. Add the same name, mapped to `unsupported`, to the stub module's `BUILTINS`.
3. Add it to `GATED_BUILTINS` with `Capability::Ffi`.
4. Add the Oxigen wrapper to `stdlib/ffi.oxi`.
5. Document it in `docs/stdlib/ffi.md`.

**Tests**: the signature parser has unit tests at the bottom of `ffi.rs`, and
the behaviour is tested from Oxigen in `tests/ffi_test.oxi`, which calls the C
runtime and libm, so it needs no extra libraries. Run them with:

```sh
cargo test -p oxigen-core --lib ffi
oxigen test tests/ffi_test.oxi
```
