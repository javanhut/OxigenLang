# ffi

Call C libraries. Declare the functions you need with their C signatures,
copied from the header, and call them like Oxigen functions. For how this
works inside the VM, see [FFI architecture](../FFI_ARCHITECTURE.md).

```oxi
introduce ffi

libm := ffi.open("m", {
    "cos": "double(double)",
    "pow": "double(double x, double y)",
})
println(libm.cos(0.0))        // 1
println(libm.pow(2.0, 10.0))  // 1024
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`open`](#openlib-decls) | `open(lib, decls?)` | a module of the declared functions |
| [`fn`](#fnlib-signature) | `fn(lib, signature)` | one function |
| [`cstruct`](#cstructname-fields) | `cstruct(name, fields)` | the struct's name |
| [`callback`](#callbacksignature-func) | `callback(signature, func)` | a C function pointer |
| [`alloc`](#memory) | `alloc(size)` | pointer to zeroed memory |
| [`new`](#memory) | `new(type, value?)` | pointer to one zeroed `type` |
| [`free`](#memory) | `free(ptr)` | `None` |
| [`sizeof`](#memory) | `sizeof(type)` | `int` |
| [`read`](#memory) | `read(ptr, type, index?)` | the value |
| [`write`](#memory) | `write(ptr, type, value, index?)` | `None` |
| [`string`](#memory) | `string(ptr, len?)` | `str` |
| [`cstring`](#memory) | `cstring(s)` | pointer to a C copy of `s` |
| [`sym`](#symlib-name) | `sym(lib, name)` | address of a symbol |

## Libraries

A library is one of:

- a bare name: `"m"`, `"sqlite3"`, `"z"`. Oxigen tries the platform's file
  names (`libm.dylib`; `libm.so`, `libm.so.6`, ...; `m.dll`) and the usual
  install directories (on macOS, Homebrew's `/opt/homebrew/lib`).
- a file name or path: `"libfoo.so.2"`, `"./build/libgame.dylib"`.
- `None`: the C runtime (`malloc`, `strlen`, `qsort`, `snprintf`, ...).

A library is loaded once and stays loaded.

## `open(lib, decls)`

Binds every function in `decls`, a map of name to C signature, and returns
them as a module:

```oxi
libc := ffi.open(None, {
    "strlen": "size_t(const char *s)",
    "getenv": "char *(const char *name)",
})
println(libc.strlen("hello"))     // 5
println(libc.getenv("HOME"))      // /Users/you
```

The key is the C function's name. To call it something else, put the C name
in the signature: `{"len": "size_t strlen(const char *)"}`. A missing symbol or
a bad signature fails here, not on the first call.

## `fn(lib, signature)`

One function, named in the signature:

```oxi
abs := ffi.fn(None, "int abs(int)")
println(abs(-3))   // 3
```

Given a function pointer instead of a library, `fn` makes it callable; the
name can then be left out: `ffi.fn(ptr, "int(int)")`.

## Signatures and types

Signatures are C: `ret name(params)`, with the name optional and parameter
names allowed and ignored. `const`, `struct` and `volatile` are ignored too.

| C type | Oxigen value |
|--------|--------------|
| `char`, `short`, `int`, `long`, `long long`, their `unsigned` forms, `int8_t` ... `uint32_t`, `ssize_t`, `intptr_t`, `enum X` | `int` |
| `unsigned long long`, `uint64_t`, `size_t`, `uintptr_t` | `uint` |
| `bool` | `bool` |
| `float`, `double` | `float` |
| `char *` | `str` in and out (`None` for NULL) |
| `void *`, function pointers | `int` address, `None` for NULL |
| `T *` | an address, or an array or map to pass in and out (see below) |
| a struct from `cstruct`, by value | `map` of field name to value |
| `void` | return type only: `None` |

The short names `i8` ... `i64`, `u8` ... `u64`, `f32`, `f64`, `str` and `ptr`
work too.

A function returning `char *` gives a copy of the string. If you must free
the pointer afterwards, declare the return as `void *`, read it with
`ffi.string`, then free it.

### Pointer parameters

For a `T *` parameter, pass an Oxigen array (or a map, for a struct). It is
copied into C memory for the call and copied back afterwards, which is how out
parameters work:

```oxi
libm := ffi.open("m", {"frexp": "double(double x, int *exp)"})
exp := [0]
println(libm.frexp(8.0, exp))   // 0.5
println(exp[0])                 // 4
```

An address from `ffi.alloc` or another C call works too, and so does `None`
for NULL. A `void *` parameter takes a string or an address, never an array:
declare the element type (`int *`) to pass one.

### Variadic functions

End the parameters with `...`. Extra arguments go as C's default promotions:
`int` as `long long`, `float` as `double`, `str` as `char *`.

```oxi
libc := ffi.open(None, {"snprintf": "int(char *buf, size_t n, const char *fmt, ...)"})
buf := ffi.alloc(64)
libc.snprintf(buf, 64, "%s=%d (%.1f)", "x", 42, 2.5)
println(ffi.string(buf))   // x=42 (2.5)
ffi.free(buf)
```

## `cstruct(name, fields)`

Declares a C struct so its name works as a type in signatures, by value or as
`Name *`. List the fields as in C:

```oxi
ffi.cstruct("div_t", "int quot; int rem")
ffi.cstruct("Vec2", ["double x", "double y"])   // an array works too

libc := ffi.open(None, {"div": "div_t(int, int)"})
println(libc.div(7, 2))   // {quot: 3, rem: 1}
```

Struct values are maps. Missing fields are zero, and an array or tuple sets the
fields in order. Layout and padding follow the platform's C ABI. A struct can
contain another struct declared before it.

To paste a declaration straight from a header, give it as one string, with
its braces escaped (`\{ \}`), since a bare `{` in an Oxigen string starts an
interpolation:

```oxi
ffi.cstruct("typedef struct \{ int id; char *name; \} Item;")
```

## `callback(signature, func)`

A C function pointer that calls `func`, for APIs that take callbacks:

```oxi
libc := ffi.open(None, {"qsort": "void(int *base, size_t n, size_t size, void *cmp)"})
cmp := ffi.callback("int(const void *a, const void *b)", fun(a, b) {
    ffi.read(a, "int") - ffi.read(b, "int")
})
xs := [5, 1, 4, 2, 3]
libc.qsort(xs, len(xs), ffi.sizeof("int"), cmp)
println(xs)   // [1, 2, 3, 4, 5]
```

The callback gets its arguments converted the same way as return values: a
`char *` arrives as a `str`, a struct as a map. If it fails, the C call that
ran it fails with the same error once C returns. A callback stays valid until
the program exits. C must call it on the script's thread while a C function
the script called is still running, which is how sorting, iteration and
event-polling APIs work. A callback that a C thread calls later, on its own,
fails.

## Memory

`alloc`, `new` and `cstring` return C memory that you free with `free`.
`read` and `write` take a C type and an optional index into an array of them:

```oxi
p := ffi.new("Vec2", {"x": 1.5, "y": -2.0})
println(ffi.read(p, "Vec2"))        // {x: 1.5, y: -2}
println(ffi.read(p, "double", 1))   // -2   (the second double)
ffi.write(p, "double", 9.0)
ffi.free(p)
```

`ffi.string(ptr)` reads a NUL-terminated string, and `ffi.string(ptr, len)`
reads `len` bytes. `ffi.cstring(s)` makes a C copy of `s` that outlives the
call, for a `char *` struct field or a global.

## `sym(lib, name)`

The address of a symbol, usually a global variable a library exports:
`ffi.read(ffi.sym("mylib", "mylib_version"), "int")`.

## A complete example

SQLite, with an out parameter and a callback per row:

```oxi
introduce ffi

sq := ffi.open("sqlite3", {
    "open": "int sqlite3_open(const char *path, void **db)",
    "exec": "int sqlite3_exec(void *db, const char *sql, void *cb, void *arg, char **err)",
    "close": "int sqlite3_close(void *db)",
})

db := [None]
sq.open(":memory:", db)
sq.exec(db[0], "create table t(name text); insert into t values ('a'), ('b');", None, None, None)

row := ffi.callback("int(void *arg, int n, char **vals, char **cols)", fun(_, n, vals, cols) {
    println(ffi.read(vals, "char *", 0))
    0
})
sq.exec(db[0], "select name from t", row, None, None)
sq.close(db[0])
```

## Threads

A library opened at the top level of a script cannot be used inside a
`diverge` task: the task runs on its own VM, and C functions do not cross into
it. Put the declarations in a function and call it inside the task. Each
thread loads the library once and reuses it after that:

```oxi
fun libc() { ffi.open(None, {"labs": "long(long)"}) }

println(diverge each n in [-1, -2, -3] { libc().labs(n) })   // [1, 2, 3]
```

Structs are declared per thread too: call `ffi.cstruct` inside the task before
using one there.

## Safety

The signature is a promise the program cannot check. A wrong one, a bad
pointer or a freed buffer is undefined behaviour, as in C, and can crash the
process.

Every `ffi` function needs the `ffi` capability. A host that calls
`vm.restrict` must list `Capability::Ffi` to allow it, and should grant it only
to a script it would trust as native code.

## Limits

- Struct fields cannot be inline arrays (`char name[16]`). Declare the bytes as
  separate fields, or use a pointer and `ffi.read` with an index.
- No unions, bit-fields or `long double`.
- Not available in the browser playground (WebAssembly).
- The prebuilt Linux release binaries are statically linked against musl,
  which cannot load shared libraries: every `ffi.open` fails there. Build
  from source (`./scripts/install.sh`) to use `ffi` on Linux. The macOS
  releases are not affected.
- Windows support (`LoadLibrary`, the universal CRT for `None`) is untested.
