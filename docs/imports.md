# Imports and Modules

OxigenLang has a module system that lets you organize code across files and use the standard library. The `introduce` keyword (or its shorthand `intro`) brings modules into scope.

## Standard Library Imports

Use a bare module name to import from the standard library:

```oxi
introduce math
println(math.sqrt(16))
println(math.abs(-5))
```

This creates a **namespace** — you access the module's functions through the module name with dot notation.

## Selective Imports

To import specific functions directly into your scope, use curly braces and `from`:

```oxi
introduce {sqrt, abs} from math
println(sqrt(16))
println(abs(-5))
```

Selective imports don't require the module prefix.

## Local File Imports

Use a dot prefix to import from local `.oxi` files relative to the current file:

```oxi
// Import from ./helpers.oxi (same directory)
introduce .helpers
helpers.my_function()

// Selective import from local file
introduce {my_function} from .helpers
my_function()

// Import from ../utils/format.oxi (parent directory)
introduce ..utils.format
```

- `.` means the current file's directory
- `..` means one directory up
- Additional dots go further up (e.g., `...` = two directories up)
- Dots between names are path separators

## The `intro` Shorthand

`intro` is an alias for `introduce` — they are identical:

```oxi
intro math
intro {split, join} from strings
intro .helpers
```

## Module Behavior

### Namespacing

Whole-module imports create a namespace object. You access members with dot notation:

```oxi
introduce math
introduce strings

println(math.pow(2, 10))
println(strings.upper("hello"))
```

### Exports

All top-level bindings (functions, variables, structs, and patterns) in a module file are automatically exported. There is no explicit export mechanism.

Use top-level definitions for reusable module APIs, and put script-only work inside `main`. The `main` block is skipped when the file is imported with `introduce`.

```oxi
// mylib.oxi
fun greet(name) { "Hello, {name}!" }
PI := 3.14159
```

```oxi
// main.oxi
introduce .mylib

main {
    println(mylib.greet("World"))
    println(mylib.PI)
}
```

### Module Caching

Each module file is evaluated only once. If multiple files import the same module, they share the same module object. This means module-level state is shared.

### Circular Import Detection

If file A imports file B which imports file A, OxigenLang will produce a clear error:

```
Error: circular import detected: /path/to/file.oxi
```

Restructure your code to avoid circular dependencies.

## Standard Library Modules

OxigenLang ships with the following standard library modules:

| Module     | Description                                          |
|------------|------------------------------------------------------|
| [`math`](stdlib/math.md) | Math functions (abs, sqrt, pow, floor, ceil, etc.)   |
| [`strings`](stdlib/strings.md) | String manipulation (split, join, trim, upper, etc.) |
| [`array`](stdlib/array.md) | Array operations (map, filter, reduce, sort, etc.)   |
| [`io`](stdlib/io.md) | File I/O (read_file, write_file, etc.)               |
| [`os`](stdlib/os.md) | OS interaction (exec, env vars, directories, etc.)   |
| [`path`](stdlib/path.md) | File path manipulation and containment checks        |
| [`time`](stdlib/time.md) | Timestamps, sleep, elapsed time measurement          |
| [`datetime`](stdlib/datetime.md) | UTC calendar dates: format, parse, components |
| [`random`](stdlib/random.md) | Random number generation                             |
| [`regex`](stdlib/regex.md) | Regular expressions (Rust regex syntax)              |
| [`json`](stdlib/json.md) | JSON parsing, serialization, dotted-path access, and merging |
| [`toml`](stdlib/toml.md) | TOML parsing, serialization, dotted-path access, and merging |
| [`encoding`](stdlib/encoding.md) | Base64, hex, and URL percent-encoding          |
| [`hash`](stdlib/hash.md) | SHA-256, SHA-1, MD5 hex digests                      |
| [`net`](stdlib/net.md) | HTTP client, streaming HTTP, TCP and UDP sockets     |
| [`api`](stdlib/api.md) | HTTP server: routing, JSON responses, accept loop    |
| [`result`](stdlib/result.md) | Helpers for the `Error \|\| Value` system      |
| [`test`](stdlib/test.md) | `expect(...)` matchers used by `oxigen test`         |
| [`ansi`](stdlib/ansi.md) | Terminal colour, modifiers, cursor control           |
| [`env`](stdlib/env.md) | Load a `.env` file into a map                        |
| [`parse_args`](stdlib/parse_args.md) | Command-line flag parsing                |

See the [Standard Library Reference](stdlib/README.md) for full details on each
module.

## Examples

### Using multiple modules together

```oxi
introduce json
introduce net
introduce io

// Fetch JSON from an API
resp := net.get("https://api.example.com/data")
data := json.parse(resp["body"])

// Save to file
io.write_file("data.json", json.stringify(data))
```

### Building a utility with local imports

```oxi
// utils/format.oxi
fun pad_left(s, width, ch) {
    result := s
    repeat when len(result) < width {
        result := str(ch) + result
    }
    result
}
```

```oxi
// main.oxi
introduce .utils.format

main {
    println(format.pad_left("42", 5, "0"))
}
```
