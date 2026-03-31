# OxigenLang

OxigenLang is a modern, lightweight, interpreted programming language implemented in Rust. It features a clean, expressive syntax, support for pattern matching, a module system with a standard library, and an optional indentation-based block syntax.

## Features

- **Module System**: Import standard library modules or local files with `introduce`/`intro`. Namespace imports, selective imports, and module caching.
- **Standard Library**: 11 built-in modules — math, strings, array, io, os, time, random, path, json, toml, and net (HTTP client with HTTPS support).
- **Flexible Block Styles**: Choose between traditional brace-based blocks or Python-style indentation blocks with the `#[indent]` directive.
- **Conditional Expressions**: Multi-arm `option` blocks, ternary shorthand, `unless` inverse conditionals, `unless ... then ...` fallback expressions, `when`/`unless` postfix guards, and short-circuit `and`/`or` logical operators.
- **Pattern Matching**: Define reusable patterns with the `pattern` keyword and match against them with `choose`.
- **Rich Type System**: Dynamic typing by default with optional type annotations for locking types and controlling mutability. Four declaration forms give precise control over value and type mutability.
- **Data Types**: Integers, Floats, Strings (with escape sequences and interpolation), Characters, Booleans, Arrays, Bytes, Uints, Tuples, Maps, Sets, and `None`.
- **Structs**: Composite data types with typed fields, single-inheritance, methods via `contains` blocks, `self` access, hidden fields, and dot-chaining.
- **First-Class Functions**: Named and anonymous functions, closures, typed parameters, default values, optional parameters, and implicit/explicit returns.
- **Unpacking**: Destructure arrays and tuples into multiple variables with `a, b := [1, 2]`.
- **Index Assignment**: Set map keys and array elements directly with `m[key] = value` and `arr[i] = value`.
- **Built-in Functions**: Built-in functions for I/O, collection manipulation, type conversion, and introspection.
- **REPL**: Interactive shell for quick experimentation with persistent state across lines.

## Installation

Requires [Rust](https://www.rust-lang.org/).

### Quick Install (Linux / macOS)

```bash
git clone https://github.com/javanhut/OxigenLang.git
cd OxigenLang
./install.sh
```

This builds oxigen, installs it to `~/.oxigen/`, and adds it to your PATH. Restart your shell or run:

```bash
export PATH="$HOME/.oxigen/bin:$PATH"
```

### Uninstall

```bash
./uninstall.sh
```

Removes the `~/.oxigen/` directory and cleans the PATH entry from your shell config.

### System-Wide Install

```bash
sudo make install                # installs to /usr/local/
sudo make install PREFIX=/opt    # custom prefix
sudo make uninstall              # removes it
```

### Development

Run directly from the source tree without installing:

```bash
cargo build --release
cargo run -- path/to/script.oxi
cargo run                        # starts the REPL
cargo run -- --version           # print version
```

## Quick Example

```oxi
introduce math
introduce strings

struct Person {
    name <str>
    age <int>
}

Person contains {
    fun greet() { "Hello, {self.name}" }
    fun is_adult() { self.age >= 18 }
}

pattern is_even(n) when n % 2 == 0

fun connect(host <str>, port <int> = 8080, tls? <bool>) {
    println("host={host} port={port} tls={tls}")
}

main {
    p <Person> := Person("Alice", 30)
    println(p.greet())
    println(p.is_adult())

    each i in range(6) {
        choose i {
            is_even -> println("{i} is even"),
            else -> println("{i} is odd")
        }
    }

    status <str> := option {
        p.is_adult() -> "adult",
        "minor"
    }
    println("{p.name} is an {status}")

    // Default and named arguments
    connect("example.com", tls=True)

    // Unpacking
    x, y := [10, 20]
    key, val := strings.split("name=Alice", "=")

    // Index assignment
    config <map>
    config["debug"] = True
    config["version"] = "0.2.0"

    // Standard library
    println(math.sqrt(16))
    println(strings.upper("hello oxigen"))
    println(strings.strip("--heading--", "-"))
}
```

Use `main { ... }` for script-only execution. Definitions outside `main` remain importable, while the `main` block is skipped when the file is brought in with `introduce`. Top-level statements still run when a file is executed directly, but `main` is the recommended style for files that may also be imported.

## Documentation

For detailed information, see the [docs](docs/) directory:

| Guide | Description |
|-------|-------------|
| [Getting Started](docs/getting_started.md) | Installation, first program, REPL usage |
| [Conventions](docs/conventions.md) | Idiomatic OxigenLang style and best practices |
| [Syntax Quick Reference](docs/syntax.md) | Concise syntax overview with links |
| [Variables and Assignments](docs/variables.md) | Declaration forms, mutability, reassignment |
| [Data Types](docs/data_types.md) | All supported types and their operations |
| [Operators](docs/operators.md) | Arithmetic, comparison, logical, and postfix operators |
| [Control Flow](docs/control_flow.md) | Conditionals, loops, guards, and block styles |
| [Angle Forms](docs/angle_forms.md) | Unified guide for type annotations, constructors, normalization, and effects |
| [Functions](docs/functions.md) | Named/anonymous functions, closures, typed parameters |
| [Pattern Matching](docs/pattern_matching.md) | Patterns, choose expressions, inline patterns |
| [Structs](docs/structs.md) | Fields, methods, inheritance, hidden fields |
| [Type System](docs/type_system.md) | Type annotations, conversions, mutability control |
| [Imports and Modules](docs/imports.md) | The `introduce` keyword and module system |
| [Built-in Functions](docs/builtins.md) | Complete reference for all built-ins |
| [Standard Library](docs/stdlib.md) | Full reference for all 11 stdlib modules |

## License

This project is licensed under the MIT License - see the LICENSE file for details.
