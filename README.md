# OxigenLang

OxigenLang is a modern, lightweight, interpreted programming language implemented in Rust. It features a unique blend of C-like and Python-like syntax, support for pattern matching, and an optional indentation-based block syntax.

## Features

- **Flexible Block Styles**: Choose between traditional brace-based blocks or Python-style indentation blocks with the `#[indent]` directive.
- **Conditional Expressions**: Multi-arm `option` blocks, ternary shorthand, `unless` inverse conditionals, `when`/`unless` postfix guards, and short-circuit `and`/`or` logical operators.
- **Pattern Matching**: Define reusable patterns with the `pattern` keyword and match against them with `choose`. Supports both top-level and inline pattern definitions.
- **Rich Type System**: Dynamic typing by default with optional type annotations for locking types and controlling mutability. Four declaration forms give precise control over value and type mutability.
- **Data Types**: Integers, Floats, Strings, Characters, Booleans, Arrays, Bytes, Uints, Tuples, Maps, Sets, and `None`.
- **Structs**: Composite data types with typed fields, single-inheritance, methods via `contains` blocks, implicit self, hidden fields, and dot-chaining.
- **First-Class Functions**: Named and anonymous functions, closures, typed parameters, and implicit/explicit returns.
- **Built-in Functions**: 22 built-in functions for I/O, collection manipulation, type conversion, and introspection.
- **REPL**: Interactive shell for quick experimentation with persistent state across lines.

## Getting Started

### Installation

Ensure you have [Rust](https://www.rust-lang.org/) installed. Clone the repository and build the project:

```bash
cargo build --release
```

### Running Scripts

Run OxigenLang scripts (`.oxi`) using the interpreter:

```bash
cargo run -- path/to/script.oxi
```

### REPL

Start the interactive REPL:

```bash
cargo run
```

## Quick Example

```oxi
struct Person {
    name <str>
    age <int>
}

Person contains {
    fun greet() { "Hello, " + name }
    fun is_adult() { age >= 18 }
}

p := Person("Alice", 30)
println(p.greet())
println(p.is_adult())

pattern is_even(n) when n % 2 == 0

each i in range(6) {
    choose i {
        is_even -> println(i, "is even"),
        else -> println(i, "is odd")
    }
}

status := option {
    p.is_adult() -> "adult",
    "minor"
}
println(p.name, "is an", status)
```

## Documentation

For detailed information, see the [docs](docs/) directory:

| Guide | Description |
|-------|-------------|
| [Getting Started](docs/getting_started.md) | Installation, first program, REPL usage |
| [Syntax Quick Reference](docs/syntax.md) | Concise syntax overview with links |
| [Variables and Assignments](docs/variables.md) | Declaration forms, mutability, reassignment |
| [Data Types](docs/data_types.md) | All supported types and their operations |
| [Operators](docs/operators.md) | Arithmetic, comparison, logical, and postfix operators |
| [Control Flow](docs/control_flow.md) | Conditionals, loops, guards, and block styles |
| [Functions](docs/functions.md) | Named/anonymous functions, closures, typed parameters |
| [Pattern Matching](docs/pattern_matching.md) | Patterns, choose expressions, inline patterns |
| [Structs](docs/structs.md) | Fields, methods, inheritance, hidden fields |
| [Type System](docs/type_system.md) | Type annotations, conversions, mutability control |
| [Built-in Functions](docs/builtins.md) | Complete reference for all 22 built-ins |

## License

This project is licensed under the MIT License - see the LICENSE file for details.
