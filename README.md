# OxigenLang

OxigenLang is a modern, lightweight, interpreted programming language implemented in Rust. It features a unique blend of C-like and Python-like syntax, support for pattern matching, and an optional indentation-based block syntax.

## Features

- **Unique Syntax**: Choose between traditional brace-based blocks or Python-style indentation blocks.
- **Pattern Matching**: Define patterns using the `pattern` keyword and use them in `choose` expressions.
- **Data Types**: Integers, Floats, Strings, Characters, Booleans, Arrays, and `None`.
- **Structs**: Composite data types with typed fields, inheritance, methods, and dot-access.
- **First-Class Functions**: Define and pass functions as values.
- **Built-in Functions**: Comprehensive set of built-ins for array manipulation, string handling, and more.
- **REPL**: Interactive shell for quick experimentation.

## Getting Started

### Installation

Ensure you have [Rust](https://www.rust-lang.org/) installed. Clone the repository and build the project:

```bash
cargo build --release
```

### Running Scripts

You can run OxigenLang scripts (`.oxi`) using the interpreter:

```bash
cargo run -- path/to/script.oxi
```

### REPL

To start the interactive REPL, simply run:

```bash
cargo run
```

## Documentation

For detailed information, see the [docs](docs/) directory:

- [Syntax Overview](docs/syntax.md)
- [Type System](docs/type_system.md)
- [Structs](docs/structs.md)
- [Built-in Functions](docs/builtins.md)
- [Getting Started Guide](docs/getting_started.md)

## License

This project is licensed under the MIT License - see the LICENSE file for details.
