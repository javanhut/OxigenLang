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
./scripts/install.sh
```

This builds oxigen and installs it to `/usr/local/bin` (requires sudo):

```bash
sudo ./scripts/install.sh
```

Or install to a custom prefix:

```bash
PREFIX=~/.local ./scripts/install.sh
```

The build artifacts are cleaned up automatically after installation.

### Uninstall

```bash
sudo ./scripts/uninstall.sh
```

Removes the oxigen binary and stdlib from `/usr/local/`.

### Makefile

```bash
sudo make install                # installs to /usr/local/
sudo make install-with-jit       # installs a JIT-enabled build to /usr/local/
sudo make install PREFIX=/opt    # custom prefix
sudo make install-with-jit PREFIX=/opt
sudo make uninstall              # removes it
```

### Development

Run directly from the source tree without installing:

```bash
cargo build --release -p oxigen
cargo run -p oxigen -- path/to/script.oxi
cargo run -p oxigen -- path/to/script.oxi Alice --flag=value
cargo run -p oxigen --              # starts the REPL
cargo run -p oxigen -- --version    # print version
cargo run -p oxigen -- fmt file.oxi # format a file
cargo run -p oxigen -- check file.oxi # parse and report errors as JSON
```

### Optional JIT (experimental)

OxigenLang ships with a baseline Cranelift-backed JIT that's off by
default. It already beats CPython 3.14 on tight numeric loops, nested
loops, and closure hot paths, and it's kept experimental until it wins
across *every* bench in the suite (recursion and method dispatch are
still behind). Build it in with the `jit` feature and opt in at run
time with `--jit`:

```bash
cargo build --release --features jit -p oxigen
make build-with-jit
sudo make install-with-jit
./target/release/oxigen --jit path/to/script.oxi   # eager (threshold=1)
./target/release/oxigen       path/to/script.oxi   # default tiering
./target/release/oxigen --no-jit path/to/script.oxi # interpreter only
```

Without `--features jit` the `--jit` flag is silently ignored and the
binary stays pure interpreter. Full architecture, supported opcodes,
safety invariants, and benchmark methodology are in
[docs/JIT_ARCHITECTURE.md](docs/JIT_ARCHITECTURE.md).

### Benchmark Against Python

Run the benchmark harness from the repo root:

```bash
python3 scripts/bench.py
```

That uses `target/release/oxigen` if it already exists, otherwise it
builds it, then runs every paired `example/bench_*.oxi` /
`example/bench_*.py` benchmark and writes both JSON and Markdown reports
to `benchmark_reports/`. The report stores raw samples, median/mean/
stdev/min/max per benchmark, and suite-level geometric-mean speedups vs
Python so you can track JIT performance over time. To run a single
benchmark:

```bash
python3 scripts/bench.py bench_loop
python3 scripts/bench.py example/bench_fib.oxi
```

Useful flags:

```bash
python3 scripts/bench.py --rebuild
python3 scripts/bench.py --oxigen-only
python3 scripts/bench.py --plain-build
python3 scripts/bench.py --runs 10 --warmups 3
python3 scripts/bench.py --report-name nightly --report-dir reports/benchmarks
```

## Editor Support

OxigenLang ships with a language server (LSP) and Neovim integration. The LSP is written in Go and provides diagnostics, completion, hover, document symbols, and formatting.

### Build the LSP

Requires [Go](https://go.dev/) 1.22+:

```bash
make build-lsp
```

Or include it with the installer:

```bash
./scripts/install.sh --with-lsp
```

See [editors/neovim/README.md](editors/neovim/README.md) for Neovim setup instructions.

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

## Script Args And Executable Files

Pass script arguments after the `.oxi` path:

```bash
oxigen example/script_args.oxi Alice --flag=value
```

Read them from Oxigen with `os.args()`:

```oxi
introduce os

main {
    println(os.args())
}
```

To run a script directly, add a real shebang as the first line and make the file executable:

```oxi
#!/usr/local/bin/oxigen
#[location=/usr/local/bin/oxigen]
introduce os

main {
    println(os.args())
}
```

```bash
chmod +x script.oxi
./script.oxi Alice
```

If Oxigen is on your `PATH`, `#!/usr/bin/env oxigen` also works.

Oxigen also accepts a top-of-file `#[location=/path/to/oxigen]` directive and preserves it during formatting. That directive is file metadata for tooling and should match the interpreter path you expect, but direct execution still depends on the real `#!` line because the OS only reads the shebang.

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
| [JIT Architecture](docs/JIT_ARCHITECTURE.md) | **Experimental** — baseline Cranelift JIT: how to build it in, runtime flags, supported opcodes, safety invariants, benchmark status vs CPython |

## License

This project is licensed under the MIT License - see the LICENSE file for details.
