# Getting Started with OxigenLang

Welcome to OxigenLang! This guide walks you through installation, running your first program, using the interactive REPL, and exploring the language's core features.

## Installation

Ensure you have [Rust](https://www.rust-lang.org/) installed. Clone the repository and build the project:

```bash
cargo build --release -p oxigen
```

The compiled binary will be at `target/release/oxigen` (or you can run directly with `cargo run -p oxigen -- ...`).

Check the installed version:

```bash
oxigen --version
```

## Your First Program

Save the following code in a file named `hello.oxi`:

```oxi
// My first OxigenLang program
main {
    name := "Explorer"
    println("Hello", name, "!")
    println("OxigenLang is a modern, lightweight, interpreted programming language.")
}
```

Run it using the command:

```bash
cargo run -p oxigen -- hello.oxi
```

You should see:

```
Hello Explorer !
OxigenLang is a modern, lightweight, interpreted programming language.
```

`main { ... }` is the recommended entry point for runnable scripts. Definitions outside `main` remain available when the file is imported with `introduce`, while the `main` block only runs when the file is executed directly.

## Using the REPL

The interactive REPL (Read-Eval-Print Loop) is perfect for testing small code snippets. Start it by running:

```bash
cargo run -p oxigen --
```

You'll see a prompt like this:

```
Oxigen REPL v0.1.1
Type 'exit' or 'quit' to exit, 'version' for version info
>>
```

Type any expression or statement at the prompt and press Enter to see the result:

```oxi
>> x := 42
42
>> x + 8
50
>> len([1, 2, 3])
3
>> println("Hello from REPL")
Hello from REPL
```

### REPL Commands

| Command   | Description                |
|-----------|----------------------------|
| `exit`    | Exit the REPL              |
| `quit`    | Exit the REPL              |
| `version` | Show the OxigenLang version |

The REPL maintains state across lines — variables, functions, structs, and patterns defined in one line are available in subsequent lines.

### Command-Line Usage

| Command / Flag | Description |
|----------------|-------------|
| `oxigen file.oxi [args...]` | Execute a script (uses the bytecode VM by default) |
| `oxigen file.oxi --tree-walk` | Execute using the tree-walking interpreter (fallback) |
| `oxigen` | Start the interactive REPL |
| `oxigen fmt file.oxi [...]` | Format one or more `.oxi` files in-place |
| `oxigen check file.oxi` | Parse a file and output diagnostics as JSON |
| `--version`, `-v` | Print the version and exit |

Scripts can read forwarded arguments through `os.args()`:

```oxi
introduce os

main {
    println(os.args())
}
```

To make a script executable directly on Unix-like systems, add a shebang as the first line:

```oxi
#!/usr/local/bin/oxigen
#[location=/usr/local/bin/oxigen]
introduce os

main {
    println("Hello from", os.args())
}
```

Then mark it executable and run it:

```bash
chmod +x hello.oxi
./hello.oxi Alice
```

If Oxigen is on your `PATH`, `#!/usr/bin/env oxigen` is also valid.

Oxigen also recognizes `#[location=/path/to/oxigen]` as top-of-file metadata and preserves it during formatting. Use it when you want the interpreter path recorded in the file, but keep the real shebang on line 1 if you want `./hello.oxi` to work.

## Exploring Indentation Mode

OxigenLang supports two block styles: brace-based (default) and Python-style indentation blocks. To enable indentation mode, add `#[indent]` near the top of your file. It can appear by itself or after a shebang / `#[location=...]` header:

```oxi
#[indent]

main:
    x := 10
    result := option:
        x > 0 -> "positive",
        "not positive"
    println(x, "is", result)

    each i in [1, 2, 3]:
        println("Iteration", i)
```

In indentation mode, a colon `:` at the end of a line starts a new block, and returning to a previous indentation level closes the block. Use `main:` as the indentation-mode equivalent of `main { ... }`. See the [Control Flow](control_flow.md) guide for more details on block styles.

## Quick Examples

### Pattern Matching

Define reusable patterns and match against them with `choose`:

```oxi
pattern is_even(n) when n % 2 == 0
pattern is_odd(n) when n % 2 != 0

main {
    nums := [1, 2, 3, 4, 5]
    each n in nums {
        choose n {
            is_even -> println(n, "is even"),
            is_odd -> println(n, "is odd")
        }
    }
}
```

See the full [Pattern Matching](pattern_matching.md) guide for details.

### Structs

Define composite data types with typed fields, methods, and inheritance:

```oxi
struct Person {
    name <str>
    age <int>
}

Person contains {
    fun greet() { println("Hi, I'm " + name) }
    fun is_adult() { age >= 18 }
}

struct Student(Person) {
    school <str>
}

main {
    p := Person("Alice", 30)
    p.greet()
    println(p.is_adult())

    p.age = 15
    println(p.is_adult())

    s := Student("Bob", 20, "MIT")
    s.greet()
    println(s.school)
}
```

See the full [Structs](structs.md) guide for details.

### Functions

Functions are first-class values in OxigenLang:

```oxi
fun add(a, b) { a + b }

fun greet(name <str>) {
    println("Hello, " + name + "!")
}

double := fun(x) { x * 2 }

main {
    println(add(3, 4))
    greet("World")
    println(double(5))
}
```

See the full [Functions](functions.md) guide for details.

### Conditional Expressions

The `option` keyword provides multi-arm conditional expressions:

```oxi
main {
    age := 25
    status := option {
        age < 13 -> "child",
        age < 20 -> "teenager",
        "adult"
    }
    println(status)
}
```

See the full [Control Flow](control_flow.md) guide for details.

## Next Steps

- [Variables and Assignments](variables.md) — how to declare and work with variables
- [Data Types](data_types.md) — all supported data types and their operations
- [Operators](operators.md) — arithmetic, comparison, logical, and postfix operators
- [Control Flow](control_flow.md) — conditionals, loops, guards, and block styles
- [Functions](functions.md) — defining and using functions
- [Pattern Matching](pattern_matching.md) — patterns and choose expressions
- [Structs](structs.md) — composite types, methods, and inheritance
- [Type System](type_system.md) — optional type annotations and mutability control
- [Built-in Functions](builtins.md) — the standard library of built-in functions
- [Imports and Modules](imports.md) — the `introduce` keyword and module system
- [Standard Library](stdlib.md) — math, strings, array, io, os, time, random, path, json, toml, net
