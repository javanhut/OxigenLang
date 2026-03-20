# Getting Started with OxigenLang

Welcome to OxigenLang! This guide will help you get started with your first OxigenLang programs.

## Your First Program

Save the following code in a file named `hello.oxi`:

```oxi
# A simple OxigenLang program
name := "Explorer"
println("Hello", name, "!")
println("OxigenLang is a modern, lightweight, interpreted programming language.")
```

Run it using the command:

```bash
cargo run -- hello.oxi
```

## Exploring Indentation Mode

OxigenLang also supports Python-style indentation blocks. Save this in `indent.oxi`:

```oxi
#[indent]
# Indentation-based block syntax
x := 10
if x > 0:
    println(x, "is positive")
    
    each i in [1, 2, 3]:
        println("Iteration", i)
```

Run it using:

```bash
cargo run -- indent.oxi
```

## Using the REPL

The interactive REPL (Read-Eval-Print Loop) is perfect for testing small code snippets.

```bash
cargo run
```

Type any expression or statement at the prompt and press Enter to see the result.

```oxi
>> x := 42
42
>> x + 8
50
>> len([1, 2, 3])
3
>> println("Hello from REPL")
Hello from REPL
None
```

## Pattern Matching Example

```oxi
# pattern_match.oxi
pattern is_even(n) when n % 2 == 0
pattern is_odd(n) when n % 2 != 0

nums := [1, 2, 3, 4, 5]
each n in nums {
    choose n {
        is_even -> println(n, "is even"),
        is_odd -> println(n, "is odd")
    }
}
```

Run it using:

```bash
cargo run -- pattern_match.oxi
```
