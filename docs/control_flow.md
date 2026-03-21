# OxigenLang Control Flow

OxigenLang provides several control flow constructs: conditional expressions with `option`, inverse conditionals with `unless`, inline guards with `when`/`unless`, and two loop forms — `each` for iteration and `repeat` for while-style loops.

## Conditional Expressions with `option`

`option` is a multi-arm conditional expression. Each arm has a condition and a body, separated by `->`. The last arm without `->` is the default. The first arm whose condition is truthy is evaluated, and its result is returned.

### Basic Usage

```oxi
age := 25
status := option {
    age >= 18 -> "adult",
    "minor"
}
println(status)
```

### Multi-Arm

```oxi
ticket := option {
    age < 5  -> 0,
    age < 12 -> 5,
    age < 65 -> 15,
    10
}
```

Arms are evaluated top to bottom. The first matching arm wins.

### Block Bodies

When an arm needs multiple statements, use a block `{ ... }`:

```oxi
option {
    x > 100 -> {
        println("large value")
        process(x)
    },
    {
        println("normal value")
        x
    }
}
```

The last expression in a block is the value of that arm.

### Ternary Form

For a simple two-way conditional, you can use the ternary shorthand with comma-separated values:

```oxi
result := option { num > 5, "greater than 5", "less than 5" }
```

This is equivalent to:

```oxi
result := option {
    num > 5 -> "greater than 5",
    "less than 5"
}
```

### No Default

If no default arm is provided and no condition matches, `option` returns `None`:

```oxi
x := option { False -> "never" }
```

### Using `option` for Side Effects

Since `option` is an expression, you can use it as a statement for conditional side effects:

```oxi
option {
    retry == 8 -> logged_in := True
}
```

## Inverse Conditional with `unless`

`unless` runs a block only when the condition is **false**. It does not support an `else` branch:

```oxi
unless logged_in {
    println("Please log in")
}

unless valid {
    give error("bad input")
}
```

`unless condition { ... }` is equivalent to `if !(condition) { ... }`. Use `unless` when the "negative" case is the primary concern — it reads more naturally than negating a condition.

## Postfix Guards

Append `when` or `unless` after a statement for inline conditional execution. The statement only runs if the guard condition is met.

### `when` Guard

Executes the statement only if the condition is true:

```oxi
println("welcome") when logged_in
give x when x > 0
skip when i % 2 == 0
stop when count >= limit
```

### `unless` Guard

Executes the statement only if the condition is false:

```oxi
println("error") unless valid
```

### Supported Statement Types

Postfix guards work on:
- Expression statements: `println("hi") when x`
- `give` (return): `give x when x > 0`
- `skip` (continue): `skip when i % 2 == 0`
- `stop` (break): `stop when count >= limit`
- `=` (assign): `x = cached when cache_hit`
- `.field =` (dot-assign): `p.name = "Bob" when update`

Postfix guards do **not** work on:
- Declarations (`:=`)
- Loops (`each`, `repeat`)
- Blocks (`option`, `unless`, `choose`)

## Loops

### `each` — Iteration Loop

Iterate over arrays, strings, or any iterable:

```oxi
each item in [1, 2, 3] {
    println(item)
}
```

The loop variable is scoped to the loop body:

```oxi
each char in "hello" {
    println(char)
}
```

Combining `each` with `range()` for counted iteration:

```oxi
each i in range(5) {
    println(i)
}
```

### `repeat` — While Loop

`repeat when` loops while the condition remains true:

```oxi
i := 0
repeat when i < 5 {
    println(i)
    i++
}
```

Infinite loop (use `stop` to break out):

```oxi
repeat when True {
    println("running")
    stop when done
}
```

### Loop Control

- **`skip`**: Skips the rest of the current iteration and moves to the next (like `continue` in other languages):

  ```oxi
  each i in range(10) {
      skip when i % 2 == 0
      println(i)
  }
  ```

- **`stop`**: Terminates the loop entirely (like `break` in other languages):

  ```oxi
  each i in range(100) {
      stop when i >= 5
      println(i)
  }
  ```

Both `skip` and `stop` work with postfix guards for concise loop control.

## Block Styles

OxigenLang supports two ways to define blocks throughout the language.

### Brace-Based Blocks (Default)

The default style uses curly braces `{ }` to delimit blocks:

```oxi
each num in range(10) {
    option {
        num % 2 == 0 -> println("even"),
        println("odd")
    }
}
```

### Indentation-Based Blocks

Enable indentation mode by adding the `#[indent]` directive at the very top of your file:

```oxi
#[indent]

each num in range(10):
    option:
        num % 2 == 0 -> println("even"),
        println("odd")
```

In indentation mode:
- A colon `:` at the end of a line starts a new block (replaces `{`).
- Indentation levels are tracked — each deeper level opens a block.
- Returning to a previous indentation level closes the block (replaces `}`).
- The `#[indent]` directive is stripped from the source before parsing.

All language constructs that use blocks (`each`, `repeat`, `unless`, `option`, `choose`, `fun`, `struct`, `contains`) work with both block styles.

#### Indentation Mode Example

```oxi
#[indent]

fun greet(name <str>):
    println("Hello, " + name + "!")

struct Person:
    name <str>
    age <int>

Person contains:
    fun greet():
        println(name)

p := Person("Alice", 30)
p.greet()

each i in range(5):
    println(i) when i % 2 == 0
```

## Combining Control Flow

Control flow constructs can be nested and combined freely. Use `and` / `or` for compound conditions:

```oxi
logged_in := False
max_retries <int> = 9
retry <int>

repeat when True {
    println("Not logged in") when not logged_in
    println("Current number of retries: ", retry)

    unless logged_in {
        println("Currently not logged in")
    }

    option {
        retry == 8 -> logged_in := True
    }

    stop when logged_in or retry > max_retries
    retry++
}

println("Logged in") when logged_in == True
```

See also:
- [Pattern Matching](pattern_matching.md) — the `choose` statement for pattern-based branching
- [Functions](functions.md) — `give` (return) and function control flow
- [Operators](operators.md) — comparison and logical operators used in conditions
