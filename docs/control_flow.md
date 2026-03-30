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

`unless` runs a block only when the condition is **false**:

```oxi
unless logged_in {
    println("Please log in")
}

unless valid {
    give error("bad input")
}
```

`unless condition { ... }` is equivalent to `if !(condition) { ... }`. Use `unless` when the "negative" case is the primary concern — it reads more naturally than negating a condition.

### `unless ... then ...` Expression Form

Use `unless ... then ...` as an expression when you want a fallback value only when the condition is true:

```oxi
name := name.upper() unless name == None then "Guest"
result := "ok" unless status.code != 200 then "error: {status.code}"
```

This reads as "use the left expression unless the condition is true, then use the fallback instead."

### `unless ... then ...` Statement Form

Postfix `unless` can also take a `then` branch for a simple alternative statement:

```oxi
println("welcome") unless logged_in then println("Please log in")
```

This is the compact form of:

```oxi
if logged_in {
    println("Please log in")
} else {
    println("welcome")
}
```

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
println("welcome") unless logged_in then println("Please log in")
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

## Angle Forms and Error Flow

Oxigen uses angle forms for several related jobs:

- type annotations and zero-initialization in declaration position
- value constructors in expression position
- effect handlers and transformations in expression position
- entry points with `main` for code that runs only when executed directly

The same bracket family is used consistently, but meaning comes from position.

For the full model, including declaration-time types, constructors, normalization, and postfix effects in one place, see [Angle Forms](angle_forms.md).

### Declaration Position

After a binding name, `<...>` is a type annotation:

```oxi
name <str> := "Oxigen"
count <int>
result <Error || Value> := <type<Error || Value>>(read_file("config"))
```

This is where zero-initialization and typed walrus conversion live.

### Expression Position

At the start of an expression, `<...>` is a constructor or angle operator:

```oxi
<Error>("failed to initialize")
<Error<retry_error>>("retry failed")
<Value>("ok")
<type<Error || Value>>(read_file("config"))
<fail>("missing config")
```

After an already-completed expression, `<...>` is a postfix effect:

```oxi
read_file("config") <guard>("")
```

## Error Values

`<Error>(...)` constructs an error value. It does not automatically stop evaluation.

```oxi
err := <Error>("missing file")
tagged := <Error<network>>("connection lost")
```

Current public fields:

```oxi
err.msg
err.tag
```

Tagged errors give you lightweight error categories without declaring a custom error type:

```oxi
<Error<retry_error>>("retry failed")
<Error<parse>>("invalid header")
```

## Success Values

`<Value>(...)` constructs an explicit success wrapper.

```oxi
wrapped := <Value>("ok")
println(wrapped.value)
```

This is mainly useful together with `<type<Error || Value>>`.

## Expected-Result Normalization

Use `<type<Error || Value>>(expr)` when a call is expected to either succeed or fail and you want that to stay explicit instead of propagating immediately.

```oxi
file_result := <type<Error || Value>>(read_file("config"))
```

Behavior:

- if `expr` succeeds, the result becomes `Value(...)`
- if `expr` fails, the result becomes `Error(...)`
- the failure is normalized into a value instead of escaping immediately

You can include a tag in the `Error` side of the union. The tag acts as a default — it is applied to errors that do not already have their own tag:

```oxi
result := <type<Error<parse> || Value>>(parse_json(input))
// if parse_json fails with an untagged error, result.tag is "parse"
// if parse_json fails with its own tagged error, that tag is preserved
```

Examples:

```oxi
ok := <type<Error || Value>>("config loaded")
println(ok.value)

failed := <type<Error || Value>>(<fail>("missing config"))
println(failed.msg)
```

This also works in typed declarations:

```oxi
result <Error || Value> := <type<Error || Value>>(read_file("config"))
```

## Error Recovery with `guard`

Use `guard` when you want a runtime error to recover into a fallback expression.

Preferred angle form:

```oxi
name := read_name() <guard>("Guest")
```

Tagged filtering is also supported:

```oxi
name := load_profile() <guard<Error<retry_error>>>("Guest")
```

Compatibility keyword form:

```oxi
name := read_name() guard err -> "Guest"
message := fail "missing config" guard err -> err.msg
println(load()) guard err -> err.msg
```

Rules:

- `guard` catches real runtime errors only
- `guard` does not catch `None`
- if a tag filter is present, only matching tagged errors are handled
- unmatched errors continue propagating

When the keyword form binds an identifier, the bound error is only visible inside the fallback:

```oxi
result := fail "boom" guard err -> err.msg
```

## Standalone Logging with `log`

`<log>` is a standalone logging utility that writes timestamped, tagged messages to stderr.

```oxi
<log>("server started")
<log<Info>>("listening on port 8080")
<log<Error>>("connection refused")
<log<Error<network>>>("timeout after 30s")
```

Output format: `YYYY-MM-DD HH:MM:SS: [TAG] message`

Tags are uppercased automatically. When no tag is given, the output omits the tag bracket. `<log>` returns `None`.

For the full reference, see [Angle Forms](angle_forms.md).

## Producing Failures with `fail`

Use `<fail>(...)` to propagate a runtime error.

```oxi
<fail>("bad input")
<fail>(<Error<retry_error>>("retry failed"))
```

`<fail>` accepts either:

- a plain value, which is converted to an error message
- an existing `Error` value, including tagged errors

The keyword form remains supported:

```oxi
fail "bad input"
```

## Error Arms in `option`

`option` can include an explicit error fallback arm:

```oxi
result := option {
    risky_call() -> {},
    <Error> -> "fallback"
}
```

This arm runs when evaluating a condition, selected body, or default arm produces a runtime error.

It works well with tagged errors and explicit failure:

```oxi
result := option {
    connected -> fetch_data(),
    <Error> -> <fail>("request failed")
}
```

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
