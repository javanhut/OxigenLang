# OxigenLang Pattern Matching

OxigenLang features a powerful pattern-matching system using the `pattern` and `choose` keywords. Patterns define named, reusable conditions that can be matched against values.

## Defining Patterns

A pattern is a named condition with parameters. Define patterns at the top level of your program using the `pattern` keyword:

```oxi
pattern is_even(n) when n % 2 == 0
pattern is_odd(n) when n % 2 != 0
pattern is_positive(n) when n > 0
pattern is_large(n) when n > 100
```

A pattern definition has three parts:
1. **Name**: An identifier for the pattern (e.g., `is_even`).
2. **Parameters**: One or more parameter names in parentheses (e.g., `(n)`). The first parameter receives the subject value from `choose`.
3. **Condition**: A `when` clause with a boolean expression that references the parameters.

Patterns are registered globally and can be referenced by name in any `choose` block that follows.

## The `choose` Statement

`choose` evaluates a subject expression against one or more pattern arms. The first arm whose pattern matches is executed:

```oxi
val := 42
choose val {
    is_even -> println("Even"),
    is_odd -> println("Odd")
}
```

### How Matching Works

When `choose val { is_even -> ... }` is evaluated:
1. The subject value (`val`, which is `42`) is bound to the pattern's parameter (`n`).
2. The pattern's condition (`n % 2 == 0`) is evaluated with `n = 42`.
3. If the condition is truthy, the arm's body executes.
4. If not, the next arm is tried.

Arms are evaluated top to bottom. The first matching arm wins — subsequent arms are skipped.

### The `else` Arm

Use `else` as a catch-all default arm that matches when no other pattern does:

```oxi
val := 42
choose val {
    is_large -> println("Large"),
    else -> println("Not large")
}
```

If no arm matches and there is no `else`, the `choose` statement produces no value.

### Arm Bodies

Each arm's body is an expression. For multi-statement bodies, use a block:

```oxi
choose val {
    is_even -> {
        println("Found an even number")
        println("Value:", val)
    },
    is_odd -> println("Odd")
}
```

## Inline Patterns

Patterns can be defined directly within `choose` arms instead of as separate top-level statements. This is useful for one-off conditions:

```oxi
val := 42
choose val {
    pattern is_even(n) when n % 2 == 0 -> println("Even"),
    pattern is_large(n) when n > 100 -> println("Large"),
    else -> println("Neither")
}
```

Inline patterns follow the same syntax as top-level patterns but are written inside the arm before the `->`.

## Mixing Pre-Defined and Inline Patterns

You can freely mix pre-defined (top-level) and inline patterns in the same `choose` block:

```oxi
pattern is_even(n) when n % 2 == 0

val := 42
choose val {
    is_even -> println("Even"),
    pattern is_large(n) when n > 100 -> println("Large"),
    else -> println("Neither")
}
```

## Pattern Matching in Loops

Patterns are especially useful when iterating over collections:

```oxi
pattern is_even(n) when n % 2 == 0
pattern is_odd(n) when n % 2 != 0

each num in range(10) {
    choose num {
        is_even -> println(num, "is even"),
        is_odd -> println(num, "is odd")
    }
}
```

## Patterns with Multiple Parameters

Patterns can define multiple parameters, though only the first receives the `choose` subject:

```oxi
pattern in_range(n) when n >= 0
pattern is_adult(age) when age >= 18
```

## Practical Example: Classification

```oxi
pattern is_negative(n) when n < 0
pattern is_zero(n) when n == 0
pattern is_small(n) when n > 0
pattern is_large(n) when n > 100

nums := [-5, 0, 42, 150]
each n in nums {
    choose n {
        is_negative -> println(n, "is negative"),
        is_zero -> println(n, "is zero"),
        is_large -> println(n, "is large (>100)"),
        is_small -> println(n, "is small positive"),
        else -> println(n, "unclassified")
    }
}
```

Note: Arm order matters. In the example above, `is_large` is checked before `is_small` because a large number would also match `is_small` (both have `n > 0`). Place more specific patterns first.

## Indentation Mode

Pattern matching works with both block styles:

```oxi
#[indent]

pattern is_even(n) when n % 2 == 0
pattern is_odd(n) when n % 2 != 0

each num in range(10):
    choose num:
        is_even -> println(num, "is even")
        is_odd -> println(num, "is odd")
```

## `choose` vs `option`

Both `choose` and `option` provide branching, but they serve different purposes:

| Feature     | `choose`                           | `option`                          |
|-------------|-------------------------------------|-----------------------------------|
| Purpose     | Match a value against named patterns | Evaluate boolean conditions       |
| Subject     | Takes an explicit subject value     | Each arm has its own condition     |
| Arms        | Pattern names or inline patterns    | Boolean expressions               |
| Default     | `else ->`                           | Last arm without `->`             |
| Best for    | Classifying a single value          | Multi-way conditionals            |

Use `choose` when you want to classify a value against named conditions. Use `option` when you have independent boolean conditions.

See also:
- [Control Flow](control_flow.md) — `option`, `unless`, and other conditionals
- [Data Types](data_types.md) — types that can be used as `choose` subjects
