# OxigenLang Syntax Guide

OxigenLang provides a modern and flexible syntax for writing clean and expressive code.

## Variables and Assignments

Variables are bound using the walrus operator `:=`:

```oxi
x := 10
y := 5.5
name := "OxigenLang"
is_active := True
```

OxigenLang supports dynamic typing by default, meaning a variable's type can change at runtime. You can optionally add type annotations to lock a variable's type. See the [Type System](type_system.md) guide for full details.

### Typed Variables

```oxi
x <int> = 10        # immutable value, locked type (strict — no conversion)
y <int> := 3.9      # mutable value, locked type (walrus — converts to 3)
z as <float>        # mutable value, locked type (zero value: 0.0)
x = 20              # reassignment (typed variables only)
```

## Data Types

- **Integer**: `42`, `-7`
- **Float**: `3.14`, `-0.5`
- **String**: `"hello"`, `'world'`
- **Character**: `` `a` ``, `` `Z` `` (enclosed in backticks)
- **Boolean**: `True`, `False`
- **Array**: `[1, 2, 3]`, `["apple", "banana"]`
- **None**: Represents the absence of a value.

## Control Flow

### If Expressions

```oxi
if x > 10 {
    print("Large")
} else {
    print("Small")
}
```

### Loops

#### `each` Loop

Iterate over arrays or strings:

```oxi
each item in [1, 2, 3] {
    print(item)
}
```

#### `repeat` Loop

Equivalent to a `while` loop:

```oxi
i := 0
repeat when i < 5 {
    print(i)
    i++
}
```

#### Loop Control

- `skip`: Skips the rest of the current iteration (like `continue`).
- `stop`: Terminates the loop (like `break`).

## Patterns and Choose

OxigenLang features a powerful pattern-matching system using the `pattern` and `choose` keywords.

### Defining Patterns

```oxi
pattern is_even(n) when n % 2 == 0
pattern is_large(n) when n > 100
```

### Choosing Based on Patterns

```oxi
val := 42
choose val {
    is_even -> print("Even"),
    is_large -> print("Large"),
    else -> print("Neither")
}
```

## Block Styles

OxigenLang supports two different ways to define blocks:

### Brace-based Blocks (Default)

```oxi
if x > 0 {
    print(x)
}
```

### Indentation-based Blocks (Python-style)

Enable indentation mode by adding the `#[indent]` directive at the top of your file:

```oxi
#[indent]
if x > 0:
    print(x)
```

In indentation mode, a colon `:` at the end of a line starts a new block, and subsequent lines must be indented. Returning to a previous indentation level closes the block.
