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
z <float>           # shorthand for above
x = 20              # reassignment (typed variables only)
p <Person> := Person("Alice", 30)   # struct names work as type annotations
p <Person>          # zero-value struct instance
```

## Data Types

- **Integer**: `42`, `-7`
- **Float**: `3.14`, `-0.5`
- **String**: `"hello"`, `'world'`
- **Character**: `` `a` ``, `` `Z` `` (enclosed in backticks)
- **Boolean**: `True`, `False`
- **Array**: `[1, 2, 3]`, `["apple", "banana"]`
- **Byte**: `byte(65)` — unsigned 8-bit integer (0-255)
- **Uint**: `uint(42)` — unsigned 64-bit integer
- **Tuple**: `(1, "hello", True)` — fixed-size, immutable ordered collection
- **Map**: `{"key": "value", 1: True}` — key-value pairs
- **Set**: `set(1, 2, 3)` — unique unordered collection
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

## Indexing and Slicing

### Indexing

Access elements by position using `[index]`:

```oxi
arr := [10, 20, 30]
arr[0]           # 10
"hello"[1]       # "e"
(1, 2, 3)[2]     # 3
{"a": 1}["a"]    # 1
```

### Slicing

Extract a sub-range using `[start:end]`. Works on arrays, strings, and tuples:

```oxi
arr := [10, 20, 30, 40, 50]
arr[1:3]         # [20, 30]
arr[2:]          # [30, 40, 50]
arr[:2]          # [10, 20]
arr[:]           # [10, 20, 30, 40, 50] (full copy)

"hello world"[0:5]   # "hello"
(1, 2, 3, 4)[1:3]   # (2, 3)
```

Start and end can be any expression. Out-of-bounds values are clamped silently.

## Patterns and Choose

OxigenLang features a powerful pattern-matching system using the `pattern` and `choose` keywords.

### Defining Patterns

Patterns can be defined as top-level statements and referenced by name in `choose` blocks:

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

### Inline Patterns

Patterns can also be defined inline directly within `choose` arms:

```oxi
val := 42
choose val {
    pattern is_even(n) when n % 2 == 0 -> print("Even"),
    pattern is_large(n) when n > 100 -> print("Large"),
    else -> print("Neither")
}
```

You can mix pre-defined and inline patterns in the same `choose` block:

```oxi
pattern is_even(n) when n % 2 == 0

val := 42
choose val {
    is_even -> print("Even"),
    pattern is_large(n) when n > 100 -> print("Large"),
    else -> print("Neither")
}
```

## Structs

OxigenLang supports structs for grouping data with typed fields, inheritance, and methods.

### Defining and Using Structs

```oxi
struct Person {
    name <str>
    age <int>
}

# Positional instantiation
p := Person("Alice", 30)

# Named instantiation
p := Person { name: "Alice", age: 30 }

# Field access and mutation
println(p.name)    # Alice
p.age = 31
```

### Methods

Attach methods using `contains`. Fields are accessible by name inside methods (implicit self):

```oxi
Person contains {
    fun greet() { "Hello, " + name }
}

p := Person("Alice", 30)
println(p.greet())   # Hello, Alice
```

### Inheritance

A child struct inherits all parent fields and methods:

```oxi
struct American(Person) {
    nationality <str>
}

a := American("John", 25, "USA")
println(a.greet())   # Hello, John — inherited from Person
```

For full details, see the [Structs](structs.md) guide.

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
