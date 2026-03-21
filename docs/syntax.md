# OxigenLang Syntax Quick Reference

This is a concise reference for OxigenLang syntax. For detailed explanations, follow the links to the individual guides.

## Variables — [Full Guide](variables.md)

```oxi
x := 10                    # untyped, mutable
x <int> = 10               # typed, immutable value
x <int> := 10              # typed, mutable value
x as <int>                 # typed, zero-value init
x <int>                    # shorthand for above
```

## Data Types — [Full Guide](data_types.md)

| Type    | Example                    | Type Keyword |
|---------|----------------------------|--------------|
| Integer | `42`, `-7`                 | `int`        |
| Float   | `3.14`, `-0.5`             | `float`      |
| String  | `"hello"`, `'world'`       | `str`        |
| Char    | `` `a` ``, `` `Z` ``      | `char`       |
| Boolean | `True`, `False`            | `bool`       |
| Array   | `[1, 2, 3]`               | `array`      |
| Tuple   | `(1, "hello", True)`       | `tuple`      |
| Map     | `{"key": "value"}`         | `map`        |
| Set     | `set(1, 2, 3)`             | `set`        |
| Byte    | `byte(65)`                 | `byte`       |
| Uint    | `uint(42)`                 | `uint`       |
| None    | `None`                     | —            |

## Operators — [Full Guide](operators.md)

```oxi
x + y    x - y    x * y    x / y    x % y       # arithmetic
x == y   x != y   x < y    x > y    x <= y  x >= y  # comparison
!x       not x    -x                             # prefix
x++      x--                                     # postfix
arr[0]   arr[1:3]   obj.field                    # access
```

## Control Flow — [Full Guide](control_flow.md)

```oxi
option { cond -> value, default }                # conditional expression
option { cond, true_val, false_val }             # ternary shorthand
unless condition { body }                        # inverse conditional
println("hi") when condition                     # postfix when guard
println("hi") unless condition                   # postfix unless guard
each item in collection { body }                 # iteration
repeat when condition { body }                   # while loop
skip                                             # continue
stop                                             # break
```

## Functions — [Full Guide](functions.md)

```oxi
fun name(a, b) { a + b }                        # named function
f := fun(x) { x * 2 }                           # anonymous function
fun add(a <int>, b <int>) { a + b }             # typed parameters
give value                                       # return
give value when condition                        # conditional return
```

## Pattern Matching — [Full Guide](pattern_matching.md)

```oxi
pattern is_even(n) when n % 2 == 0              # define pattern

choose val {                                      # match against patterns
    is_even -> println("even"),
    pattern is_odd(n) when n % 2 != 0 -> println("odd"),
    else -> println("fallback")
}
```

## Structs — [Full Guide](structs.md)

```oxi
struct Person { name <str>  age <int> }          # definition
p := Person("Alice", 30)                         # positional
p := Person { name: "Alice", age: 30 }           # named
p.name                                           # field access
p.age = 31                                       # field mutation

Person contains {                                # methods
    fun greet() { "Hello, " + name }
}

struct Student(Person) { school <str> }          # inheritance
```

## Type System — [Full Guide](type_system.md)

```oxi
x <int> = 10               # strict (immutable, no conversion)
x <int> := 3.9             # walrus (mutable, converts to 3)
x as <int>                 # as-declare (zero value)
type(x)                    # type introspection
is_mut(x)                  # value mutability check
is_type_mut(x)             # type mutability check
```

## Block Styles

```oxi
each i in [1, 2, 3] {      # brace-based (default)
    println(i)
}
```

```oxi
#[indent]                   # indentation-based
each i in [1, 2, 3]:
    println(i)
```

## Built-in Functions — [Full Guide](builtins.md)

| Category     | Functions                                          |
|--------------|----------------------------------------------------|
| I/O          | `print`, `println`                                 |
| Collections  | `len`, `push`, `first`, `last`, `rest`, `has`      |
| Iteration    | `range`                                            |
| Conversion   | `ord`, `chr`, `str`, `chars`                       |
| Constructors | `byte`, `uint`, `set`, `tuple`                     |
| Map          | `keys`, `values`, `insert`, `remove`               |
| Introspection| `type`, `is_mut`, `is_type_mut`                    |
