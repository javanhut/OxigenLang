# OxigenLang Data Types

OxigenLang supports a rich set of data types for building expressive programs. The language is dynamically typed by default, but offers optional type annotations to lock a variable's type. See the [Type System](type_system.md) guide for details on type annotations.

## Primitive Types

### Integer

64-bit signed integers. Supports standard arithmetic, comparison, and postfix increment/decrement.

```oxi
x := 42
y := -7
z := 0
```

Type keyword: `int`. Zero value: `0`.

### Float

64-bit floating-point numbers. Supports arithmetic and comparison operations.

```oxi
pi := 3.14
neg := -0.5
```

Type keyword: `float`. Zero value: `0.0`.

Floats and integers can be mixed in arithmetic — the result follows standard promotion rules.

### String

Unicode strings. Can be enclosed in double quotes or single quotes — both are equivalent:

```oxi
greeting := "hello"
name := 'world'
```

Type keyword: `str`. Zero value: `""` (empty string).

#### String Operations

- **Concatenation**: Use `+` to join strings:

  ```oxi
  full := "hello" + " " + "world"
  ```

- **String Interpolation**: Use `{}` inside any string to embed expressions. The expression is evaluated and its result is converted to a string automatically:

  ```oxi
  name := "world"
  println("hello {name}")

  x := 10
  y := 20
  println("{x} + {y} = {x + y}")
  ```

  Interpolation works with variables, function calls, arithmetic, and any valid expression:

  ```oxi
  age := 25
  println("Next year you will be {age + 1}")

  items := [1, 2, 3]
  println("There are {len(items)} items")

  println("Type: {type(42)}")
  ```

  Strings without `{}` remain plain strings — no special syntax is needed to opt out. Both double-quoted and single-quoted strings support interpolation.

- **Escape Sequences**: Strings support the following escape sequences:

  | Escape | Character |
  |--------|-----------|
  | `\n` | Newline |
  | `\t` | Tab |
  | `\r` | Carriage return |
  | `\\` | Backslash |
  | `\"` | Double quote (inside `"..."`) |
  | `\'` | Single quote (inside `'...'`) |
  | `\0` | Null character |

  ```oxi
  println("line one\nline two")
  println("col1\tcol2")
  println("she said \"hello\"")
  println('it\'s fine')
  ```

  Escape sequences work in both plain strings and interpolated strings.

- **Indexing**: Access individual characters by position (returns a string):

  ```oxi
  "hello"[0]
  "hello"[4]
  ```

- **Slicing**: Extract a substring with `[start:end]`:

  ```oxi
  "hello world"[0:5]
  "hello world"[6:]
  "hello world"[:5]
  ```

- **Length**: Use `len()` to get the number of characters:

  ```oxi
  len("hello")
  ```

- **To characters**: Use `chars()` to convert to an array of characters:

  ```oxi
  chars("abc")
  ```

- **To string**: Use `str()` to convert other types to their string representation:

  ```oxi
  str(42)
  str(True)
  ```

### Character

A single Unicode character, enclosed in backticks:

```oxi
letter := `a`
digit := `5`
symbol := `@`
```

Type keyword: `char`. Zero value: `\0` (null character).

Characters are distinct from single-character strings. Use `ord()` to get the Unicode codepoint and `chr()` to convert back:

```oxi
ord(`a`)
chr(97)
```

### Boolean

Boolean values are capitalized: `True` and `False`:

```oxi
active := True
done := False
```

Type keyword: `bool`. Zero value: `False`.

#### Truthiness

All values in OxigenLang have a truthiness. The following are **falsy**:
- `False`
- `None`
- `0` (integer zero)
- `0.0` (float zero)
- `""` (empty string)
- `\0` (null character)
- `[]` (empty array)
- `()` (empty tuple)
- `{}` (empty map)
- `set()` (empty set)
- Any `Error` value

Everything else is **truthy**, including struct instances.

### None

Represents the absence of a value:

```oxi
x := None
```

Functions that don't explicitly return a value produce `None`. `None` is falsy.

## Collection Types

### Array

Dynamic, ordered collection of values. Elements can be of any type:

```oxi
nums := [1, 2, 3]
mixed := [1, "hello", True, 3.14]
empty := []
```

Type keyword: `array`. Zero value: `[]`.

#### Array Operations

- **Indexing**: Access by position (zero-based):

  ```oxi
  arr := [10, 20, 30]
  arr[0]
  arr[2]
  ```

- **Index Assignment**: Update an element at a specific position:

  ```oxi
  arr := [10, 20, 30]
  arr[0] = 99
  arr[-1] = 77       // negative index counts from the end
  println(arr)        // [99, 20, 77]
  ```

- **Slicing**: Extract a sub-array with `[start:end]`:

  ```oxi
  arr := [10, 20, 30, 40, 50]
  arr[1:3]
  arr[2:]
  arr[:2]
  arr[:]
  ```

  Start and end can be any expression. Out-of-bounds values are clamped silently.

- **Push**: Append an element (returns a new array):

  ```oxi
  arr := [1, 2]
  arr := push(arr, 3)
  ```

- **First / Last / Rest**:

  ```oxi
  first([1, 2, 3])
  last([1, 2, 3])
  rest([1, 2, 3])
  ```

- **Length**: `len([1, 2, 3])` returns `3`.

- **Membership**: `has([1, 2, 3], 2)` returns `True`.

### Tuple

Fixed-size, immutable ordered collection. Created with parentheses or the `tuple()` built-in:

```oxi
point := (1, 2)
record := (1, "hello", True)
single := (42,)
empty := ()
```

Type keyword: `tuple`. Zero value: `()`.

Note: A single-element tuple requires a trailing comma to distinguish it from a grouped expression.

#### Tuple Operations

- **Indexing**: `(1, 2, 3)[0]` returns `1`.
- **Slicing**: `(1, 2, 3, 4)[1:3]` returns `(2, 3)`.
- **Concatenation**: `(1, 2) + (3, 4)` returns `(1, 2, 3, 4)`.
- **Length**: `len((1, 2))` returns `2`.
- **Membership**: `has((10, 20), 10)` returns `True`.

### Map

Key-value pairs, enclosed in curly braces. Keys and values can be any type:

```oxi
person := {"name": "Alice", "age": 30}
mixed := {1: "one", "two": 2}
empty := {}
```

Type keyword: `map`. Zero value: `{}`.

Note: An empty map `{}` and an empty block `{}` use the same syntax. The parser distinguishes them by context — at the expression level, `{}` is a map literal.

#### Map Operations

- **Indexing**: Access by key:

  ```oxi
  m := {"a": 1, "b": 2}
  m["a"]
  ```

- **Index Assignment**: Set or update a key directly using bracket syntax:

  ```oxi
  m <map>
  m["name"] = "Oxigen"
  m["version"] = "0.1.0"
  m["version"] = "0.2.0"    // updates existing key
  ```

  Both `=` and `:=` work for index assignment.

- **Insert**: Returns a new map with the key-value pair added or updated:

  ```oxi
  m := {"a": 1}
  m := insert(m, "b", 2)
  ```

- **Remove**: Returns a new map without the given key:

  ```oxi
  m := {"a": 1, "b": 2}
  m := remove(m, "a")
  ```

- **Keys / Values**:

  ```oxi
  keys({"a": 1, "b": 2})
  values({"a": 1, "b": 2})
  ```

- **Length**: `len({"a": 1})` returns `1`.
- **Membership**: `has({"a": 1}, "a")` returns `True` (checks keys).

### Set

An unordered collection of unique values. Created with the `set()` built-in:

```oxi
s := set(1, 2, 3)
duped := set(1, 1, 2, 2)
empty := set()
```

Type keyword: `set`. Zero value: `set()`.

Duplicates are automatically removed on creation.

#### Set Operations

- **Membership**: `has(set(1, 2, 3), 2)` returns `True`.
- **Remove**: `remove(set(1, 2, 3), 2)` returns `set(1, 3)`.
- **Length**: `len(set(1, 2, 3))` returns `3`.

### Byte

Unsigned 8-bit integer (0–255). Created with the `byte()` built-in:

```oxi
b := byte(65)
```

Type keyword: `byte`. Zero value: `0`.

Byte values can be created from integers (0–255), unsigned integers, and characters:

```oxi
byte(65)
byte(uint(200))
byte(`A`)
```

Values outside 0–255 produce an error.

### Uint

Unsigned 64-bit integer. Created with the `uint()` built-in:

```oxi
u := uint(42)
```

Type keyword: `uint`. Zero value: `0`.

Uint values can be created from non-negative integers, floats (truncated), strings (parsed), and bytes:

```oxi
uint(42)
uint(3.7)
uint("100")
uint(byte(255))
```

Negative values produce an error.

## Type Introspection

Use `type()` to get the string name of any value's type:

```oxi
type(42)
type("hello")
type(3.14)
type(`a`)
type(True)
type(None)
type([1, 2])
type((1, 2))
type({"a": 1})
type(set(1, 2))
type(byte(65))
type(uint(42))
```

For struct instances, `type()` returns the struct name instead of a generic type:

```oxi
struct Person { name <str> }
p := Person("Alice")
type(p)
```

Use `is_type()` to check a value's type directly against a type keyword:

```oxi
is_type(42, int)
is_type("hello", str)
is_type(3.14, float)
```

See also:
- [Variables and Assignments](variables.md) — how to declare variables of these types
- [Type System](type_system.md) — type annotations, conversions, and mutability
- [Operators](operators.md) — operations supported by each type
- [Built-in Functions](builtins.md) — all functions for working with these types
