# OxigenLang Type System

OxigenLang is dynamically typed by default, but offers optional type annotations that lock a variable's type for its lifetime in scope. This gives you explicit control over type safety and mutability.

## Supported Types

| Type    | Keyword | Zero Value | Example Values       |
|---------|---------|------------|----------------------|
| Integer | `int`   | `0`        | `42`, `-7`           |
| Float   | `float` | `0.0`      | `3.14`, `-0.5`       |
| String  | `str`   | `""`       | `"hello"`, `'world'` |
| Char    | `char`  | `\0`       | `` `a` ``, `` `Z` `` |
| Boolean | `bool`  | `False`    | `True`, `False`      |
| Array   | `array` | `[]`       | `[1, 2, 3]`          |
| Byte    | `byte`  | `0`        | `byte(65)`           |
| Uint    | `uint`  | `0`        | `uint(42)`           |
| Tuple   | `tuple` | `()`       | `(1, "hello")`       |
| Map     | `map`   | `{}`       | `{"a": 1}`           |
| Set     | `set`   | `set()`    | `set(1, 2, 3)`       |
| Struct  | struct name | —     | `Person("Alice", 30)` |

Structs are composite types with typed fields. They are defined with the `struct` keyword and have their own type identity (e.g., `type(p)` returns `"Person"`). Struct names can be used as type annotations to lock a variable to a specific struct type:

```oxi
struct Person { name <str> age <int> }

p <Person> := Person("Alice", 30)
p = Person("Bob", 25)
p = 42
```

Struct types support `as` declarations — the instance is created with all fields set to their zero values (e.g., `""` for `str`, `0` for `int`). See the [Structs](structs.md) guide for full details.

## Declaration Forms

OxigenLang offers four ways to declare variables, each with different mutability and type-safety semantics. See the [Variables and Assignments](variables.md) guide for a comprehensive walkthrough.

### Untyped (default)

Variables declared with `:=` have no type constraint. Their type can change freely on reassignment.

```oxi
x := 10
x := "hello"
```

### Typed Strict: `x <type> = value`

Declares a variable with a locked type **and** an immutable value. The value must already be the correct type — no conversion is performed.

```oxi
x <int> = 10
x <int> = "hello"
```

- Type is locked — cannot be reassigned to a different type.
- Value is immutable — `=` and `++`/`--` are blocked.
- Only `:=` (walrus) or explicit re-declaration can override the binding.

### Typed Walrus: `x <type> := value`

Declares a variable with a locked type but a **mutable** value. Attempts to convert the value to the target type at declaration time.

```oxi
x <int> := 3.9
x <str> := 42
x <bool> := 0
```

- Type is locked — reassignments must match the type.
- Value is mutable — `=`, `++`/`--`, and `:=` work (with type checking).

### Zero-Init: `x <type>` or `x as <type>`

Declares a typed variable initialized to its zero value. The value is mutable. The `as` keyword is optional — `x <type>` is the preferred convention.

```oxi
x <int>
y <str>
z <array>
p <Person>
```

## Reassignment Rules

### `=` (strict assign)

Only works on typed variables. Performs a strict type check — the value must already be the correct type.

```oxi
x <int> := 10
x = 20
x = "hello"

y := 10
y = 20
```

On immutable variables (declared with `<type> =`), `=` is blocked entirely:

```oxi
x <int> = 10
x = 20
```

### `:=` (walrus reassign)

For existing typed variables, `:=` performs a strict type check — no implicit conversion. The type is locked.

```oxi
x <int> := 10
x := 20
x := "hello"
```

For untyped variables, `:=` updates freely as before:

```oxi
x := 10
x := "hello"
```

### `++` / `--` (postfix)

Works on mutable integer variables. Blocked on immutable bindings.

```oxi
x <int> := 5
x++

y <int> = 5
y++
```

## Type Conversion Table

When using typed walrus declarations (`x <type> := value`), OxigenLang attempts these conversions:

| Target  | From int   | From float | From str   | From char | From bool |
|---------|------------|------------|------------|-----------|-----------|
| `int`   | identity   | truncate   | parse      | ord       | 0/1       |
| `float` | widen      | identity   | parse      | —         | 0.0/1.0   |
| `str`   | display    | display    | identity   | display   | display   |
| `char`  | chr        | —          | single chr | identity  | —         |
| `bool`  | truthy     | truthy     | truthy     | truthy    | identity  |
| `array` | —          | —          | chars      | —         | —         |
| `byte`  | 0-255      | —          | —          | as u8     | 0/1       |
| `uint`  | >= 0       | truncate   | parse      | —         | 0/1       |

A `—` means the conversion is not supported and will produce an error.

### Conversion Examples

```oxi
x <int> := 3.9
y <str> := 42
z <bool> := 0
w <float> := 10
c <char> := 65
b <byte> := `A`
u <uint> := "100"
a <array> := "hello"
```

## Indexing and Slicing

Arrays, strings, and tuples support indexing (`[i]`) and slicing (`[start:end]`). Maps support key-based indexing (`m[key]`). See the [Operators](operators.md) guide for full details.

```oxi
arr := [10, 20, 30, 40, 50]
arr[0]
arr[1:3]
arr[2:]
arr[:2]

"hello"[1:4]
(1, 2, 3)[0:2]
```

## In-Place Type Conversion

You can convert a variable's type by re-declaring it with a typed walrus:

```oxi
user_input := "42"
user_input <int> := user_input
user_input
```

After re-declaration the new type constraint is locked:

```oxi
user_input := "42"
user_input <int> := user_input
user_input := "hello"
```

## Shadowing

Typed variables can be shadowed in inner scopes. The outer binding is unaffected:

```oxi
x <int> = 10
each i in [1] {
    x <str> = "hello"
}
x
```

## Introspection

### `is_mut(variable)`

Returns `True` if the variable's **value** is mutable, `False` if it is immutable.

```oxi
a := 10
is_mut(a)

b <int> := 10
is_mut(b)

c <int> = 10
is_mut(c)

d <int>
is_mut(d)
```

### `is_type_mut(variable)`

Returns `True` if the variable's **type** can change (i.e., it has no type constraint), `False` if the type is locked.

```oxi
a := 10
is_type_mut(a)

b <int> := 10
is_type_mut(b)

c <int> = 10
is_type_mut(c)

d <int>
is_type_mut(d)
```

## Summary Table

| Declaration     | Value Mutable | Type Locked | `=` allowed | `++`/`--` allowed |
|-----------------|---------------|-------------|-------------|--------------------|
| `x := 10`       | yes           | no          | no          | yes                |
| `x <int> = 10`  | no            | yes         | no          | no                 |
| `x <int> := 10` | yes           | yes         | yes         | yes                |
| `x <int>`       | yes           | yes         | yes         | yes                |
| `x as <int>`    | yes           | yes         | yes         | yes                |

See also:
- [Variables and Assignments](variables.md) — practical guide to declaring and using variables
- [Data Types](data_types.md) — detailed description of each type
- [Structs](structs.md) — struct types and type annotations
- [Built-in Functions](builtins.md) — `type()`, `is_mut()`, `is_type_mut()`
