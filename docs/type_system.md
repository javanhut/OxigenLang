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
| Struct  | —       | —          | `Person("Alice", 30)` |

Structs are composite types with typed fields. They are defined with the `struct` keyword and have their own type identity (e.g., `type(p)` returns `"Person"`). See the [Structs](structs.md) guide for full details.

## Declaration Forms

### Untyped (default)

Variables declared with `:=` have no type constraint. Their type can change freely on reassignment.

```oxi
x := 10
x := "hello"  # valid — type can change
```

### Typed Strict: `x <type> = value`

Declares a variable with a locked type **and** an immutable value. The value must already be the correct type — no conversion is performed.

```oxi
x <int> = 10       # ok
x <int> = "hello"  # error: type mismatch
```

- Type is locked — cannot be reassigned to a different type.
- Value is immutable — `=` and `++`/`--` are blocked.
- Only `:=` (walrus) or explicit re-declaration can override the binding.

### Typed Walrus: `x <type> := value`

Declares a variable with a locked type but a **mutable** value. Attempts to convert the value to the target type at declaration time.

```oxi
x <int> := 3.9     # x is 3 (float truncated to int)
x <str> := 42      # x is "42" (int converted to string)
x <bool> := 0      # x is False (0 is falsy)
```

- Type is locked — reassignments must match the type.
- Value is mutable — `=`, `++`/`--`, and `:=` work (with type checking).

### As-Declare: `x as <type>`

Declares a typed variable initialized to its zero value. The value is mutable.

```oxi
x as <int>    # x is 0
x as <str>    # x is ""
x as <array>  # x is []
```

## Reassignment Rules

### `=` (strict assign)

Only works on typed variables. Performs a strict type check — the value must already be the correct type.

```oxi
x <int> := 10
x = 20          # ok — same type
x = "hello"     # error: type mismatch

y := 10
y = 20          # error: = requires typed variable, use :=
```

On immutable variables (declared with `<type> =`), `=` is blocked entirely:

```oxi
x <int> = 10
x = 20          # error: cannot reassign immutable variable
```

### `:=` (walrus reassign)

For existing typed variables, `:=` performs a strict type check — no implicit conversion. The type is locked.

```oxi
x <int> := 10
x := 20         # ok — same type
x := "hello"    # error: type mismatch, locked to INTEGER
```

For untyped variables, `:=` updates freely as before:

```oxi
x := 10
x := "hello"    # ok — no type constraint
```

### `++` / `--` (postfix)

Works on mutable integer variables. Blocked on immutable bindings.

```oxi
x <int> := 5
x++              # ok — x is now 6

y <int> = 5
y++              # error: cannot mutate immutable variable
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
| `array` | —          | —          | —          | —         | —         |

A `—` means the conversion is not supported and will produce an error.

## In-Place Type Conversion

You can convert a variable's type by re-declaring it with a typed walrus:

```oxi
user_input := "42"
user_input <int> := user_input    # converts "42" to 42
user_input                         # 42 (INTEGER)
```

After re-declaration the new type constraint is locked:

```oxi
user_input := "42"
user_input <int> := user_input
user_input := "hello"             # error: type mismatch, locked to INTEGER
```

## Shadowing

Typed variables can be shadowed in inner scopes. The outer binding is unaffected:

```oxi
x <int> = 10
each i in [1] {
    x <str> = "hello"    # shadows outer x in this scope
}
x                         # still 10 (INTEGER)
```

## Introspection

### `is_mut(variable)`

Returns `True` if the variable's **value** is mutable, `False` if it is immutable.

```oxi
a := 10             # untyped — mutable
is_mut(a)           # True

b <int> := 10       # typed walrus — mutable value
is_mut(b)           # True

c <int> = 10        # typed strict — immutable value
is_mut(c)           # False

d as <int>          # as-declare — mutable value
is_mut(d)           # True
```

### `is_type_mut(variable)`

Returns `True` if the variable's **type** can change (i.e., it has no type constraint), `False` if the type is locked.

```oxi
a := 10             # untyped — type can change
is_type_mut(a)      # True

b <int> := 10       # typed — type is locked
is_type_mut(b)      # False

c <int> = 10        # typed — type is locked
is_type_mut(c)      # False

d as <int>          # typed — type is locked
is_type_mut(d)      # False
```

## Summary Table

| Declaration     | Value Mutable | Type Locked | `=` allowed | `++`/`--` allowed |
|-----------------|---------------|-------------|-------------|--------------------|
| `x := 10`       | yes           | no          | no          | yes                |
| `x <int> = 10`  | no            | yes         | no          | no                 |
| `x <int> := 10` | yes           | yes         | yes         | yes                |
| `x as <int>`    | yes           | yes         | yes         | yes                |
