# OxigenLang Variables and Assignments

OxigenLang provides several ways to declare and assign variables, giving you explicit control over mutability and type safety.

## Untyped Variables

Variables are declared using the walrus operator `:=`. Untyped variables have no type constraint — their type can change freely on reassignment:

```oxi
x := 10
name := "OxigenLang"
is_active := True
pi := 3.14
```

Reassign untyped variables with `:=`:

```oxi
x := 10
x := "hello"
x := [1, 2, 3]
```

Untyped variables cannot use `=` for reassignment — `=` is reserved for typed variables:

```oxi
x := 10
x = 20
```

## Typed Variables

You can add type annotations to lock a variable's type. OxigenLang offers three forms of typed declaration, each with different mutability semantics.

### Typed Strict: `x <type> = value`

Declares a variable with a locked type **and** an immutable value. The value must already be the correct type — no conversion is performed:

```oxi
x <int> = 10
y <str> = "hello"
```

- Type is locked — cannot hold a different type.
- Value is immutable — `=`, `:=`, `++`, and `--` are all blocked after declaration.

### Typed Walrus: `x <type> := value`

Declares a variable with a locked type but a **mutable** value. Attempts to convert the value to the target type at declaration time:

```oxi
x <int> := 3.9
y <str> := 42
z <bool> := 0
```

- `x` becomes `3` (float truncated to int).
- `y` becomes `"42"` (int displayed as string).
- `z` becomes `False` (0 is falsy).

The type is locked — subsequent reassignments must match the type. The value is mutable — `=`, `:=`, `++`/`--` all work (with type checking).

### Zero-Init: `x <type>` or `x as <type>`

Declares a typed variable initialized to its **zero value**. The value is mutable. The `as` keyword is optional — `x <type>` is the preferred convention:

```oxi
x <int>
y <str>
z <array>
p <Person>
```

| Type     | Zero Value |
|----------|------------|
| `int`    | `0`        |
| `float`  | `0.0`      |
| `str`    | `""`       |
| `char`   | `\0`       |
| `bool`   | `False`    |
| `array`  | `[]`       |
| `byte`   | `0`        |
| `uint`   | `0`        |
| `tuple`  | `()`       |
| `map`    | `{}`       |
| `set`    | `set()`    |
| Struct   | Instance with all fields at their zero values |

## Reassignment Rules

### `=` (strict assign)

Only works on **typed mutable** variables. The value must already be the correct type — no conversion:

```oxi
x <int> := 10
x = 20
```

Blocked on immutable variables:

```oxi
x <int> = 10
x = 20
```

Blocked on untyped variables:

```oxi
y := 10
y = 20
```

### `:=` (walrus reassign)

For existing typed variables, `:=` performs a strict type check — no implicit conversion. The type remains locked:

```oxi
x <int> := 10
x := 20
```

For untyped variables, `:=` updates freely:

```oxi
x := 10
x := "hello"
```

### `++` / `--` (postfix)

Works on mutable integer variables. Blocked on immutable bindings:

```oxi
x <int> := 5
x++
x--

y <int> = 5
y++
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

## Summary Table

| Declaration     | Value Mutable | Type Locked | `=` allowed | `++`/`--` allowed |
|-----------------|---------------|-------------|-------------|--------------------|
| `x := 10`       | yes           | no          | no          | yes                |
| `x <int> = 10`  | no            | yes         | no          | no                 |
| `x <int> := 10` | yes           | yes         | yes         | yes                |
| `x <int>`       | yes           | yes         | yes         | yes                |
| `x as <int>`    | yes           | yes         | yes         | yes                |

## Introspection

Use `is_mut()` and `is_type_mut()` to inspect a variable's mutability at runtime:

```oxi
a := 10
is_mut(a)
is_type_mut(a)

b <int> := 10
is_mut(b)
is_type_mut(b)

c <int> = 10
is_mut(c)
is_type_mut(c)
```

See the [Type System](type_system.md) guide for full details on type annotations, conversions, and mutability semantics. See [Built-in Functions](builtins.md) for the full list of introspection functions.
