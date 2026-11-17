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

Reassign untyped variables with `:=` or `=`. Either one can change the type:

```oxi
x := 10
x := "hello"
x = [1, 2, 3]
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
- Value is immutable — `=`, `++`, `--`, and compound assignment (`+=`, `-=`, `*=`, `/=`) are all blocked after declaration.
- `:=` can still replace the value with another of the same type, as an explicit override. The variable stays immutable afterwards.
- Immutability applies to the variable, not to the value: if the value is an array, map, set or struct, its contents can still change (`x[0] = 5`, `push(x, 6)`). Use [`lock`](#lock-a-value-that-can-never-change) for a value that can never change.

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

The type is locked — subsequent reassignments must match the type. The value is mutable — `=`, `:=`, `++`/`--`, and `+=`/`-=`/`*=`/`/=` all work (with type checking).

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

### `=` (assign)

Works on untyped variables and on typed mutable variables. On a typed variable the value must already be the correct type — no conversion:

```oxi
x <int> := 10
x = 20

y := 10
y = "twenty"
```

Blocked on immutable and locked variables:

```oxi
x <int> = 10
x = 20
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

Adds or subtracts 1 and returns the old value. Works on mutable numeric variables, and also on an index (`m[k]++`) or a field (`p.count++`). Blocked on immutable bindings:

```oxi
x <int> := 5
x++
x--

y <int> = 5
y++
```

### `+=` / `-=` / `*=` / `/=` (compound assignment)

`x += e` updates `x` like `x = x + e`, and the same holds for `-=`, `*=` and `/=`. Blocked on immutable bindings, and a typed variable keeps its type:

```oxi
x <int> := 5
x += 2
x *= 3

y <int> = 5
y += 1
```

Both forms also work on an index or a field (`counts[key] += 1`, `player.health -= 10`); the key must already exist in a map. See [Operators](operators.md#compound-assignment) for details.

## `lock`: a Value That Can Never Change

`x <int> = 10` makes the *variable* immutable, but collections are shared by reference, so another name for the same array can still change it. `lock` makes both the variable and its value permanently read-only:

```oxi
arr := [1, 2, 3]
lock static_arr = arr

push(arr, 4)            // fine: arr is [1, 2, 3, 4]
println(static_arr)     // [1, 2, 3]
push(static_arr, 4)     // error: push() cannot change a locked array
```

- **The value is copied, then locked.** `lock` takes its own copy, so the original (`arr`) stays changeable and later changes to it don't show up in `static_arr`.
- **The lock is deep.** Arrays, maps, sets and structs inside the value are locked too: `lock grid = [[1, 2], [3, 4]]` refuses `grid[0][1] = 9` and `push(grid[0], 5)`.
- **The lock travels with the value.** Passing it to a function or giving it another name (`ys := static_arr`) doesn't unlock it.
- **The variable can never change.** `=`, `:=`, `++`, `--`, `+=`, `-=`, `*=` and `/=` on it are all compile errors, and `lock x = ...` can't redeclare an existing name.
- **Reading is unaffected.** Indexing, `len`, `has`, `each` and other reads work as normal.

To get a changeable version, use `copy`:

```oxi
lock defaults = {"width": 80, "height": 24}
settings := copy(defaults)
settings["width"] = 120
```

`copy` is shallow, so collections nested inside a locked value stay locked in the copy.

A lock can carry a type, which converts the value like a typed declaration:

```oxi
lock limit <int> = 10
```

`lock` is only a keyword at the start of `lock name = ...`, so `lock` still works as an ordinary variable name.

Use `lock` for constants and lookup tables (`lock DIRECTIONS = [(0, 1), (1, 0), (0, -1), (-1, 0)]`), and for data you hand to code that must not change it.

## Unpacking

Arrays and tuples can be unpacked into multiple variables in a single `:=` declaration:

```oxi
x, y := [1, 2]
a, b, c := [10, 20, 30]
first, second := (100, 200)
```

This works with any expression that produces an array or tuple:

```oxi
introduce strings
key, val := strings.split("name=Alice", "=")

fun get_pair() { [42, "hello"] }
num, msg := get_pair()
```

The number of names on the left must match the number of elements on the right — a mismatch produces an error:

```oxi
x, y := [1, 2, 3]   // error: expected 2 values, got 3
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

| Declaration        | `=`, `++`, `+=` | `:=` override | Type Locked | Contents changeable |
|--------------------|-----------------|---------------|-------------|---------------------|
| `x := 10`          | yes             | yes           | no          | yes                 |
| `x <int> = 10`     | no              | yes           | yes         | yes                 |
| `x <int> := 10`    | yes             | yes           | yes         | yes                 |
| `x <int>`          | yes             | yes           | yes         | yes                 |
| `x as <int>`       | yes             | yes           | yes         | yes                 |
| `lock x = v`       | no              | no            | if typed    | no                  |

"Contents changeable" is about arrays, maps, sets and structs: whether `x[0] = 5`, `push(x, 6)` or `x.field = 1` can change the value.

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

lock d = [1, 2]
is_mut(d)
is_type_mut(d)
```

`is_mut` is `False` for both `c` and `d`.

See the [Type System](type_system.md) guide for full details on type annotations, conversions, and mutability semantics. See [Built-in Functions](builtins.md) for the full list of introspection functions.
