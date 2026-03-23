# OxigenLang Built-in Functions

OxigenLang comes with a standard library of built-in functions for I/O, collection manipulation, type conversion, and introspection.

## I/O

### `print(arg1, arg2, ...)`

Prints the string representation of all provided arguments to standard output, separated by spaces, followed by a newline.

```oxi
print("Hello", 42, True)
```

Accepts any number of arguments of any type.

### `println(arg1, arg2, ...)`

Identical to `print` — prints arguments separated by spaces, followed by a newline.

```oxi
println("Welcome to OxigenLang")
println("Name:", name, "Age:", age)
```

Both `print` and `println` return `None`.

### `error(value)`

Creates a runtime error using the string form of `value`.

```oxi
error("missing file")
error(404)
```

This is primarily useful for compatibility.

Preferred modern forms:

```oxi
<Error>("missing file")               // construct an error value
<Error<network>>("connection lost")   // construct a tagged error value
<fail>("missing file")                // propagate a runtime error
<fail>(<Error<network>>("timeout"))   // propagate a tagged error
```

Use:

- `error(...)` for compatibility with older Oxigen code
- `<Error>(...)` when you want an explicit error value
- `<fail>(...)` when you want that error to propagate immediately

## Collection Operations

### `len(collection)`

Returns the number of elements in a collection or the number of characters in a string.

```oxi
len("OxigenLang")
len([1, 2, 3])
len((1, 2))
len({"a": 1})
len(set(1, 2, 3))
```

**Supported types:** String, Array, Tuple, Map, Set.

### `push(array, element)`

Returns a **new** array with the element appended to the end. The original array is not modified.

```oxi
arr := [1, 2]
arr := push(arr, 3)
println(arr)
```

**Argument types:** First argument must be an Array.

### `first(array)`

Returns the first element of an array, or `None` if the array is empty.

```oxi
first([1, 2, 3])
first([])
```

**Argument types:** Array only.

### `last(array)`

Returns the last element of an array, or `None` if the array is empty.

```oxi
last([1, 2, 3])
last([])
```

**Argument types:** Array only.

### `rest(array)`

Returns a **new** array containing all elements except the first one, or `None` if the array is empty.

```oxi
rest([1, 2, 3])
rest([42])
rest([])
```

**Argument types:** Array only.

### `has(collection, value)`

Returns `True` if the collection contains the value. The meaning of "contains" depends on the collection type:
- **Array/Tuple**: checks if any element equals the value.
- **Set**: checks if the value is a member.
- **Map**: checks if the value is a **key** (not a value).

```oxi
has([1, 2, 3], 2)
has(set(1, 2, 3), 5)
has({"a": 1}, "a")
has((10, 20), 10)
```

**Supported types:** Array, Tuple, Set, Map.

## Iteration

### `range(n)`

Returns an array of integers from `0` up to (but not including) `n`.

```oxi
range(5)
range(0)
range(1)
```

Commonly used with `each` for counted iteration:

```oxi
each i in range(10) {
    println(i)
}
```

**Argument types:** Integer only. Negative values produce an empty array.

## Type Conversion

### `int(value)`

Converts a value to an integer. Parses strings, truncates floats, converts booleans to 0/1, and converts chars to their Unicode codepoint.

```oxi
int("42")       // 42
int("3.14")     // 3
int(3.99)       // 3
int(True)       // 1
int(False)      // 0
int(`A`)        // 65
int(byte(200))  // 200
int(uint(100))  // 100
```

**Supported types:** String, Float, Boolean, Char, Byte, Uint, Integer (identity).

### `float(value)`

Converts a value to a float. Parses strings, converts integers, and converts booleans to 0.0/1.0.

```oxi
float("3.14")   // 3.14
float(42)       // 42.0
float(True)     // 1.0
float(byte(200))// 200.0
float(uint(100))// 100.0
```

**Supported types:** String, Integer, Boolean, Byte, Uint, Float (identity).

### `str(value)`

Converts a value to its string representation. Works on integers, floats, characters, booleans, strings, bytes, and unsigned integers.

```oxi
str(42)
str(3.14)
str(`a`)
str(True)
str(byte(65))
str(uint(100))
```

### `ord(char)`

Returns the integer value (Unicode codepoint) of a character.

```oxi
ord(`a`)
ord(`Z`)
ord(`@`)
```

**Argument types:** Char only.

### `chr(integer)`

Returns the character corresponding to the provided integer (Unicode codepoint). Produces an error if the value is out of range or not a valid Unicode codepoint.

```oxi
chr(97)
chr(65)
chr(9731)
```

**Argument types:** Integer only. Valid range: 0 to 0x10FFFF.

### `chars(string)`

Converts a string into an array of its individual characters.

```oxi
chars("abc")
chars("hello")
chars("")
```

**Argument types:** String only.

## Type Constructors

### `byte(value)`

Creates a byte (unsigned 8-bit integer) from the given value. Accepted sources:
- **Integer** (0–255): `byte(65)` returns byte `65`.
- **Uint** (0–255): `byte(uint(200))` returns byte `200`.
- **Char**: `byte(`A`)` returns byte `65`.
- **Byte**: `byte(byte(65))` returns byte `65` (identity).

```oxi
byte(65)
byte(0)
byte(255)
byte(`A`)
```

Values outside 0–255 produce an error.

### `uint(value)`

Creates an unsigned 64-bit integer from the given value. Accepted sources:
- **Integer** (non-negative): `uint(42)` returns uint `42`.
- **Float** (non-negative, truncated): `uint(3.7)` returns uint `3`.
- **String** (parsed): `uint("100")` returns uint `100`.
- **Byte**: `uint(byte(255))` returns uint `255`.
- **Uint**: `uint(uint(42))` returns uint `42` (identity).

```oxi
uint(42)
uint(3.7)
uint("100")
uint(byte(255))
```

Negative values and unparseable strings produce an error.

### `set(values...)`

Creates a set from the given values. Duplicates are removed automatically.

```oxi
set(1, 2, 3)
set(1, 1, 2, 2)
set()
```

Accepts any number of arguments of any type.

### `tuple(values...)`

Creates a tuple from the given values.

```oxi
tuple(1, 2, 3)
tuple("a", True, 42)
tuple()
```

Accepts any number of arguments of any type. This is equivalent to using the literal syntax `(1, 2, 3)`.

## Map Operations

### `keys(map)`

Returns an array of all keys in the map.

```oxi
keys({"a": 1, "b": 2})
keys({})
```

**Argument types:** Map only.

### `values(map)`

Returns an array of all values in the map.

```oxi
values({"a": 1, "b": 2})
values({})
```

**Argument types:** Map only.

### `insert(map, key, value)`

Returns a **new** map with the key-value pair added. If the key already exists, its value is updated. The original map is not modified.

```oxi
m := {"a": 1}
m := insert(m, "b", 2)
m := insert(m, "a", 10)
println(m)
```

**Argument types:** First argument must be a Map.

### `remove(collection, key_or_value)`

Returns a **new** collection with the specified element removed. Works on maps (removes by key) and sets (removes by value). The original collection is not modified.

```oxi
remove({"a": 1, "b": 2}, "a")
remove(set(1, 2, 3), 2)
```

**Supported types:** Map (removes by key), Set (removes by value).

## Introspection

### `type(value)`

Returns a string name of the value's type. For struct instances, returns the struct name instead of a generic type.

```oxi
type(42)
type("abc")
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

For structs:

```oxi
struct Person { name <str> age <int> }
p := Person("Alice", 30)
type(p)
```

### `is_mut(variable)`

Returns `True` if the variable's **value** is mutable, `False` if it is immutable. The argument must be a variable name (not an arbitrary expression).

```oxi
x := 10
is_mut(x)

y <int> = 10
is_mut(y)

z <int> := 10
is_mut(z)
```

Also works on struct fields:

```oxi
m <Math> = Math { num_one: 10, num_two: 20 }
is_mut(m.num_one)
```

### `is_type(value, type_name)`

Returns `True` if the value matches the given type, `False` otherwise. The second argument is a type keyword — not a string.

```oxi
is_type(42, int)
is_type("hello", str)
is_type(3.14, float)
is_type(`a`, char)
is_type(True, bool)
is_type([1, 2], array)
is_type(byte(65), byte)
is_type(uint(42), uint)
is_type((1, 2), tuple)
is_type({"a": 1}, map)
is_type(set(1), set)
is_type(None, None)
```

For struct instances, use the struct name as the type keyword:

```oxi
struct Dog { name <str> }
d := Dog("Rex")
is_type(d, Dog)
```

Works well with logical operators for multi-type checks:

```oxi
x := 10
y := 14
is_type(x, int) and is_type(y, int)
```

**Supported type keywords:** `int`, `str`, `float`, `char`, `bool`, `array`, `byte`, `uint`, `tuple`, `map`, `set`, `fun`, `None`, or any struct name.

### `is_type_mut(variable)`

Returns `True` if the variable's **type** can change (no type constraint), `False` if the type is locked. The argument must be a variable name.

```oxi
x := 10
is_type_mut(x)

y <int> := 10
is_type_mut(y)
```

Also works on struct fields:

```oxi
is_type_mut(m.num_one)
```

## Complete Built-in Reference

| Function    | Arguments              | Returns      | Description                           |
|-------------|------------------------|--------------|---------------------------------------|
| `print`     | `(args...)`            | `None`       | Print args separated by spaces        |
| `println`   | `(args...)`            | `None`       | Print args separated by spaces        |
| `len`       | `(collection)`         | `Integer`    | Length of string/array/tuple/map/set  |
| `push`      | `(array, element)`     | `Array`      | New array with element appended       |
| `first`     | `(array)`              | `any`/`None` | First element or None                 |
| `last`      | `(array)`              | `any`/`None` | Last element or None                  |
| `rest`      | `(array)`              | `Array`/`None`| All elements except first            |
| `has`       | `(collection, value)`  | `Boolean`    | Membership test                       |
| `range`     | `(n)`                  | `Array`      | `[0, 1, ..., n-1]`                   |
| `int`       | `(value)`              | `Integer`    | Parse/convert to integer              |
| `float`     | `(value)`              | `Float`      | Parse/convert to float                |
| `str`       | `(value)`              | `String`     | String representation                 |
| `ord`       | `(char)`               | `Integer`    | Unicode codepoint                     |
| `chr`       | `(integer)`            | `Char`       | Character from codepoint              |
| `chars`     | `(string)`             | `Array`      | Array of characters                   |
| `byte`      | `(value)`              | `Byte`       | Create byte (0-255)                   |
| `uint`      | `(value)`              | `Uint`       | Create unsigned integer               |
| `set`       | `(values...)`          | `Set`        | Create set (deduplicates)             |
| `tuple`     | `(values...)`          | `Tuple`      | Create tuple                          |
| `keys`      | `(map)`                | `Array`      | Array of map keys                     |
| `values`    | `(map)`                | `Array`      | Array of map values                   |
| `insert`    | `(map, key, value)`    | `Map`        | New map with entry added/updated      |
| `remove`    | `(map\|set, key\|val)` | `Map`/`Set`  | New collection without entry          |
| `type`      | `(value)`              | `String`     | Type name (struct name for instances) |
| `is_type`   | `(value, type_name)`   | `Boolean`    | Runtime type check                    |
| `is_mut`    | `(variable)`           | `Boolean`    | Value mutability check                |
| `is_type_mut`| `(variable)`          | `Boolean`    | Type mutability check                 |

See also:
- [Data Types](data_types.md) — types accepted by each built-in
- [Type System](type_system.md) — type conversion and introspection details
- [Variables and Assignments](variables.md) — mutability model used by `is_mut` and `is_type_mut`
- [Imports and Modules](imports.md) — the `introduce` keyword and module system
- [Standard Library](stdlib.md) — full reference for all stdlib modules (math, strings, array, io, os, time, random, path, json, net)
