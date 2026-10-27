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

A string's length is counted in characters, the same unit as `s[i]` and
slices: `len("héllo")` is 5. For the UTF-8 byte count use
`strings.byte_len`.

Arrays, maps and sets share one set of builtins for adding, removing and
searching. They all change the collection **in place**: collections are shared
references, so every variable holding the same collection sees the change.
Each one also returns the collection (except `pop`, which returns what it
removed), so `arr := push(arr, x)` still works. Use `copy` when you need a
separate collection.

| Builtin | Array | Map | Set |
|---------|-------|-----|-----|
| `push(c, x)` | append `x` | — | add `x` |
| `insert(...)` | `insert(arr, index, x)` | `insert(m, key, value)` | `insert(s, x)` |
| `remove(c, x)` | first element equal to `x` | key `x` | value `x` |
| `pop(...)` | `pop(arr)` last, `pop(arr, index)` | `pop(m, key)` | `pop(s)` last added |
| `extend(c, items)` | append each item | copy entries from a map | add each item |
| `clear(c)` | empty it | empty it | empty it |
| `copy(c)` | new array | new map | new set |
| `has(c, x)` | element equal to `x` | key `x` | member `x` |

Set algebra (union, intersection, subsets) is in the [`sets`](stdlib/sets.md)
module, and searching arrays with a predicate (`find`, `index_of`, `filter`)
is in [`array`](stdlib/array.md).

### `push(collection, element)`

Adds the element to an array (at the end) or a set, and returns **that same
collection**. Pushing a value a set already holds does nothing.

```oxi
arr := [1, 2]
push(arr, 3)
println(arr)          // [1, 2, 3]

a := [1, 2]
b := push(a, 3)
println(a)            // [1, 2, 3]  <- a changed as well

s := set(1, 2)
push(s, 2)
push(s, 3)
println(s)            // set(1, 2, 3)
```

**Argument types:** Array or Set. A set element must be hashable.

### `insert(map, key, value)` / `insert(set, value)` / `insert(array, index, value)`

Adds an entry and returns the same collection.

- **Map**: sets `key` to `value`, replacing the value if the key exists.
- **Set**: adds `value`. The same as `push(set, value)`.
- **Array**: puts `value` at `index`, moving the later elements up. `index`
  can be `len(arr)` to append, and a negative index counts from the end
  (`-1` inserts before the last element). Any other index is an error.

```oxi
m := {"a": 1}
insert(m, "b", 2)       // {a: 1, b: 2}

s := set(1)
insert(s, 2)            // set(1, 2)

arr := [1, 3]
insert(arr, 1, 2)       // [1, 2, 3]
insert(arr, 0, 0)       // [0, 1, 2, 3]
```

**Argument types:** Map, Set or Array.

### `remove(collection, key_or_value)`

Removes an entry and returns the same collection. Removing something that
isn't there does nothing.

- **Map**: removes the key.
- **Set**: removes the value.
- **Array**: removes the first element equal to the value. To remove by
  position, use `pop(arr, index)`.

```oxi
remove({"a": 1, "b": 2}, "a")    // {b: 2}
remove(set(1, 2, 3), 2)          // set(1, 3)
remove([1, 2, 3, 2], 2)          // [1, 3, 2]
```

**Supported types:** Map (by key), Set (by value), Array (by value).

### `pop(array)` / `pop(array, index)` / `pop(map, key)` / `pop(set)`

Removes one entry and returns it.

- **Array**: removes the last element, or the one at `index` (negative
  counts from the end). Popping an empty array, or an index out of range,
  is an error.
- **Map**: removes `key` and returns its value, or `None` if the key wasn't
  there (the same as reading `m[key]`).
- **Set**: removes the most recently added value. Popping an empty set is an
  error.

```oxi
stack := [1, 2, 3]
top := pop(stack)          // 3; stack is [1, 2]
head := pop(stack, 0)      // 1; stack is [2]

m := {"a": 1}
pop(m, "a")                // 1
pop(m, "a")                // None
```

**Supported types:** Array, Map, Set.

### `extend(collection, items)`

Adds many entries at once and returns the same collection.

- **Array**: appends every element of an array, tuple or set, in order.
- **Set**: adds every element of an array, tuple or set.
- **Map**: copies every entry of another map, replacing existing keys.

```oxi
arr := [1]
extend(arr, [2, 3])                 // [1, 2, 3]

s := set(1)
extend(s, [1, 2, 2])                // set(1, 2)

m := {"a": 1, "b": 2}
extend(m, {"b": 20, "c": 3})        // {a: 1, b: 20, c: 3}
```

**Supported types:** Array, Set, Map.

### `clear(collection)`

Removes every entry and returns the same, now empty, collection.

```oxi
arr := [1, 2, 3]
clear(arr)
println(len(arr))      // 0
```

**Supported types:** Array, Map, Set.

### `copy(collection)`

Returns a new collection with the same entries, so changing one doesn't
change the other. The copy is shallow: an array inside an array is still
shared.

```oxi
a := [1, 2]
b := copy(a)
push(b, 3)
println(a)            // [1, 2]
println(b)            // [1, 2, 3]
```

**Supported types:** Array, Map, Set. Tuples and strings can't change, so
`copy` returns them as they are.

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
- **Map**: checks if the value is a **key** (not a value). To search the
  values, use `has(values(m), x)`.
- **String**: checks if a substring or character appears in it.

```oxi
has([1, 2, 3], 2)
has(set(1, 2, 3), 5)
has({"a": 1}, "a")
has((10, 20), 10)
has("hello", "ell")
```

**Supported types:** Array, Tuple, Set, Map, String.

## Iteration

### `range(end)` / `range(start, end)` / `range(start, end, step)`

Returns an array of integers from `start` (default `0`) up to but not including
`end`, advancing by `step` (default `1`).

```oxi
range(5)            // [0, 1, 2, 3, 4]
range(2, 5)         // [2, 3, 4]
range(0, 10, 3)     // [0, 3, 6, 9]
```

A negative step counts down. The bound stays exclusive from whichever side it is
approached:

```oxi
range(5, 0, -1)     // [5, 4, 3, 2, 1]
range(10, 0, -3)    // [10, 7, 4, 1]
```

`range` builds the whole array, so it refuses to build more than 100,000,000
elements (`range(9223372036854775807)` is an error, not an attempt to allocate
exabytes). `each i in range(...)` has no such limit: the loop counts without
building the array.

Commonly used with `each` for counted iteration:

```oxi
each i in range(10) {
    println(i)
}

each i in range(10, 0, -2) {
    println(i)
}
```

**Argument types:** Integer only.

**Empty results:** the range is empty when the step points away from the bound —
`range(0, 5, -1)` and `range(5, 0)` both yield `[]`. A two-argument range is
never auto-reversed; pass a negative step to count down.

**Errors:** a `step` of `0` is an error, since the range could never terminate.

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

### `set(values...)` / `set(array)`

Creates a set from the given values. Duplicates are removed automatically.
Given a single array, it makes a set of the array's elements.

```oxi
set(1, 2, 3)
set(1, 1, 2, 2)
set([1, 2, 2, 3])     // set(1, 2, 3)
set()
```

Accepts any number of hashable values: int, uint, float, bool, char, byte,
str, `None`, and tuples of those.

### `tuple(values...)`

Creates a tuple from the given values.

```oxi
tuple(1, 2, 3)
tuple("a", True, 42)
tuple()
```

Accepts any number of arguments of any type. This is equivalent to using the literal syntax `(1, 2, 3)`.

### `vec2(x, y)` / `vec3(x, y, z)` / `vec4(x, y, z, w)`

Creates a vector from two, three or four numbers (ints or floats).

```oxi
v := vec3(1, 0.5, -2)
v.y          // 0.5
v * 2        // vec3(2, 1, -4)
```

The wrong count, or anything that is not a number, is an error. See
[Vectors](data_types.md#vectors).

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

### `is_value(value)`

Returns `True` if the value is a `Value` wrapper (produced by `<Value>(...)` or the success path of `<type<Error || Value>>`), `False` otherwise.

```oxi
is_value(<Value>("ok"))                              // True
is_value(<type<Error || Value>>("loaded"))            // True
is_value(<Error>("bad")    )                          // False
is_value("hello")                                     // False
is_value(42)                                          // False
```

Use this to check the success path of an expected-result normalization:

```oxi
result <Error || Value> := <type<Error || Value>>(read_file("config"))
option {
    is_value(result) -> println("Got: {result.value}"),
    println("Failed: {result.msg}")
}
```

### `is_error(value)`

Returns `True` if the value is an `ErrorValue` (produced by `<Error>(...)` or the failure path of `<type<Error || Value>>`), `False` otherwise.

```oxi
is_error(<Error>("bad"))                              // True
is_error(<Error<network>>("offline"))                 // True
is_error(<type<Error || Value>>(<fail>("boom")))      // True
is_error(<Value>("ok"))                               // False
is_error("hello")                                     // False
is_error(42)                                          // False
```

Use this to check the failure path of an expected-result normalization:

```oxi
result <Error || Value> := <type<Error || Value>>(connect_db())
option {
    is_error(result) -> println("Error: {result.tag} - {result.msg}"),
    println("Connected: {result.value}")
}
```

**Note:** `is_error` checks for error **values** (tier 2). Propagating errors (tier 1) halt the program before they can reach the function call, so they will never be seen by `is_error`.

## Complete Built-in Reference

| Function    | Arguments              | Returns      | Description                           |
|-------------|------------------------|--------------|---------------------------------------|
| `print`     | `(args...)`            | `None`       | Print args separated by spaces        |
| `println`   | `(args...)`            | `None`       | Print args separated by spaces        |
| `len`       | `(collection)`         | `Integer`    | Length of string/array/tuple/map/set  |
| `push`      | `(array\|set, element)` | same collection | Adds in place                    |
| `first`     | `(array)`              | `any`/`None` | First element or None                 |
| `last`      | `(array)`              | `any`/`None` | Last element or None                  |
| `rest`      | `(array)`              | `Array`/`None`| All elements except first            |
| `has`       | `(collection, value)`  | `Boolean`    | Membership test (also substrings)     |
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
| `vec2`/`vec3`/`vec4` | `(numbers...)` | `Vec2`/`Vec3`/`Vec4` | Create a vector             |
| `keys`      | `(map)`                | `Array`      | Array of map keys                     |
| `values`    | `(map)`                | `Array`      | Array of map values                   |
| `insert`    | `(map, key, value)` / `(set, value)` / `(array, index, value)` | same collection | Adds in place |
| `remove`    | `(collection, key\|value)` | same collection | Removes in place                 |
| `pop`       | `(array, index?)` / `(map, key)` / `(set)` | removed item | Removes and returns one entry |
| `extend`    | `(collection, items)`  | same collection | Adds many in place                 |
| `clear`     | `(collection)`         | same collection | Empties in place                   |
| `copy`      | `(collection)`         | new collection | Shallow copy                        |
| `type`      | `(value)`              | `String`     | Type name (struct name for instances) |
| `is_type`   | `(value, type_name)`   | `Boolean`    | Runtime type check                    |
| `is_mut`    | `(variable)`           | `Boolean`    | Value mutability check                |
| `is_type_mut`| `(variable)`          | `Boolean`    | Type mutability check                 |
| `is_value`  | `(value)`              | `Boolean`    | True if value is a Value wrapper      |
| `is_error`  | `(value)`              | `Boolean`    | True if value is an ErrorValue        |

See also:
- [Data Types](data_types.md) — types accepted by each built-in
- [Type System](type_system.md) — type conversion and introspection details
- [Variables and Assignments](variables.md) — mutability model used by `is_mut` and `is_type_mut`
- [Imports and Modules](imports.md) — the `introduce` keyword and module system
- [Standard Library](stdlib/README.md) — one page per stdlib module, every function documented
