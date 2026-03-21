# OxigenLang Built-in Functions

OxigenLang comes with a standard library of built-in functions for various common tasks.

## Printing

### `print(arg1, arg2, ...)`

Prints the string representation of all provided arguments to the standard output, separated by spaces.

```oxi
print("Hello", 42, True) # Output: Hello 42 True
```

### `println(arg1, arg2, ...)`

Similar to `print`, but adds a newline character at the end.

```oxi
println("Welcome to OxigenLang")
```

## Array and String Manipulation

### `len(arg)`

Returns the length of a string, array, tuple, map, or set.

```oxi
len("OxigenLang")    # Returns 10
len([1, 2, 3])       # Returns 3
len((1, 2))          # Returns 2
len({"a": 1})        # Returns 1
len(set(1, 2, 3))    # Returns 3
```

### `push(array, element)`

Appends an element to the end of an array and returns the modified array.

```oxi
push([1, 2], 3) # Returns [1, 2, 3]
```

### `first(array)`

Returns the first element of an array.

```oxi
first([1, 2, 3]) # Returns 1
```

### `last(array)`

Returns the last element of an array.

```oxi
last([1, 2, 3]) # Returns 3
```

### `rest(array)`

Returns a new array containing all elements except the first one.

```oxi
rest([1, 2, 3]) # Returns [2, 3]
```

### `range(integer)`

Returns an array of integers from `0` up to (but not including) the given value.

```oxi
range(5) # Returns [0, 1, 2, 3, 4]
range(0) # Returns []
```

## Conversions

### `ord(char)`

Returns the integer value (Unicode codepoint) of a character.

```oxi
ord(`a`) # Returns 97
```

### `chr(integer)`

Returns the character corresponding to the provided integer (Unicode codepoint).

```oxi
chr(97) # Returns `a`
```

### `str(value)`

Converts the provided value to its string representation.

```oxi
str(42)   # Returns "42"
str(True) # Returns "True"
```

### `chars(string)`

Converts a string into an array of its characters.

```oxi
chars("abc") # Returns [`a`, `b`, `c`]
```

## Type Constructors

### `byte(value)`

Converts an integer (0-255) to a byte.

```oxi
byte(65)      # byte value 65
byte(256)     # error: out of range
```

### `uint(value)`

Converts a value to an unsigned integer.

```oxi
uint(42)      # unsigned 42
uint(-1)      # error: cannot convert negative
uint(3.7)     # unsigned 3
```

### `set(values...)`

Creates a set from the given values. Duplicates are removed.

```oxi
set(1, 2, 3)       # {1, 2, 3}
set(1, 1, 2, 2)    # {1, 2}
set()               # empty set
```

### `tuple(values...)`

Creates a tuple from the given values.

```oxi
tuple(1, 2, 3)     # (1, 2, 3)
tuple()             # ()
```

## Map Operations

### `keys(map)`

Returns an array of all keys in the map.

```oxi
keys({"a": 1, "b": 2})    # ["a", "b"]
```

### `values(map)`

Returns an array of all values in the map.

```oxi
values({"a": 1, "b": 2})  # [1, 2]
```

### `insert(map, key, value)`

Returns a new map with the key-value pair added or updated.

```oxi
m := {"a": 1}
m := insert(m, "b", 2)    # {"a": 1, "b": 2}
```

### `remove(collection, key)`

Returns a new map without the given key, or a new set without the given value.

```oxi
remove({"a": 1, "b": 2}, "a")   # {"b": 2}
remove(set(1, 2, 3), 2)         # set(1, 3)
```

### `has(collection, value)`

Returns `True` if the collection contains the value. Works on arrays, tuples, sets, and maps (checks keys).

```oxi
has([1, 2, 3], 2)              # True
has(set(1, 2, 3), 5)           # False
has({"a": 1}, "a")             # True
has((10, 20), 10)              # True
```

## Introspection

### `type(value)`

Returns the string name of the value's type. For struct instances, returns the struct name.

```oxi
type(42)    # Returns "INTEGER"
type("abc") # Returns "STRING"
type(None)  # Returns "NONE"

struct Person { name <str> age <int> }
p := Person("Alice", 30)
type(p)     # Returns "Person"
```

### `is_mut(variable)`

Returns `True` if the variable's value is mutable, `False` if it is immutable. The argument must be a variable name.

```oxi
x := 10
is_mut(x)           # True (untyped variables are mutable)

y <int> = 10
is_mut(y)           # False (strict typed declaration is immutable)

z <int> := 10
is_mut(z)           # True (walrus typed declaration is mutable)
```

### `is_type_mut(variable)`

Returns `True` if the variable's type can change (no type constraint), `False` if the type is locked. The argument must be a variable name.

```oxi
x := 10
is_type_mut(x)      # True (untyped — type can change freely)

y <int> := 10
is_type_mut(y)      # False (any typed declaration locks the type)
```

See the [Type System](type_system.md) guide for full details on mutability and type constraints.
