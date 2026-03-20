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

Returns the length of an array or string.

```oxi
len("OxigenLang") # Returns 10
len([1, 2, 3])   # Returns 3
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

## Introspection

### `type(value)`

Returns the string name of the value's type.

```oxi
type(42)    # Returns "INTEGER"
type("abc") # Returns "STRING"
type(None)  # Returns "NONE"
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
