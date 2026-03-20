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
