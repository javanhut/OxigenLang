# OxigenLang Operators

OxigenLang provides arithmetic, comparison, logical, and postfix operators for building expressions.

## Operator Precedence

Operators are evaluated in the following order, from highest to lowest precedence:

| Precedence | Operators                      | Description           |
|------------|--------------------------------|-----------------------|
| Highest    | `()` `[]` `.`                  | Call, index, dot      |
| &darr;     | `++` `--`                      | Postfix inc/dec       |
| &darr;     | `-x` `!x` `not x`             | Prefix negation       |
| &darr;     | `*` `/` `%`                    | Multiplication        |
| &darr;     | `+` `-`                        | Addition              |
| &darr;     | `<` `>` `<=` `>=`             | Relational comparison |
| Lowest     | `==` `!=`                      | Equality              |

Parentheses can be used to override precedence:

```oxi
(2 + 3) * 4
```

## Arithmetic Operators

| Operator | Name           | Example     | Result |
|----------|----------------|-------------|--------|
| `+`      | Addition       | `3 + 4`     | `7`    |
| `-`      | Subtraction    | `10 - 3`    | `7`    |
| `*`      | Multiplication | `5 * 6`     | `30`   |
| `/`      | Division       | `15 / 4`    | `3`    |
| `%`      | Modulo         | `17 % 5`   | `2`    |

### Integer Arithmetic

When both operands are integers, the result is an integer. Division truncates toward zero:

```oxi
15 / 4
17 % 5
-7 / 2
```

### Float Arithmetic

When either operand is a float, the result is a float:

```oxi
3.0 + 4
10 - 2.5
5.0 * 6
15.0 / 4
```

### Mixed Integer/Float

Integer operands are promoted to float when the other operand is a float:

```oxi
3 + 4.0
10 * 2.5
```

### String Concatenation

The `+` operator concatenates strings:

```oxi
"hello" + " " + "world"
```

String concatenation only works between two strings. To concatenate other types, convert them first with `str()`:

```oxi
"age: " + str(30)
```

### Tuple Concatenation

The `+` operator concatenates tuples:

```oxi
(1, 2) + (3, 4)
```

### Division by Zero

Division `/` and modulo `%` by zero produce an error:

```oxi
10 / 0
10 % 0
```

## Comparison Operators

| Operator | Name                 | Example    |
|----------|----------------------|------------|
| `==`     | Equal                | `3 == 3`   |
| `!=`     | Not equal            | `3 != 4`   |
| `<`      | Less than            | `3 < 5`    |
| `>`      | Greater than         | `5 > 3`    |
| `<=`     | Less than or equal   | `3 <= 3`   |
| `>=`     | Greater than or equal| `5 >= 5`   |

### Equality

`==` and `!=` work on all types. Two values are equal if they have the same type and the same value:

```oxi
42 == 42
"hello" == "hello"
True == True
None == None
[1, 2] == [1, 2]
(1, 2) == (1, 2)
```

Values of different types are never equal:

```oxi
42 == "42"
0 == False
```

### Relational Comparison

`<`, `>`, `<=`, `>=` work on integers, floats, characters, bytes, and unsigned integers:

```oxi
3 < 5
5.0 > 3.0
`a` < `z`
byte(10) > byte(5)
uint(100) >= uint(50)
```

Integer and float comparisons can be mixed:

```oxi
3 < 5.0
10.0 >= 10
```

## Logical Operators

### `!` (Bang)

Negates a boolean value. Non-boolean values are first evaluated for truthiness:

```oxi
!True
!False
!0
!"hello"
```

### `not` (Keyword)

Equivalent to `!` — negates a value's truthiness:

```oxi
not True
not False
not 0
not ""
```

`!` and `not` can be used interchangeably. `not` reads more naturally in complex conditions:

```oxi
not (x == 5)
```

Note: OxigenLang does not have `&&` (and) or `||` (or) operators. Use `option` blocks or nested conditionals for compound logic. See the [Control Flow](control_flow.md) guide.

## Unary Operators

### Unary Minus `-`

Negates a numeric value:

```oxi
x := 5
-x
-3.14
```

Works on integers and floats.

## Postfix Operators

### `++` (Increment)

Increments an integer variable by 1 and returns the **new** value:

```oxi
x := 5
x++
```

### `--` (Decrement)

Decrements an integer variable by 1 and returns the **new** value:

```oxi
x := 10
x--
```

### Restrictions

Postfix operators only work on:
- **Integer variables** — applying `++`/`--` to non-integers produces an error.
- **Mutable bindings** — blocked on variables declared with `<type> =` (strict typed immutable).

```oxi
x <int> := 5
x++

y <int> = 5
y++
```

## Indexing and Dot Access

### Index Operator `[]`

Access elements by position or key:

```oxi
[10, 20, 30][0]
"hello"[1]
(1, 2, 3)[2]
{"a": 1}["a"]
```

### Slice Operator `[start:end]`

Extract a sub-range. Works on arrays, strings, and tuples:

```oxi
[10, 20, 30, 40, 50][1:3]
"hello world"[0:5]
(1, 2, 3, 4)[1:3]
```

Start and/or end can be omitted:

```oxi
arr := [10, 20, 30, 40, 50]
arr[2:]
arr[:2]
arr[:]
```

Out-of-bounds values are clamped silently — no errors.

### Dot Operator `.`

Access struct fields and call methods:

```oxi
p := Person("Alice", 30)
p.name
p.greet()
```

See the [Structs](structs.md) guide for full details.

## Type Compatibility Summary

| Operator      | Integer | Float | String | Char | Boolean | Array | Tuple | Map | Set |
|---------------|---------|-------|--------|------|---------|-------|-------|-----|-----|
| `+`           | add     | add   | concat | —    | —       | —     | concat| —   | —   |
| `-` `*` `/` `%` | yes  | yes   | —      | —    | —       | —     | —     | —   | —   |
| `==` `!=`     | yes     | yes   | yes    | yes  | yes     | yes   | yes   | yes | yes |
| `<` `>` `<=` `>=` | yes | yes  | —      | yes  | —       | —     | —     | —   | —   |
| `!` / `not`   | truthy  | truthy| truthy | truthy| yes    | truthy| truthy| truthy| truthy|
| `++` `--`     | yes     | —     | —      | —    | —       | —     | —     | —   | —   |
| `[]` index    | —       | —     | yes    | —    | —       | yes   | yes   | yes | —   |
| `[:]` slice   | —       | —     | yes    | —    | —       | yes   | yes   | —   | —   |

See also:
- [Data Types](data_types.md) — detailed description of each type
- [Control Flow](control_flow.md) — conditionals and guards that use these operators
- [Variables and Assignments](variables.md) — assignment operators `:=` and `=`
