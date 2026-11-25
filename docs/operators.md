# OxigenLang Operators

OxigenLang provides arithmetic, comparison, logical, and postfix operators for building expressions, plus compound assignment (`+=`, `-=`, `*=`, `/=`) for updating a value in place.

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
| &darr;     | `==` `!=`                      | Equality              |
| &darr;     | `and`                          | Logical AND           |
| Lowest     | `or`                           | Logical OR            |

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

### Mixed Number Types

When the two operands are different kinds of number, both are converted to a
common type first, and the operator then works as it does for that type:

| Operands | Done as |
|---|---|
| `byte` or `uint` with `int` | `int` |
| `byte` with `uint` | `uint` |
| any number with `float` | `float` |
| `byte` with `byte` | `int` |

```oxi
3 * byte(4)          // 12 (int)
uint(7) % byte(4)    // 3 (uint)
byte(2) - 0.5        // 1.5 (float)
```

So the converted pair follows that type's rules: `int` wraps on overflow, a
`uint` subtraction below zero is an error, and `/` and `%` reject a zero
divisor. A `uint` larger than the biggest `int` wraps when converted to `int`.

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

### Integer Overflow

`int` is a 64-bit signed integer. Its range is `-9223372036854775808` to
`9223372036854775807`.

`+`, `-`, and `*` **wrap** when a result leaves that range. This is defined
behaviour, not an error and not undefined: the true result is reduced modulo
2<sup>64</sup> and reinterpreted as two's complement, so the answer is the same
on every platform and under every backend — interpreter, VM, and JIT all agree.

```oxi
9223372036854775807 + 1     // -9223372036854775808 — wraps to the minimum
9223372036854775807 * 2     // -2

min := 9223372036854775807 + 1
min - 1                     // 9223372036854775807 — wraps back to the maximum
min * -1                    // -9223372036854775808 — negating the minimum wraps to itself
```

Note that the minimum cannot be written as a literal. `-9223372036854775808` is
parsed as a negation applied to the literal `9223372036854775808`, which is one
past the maximum and so is rejected as out of range. Reach the minimum by
arithmetic, as above.

Wrapping never raises an error, so it cannot be caught after the fact. If a
computation might exceed the range and you need to know, test the operands
before the operation:

```oxi
option {
    a > 9223372036854775807 - b -> <fail>("would overflow"),
    a + b
}
```

Division is the exception: `/` and `%` do **not** wrap. Dividing the minimum by
`-1` is the only overflow division can produce — the true answer
`9223372036854775808` is one past the top of the range — and it is a runtime
error rather than a wrap:

```oxi
min := 9223372036854775807 + 1
min / -1     // error: integer overflow: int_min / -1 has no int result
min % -1     // error: integer overflow: int_min % -1 has no int result
```

The asymmetry is deliberate. `+`, `-`, and `*` sit on the hot path of every
loop and index computation, and wrapping keeps them branch-free — one machine
instruction with no overflow check to predict. `/` and `%` already have to
branch on the divisor to reject zero, so range-checking the same operand costs
nothing that was not being paid anyway.

### Unsigned Integer Overflow

`uint` is a 64-bit unsigned integer, range `0` to `18446744073709551615`.

`+` and `*` wrap exactly as their signed counterparts do, modulo
2<sup>64</sup>. The maximum is likewise past the reach of an `int` literal, so
build it by arithmetic:

```oxi
umax := uint(9223372036854775807) * uint(2) + uint(1)    // 18446744073709551615

umax + uint(1)    // 0 — wraps back to zero
umax * uint(2)    // 18446744073709551614
```

Subtraction is the exception. A `uint` subtraction that would drop below zero
is a runtime error rather than a wrap to a very large positive number, because
that particular wrap is nearly always a bug — a length or index calculation
that went one step too far — and a silent `18446744073709551615` propagates far
from where it started:

```oxi
uint(0) - uint(1)    // error: unsigned integer underflow
```

Unsigned division has no overflow case, so there is no `uint` counterpart to
the most-negative-over-`-1` error above. Division and modulo by zero are still
errors, as below.

Mixing a `uint` with an `int` produces an `int`, and is therefore subject to
the signed range and the signed wrapping rules.

### Division by Zero

Division `/` and modulo `%` by zero produce an error, for every numeric type —
floats included. There is no `inf` or `NaN` result:

```oxi
10 / 0        // error: division by zero
10 % 0        // error: modulo by zero
10.0 / 0.0    // error: division by zero
10.0 / 0      // error: division by zero
0.0 / 0.0     // error: division by zero
```

Negative zero is still zero, so `10.0 / -0.0` is an error too. Guard the divisor
if it may be zero:

```oxi
option {
    d != 0.0 -> n / d,
    0.0
}
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

`<`, `<=`, `>` and `>=` compare numbers with numbers, strings with strings
and chars with chars. Numbers of different kinds compare by value:

```oxi
3 < 5.0
10.0 >= 10
byte(200) > 100
uint(7) <= byte(9)
"apple" < "banana"
```

Integers of any width (`int`, `uint`, `byte`) compare exactly, so a `uint`
larger than any `int` is still greater than every `int`. A comparison with a
`float` is done as floats. Comparing anything else, such as a number with a
string, is an error.

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

### `and` (Keyword)

Returns `True` if both sides are truthy. Short-circuits — if the left side is falsy, the right side is not evaluated:

```oxi
True and True
True and False
x > 0 and x < 100
```

### `or` (Keyword)

Returns `True` if either side is truthy. Short-circuits — if the left side is truthy, the right side is not evaluated:

```oxi
False or True
False or False
x == 0 or x == 1
```

### Combining `and` / `or`

`and` binds tighter than `or`, so `a or b and c` is evaluated as `a or (b and c)`. Use parentheses to override:

```oxi
logged_in and is_admin or is_superuser
(logged_in or is_guest) and has_permission
```

### Short-Circuit Evaluation

Both `and` and `or` use short-circuit evaluation — the right-hand side is only evaluated when necessary:

```oxi
// safe: division only happens if x != 0
x != 0 and (100 / x > 10)

// safe: expensive_check() is skipped if cached is truthy
cached or expensive_check()
```

### Distributed Comparison

When you parenthesize an `or` or `and` group on one side of a comparison, the comparison is distributed to each element. This lets you write natural multi-value checks without repeating the operator:

```oxi
x := 5
y := 10

// (x or y) == 10  →  x == 10 or y == 10  →  True
(x or y) == 10

// (x and y) > 3  →  x > 3 and y > 3  →  True
(x and y) > 3
```

This works with all comparison operators (`==`, `!=`, `<`, `>`, `<=`, `>=`) and with chained values:

```oxi
// any of the three equal 42?
(a or b or c) == 42

// all of them greater than 0?
(a and b and c) > 0

// value on either side
5 == (x or y)
```

Without parentheses, `x or y == 10` uses normal precedence — `==` binds tighter, so it means `x or (y == 10)` and `x` is evaluated for truthiness:

```oxi
x or y == 10       // normal: truthy(x) or (y == 10)
(x or y) == 10     // distributed: x == 10 or y == 10
```

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

Adds 1 to a number and returns the **old** value:

```oxi
x := 5
y := x++
```

`y` is `5` and `x` is `6`.

### `--` (Decrement)

Subtracts 1 from a number and returns the **old** value:

```oxi
x := 10
x--
```

### Targets

`++` and `--` work on a variable, an index, or a field:

```oxi
counts <map>
counts["a"] = 1
counts["a"]++

grid := [[1, 2], [3, 4]]
grid[1][0]--

struct Counter {
    n <int>
}
Counter includes {
    fun inc() { n++ }
}
c := Counter { n: 0 }
c.n++
```

Inside a method, a bare field name (`n++`) means `self.n++`, as it does for reads and `=`.

The collection and the index are evaluated once, so `m[next_key()]++` calls `next_key()` a single time.

### Restrictions

- **Numbers only** — applying `++`/`--` to a non-number produces a type mismatch error.
- **Mutable bindings** — blocked on variables declared with `<type> =` (strict typed immutable).
- **The key must exist** — a map has no default value, so `++`/`--` on a missing key is an error. Assign the key first:

```oxi
x <int> := 5
x++

y <int> = 5
y++

m <map>
m["new"]++
```

The last line fails with `key "new" not found in map`. Use `m["new"] = 1`, or check with `has(m, "new")` first. Reading a missing key with `m["new"]` still returns `None`.

## Compound Assignment

| Operator | Meaning       | Example  | Same as      |
|----------|---------------|----------|--------------|
| `+=`     | Add to        | `x += 2` | `x = x + 2`  |
| `-=`     | Subtract from | `x -= 2` | `x = x - 2`  |
| `*=`     | Multiply by   | `x *= 2` | `x = x * 2`  |
| `/=`     | Divide by     | `x /= 2` | `x = x / 2`  |

```oxi
total := 10
total += 5
total *= 2
total -= 4
total /= 2

name := "Oxi"
name += "gen"
```

`total` ends as `13`. The operator behaves exactly like its arithmetic form, so `+=` also joins strings, and `/=` on two integers truncates (`7 / 2` is `3`).

Compound assignment is a statement, not an expression: it has no value, so `y := (x += 1)` is not valid. It can take a postfix guard: `x += 1 when ready`.

### Targets

Like `++`/`--`, the target can be a variable, an index, or a field:

```oxi
scores <map>
scores[(1, 2)] = 10
scores[(1, 2)] -= 4

items := [1, 2, 3]
items[2] *= 5

player.health -= damage
```

- On a variable, `x += e` follows the same rules as `x = x + e`: it is blocked on `<type> =` bindings, keeps a typed variable's type, and works on a captured variable inside a closure or a bare field name inside a method.
- On an index or field, the collection and index are evaluated once, and the right-hand side is evaluated after the current value is read.
- A missing map key is an error, as with `++`: `m["new"] += 1` fails with `key "new" not found in map`.

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
| `and` / `or`  | truthy  | truthy| truthy | truthy| yes    | truthy| truthy| truthy| truthy|
| `++` `--`     | yes     | —     | —      | —    | —       | —     | —     | —   | —   |
| `[]` index    | —       | —     | yes    | —    | —       | yes   | yes   | yes | —   |
| `[:]` slice   | —       | —     | yes    | —    | —       | yes   | yes   | —   | —   |

## Comments

### Single-Line Comments

Use `//` to comment out the rest of a line:

```oxi
x := 10  // this is a comment
// this whole line is a comment
```

### Multi-Line Comments

Use `/* */` to comment out a block of text spanning multiple lines:

```oxi
/* This is a
   multi-line comment */
x := 10
```

See also:
- [Data Types](data_types.md) — detailed description of each type
- [Control Flow](control_flow.md) — conditionals and guards that use these operators
- [Variables and Assignments](variables.md) — assignment operators `:=` and `=`
