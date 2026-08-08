# math

Numeric helpers: arithmetic, rounding, trigonometry, logarithms, and integer
theory. Pure functions — nothing here touches the outside world.

```oxi
introduce math
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`abs`](#absx) | `abs(x)` | same type as `x` |
| [`min`](#mina-b) | `min(a, b)` | one of the arguments |
| [`max`](#maxa-b) | `max(a, b)` | one of the arguments |
| [`pow`](#powbase-exp) | `pow(base, exp)` | `int` |
| [`powf`](#powfbase-exp) | `powf(base, exp)` | `float` |
| [`clamp`](#clampx-lo-hi) | `clamp(x, lo, hi)` | number |
| [`sign`](#signx) | `sign(x)` | `int` (`-1`, `0`, `1`) |
| [`sqrt`](#sqrtx) | `sqrt(x)` | `float` |
| [`floor`](#floorx) | `floor(x)` | `int` |
| [`ceil`](#ceilx) | `ceil(x)` | `int` |
| [`round`](#roundx) | `round(x)` | `int` |
| [`trunc`](#truncx) | `trunc(x)` | `int` |
| [`gcd`](#gcda-b) | `gcd(a, b)` | `int` |
| [`lcm`](#lcma-b) | `lcm(a, b)` | `int` |
| [`factorial`](#factorialn) | `factorial(n)` | `int` or `Error` |
| [`is_even`](#is_evenn--is_oddn) | `is_even(n)` | `bool` |
| [`is_odd`](#is_evenn--is_oddn) | `is_odd(n)` | `bool` |
| [`hypot`](#hypota-b) | `hypot(a, b)` | `float` |
| [`fmod`](#fmoda-b) | `fmod(a, b)` | number |
| [`degrees`](#degreesrad--radiansdeg) | `degrees(rad)` | `float` |
| [`radians`](#degreesrad--radiansdeg) | `radians(deg)` | `float` |
| [`sin`](#sinx-cosx-tanx) | `sin(x)` | `float` |
| [`cos`](#sinx-cosx-tanx) | `cos(x)` | `float` |
| [`tan`](#sinx-cosx-tanx) | `tan(x)` | `float` |
| [`asin`](#asinx-acosx-atanx) | `asin(x)` | `float` |
| [`acos`](#asinx-acosx-atanx) | `acos(x)` | `float` |
| [`atan`](#asinx-acosx-atanx) | `atan(x)` | `float` |
| [`atan2`](#atan2y-x) | `atan2(y, x)` | `float` |
| [`exp`](#expx) | `exp(x)` | `float` |
| [`ln`](#lnx) | `ln(x)` | `float` |
| [`log2`](#log2x) | `log2(x)` | `float` |
| [`log10`](#log10x) | `log10(x)` | `float` |

Constants: [`PI`, `E`, `TAU`](#constants).

---

## Basic arithmetic

### `abs(x)`

Absolute value. Keeps the type it was given — an `int` in, an `int` out.

```oxi
println(math.abs(-5))    // 5
println(math.abs(3.5))   // 3.5
```

### `min(a, b)`

The smaller of two values. Works on anything `<=` compares (ints, floats, and
mixed int/float).

```oxi
println(math.min(3, 7))      // 3
println(math.min(2.5, 2))    // 2
```

### `max(a, b)`

The larger of two values.

```oxi
println(math.max(3, 7))      // 7
```

`min`/`max` here take **two values**. For the smallest element of an array, use
[`array.min`](array.md#minarr) / [`array.max`](array.md#maxarr).

### `pow(base, exp)`

Integer exponentiation by repeated multiplication.

```oxi
println(math.pow(2, 10))   // 1024
println(math.pow(5, 0))    // 1
```

A **negative exponent returns 1** — the multiply loop simply never runs. Use
[`powf`](#powfbase-exp) for negative or fractional exponents.

```oxi
println(math.pow(2, -1))    // 1     <- not 0.5
println(math.powf(2, -1))   // 0.5
```

### `powf(base, exp)`

Floating-point exponentiation. Handles fractional and negative exponents.

```oxi
println(math.powf(2, 0.5))   // 1.4142135623730951
println(math.powf(2, -1))    // 0.5
println(math.powf(27, 1.0 / 3.0))  // 3
```

### `clamp(x, lo, hi)`

Constrain `x` to the range `[lo, hi]`.

```oxi
println(math.clamp(15, 0, 10))   // 10
println(math.clamp(-3, 0, 10))   // 0
println(math.clamp(5, 0, 10))    // 5
```

### `sign(x)`

`1` for positive, `-1` for negative, `0` for zero. Always an `int`.

```oxi
println(math.sign(-9))    // -1
println(math.sign(0))     // 0
println(math.sign(2.5))   // 1
```

---

## Rounding

All four return an **integer**, whatever they were given.

### `floor(x)`

Round toward negative infinity.

```oxi
println(math.floor(3.7))    // 3
println(math.floor(-3.2))   // -4
```

### `ceil(x)`

Round toward positive infinity.

```oxi
println(math.ceil(3.2))     // 4
println(math.ceil(-3.7))    // -3
```

### `round(x)`

Round to nearest; halves go away from zero.

```oxi
println(math.round(2.4))    // 2
println(math.round(2.5))    // 3
println(math.round(-2.5))   // -3
```

### `trunc(x)`

Round toward zero — drop the fractional part.

```oxi
println(math.trunc(3.9))    // 3
println(math.trunc(-3.9))   // -3
```

---

## Integer helpers

### `gcd(a, b)`

Greatest common divisor, by Euclid's algorithm. Signs are ignored.

```oxi
println(math.gcd(12, 18))    // 6
println(math.gcd(-12, 18))   // 6
println(math.gcd(7, 0))      // 7
```

### `lcm(a, b)`

Least common multiple. `0` if either argument is `0`.

```oxi
println(math.lcm(4, 6))    // 12
println(math.lcm(0, 5))    // 0
```

### `factorial(n)`

`n!`. Returns a catchable `<Error<negative>>` for negative `n`.

```oxi
println(math.factorial(5))   // 120
println(math.factorial(0))   // 1

f := <type<Error> || <Value>>(math.factorial(-3))
println(f.msg)   // factorial of a negative number
```

### `is_even(n)` / `is_odd(n)`

```oxi
println(math.is_even(4))   // True
println(math.is_odd(4))    // False
```

---

## Float helpers

### `hypot(a, b)`

`sqrt(a*a + b*b)` — the length of the hypotenuse.

```oxi
println(math.hypot(3, 4))   // 5
```

### `fmod(a, b)`

Floating-point remainder, truncated toward zero (so the sign follows `a`).

```oxi
println(math.fmod(7.5, 2))    // 1.5
println(math.fmod(-7.5, 2))   // -1.5
```

The `%` operator is integer-only; `fmod` is the float version.

### `sqrt(x)`

Square root, always a `float`. A negative input gives `NaN`, not an error.

```oxi
println(math.sqrt(16))   // 4
println(math.sqrt(2))    // 1.4142135623730951
println(math.sqrt(-1))   // NaN
```

---

## Trigonometry

All angles are in **radians**.

### `sin(x)`, `cos(x)`, `tan(x)`

```oxi
println(math.sin(0))              // 0
println(math.cos(0))              // 1
println(math.sin(math.PI / 2))    // 1
```

### `asin(x)`, `acos(x)`, `atan(x)`

Inverse trig, results in radians.

```oxi
println(math.asin(1))   // 1.5707963267948966
println(math.acos(1))   // 0
println(math.atan(1))   // 0.7853981633974483
```

### `atan2(y, x)`

The angle of the point `(x, y)` from the positive x-axis — quadrant-correct,
unlike `atan(y / x)`.

```oxi
println(math.atan2(1, 1))    // 0.7853981633974483   (45°)
println(math.atan2(1, -1))   // 2.356194490192345    (135°)
```

### `degrees(rad)` / `radians(deg)`

Convert between the two.

```oxi
println(math.degrees(math.PI))   // 180
println(math.radians(180))       // 3.141592653589793
```

---

## Exponential and logarithmic

### `exp(x)`

`e**x`.

```oxi
println(math.exp(1))   // 2.718281828459045
```

### `ln(x)`

Natural log. `ln(0)` is `-inf`, `ln(negative)` is `NaN` — neither is an error.

```oxi
println(math.ln(math.E))   // 1
println(math.ln(0))        // -inf
```

### `log2(x)`

```oxi
println(math.log2(8))   // 3
```

### `log10(x)`

```oxi
println(math.log10(1000))   // 3
```

---

## Constants

| Name | Value |
|------|-------|
| `math.PI` | `3.141592653589793` |
| `math.E` | `2.718281828459045` |
| `math.TAU` | `6.283185307179586` (`2 * PI`) |

```oxi
introduce math

fun circle_area(r <float>) { math.PI * r * r }

println(circle_area(2.0))   // 12.566370614359172
```

---

## Worked example

```oxi
introduce math

// Distance between two points, rounded to whole units.
fun distance(x1 <float>, y1 <float>, x2 <float>, y2 <float>) {
    math.hypot(x2 - x1, y2 - y1)
}

println(distance(0.0, 0.0, 3.0, 4.0))          // 5
println(math.round(distance(0.0, 0.0, 1.0, 1.0)))  // 1

// Reduce a fraction.
fun reduce(n <int>, d <int>) {
    g <int> := math.gcd(n, d)
    (n / g, d / g)
}

println(reduce(6, 8))   // (3, 4)
```

---

See also: [array](array.md) for folds over collections, [random](random.md) for
random numbers.
