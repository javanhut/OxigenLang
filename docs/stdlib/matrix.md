# matrix

Two-dimensional matrices on nested arrays: building, reading, element-wise
arithmetic, the matrix product and transposition.

```oxi
introduce matrix
```

Make one from its shape, `matrix.Matrix(rows, cols)`, or with a module
function such as `matrix.zeros(2, 3)` or `matrix.from_array(...)`. A matrix
made from its shape alone holds zeros until you change it, and its methods
build others of the same shape:

```oxi
m := matrix.Matrix(4, 4)
m.print()              // four rows of [0, 0, 0, 0]
m.rand_int().print()   // a 4 x 4 of random integers
```

Every function and method returns a **new** matrix and leaves the ones you
pass in alone. The one exception is [`m.set`](#msetr-c-value), which changes a
value in place.

**Errors.** A negative size, ragged rows in `from_array`, or two matrices
whose shapes don't fit fails with an `<Error<shape>>`. It propagates like any
`<fail>`; catch it with `<type<Error> || <Value>>(...)`. An index out of range
fails the same way an array index does.

| Item | Signature | Returns |
|------|-----------|---------|
| [`full`](#fullrows-cols-value) | `full(rows, cols, value)` | `Matrix` |
| [`zeros`](#zerosrows-cols) | `zeros(rows, cols)` | `Matrix` |
| [`ones`](#onesrows-cols) | `ones(rows, cols)` | `Matrix` |
| [`identity`](#identityn) | `identity(n)` | `Matrix` |
| [`rand_int`](#rand_introws-cols-min-max) | `rand_int(rows, cols, min, max)` | `Matrix` |
| [`rand_float`](#rand_floatrows-cols) | `rand_float(rows, cols)` | `Matrix` |
| [`from_array`](#from_arrayarr) | `from_array(arr)` | `Matrix` |
| [`m.full` and the rest](#building-from-a-shape) | `m.full(value)`, `m.zeros()`, `m.ones()`, `m.rand_int(min, max)`, `m.rand_float()` | `Matrix` |
| [`get`](#mgetr-c) | `m.get(r, c)` | one value |
| [`set`](#msetr-c-value) | `m.set(r, c, value)` | `None` |
| [`row`](#mrowr) | `m.row(r)` | `array` |
| [`col`](#mcolc) | `m.col(c)` | `array` |
| [`to_array`](#mto_array) | `m.to_array()` | `array` of arrays |
| [`map`](#mmapf) | `m.map(f)` | `Matrix` |
| [`add`](#maddother) | `m.add(other)` | `Matrix` |
| [`sub`](#msubother) | `m.sub(other)` | `Matrix` |
| [`scale`](#mscalek) | `m.scale(k)` | `Matrix` |
| [`mul`](#mmulother) | `m.mul(other)` | `Matrix` |
| [`transpose`](#mtranspose) | `m.transpose()` | `Matrix` |
| [`print`](#mprint) | `m.print()` | `None` |

---

## Struct: `Matrix`

| Field | Type | Contents |
|-------|------|----------|
| `rows` | `int` | number of rows |
| `cols` | `int` | number of columns |
| `data` | `array` or `None` | the values, one array per row; `None` reads as all zeros |

`data` may be left off: `Matrix(2, 3)` is a 2 x 3 matrix of zeros, and
`Matrix(2, 3, [[1, 2, 3], [4, 5, 6]])` gives the values directly (unchecked;
`from_array` checks the shape). Read `rows` and `cols` directly. Change
values through `set` rather than `data`, so the shape stays consistent.

Two matrices are `==` when their shapes and values match:

```oxi
a := matrix.from_array([[1, 2], [3, 4]])
println(a.mul(matrix.identity(2)) == a)   // True
```

`==` compares the `data` field as it is, so `Matrix(2, 2)` is not `==` to
`zeros(2, 2)` even though both read as zeros. Compare `to_array()` instead.

---

## Building

### `full(rows, cols, value)`

A `rows` x `cols` matrix with every value set to `value`, whatever its type.

```oxi
matrix.full(2, 2, 7).print()
// [7, 7]
// [7, 7]
matrix.full(1, 2, False).print()   // [False, False]
```

### `zeros(rows, cols)`

`full(rows, cols, 0)`.

```oxi
println(matrix.zeros(2, 3).to_array())   // [[0, 0, 0], [0, 0, 0]]
```

### `ones(rows, cols)`

`full(rows, cols, 1)`.

```oxi
println(matrix.ones(1, 2).to_array())   // [[1, 1]]
```

### `identity(n)`

The `n` x `n` identity matrix: 1 on the diagonal, 0 elsewhere.

```oxi
println(matrix.identity(3).to_array())   // [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
```

### `rand_int(rows, cols, min, max)`

Random integers between `min` and `max`, **both inclusive**. `min` defaults
to 0 and `max` to 1000. Built on [`random.rand_int`](random.md#rand_intmin-max),
so [`random.seed`](random.md#seedn) makes it repeatable.

```oxi
println(matrix.rand_int(2, 2).to_array())         // [[493, 412], [57, 655]]
println(matrix.rand_int(1, 3, 1, 6).to_array())   // [[4, 1, 6]]
```

### `rand_float(rows, cols)`

Random floats in `[0.0, 1.0)`, from [`random.rand_float`](random.md#rand_float).

```oxi
println(matrix.rand_float(1, 2).to_array())   // [[0.383..., 0.859...]]
```

### `from_array(arr)`

A matrix from an array of rows. Every row must have the same length, and the
rows are copied, so changing `arr` later doesn't change the matrix. An empty
array gives a 0 x 0 matrix.

```oxi
m := matrix.from_array([[1, 2, 3], [4, 5, 6]])
println(m.rows)   // 2
println(m.cols)   // 3
matrix.from_array([[1, 2], [3]])   // fails: rows of a matrix must all have the same length
```

### Building from a shape

Each of these returns a new matrix with the same `rows` and `cols` as `m`,
taking its arguments from the module function of the same name. `m` is left
alone.

| Method | Same as |
|--------|---------|
| `m.full(value)` | `full(m.rows, m.cols, value)` |
| `m.zeros()` | `zeros(m.rows, m.cols)` |
| `m.ones()` | `ones(m.rows, m.cols)` |
| `m.rand_int(min, max)` | `rand_int(m.rows, m.cols, min, max)`, `min` 0 and `max` 1000 by default |
| `m.rand_float()` | `rand_float(m.rows, m.cols)` |

```oxi
shape := matrix.Matrix(2, 3)
println(shape.full(7).to_array())          // [[7, 7, 7], [7, 7, 7]]
println(shape.rand_int(1, 6).to_array())   // [[3, 6, 1], [2, 2, 5]]
```

---

## Reading and writing

### `m.get(r, c)`

The value at row `r`, column `c`, counting from 0.

```oxi
m := matrix.from_array([[1, 2], [3, 4]])
println(m.get(1, 0))   // 3
```

### `m.set(r, c, value)`

Changes the value at row `r`, column `c`, in place.

```oxi
m := matrix.zeros(2, 2)
m.set(0, 1, 5)
println(m.to_array())   // [[0, 5], [0, 0]]
```

### `m.row(r)`

A copy of row `r`.

```oxi
println(matrix.from_array([[1, 2], [3, 4]]).row(1))   // [3, 4]
```

### `m.col(c)`

Column `c` as an array.

```oxi
println(matrix.from_array([[1, 2], [3, 4]]).col(1))   // [2, 4]
```

### `m.to_array()`

The values as an array of rows, copied, so changing the result doesn't change
`m`.

```oxi
println(matrix.identity(2).to_array())   // [[1, 0], [0, 1]]
```

---

## Arithmetic

### `m.map(f)`

A matrix of the same shape with `f` applied to every value.

```oxi
m := matrix.from_array([[1, 2], [3, 4]])
println(m.map(fun(x <int>) { x * x }).to_array())   // [[1, 4], [9, 16]]
```

### `m.add(other)`

The element-wise sum. Both matrices must have the same shape.

```oxi
a := matrix.from_array([[1, 2], [3, 4]])
b := matrix.from_array([[5, 6], [7, 8]])
println(a.add(b).to_array())   // [[6, 8], [10, 12]]
```

### `m.sub(other)`

The element-wise difference, `m` minus `other`. Both matrices must have the
same shape.

```oxi
println(b.sub(a).to_array())   // [[4, 4], [4, 4]]
```

### `m.scale(k)`

Every value multiplied by the number `k`.

```oxi
println(a.scale(2).to_array())     // [[2, 4], [6, 8]]
println(a.scale(0.5).to_array())   // [[0.5, 1.0], [1.5, 2.0]]
```

### `m.mul(other)`

The matrix product. `m` is n x k and `other` must be k x p (the columns of
`m` equal the rows of `other`); the result is n x p. For element-wise
multiplication, use `map` or `scale`.

```oxi
a := matrix.from_array([[1, 2, 3], [4, 5, 6]])
b := matrix.from_array([[1], [0], [2]])
println(a.mul(b).to_array())   // [[7], [16]]
```

### `m.transpose()`

Rows become columns: an n x k matrix gives k x n.

```oxi
println(matrix.from_array([[1, 2, 3], [4, 5, 6]]).transpose().to_array())
// [[1, 4], [2, 5], [3, 6]]
```

---

## Output

### `m.print()`

Prints one row per line.

```oxi
matrix.identity(2).print()
// [1, 0]
// [0, 1]
```

`println(m)` shows the whole struct instead:
`Matrix { rows: 2, cols: 2, data: [[1, 0], [0, 1]] }`.
