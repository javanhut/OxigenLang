# matrix

Matrices built off arrays. `Matrix(rows, cols)` holds a size; its methods
build a new `Matrix` of that size each time and leave the original alone, so
one `Matrix` can make as many as you like. Every method that builds or
transforms returns a new `Matrix`, so calls chain:
`matrix.Matrix(2, 3).ones().transpose()`.

```oxi
introduce matrix

mat := matrix.Matrix(4, 4)
ones := mat.ones()
zeros := mat.zeros()
r_mat := matrix.Matrix(4, 4).rand_int()
matrix.print_matrix(r_mat)

arr := [[0, 2], [2, 3]]
mat := matrix.Matrix(2, 2).init(arr)
transposed := mat.transpose()
dp := matrix.dot_product(mat, transposed)
```

A `Matrix` made from its size alone reads as all zeros, like `init()`, in
the transformations.

**Errors.** Sizes that don't fit fail with `<Error<size_mismatch>>`; `trace`,
`determinant` and `inverse` of a non-square matrix fail with
`<Error<not_square>>`, and `inverse` of a matrix whose determinant is 0 fails
with `<Error<singular>>`. Catch one with `<type<Error> || <Value>>(...)`.

| Item | Signature | Returns |
|------|-----------|---------|
| [`init`](#matinitvalue) | `mat.init(value?)` | `Matrix` |
| [`zeros_matrix`](#matzeros_matrix) | `mat.zeros_matrix()` | `Matrix` |
| [`ones_matrix`](#matones_matrix) | `mat.ones_matrix()` | `Matrix` |
| [`zeros`](#matzeros) | `mat.zeros()` | `Matrix` |
| [`ones`](#matones) | `mat.ones()` | `Matrix` |
| [`random_matrix`](#matrandom_matrixrandom_value) | `mat.random_matrix(random_value)` | `Matrix` |
| [`randomized_matrix`](#matrandomized_matrixvalue_type) | `mat.randomized_matrix(value_type)` | `Matrix` |
| [`rand_int`](#matrand_int) | `mat.rand_int()` | `Matrix` |
| [`to_array`](#matto_array) | `mat.to_array()` | `array` of rows |
| [`shape`](#matshape) | `mat.shape()` | `[rows, cols]` |
| [`transpose`](#mattranspose) | `mat.transpose()` | `Matrix` |
| [`flatten`](#matflatten) | `mat.flatten()` | `array` |
| [`reshape`](#matreshapenew_row_size-new_col_size) | `mat.reshape(new_row_size, new_col_size)` | `Matrix` |
| [`scale`](#matscalek) | `mat.scale(k)` | `Matrix` |
| [`get_row`](#matget_rowrow) | `mat.get_row(row)` | `array` |
| [`get_col`](#matget_colcol) | `mat.get_col(col)` | `array` |
| [`diagonal`](#matdiagonal) | `mat.diagonal()` | `array` |
| [`trace`](#mattrace) | `mat.trace()` | number |
| [`determinant`](#matdeterminant) | `mat.determinant()` | number |
| [`inverse`](#matinverse) | `mat.inverse()` | `Matrix` |
| [`identity`](#identitysize) | `identity(size)` | `Matrix` |
| [`add`](#addmat1-mat2) | `add(mat1, mat2)` | `Matrix` |
| [`subtract`](#subtractmat1-mat2) | `subtract(mat1, mat2)` | `Matrix` |
| [`multiply`](#multiplymat1-mat2) | `multiply(mat1, mat2)` | `Matrix` |
| [`dot_product`](#dot_productmat1-mat2) | `dot_product(mat1, mat2)` | `Matrix` |
| [`print_matrix`](#print_matrixmatrix) | `print_matrix(matrix)` | `None` |

---

## Struct: `Matrix`

| Field | Type | Contents |
|-------|------|----------|
| `row_size` | `int` | number of rows |
| `col_size` | `int` | number of columns |
| `data` | `array` or `None` | the values, set by `init(arr)`; left off, it is `None` |

```oxi
mat := matrix.Matrix(2, 3)
mat := matrix.Matrix(row_size=2, col_size=3)
```

---

## Methods

### `mat.init(value?)`

A new `row_size` x `col_size` `Matrix` with every value set to `value`, or
to `0` when `value` is left off.

```oxi
mat := matrix.Matrix(2, 3)
println(mat.init().to_array())    // [[0, 0, 0], [0, 0, 0]]
println(mat.init(7).to_array())   // [[7, 7, 7], [7, 7, 7]]
```

Given an array of rows, `init` holds those values instead. The array must be exactly `row_size` x `col_size`, and its rows are
copied, so changing the array later doesn't change the matrix.

```oxi
mat := matrix.Matrix(2, 2).init([[0, 2], [2, 3]])
println(mat.shape())                       // [2, 2]
matrix.Matrix(4, 4).init([[0, 2], [2, 3]])   // fails: Array doesn't fit the matrix: expected 4x4
```

### `mat.zeros_matrix()`

`mat.init(0)`.

```oxi
println(matrix.Matrix(2, 2).zeros_matrix().to_array())   // [[0, 0], [0, 0]]
```

### `mat.ones_matrix()`

`mat.init(1)`.

```oxi
println(matrix.Matrix(2, 2).ones_matrix().to_array())   // [[1, 1], [1, 1]]
```

### `mat.zeros()`

Same as `mat.zeros_matrix()`.

```oxi
zeros_matrix := matrix.Matrix(4, 4).zeros()
```

### `mat.ones()`

Same as `mat.ones_matrix()`.

```oxi
mat := matrix.Matrix(4, 4)
ones := mat.ones()
```

### `mat.random_matrix(random_value)`

`mat.init(random_value)`: every value is `random_value`, an `int` or a
`float`.

```oxi
println(matrix.Matrix(1, 3).random_matrix(5).to_array())   // [[5, 5, 5]]
```

### `mat.randomized_matrix(value_type)`

A matrix of random values. `value_type` defaults to `"int"`, giving integers
from 0 to 1000; `"float"` gives floats in `[0.0, 1.0)`. Built on the
[`random`](random.md) module, so [`random.seed`](random.md#seedn) makes it
repeatable.

```oxi
println(matrix.Matrix(2, 2).randomized_matrix().to_array())          // [[493, 412], [57, 655]]
println(matrix.Matrix(1, 2).randomized_matrix("float").to_array())   // [[0.383..., 0.859...]]
```

### `mat.rand_int()`

Same as `mat.randomized_matrix("int")`.

```oxi
r_mat := matrix.Matrix(4, 4).rand_int()
```

---

## Transformations

### `mat.to_array()`

The values as an array of rows, copied. For a `Matrix` made from its size
alone, this is all zeros.

```oxi
println(matrix.Matrix(2, 2).init([[0, 2], [2, 3]]).to_array())   // [[0, 2], [2, 3]]
println(matrix.Matrix(1, 2).to_array())                          // [[0, 0]]
```

### `mat.shape()`

`[row_size, col_size]`.

```oxi
println(matrix.Matrix(2, 3).shape())   // [2, 3]
```

### `mat.transpose()`

Rows become columns, so a 2 x 3 matrix becomes 3 x 2.

```oxi
transposed := matrix.Matrix(2, 3).init([[1, 2, 3], [4, 5, 6]]).transpose()
println(transposed.shape())        // [3, 2]
println(transposed.to_array())     // [[1, 4], [2, 5], [3, 6]]
```

### `mat.flatten()`

Every value in one array, row by row.

```oxi
println(matrix.Matrix(2, 2).init([[0, 2], [2, 3]]).flatten())   // [0, 2, 2, 3]
```

### `mat.reshape(new_row_size, new_col_size)`

The same values, row by row, in a new size. The new size must hold exactly
as many values as the old one.

```oxi
mat := matrix.Matrix(2, 3).init([[1, 2, 3], [4, 5, 6]])
println(mat.reshape(3, 2).to_array())   // [[1, 2], [3, 4], [5, 6]]
mat.reshape(4, 2)   // fails: Can't reshape 2x3 into 4x2: expected 6 values, got 8
```

### `mat.scale(k)`

Every value times `k`.

```oxi
println(matrix.Matrix(2, 2).init([[1, 2], [3, 4]]).scale(2).to_array())   // [[2, 4], [6, 8]]
```

### `mat.get_row(row)`

Row `row`, copied.

```oxi
println(matrix.Matrix(2, 2).init([[0, 2], [2, 3]]).get_row(1))   // [2, 3]
```

### `mat.get_col(col)`

Column `col`.

```oxi
println(matrix.Matrix(2, 2).init([[0, 2], [2, 3]]).get_col(1))   // [2, 3]
```

### `mat.diagonal()`

The values where the row equals the column, from the top left.

```oxi
println(matrix.Matrix(2, 3).init([[1, 2, 3], [4, 5, 6]]).diagonal())   // [1, 5]
```

### `mat.trace()`

The sum of the diagonal. The matrix must be square.

```oxi
println(matrix.Matrix(2, 2).init([[0, 2], [2, 3]]).trace())   // 3
```

### `mat.determinant()`

The determinant. The matrix must be square. A matrix of ints gives an exact
int.

```oxi
println(matrix.Matrix(2, 2).init([[0, 2], [2, 3]]).determinant())   // -4
```

### `mat.inverse()`

The inverse, as floats: `dot_product(mat, mat.inverse())` is the identity.
The matrix must be square and its determinant not 0.

```oxi
mat := matrix.Matrix(2, 2).init([[0, 2], [2, 3]])
println(mat.inverse().to_array())   // [[-0.75, 0.5], [0.5, 0]]
matrix.Matrix(2, 2).init([[1, 2], [2, 4]]).inverse()   // fails: Matrix has no inverse: its determinant is 0
```

---

## Module functions

### `identity(size)`

The `size` x `size` identity matrix: 1 on the diagonal, 0 elsewhere.

```oxi
println(matrix.identity(2).to_array())   // [[1, 0], [0, 1]]
```

### `add(mat1, mat2)`

Value by value. Both must be the same size.

```oxi
mat1 := matrix.Matrix(2, 2).init([[1, 2], [3, 4]])
mat2 := matrix.Matrix(2, 2).init([[5, 6], [7, 8]])
println(matrix.add(mat1, mat2).to_array())   // [[6, 8], [10, 12]]
```

### `subtract(mat1, mat2)`

Value by value, `mat1` minus `mat2`. Both must be the same size.

```oxi
println(matrix.subtract(mat2, mat1).to_array())   // [[4, 4], [4, 4]]
```

### `multiply(mat1, mat2)`

Value by value. Both must be the same size. For the matrix product, use
`dot_product`.

```oxi
println(matrix.multiply(mat1, mat2).to_array())   // [[5, 12], [21, 32]]
```

### `dot_product(mat1, mat2)`

The matrix product: an `n` x `m` and an `m` x `p` give an `n` x `p`.
`mat1`'s columns must equal `mat2`'s rows.

```oxi
mat1 := matrix.Matrix(2, 3).init([[1, 2, 3], [4, 5, 6]])
mat2 := matrix.Matrix(3, 1).init([[1], [0], [2]])
println(matrix.dot_product(mat1, mat2).to_array())   // [[7], [16]]
```

---

## Output

### `print_matrix(matrix)`

Prints a matrix one row per line. `matrix` may be an array of rows or a
`Matrix`.

```oxi
matrix.print_matrix(matrix.Matrix(2, 2).ones())
// [1, 1]
// [1, 1]
matrix.print_matrix(matrix.Matrix(2, 2).init([[0, 2], [2, 3]]).transpose())
// [0, 2]
// [2, 3]
```
