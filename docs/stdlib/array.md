# array

Higher-order operations over arrays: map/filter/reduce, folds, searches,
transforms, grouping, and sorting.

```oxi
introduce array
```

Every function here returns a **new** array; none modify the one you passed in.
(The `push` builtin is the exception to watch — it appends in place and returns
the same array.) Note that the *records inside* an array are shared references,
so editing one edits the array — see
[`find_where`](#find_wherearr-key-val).

The functions that take an `f`/`pred`/`key_fn` accept a lambda
(`fun(x) { ... }`) or a named function — except [`pmap`](#pmaparr-f), which
needs a named one.

| Function | Signature | Returns |
|----------|-----------|---------|
| [`map`](#maparr-f) | `map(arr, f)` | `array` |
| [`filter`](#filterarr-f) | `filter(arr, f)` | `array` |
| [`reduce`](#reducearr-initial-f) | `reduce(arr, initial, f)` | any |
| [`flat_map`](#flat_maparr-f) | `flat_map(arr, f)` | `array` |
| [`each_with_index`](#each_with_indexarr-f) | `each_with_index(arr, f)` | `None` |
| [`reverse`](#reversearr) | `reverse(arr)` | `array` |
| [`zip`](#zipa-b) | `zip(a, b)` | `array` of tuples |
| [`flatten`](#flattenarr) | `flatten(arr)` | `array` |
| [`includes`](#includesarr-val) | `includes(arr, val)` | `bool` |
| [`sort`](#sortarr) | `sort(arr)` | `array` |
| [`sort_by`](#sort_byarr-key_fn) | `sort_by(arr, key_fn)` | `array` |
| [`sum`](#sumarr) | `sum(arr)` | number |
| [`product`](#productarr) | `product(arr)` | number |
| [`min`](#minarr) | `min(arr)` | element or `Error` |
| [`max`](#maxarr) | `max(arr)` | element or `Error` |
| [`any`](#anyarr-pred) | `any(arr, pred)` | `bool` |
| [`all`](#allarr-pred) | `all(arr, pred)` | `bool` |
| [`count`](#countarr-pred) | `count(arr, pred)` | `int` |
| [`find`](#findarr-pred) | `find(arr, pred)` | element or `Error` |
| [`find_index`](#find_indexarr-pred) | `find_index(arr, pred)` | `int` (`-1` if none) |
| [`index_of`](#index_ofarr-val) | `index_of(arr, val)` | `int` (`-1` if absent) |
| [`find_where`](#find_wherearr-key-val) | `find_where(arr, key, val)` | record or `None` |
| [`index_where`](#index_wherearr-key-val) | `index_where(arr, key, val)` | `int` (`-1` if absent) |
| [`filter_where`](#filter_wherearr-key-val) | `filter_where(arr, key, val)` | `array` |
| [`remove_where`](#remove_wherearr-key-val) | `remove_where(arr, key, val)` | `array` |
| [`unique`](#uniquearr) | `unique(arr)` | `array` |
| [`enumerate`](#enumeratearr) | `enumerate(arr)` | `array` of `(i, x)` |
| [`slice`](#slicearr-start-end) | `slice(arr, start, end)` | `array` |
| [`take`](#takearr-n) | `take(arr, n)` | `array` |
| [`drop`](#droparr-n) | `drop(arr, n)` | `array` |
| [`chunk`](#chunkarr-size) | `chunk(arr, size)` | `array` of arrays |
| [`group_by`](#group_byarr-key_fn) | `group_by(arr, key_fn)` | `map` |
| [`pmap`](#pmaparr-f) | `pmap(arr, f)` | `array` |

Basic array operations are **global builtins**, not part of this module:
`len(arr)`, `push(arr, x)`, `rest(arr)`, `has(arr, x)`, `range(n)`, `tuple(...)`.
See [builtins.md](../builtins.md).

---

## Mapping and folding

### `map(arr, f)`

Apply `f` to every element, collecting the results.

```oxi
println(array.map([1, 2, 3], fun(x) { x * 2 }))   // [2, 4, 6]
println(array.map(["a", "b"], fun(s) { strings.upper(s) }))  // [A, B]
```

### `filter(arr, f)`

Keep the elements for which `f` returns true.

```oxi
println(array.filter([1, 2, 3, 4], fun(x) { x > 2 }))   // [3, 4]
```

### `reduce(arr, initial, f)`

Fold the array into one value. `f(acc, item)` returns the new accumulator.

```oxi
println(array.reduce([1, 2, 3], 0, fun(a, b) { a + b }))    // 6
println(array.reduce(["a", "b"], "", fun(a, b) { a + b }))  // ab
```

### `flat_map(arr, f)`

Map, then flatten one level. `f` must return an array (or another iterable).

```oxi
println(array.flat_map([1, 2], fun(x) { [x, x * 10] }))   // [1, 10, 2, 20]
```

### `each_with_index(arr, f)`

Call `f(i, x)` for each element, for the side effect. Returns `None`.

```oxi
array.each_with_index(["a", "b"], fun(i, x) { println(i, x) })
// 0 a
// 1 b
```

---

## Folds

### `sum(arr)`

Add every element. An empty array sums to `0`. Works on anything `+` accepts,
including strings.

```oxi
println(array.sum([1, 2, 3, 4]))   // 10
println(array.sum([]))             // 0
```

### `product(arr)`

Multiply every element. An empty array gives `1`.

```oxi
println(array.product([1, 2, 3, 4]))   // 24
```

### `min(arr)`

Smallest element. Returns `<Error<empty>>("min of empty array")` for an empty
array.

```oxi
println(array.min([3, 1, 2]))   // 1

e := <type<Error> || <Value>>(array.min([]))
println(e.msg)   // min of empty array
```

### `max(arr)`

Largest element; same empty-array error.

```oxi
println(array.max([3, 1, 2]))   // 3
```

---

## Predicates and search

### `any(arr, pred)`

True if **at least one** element satisfies `pred`. Stops at the first match.

```oxi
fun is_big(x <int>) { x > 2 }

println(array.any([1, 2, 3], is_big))   // True
```

### `all(arr, pred)`

True if **every** element satisfies `pred`. Stops at the first failure. An
empty array is `True`.

```oxi
println(array.all([1, 2, 3], is_big))   // False
```

### `count(arr, pred)`

How many elements satisfy `pred`.

```oxi
println(array.count([1, 2, 3, 4], is_big))   // 2
```

### `find(arr, pred)`

The first matching element, or `<Error<not_found>>("no element matched")`.

```oxi
println(array.find([1, 2, 3, 4], is_big))   // 3

e := <type<Error> || <Value>>(array.find([1], is_big))
println(e.msg)   // no element matched
```

### `find_index(arr, pred)`

Index of the first match, or `-1`. Use this when "not found" is ordinary and
you would rather not unwrap an error.

```oxi
println(array.find_index([1, 2, 3], is_big))   // 2
println(array.find_index([1], is_big))         // -1
```

### `index_of(arr, val)`

Index of the first element equal to `val`, or `-1`.

```oxi
println(array.index_of(["a", "b"], "b"))   // 1
println(array.index_of(["a"], "z"))        // -1
```

### `includes(arr, val)`

True if `val` is in the array.

```oxi
println(array.includes([1, 2, 3], 2))   // True
println(array.includes([1, 2], 9))      // False
```

The builtin `has(arr, val)` does the same thing without importing the module.

---

## Searching an array of records

An array of maps — records loaded from JSON, rows, config entries — is common
enough that matching on a field has its own functions. They save you a lambda,
and they skip elements that are not maps or that lack the key, so a **ragged**
array is safe to search without guarding every element.

Given:

```oxi
people := [
    {"person": "john", "info": {"age": 32, "job": "unemployed"}},
    {"person": "jane"},
    {"info": {"age": 28, "job": "engineer"}},    // no "person" key at all
]
```

### `find_where(arr, key, val)`

The first record whose `key` field equals `val`, or `None`.

```oxi
println(array.find_where(people, "person", "john"))
// {person: john, info: {age: 32, job: unemployed}}

println(array.find_where(people, "person", "nobody"))   // None
```

Unlike [`find`](#findarr-pred), a miss is `None` rather than an error — with a
lookup by field, "no such record" is an ordinary answer.

**The record it returns is the one in the array**, not a copy, so editing it
edits the array:

```oxi
john := array.find_where(people, "person", "john")
john.info.job = "developer"
println(people[0])   // {person: john, info: {age: 32, job: developer}}
```

For the same thing plus "insert it if absent", see
[`json.set_where`](json.md#set_wherearr-key-val-fields).

### `index_where(arr, key, val)`

Its index, or `-1`. Use it when you need to replace or splice the record rather
than edit it in place.

```oxi
println(array.index_where(people, "person", "jane"))   // 1
println(array.index_where(people, "person", "zz"))     // -1
```

### `filter_where(arr, key, val)`

Every record with that field value, in order.

```oxi
println(array.filter_where(people, "person", "john"))
// [{person: john, info: {age: 32, job: unemployed}}]
```

### `remove_where(arr, key, val)`

A copy of the array **without** the matching records. The original is left
alone.

```oxi
people = array.remove_where(people, "person", "john")
println(len(people))   // 2
```

---

## Transforms

### `reverse(arr)`

```oxi
println(array.reverse([1, 2, 3]))   // [3, 2, 1]
```

### `zip(a, b)`

Pair up elements positionally. Stops at the shorter array.

```oxi
println(array.zip([1, 2, 3], ["a", "b"]))   // [(1, a), (2, b)]

each pair in array.zip(["x", "y"], [10, 20]) {
    name, value := pair
    println(name, value)
}
// x 10
// y 20
```

### `flatten(arr)`

Flatten **one** level. Non-array elements pass through untouched.

```oxi
println(array.flatten([[1, 2], [3], 4]))   // [1, 2, 3, 4]
```

Call it again for deeper nesting.

### `unique(arr)`

Remove duplicates, keeping the first occurrence and the original order.

```oxi
println(array.unique([1, 1, 2, 3, 3, 1]))   // [1, 2, 3]
```

### `enumerate(arr)`

Pair every element with its index.

```oxi
println(array.enumerate(["a", "b"]))   // [(0, a), (1, b)]

each pair in array.enumerate(["a", "b"]) {
    i, x := pair
    println(i, x)
}
```

### `slice(arr, start, end)`

Sub-array from `start` up to (not including) `end`. Same as `arr[start:end]`.

```oxi
println(array.slice([1, 2, 3, 4, 5], 1, 3))   // [2, 3]
```

### `take(arr, n)`

First `n` elements.

```oxi
println(array.take([1, 2, 3, 4], 2))   // [1, 2]
```

### `drop(arr, n)`

Everything after the first `n`.

```oxi
println(array.drop([1, 2, 3, 4], 2))   // [3, 4]
```

### `chunk(arr, size)`

Split into consecutive sub-arrays of `size`. The last chunk is short if the
array does not divide evenly.

```oxi
println(array.chunk([1, 2, 3, 4, 5], 2))   // [[1, 2], [3, 4], [5]]
println(array.chunk([1, 2, 3], 3))         // [[1, 2, 3]]
```

### `group_by(arr, key_fn)`

Build a map of `key_fn(x)` → array of the elements with that key.

```oxi
fun first_letter(s <str>) { s[0:1] }

println(array.group_by(["apple", "avocado", "beet"], first_letter))
// {a: [apple, avocado], b: [beet]}
```

---

## Sorting

### `sort(arr)`

Sort ascending. Handles an array of **all** integers, **all** floats, or
**all** strings.

```oxi
println(array.sort([3, 1, 4, 1, 5]))       // [1, 1, 3, 4, 5]
println(array.sort(["pear", "apple"]))     // [apple, pear]
```

Mixed types have no defined order, so pairs of different types compare equal
and the array comes back essentially unsorted:

```oxi
println(array.sort([2.5, 1]))   // [2.5, 1]   <- int vs float: not sorted
```

Convert first (`array.map(xs, fun(x) { float(x) })`) or use `sort_by`.

### `sort_by(arr, key_fn)`

Sort by a computed key. Stable for equal keys.

```oxi
fun word_len(s <str>) { len(s) }

println(array.sort_by(["ccc", "a", "bb"], word_len))   // [a, bb, ccc]
```

Sorting structs or maps by a field is the usual use:

```oxi
fun by_age(p <map>) { p["age"] }

people := [{"name": "Ada", "age": 36}, {"name": "Bo", "age": 21}]
println(array.map(array.sort_by(people, by_age), fun(p) { p["name"] }))
// [Bo, Ada]
```

`sort_by` is a selection sort — O(n²). Fine for hundreds of items; for large
arrays of plain numbers or strings, `sort` is the built-in comparison sort.

---

## Parallel

### `pmap(arr, f)`

Like `map`, but every call to `f` is spawned on the worker pool and the results
are joined in order. All tasks are spawned before any is joined, so the work
actually overlaps.

```oxi
introduce array

fun double(x <int>) { x * 2 }

println(array.pmap([1, 2, 3], double))   // [2, 4, 6]
```

`f` **must be a named top-level function** — a lambda cannot cross the spawn
boundary. Prefer the `diverge each` keyword form for new code; `pmap` is a thin
wrapper kept for compatibility. See [concurrency.md](../concurrency.md).

Worth it only when `f` is genuinely expensive; spawning has a cost that
dominates for something like doubling a number.

---

## Internal helpers

`sort_by` is a selection sort built on two helpers, both declared `hide fun` —
they are module-private and calling them raises:

| Function | Purpose |
|----------|---------|
| `_min_index_by(arr, key_fn)` | Index of the element with the smallest key |
| `_remove_at(arr, idx)` | Copy of `arr` without the element at `idx` |

```oxi
array._remove_at([1, 2, 3], 0)
// error: '_remove_at' is hidden inside module 'array'
```

---

## Worked example

```oxi
introduce array

orders := [
    {"item": "pen", "qty": 3, "price": 2},
    {"item": "pad", "qty": 1, "price": 5},
    {"item": "pen", "qty": 2, "price": 2},
]

fun line_total(o <map>) { o["qty"] * o["price"] }

fun item_of(o <map>) { o["item"] }

println(array.sum(array.map(orders, line_total)))   // 15
println(keys(array.group_by(orders, item_of)))      // [pen, pad]
println(array.count(orders, fun(o) { o["qty"] > 1 }))  // 2
```

---

See also: [math](math.md) for numeric helpers, [json](json.md) for map
manipulation, [builtins.md](../builtins.md) for `len`/`push`/`range`/`has`.
