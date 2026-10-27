# sets

Set algebra: union, intersection, difference, subset tests, and filtering.

```oxi
introduce sets
```

Every function here returns a **new** set and leaves the ones you pass in
alone. Adding and removing single values are global builtins that change a
set in place: `push(s, x)`, `insert(s, x)`, `remove(s, x)`, `pop(s)`,
`extend(s, items)`, `clear(s)`, `copy(s)` and `has(s, x)`. See
[builtins.md](../builtins.md#collection-operations).

The module is `sets`, not `set`, so importing it doesn't hide the `set()`
builtin.

| Function | Signature | Returns |
|----------|-----------|---------|
| [`union`](#uniona-b) | `union(a, b)` | `set` |
| [`intersection`](#intersectiona-b) | `intersection(a, b)` | `set` |
| [`difference`](#differencea-b) | `difference(a, b)` | `set` |
| [`symmetric_difference`](#symmetric_differencea-b) | `symmetric_difference(a, b)` | `set` |
| [`is_subset`](#is_subseta-b) | `is_subset(a, b)` | `bool` |
| [`is_superset`](#is_superseta-b) | `is_superset(a, b)` | `bool` |
| [`is_disjoint`](#is_disjointa-b) | `is_disjoint(a, b)` | `bool` |
| [`to_array`](#to_arrays) | `to_array(s)` | `array` |
| [`filter`](#filters-pred) | `filter(s, pred)` | `set` |
| [`map`](#maps-f) | `map(s, f)` | `set` |

---

## Combining

### `union(a, b)`

Every value in `a` or `b`. Values from `a` come first, then the new ones
from `b`.

```oxi
println(sets.union(set(1, 2), set(2, 3)))   // set(1, 2, 3)
```

### `intersection(a, b)`

The values in both `a` and `b`, in `a`'s order.

```oxi
println(sets.intersection(set(1, 2, 3), set(2, 3, 4)))   // set(2, 3)
```

### `difference(a, b)`

The values in `a` that aren't in `b`.

```oxi
println(sets.difference(set(1, 2, 3), set(2)))   // set(1, 3)
```

### `symmetric_difference(a, b)`

The values in exactly one of `a` and `b`.

```oxi
println(sets.symmetric_difference(set(1, 2), set(2, 3)))   // set(1, 3)
```

---

## Comparing

### `is_subset(a, b)`

Whether every value in `a` is also in `b`. The empty set is a subset of
every set, and a set is a subset of itself.

```oxi
println(sets.is_subset(set(1, 2), set(1, 2, 3)))   // True
println(sets.is_subset(set(1, 9), set(1, 2, 3)))   // False
```

### `is_superset(a, b)`

Whether `a` holds every value in `b`: `is_subset(b, a)`.

```oxi
println(sets.is_superset(set(1, 2, 3), set(3)))   // True
```

### `is_disjoint(a, b)`

Whether `a` and `b` have no values in common.

```oxi
println(sets.is_disjoint(set(1, 2), set(3, 4)))   // True
println(sets.is_disjoint(set(1, 2), set(2, 3)))   // False
```

To compare two sets for equality, use `==`: sets are equal when they hold
the same values, in any order.

---

## Transforms

### `to_array(s)`

The values as an array, in the set's order. To go the other way, use
`set(arr)`.

```oxi
println(sets.to_array(set(3, 1, 2)))   // [3, 1, 2]
```

### `filter(s, pred)`

Keep the values for which `pred` returns true.

```oxi
println(sets.filter(set(1, 2, 3, 4), fun(x) { x % 2 == 0 }))   // set(2, 4)
```

### `map(s, f)`

Apply `f` to every value. Results that come out equal are kept once, so the
new set can be smaller.

```oxi
println(sets.map(set(1, 2, 3), fun(x) { x % 2 }))   // set(1, 0)
```
