# random

Random integers, floats, booleans, and picks from an array.

```oxi
introduce random
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`rand_int`](#rand_intmin-max) | `rand_int(min, max)` | `int` in `[min, max]` |
| [`rand_float`](#rand_float) | `rand_float()` | `float` in `[0.0, 1.0)` |
| [`rand_bool`](#rand_bool) | `rand_bool()` | `bool` |
| [`choice`](#choicearr) | `choice(arr)` | one element |
| [`seed`](#seedn) | `seed(n)` | `None` |

> Not cryptographically secure. Do not use this for tokens, passwords, session
> IDs, or keys.

---

### `rand_int(min, max)`

A random integer between `min` and `max`, **both inclusive**.

```oxi
println(random.rand_int(1, 6))     // 4
println(random.rand_int(0, 1))     // 0
```

### `rand_float()`

A random float in `[0.0, 1.0)` — zero is possible, one is not.

```oxi
println(random.rand_float())   // 0.5488135039273248
```

Scale it for any other range:

```oxi
fun uniform(lo <float>, hi <float>) { lo + random.rand_float() * (hi - lo) }
```

### `rand_bool()`

A coin flip. Equivalent to `rand_int(0, 1) == 1`.

```oxi
println(random.rand_bool())   // True
```

### `choice(arr)`

One element picked uniformly at random.

```oxi
println(random.choice(["red", "green", "blue"]))   // green
```

An **empty array** is an error — `len(arr) - 1` is `-1`, so the index is out of
range. Guard it if the array might be empty.

### `seed(n)`

Seed the generator. The same seed replays the same sequence, which is what
makes a test with random input reproducible.

```oxi
introduce random

random.seed(42)
println(random.rand_int(1, 100))   // 26 — the same value on every run
```

Without a `seed` call the generator starts from an unpredictable value, so each
run differs.

---

## Worked example

```oxi
introduce random
introduce array

// Shuffle by decorating with random keys and sorting on them.
fun shuffled(arr <array>) {
    keyed := array.map(arr, fun(x) { (random.rand_int(0, 1000000), x) })
    array.map(array.sort_by(keyed, fun(p) { p[0] }), fun(p) { p[1] })
}

random.seed(7)
println(shuffled([1, 2, 3, 4, 5]))   // [2, 5, 3, 4, 1]  (same every run, given the seed)

// Weighted coin: True about 30% of the time.
fun chance(p <float>) { random.rand_float() < p }

println(chance(0.3))
```

---

See also: [math](math.md) for numeric helpers, [hash](hash.md) for digests
(also not a random source).
