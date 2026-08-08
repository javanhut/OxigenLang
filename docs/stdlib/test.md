# test

The assertion matchers behind `oxigen test`. `expect(value)` wraps a value so
you can assert on it fluently.

```oxi
<test>("addition works") {
    expect(2 + 3).eq(5)
    expect([1, 2, 3]).contains(2)
}
```

**You do not import this module.** The test runner injects `expect` into every
`*_test.oxi` file automatically, so there is no `introduce test` in a test
file. (The module is importable if you want the matchers elsewhere:
`introduce {expect} from test`.)

For the test runner itself — file discovery, the CLI, output format — see
[testing.md](../testing.md).

| Matcher | Passes when |
|---------|-------------|
| [`.eq(expected)`](#eqexpected) | `actual == expected` |
| [`.ne(expected)`](#neexpected) | `actual != expected` |
| [`.gt(bound)`](#gtbound) | `actual > bound` |
| [`.gte(bound)`](#gtebound) | `actual >= bound` |
| [`.lt(bound)`](#ltbound) | `actual < bound` |
| [`.lte(bound)`](#ltebound) | `actual <= bound` |
| [`.truthy()`](#truthy) | `actual` is truthy |
| [`.falsy()`](#falsy) | `actual` is falsy |
| [`.contains(item)`](#containsitem) | the collection contains `item` |
| [`.is_error()`](#is_error) | `actual` is an error value |
| [`.is_value()`](#is_value) | `actual` is an explicitly wrapped `<Value>` |

---

## `expect(actual)`

Wrap a value in an [`Expectation`](#struct-expectation) so a matcher can be
called on it.

```oxi
expect(compute()).eq(42)
```

A failing matcher calls `<fail>`, which produces an error and
**short-circuits the rest of the test body** — statements after the first
failure in a test do not run. Failure messages are returned plain; the
`oxigen test` runner colorizes them when the terminal supports it.

---

## Matchers

### `.eq(expected)`

```oxi
expect(2 + 3).eq(5)
```

Failure message: `expected 5 but got 4`.

### `.ne(expected)`

```oxi
expect(roll()).ne(0)
```

Failure message: `expected value to differ from 0`.

### `.gt(bound)`

```oxi
expect(len(results)).gt(0)
```

Failure message: `expected 0 to be greater than 0`.

### `.gte(bound)`

```oxi
expect(score).gte(60)
```

### `.lt(bound)`

```oxi
expect(elapsed_ms).lt(1000)
```

### `.lte(bound)`

```oxi
expect(len(queue)).lte(10)
```

### `.truthy()`

```oxi
expect(user["active"]).truthy()
```

Failure message: `expected a truthy value but got False`.

### `.falsy()`

```oxi
expect(strings.is_empty(name)).falsy()
```

### `.contains(item)`

Works on any collection the `has` builtin accepts — array, set, map (checks
keys), tuple. **Not strings**: `expect("hello").contains("ell")` fails with
`has() requires a collection`. Use `.truthy()` over
[`strings.contains`](strings.md#contains_strs-sub--containss-sub) for a
substring check.

```oxi
expect([1, 2, 3]).contains(2)
expect({"a": 1}).contains("a")
expect((1, 2)).contains(2)
expect(strings.contains("hello", "ell")).truthy()
```

Failure message: `expected [1, 2, 3] to contain 9`.

### `.is_error()`

Asserts the value is an error. Pair it with
[`<type<Error> || <Value>>(...)`](../angle_forms.md) so the error is a value
rather than something that halts the test.

```oxi
<test>("rejects a negative factorial") {
    expect(<type<Error> || <Value>>(math.factorial(-1))).is_error()
}
```

### `.is_value()`

Asserts the value is an **explicitly wrapped** `<Value>`. This is the `is_value`
builtin, not "is not an error" — `expect(42).is_value()` **fails** with
`expected a value but got an error: 42`.

```oxi
expect(<Value>(42)).is_value()                       // passes
expect(<type<Error> || <Value>>(int("7"))).is_value()  // passes
expect(42).is_value()                                // FAILS
```

For "this did not fail", assert on the value itself with `.eq`, or use
[`result.is_ok`](result.md#is_okv).

---

## Struct: `Expectation`

| Field | Type | Contents |
|-------|------|----------|
| `actual` | `generic` | the value passed to `expect` |

`expect(x)` is exactly `Expectation(actual=x)`. The matchers are ordinary
methods on this struct, implemented in pure Oxigen in `stdlib/test.oxi` — read
it if you want to know precisely what a matcher does.

---

## Worked example

```oxi
// math_test.oxi
introduce math

fun add(a <int>, b <int>) { a + b }

<test>("addition") {
    expect(add(2, 3)).eq(5)
    expect(add(0, 0)).eq(0)
    expect(add(2, 2)).gt(3)
}

<test>("factorial rejects negatives") {
    expect(math.factorial(5)).eq(120)
    expect(<type<Error> || <Value>>(math.factorial(-1))).is_error()
}
```

```
$ oxigen test math_test.oxi
math_test.oxi
  [ok]   addition
  [ok]   factorial rejects negatives

test result: ok. 2 passed; 0 failed
```

A failure names the test and the mismatch:

```
math_test.oxi
  [fail] addition
       expected 5 but got 4

test result: FAILED. 0 passed; 1 failed
```

---

See also: [testing.md](../testing.md) for the runner and conventions,
[result](result.md) for the `Error || Value` helpers.
