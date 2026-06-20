# Testing

Oxigen has a built-in test runner. You write test cases with the `<test>` angle
form and run them with `oxigen test`.

## Writing tests

A test case is a `<test>("name") { ... }` block. Inside it, use `expect(value)`
followed by a matcher to make assertions:

```oxi
fun add(a <int>, b <int>) {
    a + b
}

<test>("addition works") {
    expect(add(2, 3)).eq(5)
    expect(add(0, 0)).eq(0)
}

<test>("comparisons and collections") {
    expect(add(2, 2)).gt(3)
    expect([1, 2, 3]).contains(2)
}
```

`expect` is provided automatically by the test runner — you do **not** need to
`introduce` anything.

A test **passes** when its body runs without producing an error. A test **fails**
as soon as any matcher fails (or any other runtime error occurs); evaluation of
that test stops at the first failure and the message is reported.

## Matchers

`expect(actual)` returns an expectation with these matchers:

| Matcher | Passes when |
|---------|-------------|
| `.eq(expected)` | `actual == expected` |
| `.ne(expected)` | `actual != expected` |
| `.gt(bound)` | `actual > bound` |
| `.gte(bound)` | `actual >= bound` |
| `.lt(bound)` | `actual < bound` |
| `.lte(bound)` | `actual <= bound` |
| `.truthy()` | `actual` is truthy |
| `.falsy()` | `actual` is falsy |
| `.contains(item)` | `actual` (array/set/map/tuple) contains `item` |
| `.is_value()` | `actual` is a `<Value>`-wrapped value |
| `.is_error()` | `actual` is an `<Error>`-tagged error value |

The matchers are implemented in pure Oxigen in `stdlib/test.oxi`, so you can read
or extend them.

## Running tests

```sh
oxigen test                 # discover and run every *_test.oxi under the cwd
oxigen test math_test.oxi   # run a single file
oxigen test ./tests         # run every *_test.oxi under a directory
```

Discovery is recursive and skips hidden directories, `target/`, and
`node_modules/`. By convention, test files end in `_test.oxi`.

Example output:

```
math_test.oxi
  ok   addition works
  FAIL comparisons and collections
       expected 5 but got 4

test result: FAILED. 1 passed; 1 failed
```

`oxigen test` exits with a non-zero status if any test fails, so it works in CI.

## Tests and normal runs

`<test>` blocks only execute under `oxigen test`. Running a file the normal way
(`oxigen file.oxi`) skips every `<test>` block, so you can keep tests next to the
code they exercise without affecting production runs.
