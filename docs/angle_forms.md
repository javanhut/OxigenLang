# OxigenLang Angle Forms

Angle forms are one of Oxigen's core syntax families. The same `<...>` shape is used for:

- type annotations in declarations
- zero-initialization targets
- typed walrus conversion
- value constructors
- explicit error and result handling
- postfix effects on expressions

The syntax stays the same, but the meaning depends on position.

## Mental Model

Read angle forms by context:

- after a binding name, `<...>` means "this binding has this type"
- after `as`, `<...>` means "initialize this binding to the zero value of this type"
- at the start of an expression, `<...>` means "construct or transform a value"
- after a completed expression, `<...>` means "apply an effect to this expression"

That gives Oxigen one reusable syntax family instead of separate type, result, and effect syntaxes.

## 1. Type Annotations

In declaration position, angle forms describe the type of a binding.

```oxi
name <str> = "Oxigen"
count <int> := 10
payload <map> := {"ok": True}
```

This is the same type system used across variables, struct fields, and function parameters.

## 2. Zero Initialization

With `as`, angle forms request the zero value for a type.

```oxi
count as <int>
name as <str>
items as <array>
```

Examples of zero values:

- `<int>` -> `0`
- `<str>` -> `""`
- `<bool>` -> `False`
- `<array>` -> `[]`
- `<map>` -> `{}`
- `<None>` -> `None`

This is useful when you want a typed binding before assigning a real value later.

## 3. Typed Walrus Conversion

With `:=`, angle forms can force a value into a target type when the conversion is supported.

```oxi
num <int> := "10"
label <str> := 42
chars <array> := "hello"
```

This differs from strict `=` declarations:

```oxi
num <int> = "10"    // error
num <int> := "10"   // converts to 10
```

Use:

- `=` when you want strict type locking
- `:=` when you want typed conversion plus mutability

## 4. Angle Constructors

At the start of an expression, angle forms construct values.

### Error Construction

```oxi
<Error>("failed")
<Error<network>>("connection lost")
<Error<retry_error>>("retry failed")
```

`<Error>(...)` creates an error value. It does not automatically propagate by itself.

Tagged errors let you group related failures without defining a custom error type:

```oxi
read_issue := <Error<file>>("missing file")
retry_issue := <Error<retry_error>>("request timed out")
```

Public fields on error values:

```oxi
err.msg
err.tag
```

Examples:

```oxi
err := <Error<network>>("offline")
println(err.msg)   // "offline"
println(err.tag)   // "network"
```

### Value Construction

```oxi
<Value>("ok")
<Value>(42)
<Value>(user)
```

`<Value>(...)` wraps a success value explicitly. This is most useful when working in an expected-result domain such as `<Error || Value>`.

The wrapped success can be accessed with `.value`:

```oxi
wrapped := <Value>("ok")
println(wrapped.value)
```

## 5. Expected-Result Normalization

One of the main uses of angle forms is to make expected failure explicit.

```oxi
result := <type<Error || Value>>(read_file("config"))
```

This means:

- evaluate `read_file("config")`
- if it succeeds, wrap the result as `Value(...)`
- if it fails, normalize that failure into `Error(...)`

So instead of immediate propagation, the result becomes a value that can be inspected.

### Why This Exists

Oxigen distinguishes between:

- unexpected runtime propagation
- explicitly expected fallible results

If you expect a call may fail and you want to handle that explicitly, normalize it:

```oxi
config <Error || Value> := <type<Error || Value>>(read_file("config"))
```

Examples:

```oxi
ok := <type<Error || Value>>("loaded")
println(ok.value)

failed := <type<Error || Value>>(<fail>("missing config"))
println(failed.msg)
```

You can use this in typed declarations:

```oxi
file_result <Error || Value> := <type<Error || Value>>(read_file("settings.oxi"))
```

## 6. Postfix Angle Effects

After a completed expression, angle forms behave like effect operators.

General shape:

```oxi
expr <effect>(...)
expr <effect<Type>> name -> handler
```

Current built-in effect forms are `guard` and `log`.

### `guard`

`guard` recovers from runtime errors by yielding a fallback expression.

```oxi
name := read_name() <guard>("Guest")
```

Behavior:

- if the left expression succeeds, its value is returned unchanged
- if it fails, the fallback is returned instead

Tagged filtering is supported:

```oxi
name := fetch_profile() <guard<Error<retry_error>>>("Guest")
```

That only handles tagged `retry_error` failures. Other errors still propagate.

### `log`

`log` intercepts matching errors and runs a handler expression.

```oxi
parsed := parse(text) <log<Error>> err -> println("parse failed: {err.msg}")
```

Behavior:

- on success, the original value passes through unchanged
- on matching error, the handler runs and its result becomes the expression result
- on non-matching tagged error, the original error keeps propagating

Tagged example:

```oxi
request()
    <log<Error<network>>> err -> println("network issue: {err.msg}")
```

## 7. Propagation with `fail`

`<fail>(...)` turns a value or error into a propagating runtime failure.

```oxi
<fail>("bad input")
<fail>(<Error<network>>("connection lost"))
```

Rules:

- plain values are converted to error messages
- `Error` values keep their message and tag
- propagation stops normal evaluation unless recovered later

This is the preferred modern form. The keyword form still exists:

```oxi
fail "bad input"
```

## 8. `error(...)` vs `<Error>(...)` vs `<fail>(...)`

These three forms are related but not identical.

### `error(...)`

Compatibility helper:

```oxi
error("missing file")
```

Use this mainly for older Oxigen code.

### `<Error>(...)`

Constructs an explicit error value:

```oxi
err := <Error>("missing file")
```

This is a value-level operation.

### `<fail>(...)`

Propagates a runtime error:

```oxi
<fail>("missing file")
<fail>(<Error<network>>("timeout"))
```

This is a control-flow operation.

## 9. Angle Forms Inside `option`

`option` can react to runtime errors with an explicit error arm:

```oxi
result := option {
    risky_call() -> {},
    <Error> -> "fallback"
}
```

This arm runs when:

- a condition evaluation errors
- a chosen body errors
- the default body errors

You can also rethrow explicitly:

```oxi
result := option {
    connected -> fetch_data(),
    <Error> -> <fail>("request failed")
}
```

## 10. Current Built-In Angle Forms

### Type-side

```oxi
<int>
<str>
<array>
<map>
<Error>
<Error<tag>>
<Value>
<Error || Value>
```

### Expression-side

```oxi
<Error>(...)
<Error<tag>>(...)
<Value>(...)
<type<T>>(...)
<fail>(...)
```

### Postfix effect-side

```oxi
expr <guard>(fallback)
expr <guard<Error<tag>>>(fallback)
expr <log<Error>> err -> handler
expr <log<Error<tag>>> err -> handler
```

## 11. Design Rules

If you are reading or writing new Oxigen code, these rules keep angle forms understandable:

1. Use declaration position for types.
2. Use prefix angle forms for construction or normalization.
3. Use postfix angle forms for recovery or handling.
4. Use `<Error<tag>>` when you need lightweight error categories.
5. Use `<type<Error || Value>>(expr)` when failure is expected and should stay explicit.
6. Use `<fail>(...)` when the error should propagate immediately.

## 12. Examples

### Explicit expected result

```oxi
config <Error || Value> := <type<Error || Value>>(read_file("config"))
```

### Tagged error construction

```oxi
issue := <Error<retry_error>>("request timed out")
println(issue.tag)
println(issue.msg)
```

### Guarded fallback

```oxi
name := read_name() <guard>("Guest")
```

### Tagged guard

```oxi
profile := fetch_profile() <guard<Error<retry_error>>>("Guest")
```

### Logged network error

```oxi
response := request()
    <log<Error<network>>> err -> println("network issue: {err.msg}")
```

### Rethrow with explicit failure

```oxi
option {
    initialized -> start(),
    <Error> -> <fail>("failed to initialize")
}
```
