# OxigenLang Angle Forms

Angle forms are one of Oxigen's core syntax families. The same `<...>` bracket shape is reused throughout the language for several different purposes:

- type annotations in declarations
- zero-initialization targets
- typed walrus conversion
- value constructors (creating error and success values)
- explicit error and result handling
- postfix effects on expressions (recovery, logging)

The syntax stays the same, but the meaning depends on where the angle form appears in a statement.

This guide explains every angle form from scratch. If you have never written Oxigen before, start here.

---

## Mental Model: How to Read Angle Forms

Whenever you see `<...>` in Oxigen, look at what comes **before** the opening `<` bracket. That tells you what the angle form is doing:

| What comes before `<` | What `<...>` means | Example |
|---|---|---|
| A variable name | Type annotation — declares what type this variable holds | `name <str> = "hello"` |
| The keyword `as` | Zero-initialize — create an empty value of this type | `count as <int>` |
| Nothing (start of expression) | Constructor — build a value like an error or success wrapper | `<Error>("failed")` |
| A completed expression | Effect — do something with the result of that expression | `expr <guard>("fallback")` |

One bracket family, four roles. Position is everything.

---

## Understanding Oxigen's Two-Tier Error Model

Before diving into syntax, you need to understand how Oxigen thinks about errors. This is the foundation for everything that follows.

Oxigen has **two fundamentally different kinds** of error objects. Confusing them is the most common mistake when learning angle forms.

### Tier 1: Propagating Errors (the "halt everything" kind)

A **propagating error** is a runtime failure that **immediately stops your program**. When a propagating error occurs, it bubbles up through every function call and halts evaluation — nothing after it runs unless something explicitly catches it.

You create propagating errors with `<fail>` or `fail`:

```oxi
<fail>("something went wrong")
fail "something went wrong"
```

Think of these like **unhandled exceptions** in Python or Java. If nothing catches them, your program stops.

**Example — propagating error halts everything:**

```oxi
<fail>("broken")
println("this line never runs")
// The program stopped at <fail>. Nothing below it executes.
```

### Tier 2: Error Values (the "data" kind)

An **error value** is a piece of data that *describes* a failure but does **not** halt anything. It sits in a variable like a string or integer. You can inspect it, pass it to functions, store it in arrays, or ignore it entirely.

You create error values with `<Error>`:

```oxi
err := <Error>("something went wrong")
```

This does **not** stop your program. It creates a value you can hold onto.

**Example — error value does NOT halt anything:**

```oxi
err := <Error>("broken")
println("this line runs just fine")
println(err.msg)    // prints: broken
// The program keeps running. err is just a value sitting in a variable.
```

### Why Two Tiers?

Most languages force you into one model: either everything is an exception that you `try/catch` (Python, Java, JavaScript) or everything is a result type that you must unwrap (Rust, Go). Oxigen gives you both and lets you choose:

- **Use propagating errors** (`<fail>`) when something is genuinely broken and the caller should not silently continue. The program will stop unless the caller explicitly handles it.
- **Use error values** (`<Error>`) when you want to represent failure as data — to inspect it, branch on it, collect multiple errors, or pass it around between functions.

The angle form system lets you move between these two tiers explicitly. You can take a propagating error and turn it into a value (with `<type<Error || Value>>`), or take an error value and turn it into a propagating error (with `<fail>`).

### Key Behavioral Differences

| | Propagating Error (`<fail>`) | Error Value (`<Error>`) |
|---|---|---|
| Halts program? | Yes — stops evaluation immediately | No — just a value |
| Created with | `<fail>("msg")` or `fail "msg"` | `<Error>("msg")` |
| Is truthy? | No (falsy) | Yes (truthy — it is a real value) |
| Can be stored in variable? | No — it propagates before assignment | Yes — sits in a variable like any value |
| How to inspect | Must catch it first (with `guard` or `option <Error>`) | Access `.msg` and `.tag` directly |

---

## 1. Type Annotations

In declaration position (after a variable name, before `=` or `:=`), angle forms describe the **type** of a binding.

### What the Syntax Means

```oxi
name <str> = "Oxigen"
```

Reading this left to right:

1. `name` — the variable name
2. `<str>` — the angle form says "this variable holds a string"
3. `=` — strict assignment (the value must already be a string)
4. `"Oxigen"` — the value being assigned

### More Examples

```oxi
count <int> := 10           // count holds an integer, := means mutable
payload <map> := {"ok": True}  // payload holds a map
active <bool> = True         // active holds a boolean, = means immutable
```

### Why Type Annotations Exist

Without a type annotation, Oxigen infers the type from the value. But explicit types serve two purposes:

1. **Documentation** — reading `count <int> := 10` tells you the variable is meant to hold integers.
2. **Type safety** — Oxigen will reject assignments of the wrong type:

```oxi
count <int> := 10
count = "hello"    // error: type mismatch — count is locked to int
```

This is the same type system used across variables, struct fields, and function parameters.

---

## 2. Zero Initialization

With the `as` keyword, angle forms create an **empty value** of the specified type.

### What the Syntax Means

```oxi
count as <int>
```

Reading this left to right:

1. `count` — the variable name
2. `as` — the keyword that says "initialize this to the default"
3. `<int>` — the type to create a zero value for

After this line, `count` exists and holds `0` (the zero value for integers).

### All Zero Values

Each type has a specific zero value:

| Type | Zero Value | What it looks like |
|---|---|---|
| `<int>` | `0` | The integer zero |
| `<float>` | `0.0` | The float zero |
| `<str>` | `""` | An empty string |
| `<bool>` | `False` | The boolean false |
| `<array>` | `[]` | An empty array |
| `<map>` | `{}` | An empty map |
| `<None>` | `None` | The None value |

### When to Use Zero Initialization

Use `as` when you need a typed variable **before** you have a value for it. This is common when the value will be set inside a conditional or loop:

```oxi
result as <str>

option {
    ready -> result = fetch_data(),
    result = "not ready"
}

println(result)
```

Without `as <str>`, the variable `result` would not exist before the `option` block, and the assignments inside would fail.

### Why Not Just Write `result <str> := ""`?

You can. `as <str>` is a shorthand that means the same thing as `:= ""` for strings, `:= 0` for ints, etc. Use whichever reads more clearly. `as` is especially useful when you do not have a meaningful initial value.

---

## 3. Typed Walrus Conversion

With `:=` (the walrus operator), angle forms can **force a value into a different type** when the conversion is supported.

### What the Syntax Means

```oxi
num <int> := "10"
```

Reading this left to right:

1. `num` — the variable name
2. `<int>` — the target type
3. `:=` — the walrus operator, which means "convert if needed and allow mutation"
4. `"10"` — the value (a string)

Oxigen sees that `"10"` is a string but the target type is `<int>`. Because `:=` was used (not `=`), Oxigen attempts to convert the string `"10"` to the integer `10`. This succeeds, and `num` holds `10`.

### More Conversion Examples

```oxi
label <str> := 42          // integer 42 becomes string "42"
chars <array> := "hello"   // string "hello" becomes ["h", "e", "l", "l", "o"]
flag <bool> := 1           // integer 1 becomes True
```

### Strict vs Walrus Assignment

The difference between `=` and `:=` is critical:

```oxi
num <int> = "10"     // ERROR: type mismatch — "10" is a string, not an int
num <int> := "10"    // OK: converts "10" to 10
```

**`=` (strict):**
- The value must already be the correct type
- No conversion is attempted
- The binding is **immutable** (cannot be reassigned)

**`:=` (walrus):**
- Oxigen will attempt type conversion
- If conversion is impossible, you get an error
- The binding is **mutable** (can be reassigned later)

### When Conversion Fails

If the conversion is not possible, you get a propagating error:

```oxi
num <int> := "hello"    // error: cannot convert "hello" to int
```

---

## 4. Error Construction with `<Error>`

`<Error>(...)` creates an **error value** (tier 2). This is a **value-level** operation — it does **not** halt your program.

### What the Syntax Means

```oxi
<Error>("file not found")
```

Breaking this down:

1. `<Error>` — the angle form that says "construct an error value"
2. `(` — open parenthesis, start of the argument
3. `"file not found"` — the error message (any expression that produces a value)
4. `)` — close parenthesis

The result is an error value object. You typically assign it to a variable:

```oxi
err := <Error>("file not found")
```

Now `err` holds an error value. Your program is still running. You can inspect it:

```oxi
println(err.msg)    // prints: file not found
println(type(err))  // prints: ERROR_VALUE
```

### Tagged Errors

Add a **tag** inside nested angle brackets to categorize the error:

```oxi
<Error<network>>("connection timed out")
```

Breaking this down:

1. `<Error` — start of the error constructor
2. `<network>` — the tag, nested inside the outer angle brackets
3. `>` — close the outer angle bracket
4. `("connection timed out")` — the error message

The tag is a free-form string identifier. It does not need to be declared anywhere — just pick a name that describes the category of failure.

```oxi
err := <Error<network>>("connection timed out")
println(err.msg)    // "connection timed out"
println(err.tag)    // "network"
```

### Why Tags Exist

Tags give you a way to group related errors without defining custom error types or classes. They are lightweight labels:

```oxi
<Error<file>>("missing config")          // file-related error
<Error<auth>>("invalid token")           // authentication error
<Error<parse>>("unexpected character")   // parsing error
<Error<retry_error>>("max retries hit")  // retryable error
```

Later, when you catch errors with `guard` or `log`, you can filter by tag — only handling specific categories of errors and letting others propagate.

### Public Fields on Error Values

Every error value has two fields you can access with dot notation:

| Field | Type | Description |
|---|---|---|
| `.msg` | `str` | The error message you passed to `<Error>` |
| `.tag` | `str` or `None` | The tag if you used `<Error<tag>>`, otherwise `None` |

```oxi
tagged := <Error<network>>("offline")
println(tagged.msg)    // "offline"
println(tagged.tag)    // "network"

plain := <Error>("something broke")
println(plain.msg)     // "something broke"
println(plain.tag)     // None
```

### When to Use `<Error>`

Use `<Error>` when you want to represent a failure as **data** that you will inspect, store, or make decisions about later:

```oxi
// Collect multiple validation errors instead of stopping at the first one
issues <array> := []
push(issues, <Error<validation>>("name is required"))
push(issues, <Error<validation>>("email is invalid"))

each issue in issues {
    println("{issue.tag}: {issue.msg}")
}
// prints:
// validation: name is required
// validation: email is invalid
```

This would not work with `<fail>` because the program would stop at the first error.

---

## 5. Success Wrapping with `<Value>`

`<Value>(...)` wraps any value as an explicit **success**. It is the counterpart to `<Error>`.

### What the Syntax Means

```oxi
<Value>("ok")
```

Breaking this down:

1. `<Value>` — the angle form that says "wrap this as a success value"
2. `("ok")` — the value to wrap

The result is a `Value` object. You access the inner value with `.value`:

```oxi
wrapped := <Value>("loaded successfully")
println(wrapped.value)    // "loaded successfully"
println(type(wrapped))    // "VALUE"
```

### Why `<Value>` Exists

On its own, `<Value>` might seem pointless — why wrap a value that is already fine? The answer is that `<Value>` pairs with `<Error>` to form the **expected-result** pattern.

When a function might succeed or fail, you want a uniform way to represent both outcomes as values. `<Value>` and `<Error>` give you that:

```oxi
fun safe_divide(a <int>, b <int>) {
    option {
        b == 0 -> <Error<math>>("division by zero"),
        <Value>(a / b)
    }
}

result := safe_divide(10, 2)
println(result.value)    // 5

result := safe_divide(10, 0)
println(result.msg)      // "division by zero"
println(result.tag)      // "math"
```

Both paths return a value — one is a `Value`, the other is an `ErrorValue`. Neither halts the program. The caller decides what to do.

---

## 6. Expected-Result Normalization with `<type<Error || Value>>`

This is one of the most powerful angle forms. It takes any expression — even one that might produce a propagating error — and converts the result into either a `Value` or an `ErrorValue`. The error is **caught** and turned into data instead of halting your program.

### The Problem It Solves

Normally, if a function fails with a propagating error, your program stops:

```oxi
data := read_file("missing.txt")    // propagating error — program halts here
println("this never runs")
```

But sometimes you **expect** a call might fail and you want to handle both outcomes. You do not want the error to halt your program — you want to inspect it and decide what to do.

### What the Syntax Means

```oxi
<type<Error || Value>>(read_file("config.txt"))
```

Breaking this down:

1. `<type` — starts a type-conversion angle form
2. `<Error || Value>` — the target type is "either an Error or a Value"
3. `>` — closes the outer angle bracket
4. `(read_file("config.txt"))` — the expression to evaluate

### How It Works, Step by Step

```oxi
result := <type<Error || Value>>(read_file("config.txt"))
```

Here is what happens:

1. Oxigen evaluates `read_file("config.txt")` inside the angle form.
2. **If the call succeeds:** The return value is wrapped as `Value(...)`. You can access it with `result.value`.
3. **If the call fails** (produces a propagating error): The error is **caught** and converted to an `ErrorValue`. The propagation stops. You can access it with `result.msg` and `result.tag`.
4. **Either way, your program continues.** The error is now a value, not a propagation.

### Checking Which Outcome You Got

After normalization, you have a value that is either a `Value` or an `ErrorValue`. Use `type()` to check:

```oxi
result := <type<Error || Value>>(read_file("settings.oxi"))

option {
    type(result) == "VALUE" -> {
        // Success path — use result.value
        println("File contents: {result.value}")
    },
    {
        // Failure path — use result.msg
        println("Could not read file: {result.msg}")
    }
}
```

### Why `type()` Works for This

- `type(result)` returns `"VALUE"` if the call succeeded
- `type(result)` returns `"ERROR_VALUE"` if the call failed

These are the type names of the `Value` and `ErrorValue` objects.

### With Typed Declarations

You can declare the variable with the `Error || Value` type annotation to document what it holds:

```oxi
config <Error || Value> := <type<Error || Value>>(read_file("config"))
```

This reads as: "config holds either an Error or a Value, and we got it by normalizing `read_file("config")`."

### Quick Examples

```oxi
// Success path — wraps "loaded" as a Value
ok := <type<Error || Value>>("loaded")
println(ok.value)    // "loaded"

// Failure path — catches the propagating error and wraps it as ErrorValue
failed := <type<Error || Value>>(<fail>("missing config"))
println(failed.msg)  // "missing config"
// Note: <fail> would normally halt the program, but <type<Error || Value>> caught it
```

### When to Use `<type<Error || Value>>`

Use it whenever you are calling something that might fail and you want to:
- Keep the program running regardless of the outcome
- Inspect the error as data instead of crashing
- Make decisions based on success or failure

---

## 7. Propagating Failures with `<fail>`

`<fail>(...)` creates a **propagating** runtime error (tier 1). This is a **control-flow** operation — it halts normal evaluation immediately.

### What the Syntax Means

```oxi
<fail>("invalid input")
```

Breaking this down:

1. `<fail>` — the angle form that says "produce a propagating error"
2. `("invalid input")` — the error message

After this line, nothing else in the current scope runs. The error propagates up through every caller until something catches it (a `guard`, an `option <Error>` arm, or a `<type<Error || Value>>` wrapper).

### From Plain Values

Any value passed to `<fail>` is converted to an error message string:

```oxi
<fail>("bad input")     // message: "bad input"
<fail>(42)              // message: "42"
<fail>(True)            // message: "True"
```

### From Error Values

If you pass an `ErrorValue` to `<fail>`, it preserves the message and tag. This is how you **upgrade** a tier-2 error value into a tier-1 propagating error:

```oxi
err := <Error<network>>("connection lost")

// err is just a value — the program is still running
// Now convert it to a propagating error:
<fail>(err)

// This propagating error has tag "network" and message "connection lost"
// The program halts here
```

### Keyword Form

The keyword form `fail` does the same thing as `<fail>`:

```oxi
fail "bad input"
```

Both forms are supported. `<fail>` is the preferred modern form because it is visually consistent with other angle forms.

### When to Use `<fail>`

Use `<fail>` inside a function when you have encountered a condition where the function **cannot continue** and needs to signal failure to its caller:

```oxi
fun parse_age(input <str>) {
    age <int> := input
    option {
        age < 0 -> <fail>(<Error<validation>>("age cannot be negative")),
        age > 150 -> <fail>(<Error<validation>>("age seems unrealistic")),
        age
    }
}
```

Here `<fail>` is used with `<Error<tag>>` together: `<Error<validation>>(...)` creates a tagged error value, and `<fail>(...)` converts it into a propagating error. The caller of `parse_age` will see a propagating error with tag `"validation"`.

---

## 8. `error(...)` vs `<Error>(...)` vs `<fail>(...)`

These three forms are related but serve different purposes. This section clarifies exactly when to use each one.

### `error(...)` — Legacy Form

```oxi
error("missing file")
```

This is the old compatibility form. It creates a propagating error (like `<fail>`). Use it only if you are working with older Oxigen code. Prefer `<fail>` for new code.

### `<Error>(...)` — Create Error Value (Does NOT Halt)

```oxi
err := <Error>("missing file")
```

Creates an error value. The program keeps running. You get an object with `.msg` and `.tag` fields. Use this when you want failure represented as **data**.

### `<fail>(...)` — Create Propagating Error (Halts Program)

```oxi
<fail>("missing file")
```

Creates a propagating error. The program stops. Use this when the current function cannot continue and needs to tell its caller about the failure.

### Decision Guide

Ask yourself:

1. **Do I want the program to keep running?** Use `<Error>("msg")` — it creates a value, not a halt.
2. **Do I want the program to stop and signal failure?** Use `<fail>("msg")` — it propagates.
3. **Do I need a tag on the error?** Use `<Error<tag>>("msg")` for error values, or `<fail>(<Error<tag>>("msg"))` to propagate a tagged error.
4. **Am I working with old code?** `error("msg")` still works, but prefer `<fail>` in new code.

### Quick Comparison

| Form | What it produces | Halts program? | Best for |
|---|---|---|---|
| `error("msg")` | Propagating error | Yes | Legacy code |
| `<Error>("msg")` | Error value | No | Representing failure as data |
| `<Error<tag>>("msg")` | Tagged error value | No | Categorized failure as data |
| `<fail>("msg")` | Propagating error | Yes | Signaling failure to callers |
| `<fail>(<Error<tag>>("msg"))` | Tagged propagating error | Yes | Signaling categorized failure |

---

## 9. Error Recovery with `guard`

`guard` catches propagating errors and replaces them with a fallback value. It converts a tier-1 propagating error back into normal program flow.

Think of `guard` as a safety net: "try this expression, but if it fails, use this fallback instead."

### Angle Form (Preferred)

```oxi
name := read_name() <guard>("Guest")
```

**What happens step by step:**

1. `read_name()` is evaluated.
2. **If it succeeds:** Its return value is used. `name` gets that value. The `<guard>` part does nothing.
3. **If it fails** (propagating error): The error is caught, discarded, and `"Guest"` is used instead. `name` gets `"Guest"`.

**Breaking down the syntax:**

1. `read_name()` — the expression that might fail
2. `<guard>` — the angle effect that says "catch errors from the left expression"
3. `("Guest")` — the fallback value to use if the left expression fails

### With a Binding (Keyword Form)

When you need to **access the caught error** to decide the fallback, use the keyword form with `guard err ->`:

```oxi
name := read_name() guard err -> "Unknown (error: {err.msg})"
```

**What happens step by step:**

1. `read_name()` is evaluated.
2. **If it succeeds:** Its return value is used. The guard does nothing.
3. **If it fails:** The error is caught and bound to the variable `err`. The `err` variable is an error value with `.msg` and `.tag` fields. The expression after `->` is evaluated and its result becomes the value.

The `err` binding is **only visible** inside the fallback expression (after `->`). You cannot use it outside.

### Tagged Filtering

Add a tag filter to only catch **specific categories** of errors:

```oxi
profile := fetch_profile() <guard<Error<retry_error>>>("Guest")
```

**Breaking down the syntax:**

1. `fetch_profile()` — the expression that might fail
2. `<guard` — start of the guard effect
3. `<Error<retry_error>>` — the tag filter: only catch errors tagged `retry_error`
4. `>` — close the guard angle bracket
5. `("Guest")` — the fallback

**What happens step by step:**

1. `fetch_profile()` is evaluated.
2. **If it succeeds:** The value passes through. Guard does nothing.
3. **If it fails with a `retry_error` tag:** The error is caught. `"Guest"` is used.
4. **If it fails with a different tag** (like `network` or `auth`) **or no tag:** The error is **not caught**. It keeps propagating. The guard only handles the specific tag you specified.

This is powerful because it lets you handle expected failures while allowing unexpected ones to propagate normally.

### What `guard` Does NOT Catch

`guard` only catches **errors** (both propagating errors and error values). It does **not** catch other values:

```oxi
result := None guard err -> "fallback"
// result is None, NOT "fallback"
// guard did not fire because None is not an error
```

`None` is a value, not an error. Guard ignores it completely.

### Keyword Form at Statement Level

The keyword form can also be used at the statement level, without assignment:

```oxi
println(fail "boom") guard err -> err.msg
```

Here:
1. `println(fail "boom")` — `fail "boom"` creates a propagating error before `println` can run.
2. `guard err -> err.msg` — catches the error, binds it to `err`, and evaluates `err.msg` which returns the string `"boom"`.

---

## 10. Side-Effect Observation with `log`

`<log>` intercepts matching errors and runs a handler for its **side effects** (like printing or recording). Unlike `guard`, **`log` does not recover from the error**. After the handler runs, the original error **continues propagating**.

Think of `log` as a window: you can observe the error as it passes through, but you do not catch it.

### What the Syntax Means

```oxi
parse(text) <log<Error>> err -> println("parse failed: {err.msg}")
```

Breaking this down:

1. `parse(text)` — the expression that might fail
2. `<log` — start of the log effect
3. `<Error>` — the filter: intercept errors (you can also use `<Error<tag>>` for specific tags)
4. `>` — close the log angle bracket
5. `err` — the binding name for the caught error
6. `->` — separates the binding from the handler
7. `println("parse failed: {err.msg}")` — the handler expression that runs for side effects

### How It Works, Step by Step

```oxi
parse(text) <log<Error>> err -> println("parse failed: {err.msg}")
```

1. `parse(text)` is evaluated.
2. **If it succeeds:** The value passes through unchanged. The handler does not run. `log` has no effect.
3. **If it fails with a matching error:**
   - The error is bound to `err` as an error value (with `.msg` and `.tag` fields).
   - The handler expression `println(...)` runs — its side effect (printing to the console) happens.
   - The handler's **return value is discarded**.
   - The **original error continues propagating** — it is NOT caught, NOT replaced.
4. **If it fails with a non-matching tagged error:** The handler does not run. The error propagates unchanged.

### `log` vs `guard` — The Critical Difference

| | `guard` | `log` |
|---|---|---|
| Purpose | **Recover** from an error | **Observe** an error without recovering |
| What happens to the error | Caught and replaced by the fallback | Continues propagating after handler runs |
| Handler's return value | Becomes the new value (replaces the error) | Discarded — does not affect anything |
| Use when | You have a fallback value and want to keep going | You want to record, print, or log the error but still let it propagate |

**Example showing the difference:**

```oxi
// guard: catches the error, returns "default" — program continues normally
result := <fail>("boom") <guard>("default")
// result is "default"

// log: runs the handler, but the error still propagates — program halts
<fail>("boom") <log<Error>> err -> println("saw: {err.msg}")
// The println runs (you see "saw: boom" in the console)
// But then the error continues propagating and the program halts
```

### Tagged Filtering

Just like `guard`, `log` supports tag filters:

```oxi
request()
    <log<Error<network>>> err -> println("network issue: {err.msg}")
```

This only runs the handler for errors tagged `network`. Errors with other tags (or no tag) pass through without triggering the handler.

### Why `log` Exists

`log` exists for scenarios where you want to **observe** errors without **hiding** them:

- **Debugging:** Print error details to the console while letting the error propagate to its normal handler.
- **Telemetry:** Record the error to a log file or monitoring system without changing program behavior.
- **Tracing:** See which errors pass through a particular point in your code.

If `log` caught the error (like `guard` does), you would accidentally swallow failures. `log` deliberately avoids that.

---

## 11. Angle Forms Inside `option`

`option` is Oxigen's multi-arm conditional. It evaluates conditions top to bottom and runs the first truthy arm. Angle forms extend `option` with explicit error handling through the **`<Error>` arm**.

### How `option` Works (Quick Review)

```oxi
status := option {
    age >= 18 -> "adult",
    "minor"
}
```

- Each arm has a condition (`age >= 18`) and a body (`"adult"`), separated by `->`.
- The last arm without `->` is the default.
- The first truthy condition wins. Its body is evaluated and becomes the result.

### The `<Error>` Arm

You can add an `<Error> -> body` arm to `option`. This arm catches any propagating error that occurs **anywhere** during the `option` evaluation:

```oxi
result := option {
    risky_call() -> "success",
    <Error> -> "something went wrong"
}
```

**Breaking down the syntax:**

1. `option { ... }` — the conditional block
2. `risky_call() -> "success"` — a normal arm: if `risky_call()` is truthy, return `"success"`
3. `<Error> -> "something went wrong"` — the error arm: if any propagating error occurs, return `"something went wrong"`

### When Does the `<Error>` Arm Fire?

The `<Error>` arm fires in **three situations**. Understanding all three is important.

**Situation 1: A condition evaluation produces an error.**

If evaluating the condition on the **left** side of `->` produces a propagating error, the `<Error>` arm catches it:

```oxi
result := option {
    missing_variable == 1 -> "found it",
    <Error> -> "condition failed"
}
// result is "condition failed"
```

Why this works: `missing_variable` is not defined. Evaluating `missing_variable == 1` produces a propagating error ("identifier not found"). Normally this would halt the program, but the `<Error>` arm catches it and returns `"condition failed"`.

**Situation 2: A matched body produces an error.**

If a condition is truthy but evaluating its **body** (the right side of `->`) produces an error, the `<Error>` arm catches it:

```oxi
result := option {
    True -> nonexistent_function(),
    <Error> -> "body failed"
}
// result is "body failed"
```

Why this works: The condition `True` is truthy, so Oxigen tries to evaluate the body `nonexistent_function()`. That produces a propagating error. The `<Error>` arm catches it.

**Situation 3: The default body produces an error.**

If no condition matches and the default arm (the last arm without `->`) itself produces an error:

```oxi
result := option {
    False -> "nope",
    nonexistent_function(),
    <Error> -> "default failed"
}
// result is "default failed"
```

Why this works: `False` does not match, so Oxigen falls through to the default arm `nonexistent_function()`. That errors. The `<Error>` arm catches it.

### Without an `<Error>` Arm

If there is no `<Error>` arm, errors propagate normally and can halt your program:

```oxi
result := option {
    missing == 1 -> "found"
}
// This propagates the "identifier not found" error — program halts
```

The `<Error>` arm is **optional**. Only add it when you want `option` to handle errors internally.

### Rethrowing from the `<Error>` Arm

You can use `<fail>` inside the `<Error>` arm to propagate a **different** error with a clearer message:

```oxi
result := option {
    connected -> fetch_data(),
    <Error> -> <fail>("request failed")
}
```

This catches whatever error occurred (maybe "connection refused" or "DNS lookup failed") and replaces it with a single clear message: "request failed". The new error propagates to the caller.

### Using Error Values in `option` Conditions

Because error values (created with `<Error>`) are **truthy**, they work as `option` conditions. This lets you branch on error properties:

```oxi
err := <Error<auth>>("token expired")

message := option {
    err.tag == "auth" -> "please log in again",
    err.tag == "network" -> "check your connection",
    "unknown error: {err.msg}"
}
println(message)    // "please log in again"
```

This works because `err` is a value (tier 2), not a propagating error. It sits in a variable and you can inspect its `.tag` field in conditions.

### Complete `option` + Error Example

Here is a realistic example: a function that tries to read a config file and falls back to defaults if anything fails.

```oxi
fun load_config() {
    // Wrap the file read so it doesn't propagate
    file_result <Error || Value> := <type<Error || Value>>(read_file("config.oxi"))

    config := option {
        type(file_result) == "VALUE" -> file_result.value,
        {
            // File read failed — try environment variable
            env_result <Error || Value> := <type<Error || Value>>(get_env("APP_CONFIG"))

            option {
                type(env_result) == "VALUE" -> env_result.value,
                <fail>(<Error<config>>("no config source available"))
            }
        }
    }

    config
}
```

Step by step:

1. `read_file("config.oxi")` is wrapped with `<type<Error || Value>>` so it does not propagate.
2. If the file read succeeded (`type == "VALUE"`), use its contents.
3. If the file read failed, try the environment variable (also wrapped).
4. If both failed, propagate a tagged error with `<fail>`.

---

## 12. Angle Forms and `choose` Patterns

`choose` evaluates a subject value against named patterns. It is different from `option` — `option` checks boolean conditions, while `choose` matches a value against named patterns.

### How `choose` Works (Quick Review)

```oxi
pattern is_even(n) when n % 2 == 0
pattern is_odd(n) when n % 2 != 0

choose 42 {
    is_even -> "even",
    is_odd -> "odd"
}
```

The subject (`42`) is passed to each pattern. The first pattern whose condition is truthy wins.

### Error Flow in `choose`

Errors can occur at three points during a `choose`. Unlike `option`, `choose` does **not** have a built-in `<Error>` arm. Errors propagate immediately.

**Point 1: Subject evaluation errors.**

If the subject expression itself errors, the error propagates and the `choose` stops:

```oxi
choose nonexistent_function() {
    is_even -> "even",
    else -> "other"
}
// propagating error — program halts
```

**Point 2: Pattern condition errors.**

If evaluating a pattern's `when` condition produces an error, it propagates:

```oxi
pattern bad_pattern(n) when n / 0 == 1

choose 42 {
    bad_pattern -> "matched",
    else -> "other"
}
// propagating error from the division by zero
```

**Point 3: Arm body errors.**

If the matched arm's body expression errors, it propagates:

```oxi
choose 42 {
    is_even -> nonexistent_function(),
    else -> "other"
}
// propagating error from the body
```

### Protecting `choose` with `guard`

Since `choose` has no built-in error arm, use `guard` to recover from errors:

```oxi
result := (choose some_value {
    is_valid -> process(some_value),
    else -> "unknown"
}) guard err -> "error during classification: {err.msg}"
```

The parentheses `(...)` around the `choose` let the `guard` apply to the whole `choose` expression. Without them, `guard` would only apply to the last arm's body.

### Protecting `choose` with `option` + `<Error>`

Wrap the `choose` inside an `option` to use the `<Error>` arm:

```oxi
result := option {
    True -> {
        choose user_input {
            is_number -> parse_number(user_input),
            is_date -> parse_date(user_input),
            else -> user_input
        }
    },
    <Error> -> "failed to classify input"
}
```

Here `option` acts as a safety net around `choose`. If anything inside the `choose` errors, the `<Error>` arm catches it.

### Using Error Values as `choose` Subjects

Because error values are regular values, you can match on them with patterns:

```oxi
pattern is_network_error(e) when e.tag == "network"
pattern is_auth_error(e) when e.tag == "auth"
pattern is_timeout(e) when e.tag == "timeout"

err := <Error<network>>("connection refused")

message := choose err {
    is_network_error -> "check your connection",
    is_auth_error -> "please log in again",
    is_timeout -> "try again later",
    else -> "unknown error: {err.msg}"
}
println(message)    // "check your connection"
```

This works because:
1. `err` is an error value (tier 2), not a propagating error. It is a regular value.
2. Each pattern receives `err` as its parameter and checks `e.tag`.
3. The first matching pattern wins.

### Combining `choose` with Expected Results

A practical pattern — classify the outcome of a fallible operation:

```oxi
pattern is_success(r) when type(r) == "VALUE"
pattern is_error(r) when type(r) == "ERROR_VALUE"

result <Error || Value> := <type<Error || Value>>(fetch_user(user_id))

choose result {
    is_success -> println("Got user: {result.value}"),
    is_error -> println("Failed: {result.msg}"),
    else -> println("unexpected result type")
}
```

---

## 13. Complete Error Handling Recipes

These recipes show how to combine angle forms for common scenarios. Each one explains what is happening and why it works.

### Recipe: Simple Try-Catch

The closest thing to `try/catch` in other languages. Try an expression, use a fallback if it fails:

```oxi
result := risky_operation() <guard>("safe default")
```

Why it works: `<guard>` catches the propagating error from `risky_operation()` and replaces it with `"safe default"`. The program continues normally.

### Recipe: Try-Catch with Error Inspection

Same idea, but examine the error to decide what fallback to use:

```oxi
result := risky_operation() guard err -> {
    option {
        err.tag == "network" -> "offline mode",
        err.tag == "auth" -> "please log in",
        "unknown error"
    }
}
```

Why it works: The keyword `guard err ->` binds the caught error to `err`. The `option` block inspects `err.tag` to pick the right fallback message. The `err` variable is an error value with `.msg` and `.tag` fields available inside the fallback.

### Recipe: Log-Then-Recover

Observe the error for debugging, then provide a fallback. Due to parser precedence, `<guard>` placed after a `<log>` handler becomes part of the handler expression — this means the guard catches the re-propagated error within the handler's scope:

```oxi
result := fetch_data()
    <log<Error>> err -> println("[ERROR] {err.tag}: {err.msg}")
    <guard>("cached fallback")
```

Why it works: `<log>` runs `println(...)` for the side effect, then the original error continues. `<guard>` catches that error and replaces it with `"cached fallback"`.

### Recipe: Normalize and Branch

Wrap a fallible call into an expected-result, then branch on success or failure:

```oxi
outcome <Error || Value> := <type<Error || Value>>(connect_to_database())

option {
    type(outcome) == "VALUE" -> {
        db := outcome.value
        println("Connected to database")
    },
    {
        println("Database unavailable: {outcome.msg}")
        println("Running in offline mode")
    }
}
```

Why it works: `<type<Error || Value>>` catches the propagating error and converts it to an error value. The `option` block checks `type(outcome)` to decide which path to take. Both paths are normal program flow — no errors propagate.

### Recipe: Multiple Fallible Steps

When several steps might fail, normalize each one independently:

```oxi
config <Error || Value> := <type<Error || Value>>(read_file("config.oxi"))
db <Error || Value> := <type<Error || Value>>(connect_db())

option {
    type(config) != "VALUE" -> <fail>(<Error<startup>>("config: {config.msg}")),
    type(db) != "VALUE" -> <fail>(<Error<startup>>("database: {db.msg}"))
}

// If we reach here, both succeeded
println("Config: {config.value}")
println("Database: {db.value}")
```

Why it works: Each call is individually normalized. If either fails, the `option` block detects the failure type and propagates a clear tagged error. If both succeed, the program continues to use `.value` on each.

### Recipe: Error Arm as Safety Net

Use `option`'s `<Error>` arm as a catch-all when multiple branches might fail:

```oxi
result := option {
    input_valid(data) -> process(data),
    needs_retry(data) -> retry(data),
    <fail>(<Error<validation>>("unrecognized input")),
    <Error> -> <Error<internal>>("unexpected failure during processing")
}
```

Why it works: If `input_valid(data)`, `process(data)`, `needs_retry(data)`, or `retry(data)` produce a propagating error, the `<Error>` arm catches it and returns an error value (not a propagating error). The default arm uses `<fail>` intentionally — if no condition matches, it propagates a validation error.

### Recipe: Tagged Error Pipeline

Create errors with tags, propagate them, and catch only specific tags downstream:

```oxi
fun validate(input <str>) {
    option {
        len(input) == 0 -> <fail>(<Error<validation>>("input is empty")),
        len(input) > 100 -> <fail>(<Error<validation>>("input too long")),
        input
    }
}

// Only catch validation errors — let all other errors propagate normally
cleaned := validate(user_input) <guard<Error<validation>>>("(invalid)")
```

Why it works: `validate` uses `<fail>` with tagged error values. The `<guard<Error<validation>>>` at the call site only catches errors tagged `validation`. If `validate` encountered a different kind of error (like a system-level failure), it would propagate past the guard.

### Recipe: Classify Errors with `choose`

Use patterns to route different categories of errors to different handlers:

```oxi
pattern is_network(e) when e.tag == "network"
pattern is_auth(e) when e.tag == "auth"
pattern is_retryable(e) when e.tag == "retry_error"

err := <Error<network>>("connection refused")

action := choose err {
    is_retryable -> "retrying...",
    is_network -> "switching to offline mode",
    is_auth -> "redirecting to login",
    else -> "unhandled error: {err.msg}"
}
```

Why it works: The error value is a regular value, so `choose` can match on it using patterns. Each pattern checks `.tag` to classify the error. The matched arm's body becomes the result.

---

## 14. Current Built-In Angle Forms Reference

### Type-side (declaration position)

Used after a variable name to declare its type:

```oxi
<int>             // integer
<float>           // floating point
<str>             // string
<bool>            // boolean
<char>            // character
<byte>            // byte (0-255)
<uint>            // unsigned integer
<array>           // array
<map>             // map (dictionary)
<set>             // set
<tuple>           // tuple
<Error>           // error value type
<Error<tag>>      // tagged error value type
<Value>           // success value type
<Error || Value>  // expected-result union type
```

### Expression-side (prefix position)

Used at the start of an expression to construct or transform:

```oxi
<Error>("message")                  // create error value (does NOT halt)
<Error<tag>>("message")             // create tagged error value (does NOT halt)
<Value>(expr)                       // wrap value as explicit success
<type<Error || Value>>(expr)        // normalize expression to expected-result
<fail>("message")                   // propagate as runtime error (HALTS)
<fail>(<Error<tag>>("message"))     // propagate tagged error (HALTS)
```

### Effect-side (postfix position)

Used after a completed expression to apply recovery or observation:

```oxi
expr <guard>("fallback")                        // recover with fallback value
expr <guard<Error<tag>>>("fallback")            // recover only if tag matches
expr guard err -> fallback_expr                 // recover with error binding (keyword form)
expr <log<Error>> err -> handler_expr           // observe error, keep propagating
expr <log<Error<tag>>> err -> handler_expr      // observe only if tag matches
```

---

## 15. Design Rules

When reading or writing Oxigen code, these rules keep angle forms understandable:

1. **Declaration position = types.** After a variable name, `<...>` is always a type annotation.
2. **Prefix position = construction.** At the start of an expression, `<...>` constructs or transforms a value.
3. **Postfix position = effects.** After a completed expression, `<...>` applies recovery or observation.
4. **`<Error>` creates values, `<fail>` creates propagation.** Never confuse the two. `<Error>` is data, `<fail>` is control flow.
5. **`<Error<tag>>` for categories.** Use tags for lightweight error grouping without custom types.
6. **`<type<Error || Value>>` for expected failure.** When you know a call might fail and you want to handle both paths explicitly without the program halting.
7. **`guard` for recovery.** When you have a fallback and want to keep the program running.
8. **`log` for observation.** When you want to see or record an error without swallowing it.
9. **`option` + `<Error>` arm for structured recovery.** When multiple conditions or bodies inside an `option` might fail and you want one safety net.
10. **`choose` does not have `<Error>`.** Wrap `choose` in `guard` or `option` if you need error recovery.
