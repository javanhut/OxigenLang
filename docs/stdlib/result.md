# result

Helpers for the `Error || Value` system: check, default, chain, and recover
without writing an `option` block every time.

```oxi
introduce result
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`is_ok`](#is_okv) | `is_ok(v)` | `bool` |
| [`is_err`](#is_errv) | `is_err(v)` | `bool` |
| [`unwrap_or`](#unwrap_orv-default) | `unwrap_or(v, default)` | `v` or `default` |
| [`map_value`](#map_valuev-f) | `map_value(v, f)` | `f(v)` or the error |
| [`and_then`](#and_thenv-f) | `and_then(v, f)` | `f(v)` or the error |
| [`or_else`](#or_elsev-f) | `or_else(v, f)` | `v` or `f(v)` |
| [`ok_or`](#ok_orv-msg) | `ok_or(v, msg)` | `v` or a tagged error |

The model these functions assume: **a value is "ok" unless it is an error.**
`5`, `""`, `None` and `False` are all ok; only `<Error<tag>>(...)` is not.

See [type_system.md](../type_system.md) for the `Error || Value` types
themselves and [`<type<Error> || <Value>>(...)`](../angle_forms.md), the
normalizer that turns a halting error into an inspectable one.

---

### `is_ok(v)`

True when `v` is **not** an error.

```oxi
introduce result

println(result.is_ok(5))       // True
println(result.is_ok(None))    // True    <- None is a value, not an error
println(result.is_ok(<Error<oops>>("bad")))   // False
```

> Not the same as the `is_value` builtin. `is_value` is True only for something
> explicitly wrapped as `<Value>(...)`, so `is_value(42)` is `False` while
> `result.is_ok(42)` is `True`. For "did this fail?", use `is_ok`/`is_err`.

### `is_err(v)`

True when `v` is an error. The same as the `is_error` builtin, named to pair
with `is_ok`.

```oxi
println(result.is_err(<Error<oops>>("bad")))   // True
println(result.is_err(5))                      // False
```

### `unwrap_or(v, default)`

`v` if it is ok, otherwise `default`. The one-liner for "use the config file if
it loaded, otherwise the built-in defaults".

```oxi
introduce result

config <map> := result.unwrap_or(load_config(path), default_config())
port <int> := result.unwrap_or(parse_port(text), 8000)
```

```oxi
println(result.unwrap_or(<Error<x>>("bad"), 0))   // 0
println(result.unwrap_or(7, 0))                   // 7
```

`default` is evaluated either way — it is an ordinary argument, not a lazy
fallback. Keep it cheap, or use `or_else` to defer the work.

### `map_value(v, f)`

Apply `f` to `v` when ok; pass the error straight through when not. Lets a
chain of transforms skip its own error checks.

```oxi
fun double(x) { x * 2 }

println(result.map_value(5, double))                    // 10
println(result.map_value(<Error<x>>("bad"), double))    // Error { tag: x, msg: bad }
```

### `and_then(v, f)`

The same, for an `f` that itself returns an `Error || Value` — so the errors do
not nest.

```oxi
introduce result

fun read_config(path <str>) { ... }     // returns a map or an Error
fun validate(cfg <map>) { ... }         // returns a map or an Error

cfg := result.and_then(read_config("app.toml"), validate)
```

Use `map_value` when `f` cannot fail and `and_then` when it can.

### `or_else(v, f)`

If `v` is an error, recover by calling `f(v)`; otherwise return `v` unchanged.
`f` receives the error, so it can inspect the tag or the message.

```oxi
fun recover(e) { 0 }

println(result.or_else(<Error<x>>("bad"), recover))   // 0
println(result.or_else(5, recover))                   // 5
```

Falling back from cache to origin:

```oxi
fun from_origin(e) { net.get(url)["body"] }

body := result.or_else(read_cache(key), from_origin)
```

### `ok_or(v, msg)`

Turn a `None` into a tagged `<Error<none>>`; anything else passes through
unchanged. The bridge from "function returned nothing" to "function failed".

```oxi
introduce result

r := result.ok_or(None, "user not found")
println(is_error(r))   // True
println(r.msg)         // user not found

println(result.ok_or(3, "user not found"))   // 3
```

---

## Worked example

```oxi
introduce result
introduce io
introduce json

// Load a config file, falling back to defaults on any failure along the way.
fun read_json(path <str>) {
    r := <type<Error> || <Value>>(io.read_file(path))
    option {
        is_error(r) -> { <Error<io>>("cannot read {path}") }
        { json.parse(r.value) }
    }
}

fun with_defaults(cfg <map>) {
    json.merge({"port": 8000, "debug": False}, cfg)
}

fun defaults(e) {
    println("config unavailable: {e.msg}")
    {"port": 8000, "debug": False}
}

config := result.or_else(result.map_value(read_json("app.json"), with_defaults), defaults)
println(config["port"])
```

The chain reads top to bottom: read it, apply defaults if it read, fall back
entirely if it did not.

---

See also: [type_system.md](../type_system.md) and
[angle_forms.md](../angle_forms.md) for the underlying `Error || Value` forms,
[test](test.md) for asserting on them.
