# json

Parse and serialize JSON, read and write JSON files, and work with nested maps
by dotted path.

```oxi
introduce json
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`parse`](#parses) | `parse(s)` | Oxigen value |
| [`stringify`](#stringifyval) | `stringify(val)` | `str` |
| [`read`](#readpath) | `read(path)` | Oxigen value |
| [`write`](#writepath-val) | `write(path, val)` | `None` |
| [`get_in`](#get_inobj-path) | `get_in(obj, path)` | value or `None` |
| [`set_in`](#set_inobj-path-val) | `set_in(obj, path, val)` | the root `map` |
| [`del_in`](#del_inobj-path) | `del_in(obj, path)` | the root `map` |
| [`has_key_in`](#has_key_inobj-path) | `has_key_in(obj, path)` | `bool` |
| [`merge`](#mergea-b) | `merge(a, b)` | `map` |
| [`deep_merge`](#deep_mergea-b) | `deep_merge(a, b)` | `map` |

Basic map operations are **global builtins** — no import needed:
`has(m, key)`, `keys(m)`, `values(m)`, `insert(m, key, val)`, `remove(m, key)`.

## Type mapping

| JSON | Oxigen |
|------|--------|
| `null` | `None` |
| `true` / `false` | `True` / `False` |
| number (integer) | `Integer` |
| number (fractional) | `Float` |
| `"string"` | `String` |
| `[array]` | `Array` |
| `{object}` | `Map` |

---

## Parsing and serializing

### `parse(s)`

Parse a JSON string. Objects become maps, arrays stay arrays, `null` becomes
`None`.

```oxi
introduce json

d := json.parse("\{\"a\": [1, 2], \"b\": null, \"c\": 1.5\}")
println(d)             // {a: [1, 2], b: None, c: 1.5}
println(type(d["c"]))  // FLOAT
```

Note the `\{` escapes: a bare `{` inside an Oxigen string opens an
interpolation, so a JSON literal written inline has to escape its braces. Most
of the time the JSON comes from a file or a response body and the question does
not arise.

Invalid JSON is an `Error`:

```oxi
r := <type<Error> || <Value>>(json.parse("not json"))
println(is_error(r))   // True
```

### `stringify(val)`

Serialize any Oxigen value to a compact JSON string (no spaces, no trailing
newline).

```oxi
println(json.stringify({"name": "Oxigen", "version": 1}))
// {"name":"Oxigen","version":1}
```

Map key order is preserved as inserted.

### `read(path)`

`parse(io.read_file(path))` — read a file and parse it in one step.

```oxi
config := json.read("config.json")
println(config["debug"])
```

### `write(path, val)`

`io.write_file(path, stringify(val))` — serialize and write in one step.
Replaces the file if it exists.

```oxi
json.write("config.json", {"debug": True, "level": 3})
println(json.read("config.json"))   // {debug: True, level: 3}
```

---

## Nested access by dotted path

The four `*_in` functions address nested structure with `"a.b.c"` strings, so
you do not need to check each level yourself.

### `get_in(obj, path)`

The value at a dotted path, or `None` if any segment is missing or is not a
map.

```oxi
user := {"name": "Alice", "address": {"city": "NYC", "zip": "10001"}}

println(json.get_in(user, "address.city"))   // NYC
println(json.get_in(user, "address.nope"))   // None
println(json.get_in(user, "name.city"))      // None
```

`None` is also a legitimate stored value, so a `None` result means "missing or
null". Use `has_key_in` when you need to tell those apart.

Array indices are **not** supported in a path — `"items.0.name"` will not work.
Index the array yourself: `json.get_in(d, "items")[0]["name"]`.

### `set_in(obj, path, val)`

Set the value at a dotted path, creating intermediate maps as needed. Returns
the root map.

```oxi
user := {"name": "Alice", "address": {"city": "NYC"}}
updated := json.set_in(user, "address.state", "NY")
println(updated)
// {name: Alice, address: {city: NYC, state: NY}}

// Intermediate levels are created:
println(json.set_in({}, "a.b.c", 1))   // {a: {b: {c: 1}}}
```

> **Maps are mutable and shared.** `set_in` modifies the map you passed and
> returns that same map — `user` above is changed too, not just `updated`. The
> same is true of `del_in`, `merge`, `deep_merge`, and the `insert`/`remove`
> builtins. If you need the original preserved, copy it first (for example
> `json.parse(json.stringify(user))`).

### `del_in(obj, path)`

Remove the key at a dotted path. Returns the root map. A path that does not
exist is a no-op.

```oxi
user := {"name": "Alice", "address": {"city": "NYC", "zip": "10001"}}
println(json.del_in(user, "address.zip"))
// {name: Alice, address: {city: NYC}}
```

### `has_key_in(obj, path)`

Does the dotted path exist? True even when the stored value is `None`.

```oxi
println(json.has_key_in(user, "address.city"))   // True
println(json.has_key_in(user, "a.b"))            // False
```

---

## Merging

### `merge(a, b)`

Shallow merge: every key in `b` overwrites the same key in `a`.

```oxi
defaults := {"port": 8080, "debug": False}
overrides := {"debug": True, "host": "0.0.0.0"}

println(json.merge(defaults, overrides))
// {port: 8080, debug: True, host: 0.0.0.0}
```

A nested map in `b` replaces the whole nested map in `a` — that is what
"shallow" means.

### `deep_merge(a, b)`

Recursive merge: where both sides have a map under the same key, the maps are
merged; anything else in `b` overwrites.

```oxi
a := {"server": {"host": "localhost", "port": 8080}}
b := {"server": {"port": 9090}}

println(json.deep_merge(a, b))
// {server: {host: localhost, port: 9090}}
```

This is the one to use for layering config: defaults, then a file, then
environment overrides.

---

## Worked example

```oxi
introduce json
introduce net
introduce array

// Fetch a JSON list, keep the interesting fields, and save it.
resp := net.get("https://api.example.com/users")
users := json.parse(resp["body"])

fun summary(u <map>) {
    {"id": u["id"], "city": json.get_in(u, "address.city")}
}

json.write("users.json", array.map(users, summary))
```

Layering configuration:

```oxi
introduce json
introduce io
introduce os

defaults := {"port": 8000, "log": {"level": "info", "file": "app.log"}}

file_cfg := option {
    io.file_exists("app.json") -> { json.read("app.json") }
    { {} }
}

config := json.deep_merge(defaults, file_cfg)
config = json.set_in(config, "log.level", os.env_get("LOG_LEVEL")) when os.env_get("LOG_LEVEL") != None

println(json.get_in(config, "log.level"))
```

---

See also: [toml](toml.md) — the same API for TOML, [net](net.md) and
[api](api.md) for sending and receiving JSON over HTTP, [io](io.md) for raw
file access.
