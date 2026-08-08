# toml

Parse and serialize TOML, read and write TOML files, and work with nested
tables by dotted path. The API mirrors [json](json.md) exactly.

```oxi
introduce toml
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`parse`](#parses) | `parse(s)` | `map` |
| [`stringify`](#stringifyval) | `stringify(val)` | `str` |
| [`read`](#readpath) | `read(path)` | `map` |
| [`write`](#writepath-val) | `write(path, val)` | `None` |
| [`get_in`](#get_intbl-path) | `get_in(tbl, path)` | value or `None` |
| [`set_in`](#set_intbl-path-val) | `set_in(tbl, path, val)` | the root `map` |
| [`del_in`](#del_intbl-path) | `del_in(tbl, path)` | the root `map` |
| [`has_key_in`](#has_key_intbl-path) | `has_key_in(tbl, path)` | `bool` |
| [`set_where`](#set_wherearr-key-val-fields) | `set_where(arr, key, val, fields)` | `array` |
| [`merge`](#mergea-b) | `merge(a, b)` | `map` |
| [`deep_merge`](#deep_mergea-b) | `deep_merge(a, b)` | `map` |

Basic map operations are **global builtins**: `has(m, key)`, `keys(m)`,
`values(m)`, `insert(m, key, val)`, `remove(m, key)`.

## Type mapping

| TOML | Oxigen |
|------|--------|
| string | `String` |
| integer | `Integer` |
| float | `Float` |
| boolean | `True` / `False` |
| datetime / date / time | `String` |
| array | `Array` |
| table | `Map` |
| array of tables | `Array` of `Map` |

TOML has no null, so nothing maps to `None`. Dates come back as **strings** —
pass one through [`datetime.parse`](datetime.md#parsetext-fmt) if you need a
timestamp.

---

## Parsing and serializing

### `parse(s)`

Parse a TOML document. Tables become maps.

```oxi
introduce toml

t := toml.parse("title = \"MyApp\"\n[server]\nhost = \"localhost\"\nport = 8080\n")
println(t)
// {server: {host: localhost, port: 8080}, title: MyApp}
```

Note that top-level keys and `[section]` tables end up in the same map; the
order you see is the parser's, not the file's.

Invalid TOML is an `Error`.

### `stringify(val)`

Serialize a map to a **proper TOML document** — `[section]` headers,
`[[array_of_tables]]` blocks, and blank lines between sections.

```oxi
config := {"title": "MyApp", "server": {"host": "localhost", "port": 8080}}
println(toml.stringify(config))
```

```toml
title = "MyApp"

[server]
host = "localhost"
port = 8080
```

Scalars are emitted before tables, because TOML requires any bare key to come
before the first `[section]` header.

### `read(path)`

Read and parse a TOML file.

```oxi
settings := toml.read("Cargo.toml")
println(settings["package"]["name"])   // oxigen
```

A key that is not in the file reads back as `None`, and indexing `None` is an
error — so reach for `get_in` when a section may be absent (a workspace
`Cargo.toml` has no `[package]`, for instance).

### `write(path, val)`

Serialize a map and write it to a file, replacing what was there.

```oxi
toml.write("output.toml", {"title": "My App", "version": 1})
println(toml.read("output.toml"))   // {title: My App, version: 1}
```

---

## Nested access by dotted path

### `get_in(tbl, path)`

The value at a dotted path, or `None` when any segment is missing.

```oxi
println(toml.get_in(t, "server.port"))   // 8080
println(toml.get_in(t, "server.nope"))   // None
```

A path addresses **tables only** — it does not descend into arrays. For an
`[[array of tables]]`, see [`set_where`](#set_wherearr-key-val-fields).

### `set_in(tbl, path, val)`

Set a value, creating intermediate tables as needed. Returns the root map.

```oxi
config = toml.set_in(config, "server.ssl", True)
println(toml.get_in(config, "server.ssl"))   // True
```

> **Maps are mutable and shared.** `set_in` modifies the map you passed and
> returns that same map. So do `del_in`, `merge`, `deep_merge`, and the
> `insert`/`remove` builtins. Copy first
> (`toml.parse(toml.stringify(config))`) if you need the original intact.

### `del_in(tbl, path)`

Remove a key at a dotted path. Returns the root map.

```oxi
config = toml.del_in(config, "server.ssl")
println(toml.has_key_in(config, "server.ssl"))   // False
```

### `has_key_in(tbl, path)`

```oxi
println(toml.has_key_in(config, "server.host"))   // True
```

---

## Arrays of tables

An `[[array of tables]]` parses to an array of maps, which a dotted path cannot
descend into. `set_where` edits one by field value; the
[`array`](array.md#searching-an-array-of-records) module has the matching
`find_where` / `index_where` / `filter_where` / `remove_where`.

### `set_where(arr, key, val, fields)`

Update the first table whose `key` equals `val` by deep-merging `fields` into
it — or append a new table when there is none.

```oxi
introduce toml

cfg := toml.read("products.toml")

cfg.products = toml.set_where(cfg.products, "name", "hammer", {"sku": 738594937})
cfg.products = toml.set_where(cfg.products, "name", "screw", {"sku": 3})

toml.write("products.toml", cfg)
```

```toml
[[products]]
name = "hammer"
sku = 738594937

[[products]]
name = "nail"
sku = 2

[[products]]
name = "screw"
sku = 3
```

Fields not named survive the update, tables lacking the key are skipped rather
than erroring, and only the first match is updated. It returns the array,
edited in place — assigning the result back is the clearer form.

---

## Merging

### `merge(a, b)`

Shallow merge; keys in `b` win.

```oxi
println(toml.merge({"a": 1}, {"a": 2, "b": 3}))   // {a: 2, b: 3}
```

### `deep_merge(a, b)`

Recursive merge; nested tables are merged rather than replaced.

```oxi
a := {"server": {"host": "localhost", "port": 8080}}
b := {"server": {"port": 9090}}

println(toml.deep_merge(a, b))
// {server: {host: localhost, port: 9090}}
```

---

## Worked example

```oxi
introduce toml
introduce strings

// Read a project manifest, bump the patch version, write it back.
fun bump(path <str>) {
    cfg := toml.read(path)
    parts := strings.split(toml.get_in(cfg, "package.version"), ".")
    next := "{parts[0]}.{parts[1]}.{int(parts[2]) + 1}"
    toml.write(path, toml.set_in(cfg, "package.version", next))
    next
}

println(bump("m.toml"))   // 0.1.4
```

Layered configuration, defaults last-write-wins:

```oxi
introduce toml
introduce io

defaults := {"server": {"host": "127.0.0.1", "port": 8000}, "debug": False}

user := option {
    io.file_exists("app.toml") -> { toml.read("app.toml") }
    { {} }
}

config := toml.deep_merge(defaults, user)
println(toml.get_in(config, "server.port"))
```

---

See also: [json](json.md) — same API, different format; [datetime](datetime.md)
for turning TOML date strings into timestamps.
