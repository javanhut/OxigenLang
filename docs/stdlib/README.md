# Standard Library Reference

OxigenLang's standard library is a set of `.oxi` modules that ship with the
interpreter. Import one with `introduce`:

```oxi
introduce math
println(math.sqrt(16))   // 4
```

Or pull specific names into scope:

```oxi
introduce {sqrt, abs} from math
println(sqrt(16))
```

Structs must be imported by name — `introduce {Parser} from parse_args` — since
a struct is not constructible through a module namespace. See
[imports.md](../imports.md) for the full import syntax.

One page per module, each documenting **every** callable function with
examples:

## Data and text

| Module | What it covers |
|--------|----------------|
| [strings](strings.md) | split, join, trim, case, search, pad |
| [array](array.md) | map/filter/reduce, folds, search, sort, group, `pmap` |
| [math](math.md) | arithmetic, rounding, trig, logs, gcd/lcm, `PI`/`E`/`TAU` |
| [regex](regex.md) | match, find, captures, replace, split (Rust regex syntax) |
| [json](json.md) | parse/stringify, files, dotted paths, merging |
| [toml](toml.md) | same API as `json`, for TOML documents |
| [encoding](encoding.md) | base64, hex, URL percent-encoding |
| [hash](hash.md) | SHA-256, SHA-1, MD5 hex digests |

## System

| Module | What it covers |
|--------|----------------|
| [io](io.md) | read/write/append files, console input |
| [os](os.md) | run commands, environment, directories, process |
| [path](path.md) | join, split, normalize, `is_within` containment check |
| [time](time.md) | timestamps, sleep, monotonic durations |
| [datetime](datetime.md) | UTC calendar dates, formatting, parsing |
| [random](random.md) | random ints/floats/bools, `choice`, `seed` |
| [env](env.md) | load a `.env` file into a map |
| [parse_args](parse_args.md) | command-line flag parsing |

## Network

| Module | What it covers |
|--------|----------------|
| [net](net.md) | HTTP client, streaming HTTP, TCP and UDP sockets |
| [api](api.md) | HTTP server: routing, JSON responses, the accept loop |

## Language support

| Module | What it covers |
|--------|----------------|
| [result](result.md) | helpers for the `Error \|\| Value` system |
| [test](test.md) | `expect(...)` matchers used by `oxigen test` |
| [ansi](ansi.md) | terminal colour, modifiers, cursor control |

---

## Conventions across the library

**Errors.** Two kinds show up. A *catchable* error is an ordinary value you can
inspect (`math.factorial(-1)`, `json.parse` on bad input). A *terminal* error
halts the program (most of [net](net.md), a missing file in [io](io.md)). Turn
the second into the first by normalizing the call:

```oxi
r := <type<Error> || <Value>>(io.read_file("maybe.txt"))
option {
    is_error(r) -> { println("failed: {r.msg}") }
    { println(r.value) }
}
```

[result](result.md) has the helpers that make chains of these readable.

**Maps are mutable and shared.** `insert`, `remove`, and every `*_in`/`merge`
function in [json](json.md) and [toml](toml.md) modify the map you pass and
return that same map. Round-trip through `stringify`/`parse` for a copy.

**Strings and arrays are not.** Everything in [strings](strings.md) and
[array](array.md) returns a new value.

**Sockets and streams use handles.** [net](net.md) hands back an opaque number;
always `close` it.

---

## Where the rest of the language is documented

| Page | Contents |
|------|----------|
| [builtins.md](../builtins.md) | global functions — `len`, `push`, `keys`, `range`, `type`, `has`, … |
| [imports.md](../imports.md) | `introduce` syntax, namespacing, local modules |
| [type_system.md](../type_system.md) | type annotations, `Error \|\| Value` |
| [angle_forms.md](../angle_forms.md) | `<fail>`, `<log>`, `<type<...>>` and friends |
| [concurrency.md](../concurrency.md) | `diverge`, `converge`, share-nothing threading |
| [testing.md](../testing.md) | the `oxigen test` runner |

The sources are worth reading — every module is plain Oxigen in `stdlib/`.
