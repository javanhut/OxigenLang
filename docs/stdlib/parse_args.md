# parse_args

A small command-line flag parser built on [`os.args()`](os.md#args).

```oxi
introduce {Parser} from parse_args
```

**Import the struct by name.** A struct is not constructible through a module
namespace, so `parse_args.Parser` does not work — `introduce {Parser} from
parse_args` does.

| Item | Signature | Purpose |
|------|-----------|---------|
| [`Parser`](#struct-parser) | struct | holds the normalized args and the flag map |
| [`define_flag`](#parserdefine_flagverbose_name-alias) | `parser.define_flag(verbose_name, alias?)` | declare a flag |
| [`handle_args`](#parserhandle_args) | `parser.handle_args()` | parse and populate in one call |
| [`parse_args`](#parserparse_args) | `parser.parse_args()` | read and normalize `os.args()` |
| [`process_args`](#parserprocess_args) | `parser.process_args()` | fill `args_map` from the normalized args |
| [`normalize_array`](#normalize_arrayarr) | `normalize_array(arr)` | join `--k v` pairs into `--k=v` |
| [`k_v_pair`](#k_v_pairs) | `k_v_pair(s)` | split `"k=v"` into a tuple |
| [`double_dash`](#patterns) / [`single_dash`](#patterns) | patterns | flag-shape tests |

---

## Quick start

```oxi
introduce {Parser} from parse_args

main {
    p <Parser>
    p.define_flag(verbose_name="name", alias="n")
    p.define_flag(verbose_name="count")
    p.handle_args()

    println(p.args_map.name)
    println(p.args_map.count)
}
```

```
$ oxigen script.oxi --name=Alice --count 5
Alice
5

$ oxigen script.oxi -n Bob
Bob

```

Both `--flag=value` and `--flag value` are accepted, and a one-character alias
may be given with a single dash.

---

## Struct: `Parser`

| Field | Type | Contents |
|-------|------|----------|
| `args` | `array` | the normalized argument list, every flag as `key=value` |
| `args_map` | `map` | declared flag → value; `""` for a flag that was not passed |

Declare one with no arguments — both fields start empty:

```oxi
p <Parser>
```

### `parser.define_flag(verbose_name, alias?)`

Declare a flag. `verbose_name` is the long form (used as `--name`), `alias` an
optional **one-character** short form (used as `-n`).

```oxi
p.define_flag(verbose_name="name", alias="n")
p.define_flag(verbose_name="count")
```

Both names become separate keys in `args_map`, so `--name=x` and `-n x` land in
`args_map["name"]` and `args_map["n"]` respectively — they are **not** merged.
Read whichever the user supplied:

```oxi
name := option {
    p.args_map.name != "" -> { p.args_map.name }
    { p.args_map.n }
}
```

Errors:

| Condition | Error |
|-----------|-------|
| `verbose_name` is empty | `<Error<too_short>>` "Verbose name is too short…" |
| `alias` longer than 1 character | `<Error<too_long>>` "Alias name is too long…" |
| `alias` is an empty string | `<Error<too_short>>` "Alias name is empty…" |

### `parser.handle_args()`

Run [`parse_args`](#parserparse_args) then
[`process_args`](#parserprocess_args). This is the one to call.

```oxi
p.handle_args()
```

### `parser.parse_args()`

Read `os.args()`, normalize it, and store the result in `self.args`. Fails with
"Length of args = 0. Expected at least 1." when the script was given no
arguments at all — so a script whose flags are all optional should guard:

```oxi
p.handle_args() unless len(os.args()) == 0
```

### `parser.process_args()`

Walk `self.args` and copy each recognised flag's value into `args_map`. An
unrecognised flag is **skipped with a log line**, not an error:

```
2026-08-06 16:24:41: Key: --unknown is undefined. Skipping.
```

Positional arguments (anything with no `-`) are ignored entirely — read them
from `os.args()` yourself.

---

## Module functions

### `normalize_array(arr)`

Turn a raw argument list into one where every flag carries its value:
`["--name", "Alice"]` becomes `["--name=Alice"]`. An argument already
containing `=`, one at the end of the list, and one followed by another
`-`-prefixed argument are all left alone.

```oxi
introduce parse_args

println(parse_args.normalize_array(["--name", "Alice", "-v"]))
// [--name=Alice, -v]
```

### `k_v_pair(s)`

Split `"key=value"` into a `(key, value)` tuple.

```oxi
println(parse_args.k_v_pair("--name=Alice"))   // (--name, Alice)
```

### Patterns

| Pattern | Matches |
|---------|---------|
| `double_dash(s)` | `s` contains `--` |
| `single_dash(s)` | `s` contains `-` |

Used by `process_args` in a `choose` block. They test for a *containing*
dash, not a prefix, so a value with a dash in it can match — the parser strips
leading dashes from the key either way.

---

## Worked example

```oxi
introduce {Parser} from parse_args
introduce os

fun usage() {
    println("usage: greet.oxi --name=NAME [--greeting=TEXT]")
    os.exit(2)
}

main {
    p <Parser>
    p.define_flag(verbose_name="name", alias="n")
    p.define_flag(verbose_name="greeting", alias="g")

    usage() when len(os.args()) == 0
    p.handle_args()

    name := option {
        p.args_map.name != "" -> { p.args_map.name }
        p.args_map.n != "" -> { p.args_map.n }
        { usage() }
    }
    greeting := option {
        p.args_map.greeting != "" -> { p.args_map.greeting }
        p.args_map.g != "" -> { p.args_map.g }
        { "Hello" }
    }

    println("{greeting}, {name}!")
}
```

```
$ oxigen greet.oxi --name=Ada
Hello, Ada!

$ oxigen greet.oxi -n Ada -g Hi
Hi, Ada!
```

---

## Limits

Worth knowing before you build a large CLI on it:

- No `--help` generation, no required-flag checking, no type conversion —
  every value is a string.
- No boolean flags: `--verbose` with no value stores `""`, which is
  indistinguishable from "not passed".
- Long and short forms are separate keys, not aliases of one value.
- No `--` end-of-flags marker, and no subcommands.

For anything past that, read `os.args()` directly.

---

See also: [os](os.md) for `args()` and `exit()`, [env](env.md) and
[toml](toml.md) for configuration that does not come from the command line.
