# env

Load key/value pairs from a `.env` file into an in-memory map.

```oxi
introduce env
```

| Item | Signature | Returns |
|------|-----------|---------|
| [`env_map`](#env_map) | module variable, `map` | everything loaded so far |
| [`discover_dot_file`](#discover_dot_filefilename) | `discover_dot_file(filename?)` | `None` |
| [`get_env`](#get_envkey) | `get_env(key)` | `str` or `None` |
| [`set_env`](#set_envkey-value) | `set_env(key, value)` | the stored value |

This module does **not** touch the real process environment — it is a map that
happens to be populated from a file. For actual environment variables use
[`os.env_get`](os.md#env_getkey) / [`os.env_set`](os.md#env_setkey-val).

---

### `discover_dot_file(filename?)`

Search the **current working directory** for a file named `.env`, read it, and
load every `KEY=VALUE` line into [`env_map`](#env_map).

```oxi
introduce env

env.discover_dot_file()
println(env.get_env("API_KEY"))   // abc123
```

Given `.env`:

```
API_KEY=abc123
DEBUG=true
```

If no `.env` is found in the current directory, it fails with
`No environment file was found file.`

> **The `filename` argument does not work.** Passing one prints the file's
> contents and then raises a type error instead of loading it. Call
> `discover_dot_file()` with no arguments, and `os.chdir` first if the file
> lives elsewhere.

**Parsing is deliberately simple, and it shows:**

| Input line | Stored as |
|------------|-----------|
| `API_KEY=abc123` | `API_KEY` → `abc123` |
| `QUOTED="has spaces"` | `QUOTED` → `"has spaces"` — quotes are **kept** |
| `EQ=a=b` | `EQ` → `a` — everything after the second `=` is dropped |
| `# comment` | ignored (no `=`, so no key) |
| blank line | ignored |

Strip quotes yourself if your file uses them:

```oxi
introduce env
introduce strings

env.discover_dot_file()
key := strings.strip(env.get_env("QUOTED"), "\"")
```

### `get_env(key)`

The value for `key`, or `None`. A miss also **prints** a line to stdout —
`Key: NOPE not found in env_map` — so it is not silent.

```oxi
println(env.get_env("API_KEY"))   // abc123
println(env.get_env("NOPE"))
// Key: NOPE not found in env_map
// None
```

To check quietly, look at the map directly: `has(env.env_map, "NOPE")`.

### `set_env(key, value)`

Store a value in the map by hand — useful for defaults before loading a file,
or for values that are not in one.

```oxi
env.set_env("REGION", "us-east-1")
println(env.get_env("REGION"))   // us-east-1
```

### `env_map`

The map itself, readable and writable.

```oxi
env.discover_dot_file()
println(env.env_map)          // {API_KEY: abc123, DEBUG: true}
println(has(env.env_map, "DEBUG"))   // True
```

Module-level state is shared across every file that imports the module, so one
`discover_dot_file()` at startup is enough for the whole program.

---

## Worked example

```oxi
introduce env
introduce os
introduce strings

// Config precedence: real environment beats .env, .env beats the default.
fun setting(key <str>, fallback <str>) {
    from_os := os.env_get(key)
    option {
        from_os != None -> { from_os }
        has(env.env_map, key) -> { strings.strip(env.env_map[key], "\"") }
        { fallback }
    }
}

env.discover_dot_file()
println(setting("PORT", "8000"))
```

Reading `env_map` directly rather than `get_env` keeps a miss from printing.

---

See also: [os](os.md) for the real process environment,
[toml](toml.md) / [json](json.md) for structured configuration files.
