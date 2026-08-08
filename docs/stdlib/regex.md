# regex

Regular expressions, using [Rust regex syntax](https://docs.rs/regex/latest/regex/#syntax).

```oxi
introduce regex
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`matches`](#matchespat-text) | `matches(pat, text)` | `bool` |
| [`find`](#findpat-text) | `find(pat, text)` | `str` or `None` |
| [`find_all`](#find_allpat-text) | `find_all(pat, text)` | `array` |
| [`captures`](#capturespat-text) | `captures(pat, text)` | `array` or `None` |
| [`replace`](#replacepat-text-replacement) | `replace(pat, text, replacement)` | `str` |
| [`split`](#splitpat-text) | `split(pat, text)` | `array` |

**The pattern comes first** in every function, and the argument is named `pat`
because `pattern` is a reserved keyword in Oxigen.

**Backslashes need doubling** in a normal string: `"\\d+"`, not `"\d+"`.

The syntax is Rust's `regex` crate: no backtracking, and therefore **no
backreferences and no lookaround**. Every pattern runs in time linear in the
input, so a hostile input cannot make one blow up.

---

### `matches(pat, text)`

True if the pattern matches **anywhere** in the text. Anchor with `^`/`$` if
you mean the whole string.

```oxi
println(regex.matches("\\d+", "abc123"))     // True
println(regex.matches("^\\d+$", "abc123"))   // False
```

### `find(pat, text)`

The first match as a string, or `None`.

```oxi
println(regex.find("\\d+", "a1 b22"))   // 1
println(regex.find("\\d+", "abc"))      // None
```

### `find_all(pat, text)`

Every non-overlapping match, in order. An empty array if there are none.

```oxi
println(regex.find_all("\\d+", "a1 b22 c333"))   // [1, 22, 333]
```

### `captures(pat, text)`

The capture groups of the **first** match, as an array: index 0 is the whole
match, index 1 the first group, and so on. `None` if nothing matched. A group
that did not participate in the match comes back as `""`.

```oxi
println(regex.captures("(\\d+)-(\\d+)", "tel 555-1234"))
// [555-1234, 555, 1234]

println(regex.captures("(\\d+)", "abc"))   // None
```

```oxi
introduce regex

fun parse_version(s <str>) {
    c := regex.captures("^v?(\\d+)\\.(\\d+)\\.(\\d+)$", s)
    option {
        c == None -> { None }
        { (int(c[1]), int(c[2]), int(c[3])) }
    }
}

println(parse_version("v1.4.2"))   // (1, 4, 2)
println(parse_version("nope"))     // None
```

### `replace(pat, text, replacement)`

Replace **every** match. Note the argument order: pattern, then text, then
replacement.

`$1`, `$2`, … in the replacement refer to capture groups; `$0` is the whole
match. Use `${1}` when a digit would otherwise run into following text.

```oxi
println(regex.replace("(\\w+)@(\\w+)", "x@y", "$2.$1"))   // y.x
println(regex.replace("\\s+", "a   b\tc", " "))           // a b c
```

For a literal (non-pattern) replacement, use
[`strings.replace`](strings.md#replaces-old-new) — it is faster and needs no
escaping.

### `split(pat, text)`

Split on a pattern — what `strings.split` cannot do.

```oxi
println(regex.split("\\s*,\\s*", "a , b,c"))   // [a, b, c]
```

---

## Worked example

```oxi
introduce regex
introduce io
introduce array

// Pull every URL out of a document and keep the unique hosts.
url_pat <str> = "https?://([\\w.-]+)[^\\s]*"

fun hosts(text <str>) {
    out <array> := []
    each url in regex.find_all(url_pat, text) {
        c := regex.captures(url_pat, url)
        out = push(out, c[1]) unless c == None
    }
    array.unique(out)
}

println(hosts("see https://oxigen.dev/docs and http://oxigen.dev/blog and https://example.com"))
// [oxigen.dev, example.com]
```

Each call compiles its pattern, so hoisting a hot pattern out of a loop is not
possible yet — prefer one `find_all` over calling `matches` per line when you
can.

---

See also: [strings](strings.md) for literal search and replace,
[parse_args](parse_args.md) for command-line flags.
