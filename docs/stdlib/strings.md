# strings

String manipulation: splitting, joining, trimming, case, search, padding.

```oxi
introduce strings
```

Strings are immutable — every function here returns a **new** string.

| Function | Signature | Returns |
|----------|-----------|---------|
| [`split`](#splits-delim) | `split(s, delim)` | `array` |
| [`join`](#joinarr-delim) | `join(arr, delim)` | `str` |
| [`lines`](#liness) | `lines(s)` | `array` |
| [`trim`](#trims) | `trim(s)` | `str` |
| [`strip`](#strips-chars) | `strip(s, chars)` | `str` |
| [`strip_left`](#strip_lefts-chars) | `strip_left(s, chars)` | `str` |
| [`strip_right`](#strip_rights-chars) | `strip_right(s, chars)` | `str` |
| [`upper`](#uppers) | `upper(s)` | `str` |
| [`lower`](#lowers) | `lower(s)` | `str` |
| [`capitalize`](#capitalizes) | `capitalize(s)` | `str` |
| [`replace`](#replaces-old-new) | `replace(s, old, new)` | `str` |
| [`starts_with`](#starts_withs-prefix) | `starts_with(s, prefix)` | `bool` |
| [`ends_with`](#ends_withs-suffix) | `ends_with(s, suffix)` | `bool` |
| [`contains_str`](#contains_strs-sub--containss-sub) | `contains_str(s, sub)` | `bool` |
| [`contains`](#contains_strs-sub--containss-sub) | `contains(s, sub)` | `bool` |
| [`index_of`](#index_ofs-sub) | `index_of(s, sub)` | `int` (`-1` if absent) |
| [`count`](#counts-sub) | `count(s, sub)` | `int` |
| [`slice`](#slices-start-end) | `slice(s, start, end)` | `str` |
| [`char_at`](#char_ats-i) | `char_at(s, i)` | `str` |
| [`is_empty`](#is_emptys) | `is_empty(s)` | `bool` |
| [`reverse`](#reverses) | `reverse(s)` | `str` |
| [`repeated`](#repeateds-n) | `repeated(s, n)` | `str` |
| [`pad_left`](#pad_lefts-width-fill) | `pad_left(s, width, fill)` | `str` |
| [`pad_right`](#pad_rights-width-fill) | `pad_right(s, width, fill)` | `str` |

---

## Splitting and joining

### `split(s, delim)`

Split on a literal delimiter. Returns an array of strings.

```oxi
println(strings.split("a,b,c", ","))       // [a, b, c]
println(strings.split("one two", " "))     // [one, two]
```

Destructuring the result is the usual way to take a key/value pair apart:

```oxi
key, value := strings.split("PORT=8080", "=")
println(key, value)   // PORT 8080
```

For a **pattern** delimiter, use [`regex.split`](regex.md#splitpat-text).

### `join(arr, delim)`

Join an array of strings with a delimiter — the inverse of `split`.

```oxi
println(strings.join(["x", "y", "z"], "-"))   // x-y-z
println(strings.join(["a", "b"], ""))         // ab
```

### `lines(s)`

Split on newlines. Shorthand for `split(s, "\n")`.

```oxi
introduce io
introduce strings

each line in strings.lines(io.read_file("notes.txt")) {
    println("> " + line)
}
```

---

## Trimming

### `trim(s)`

Remove leading and trailing **whitespace**.

```oxi
println("[" + strings.trim("  hi  ") + "]")   // [hi]
```

### `strip(s, chars)`

Remove any of the characters in `chars` from **both** ends. `chars` is a set of
characters, not a substring.

```oxi
println(strings.strip("##head##", "#"))      // head
println(strings.strip("  --test--  ", " -")) // test
```

### `strip_left(s, chars)`

Same, left end only.

```oxi
println(strings.strip_left("--value", "-"))   // value
```

### `strip_right(s, chars)`

Same, right end only.

```oxi
println(strings.strip_right("value--", "-"))   // value
```

---

## Case

### `upper(s)`

```oxi
println(strings.upper("hi"))   // HI
```

### `lower(s)`

```oxi
println(strings.lower("HI"))   // hi
```

### `capitalize(s)`

Uppercase the first character only; the rest is untouched. An empty string
comes back unchanged.

```oxi
println(strings.capitalize("hello world"))   // Hello world
println(strings.capitalize(""))              //
```

---

## Search and replace

### `replace(s, old, new)`

Replace **every** occurrence of `old`.

```oxi
println(strings.replace("foo bar bar", "bar", "baz"))   // foo baz baz
```

For a pattern, use [`regex.replace`](regex.md#replacepat-text-replacement).

### `starts_with(s, prefix)`

```oxi
println(strings.starts_with("hello", "he"))   // True
```

### `ends_with(s, suffix)`

```oxi
println(strings.ends_with("hello", "lo"))   // True
```

### `contains_str(s, sub)` / `contains(s, sub)`

True if `sub` appears anywhere in `s`. The two names are the same function —
`contains` is the shorter alias.

```oxi
println(strings.contains_str("hello", "ell"))   // True
println(strings.contains("hello", "z"))         // False
```

### `index_of(s, sub)`

Index of the first occurrence of `sub`, or `-1` if it never appears.

```oxi
println(strings.index_of("hello", "ll"))   // 2
println(strings.index_of("hello", "z"))    // -1
```

### `count(s, sub)`

Number of **non-overlapping** occurrences.

```oxi
println(strings.count("banana", "an"))   // 2
println(strings.count("aaaa", "aa"))     // 2   <- non-overlapping
println(strings.count("abc", ""))        // 0
```

---

## Slicing and characters

### `slice(s, start, end)`

Substring from `start` up to (not including) `end`. Equivalent to the
`s[start:end]` operator. An `end` past the string length is clamped.

```oxi
println(strings.slice("hello world", 0, 5))   // hello
println(strings.slice("abc", 0, 99))          // abc
```

### `char_at(s, i)`

The character at index `i`, as a one-character string.

```oxi
println(strings.char_at("hello", 1))   // e
```

### `is_empty(s)`

True when the length is 0.

```oxi
println(strings.is_empty(""))    // True
println(strings.is_empty("x"))   // False
```

---

## Building

### `reverse(s)`

Reverse the characters.

```oxi
println(strings.reverse("hello"))   // olleh
```

### `repeated(s, n)`

`s` repeated `n` times. It is not called `repeat` because that is a loop
keyword.

```oxi
println(strings.repeated("ab", 3))   // ababab
println(strings.repeated("-", 20))   // --------------------
```

### `pad_left(s, width, fill)`

Prepend `fill` until the string is at least `width` long. A string already at
least that long is returned unchanged.

```oxi
println(strings.pad_left("7", 3, "0"))   // 007
```

### `pad_right(s, width, fill)`

Append `fill` until the string is at least `width` long — the way to line up
columns.

```oxi
println(strings.pad_right("7", 3, "."))   // 7..
```

`fill` may be more than one character, in which case the result can overshoot
`width`.

---

## Worked example

```oxi
introduce strings

// Parse "key: value" config lines into a map, ignoring blanks and comments.
fun parse_config(text <str>) {
    out <map> := {}
    each line in strings.lines(text) {
        trimmed := strings.trim(line)
        option {
            strings.is_empty(trimmed) -> { None }
            strings.starts_with(trimmed, "#") -> { None }
            strings.contains(trimmed, ":") -> {
                k, v := strings.split(trimmed, ":")
                out[strings.trim(k)] = strings.trim(v)
            }
        }
    }
    out
}

println(parse_config("# settings\nhost: localhost\nport: 8080\n"))
// {host: localhost, port: 8080}
```

---

See also: [regex](regex.md) for pattern matching, [encoding](encoding.md) for
base64/hex/URL escaping, and the global builtins `len`, `str`, `int`, `float`,
`chars` in [builtins.md](../builtins.md).
