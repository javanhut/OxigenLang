# encoding

Base64, hex, and URL percent-encoding. All six functions operate on UTF-8
strings.

```oxi
introduce encoding
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`base64_encode`](#base64_encodes) | `base64_encode(s)` | `str` |
| [`base64_decode`](#base64_decodes) | `base64_decode(s)` | `str` or `Error` |
| [`hex_encode`](#hex_encodes) | `hex_encode(s)` | `str` |
| [`hex_decode`](#hex_decodes) | `hex_decode(s)` | `str` or `Error` |
| [`url_encode`](#url_encodes) | `url_encode(s)` | `str` |
| [`url_decode`](#url_decodes) | `url_decode(s)` | `str` or `Error` |

The three `*_decode` functions return a **catchable `<Error>`** on malformed
input rather than guessing. Encoding never fails. (`url_decode` is the lenient
one — see [below](#url_decodes).)

Encoding is not encryption and not hashing: anyone can reverse it. It exists to
move bytes through a channel that only accepts certain characters.

---

## Base64

### `base64_encode(s)`

Standard base64, with `=` padding.

```oxi
println(encoding.base64_encode("hi"))   // aGk=
```

### `base64_decode(s)`

```oxi
println(encoding.base64_decode("aGk="))   // hi
```

Handling bad input:

```oxi
introduce encoding

r := <type<Error> || <Value>>(encoding.base64_decode("not base64!!"))
option {
    is_error(r) -> { println("bad payload: {r.msg}") }
    { println(r.value) }
}
// bad payload: invalid base64: Invalid symbol 32, offset 3.
```

Typical use — an HTTP Basic auth header:

```oxi
introduce encoding
introduce net

token := encoding.base64_encode("user:password")
resp := net.request(net.HttpMethod.get, "https://example.com/private",
                    {"Authorization": "Basic " + token}, None)
```

---

## Hex

### `hex_encode(s)`

Lowercase hex, two characters per byte.

```oxi
println(encoding.hex_encode("hi"))   // 6869
```

### `hex_decode(s)`

```oxi
println(encoding.hex_decode("6869"))   // hi
```

An odd-length string or a non-hex character is an `Error`.

---

## URL percent-encoding

### `url_encode(s)`

Percent-encode a string so it is safe inside a URL query value.

```oxi
println(encoding.url_encode("a b&c"))   // a%20b%26c
```

Building a query string by hand:

```oxi
introduce encoding
introduce strings

fun query(params <map>) {
    parts <array> := []
    each k, v in params {
        parts = push(parts, encoding.url_encode(k) + "=" + encoding.url_encode(str(v)))
    }
    strings.join(parts, "&")
}
```

### `url_decode(s)`

```oxi
println(encoding.url_decode("a%20b%26c"))   // a b&c
```

An escape that is not valid hex is **left alone** rather than rejected —
`url_decode("%ZZ")` is `"%ZZ"`, and so is a trailing `%`. What does fail is an
escape sequence that decodes to bytes which are not valid UTF-8:

```oxi
r := <type<Error> || <Value>>(encoding.url_decode("%FF"))
println(r.msg)
// invalid url encoding: invalid utf-8 sequence of 1 bytes from index 0
```

The [api](api.md) module decodes request paths and query strings for you and
falls back to the raw text on such input, so a handler never receives an error
where it expects a string.

---

## Worked example

```oxi
introduce encoding
introduce json
introduce strings

// Decode a JWT payload (the middle segment). Base64url uses - and _ instead
// of + and /, so translate before decoding.
fun jwt_payload(token <str>) {
    segments := strings.split(token, ".")
    b64 := strings.replace(strings.replace(segments[1], "-", "+"), "_", "/")
    padded := b64 + strings.repeated("=", (4 - len(b64) % 4) % 4)
    json.parse(encoding.base64_decode(padded))
}
```

---

See also: [hash](hash.md) for one-way digests, [json](json.md) for structured
payloads, [net](net.md) and [api](api.md) for HTTP.
