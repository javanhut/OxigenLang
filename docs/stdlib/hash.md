# hash

Hash digests of a string, returned as lowercase hex.

```oxi
introduce hash
```

| Function | Signature | Digest | Hex length |
|----------|-----------|--------|------------|
| [`sha256`](#sha256s) | `sha256(s)` | SHA-256 | 64 |
| [`sha1`](#sha1s) | `sha1(s)` | SHA-1 | 40 |
| [`md5`](#md5s) | `md5(s)` | MD5 | 32 |

All three take a UTF-8 string and hash its bytes.

> **`md5` and `sha1` are for checksums and compatibility only.** Both are
> broken against collision attacks. Do not use them to verify anything an
> attacker could have chosen. `sha256` is the one to reach for by default.
>
> None of these is a password hash — those need a slow, salted KDF (bcrypt,
> scrypt, Argon2), which this module does not provide.

---

### `sha256(s)`

```oxi
introduce hash

println(hash.sha256("hello"))
// 2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
```

### `sha1(s)`

```oxi
println(hash.sha1("hello"))
// aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d
```

### `md5(s)`

```oxi
println(hash.md5("hello"))
// 5d41402abc4b2a76b9719d911017c592
```

---

## Worked examples

**Content addressing** — name a cache entry after what is in it, so identical
content lands in the same file:

```oxi
introduce hash
introduce io
introduce path

fun cache_put(dir <str>, content <str>) {
    target := path.join([dir, hash.sha256(content) + ".txt"])
    io.write_file(target, content) unless io.file_exists(target)
    target
}
```

**Change detection** — cheaper than diffing, and stable across runs:

```oxi
introduce hash
introduce io

fun changed(path <str>, known_digest <str>) {
    hash.sha256(io.read_file(path)) != known_digest
}
```

**Verifying a download** against a published checksum:

```oxi
introduce hash
introduce io
introduce net

net.download("https://example.com/tool.tar.gz", "tool.tar.gz")
expected := "..."   // from the project's checksum file
println(hash.sha256(io.read_file("tool.tar.gz")) == expected)
```

Note that `io.read_file` decodes as UTF-8, so this works for text payloads;
a binary archive is not reliably round-tripped through a string.

---

See also: [encoding](encoding.md) for base64/hex/URL encoding (encoding is
reversible; hashing is not), [random](random.md).
