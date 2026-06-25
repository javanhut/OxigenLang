# Standard Library Reference

OxigenLang's standard library is a collection of `.oxi` modules that provide common functionality. Import them with `introduce`.

## math

```oxi
introduce math
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `abs` | `abs(x)` | Absolute value |
| `min` | `min(a, b)` | Smaller of two values |
| `max` | `max(a, b)` | Larger of two values |
| `pow` | `pow(base, exp)` | Integer exponentiation |
| `clamp` | `clamp(x, lo, hi)` | Constrain value to range [lo, hi] |
| `sign` | `sign(x)` | Returns 1, -1, or 0 |
| `sqrt` | `sqrt(x)` | Square root (returns float) |
| `floor` | `floor(x)` | Round down |
| `ceil` | `ceil(x)` | Round up |
| `round` | `round(x)` | Round to nearest |
| `trunc` | `trunc(x)` | Round toward zero |
| `gcd` | `gcd(a, b)` | Greatest common divisor |
| `lcm` | `lcm(a, b)` | Least common multiple |
| `factorial` | `factorial(n)` | `n!` (errors on negative) |
| `is_even` | `is_even(n)` | True if `n` is even |
| `is_odd` | `is_odd(n)` | True if `n` is odd |
| `hypot` | `hypot(a, b)` | `sqrt(a*a + b*b)` |
| `fmod` | `fmod(a, b)` | Floating-point remainder |
| `degrees` | `degrees(rad)` | Radians → degrees |
| `radians` | `radians(deg)` | Degrees → radians |
| `powf` | `powf(base, exp)` | Floating-point power (fractional/negative exponents) |
| `sin` `cos` `tan` | `sin(x)` … | Trigonometric functions (radians) |
| `asin` `acos` `atan` | `asin(x)` … | Inverse trig (radians) |
| `atan2` | `atan2(y, x)` | Angle of the point `(x, y)` |
| `exp` | `exp(x)` | `e**x` |
| `ln` | `ln(x)` | Natural log |
| `log2` | `log2(x)` | Base-2 log |
| `log10` | `log10(x)` | Base-10 log |

Constants: `math.PI`, `math.E`, `math.TAU`.

```oxi
introduce math
println(math.sqrt(16))     // 4
println(math.pow(2, 10))   // 1024
println(math.gcd(12, 18))  // 6
println(math.factorial(5)) // 120
println(math.PI)           // 3.141592653589793
```

## strings

```oxi
introduce strings
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `split` | `split(s, delim)` | Split string into array |
| `join` | `join(arr, delim)` | Join array into string |
| `trim` | `trim(s)` | Remove leading/trailing whitespace |
| `strip` | `strip(s, chars)` | Remove specific characters from both ends |
| `strip_left` | `strip_left(s, chars)` | Remove specific characters from the left |
| `strip_right` | `strip_right(s, chars)` | Remove specific characters from the right |
| `upper` | `upper(s)` | Convert to uppercase |
| `lower` | `lower(s)` | Convert to lowercase |
| `replace` | `replace(s, old, new)` | Replace all occurrences |
| `starts_with` | `starts_with(s, prefix)` | Check prefix |
| `ends_with` | `ends_with(s, suffix)` | Check suffix |
| `contains_str` | `contains_str(s, sub)` | Check if substring exists |
| `slice` | `slice(s, start, end)` | Substring by index range |
| `char_at` | `char_at(s, i)` | Character at index, as a string |
| `is_empty` | `is_empty(s)` | True if length is 0 |
| `reverse` | `reverse(s)` | Reverse the characters |
| `repeated` | `repeated(s, n)` | `s` repeated `n` times (`repeat` is a keyword) |
| `pad_left` | `pad_left(s, width, fill)` | Left-pad to `width` with `fill` |
| `pad_right` | `pad_right(s, width, fill)` | Right-pad to `width` with `fill` |
| `capitalize` | `capitalize(s)` | Uppercase the first character |
| `index_of` | `index_of(s, sub)` | Index of `sub`, or -1 |
| `count` | `count(s, sub)` | Non-overlapping occurrences of `sub` |
| `lines` | `lines(s)` | Split on newlines |

```oxi
introduce strings
println(strings.upper("hello"))          // HELLO
println(strings.split("a,b,c", ","))     // [a, b, c]
println(strings.join(["x", "y"], "-"))   // x-y
println(strings.replace("foo bar", "bar", "baz")) // foo baz

// Strip specific characters
println(strings.strip("##heading##", "#"))       // heading
println(strings.strip_left("--value", "-"))      // value
println(strings.strip_right("value--", "-"))     // value
println(strings.strip("  --test--  ", " -"))     // test
```

## array

```oxi
introduce array
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `map` | `map(arr, f)` | Apply function to each element |
| `filter` | `filter(arr, f)` | Keep elements where f returns true |
| `reduce` | `reduce(arr, initial, f)` | Fold array into single value |
| `reverse` | `reverse(arr)` | Reverse an array |
| `zip` | `zip(a, b)` | Pair elements from two arrays |
| `flatten` | `flatten(arr)` | Flatten nested arrays one level |
| `includes` | `includes(arr, val)` | Check if value exists in array |
| `sort` | `sort(arr)` | Sort array (integers, floats, or strings) |
| `sort_by` | `sort_by(arr, key_fn)` | Sort by a key function |
| `sum` | `sum(arr)` | Sum of elements |
| `product` | `product(arr)` | Product of elements |
| `min` | `min(arr)` | Smallest element (errors if empty) |
| `max` | `max(arr)` | Largest element (errors if empty) |
| `any` | `any(arr, pred)` | True if any element matches |
| `all` | `all(arr, pred)` | True if all elements match |
| `count` | `count(arr, pred)` | Number of matching elements |
| `find` | `find(arr, pred)` | First matching element (errors if none) |
| `find_index` | `find_index(arr, pred)` | Index of first match, or -1 |
| `index_of` | `index_of(arr, val)` | Index of value, or -1 |
| `unique` | `unique(arr)` | Remove duplicates, preserving order |
| `enumerate` | `enumerate(arr)` | Array of `(index, value)` tuples |
| `slice` | `slice(arr, start, end)` | Sub-array by index range |
| `take` | `take(arr, n)` | First `n` elements |
| `drop` | `drop(arr, n)` | All but the first `n` elements |
| `chunk` | `chunk(arr, size)` | Split into sub-arrays of `size` |
| `flat_map` | `flat_map(arr, f)` | Map then flatten one level |
| `each_with_index` | `each_with_index(arr, f)` | Call `f(i, x)` for each element |
| `group_by` | `group_by(arr, key_fn)` | Map of key → elements |

```oxi
introduce array
println(array.map([1, 2, 3], fun(x) { x * 2 }))    // [2, 4, 6]
println(array.filter([1, 2, 3, 4], fun(x) { x > 2 })) // [3, 4]
println(array.reduce([1, 2, 3], 0, fun(a, b) { a + b })) // 6
println(array.sum([1, 2, 3, 4]))                   // 10
println(array.unique([1, 1, 2, 3, 3]))             // [1, 2, 3]
println(array.sort([3, 1, 4, 1, 5]))               // [1, 1, 3, 4, 5]
```

## io

```oxi
introduce io
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `read_file` | `read_file(path)` | Read file contents as string |
| `write_file` | `write_file(path, content)` | Write string to file |
| `append_file` | `append_file(path, content)` | Append string to file |
| `file_exists` | `file_exists(path)` | Check if file exists |
| `input` | `input(prompt)` | Print prompt and read a line from stdin |
| `read_line` | `read_line()` | Read a line from stdin (no prompt) |

```oxi
introduce io

// File operations
io.write_file("test.txt", "Hello!")
println(io.read_file("test.txt"))   // Hello!
println(io.file_exists("test.txt")) // True

// Console input
name := io.input("What is your name? ")
println("Hello, {name}!")

line := io.read_line()
```

## os

```oxi
introduce os
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `exec` | `exec(cmd)` | Run shell command, returns map with `stdout`, `stderr`, `code` |
| `name` | `name()` | OS name ("linux", "macos", "windows") |
| `arch` | `arch()` | CPU architecture ("x86_64", "aarch64") |
| `args` | `args()` | Arguments passed to the current script |
| `env_get` | `env_get(key)` | Get environment variable (None if unset) |
| `env_set` | `env_set(key, val)` | Set environment variable |
| `env_vars` | `env_vars()` | All environment variables as a map |
| `cwd` | `cwd()` | Current working directory |
| `chdir` | `chdir(path)` | Change working directory |
| `list_dir` | `list_dir(path)` | List directory entries (non-recursive) |
| `walk_dir` | `walk_dir(path)` | Recursively list all paths in directory tree |
| `mkdir` | `mkdir(path)` | Create directory (and parents) |
| `rmdir` | `rmdir(path)` | Remove directory recursively |
| `remove_file` | `remove_file(path)` | Remove a file |
| `is_dir` | `is_dir(path)` | Check if path is a directory |
| `is_file` | `is_file(path)` | Check if path is a file |
| `exit` | `exit(code)` | Exit process with code |
| `pid` | `pid()` | Current process ID |

```oxi
introduce os
result := os.exec("echo hello")
println(result["stdout"])  // hello
println(os.name())         // linux
println(os.cwd())          // /current/directory
println(os.args())         // [Alice, --flag=value]
```

## time

```oxi
introduce time
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `now` | `now()` | Unix timestamp in seconds (float) |
| `now_ms` | `now_ms()` | Unix timestamp in milliseconds (integer) |
| `sleep` | `sleep(ms)` | Sleep for milliseconds |
| `monotonic` | `monotonic()` | Monotonic clock in nanoseconds |
| `elapsed_ms` | `elapsed_ms(start, end)` | Elapsed milliseconds between two monotonic values |
| `elapsed_s` | `elapsed_s(start, end)` | Elapsed seconds between two monotonic values |

```oxi
introduce time

start := time.monotonic()
time.sleep(100)
end := time.monotonic()
println("Elapsed:", time.elapsed_ms(start, end), "ms")

println("Timestamp:", time.now())
```

## random

```oxi
introduce random
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `rand_int` | `rand_int(min, max)` | Random integer in [min, max] inclusive |
| `rand_float` | `rand_float()` | Random float in [0.0, 1.0) |
| `seed` | `seed(n)` | Seed the random number generator |
| `rand_bool` | `rand_bool()` | Random boolean |
| `choice` | `choice(arr)` | Pick random element from array |

```oxi
introduce random
random.seed(42)
println(random.rand_int(1, 100))
println(random.rand_float())
println(random.choice(["red", "green", "blue"]))
```

## path

```oxi
introduce path
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `join` | `join(parts)` | Join path components (array of strings) |
| `ext` | `ext(p)` | File extension (without dot), or None |
| `filename` | `filename(p)` | Filename component |
| `parent` | `parent(p)` | Parent directory |
| `stem` | `stem(p)` | Filename without extension |
| `is_absolute` | `is_absolute(p)` | Check if path is absolute |
| `is_relative` | `is_relative(p)` | Check if path is relative |

```oxi
introduce path
println(path.join(["/home", "user", "file.txt"])) // /home/user/file.txt
println(path.ext("photo.png"))       // png
println(path.filename("/a/b/c.txt")) // c.txt
println(path.parent("/a/b/c.txt"))   // /a/b
println(path.stem("data.csv"))       // data
```

## json

```oxi
introduce json
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `parse` | `parse(s)` | Parse JSON string into Oxigen values |
| `stringify` | `stringify(val)` | Serialize Oxigen value to JSON string |
| `read` | `read(path)` | Read and parse a JSON file |
| `write` | `write(path, val)` | Write an Oxigen value to a JSON file |
| `get_in` | `get_in(obj, path)` | Get nested value by dotted path (e.g. `"user.address.city"`) |
| `set_in` | `set_in(obj, path, val)` | Set nested value by dotted path, creates intermediate objects |
| `del_in` | `del_in(obj, path)` | Remove nested key by dotted path |
| `has_key_in` | `has_key_in(obj, path)` | Check if dotted key path exists |
| `merge` | `merge(a, b)` | Shallow merge, keys in `b` overwrite `a` |
| `deep_merge` | `deep_merge(a, b)` | Recursive merge for nested objects |

For basic map operations use the global builtins directly: `has(m, key)`, `keys(m)`, `values(m)`, `insert(m, key, val)`, `remove(m, key)`.

**Type mapping:**

| JSON | Oxigen |
|------|--------|
| `null` | `None` |
| `true`/`false` | `True`/`False` |
| number (int) | `Integer` |
| number (float) | `Float` |
| `"string"` | `String` |
| `[array]` | `Array` |
| `{object}` | `Map` |

```oxi
introduce json

// Parse and stringify
data := json.parse("[1, 2, 3]")
println(data)  // [1, 2, 3]

m := {"name": "Oxigen", "version": 1}
println(json.stringify(m))  // {"name":"Oxigen","version":1}

// File I/O
json.write("config.json", {"debug": True, "level": 3})
config := json.read("config.json")

// Basic map operations use builtins directly
println(has(config, "debug"))   // True
println(keys(config))           // [debug, level]
config = insert(config, "name", "MyApp")

// Nested access and updates
user := {"name": "Alice", "address": {"city": "NYC", "zip": "10001"}}
println(json.get_in(user, "address.city"))  // NYC

updated := json.set_in(user, "address.state", "NY")
println(json.has_key_in(updated, "address.state"))  // True

// Merging
defaults := {"port": 8080, "debug": False}
overrides := {"debug": True, "host": "0.0.0.0"}
config := json.merge(defaults, overrides)
// {"port": 8080, "debug": True, "host": "0.0.0.0"}
```

## toml

```oxi
introduce toml
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `parse` | `parse(s)` | Parse TOML string into Oxigen values |
| `stringify` | `stringify(val)` | Serialize a map to a proper TOML document |
| `read` | `read(path)` | Read and parse a TOML file |
| `write` | `write(path, val)` | Write a map to a TOML file |
| `get_in` | `get_in(tbl, path)` | Get nested value by dotted path (e.g. `"server.port"`) |
| `set_in` | `set_in(tbl, path, val)` | Set nested value by dotted path, creates intermediate tables |
| `del_in` | `del_in(tbl, path)` | Remove nested key by dotted path |
| `has_key_in` | `has_key_in(tbl, path)` | Check if dotted key path exists |
| `merge` | `merge(a, b)` | Shallow merge, keys in `b` overwrite `a` |
| `deep_merge` | `deep_merge(a, b)` | Recursive merge for nested tables |

For basic map operations use the global builtins directly: `has(m, key)`, `keys(m)`, `values(m)`, `insert(m, key, val)`, `remove(m, key)`.

**Type mapping:**

| TOML | Oxigen |
|------|--------|
| string | `String` |
| integer | `Integer` |
| float | `Float` |
| boolean | `True`/`False` |
| datetime/date/time | `String` |
| array | `Array` |
| table | `Map` |

`stringify` produces proper TOML document format with `[section]` headers, `[[array_of_tables]]` syntax, and blank lines between sections:

```oxi
introduce toml

// stringify produces proper TOML documents
config := {"title": "MyApp", "server": {"host": "localhost", "port": 8080}}
println(toml.stringify(config))
// title = "MyApp"
//
// [server]
// host = "localhost"
// port = 8080

// File I/O
settings := toml.read("Cargo.toml")
println(settings["package"]["name"])  // oxigen

toml.write("output.toml", {"title": "My App", "version": 1})

// Basic map operations use builtins directly
println(has(config, "title"))   // True
println(keys(config))           // [title, server]
config = insert(config, "debug", True)
config = remove(config, "debug")

// Dotted-path access for nested tables
println(toml.get_in(config, "server.host"))  // localhost
config = toml.set_in(config, "server.ssl", True)
println(toml.has_key_in(config, "server.ssl"))  // True
config = toml.del_in(config, "server.ssl")

// Deep merge preserves nested structure
a := {"server": {"host": "localhost", "port": 8080}}
b := {"server": {"port": 9090}}
result := toml.deep_merge(a, b)
// {"server": {"host": "localhost", "port": 9090}}
```

## net

```oxi
introduce net
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `get` | `get(url)` | HTTP GET request |
| `post` | `post(url, body)` | HTTP POST with JSON content type |
| `put` | `put(url, body)` | HTTP PUT with JSON content type |
| `patch` | `patch(url, body)` | HTTP PATCH with JSON content type |
| `delete` | `delete(url)` | HTTP DELETE request |
| `head` | `head(url)` | HTTP HEAD request |
| `request` | `request(method, url, headers, body)` | Full control HTTP request |
| `download` | `download(url, path)` | Stream a GET body to a file; returns the status code |
| `upload` | `upload(method, url, file_path, headers = {})` | Stream a file as the request body; returns `{status, body}` |
| `open_stream` | `open_stream(url)` | Open a streaming GET; returns a stream handle (body not buffered) |
| `read_chunk` | `read_chunk(stream, max = 4096, timeout_ms = 0)` | Read up to `max` bytes; `""` at end of stream. With `timeout_ms > 0`, returns `None` if nothing arrived yet (poll + animate a spinner) |
| `connect` | `connect(host, port)` | Open a TCP connection; returns a connection handle |
| `listen` | `listen(host, port)` | Bind a TCP server; returns a server handle |
| `accept` | `accept(server)` | Block until a client connects; returns a connection handle |
| `send` | `send(conn, data)` | Send text on a connection; returns bytes written |
| `receive` | `receive(conn, max = 4096)` | Read up to `max` bytes; returns `""` on a clean close |
| `close` | `close(handle)` | Close a socket handle (connection or server); idempotent |
| `udp_bind` | `udp_bind(host, port)` | Bind a UDP socket; returns a socket handle |
| `udp_send` | `udp_send(sock, data, host, port)` | Send a datagram; returns bytes sent |
| `udp_receive` | `udp_receive(sock, max = 4096)` | Receive a datagram; returns `(data, sender_address)` |

The HTTP request functions return a map with `"status"` (integer) and `"body"` (string).

`download`/`upload` stream their payloads so large transfers never buffer the
whole body in memory. `open_stream`/`read_chunk` expose a response body
incrementally instead (SSE, chunked, long-poll, large downloads): `open_stream`
returns a handle, `read_chunk` pulls the next bytes until it returns `""`, and
`close` frees it. `read_chunk` always returns whole UTF-8 characters, so
multi-byte tokens (emoji, accents, CJK) never split across reads. Pass a
`timeout_ms` to poll without blocking — it returns `None` when no data has
arrived yet, which is how you animate a spinner while waiting for the first
token:

```oxi
chunk := net.read_chunk(s, 4096, 80)   // wait up to 80ms
option {
    chunk == None    -> { /* still waiting — draw a spinner frame */ },
    chunk == ""      -> { /* end of stream */ },
    { /* got bytes — split on "\n", parse, print */ }
}
```

Socket I/O works on UTF-8 text — the common case (HTTP, line protocols, JSON
over TCP). A **handle** is an opaque ticket number for an open socket; always
`close` it when done, or it leaks one connection until the program exits — like
a file you never close.

**Errors.** On failure (connection refused, host not found, file missing, bad
status), every `net` function returns a terminal `Error` that halts execution —
the same model as the HTTP request functions. Normalize a call into a value you
can inspect with `<map<Error || Value>>(...)` and the `result` module:

```oxi
res := <type<Error || Value>>(net.connect("127.0.0.1", 9999))
print("connect failed\n") when result.is_err(res)
```

**Blocking.** All socket calls are synchronous and block the (single-threaded)
interpreter until the OS operation completes: `connect` waits for the handshake,
`accept` waits for a client, `receive`/`udp_receive` wait for data. There is no
timeout — a `connect` to an unreachable host blocks until the OS gives up. One
slow peer stalls the whole program; non-blocking/timeout support is not yet
available.

```oxi
introduce net
introduce json

// Simple GET
resp := net.get("https://api.example.com/data")
println(resp["status"])  // 200
data := json.parse(resp["body"])

// POST with JSON body
body := json.stringify({"name": "test"})
resp := net.post("https://api.example.com/items", body)

// Custom request with headers
resp := net.request("GET", "https://api.example.com/secure", {"Authorization": "Bearer token123"}, None)

// Stream a large file to disk without buffering it
status := net.download("https://example.com/big.iso", "big.iso")

// TCP echo server
srv := net.listen("127.0.0.1", 8080)
conn := net.accept(srv)
msg := net.receive(conn)
net.send(conn, "echo: " + msg)
net.close(conn)
net.close(srv)

// UDP
sock := net.udp_bind("127.0.0.1", 9000)
data, sender := net.udp_receive(sock)
net.close(sock)
```

Supports HTTP and HTTPS.

## result

Helpers for the `Error || Value` system. They work with the tagged forms
`<Error<tag>>(...)` / `<Value>(...)` and the `is_error` / `is_value` builtins: a
value is treated as "ok" unless it is an error.

```oxi
introduce result
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `is_ok` | `is_ok(v)` | True when `v` is not an error |
| `is_err` | `is_err(v)` | True when `v` is an error |
| `unwrap_or` | `unwrap_or(v, default)` | `v` if ok, otherwise `default` |
| `map_value` | `map_value(v, f)` | Apply `f` to `v` if ok; propagate the error otherwise |
| `and_then` | `and_then(v, f)` | Like `map_value`, for an `f` that returns an Error \|\| Value |
| `or_else` | `or_else(v, f)` | Recover with `f(v)` if `v` is an error; otherwise return `v` |
| `ok_or` | `ok_or(v, msg)` | Convert `None` into a tagged error; pass anything else through |

```oxi
introduce result

config <map> := result.unwrap_or(load_config(path), default_config())
doubled := result.map_value(parse_int(s), fun(n <int>) { n * 2 })
```

## regex

Regular expressions using Rust regex syntax. (`pattern` is a keyword, so the
pattern argument is named `pat`.)

```oxi
introduce regex
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `matches` | `matches(pat, text)` | True if the pattern matches anywhere |
| `find` | `find(pat, text)` | First match string, or `None` |
| `find_all` | `find_all(pat, text)` | Array of all matches |
| `replace` | `replace(pat, text, replacement)` | Replace all matches (`$1` group refs) |
| `split` | `split(pat, text)` | Split text on the pattern |
| `captures` | `captures(pat, text)` | First match's groups (index 0 = whole match), or `None` |

```oxi
introduce regex
println(regex.matches("\\d+", "abc123"))        // True
println(regex.find_all("\\d+", "a1 b22 c333"))  // [1, 22, 333]
println(regex.replace("(\\w+)@(\\w+)", "x@y", "$2.$1"))  // y.x
```

## datetime

Dates and times in UTC. Timestamps are unix seconds (integers). Format strings
use strftime-style directives.

```oxi
introduce datetime
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `now` | `now()` | Current time as unix seconds |
| `format` | `format(ts, fmt)` | Format a timestamp (UTC) |
| `parse` | `parse(text, fmt)` | Parse to unix seconds (catchable `<Error>` on mismatch) |
| `year` `month` `day` | `year(ts)` … | Date components |
| `hour` `minute` `second` | `hour(ts)` … | Time components |
| `weekday` | `weekday(ts)` | Full weekday name, e.g. `"Saturday"` |
| `iso8601` | `iso8601(ts)` | RFC 3339 string, e.g. `"2026-06-20T13:45:00Z"` |

```oxi
introduce datetime
now <int> := datetime.now()
println(datetime.format(now, "%Y-%m-%d"))
ts <int> := datetime.parse("2000-01-01 00:00:00", "%Y-%m-%d %H:%M:%S")
println(datetime.year(ts))   // 2000
```

## encoding

Base64, hex, and URL percent-encoding. All operate on UTF-8 strings; the
`*_decode` functions return a catchable `<Error>` on invalid input.

```oxi
introduce encoding
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `base64_encode` / `base64_decode` | `base64_encode(s)` | Standard base64 |
| `hex_encode` / `hex_decode` | `hex_encode(s)` | Lowercase hex |
| `url_encode` / `url_decode` | `url_encode(s)` | URL percent-encoding |

```oxi
introduce encoding
println(encoding.base64_encode("hi"))    // aGk=
println(encoding.base64_decode("aGk="))  // hi
println(encoding.url_encode("a b&c"))    // a%20b%26c
```

## hash

Hash digests, returned as lowercase hex strings. `md5`/`sha1` are for checksums
and compatibility, not security-sensitive use.

```oxi
introduce hash
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `sha256` | `sha256(s)` | SHA-256 hex digest |
| `sha1` | `sha1(s)` | SHA-1 hex digest |
| `md5` | `md5(s)` | MD5 hex digest |

```oxi
introduce hash
println(hash.sha256("hello"))
// 2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
```
