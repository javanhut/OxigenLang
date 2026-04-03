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
| `floor` | `floor(x)` | Round down to integer |
| `ceil` | `ceil(x)` | Round up to integer |
| `round` | `round(x)` | Round to nearest integer |

```oxi
introduce math
println(math.sqrt(16))     // 4
println(math.pow(2, 10))   // 1024
println(math.clamp(15, 0, 10)) // 10
println(math.abs(-42))     // 42
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

```oxi
introduce array
println(array.map([1, 2, 3], fun(x) { x * 2 }))    // [2, 4, 6]
println(array.filter([1, 2, 3, 4], fun(x) { x > 2 })) // [3, 4]
println(array.reduce([1, 2, 3], 0, fun(a, b) { a + b })) // 6
println(array.sort([3, 1, 4, 1, 5]))                // [1, 1, 3, 4, 5]
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

// Parse
data := json.parse("[1, 2, 3]")
println(data)  // [1, 2, 3]

// Stringify
m := {"name": "Oxigen", "version": 1}
println(json.stringify(m))  // {"name":"Oxigen","version":1}
```

## toml

```oxi
introduce toml
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `parse` | `parse(s)` | Parse TOML string into Oxigen values |
| `read` | `read(path)` | Read and parse a TOML file |

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

```oxi
introduce toml

config := toml.parse("title = \"Oxigen\"\n[owner]\nname = \"Javan\"")
println(config["owner"]["name"])  // Javan

settings := toml.read("Cargo.toml")
println(settings["package"]["name"])  // oxigen
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

All functions return a map with `"status"` (integer) and `"body"` (string).

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
```

Supports HTTP and HTTPS.
