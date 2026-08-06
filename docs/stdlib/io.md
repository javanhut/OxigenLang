# io

Whole-file reads and writes, and line input from the console.

```oxi
introduce io
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`read_file`](#read_filepath) | `read_file(path)` | `str` |
| [`write_file`](#write_filepath-content) | `write_file(path, content)` | `None` |
| [`append_file`](#append_filepath-content) | `append_file(path, content)` | `None` |
| [`file_exists`](#file_existspath) | `file_exists(path)` | `bool` |
| [`input`](#inputprompt) | `input(prompt)` | `str` |
| [`read_line`](#read_line) | `read_line()` | `str` |

Files are read and written as **UTF-8 text**. There is no streaming reader and
no binary mode: a file is one string. For a large download, stream it straight
to disk with [`net.download`](net.md#downloadurl-path) instead of building the
string in memory.

Directory work (create, list, walk, delete) lives in [os](os.md); path string
manipulation lives in [path](path.md).

---

## Files

### `read_file(path)`

Read the whole file into a string. A missing or unreadable file is an `Error`
that halts the program unless you normalize it.

```oxi
introduce io

println(io.read_file("notes.txt"))
```

Handling a missing file rather than aborting:

```oxi
introduce io

r := <type<Error> || <Value>>(io.read_file("maybe.txt"))
text := option {
    is_error(r) -> { "" }
    { r.value }
}
println(len(text))     // 0
println(r.msg)         // No such file or directory (os error 2)
```

The simpler guard is to ask first:

```oxi
option {
    io.file_exists("notes.txt") -> { println(io.read_file("notes.txt")) }
    { println("no notes yet") }
}
```

### `write_file(path, content)`

Write a string to a file, **replacing** whatever was there. Creates the file if
it does not exist; the parent directory must already exist (see
[`os.mkdir`](os.md#mkdirpath)).

```oxi
io.write_file("test.txt", "Hello!")
println(io.read_file("test.txt"))   // Hello!
```

### `append_file(path, content)`

Append to the end of a file, creating it if needed. No newline is added — put
one in the string yourself.

```oxi
io.append_file("log.txt", "started\n")
io.append_file("log.txt", "finished\n")
```

### `file_exists(path)`

```oxi
println(io.file_exists("test.txt"))   // True
println(io.file_exists("nope.txt"))   // False
```

This answers only "is there something readable at this path". Use
[`os.is_file`](os.md#is_dirpath--is_filepath) / [`os.is_dir`](os.md#is_dirpath--is_filepath) when the
distinction matters.

---

## Console input

### `input(prompt)`

Print `prompt` (no newline), then read one line from stdin. The trailing
newline is not included.

```oxi
introduce io

name := io.input("What is your name? ")
println("Hello, {name}!")
```

Everything comes back as a string — convert it yourself:

```oxi
age := int(io.input("Age: "))
println(age + 1)
```

### `read_line()`

Read one line from stdin with no prompt. This is how you consume piped input.

```oxi
// echo.oxi — repeat every piped line, uppercased
introduce io
introduce strings

line := io.read_line()
repeat unless line == "" {
    println(strings.upper(line))
    line = io.read_line()
}
```

```
$ printf 'a\nb\n' | oxigen echo.oxi
A
B
```

At end of input `read_line` returns `""`.

---

## Worked example

```oxi
introduce io
introduce json
introduce os

// Append a line to a log, rotating it once it gets long.
fun log_line(path <str>, msg <str>) {
    option {
        io.file_exists(path) and len(io.read_file(path)) > 1000 -> {
            io.write_file(path + ".old", io.read_file(path))
            io.write_file(path, "")
        }
    }
    io.append_file(path, msg + "\n")
}

log_line("app.log", "started")
print(io.read_file("app.log"))   // started
os.remove_file("app.log")
```

---

See also: [os](os.md) for directories and processes, [path](path.md) for path
strings, [json](json.md) / [toml](toml.md) for `read`/`write` helpers that parse
as they load.
