# os

Running programs (to completion, or in the background), signals, environment
variables, directories, and facts about the machine.

```oxi
introduce os
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`exec`](#execcmd) | `exec(cmd)` | `map` — `stdout`, `stderr`, `code` |
| [`exec_argv`](#exec_argvprog-argv-stdin) | `exec_argv(prog, argv, stdin?)` | same map |
| [`spawn`](#spawnprog-argv-capture) | `spawn(prog, argv?, capture?)` | a process handle |
| [`write_stdin`](#write_stdinproc-text) | `write_stdin(proc, text)` | `None` |
| [`close_stdin`](#close_stdinproc) | `close_stdin(proc)` | `None` |
| [`read_output_line`](#read_output_lineproc) | `read_output_line(proc)` | `str` or `None` |
| [`read_output`](#read_outputproc) | `read_output(proc)` | `str` |
| [`wait`](#waitproc-timeout_ms) | `wait(proc, timeout_ms?)` | exit code, or `None` |
| [`kill`](#killproc-signal) | `kill(proc, signal?)` | `None` |
| [`is_running`](#is_runningproc) | `is_running(proc)` | `bool` |
| [`process_pid`](#process_pidproc) | `process_pid(proc)` | `int` |
| [`signals`](#signalsnames) | `signals(names)` | a [channel](channel.md) |
| [`name`](#name) | `name()` | `str` |
| [`arch`](#arch) | `arch()` | `str` |
| [`args`](#args) | `args()` | `array` |
| [`env_get`](#env_getkey) | `env_get(key)` | `str` or `None` |
| [`env_set`](#env_setkey-val) | `env_set(key, val)` | `None` |
| [`env_vars`](#env_vars) | `env_vars()` | `map` |
| [`cwd`](#cwd) | `cwd()` | `str` |
| [`chdir`](#chdirpath) | `chdir(path)` | `None` |
| [`list_dir`](#list_dirpath) | `list_dir(path)` | `array` of paths |
| [`walk_dir`](#walk_dirpath) | `walk_dir(path)` | `array` of paths |
| [`mkdir`](#mkdirpath) | `mkdir(path)` | `None` |
| [`rmdir`](#rmdirpath) | `rmdir(path)` | `None` |
| [`remove_file`](#remove_filepath) | `remove_file(path)` | `None` |
| [`is_dir`](#is_dirpath--is_filepath) | `is_dir(path)` | `bool` |
| [`is_file`](#is_dirpath--is_filepath) | `is_file(path)` | `bool` |
| [`exit`](#exitcode) | `exit(code)` | does not return |
| [`pid`](#pid) | `pid()` | `int` |

---

## Running commands

Both runners return the same map:

| Key | Meaning |
|-----|---------|
| `stdout` | everything the program printed to stdout, including its trailing newline |
| `stderr` | everything it printed to stderr |
| `code` | its exit status |

### `exec(cmd)`

Run a command **through a shell** (`sh -c` on Unix, `cmd /C` on Windows).

```oxi
introduce os

result := os.exec("echo hello")
println(result.stdout)   // hello
println(result.code)     // 0
```

Because a shell is involved, pipelines, globs and redirection work — and every
shell metacharacter in the string is live: `;`, `|`, `&`, `$(...)`, backticks,
`>`, `*`.

```oxi
println(os.exec("ls *.oxi | wc -l").stdout)
```

`exec` takes exactly **one** argument. To send a command input, use
[`exec_argv`](#exec_argvprog-argv-stdin), which takes `stdin`. The older variadic form
(`os.exec("cat", user_file)`) space-joined extras into the shell string — which
looked like an argument vector while being shell concatenation — and is now an
error pointing here.

### `exec_argv(prog, argv, stdin?)`

Run a program **directly**, with no shell. Each element of `argv` arrives as
exactly one literal argument.

```oxi
println(os.exec_argv("echo", ["a; b"]).stdout)   // a; b
```

**Use this whenever any part of the command comes from outside your program** —
user input, filenames, config values, network data, `os.args()`.

```oxi
name := "notes.txt; echo PWNED"   // untrusted

print(os.exec("echo " + name).stdout)
// notes.txt
// PWNED                       <- the shell ran a second command

print(os.exec_argv("echo", [name]).stdout)
// notes.txt; echo PWNED       <- one literal argument
```

Reach for `exec` only for a command you wrote out in full yourself, or when you
actually want shell features. If you need a pipeline *and* untrusted data,
restructure so the untrusted part goes through `exec_argv`.

**Missing programs are reported differently.** The shell form returns normally
with `code` 127 and a "command not found" message in `stderr`; `exec_argv`
fails to spawn and returns a terminal `Error`
(`__exec_argv(prog): No such file or directory`). Normalize it if a missing
program is something you want to handle:

```oxi
r := <type<Error> || <Value>>(os.exec_argv("no_such_prog", []))
println(r.msg) when is_error(r)
```

**Sending input.** Pass a string as `stdin` and the program reads it as its
standard input. Data never goes through a shell or a temporary file, so it
cannot inject commands, whatever it contains:

```oxi
introduce os
introduce json

body := json.stringify({"model": "llama3", "prompt": "hi; $(rm -rf /)"})
r := os.exec_argv("curl", ["-s", "-d", "@-", "http://localhost:11434/api/generate"], body)
```

Without `stdin`, the program shares the script's own standard input.

---

## Background processes

`exec` and `exec_argv` wait for the program to finish. `spawn` starts one and
returns at once with a handle, so the script can keep talking to it:

```oxi
introduce os

speaker := os.spawn("say", [])          // or "espeak", "piper", ...
os.write_stdin(speaker, "Hello there.\n")
os.write_stdin(speaker, "Second sentence.\n")
os.close_stdin(speaker)                 // no more input
os.wait(speaker)
```

A handle is an `int`, so it can be passed into a
[`diverge`](../concurrency.md) task, and any task can write to, wait for or
kill the program.

**Programs still running when the script ends are stopped.** At a normal exit,
at `os.exit`, and on Ctrl-C, SIGTERM or SIGHUP, each child gets SIGTERM, then
SIGKILL if it is still running 200 ms later. A script no longer needs to watch
its own process ID to clean up. (On Windows, children are not yet stopped at
exit.)

### `spawn(prog, argv?, capture?)`

Start `prog` with the arguments in `argv` (no shell, as with `exec_argv`).
Its standard input is a pipe for `write_stdin`. Its output goes to the
terminal, unless `capture` is `True`: then it goes to a pipe that
`read_output_line` and `read_output` read. stderr always goes to the terminal.

A missing program is an error, like `exec_argv`.

Only capture output you will read. A program writing to a captured pipe nobody
reads stops once the pipe is full.

### `write_stdin(proc, text)`

Send `text` to the program's standard input and flush it, so the program sees
it at once. No newline is added. Writing after `close_stdin`, or to a program
that has exited, is an error.

### `close_stdin(proc)`

Close the program's standard input. Programs that read until end of input
(`cat`, `wc`, many text-to-speech tools) finish their work then.

### `read_output_line(proc)`

The next line the program printed, without its newline, waiting for it. Returns
`None` once the program has closed its output (usually because it exited).
Needs `capture=True`:

```oxi
introduce os

p := os.spawn("cat", [], True)
os.write_stdin(p, "ping\n")
println(os.read_output_line(p))   // ping
os.close_stdin(p)
```

### `read_output(proc)`

Everything the program prints from now until it closes its output. Needs
`capture=True`.

### `wait(proc, timeout_ms?)`

Wait for the program to exit and return its exit code. A program ended by a
signal returns `-1`. With `timeout_ms`, it returns `None` if the program is
still running after that long. Waiting closes the program's stdin first,
since a program waiting for more input would never exit.

### `kill(proc, signal?)`

Send the program a signal: `"TERM"` by default (asks it to exit), or `"KILL"`
(cannot be ignored), `"INT"`, `"HUP"`, `"USR1"` and so on. Killing a program
that has already exited does nothing. Follow with `wait` to collect it.

### `is_running(proc)`

Whether the program is still running.

### `process_pid(proc)`

The operating system's process ID for the program, for logging or for other
tools. `pid()` is the script's own ID.

---

## Signals

### `signals(names)`

A [channel](channel.md) that receives each of the named signals, as a string
such as `"INT"`, whenever one arrives. The names are `"INT"` (Ctrl-C),
`"TERM"`, `"HUP"`, `"QUIT"`, `"USR1"`, `"USR2"` and `"WINCH"`, with or without
the `SIG` prefix.

Once a signal is subscribed, it no longer ends the program. The script decides
what to do, usually to clean up and exit:

```oxi
introduce os
introduce channel

stop := os.signals(["INT", "TERM"])
speaker := os.spawn("say", [])

repeat when channel.try_recv(stop) == None {
    line := next_sentence()          // your own function
    os.write_stdin(speaker, line + "\n")
}

println("shutting down")
os.kill(speaker)
```

To wait for a signal and do nothing else, use `channel.recv(stop)`. `"KILL"`
and `"STOP"` cannot be caught, so `signals` refuses them. Signals are not
supported on Windows yet; `signals` fails there.

---

## Machine and process

### `name()`

Operating system name: `"linux"`, `"macos"`, `"windows"`.

```oxi
println(os.name())   // macos
```

### `arch()`

CPU architecture: `"x86_64"`, `"aarch64"`, …

```oxi
println(os.arch())   // aarch64
```

### `args()`

The arguments passed to the running script — **not** including the interpreter
or the script path.

```oxi
println(os.args())   // [hello, --flag=value]
```

```
$ oxigen script.oxi hello --flag=value
```

For flag parsing on top of this, see [parse_args](parse_args.md).

### `pid()`

The current process ID.

```oxi
println(os.pid())   // 51234
```

### `exit(code)`

Exit the process immediately with the given status. Does not return.

```oxi
option {
    len(os.args()) == 0 -> {
        println("usage: script.oxi <file>")
        os.exit(2)
    }
}
```

---

## Environment variables

### `env_get(key)`

The value, or `None` if the variable is not set.

```oxi
println(os.env_get("HOME"))       // /Users/you
println(os.env_get("NOPE_XYZ"))   // None
```

```oxi
port := option {
    os.env_get("PORT") != None -> { int(os.env_get("PORT")) }
    { 8000 }
}
```

### `env_set(key, val)`

Set a variable for this process and anything it spawns afterwards. It does not
affect the parent shell. A key that is empty or contains `=` or a NUL byte, or
a value containing NUL, is an error.

```oxi
os.env_set("OXI_MODE", "debug")
println(os.env_get("OXI_MODE"))   // debug
```

### `env_vars()`

Every environment variable as a map.

```oxi
println(len(keys(os.env_vars())) > 0)   // True
```

For loading a `.env` file instead of the real environment, see [env](env.md).

---

## Directories and files

### `cwd()`

Current working directory, absolute.

```oxi
println(os.cwd())   // /Users/you/project
```

### `chdir(path)`

Change the working directory. Every relative path afterwards resolves against
the new one.

```oxi
os.chdir("/tmp")
println(os.cwd())   // /tmp
```

### `list_dir(path)`

Entries directly inside a directory, non-recursive. Returns paths, not bare
names — pass one through [`path.filename`](path.md#filenamep) for the name.

```oxi
introduce os
introduce path

each entry in os.list_dir(".") {
    println(path.filename(entry))
}
```

### `walk_dir(path)`

Every path under a directory, recursively. A symlink to a directory is listed
but not followed, so a link back up the tree (`a -> .`) cannot make the walk
loop.

```oxi
introduce os
introduce array
introduce path

fun is_oxi(p <str>) { path.ext(p) == "oxi" }

sources := array.filter(os.walk_dir("."), is_oxi)
println(len(sources))
```

### `mkdir(path)`

Create a directory, including any missing parents. Creating one that already
exists is fine.

```oxi
os.mkdir("build/reports")
```

### `rmdir(path)`

Remove a directory **and everything inside it**, recursively. There is no
confirmation and no undo — check the path before calling it.

```oxi
os.rmdir("build")
```

### `remove_file(path)`

Delete one file.

```oxi
os.remove_file("tmp.txt")
```

### `is_dir(path)` / `is_file(path)`

```oxi
println(os.is_dir("."))          // True
println(os.is_file("main.oxi"))  // True
println(os.is_file("nope"))      // False
```

---

## Worked example

```oxi
introduce os
introduce path
introduce array
introduce strings

// List Oxigen sources under a directory, using a git-aware listing when we are
// in a repo and a plain walk otherwise.
fun source_files(root <str>) {
    option {
        os.is_dir(path.join([root, ".git"])) -> {
            strings.lines(os.exec_argv("git", ["-C", root, "ls-files"]).stdout)
        }
        { os.walk_dir(root) }
    }
}

fun is_oxi(p <str>) { path.ext(p) == "oxi" }

println(len(array.filter(source_files("."), is_oxi)))
```

`exec_argv` is used there deliberately: `root` could contain a space or a
semicolon, and as an argument vector element that is just a directory name.

---

See also: [io](io.md) for reading and writing files, [path](path.md) for path
strings, [env](env.md) for `.env` files, [parse_args](parse_args.md) for CLI
flags.
