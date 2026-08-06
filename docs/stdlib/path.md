# path

Path strings: build them, take them apart, and check that one is inside
another. Only `is_within` touches the filesystem — everything else is pure
string work.

```oxi
introduce path
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`join`](#joinparts) | `join(parts)` | `str` |
| [`ext`](#extp) | `ext(p)` | `str` (`""` if none) |
| [`filename`](#filenamep) | `filename(p)` | `str` |
| [`parent`](#parentp) | `parent(p)` | `str` |
| [`stem`](#stemp) | `stem(p)` | `str` |
| [`is_absolute`](#is_absolutep--is_relativep) | `is_absolute(p)` | `bool` |
| [`is_relative`](#is_absolutep--is_relativep) | `is_relative(p)` | `bool` |
| [`normalize`](#normalizep) | `normalize(p)` | `str` |
| [`is_within`](#is_withinbase-candidate--the-containment-check) | `is_within(base, candidate)` | `bool` |

---

## Building and splitting

### `join(parts)`

Join path components with the platform separator. Takes **one array**, not
varargs.

```oxi
println(path.join(["/home", "user", "file.txt"]))   // /home/user/file.txt
println(path.join(["a", "b", "c.txt"]))             // a/b/c.txt
```

> `join` is not a security boundary — see [below](#join-is-not-a-security-boundary).

### `ext(p)`

File extension **without** the dot, or `""` when there is none.

```oxi
println(path.ext("photo.png"))     // png
println(path.ext("archive.tar.gz"))// gz
println("[" + path.ext("README") + "]")  // []
```

### `filename(p)`

The last component, extension included.

```oxi
println(path.filename("/a/b/c.txt"))   // c.txt
```

### `parent(p)`

Everything before the last component.

```oxi
println(path.parent("/a/b/c.txt"))   // /a/b
```

### `stem(p)`

The filename without its extension.

```oxi
println(path.stem("data.csv"))   // data
```

Renaming while keeping a directory:

```oxi
introduce path

fun to_json(p <str>) {
    path.join([path.parent(p), path.stem(p) + ".json"])
}

println(to_json("/tmp/report.csv"))   // /tmp/report.json
```

### `is_absolute(p)` / `is_relative(p)`

Exact opposites.

```oxi
println(path.is_absolute("/a"))    // True
println(path.is_relative("a/b"))   // True
```

---

## `normalize(p)`

Collapse `.` and `..` **lexically** — it never touches the filesystem, so it
works on paths that do not exist yet.

```oxi
println(path.normalize("/srv/uploads/../../etc/passwd")) // /etc/passwd
println(path.normalize("a/./b/../c"))                    // a/c
println(path.normalize("../../a"))                       // ../../a
println(path.normalize("/../x"))                         // /x
println(path.normalize(""))                              // .
```

Note the last three. In a relative path a leading `..` has nothing to cancel
and genuinely names a sibling, so it survives. Above the root there is no
parent to climb to, so `..` is dropped. An empty path normalizes to `"."`.

Because it is lexical, `normalize` does **not** follow symlinks: if `a` is a
symlink to `/etc`, `a/../x` normalizes to `x` but opening it reaches `/x`. Use
it to tidy a path for display or comparison, not to decide whether a path is
safe.

---

## `join` is not a security boundary

`join` follows the same rule as every other language's path join, and that rule
is surprising the first time it bites you: **an absolute component throws away
everything joined before it**, and `..` is left in the string untouched.

```oxi
println(path.join(["/srv/uploads", "/etc/passwd"]))
// /etc/passwd            <- the base is simply gone

println(path.join(["/srv/uploads", "../../etc/passwd"]))
// /srv/uploads/../../etc/passwd   <- still points at /etc/passwd once opened
```

So `path.join([upload_dir, filename])` proves nothing about where the result
lands if `filename` came from a user. The function that makes that decision is
`is_within`.

---

## `is_within(base, candidate)` — the containment check

Does `candidate` resolve to `base` itself or to something inside it?

The pattern is: **join, then check, then open** — never join then open.

```oxi
introduce path

fun safe_target(base <str>, user_name <str>) {
    target := path.join([base, user_name])
    option {
        path.is_within(base, target) -> { target }
        { None }
    }
}

println(safe_target("/srv/uploads", "report.pdf"))       // /srv/uploads/report.pdf
println(safe_target("/srv/uploads", "../../etc/passwd")) // None
println(safe_target("/srv/uploads", "/etc/passwd"))      // None
```

Behaviour in detail:

```oxi
println(path.is_within("/srv/uploads", "/srv/uploads/report.pdf")) // True
println(path.is_within("/srv/uploads", "/srv/uploads"))            // True  (base itself counts)
println(path.is_within("/srv/uploads", "/etc/passwd"))             // False
println(path.is_within("/srv/up", "/srv/uploaded/x"))              // False (component-wise, not string prefix)
println(path.is_within("/srv/uploads",
                       path.join(["/srv/uploads", "../../etc/passwd"]))) // False
```

The sibling case matters: a naive string-prefix check would call
`/srv/uploaded/x` "inside" `/srv/up`. `is_within` compares whole path
components, so it does not.

Both arguments are made absolute (relative paths resolve against the current
working directory) and then resolved as far as the filesystem allows: the part
of each path that already exists is canonicalised, which **does** follow
symlinks, and any trailing components that do not exist yet are resolved
lexically. So an existing symlink inside `base` that points outside it is
correctly rejected:

```oxi
// /srv/uploads/link.txt exists and is a symlink to ../secret.txt
println(path.is_within("/srv/uploads", "/srv/uploads/link.txt"))  // False
```

### Honest limits — read these before relying on it

The symlink guarantee is partial, and the check is only as good as the
filesystem at the instant it runs:

- **Components that do not exist yet are only checked lexically.**
  `is_within(base, base + "/newdir/f.txt")` is `True` while `newdir` does not
  exist. If something then creates `newdir` as a symlink pointing elsewhere,
  your later open leaves `base` and `is_within` never saw it.
- **TOCTOU.** Between `is_within` returning `True` and your program opening the
  file, anything with write access to `base` can swap a component for a
  symlink. There is no atomic "check and open" here.
- It is a **lexical/canonical** answer, not a kernel-enforced one. It does not
  consider mount points, hard links, bind mounts, or permissions.

Treat `is_within` as the thing that rejects traversal input — `../../etc/passwd`
and friends — which is what most programs actually need. If you are guarding
real secrets against a hostile local process, you still want an OS-level
sandbox (a container, a chroot, `openat`-style resolved handles), not this
function.

---

## Worked example

```oxi
introduce path
introduce io
introduce os

// Serve a file out of an uploads directory, safely.
fun read_upload(base <str>, requested <str>) {
    target := path.join([base, requested])
    option {
        path.is_within(base, target) == False -> { <Error<forbidden>>("path escapes base") }
        os.is_file(target) == False -> { <Error<not_found>>("no such upload") }
        { io.read_file(target) }
    }
}
```

---

See also: [os](os.md) for directory listing and creation, [io](io.md) for
reading and writing.
