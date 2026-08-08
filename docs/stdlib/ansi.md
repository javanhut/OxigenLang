# ansi

Terminal colour, text modifiers, and cursor control — as plain escape strings
you print.

```oxi
introduce ansi
```

| Item | Signature | Returns |
|------|-----------|---------|
| [`ForeGroundColors`](#enum-foregroundcolors) | enum | text colour codes |
| [`BackGroundColors`](#enum-backgroundcolors) | enum | background colour codes |
| [`ModCodes`](#enum-modcodes) | enum | bold / dim / underline / reset |
| [`add_color`](#add_colorstring-color) | `add_color(string, color)` | `str` |
| [`add_modifier`](#add_modifierstring-modifer) | `add_modifier(string, modifer)` | `str` |
| [`clear_line`](#clear_line) | `clear_line()` | `str` |
| [`line_start`](#line_start) | `line_start()` | `str` |
| [`hide_cursor`](#hide_cursor--show_cursor) | `hide_cursor()` | `str` |
| [`show_cursor`](#hide_cursor--show_cursor) | `show_cursor()` | `str` |

Nothing here checks whether output is a terminal. Piping coloured output to a
file writes the escape codes into it — gate on
`os.env_get("NO_COLOR") == None` yourself if that matters.

---

## Enums

### Enum: `ForeGroundColors`

Text colour. Each member's `.value` is the raw escape sequence.

| Member | Colour |
|--------|--------|
| `Red` | red |
| `Green` | green |
| `Yellow` | yellow |
| `Blue` | blue |
| `Magenta` | magenta |
| `Cyan` | cyan |
| `White` | white |
| `Gray` | bright black / gray |

```oxi
println(ansi.ForeGroundColors.Red.value == "\e[31m")   // True
```

### Enum: `BackGroundColors`

Background colour: `Black`, `Red`, `Green`, `Yellow`, `Blue`, `Magenta`,
`Cyan`, `White`.

### Enum: `ModCodes`

| Member | Effect |
|--------|--------|
| `Reset` | clear all attributes |
| `Bold` | bold |
| `Dim` | dim |
| `Underline` | underline |

---

## Styling text

### `add_color(string, color)`

Wrap a string in a colour code and a reset. Takes any member of
`ForeGroundColors` **or** `BackGroundColors`.

```oxi
introduce ansi

println(ansi.add_color("error", ansi.ForeGroundColors.Red))
println(ansi.add_color("ok", ansi.BackGroundColors.Green))
```

The reset is always appended, so colour never leaks into later output.

### `add_modifier(string, modifer)`

The same, for a `ModCodes` member. (The parameter is spelled `modifer` in the
source — it only matters if you call it by keyword.)

```oxi
println(ansi.add_modifier("important", ansi.ModCodes.Bold))
```

Combining a colour and a modifier is just nesting:

```oxi
println(ansi.add_modifier(ansi.add_color("fatal", ansi.ForeGroundColors.Red),
                          ansi.ModCodes.Bold))
// \e[1m\e[31mfatal\e[0m\e[0m — both styles apply, then everything resets
```

---

## Cursor and line control

These return escape strings to `print` (not `println` — a newline would defeat
the point).

### `clear_line()`

Move to column 0 and erase the line. The way to overwrite an in-place status
line on each redraw.

### `line_start()`

Move to the start of the current line **without** erasing it.

### `hide_cursor()` / `show_cursor()`

Hide or show the terminal caret. Wrap a spinner loop in these so the caret does
not flicker — and always restore it when done, including on the error path;
a program that exits with the cursor hidden leaves the user's shell without
one.

```oxi
introduce ansi
introduce time

frames <array> = ["|", "/", "-", "\\"]

print(ansi.hide_cursor())
i <int>
repeat when i < 20 {
    print(ansi.clear_line() + "working " + frames[i % 4])
    time.sleep(100)
    i++
}
print(ansi.clear_line() + ansi.show_cursor())
println("done")
```

---

## Worked example

A log printer that colours by level and dims the timestamp:

```oxi
introduce ansi
introduce datetime

fun log(level <str>, msg <str>) {
    color := option {
        level == "ERROR" -> { ansi.ForeGroundColors.Red }
        level == "WARN" -> { ansi.ForeGroundColors.Yellow }
        { ansi.ForeGroundColors.Gray }
    }
    stamp := ansi.add_modifier(datetime.iso8601(datetime.now()), ansi.ModCodes.Dim)
    println("{stamp} {ansi.add_color(level, color)} {msg}")
}

log("ERROR", "disk full")
log("INFO", "started")
```

A progress bar:

```oxi
introduce ansi
introduce strings

fun bar(done <int>, total <int>) {
    width <int> := 30
    filled <int> := done * width / total
    ansi.add_color(strings.repeated("█", filled), ansi.ForeGroundColors.Green)
        + strings.repeated("░", width - filled)
}

print(ansi.clear_line() + bar(7, 10) + " 70%")
```

---

See also: [io](io.md) for console input, [os](os.md) for `env_get` when
deciding whether to colour at all.
