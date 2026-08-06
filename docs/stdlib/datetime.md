# datetime

Calendar dates and times, in **UTC**. A timestamp is unix seconds as an
integer.

```oxi
introduce datetime
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`now`](#now) | `now()` | `int` — unix seconds |
| [`format`](#formatts-fmt) | `format(ts, fmt)` | `str` |
| [`parse`](#parsetext-fmt) | `parse(text, fmt)` | `int` or `Error` |
| [`year`](#components) | `year(ts)` | `int` |
| [`month`](#components) | `month(ts)` | `int` (1–12) |
| [`day`](#components) | `day(ts)` | `int` (1–31) |
| [`hour`](#components) | `hour(ts)` | `int` (0–23) |
| [`minute`](#components) | `minute(ts)` | `int` (0–59) |
| [`second`](#components) | `second(ts)` | `int` (0–59) |
| [`weekday`](#weekdayts) | `weekday(ts)` | `str` |
| [`iso8601`](#iso8601ts) | `iso8601(ts)` | `str` |

**Everything is UTC.** There is no timezone or local-time support — no
conversion, no DST. If you need a local time, offset the timestamp yourself and
be explicit about it.

For measuring how long something took, use
[`time.monotonic`](time.md#monotonic), not this module.

---

### `now()`

Current time as unix seconds, truncated to a whole second.

```oxi
introduce datetime

now <int> := datetime.now()
println(datetime.iso8601(now))   // 2026-08-06T18:03:11Z
```

`time.now()` returns the same clock as a float with fractional seconds;
`datetime.now()` is the integer form the rest of this module expects.

### `format(ts, fmt)`

Format a timestamp with strftime-style directives.

```oxi
ts <int> := 1781963100

println(datetime.format(ts, "%Y-%m-%d"))            // 2026-06-20
println(datetime.format(ts, "%H:%M"))               // 13:45
println(datetime.format(ts, "%A, %d %B %Y"))        // Saturday, 20 June 2026
println(datetime.format(ts, "%Y-%m-%dT%H:%M:%SZ"))  // 2026-06-20T13:45:00Z
```

Common directives:

| Directive | Meaning | Example |
|-----------|---------|---------|
| `%Y` | 4-digit year | `2026` |
| `%m` | month, zero-padded | `06` |
| `%d` | day of month, zero-padded | `20` |
| `%H` | hour, 24-hour | `13` |
| `%M` | minute | `45` |
| `%S` | second | `00` |
| `%A` | weekday name | `Saturday` |
| `%B` | month name | `June` |
| `%j` | day of year | `171` |
| `%s` | unix timestamp | `1781963100` |
| `%%` | a literal `%` | `%` |

### `parse(text, fmt)`

Parse text into unix seconds. The format must describe the **whole** string.
Returns a catchable `<Error>` when the text does not match.

```oxi
ts <int> := datetime.parse("2026-06-20 13:45:00", "%Y-%m-%d %H:%M:%S")
println(ts)   // 1781963100

r := <type<Error> || <Value>>(datetime.parse("20/06/2026", "%Y-%m-%d"))
println(r.msg)
// could not parse '20/06/2026': input contains invalid characters
```

**The text must carry a full date *and* time.** A date-only format is not
enough, even though it looks like it should be:

```oxi
d := <type<Error> || <Value>>(datetime.parse("2026-06-20", "%Y-%m-%d"))
println(d.msg)
// could not parse '2026-06-20': input is not enough for unique date and time
```

Supply the missing half yourself:

```oxi
fun parse_date(s <str>) { datetime.parse(s + " 00:00:00", "%Y-%m-%d %H:%M:%S") }

println(parse_date("2026-06-20"))   // 1781913600
```

### Components

`year`, `month`, `day`, `hour`, `minute`, `second` each pull one field out of a
timestamp, as an integer, in UTC.

```oxi
ts <int> := datetime.parse("2026-06-20 13:45:00", "%Y-%m-%d %H:%M:%S")

println(datetime.year(ts))     // 2026
println(datetime.month(ts))    // 6
println(datetime.day(ts))      // 20
println(datetime.hour(ts))     // 13
println(datetime.minute(ts))   // 45
println(datetime.second(ts))   // 0
```

Each one formats and re-parses internally, so pulling all six costs six passes;
call `format` once with the layout you want if that matters.

### `weekday(ts)`

The full weekday name.

```oxi
println(datetime.weekday(ts))   // Saturday
```

### `iso8601(ts)`

RFC 3339 / ISO 8601 in UTC — the format to use in logs, filenames, and JSON.

```oxi
println(datetime.iso8601(ts))   // 2026-06-20T13:45:00Z
```

---

## Arithmetic

Timestamps are plain integers, so date arithmetic is just arithmetic. There is
no `add_days` function because it would only be `+ 86400`.

```oxi
introduce datetime

DAY <int> = 86400

fun parse_date(s <str>) { datetime.parse(s + " 00:00:00", "%Y-%m-%d %H:%M:%S") }

ts <int> := datetime.now()
println(datetime.format(ts + 7 * DAY, "%Y-%m-%d"))     // one week out
println((ts - parse_date("2026-01-01")) / DAY)         // days into the year
```

Months and years are not fixed-length, so `+ 30 * DAY` is "30 days", not "a
month". For calendar-aware stepping, decompose with the component functions and
re-`parse`.

---

## Worked example

```oxi
introduce datetime
introduce io

// Append a timestamped line to a daily log file.
fun log_event(msg <str>) {
    now <int> := datetime.now()
    file := "events-" + datetime.format(now, "%Y-%m-%d") + ".log"
    io.append_file(file, "{datetime.iso8601(now)} {msg}\n")
}

log_event("deploy started")
// events-2026-08-06.log  ->  2026-08-06T18:03:11Z deploy started
```

---

See also: [time](time.md) for durations and sleeping, [strings](strings.md) for
formatting output.
