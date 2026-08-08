# time

Clocks, sleeping, and measuring how long something took.

```oxi
introduce time
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`now`](#now) | `now()` | `float` — unix seconds |
| [`now_ms`](#now_ms) | `now_ms()` | `int` — unix milliseconds |
| [`sleep`](#sleepms) | `sleep(ms)` | `None` |
| [`monotonic`](#monotonic) | `monotonic()` | `int` — nanoseconds |
| [`elapsed_ms`](#elapsed_msstart-end) | `elapsed_ms(start, end)` | `int` |
| [`elapsed_s`](#elapsed_sstart-end) | `elapsed_s(start, end)` | `float` |

Two different clocks live here, and mixing them up is the usual bug:

| Clock | Function | Use it for |
|-------|----------|------------|
| **Wall clock** | `now`, `now_ms` | timestamps, "when did this happen" |
| **Monotonic** | `monotonic` | durations, "how long did this take" |

The wall clock can jump backwards (NTP correction, someone changing the system
time), which makes `end - start` on it occasionally negative or wildly wrong.
The monotonic clock only ever moves forward, but its zero point is arbitrary —
its value alone means nothing, only the difference between two readings does.

For calendar dates and formatting, use [datetime](datetime.md).

---

## Wall clock

### `now()`

Unix timestamp in seconds, as a **float** (fractional seconds included).

```oxi
println(time.now())   // 1785000000.123456
```

### `now_ms()`

Unix timestamp in milliseconds, as an **integer**. The convenient form for
stamping records and for JSON.

```oxi
println(time.now_ms())   // 1785000000123
```

---

## Sleeping

### `sleep(ms)`

Block the current task for `ms` milliseconds.

```oxi
introduce time

println("starting")
time.sleep(500)
println("half a second later")
```

Sleeping inside a `diverge` block parks only that task, so a retry loop with
backoff does not stall the rest of the program — see
[concurrency.md](../concurrency.md).

```oxi
introduce time
introduce net

// Retry with exponential backoff.
fun fetch_with_retry(url <str>, tries <int>) {
    delay <int> := 100
    i <int>
    repeat when i < tries {
        r := <type<Error> || <Value>>(net.get(url))
        give r.value unless is_error(r)
        time.sleep(delay)
        delay = delay * 2
        i++
    }
    <Error<gave_up>>("{tries} attempts failed")
}
```

---

## Measuring durations

### `monotonic()`

A monotonic clock reading in **nanoseconds**. Only differences are meaningful.

### `elapsed_ms(start, end)`

Milliseconds between two `monotonic` readings (integer division, so it
truncates).

### `elapsed_s(start, end)`

Seconds between two `monotonic` readings, as a float.

```oxi
introduce time

start := time.monotonic()
time.sleep(100)
end := time.monotonic()

println(time.elapsed_ms(start, end))   // 105    <- sleep is a floor, not a promise
println(time.elapsed_s(start, end))    // 0.105061208
```

Timing a function:

```oxi
introduce time

fun timed(label <str>, f <generic>) {
    start := time.monotonic()
    out := f()
    println("{label} took {time.elapsed_ms(start, time.monotonic())}ms")
    out
}

timed("work", fun() { time.sleep(50) })
// work took 53ms
```

For sub-millisecond work `elapsed_ms` truncates to `0` — subtract the
`monotonic` values directly for nanoseconds, or use `elapsed_s`.

---

See also: [datetime](datetime.md) for calendar dates and formatting,
[concurrency.md](../concurrency.md) for sleeping inside spawned tasks.
