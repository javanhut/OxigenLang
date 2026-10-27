# channel

Pass messages between tasks, in order: a queue that one task puts values on
and another takes them off.

```oxi
introduce channel
```

| Function | Signature | Returns |
|----------|-----------|---------|
| [`new`](#new) | `new()` | a channel |
| [`send`](#sendch-value) | `send(ch, value)` | `None` |
| [`recv`](#recvch-timeout_ms) | `recv(ch, timeout_ms?)` | the next value, or `None` |
| [`try_recv`](#try_recvch) | `try_recv(ch)` | the next value, or `None` |
| [`close`](#closech) | `close(ch)` | `None` |
| [`len`](#lench) | `len(ch)` | `int` |
| [`is_closed`](#is_closedch) | `is_closed(ch)` | `bool` |

A channel is a handle, an `int`, so it crosses into a
[`diverge`](../concurrency.md) task like any other value, and every task that
has it uses the same queue. A value sent is **copied**, the same way task
arguments and results are, so anything that can cross threads can be sent:
numbers, strings, collections, structs and enums. Functions cannot.

A worker that processes jobs in the order they arrive:

```oxi
introduce channel

jobs := channel.new()

worker := diverge {
    done := 0
    repeat when True {
        job := channel.recv(jobs)
        stop when job == None          // closed and empty
        println("speaking: " + job)
        done = done + 1
    }
    done
}

each line in ["hello", "how are you", "goodbye"] {
    channel.send(jobs, line)
}
channel.close(jobs)
println(converge worker)   // 3
```

Channels need the `threads` capability, like `diverge`.

---

## `new()`

A new, empty channel. It has no size limit: `send` never waits.

## `send(ch, value)`

Put `value` at the end of the queue. Sending on a closed channel is an error.

## `recv(ch, timeout_ms?)`

Take the value at the front of the queue, waiting for one if the queue is
empty. Returns `None` once the channel is closed and empty, so a loop ends
when the sender closes it. With `timeout_ms`, it gives up after that many
milliseconds and returns `None`:

```oxi
introduce channel

ch := channel.new()
println(channel.recv(ch, 100))   // None, after 100 ms
```

`None` can also be sent as a value. A channel that carries `None` needs
`is_closed` to tell the two apart.

## `try_recv(ch)`

`recv` without waiting: the next value if one is queued, or `None` at once.
For checking a channel from inside a loop that has other work:

```oxi
introduce channel

ch := channel.new()
channel.send(ch, "stop")
println(channel.try_recv(ch))   // stop
println(channel.try_recv(ch))   // None
```

## `close(ch)`

Stop the channel taking new values. Receivers waiting on it wake up. Values
already queued can still be received; after them, `recv` returns `None`.

## `len(ch)`

How many values are queued.

## `is_closed(ch)`

Whether `close` has been called.

---

## Signals

[`os.signals`](os.md#signalsnames) returns a channel that receives Ctrl-C and
other signals as strings, so a program can clean up before it exits:

```oxi
introduce os
introduce channel

stop := os.signals(["INT", "TERM"])
speaker := os.spawn("say", [])
repeat when channel.try_recv(stop) == None {
    // ... feed the speaker ...
}
os.kill(speaker)
```
