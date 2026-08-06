# net

HTTP client, streaming HTTP, TCP sockets, and UDP sockets. Supports both HTTP
and HTTPS.

```oxi
introduce net
```

For an HTTP **server**, use [api](api.md), which is built on this module.

## Contents

- [Enum: `HttpMethod`](#enum-httpmethod)
- [HTTP requests](#http-requests) — `get`, `post`, `put`, `patch`, `delete`, `head`, `request`
- [Streaming HTTP](#streaming-http) — `download`, `upload`, `open_stream`, `stream_*`, `read_line`, `read_chunk`
- [TCP sockets](#tcp-sockets) — `connect`, `listen`, `accept`, `send`, `receive`, `set_timeout`, `close`
- [UDP sockets](#udp-sockets) — `udp_bind`, `udp_send`, `udp_receive`
- [Errors](#errors), [ports](#ports), [blocking and timeouts](#blocking-and-timeouts), [receive buffers](#receive-buffers)

| Function | Signature | Returns |
|----------|-----------|---------|
| [`get`](#geturl) | `get(url)` | `{status, body}` |
| [`post`](#posturl-body-headers) | `post(url, body, headers = {"Content-Type": "application/json"})` | `{status, body}` |
| [`put`](#puturl-body-headers) | `put(url, body, headers = {...})` | `{status, body}` |
| [`patch`](#patchurl-body-headers) | `patch(url, body, headers = {...})` | `{status, body}` |
| [`delete`](#deleteurl) | `delete(url)` | `{status, body}` |
| [`head`](#headurl) | `head(url)` | `{status, body}` |
| [`request`](#requestmethod-url-headers-body) | `request(method, url, headers, body)` | `{status, body}` |
| [`download`](#downloadurl-path) | `download(url, path)` | `int` status |
| [`upload`](#uploadmethod-url-file_path-headers) | `upload(method, url, file_path, headers = {})` | `{status, body}` |
| [`open_stream`](#open_streamurl-method-headers-body) | `open_stream(url, method = "GET", headers = {}, body = "")` | stream handle |
| [`stream_get`](#stream_get-stream_post-stream_put-stream_patch) | `stream_get(url, headers = {})` | stream handle |
| [`stream_post`](#stream_get-stream_post-stream_put-stream_patch) | `stream_post(url, body, headers = {...})` | stream handle |
| [`stream_put`](#stream_get-stream_post-stream_put-stream_patch) | `stream_put(url, body, headers = {...})` | stream handle |
| [`stream_patch`](#stream_get-stream_post-stream_put-stream_patch) | `stream_patch(url, body, headers = {...})` | stream handle |
| [`read_line`](#read_linestream-timeout_ms) | `read_line(stream, timeout_ms = 0)` | `str` or `None` |
| [`read_chunk`](#read_chunkstream-max-timeout_ms) | `read_chunk(stream, max = 4096, timeout_ms = 0)` | `str` or `None` |
| [`connect`](#connecthost-port) | `connect(host, port)` | connection handle |
| [`listen`](#listenhost-port) | `listen(host, port)` | server handle |
| [`accept`](#acceptserver) | `accept(server)` | connection handle |
| [`send`](#sendconn-data) | `send(conn, data)` | `int` bytes written |
| [`receive`](#receiveconn-max) | `receive(conn, max = 4096)` | `str` |
| [`set_timeout`](#set_timeoutconn-ms) | `set_timeout(conn, ms)` | `None` |
| [`close`](#closehandle) | `close(handle)` | `None` |
| [`udp_bind`](#udp_bindhost-port) | `udp_bind(host, port)` | socket handle |
| [`udp_send`](#udp_sendsock-data-host-port) | `udp_send(sock, data, host, port)` | `int` bytes sent |
| [`udp_receive`](#udp_receivesock-max) | `udp_receive(sock, max = 4096)` | `(data, sender)` |

---

## Enum: `HttpMethod`

```oxi
net.HttpMethod.get      // .value == "GET"
net.HttpMethod.post     // "POST"
net.HttpMethod.put      // "PUT"
net.HttpMethod.patch    // "PATCH"
net.HttpMethod.delete   // "DELETE"
```

Only [`request`](#requestmethod-url-headers-body) takes one of these; the
shorthand functions have the method baked in.

---

## HTTP requests

Every function in this section returns a map:

| Key | Type | Contents |
|-----|------|----------|
| `status` | `int` | HTTP status code |
| `body` | `str` | response body |

and **raises a terminal `Error` on any non-2xx status** — see
[Errors](#errors) below for how to inspect one instead.

### `get(url)`

```oxi
introduce net
introduce json

resp := net.get("https://api.example.com/data")
println(resp.status)          // 200
data := json.parse(resp.body)
```

### `post(url, body, headers)`

`headers` defaults to `{"Content-Type": "application/json"}`.

```oxi
body := json.stringify({"name": "test"})
resp := net.post("https://api.example.com/items", body)
println(resp.status)   // 201
```

Posting a form instead:

```oxi
net.post(url, "a=1&b=2", {"Content-Type": "application/x-www-form-urlencoded"})
```

### `put(url, body, headers)`

Same shape as `post`.

### `patch(url, body, headers)`

Same shape as `post`.

### `delete(url)`

```oxi
println(net.delete("https://api.example.com/items/7").status)   // 204
```

### `head(url)`

Headers only — the body comes back as `""`.

```oxi
println(net.head("https://example.com/").status)   // 200
```

### `request(method, url, headers, body)`

Full control. `method` is an [`HttpMethod`](#enum-httpmethod) enum member, not
a string. Pass `None` for a body when there is none.

```oxi
resp := net.request(net.HttpMethod.get,
                    "https://api.example.com/secure",
                    {"Authorization": "Bearer token123"},
                    None)
```

---

## Streaming HTTP

`download`/`upload` move a payload without ever holding it all in memory.
`open_stream` hands you the response body incrementally — LLM token streams,
SSE, chunked responses, long polls.

A stream applies **back-pressure**: the reader stays at most ~72 KiB ahead of
you, so a consumer slower than the network cannot accumulate the whole body in
memory.

### `download(url, path)`

Stream a GET response body straight to a file. Returns the HTTP status code.

```oxi
status := net.download("https://example.com/big.iso", "big.iso")
println(status)   // 200
```

### `upload(method, url, file_path, headers)`

Stream a file as the request body. Returns `{status, body}`.

```oxi
resp := net.upload("POST", "https://example.com/upload", "big.iso",
                   {"Content-Type": "application/octet-stream"})
```

### `open_stream(url, method, headers, body)`

Open a streaming request and return a **stream handle**. Read it with
[`read_line`](#read_linestream-timeout_ms) or
[`read_chunk`](#read_chunkstream-max-timeout_ms); free it with
[`close`](#closehandle).

```oxi
net.open_stream(url)                          // GET
net.open_stream(url, "POST", headers, body)   // e.g. an LLM API
```

### `stream_get`, `stream_post`, `stream_put`, `stream_patch`

Method shorthands over `open_stream`. The three with a body default to
`{"Content-Type": "application/json"}`.

```oxi
s := net.stream_get("http://localhost:8000/events")
s := net.stream_post(url, json.stringify({"prompt": "hi"}))
```

### `read_line(stream, timeout_ms)`

Read the next newline-delimited line, **without** the newline. This is the easy
way to consume NDJSON and SSE: one call, one complete record.

- `timeout_ms = 0` (default) — block until a full line or end of stream.
- `timeout_ms > 0` — return `None` if no full line has arrived yet.
- Returns `""` at end of stream.

It returns whole UTF-8 characters, so emoji, accents and CJK are never split
across two reads.

```oxi
introduce net
introduce json

headers := {"Content-Type": "application/json"}
body := json.stringify({"model": "llama3", "prompt": "Say hi"})
s := net.open_stream("http://localhost:11434/api/generate", "POST", headers, body)

done := False
repeat unless done {
    line := net.read_line(s)               // one NDJSON object, or "" at EOF
    option {
        line == "" -> { done = True }
        {
            obj := json.parse(line)
            print(obj["response"])         // token, live
            done = obj["done"]
        }
    }
}
net.close(s)
```

The `timeout_ms` form is how you animate a spinner while the model thinks:

```oxi
line := net.read_line(s, 100)
option {
    line == None -> { print(spinner_frame()) }   // nothing yet
    { handle(line) }
}
```

### `read_chunk(stream, max, timeout_ms)`

Lower-level: read up to `max` raw bytes, for binary or non-line protocols.
Same timeout rules; `""` at end of stream. Prefer `read_line` for text.

---

## TCP sockets

A **handle** is an opaque ticket number for an open socket. Pass it to
`send`/`receive`/`close`. Always `close` a handle when you are done — a
forgotten handle leaks one connection until the program exits, like a file you
never close.

### `connect(host, port)`

Open a TCP connection; returns a connection handle.

### `listen(host, port)`

Bind a TCP server socket; returns a server handle.

### `accept(server)`

Block until a client connects; returns a connection handle for that client.

### `send(conn, data)`

Send text; returns the number of bytes written.

### `receive(conn, max)`

Read up to `max` bytes. Returns `""` on a clean close — and also when a
[`set_timeout`](#set_timeoutconn-ms) deadline passes with nothing to read.

### `set_timeout(conn, ms)`

Bound how long `receive` waits on this connection. `0` restores waiting
forever.

### `close(handle)`

Close a connection or server handle. Idempotent.

```oxi
// Server
introduce net

srv := net.listen("127.0.0.1", 8080)
conn := net.accept(srv)
net.set_timeout(conn, 2000)
msg := net.receive(conn)
net.send(conn, "echo: " + msg)
net.close(conn)
net.close(srv)
```

```oxi
// Client
introduce net

c := net.connect("127.0.0.1", 8080)
println(net.send(c, "hello"))    // 5
println(net.receive(c))          // echo: hello
net.close(c)
```

Socket I/O works on **UTF-8 text** — the common case (HTTP, line protocols,
JSON over TCP).

---

## UDP sockets

### `udp_bind(host, port)`

Bind a UDP socket; returns a socket handle.

### `udp_send(sock, data, host, port)`

Send one datagram to `host:port`; returns the number of bytes sent.

### `udp_receive(sock, max)`

Receive one datagram (up to `max` bytes). Returns a `(data, sender_address)`
tuple.

```oxi
introduce net

sock := net.udp_bind("127.0.0.1", 9000)
net.udp_send(sock, "ping", "127.0.0.1", 9000)
data, sender := net.udp_receive(sock)
println(data)   // ping
net.close(sock)
```

**A datagram larger than `max` is an error, not a short read.** UDP has no
stream to resume from: the OS keeps what fits your buffer and discards the
rest.

```oxi
// a 16-byte datagram arrives
net.udp_receive(sock, 4)
// error: udp_receive: datagram is larger than max (4 bytes); the rest was
//        discarded by the OS and cannot be recovered — retry with a larger max
//        (up to 65536)
```

> **This changed.** That call used to return `"0123"` and drop 12 bytes with no
> indication anything was missing — a partial message that looks exactly like a
> complete one. If you want whatever arrives regardless of size, pass a `max`
> of `65536`; no datagram can exceed it, so the error can never fire.

---

## Errors

On failure — connection refused, host not found, file missing, non-2xx HTTP
status — every `net` function returns a **terminal `Error` that halts
execution**. Normalize the call into a value you can inspect:

```oxi
introduce net
introduce result

res := <type<Error> || <Value>>(net.connect("127.0.0.1", 9999))
println(res.msg) when result.is_err(res)
// Connection refused (os error 61)

resp := <type<Error> || <Value>>(net.get("http://localhost:8000/missing"))
println(resp.msg) when is_error(resp)
// http error: http status: 404
```

That last one is worth remembering: because a non-2xx raises, a client cannot
read the error body a server sent unless the call is normalized first.

---

## Ports

`connect`, `listen`, `udp_bind` and `udp_send` validate the port before use.
The accepted range is **1–65535**, plus **0 for the two binding calls**
(`listen` and `udp_bind`), where 0 means "let the OS pick a free ephemeral
port". Connecting or sending *to* port 0 is never meaningful and is rejected.

```oxi
net.connect("127.0.0.1", 74626)  // Error: port 74626 out of range: must be 1-65535
net.connect("127.0.0.1", 0)      // Error: port 0 out of range: must be 1-65535
net.listen("127.0.0.1", 65536)   // Error: port 65536 out of range: must be 0-65535
net.udp_bind("127.0.0.1", -1)    // Error: port -1 out of range: must be 0-65535
```

> **This changed.** An out-of-range port used to be truncated to its low 16
> bits and silently used. `net.connect(host, 74626)` connected to **port
> 9090**, `65616` reached **port 80**, `-1` reached **65535**, and
> `net.listen(host, 65536)` masked to 0 and bound a *random* ephemeral port
> while reporting success — a server that believed it was listening on the port
> you asked for. If you have code that computed a port arithmetically and
> relied on the wrap-around, it now errors instead; apply the `% 65536`
> yourself if that was deliberate.

---

## Blocking and timeouts

All socket calls are synchronous and block the calling VM until the OS
operation completes: `connect` waits for the handshake, `accept` waits for a
client, `receive`/`udp_receive` wait for data. Run them under `diverge` (see
[concurrency.md](../concurrency.md)) so a slow peer stalls one thread instead
of your program.

| Call | Timeout | Default |
|------|---------|---------|
| `connect` | TCP handshake | **30 seconds**, then `Error: connection timed out`. Not configurable. |
| `receive` | `set_timeout(conn, ms)` | none until you set one; then an expired read returns `""` |
| `accept`, `udp_receive`, `send` | none | block until the OS returns |
| `read_line`, `read_chunk` | `timeout_ms` argument | `0` = block forever; `> 0` returns `None` when nothing has arrived yet |

Set a `receive` timeout on any connection a **remote peer** can hold open. A
server that accepts a connection and reads from it has handed a stranger the
power to park that thread indefinitely by connecting and saying nothing; with a
timeout the read ends and the thread moves on. An expired read reports `""`,
the same as a closed peer — to a caller that asked to stop waiting, a silent
client and a departed one are the same thing, and reporting it as an error
would make every timeout something to unwrap.

The connect bound exists because a blackholed SYN otherwise hangs forever, and
program exit waits for spawned tasks to finish — one stuck `connect` wedged the
whole process. Name resolution happens before the timer starts and is still
bounded only by the OS resolver.

---

## Receive buffers

`receive`/`udp_receive` take `max` as an **upper bound, not an allocation
request**: a single TCP `receive` returns at most 64 KiB however large `max`
is (it is one read of a byte stream, so it already returned "up to `max`" —
loop if you want more), and a UDP datagram cannot exceed 64 KiB in the first
place, so the internal cap never truncates. `max` must be positive.

Previously the buffer was allocated at exactly `max`, so
`receive(c, 10000000000)` tried to allocate 10 GB up front — and an allocation
failure aborts the process outright, with no catchable error.

---

## Worked example

```oxi
introduce net
introduce json
introduce result

// A tiny API client that reports failures instead of halting.
fun api_get(url <str>) {
    r := <type<Error> || <Value>>(net.get(url))
    option {
        is_error(r) -> { <Error<http>>(r.msg) }
        { json.parse(r.value.body) }
    }
}

data := result.unwrap_or(api_get("https://api.example.com/items"), [])
println(len(data))
```

---

See also: [api](api.md) for the HTTP server, [json](json.md) for bodies,
[encoding](encoding.md) for auth headers and query escaping,
[concurrency.md](../concurrency.md) for running blocking calls off the main
thread.
