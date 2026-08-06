# api

An HTTP API server: routing, JSON, and the accept loop in one module. It plays
the part of a web framework and its server at once, so there is nothing to
install alongside it.

```oxi
introduce api
```

[net](net.md) gives you sockets; `api` gives you a server built on them.

## Contents

- [Quick start](#quick-start)
- [Running a server](#running-a-server) — `serve`, `app`
- [Routes](#routes) — `app.get`/`post`/`put`/`patch`/`delete`/`route`, `app.listen`
- [Handlers and the request](#handlers-and-the-request)
- [Responses](#responses) — `json`, `text`, `html`, `redirect`, `respond`, `fail_with`
- [Reading the request](#reading-the-request) — `body_json`, `form`, `parse_query`, `decode`
- [Concurrency and limits](#concurrency-and-limits)
- [Structs and constants](#structs-and-constants)
- [Internals](#internals)

---

## Quick start

```oxi
introduce api

fun index(req) { {"service": "oxigen"} }

fun show_user(req) { {"id": req.params["id"], "page": req.query["page"]} }

fun create_user(req) { api.json(api.body_json(req), 201) }

fun routes(cfg) {
    app := api.app(cfg)
    app.get("/", index)
    app.get("/users/:id", show_user)
    app.post("/users", create_user)
    app.listen()
}

main { api.serve(routes, port=8000) }
```

```
$ curl localhost:8000/
{"service":"oxigen"}

$ curl 'localhost:8000/users/42?page=2'
{"id":"42","page":"2"}
```

---

## Running a server

| Function | Signature |
|----------|-----------|
| [`serve`](#serveworker-host-port-workers-keep_alive-idle_timeout) | `serve(worker, host = "0.0.0.0", port = 8000, workers = 64, keep_alive = True, idle_timeout = 5000)` |
| [`app`](#appcfg) | `app(cfg)` |

### `serve(worker, host, port, workers, keep_alive, idle_timeout)`

Bind `host:port` and run `worker` on `workers` threads, each with its own
router over the shared listening socket. **Blocks** until the acceptors stop.

| Parameter | Default | Meaning |
|-----------|---------|---------|
| `worker` | — | a top-level function taking one argument, the config map |
| `host` | `"0.0.0.0"` | interface to bind; `"127.0.0.1"` for local-only |
| `port` | `8000` | port to bind |
| `workers` | `64` | how many connections are served at once |
| `keep_alive` | `True` | reuse a connection for more requests |
| `idle_timeout` | `5000` | ms a single read may stall before the connection is dropped |

`serve` takes a **top-level function**, not a configured app. Oxigen's
concurrency is share-nothing — a handler closure cannot be copied to another
thread — so `serve` hands each worker only the listening socket, and each
worker calls your function to build its own routing table from the module's
top-level functions. The table is built once per worker, not once per request,
and no handler ever crosses a thread boundary.

That is why the routes live inside a function and why it ends with
`app.listen()`.

```oxi
main { api.serve(routes, host="127.0.0.1", port=3000, workers=16) }
```

On start it prints:

```
api listening on http://127.0.0.1:3000 (16 workers)
```

### `app(cfg)`

Build this worker's router from the config map `serve` handed it. Returns an
[`App`](#struct-app). Call it as the first line of your worker function.

```oxi
fun routes(cfg) {
    app := api.app(cfg)
    // ... register routes ...
    app.listen()
}
```

A struct cannot be rebuilt on a thread that did not declare it, which is why
what crosses the thread boundary is a plain map and the `App` is constructed
here, on the worker.

---

## Routes

| Method | Signature |
|--------|-----------|
| `app.get` / `post` / `put` / `patch` / `delete` | `app.get(path, handler)` |
| [`app.route`](#approutemethod-path-handler) | `app.route(method, path, handler)` — any other verb |
| [`app.listen`](#applisten) | `app.listen()` |

### `app.get(path, handler)` and friends

Register a handler for one method and path. Each returns the `App`, so calls
can be chained.

```oxi
app.get("/health", health)
app.post("/users", create_user)
app.put("/users/:id", replace_user)
app.patch("/users/:id", update_user)
app.delete("/users/:id", delete_user)
```

Paths take parameters as **`:name`** — `"/users/:id/posts"`. (Not `{name}`: a
bare `{` inside an Oxigen string opens an interpolation, so that form would
need escaping at every call site.)

Registration sorts routes by shape: a path with no parameter is an O(1) map
lookup, and only parameterized paths are compared segment by segment.

### `app.route(method, path, handler)`

The general form — use it for verbs without a shorthand.

```oxi
app.route("OPTIONS", "/", preflight)
app.route("HEAD", "/", index)
```

`HEAD` is not answered automatically: register it if a client needs it,
otherwise a `HEAD` request to a `GET`-only path gets a
[405](#handlers-and-the-request).

### `app.listen()`

Accept connections forever. Call it **last** in your worker function.

It contains failures rather than dying on them: a failed `accept` is logged and
retried, and a connection that breaks mid-request is logged and dropped without
taking the acceptor with it. After 100 consecutive accept failures the loop
gives up and logs why — one failure is routine (a peer vanished mid-handshake),
a hundred in a row means the listener is broken and retrying forever would spin
a core.

---

## Handlers and the request

A handler is a function of one argument that returns whatever it wants to send:

| Returned | Sent as |
|----------|---------|
| a `Res` from `json`/`text`/`html`/`redirect`/`respond`/`fail_with` | that response |
| a string | `200 text/plain` |
| anything else (map, array, number, struct) | `200 application/json` |

```oxi
fun index(req) { {"service": "oxigen"} }        // 200 application/json
fun plain(req) { "just text" }                  // 200 text/plain
fun page(req) { api.html("<h1>hi</h1>") }       // 200 text/html
```

A handler that raises answers **500** and logs the real message server-side;
the client is told only `{"error": "internal server error"}`, and the worker
keeps serving.

A request that matches no route answers **404** — unless the *path* is
registered under some other method, which answers **405** with an `Allow`
header naming the methods that would have worked:

```
$ curl -i localhost:8000/echo        # /echo is registered as POST
HTTP/1.1 405 Method Not Allowed
Allow: POST

{"error":"method not allowed","allow":["POST"]}
```

The distinction matters when calling your own server: a 404 on a URL you know
is correct sends you hunting for a typo, when the real answer is that you used
the wrong verb. Note that `net.get` and friends raise on any non-2xx, so a
client halts before it can read that body — normalize the call to see it:

```oxi
resp := <type<Error> || <Value>>(net.get("{url}/echo"))
println(resp.msg) when is_error(resp)     // http error: http status: 405
```

### The request map

The request is a map, so `req.params` and `req["params"]` both work:

| Key | Type | Contents |
|-----|------|----------|
| `method` | `str` | `"GET"` |
| `path` | `str` | `"/users/42"` (no query string) |
| `version` | `str` | `"HTTP/1.1"` |
| `params` | `map` | path parameters, percent-decoded — `{"id": "42"}` |
| `query` | `map` | query string, percent-decoded — `{"page": "2"}` |
| `headers` | `map` | header names **lowercased** — `{"content-type": "application/json"}` |
| `body` | `str` | the raw body string |

A missing query key reads as `None`, which serializes to `null`.

```oxi
fun show_user(req) {
    id := req.params["id"]
    option {
        has(users, id) -> { {"id": id, "name": users[id], "query": req.query} }
        { api.fail_with(404, "no user {id}") }
    }
}
```

---

## Responses

| Function | Signature | Content-Type |
|----------|-----------|--------------|
| [`json`](#jsondata-status) | `json(data, status = 200)` | `application/json` |
| [`text`](#textbody-status) | `text(body, status = 200)` | `text/plain; charset=utf-8` |
| [`html`](#htmlbody-status) | `html(body, status = 200)` | `text/html; charset=utf-8` |
| [`redirect`](#redirectlocation-status) | `redirect(location, status = 302)` | — (`Location` header) |
| [`respond`](#respondstatus-body-headers) | `respond(status, body, headers = {})` | whatever you pass |
| [`fail_with`](#fail_withstatus-message) | `fail_with(status, message)` | `application/json` |

Each returns a [`Res`](#struct-res).

### `json(data, status)`

JSON response from any Oxigen value — maps, arrays, structs, primitives.

```oxi
fun create_user(req) { api.json(api.body_json(req), 201) }
```

Returning a plain map from a handler does the same thing with status 200; use
`json` when you need a different status.

### `text(body, status)`

```oxi
fun health(req) { api.text("ok") }
```

### `html(body, status)`

```oxi
fun page(req) { api.html("<h1>hi</h1>") }
```

### `redirect(location, status)`

302 by default; pass 301 for a permanent move.

```oxi
fun go(req) { api.redirect("/") }
fun moved(req) { api.redirect("https://new.example.com", 301) }
```

### `respond(status, body, headers)`

The escape hatch when the helpers do not fit — explicit status, body, and
headers.

```oxi
fun custom(req) { api.respond(202, "queued", {"X-Job": "7"}) }
```

```
HTTP/1.1 202 Accepted
X-Job: 7
Content-Length: 6
```

### `fail_with(status, message)`

A JSON error body under the given status — the same shape unmatched routes and
failed handlers return, so clients see one error format.

```oxi
fun show(req) {
    option {
        has(users, req.params["id"]) -> { users[req.params["id"]] }
        { api.fail_with(404, "no such user") }
    }
}
// {"error":"no such user"}
```

---

## Reading the request

| Function | Signature | Returns |
|----------|-----------|---------|
| [`body_json`](#body_jsonreq) | `body_json(req)` | parsed value |
| [`form`](#formreq) | `form(req)` | `map` |
| [`parse_query`](#parse_querys) | `parse_query(s)` | `map` |
| [`decode`](#decodes) | `decode(s)` | `str` |

### `body_json(req)`

Parse the request body as JSON.

```oxi
fun echo(req) { {"you_sent": api.body_json(req), "bytes": len(req.body)} }
```

A malformed body makes this raise, which the dispatcher turns into a 500. To
answer 400 instead, normalize it:

```oxi
fun echo(req) {
    parsed := <type<Error> || <Value>>(api.body_json(req))
    option {
        is_error(parsed) -> { api.fail_with(400, "body must be JSON") }
        { parsed.value }
    }
}
```

### `form(req)`

Parse the body as an HTML form (`application/x-www-form-urlencoded`). Keys and
values are percent-decoded.

```oxi
fun submit(req) { api.form(req) }
```

```
$ curl -X POST localhost:8000/submit -d 'a=1&b=hello%20world'
{"a":"1","b":"hello world"}
```

### `parse_query(s)`

Parse a query string (`"a=1&b=hello%20world"`) into a map. `req.query` is
already parsed for you; this is for query strings from elsewhere.

A key with no `=` gets `""`; a value containing `=` keeps everything after the
first one.

### `decode(s)`

Percent-decode, **falling back to the raw text on a malformed escape**. A
client controls these strings, so a bad `%ZZ` must not put an error value where
a handler expects a string. Used internally for path parameters and query
values.

---

## Concurrency and limits

This is a **thread-per-connection** server: `serve` runs `workers` accept loops
on real OS threads over one shared listening socket, and a connection holds its
worker until it closes. So `workers` is the number of connections served at
once — size it for the clients you expect connected simultaneously, not for
core count.

Keeping connections alive is worth roughly double the throughput and is the
default. Two limits keep that from turning into a stuck server when clients
outnumber workers:

- `idle_timeout` (5s) reclaims a worker whose client went quiet.
- `keep_alive_max` (100 requests) closes a connection so a busy client's worker
  returns to the queue.

Over capacity the server slows down; it does not stop accepting. Pass
`keep_alive=False` to answer one request per connection instead — concurrency
then caps at `workers` *requests* rather than `workers` *clients*.

Because `idle_timeout` bounds each individual read rather than the request as a
whole, a slowloris client loses its connection while a genuinely slow upload,
which keeps sending, does not.

A client cannot make the server allocate without bound:

| Limit | Value | Response when exceeded |
|-------|-------|------------------------|
| `head_limit` | 64 KiB | **431** request headers too large |
| `body_limit` | 8 MiB | **413** request body too large |
| unparseable `Content-Length` | — | **400** invalid Content-Length |

**Not implemented:** request pipelining, TLS (terminate it at a proxy), and
multipart uploads. Binary request bodies are read as UTF-8 text.

---

## Structs and constants

### Struct: `Res`

An HTTP response.

| Field | Type | Meaning |
|-------|------|---------|
| `status` | `int` | status code |
| `headers` | `map` | response headers |
| `body` | `str` | response body |

Build one with the [response helpers](#responses) rather than by hand. If you
do need the constructor, import the struct by name — a struct is not
constructible through a module namespace, so `api.Res(...)` fails with
*struct 'Res' is not defined in this scope*:

```oxi
introduce api
introduce {Res} from api

fun raw(req) { Res(200, {"X-A": "1"}, "hi") }
```

### Struct: `App`

A router plus the socket it serves. Each worker thread gets its own.

| Field | Type | Meaning |
|-------|------|---------|
| `server` | `int` | listening socket handle |
| `routes` | `map` | static routes, keyed `"METHOD /path"` |
| `dynamic` | `array` | parameterized routes as `(method, segments, handler)` |
| `keep_alive` | `bool` | whether connections are reused |
| `idle_timeout` | `int` | per-read timeout in ms |

### Module constants

| Constant | Value | Meaning |
|----------|-------|---------|
| `head_limit` | `65536` | max request head size before a 431 |
| `body_limit` | `8388608` | max body size before a 413 |
| `read_size` | `65536` | socket read chunk size |
| `accept_retry_limit` | `100` | consecutive accept failures before `listen` gives up |
| `keep_alive_max` | `100` | requests one kept-alive connection may serve |

These are module-level bindings, readable as `api.body_limit`. Changing them
means editing `stdlib/api.oxi`.

---

## Internals

Everything below is exported because every top-level binding in an Oxigen
module is. You do not need it to write a server — it is documented so that
reading a stack trace, or extending the module, is not guesswork.

### Connection handling

| Function | Signature | Purpose |
|----------|-----------|---------|
| `app.serve_conn` | `serve_conn(conn)` | Serve one connection: read, route, respond, repeat while kept alive |
| `app.dispatch` | `dispatch(req)` | Find the handler, run it contained, and normalize the result to a `Res` |
| `read_request` | `read_request(conn)` | Read one request. Returns a request map, a `Res` to send back when the request is unusable, or `None` when the peer closed |
| `read_body` | `read_body(conn, so_far, length)` | Read the rest of the body, collecting chunks and joining once |
| `content_length` | `content_length(headers)` | `Content-Length` as an `Error \|\| Value`; absent means `0`, unparseable is an error |
| `wants_keep_alive` | `wants_keep_alive(req)` | HTTP/1.1 keeps the connection open unless told otherwise; HTTP/1.0 is the reverse |

### Routing

| Function | Signature | Purpose |
|----------|-----------|---------|
| `compile_path` | `compile_path(path)` | Split a route path into `(is_param, name)` segments once, at registration |
| `match_segments` | `match_segments(segments, segs)` | Match one compiled route against a request's segments; returns the captured params or `None` |
| `match_dynamic` | `match_dynamic(dynamic, method, segs)` | Scan the parameterized routes; returns `(handler, params)` or `(None, {})` |
| `allowed_methods` | `allowed_methods(routes, dynamic, path)` | Every method registered for a path |
| `miss` | `miss(routes, dynamic, path)` | The 404-or-405 answer for an unmatched request |
| `to_res` | `to_res(v)` | Normalize whatever a handler returned into a `Res` |

### Wire format

| Function | Signature | Purpose |
|----------|-----------|---------|
| `render` | `render(res, live)` | Serialize a response as one string, sent in a single write, so the status line and body never land in separate TCP segments |
| `reason` | `reason(status)` | Reason phrase for a status code; anything unlisted renders as `"OK"`, which clients ignore |

`reason` covers 200, 201, 202, 204, 301, 302, 304, 400, 401, 403, 404, 405,
409, 413, 422, 429, 431, 500, 503.

---

## Worked example

A small JSON API with path parameters, a JSON body, a deliberate failure, and
an error response — the same shape as `example/api_server.oxi`:

```oxi
introduce api

users <map> = {"1": "ada", "42": "grace"}

fun index(req) {
    {"service": "oxigen api", "routes": ["/", "/users/:id", "/echo"]}
}

fun show_user(req) {
    id := req.params["id"]
    option {
        has(users, id) -> { {"id": id, "name": users[id], "query": req.query} }
        { api.fail_with(404, "no user {id}") }
    }
}

fun echo(req) {
    {"you_sent": api.body_json(req), "bytes": len(req.body)}
}

fun boom(req) { <fail>("this handler is broken on purpose") }

fun routes(cfg) {
    app := api.app(cfg)
    app.get("/", index)
    app.get("/users/:id", show_user)
    app.post("/echo", echo)
    app.get("/boom", boom)
    app.listen()
}

main { api.serve(routes, port=8000) }
```

```
$ curl localhost:8000/users/42
{"id":"42","name":"grace","query":{}}

$ curl -X POST localhost:8000/echo -d '{"hello":"world"}'
{"you_sent":{"hello":"world"},"bytes":17}

$ curl -i localhost:8000/boom
HTTP/1.1 500 Internal Server Error
{"error":"internal server error"}
```

---

See also: [net](net.md) for the sockets underneath and for calling other
services, [json](json.md) for bodies, [concurrency.md](../concurrency.md) for
why routes are registered inside a function.
