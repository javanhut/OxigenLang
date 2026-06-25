# OxigenLang Concurrency

OxigenLang runs parallel work on **real OS threads across all your cores** — not single-threaded async. The model is **share-nothing**: each task runs on its own VM with its own heap, and data crosses between threads by ownership transfer, not shared references. There are no locks, no mutexes, and no data races by construction.

The surface is two verbs — **`diverge`** to split work off the current flow, and **`converge`** to bring a result back.

## `diverge` / `converge`

### One task off to the side

`diverge { ... }` runs a block on a worker thread and immediately returns a **task** handle. Your code keeps going. `converge` waits for the task and gives you its value.

```oxi
work := diverge { expensive() }   // starts running in the background
// ... do other things here while it runs ...
result := converge work           // wait for it, get the value
```

`converge` is idempotent — joining the same task twice returns the same value (it is remembered after the first join).

### Fan out over a collection

`diverge each X in XS { ... }` runs the block for every item **in parallel** and gathers the results into an array, in input order.

```oxi
nums := [1, 2, 3, 4, 5, 6]

squares := diverge each n in nums {
    n * n
}
// squares == [1, 4, 9, 16, 25, 36]
```

Every iteration is in flight at once, so the wall-clock time is roughly one item's work, not the sum. This is the idiomatic way to parallelize a loop.

## Async API calls

Network calls in the `net` library are **blocking** — `net.get(url)` returns once the response arrives. You make them asynchronous by running them with `diverge`: a blocked worker stalls one thread, not your program. There is no `async`/`await` and no function coloring.

```oxi
introduce net

urls := [
    "https://api.example.com/users/1",
    "https://api.example.com/users/2",
    "https://api.example.com/users/3",
]

// all three requests run at the same time
responses := diverge each url in urls {
    net.get(url)
}

each r in responses {
    println(r["status"])
}
```

Three sequential calls would take three round-trips; this takes one. The same pattern works for `net.post`, `net.put`, sockets, file I/O, or any blocking call.

## Timeouts

Add `within <ms>` to a `converge` to bound the wait. On timeout it returns an **error value** you can test with `is_error`; the task itself keeps running.

```oxi
work := diverge { slow_job() }
result := converge work within 500   // wait at most 500ms

answer := option {
    is_error(result) -> "still working, gave up waiting",
    result
}
```

## Cancellation

`cancel(task)` asks a task to stop. Cancellation is cooperative — the task stops at its next function call. Joining a cancelled task returns an error value (test it with `is_error`).

```oxi
work := diverge { long_running() }
cancel(work)

result := converge work
done := option {
    is_error(result) -> "was cancelled",
    result
}
```

## Joining a batch

`converge` also accepts a **list** of tasks and joins them all, in order — no import needed. Use it to start several *different* tasks and collect them together:

```oxi
user   := diverge { get_user(42) }
orders := diverge { get_orders() }

results := converge [user, orders]   // [userResult, ordersResult]
```

For fanning out the *same* work over a collection, use `diverge each`.

## What can cross threads

When a value is passed into a task or returned from one, it is **copied** across the thread boundary (the original side keeps its own copy). These types can cross:

- Primitives: integers, floats, booleans, `None`
- Strings
- Arrays, tuples, maps, sets (including nested)
- Structs and enums
- Closures, including captured variables
- Open TCP/UDP sockets (so a server can `diverge` each connection to a handler)

## Limitations

The model is built for parallel compute and I/O fan-out into the hundreds. A few current edges to know:

- **One OS thread per in-flight task.** Excellent to a few hundred concurrent tasks; there is no event-loop reactor yet, so this is not tuned for tens of thousands of idle sockets.
- **Workers always use the default tiered JIT.** They ignore the main process's `--jit` / `--no-jit` flags; heavy per-task loops still JIT on their own.
- **Auto-join only triggers on `+` today.** A task used in `a + b` resolves automatically, but other operators (`*`, `==`), indexing, and `println` still need an explicit `converge`.
- **Structured `converge { ... }` blocks are not implemented yet** — use `converge <task>` or `diverge each`.
- **Keep tasks as leaves.** Deeply nested diverge-and-converge inside a task is not the tested path; have a task do work and return, rather than orchestrate more tasks.

## Quick reference

| Form | Meaning |
| --- | --- |
| `diverge { B }` | run `B` on a worker, return a task |
| `diverge each X in XS { B }` | run `B` for each item in parallel, gather results |
| `converge T` | wait for task `T`, return its value |
| `converge [T1, T2, …]` | wait for a list of tasks, return their values in order |
| `converge T within ms` | wait up to `ms`; error value on timeout (test with `is_error`) |
| `cancel(T)` | ask task `T` to stop (cooperative) |
