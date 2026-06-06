# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-06-06T04:00:54Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.14.5`
- Bun:       `1.3.14`
- Node:      `v26.0.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `43ad199`
- Git branch: `jit-improvements`

## Min times (ms)

Each cell is the fastest single timed run across `5` rounds
after `3` warmup rounds. Variants are interleaved A/B/A/B
per round so every variant observes the same thermal state, and
any thermal drift across the run affects them equally. Min is
the most reproducible single number on a desktop CPU that may
throttle after sustained full-CPU work. See
`bench_*.native.json` for full per-round samples.

| benchmark | no-jit | default | jit | python | bun (ts) | node (ts) | jit vs py | jit vs bun | jit vs node |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| bench_arith | 78 | 10 | 10 | 29 | 10 | 61 | 2.9x | 1x | 6.1x |
| bench_closure | 42 | 8 | 8 | 25 | 8 | 60 | 3.13x | 1x | 7.5x |
| bench_collatz | 423 | 10 | 11 | 161 | 19 | 65 | 14.64x | 1.73x | 5.91x |
| bench_fib | 196 | 20 | 21 | 51 | 12 | 66 | 2.43x | 0.57x | 3.14x |
| bench_loop | 61 | 3 | 4 | 32 | 10 | 64 | 8x | 2.5x | 16x |
| bench_nested_loop | 17 | 3 | 4 | 19 | 9 | 66 | 4.75x | 2.25x | 16.5x |
| bench_nested_loop_big | 229 | 3 | 4 | 86 | 10 | 63 | 21.5x | 2.5x | 15.75x |
| bench_struct_method | 148 | 8 | 8 | 37 | 10 | 61 | 4.63x | 1.25x | 7.63x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 10 | 10 |
| bench_closure | 8 | 8 |
| bench_collatz | 11 | 11 |
| bench_fib | 21 | 22 |
| bench_loop | 4 | 4 |
| bench_nested_loop | 4 | 4 |
| bench_nested_loop_big | 4 | 4 |
| bench_struct_method | 8 | 8 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/OxigenLang/benchmark_reports/`.
