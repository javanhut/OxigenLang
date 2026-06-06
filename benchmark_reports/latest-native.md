# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-06-06T03:54:00Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.9.6`
- Bun:       `1.3.14`
- Node:      `v26.0.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `c77ce82`
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
| bench_arith | 78 | 10 | 10 | 57 | 9 | 59 | 5.7x | 0.9x | 5.9x |
| bench_closure | 42 | 8 | 8 | 42 | 8 | 59 | 5.25x | 1x | 7.38x |
| bench_collatz | 425 | 10 | 11 | 207 | 18 | 64 | 18.82x | 1.64x | 5.82x |
| bench_fib | 184 | 19 | 19 | 107 | 11 | 58 | 5.63x | 0.58x | 3.05x |
| bench_loop | 58 | 3 | 3 | 49 | 9 | 59 | 16.33x | 3x | 19.67x |
| bench_nested_loop | 16 | 3 | 3 | 23 | 8 | 57 | 7.67x | 2.67x | 19x |
| bench_nested_loop_big | 227 | 3 | 4 | 150 | 10 | 63 | 37.5x | 2.5x | 15.75x |
| bench_struct_method | 148 | 8 | 8 | 67 | 10 | 60 | 8.38x | 1.25x | 7.5x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 10 | 11 |
| bench_closure | 8 | 8 |
| bench_collatz | 11 | 11 |
| bench_fib | 19 | 20 |
| bench_loop | 3 | 3 |
| bench_nested_loop | 3 | 3 |
| bench_nested_loop_big | 4 | 4 |
| bench_struct_method | 8 | 8 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/OxigenLang/benchmark_reports/`.
