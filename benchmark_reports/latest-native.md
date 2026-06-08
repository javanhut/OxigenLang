# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-06-08T13:41:22Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.14.5`
- Bun:       `1.3.14`
- Node:      `v26.0.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `0842ce2`
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
| bench_arith | 76 | 10 | 10 | 28 | 9 | 59 | 2.8x | 0.9x | 5.9x |
| bench_closure | 42 | 8 | 8 | 25 | 8 | 57 | 3.13x | 1x | 7.13x |
| bench_collatz | 422 | 11 | 12 | 156 | 18 | 61 | 13x | 1.5x | 5.08x |
| bench_fib | 184 | 19 | 20 | 47 | 11 | 60 | 2.35x | 0.55x | 3x |
| bench_loop | 57 | 3 | 3 | 33 | 8 | 58 | 11x | 2.67x | 19.33x |
| bench_nested_loop | 16 | 2 | 3 | 16 | 8 | 55 | 5.33x | 2.67x | 18.33x |
| bench_nested_loop_big | 223 | 3 | 4 | 87 | 10 | 62 | 21.75x | 2.5x | 15.5x |
| bench_struct_method | 147 | 7 | 8 | 35 | 10 | 57 | 4.38x | 1.25x | 7.13x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 10 | 10 |
| bench_closure | 8 | 9 |
| bench_collatz | 12 | 12 |
| bench_fib | 20 | 20 |
| bench_loop | 3 | 3 |
| bench_nested_loop | 3 | 3 |
| bench_nested_loop_big | 4 | 4 |
| bench_struct_method | 8 | 8 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/OxigenLang/benchmark_reports/`.
