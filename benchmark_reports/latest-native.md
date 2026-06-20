# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-06-20T07:12:22Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.14.5`
- Bun:       `1.3.14`
- Node:      `v26.3.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `9dfd845`
- Git branch: `new_lib`

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
| bench_arith | 76 | 10 | 11 | 29 | 10 | 61 | 2.64x | 0.91x | 5.55x |
| bench_closure | 42 | 8 | 9 | 25 | 9 | 62 | 2.78x | 1x | 6.89x |
| bench_collatz | 429 | 12 | 12 | 161 | 18 | 69 | 13.42x | 1.5x | 5.75x |
| bench_fib | 182 | 19 | 20 | 48 | 11 | 62 | 2.4x | 0.55x | 3.1x |
| bench_loop | 58 | 3 | 3 | 34 | 9 | 60 | 11.33x | 3x | 20x |
| bench_nested_loop | 16 | 3 | 3 | 17 | 8 | 62 | 5.67x | 2.67x | 20.67x |
| bench_nested_loop_big | 225 | 4 | 4 | 90 | 10 | 68 | 22.5x | 2.5x | 17x |
| bench_struct_method | 147 | 7 | 8 | 37 | 10 | 61 | 4.63x | 1.25x | 7.63x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 11 | 11 |
| bench_closure | 9 | 9 |
| bench_collatz | 12 | 12 |
| bench_fib | 20 | 20 |
| bench_loop | 3 | 3 |
| bench_nested_loop | 3 | 3 |
| bench_nested_loop_big | 4 | 5 |
| bench_struct_method | 8 | 8 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/ToolsForRaven/OxigenLang/benchmark_reports/`.
