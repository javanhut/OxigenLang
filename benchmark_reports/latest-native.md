# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-06-10T14:49:17Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.14.5`
- Bun:       `1.3.14`
- Node:      `v26.0.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `f37c871`
- Git branch: `jit_optimizations`

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
| bench_arith | 78 | 10 | 10 | 29 | 9 | 58 | 2.9x | 0.9x | 5.8x |
| bench_closure | 42 | 8 | 9 | 25 | 8 | 59 | 2.78x | 0.89x | 6.56x |
| bench_collatz | 427 | 11 | 12 | 158 | 18 | 64 | 13.17x | 1.5x | 5.33x |
| bench_fib | 184 | 17 | 18 | 47 | 11 | 61 | 2.61x | 0.61x | 3.39x |
| bench_loop | 57 | 3 | 3 | 30 | 9 | 57 | 10x | 3x | 19x |
| bench_nested_loop | 16 | 3 | 3 | 16 | 8 | 58 | 5.33x | 2.67x | 19.33x |
| bench_nested_loop_big | 221 | 3 | 4 | 88 | 10 | 63 | 22x | 2.5x | 15.75x |
| bench_struct_method | 150 | 7 | 8 | 35 | 10 | 60 | 4.38x | 1.25x | 7.5x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 10 | 10 |
| bench_closure | 9 | 9 |
| bench_collatz | 12 | 12 |
| bench_fib | 18 | 18 |
| bench_loop | 3 | 3 |
| bench_nested_loop | 3 | 3 |
| bench_nested_loop_big | 4 | 4 |
| bench_struct_method | 8 | 8 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/ToolsForRaven/OxigenLang/benchmark_reports/`.
