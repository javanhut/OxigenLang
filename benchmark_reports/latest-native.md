# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-07-19T19:30:09Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.3`
- Python:    `Python 3.14.6`
- Bun:       `1.3.14`
- Node:      `v26.5.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `f98a304`
- Git branch: `jit_and_vm_optimizations`

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
| bench_arith | 66 | 12 | 14 | 28 | 9 | 39 | 2x | 0.64x | 2.79x |
| bench_closure | 78 | 10 | 13 | 24 | 8 | 38 | 1.85x | 0.62x | 2.92x |
| bench_collatz | 731 | 13 | 15 | 154 | 18 | 43 | 10.27x | 1.2x | 2.87x |
| bench_fib | 161 | 20 | 24 | 46 | 11 | 41 | 1.92x | 0.46x | 1.71x |
| bench_loop | 56 | 4 | 5 | 29 | 9 | 39 | 5.8x | 1.8x | 7.8x |
| bench_nested_loop | 17 | 4 | 5 | 15 | 8 | 38 | 3x | 1.6x | 7.6x |
| bench_nested_loop_big | 217 | 6 | 6 | 82 | 11 | 43 | 13.67x | 1.83x | 7.17x |
| bench_primes_parallel | 21 | 23 | 23 | 288 | - | - | 12.52x | - | - |
| bench_primes_serial | 7918 | 124 | 128 | 1394 | - | - | 10.89x | - | - |
| bench_struct_method | 143 | 10 | 11 | 34 | 10 | 40 | 3.09x | 0.91x | 3.64x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 14 | 16 |
| bench_closure | 13 | 13 |
| bench_collatz | 15 | 16 |
| bench_fib | 24 | 25 |
| bench_loop | 5 | 6 |
| bench_nested_loop | 5 | 5 |
| bench_nested_loop_big | 6 | 6 |
| bench_primes_parallel | 23 | 26 |
| bench_primes_serial | 128 | 128 |
| bench_struct_method | 11 | 12 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/ToolsForRaven/OxigenLang/benchmark_reports/`.
