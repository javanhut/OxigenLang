# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-07-18T11:55:40Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.3`
- Python:    `Python 3.14.6`
- Bun:       `1.3.14`
- Node:      `v26.5.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `6f54b62`
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
| bench_arith | 113 | 16 | 17 | 40 | 13 | 51 | 2.35x | 0.76x | 3x |
| bench_closure | 106 | 13 | 14 | 33 | 12 | 50 | 2.36x | 0.86x | 3.57x |
| bench_collatz | 1080 | 18 | 19 | 211 | 23 | 55 | 11.11x | 1.21x | 2.89x |
| bench_fib | 248 | 28 | 28 | 63 | 15 | 52 | 2.25x | 0.54x | 1.86x |
| bench_loop | 80 | 5 | 6 | 41 | 12 | 51 | 6.83x | 2x | 8.5x |
| bench_nested_loop | 24 | 5 | 6 | 21 | 11 | 49 | 3.5x | 1.83x | 8.17x |
| bench_nested_loop_big | 308 | 7 | 7 | 114 | 14 | 54 | 16.29x | 2x | 7.71x |
| bench_primes_parallel | 30 | 31 | 31 | 448 | - | - | 14.45x | - | - |
| bench_primes_serial | 11482 | 171 | 170 | 1975 | - | - | 11.62x | - | - |
| bench_struct_method | 203 | 12 | 13 | 46 | 13 | 53 | 3.54x | 1x | 4.08x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 17 | 17 |
| bench_closure | 14 | 15 |
| bench_collatz | 19 | 19 |
| bench_fib | 28 | 28 |
| bench_loop | 6 | 6 |
| bench_nested_loop | 6 | 6 |
| bench_nested_loop_big | 7 | 8 |
| bench_primes_parallel | 31 | 34 |
| bench_primes_serial | 170 | 172 |
| bench_struct_method | 13 | 13 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/ToolsForRaven/OxigenLang/benchmark_reports/`.
