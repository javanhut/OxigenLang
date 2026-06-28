# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-06-28T02:06:17Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.3`
- Python:    `Python 3.14.5`
- Bun:       `1.3.14`
- Node:      `v26.3.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `d374bf7`
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
| bench_arith | 88 | 14 | 15 | 31 | 10 | 66 | 2.07x | 0.67x | 4.4x |
| bench_closure | 85 | 29 | 30 | 27 | 9 | 66 | 0.9x | 0.3x | 2.2x |
| bench_collatz | 839 | 17 | 17 | 168 | 20 | 72 | 9.88x | 1.18x | 4.24x |
| bench_fib | 199 | 25 | 25 | 52 | 12 | 69 | 2.08x | 0.48x | 2.76x |
| bench_loop | 65 | 6 | 6 | 33 | 9 | 68 | 5.5x | 1.5x | 11.33x |
| bench_nested_loop | 19 | 5 | 5 | 18 | 9 | 65 | 3.6x | 1.8x | 13x |
| bench_nested_loop_big | 243 | 6 | 6 | 92 | 11 | 70 | 15.33x | 1.83x | 11.67x |
| bench_primes_parallel | 26 | 26 | 27 | 329 | - | - | 12.19x | - | - |
| bench_primes_serial | 9087 | 133 | 134 | 1491 | - | - | 11.13x | - | - |
| bench_struct_method | 155 | 11 | 11 | 39 | 11 | 69 | 3.55x | 1x | 6.27x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 15 | 15 |
| bench_closure | 30 | 30 |
| bench_collatz | 17 | 18 |
| bench_fib | 25 | 26 |
| bench_loop | 6 | 6 |
| bench_nested_loop | 5 | 6 |
| bench_nested_loop_big | 6 | 7 |
| bench_primes_parallel | 27 | 27 |
| bench_primes_serial | 134 | 135 |
| bench_struct_method | 11 | 12 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/ToolsForRaven/OxigenLang/benchmark_reports/`.
