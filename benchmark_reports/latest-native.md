# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-07-11T21:39:20Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.3`
- Python:    `Python 3.14.6`
- Bun:       `1.3.14`
- Node:      `v26.5.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `eb102eb`
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
| bench_arith | 82 | 13 | 13 | 29 | 10 | 40 | 2.23x | 0.77x | 3.08x |
| bench_closure | 83 | 11 | 11 | 26 | 9 | 40 | 2.36x | 0.82x | 3.64x |
| bench_collatz | 807 | 14 | 15 | 161 | 18 | 45 | 10.73x | 1.2x | 3x |
| bench_fib | 189 | 22 | 22 | 48 | 11 | 42 | 2.18x | 0.5x | 1.91x |
| bench_loop | 61 | 5 | 5 | 32 | 9 | 40 | 6.4x | 1.8x | 8x |
| bench_nested_loop | 18 | 5 | 5 | 16 | 8 | 40 | 3.2x | 1.6x | 8x |
| bench_nested_loop_big | 239 | 6 | 6 | 87 | 10 | 45 | 14.5x | 1.67x | 7.5x |
| bench_primes_parallel | 21 | 22 | 23 | 300 | - | - | 13.04x | - | - |
| bench_primes_serial | 8805 | 128 | 129 | 1415 | - | - | 10.97x | - | - |
| bench_struct_method | 153 | 10 | 11 | 35 | 10 | 42 | 3.18x | 0.91x | 3.82x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 13 | 14 |
| bench_closure | 11 | 11 |
| bench_collatz | 15 | 15 |
| bench_fib | 22 | 23 |
| bench_loop | 5 | 5 |
| bench_nested_loop | 5 | 5 |
| bench_nested_loop_big | 6 | 7 |
| bench_primes_parallel | 23 | 25 |
| bench_primes_serial | 129 | 130 |
| bench_struct_method | 11 | 11 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/ToolsForRaven/OxigenLang/benchmark_reports/`.
