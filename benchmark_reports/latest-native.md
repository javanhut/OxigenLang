# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-08-08T18:20:53Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `Oxigen Version: 0.1.4`
- Python:    `Python 3.14.6` (JIT: enabled)
- Bun:       `1.3.14`
- Node:      `v26.5.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `8e50055`
- Git branch: `main`

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
| bench_arith | 1151 | 128 | 31 | 317 | 36 | 73 | 10.23x | 1.16x | 2.35x |
| bench_closure | 1693 | 52 | 53 | 350 | 15 | 54 | 6.6x | 0.28x | 1.02x |
| bench_collatz | 3409 | 41 | 41 | 679 | 72 | 145 | 16.56x | 1.76x | 3.54x |
| bench_fib | 1787 | 185 | 47 | 420 | 38 | 82 | 8.94x | 0.81x | 1.74x |
| bench_loop | 1127 | 8 | 9 | 423 | 19 | 63 | 47x | 2.11x | 7x |
| bench_nested_loop | 511 | 5 | 6 | 204 | 13 | 50 | 34x | 2.17x | 8.33x |
| bench_nested_loop_big | 2046 | 12 | 13 | 785 | 28 | 83 | 60.38x | 2.15x | 6.38x |
| bench_primes_parallel | 20 | 21 | 21 | 401 | - | - | 19.1x | - | - |
| bench_primes_serial | 8141 | 112 | 112 | 2376 | - | - | 21.21x | - | - |
| bench_struct_method | 2297 | 55 | 55 | 401 | 19 | 80 | 7.29x | 0.35x | 1.45x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 31 | 32 |
| bench_closure | 53 | 54 |
| bench_collatz | 41 | 42 |
| bench_fib | 47 | 47 |
| bench_loop | 9 | 9 |
| bench_nested_loop | 6 | 7 |
| bench_nested_loop_big | 13 | 14 |
| bench_primes_parallel | 21 | 22 |
| bench_primes_serial | 112 | 113 |
| bench_struct_method | 55 | 56 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/ToolsForRaven/OxigenLang/benchmark_reports/`.
