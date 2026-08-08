# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-08-08T00:58:18Z`
- Host:      `L6VW6YWMJ2`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `Oxigen Version: 0.1.3`
- Python:    `Python 3.14.5` (JIT: not built in)
- Bun:       `1.3.14`
- Node:      `v25.7.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `5310457`
- Git branch: `feature_enhancements_for_0_1_4`

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
| bench_arith | 1414.1 | 142.8 | 34.7 | 514.1 | 40.8 | 109.6 | 14.83x | 1.18x | 3.16x |
| bench_closure | 2037 | 62.4 | 62.8 | 432.7 | 18 | 84.6 | 6.89x | 0.29x | 1.35x |
| bench_collatz | 4116.6 | 53.6 | 53.7 | 866.1 | 81.3 | 188.6 | 16.14x | 1.52x | 3.51x |
| bench_fib | 2167.4 | 198.5 | 51.3 | 608.4 | 42.7 | 126.8 | 11.86x | 0.83x | 2.47x |
| bench_loop | 1223.7 | 13.9 | 15.5 | 467.8 | 23 | 94.9 | 30.25x | 1.48x | 6.13x |
| bench_nested_loop | 552.2 | 8.8 | 10.4 | 223.3 | 15.6 | 79.3 | 21.52x | 1.51x | 7.64x |
| bench_nested_loop_big | 2202.6 | 21.5 | 22.7 | 839.4 | 34 | 116.7 | 37.04x | 1.5x | 5.15x |
| bench_primes_parallel | 32.7 | 33.3 | 33.5 | 369.9 | - | - | 11.04x | - | - |
| bench_primes_serial | 9242.8 | 182.2 | 158.2 | 1749 | - | - | 11.06x | - | - |
| bench_struct_method | 2658.7 | 66.5 | 67.8 | 577.1 | 24 | 119.6 | 8.51x | 0.35x | 1.76x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 34.7 | 35.3 |
| bench_closure | 62.8 | 63.5 |
| bench_collatz | 53.7 | 54.9 |
| bench_fib | 51.3 | 51.5 |
| bench_loop | 15.5 | 16.5 |
| bench_nested_loop | 10.4 | 11 |
| bench_nested_loop_big | 22.7 | 24.4 |
| bench_primes_parallel | 33.5 | 35.7 |
| bench_primes_serial | 158.2 | 215.1 |
| bench_struct_method | 67.8 | 71.4 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/jhutchinson/Development/OxigenLang/benchmark_reports/`.
