# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-06-25T03:25:04Z`
- Host:      `L6VW6YWMJ2`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.3`
- Python:    `Python 3.14.5`
- Bun:       `1.3.13`
- Node:      `v25.7.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `268d925`
- Git branch: `threading`

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
| bench_arith | 89.5 | 14.8 | 15.7 | 44.6 | 11.3 | 67.1 | 2.85x | 0.72x | 4.28x |
| bench_closure | 93.2 | 33.7 | 33.8 | 34.9 | 10 | 67.3 | 1.03x | 0.29x | 1.99x |
| bench_collatz | 942.8 | 18.2 | 20 | 194.5 | 20.2 | 73.6 | 9.7x | 1.01x | 3.67x |
| bench_fib | 208 | 27.1 | 27.7 | 70.8 | 12 | 70 | 2.55x | 0.43x | 2.53x |
| bench_loop | 66.4 | 5.5 | 5.8 | 39.5 | 10.6 | 66.3 | 6.87x | 1.84x | 11.53x |
| bench_nested_loop | 19.5 | 5.1 | 5.5 | 22.9 | 9.3 | 74.6 | 4.16x | 1.7x | 13.57x |
| bench_nested_loop_big | 243.7 | 6.9 | 7.6 | 110 | 12.6 | 82.2 | 14.49x | 1.66x | 10.83x |
| bench_primes_parallel | 31 | 29.5 | 32.3 | 367 | - | - | 11.37x | - | - |
| bench_primes_serial | 10103.5 | 184.9 | 171.3 | 1739.8 | - | - | 10.16x | - | - |
| bench_struct_method | 178.3 | 11.5 | 11.9 | 49.4 | 10.9 | 69.9 | 4.15x | 0.91x | 5.88x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 15.7 | 17.7 |
| bench_closure | 33.8 | 34.9 |
| bench_collatz | 20 | 21.5 |
| bench_fib | 27.7 | 28.4 |
| bench_loop | 5.8 | 7.6 |
| bench_nested_loop | 5.5 | 6.4 |
| bench_nested_loop_big | 7.6 | 7.9 |
| bench_primes_parallel | 32.3 | 33.8 |
| bench_primes_serial | 171.3 | 179.7 |
| bench_struct_method | 11.9 | 12.9 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/jhutchinson/Development/OxigenLang/benchmark_reports/`.
