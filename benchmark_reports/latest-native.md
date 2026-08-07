# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-08-07T13:51:40Z`
- Host:      `L6VW6YWMJ2`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `Oxigen Version: 0.1.3`
- Python:    `Python 3.14.5` (JIT: not built in)
- Bun:       `1.3.14`
- Node:      `v25.7.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `ad55942`
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
| bench_arith | 77.1 | 13.8 | 14.9 | 43 | 11.6 | 68.2 | 2.89x | 0.78x | 4.58x |
| bench_closure | 83.9 | 11.2 | 12.6 | 33.4 | 10.8 | 77.5 | 2.66x | 0.86x | 6.16x |
| bench_collatz | 844.1 | 15.6 | 16.6 | 198.7 | 20.7 | 81.3 | 11.99x | 1.25x | 4.91x |
| bench_fib | 184.6 | 24.7 | 26.3 | 69.7 | 12.8 | 68.8 | 2.65x | 0.49x | 2.61x |
| bench_loop | 61.3 | 5.1 | 7.3 | 39.9 | 11.2 | 65.6 | 5.45x | 1.53x | 8.96x |
| bench_nested_loop | 18.6 | 4.5 | 5.7 | 24.5 | 9.8 | 64.7 | 4.26x | 1.7x | 11.26x |
| bench_nested_loop_big | 239.1 | 6.6 | 7.7 | 107.2 | 12.7 | 69.8 | 13.97x | 1.65x | 9.1x |
| bench_primes_parallel | 30.2 | 31.3 | 34.3 | 367 | - | - | 10.7x | - | - |
| bench_primes_serial | 9076.7 | 175.3 | 176.1 | 1738.9 | - | - | 9.87x | - | - |
| bench_struct_method | 170.4 | 10.9 | 12.6 | 50.1 | 11.7 | 70.1 | 3.98x | 0.93x | 5.56x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 14.9 | 15.7 |
| bench_closure | 12.6 | 12.9 |
| bench_collatz | 16.6 | 19.1 |
| bench_fib | 26.3 | 27.2 |
| bench_loop | 7.3 | 8.4 |
| bench_nested_loop | 5.7 | 6.2 |
| bench_nested_loop_big | 7.7 | 8.7 |
| bench_primes_parallel | 34.3 | 34.7 |
| bench_primes_serial | 176.1 | 178.2 |
| bench_struct_method | 12.6 | 12.8 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/jhutchinson/Development/OxigenLang/benchmark_reports/`.
