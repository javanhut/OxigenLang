# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-06-28T03:46:30Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.3`
- Python:    `Python 3.14.5`
- Bun:       `1.3.14`
- Node:      `v26.3.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `72b5fba`
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
| bench_arith | 86 | 14 | 15 | 31 | 10 | 65 | 2.07x | 0.67x | 4.33x |
| bench_closure | 85 | 11 | 12 | 27 | 9 | 66 | 2.25x | 0.75x | 5.5x |
| bench_collatz | 851 | 15 | 16 | 166 | 20 | 71 | 10.38x | 1.25x | 4.44x |
| bench_fib | 195 | 23 | 23 | 51 | 12 | 68 | 2.22x | 0.52x | 2.96x |
| bench_loop | 63 | 5 | 5 | 32 | 9 | 65 | 6.4x | 1.8x | 13x |
| bench_nested_loop | 19 | 5 | 5 | 18 | 9 | 65 | 3.6x | 1.8x | 13x |
| bench_nested_loop_big | 243 | 6 | 7 | 88 | 11 | 70 | 12.57x | 1.57x | 10x |
| bench_primes_parallel | 22 | 24 | 24 | 316 | - | - | 13.17x | - | - |
| bench_primes_serial | 8864 | 130 | 130 | 1392 | - | - | 10.71x | - | - |
| bench_struct_method | 151 | 10 | 11 | 39 | 11 | 68 | 3.55x | 1x | 6.18x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 15 | 15 |
| bench_closure | 12 | 12 |
| bench_collatz | 16 | 16 |
| bench_fib | 23 | 24 |
| bench_loop | 5 | 6 |
| bench_nested_loop | 5 | 5 |
| bench_nested_loop_big | 7 | 7 |
| bench_primes_parallel | 24 | 27 |
| bench_primes_serial | 130 | 132 |
| bench_struct_method | 11 | 11 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/ToolsForRaven/OxigenLang/benchmark_reports/`.
