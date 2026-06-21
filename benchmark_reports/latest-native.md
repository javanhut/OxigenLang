# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-06-21T15:18:41Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.14.5`
- Bun:       `1.3.14`
- Node:      `v26.3.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `79443cc`
- Git branch: `vm_parity_and_bug_fixes`

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
| bench_arith | 75 | 13 | 13 | 29 | 9 | 61 | 2.23x | 0.69x | 4.69x |
| bench_closure | 82 | 28 | 28 | 25 | 8 | 62 | 0.89x | 0.29x | 2.21x |
| bench_collatz | 790 | 15 | 16 | 158 | 18 | 67 | 9.88x | 1.13x | 4.19x |
| bench_fib | 177 | 23 | 24 | 48 | 11 | 62 | 2x | 0.46x | 2.58x |
| bench_loop | 58 | 4 | 5 | 30 | 8 | 62 | 6x | 1.6x | 12.4x |
| bench_nested_loop | 18 | 4 | 5 | 16 | 8 | 61 | 3.2x | 1.6x | 12.2x |
| bench_nested_loop_big | 223 | 5 | 6 | 91 | 10 | 66 | 15.17x | 1.67x | 11x |
| bench_struct_method | 175 | 9 | 10 | 36 | 10 | 61 | 3.6x | 1x | 6.1x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 13 | 14 |
| bench_closure | 28 | 29 |
| bench_collatz | 16 | 16 |
| bench_fib | 24 | 24 |
| bench_loop | 5 | 5 |
| bench_nested_loop | 5 | 5 |
| bench_nested_loop_big | 6 | 7 |
| bench_struct_method | 10 | 11 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/ToolsForRaven/OxigenLang/benchmark_reports/`.
