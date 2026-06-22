# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-06-22T14:07:02Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.3`
- Python:    `Python 3.14.5`
- Bun:       `1.3.14`
- Node:      `v26.3.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `dc5a1fd`
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
| bench_arith | 77 | 13 | 14 | 28 | 10 | 62 | 2x | 0.71x | 4.43x |
| bench_closure | 77 | 28 | 29 | 25 | 9 | 62 | 0.86x | 0.31x | 2.14x |
| bench_collatz | 785 | 16 | 16 | 158 | 18 | 65 | 9.88x | 1.13x | 4.06x |
| bench_fib | 180 | 23 | 24 | 47 | 11 | 64 | 1.96x | 0.46x | 2.67x |
| bench_loop | 59 | 5 | 5 | 34 | 9 | 62 | 6.8x | 1.8x | 12.4x |
| bench_nested_loop | 18 | 5 | 5 | 16 | 8 | 62 | 3.2x | 1.6x | 12.4x |
| bench_nested_loop_big | 224 | 6 | 6 | 88 | 10 | 66 | 14.67x | 1.67x | 11x |
| bench_struct_method | 143 | 10 | 10 | 37 | 10 | 63 | 3.7x | 1x | 6.3x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 14 | 14 |
| bench_closure | 29 | 29 |
| bench_collatz | 16 | 17 |
| bench_fib | 24 | 24 |
| bench_loop | 5 | 5 |
| bench_nested_loop | 5 | 5 |
| bench_nested_loop_big | 6 | 7 |
| bench_struct_method | 10 | 11 |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/ToolsForRaven/OxigenLang/benchmark_reports/`.
