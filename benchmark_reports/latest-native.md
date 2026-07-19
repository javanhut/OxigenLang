# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-07-19T13:40:28Z`
- Host:      `Javans-MacBook-Pro.local`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `oxigen 0.1.3`
- Python:    `Python 3.14.6`
- Bun:       `1.3.14`
- Node:      `v26.5.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `10`
- Git commit: `a8421ec`
- Git branch: `jit_and_vm_optimizations`

## Min times (ms)

Each cell is the fastest single timed run across `10` rounds
after `3` warmup rounds. Variants are interleaved A/B/A/B
per round so every variant observes the same thermal state, and
any thermal drift across the run affects them equally. Min is
the most reproducible single number on a desktop CPU that may
throttle after sustained full-CPU work. See
`bench_*.native.json` for full per-round samples.

| benchmark | no-jit | default | jit | python | bun (ts) | node (ts) | jit vs py | jit vs bun | jit vs node |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| bench_fib | 162 | 21 | 21 | 48 | 11 | 42 | 2.29x | 0.52x | 2x |
| bench_loop | 59 | 4 | 4 | 32 | 9 | 39 | 8x | 2.25x | 9.75x |
| bench_arith | 69 | 12 | 13 | 29 | 10 | 39 | 2.23x | 0.77x | 3x |
| bench_collatz | 778 | 13 | 14 | 162 | 18 | 44 | 11.57x | 1.29x | 3.14x |
| bench_nested_loop | 17 | 3 | 4 | 15 | 8 | 39 | 3.75x | 2x | 9.75x |
| bench_closure | 82 | 10 | 10 | 25 | 8 | 39 | 2.5x | 0.8x | 3.9x |
| bench_struct_method | 145 | 9 | 10 | 34 | 10 | 40 | 3.4x | 1x | 4x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_fib | 21 | 22 |
| bench_loop | 4 | 4 |
| bench_arith | 13 | 13 |
| bench_collatz | 14 | 14 |
| bench_nested_loop | 4 | 4 |
| bench_closure | 10 | 10 |
| bench_struct_method | 10 | 10 |

## A/B comparison: `/Users/javanhutchinson/Development/ToolsForRaven/OxigenLang/target/release/oxigen` vs `/private/tmp/oxigen-baseline/target/release/oxigen` (--jit)

Both binaries run interleaved A/B/A/B per round so they share
the same thermal/cache state. `B/A < 1.00` means OXIGEN_BIN_B
(B) is faster than OXIGEN_BIN (A); `> 1.00` means slower.

| benchmark | A min | A p50 | B min | B p50 | B/A min | B/A p50 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| bench_fib | 21 | 22 | 22 | 22 | 1.048x | 1x |
| bench_loop | 4 | 4 | 4 | 4 | 1x | 1x |
| bench_arith | 13 | 13 | 13 | 13 | 1x | 1x |
| bench_collatz | 14 | 14 | 14 | 14 | 1x | 1x |
| bench_nested_loop | 4 | 4 | 4 | 4 | 1x | 1x |
| bench_closure | 10 | 10 | 10 | 10 | 1x | 1x |
| bench_struct_method | 10 | 10 | 9 | 9.5 | 0.9x | 0.95x |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/javanhutchinson/Development/ToolsForRaven/OxigenLang/benchmark_reports/`.
