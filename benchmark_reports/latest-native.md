# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-04-29T08:02:20+00:00`
- Host:      `archlinux`
- Kernel:    `Linux 6.19.11-arch1-1 x86_64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.14.4`
- Bun:       `1.3.11`
- Node:      `v25.9.0` (built-in type-stripping)
- Warmups:   `8`
- Runs:      `15`
- Git commit: `ce45208`
- Git branch: `jit_performance_improvements`

## Min times (ms)

Each cell is the fastest single timed run across `15` rounds
after `8` warmup rounds. Variants are interleaved A/B/A/B
per round so every variant observes the same thermal state, and
any thermal drift across the run affects them equally. Min is
the most reproducible single number on a desktop CPU that may
throttle after sustained full-CPU work. See
`bench_*.native.json` for full per-round samples.

| benchmark | no-jit | default | jit | python | bun (ts) | node (ts) | jit vs py | jit vs bun | jit vs node |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| bench_arith | 159.4 | 17.5 | 18.1 | 56.6 | 17.6 | 67.1 | 3.13x | 0.97x | 3.71x |
| bench_closure | 88.2 | 16 | 17 | 51.6 | 14.9 | 62.9 | 3.04x | 0.87x | 3.7x |
| bench_collatz | 845.4 | 17.7 | 18.7 | 319.9 | 36.2 | 73.8 | 17.11x | 1.93x | 3.95x |
| bench_fib | 365 | 30.5 | 31.3 | 102.6 | 20.3 | 68.7 | 3.28x | 0.65x | 2.2x |
| bench_loop | 133.7 | 2.3 | 3.2 | 74.3 | 12.9 | 64.4 | 23.5x | 4.08x | 20.39x |
| bench_nested_loop | 34.2 | 2 | 2.7 | 24.9 | 9.6 | 64.8 | 9.19x | 3.55x | 23.94x |
| bench_nested_loop_big | 531.9 | 4.7 | 5.4 | 255.2 | 24.7 | 69.3 | 47.22x | 4.57x | 12.82x |
| bench_struct_method | 1431.5 | 22.8 | 15.8 | 359.9 | 42.2 | 284.5 | 22.79x | 2.67x | 18.02x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 18.1 | 98.4 |
| bench_closure | 17 | 17.4 |
| bench_collatz | 18.7 | 19.2 |
| bench_fib | 31.3 | 165.1 |
| bench_loop | 3.2 | 3.3 |
| bench_nested_loop | 2.7 | 13.6 |
| bench_nested_loop_big | 5.4 | 6.3 |
| bench_struct_method | 15.8 | 75.4 |

Per-benchmark JSON (per-round samples + summary stats) in `/home/javanstorm/Development/OxigenLang/benchmark_reports/`.
