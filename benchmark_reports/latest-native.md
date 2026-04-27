# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-04-27T09:50:56+00:00`
- Host:      `archlinux`
- Kernel:    `Linux 6.19.11-arch1-1 x86_64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.14.4`
- Bun:       `1.3.11`
- Node:      `v25.9.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `4d890e2`
- Git branch: `oxigen-jit`

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
| bench_arith | 160.3 | 15.6 | 15.7 | 55.2 | 11.9 | 58.8 | 3.52x | 0.76x | 3.75x |
| bench_closure | 90.4 | 17.3 | 17.7 | 50.9 | 10.2 | 57.1 | 2.87x | 0.57x | 3.22x |
| bench_collatz | 852.9 | 28.4 | 29.7 | 317.3 | 26.6 | 62.7 | 10.69x | 0.89x | 2.11x |
| bench_fib | 372.8 | 27.7 | 28 | 103.9 | 14.5 | 59.8 | 3.7x | 0.52x | 2.13x |
| bench_loop | 128.5 | 11 | 13.5 | 74.4 | 48.1 | 210.2 | 5.51x | 3.56x | 15.57x |
| bench_nested_loop | 34.8 | 1.6 | 2.2 | 24.8 | 8.7 | 55.7 | 11.1x | 3.89x | 24.97x |
| bench_nested_loop_big | 511 | 3.4 | 3.9 | 253 | 13.6 | 59.4 | 64.37x | 3.46x | 15.12x |
| bench_struct_method | 301.8 | 17 | 17.7 | 69.2 | 11.8 | 53.1 | 3.92x | 0.67x | 3.01x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 15.7 | 16.7 |
| bench_closure | 17.7 | 18 |
| bench_collatz | 29.7 | 30 |
| bench_fib | 28 | 29.6 |
| bench_loop | 13.5 | 13.6 |
| bench_nested_loop | 2.2 | 2.4 |
| bench_nested_loop_big | 3.9 | 4.1 |
| bench_struct_method | 17.7 | 19.1 |

Per-benchmark JSON (per-round samples + summary stats) in `/home/javanstorm/Development/OxigenLang/benchmark_reports/`.
