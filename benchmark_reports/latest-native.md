# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-04-27T13:44:01+00:00`
- Host:      `archlinux`
- Kernel:    `Linux 6.19.11-arch1-1 x86_64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.14.4`
- Bun:       `1.3.11`
- Node:      `v25.9.0` (built-in type-stripping)
- Warmups:   `8`
- Runs:      `15`
- Git commit: `45b205f`
- Git branch: `jit-improvements`

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
| bench_arith | 153.2 | 15 | 15.4 | 55.4 | 11.6 | 54.2 | 3.59x | 0.75x | 3.51x |
| bench_closure | 89.4 | 16.8 | 17.6 | 51.5 | 9.5 | 54.3 | 2.93x | 0.54x | 3.09x |
| bench_collatz | 872.4 | 27.9 | 28.4 | 314.9 | 25.6 | 64.3 | 11.08x | 0.9x | 2.26x |
| bench_fib | 360.7 | 26.7 | 26.7 | 103.2 | 14.4 | 57.9 | 3.87x | 0.54x | 2.17x |
| bench_loop | 127.2 | 2.1 | 2.6 | 72.7 | 10.2 | 53.5 | 28.19x | 3.95x | 20.74x |
| bench_nested_loop | 35.1 | 1.6 | 2.1 | 24.7 | 8.6 | 52.3 | 11.58x | 4.03x | 24.52x |
| bench_nested_loop_big | 512.6 | 3.4 | 4 | 255 | 13.4 | 60.6 | 64.47x | 3.38x | 15.33x |
| bench_struct_method | 301.8 | 16.3 | 17.3 | 68.5 | 11.5 | 56 | 3.97x | 0.67x | 3.25x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 15.4 | 16.2 |
| bench_closure | 17.6 | 18.7 |
| bench_collatz | 28.4 | 29.1 |
| bench_fib | 26.7 | 27.5 |
| bench_loop | 2.6 | 2.7 |
| bench_nested_loop | 2.1 | 2.2 |
| bench_nested_loop_big | 4 | 4.1 |
| bench_struct_method | 17.3 | 17.6 |

Per-benchmark JSON (per-round samples + summary stats) in `/home/javanstorm/Development/OxigenLang/benchmark_reports/`.
