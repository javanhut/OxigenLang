# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-04-27T12:16:59+00:00`
- Host:      `archlinux`
- Kernel:    `Linux 6.19.11-arch1-1 x86_64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.14.4`
- Bun:       `1.3.11`
- Node:      `v25.9.0` (built-in type-stripping)
- Warmups:   `8`
- Runs:      `15`
- Git commit: `79439db`
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
| bench_arith | 153.5 | 15.8 | 15.6 | 55.3 | 11.6 | 53.6 | 3.54x | 0.75x | 3.44x |
| bench_closure | 88.3 | 17.2 | 17.8 | 51 | 9.6 | 55.2 | 2.87x | 0.54x | 3.1x |
| bench_collatz | 853.3 | 27.4 | 28.1 | 317.4 | 25.9 | 63.7 | 11.3x | 0.92x | 2.27x |
| bench_fib | 359.7 | 26.7 | 27.1 | 102.1 | 14.4 | 59.5 | 3.77x | 0.53x | 2.2x |
| bench_loop | 127.9 | 2 | 2.7 | 73.8 | 10.6 | 52 | 27.7x | 3.99x | 19.54x |
| bench_nested_loop | 33.5 | 1.6 | 2.2 | 24.7 | 8.7 | 55 | 11.22x | 3.95x | 24.96x |
| bench_nested_loop_big | 503.7 | 3.4 | 3.9 | 252.2 | 13.8 | 58.6 | 64.03x | 3.51x | 14.86x |
| bench_struct_method | 303.7 | 16.7 | 17.3 | 67.9 | 11.6 | 58.4 | 3.93x | 0.67x | 3.38x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 15.6 | 16.1 |
| bench_closure | 17.8 | 18.4 |
| bench_collatz | 28.1 | 28.5 |
| bench_fib | 27.1 | 27.9 |
| bench_loop | 2.7 | 2.8 |
| bench_nested_loop | 2.2 | 2.3 |
| bench_nested_loop_big | 3.9 | 4.1 |
| bench_struct_method | 17.3 | 18.1 |

Per-benchmark JSON (per-round samples + summary stats) in `/home/javanstorm/Development/OxigenLang/benchmark_reports/`.
