# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-04-28T06:30:27+00:00`
- Host:      `archlinux`
- Kernel:    `Linux 6.19.11-arch1-1 x86_64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.14.4`
- Bun:       `1.3.11`
- Node:      `v25.9.0` (built-in type-stripping)
- Warmups:   `8`
- Runs:      `15`
- Git commit: `3fa77fc`
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
| bench_arith | 155.7 | 16.8 | 17.1 | 55.3 | 11.8 | 53.3 | 3.24x | 0.69x | 3.12x |
| bench_closure | 87.2 | 17.6 | 18.5 | 51 | 9.8 | 52.1 | 2.76x | 0.53x | 2.82x |
| bench_collatz | 830.2 | 28.8 | 29.8 | 315.2 | 25.8 | 61.7 | 10.57x | 0.87x | 2.07x |
| bench_fib | 360.4 | 29.1 | 30.1 | 102.4 | 14.5 | 58.2 | 3.4x | 0.48x | 1.93x |
| bench_loop | 132.1 | 2.1 | 2.7 | 73.7 | 10.3 | 53.8 | 27.78x | 3.89x | 20.27x |
| bench_nested_loop | 34.5 | 1.7 | 2.3 | 24.8 | 8.9 | 56.8 | 11.01x | 3.93x | 25.21x |
| bench_nested_loop_big | 522.5 | 4.4 | 4.9 | 253.1 | 13.6 | 59.2 | 51.15x | 2.75x | 11.96x |
| bench_struct_method | 296.3 | 16 | 17.1 | 68 | 11.6 | 56.8 | 3.98x | 0.68x | 3.33x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 17.1 | 17.6 |
| bench_closure | 18.5 | 18.9 |
| bench_collatz | 29.8 | 30.9 |
| bench_fib | 30.1 | 30.9 |
| bench_loop | 2.7 | 2.8 |
| bench_nested_loop | 2.3 | 11.4 |
| bench_nested_loop_big | 4.9 | 5.1 |
| bench_struct_method | 17.1 | 18.3 |

Per-benchmark JSON (per-round samples + summary stats) in `/home/javanstorm/Development/OxigenLang/benchmark_reports/`.
