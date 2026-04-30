# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-04-29T12:37:25+00:00`
- Host:      `archlinux`
- Kernel:    `Linux 6.19.11-arch1-1 x86_64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.14.4`
- Bun:       `1.3.11`
- Node:      `v25.9.0` (built-in type-stripping)
- Warmups:   `8`
- Runs:      `15`
- Git commit: `d04afab`
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
| bench_arith | 161.8 | 17.2 | 18 | 56.7 | 18.3 | 65.7 | 3.14x | 1.01x | 3.64x |
| bench_closure | 91.1 | 16.2 | 17.1 | 51.9 | 16.3 | 64.9 | 3.03x | 0.95x | 3.79x |
| bench_collatz | 874.2 | 18.1 | 18.7 | 327.1 | 38 | 76.1 | 17.47x | 2.03x | 4.06x |
| bench_fib | 373.9 | 30.8 | 31.4 | 104.5 | 19.8 | 68.7 | 3.33x | 0.63x | 2.19x |
| bench_loop | 137 | 2.3 | 3.1 | 74.9 | 12.7 | 64.7 | 24.34x | 4.13x | 21.02x |
| bench_nested_loop | 34.8 | 2 | 2.7 | 25.8 | 9.5 | 66.5 | 9.4x | 3.46x | 24.25x |
| bench_nested_loop_big | 599.6 | 4.9 | 5.6 | 262.4 | 23.4 | 73.1 | 46.59x | 4.15x | 12.97x |
| bench_struct_method | 309 | 13.3 | 14.4 | 72 | 14.9 | 72.1 | 5.01x | 1.04x | 5.02x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 18 | 18.4 |
| bench_closure | 17.1 | 20 |
| bench_collatz | 18.7 | 20.7 |
| bench_fib | 31.4 | 33.4 |
| bench_loop | 3.1 | 3.3 |
| bench_nested_loop | 2.7 | 2.9 |
| bench_nested_loop_big | 5.6 | 30 |
| bench_struct_method | 14.4 | 80.5 |

Per-benchmark JSON (per-round samples + summary stats) in `/home/javanstorm/Development/OxigenLang/benchmark_reports/`.
