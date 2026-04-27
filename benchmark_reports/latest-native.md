# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-04-27T10:09:34+00:00`
- Host:      `archlinux`
- Kernel:    `Linux 6.19.11-arch1-1 x86_64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.14.4`
- Bun:       `1.3.11`
- Node:      `v25.9.0` (built-in type-stripping)
- Warmups:   `8`
- Runs:      `15`
- Git commit: `f033709`
- Git branch: `oxigen-jit`

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
| bench_arith | 896.5 | 61.9 | 77.3 | 303.2 | 23 | 259.4 | 3.92x | 0.3x | 3.36x |
| bench_closure | 478 | 74.4 | 95.1 | 278.4 | 67.4 | 341.8 | 2.93x | 0.71x | 3.59x |
| bench_collatz | 3444.3 | 157.7 | 124.3 | 342.9 | 157.5 | 369.9 | 2.76x | 1.27x | 2.98x |
| bench_fib | 975.6 | 117.1 | 149.4 | 117.2 | 66.2 | 324.8 | 0.78x | 0.44x | 2.17x |
| bench_loop | 136.9 | 2.4 | 3.1 | 76.2 | 16.5 | 66 | 24.98x | 5.4x | 21.64x |
| bench_nested_loop | 35.8 | 1.9 | 2.5 | 25.6 | 11.9 | 68.8 | 10.27x | 4.76x | 27.55x |
| bench_nested_loop_big | 2112.1 | 4.2 | 4.4 | 292.8 | 21.7 | 76.8 | 66.03x | 4.9x | 17.32x |
| bench_struct_method | 1684.9 | 89.9 | 71.8 | 383.7 | 87.9 | 353.9 | 5.35x | 1.23x | 4.93x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 77.3 | 90 |
| bench_closure | 95.1 | 103.6 |
| bench_collatz | 124.3 | 167.8 |
| bench_fib | 149.4 | 158 |
| bench_loop | 3.1 | 16.1 |
| bench_nested_loop | 2.5 | 12.8 |
| bench_nested_loop_big | 4.4 | 23.5 |
| bench_struct_method | 71.8 | 100.4 |

Per-benchmark JSON (per-round samples + summary stats) in `/home/javanstorm/Development/OxigenLang/benchmark_reports/`.
