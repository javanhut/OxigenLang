# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-04-28T05:12:58+00:00`
- Host:      `archlinux`
- Kernel:    `Linux 6.19.11-arch1-1 x86_64`
- Oxigen:    `oxigen 0.1.2`
- Python:    `Python 3.14.4`
- Bun:       `1.3.11`
- Node:      `v25.9.0` (built-in type-stripping)
- Warmups:   `3`
- Runs:      `5`
- Git commit: `aace1f2`
- Git branch: `jit-improvements`

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
| bench_arith | 154.7 | 16.3 | 16.7 | 55.7 | 12 | 54.8 | 3.34x | 0.72x | 3.29x |
| bench_closure | 92.5 | 18 | 18 | 51.1 | 10 | 58.1 | 2.83x | 0.56x | 3.22x |
| bench_collatz | 847.9 | 28 | 28.4 | 314.4 | 26.5 | 63.9 | 11.06x | 0.93x | 2.25x |
| bench_fib | 1058.4 | 28.6 | 28.1 | 105.7 | 15.1 | 180.2 | 3.77x | 0.54x | 6.42x |
| bench_loop | 128.9 | 2.2 | 2.6 | 75.1 | 10.8 | 59.7 | 28.37x | 4.08x | 22.56x |
| bench_nested_loop | 33.2 | 1.8 | 2.2 | 25 | 8.8 | 57.6 | 11.24x | 3.95x | 25.92x |
| bench_nested_loop_big | 511.9 | 3.4 | 4 | 255 | 13.7 | 64.5 | 63.65x | 3.42x | 16.1x |
| bench_struct_method | 300.5 | 16.3 | 17.6 | 69.1 | 12.1 | 56.9 | 3.94x | 0.69x | 3.24x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_arith | 16.7 | 84.8 |
| bench_closure | 18 | 37 |
| bench_collatz | 28.4 | 29 |
| bench_fib | 28.1 | 142.8 |
| bench_loop | 2.6 | 2.9 |
| bench_nested_loop | 2.2 | 2.3 |
| bench_nested_loop_big | 4 | 4.1 |
| bench_struct_method | 17.6 | 17.9 |

Per-benchmark JSON (per-round samples + summary stats) in `/home/javanstorm/Development/OxigenLang/benchmark_reports/`.
