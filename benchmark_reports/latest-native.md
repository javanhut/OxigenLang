# Oxigen vs Python — native harness (interleaved A/B)

- Generated: `2026-08-07T16:21:54Z`
- Host:      `L6VW6YWMJ2`
- Kernel:    `Darwin 25.5.0 arm64`
- Oxigen:    `Oxigen Version: 0.1.3`
- Python:    `Python 3.14.5` (JIT: not built in)
- Bun:       `1.3.14`
- Node:      `v25.7.0` (built-in type-stripping)
- Warmups:   `6`
- Runs:      `25`
- Git commit: `008ba29`
- Git branch: `feature_enhancements_for_0_1_4`

## Min times (ms)

Each cell is the fastest single timed run across `25` rounds
after `6` warmup rounds. Variants are interleaved A/B/A/B
per round so every variant observes the same thermal state, and
any thermal drift across the run affects them equally. Min is
the most reproducible single number on a desktop CPU that may
throttle after sustained full-CPU work. See
`bench_*.native.json` for full per-round samples.

| benchmark | no-jit | default | jit | python | bun (ts) | node (ts) | jit vs py | jit vs bun | jit vs node |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| bench_loop | 1210.8 | 13.5 | 14.7 | 460.6 | 21.9 | 94.7 | 31.39x | 1.49x | 6.45x |
| bench_nested_loop | 556.5 | 8.4 | 9.7 | 222.6 | 15 | 79.3 | 22.96x | 1.55x | 8.18x |

## JIT min / p50 (ms)

| benchmark | jit min | jit p50 |
| --- | ---: | ---: |
| bench_loop | 14.7 | 16.6 |
| bench_nested_loop | 9.7 | 10.7 |

## A/B comparison: `/private/tmp/claude-502/-Users-jhutchinson-Development-OxigenLang/5aa73862-e3f8-4386-ab84-5ed8fc41ed51/scratchpad/target/release/oxigen` vs `/private/tmp/claude-502/-Users-jhutchinson-Development-OxigenLang/5aa73862-e3f8-4386-ab84-5ed8fc41ed51/scratchpad/oxigen_orig_samedir` (--jit)

Both binaries run interleaved A/B/A/B per round so they share
the same thermal/cache state. `B/A < 1.00` means OXIGEN_BIN_B
(B) is faster than OXIGEN_BIN (A); `> 1.00` means slower.

| benchmark | A min | A p50 | B min | B p50 | B/A min | B/A p50 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| bench_loop | 14.7 | 16.6 | 14.1 | 15.6 | 0.96x | 0.94x |
| bench_nested_loop | 9.7 | 10.7 | 9.2 | 10.2 | 0.949x | 0.948x |

Per-benchmark JSON (per-round samples + summary stats) in `/Users/jhutchinson/Development/OxigenLang/benchmark_reports/`.
