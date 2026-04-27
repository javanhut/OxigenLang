# Benchmark Reports

This directory stores benchmark output from `scripts/bench.py`.

The benchmark suite only compares equivalent paired tests:

- `example/bench_*.oxi`
- `example/bench_*.py`

Each Oxigen benchmark is matched with the Python file that has the same
stem, so the suite compares the same workload across runtimes.

## Quick Start

Run from the repo root:

```bash
python3 scripts/bench.py
```

That will:

- use `target/release/oxigen` if it already exists
- otherwise build it
- run the full paired benchmark suite
- write `*.json` and `*.md` reports into this directory
- refresh `latest.json` and `latest.md`

## Recommended Workflow

Use named reports so you can compare before and after a change.

1. Capture a baseline:

```bash
python3 scripts/bench.py --report-name before-change
```

2. Make your Oxigen changes.

3. Capture the updated result:

```bash
python3 scripts/bench.py --report-name after-change
```

4. Compare the reports:

```bash
diff -u benchmark_reports/before-change.md benchmark_reports/after-change.md
```

If you want to focus on one benchmark while iterating:

```bash
python3 scripts/bench.py bench_loop --report-name bench-loop-before
python3 scripts/bench.py bench_loop --report-name bench-loop-after
```

## Useful Commands

Run the full suite with more stable sampling:

```bash
python3 scripts/bench.py --runs 10 --warmups 3 --report-name full-suite
```

Run a single paired benchmark:

```bash
python3 scripts/bench.py bench_fib --report-name fib-check
```

Force a rebuild first:

```bash
python3 scripts/bench.py --rebuild --report-name rebuilt-suite
```

Use a custom report directory:

```bash
python3 scripts/bench.py --report-dir reports/benchmarks --report-name nightly
```

Skip the Python comparison if you only want Oxigen mode-to-mode numbers:

```bash
python3 scripts/bench.py --oxigen-only --report-name oxigen-only
```

## What To Watch

The most useful fields for tracking improvements are:

- `median_ms`: the primary per-benchmark timing number
- `median_speedup_vs_python`: how each Oxigen mode compares to Python
- `median_speedup_vs_python_geomean`: the suite-wide rollup
- `wins_vs_python`: how many paired tests Oxigen beats Python on

Prefer the median for comparisons between runs. It is less noisy than
the mean for small benchmark samples.

## Report Files

Each run writes:

- `<name>.md`: human-readable summary
- `<name>.json`: raw samples and full metrics
- `latest.md`: most recent Markdown report
- `latest.json`: most recent JSON report

The JSON report is the better source if you want to build your own
history tracking or plotting later.
