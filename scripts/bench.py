#!/usr/bin/env python3
"""Quick benchmark harness for the Oxigen JIT.

Runs each configuration a few times, prints best/mean/stddev. Not a
real statistics engine — just enough to see order-of-magnitude changes.

With --python, also runs the sibling `<stem>.py` script through CPython
so Oxigen --no-jit / --jit can be compared against a mainstream interpreter.
"""

import argparse
import os
import statistics
import subprocess
import sys
import time

BIN = os.path.join(os.path.dirname(__file__), "..", "target", "release", "oxigen")

# Optional CPU pin: `OXIGEN_BENCH_CPU=2` pins every bench invocation to core 2
# via `taskset`, which cuts cross-core scheduling noise from a loaded desktop.
_PIN_CPU = os.environ.get("OXIGEN_BENCH_CPU")
_TASKSET_PREFIX = ["taskset", "-c", _PIN_CPU] if _PIN_CPU else []


def run_once(args):
    cmd = _TASKSET_PREFIX + args
    start = time.perf_counter()
    subprocess.run(
        cmd,
        check=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    return time.perf_counter() - start


def bench(label, args, warmups, runs):
    # Warmups (OS cache, etc.) — not counted.
    for _ in range(warmups):
        run_once(args)
    samples = [run_once(args) for _ in range(runs)]
    best = min(samples)
    mean = statistics.mean(samples)
    stdev = statistics.stdev(samples) if len(samples) > 1 else 0.0
    print(
        f"  {label:<14} best={best*1000:8.1f}ms  mean={mean*1000:8.1f}ms  "
        f"stdev={stdev*1000:5.1f}ms  (n={len(samples)})"
    )
    return best


def compare(script, warmups=2, runs=5, include_python=False):
    print(f"=== {script} ===")
    interp = [BIN, "--no-jit", script]
    default = [BIN, script]
    eager = [BIN, "--jit", script]
    interp_best = bench("--no-jit", interp, warmups, runs)
    default_best = bench("default", default, warmups, runs)
    eager_best = bench("--jit", eager, warmups, runs)

    py_best = None
    if include_python:
        py_path = os.path.splitext(script)[0] + ".py"
        if os.path.exists(py_path):
            py_best = bench("python3", [sys.executable, py_path], warmups, runs)
        else:
            print(f"  (python3       skipped: {py_path} not found)")

    print(
        f"  → default speedup: {interp_best/default_best:.2f}x    "
        f"--jit speedup: {interp_best/eager_best:.2f}x    "
        f"(best-of-{runs})"
    )
    if py_best is not None:
        print(
            f"  → vs python3:     --no-jit {py_best/interp_best:.2f}x    "
            f"default {py_best/default_best:.2f}x    "
            f"--jit {py_best/eager_best:.2f}x"
        )
    print()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("scripts", nargs="+")
    ap.add_argument("--runs", type=int, default=5)
    ap.add_argument("--warmups", type=int, default=2)
    ap.add_argument(
        "--python",
        action="store_true",
        help="Also benchmark the sibling <stem>.py script through CPython.",
    )
    args = ap.parse_args()
    for s in args.scripts:
        compare(s, warmups=args.warmups, runs=args.runs, include_python=args.python)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        sys.exit(130)
