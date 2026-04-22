#!/usr/bin/env python3
"""Benchmark the paired Oxigen/Python suite and write a trackable report."""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import json
import math
import os
from pathlib import Path
import platform
import shutil
import statistics
import subprocess
import sys
import time

REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_OXIGEN_BIN = REPO_ROOT / "target" / "release" / "oxigen"
DEFAULT_BENCH_DIR = REPO_ROOT / "example"
DEFAULT_REPORT_DIR = REPO_ROOT / "benchmark_reports"

_PIN_CPU = os.environ.get("OXIGEN_BENCH_CPU")
_TASKSET = shutil.which("taskset")
_TASKSET_PREFIX = ["taskset", "-c", _PIN_CPU] if _PIN_CPU and _TASKSET else []

OXIGEN_NO_JIT = "oxigen_no_jit"
OXIGEN_DEFAULT = "oxigen_default"
OXIGEN_JIT = "oxigen_jit"
PYTHON = "python"
OXIGEN_MODE_KEYS = (OXIGEN_NO_JIT, OXIGEN_DEFAULT, OXIGEN_JIT)


def run_capture(args: list[str], cwd: Path | None = None) -> str | None:
    try:
        result = subprocess.run(
            args,
            cwd=cwd,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
    except (FileNotFoundError, subprocess.CalledProcessError):
        return None
    return result.stdout.strip() or result.stderr.strip() or None


def resolve_script_path(raw: str) -> Path:
    candidate = Path(raw)
    if not candidate.is_absolute():
        candidate = REPO_ROOT / candidate

    if candidate.suffix == ".py":
        candidate = candidate.with_suffix(".oxi")
    elif candidate.suffix == "":
        oxi_candidate = candidate.with_suffix(".oxi")
        if oxi_candidate.exists():
            candidate = oxi_candidate
        else:
            example_candidate = DEFAULT_BENCH_DIR / f"{raw}.oxi"
            if example_candidate.exists():
                candidate = example_candidate

    return candidate.resolve()


def discover_scripts(requested: list[str]) -> list[Path]:
    if not requested:
        return sorted(
            path
            for path in DEFAULT_BENCH_DIR.glob("bench_*.oxi")
            if path.with_suffix(".py").exists()
        )

    scripts: list[Path] = []
    for raw in requested:
        path = resolve_script_path(raw)
        if not path.exists():
            raise SystemExit(f"Benchmark script not found: {raw}")
        if path.suffix != ".oxi":
            raise SystemExit(f"Expected an .oxi benchmark, got: {path}")
        scripts.append(path)
    return scripts


def ensure_python_pairs(scripts: list[Path]) -> None:
    missing = [script for script in scripts if not script.with_suffix(".py").exists()]
    if missing:
        joined = ", ".join(path.name for path in missing)
        raise SystemExit(f"Missing Python benchmark pair(s): {joined}")


def ensure_built(
    oxigen_bin: Path, skip_build: bool, plain_build: bool, rebuild: bool
) -> dict[str, str | bool]:
    if skip_build:
        if not oxigen_bin.exists():
            raise SystemExit(
                f"Oxigen binary not found at {oxigen_bin}. "
                "Run without --skip-build or pass --oxigen-bin."
            )
        return {"action": "skip-build", "jit_enabled": "unknown"}

    if oxigen_bin != DEFAULT_OXIGEN_BIN:
        if not oxigen_bin.exists():
            raise SystemExit(
                "Custom --oxigen-bin does not exist. "
                "Build it first or point to target/release/oxigen."
            )
        return {"action": "custom-binary", "jit_enabled": "unknown"}

    if oxigen_bin.exists() and not rebuild:
        return {"action": "used-existing-binary", "jit_enabled": "unknown"}

    cmd = ["cargo", "build", "--release", "-p", "oxigen"]
    if not plain_build:
        cmd.extend(["--features", "jit"])

    mode_label = "without JIT" if plain_build else "with JIT support"
    print(f"Building Oxigen {mode_label}...")
    subprocess.run(cmd, cwd=REPO_ROOT, check=True)
    return {
        "action": "rebuilt" if rebuild else "built-missing-binary",
        "jit_enabled": not plain_build,
    }


def run_once(args: list[str]) -> float:
    cmd = _TASKSET_PREFIX + args
    start = time.perf_counter()
    subprocess.run(
        cmd,
        check=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    return time.perf_counter() - start


def benchmark_stats(label: str, args: list[str], warmups: int, runs: int) -> dict[str, object]:
    for _ in range(warmups):
        run_once(args)

    samples = [run_once(args) for _ in range(runs)]
    min_s = min(samples)
    median_s = statistics.median(samples)
    mean_s = statistics.mean(samples)
    stdev_s = statistics.stdev(samples) if len(samples) > 1 else 0.0
    max_s = max(samples)

    print(
        f"  {label:<15} median={median_s*1000:8.1f}ms  "
        f"mean={mean_s*1000:8.1f}ms  stdev={stdev_s*1000:6.1f}ms  "
        f"min={min_s*1000:8.1f}ms  (n={len(samples)})"
    )

    return {
        "command": args,
        "samples_ms": [round(sample * 1000, 3) for sample in samples],
        "min_ms": round(min_s * 1000, 3),
        "median_ms": round(median_s * 1000, 3),
        "mean_ms": round(mean_s * 1000, 3),
        "stdev_ms": round(stdev_s * 1000, 3),
        "max_ms": round(max_s * 1000, 3),
        "runs": len(samples),
    }


def speedup_ratio(baseline_ms: float | None, sample_ms: float | None) -> float | None:
    if baseline_ms is None or sample_ms is None or sample_ms <= 0:
        return None
    return baseline_ms / sample_ms


def format_ms(value: float | None) -> str:
    if value is None:
        return "-"
    return f"{value:.1f}"


def format_speedup(value: float | None) -> str:
    if value is None:
        return "-"
    return f"{value:.2f}x"


def geometric_mean(values: list[float]) -> float | None:
    if not values:
        return None
    return math.exp(sum(math.log(value) for value in values) / len(values))


def compare(
    script: Path,
    oxigen_bin: Path,
    python_bin: str,
    warmups: int,
    runs: int,
    include_python: bool,
    include_jit: bool,
) -> dict[str, object]:
    print(f"=== {script.stem} ===")
    python_script = script.with_suffix(".py")

    metrics: dict[str, dict[str, object] | None] = {
        OXIGEN_NO_JIT: benchmark_stats(
            "oxigen --no-jit",
            [str(oxigen_bin), "--no-jit", str(script)],
            warmups,
            runs,
        ),
        OXIGEN_DEFAULT: benchmark_stats(
            "oxigen default",
            [str(oxigen_bin), str(script)],
            warmups,
            runs,
        ),
        OXIGEN_JIT: None,
        PYTHON: None,
    }

    if include_jit:
        metrics[OXIGEN_JIT] = benchmark_stats(
            "oxigen --jit",
            [str(oxigen_bin), "--jit", str(script)],
            warmups,
            runs,
        )

    if include_python:
        metrics[PYTHON] = benchmark_stats(
            Path(python_bin).name,
            [python_bin, str(python_script)],
            warmups,
            runs,
        )

    python_median = (
        None if metrics[PYTHON] is None else float(metrics[PYTHON]["median_ms"])
    )
    comparisons = {
        "median_speedup_vs_python": {
            mode: speedup_ratio(
                python_median,
                None if metrics[mode] is None else float(metrics[mode]["median_ms"]),
            )
            for mode in OXIGEN_MODE_KEYS
        }
    }

    if python_median is not None:
        print(
            "  median speedup vs python: "
            f"--no-jit {format_speedup(comparisons['median_speedup_vs_python'][OXIGEN_NO_JIT])}  "
            f"default {format_speedup(comparisons['median_speedup_vs_python'][OXIGEN_DEFAULT])}  "
            f"--jit {format_speedup(comparisons['median_speedup_vs_python'][OXIGEN_JIT])}"
        )

    print()
    return {
        "name": script.stem,
        "oxigen_script": str(script.relative_to(REPO_ROOT)),
        "python_script": str(python_script.relative_to(REPO_ROOT)) if include_python else None,
        "metrics": metrics,
        "comparisons": comparisons,
    }


def suite_summary(benchmarks: list[dict[str, object]]) -> dict[str, object]:
    summary: dict[str, object] = {
        "median_speedup_vs_python_geomean": {},
        "wins_vs_python": {},
    }

    for mode in OXIGEN_MODE_KEYS:
        ratios = []
        wins = 0
        total = 0
        for bench in benchmarks:
            ratio = bench["comparisons"]["median_speedup_vs_python"][mode]
            if ratio is None:
                continue
            total += 1
            ratios.append(float(ratio))
            if ratio > 1.0:
                wins += 1

        summary["median_speedup_vs_python_geomean"][mode] = geometric_mean(ratios)
        summary["wins_vs_python"][mode] = {"wins": wins, "total": total}

    return summary


def markdown_report(report: dict[str, object]) -> str:
    lines = [
        "# Oxigen vs Python Benchmark Report",
        "",
        f"- Generated: `{report['generated_at']}`",
        f"- Host: `{report['environment']['hostname']}`",
        f"- Platform: `{report['environment']['platform']}`",
        f"- Python: `{report['environment']['python_version']}` at `{report['config']['python_bin']}`",
        f"- Oxigen: `{report['environment']['oxigen_version'] or 'unknown'}` at `{report['config']['oxigen_bin']}`",
        f"- Build action: `{report['config']['build_action']}`",
        f"- Warmups: `{report['config']['warmups']}`",
        f"- Runs: `{report['config']['runs']}`",
        "",
    ]

    if report["environment"]["git_commit"]:
        lines.append(f"- Git commit: `{report['environment']['git_commit']}`")
    if report["environment"]["git_branch"]:
        lines.append(f"- Git branch: `{report['environment']['git_branch']}`")
    lines.append("")

    lines.extend(
        [
            "## Suite Summary",
            "",
            "| mode | geomean speedup vs python (median) | wins vs python |",
            "| --- | ---: | ---: |",
        ]
    )
    for mode in OXIGEN_MODE_KEYS:
        geomean = report["suite_summary"]["median_speedup_vs_python_geomean"][mode]
        wins = report["suite_summary"]["wins_vs_python"][mode]
        lines.append(
            f"| {mode} | {format_speedup(geomean)} | {wins['wins']}/{wins['total']} |"
        )

    lines.extend(
        [
            "",
            "## Benchmarks",
            "",
            "| benchmark | no-jit median ms | default median ms | jit median ms | python median ms | jit vs python |",
            "| --- | ---: | ---: | ---: | ---: | ---: |",
        ]
    )

    for bench in report["benchmarks"]:
        metrics = bench["metrics"]
        jit_vs_python = bench["comparisons"]["median_speedup_vs_python"][OXIGEN_JIT]
        lines.append(
            "| "
            + " | ".join(
                [
                    bench["name"],
                    format_ms(
                        None if metrics[OXIGEN_NO_JIT] is None else metrics[OXIGEN_NO_JIT]["median_ms"]
                    ),
                    format_ms(
                        None if metrics[OXIGEN_DEFAULT] is None else metrics[OXIGEN_DEFAULT]["median_ms"]
                    ),
                    format_ms(
                        None if metrics[OXIGEN_JIT] is None else metrics[OXIGEN_JIT]["median_ms"]
                    ),
                    format_ms(None if metrics[PYTHON] is None else metrics[PYTHON]["median_ms"]),
                    format_speedup(jit_vs_python),
                ]
            )
            + " |"
        )

    lines.extend(
        [
            "",
            "Raw samples, commands, and full per-benchmark statistics are stored in the JSON report.",
            "",
        ]
    )
    return "\n".join(lines)


def write_report(report: dict[str, object], report_dir: Path, report_name: str) -> tuple[Path, Path]:
    report_dir.mkdir(parents=True, exist_ok=True)
    json_path = report_dir / f"{report_name}.json"
    md_path = report_dir / f"{report_name}.md"
    latest_json = report_dir / "latest.json"
    latest_md = report_dir / "latest.md"

    json_text = json.dumps(report, indent=2)
    md_text = markdown_report(report)

    json_path.write_text(json_text + "\n", encoding="utf-8")
    md_path.write_text(md_text + "\n", encoding="utf-8")
    latest_json.write_text(json_text + "\n", encoding="utf-8")
    latest_md.write_text(md_text + "\n", encoding="utf-8")
    return json_path, md_path


def print_summary_table(benchmarks: list[dict[str, object]]) -> None:
    headers = ["bench", "no-jit", "default", "jit", "python", "jit vs py"]
    rows = []
    for bench in benchmarks:
        metrics = bench["metrics"]
        rows.append(
            [
                bench["name"],
                format_ms(
                    None if metrics[OXIGEN_NO_JIT] is None else metrics[OXIGEN_NO_JIT]["median_ms"]
                ),
                format_ms(
                    None if metrics[OXIGEN_DEFAULT] is None else metrics[OXIGEN_DEFAULT]["median_ms"]
                ),
                format_ms(None if metrics[OXIGEN_JIT] is None else metrics[OXIGEN_JIT]["median_ms"]),
                format_ms(None if metrics[PYTHON] is None else metrics[PYTHON]["median_ms"]),
                format_speedup(
                    bench["comparisons"]["median_speedup_vs_python"][OXIGEN_JIT]
                ),
            ]
        )

    widths = [
        max(len(header), *(len(row[idx]) for row in rows))
        for idx, header in enumerate(headers)
    ]

    print("Summary (median ms)")
    print("  " + "  ".join(header.ljust(widths[idx]) for idx, header in enumerate(headers)))
    for row in rows:
        print("  " + "  ".join(cell.ljust(widths[idx]) for idx, cell in enumerate(row)))


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Benchmark paired Oxigen and Python example scripts and write a report."
    )
    parser.add_argument(
        "scripts",
        nargs="*",
        help="Optional benchmark scripts or stems. Example: example/bench_loop.oxi or bench_loop",
    )
    parser.add_argument("--runs", type=int, default=7, help="Measured runs per benchmark.")
    parser.add_argument("--warmups", type=int, default=2, help="Warmup runs per benchmark.")
    parser.add_argument(
        "--oxigen-bin",
        default=str(DEFAULT_OXIGEN_BIN),
        help="Path to the Oxigen binary to benchmark.",
    )
    parser.add_argument(
        "--python-bin",
        default=sys.executable,
        help="Python binary used for the comparison runs.",
    )
    parser.add_argument(
        "--report-dir",
        default=str(DEFAULT_REPORT_DIR),
        help="Directory where JSON and Markdown reports are written.",
    )
    parser.add_argument(
        "--report-name",
        help="Base filename for the report. Defaults to a timestamped suite name.",
    )
    parser.add_argument(
        "--skip-build",
        action="store_true",
        help="Do not build Oxigen before benchmarking.",
    )
    parser.add_argument(
        "--rebuild",
        action="store_true",
        help="Force a fresh Oxigen build before benchmarking.",
    )
    parser.add_argument(
        "--plain-build",
        action="store_true",
        help="Build without the `jit` feature and skip the --jit benchmark.",
    )
    parser.add_argument(
        "--oxigen-only",
        action="store_true",
        help="Skip the Python comparison and run Oxigen modes only.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    scripts = discover_scripts(args.scripts)
    include_python = not args.oxigen_only
    include_jit = not args.plain_build
    oxigen_bin = Path(args.oxigen_bin).resolve()
    report_dir = Path(args.report_dir).resolve()

    if not scripts:
        raise SystemExit("No paired benchmarks found.")

    if include_python:
        ensure_python_pairs(scripts)

    if _PIN_CPU and not _TASKSET:
        print(
            "Warning: OXIGEN_BENCH_CPU is set but `taskset` is unavailable; CPU pinning skipped.",
            file=sys.stderr,
        )

    build_info = ensure_built(
        oxigen_bin,
        skip_build=args.skip_build,
        plain_build=args.plain_build,
        rebuild=args.rebuild,
    )

    print(f"Oxigen binary: {oxigen_bin}")
    print(f"Python binary: {Path(args.python_bin).resolve()}")
    print(f"Benchmarks: {len(scripts)}")
    print(f"Build action: {build_info['action']}")
    if build_info["action"] == "used-existing-binary":
        print("Using existing Oxigen binary. Pass --rebuild for a fresh JIT-enabled build.")
    print()

    benchmark_results = [
        compare(
            script,
            oxigen_bin=oxigen_bin,
            python_bin=args.python_bin,
            warmups=args.warmups,
            runs=args.runs,
            include_python=include_python,
            include_jit=include_jit,
        )
        for script in scripts
    ]

    summary = suite_summary(benchmark_results)
    print_summary_table(benchmark_results)

    timestamp = datetime.now(timezone.utc).strftime("%Y%m%d-%H%M%S")
    report_name = args.report_name or f"oxigen-vs-python-{timestamp}"
    report = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "config": {
            "runs": args.runs,
            "warmups": args.warmups,
            "oxigen_bin": str(oxigen_bin),
            "python_bin": str(Path(args.python_bin).resolve()),
            "report_dir": str(report_dir),
            "build_action": build_info["action"],
            "include_python": include_python,
            "include_jit": include_jit,
            "cpu_pin": _PIN_CPU,
        },
        "environment": {
            "hostname": platform.node(),
            "platform": platform.platform(),
            "python_version": sys.version.replace("\n", " "),
            "oxigen_version": run_capture([str(oxigen_bin), "--version"]),
            "git_commit": run_capture(["git", "rev-parse", "HEAD"], cwd=REPO_ROOT),
            "git_branch": run_capture(
                ["git", "rev-parse", "--abbrev-ref", "HEAD"], cwd=REPO_ROOT
            ),
        },
        "benchmarks": benchmark_results,
        "suite_summary": summary,
    }

    json_path, md_path = write_report(report, report_dir, report_name)
    print()
    print(f"Report written: {json_path}")
    print(f"Report written: {md_path}")
    print(f"Latest report:  {report_dir / 'latest.md'}")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        sys.exit(130)
