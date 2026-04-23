#!/usr/bin/env bash
# Benchmark Oxigen vs CPython using hyperfine (no Python in the
# measurement harness, unlike scripts/bench.py).
#
# Usage:
#   scripts/bench.sh                        # full paired suite
#   scripts/bench.sh bench_arith bench_fib  # specific benchmarks
#   WARMUPS=3 RUNS=10 scripts/bench.sh      # tune warmups/runs
#   OXIGEN_BENCH_CPU=3 scripts/bench.sh     # pin to a specific CPU
#
# Each paired benchmark runs four variants — oxigen --no-jit,
# oxigen default, oxigen --jit, and python3 — with hyperfine doing
# warmups, N timed runs, and stats. Results go to benchmark_reports/
# as JSON (per-bench) and a consolidated Markdown summary.

set -euo pipefail

# ── Configuration ──────────────────────────────────────────────────────
WARMUPS="${WARMUPS:-3}"
# Bumped to 15 for outlier resilience: one hot bench showed 4 of 7 runs
# degraded by ~5x due to transient system noise (thermal or scheduling),
# which corrupts a 7-run median. 15 runs make the middle 7 robust.
RUNS="${RUNS:-15}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BENCH_DIR="$REPO_ROOT/example"
REPORT_DIR="$REPO_ROOT/benchmark_reports"
OXIGEN_BIN="${OXIGEN_BIN:-$REPO_ROOT/target/release/oxigen}"
PYTHON_BIN="${PYTHON_BIN:-$(command -v python3)}"

# Optional CPU pinning (mirrors scripts/bench.py's OXIGEN_BENCH_CPU).
TASKSET_PREFIX=()
if [[ -n "${OXIGEN_BENCH_CPU:-}" ]] && command -v taskset >/dev/null 2>&1; then
    TASKSET_PREFIX=(taskset -c "$OXIGEN_BENCH_CPU")
fi

# ── Helpers ────────────────────────────────────────────────────────────

die() { echo "error: $*" >&2; exit 1; }

command -v hyperfine >/dev/null 2>&1 \
    || die "hyperfine not found in PATH — install it (e.g. pacman -S hyperfine)"
command -v jq >/dev/null 2>&1 \
    || die "jq not found in PATH"

[[ -x "$OXIGEN_BIN" ]] \
    || die "Oxigen binary not found at $OXIGEN_BIN. Build with 'cargo build --release --features jit -p oxigen' first."
[[ -x "$PYTHON_BIN" ]] \
    || die "python binary not found ($PYTHON_BIN)"

discover_benchmarks() {
    if [[ $# -gt 0 ]]; then
        for name in "$@"; do
            local stem="$name"
            stem="${stem%.oxi}"
            stem="${stem%.py}"
            local oxi_file="$BENCH_DIR/${stem##*/}.oxi"
            local py_file="$BENCH_DIR/${stem##*/}.py"
            [[ -f "$oxi_file" ]] || die "missing .oxi: $oxi_file"
            [[ -f "$py_file" ]]  || die "missing .py:  $py_file"
            echo "${stem##*/}"
        done
    else
        for f in "$BENCH_DIR"/bench_*.oxi; do
            local stem
            stem="$(basename "$f" .oxi)"
            if [[ -f "$BENCH_DIR/$stem.py" ]]; then
                echo "$stem"
            fi
        done | sort
    fi
}

# Run one hyperfine invocation against four commands, emitting a JSON
# results file. hyperfine handles warmups, timed runs, and stats.
run_one_benchmark() {
    local stem=$1
    local oxi_file="$BENCH_DIR/$stem.oxi"
    local py_file="$BENCH_DIR/$stem.py"
    local out_json="$REPORT_DIR/${stem}.native.json"

    echo "=== $stem ==="
    hyperfine \
        --warmup "$WARMUPS" \
        --runs "$RUNS" \
        --shell=none \
        --export-json "$out_json" \
        --style=basic \
        -n "oxigen --no-jit" "${TASKSET_PREFIX[@]} $OXIGEN_BIN --no-jit $oxi_file" \
        -n "oxigen default"  "${TASKSET_PREFIX[@]} $OXIGEN_BIN $oxi_file" \
        -n "oxigen --jit"    "${TASKSET_PREFIX[@]} $OXIGEN_BIN --jit $oxi_file" \
        -n "python3"         "${TASKSET_PREFIX[@]} $PYTHON_BIN $py_file"
    echo
}

# Extract key metrics from hyperfine's JSON (median in ms) into a compact
# line that we accumulate into the summary report.
summarize_one() {
    local stem=$1
    local out_json="$REPORT_DIR/${stem}.native.json"
    jq -r --arg stem "$stem" '
        .results
        | map({name: .command, median_ms: (.median * 1000)})
        | (.[0].median_ms) as $nojit
        | (.[1].median_ms) as $default
        | (.[2].median_ms) as $jit
        | (.[3].median_ms) as $python
        | [
            $stem,
            ($nojit   | . * 10 | round / 10 | tostring),
            ($default | . * 10 | round / 10 | tostring),
            ($jit     | . * 10 | round / 10 | tostring),
            ($python  | . * 10 | round / 10 | tostring),
            (($python / $jit) * 100 | round / 100 | tostring + "x")
          ]
        | @tsv
    ' "$out_json"
}

# ── Main ───────────────────────────────────────────────────────────────

mkdir -p "$REPORT_DIR"

benchmarks=()
while IFS= read -r line; do
    benchmarks+=("$line")
done < <(discover_benchmarks "$@")

[[ ${#benchmarks[@]} -gt 0 ]] || die "no benchmarks found"

echo "Oxigen binary: $OXIGEN_BIN"
echo "Python binary: $PYTHON_BIN"
echo "Benchmarks:    ${#benchmarks[@]} (${benchmarks[*]})"
echo "Warmups:       $WARMUPS"
echo "Runs:          $RUNS"
if [[ ${#TASKSET_PREFIX[@]} -gt 0 ]]; then
    echo "CPU pin:       ${TASKSET_PREFIX[*]:2}"
fi
echo

for stem in "${benchmarks[@]}"; do
    run_one_benchmark "$stem"
done

# ── Summary ────────────────────────────────────────────────────────────

timestamp="$(date -u +%Y%m%d-%H%M%S)"
md="$REPORT_DIR/oxigen-vs-python-native-$timestamp.md"
latest_md="$REPORT_DIR/latest-native.md"

{
    echo "# Oxigen vs Python — native harness (hyperfine)"
    echo
    echo "- Generated: \`$(date -u -Iseconds)\`"
    echo "- Host:      \`${HOSTNAME:-$(uname -n)}\`"
    echo "- Kernel:    \`$(uname -srm)\`"
    echo "- Oxigen:    \`$("$OXIGEN_BIN" --version 2>/dev/null | head -1 || echo unknown)\`"
    echo "- Python:    \`$("$PYTHON_BIN" --version 2>&1)\`"
    echo "- Warmups:   \`$WARMUPS\`"
    echo "- Runs:      \`$RUNS\`"
    if git -C "$REPO_ROOT" rev-parse --short HEAD >/dev/null 2>&1; then
        echo "- Git commit: \`$(git -C "$REPO_ROOT" rev-parse --short HEAD)\`"
        echo "- Git branch: \`$(git -C "$REPO_ROOT" rev-parse --abbrev-ref HEAD)\`"
    fi
    echo
    echo "## Medians (ms)"
    echo
    echo "| benchmark | no-jit | default | jit | python | jit vs py |"
    echo "| --- | ---: | ---: | ---: | ---: | ---: |"
    for stem in "${benchmarks[@]}"; do
        summarize_one "$stem" \
            | awk -F'\t' '{printf "| %s | %s | %s | %s | %s | %s |\n", $1,$2,$3,$4,$5,$6}'
    done
    echo
    echo "Per-benchmark JSON (full samples + hyperfine stats) in \`$REPORT_DIR/\`."
} | tee "$md" > "$latest_md"

cp -f "$md" "$latest_md"

echo
echo "Native report: $md"
echo "Latest:        $latest_md"
