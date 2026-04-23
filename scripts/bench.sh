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
# Bumped to 8 so CPU frequency / p-state has settled by the time hyperfine
# starts taking timed measurements. 3 warmups was not enough for short
# (<20 ms) benches that run immediately after long ones: the first few
# timed iterations saw a downclocked CPU and poisoned the median.
WARMUPS="${WARMUPS:-8}"
# Bumped to 15 for outlier resilience: one hot bench showed 4 of 7 runs
# degraded by ~5x due to transient system noise (thermal or scheduling),
# which corrupts a 7-run median. 15 runs make the middle 7 robust.
RUNS="${RUNS:-15}"
# Seconds to sleep between variants within a benchmark. Lets the CPU idle
# down and re-enter a consistent p-state after a previous 200 ms+ variant,
# so short variants (~10 ms) get a clean measurement baseline rather than
# starting on a hot/throttled core.
INTER_VARIANT_SLEEP="${INTER_VARIANT_SLEEP:-0.5}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BENCH_DIR="$REPO_ROOT/example"
REPORT_DIR="$REPO_ROOT/benchmark_reports"
OXIGEN_BIN="${OXIGEN_BIN:-$REPO_ROOT/target/release/oxigen}"
PYTHON_BIN="${PYTHON_BIN:-$(command -v python3)}"
BUN_BIN="${BUN_BIN:-$(command -v bun 2>/dev/null || true)}"
NODE_BIN="${NODE_BIN:-$(command -v node 2>/dev/null || true)}"

# Node ≥ 22 runs .ts files with built-in type-stripping. Older versions
# require --experimental-strip-types. Default to off; probe at runtime.
NODE_TS_ARGS=()
if [[ -n "$NODE_BIN" && -x "$NODE_BIN" ]]; then
    # Build a tiny .ts file in /tmp and see if node chokes without the flag.
    _tmp_ts="$(mktemp --suffix=.ts)"
    printf 'const x: number = 1; x;\n' > "$_tmp_ts"
    if ! "$NODE_BIN" "$_tmp_ts" >/dev/null 2>&1; then
        NODE_TS_ARGS=(--experimental-strip-types)
    fi
    rm -f "$_tmp_ts"
fi

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
            stem="${stem%.ts}"
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

# True if a TypeScript peer exists for $1.
has_ts_peer() {
    local stem=$1
    [[ -f "$BENCH_DIR/$stem.ts" ]]
}

has_bun_ts() {
    local stem=$1
    has_ts_peer "$stem" && [[ -n "$BUN_BIN" && -x "$BUN_BIN" ]]
}

has_node_ts() {
    local stem=$1
    has_ts_peer "$stem" && [[ -n "$NODE_BIN" && -x "$NODE_BIN" ]]
}

# Run each variant in its own hyperfine invocation and merge the
# per-variant JSONs into the combined results file the summarizer
# expects. This avoids the interleaving that caused short variants
# (e.g. oxigen --jit on bench_loop at ~8 ms) to start on a CPU whose
# frequency/thermal state was still carrying over from a preceding
# long variant (e.g. oxigen --no-jit at ~250 ms), producing bimodal
# distributions where the median was pulled off the fast cluster.
run_one_benchmark() {
    local stem=$1
    local oxi_file="$BENCH_DIR/$stem.oxi"
    local py_file="$BENCH_DIR/$stem.py"
    local ts_file="$BENCH_DIR/$stem.ts"
    local out_json="$REPORT_DIR/${stem}.native.json"

    echo "=== $stem ==="

    # Each element: "<display name>\t<command string>". Tab-separated so
    # names can contain spaces (they do: "oxigen --no-jit", "bun (ts)").
    local specs=()
    specs+=("oxigen --no-jit"$'\t'"${TASKSET_PREFIX[*]} $OXIGEN_BIN --no-jit $oxi_file")
    specs+=("oxigen default"$'\t'"${TASKSET_PREFIX[*]} $OXIGEN_BIN $oxi_file")
    specs+=("oxigen --jit"$'\t'"${TASKSET_PREFIX[*]} $OXIGEN_BIN --jit $oxi_file")
    specs+=("python3"$'\t'"${TASKSET_PREFIX[*]} $PYTHON_BIN $py_file")
    if has_bun_ts "$stem"; then
        specs+=("bun (ts)"$'\t'"${TASKSET_PREFIX[*]} $BUN_BIN run $ts_file")
    fi
    if has_node_ts "$stem"; then
        specs+=("node (ts)"$'\t'"${TASKSET_PREFIX[*]} $NODE_BIN ${NODE_TS_ARGS[*]} $ts_file")
    fi

    local tmp_dir
    tmp_dir="$(mktemp -d)"
    local per_variant_jsons=()
    local idx=0
    local first=1
    for spec in "${specs[@]}"; do
        local name="${spec%%$'\t'*}"
        local cmd="${spec#*$'\t'}"
        local vjson="$tmp_dir/variant-$idx.json"
        per_variant_jsons+=("$vjson")
        idx=$((idx + 1))

        if [[ $first -eq 0 ]]; then
            # Let the CPU idle down between variants so the next one
            # starts from a consistent p-state.
            sleep "$INTER_VARIANT_SLEEP"
        fi
        first=0

        hyperfine \
            --warmup "$WARMUPS" \
            --runs "$RUNS" \
            --shell=none \
            --export-json "$vjson" \
            --style=basic \
            -n "$name" "$cmd"
    done

    # Stitch all variants' `results` arrays into a single JSON document
    # matching the shape hyperfine produces when given multiple commands.
    # Downstream `summarize_one` relies on this format.
    jq -s '{results: (map(.results) | add)}' "${per_variant_jsons[@]}" > "$out_json"
    rm -rf "$tmp_dir"
    echo
}

# Extract key metrics from hyperfine's JSON (median in ms) into a TSV
# line. Columns: stem, no-jit, default, jit, python, bun, node,
# jit-vs-python, jit-vs-bun, jit-vs-node. Missing TS runtimes show "-".
# Command-name-based lookup so the "bun" and "node" columns are filled
# regardless of which position each ran in.
summarize_one() {
    local stem=$1
    local out_json="$REPORT_DIR/${stem}.native.json"
    jq -r --arg stem "$stem" '
        def fmt1: . * 10 | round / 10 | tostring;
        def fmtx: . * 100 | round / 100 | tostring + "x";
        def by_name($n): [.results[] | select(.command | startswith($n))][0];
        (by_name("oxigen --no-jit").median * 1000) as $nojit
        | (by_name("oxigen default").median * 1000)  as $default
        | (by_name("oxigen --jit").median * 1000)    as $jit
        | (by_name("python3").median * 1000)         as $python
        | (by_name("bun (ts)")  | if . == null then null else (.median * 1000) end) as $bun
        | (by_name("node (ts)") | if . == null then null else (.median * 1000) end) as $node
        | [
            $stem,
            ($nojit   | fmt1),
            ($default | fmt1),
            ($jit     | fmt1),
            ($python  | fmt1),
            (if $bun  == null then "-" else ($bun  | fmt1) end),
            (if $node == null then "-" else ($node | fmt1) end),
            (($python / $jit) | fmtx),
            (if $bun  == null then "-" else (($bun  / $jit) | fmtx) end),
            (if $node == null then "-" else (($node / $jit) | fmtx) end)
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
    if [[ -n "$BUN_BIN" && -x "$BUN_BIN" ]]; then
        echo "- Bun:       \`$("$BUN_BIN" --version 2>&1 | head -1)\`"
    fi
    if [[ -n "$NODE_BIN" && -x "$NODE_BIN" ]]; then
        node_ts_note=""
        if [[ ${#NODE_TS_ARGS[@]} -gt 0 ]]; then
            node_ts_note=" (${NODE_TS_ARGS[*]})"
        else
            node_ts_note=" (built-in type-stripping)"
        fi
        echo "- Node:      \`$("$NODE_BIN" --version 2>&1 | head -1)\`${node_ts_note}"
    fi
    echo "- Warmups:   \`$WARMUPS\`"
    echo "- Runs:      \`$RUNS\`"
    if git -C "$REPO_ROOT" rev-parse --short HEAD >/dev/null 2>&1; then
        echo "- Git commit: \`$(git -C "$REPO_ROOT" rev-parse --short HEAD)\`"
        echo "- Git branch: \`$(git -C "$REPO_ROOT" rev-parse --abbrev-ref HEAD)\`"
    fi
    echo
    echo "## Medians (ms)"
    echo
    echo "| benchmark | no-jit | default | jit | python | bun (ts) | node (ts) | jit vs py | jit vs bun | jit vs node |"
    echo "| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |"
    for stem in "${benchmarks[@]}"; do
        summarize_one "$stem" \
            | awk -F'\t' '{printf "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n", $1,$2,$3,$4,$5,$6,$7,$8,$9,$10}'
    done
    echo
    echo "Per-benchmark JSON (full samples + hyperfine stats) in \`$REPORT_DIR/\`."
} | tee "$md" > "$latest_md"

cp -f "$md" "$latest_md"

echo
echo "Native report: $md"
echo "Latest:        $latest_md"
