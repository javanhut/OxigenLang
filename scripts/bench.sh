#!/usr/bin/env bash
# Benchmark Oxigen vs CPython, Bun, and Node using a thermal-fair
# interleaved A/B/A/B/... harness. No Python in the measurement
# harness — pure bash + awk + jq.
#
# Usage:
#   scripts/bench.sh                                # full paired suite
#   scripts/bench.sh bench_arith bench_fib          # specific benchmarks
#   scripts/bench.sh --warmup=8 --runs=15           # tune warmups/runs
#   WARMUPS=3 RUNS=15 scripts/bench.sh              # legacy env-var form
#   OXIGEN_BENCH_CPU=3 scripts/bench.sh             # pin to a specific CPU
#   OXIGEN_BIN_B=/path/to/oxigen scripts/bench.sh   # A/B between two oxigen
#                                                   # builds, both --jit, fully
#                                                   # interleaved with the rest
#                                                   # of the round so thermal
#                                                   # drift hits both equally
#
# Flags (override env vars):
#   --warmup=N | --warmups=N | --warmup N   untimed rounds per variant
#   --runs=N                                timed rounds per variant
#   --                                      stop flag parsing
# Anything else is treated as a positional benchmark name.
#
# For each benchmark we run V variants (oxigen --jit, oxigen
# default, oxigen --no-jit, python3, optional bun (ts), optional
# node (ts)). Each measurement ROUND runs every variant once in
# fixed order before the next round starts — so every variant
# observes the same thermal state per round, and any thermal drift
# affects all variants equally. WARMUPS rounds run untimed first to
# fill caches and prime the JIT.
#
# This is strictly stronger than the previous "run all of variant
# A's RUNS samples, then sleep, then all of variant B's" layout
# (which gave the first variant a cold-CPU advantage and the last
# variant a hot-CPU disadvantage). Min and median are now directly
# comparable across variants.
#
# Output: per-bench JSON (hyperfine-shape, so legacy tooling keeps
# working) plus a consolidated Markdown report under
# benchmark_reports/.

set -euo pipefail

# ── Configuration ──────────────────────────────────────────────────────
# Tuned for fast iteration (~2-5 min full run). Bump RUNS for a more
# stable median on noisy systems.
#
# 3 warmups + 5 measure rounds, with all variants interleaved per
# round, is enough to filter cold-cache noise on the dev loop. RUNS
# is the number of times each variant is timed — total per-bench
# work is `(WARMUPS + RUNS) * V` invocations, where V is the variant
# count.
WARMUPS="${WARMUPS:-3}"
RUNS="${RUNS:-5}"

# Flag parsing — `--warmup=8 --runs=15` overrides the env-var defaults
# above. Unrecognised non-flag args drop through as positional benchmark
# names (consumed by `discover_benchmarks` below).
POSITIONAL=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --warmup=*|--warmups=*) WARMUPS="${1#*=}"; shift ;;
        --warmup|--warmups)
            [[ $# -ge 2 ]] || { echo "error: $1 needs a value" >&2; exit 1; }
            WARMUPS="$2"; shift 2 ;;
        --runs=*) RUNS="${1#*=}"; shift ;;
        --runs)
            [[ $# -ge 2 ]] || { echo "error: --runs needs a value" >&2; exit 1; }
            RUNS="$2"; shift 2 ;;
        --) shift; POSITIONAL+=("$@"); break ;;
        --*) echo "error: unknown flag: $1" >&2; exit 1 ;;
        *) POSITIONAL+=("$1"); shift ;;
    esac
done
set -- "${POSITIONAL[@]}"

# Validate numeric.
[[ "$WARMUPS" =~ ^[0-9]+$ ]] || { echo "error: --warmup must be a non-negative integer (got: $WARMUPS)" >&2; exit 1; }
[[ "$RUNS"    =~ ^[0-9]+$ ]] || { echo "error: --runs must be a non-negative integer (got: $RUNS)" >&2; exit 1; }
[[ "$RUNS" -ge 1 ]] || { echo "error: --runs must be >= 1 (got: $RUNS)" >&2; exit 1; }

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BENCH_DIR="$REPO_ROOT/example"
REPORT_DIR="$REPO_ROOT/benchmark_reports"
OXIGEN_BIN="${OXIGEN_BIN:-$REPO_ROOT/target/release/oxigen}"
# A/B mode: when OXIGEN_BIN_B is set to a second oxigen binary, an
# "oxigen-B --jit" variant is added to the interleaved harness so the
# two builds are compared under identical thermal conditions per round.
# The B-binary is also reported with min/median/p50 in the summary
# table along with its ratio against OXIGEN_BIN.
OXIGEN_BIN_B="${OXIGEN_BIN_B:-}"
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

command -v jq >/dev/null 2>&1 \
    || die "jq not found in PATH"
command -v awk >/dev/null 2>&1 \
    || die "awk not found in PATH"

[[ -x "$OXIGEN_BIN" ]] \
    || die "Oxigen binary not found at $OXIGEN_BIN. Build with 'cargo build --release --features jit -p oxigen' first."
[[ -x "$PYTHON_BIN" ]] \
    || die "python binary not found ($PYTHON_BIN)"
if [[ -n "$OXIGEN_BIN_B" ]]; then
    [[ -x "$OXIGEN_BIN_B" ]] \
        || die "OXIGEN_BIN_B set but not executable: $OXIGEN_BIN_B"
fi

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

# Time a single command-line invocation. Returns elapsed seconds as a
# decimal, printed to stdout. Stderr/stdout from the command are
# silenced so they don't leak into the parent script's output.
time_one() {
    local cmd_str="$*"
    # Use bash's $EPOCHREALTIME (decimal seconds, microsecond
    # precision) so we don't fork `date` per call.
    local s e
    s=$EPOCHREALTIME
    eval "$cmd_str" >/dev/null 2>&1
    e=$EPOCHREALTIME
    awk -v s="$s" -v e="$e" 'BEGIN { printf "%.6f", e - s }'
}

# A/B/A/B/... interleaved runner. Every variant runs ONCE per round;
# WARMUPS rounds are untimed, RUNS rounds are timed. Per-variant
# samples accumulate in a per-variant samples file; at the end we
# emit a hyperfine-shaped JSON so the existing summarizer keeps
# working without changes.
run_one_benchmark() {
    local stem=$1
    local oxi_file="$BENCH_DIR/$stem.oxi"
    local py_file="$BENCH_DIR/$stem.py"
    local ts_file="$BENCH_DIR/$stem.ts"
    local out_json="$REPORT_DIR/${stem}.native.json"

    echo "=== $stem ==="

    # Each element: "<display name>\t<command string>". Tab-separated
    # so names can contain spaces ("oxigen --no-jit", "bun (ts)").
    #
    # Order is fixed but no longer carries the same correctness weight
    # — every variant runs once per round so all share the same
    # thermal state per round. Order here just controls how variants
    # are reported.
    local specs=()
    specs+=("oxigen --jit"$'\t'"${TASKSET_PREFIX[*]} $OXIGEN_BIN --jit $oxi_file")
    # A/B mode: B variant runs back-to-back with A in the same round
    # so both observe the same thermal/cache state.
    if [[ -n "$OXIGEN_BIN_B" ]]; then
        specs+=("oxigen-B --jit"$'\t'"${TASKSET_PREFIX[*]} $OXIGEN_BIN_B --jit $oxi_file")
    fi
    specs+=("oxigen default"$'\t'"${TASKSET_PREFIX[*]} $OXIGEN_BIN $oxi_file")
    if has_bun_ts "$stem"; then
        specs+=("bun (ts)"$'\t'"${TASKSET_PREFIX[*]} $BUN_BIN run $ts_file")
    fi
    if has_node_ts "$stem"; then
        specs+=("node (ts)"$'\t'"${TASKSET_PREFIX[*]} $NODE_BIN ${NODE_TS_ARGS[*]} $ts_file")
    fi
    specs+=("python3"$'\t'"${TASKSET_PREFIX[*]} $PYTHON_BIN $py_file")
    specs+=("oxigen --no-jit"$'\t'"${TASKSET_PREFIX[*]} $OXIGEN_BIN --no-jit $oxi_file")

    local n_variants=${#specs[@]}
    local tmp_dir
    tmp_dir="$(mktemp -d)"

    # ── Warmup rounds ──
    if [[ $WARMUPS -gt 0 ]]; then
        printf "  warmup:"
        for ((round=1; round<=WARMUPS; round++)); do
            for spec in "${specs[@]}"; do
                local cmd="${spec#*$'\t'}"
                eval "$cmd" >/dev/null 2>&1 || true
            done
            printf " %d" "$round"
        done
        echo
    fi

    # ── Measure rounds ──
    local sample_files=()
    for i in $(seq 0 $((n_variants - 1))); do
        sample_files+=("$tmp_dir/var-$i.txt")
        : > "${sample_files[$i]}"
    done

    printf "  measure:"
    for ((round=1; round<=RUNS; round++)); do
        for i in $(seq 0 $((n_variants - 1))); do
            local cmd="${specs[$i]#*$'\t'}"
            local t
            t="$(time_one "$cmd")"
            printf "%s\n" "$t" >> "${sample_files[$i]}"
        done
        printf " %d" "$round"
    done
    echo

    # ── Per-variant stats + hyperfine-shape JSON ──
    {
        echo '{'
        echo '  "results": ['
        for i in $(seq 0 $((n_variants - 1))); do
            local name="${specs[$i]%%$'\t'*}"
            local cmd_str="${specs[$i]#*$'\t'}"
            # Compute min, max, mean, median, stddev with awk, plus
            # the times array as a JSON list. One pass over the
            # samples file.
            local stats
            stats="$(awk '
                {
                    n++
                    a[n] = $1 + 0
                    sum += a[n]
                    if (n == 1 || a[n] < min) min = a[n]
                    if (n == 1 || a[n] > max) max = a[n]
                }
                END {
                    if (n == 0) { print "0,0,0,0,0,[]"; exit }
                    mean = sum / n
                    # insertion sort (n is small, ≤ a few dozen)
                    for (i = 2; i <= n; i++) {
                        v = a[i]; j = i - 1
                        while (j > 0 && a[j] > v) { a[j+1] = a[j]; j-- }
                        a[j+1] = v
                    }
                    if (n % 2 == 1) median = a[(n+1)/2]
                    else median = (a[n/2] + a[n/2+1]) / 2
                    if (n > 1) {
                        ssq = 0
                        for (i = 1; i <= n; i++) ssq += (a[i] - mean) * (a[i] - mean)
                        sd = sqrt(ssq / (n - 1))
                    } else { sd = 0 }
                    printf "%.6f,%.6f,%.6f,%.6f,%.6f,[", min, max, mean, median, sd
                    for (i = 1; i <= n; i++) {
                        if (i > 1) printf ","
                        printf "%.6f", a[i]
                    }
                    printf "]"
                }
            ' "${sample_files[$i]}")"
            local min_v="${stats%%,*}"; stats="${stats#*,}"
            local max_v="${stats%%,*}"; stats="${stats#*,}"
            local mean_v="${stats%%,*}"; stats="${stats#*,}"
            local median_v="${stats%%,*}"; stats="${stats#*,}"
            local sd_v="${stats%%,[*}"; stats="${stats#*,[}"
            local times_v="[${stats}"
            local trailing_comma=","
            [[ $i -eq $((n_variants - 1)) ]] && trailing_comma=""
            # Escape command string for JSON (quotes only — paths
            # don't contain backslashes in our setup).
            local cmd_json="${cmd_str//\"/\\\"}"
            cat <<EOF
    {
      "command": "${name}",
      "command_full": "${cmd_json}",
      "mean": ${mean_v},
      "stddev": ${sd_v},
      "median": ${median_v},
      "min": ${min_v},
      "max": ${max_v},
      "times": ${times_v}
    }${trailing_comma}
EOF
            # One-line summary to stdout so the user sees per-bench
            # progress.
            awk -v name="$name" -v min="$min_v" -v med="$median_v" -v mean="$mean_v" '
                BEGIN {
                    printf "  %-20s min=%6.1fms  med=%6.1fms  mean=%6.1fms\n",
                        name, min*1000, med*1000, mean*1000
                }
            ' >&2
        done
        echo '  ]'
        echo '}'
    } > "$out_json"

    rm -rf "$tmp_dir"
    echo
}

# Extract key metrics from hyperfine's JSON (min in ms) into a TSV
# line. Columns: stem, no-jit, default, jit, python, bun, node,
# jit-vs-python, jit-vs-bun, jit-vs-node. Missing TS runtimes show "-".
# Command-name-based lookup so the "bun" and "node" columns are filled
# regardless of which position each ran in.
#
# Uses the per-variant `min` rather than `median` so a single thermal
# event mid-sequence can't pull the headline number off the steady-
# state cluster. Median is only stable when >50% of runs land in the
# same thermal regime; once even a few runs throttle, median becomes
# meaningless on small datasets (15 runs). The `min` tracks the
# best-case CPU state, which is what every other process on a healthy
# system would also see; it's the closest single number to "actual
# performance under non-thermal-stressed conditions".
summarize_one() {
    local stem=$1
    local out_json="$REPORT_DIR/${stem}.native.json"
    jq -r --arg stem "$stem" '
        def fmt1: . * 10 | round / 10 | tostring;
        def fmtx: . * 100 | round / 100 | tostring + "x";
        def by_name($n): [.results[] | select(.command | startswith($n))][0];
        (by_name("oxigen --no-jit").min * 1000) as $nojit
        | (by_name("oxigen default").min * 1000)  as $default
        | (by_name("oxigen --jit").min * 1000)    as $jit
        | (by_name("python3").min * 1000)         as $python
        | (by_name("bun (ts)")  | if . == null then null else (.min * 1000) end) as $bun
        | (by_name("node (ts)") | if . == null then null else (.min * 1000) end) as $node
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

# A/B summary row when OXIGEN_BIN_B is in use. Reports min, median, and
# B/A min-ratio for both binaries side-by-side. < 1.00 means B is faster.
summarize_ab() {
    local stem=$1
    local out_json="$REPORT_DIR/${stem}.native.json"
    jq -r --arg stem "$stem" '
        def fmt1: . * 10 | round / 10 | tostring;
        def fmtx: . * 1000 | round / 1000 | tostring + "x";
        def median(arr):
            (arr | sort) as $s
            | ($s | length) as $n
            | if $n == 0 then null
              elif ($n % 2) == 1 then $s[($n - 1) / 2]
              else (($s[$n/2 - 1] + $s[$n/2]) / 2)
              end;
        def by_name($n): [.results[] | select(.command | startswith($n))][0];
        (by_name("oxigen --jit"))   as $a
        | (by_name("oxigen-B --jit")) as $b
        | ($a.min * 1000)                          as $a_min
        | (median([$a.times[] | . * 1000]))        as $a_med
        | ($b.min * 1000)                          as $b_min
        | (median([$b.times[] | . * 1000]))        as $b_med
        | [
            $stem,
            ($a_min | fmt1),
            ($a_med | fmt1),
            ($b_min | fmt1),
            ($b_med | fmt1),
            (($b_min / $a_min) | fmtx),
            (($b_med / $a_med) | fmtx)
          ]
        | @tsv
    ' "$out_json"
}

# Step 0 (plan): emit JIT min/p50 in ms for the under-10ms pass criterion.
# `min < 10ms AND p50 comfortably below 11ms` — bare min can be a thermal
# outlier on a desktop. Median is computed in jq from `.times[]`.
summarize_jit_min_p50() {
    local stem=$1
    local out_json="$REPORT_DIR/${stem}.native.json"
    jq -r --arg stem "$stem" '
        def fmt1: . * 10 | round / 10 | tostring;
        def median(arr):
            (arr | sort) as $s
            | ($s | length) as $n
            | if $n == 0 then null
              elif ($n % 2) == 1 then $s[($n - 1) / 2]
              else (($s[$n/2 - 1] + $s[$n/2]) / 2)
              end;
        def by_name($n): [.results[] | select(.command | startswith($n))][0];
        (by_name("oxigen --jit")) as $j
        | ($j.min * 1000) as $jit_min
        | (median([$j.times[] | . * 1000])) as $jit_p50
        | [ $stem, ($jit_min | fmt1), ($jit_p50 | fmt1) ]
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
    echo "# Oxigen vs Python — native harness (interleaved A/B)"
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
    echo "## Min times (ms)"
    echo
    echo "Each cell is the fastest single timed run across \`$RUNS\` rounds"
    echo "after \`$WARMUPS\` warmup rounds. Variants are interleaved A/B/A/B"
    echo "per round so every variant observes the same thermal state, and"
    echo "any thermal drift across the run affects them equally. Min is"
    echo "the most reproducible single number on a desktop CPU that may"
    echo "throttle after sustained full-CPU work. See"
    echo "\`bench_*.native.json\` for full per-round samples."
    echo
    echo "| benchmark | no-jit | default | jit | python | bun (ts) | node (ts) | jit vs py | jit vs bun | jit vs node |"
    echo "| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |"
    for stem in "${benchmarks[@]}"; do
        summarize_one "$stem" \
            | awk -F'\t' '{printf "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n", $1,$2,$3,$4,$5,$6,$7,$8,$9,$10}'
    done
    echo
    # Step 0: JIT min vs p50 for the under-10ms pass criterion. A bench
    # passes when `min < 10` AND `p50 < ~11` — guards against celebrating
    # a thermally lucky outlier.
    echo "## JIT min / p50 (ms)"
    echo
    echo "| benchmark | jit min | jit p50 |"
    echo "| --- | ---: | ---: |"
    for stem in "${benchmarks[@]}"; do
        summarize_jit_min_p50 "$stem" \
            | awk -F'\t' '{printf "| %s | %s | %s |\n", $1,$2,$3}'
    done
    echo
    if [[ -n "$OXIGEN_BIN_B" ]]; then
        echo "## A/B comparison: \`$OXIGEN_BIN\` vs \`$OXIGEN_BIN_B\` (--jit)"
        echo
        echo "Both binaries run interleaved A/B/A/B per round so they share"
        echo "the same thermal/cache state. \`B/A < 1.00\` means OXIGEN_BIN_B"
        echo "(B) is faster than OXIGEN_BIN (A); \`> 1.00\` means slower."
        echo
        echo "| benchmark | A min | A p50 | B min | B p50 | B/A min | B/A p50 |"
        echo "| --- | ---: | ---: | ---: | ---: | ---: | ---: |"
        for stem in "${benchmarks[@]}"; do
            summarize_ab "$stem" \
                | awk -F'\t' '{printf "| %s | %s | %s | %s | %s | %s | %s |\n", $1,$2,$3,$4,$5,$6,$7}'
        done
        echo
    fi
    echo "Per-benchmark JSON (per-round samples + summary stats) in \`$REPORT_DIR/\`."
} | tee "$md" > "$latest_md"

cp -f "$md" "$latest_md"

echo
echo "Native report: $md"
echo "Latest:        $latest_md"
