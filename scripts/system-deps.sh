#!/usr/bin/env bash
# Make sure everything the website build needs from the system is installed,
# and install whatever is missing with rvn. `imlazy site` depends on this, so
# a fresh checkout needs nothing done by hand.
#
# Checked by what the build actually looks for -- a command on PATH or a Rust
# target in the sysroot -- rather than by package name, so it costs a few
# milliseconds when all is well and also counts things installed some other
# way (a rustup toolchain with the target added, say).
#
#   scripts/system-deps.sh           install what is missing
#   scripts/system-deps.sh --check   only report; exit 1 if anything is missing
set -euo pipefail

check_only=false
[[ ${1:-} == --check ]] && check_only=true

# package | how to tell it is there
requirements=(
    "rust|cmd:cargo"
    # The playground runs the interpreter compiled to WebAssembly.
    "rust-wasm|target:wasm32-unknown-unknown"
    # wasm-opt shrinks the playground module by about a third.
    "binaryen|cmd:wasm-opt"
)

present() {
    local probe=$1
    case $probe in
        cmd:*) command -v "${probe#cmd:}" >/dev/null 2>&1 ;;
        target:*)
            command -v rustc >/dev/null 2>&1 &&
                [[ -d "$(rustc --print sysroot)/lib/rustlib/${probe#target:}" ]]
            ;;
    esac
}

missing=()
for requirement in "${requirements[@]}"; do
    package=${requirement%%|*}
    probe=${requirement#*|}
    present "$probe" || missing+=("$package")
done

if ((${#missing[@]} == 0)); then
    exit 0
fi

echo "The OxigenLang website build needs: ${missing[*]}"
if $check_only; then
    exit 1
fi

if command -v rustup >/dev/null 2>&1 && [[ " ${missing[*]} " == *" rust-wasm "* ]]; then
    # A rustup toolchain ignores the system package; add the target to it.
    rustup target add wasm32-unknown-unknown
    rest=()
    for package in "${missing[@]}"; do
        [[ $package == rust-wasm ]] || rest+=("$package")
    done
    missing=("${rest[@]}")
    ((${#missing[@]} == 0)) && exit 0
fi

if ! command -v rvn >/dev/null 2>&1; then
    echo "rvn is not available; install these packages with your package manager." >&2
    exit 1
fi

# System packages need root. Official repositories only: these are all there,
# and nothing here should quietly build something from the AUR.
if ((EUID == 0)); then
    rvn install --repo-only --yes "${missing[@]}"
else
    sudo rvn install --repo-only --yes "${missing[@]}"
fi

# Say so if the install did not actually provide what was missing, rather than
# letting the build fail with a missing-target error further down.
still=()
for requirement in "${requirements[@]}"; do
    present "${requirement#*|}" || still+=("${requirement%%|*}")
done
if ((${#still[@]} > 0)); then
    echo "Still missing after install: ${still[*]}" >&2
    exit 1
fi
echo "System dependencies installed."
