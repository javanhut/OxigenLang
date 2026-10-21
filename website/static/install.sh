#!/bin/sh
# Install OxigenLang from a prebuilt release.
#
#   curl -fsSL https://www.oxigenlang.org/install.sh | sh
#
# Works out your operating system and CPU, downloads the matching build of the
# latest release from GitHub, checks its SHA-256, and installs
#
#   <prefix>/bin/oxigen
#   <prefix>/lib/oxigen/stdlib/
#
# where <prefix> is ~/.local, or /usr/local when run as root.
#
# Options (after `sh -s --` when piping):
#   --version X.Y.Z   install that release instead of the latest
#   --prefix DIR      install under DIR
#   --uninstall       remove what this script installed under the prefix
#   --help
#
# The environment can say the same: OXIGEN_VERSION, OXIGEN_PREFIX. Builds exist
# for Linux and macOS on x86-64 and ARM64; the Linux ones are static, so they
# run on any distribution. Anywhere else, build from source:
# https://www.oxigenlang.org/docs/getting-started/
#
# Everything happens inside main(), called on the last line, so a download cut
# off part way through runs nothing.

set -eu

REPO="javanhut/OxigenLang"
DOWNLOADS="${OXIGEN_DOWNLOAD_BASE:-https://github.com/$REPO/releases/download}"

say() { printf '%s\n' "$*"; }
fail() {
    printf 'oxigen install: %s\n' "$*" >&2
    exit 1
}

usage() {
    sed -n '2,24p' "$0" 2>/dev/null | sed 's/^# \{0,1\}//' || true
    say "Usage: install.sh [--version X.Y.Z] [--prefix DIR] [--uninstall]"
}

# The release file suffix for this machine, such as linux_x86 or macos_arm64.
platform() {
    os=$(uname -s)
    arch=$(uname -m)
    case "$os" in
        Linux) os=linux ;;
        Darwin) os=macos ;;
        MINGW* | MSYS* | CYGWIN* | Windows_NT)
            fail "there is no native Windows build yet. Install WSL (https://learn.microsoft.com/windows/wsl/install) and run this in its terminal."
            ;;
        *) fail "no prebuilt OxigenLang for $os. Build it from source: https://www.oxigenlang.org/docs/getting-started/" ;;
    esac
    case "$arch" in
        x86_64 | amd64) arch=x86 ;;
        aarch64 | arm64) arch=arm64 ;;
        *) fail "no prebuilt OxigenLang for $os on $arch. Build it from source: https://www.oxigenlang.org/docs/getting-started/" ;;
    esac
    # A terminal running under Rosetta reports x86_64 on Apple silicon; the
    # native build is the one to install.
    if [ "$os" = macos ] && [ "$arch" = x86 ] &&
        [ "$(sysctl -n sysctl.proc_translated 2>/dev/null || echo 0)" = 1 ]; then
        arch=arm64
    fi
    printf '%s_%s' "$os" "$arch"
}

# Fetch URL $1 to file $2, or to stdout without $2.
fetch() {
    if command -v curl >/dev/null 2>&1; then
        if [ $# -ge 2 ]; then curl -fsSL --retry 3 -o "$2" "$1"; else curl -fsSL --retry 3 "$1"; fi
    elif command -v wget >/dev/null 2>&1; then
        if [ $# -ge 2 ]; then wget -q -O "$2" "$1"; else wget -q -O - "$1"; fi
    else
        fail "this needs curl or wget to download OxigenLang"
    fi
}

# The newest release's version. The releases/latest page redirects to the
# tag, which avoids the API's rate limit; wget falls back to the API.
latest_version() {
    if command -v curl >/dev/null 2>&1; then
        url=$(curl -fsSLI -o /dev/null -w '%{url_effective}' "https://github.com/$REPO/releases/latest") ||
            fail "could not reach GitHub to find the latest release"
        tag=${url##*/}
    else
        tag=$(fetch "https://api.github.com/repos/$REPO/releases/latest" |
            sed -n 's/.*"tag_name": *"\([^"]*\)".*/\1/p' | head -n 1)
    fi
    case "$tag" in
        v[0-9]*) printf '%s' "${tag#v}" ;;
        *) fail "could not work out the latest release (got '$tag')" ;;
    esac
}

sha256() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | cut -d ' ' -f 1
    elif command -v shasum >/dev/null 2>&1; then
        shasum -a 256 "$1" | cut -d ' ' -f 1
    fi
}

# The published SHA-256 of release file $2 in tag $1: from the release's
# SHA256SUMS file, or else from the digest GitHub records for every upload.
expected_sha256() {
    sums=$(fetch "$DOWNLOADS/$1/SHA256SUMS" 2>/dev/null || true)
    sum=$(printf '%s\n' "$sums" | awk -v f="$2" '$2 == f || $2 == "*" f { print $1; exit }')
    if [ -z "$sum" ]; then
        sum=$(fetch "https://api.github.com/repos/$REPO/releases/tags/$1" 2>/dev/null |
            awk -v f="\"$2\"" '
                index($0, "\"name\": " f) { found = 1 }
                found && /"digest":/ { sub(/.*"sha256:/, ""); sub(/".*/, ""); print; exit }
            ' || true)
    fi
    printf '%s' "$sum"
}

# How people name platform $1 (linux_x86 and so on).
describe() {
    case "$1" in
        linux_x86) say "Linux x86-64" ;;
        linux_arm64) say "Linux ARM64" ;;
        macos_x86) say "macOS on Intel" ;;
        macos_arm64) say "macOS on Apple silicon" ;;
        *) say "$1" ;;
    esac
}

on_path() {
    case ":$PATH:" in
        *":$1:"*) return 0 ;;
        *) return 1 ;;
    esac
}

uninstall() {
    prefix=$1
    removed=no
    if [ -e "$prefix/bin/oxigen" ]; then
        rm -f "$prefix/bin/oxigen"
        removed=yes
    fi
    if [ -d "$prefix/lib/oxigen" ]; then
        rm -rf "$prefix/lib/oxigen"
        removed=yes
    fi
    if [ "$removed" = yes ]; then
        say "Removed OxigenLang from $prefix."
    else
        say "OxigenLang is not installed under $prefix."
    fi
}

main() {
    version="${OXIGEN_VERSION:-}"
    prefix="${OXIGEN_PREFIX:-}"
    action=install
    while [ $# -gt 0 ]; do
        case "$1" in
            --version) [ $# -ge 2 ] || fail "--version needs a version, such as 0.1.4"; version=$2; shift 2 ;;
            --version=*) version=${1#*=}; shift ;;
            --prefix) [ $# -ge 2 ] || fail "--prefix needs a directory"; prefix=$2; shift 2 ;;
            --prefix=*) prefix=${1#*=}; shift ;;
            --uninstall) action=uninstall; shift ;;
            -h | --help) usage; exit 0 ;;
            *) fail "unknown option '$1' (try --help)" ;;
        esac
    done
    if [ -z "$prefix" ]; then
        if [ "$(id -u)" = 0 ]; then prefix=/usr/local; else prefix="$HOME/.local"; fi
    fi

    if [ "$action" = uninstall ]; then
        uninstall "$prefix"
        return
    fi

    target=$(platform)
    version=${version#v}
    [ -n "$version" ] || version=$(latest_version)
    tag="v$version"
    name="oxigen_lang_$(printf '%s' "$tag" | tr . _)_$target"
    file="$name.tar.gz"

    tmp=$(mktemp -d 2>/dev/null || mktemp -d -t oxigen)
    trap 'rm -rf "$tmp"' EXIT INT TERM

    say "Installing OxigenLang $version for $(describe "$target")..."
    fetch "$DOWNLOADS/$tag/$file" "$tmp/$file" ||
        fail "could not download $file. Is $version a released version? See https://github.com/$REPO/releases"

    want=$(expected_sha256 "$tag" "$file")
    have=$(sha256 "$tmp/$file")
    if [ -z "$have" ]; then
        say "warning: no sha256sum or shasum here, so the download was not verified."
    elif [ -z "$want" ]; then
        say "warning: GitHub published no checksum for $file, so the download was not verified."
    elif [ "$want" != "$have" ]; then
        fail "$file does not match its published SHA-256 (expected $want, got $have). Nothing was installed."
    else
        say "Verified SHA-256 $have"
    fi

    tar -xzf "$tmp/$file" -C "$tmp" || fail "$file is not a valid archive"
    [ -x "$tmp/$name/oxigen" ] && [ -d "$tmp/$name/stdlib" ] || fail "$file does not contain oxigen and its stdlib"
    # Before anything is replaced: a build that cannot run here changes nothing.
    "$tmp/$name/oxigen" --version >/dev/null 2>&1 ||
        fail "the $(describe "$target") build does not run on this machine. Nothing was installed."

    mkdir -p "$prefix/bin" "$prefix/lib/oxigen" 2>/dev/null ||
        fail "cannot write to $prefix. Run as root, or choose a directory with --prefix."
    [ -w "$prefix/bin" ] && [ -w "$prefix/lib/oxigen" ] ||
        fail "cannot write to $prefix. Run as root, or choose a directory with --prefix."

    # Copy beside the old files and rename over them, so a failed copy leaves
    # the current install working.
    cp "$tmp/$name/oxigen" "$prefix/bin/.oxigen.new"
    chmod 755 "$prefix/bin/.oxigen.new"
    rm -rf "$prefix/lib/oxigen/.stdlib.new"
    cp -R "$tmp/$name/stdlib" "$prefix/lib/oxigen/.stdlib.new"
    mv -f "$prefix/bin/.oxigen.new" "$prefix/bin/oxigen"
    rm -rf "$prefix/lib/oxigen/stdlib"
    mv "$prefix/lib/oxigen/.stdlib.new" "$prefix/lib/oxigen/stdlib"

    "$prefix/bin/oxigen" --version >/dev/null 2>&1 || fail "installed $prefix/bin/oxigen, but it does not run"
    say "Installed OxigenLang $version to $prefix/bin/oxigen"

    if ! on_path "$prefix/bin"; then
        say ""
        say "$prefix/bin is not on your PATH. Add it with:"
        case "${SHELL:-}" in
            */fish) say "  fish_add_path $prefix/bin" ;;
            */zsh) say "  echo 'export PATH=\"$prefix/bin:\$PATH\"' >> ~/.zshrc" ;;
            *) say "  echo 'export PATH=\"$prefix/bin:\$PATH\"' >> ~/.profile" ;;
        esac
        say "then open a new terminal."
    else
        found=$(command -v oxigen 2>/dev/null || true)
        if [ -n "$found" ] && [ "$found" != "$prefix/bin/oxigen" ]; then
            say ""
            say "note: $found comes first on your PATH, so \`oxigen\` still runs that copy."
        fi
    fi
    say ""
    say "Try it:  oxigen --help    Docs: https://www.oxigenlang.org/docs/"
}

main "$@"
