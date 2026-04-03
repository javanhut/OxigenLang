#!/usr/bin/env bash
set -e

# ── OxigenLang Installer ──

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/.." && pwd)"
WORKSPACE_MANIFEST="$REPO_ROOT/Cargo.toml"
TARGET_DIR="$REPO_ROOT/target/release"
STDLIB_SRC_DIR="$REPO_ROOT/stdlib"

PREFIX="${PREFIX:-/usr/local}"
BIN_DIR="$PREFIX/bin"
LIB_DIR="$PREFIX/lib/oxigen/stdlib"
WITH_LSP=false

# Parse arguments
for arg in "$@"; do
    case "$arg" in
        --with-lsp) WITH_LSP=true ;;
    esac
done

echo "=== OxigenLang Installer ==="
echo ""

# Check for Rust/Cargo
if ! command -v cargo &> /dev/null; then
    echo "Error: Rust is not installed."
    echo "Install it from https://rustup.rs and try again."
    exit 1
fi

# Check write permissions
if [ ! -w "$BIN_DIR" ]; then
    echo "Error: No write permission to $BIN_DIR"
    echo "Run with sudo or set PREFIX to a writable location:"
    echo "  sudo ./scripts/install.sh"
    echo "  PREFIX=~/.local ./scripts/install.sh"
    exit 1
fi

# Build
echo "Building oxigen..."
cargo build --manifest-path "$WORKSPACE_MANIFEST" --release -p oxigen

if [ "$WITH_LSP" = true ]; then
    echo "Building oxigen-lsp..."
    (cd "$REPO_ROOT/lsp-go" && go build -o "$TARGET_DIR/oxigen-lsp" .)
fi

# Create directories
echo "Installing to $PREFIX..."
mkdir -p "$BIN_DIR"
mkdir -p "$LIB_DIR"

# Copy binary and stdlib
cp "$TARGET_DIR/oxigen" "$BIN_DIR/oxigen"
chmod 755 "$BIN_DIR/oxigen"
cp "$STDLIB_SRC_DIR"/*.oxi "$LIB_DIR/"

if [ "$WITH_LSP" = true ]; then
    cp "$TARGET_DIR/oxigen-lsp" "$BIN_DIR/oxigen-lsp"
    chmod 755 "$BIN_DIR/oxigen-lsp"
fi

# Clean up build artifacts
echo "Cleaning up build artifacts..."
rm -rf "$REPO_ROOT/target"

echo ""
echo "=== OxigenLang installed successfully ==="
echo ""
echo "  Binary:  $BIN_DIR/oxigen"
echo "  Stdlib:  $LIB_DIR/"
if [ "$WITH_LSP" = true ]; then
    echo "  LSP:     $BIN_DIR/oxigen-lsp"
fi
echo ""
echo "Then try:"
echo "  oxigen --version"
echo "  oxigen path/to/script.oxi"
echo "  oxigen   # starts the REPL"
