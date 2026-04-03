#!/usr/bin/env bash
set -e

# ── OxigenLang Uninstaller ──

PREFIX="${PREFIX:-/usr/local}"
BIN_DIR="$PREFIX/bin"
LIB_DIR="$PREFIX/lib/oxigen"

echo "=== OxigenLang Uninstaller ==="
echo ""

# Check write permissions
if [ ! -w "$BIN_DIR" ]; then
    echo "Error: No write permission to $BIN_DIR"
    echo "Run with sudo: sudo ./scripts/uninstall.sh"
    exit 1
fi

# Remove binary
if [ -f "$BIN_DIR/oxigen" ]; then
    rm -f "$BIN_DIR/oxigen"
    echo "  Removed $BIN_DIR/oxigen"
else
    echo "  $BIN_DIR/oxigen not found, skipping."
fi

# Remove LSP binary
if [ -f "$BIN_DIR/oxigen-lsp" ]; then
    rm -f "$BIN_DIR/oxigen-lsp"
    echo "  Removed $BIN_DIR/oxigen-lsp"
fi

# Remove stdlib
if [ -d "$LIB_DIR" ]; then
    rm -rf "$LIB_DIR"
    echo "  Removed $LIB_DIR"
else
    echo "  $LIB_DIR not found, skipping."
fi

# Also clean up legacy ~/.oxigen install if present
LEGACY_DIR="$HOME/.oxigen"
if [ -d "$LEGACY_DIR" ]; then
    echo "  Removing legacy install at $LEGACY_DIR..."
    rm -rf "$LEGACY_DIR"
    echo "  Removed."
fi

# Clean legacy shell config entries
remove_from_rc() {
    local rc_file="$1"
    if [ -f "$rc_file" ] && grep -q "# OxigenLang" "$rc_file"; then
        sed -i.bak '/# OxigenLang/,+1d' "$rc_file"
        sed -i.bak -e :a -e '/^\n*$/{$d;N;ba' -e '}' "$rc_file"
        rm -f "${rc_file}.bak"
        echo "  Removed legacy PATH entry from $rc_file"
    fi
}

remove_from_rc "$HOME/.bashrc"
remove_from_rc "$HOME/.bash_profile"
remove_from_rc "$HOME/.zshrc"

FISH_CONFIG="$HOME/.config/fish/config.fish"
if [ -f "$FISH_CONFIG" ] && grep -q "OxigenLang" "$FISH_CONFIG"; then
    sed -i.bak '/# OxigenLang/,+1d' "$FISH_CONFIG"
    rm -f "${FISH_CONFIG}.bak"
    echo "  Removed legacy PATH entry from $FISH_CONFIG"
fi

echo ""
echo "=== OxigenLang uninstalled ==="
