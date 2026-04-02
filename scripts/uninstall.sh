#!/usr/bin/env bash
set -e

# ── OxigenLang Uninstaller ──

INSTALL_DIR="$HOME/.oxigen"

echo "=== OxigenLang Uninstaller ==="
echo ""

# Remove installation directory
if [ -d "$INSTALL_DIR" ]; then
    echo "Removing $INSTALL_DIR..."
    rm -rf "$INSTALL_DIR"
    echo "  Removed."
else
    echo "  $INSTALL_DIR not found, skipping."
fi

# Clean shell config files
remove_from_rc() {
    local rc_file="$1"
    if [ -f "$rc_file" ] && grep -q "# OxigenLang" "$rc_file"; then
        # Remove the marker line and the line after it (the export)
        sed -i.bak '/# OxigenLang/,+1d' "$rc_file"
        # Remove any trailing blank lines we left behind
        sed -i.bak -e :a -e '/^\n*$/{$d;N;ba' -e '}' "$rc_file"
        rm -f "${rc_file}.bak"
        echo "  Removed oxigen from $rc_file"
    fi
}

remove_from_rc "$HOME/.bashrc"
remove_from_rc "$HOME/.bash_profile"
remove_from_rc "$HOME/.zshrc"

# Handle fish separately
FISH_CONFIG="$HOME/.config/fish/config.fish"
if [ -f "$FISH_CONFIG" ] && grep -q "OxigenLang" "$FISH_CONFIG"; then
    sed -i.bak '/# OxigenLang/,+1d' "$FISH_CONFIG"
    rm -f "${FISH_CONFIG}.bak"
    echo "  Removed oxigen from $FISH_CONFIG"
fi

echo ""
echo "=== OxigenLang uninstalled ==="
echo ""
echo "Restart your shell to complete removal."
