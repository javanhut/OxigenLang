#!/usr/bin/env bash
set -e

# ── OxigenLang Installer ──

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/.." && pwd)"
WORKSPACE_MANIFEST="$REPO_ROOT/Cargo.toml"
TARGET_DIR="$REPO_ROOT/target/release"
STDLIB_SRC_DIR="$REPO_ROOT/stdlib"

INSTALL_DIR="$HOME/.oxigen"
BIN_DIR="$INSTALL_DIR/bin"
LIB_DIR="$INSTALL_DIR/lib/stdlib"
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

# Build
echo "Building oxigen..."
cargo build --manifest-path "$WORKSPACE_MANIFEST" --release -p oxigen

if [ "$WITH_LSP" = true ]; then
    echo "Building oxigen-lsp..."
    cargo build --manifest-path "$WORKSPACE_MANIFEST" --release -p oxigen-lsp
fi

# Create directories
echo "Installing to $INSTALL_DIR..."
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

# Detect shell and rc file
add_to_path() {
    local rc_file="$1"
    local path_line="export PATH=\"$BIN_DIR:\$PATH\""
    local marker="# OxigenLang"

    if [ -f "$rc_file" ] && grep -q "$marker" "$rc_file"; then
        echo "  Path already configured in $rc_file"
        return
    fi

    echo "" >> "$rc_file"
    echo "$marker" >> "$rc_file"
    echo "$path_line" >> "$rc_file"
    echo "  Added oxigen to PATH in $rc_file"
}

SHELL_NAME="$(basename "$SHELL")"
case "$SHELL_NAME" in
    zsh)
        add_to_path "$HOME/.zshrc"
        ;;
    bash)
        # macOS uses .bash_profile, Linux uses .bashrc
        if [ "$(uname)" = "Darwin" ]; then
            add_to_path "$HOME/.bash_profile"
        else
            add_to_path "$HOME/.bashrc"
        fi
        ;;
    fish)
        FISH_CONFIG="$HOME/.config/fish/config.fish"
        mkdir -p "$(dirname "$FISH_CONFIG")"
        if [ -f "$FISH_CONFIG" ] && grep -q "OxigenLang" "$FISH_CONFIG"; then
            echo "  Path already configured in $FISH_CONFIG"
        else
            echo "" >> "$FISH_CONFIG"
            echo "# OxigenLang" >> "$FISH_CONFIG"
            echo "set -gx PATH $BIN_DIR \$PATH" >> "$FISH_CONFIG"
            echo "  Added oxigen to PATH in $FISH_CONFIG"
        fi
        ;;
    *)
        echo "  Unknown shell: $SHELL_NAME"
        echo "  Add this to your shell config manually:"
        echo "    export PATH=\"$BIN_DIR:\$PATH\""
        ;;
esac

echo ""
echo "=== OxigenLang installed successfully ==="
echo ""
echo "  Binary:  $BIN_DIR/oxigen"
echo "  Stdlib:  $LIB_DIR/"
if [ "$WITH_LSP" = true ]; then
    echo "  LSP:     $BIN_DIR/oxigen-lsp"
fi
echo ""
echo "Restart your shell or run:"
echo "  export PATH=\"$BIN_DIR:\$PATH\""
echo ""
echo "Then try:"
echo "  oxigen --version"
echo "  oxigen path/to/script.oxi"
echo "  oxigen   # starts the REPL"
