# OxigenLang Neovim Support

Neovim integration for OxigenLang (`.oxi` files) with LSP support.

## Installation

### 1. Build and install the LSP server

```bash
# From the repo root
cargo build --release -p oxigen-lsp

# Install to ~/.oxigen/bin/ (alongside the interpreter)
./scripts/install.sh --with-lsp

# Or install system-wide
sudo make install-lsp
```

Ensure `oxigen-lsp` is on your `$PATH`.

### 2. Neovim setup

#### Using lazy.nvim

Add the `editors/neovim` directory as a local plugin for filetype detection and syntax highlighting:

```lua
{
    dir = "/path/to/OxigenLang/editors/neovim",
    ft = "oxigen",
}
```

#### Manual filetype detection

Add to your Neovim config (`init.lua`):

```lua
vim.filetype.add({
    extension = {
        oxi = "oxigen",
    },
})
```

### 3. LSP configuration

#### Using nvim-lspconfig

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

-- Register the oxigen LSP server
if not configs.oxigen_lsp then
    configs.oxigen_lsp = {
        default_config = {
            cmd = { 'oxigen-lsp' },
            filetypes = { 'oxigen' },
            root_dir = lspconfig.util.root_pattern('.git'),
            settings = {},
        },
    }
end

lspconfig.oxigen_lsp.setup({})
```

#### Using mason.nvim (custom registry)

If you prefer mason.nvim for managing the LSP server:

```lua
-- In your mason config, after oxigen-lsp is published to crates.io:
require('mason-lspconfig').setup({
    ensure_installed = { 'oxigen_lsp' },
})
```

For local development before crates.io publishing, use the lspconfig approach above.

## Features

The LSP server provides:

- **Diagnostics** — Real-time parse error reporting with hints
- **Completion** — Keywords, builtin functions, type names, and stdlib modules
- **Hover** — Documentation for keywords, builtins, and types
- **Document Symbols** — Outline of functions, structs, patterns, and variables

## Syntax Highlighting

The included `syntax/oxigen.lua` provides basic syntax highlighting for:

- Keywords and control flow
- Builtin functions
- String literals and interpolation
- Numbers and floats
- Comments (`//`)
- Type annotations (`<int>`, `<str>`, etc.)
- Operators (`:=`, `->`, `||`, etc.)
