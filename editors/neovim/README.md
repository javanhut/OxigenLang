# OxigenLang Neovim Support

Neovim integration for OxigenLang (`.oxi` files) with LSP, formatting, and completion support.

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

### 2. Copy editor files

Copy the filetype detection, syntax highlighting, and LSP config files into your Neovim config directory:

```bash
# From the repo root
mkdir -p ~/.config/nvim/ftdetect ~/.config/nvim/syntax ~/.config/nvim/lsp
cp editors/neovim/ftdetect/oxigen.lua ~/.config/nvim/ftdetect/
cp editors/neovim/syntax/oxigen.lua ~/.config/nvim/syntax/
cp editors/neovim/lsp/oxigen_lsp.lua ~/.config/nvim/lsp/
```

Or use the provided script:

```bash
oxigen scripts/cp_to_neovim.oxi
```

> **Note:** These files must be placed directly in `~/.config/nvim/ftdetect/`, `~/.config/nvim/syntax/`, and `~/.config/nvim/lsp/` — not in a subdirectory. Neovim only discovers them from its runtimepath root.

### 3. LSP configuration

#### Neovim 0.11+ (native)

The `lsp/oxigen_lsp.lua` file you copied in step 2 is auto-discovered by Neovim 0.11+. Just enable it in your LSP config:

```lua
vim.lsp.enable("oxigen_lsp")
```

If you use NvChad, add `"oxigen_lsp"` to your servers list in `lua/configs/lspconfig.lua`:

```lua
local servers = {
  -- your other servers...
  "oxigen_lsp",
}
vim.lsp.enable(servers)
```

#### Neovim < 0.11 (nvim-lspconfig)

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

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

### 4. Formatting

The `oxigen fmt` command formats `.oxi` files in-place. To enable format-on-save with [conform.nvim](https://github.com/stevearc/conform.nvim):

```lua
require("conform").setup({
  formatters_by_ft = {
    oxigen = { "oxigen_fmt" },
  },
  formatters = {
    oxigen_fmt = {
      command = "oxigen",
      args = function(self, ctx)
        return { "fmt", ctx.filename }
      end,
      stdin = false,
    },
  },
})
```

The LSP server also supports `textDocument/formatting`, so if you prefer LSP-based formatting no extra setup is needed beyond enabling the LSP.

## Features

The LSP server provides:

- **Diagnostics** -- Real-time parse error reporting with hints
- **Completion** -- Keywords, builtin functions, type names, and stdlib modules
- **Hover** -- Documentation for keywords, builtins, and types
- **Document Symbols** -- Outline of functions, structs, patterns, and variables
- **Formatting** -- Format documents via LSP or `oxigen fmt`

## Syntax Highlighting

The included `syntax/oxigen.lua` provides basic syntax highlighting for:

- Keywords and control flow
- Builtin functions
- String literals and interpolation
- Numbers and floats
- Comments (`//`)
- Type annotations (`<int>`, `<str>`, etc.)
- Operators (`:=`, `->`, `||`, etc.)
