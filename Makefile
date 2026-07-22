# /usr/local when writable (root, sudo, Intel-mac Homebrew), else ~/.local so
# plain `make install` never needs sudo. Override with PREFIX=.
PREFIX ?= $(shell [ -w /usr/local/bin ] && echo /usr/local || echo $$HOME/.local)
BINDIR = $(PREFIX)/bin
LIBDIR = $(PREFIX)/lib/oxigen/stdlib

# Neovim config dir for `install-lsp-nvcrow`. Empty -> the target picks the
# invoking user's `~/.config/nvim` (the real user even under sudo). Override
# with `make install-lsp-nvcrow NVIM_CONFIG=/path/to/nvim`.
NVIM_CONFIG ?=

.PHONY: build build-with-jit build-no-jit build-lsp install install-with-jit install-no-jit install-lsp install-lsp-nvcrow install-all uninstall clean

# JIT is on by default (see core/Cargo.toml `default = ["jit"]`), so `make
# build`/`make install` produce the JIT-enabled, VM-improved binary.
build:
	cargo build --release -p oxigen

# Explicit JIT build — identical to `build` now that JIT is the default;
# kept for clarity / backward compatibility.
build-with-jit:
	cargo build --release -p oxigen --features jit

# VM-interpreter-only build (no JIT / cranelift). `--jit` has no effect on it.
build-no-jit:
	cargo build --release -p oxigen --no-default-features

build-lsp:
	cd lsp-go && go build -o ../target/release/oxigen-lsp .

install: build
	@echo "Installing oxigen to $(BINDIR)..."
	install -d $(BINDIR)
	install -m 755 target/release/oxigen $(BINDIR)/oxigen
	@echo "Installing stdlib to $(LIBDIR)..."
	install -d $(LIBDIR)
	install -m 644 stdlib/*.oxi $(LIBDIR)/
	@echo ""
	@echo "Oxigen installed successfully."
	@echo "  Binary: $(BINDIR)/oxigen"
	@echo "  Stdlib: $(LIBDIR)/"

install-with-jit: build-with-jit
	@echo "Installing oxigen with JIT enabled to $(BINDIR)..."
	install -d $(BINDIR)
	install -m 755 target/release/oxigen $(BINDIR)/oxigen
	@echo "Installing stdlib to $(LIBDIR)..."
	install -d $(LIBDIR)
	install -m 644 stdlib/*.oxi $(LIBDIR)/
	@echo ""
	@echo "Oxigen with JIT installed successfully."
	@echo "  Binary: $(BINDIR)/oxigen"
	@echo "  Stdlib: $(LIBDIR)/"

install-no-jit: build-no-jit
	@echo "Installing oxigen (VM-only, no JIT) to $(BINDIR)..."
	install -d $(BINDIR)
	install -m 755 target/release/oxigen $(BINDIR)/oxigen
	@echo "Installing stdlib to $(LIBDIR)..."
	install -d $(LIBDIR)
	install -m 644 stdlib/*.oxi $(LIBDIR)/
	@echo ""
	@echo "Oxigen (VM-only) installed successfully."
	@echo "  Binary: $(BINDIR)/oxigen"
	@echo "  Stdlib: $(LIBDIR)/"

install-lsp: build-lsp
	@echo "Installing oxigen-lsp to $(BINDIR)..."
	install -d $(BINDIR)
	install -m 755 target/release/oxigen-lsp $(BINDIR)/oxigen-lsp
	@echo "Installing stdlib to $(LIBDIR) (the LSP reads it for module completions)..."
	install -d $(LIBDIR)
	install -m 644 stdlib/*.oxi $(LIBDIR)/
	@echo "oxigen-lsp installed successfully."
	@echo "  Binary: $(BINDIR)/oxigen-lsp"
	@echo "  Stdlib: $(LIBDIR)/"

# One-shot setup for a Neovim / NvChad ("nvcrow") user: installs the oxigen-lsp
# binary (via install-lsp) AND drops the ftdetect, syntax, and lsp config files
# into the invoking user's Neovim config. Safe to run with sudo — the editor
# files go to the REAL user's ~/.config/nvim (via $SUDO_USER) and are chowned
# back to them, never left root-owned. Override the config dir with NVIM_CONFIG=.
install-lsp-nvcrow: install-lsp
	@user="$${SUDO_USER:-$$(id -un)}"; \
	home="$$(eval echo ~$$user)"; \
	cfg="$(NVIM_CONFIG)"; [ -z "$$cfg" ] && cfg="$$home/.config/nvim"; \
	echo "Installing Neovim/NvChad editor files for '$$user' to $$cfg..."; \
	install -d "$$cfg/ftdetect" "$$cfg/syntax" "$$cfg/lsp"; \
	install -m 644 editors/neovim/ftdetect/oxigen.lua "$$cfg/ftdetect/oxigen.lua"; \
	install -m 644 editors/neovim/syntax/oxigen.lua "$$cfg/syntax/oxigen.lua"; \
	install -m 644 editors/neovim/lsp/oxigen_lsp.lua "$$cfg/lsp/oxigen_lsp.lua"; \
	if [ -n "$$SUDO_USER" ]; then \
		chown "$$user" "$$cfg/ftdetect" "$$cfg/syntax" "$$cfg/lsp" \
			"$$cfg/ftdetect/oxigen.lua" "$$cfg/syntax/oxigen.lua" "$$cfg/lsp/oxigen_lsp.lua" 2>/dev/null || true; \
	fi; \
	echo ""; \
	echo "Oxigen LSP + Neovim files installed."; \
	echo "  Binary:   $(BINDIR)/oxigen-lsp"; \
	echo "  ftdetect: $$cfg/ftdetect/oxigen.lua"; \
	echo "  syntax:   $$cfg/syntax/oxigen.lua"; \
	echo "  lsp:      $$cfg/lsp/oxigen_lsp.lua"; \
	echo ""; \
	echo "Enable the server in your NvChad config (lua/configs/lspconfig.lua):"; \
	echo "    vim.lsp.enable(\"oxigen_lsp\")"; \
	echo "(or add \"oxigen_lsp\" to your servers list). Then restart Neovim or run"; \
	echo ":LspRestart and confirm with :LspInfo / :checkhealth lsp."

install-all: install install-lsp

uninstall:
	@echo "Uninstalling oxigen..."
	rm -f $(BINDIR)/oxigen
	rm -f $(BINDIR)/oxigen-lsp
	rm -rf $(PREFIX)/lib/oxigen
	@echo "Oxigen uninstalled."

clean:
	cargo clean
	rm -f lsp-go/oxigen-lsp
