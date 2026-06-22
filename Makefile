PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin
LIBDIR = $(PREFIX)/lib/oxigen/stdlib

.PHONY: build build-with-jit build-no-jit build-lsp install install-with-jit install-no-jit install-lsp install-all uninstall clean

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
	@echo "oxigen-lsp installed successfully."
	@echo "  Binary: $(BINDIR)/oxigen-lsp"

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
