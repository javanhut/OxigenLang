PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin
LIBDIR = $(PREFIX)/lib/oxigen/stdlib

.PHONY: build build-lsp install install-lsp install-all uninstall clean

build:
	cargo build --release -p oxigen

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
