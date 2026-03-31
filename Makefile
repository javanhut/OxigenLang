PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin
LIBDIR = $(PREFIX)/lib/oxigen/stdlib

.PHONY: build install uninstall clean

build:
	cargo build --release

install:
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

uninstall:
	@echo "Uninstalling oxigen..."
	rm -f $(BINDIR)/oxigen
	rm -rf $(PREFIX)/lib/oxigen
	@echo "Oxigen uninstalled."

clean:
	cargo clean
