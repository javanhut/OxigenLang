package main

import (
	"os"
	"path/filepath"
	"testing"
)

// resetStdlibCache clears the global stdlib cache so tests don't leak state
// into each other.
func resetStdlibCache() {
	stdlibMu.Lock()
	defer stdlibMu.Unlock()
	stdlibSig = ""
	stdlibCache = nil
	stdlibFuncMap = nil
}

func writeModule(t *testing.T, dir, name, body string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
}

func TestStdlibCachePicksUpNewModules(t *testing.T) {
	resetStdlibCache()
	t.Cleanup(resetStdlibCache)

	dir := t.TempDir()
	writeModule(t, dir, "alpha.oxi", "fun one() {}\n")

	modules := getStdlib(dir)
	if len(modules) != 1 || modules[0].name != "alpha" {
		t.Fatalf("expected [alpha], got %v", modules)
	}

	// Add a new module after the first scan: it must appear without any
	// manual cache reset.
	writeModule(t, dir, "beta.oxi", "fun two() {}\n")

	modules = getStdlib(dir)
	if len(modules) != 2 || modules[0].name != "alpha" || modules[1].name != "beta" {
		t.Fatalf("expected [alpha beta], got %v", modules)
	}

	funcMap := getStdlibFuncMap(dir)
	if _, ok := funcMap["beta"]; !ok {
		t.Fatalf("function map missing new module beta: %v", funcMap)
	}
}

func TestStdlibCachePicksUpRemovedModules(t *testing.T) {
	resetStdlibCache()
	t.Cleanup(resetStdlibCache)

	dir := t.TempDir()
	writeModule(t, dir, "alpha.oxi", "fun one() {}\n")
	writeModule(t, dir, "beta.oxi", "fun two() {}\n")

	if modules := getStdlib(dir); len(modules) != 2 {
		t.Fatalf("expected 2 modules, got %v", modules)
	}

	if err := os.Remove(filepath.Join(dir, "beta.oxi")); err != nil {
		t.Fatal(err)
	}

	modules := getStdlib(dir)
	if len(modules) != 1 || modules[0].name != "alpha" {
		t.Fatalf("expected [alpha] after removal, got %v", modules)
	}

	funcMap := getStdlibFuncMap(dir)
	if _, ok := funcMap["beta"]; ok {
		t.Fatalf("function map still has removed module beta: %v", funcMap)
	}
}
