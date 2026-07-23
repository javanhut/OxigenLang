package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"sync"
)

type moduleInfo struct {
	name      string
	functions []functionInfo
}

type functionInfo struct {
	name      string
	signature string
}

// stdlib discovery is cached, but the cache is invalidated whenever the set of
// .oxi files in the stdlib directory changes (added, removed, or edited), so
// newly installed or modified modules are picked up without a server restart.
var (
	stdlibMu      sync.Mutex
	stdlibSig     string
	stdlibCache   []moduleInfo
	stdlibFuncMap map[string][]functionInfo
)

var funDeclRe = regexp.MustCompile(`^fun\s+(\w+)\s*\(([^)]*)\)`)

func findOxigenBinary() string {
	if exe, err := os.Executable(); err == nil {
		dir := filepath.Dir(exe)
		candidate := filepath.Join(dir, "oxigen")
		if _, err := os.Stat(candidate); err == nil {
			return candidate
		}
	}
	if path, err := exec.LookPath("oxigen"); err == nil {
		return path
	}
	return "oxigen"
}

func findStdlibPath() string {
	// 1. Relative to LSP executable
	if exe, err := os.Executable(); err == nil {
		dir := filepath.Dir(exe)
		candidate := filepath.Join(dir, "stdlib")
		if isDir(candidate) {
			return candidate
		}
		// 2. System install: <prefix>/lib/oxigen/stdlib
		prefix := filepath.Dir(dir)
		candidate = filepath.Join(prefix, "lib", "oxigen", "stdlib")
		if isDir(candidate) {
			return candidate
		}
	}
	// 3. Current working directory
	if isDir("stdlib") {
		return "stdlib"
	}
	// 4. Parent of current working directory
	if isDir(filepath.Join("..", "stdlib")) {
		return filepath.Join("..", "stdlib")
	}
	// 5. User install: ~/.oxigen/lib/stdlib
	if home, err := os.UserHomeDir(); err == nil {
		candidate := filepath.Join(home, ".oxigen", "lib", "stdlib")
		if isDir(candidate) {
			return candidate
		}
	}
	return ""
}

func isDir(path string) bool {
	info, err := os.Stat(path)
	return err == nil && info.IsDir()
}

func discoverStdlib(stdlibPath string) []moduleInfo {
	if stdlibPath == "" {
		return nil
	}

	entries, err := os.ReadDir(stdlibPath)
	if err != nil {
		return nil
	}

	var modules []moduleInfo
	for _, entry := range entries {
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".oxi") {
			continue
		}

		moduleName := strings.TrimSuffix(entry.Name(), ".oxi")
		path := filepath.Join(stdlibPath, entry.Name())
		modules = append(modules, moduleInfo{name: moduleName, functions: parseModuleFuncs(path)})
	}

	sort.Slice(modules, func(i, j int) bool {
		return modules[i].name < modules[j].name
	})

	return modules
}

// stdlibSignature fingerprints the .oxi files in the stdlib directory by name,
// size, and modification time. Any module that is added, removed, or edited
// changes the signature.
func stdlibSignature(stdlibPath string) (string, bool) {
	entries, err := os.ReadDir(stdlibPath)
	if err != nil {
		return "", false
	}

	var b strings.Builder
	for _, entry := range entries {
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".oxi") {
			continue
		}
		info, err := entry.Info()
		if err != nil {
			continue
		}
		fmt.Fprintf(&b, "%s:%d:%d;", entry.Name(), info.Size(), info.ModTime().UnixNano())
	}
	return b.String(), true
}

func buildFuncMap(modules []moduleInfo) map[string][]functionInfo {
	funcMap := make(map[string][]functionInfo, len(modules))
	for _, mod := range modules {
		funcMap[mod.name] = mod.functions
	}
	return funcMap
}

// refreshStdlib re-scans the stdlib directory if its contents changed since
// the last scan. Callers must not hold stdlibMu.
func refreshStdlib(stdlibPath string) {
	if stdlibPath == "" {
		return
	}
	sig, ok := stdlibSignature(stdlibPath)
	if !ok {
		return
	}

	stdlibMu.Lock()
	defer stdlibMu.Unlock()
	if sig == stdlibSig {
		return
	}
	stdlibCache = discoverStdlib(stdlibPath)
	stdlibFuncMap = buildFuncMap(stdlibCache)
	stdlibSig = sig
}

func getStdlib(stdlibPath string) []moduleInfo {
	refreshStdlib(stdlibPath)
	stdlibMu.Lock()
	defer stdlibMu.Unlock()
	return stdlibCache
}

func getStdlibFuncMap(stdlibPath string) map[string][]functionInfo {
	refreshStdlib(stdlibPath)
	stdlibMu.Lock()
	defer stdlibMu.Unlock()
	return stdlibFuncMap
}
