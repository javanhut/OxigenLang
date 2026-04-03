package main

import (
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

var (
	stdlibCache     []moduleInfo
	stdlibCacheOnce sync.Once
	stdlibFuncMap   map[string][]functionInfo
	stdlibFuncOnce  sync.Once
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
	// 3. User install: ~/.oxigen/lib/stdlib
	if home, err := os.UserHomeDir(); err == nil {
		candidate := filepath.Join(home, ".oxigen", "lib", "stdlib")
		if isDir(candidate) {
			return candidate
		}
	}
	// 4. Current working directory
	if isDir("stdlib") {
		return "stdlib"
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

		source, err := os.ReadFile(path)
		if err != nil {
			continue
		}

		var functions []functionInfo
		for _, line := range strings.Split(string(source), "\n") {
			if matches := funDeclRe.FindStringSubmatch(line); matches != nil {
				name := matches[1]
				params := matches[2]
				sig := name + "(" + params + ")"
				functions = append(functions, functionInfo{name: name, signature: sig})
			}
		}

		modules = append(modules, moduleInfo{name: moduleName, functions: functions})
	}

	sort.Slice(modules, func(i, j int) bool {
		return modules[i].name < modules[j].name
	})

	return modules
}

func getStdlib(stdlibPath string) []moduleInfo {
	stdlibCacheOnce.Do(func() {
		stdlibCache = discoverStdlib(stdlibPath)
	})
	return stdlibCache
}

func getStdlibFuncMap(stdlibPath string) map[string][]functionInfo {
	stdlibFuncOnce.Do(func() {
		stdlibFuncMap = make(map[string][]functionInfo)
		for _, mod := range getStdlib(stdlibPath) {
			stdlibFuncMap[mod.name] = mod.functions
		}
	})
	return stdlibFuncMap
}
