package main

import (
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

// docIndex is a best-effort, regex-based model of the symbols declared in a
// single document: its structs (with fields, methods, and parents), enums,
// top-level functions, patterns, variables, and any local `.oxi` modules it
// imports. It powers document-aware completion — new structs/functions show up
// in the menu, `instance.` and `self.` complete struct members, and
// `localmod.` completes a local module's functions.
//
// This is a completion heuristic, not a parser; the Oxigen compiler remains the
// source of truth. ponytail: brace-counted block ranges ignore strings/comments
// and indentation-mode (`includes:`) blocks; upgrade to a real parse if it ever
// misleads.

type docMember struct {
	name   string
	detail string
}

type docStruct struct {
	name    string
	parent  string
	fields  []docMember
	methods []docMember
}

type includesBlock struct {
	structName string
	start, end int // [start, end) line indices the block spans
}

type docIndex struct {
	structs      map[string]*docStruct
	structList   []*docStruct // declaration order, for deterministic output
	enums        []docMember
	funcs        []docMember
	patterns     []docMember
	topVars      []docMember
	varTypes     map[string]string         // variable name -> struct type (best effort)
	localMods    map[string][]functionInfo // local import base name -> functions
	includesBlks []includesBlock
}

var (
	structParentRe = regexp.MustCompile(`^\s*struct\s+(\w+)\s*\(\s*(\w+)\s*\)`)
	funSigRe       = regexp.MustCompile(`^\s*fun\s+(\w+)\s*\(([^)]*)\)`)
	ctorVarRe      = regexp.MustCompile(`^\s*(\w+)\s*:?=\s*(\w+)\s*[({]`)
)

func indexDocument(source, uri string) *docIndex {
	idx := &docIndex{
		structs:   map[string]*docStruct{},
		varTypes:  map[string]string{},
		localMods: map[string][]functionInfo{},
	}
	lines := strings.Split(source, "\n")

	// ── Pass 1: declarations ──
	for i := 0; i < len(lines); i++ {
		line := lines[i]
		topLevel := countLeadingSpaces(line) == 0

		// struct Name { ... } / struct Name(Parent) { ... }
		if m := structRe.FindStringSubmatch(line); m != nil {
			st := idx.getOrAddStruct(m[1])
			if pm := structParentRe.FindStringSubmatch(line); pm != nil {
				st.parent = pm[2]
			}
			for j := i + 1; j < len(lines); j++ {
				field := strings.TrimSpace(lines[j])
				if field == "}" || field == "" {
					break
				}
				field = strings.TrimPrefix(field, "hidden ")
				field = strings.TrimPrefix(field, "hide ")
				parts := strings.Fields(field)
				if len(parts) >= 1 && isIdentStart(parts[0]) {
					st.fields = append(st.fields, docMember{name: parts[0], detail: field})
				}
			}
			continue
		}

		// enum Name { Variant(...) ... }
		if m := enumRe.FindStringSubmatch(line); m != nil {
			idx.enums = append(idx.enums, docMember{name: m[1], detail: "enum " + m[1]})
			continue
		}

		// StructName includes { ... } — method block.
		if m := includesRe.FindStringSubmatch(line); m != nil {
			sname := m[1]
			st := idx.getOrAddStruct(sname)
			depth := braceDelta(line)
			end := i + 1
			for j := i + 1; j < len(lines) && depth > 0; j++ {
				if fm := funSigRe.FindStringSubmatch(lines[j]); fm != nil {
					st.methods = append(st.methods, docMember{
						name:   fm[1],
						detail: fm[1] + "(" + strings.TrimSpace(fm[2]) + ")",
					})
				}
				depth += braceDelta(lines[j])
				end = j + 1
			}
			idx.includesBlks = append(idx.includesBlks, includesBlock{structName: sname, start: i, end: end})
			continue
		}

		// Top-level function.
		if topLevel {
			if m := funSigRe.FindStringSubmatch(line); m != nil {
				idx.funcs = append(idx.funcs, docMember{
					name:   m[1],
					detail: m[1] + "(" + strings.TrimSpace(m[2]) + ")",
				})
				continue
			}
		}

		// Pattern declaration.
		if m := patternRe.FindStringSubmatch(line); m != nil {
			idx.patterns = append(idx.patterns, docMember{name: m[1], detail: "pattern " + m[1]})
			continue
		}

		// Top-level variable (walrus or typed).
		if topLevel {
			if m := letRe.FindStringSubmatch(line); m != nil {
				idx.topVars = append(idx.topVars, docMember{name: m[1], detail: "variable"})
				continue
			}
			if m := typedLetRe.FindStringSubmatch(line); m != nil {
				trimmed := strings.TrimSpace(line)
				if !strings.HasPrefix(trimmed, "fun ") && !strings.HasPrefix(trimmed, "hide ") && !strings.HasPrefix(trimmed, "hidden ") {
					idx.topVars = append(idx.topVars, docMember{name: m[1], detail: "variable <" + m[2] + ">"})
				}
			}
		}
	}

	// ── Pass 2: infer variable -> struct type bindings ──
	for _, line := range lines {
		if m := ctorVarRe.FindStringSubmatch(line); m != nil {
			if _, ok := idx.structs[m[2]]; ok {
				idx.varTypes[m[1]] = m[2]
			}
		}
		if m := typedLetRe.FindStringSubmatch(line); m != nil {
			if _, ok := idx.structs[m[2]]; ok {
				idx.varTypes[m[1]] = m[2]
			}
		}
	}

	// ── Pass 3: resolve local `.oxi` module imports ──
	docPath := uriToPath(uri)
	if docPath != "" {
		for _, line := range lines {
			m := introduceRe.FindStringSubmatch(line)
			if m == nil {
				continue
			}
			spec := strings.TrimSpace(m[1])
			// Whole-module local import only: `.mylib`, `..utils.format`.
			// Selective (`{a} from .x`) imports names directly, no namespace.
			if !strings.HasPrefix(spec, ".") || strings.ContainsAny(spec, "{ \t") {
				continue
			}
			base, fsPath, ok := resolveLocalImport(docPath, spec)
			if !ok {
				continue
			}
			if fns := parseModuleFuncs(fsPath); fns != nil {
				idx.localMods[base] = fns
			}
		}
	}

	return idx
}

func (idx *docIndex) getOrAddStruct(name string) *docStruct {
	if st, ok := idx.structs[name]; ok {
		return st
	}
	st := &docStruct{name: name}
	idx.structs[name] = st
	idx.structList = append(idx.structList, st)
	return st
}

// structMembers returns the fields and methods of a struct, walking the parent
// chain. Members declared closer to the struct (children) shadow inherited ones.
func (idx *docIndex) structMembers(name string) (fields, methods []docMember) {
	seenF, seenM, visited := map[string]bool{}, map[string]bool{}, map[string]bool{}
	for name != "" && !visited[name] {
		visited[name] = true
		st, ok := idx.structs[name]
		if !ok {
			break
		}
		for _, f := range st.fields {
			if !seenF[f.name] {
				seenF[f.name] = true
				fields = append(fields, f)
			}
		}
		for _, m := range st.methods {
			if !seenM[m.name] {
				seenM[m.name] = true
				methods = append(methods, m)
			}
		}
		name = st.parent
	}
	return fields, methods
}

// enclosingStruct reports which struct's `includes` block (if any) contains the
// given line — used to resolve `self.` and bare field/method references.
func (idx *docIndex) enclosingStruct(lineIdx int) string {
	for _, b := range idx.includesBlks {
		if lineIdx >= b.start && lineIdx < b.end {
			return b.structName
		}
	}
	return ""
}

func braceDelta(line string) int {
	return strings.Count(line, "{") - strings.Count(line, "}")
}

// resolveLocalImport maps an import spec like `.mylib` or `..utils.format` to a
// (baseName, filesystem path). Leading dots count directory levels: one dot is
// the current file's directory, each extra dot goes one level up. Names after
// the dots are path segments; the last is the module file.
func resolveLocalImport(docPath, spec string) (base, fsPath string, ok bool) {
	n := 0
	for n < len(spec) && spec[n] == '.' {
		n++
	}
	rest := spec[n:]
	if rest == "" {
		return "", "", false
	}
	segs := strings.Split(rest, ".")
	dir := filepath.Dir(docPath)
	for k := 1; k < n; k++ {
		dir = filepath.Dir(dir)
	}
	parts := append([]string{dir}, segs...)
	return segs[len(segs)-1], filepath.Join(parts...) + ".oxi", true
}

// parseModuleFuncs extracts the exported function signatures from an `.oxi` file.
func parseModuleFuncs(path string) []functionInfo {
	source, err := os.ReadFile(path)
	if err != nil {
		return nil
	}
	var functions []functionInfo
	for _, line := range strings.Split(string(source), "\n") {
		if m := funDeclRe.FindStringSubmatch(line); m != nil {
			functions = append(functions, functionInfo{name: m[1], signature: m[1] + "(" + m[2] + ")"})
		}
	}
	return functions
}

func uriToPath(uri string) string {
	if uri == "" {
		return ""
	}
	if strings.HasPrefix(uri, "file://") {
		if u, err := url.Parse(uri); err == nil && u.Path != "" {
			return u.Path
		}
		return strings.TrimPrefix(uri, "file://")
	}
	return uri
}
