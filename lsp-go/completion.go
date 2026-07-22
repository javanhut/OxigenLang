package main

import (
	"regexp"
	"strings"
)

type keywordInfo struct {
	name   string
	detail string
}

var keywords = []keywordInfo{
	{"fun", "Define a function"},
	{"struct", "Define a struct"},
	{"enum", "Define an enumeration"},
	{"includes", "Add methods to a struct: StructName includes { ... }"},
	{"main", "Top-level entry block: main { ... } (skipped under `oxigen test`)"},
	{"introduce", "Import a module"},
	{"intro", "Import a module (shorthand)"},
	{"from", "Selective import from a module"},
	{"each", "Iterate over a collection"},
	{"repeat", "Loop while condition is true"},
	{"diverge", "Run a block on a worker thread, returning a task"},
	{"converge", "Wait for a task (or list of tasks) and return its value"},
	{"within", "Bound a converge wait: converge t within <ms>"},
	{"option", "Multi-arm conditional expression"},
	{"unless", "Inverse conditional expression"},
	{"choose", "Pattern matching expression"},
	{"pattern", "Define a named pattern"},
	{"when", "Postfix conditional guard"},
	{"guard", "Error recovery expression (<guard<Error<tag>>>)"},
	{"fail", "Propagate an error"},
	{"give", "Return a value from a function"},
	{"skip", "Skip to next loop iteration"},
	{"stop", "Break out of a loop"},
	{"in", "Membership / iteration keyword"},
	{"not", "Logical negation"},
	{"and", "Logical AND"},
	{"or", "Logical OR"},
	{"then", "Consequence in postfix conditional"},
	{"as", "Type conversion"},
	{"hide", "Mark struct field as private"},
	{"hidden", "Mark a struct field as private (StructName includes { hidden field })"},
	{"self", "Reference to current struct instance"},
	{"True", "Boolean true literal"},
	{"False", "Boolean false literal"},
	{"None", "Absence of a value"},
}

// Angle-bracket constructs offered after `<`: the error/value model wrappers,
// logging, result normalization, and the test block, alongside the plain type
// names.
var angleConstructs = []keywordInfo{
	{"Error", "Error wrapper: <Error>(\"msg\") or tagged <Error<tag>>(\"msg\")"},
	{"Value", "Value wrapper: <Value>(expr) — the success arm of the error/value model"},
	{"fail", "Raise an error: <fail>(\"msg\") — short-circuits the current body"},
	{"guard", "Recover from an error: expr <guard>(fallback) or expr <guard<Error<tag>>>(fallback)"},
	{"log", "Timestamped log to stdout: <log>(\"msg\") or <log<tag>>(\"msg\")"},
	{"type", "Normalize a result: <type<Error || Value>>(expr) — catches errors into values"},
	{"test", "Test block: <test>(\"name\") { ... } (run with `oxigen test`)"},
}

var builtins = []keywordInfo{
	{"print", "print(...args) — Print values to stdout"},
	{"println", "println(...args) — Print values with newline"},
	{"len", "len(collection) — Get length of a collection"},
	{"push", "push(array, value) — Append to an array"},
	{"first", "first(array) — Get first element"},
	{"last", "last(array) — Get last element"},
	{"rest", "rest(array) — Get all but first element"},
	{"type", "type(value) — Get the type name of a value"},
	{"ord", "ord(char) — Get Unicode codepoint of a character"},
	{"chr", "chr(int) — Get character from Unicode codepoint"},
	{"str", "str(value) — Convert to string"},
	{"int", "int(value) — Convert to integer"},
	{"float", "float(value) — Convert to float"},
	{"range", "range(end) or range(start, end) — Generate an integer range"},
	{"chars", "chars(string) — Split string into char array"},
	{"byte", "byte(value) — Convert to byte"},
	{"uint", "uint(value) — Convert to unsigned integer"},
	{"set", "set(array) — Create a set from an array"},
	{"keys", "keys(map) — Get keys of a map"},
	{"values", "values(map) — Get values of a map"},
	{"insert", "insert(map, key, value) — Insert into a map"},
	{"remove", "remove(collection, item) — Remove from collection"},
	{"has", "has(collection, item) — Check membership"},
	{"tuple", "tuple(...values) — Create a tuple"},
	{"error", "error(message) — Create a propagating error (legacy; prefer <fail>)"},
	{"is_value", "is_value(obj) — Check if obj is a Value wrapper"},
	{"is_error", "is_error(obj) — Check if obj is an error"},
	{"is_type", "is_type(value, type_name) — Runtime type check"},
	{"is_mut", "is_mut(variable) — Check if a binding is mutable"},
	{"is_type_mut", "is_type_mut(variable) — Check if a binding's type is mutable"},
	{"cancel", "cancel(task) — Ask a task to stop (cooperative)"},
}

var typeNames = []keywordInfo{
	{"int", "64-bit signed integer"},
	{"str", "Unicode string"},
	{"float", "64-bit floating point"},
	{"char", "Single Unicode character"},
	{"bool", "Boolean (True/False)"},
	{"array", "Dynamic ordered collection"},
	{"byte", "Unsigned 8-bit integer"},
	{"uint", "Unsigned 64-bit integer"},
	{"tuple", "Fixed-size immutable collection"},
	{"map", "Key-value pairs"},
	{"set", "Unique unordered collection"},
	{"generic", "Dynamic type (any)"},
	{"Error||Value", "A value that is either an Error or a Value (error/value model)"},
}

// Block templates offered as snippet completions (when the client supports
// snippets). Bodies use LSP snippet syntax: ${n:placeholder}, $0 = final stop.
type snippetInfo struct {
	label  string
	detail string
	body   string
}

var snippets = []snippetInfo{
	{"fun", "fun name(args) { ... }", "fun ${1:name}($2) {\n\t$0\n}"},
	{"struct", "struct Name { ... }", "struct ${1:Name} {\n\t${2:field} <${3:type}>\n}"},
	{"main", "main { ... }", "main {\n\t$0\n}"},
	{"each", "each item in collection { ... }", "each ${1:item} in ${2:collection} {\n\t$0\n}"},
	{"repeat", "repeat when condition { ... }", "repeat when ${1:condition} {\n\t$0\n}"},
	{"option", "option { cond -> value, default }", "option { ${1:cond} -> ${2:value}, ${3:default} }"},
	{"choose", "choose val { pattern -> ... }", "choose ${1:val} {\n\t${2:pattern} -> ${3:expr}\n}"},
	{"introduce", "introduce {names} from module", "introduce {${2:names}} from ${1:module}"},
	{"test", "<test>(\"name\") { ... }", "<test>(\"${1:name}\") {\n\t$0\n}"},
	{"includes", "StructName includes { ... }", "${1:StructName} includes {\n\tfun ${2:method}($3) {\n\t\t$0\n\t}\n}"},
}

var (
	introduceModRe    = regexp.MustCompile(`^(?:introduce|intro)(?:\s+\w*)?$`)
	selectiveFromRe   = regexp.MustCompile(`^(?:introduce|intro)\s*\{[^}]*\}\s*from\s+\w*$`)
	selectiveBracesRe = regexp.MustCompile(`^(?:introduce|intro)\s*\{[^}]*$`)
	fromModuleRe      = regexp.MustCompile(`\}\s*from\s+(\w+)`)
)

type completionContext int

const (
	contextGeneral completionContext = iota
	contextAfterIntroduce
	contextAfterModuleDot
	contextAfterLocalModuleDot
	contextAfterInstanceDot
	contextTypeAnnotation
	contextHeaderDirective
	contextShebang
)

func detectCompletionContext(source string, pos Position, idx *docIndex, stdlibPath string) (completionContext, string) {
	lines := strings.Split(source, "\n")
	lineIdx := int(pos.Line)
	if lineIdx >= len(lines) {
		return contextGeneral, ""
	}

	line := lines[lineIdx]
	col := int(pos.Character)
	if col > len(line) {
		col = len(line)
	}
	textBefore := line[:col]
	trimmed := strings.TrimSpace(textBefore)
	trimmedLeft := strings.TrimLeft(textBefore, " \t")

	if strings.HasPrefix(trimmedLeft, "#[") {
		return contextHeaderDirective, ""
	}

	if lineIdx == 0 && strings.HasPrefix(trimmedLeft, "#!") {
		return contextShebang, ""
	}

	// Check for introduce/intro context. Matches while the module name is
	// still being typed (`introduce o`), and after `from` in a selective
	// import (`introduce {upper} from st`).
	if introduceModRe.MatchString(trimmed) || selectiveFromRe.MatchString(trimmed) {
		return contextAfterIntroduce, ""
	}

	// Cursor inside the braces of a selective import: complete the module's
	// functions if the module is already named after `from` on this line.
	if selectiveBracesRe.MatchString(trimmed) {
		if m := fromModuleRe.FindStringSubmatch(line); m != nil {
			return contextAfterModuleDot, m[1]
		}
	}

	// Check for `receiver.member` access. The receiver may be a stdlib module, a
	// local imported module, or a variable/`self` bound to a struct instance.
	if dotIdx := strings.LastIndex(trimmed, "."); dotIdx >= 0 {
		word := extractLastWord(trimmed[:dotIdx])
		if word != "" {
			if _, ok := getStdlibFuncMap(stdlibPath)[word]; ok {
				return contextAfterModuleDot, word
			}
			if t, ok := idx.varTypes[word]; ok {
				return contextAfterInstanceDot, t
			}
			if word == "self" {
				if es := idx.enclosingStruct(lineIdx); es != "" {
					return contextAfterInstanceDot, es
				}
			}
			if _, ok := idx.localMods[word]; ok {
				return contextAfterLocalModuleDot, word
			}
		}
	}

	// Check for type annotation — last non-space char is '<'
	trimmedRight := strings.TrimRight(textBefore, " \t")
	if len(trimmedRight) > 0 && trimmedRight[len(trimmedRight)-1] == '<' {
		return contextTypeAnnotation, ""
	}

	return contextGeneral, ""
}

// positionToOffset converts an LSP (line, character) position to a byte offset
// into source. Columns are treated as byte offsets within the line, matching
// the rest of this server's simple line/column handling.
func positionToOffset(source string, pos Position) int {
	line := int(pos.Line)
	char := int(pos.Character)
	n := len(source)

	i := 0
	for curLine := 0; i < n && curLine < line; i++ {
		if source[i] == '\n' {
			curLine++
		}
	}
	// i now sits at the first byte of the target line (or at EOF).
	for col := 0; i < n && col < char && source[i] != '\n'; col, i = col+1, i+1 {
	}
	return i
}

// inStringLiteralText reports whether pos sits inside the text of a string
// literal, including triple-quoted multi-line strings (with either quote
// style), but NOT inside a {...} interpolation expression, where code
// completions are still wanted. It scans the document from the start tracking
// the active string delimiter and interpolation-brace depth. This is a
// completion heuristic; the compiler remains the source of truth for parsing.
func inStringLiteralText(source string, pos Position) bool {
	offset := positionToOffset(source, pos)
	n := len(source)
	if offset > n {
		offset = n
	}

	var delim byte // 0 when outside a string, otherwise '"' or '\''
	triple := false
	interpDepth := 0 // >0 while inside `{...}` within a string

	for i := 0; i < offset; {
		c := source[i]

		// Outside any string: only a quote can open one.
		if delim == 0 {
			if c == '"' || c == '\'' {
				delim = c
				triple = i+2 < n && source[i+1] == c && source[i+2] == c
				if triple {
					i += 3
				} else {
					i++
				}
				continue
			}
			i++
			continue
		}

		// Inside a string's interpolation expression: track nested braces.
		if interpDepth > 0 {
			switch c {
			case '{':
				interpDepth++
			case '}':
				interpDepth--
			}
			i++
			continue
		}

		// Inside string literal text.
		switch {
		case c == '\\':
			i += 2 // skip the escaped character
		case c == '{':
			interpDepth = 1
			i++
		case triple && c == delim && i+2 < n && source[i+1] == delim && source[i+2] == delim:
			delim = 0
			i += 3
		case !triple && c == delim:
			delim = 0
			i++
		case !triple && c == '\n':
			// Single-line strings cannot span newlines; an unterminated one
			// ends here so later lines aren't treated as string text.
			delim = 0
			i++
		default:
			i++
		}
	}

	return delim != 0 && interpDepth == 0
}

func extractLastWord(s string) string {
	s = strings.TrimSpace(s)
	for i := len(s) - 1; i >= 0; i-- {
		if !isIdentChar(s[i]) {
			return s[i+1:]
		}
	}
	return s
}

func isIdentChar(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
}

func getCompletions(source string, pos Position, uri, stdlibPath string, snippetsOK bool) []CompletionItem {
	// Don't pop up keyword/builtin completions while the cursor is inside the
	// text of a string literal — including triple-quoted multi-line strings.
	// Interpolation expressions (`{...}`) are still real code, so completions
	// remain available there.
	if inStringLiteralText(source, pos) {
		return []CompletionItem{}
	}

	idx := indexDocument(source, uri)
	ctx, payload := detectCompletionContext(source, pos, idx, stdlibPath)

	switch ctx {
	case contextHeaderDirective:
		return headerDirectiveCompletions()
	case contextShebang:
		return shebangCompletions()
	case contextAfterIntroduce:
		return moduleCompletions(stdlibPath)
	case contextAfterModuleDot:
		return moduleFunctionCompletions(payload, stdlibPath)
	case contextAfterLocalModuleDot:
		return localModuleFunctionCompletions(idx, payload)
	case contextAfterInstanceDot:
		return structMemberCompletions(idx, payload)
	case contextTypeAnnotation:
		return typeCompletions(idx)
	default:
		// `expect` is auto-imported by the test runner in *_test.oxi files, so
		// only offer it where tests plausibly live.
		testCtx := strings.HasSuffix(uri, "_test.oxi") || strings.Contains(source, "<test")
		return generalCompletions(idx, int(pos.Line), testCtx, snippetsOK)
	}
}

func headerDirectiveCompletions() []CompletionItem {
	kwKind := CompletionKindKeyword
	return []CompletionItem{
		{
			Label:  "#[indent]",
			Kind:   &kwKind,
			Detail: "Enable indentation-based block syntax for this file",
		},
		{
			Label:  "#[location=/usr/local/bin/oxigen]",
			Kind:   &kwKind,
			Detail: "Record the preferred Oxigen interpreter path as file metadata",
		},
	}
}

func shebangCompletions() []CompletionItem {
	kwKind := CompletionKindKeyword
	return []CompletionItem{
		{
			Label:  "#!/usr/bin/env oxigen",
			Kind:   &kwKind,
			Detail: "Portable shebang for directly executable Oxigen scripts",
		},
	}
}

func generalCompletions(idx *docIndex, lineIdx int, testCtx, snippetsOK bool) []CompletionItem {
	items := make([]CompletionItem, 0, len(keywords)+len(builtins)+len(snippets))

	if snippetsOK {
		snipKind := CompletionKindSnippet
		snipFmt := 2
		for _, sn := range snippets {
			items = append(items, CompletionItem{
				Label:            sn.label,
				Kind:             &snipKind,
				Detail:           sn.detail,
				InsertText:       sn.body,
				InsertTextFormat: &snipFmt,
			})
		}
	}

	kwKind := CompletionKindKeyword
	for _, kw := range keywords {
		items = append(items, CompletionItem{
			Label:         kw.name,
			Kind:          &kwKind,
			Detail:        kw.detail,
			Documentation: docMarkup(keywordHover(kw.name)),
		})
	}

	fnKind := CompletionKindFunction
	for _, b := range builtins {
		items = append(items, CompletionItem{
			Label:         b.name,
			Kind:          &fnKind,
			Detail:        b.detail,
			Documentation: docMarkup(builtinHover(b.name)),
		})
	}

	if testCtx {
		items = append(items, CompletionItem{
			Label:         "expect",
			Kind:          &fnKind,
			Detail:        "expect(value) — Test assertion matcher (auto-imported by `oxigen test`)",
			Documentation: docMarkup(builtinHover("expect")),
		})
	}

	// Symbols declared in this document — new structs, functions, enums,
	// patterns, and top-level variables show up as you write them.
	items = append(items, idx.symbolCompletions()...)

	// Inside a method body, the enclosing struct's fields and methods are in
	// scope via implicit self, so offer them bare.
	if es := idx.enclosingStruct(lineIdx); es != "" {
		items = append(items, structMemberCompletions(idx, es)...)
	}

	return items
}

func typeCompletions(idx *docIndex) []CompletionItem {
	tpKind := CompletionKindTypeParameter
	kwKind := CompletionKindKeyword
	stKind := CompletionKindStruct
	enKind := CompletionKindEnum
	items := make([]CompletionItem, 0, len(typeNames)+len(angleConstructs))
	for _, t := range typeNames {
		items = append(items, CompletionItem{
			Label:         t.name,
			Kind:          &tpKind,
			Detail:        t.detail,
			Documentation: docMarkup(typeHover(t.name)),
		})
	}
	// `<` also begins the error/value model wrappers and a `<test>` block, not
	// only a type annotation.
	for _, c := range angleConstructs {
		items = append(items, CompletionItem{
			Label:  c.name,
			Kind:   &kwKind,
			Detail: c.detail,
		})
	}
	// User-defined structs and enums are usable as type annotations too.
	for _, st := range idx.structList {
		items = append(items, CompletionItem{Label: st.name, Kind: &stKind, Detail: structDetail(st)})
	}
	for _, e := range idx.enums {
		items = append(items, CompletionItem{Label: e.name, Kind: &enKind, Detail: e.detail})
	}
	return items
}

// symbolCompletions turns the document's declared symbols into completion items.
func (idx *docIndex) symbolCompletions() []CompletionItem {
	items := make([]CompletionItem, 0, len(idx.structList)+len(idx.funcs)+len(idx.enums)+len(idx.patterns)+len(idx.topVars))
	stKind := CompletionKindStruct
	enKind := CompletionKindEnum
	fnKind := CompletionKindFunction
	varKind := CompletionKindVariable
	for _, st := range idx.structList {
		items = append(items, CompletionItem{Label: st.name, Kind: &stKind, Detail: structDetail(st)})
	}
	for _, e := range idx.enums {
		items = append(items, CompletionItem{Label: e.name, Kind: &enKind, Detail: e.detail})
	}
	for _, f := range idx.funcs {
		items = append(items, CompletionItem{Label: f.name, Kind: &fnKind, Detail: f.detail})
	}
	for _, p := range idx.patterns {
		items = append(items, CompletionItem{Label: p.name, Kind: &fnKind, Detail: p.detail})
	}
	for _, v := range idx.topVars {
		items = append(items, CompletionItem{Label: v.name, Kind: &varKind, Detail: v.detail})
	}
	return items
}

// structMemberCompletions offers the fields and methods of a struct (including
// inherited ones) — what shows after `instance.` or `self.`.
func structMemberCompletions(idx *docIndex, structName string) []CompletionItem {
	fields, methods := idx.structMembers(structName)
	items := make([]CompletionItem, 0, len(fields)+len(methods))
	methodKind := CompletionKindMethod
	fieldKind := CompletionKindField
	for _, m := range methods {
		items = append(items, CompletionItem{Label: m.name, Kind: &methodKind, Detail: structName + "." + m.detail})
	}
	for _, f := range fields {
		items = append(items, CompletionItem{Label: f.name, Kind: &fieldKind, Detail: f.detail})
	}
	return items
}

// localModuleFunctionCompletions offers the functions of a locally-imported
// `.oxi` module — what shows after `localmod.`.
func localModuleFunctionCompletions(idx *docIndex, moduleName string) []CompletionItem {
	funcs := idx.localMods[moduleName]
	fnKind := CompletionKindFunction
	items := make([]CompletionItem, 0, len(funcs))
	for _, f := range funcs {
		items = append(items, CompletionItem{Label: f.name, Kind: &fnKind, Detail: f.signature})
	}
	return items
}

func structDetail(st *docStruct) string {
	if len(st.fields) == 0 {
		return "struct " + st.name
	}
	names := make([]string, 0, len(st.fields))
	for _, f := range st.fields {
		names = append(names, f.name)
	}
	return "struct " + st.name + "(" + strings.Join(names, ", ") + ")"
}

func docMarkup(value string) *MarkupContent {
	if value == "" {
		return nil
	}
	return &MarkupContent{Kind: "markdown", Value: value}
}

func moduleCompletions(stdlibPath string) []CompletionItem {
	modules := getStdlib(stdlibPath)
	modKind := CompletionKindModule
	items := make([]CompletionItem, 0, len(modules))
	for _, mod := range modules {
		names := make([]string, 0, len(mod.functions))
		for _, f := range mod.functions {
			names = append(names, f.name)
		}
		detail := mod.name
		if len(names) > 0 {
			detail = strings.Join(names, ", ")
		}
		items = append(items, CompletionItem{
			Label:  mod.name,
			Kind:   &modKind,
			Detail: detail,
		})
	}
	return items
}

func moduleFunctionCompletions(moduleName, stdlibPath string) []CompletionItem {
	funcMap := getStdlibFuncMap(stdlibPath)
	funcs, ok := funcMap[moduleName]
	if !ok {
		return []CompletionItem{}
	}
	fnKind := CompletionKindFunction
	items := make([]CompletionItem, 0, len(funcs))
	for _, f := range funcs {
		items = append(items, CompletionItem{
			Label:  f.name,
			Kind:   &fnKind,
			Detail: f.signature,
		})
	}
	return items
}
