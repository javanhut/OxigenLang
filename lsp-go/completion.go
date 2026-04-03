package main

import "strings"

type keywordInfo struct {
	name   string
	detail string
}

var keywords = []keywordInfo{
	{"fun", "Define a function"},
	{"struct", "Define a struct"},
	{"contains", "Define methods for a struct"},
	{"introduce", "Import a module"},
	{"intro", "Import a module (shorthand)"},
	{"from", "Selective import from a module"},
	{"each", "Iterate over a collection"},
	{"repeat", "Loop while condition is true"},
	{"option", "Multi-arm conditional expression"},
	{"unless", "Inverse conditional expression"},
	{"choose", "Pattern matching expression"},
	{"pattern", "Define a named pattern"},
	{"when", "Postfix conditional guard"},
	{"guard", "Error recovery expression"},
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
	{"self", "Reference to current struct instance"},
	{"True", "Boolean true literal"},
	{"False", "Boolean false literal"},
	{"None", "Absence of a value"},
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
	{"range", "range(start, end) — Generate integer range"},
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
	{"error", "error(message) — Create an error value"},
	{"is_value", "is_value(obj) — Check if obj is a Value wrapper"},
	{"is_error", "is_error(obj) — Check if obj is an error"},
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
}

type completionContext int

const (
	contextGeneral completionContext = iota
	contextAfterIntroduce
	contextAfterModuleDot
	contextTypeAnnotation
	contextHeaderDirective
	contextShebang
)

func detectCompletionContext(source string, pos Position, stdlibPath string) (completionContext, string) {
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

	// Check for introduce/intro context
	if trimmed == "introduce" || trimmed == "intro" ||
		strings.HasSuffix(trimmed, "introduce ") || strings.HasSuffix(trimmed, "intro ") {
		return contextAfterIntroduce, ""
	}

	// Check for module.member access
	if dotIdx := strings.LastIndex(trimmed, "."); dotIdx >= 0 {
		beforeDot := trimmed[:dotIdx]
		moduleName := extractLastWord(beforeDot)
		if moduleName != "" {
			funcMap := getStdlibFuncMap(stdlibPath)
			if _, ok := funcMap[moduleName]; ok {
				return contextAfterModuleDot, moduleName
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

func getCompletions(source string, pos Position, stdlibPath string) []CompletionItem {
	ctx, moduleName := detectCompletionContext(source, pos, stdlibPath)

	switch ctx {
	case contextHeaderDirective:
		return headerDirectiveCompletions()
	case contextShebang:
		return shebangCompletions()
	case contextAfterIntroduce:
		return moduleCompletions(stdlibPath)
	case contextAfterModuleDot:
		return moduleFunctionCompletions(moduleName, stdlibPath)
	case contextTypeAnnotation:
		return typeCompletions()
	default:
		return generalCompletions()
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

func generalCompletions() []CompletionItem {
	items := make([]CompletionItem, 0, len(keywords)+len(builtins))

	kwKind := CompletionKindKeyword
	for _, kw := range keywords {
		items = append(items, CompletionItem{
			Label:  kw.name,
			Kind:   &kwKind,
			Detail: kw.detail,
		})
	}

	fnKind := CompletionKindFunction
	for _, b := range builtins {
		items = append(items, CompletionItem{
			Label:  b.name,
			Kind:   &fnKind,
			Detail: b.detail,
		})
	}

	return items
}

func typeCompletions() []CompletionItem {
	tpKind := CompletionKindTypeParameter
	items := make([]CompletionItem, 0, len(typeNames))
	for _, t := range typeNames {
		items = append(items, CompletionItem{
			Label:  t.name,
			Kind:   &tpKind,
			Detail: t.detail,
		})
	}
	return items
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
