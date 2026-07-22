package main

import "strings"

func getHoverInfo(source string, pos Position, stdlibPath string) *Hover {
	if info := headerHover(source, pos); info != "" {
		return &Hover{
			Contents: MarkupContent{
				Kind:  "markdown",
				Value: info,
			},
		}
	}

	word := getWordAtPosition(source, pos)
	if word == "" {
		return nil
	}

	info := getHoverText(word)
	if info == "" {
		info = stdlibHover(source, pos, word, stdlibPath)
	}
	if info == "" {
		return nil
	}

	return &Hover{
		Contents: MarkupContent{
			Kind:  "markdown",
			Value: info,
		},
	}
}

// stdlibHover describes standard-library modules and their functions. It fires
// when the hovered word is a module name (`math`) or a function qualified by a
// module (`math.sqrt`), using the same discovered stdlib data as completion.
func stdlibHover(source string, pos Position, word, stdlibPath string) string {
	funcMap := getStdlibFuncMap(stdlibPath)
	if len(funcMap) == 0 {
		return ""
	}

	lines := strings.Split(source, "\n")
	lineIdx := int(pos.Line)
	if lineIdx >= len(lines) {
		return ""
	}
	line := lines[lineIdx]
	col := int(pos.Character)
	if col > len(line) {
		col = len(line)
	}

	// Qualified access: the word sits right after `module.`
	start := col
	for start > 0 && isIdentChar(line[start-1]) {
		start--
	}
	if start > 0 && line[start-1] == '.' {
		if mod := extractLastWord(line[:start-1]); mod != "" {
			if funcs, ok := funcMap[mod]; ok {
				for _, f := range funcs {
					if f.name == word {
						return "**" + mod + "." + f.signature + "**\n\nStandard library function from `" + mod + "`."
					}
				}
			}
		}
		return ""
	}

	// Bare module name.
	if funcs, ok := funcMap[word]; ok {
		var b strings.Builder
		b.WriteString("**" + word + "** — Oxigen standard library module\n\n```oxigen\nintroduce " + word + "\n```\n\nFunctions:\n")
		for _, f := range funcs {
			b.WriteString("- `" + f.signature + "`\n")
		}
		return b.String()
	}
	return ""
}

func headerHover(source string, pos Position) string {
	lines := strings.Split(source, "\n")
	lineIdx := int(pos.Line)
	if lineIdx >= len(lines) {
		return ""
	}

	line := strings.TrimSpace(lines[lineIdx])
	switch {
	case lineIdx == 0 && strings.HasPrefix(line, "#!"):
		return "**#!** — Unix shebang for direct execution\n\nUse a real shebang such as `#!/usr/bin/env oxigen` or `#!/usr/local/bin/oxigen` when you want to run a script with `./script.oxi`."
	case strings.HasPrefix(line, "#[indent]"):
		return "**#[indent]** — Enable indentation-based block syntax for this file."
	case strings.HasPrefix(line, "#[location="):
		return "**#[location=...]** — File metadata for the preferred Oxigen interpreter path.\n\nThis directive is preserved by Oxigen tooling, but direct execution still requires a real shebang on the first line."
	default:
		return ""
	}
}

func getWordAtPosition(source string, pos Position) string {
	lines := strings.Split(source, "\n")
	lineIdx := int(pos.Line)
	if lineIdx >= len(lines) {
		return ""
	}

	line := lines[lineIdx]
	col := int(pos.Character)
	if col > len(line) {
		return ""
	}

	start := col
	for start > 0 && isIdentChar(line[start-1]) {
		start--
	}

	end := col
	for end < len(line) && isIdentChar(line[end]) {
		end++
	}

	if start == end {
		return ""
	}

	return line[start:end]
}

func getHoverText(word string) string {
	if info := keywordHover(word); info != "" {
		return info
	}
	if info := builtinHover(word); info != "" {
		return info
	}
	if info := typeHover(word); info != "" {
		return info
	}
	return ""
}

func keywordHover(word string) string {
	switch word {
	case "fun":
		return "**fun** — Define a function\n\n```oxigen\nfun name(param <type>) {\n    body\n}\n```"
	case "struct":
		return "**struct** — Define a composite data type\n\n```oxigen\nstruct Name {\n    field <type>\n}\n```"
	case "enum":
		return "**enum** — Define an enumeration\n\n```oxigen\nenum Shape {\n    Circle(radius <float>)\n    Square(side <float>)\n}\n```"
	case "includes":
		return "**includes** — Add methods to a struct\n\n```oxigen\nStructName includes {\n    fun method() { self.field }\n}\n```"
	case "main":
		return "**main** — Top-level entry block. Skipped when the file is run with `oxigen test`.\n\n```oxigen\nmain {\n    println(\"hello\")\n}\n```"
	case "test":
		return "**test** — A test block, run by `oxigen test` (on the VM). Passes unless its body raises an error; assert with `expect`.\n\n```oxigen\n<test>(\"name\") {\n    expect(2 + 3).eq(5)\n}\n```"
	case "introduce", "intro":
		return "**introduce** — Import a module\n\n```oxigen\nintroduce math\nintroduce {split, join} from strings\n```"
	case "each":
		return "**each** — Iterate over a collection\n\n```oxigen\neach item in collection {\n    body\n}\n```"
	case "repeat":
		return "**repeat** — Loop while condition is true\n\n```oxigen\nrepeat condition {\n    body\n}\n```"
	case "diverge":
		return "**diverge** — Run work on a worker thread (real OS thread, share-nothing).\n\n`diverge { ... }` returns a task immediately; `diverge each x in xs { ... }` fans out over a collection in parallel and gathers results in order.\n\n```oxigen\nwork := diverge { expensive() }\nsquares := diverge each n in nums { n * n }\n```"
	case "converge":
		return "**converge** — Wait for a task and return its value (idempotent). Accepts a single task or a list of tasks (joined in order). Add `within <ms>` to bound the wait.\n\n```oxigen\nresult := converge work\nall := converge [a, b]\nr := converge work within 500\n```"
	case "within":
		return "**within** — Bound a `converge` wait to a number of milliseconds. On timeout it returns an error value (test with `is_error`); the task keeps running.\n\n```oxigen\nresult := converge work within 500\n```"
	case "option":
		return "**option** — Multi-arm conditional expression. Arms are comma-separated `condition -> result`; a trailing bare `{ ... }` block (no `->`) is the default.\n\n```oxigen\noption {\n    n < 0 -> \"negative\",\n    n == 0 -> \"zero\",\n    { \"positive\" }\n}\n```"
	case "unless":
		return "**unless** — Inverse conditional\n\n```oxigen\nvalue unless condition then alternative\n```"
	case "choose":
		return "**choose** — Pattern matching\n\n```oxigen\nchoose value {\n    is_red -> println(\"stop\")\n    is_green -> println(\"go\")\n}\n```"
	case "pattern":
		return "**pattern** — Define a named pattern for use with choose\n\n```oxigen\npattern name(x) = condition\n```"
	case "guard":
		return "**guard** — Recover from an error with a fallback value.\n\nPostfix angle form (optionally tag-filtered), or keyword form with the caught error bound:\n\n```oxigen\nname := read_name() <guard>(\"Guest\")\nprofile := fetch_profile() <guard<Error<retry_error>>>(\"Guest\")\nname := read_name() guard err -> \"Unknown (error: {err.msg})\"\n```"
	case "log":
		return "**<log>** — Write a timestamped message to stdout. Tags are uppercased; nested tags join with `:`.\n\n```oxigen\n<log>(\"server started\")\n<log<info>>(\"request received\")\n<log<Error<network>>>(\"connection lost\")\n```"
	case "fail":
		return "**fail** — Raise an error value, short-circuiting the current body.\n\n```oxigen\n<fail>(\"something went wrong\")\n```"
	case "Error":
		return "**<Error>** — Error wrapper in the error/value model. Optionally tagged.\n\n```oxigen\n<Error>(\"boom\")\n<Error<div_by_zero>>(\"cannot divide by zero\")\n```"
	case "Value":
		return "**<Value>** — Value (success) wrapper in the error/value model.\n\n```oxigen\noption {\n    n < 0 -> <Error>(\"neg\"),\n    <Value>(n * 2)\n}\n```"
	case "give":
		return "**give** — Return a value from a function\n\n```oxigen\ngive value\n```"
	case "skip":
		return "**skip** — Skip to next loop iteration (continue)"
	case "stop":
		return "**stop** — Break out of a loop"
	case "when":
		return "**when** — Postfix conditional guard\n\n```oxigen\nexpression when condition\n```"
	case "and":
		return "**and** — Logical AND operator"
	case "or":
		return "**or** — Logical OR operator"
	case "not":
		return "**not** — Logical negation operator"
	case "in":
		return "**in** — Membership test or iteration keyword"
	case "as":
		return "**as** — Type conversion\n\n```oxigen\nvalue as <type>\n```"
	case "hide", "hidden":
		return "**hidden** — Mark a struct field as private\n\n```oxigen\nstruct Account {\n    name <str>\n    hidden balance <int>\n}\n```"
	case "self":
		return "**self** — Reference to the current struct instance in methods"
	case "True":
		return "**True** — Boolean true literal"
	case "False":
		return "**False** — Boolean false literal"
	case "None":
		return "**None** — Represents the absence of a value"
	case "from":
		return "**from** — Selective import from a module\n\n```oxigen\nintroduce {name} from module\n```"
	case "then":
		return "**then** — Consequence branch in postfix conditionals"
	case "indent":
		return "**indent** — Header directive name used in `#[indent]` to enable indentation-based syntax."
	case "location":
		return "**location** — Header directive name used in `#[location=/path/to/oxigen]`."
	default:
		return ""
	}
}

func builtinHover(word string) string {
	switch word {
	case "print":
		return "**print**(...args)\n\nPrint values to stdout separated by spaces."
	case "println":
		return "**println**(...args)\n\nPrint values to stdout with a trailing newline."
	case "len":
		return "**len**(collection) -> int\n\nReturn the length of a string, array, map, tuple, or set."
	case "push":
		return "**push**(array, value) -> array\n\nAppend a value to the end of an array, **mutating it in place** (and returning it)."
	case "first":
		return "**first**(array) -> value\n\nReturn the first element of an array."
	case "last":
		return "**last**(array) -> value\n\nReturn the last element of an array."
	case "rest":
		return "**rest**(array) -> array\n\nReturn all elements except the first."
	case "type":
		return "**type**(value) -> str\n\nReturn the type name of a value as a string."
	case "ord":
		return "**ord**(char) -> int\n\nReturn the Unicode codepoint of a character."
	case "chr":
		return "**chr**(int) -> char\n\nReturn the character for a Unicode codepoint."
	case "str":
		return "**str**(value) -> str\n\nConvert any value to its string representation."
	case "int":
		return "**int**(value) -> int\n\nConvert a value to an integer."
	case "float":
		return "**float**(value) -> float\n\nConvert a value to a float."
	case "range":
		return "**range**(end) or **range**(start, end) -> array\n\nGenerate an array of integers. One argument: `[0, ..., end-1]`; two: `[start, ..., end-1]`."
	case "chars":
		return "**chars**(string) -> array\n\nSplit a string into an array of individual characters."
	case "byte":
		return "**byte**(value) -> byte\n\nConvert a value to a byte (u8)."
	case "uint":
		return "**uint**(value) -> uint\n\nConvert a value to an unsigned integer."
	case "set":
		return "**set**(array) -> set\n\nCreate a set from an array, removing duplicates."
	case "keys":
		return "**keys**(map) -> array\n\nReturn the keys of a map as an array."
	case "values":
		return "**values**(map) -> array\n\nReturn the values of a map as an array."
	case "insert":
		return "**insert**(map, key, value) -> map\n\nInsert a key-value pair into a map (or an element into a set), **mutating it in place** (and returning it)."
	case "remove":
		return "**remove**(collection, item) -> collection\n\nRemove an item from a collection."
	case "has":
		return "**has**(collection, item) -> bool\n\nCheck if a collection contains an item."
	case "tuple":
		return "**tuple**(...values) -> tuple\n\nCreate a tuple from the given values."
	case "error":
		return "**error**(message)\n\nCreate a propagating error (legacy form). Prefer `<fail>(\"msg\")` in new code; use `<Error>(\"msg\")` for an error *value* that does not halt the program."
	case "is_value":
		return "**is_value**(obj) -> bool\n\nCheck if an object is a Value wrapper."
	case "is_error":
		return "**is_error**(obj) -> bool\n\nCheck if an object is an error value."
	case "is_type":
		return "**is_type**(value, type_name) -> bool\n\nRuntime type check — true if the value's type matches `type_name` (a string, e.g. `\"int\"`, or a struct name)."
	case "is_mut":
		return "**is_mut**(variable) -> bool\n\nCheck whether a binding is mutable (declared with `:=`)."
	case "is_type_mut":
		return "**is_type_mut**(variable) -> bool\n\nCheck whether a binding's type annotation allows reassignment to a different type."
	case "expect":
		return "**expect**(actual) -> Expectation\n\nTest assertion matcher, auto-imported by `oxigen test` in `*_test.oxi` files. Matchers: `eq`, `ne`, `gt`, `gte`, `lt`, `lte`, `truthy`, `falsy`, `contains`, `is_error`, `is_value`.\n\n```oxigen\n<test>(\"addition\") {\n    expect(2 + 3).eq(5)\n}\n```"
	case "cancel":
		return "**cancel**(task)\n\nAsk a task to stop. Cancellation is cooperative — the task stops at its next function call. Joining a cancelled task returns an error value (test with `is_error`)."
	default:
		return ""
	}
}

func typeHover(word string) string {
	switch word {
	case "int":
		return "**int** — 64-bit signed integer type\n\nZero value: `0`"
	case "str":
		return "**str** — Unicode string type\n\nZero value: `\"\"`"
	case "float":
		return "**float** — 64-bit floating point type\n\nZero value: `0.0`"
	case "char":
		return "**char** — Single Unicode character type"
	case "byte":
		return "**byte** — Unsigned 8-bit integer type (0-255)"
	case "uint":
		return "**uint** — Unsigned 64-bit integer type"
	case "set":
		return "**set** — Unique unordered collection type\n\nCreate with `set(array)`."
	case "array":
		return "**array** — Dynamic ordered collection type\n\nLiteral: `[1, 2, 3]`\n\nZero value: `[]`"
	case "tuple":
		return "**tuple** — Fixed-size immutable collection type\n\nLiteral: `(1, 2, 3)`"
	case "map":
		return "**map** — Key-value pair collection type\n\nLiteral: `{\"key\": value}`\n\nZero value: `{}`"
	case "bool":
		return "**bool** — Boolean type (`True` or `False`)\n\nZero value: `False`"
	case "generic":
		return "**generic** — Dynamic type that accepts any value"
	default:
		return ""
	}
}
