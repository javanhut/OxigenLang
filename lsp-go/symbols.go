package main

import (
	"regexp"
	"strings"
)

var (
	funRe       = regexp.MustCompile(`^\s*fun\s+(\w+)\s*\(`)
	structRe    = regexp.MustCompile(`^\s*struct\s+(\w+)\s*\{`)
	containsRe  = regexp.MustCompile(`^\s*contains\s+(\w+)\s*\{`)
	patternRe   = regexp.MustCompile(`^\s*pattern\s+(\w+)\s*\(`)
	introduceRe = regexp.MustCompile(`^\s*intro(?:duce)?\s+(.+)`)
	letRe       = regexp.MustCompile(`^\s*(\w+)\s*:=`)
	typedLetRe  = regexp.MustCompile(`^\s*(\w+)\s+<(\w+)>`)
)

func getDocumentSymbols(source, uri string) []SymbolInformation {
	symbols := make([]SymbolInformation, 0)
	lines := strings.Split(source, "\n")

	var currentContains string

	for i, line := range lines {
		lineNum := uint32(i)

		// Function declaration
		if m := funRe.FindStringSubmatchIndex(line); m != nil {
			name := line[m[2]:m[3]]
			col := uint32(m[2])
			displayName := name
			if currentContains != "" {
				displayName = currentContains + "." + name
				symbols = append(symbols, makeSymbol(displayName, SymbolKindMethod, uri, lineNum, col))
			} else {
				symbols = append(symbols, makeSymbol(displayName, SymbolKindFunction, uri, lineNum, col))
			}
			continue
		}

		// Struct definition
		if m := structRe.FindStringSubmatchIndex(line); m != nil {
			name := line[m[2]:m[3]]
			col := uint32(m[2])
			symbols = append(symbols, makeSymbol(name, SymbolKindStruct, uri, lineNum, col))

			// Extract fields from subsequent lines
			for j := i + 1; j < len(lines); j++ {
				fieldLine := strings.TrimSpace(lines[j])
				if fieldLine == "}" || fieldLine == "" {
					break
				}
				fieldLine = strings.TrimPrefix(fieldLine, "hide ")
				parts := strings.Fields(fieldLine)
				if len(parts) >= 1 && isIdentStart(parts[0]) {
					symbols = append(symbols, makeSymbol(parts[0], SymbolKindField, uri, uint32(j), 0))
				}
			}
			continue
		}

		// Contains block start
		if m := containsRe.FindStringSubmatchIndex(line); m != nil {
			currentContains = line[m[2]:m[3]]
			continue
		}

		// Contains block end (unindented closing brace)
		if currentContains != "" && strings.TrimSpace(line) == "}" {
			if len(line) > 0 && line[0] == '}' {
				currentContains = ""
			}
			continue
		}

		// Pattern declaration
		if m := patternRe.FindStringSubmatchIndex(line); m != nil {
			name := line[m[2]:m[3]]
			col := uint32(m[2])
			symbols = append(symbols, makeSymbol(name, SymbolKindFunction, uri, lineNum, col))
			continue
		}

		// Introduce (import)
		if m := introduceRe.FindStringSubmatchIndex(line); m != nil {
			path := strings.TrimSpace(line[m[2]:m[3]])
			if idx := strings.Index(path, "{"); idx >= 0 {
				// Handle selective imports: introduce {a, b} from module
				// Keep the full text for display
			}
			symbols = append(symbols, makeSymbol("introduce "+path, SymbolKindModule, uri, lineNum, 0))
			continue
		}

		// Variable assignment at top level
		if m := letRe.FindStringSubmatchIndex(line); m != nil {
			indent := countLeadingSpaces(line)
			if indent == 0 {
				name := line[m[2]:m[3]]
				col := uint32(m[2])
				rest := strings.TrimSpace(line[m[1]:])
				kind := SymbolKindVariable
				if strings.HasPrefix(rest, "fun(") || strings.HasPrefix(rest, "fun (") {
					kind = SymbolKindFunction
				}
				symbols = append(symbols, makeSymbol(name, kind, uri, lineNum, col))
			}
			continue
		}

		// Typed variable at top level
		if m := typedLetRe.FindStringSubmatchIndex(line); m != nil {
			indent := countLeadingSpaces(line)
			if indent == 0 {
				trimmed := strings.TrimSpace(line)
				if !strings.HasPrefix(trimmed, "fun ") && !strings.HasPrefix(trimmed, "hide ") {
					name := line[m[2]:m[3]]
					col := uint32(m[2])
					symbols = append(symbols, makeSymbol(name, SymbolKindVariable, uri, lineNum, col))
				}
			}
		}
	}

	return symbols
}

func makeSymbol(name string, kind int, uri string, line, col uint32) SymbolInformation {
	return SymbolInformation{
		Name: name,
		Kind: kind,
		Location: Location{
			URI: uri,
			Range: Range{
				Start: Position{Line: line, Character: col},
				End:   Position{Line: line, Character: col + uint32(len(name))},
			},
		},
	}
}

func countLeadingSpaces(s string) int {
	count := 0
	for _, c := range s {
		if c == ' ' {
			count++
		} else if c == '\t' {
			count += 4
		} else {
			break
		}
	}
	return count
}

func isIdentStart(s string) bool {
	if len(s) == 0 {
		return false
	}
	c := s[0]
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}
