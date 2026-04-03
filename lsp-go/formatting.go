package main

import (
	"os"
	"os/exec"
	"strings"
)

func formatDocument(content, oxigenBin string) []TextEdit {
	tmpFile, err := os.CreateTemp("", "oxigen-fmt-*.oxi")
	if err != nil {
		return nil
	}
	tmpPath := tmpFile.Name()
	defer os.Remove(tmpPath)

	tmpFile.WriteString(content)
	tmpFile.Close()

	// oxigen fmt modifies the file in place
	cmd := exec.Command(oxigenBin, "fmt", tmpPath)
	if err := cmd.Run(); err != nil {
		return nil
	}

	formatted, err := os.ReadFile(tmpPath)
	if err != nil {
		return nil
	}

	formattedStr := string(formatted)
	if formattedStr == content {
		return nil
	}

	// Replace entire document
	lines := strings.Split(content, "\n")
	lineCount := uint32(len(lines))
	lastLineLen := uint32(0)
	if lineCount > 0 {
		lastLineLen = uint32(len(lines[lineCount-1]))
	}

	return []TextEdit{{
		Range: Range{
			Start: Position{Line: 0, Character: 0},
			End:   Position{Line: lineCount, Character: lastLineLen},
		},
		NewText: formattedStr,
	}}
}
