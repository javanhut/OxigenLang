package main

import (
	"context"
	"encoding/json"
	"os"
	"os/exec"
)

type checkDiagnostic struct {
	Line       int     `json:"line"`
	Column     int     `json:"column"`
	Message    string  `json:"message"`
	Suggestion *string `json:"suggestion"`
	Severity   string  `json:"severity"`
}

func getDiagnosticsCtx(ctx context.Context, content, oxigenBin string) []Diagnostic {
	tmpFile, err := os.CreateTemp("", "oxigen-check-*.oxi")
	if err != nil {
		return []Diagnostic{}
	}
	tmpPath := tmpFile.Name()
	defer os.Remove(tmpPath)

	tmpFile.WriteString(content)
	tmpFile.Close()

	cmd := exec.CommandContext(ctx, oxigenBin, "check", tmpPath)
	output, err := cmd.Output()
	if ctx.Err() != nil {
		return nil // cancelled or timed out
	}
	if err != nil {
		// oxigen check not available or failed — return empty
		if _, ok := err.(*exec.ExitError); !ok {
			return []Diagnostic{}
		}
		// ExitError with output is fine — parse stdout anyway
		if len(output) == 0 {
			return []Diagnostic{}
		}
	}

	var checks []checkDiagnostic
	if err := json.Unmarshal(output, &checks); err != nil {
		return []Diagnostic{}
	}

	diagnostics := make([]Diagnostic, 0, len(checks))
	source := "oxigen"
	for _, c := range checks {
		line := uint32(0)
		if c.Line > 0 {
			line = uint32(c.Line - 1)
		}
		col := uint32(0)
		if c.Column > 0 {
			col = uint32(c.Column - 1)
		}

		severity := SeverityError
		if c.Severity == "warning" {
			severity = SeverityWarning
		}

		message := c.Message
		if c.Suggestion != nil && *c.Suggestion != "" {
			message += "\nhint: " + *c.Suggestion
		}

		diagnostics = append(diagnostics, Diagnostic{
			Range: Range{
				Start: Position{Line: line, Character: col},
				End:   Position{Line: line, Character: col + 1},
			},
			Severity: &severity,
			Source:   &source,
			Message:  message,
		})
	}

	return diagnostics
}
