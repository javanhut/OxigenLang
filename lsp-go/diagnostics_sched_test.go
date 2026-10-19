package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"
)

// syncBuffer is a bytes.Buffer safe to write from the diagnostics goroutines.
type syncBuffer struct {
	mu  sync.Mutex
	buf bytes.Buffer
}

func (b *syncBuffer) Write(p []byte) (int, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.Write(p)
}

func (b *syncBuffer) String() string {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.String()
}

// fakeServer runs diagnostics with a stand-in for `oxigen check` that takes
// `delay` and reports one error.
func fakeServer(t *testing.T, delay string) (*Server, *syncBuffer) {
	t.Helper()
	bin := filepath.Join(t.TempDir(), "oxigen")
	script := "#!/bin/sh\nsleep " + delay + "\necho '[{\"line\":1,\"column\":1,\"message\":\"boom\",\"severity\":\"error\"}]'\n"
	if err := os.WriteFile(bin, []byte(script), 0o755); err != nil {
		t.Fatal(err)
	}
	out := &syncBuffer{}
	s := NewServer()
	s.writer = bufio.NewWriter(out)
	s.oxigenBin = bin
	return s, out
}

// published returns each publishDiagnostics notification, in order.
func published(t *testing.T, raw string) []PublishDiagnosticsParams {
	t.Helper()
	var out []PublishDiagnosticsParams
	r := bufio.NewReader(strings.NewReader(raw))
	for {
		body, err := readMessage(r)
		if err != nil {
			return out
		}
		var n struct {
			Method string                   `json:"method"`
			Params PublishDiagnosticsParams `json:"params"`
		}
		if json.Unmarshal(body, &n) == nil && n.Method == "textDocument/publishDiagnostics" {
			out = append(out, n.Params)
		}
	}
}

func open(t *testing.T, s *Server, uri string) {
	t.Helper()
	params, _ := json.Marshal(DidOpenTextDocumentParams{
		TextDocument: TextDocumentItem{URI: uri, Text: "x := 1\n", Version: 1},
	})
	s.handleDidOpen(Request{Method: "textDocument/didOpen", Params: params})
}

// One in-flight run was shared by every document: opening a second file
// cancelled the first file's diagnostics, which then never arrived.
func TestEveryOpenedDocumentGetsDiagnostics(t *testing.T) {
	s, out := fakeServer(t, "0.2")
	open(t, s, "file:///a.oxi")
	open(t, s, "file:///b.oxi")
	time.Sleep(1500 * time.Millisecond)

	got := map[string]bool{}
	for _, p := range published(t, out.String()) {
		if len(p.Diagnostics) > 0 {
			got[p.URI] = true
		}
	}
	if !got["file:///a.oxi"] || !got["file:///b.oxi"] {
		t.Fatalf("diagnostics published for %v, want both files", got)
	}
}

// A run still in flight when its document closed published afterwards,
// leaving diagnostics on a file the editor no longer has open.
func TestNoDiagnosticsArriveAfterClose(t *testing.T) {
	s, out := fakeServer(t, "0.3")
	open(t, s, "file:///c.oxi")
	time.Sleep(50 * time.Millisecond)
	params, _ := json.Marshal(DidCloseTextDocumentParams{
		TextDocument: TextDocumentIdentifier{URI: "file:///c.oxi"},
	})
	s.handleDidClose(Request{Method: "textDocument/didClose", Params: params})
	time.Sleep(1 * time.Second)

	pubs := published(t, out.String())
	if len(pubs) == 0 {
		t.Fatal("expected the close to clear diagnostics")
	}
	if last := pubs[len(pubs)-1]; len(last.Diagnostics) != 0 {
		t.Fatalf("diagnostics published after close: %+v", last)
	}
}
