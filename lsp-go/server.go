package main

import (
	"bufio"
	"encoding/json"
	"log"
	"os"
	"sync"
)

type documentState struct {
	content string
	version int
}

type Server struct {
	documents  map[string]*documentState
	mu         sync.Mutex
	writer     *bufio.Writer
	writerMu   sync.Mutex
	oxigenBin  string
	stdlibPath string
}

func NewServer() *Server {
	return &Server{
		documents:  make(map[string]*documentState),
		writer:     bufio.NewWriter(os.Stdout),
		oxigenBin:  findOxigenBinary(),
		stdlibPath: findStdlibPath(),
	}
}

func (s *Server) Run() {
	reader := bufio.NewReader(os.Stdin)

	for {
		body, err := readMessage(reader)
		if err != nil {
			log.Printf("read error: %v", err)
			return
		}

		var msg Request
		if err := json.Unmarshal(body, &msg); err != nil {
			log.Printf("unmarshal error: %v", err)
			continue
		}

		if msg.ID != nil {
			s.handleRequest(msg)
		} else {
			s.handleNotification(msg)
		}
	}
}

// ── Transport helpers ──

func (s *Server) send(data []byte) {
	s.writerMu.Lock()
	defer s.writerMu.Unlock()
	if err := writeMessage(s.writer, data); err != nil {
		log.Printf("write error: %v", err)
	}
	s.writer.Flush()
}

func (s *Server) respond(id *json.RawMessage, result interface{}) {
	resp := Response{JSONRPC: "2.0", ID: id}
	if result != nil {
		raw, err := json.Marshal(result)
		if err != nil {
			log.Printf("marshal result: %v", err)
			resp.Error = &ResponseError{Code: -32603, Message: "internal error"}
		} else {
			resp.Result = raw
		}
	} else {
		resp.Result = json.RawMessage("null")
	}
	body, err := json.Marshal(resp)
	if err != nil {
		log.Printf("marshal response: %v", err)
		return
	}
	s.send(body)
}

func (s *Server) notify(method string, params interface{}) {
	raw, err := json.Marshal(params)
	if err != nil {
		log.Printf("marshal notification: %v", err)
		return
	}
	msg := Notification{JSONRPC: "2.0", Method: method, Params: raw}
	body, err := json.Marshal(msg)
	if err != nil {
		log.Printf("marshal notification msg: %v", err)
		return
	}
	s.send(body)
}

// ── Dispatch ──

func (s *Server) handleRequest(msg Request) {
	switch msg.Method {
	case "initialize":
		s.handleInitialize(msg)
	case "shutdown":
		s.respond(msg.ID, nil)
	case "textDocument/completion":
		s.handleCompletion(msg)
	case "textDocument/hover":
		s.handleHover(msg)
	case "textDocument/documentSymbol":
		s.handleDocumentSymbol(msg)
	case "textDocument/formatting":
		s.handleFormatting(msg)
	default:
		resp := Response{
			JSONRPC: "2.0",
			ID:      msg.ID,
			Error:   &ResponseError{Code: -32601, Message: "method not found: " + msg.Method},
		}
		body, _ := json.Marshal(resp)
		s.send(body)
	}
}

func (s *Server) handleNotification(msg Request) {
	switch msg.Method {
	case "initialized":
		log.Println("oxigen-lsp initialized")
	case "exit":
		os.Exit(0)
	case "textDocument/didOpen":
		s.handleDidOpen(msg)
	case "textDocument/didChange":
		s.handleDidChange(msg)
	case "textDocument/didClose":
		s.handleDidClose(msg)
	}
}

// ── Lifecycle ──

func (s *Server) handleInitialize(msg Request) {
	result := InitializeResult{
		Capabilities: ServerCapabilities{
			TextDocumentSync: 1, // Full
			CompletionProvider: &CompletionOptions{
				TriggerCharacters: []string{".", "<"},
			},
			HoverProvider:              true,
			DocumentSymbolProvider:     true,
			DocumentFormattingProvider: true,
		},
		ServerInfo: &ServerInfo{
			Name:    "oxigen-lsp",
			Version: "0.2.0",
		},
	}
	s.respond(msg.ID, result)
}

// ── Document sync ──

func (s *Server) handleDidOpen(msg Request) {
	var params DidOpenTextDocumentParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return
	}

	s.mu.Lock()
	s.documents[params.TextDocument.URI] = &documentState{
		content: params.TextDocument.Text,
		version: params.TextDocument.Version,
	}
	s.mu.Unlock()

	s.publishDiagnostics(params.TextDocument.URI, params.TextDocument.Text, &params.TextDocument.Version)
}

func (s *Server) handleDidChange(msg Request) {
	var params DidChangeTextDocumentParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return
	}

	if len(params.ContentChanges) == 0 {
		return
	}
	text := params.ContentChanges[len(params.ContentChanges)-1].Text

	s.mu.Lock()
	s.documents[params.TextDocument.URI] = &documentState{
		content: text,
		version: params.TextDocument.Version,
	}
	s.mu.Unlock()

	s.publishDiagnostics(params.TextDocument.URI, text, &params.TextDocument.Version)
}

func (s *Server) handleDidClose(msg Request) {
	var params DidCloseTextDocumentParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return
	}

	s.mu.Lock()
	delete(s.documents, params.TextDocument.URI)
	s.mu.Unlock()

	// Clear diagnostics for closed document
	s.notify("textDocument/publishDiagnostics", PublishDiagnosticsParams{
		URI:         params.TextDocument.URI,
		Diagnostics: []Diagnostic{},
	})
}

// ── Feature handlers ──

func (s *Server) handleCompletion(msg Request) {
	var params CompletionParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		s.respond(msg.ID, nil)
		return
	}

	s.mu.Lock()
	doc, ok := s.documents[params.TextDocument.URI]
	var content string
	if ok {
		content = doc.content
	}
	s.mu.Unlock()

	if !ok {
		s.respond(msg.ID, nil)
		return
	}

	items := getCompletions(content, params.Position, s.stdlibPath)
	s.respond(msg.ID, items)
}

func (s *Server) handleHover(msg Request) {
	var params HoverParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		s.respond(msg.ID, nil)
		return
	}

	s.mu.Lock()
	doc, ok := s.documents[params.TextDocument.URI]
	var content string
	if ok {
		content = doc.content
	}
	s.mu.Unlock()

	if !ok {
		s.respond(msg.ID, nil)
		return
	}

	hover := getHoverInfo(content, params.Position)
	s.respond(msg.ID, hover)
}

func (s *Server) handleDocumentSymbol(msg Request) {
	var params DocumentSymbolParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		s.respond(msg.ID, nil)
		return
	}

	s.mu.Lock()
	doc, ok := s.documents[params.TextDocument.URI]
	var content string
	if ok {
		content = doc.content
	}
	s.mu.Unlock()

	if !ok {
		s.respond(msg.ID, nil)
		return
	}

	symbols := getDocumentSymbols(content, params.TextDocument.URI)
	s.respond(msg.ID, symbols)
}

func (s *Server) handleFormatting(msg Request) {
	var params DocumentFormattingParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		s.respond(msg.ID, nil)
		return
	}

	s.mu.Lock()
	doc, ok := s.documents[params.TextDocument.URI]
	var content string
	if ok {
		content = doc.content
	}
	s.mu.Unlock()

	if !ok {
		s.respond(msg.ID, nil)
		return
	}

	edits := formatDocument(content, s.oxigenBin)
	if len(edits) == 0 {
		s.respond(msg.ID, nil)
	} else {
		s.respond(msg.ID, edits)
	}
}

// ── Diagnostics publishing ──

func (s *Server) publishDiagnostics(uri, content string, version *int) {
	diagnostics := getDiagnostics(content, s.oxigenBin)
	if diagnostics == nil {
		diagnostics = []Diagnostic{}
	}
	s.notify("textDocument/publishDiagnostics", PublishDiagnosticsParams{
		URI:         uri,
		Version:     version,
		Diagnostics: diagnostics,
	})
}
