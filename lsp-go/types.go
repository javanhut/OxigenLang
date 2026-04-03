package main

import "encoding/json"

// ── JSON-RPC 2.0 ──

type Request struct {
	JSONRPC string           `json:"jsonrpc"`
	ID      *json.RawMessage `json:"id,omitempty"`
	Method  string           `json:"method"`
	Params  json.RawMessage  `json:"params,omitempty"`
}

type Response struct {
	JSONRPC string           `json:"jsonrpc"`
	ID      *json.RawMessage `json:"id"`
	Result  json.RawMessage  `json:"result,omitempty"`
	Error   *ResponseError   `json:"error,omitempty"`
}

type Notification struct {
	JSONRPC string          `json:"jsonrpc"`
	Method  string          `json:"method"`
	Params  json.RawMessage `json:"params,omitempty"`
}

type ResponseError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

// ── LSP position types ──

type Position struct {
	Line      uint32 `json:"line"`
	Character uint32 `json:"character"`
}

type Range struct {
	Start Position `json:"start"`
	End   Position `json:"end"`
}

type Location struct {
	URI   string `json:"uri"`
	Range Range  `json:"range"`
}

// ── Text document types ──

type TextDocumentIdentifier struct {
	URI string `json:"uri"`
}

type VersionedTextDocumentIdentifier struct {
	URI     string `json:"uri"`
	Version int    `json:"version"`
}

type TextDocumentItem struct {
	URI        string `json:"uri"`
	LanguageID string `json:"languageId"`
	Version    int    `json:"version"`
	Text       string `json:"text"`
}

// ── Initialize ──

type InitializeParams struct {
	RootURI string `json:"rootUri,omitempty"`
}

type InitializeResult struct {
	Capabilities ServerCapabilities `json:"capabilities"`
	ServerInfo   *ServerInfo        `json:"serverInfo,omitempty"`
}

type ServerCapabilities struct {
	TextDocumentSync           int                `json:"textDocumentSync"`
	CompletionProvider         *CompletionOptions `json:"completionProvider,omitempty"`
	HoverProvider              bool               `json:"hoverProvider,omitempty"`
	DocumentSymbolProvider     bool               `json:"documentSymbolProvider,omitempty"`
	DocumentFormattingProvider bool               `json:"documentFormattingProvider,omitempty"`
}

type CompletionOptions struct {
	TriggerCharacters []string `json:"triggerCharacters,omitempty"`
}

type ServerInfo struct {
	Name    string `json:"name"`
	Version string `json:"version,omitempty"`
}

// ── Document sync ──

type DidOpenTextDocumentParams struct {
	TextDocument TextDocumentItem `json:"textDocument"`
}

type DidChangeTextDocumentParams struct {
	TextDocument   VersionedTextDocumentIdentifier `json:"textDocument"`
	ContentChanges []TextDocumentContentChangeEvent `json:"contentChanges"`
}

type TextDocumentContentChangeEvent struct {
	Text string `json:"text"`
}

type DidCloseTextDocumentParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
}

// ── Diagnostics ──

type PublishDiagnosticsParams struct {
	URI         string       `json:"uri"`
	Version     *int         `json:"version,omitempty"`
	Diagnostics []Diagnostic `json:"diagnostics"`
}

type Diagnostic struct {
	Range    Range   `json:"range"`
	Severity *int    `json:"severity,omitempty"`
	Source   *string `json:"source,omitempty"`
	Message  string  `json:"message"`
}

// ── Completion ──

type CompletionParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Position     Position               `json:"position"`
}

type CompletionItem struct {
	Label  string `json:"label"`
	Kind   *int   `json:"kind,omitempty"`
	Detail string `json:"detail,omitempty"`
}

// ── Hover ──

type HoverParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Position     Position               `json:"position"`
}

type Hover struct {
	Contents MarkupContent `json:"contents"`
}

type MarkupContent struct {
	Kind  string `json:"kind"`
	Value string `json:"value"`
}

// ── Document symbols ──

type DocumentSymbolParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
}

type SymbolInformation struct {
	Name     string   `json:"name"`
	Kind     int      `json:"kind"`
	Location Location `json:"location"`
}

// ── Formatting ──

type DocumentFormattingParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
}

type TextEdit struct {
	Range   Range  `json:"range"`
	NewText string `json:"newText"`
}

// ── LSP constants ──

const (
	SeverityError   = 1
	SeverityWarning = 2
)

const (
	CompletionKindFunction      = 3
	CompletionKindModule        = 9
	CompletionKindKeyword       = 14
	CompletionKindTypeParameter = 25
)

const (
	SymbolKindModule   = 2
	SymbolKindStruct   = 23
	SymbolKindMethod   = 6
	SymbolKindField    = 8
	SymbolKindFunction = 12
	SymbolKindVariable = 13
)
