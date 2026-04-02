use std::collections::HashMap;
use std::sync::Mutex;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use oxigen_core::ast::Program;
use oxigen_core::formatter::Formatter;
use oxigen_core::lexer::Lexer;
use oxigen_core::parser::{Diagnostic as OxiDiagnostic, Parser};

use crate::completion::get_completions;
use crate::diagnostics::to_lsp_diagnostics;
use crate::hover::get_hover;
use crate::symbols::get_document_symbols;

struct DocumentState {
    content: String,
    #[allow(dead_code)]
    diagnostics: Vec<OxiDiagnostic>,
    program: Option<Program>,
}

pub struct OxigenLanguageServer {
    client: Client,
    documents: Mutex<HashMap<Url, DocumentState>>,
}

impl OxigenLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Mutex::new(HashMap::new()),
        }
    }

    fn parse_and_publish(&self, uri: Url, text: String, version: Option<i32>) {
        let lexer = Lexer::new(&text);
        let mut parser = Parser::new(lexer, &text);
        let program = parser.parse_program();
        let errors = parser.errors().to_vec();
        let lsp_diagnostics = to_lsp_diagnostics(&errors);

        let has_errors = !errors.is_empty();
        let doc = DocumentState {
            content: text,
            diagnostics: errors,
            program: if has_errors { None } else { Some(program) },
        };

        if let Ok(mut docs) = self.documents.lock() {
            docs.insert(uri.clone(), doc);
        }

        let client = self.client.clone();
        tokio::spawn(async move {
            client
                .publish_diagnostics(uri, lsp_diagnostics, version)
                .await;
        });
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for OxigenLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), "<".to_string()]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "oxigen-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "oxigen-lsp initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let version = Some(params.text_document.version);
        self.parse_and_publish(uri, text, version);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = Some(params.text_document.version);
        if let Some(change) = params.content_changes.into_iter().last() {
            self.parse_and_publish(uri, change.text, version);
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        if let Ok(mut docs) = self.documents.lock() {
            docs.remove(&params.text_document.uri);
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let content = {
            let docs = self.documents.lock().unwrap();
            docs.get(uri).map(|d| d.content.clone())
        };

        if let Some(text) = content {
            let items = get_completions(&text, position);
            Ok(Some(CompletionResponse::Array(items)))
        } else {
            Ok(None)
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let content = {
            let docs = self.documents.lock().unwrap();
            docs.get(uri).map(|d| d.content.clone())
        };

        if let Some(text) = content {
            Ok(get_hover(&text, position))
        } else {
            Ok(None)
        }
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;

        let program = {
            let docs = self.documents.lock().unwrap();
            docs.get(uri).and_then(|d| d.program.clone())
        };

        if let Some(program) = program {
            let symbols = get_document_symbols(&program);
            Ok(Some(DocumentSymbolResponse::Flat(symbols)))
        } else {
            Ok(None)
        }
    }

    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;

        let content = {
            let docs = self.documents.lock().unwrap();
            docs.get(uri).map(|d| d.content.clone())
        };

        let Some(text) = content else {
            return Ok(None);
        };

        // Parse the document — only format if it parses without errors
        let lexer = Lexer::new(&text);
        let mut parser = Parser::new(lexer, &text);
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            return Ok(None);
        }

        let formatted = Formatter::format(&program);

        if formatted == text {
            return Ok(None);
        }

        // Replace the entire document
        let line_count = text.lines().count() as u32;
        let last_line_len = text.lines().last().map(|l| l.len() as u32).unwrap_or(0);

        Ok(Some(vec![TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: line_count,
                    character: last_line_len,
                },
            },
            new_text: formatted,
        }]))
    }
}
