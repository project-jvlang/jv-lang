// jv-lsp - Language Server Protocol server for jv language
use jv_lsp::JvLanguageServer;
use std::error::Error;
use tokio::io::{stdin, stdout};
use tower_lsp::{jsonrpc::Result as LspResult, lsp_types::*, LspService, Server};

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    tracing_subscriber::fmt().init();

    let stdin = stdin();
    let stdout = stdout();

    let (service, socket) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;

    Ok(())
}

struct Backend {
    client: tower_lsp::Client,
    language_server: JvLanguageServer,
}

impl Backend {
    fn new(client: tower_lsp::Client) -> Self {
        Self {
            client,
            language_server: JvLanguageServer::new(),
        }
    }
}

#[tower_lsp::async_trait]
impl tower_lsp::LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> LspResult<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "jv-lsp".to_string(),
                version: Some("0.1.0".to_string()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: Some("jv".to_string()),
                        inter_file_dependencies: true,
                        workspace_diagnostics: false,
                        work_done_progress_options: Default::default(),
                    },
                )),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "jv-lsp server initialized!")
            .await;
    }

    async fn shutdown(&self) -> LspResult<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let content = params.text_document.text;

        // Store document content
        // Note: This is a simplified approach. In a real implementation,
        // you'd want thread-safe access to the language server state.
        // For now, we'll just trigger diagnostics
        self.publish_diagnostics(uri, content).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        if let Some(change) = params.content_changes.into_iter().next() {
            self.publish_diagnostics(uri, change.text).await;
        }
    }

    async fn completion(&self, params: CompletionParams) -> LspResult<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;

        let jv_position = jv_lsp::Position {
            line: position.line,
            character: position.character,
        };

        let completions = self.language_server.get_completions(&uri, jv_position);

        let completion_items: Vec<CompletionItem> = completions
            .into_iter()
            .map(|label| CompletionItem {
                label,
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            })
            .collect();

        Ok(Some(CompletionResponse::Array(completion_items)))
    }

    async fn hover(&self, params: HoverParams) -> LspResult<Option<Hover>> {
        let _uri = params.text_document_position_params.text_document.uri.to_string();
        let _position = params.text_document_position_params.position;

        // Placeholder hover implementation
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(
                "jv language hover support".to_string(),
            )),
            range: None,
        }))
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> LspResult<DocumentDiagnosticReportResult> {
        let uri = params.text_document.uri.to_string();
        let diagnostics = self.get_diagnostics_for_document(&uri).await;

        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: diagnostics,
                },
            }),
        ))
    }
}

impl Backend {
    async fn publish_diagnostics(&self, uri: String, content: String) {
        // For now, we'll create a temporary document storage approach
        // In a real implementation, you'd want proper document management
        let mut temp_server = JvLanguageServer::new();
        temp_server.open_document(uri.clone(), content);

        let diagnostics = temp_server.get_diagnostics(&uri);
        let lsp_diagnostics = diagnostics
            .into_iter()
            .map(|diag| Diagnostic {
                range: Range {
                    start: Position {
                        line: diag.range.start.line,
                        character: diag.range.start.character,
                    },
                    end: Position {
                        line: diag.range.end.line,
                        character: diag.range.end.character,
                    },
                },
                severity: diag.severity.map(|sev| match sev {
                    jv_lsp::DiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
                    jv_lsp::DiagnosticSeverity::Warning => DiagnosticSeverity::WARNING,
                    jv_lsp::DiagnosticSeverity::Information => DiagnosticSeverity::INFORMATION,
                    jv_lsp::DiagnosticSeverity::Hint => DiagnosticSeverity::HINT,
                }),
                message: diag.message,
                source: Some("jv-lsp".to_string()),
                ..Default::default()
            })
            .collect();

        self.client
            .publish_diagnostics(
                tower_lsp::lsp_types::Url::parse(&uri).unwrap(),
                lsp_diagnostics,
                None,
            )
            .await;
    }

    async fn get_diagnostics_for_document(&self, uri: &str) -> Vec<Diagnostic> {
        // Placeholder - in a real implementation you'd retrieve stored document content
        let temp_server = JvLanguageServer::new();
        let diagnostics = temp_server.get_diagnostics(uri);

        diagnostics
            .into_iter()
            .map(|diag| Diagnostic {
                range: Range {
                    start: Position {
                        line: diag.range.start.line,
                        character: diag.range.start.character,
                    },
                    end: Position {
                        line: diag.range.end.line,
                        character: diag.range.end.character,
                    },
                },
                severity: diag.severity.map(|sev| match sev {
                    jv_lsp::DiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
                    jv_lsp::DiagnosticSeverity::Warning => DiagnosticSeverity::WARNING,
                    jv_lsp::DiagnosticSeverity::Information => DiagnosticSeverity::INFORMATION,
                    jv_lsp::DiagnosticSeverity::Hint => DiagnosticSeverity::HINT,
                }),
                message: diag.message,
                source: Some("jv-lsp".to_string()),
                ..Default::default()
            })
            .collect()
    }
}