// jv-lsp - Language Server Protocol server for jv language
use jv_lsp::{ImportsParams, JvLanguageServer};
use std::collections::HashMap;
use std::error::Error;
use tokio::io::{stdin, stdout};
use tokio::sync::{Mutex, RwLock};
use tower_lsp::{
    jsonrpc::{Error, ErrorCode, Result as LspResult},
    lsp_types::*,
    LspService,
    Server,
};

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
    language_server: Mutex<JvLanguageServer>,
    documents: RwLock<HashMap<String, String>>,
}

impl Backend {
    fn new(client: tower_lsp::Client) -> Self {
        Self {
            client,
            language_server: Mutex::new(JvLanguageServer::new()),
            documents: RwLock::new(HashMap::new()),
        }
    }
}

fn map_diagnostic(diag: jv_lsp::Diagnostic) -> Diagnostic {
    Diagnostic {
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
        source: diag.source.clone(),
        code: diag
            .code
            .map(tower_lsp::lsp_types::NumberOrString::String),
        ..Default::default()
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
        {
            let mut server = self.language_server.lock().await;
            server.open_document(uri.clone(), content.clone());
        }
        {
            let mut docs = self.documents.write().await;
            docs.insert(uri.clone(), content);
        }

        self.publish_diagnostics(uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        if let Some(change) = params.content_changes.into_iter().next() {
            let content = change.text;
            {
                let mut server = self.language_server.lock().await;
                server.open_document(uri.clone(), content.clone());
            }
            {
                let mut docs = self.documents.write().await;
                docs.insert(uri.clone(), content);
            }
            self.publish_diagnostics(uri).await;
        }
    }
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        {
            let mut server = self.language_server.lock().await;
            server.close_document(&uri);
        }
        {
            let mut docs = self.documents.write().await;
            docs.remove(&uri);
        }

        if let Ok(url) = Url::parse(&uri) {
            self.client.publish_diagnostics(url, Vec::new(), None).await;
        }
    }

    async fn completion(&self, params: CompletionParams) -> LspResult<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;

        let jv_position = jv_lsp::Position {
            line: position.line,
            character: position.character,
        };

        let completions = {
            let server = self.language_server.lock().await;
            server.get_completions(&uri, jv_position)
        };

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
        let _uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let _position = params.text_document_position_params.position;

        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let jv_position = jv_lsp::Position {
            line: position.line,
            character: position.character,
        };

        let hover = {
            let server = self.language_server.lock().await;
            server.get_hover(&uri, jv_position)
        };

        Ok(hover.map(|result| Hover {
            contents: HoverContents::Scalar(MarkedString::String(result.contents)),
            range: Some(Range {
                start: Position {
                    line: result.range.start.line,
                    character: result.range.start.character,
                },
                end: Position {
                    line: result.range.end.line,
                    character: result.range.end.character,
                },
            }),
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

    async fn request(
        &self,
        method: &str,
        params: serde_json::Value,
    ) -> LspResult<Option<serde_json::Value>> {
        match method {
            "jv/imports" => {
                let params: ImportsParams = serde_json::from_value(params).map_err(|error| {
                    Error::invalid_params(format!("Invalid params: {error}"))
                })?;
                let uri = params.text_document.uri.to_string();
                let content = {
                    let docs = self.documents.read().await;
                    docs.get(&uri).cloned()
                }
                .ok_or_else(|| Error::invalid_params(format!("Document not open: {uri}")))?;

                let response = {
                    let server = self.language_server.lock().await;
                    match server.imports_response(&content) {
                        Ok(response) => response,
                        Err(error) => {
                            return Err(Error {
                                code: ErrorCode::InternalError,
                                message: error.message().to_string(),
                                data: None,
                            });
                        }
                    }
                };

                let value = serde_json::to_value(response).map_err(|error| Error {
                    code: ErrorCode::InternalError,
                    message: error.to_string(),
                    data: None,
                })?;
                Ok(Some(value))
            }
            _ => Ok(None),
        }
    }
}

impl Backend {
    async fn publish_diagnostics(&self, uri: String) {
        let diagnostics = {
            let mut server = self.language_server.lock().await;
            server.get_diagnostics(&uri)
        };

        let lsp_diagnostics = diagnostics
            .into_iter()
            .map(|diag| map_diagnostic(diag))
            .collect::<Vec<_>>();

        if let Ok(url) = Url::parse(&uri) {
            self.client.publish_diagnostics(url, lsp_diagnostics, None).await;
        }
    }

    async fn get_diagnostics_for_document(&self, uri: &str) -> Vec<Diagnostic> {
        let diagnostics = {
            let mut server = self.language_server.lock().await;
            server.get_diagnostics(uri)
        };

        diagnostics.into_iter().map(map_diagnostic).collect()
    }
}
