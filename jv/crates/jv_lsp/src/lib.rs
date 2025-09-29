// jv_lsp - Language Server Protocol implementation
use jv_checker::diagnostics::{from_check_error, from_parse_error, from_transform_error};
use jv_checker::{CheckError, TypeChecker};
use jv_inference::service::TypeFactsSnapshot;
use jv_ir::transform_program;
use jv_parser::Parser as JvParser;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LspError {
    #[error("Protocol error: {0}")]
    ProtocolError(String),
    #[error("Parse error: {0}")]
    ParseError(String),
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    pub line: u32,
    pub character: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Location {
    pub uri: String,
    pub range: Range,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    pub range: Range,
    pub severity: Option<DiagnosticSeverity>,
    pub message: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DiagnosticSeverity {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4,
}

pub struct JvLanguageServer {
    documents: HashMap<String, String>,
    type_facts: HashMap<String, TypeFactsSnapshot>,
}

impl JvLanguageServer {
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
            type_facts: HashMap::new(),
        }
    }

    pub fn open_document(&mut self, uri: String, content: String) {
        self.documents.insert(uri.clone(), content);
        self.type_facts.remove(&uri);
    }

    pub fn get_diagnostics(&mut self, uri: &str) -> Vec<Diagnostic> {
        let Some(content) = self.documents.get(uri) else {
            return Vec::new();
        };

        let program = match JvParser::parse(content) {
            Ok(program) => program,
            Err(error) => {
                self.type_facts.remove(uri);
                return match from_parse_error(&error) {
                    Some(diagnostic) => vec![tooling_diagnostic_to_lsp(uri, diagnostic)],
                    None => vec![fallback_diagnostic(uri, "Parser error")],
                };
            }
        };

        let mut diagnostics = Vec::new();
        let mut type_facts_snapshot: Option<TypeFactsSnapshot> = None;

        let mut checker = TypeChecker::new();
        match checker.check_program(&program) {
            Ok(_) => {
                if let Some(snapshot) = checker.take_inference_snapshot() {
                    type_facts_snapshot = Some(snapshot.type_facts().clone());
                } else {
                    self.type_facts.remove(uri);
                }
                diagnostics.extend(
                    checker
                        .check_null_safety(&program)
                        .into_iter()
                        .map(|warning| warning_diagnostic(uri, warning)),
                );
            }
            Err(errors) => {
                self.type_facts.remove(uri);
                return errors
                    .into_iter()
                    .map(|error| type_error_to_diagnostic(uri, error))
                    .collect();
            }
        }

        match transform_program(program) {
            Ok(_) => {}
            Err(error) => {
                diagnostics.push(match from_transform_error(&error) {
                    Some(diagnostic) => tooling_diagnostic_to_lsp(uri, diagnostic),
                    None => fallback_diagnostic(uri, "IR transformation error"),
                });
            }
        }

        if let Some(snapshot) = type_facts_snapshot {
            self.type_facts.insert(uri.to_string(), snapshot);
        }

        diagnostics
    }

    pub fn get_completions(&self, _uri: &str, _position: Position) -> Vec<String> {
        // Placeholder implementation
        vec!["val".to_string(), "var".to_string(), "fun".to_string()]
    }

    pub fn type_facts(&self, uri: &str) -> Option<&TypeFactsSnapshot> {
        self.type_facts.get(uri)
    }
}

impl Default for JvLanguageServer {
    fn default() -> Self {
        Self::new()
    }
}

fn tooling_diagnostic_to_lsp(
    uri: &str,
    diagnostic: jv_checker::diagnostics::ToolingDiagnostic,
) -> Diagnostic {
    let range = diagnostic
        .span
        .as_ref()
        .map(span_to_range)
        .unwrap_or_else(default_range);

    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::Error),
        message: format!(
            "{code}: {title}\nSource: {uri}\n{detail}\nHint: {help}",
            code = diagnostic.code,
            title = diagnostic.title,
            uri = uri,
            detail = diagnostic.message,
            help = diagnostic.help
        ),
    }
}

fn fallback_diagnostic(uri: &str, label: &str) -> Diagnostic {
    let message = format!("{}: unable to analyse {}", label, uri);
    Diagnostic {
        range: default_range(),
        severity: Some(DiagnosticSeverity::Warning),
        message,
    }
}

fn warning_diagnostic(uri: &str, warning: CheckError) -> Diagnostic {
    Diagnostic {
        range: default_range(),
        severity: Some(DiagnosticSeverity::Warning),
        message: format!("Warning ({uri}): {warning}"),
    }
}

fn type_error_to_diagnostic(uri: &str, error: CheckError) -> Diagnostic {
    if let Some(diagnostic) = from_check_error(&error) {
        tooling_diagnostic_to_lsp(uri, diagnostic)
    } else {
        Diagnostic {
            range: default_range(),
            severity: Some(DiagnosticSeverity::Error),
            message: format!("Type error: {error}"),
        }
    }
}

fn default_range() -> Range {
    Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: 0,
            character: 1,
        },
    }
}

fn span_to_range(span: &jv_ast::Span) -> Range {
    Range {
        start: Position {
            line: span.start_line.saturating_sub(1) as u32,
            character: span.start_column.saturating_sub(1) as u32,
        },
        end: Position {
            line: span.end_line.saturating_sub(1) as u32,
            character: span.end_column.saturating_sub(1) as u32,
        },
    }
}

#[cfg(test)]
mod tests;
