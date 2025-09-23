// jv_lsp - Language Server Protocol implementation
use jv_checker::diagnostics::{from_parse_error, from_transform_error};
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
}

impl JvLanguageServer {
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
        }
    }

    pub fn open_document(&mut self, uri: String, content: String) {
        self.documents.insert(uri, content);
    }

    pub fn get_diagnostics(&self, uri: &str) -> Vec<Diagnostic> {
        let Some(content) = self.documents.get(uri) else {
            return Vec::new();
        };

        match JvParser::parse(content) {
            Ok(program) => match transform_program(program) {
                Ok(_) => Vec::new(),
                Err(error) => match from_transform_error(&error) {
                    Some(diagnostic) => vec![tooling_diagnostic_to_lsp(uri, diagnostic)],
                    None => vec![fallback_diagnostic(uri, "IR transformation error")],
                },
            },
            Err(error) => match from_parse_error(&error) {
                Some(diagnostic) => vec![tooling_diagnostic_to_lsp(uri, diagnostic)],
                None => vec![fallback_diagnostic(uri, "Parser error")],
            },
        }
    }

    pub fn get_completions(&self, _uri: &str, _position: Position) -> Vec<String> {
        // Placeholder implementation
        vec!["val".to_string(), "var".to_string(), "fun".to_string()]
    }
}

impl Default for JvLanguageServer {
    fn default() -> Self {
        Self::new()
    }
}

fn tooling_diagnostic_to_lsp(uri: &str, diagnostic: jv_checker::diagnostics::ToolingDiagnostic) -> Diagnostic {
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
