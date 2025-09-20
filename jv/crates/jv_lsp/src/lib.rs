// jv_lsp - Language Server Protocol implementation
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

    pub fn get_diagnostics(&self, _uri: &str) -> Vec<Diagnostic> {
        // Placeholder implementation
        vec![]
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

#[cfg(test)]
mod tests;
