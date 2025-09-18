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
mod tests {
    use super::*;

    #[test]
    fn test_position_creation() {
        let pos = Position {
            line: 5,
            character: 10,
        };
        assert_eq!(pos.line, 5);
        assert_eq!(pos.character, 10);
    }

    #[test]
    fn test_range_creation() {
        let start = Position {
            line: 1,
            character: 0,
        };
        let end = Position {
            line: 1,
            character: 10,
        };
        let range = Range {
            start: start.clone(),
            end: end.clone(),
        };

        assert_eq!(range.start.line, start.line);
        assert_eq!(range.start.character, start.character);
        assert_eq!(range.end.line, end.line);
        assert_eq!(range.end.character, end.character);
    }

    #[test]
    fn test_location_creation() {
        let location = Location {
            uri: "file:///test.jv".to_string(),
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 5,
                },
            },
        };

        assert_eq!(location.uri, "file:///test.jv");
        assert_eq!(location.range.start.line, 0);
    }

    #[test]
    fn test_diagnostic_creation() {
        let diagnostic = Diagnostic {
            range: Range {
                start: Position {
                    line: 2,
                    character: 5,
                },
                end: Position {
                    line: 2,
                    character: 15,
                },
            },
            severity: Some(DiagnosticSeverity::Error),
            message: "Type error".to_string(),
        };

        assert_eq!(diagnostic.message, "Type error");
        assert!(matches!(
            diagnostic.severity,
            Some(DiagnosticSeverity::Error)
        ));
    }

    #[test]
    fn test_diagnostic_severity_values() {
        assert_eq!(DiagnosticSeverity::Error as u8, 1);
        assert_eq!(DiagnosticSeverity::Warning as u8, 2);
        assert_eq!(DiagnosticSeverity::Information as u8, 3);
        assert_eq!(DiagnosticSeverity::Hint as u8, 4);
    }

    #[test]
    fn test_language_server_creation() {
        let server = JvLanguageServer::new();
        assert!(server.documents.is_empty());

        let server_default = JvLanguageServer::default();
        assert!(server_default.documents.is_empty());
    }

    #[test]
    fn test_document_management() {
        let mut server = JvLanguageServer::new();

        let uri = "file:///test.jv".to_string();
        let content = "val x = 42".to_string();

        server.open_document(uri.clone(), content.clone());
        assert_eq!(server.documents.get(&uri), Some(&content));
    }

    #[test]
    fn test_multiple_documents() {
        let mut server = JvLanguageServer::new();

        server.open_document("file:///test1.jv".to_string(), "val x = 1".to_string());
        server.open_document("file:///test2.jv".to_string(), "val y = 2".to_string());

        assert_eq!(server.documents.len(), 2);
        assert_eq!(
            server.documents.get("file:///test1.jv"),
            Some(&"val x = 1".to_string())
        );
        assert_eq!(
            server.documents.get("file:///test2.jv"),
            Some(&"val y = 2".to_string())
        );
    }

    #[test]
    fn test_diagnostics_placeholder() {
        let server = JvLanguageServer::new();
        let diagnostics = server.get_diagnostics("file:///test.jv");
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_completions_placeholder() {
        let server = JvLanguageServer::new();
        let position = Position {
            line: 0,
            character: 0,
        };
        let completions = server.get_completions("file:///test.jv", position);

        assert!(!completions.is_empty());
        assert!(completions.contains(&"val".to_string()));
        assert!(completions.contains(&"var".to_string()));
        assert!(completions.contains(&"fun".to_string()));
    }

    #[test]
    fn test_lsp_error_display() {
        let protocol_error = LspError::ProtocolError("Invalid request".to_string());
        let parse_error = LspError::ParseError("Syntax error".to_string());

        assert!(protocol_error.to_string().contains("Protocol error"));
        assert!(parse_error.to_string().contains("Parse error"));
    }

    #[test]
    fn test_serialization() {
        let pos = Position {
            line: 1,
            character: 5,
        };
        let json = serde_json::to_string(&pos).unwrap();
        assert!(json.contains("\"line\":1"));
        assert!(json.contains("\"character\":5"));

        let deserialized: Position = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized.line, pos.line);
        assert_eq!(deserialized.character, pos.character);
    }

    #[test]
    fn test_document_update() {
        let mut server = JvLanguageServer::new();
        let uri = "file:///test.jv".to_string();

        // Open initial document
        server.open_document(uri.clone(), "val x = 1".to_string());
        assert_eq!(server.documents.get(&uri), Some(&"val x = 1".to_string()));

        // Update document content
        server.open_document(uri.clone(), "val x = 42".to_string());
        assert_eq!(server.documents.get(&uri), Some(&"val x = 42".to_string()));
    }
}
