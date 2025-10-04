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
        help: None,
        suggestions: Vec::new(),
        strategy: None,
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
fn test_diagnostics_for_clean_source() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///test.jv".to_string();
    server.open_document(uri.clone(), "val numbers = [1, 2, 3]".to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert!(diagnostics.is_empty());
}

#[test]
fn test_diagnostics_for_mixed_delimiters() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///test.jv".to_string();
    server.open_document(uri.clone(), "val numbers = [1, 2 3]".to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert_eq!(diagnostics.len(), 1);
    assert!(diagnostics[0].message.contains("JV2101"));
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

#[test]
fn caches_type_facts_after_successful_inference() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///facts.jv".to_string();
    server.open_document(uri.clone(), "val greeting = \"hello\"".to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert!(diagnostics.is_empty());

    let facts = server
        .type_facts(&uri)
        .expect("type facts should be cached");
    let environment = facts.to_json();
    let bindings = environment
        .get("environment")
        .and_then(serde_json::Value::as_object)
        .expect("environment map present");
    let greeting = bindings
        .get("greeting")
        .and_then(serde_json::Value::as_str)
        .expect("greeting binding exported");
    assert!(greeting.contains("Primitive(\"String\")"));
}

#[test]
fn reports_type_error_for_ambiguous_function() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///ambiguous.jv".to_string();
    server.open_document(uri.clone(), "fun ambiguous(x) { null }".to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert!(!diagnostics.is_empty());
    assert!(diagnostics
        .iter()
        .any(|diag| diag.message.contains("ambiguous function signature")));
    assert!(server.type_facts(&uri).is_none());
}

#[test]
fn surfaces_null_safety_warning() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///null.jv".to_string();
    server.open_document(uri.clone(), "val message: String = null".to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert!(diagnostics.iter().any(|diag| matches!(
        diag.severity,
        Some(DiagnosticSeverity::Warning)
    ) && diag.message.contains("Null safety")));
}
