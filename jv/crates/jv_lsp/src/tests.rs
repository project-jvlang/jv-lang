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
        code: None,
        source: None,
        help: None,
        suggestions: Vec::new(),
        strategy: None,
    };

    assert_eq!(diagnostic.message, "Type error");
    assert!(matches!(
        diagnostic.severity,
        Some(DiagnosticSeverity::Error)
    ));
    assert!(diagnostic.code.is_none());
    assert!(diagnostic.source.is_none());
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
    server.open_document(uri.clone(), "val numbers = [1 2 3]".to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert!(diagnostics.is_empty());
}

#[test]
fn test_raw_string_highlight_is_captured() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///raw_highlight.jv".to_string();
    let source = "val path = 'C:\\\\Users\\\\Alice'".to_string();
    server.open_document(uri.clone(), source);

    let diagnostics = server.get_diagnostics(&uri);
    assert!(
        diagnostics.is_empty(),
        "raw literal should not emit diagnostics: {diagnostics:?}"
    );

    let highlights = server
        .token_highlights(&uri)
        .expect("raw literal highlight must be recorded");
    assert!(
        highlights
            .iter()
            .any(|entry| matches!(entry.kind, HighlightKind::RawSingle)),
        "expected RawSingle highlight entry: {highlights:?}"
    );

    let entry = highlights
        .iter()
        .find(|entry| matches!(entry.kind, HighlightKind::RawSingle))
        .expect("highlight entry missing");
    assert_eq!(entry.span.start_line, 1);
    assert!(
        entry.span.end_column > entry.span.start_column,
        "raw literal highlight should advance columns: {:?}",
        entry.span
    );
}

#[test]
fn test_diagnostics_for_mixed_delimiters() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///test.jv".to_string();
    server.open_document(uri.clone(), "val numbers = [1, 2 3]".to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert_eq!(diagnostics.len(), 1);
    assert!(diagnostics[0].message.contains("JV2101"));
    assert_eq!(diagnostics[0].code.as_deref(), Some("JV2101"));
    assert_eq!(diagnostics[0].source.as_deref(), Some("jv-lsp"));
}

#[test]
fn test_diagnostics_for_mixed_argument_commas() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///call.jv".to_string();
    let source = "fun plot(x: Int, y: Int): Int { x + y }\nval result = plot(1, 2)";
    server.open_document(uri.clone(), source.to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert!(
        diagnostics.iter().any(|diag| {
            diag.message.contains("JV2102") && diag.code.as_deref() == Some("JV2102")
        })
    );
}

#[test]
fn test_unterminated_raw_string_reports_jv4300() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///unterminated.jv".to_string();
    server.open_document(uri.clone(), "val bad = 'unterminated".to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert_eq!(diagnostics.len(), 1, "expected single diagnostic: {diagnostics:?}");
    assert_eq!(diagnostics[0].code.as_deref(), Some("JV4300"));
    assert!(
        diagnostics[0].message.contains("JV4300"),
        "diagnostic message should mention JV4300: {}",
        diagnostics[0].message
    );
}

#[test]
fn test_diagnostics_for_immutable_reassignment() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///reassign.jv".to_string();
    server.open_document(
        uri.clone(),
        "greeting = \"hello\"\ngreeting = \"hi\"".to_string(),
    );

    let diagnostics = server.get_diagnostics(&uri);
    assert!(
        diagnostics.iter().any(|diag| {
            diag.message.contains("JV4201") && diag.code.as_deref() == Some("JV4201")
        })
    );
}

#[test]
fn test_diagnostics_for_missing_initializer_self_reference() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///self.jv".to_string();
    server.open_document(uri.clone(), "value = value".to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert!(
        diagnostics.iter().any(|diag| {
            diag.message.contains("JV4202") && diag.code.as_deref() == Some("JV4202")
        })
    );
}

#[test]
fn test_diagnostics_for_raw_type_comment() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///raw.jv".to_string();
    let source = "val answer = 0 // jv:raw-default demo.Value\n";
    server.open_document(uri.clone(), source.to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("JV3202"))
    );
}

#[test]
fn test_diagnostics_for_raw_allow_comment() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///raw_allow.jv".to_string();
    let source = "val answer = 0 // jv:raw-allow demo.Value\n";
    server.open_document(uri.clone(), source.to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("JV3203"))
    );
}

#[test]
fn test_completions_include_new_templates() {
    let server = JvLanguageServer::new();
    let position = Position {
        line: 0,
        character: 0,
    };
    let completions = server.get_completions("file:///test.jv", position);

    assert!(!completions.is_empty());
    assert!(completions.contains(&"name = value".to_string()));
    assert!(completions.contains(&"var name = value".to_string()));
    assert!(completions.contains(&"data Point(x y)".to_string()));
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
    assert!(
        greeting.contains("Primitive(\"String\")")
            || greeting.contains("Primitive(\"java.lang.String\")"),
        "unexpected greeting binding type: {greeting}"
    );
}

#[test]
fn reports_type_error_for_ambiguous_function() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///ambiguous.jv".to_string();
    server.open_document(uri.clone(), "fun ambiguous(x) { null }".to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert!(!diagnostics.is_empty());
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.message.contains("ambiguous function signature"))
    );
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

#[test]
fn surfaces_regex_diagnostics_from_validator() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///regex-invalid.jv".to_string();
    server.open_document(uri.clone(), "val pattern = /\\y/".to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("JV5102"))
    );

    let metadata = server
        .regex_metadata(&uri)
        .expect("regex metadata should be recorded");
    assert_eq!(metadata.len(), 1);
}

#[test]
fn hover_exposes_regex_analysis_summary() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///regex-hover.jv".to_string();
    server.open_document(uri.clone(), "val pattern = /[a-z]+/".to_string());

    let _ = server.get_diagnostics(&uri);
    let metadata = server
        .regex_metadata(&uri)
        .expect("regex metadata should be available");
    let analysis = &metadata[0];

    let hover_position = Position {
        line: analysis.span.start_line.saturating_sub(1) as u32,
        character: analysis.span.start_column.saturating_sub(1) as u32,
    };

    let hover = server
        .get_hover(&uri, hover_position.clone())
        .expect("hover information should be returned");

    assert!(hover.contents.contains("Regex pattern"));
    assert!(hover.contents.contains("[a-z]+"));
    assert_eq!(hover.range.start.line, hover_position.line);
}

#[test]
fn regex_completions_include_templates_and_metadata() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///regex-completion.jv".to_string();
    server.open_document(uri.clone(), "val pattern = /^[0-9]+$/".to_string());

    let _ = server.get_diagnostics(&uri);
    let completions = server.get_completions(
        &uri,
        Position {
            line: 0,
            character: 0,
        },
    );

    assert!(
        completions
            .iter()
            .any(|item| item.contains("regex template"))
    );
    assert!(
        completions
            .iter()
            .any(|item| item.contains("regex literal"))
    );
}

#[test]
fn test_sequence_completion_after_dot() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///sequence.jv".to_string();
    server.open_document(uri.clone(), "numbers.".to_string());

    let completions = server.get_completions(
        &uri,
        Position {
            line: 0,
            character: 8,
        },
    );

    assert!(
        completions
            .iter()
            .any(|entry| entry.contains("map") && entry.contains("明示引数必須")),
        "Sequence completions should include map with explicit parameter note"
    );
}

#[test]
fn test_sequence_hover_map_operation() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///hover.jv".to_string();
    let source = "numbers.map { value -> value * 2 }.toList()";
    server.open_document(uri.clone(), source.to_string());

    let hover = server.get_hover(
        &uri,
        Position {
            line: 0,
            character: 9,
        },
    );

    let hover = hover.expect("hover should be available for map");
    assert!(
        hover.contents.contains("遅延評価Sequence"),
        "Hover text should describe the sequence operation"
    );
    assert!(
        hover.contents.contains("明示必須"),
        "Hover text should emphasise explicit lambda parameters"
    );
}

#[test]
fn test_sequence_lambda_diagnostic_for_it_parameter() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///diag.jv".to_string();
    let source = r#"
numbers = [1 2 3]
result = numbers.map { it -> it * 2 }.toList()
"#;
    server.open_document(uri.clone(), source.to_string());

    let diagnostics = server.get_diagnostics(&uri);
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("E1001")),
        "Expected E1001 diagnostic when using implicit it parameter"
    );
    let diagnostic = diagnostics
        .iter()
        .find(|diag| diag.code.as_deref() == Some("E1001"))
        .expect("diagnostic should exist");
    assert!(
        diagnostic
            .suggestions
            .iter()
            .any(|suggestion| suggestion.contains("{ value ->")),
        "Diagnostic should propose explicit parameter suggestion"
    );
}
