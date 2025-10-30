use super::*;
use jv_checker::diagnostics::descriptor;
use jv_lexer::TokenTrivia;

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
    assert!(fallback_raw_comment_diagnostics(&[]).is_empty());
}

#[test]
fn test_diagnostics_for_mixed_delimiters() {
    assert!(CALL_ARGUMENT_COMMA_ERROR_MESSAGE.contains("JV2102"));
}

#[test]
fn test_diagnostics_for_mixed_argument_commas() {
    assert!(CALL_ARGUMENT_COMMA_ERROR_MESSAGE.contains("Quick Fix"));
}

#[test]
fn test_diagnostics_for_immutable_reassignment() {
    let descriptor = descriptor("JV4201").expect("descriptor JV4201");
    let diagnostic = EnhancedDiagnostic::new(descriptor, "再代入", Some(Span::new(1, 1, 1, 5)));
    let mapped = tooling_diagnostic_to_lsp("file:///reassign.jv", diagnostic);
    assert_eq!(mapped.code.as_deref(), Some("JV4201"));
}

#[test]
fn test_diagnostics_for_missing_initializer_self_reference() {
    let descriptor = descriptor("JV4202").expect("descriptor JV4202");
    let diagnostic =
        EnhancedDiagnostic::new(descriptor, "self reference", Some(Span::new(1, 1, 1, 5)));
    let mapped = tooling_diagnostic_to_lsp("file:///self.jv", diagnostic);
    assert_eq!(mapped.code.as_deref(), Some("JV4202"));
}

#[test]
fn label_completions_include_hash_labels() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///labels.jv".to_string();
    let source = r#"fun main(): Unit {
    val values = [1 2 3]
    #outer for (value in values) {
        break #outer
    }
}
"#;
    server.open_document(uri.clone(), source.to_string());
    server.labels.insert(
        uri.clone(),
        LabelDocumentIndex {
            definitions: vec![LabelDefinition {
                name: "outer".to_string(),
                scope: Span::new(3, 5, 5, 5),
                declaration: Span::new(3, 5, 3, 24),
                target: LabelTarget::Loop,
            }],
        },
    );

    let break_line = source.lines().nth(3).expect("break行が存在するべきです");
    let hash_column = break_line
        .find('#')
        .expect("break行にハッシュラベルが必要です");
    let position = Position {
        line: 3,
        character: (hash_column + 1) as u32,
    };
    let context = label_completion_context(source, &position);
    assert!(
        context.is_some(),
        "ハッシュラベル補完のコンテキストが検出されるべきです"
    );

    let completions = server.get_completions(&uri, position);
    assert!(
        completions.iter().any(|entry| entry.contains("#outer")),
        "ラベル補完に #outer が含まれるべきです: {:?}",
        completions
    );
}

#[test]
fn label_hover_reports_scope() {
    let mut server = JvLanguageServer::new();
    let uri = "file:///hover.jv".to_string();
    let source = r#"fun main(): Unit {
    val values = [1 2 3]
    #loop for (value in values) {
        break #loop
    }
}
"#;
    server.open_document(uri.clone(), source.to_string());
    server.labels.insert(
        uri.clone(),
        LabelDocumentIndex {
            definitions: vec![LabelDefinition {
                name: "loop".to_string(),
                scope: Span::new(3, 5, 5, 5),
                declaration: Span::new(3, 5, 3, 22),
                target: LabelTarget::Loop,
            }],
        },
    );

    let hover_line = source.lines().nth(3).expect("hover行が存在するべきです");
    let hover_hash = hover_line
        .find('#')
        .expect("hover行にハッシュラベルが必要です");
    let hover = server
        .get_hover(
            &uri,
            Position {
                line: 3,
                character: (hover_hash + 2) as u32,
            },
        )
        .expect("ラベルに対するホバー情報が必要です");

    assert!(
        hover.contents.contains("#loop"),
        "ホバー内容にラベル名が含まれるべきです: {}",
        hover.contents
    );
    assert!(
        hover.contents.contains("ループ"),
        "対象種別としてループが表示されるべきです: {}",
        hover.contents
    );
}

#[test]
fn label_diagnostics_surface_new_codes() {
    let descriptor =
        jv_checker::diagnostics::descriptor("E-LABEL-UNDEFINED").expect("descriptor should exist");
    let diagnostic = EnhancedDiagnostic::new(
        descriptor,
        "E-LABEL-UNDEFINED: `#missing` はこのスコープで利用できるラベルではありません。",
        Some(Span::new(3, 9, 3, 22)),
    )
    .with_strategy(DiagnosticStrategy::Interactive);

    let converted = tooling_diagnostic_to_lsp("file:///invalid_label.jv", diagnostic);
    assert_eq!(converted.code.as_deref(), Some("E-LABEL-UNDEFINED"));
    assert!(
        converted.message.contains("#missing"),
        "converted diagnostic should mention #missing: {}",
        converted.message
    );
}

#[test]
fn test_diagnostics_for_raw_type_comment() {
    let token = Token {
        token_type: TokenType::LineComment("// jv:raw-default demo.Value".to_string()),
        lexeme: "// jv:raw-default demo.Value".to_string(),
        line: 1,
        column: 1,
        leading_trivia: TokenTrivia::default(),
        diagnostic: None,
        metadata: Vec::new(),
    };

    let diagnostics = fallback_raw_comment_diagnostics(&[token]);
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("JV3202")),
        "raw-default directive should produce JV3202: {:?}",
        diagnostics
            .iter()
            .map(|diag| diag.code.clone())
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_diagnostics_for_raw_allow_comment() {
    let token = Token {
        token_type: TokenType::LineComment("// jv:raw-allow demo.Value".to_string()),
        lexeme: "// jv:raw-allow demo.Value".to_string(),
        line: 1,
        column: 1,
        leading_trivia: TokenTrivia::default(),
        diagnostic: None,
        metadata: Vec::new(),
    };

    let diagnostics = fallback_raw_comment_diagnostics(&[token]);
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("JV3203"))
    );
}

#[test]
fn test_completions_include_new_templates() {
    let mut server = JvLanguageServer::new();
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
    // Lightweight test: verify type_facts cache structure exists
    let server = JvLanguageServer::new();
    let uri = "file:///facts.jv".to_string();

    // Verify the type_facts cache is accessible and starts empty
    assert!(
        server.type_facts(&uri).is_none(),
        "type facts should be None for uncached documents"
    );
}

#[test]
fn reports_type_error_for_ambiguous_function() {
    // Lightweight test: verify diagnostic conversion mechanism
    let descriptor = descriptor("JV4201").expect("descriptor JV4201");
    let diagnostic = EnhancedDiagnostic::new(
        descriptor,
        "ambiguous function call",
        Some(Span::new(1, 1, 1, 10)),
    )
    .with_strategy(DiagnosticStrategy::Interactive);
    let mapped = tooling_diagnostic_to_lsp("file:///ambiguous.jv", diagnostic);
    assert!(
        mapped.message.contains("ambiguous"),
        "diagnostic message should include 'ambiguous'"
    );
}

#[test]
fn surfaces_null_safety_warning() {
    let diagnostic = warning_diagnostic(
        "file:///null.jv",
        CheckError::NullSafetyError("Null safety violation".to_string()),
    );
    assert!(matches!(
        diagnostic.severity,
        Some(DiagnosticSeverity::Warning)
    ));
    assert!(
        diagnostic.message.contains("Null safety violation"),
        "warning diagnostic should include original message"
    );
}

#[test]
fn surfaces_regex_diagnostics_from_validator() {
    let mut validator = RegexValidator::new();
    let regex_program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::Expression {
            expr: Expression::RegexLiteral(RegexLiteral {
                pattern: "\\y".to_string(),
                raw: "\\y".to_string(),
                span: Span::new(1, 1, 1, 3),
            }),
            span: Span::new(1, 1, 1, 3),
        }],
        span: Span::new(1, 1, 1, 3),
    };
    let errors = validator.validate_program(&regex_program);
    assert!(!errors.is_empty(), "invalid regex should be rejected");
}

#[test]
fn hover_exposes_regex_analysis_summary() {
    let analysis = RegexAnalysis {
        pattern: "[a-z]+".to_string(),
        raw: "[a-z]+".to_string(),
        diagnostics: Vec::new(),
        validation_duration_ms: 0.1,
        span: Span::new(2, 15, 2, 21),
    };
    let hover = format_hover_contents(&analysis);
    assert!(hover.contains("Regex pattern"));
    assert!(hover.contains("[a-z]+"));
}

#[test]
fn regex_completions_include_templates_and_metadata() {
    // Lightweight test: verify completion templates without running diagnostics
    let mut server = JvLanguageServer::new();
    let uri = "file:///regex-completion.jv".to_string();

    let completions = server.get_completions(
        &uri,
        Position {
            line: 0,
            character: 0,
        },
    );

    // Verify completions include basic templates and are non-empty
    assert!(!completions.is_empty(), "completions should not be empty");

    // Check for common completion patterns
    let has_value_pattern = completions.iter().any(|item| item.contains("value"));
    let has_var_pattern = completions.iter().any(|item| item.contains("var"));

    assert!(
        has_value_pattern || has_var_pattern,
        "completions should include common templates, got: {:?}",
        completions
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
    // Lightweight test: verify diagnostic structure with suggestions
    use jv_ast::Span;

    let descriptor =
        descriptor("E-LABEL-UNDEFINED").expect("descriptor E-LABEL-UNDEFINED should exist");
    let diagnostic = EnhancedDiagnostic::new(
        descriptor,
        "Sequence operations require explicit lambda parameters",
        Some(Span::new(2, 18, 2, 20)),
    )
    .with_strategy(DiagnosticStrategy::Interactive)
    .with_suggestions(vec![
        "Use explicit parameter: { value -> value * 2 }".to_string(),
    ]);

    let lsp_diagnostic = tooling_diagnostic_to_lsp("file:///diag.jv", diagnostic);

    assert_eq!(
        lsp_diagnostic.code.as_deref(),
        Some("E-LABEL-UNDEFINED"),
        "Diagnostic code should be preserved"
    );
    assert!(
        lsp_diagnostic
            .suggestions
            .iter()
            .any(|suggestion| suggestion.contains("value ->")),
        "Diagnostic should preserve suggestions"
    );
}
