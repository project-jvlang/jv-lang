use super::*;
use serde_json::json;

fn build_sample_annotation() -> Annotation {
    let positional_span = Span::new(1, 1, 1, 12);
    let named_span = Span::new(1, 14, 1, 30);
    let name_span = Span::new(1, 0, 1, 7);

    Annotation {
        name: AnnotationName::new(vec!["Sample".to_string()], name_span.clone()),
        arguments: vec![
            AnnotationArgument::Positional {
                value: AnnotationValue::Literal(Literal::String("examples/users.json".to_string())),
                span: positional_span,
            },
            AnnotationArgument::Named {
                name: "mode".to_string(),
                value: AnnotationValue::EnumConstant {
                    type_path: vec!["SampleMode".to_string()],
                    constant: "Load".to_string(),
                },
                span: named_span,
            },
        ],
        span: Span::new(1, 0, 1, 32),
    }
}

#[test]
fn annotation_roundtrips_through_serde_with_modifiers() {
    let mut modifiers = Modifiers {
        visibility: Visibility::Public,
        ..Modifiers::default()
    };
    modifiers.annotations.push(build_sample_annotation());

    let serialized = serde_json::to_string(&modifiers).expect("serialize modifiers");
    let decoded: Modifiers = serde_json::from_str(&serialized).expect("deserialize modifiers");

    assert_eq!(decoded, modifiers);
}

#[test]
fn annotation_argument_span_access_returns_original_span() {
    let positional_span = Span::new(2, 5, 2, 18);
    let positional = AnnotationArgument::Positional {
        value: AnnotationValue::Literal(Literal::String("users.csv".to_string())),
        span: positional_span.clone(),
    };
    assert_eq!(positional.span(), &positional_span);

    let named_span = Span::new(2, 20, 2, 34);
    let named = AnnotationArgument::Named {
        name: "format".to_string(),
        value: AnnotationValue::ClassLiteral {
            type_path: vec!["java".to_string(), "lang".to_string(), "String".to_string()],
        },
        span: named_span.clone(),
    };
    assert_eq!(named.span(), &named_span);
}

#[test]
fn modifiers_default_annotations_field_is_empty_when_missing() {
    let value = json!({
        "visibility": "Internal",
        "is_abstract": false,
        "is_final": true,
        "is_static": false,
        "is_override": false,
        "is_open": false
    });

    let modifiers: Modifiers = serde_json::from_value(value).expect("deserialize modifiers");

    assert!(modifiers.annotations.is_empty());
    assert_eq!(modifiers.visibility, Visibility::Internal);
    assert!(modifiers.is_final);
}

#[test]
fn call_argument_metadata_deserializes_from_legacy_style() {
    let metadata: CallArgumentMetadata =
        serde_json::from_str("\"Whitespace\"").expect("deserialize legacy style");

    assert_eq!(metadata.style, CallArgumentStyle::Whitespace);
    assert!(metadata.homogeneous_kind.is_none());
    assert!(metadata.separator_diagnostics.is_empty());
    assert!(!metadata.used_commas);
}

#[test]
fn call_argument_metadata_roundtrips_with_extended_fields() {
    let span = Span::new(1, 0, 1, 10);
    let metadata = CallArgumentMetadata {
        style: CallArgumentStyle::Whitespace,
        homogeneous_kind: Some(ArgumentElementKind::Number),
        separator_diagnostics: vec![CallArgumentIssue {
            message: "mixed separators".to_string(),
            span: Some(span.clone()),
        }],
        used_commas: true,
    };

    let serialized = serde_json::to_string(&metadata).expect("serialize metadata");
    let decoded: CallArgumentMetadata =
        serde_json::from_str(&serialized).expect("deserialize metadata");

    assert_eq!(decoded, metadata);
}

#[test]
fn reserved_annotation_lookup_and_conflict_detection() {
    assert!(is_jv_reserved("Sample"));
    assert!(!is_jv_reserved("Custom"));

    let span_primary = Span::new(1, 0, 1, 6);
    let span_duplicate = Span::new(2, 0, 2, 6);
    let span_shadow = Span::new(3, 0, 3, 15);

    let sample_primary = Annotation {
        name: AnnotationName::new(vec!["Sample".to_string()], span_primary.clone()),
        arguments: Vec::new(),
        span: span_primary,
    };
    let sample_duplicate = Annotation {
        name: AnnotationName::new(vec!["Sample".to_string()], span_duplicate.clone()),
        arguments: Vec::new(),
        span: span_duplicate,
    };
    let sample_shadow = Annotation {
        name: AnnotationName::new(
            vec![
                "com".to_string(),
                "example".to_string(),
                "Sample".to_string(),
            ],
            span_shadow.clone(),
        ),
        arguments: Vec::new(),
        span: span_shadow,
    };

    let binding = [sample_primary, sample_duplicate, sample_shadow];
    let conflicts = detect_reserved_conflicts(&binding);

    assert!(
        conflicts
            .iter()
            .any(|conflict| matches!(conflict.kind, ReservedConflictKind::DuplicateUsage))
    );
    assert!(conflicts.iter().any(|conflict| matches!(
        conflict.kind,
        ReservedConflictKind::NameShadowing { reserved } if reserved == "Sample"
    )));
}

#[test]
fn json_literal_roundtrips_through_serde() {
    let span = Span::new(2, 1, 6, 2);
    let entry_span = Span::new(3, 3, 3, 20);
    let comment_span = Span::new(2, 3, 2, 18);

    let literal = JsonLiteral {
        value: JsonValue::Object {
            entries: vec![JsonEntry {
                key: "timeout".to_string(),
                comments: vec![JsonComment {
                    kind: JsonCommentKind::Line,
                    text: " in milliseconds".to_string(),
                    span: comment_span.clone(),
                }],
                value: JsonValue::Number {
                    literal: "5_000".to_string(),
                    grouping: NumberGrouping::Underscore,
                    span: entry_span.clone(),
                },
                span: entry_span.clone(),
            }],
            span: span.clone(),
        },
        leading_comments: vec![JsonComment {
            kind: JsonCommentKind::Block,
            text: " Service configuration ".to_string(),
            span: Span::new(1, 1, 1, 26),
        }],
        trailing_comments: vec![],
        span: span.clone(),
        inferred_schema: Some(SchemaId("config.TimeoutSchema".to_string())),
    };

    let serialized = serde_json::to_string(&literal).expect("serialize json literal");
    let decoded: JsonLiteral = serde_json::from_str(&serialized).expect("deserialize literal");

    assert_eq!(decoded, literal);
}

#[test]
fn multiline_string_literal_roundtrips_through_serde() {
    let span = Span::new(10, 1, 14, 4);
    let literal = MultilineStringLiteral {
        kind: MultilineKind::TripleQuote,
        normalized: "Hello, ${name}!".to_string(),
        raw: "\"\"\"\nHello, ${name}!\n\"\"\"".to_string(),
        parts: vec![
            StringPart::Text("Hello, ".to_string()),
            StringPart::Expression(Expression::Identifier(
                "name".to_string(),
                Span::new(11, 10, 11, 14),
            )),
            StringPart::Text("!".to_string()),
        ],
        indent: Some(IndentMetadata::new(4, true)),
        raw_flavor: None,
        span: span.clone(),
    };

    let serialized = serde_json::to_string(&literal).expect("serialize multiline literal");
    let decoded: MultilineStringLiteral =
        serde_json::from_str(&serialized).expect("deserialize multiline literal");

    assert_eq!(decoded, literal);
}

#[test]
fn regex_literal_roundtrips_through_serde() {
    let span = Span::new(5, 10, 5, 16);
    let literal = RegexLiteral {
        pattern: "a/b".to_string(),
        raw: "/a\\/b/".to_string(),
        span: span.clone(),
    };

    let variant = Literal::Regex(literal.clone());
    let serialized = serde_json::to_string(&variant).expect("serialize regex literal");
    let decoded: Literal = serde_json::from_str(&serialized).expect("deserialize literal");
    assert_eq!(decoded, variant);

    let expr = Expression::RegexLiteral(literal.clone());
    let expr_serialized = serde_json::to_string(&expr).expect("serialize regex expression");
    let expr_decoded: Expression =
        serde_json::from_str(&expr_serialized).expect("deserialize regex expression");
    assert_eq!(expr_decoded, expr);
}

#[test]
fn raw_single_literal_roundtrips_with_empty_parts() {
    let span = Span::new(3, 1, 3, 20);
    let literal = MultilineStringLiteral {
        kind: MultilineKind::RawSingle,
        normalized: "C:\\Users\\dev".to_string(),
        raw: "'C:\\Users\\dev'".to_string(),
        parts: Vec::new(),
        indent: None,
        raw_flavor: Some(RawStringFlavor::SingleLine),
        span: span.clone(),
    };

    let serialized = serde_json::to_string(&literal).expect("serialize raw literal");
    let decoded: MultilineStringLiteral =
        serde_json::from_str(&serialized).expect("deserialize raw literal");

    assert_eq!(decoded, literal);
    assert!(decoded.parts.is_empty());
}

#[test]
fn val_declaration_origin_roundtrips_through_serde() {
    let stmt = Statement::ValDeclaration {
        name: "temperature".to_string(),
        binding: Some(BindingPatternKind::identifier("temperature", Span::dummy())),
        type_annotation: Some(TypeAnnotation::Simple("Double".to_string())),
        initializer: Expression::Literal(Literal::Number("36.5".to_string()), Span::dummy()),
        modifiers: Modifiers::default(),
        origin: ValBindingOrigin::ImplicitTyped,
        span: Span::dummy(),
    };

    let serialized = serde_json::to_string(&stmt).expect("serialize val declaration");
    let decoded: Statement =
        serde_json::from_str(&serialized).expect("deserialize val declaration");

    match decoded {
        Statement::ValDeclaration {
            name,
            type_annotation,
            origin,
            ..
        } => {
            assert_eq!(name, "temperature");
            assert!(matches!(
                type_annotation,
                Some(TypeAnnotation::Simple(ref type_name)) if type_name == "Double"
            ));
            assert_eq!(origin, ValBindingOrigin::ImplicitTyped);
        }
        other => panic!("expected val declaration, got {:?}", other),
    }
}
