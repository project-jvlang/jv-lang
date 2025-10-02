use super::*;
use serde_json::json;

fn build_sample_annotation() -> Annotation {
    let positional_span = Span::new(1, 1, 1, 12);
    let named_span = Span::new(1, 14, 1, 30);
    let value_span = Span::new(1, 20, 1, 24);

    Annotation {
        name: "Sample".to_string(),
        arguments: vec![
            AnnotationArgument::PositionalLiteral {
                value: Literal::String("examples/users.json".to_string()),
                span: positional_span,
            },
            AnnotationArgument::Named {
                name: "mode".to_string(),
                value: Expression::Identifier("Load".to_string(), value_span),
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
    let positional = AnnotationArgument::PositionalLiteral {
        value: Literal::String("users.csv".to_string()),
        span: positional_span.clone(),
    };
    assert_eq!(positional.span(), &positional_span);

    let named_span = Span::new(2, 20, 2, 34);
    let value_span = Span::new(2, 27, 2, 31);
    let named = AnnotationArgument::Named {
        name: "format".to_string(),
        value: Expression::Identifier("Csv".to_string(), value_span),
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
    };

    let serialized = serde_json::to_string(&metadata).expect("serialize metadata");
    let decoded: CallArgumentMetadata =
        serde_json::from_str(&serialized).expect("deserialize metadata");

    assert_eq!(decoded, metadata);
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
        span: span.clone(),
    };

    let serialized = serde_json::to_string(&literal).expect("serialize multiline literal");
    let decoded: MultilineStringLiteral =
        serde_json::from_str(&serialized).expect("deserialize multiline literal");

    assert_eq!(decoded, literal);
}
