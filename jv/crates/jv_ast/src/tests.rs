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
