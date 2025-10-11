use jv_ast::types::{
    ConstParameter, GenericParameter, GenericSignature, Kind, QualifiedName, RawTypeContinuation,
    RawTypeDirective, TypeAnnotation, TypeLevelExpr, VarianceMarker, WhereClause,
};
use jv_ast::Span;
use serde_json::json;

fn span() -> Span {
    Span::new(1, 0, 1, 10)
}

fn directive(mode: RawTypeContinuation) -> RawTypeDirective {
    RawTypeDirective {
        owner: QualifiedName::new(vec!["demo".into(), "Widget".into()], span()),
        span: span(),
        mode,
    }
}

#[test]
fn generic_parameter_records_variance_and_default() {
    let parameter = GenericParameter {
        name: "T".to_string(),
        bounds: vec![TypeAnnotation::Simple("Comparable".to_string())],
        variance: Some(VarianceMarker::Covariant),
        default: Some(TypeAnnotation::Simple("String".to_string())),
        kind: Some(Kind::Star),
        span: span(),
    };

    assert_eq!(parameter.name, "T");
    assert_eq!(parameter.bounds.len(), 1);
    assert_eq!(parameter.variance, Some(VarianceMarker::Covariant));
    assert_eq!(
        parameter.default,
        Some(TypeAnnotation::Simple("String".to_string()))
    );
    assert!(parameter.has_kind());
    assert!(matches!(parameter.kind(), Some(Kind::Star)));
}

#[test]
fn signature_collects_raw_directives_and_where_clause() {
    let where_clause = WhereClause {
        predicates: Vec::new(),
        span: span(),
    };

    let signature = GenericSignature {
        parameters: vec![GenericParameter {
            name: "U".to_string(),
            bounds: Vec::new(),
            variance: Some(VarianceMarker::Contravariant),
            default: None,
            kind: None,
            span: span(),
        }],
        const_parameters: vec![ConstParameter {
            name: "N".to_string(),
            type_annotation: TypeAnnotation::Simple("Int".to_string()),
            default: Some(TypeLevelExpr::LiteralInt(4)),
            span: span(),
        }],
        where_clause: Some(where_clause),
        raw_directives: vec![directive(RawTypeContinuation::DefaultPolicy)],
        span: span(),
    };

    assert_eq!(signature.parameters.len(), 1);
    assert!(signature.where_clause.is_some());
    assert_eq!(signature.raw_directives.len(), 1);
    assert!(signature.has_const_parameters());
    assert_eq!(signature.const_parameters().len(), 1);
    assert!(signature.const_parameters()[0].has_default());

    let owner = &signature.raw_directives[0].owner;
    assert_eq!(owner.qualified(), "demo.Widget");
    assert!(signature
        .where_clause
        .as_ref()
        .unwrap()
        .predicates
        .is_empty());
}

#[test]
fn default_signature_has_no_generics() {
    let signature = GenericSignature::default();
    assert!(signature.parameters.is_empty());
    assert!(signature.raw_directives.is_empty());
    assert!(signature.where_clause.is_none());
    assert!(!signature.has_const_parameters());
}

#[test]
fn generic_parameter_kind_defaults_to_none_when_missing() {
    let value = json!({
        "name": "X",
        "bounds": [],
        "variance": null,
        "default": null,
        "span": {
            "start_line": 1,
            "start_column": 0,
            "end_line": 1,
            "end_column": 1
        }
    });

    let parameter: GenericParameter =
        serde_json::from_value(value).expect("deserialize generic parameter without kind");

    assert!(!parameter.has_kind());
    assert!(parameter.kind().is_none());
}

#[test]
fn signature_defaults_const_parameters_when_missing() {
    let value = json!({
        "parameters": [],
        "where_clause": null,
        "raw_directives": [],
        "span": {
            "start_line": 1,
            "start_column": 0,
            "end_line": 1,
            "end_column": 0
        }
    });

    let signature: GenericSignature =
        serde_json::from_value(value).expect("deserialize signature without const parameters");

    assert!(signature.const_parameters().is_empty());
    assert!(!signature.has_const_parameters());
}
