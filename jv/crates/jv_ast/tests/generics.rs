use jv_ast::types::{
    GenericParameter, GenericSignature, QualifiedName, RawTypeContinuation, RawTypeDirective,
    TypeAnnotation, VarianceMarker, WhereClause,
};
use jv_ast::Span;

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
        span: span(),
    };

    assert_eq!(parameter.name, "T");
    assert_eq!(parameter.bounds.len(), 1);
    assert_eq!(parameter.variance, Some(VarianceMarker::Covariant));
    assert_eq!(
        parameter.default,
        Some(TypeAnnotation::Simple("String".to_string()))
    );
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
            span: span(),
        }],
        where_clause: Some(where_clause),
        raw_directives: vec![directive(RawTypeContinuation::DefaultPolicy)],
        span: span(),
    };

    assert_eq!(signature.parameters.len(), 1);
    assert!(signature.where_clause.is_some());
    assert_eq!(signature.raw_directives.len(), 1);

    let owner = &signature.raw_directives[0].owner;
    assert_eq!(owner.qualified(), "demo.Widget");
    assert!(signature.where_clause.as_ref().unwrap().predicates.is_empty());
}

#[test]
fn default_signature_has_no_generics() {
    let signature = GenericSignature::default();
    assert!(signature.parameters.is_empty());
    assert!(signature.raw_directives.is_empty());
    assert!(signature.where_clause.is_none());
}
