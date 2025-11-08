use crate::solver::{
    RawTypeAnalyzer, RawTypeEvent, RawTypeMitigation, SolverTelemetry, TypeArgumentSolution,
};
use crate::types::{GenericSignature, RawTypeContinuation, RawTypeDirective};
use jv_ast::Span;
use jv_ast::types::QualifiedName;

fn span() -> Span {
    Span::new(10, 0, 10, 20)
}

fn directive(owner: &str, continuation: RawTypeContinuation) -> RawTypeDirective {
    RawTypeDirective {
        owner: QualifiedName::new(
            owner.split('.').map(str::to_string).collect::<Vec<_>>(),
            span(),
        ),
        span: span(),
        mode: continuation,
    }
}

#[test]
fn analyze_signature_records_events_and_policy() {
    let signature = GenericSignature::new(
        Vec::new(),
        None,
        vec![directive(
            "java.util.List",
            RawTypeContinuation::DefaultPolicy,
        )],
        span(),
    );

    let mut telemetry = SolverTelemetry::default();
    let events = RawTypeAnalyzer::analyze_signature(&signature, &mut telemetry);

    assert_eq!(events.len(), 1);
    assert_eq!(telemetry.raw_type_events().len(), 1);
    assert!(telemetry.raw_type_severity_todo());

    let event = &events[0];
    assert_eq!(event.symbol, "java.util.List");
    assert_eq!(event.telemetry_key, crate::solver::RAW_TYPE_TELEMETRY_KEY);
    assert_eq!(event.mitigation, RawTypeMitigation::NullCheck);
    assert_eq!(event.continuation, RawTypeContinuation::DefaultPolicy);
}

#[test]
fn annotate_solution_appends_events() {
    let signature = GenericSignature::new(
        Vec::new(),
        None,
        vec![directive(
            "java.util.Map",
            RawTypeContinuation::AllowWithComment,
        )],
        span(),
    );
    let mut telemetry = SolverTelemetry::default();
    let mut solution = TypeArgumentSolution::default();

    solution.annotate_raw_types_from_signature(&signature, &mut telemetry);

    assert_eq!(solution.raw_type_events().len(), 1);
    assert_eq!(telemetry.raw_type_events().len(), 1);
    assert_eq!(
        solution.raw_type_events()[0].mitigation,
        RawTypeMitigation::CommentOnly
    );
}

#[test]
fn record_events_noops_when_empty() {
    let mut telemetry = SolverTelemetry::default();
    let events: Vec<RawTypeEvent> = Vec::new();
    let recorded = RawTypeAnalyzer::record_events(events, &mut telemetry);

    assert!(recorded.is_empty());
    assert!(telemetry.raw_type_events().is_empty());
    assert!(!telemetry.raw_type_severity_todo());
}

#[test]
fn record_directives_sets_policy_and_returns_events() {
    let directives = vec![directive(
        "demo.Service",
        RawTypeContinuation::AllowWithComment,
    )];
    let mut telemetry = SolverTelemetry::default();

    let events = RawTypeAnalyzer::record_directives(&directives, &mut telemetry);

    assert_eq!(events.len(), 1);
    assert_eq!(telemetry.raw_type_events().len(), 1);
    assert!(telemetry.raw_type_severity_todo());
    assert_eq!(events[0].symbol, "demo.Service");
    assert_eq!(events[0].mitigation, RawTypeMitigation::CommentOnly);
}
