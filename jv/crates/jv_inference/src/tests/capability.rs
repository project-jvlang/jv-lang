use crate::constraint::{GenericConstraint, GenericConstraintKind};
use crate::environment::{CapabilityEnvironment, CapabilityImplementation, CapabilityPriority};
use crate::solver::{GenericSolver, GenericSolverDiagnostic};
use crate::types::{
    BoundPredicate, BoundTypeReference, CapabilityBound, CapabilityHints, DispatchKind, SymbolId,
    TypeId, TypeKind, TypeVariant,
};
use jv_ast::Span;

fn dummy_span() -> Span {
    Span::dummy()
}

fn numeric_capability() -> BoundPredicate {
    BoundPredicate::Capability(CapabilityBound::new(
        "Numeric",
        BoundTypeReference::Named("Int".into()),
        CapabilityHints::default(),
    ))
}

fn type_argument_constraint(symbol: &SymbolId, param: TypeId) -> GenericConstraint {
    GenericConstraint::new(
        GenericConstraintKind::TypeArgument {
            callee: symbol.clone(),
            parameter: param,
            argument: TypeKind::new(TypeVariant::Primitive("Int")),
            argument_index: 0,
        },
        dummy_span(),
    )
}

fn capability_constraint(symbol: &SymbolId, param: TypeId) -> GenericConstraint {
    GenericConstraint::new(
        GenericConstraintKind::BoundRequirement {
            owner: symbol.clone(),
            parameter: param,
            predicate: numeric_capability(),
        },
        dummy_span(),
    )
}

#[test]
fn records_capability_solution_when_environment_matches() {
    let symbol = SymbolId::from("pkg::Ops");
    let param = TypeId::new(1);
    let mut env = CapabilityEnvironment::new();
    env.register(CapabilityImplementation::new(
        "Numeric",
        TypeKind::new(TypeVariant::Primitive("Int")),
        SymbolId::from("impl.NumericInt"),
        DispatchKind::Static,
        CapabilityPriority::Local,
        None,
    ));

    let constraints = vec![
        type_argument_constraint(&symbol, param),
        capability_constraint(&symbol, param),
    ];

    let solution = GenericSolver::resolve_with_environment(&constraints, &env);
    assert!(solution.diagnostics().is_empty());
    let capabilities = solution
        .capability_solutions(&symbol)
        .expect("solution missing");
    assert_eq!(capabilities.len(), 1);
    assert_eq!(capabilities[0].implementor.as_str(), "impl.NumericInt");
    assert_eq!(capabilities[0].dispatch_kind, DispatchKind::Static);
}

#[test]
fn emits_diagnostic_when_capability_missing() {
    let symbol = SymbolId::from("pkg::Ops");
    let param = TypeId::new(2);
    let env = CapabilityEnvironment::new();
    let constraints = vec![
        type_argument_constraint(&symbol, param),
        capability_constraint(&symbol, param),
    ];

    let solution = GenericSolver::resolve_with_environment(&constraints, &env);
    assert_eq!(solution.diagnostics().len(), 1);
    assert!(matches!(
        solution.diagnostics()[0],
        GenericSolverDiagnostic::CapabilityResolutionFailed { .. }
    ));
}

#[test]
fn reports_ambiguity_for_same_priority_candidates() {
    let symbol = SymbolId::from("pkg::Ops");
    let param = TypeId::new(3);
    let mut env = CapabilityEnvironment::new();
    env.register(CapabilityImplementation::new(
        "Numeric",
        TypeKind::new(TypeVariant::Primitive("Int")),
        SymbolId::from("impl.NumericA"),
        DispatchKind::Static,
        CapabilityPriority::Local,
        None,
    ));
    env.register(CapabilityImplementation::new(
        "Numeric",
        TypeKind::new(TypeVariant::Primitive("Int")),
        SymbolId::from("impl.NumericB"),
        DispatchKind::Static,
        CapabilityPriority::Local,
        None,
    ));
    let constraints = vec![
        type_argument_constraint(&symbol, param),
        capability_constraint(&symbol, param),
    ];

    let solution = GenericSolver::resolve_with_environment(&constraints, &env);
    assert_eq!(solution.diagnostics().len(), 1);
    match &solution.diagnostics()[0] {
        GenericSolverDiagnostic::CapabilityResolutionFailed { error, .. } => {
            assert!(error.to_string().contains("ambiguous"));
        }
        other => panic!("unexpected diagnostic: {other:?}"),
    }
}
