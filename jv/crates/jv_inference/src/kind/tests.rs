use super::*;
use crate::types::{SymbolId, TypeId};
use jv_ast::types::Kind;
use jv_ast::Span;

fn arrow_kind() -> Kind {
    Kind::Arrow {
        parameter: Box::new(Kind::Star),
        result: Box::new(Kind::Star),
    }
}

#[test]
fn solver_accepts_matching_constructor_arguments() {
    let mut environment = KindEnvironment::default();
    let parameter = TypeId::new(1);
    let expected_kind = arrow_kind();
    environment.declare_parameter(
        parameter,
        Some("F".to_string()),
        Some(expected_kind.clone()),
    );

    let list_symbol = SymbolId::from("core::List");
    environment.register_constructor_kind(list_symbol.clone(), expected_kind.clone());

    let owner = SymbolId::from("core::Functor");
    let constraints = vec![KindConstraint::application(
        owner,
        parameter,
        KindArgument::Constructor(list_symbol),
        Span::dummy(),
    )];

    let solution = KindSolver::new().solve(&environment, &constraints);

    assert!(solution.diagnostics().is_empty());
    assert_eq!(solution.telemetry().checks, 1);
    assert_eq!(solution.telemetry().cache_hits, 0);
    assert_eq!(solution.assignments().get(&parameter), Some(&expected_kind));
}

#[test]
fn solver_reports_kind_mismatch() {
    let mut environment = KindEnvironment::default();
    let parameter = TypeId::new(2);
    let expected_kind = arrow_kind();
    environment.declare_parameter(
        parameter,
        Some("F".to_string()),
        Some(expected_kind.clone()),
    );

    let owner = SymbolId::from("core::Functor");
    let constraints = vec![KindConstraint::application(
        owner.clone(),
        parameter,
        KindArgument::Kind(Kind::Star),
        Span::dummy(),
    )];

    let solution = KindSolver::new().solve(&environment, &constraints);

    assert_eq!(solution.diagnostics().len(), 1);
    let diagnostic = &solution.diagnostics()[0];
    assert_eq!(diagnostic.owner, owner);
    assert_eq!(diagnostic.parameter, parameter);
    assert_eq!(diagnostic.parameter_name.as_deref(), Some("F"));
    assert_eq!(diagnostic.expected, expected_kind);
    assert_eq!(diagnostic.actual, Kind::Star);
}

#[test]
fn solver_propagates_parameter_kinds() {
    let mut environment = KindEnvironment::default();
    let higher = TypeId::new(3);
    let alias = TypeId::new(4);

    let applied = arrow_kind();
    environment.declare_parameter(higher, Some("F".to_string()), Some(applied.clone()));
    environment.declare_parameter(alias, Some("G".to_string()), None);

    let owner = SymbolId::from("core::Functor");
    let constraints = vec![
        KindConstraint::application(
            owner.clone(),
            higher,
            KindArgument::Kind(applied.clone()),
            Span::dummy(),
        ),
        KindConstraint::application(owner, alias, KindArgument::Parameter(higher), Span::dummy()),
    ];

    let solution = KindSolver::new().solve(&environment, &constraints);

    assert!(solution.diagnostics().is_empty());
    assert_eq!(solution.assignments().get(&alias), Some(&applied));
}
