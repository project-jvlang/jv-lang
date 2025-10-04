use crate::constraint::{GenericConstraint, GenericConstraintKind};
use crate::solver::variance::{Variance, VarianceAnalyzer, VariancePosition, VarianceTable};
use crate::types::TypeId;
use jv_ast::Span;

fn usage_constraint(parameter: TypeId, position: VariancePosition) -> GenericConstraint {
    GenericConstraint::new(
        GenericConstraintKind::VarianceUsage { parameter, position },
        Span::dummy(),
    )
}

#[test]
fn covariant_usage_results_in_covariant_variance() {
    let param = TypeId::new(10);
    let table = VarianceAnalyzer::analyze_usages(vec![(param, VariancePosition::Covariant)]);
    assert_eq!(table.variance_for(param), Some(Variance::Covariant));
}

#[test]
fn mixed_usage_forces_invariance() {
    let param = TypeId::new(11);
    let usages = vec![
        (param, VariancePosition::Covariant),
        (param, VariancePosition::Contravariant),
    ];
    let table = VarianceAnalyzer::analyze_usages(usages);
    assert_eq!(table.variance_for(param), Some(Variance::Invariant));
}

#[test]
fn invariant_usage_takes_precedence() {
    let param = TypeId::new(12);
    let usages = vec![
        (param, VariancePosition::Invariant),
        (param, VariancePosition::Covariant),
    ];
    let table = VarianceAnalyzer::analyze_usages(usages);
    assert_eq!(table.variance_for(param), Some(Variance::Invariant));
}

#[test]
fn analyze_reads_generic_constraints() {
    let param = TypeId::new(13);
    let constraints = vec![
        usage_constraint(param, VariancePosition::Covariant),
        usage_constraint(param, VariancePosition::Covariant),
    ];
    let table = VarianceAnalyzer::analyze(&constraints);
    assert_eq!(table.variance_for(param), Some(Variance::Covariant));
}

#[test]
fn parameters_without_usage_default_to_bivariant() {
    let table = VarianceTable::new();
    assert_eq!(table.variance_for(TypeId::new(99)), None);
}
