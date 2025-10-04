use crate::constraint::{GenericConstraint, GenericConstraintKind};
use crate::solver::{GenericSolver, GenericSolverDiagnostic};
use crate::types::{
    BoundConstraint, BoundPredicate, GenericBounds, SymbolId, TraitBound, TypeId, TypeKind,
    TypeVariant,
};
use jv_ast::Span;

fn dummy_span() -> Span {
    Span::dummy()
}

fn constraint(kind: GenericConstraintKind) -> GenericConstraint {
    GenericConstraint::new(kind, dummy_span())
}

#[test]
fn detects_conflicting_assignments() {
    let symbol = SymbolId::from("pkg::Foo");
    let param = TypeId::new(1);
    let mut constraints = Vec::new();
    constraints.push(constraint(GenericConstraintKind::TypeArgument {
        callee: symbol.clone(),
        parameter: param,
        argument: TypeKind::new(TypeVariant::Primitive("Int")),
        argument_index: 0,
    }));
    constraints.push(constraint(GenericConstraintKind::TypeArgument {
        callee: symbol.clone(),
        parameter: param,
        argument: TypeKind::new(TypeVariant::Primitive("String")),
        argument_index: 0,
    }));

    let solution = GenericSolver::resolve(&constraints);
    assert_eq!(solution.diagnostics().len(), 1);
    assert!(matches!(
        solution.diagnostics()[0],
        GenericSolverDiagnostic::ConflictingArgument { .. }
    ));
}

#[test]
fn reports_bound_violation_when_predicate_missing() {
    let symbol = SymbolId::from("pkg::Foo");
    let param = TypeId::new(2);
    let argument = TypeKind::new(TypeVariant::Primitive("Int"));
    let mut constraints = Vec::new();
    constraints.push(constraint(GenericConstraintKind::TypeArgument {
        callee: symbol.clone(),
        parameter: param,
        argument,
        argument_index: 0,
    }));
    constraints.push(constraint(GenericConstraintKind::BoundRequirement {
        owner: symbol.clone(),
        parameter: param,
        predicate: BoundPredicate::Trait(TraitBound::simple("Display")),
    }));

    let solution = GenericSolver::resolve(&constraints);
    assert_eq!(solution.diagnostics().len(), 1);
    assert!(matches!(
        solution.diagnostics()[0],
        GenericSolverDiagnostic::BoundViolation { .. }
    ));
}

#[test]
fn accepts_argument_when_bound_present() {
    let symbol = SymbolId::from("pkg::Foo");
    let param = TypeId::new(3);
    let bounds = GenericBounds::new(vec![BoundConstraint::new(
        param,
        BoundPredicate::Trait(TraitBound::simple("Display")),
    )]);
    let argument = TypeKind::variable(param).with_bounds(bounds);
    let mut constraints = Vec::new();
    constraints.push(constraint(GenericConstraintKind::TypeArgument {
        callee: symbol.clone(),
        parameter: param,
        argument,
        argument_index: 0,
    }));
    constraints.push(constraint(GenericConstraintKind::BoundRequirement {
        owner: symbol.clone(),
        parameter: param,
        predicate: BoundPredicate::Trait(TraitBound::simple("Display")),
    }));

    let solution = GenericSolver::resolve(&constraints);
    assert!(solution.diagnostics().is_empty());
}
