use crate::constraint::{GenericConstraint, GenericConstraintKind};
use crate::solver::{GenericSolver, GenericSolverDiagnostic};
use crate::types::{
    BoundConstraint, BoundPredicate, GenericBounds, PrimitiveBoundConstraint, PrimitiveTypeName,
    SymbolId, TraitBound, TypeId, TypeKind, TypeVariant,
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

#[test]
fn primitive_bound_accepts_canonical_and_alias_variants() {
    let symbol = SymbolId::from("pkg::Prim");
    let param = TypeId::new(10);
    let predicate = BoundPredicate::Primitive(PrimitiveBoundConstraint::new(
        PrimitiveTypeName::Int,
        vec![PrimitiveTypeName::Char, PrimitiveTypeName::Short],
    ));

    // Char alias should satisfy the bound.
    let mut constraints = Vec::new();
    constraints.push(constraint(GenericConstraintKind::TypeArgument {
        callee: symbol.clone(),
        parameter: param,
        argument: TypeKind::new(TypeVariant::Primitive("Char")),
        argument_index: 0,
    }));
    constraints.push(constraint(GenericConstraintKind::BoundRequirement {
        owner: symbol.clone(),
        parameter: param,
        predicate: predicate.clone(),
    }));
    let solution = GenericSolver::resolve(&constraints);
    assert!(solution.diagnostics().is_empty());

    // Optional Int should also satisfy the bound.
    let mut optional_constraints = Vec::new();
    optional_constraints.push(constraint(GenericConstraintKind::TypeArgument {
        callee: symbol.clone(),
        parameter: param,
        argument: TypeKind::optional(TypeKind::new(TypeVariant::Primitive("Int"))),
        argument_index: 0,
    }));
    optional_constraints.push(constraint(GenericConstraintKind::BoundRequirement {
        owner: symbol,
        parameter: param,
        predicate,
    }));
    let optional_solution = GenericSolver::resolve(&optional_constraints);
    assert!(optional_solution.diagnostics().is_empty());
}

#[test]
fn primitive_bound_reports_violation_for_unrelated_type() {
    let symbol = SymbolId::from("pkg::Prim");
    let param = TypeId::new(11);
    let mut constraints = Vec::new();
    constraints.push(constraint(GenericConstraintKind::TypeArgument {
        callee: symbol.clone(),
        parameter: param,
        argument: TypeKind::new(TypeVariant::Primitive("Double")),
        argument_index: 0,
    }));
    constraints.push(constraint(GenericConstraintKind::BoundRequirement {
        owner: symbol.clone(),
        parameter: param,
        predicate: BoundPredicate::Primitive(PrimitiveBoundConstraint::new(
            PrimitiveTypeName::Int,
            vec![PrimitiveTypeName::Char],
        )),
    }));

    let solution = GenericSolver::resolve(&constraints);
    assert_eq!(solution.diagnostics().len(), 1);
    match &solution.diagnostics()[0] {
        GenericSolverDiagnostic::BoundViolation { predicate, .. } => match predicate {
            BoundPredicate::Primitive(bound) => {
                assert_eq!(bound.canonical(), PrimitiveTypeName::Int);
                assert_eq!(bound.alias_families(), &[PrimitiveTypeName::Char]);
            }
            other => panic!("expected primitive predicate, got {other:?}"),
        },
        other => panic!("expected bound violation, got {other:?}"),
    }
}
