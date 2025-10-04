use crate::constraint::{GenericConstraint, GenericConstraintKind};
use crate::types::{BoundPredicate, SymbolId, TypeId, TypeKind};
use jv_ast::Span;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

/// Result produced by [`GenericSolver::resolve`].
#[derive(Debug, Default, Clone)]
pub struct TypeArgumentSolution {
    assignments: HashMap<SymbolId, HashMap<TypeId, TypeKind>>,
    diagnostics: Vec<GenericSolverDiagnostic>,
}

impl TypeArgumentSolution {
    pub fn assignments(&self) -> &HashMap<SymbolId, HashMap<TypeId, TypeKind>> {
        &self.assignments
    }

    pub fn assignment(&self, symbol: &SymbolId, param: &TypeId) -> Option<&TypeKind> {
        self.assignments.get(symbol).and_then(|map| map.get(param))
    }

    pub fn diagnostics(&self) -> &[GenericSolverDiagnostic] {
        &self.diagnostics
    }
}

/// Diagnostics captured while processing generic constraints.
#[derive(Debug, Clone, PartialEq)]
pub enum GenericSolverDiagnostic {
    ConflictingArgument {
        symbol: SymbolId,
        parameter: TypeId,
        previous: TypeKind,
        candidate: TypeKind,
        previous_span: Span,
        candidate_span: Span,
    },
    BoundViolation {
        symbol: SymbolId,
        parameter: TypeId,
        predicate: BoundPredicate,
        span: Span,
    },
    UnresolvedParameter {
        symbol: SymbolId,
        parameter: TypeId,
        predicate: Option<BoundPredicate>,
        span: Span,
    },
}

#[derive(Debug, Clone)]
struct BoundRequirementRecord {
    symbol: SymbolId,
    parameter: TypeId,
    predicate: BoundPredicate,
    span: Span,
}

/// SAT-style solver that evaluates generic constraints emitted by builders.
#[derive(Debug, Default)]
pub struct GenericSolver;

impl GenericSolver {
    pub fn resolve(constraints: &[GenericConstraint]) -> TypeArgumentSolution {
        let mut assignments: HashMap<SymbolId, HashMap<TypeId, TypeKind>> = HashMap::new();
        let mut sources: HashMap<SymbolId, HashMap<TypeId, Span>> = HashMap::new();
        let mut pending_bounds: Vec<BoundRequirementRecord> = Vec::new();
        let mut diagnostics: Vec<GenericSolverDiagnostic> = Vec::new();

        for constraint in constraints {
            match &constraint.kind {
                GenericConstraintKind::TypeArgument {
                    callee,
                    parameter,
                    argument,
                    ..
                } => {
                    process_assignment(
                        &mut assignments,
                        &mut sources,
                        &mut diagnostics,
                        callee.clone(),
                        *parameter,
                        argument.clone(),
                        constraint.span.clone(),
                    );
                }
                GenericConstraintKind::ConstructorArgument {
                    ctor,
                    parameter,
                    argument,
                    ..
                } => {
                    process_assignment(
                        &mut assignments,
                        &mut sources,
                        &mut diagnostics,
                        ctor.clone(),
                        *parameter,
                        argument.clone(),
                        constraint.span.clone(),
                    );
                }
                GenericConstraintKind::BoundRequirement {
                    owner,
                    parameter,
                    predicate,
                } => {
                    pending_bounds.push(BoundRequirementRecord {
                        symbol: owner.clone(),
                        parameter: *parameter,
                        predicate: predicate.clone(),
                        span: constraint.span.clone(),
                    });
                }
                GenericConstraintKind::VarianceUsage { .. } => {
                    // Variance constraints are processed by `VarianceAnalyzer`.
                }
            }
        }

        for requirement in pending_bounds {
            match assignments
                .get(&requirement.symbol)
                .and_then(|map| map.get(&requirement.parameter))
            {
                Some(candidate) => {
                    if !BoundSatisfactionChecker::evaluate(candidate, &requirement.predicate) {
                        diagnostics.push(GenericSolverDiagnostic::BoundViolation {
                            symbol: requirement.symbol.clone(),
                            parameter: requirement.parameter,
                            predicate: requirement.predicate.clone(),
                            span: requirement.span.clone(),
                        });
                    }
                }
                None => diagnostics.push(GenericSolverDiagnostic::UnresolvedParameter {
                    symbol: requirement.symbol.clone(),
                    parameter: requirement.parameter,
                    predicate: Some(requirement.predicate.clone()),
                    span: requirement.span.clone(),
                }),
            }
        }

        TypeArgumentSolution {
            assignments,
            diagnostics,
        }
    }
}

fn process_assignment(
    assignments: &mut HashMap<SymbolId, HashMap<TypeId, TypeKind>>,
    sources: &mut HashMap<SymbolId, HashMap<TypeId, Span>>,
    diagnostics: &mut Vec<GenericSolverDiagnostic>,
    symbol: SymbolId,
    parameter: TypeId,
    argument: TypeKind,
    span: Span,
) {
    let entry = assignments.entry(symbol.clone()).or_default();
    let source_entry = sources.entry(symbol.clone()).or_default();

    match entry.entry(parameter) {
        Entry::Vacant(vacant) => {
            source_entry.insert(parameter, span.clone());
            vacant.insert(argument);
        }
        Entry::Occupied(occupied) => {
            if occupied.get() != &argument {
                let previous = occupied.get().clone();
                let previous_span = source_entry
                    .get(&parameter)
                    .cloned()
                    .unwrap_or_else(Span::dummy);
                diagnostics.push(GenericSolverDiagnostic::ConflictingArgument {
                    symbol,
                    parameter,
                    previous,
                    candidate: argument,
                    previous_span,
                    candidate_span: span,
                });
            }
        }
    }
}

/// Evaluates whether a candidate type satisfies a required predicate.
#[derive(Debug, Default)]
pub struct BoundSatisfactionChecker;

impl BoundSatisfactionChecker {
    pub fn evaluate(candidate: &TypeKind, predicate: &BoundPredicate) -> bool {
        candidate
            .bounds()
            .map(|bounds| bounds.contains_predicate(predicate))
            .unwrap_or(false)
    }
}
