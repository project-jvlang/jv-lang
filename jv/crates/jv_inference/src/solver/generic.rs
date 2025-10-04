use crate::constraint::{
    CapabilityDictionaryResolver, CapabilityResolutionError, GenericConstraint,
    GenericConstraintKind,
};
use crate::environment::CapabilityEnvironment;
use crate::types::{
    BoundPredicate, CapabilityBound, CapabilitySolution, SymbolId, TypeId, TypeKind, TypeVariant,
};
use jv_ast::Span;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};

/// Result produced by [`GenericSolver::resolve`].
#[derive(Debug, Default, Clone)]
pub struct TypeArgumentSolution {
    assignments: HashMap<SymbolId, HashMap<TypeId, TypeKind>>,
    capabilities: HashMap<SymbolId, Vec<CapabilitySolution>>,
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

    pub fn capability_solutions(&self, symbol: &SymbolId) -> Option<&[CapabilitySolution]> {
        self.capabilities
            .get(symbol)
            .map(|solutions| solutions.as_slice())
    }

    pub fn all_capability_solutions(&self) -> &HashMap<SymbolId, Vec<CapabilitySolution>> {
        &self.capabilities
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
    CapabilityResolutionFailed {
        symbol: SymbolId,
        parameter: TypeId,
        bound: CapabilityBound,
        span: Span,
        error: CapabilityResolutionError,
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
        Self::resolve_with_environment(constraints, &CapabilityEnvironment::new())
    }

    pub fn resolve_with_environment(
        constraints: &[GenericConstraint],
        environment: &CapabilityEnvironment,
    ) -> TypeArgumentSolution {
        let mut assignments: HashMap<SymbolId, HashMap<TypeId, TypeKind>> = HashMap::new();
        let mut sources: HashMap<SymbolId, HashMap<TypeId, Span>> = HashMap::new();
        let mut pending_bounds: Vec<BoundRequirementRecord> = Vec::new();
        let mut diagnostics: Vec<GenericSolverDiagnostic> = Vec::new();
        let mut capability_solutions: HashMap<SymbolId, Vec<CapabilitySolution>> = HashMap::new();
        let resolver = CapabilityDictionaryResolver::new(environment);

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

        let mut processed_requirements: HashSet<(String, TypeId, String)> = HashSet::new();

        for requirement in pending_bounds {
            let symbol = requirement.symbol.clone();
            let parameter = requirement.parameter;
            let span = requirement.span.clone();
            let predicate = requirement.predicate.clone();
            let predicate_key = predicate.key();
            let key = (symbol.as_str().to_owned(), parameter, predicate_key.clone());
            if !processed_requirements.insert(key) {
                continue;
            }

            match assignments.get(&symbol).and_then(|map| map.get(&parameter)) {
                Some(candidate) => {
                    if matches!(candidate.variant(), TypeVariant::Unknown) {
                        diagnostics.push(GenericSolverDiagnostic::UnresolvedParameter {
                            symbol,
                            parameter,
                            predicate: Some(predicate),
                            span,
                        });
                        continue;
                    }

                    match predicate {
                        BoundPredicate::Capability(bound) => {
                            let bound_clone = bound.clone();
                            match resolver.resolve(&bound, candidate) {
                                Ok(solution) => {
                                    capability_solutions
                                        .entry(symbol.clone())
                                        .or_default()
                                        .push(solution);
                                }
                                Err(error) => {
                                    diagnostics.push(
                                        GenericSolverDiagnostic::CapabilityResolutionFailed {
                                            symbol: symbol.clone(),
                                            parameter,
                                            bound: bound_clone,
                                            span: span.clone(),
                                            error,
                                        },
                                    );
                                }
                            }
                        }
                        other => {
                            if !BoundSatisfactionChecker::evaluate(candidate, &other) {
                                diagnostics.push(GenericSolverDiagnostic::BoundViolation {
                                    symbol: symbol.clone(),
                                    parameter,
                                    predicate: other.clone(),
                                    span: span.clone(),
                                });
                            }
                        }
                    }
                }
                None => diagnostics.push(GenericSolverDiagnostic::UnresolvedParameter {
                    symbol,
                    parameter,
                    predicate: Some(predicate),
                    span,
                }),
            }
        }

        TypeArgumentSolution {
            assignments,
            capabilities: capability_solutions,
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
