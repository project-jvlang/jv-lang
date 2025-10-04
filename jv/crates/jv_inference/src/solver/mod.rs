//! Constraint solving interfaces and unification algorithms for the jv inference engine.
//!
//! This module reimplements the historical `jv_checker` solver with the richer
//! `TypeKind` data model, nullability propagation, and value restriction
//! bookkeeping described in the design specification. The implementation remains
//! deterministic and focuses on providing a solid foundation for later
//! extensions such as SAT-style bound reduction and incremental solving hooks.

pub mod generic;
pub mod null_safety_bridge;
pub mod variance;

pub use generic::{
    BoundSatisfactionChecker, GenericSolver, GenericSolverDiagnostic, TypeArgumentSolution,
};
pub use null_safety_bridge::NullSafetyBridge;
pub use variance::{Variance, VarianceAnalyzer, VariancePosition, VarianceTable};

use crate::constraint::{ConstraintGraph, ConstraintKind, NodeId};
use crate::types::{FieldType, TypeId, TypeKind, TypeVariant};
use jv_ast::Span;
use std::collections::{HashMap, VecDeque};
use std::fmt;

/// Constraint item processed by the solver.
#[derive(Debug, Clone)]
pub struct Constraint {
    pub kind: ConstraintKind,
    pub span: Option<Span>,
    pub note: Option<String>,
}

impl Constraint {
    pub fn new(kind: ConstraintKind) -> Self {
        Self {
            kind,
            span: None,
            note: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.note = Some(note.into());
        self
    }
}

/// FIFO queue of constraints waiting to be solved.
#[derive(Debug, Default, Clone)]
pub struct ConstraintSet {
    queue: VecDeque<Constraint>,
}

impl ConstraintSet {
    pub fn new() -> Self {
        Self {
            queue: VecDeque::new(),
        }
    }

    pub fn push(&mut self, constraint: Constraint) {
        self.queue.push_back(constraint);
    }

    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }

    pub fn len(&self) -> usize {
        self.queue.len()
    }

    fn pop(&mut self) -> Option<Constraint> {
        self.queue.pop_front()
    }

    pub fn drain(self) -> Vec<Constraint> {
        self.queue.into_iter().collect()
    }
}

/// Solver errors surfaced during constraint processing.
#[derive(Debug, Clone, PartialEq)]
pub enum SolveError {
    PlaceholderConstraint {
        placeholder: String,
    },
    TypeMismatch {
        left: TypeKind,
        right: TypeKind,
        note: Option<String>,
    },
    OccursCheck {
        id: TypeId,
        ty: TypeKind,
        note: Option<String>,
    },
}

impl fmt::Display for SolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SolveError::PlaceholderConstraint { placeholder } => {
                write!(f, "encountered placeholder constraint: {placeholder}")
            }
            SolveError::TypeMismatch { left, right, note } => {
                if let Some(note) = note {
                    write!(f, "type mismatch between {left:?} and {right:?} ({note})")
                } else {
                    write!(f, "type mismatch between {left:?} and {right:?}")
                }
            }
            SolveError::OccursCheck { id, ty, note } => {
                if let Some(note) = note {
                    write!(f, "occurs check failed for {id:?} in {ty:?} ({note})")
                } else {
                    write!(f, "occurs check failed for {id:?} in {ty:?}")
                }
            }
        }
    }
}

impl std::error::Error for SolveError {}

/// Binding produced by the solver once all constraints are resolved.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeBinding {
    pub id: TypeId,
    pub ty: TypeKind,
}

/// Telemetry collected during solving for performance monitoring.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct SolverTelemetry {
    pub constraints_processed: usize,
    pub nullability_merges: usize,
    pub optional_propagations: usize,
    pub preserved_constraints: usize,
    pub cache_hit_rate: Option<f64>,
    pub reinference_duration_ms: Option<f64>,
    pub invalidation_cascade_depth: usize,
}

impl SolverTelemetry {
    pub fn record_preserved_constraints(&mut self, preserved: usize) {
        self.preserved_constraints = preserved;
    }

    pub fn set_cache_hit_rate(&mut self, rate: Option<f64>) {
        self.cache_hit_rate = rate;
    }

    pub fn set_reinference_duration(&mut self, duration_ms: f64) {
        self.reinference_duration_ms = Some(duration_ms);
    }

    pub fn set_invalidation_cascade_depth(&mut self, depth: usize) {
        self.invalidation_cascade_depth = depth;
    }
}

/// Final result returned by the solver.
#[derive(Debug, Clone)]
pub struct SolveOutcome {
    pub bindings: Vec<TypeBinding>,
    pub residual_constraints: Vec<Constraint>,
    pub value_restrictions: Vec<Vec<NodeId>>,
    pub telemetry: SolverTelemetry,
}

impl SolveOutcome {
    fn new(
        bindings: Vec<TypeBinding>,
        residual_constraints: Vec<Constraint>,
        telemetry: SolverTelemetry,
        value_restrictions: Vec<Vec<NodeId>>,
    ) -> Self {
        Self {
            bindings,
            residual_constraints,
            value_restrictions,
            telemetry,
        }
    }
}

/// Hindley-Milner style constraint solver with nullability awareness.
#[derive(Debug, Default)]
pub struct ConstraintSolver {
    substitutions: HashMap<TypeId, TypeKind>,
    telemetry: SolverTelemetry,
}

impl ConstraintSolver {
    pub fn new() -> Self {
        Self::default()
    }

    /// Consumes the solver and resolves the provided constraint set. The caller
    /// may optionally pass a constraint graph to enable value restriction
    /// detection.
    pub fn solve(
        mut self,
        mut constraints: ConstraintSet,
        graph: Option<&ConstraintGraph>,
    ) -> Result<SolveOutcome, SolveError> {
        while let Some(constraint) = constraints.pop() {
            self.telemetry.constraints_processed += 1;
            self.process_constraint(constraint)?;
        }

        let bindings = self
            .substitutions
            .into_iter()
            .map(|(id, ty)| TypeBinding { id, ty })
            .collect::<Vec<_>>();

        let mut bindings = bindings;
        bindings.sort_by_key(|binding| binding.id.to_raw());

        let residual = constraints.drain();
        let value_restrictions = graph
            .map(|g| value_restriction_components(g))
            .unwrap_or_default();

        Ok(SolveOutcome::new(
            bindings,
            residual,
            self.telemetry,
            value_restrictions,
        ))
    }

    fn process_constraint(&mut self, constraint: Constraint) -> Result<(), SolveError> {
        match constraint.kind {
            ConstraintKind::Equality { left, right } => {
                let left_ty = TypeKind::new(TypeVariant::Variable(left));
                let right_ty = TypeKind::new(TypeVariant::Variable(right));
                self.unify(left_ty, right_ty, constraint.note)?;
            }
            ConstraintKind::Assignment { from, to } => {
                let left_ty = TypeKind::new(TypeVariant::Variable(from));
                let right_ty = TypeKind::new(TypeVariant::Variable(to));
                self.unify(left_ty, right_ty, constraint.note)?;
            }
            ConstraintKind::BoundSatisfaction { .. } | ConstraintKind::Custom(_) => {
                // SAT-style bound validation will be implemented later. For now we
                // treat these constraints as already satisfied and record no-op.
            }
        }
        Ok(())
    }

    fn unify(
        &mut self,
        left: TypeKind,
        right: TypeKind,
        note: Option<String>,
    ) -> Result<TypeKind, SolveError> {
        let left = self.prune(left);
        let right = self.prune(right);
        let left_clone = left.clone();
        let right_clone = right.clone();

        let merged_nullability = left.nullability().combine(right.nullability());

        let result = match (left.variant().clone(), right.variant().clone()) {
            (TypeVariant::Variable(left_id), TypeVariant::Variable(right_id)) => {
                if left_id == right_id {
                    left_clone
                } else {
                    self.bind_variable(
                        left_id,
                        right_clone.clone().with_nullability(merged_nullability),
                        note.clone(),
                    )?;
                    right_clone
                }
            }
            (TypeVariant::Variable(id), _) => {
                let ty = right_clone.clone().with_nullability(merged_nullability);
                self.bind_variable(id, ty.clone(), note.clone())?;
                ty
            }
            (_, TypeVariant::Variable(id)) => {
                let ty = left_clone.clone().with_nullability(merged_nullability);
                self.bind_variable(id, ty.clone(), note.clone())?;
                ty
            }
            (TypeVariant::Primitive(a), TypeVariant::Primitive(b)) => {
                if a == b {
                    TypeKind::new(TypeVariant::Primitive(a))
                } else {
                    return Err(SolveError::TypeMismatch {
                        left: TypeKind::new(TypeVariant::Primitive(a)),
                        right: TypeKind::new(TypeVariant::Primitive(b)),
                        note,
                    });
                }
            }
            (TypeVariant::Optional(left_inner), TypeVariant::Optional(right_inner)) => {
                self.telemetry.optional_propagations += 1;
                let unified = self.unify(*left_inner, *right_inner, note.clone())?;
                TypeKind::optional(unified)
            }
            (TypeVariant::Optional(_), _) | (_, TypeVariant::Optional(_)) => {
                return Err(SolveError::TypeMismatch {
                    left: left_clone,
                    right: right_clone,
                    note,
                });
            }
            (TypeVariant::Function(params_a, ret_a), TypeVariant::Function(params_b, ret_b)) => {
                if params_a.len() != params_b.len() {
                    return Err(SolveError::TypeMismatch {
                        left: left_clone,
                        right: right_clone,
                        note: note.clone(),
                    });
                }
                let mut new_params = Vec::with_capacity(params_a.len());
                for (p_a, p_b) in params_a.into_iter().zip(params_b.into_iter()) {
                    new_params.push(self.unify(p_a, p_b, note.clone())?);
                }
                let new_ret = self.unify(*ret_a, *ret_b, note.clone())?;
                TypeKind::function(new_params, new_ret)
            }
            (
                TypeVariant::Record { fields: fields_a },
                TypeVariant::Record { fields: fields_b },
            ) => {
                if fields_a.len() != fields_b.len() {
                    return Err(SolveError::TypeMismatch {
                        left: left_clone,
                        right: right_clone,
                        note: note.clone(),
                    });
                }
                let mut merged_fields = Vec::with_capacity(fields_a.len());
                for (field_a, field_b) in fields_a.into_iter().zip(fields_b.into_iter()) {
                    if field_a.name != field_b.name {
                        return Err(SolveError::TypeMismatch {
                            left: left_clone,
                            right: right_clone,
                            note: note.clone(),
                        });
                    }
                    let ty = self.unify(field_a.ty.clone(), field_b.ty.clone(), note.clone())?;
                    merged_fields.push(FieldType::new(field_a.name.clone(), ty));
                }
                TypeKind::record(merged_fields)
            }
            (TypeVariant::Union { arms: arms_a }, TypeVariant::Union { arms: arms_b }) => {
                if arms_a.len() != arms_b.len() {
                    return Err(SolveError::TypeMismatch {
                        left: left_clone,
                        right: right_clone,
                        note: note.clone(),
                    });
                }
                let merged = arms_a
                    .into_iter()
                    .zip(arms_b.into_iter())
                    .map(|(a, b)| self.unify(a, b, note.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                TypeKind::union(merged)
            }
            (TypeVariant::Unknown, _) => right_clone,
            (_, TypeVariant::Unknown) => left_clone,
            _ => {
                return Err(SolveError::TypeMismatch {
                    left: left_clone,
                    right: right_clone,
                    note: note.clone(),
                });
            }
        };

        self.telemetry.nullability_merges += 1;
        Ok(result.with_nullability(merged_nullability))
    }

    fn bind_variable(
        &mut self,
        id: TypeId,
        ty: TypeKind,
        note: Option<String>,
    ) -> Result<(), SolveError> {
        if ty.free_type_vars().contains(&id) {
            return Err(SolveError::OccursCheck { id, ty, note });
        }
        self.substitutions.insert(id, ty);
        Ok(())
    }

    fn prune(&self, ty: TypeKind) -> TypeKind {
        if let TypeVariant::Variable(id) = ty.variant().clone() {
            if let Some(replacement) = self.substitutions.get(&id) {
                return self.prune(replacement.clone());
            }
        }
        ty
    }
}

fn value_restriction_components(graph: &ConstraintGraph) -> Vec<Vec<NodeId>> {
    let cache = graph.strongly_connected_components();
    cache
        .components()
        .filter(|component| component.len() > 1)
        .map(|component| component.to_vec())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constraint::{ConstraintGraph, ConstraintNode, EdgeKind, TypeNode};
    use crate::types::{FieldType, NullabilityFlag, TypeId, TypeKind, TypeVariant};

    fn make_constraint_set(constraints: Vec<Constraint>) -> ConstraintSet {
        let mut set = ConstraintSet::new();
        for constraint in constraints {
            set.push(constraint);
        }
        set
    }

    #[test]
    fn solves_variable_equality() {
        let constraints = make_constraint_set(vec![Constraint::new(ConstraintKind::Equality {
            left: TypeId::new(0),
            right: TypeId::new(1),
        })]);
        let graph = None;
        let outcome = ConstraintSolver::new()
            .solve(constraints, graph)
            .expect("solve");
        assert_eq!(outcome.bindings.len(), 1);
    }

    #[test]
    fn occurs_check_failure() {
        let mut solver = ConstraintSolver::new();
        let ty = TypeKind::function(
            vec![TypeKind::new(TypeVariant::Variable(TypeId::new(0)))],
            TypeKind::new(TypeVariant::Variable(TypeId::new(0))),
        );
        let err = solver.bind_variable(TypeId::new(0), ty, None).unwrap_err();
        assert!(matches!(err, SolveError::OccursCheck { .. }));
    }

    #[test]
    fn optional_unification_propagates_inner_type() {
        let opt_int = TypeKind::optional(TypeKind::new(TypeVariant::Primitive("Int")));
        let other_opt_int = TypeKind::optional(TypeKind::new(TypeVariant::Primitive("Int")));
        let mut solver = ConstraintSolver::new();
        let result = solver.unify(opt_int, other_opt_int, None).expect("unify");
        assert!(matches!(result.variant(), TypeVariant::Optional(_)));
        assert_eq!(result.nullability(), NullabilityFlag::Nullable);
    }

    #[test]
    fn function_type_mismatch_is_reported() {
        let mut solver = ConstraintSolver::new();
        let left = TypeKind::function(
            vec![TypeKind::new(TypeVariant::Primitive("Int"))],
            TypeKind::new(TypeVariant::Primitive("Int")),
        );
        let right = TypeKind::function(
            vec![
                TypeKind::new(TypeVariant::Primitive("Int")),
                TypeKind::new(TypeVariant::Primitive("String")),
            ],
            TypeKind::new(TypeVariant::Primitive("Int")),
        );
        let err = solver.unify(left, right, None).unwrap_err();
        assert!(matches!(err, SolveError::TypeMismatch { .. }));
    }

    #[test]
    fn records_value_restriction_components() {
        let mut graph = ConstraintGraph::new();
        let t1 = graph.add_type_node(TypeNode::new(TypeId::new(0), TypeKind::default()));
        let t2 = graph.add_type_node(TypeNode::new(TypeId::new(1), TypeKind::default()));
        let constraint = graph.add_constraint_node(ConstraintNode::new(ConstraintKind::Equality {
            left: TypeId::new(0),
            right: TypeId::new(1),
        }));
        graph.add_edge(
            NodeId::Type(t1),
            NodeId::Constraint(constraint),
            EdgeKind::Equality,
        );
        graph.add_edge(
            NodeId::Constraint(constraint),
            NodeId::Type(t2),
            EdgeKind::Equality,
        );
        graph.add_edge(NodeId::Type(t2), NodeId::Type(t1), EdgeKind::Assignment);

        let outcome = ConstraintSolver::new()
            .solve(ConstraintSet::new(), Some(&graph))
            .expect("solve");
        assert_eq!(outcome.value_restrictions.len(), 1);
    }

    #[test]
    fn record_type_unification_merges_fields() {
        let record_a = TypeKind::record(vec![FieldType::new(
            "value",
            TypeKind::new(TypeVariant::Primitive("Int")),
        )]);
        let record_b = TypeKind::record(vec![FieldType::new(
            "value",
            TypeKind::new(TypeVariant::Primitive("Int")),
        )]);
        let mut solver = ConstraintSolver::new();
        let unified = solver.unify(record_a, record_b, None).expect("unify");
        assert!(matches!(unified.variant(), TypeVariant::Record { .. }));
    }

    #[test]
    fn union_unification_requires_same_arms() {
        let union_a = TypeKind::union(vec![TypeKind::new(TypeVariant::Primitive("Int"))]);
        let union_b = TypeKind::union(vec![TypeKind::new(TypeVariant::Primitive("String"))]);
        let mut solver = ConstraintSolver::new();
        let err = solver.unify(union_a, union_b, None).unwrap_err();
        assert!(matches!(err, SolveError::TypeMismatch { .. }));
    }
}
