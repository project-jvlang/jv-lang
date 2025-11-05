//! 単一化ソルバと制約解決ロジックを提供する。
//!
//! `ConstraintSolver` は制約キューを反復的に消化し、Robinson 型単一化を用いて
//! 型変数への束縛を決定する。Optional 型は内包する型を再帰的に単一化することで
//! 伝播させ、Null 安全と一致する解を導出する。

use crate::inference::constraint::{Constraint, ConstraintKind, ConstraintSet};
use crate::inference::conversions::{
    AppliedConversion, ConversionHelperCatalog, ConversionMetadata, ConversionOutcome,
    ConversionRulesEngine,
};
use crate::inference::types::{
    TypeBinding, TypeError, TypeId, TypeKind, TypeVariable, TypeVariableKind,
};
use jv_inference::ParallelInferenceConfig;
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

/// 制約解決におけるエラー。
#[derive(Debug, PartialEq)]
pub enum SolveError {
    Placeholder {
        placeholder: &'static str,
        note: Option<String>,
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
    TypeError(TypeError),
}

impl SolveError {
    fn note_suffix(&self) -> String {
        match self {
            SolveError::Placeholder { note, .. }
            | SolveError::TypeMismatch { note, .. }
            | SolveError::OccursCheck { note, .. } => note
                .as_ref()
                .map(|n| format!(" ({})", n))
                .unwrap_or_default(),
            SolveError::TypeError(_) => String::new(),
        }
    }
}

impl fmt::Display for SolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SolveError::Placeholder { placeholder, .. } => {
                write!(
                    f,
                    "encountered placeholder constraint: {placeholder}{}",
                    self.note_suffix()
                )
            }
            SolveError::TypeMismatch { left, right, .. } => {
                write!(
                    f,
                    "type mismatch between {left:?} and {right:?}{}",
                    self.note_suffix()
                )
            }
            SolveError::OccursCheck { id, ty, .. } => {
                write!(
                    f,
                    "occurs check failed for {id} in {ty:?}{}",
                    self.note_suffix()
                )
            }
            SolveError::TypeError(error) => write!(f, "type error: {error}"),
        }
    }
}

impl std::error::Error for SolveError {}

/// 制約集合を解いた結果。
#[derive(Debug, Default)]
pub struct SolveResult {
    pub bindings: Vec<TypeBinding>,
    pub remaining: ConstraintSet,
    pub conversions: Vec<AppliedConversion>,
}

/// 単一化ソルバ本体。
#[derive(Debug)]
pub struct ConstraintSolver {
    substitutions: HashMap<TypeId, TypeKind>,
    parallel_config: ParallelInferenceConfig,
    conversions: Vec<AppliedConversion>,
    conversion_catalog: Option<Arc<ConversionHelperCatalog>>,
}

impl Default for ConstraintSolver {
    fn default() -> Self {
        Self::with_config(ParallelInferenceConfig::default())
    }
}

impl ConstraintSolver {
    /// 新しいソルバを構築する。
    pub fn new() -> Self {
        Self::default()
    }

    /// 並列設定を指定してソルバを構築する。
    pub fn with_config(config: ParallelInferenceConfig) -> Self {
        Self {
            substitutions: HashMap::new(),
            parallel_config: config.sanitized(),
            conversions: Vec::new(),
            conversion_catalog: None,
        }
    }

    /// 制約集合を解決し、型束縛を返す。
    pub fn solve(mut self, mut constraints: ConstraintSet) -> Result<SolveResult, SolveError> {
        let batch_size = self.parallel_config.constraint_batching.max(1);

        while !constraints.is_empty() {
            let batch = constraints.pop_batch(batch_size);

            if batch.is_empty() {
                break;
            }

            for constraint in batch {
                self.process_constraint(&constraint)?;
            }
        }

        let mut bindings: Vec<_> = self
            .substitutions
            .into_iter()
            .map(|(id, ty)| {
                let variable = TypeVariable {
                    id,
                    name: None,
                    kind: TypeVariableKind::Bound(ty.clone()),
                };
                TypeBinding::new(variable, ty)
            })
            .collect();

        bindings.sort_by_key(|binding| binding.variable.id.to_raw());

        Ok(SolveResult {
            bindings,
            remaining: constraints,
            conversions: self.conversions,
        })
    }

    pub fn set_conversion_catalog(&mut self, catalog: Option<Arc<ConversionHelperCatalog>>) {
        self.conversion_catalog = catalog;
    }

    fn process_constraint(&mut self, constraint: &Constraint) -> Result<(), SolveError> {
        match &constraint.kind {
            ConstraintKind::Equal(left, right) => {
                self.unify(left.clone(), right.clone(), constraint.note.clone())?;
            }
            ConstraintKind::Assign(id, ty) => {
                self.unify(TypeKind::Variable(*id), ty.clone(), constraint.note.clone())?;
            }
            ConstraintKind::Convertible { from, to } => {
                self.handle_convertible(from.clone(), to.clone(), false, constraint.note.clone())?;
            }
            ConstraintKind::ConvertibleWithWarning { from, to } => {
                self.handle_convertible(from.clone(), to.clone(), true, constraint.note.clone())?;
            }
            ConstraintKind::Placeholder(placeholder) => {
                return Err(SolveError::Placeholder {
                    placeholder: *placeholder,
                    note: constraint.note.clone(),
                });
            }
            ConstraintKind::ReportError(error) => {
                return Err(SolveError::TypeError(error.clone()));
            }
        }
        Ok(())
    }

    fn handle_convertible(
        &mut self,
        from: TypeKind,
        to: TypeKind,
        warned: bool,
        note: Option<String>,
    ) -> Result<(), SolveError> {
        let from = self.prune(from);
        let to = self.prune(to);

        match ConversionRulesEngine::analyze_with_catalog(
            &from,
            &to,
            self.conversion_catalog.as_deref(),
        ) {
            ConversionOutcome::Identity => {
                self.unify(from, to, note)?;
            }
            ConversionOutcome::Allowed(metadata) => {
                self.register_conversion(from.clone(), to.clone(), metadata, warned);
                self.bind_for_conversion(&from, &to, note)?;
            }
            ConversionOutcome::Rejected(error) => {
                return Err(SolveError::TypeError(error));
            }
        }
        Ok(())
    }

    fn register_conversion(
        &mut self,
        from: TypeKind,
        to: TypeKind,
        metadata: ConversionMetadata,
        warned: bool,
    ) {
        let ConversionMetadata {
            kind,
            helper,
            nullable_guard,
        } = metadata;
        self.conversions.push(AppliedConversion::new(
            from,
            to,
            kind,
            helper,
            nullable_guard,
            warned,
        ));
    }

    fn bind_for_conversion(
        &mut self,
        from: &TypeKind,
        to: &TypeKind,
        note: Option<String>,
    ) -> Result<(), SolveError> {
        match from {
            TypeKind::Variable(id) => {
                self.bind_variable(*id, to.clone(), note.clone())?;
            }
            TypeKind::Optional(inner) => {
                self.bind_for_conversion(inner, to, note.clone())?;
            }
            _ => {}
        }

        match to {
            TypeKind::Variable(id) => {
                self.bind_variable(*id, from.clone(), note)?;
            }
            TypeKind::Optional(inner) => {
                self.bind_for_conversion(from, inner, None)?;
            }
            _ => {}
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
        let right = self.prune(right.clone());

        match (left.clone(), right.clone()) {
            (TypeKind::Variable(id_left), TypeKind::Variable(id_right)) => {
                if id_left == id_right {
                    return Ok(TypeKind::Variable(id_left));
                }
                self.bind_variable(id_left, TypeKind::Variable(id_right), note)
            }
            (TypeKind::Variable(id), ty) => self.bind_variable(id, ty, note),
            (ty, TypeKind::Variable(id)) => self.bind_variable(id, ty, note),
            (TypeKind::Primitive(a), TypeKind::Primitive(b)) => {
                if a == b {
                    Ok(TypeKind::Primitive(a))
                } else {
                    Err(SolveError::TypeMismatch {
                        left: TypeKind::Primitive(a),
                        right: TypeKind::Primitive(b),
                        note,
                    })
                }
            }
            (TypeKind::Boxed(a), TypeKind::Boxed(b)) => {
                if a == b {
                    Ok(TypeKind::Boxed(a))
                } else {
                    Err(SolveError::TypeMismatch {
                        left: TypeKind::Boxed(a),
                        right: TypeKind::Boxed(b),
                        note,
                    })
                }
            }
            (TypeKind::Reference(a), TypeKind::Reference(b)) => {
                if a == b {
                    Ok(TypeKind::reference(a))
                } else {
                    Err(SolveError::TypeMismatch {
                        left: TypeKind::Reference(a),
                        right: TypeKind::Reference(b),
                        note,
                    })
                }
            }
            (TypeKind::Function(mut params_a, ret_a), TypeKind::Function(mut params_b, ret_b)) => {
                if params_a.len() != params_b.len() {
                    return Err(SolveError::TypeMismatch {
                        left: TypeKind::Function(params_a, ret_a),
                        right: TypeKind::Function(params_b, ret_b),
                        note,
                    });
                }

                let mut unified_params = Vec::with_capacity(params_a.len());
                for (a, b) in params_a.drain(..).zip(params_b.drain(..)) {
                    let unified = self.unify(a, b, note.clone())?;
                    unified_params.push(unified);
                }

                let unified_return = self.unify(*ret_a, *ret_b, note)?;
                Ok(TypeKind::Function(unified_params, Box::new(unified_return)))
            }
            (TypeKind::Tuple(mut elements_a), TypeKind::Tuple(mut elements_b)) => {
                if elements_a.len() != elements_b.len() {
                    return Err(SolveError::TypeMismatch {
                        left: TypeKind::Tuple(elements_a),
                        right: TypeKind::Tuple(elements_b),
                        note,
                    });
                }

                let mut unified_elements = Vec::with_capacity(elements_a.len());
                for (left_elem, right_elem) in elements_a.drain(..).zip(elements_b.drain(..)) {
                    let unified = self.unify(left_elem, right_elem, note.clone())?;
                    unified_elements.push(unified);
                }

                Ok(TypeKind::Tuple(unified_elements))
            }
            (TypeKind::Optional(left_inner), TypeKind::Optional(right_inner)) => {
                let unified = self.unify(*left_inner, *right_inner, note)?;
                Ok(TypeKind::Optional(Box::new(unified)))
            }
            (TypeKind::Optional(left_inner), other) => {
                let unified = self.unify(*left_inner, other, note)?;
                Ok(TypeKind::Optional(Box::new(unified)))
            }
            (other, TypeKind::Optional(right_inner)) => {
                let unified = self.unify(other, *right_inner, note)?;
                Ok(TypeKind::Optional(Box::new(unified)))
            }
            (TypeKind::Unknown, _) => Ok(right),
            (_, TypeKind::Unknown) => Ok(left),
            (l, r) => Err(SolveError::TypeMismatch {
                left: l,
                right: r,
                note,
            }),
        }
    }

    fn bind_variable(
        &mut self,
        id: TypeId,
        ty: TypeKind,
        note: Option<String>,
    ) -> Result<TypeKind, SolveError> {
        let ty = self.prune(ty);

        if matches!(ty, TypeKind::Variable(var_id) if var_id == id) {
            return Ok(ty);
        }

        if self.occurs_in(id, &ty) {
            return Err(SolveError::OccursCheck { id, ty, note });
        }

        self.substitutions.insert(id, ty.clone());
        Ok(ty)
    }

    fn prune(&self, ty: TypeKind) -> TypeKind {
        match ty {
            TypeKind::Variable(id) => {
                if let Some(sub) = self.substitutions.get(&id) {
                    self.prune(sub.clone())
                } else {
                    TypeKind::Variable(id)
                }
            }
            TypeKind::Optional(inner) => TypeKind::Optional(Box::new(self.prune(*inner))),
            TypeKind::Tuple(elements) => {
                let pruned = elements.into_iter().map(|elem| self.prune(elem)).collect();
                TypeKind::Tuple(pruned)
            }
            other => other,
        }
    }

    fn occurs_in(&self, id: TypeId, ty: &TypeKind) -> bool {
        match ty {
            TypeKind::Variable(other_id) => {
                if *other_id == id {
                    true
                } else if let Some(bound) = self.substitutions.get(other_id) {
                    self.occurs_in(id, bound)
                } else {
                    false
                }
            }
            TypeKind::Optional(inner) => self.occurs_in(id, inner),
            TypeKind::Function(params, ret) => {
                params.iter().any(|param| self.occurs_in(id, param)) || self.occurs_in(id, ret)
            }
            TypeKind::Tuple(elements) => elements.iter().any(|elem| self.occurs_in(id, elem)),
            TypeKind::Primitive(_)
            | TypeKind::Boxed(_)
            | TypeKind::Reference(_)
            | TypeKind::Unknown => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inference::types::PrimitiveType;

    fn constraint_equal(left: TypeKind, right: TypeKind) -> Constraint {
        Constraint::new(ConstraintKind::Equal(left, right))
    }

    fn constraint_assign(id: TypeId, ty: TypeKind) -> Constraint {
        Constraint::new(ConstraintKind::Assign(id, ty))
    }

    fn collect_bindings(result: SolveResult) -> Vec<(TypeId, TypeKind)> {
        result
            .bindings
            .into_iter()
            .map(|binding| (binding.variable.id, binding.ty))
            .collect()
    }

    #[test]
    fn unifies_primitives_and_variables() {
        let mut set = ConstraintSet::new();
        set.push(constraint_equal(
            TypeKind::Variable(TypeId::new(0)),
            TypeKind::primitive(PrimitiveType::Int),
        ));

        let solver = ConstraintSolver::new();
        let result = solver.solve(set).expect("unification must succeed");

        let bindings = collect_bindings(result);
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, TypeId::new(0));
        assert_eq!(bindings[0].1, TypeKind::primitive(PrimitiveType::Int));
    }

    #[test]
    fn propagates_optional_inner_type() {
        let mut set = ConstraintSet::new();
        set.push(constraint_equal(
            TypeKind::Optional(Box::new(TypeKind::Variable(TypeId::new(1)))),
            TypeKind::reference("java.lang.String"),
        ));

        let solver = ConstraintSolver::new();
        let result = solver.solve(set).expect("unification must succeed");

        let bindings = collect_bindings(result);
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, TypeId::new(1));
        assert_eq!(bindings[0].1, TypeKind::reference("java.lang.String"));
    }

    #[test]
    fn detects_occurs_check_violation() {
        let mut set = ConstraintSet::new();
        set.push(constraint_equal(
            TypeKind::Variable(TypeId::new(2)),
            TypeKind::Optional(Box::new(TypeKind::Variable(TypeId::new(2)))),
        ));

        let solver = ConstraintSolver::new();
        let err = solver.solve(set).expect_err("occurs check must fail");

        match err {
            SolveError::OccursCheck { id, .. } => assert_eq!(id, TypeId::new(2)),
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn processes_assign_constraints() {
        let mut set = ConstraintSet::new();
        set.push(constraint_assign(
            TypeId::new(3),
            TypeKind::optional(TypeKind::primitive(PrimitiveType::Boolean)),
        ));

        let solver = ConstraintSolver::new();
        let result = solver.solve(set).expect("assignment must succeed");

        let bindings = collect_bindings(result);
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, TypeId::new(3));
        assert_eq!(
            bindings[0].1,
            TypeKind::optional(TypeKind::primitive(PrimitiveType::Boolean))
        );
    }

    #[test]
    fn solver_handles_batched_mode() {
        let mut set = ConstraintSet::new();
        set.push(constraint_assign(
            TypeId::new(4),
            TypeKind::primitive(PrimitiveType::Int),
        ));
        set.push(constraint_equal(
            TypeKind::Variable(TypeId::new(4)),
            TypeKind::primitive(PrimitiveType::Int),
        ));

        let solver = ConstraintSolver::with_config(ParallelInferenceConfig::new(false, 8, 1));
        let result = solver.solve(set).expect("batched solving should succeed");
        assert!(!collect_bindings(result).is_empty());
    }
}
