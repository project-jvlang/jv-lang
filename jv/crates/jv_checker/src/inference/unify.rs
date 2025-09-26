//! 単一化ソルバと制約解決ロジックを提供する。
//!
//! `ConstraintSolver` は制約キューを反復的に消化し、Robinson 型単一化を用いて
//! 型変数への束縛を決定する。Optional 型は内包する型を再帰的に単一化することで
//! 伝播させ、Null 安全と一致する解を導出する。

use crate::inference::constraint::{Constraint, ConstraintKind, ConstraintSet};
use crate::inference::types::{TypeBinding, TypeId, TypeKind, TypeVariable, TypeVariableKind};
use std::collections::HashMap;
use std::fmt;

/// 制約解決におけるエラー。
#[derive(Debug, PartialEq)]
pub enum SolveError {
    Placeholder {
        placeholder: &'static str,
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

impl SolveError {
    fn note_suffix(&self) -> String {
        match self {
            SolveError::TypeMismatch { note, .. } | SolveError::OccursCheck { note, .. } => note
                .as_ref()
                .map(|n| format!(" ({})", n))
                .unwrap_or_default(),
            _ => String::new(),
        }
    }
}

impl fmt::Display for SolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SolveError::Placeholder { placeholder } => {
                write!(f, "encountered placeholder constraint: {placeholder}")
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
        }
    }
}

impl std::error::Error for SolveError {}

/// 制約集合を解いた結果。
#[derive(Debug, Default)]
pub struct SolveResult {
    pub bindings: Vec<TypeBinding>,
    pub remaining: ConstraintSet,
}

/// 単一化ソルバ本体。
#[derive(Debug, Default)]
pub struct ConstraintSolver {
    substitutions: HashMap<TypeId, TypeKind>,
}

impl ConstraintSolver {
    /// 新しいソルバを構築する。
    pub fn new() -> Self {
        Self {
            substitutions: HashMap::new(),
        }
    }

    /// 制約集合を解決し、型束縛を返す。
    pub fn solve(mut self, mut constraints: ConstraintSet) -> Result<SolveResult, SolveError> {
        while let Some(constraint) = constraints.pop() {
            self.process_constraint(&constraint)?;
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
        })
    }

    fn process_constraint(&mut self, constraint: &Constraint) -> Result<(), SolveError> {
        match &constraint.kind {
            ConstraintKind::Equal(left, right) => {
                self.unify(left.clone(), right.clone(), constraint.note.clone())?;
            }
            ConstraintKind::Assign(id, ty) => {
                self.unify(TypeKind::Variable(*id), ty.clone(), constraint.note.clone())?;
            }
            ConstraintKind::Placeholder(placeholder) => {
                return Err(SolveError::Placeholder {
                    placeholder: *placeholder,
                });
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
            TypeKind::Primitive(_) | TypeKind::Unknown => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            TypeKind::Primitive("Int"),
        ));

        let solver = ConstraintSolver::new();
        let result = solver.solve(set).expect("unification must succeed");

        let bindings = collect_bindings(result);
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, TypeId::new(0));
        assert_eq!(bindings[0].1, TypeKind::Primitive("Int"));
    }

    #[test]
    fn propagates_optional_inner_type() {
        let mut set = ConstraintSet::new();
        set.push(constraint_equal(
            TypeKind::Optional(Box::new(TypeKind::Variable(TypeId::new(1)))),
            TypeKind::Primitive("String"),
        ));

        let solver = ConstraintSolver::new();
        let result = solver.solve(set).expect("unification must succeed");

        let bindings = collect_bindings(result);
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, TypeId::new(1));
        assert_eq!(bindings[0].1, TypeKind::Primitive("String"));
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
            TypeKind::Optional(Box::new(TypeKind::Primitive("Boolean"))),
        ));

        let solver = ConstraintSolver::new();
        let result = solver.solve(set).expect("assignment must succeed");

        let bindings = collect_bindings(result);
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, TypeId::new(3));
        assert_eq!(
            bindings[0].1,
            TypeKind::Optional(Box::new(TypeKind::Primitive("Boolean")))
        );
    }
}
