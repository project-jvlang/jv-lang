use crate::types::{SymbolId, TypeId, TypeKind};
use jv_ast::Span;

/// Categories of generic constraints emitted by builders prior to solving.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenericConstraintKind {
    /// Associates a concrete argument type with a callee type parameter.
    TypeArgument {
        callee: SymbolId,
        parameter: TypeId,
        argument: TypeKind,
        argument_index: usize,
    },
}

/// Lightweight constraint record produced by builders such as [`CallConstraintBuilder`].
#[derive(Debug, Clone, PartialEq)]
pub struct GenericConstraint {
    pub kind: GenericConstraintKind,
    pub span: Span,
}

impl GenericConstraint {
    /// Creates a new generic constraint entry.
    pub fn new(kind: GenericConstraintKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Returns the stored span.
    pub fn span(&self) -> &Span {
        &self.span
    }
}
