//! Diagnostic reporting and metadata carriers for the inference pipeline.
//!
//! Tasks 1 and 2 of the type-parameter-inference specification populate the
//! infrastructure required for later solver-driven diagnostics.  At this stage we
//! focus on recording constructor-related metadata so that future passes can emit
//! JV2001–JV2004 style reports with rich context.

pub mod generic;

pub use generic::{GenericDiagnostic, GenericDiagnostics, translate_solver_diagnostic};

use crate::types::SymbolId;
use jv_ast::Span;

/// Origin of a constructor-style initialization encountered by the builder layer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstructorOrigin {
    /// Classic `Type(...)` constructor invocation.
    NewExpression,
    /// Domain specific language style builder (e.g. fluent builders).
    BuilderDsl,
    /// Literal-based collection syntax such as `[a, b]` or `{ key: value }`.
    CollectionLiteral,
    /// JSON literal fed directly into inference.
    JsonLiteral,
}

/// Constructor-specific diagnostic seed captured during constraint building.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstructorDiagnosticKind {
    /// Empty initialization that might require explicit type annotation.
    EmptyInitialization {
        symbol: SymbolId,
        span: Span,
        origin: ConstructorOrigin,
    },
    /// JSON derived literal with counters for downstream schema suggestions.
    JsonLiteralShape {
        span: Span,
        object_keys: usize,
        array_entries: usize,
    },
}

/// Bag of constructor diagnostic seeds.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct ConstructorDiagnostics {
    entries: Vec<ConstructorDiagnosticKind>,
}

impl ConstructorDiagnostics {
    /// Creates an empty diagnostic bag.
    pub fn new() -> Self {
        Self::default()
    }

    /// Appends a diagnostic seed to the bag.
    pub fn push(&mut self, entry: ConstructorDiagnosticKind) {
        self.entries.push(entry);
    }

    /// Records an empty initialization sighting.
    pub fn record_empty_initialization(
        &mut self,
        symbol: SymbolId,
        span: Span,
        origin: ConstructorOrigin,
    ) {
        self.push(ConstructorDiagnosticKind::EmptyInitialization {
            symbol,
            span,
            origin,
        });
    }

    /// Records metadata extracted from a JSON literal.
    pub fn record_json_literal_shape(
        &mut self,
        span: Span,
        object_keys: usize,
        array_entries: usize,
    ) {
        self.push(ConstructorDiagnosticKind::JsonLiteralShape {
            span,
            object_keys,
            array_entries,
        });
    }

    /// Returns true when no diagnostics were recorded.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Immutable access to the stored entries.
    pub fn entries(&self) -> &[ConstructorDiagnosticKind] {
        &self.entries
    }

    /// Consumes the bag returning the underlying vector.
    pub fn into_inner(self) -> Vec<ConstructorDiagnosticKind> {
        self.entries
    }
}

impl IntoIterator for ConstructorDiagnostics {
    type Item = ConstructorDiagnosticKind;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.into_iter()
    }
}
