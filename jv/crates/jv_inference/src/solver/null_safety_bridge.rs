use crate::constraint::WhereConstraintSummary;
use crate::types::{NullabilityFlag, TypeId};
use jv_ast::Span;
use std::collections::HashMap;

/// Collects nullability hints derived during constraint processing so that they
/// can be forwarded to flow analysis.
#[derive(Debug, Default, Clone)]
pub struct NullSafetyBridge {
    nullable_parameters: HashMap<TypeId, Span>,
}

impl NullSafetyBridge {
    /// Creates an empty bridge without any recorded hints.
    pub fn new() -> Self {
        Self {
            nullable_parameters: HashMap::new(),
        }
    }

    /// Records that the provided type parameter should be treated as nullable.
    /// The first span wins so that diagnostics point to the earliest cause.
    pub fn register_nullable_bound(&mut self, parameter: TypeId, span: Span) {
        self.nullable_parameters.entry(parameter).or_insert(span);
    }

    /// Integrates nullable parameter information derived from where-clause constraints.
    pub fn absorb_where_summary(&mut self, summary: &WhereConstraintSummary) {
        for (parameter, span) in summary.nullable_parameters() {
            self.register_nullable_bound(parameter, span.clone());
        }
    }

    /// Returns the number of nullable parameters recorded.
    pub fn len(&self) -> usize {
        self.nullable_parameters.len()
    }

    /// Returns true when no nullable parameters have been recorded.
    pub fn is_empty(&self) -> bool {
        self.nullable_parameters.is_empty()
    }

    /// Iterates over recorded nullable parameters.
    pub fn nullable_parameters(&self) -> impl Iterator<Item = (TypeId, &Span)> {
        self.nullable_parameters
            .iter()
            .map(|(id, span)| (*id, span))
    }

    /// Converts the recorded hints into a map of nullability overrides that can
    /// be consumed by downstream services.
    pub fn as_overrides(&self) -> HashMap<TypeId, NullabilityFlag> {
        self.nullable_parameters
            .keys()
            .copied()
            .map(|id| (id, NullabilityFlag::Nullable))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn records_nullable_parameters_without_duplicates() {
        let mut bridge = NullSafetyBridge::new();
        bridge.register_nullable_bound(TypeId::new(1), Span::dummy());
        bridge.register_nullable_bound(TypeId::new(1), Span::new(2, 0, 2, 5));
        bridge.register_nullable_bound(TypeId::new(2), Span::dummy());

        assert_eq!(bridge.len(), 2);
        let overrides = bridge.as_overrides();
        assert_eq!(overrides.len(), 2);
        assert_eq!(overrides[&TypeId::new(1)], NullabilityFlag::Nullable);
    }

    #[test]
    fn iterator_exposes_spans() {
        let mut bridge = NullSafetyBridge::new();
        let span = Span::new(3, 1, 3, 4);
        bridge.register_nullable_bound(TypeId::new(7), span.clone());
        let collected: Vec<_> = bridge.nullable_parameters().collect();
        assert_eq!(collected.len(), 1);
        assert_eq!(collected[0].0, TypeId::new(7));
        assert_eq!(collected[0].1.start_line, 3);
    }

    #[test]
    fn absorbs_where_summary_records_nullable_parameters() {
        use crate::constraint::{ConstraintGraph, WhereConstraintResolver};
        use crate::types::SymbolId;
        use jv_ast::types::{QualifiedName, TypeAnnotation, WhereClause, WherePredicate};
        use std::collections::HashMap;

        let mut params = HashMap::new();
        params.insert("T".to_string(), TypeId::new(5));

        let clause = WhereClause {
            predicates: vec![WherePredicate::TraitBound {
                type_param: "T".into(),
                trait_name: QualifiedName::new(vec!["Comparable".into()], Span::dummy()),
                type_args: vec![TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple(
                    "T".into(),
                )))],
                span: Span::dummy(),
            }],
            primitive_bounds: Vec::new(),
            span: Span::dummy(),
        };

        let resolver = WhereConstraintResolver::new(SymbolId::from("pkg::Owner"), &params);
        let constraints = resolver.from_clause(&clause);
        let mut graph = ConstraintGraph::new();
        let summary = graph.add_where_constraints(&constraints);

        let mut bridge = NullSafetyBridge::new();
        bridge.absorb_where_summary(&summary);
        let collected: Vec<_> = bridge.nullable_parameters().collect();
        assert_eq!(collected.len(), 1);
        assert_eq!(collected[0].0, TypeId::new(5));
    }
}
