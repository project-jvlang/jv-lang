//! Incremental constraint maintenance utilities.
//!
//! The incremental builder tracks AST nodes affected by edits and produces a
//! [`ConstraintDiff`] describing additions, removals, and metadata updates that
//! should be applied to an existing [`ConstraintGraph`]. It also records
//! preservation metrics so that higher layers can surface telemetry about how
//! much work was avoided during a hot reload.

#[cfg(test)]
use super::EdgeKind;
use super::{ConstraintGraph, Edge, NodeId, SourceSpanTable};
use jv_ast::Span;
use std::collections::HashSet;

/// Temporary identifier representing an AST node. The dedicated `AstId` type
/// will be introduced once the parser exports stable identifiers.
pub type AstId = u32;

/// Set of changes that must be applied to reconcile a constraint graph with the
/// latest AST snapshot.
#[derive(Debug, Default, Clone)]
pub struct ConstraintDiff {
    pub added_edges: Vec<Edge>,
    pub removed_edges: Vec<Edge>,
    pub updated_spans: Vec<(NodeId, Span)>,
}

impl ConstraintDiff {
    pub fn is_empty(&self) -> bool {
        self.added_edges.is_empty()
            && self.removed_edges.is_empty()
            && self.updated_spans.is_empty()
    }

    pub fn record_edge_addition(&mut self, edge: Edge) {
        self.added_edges.push(edge);
    }

    pub fn record_edge_removal(&mut self, edge: Edge) {
        self.removed_edges.push(edge);
    }

    pub fn record_span_update(&mut self, node: NodeId, span: Span) {
        self.updated_spans.push((node, span));
    }
}

/// Snapshot of telemetry counters collected while processing incremental
/// updates. Downstream systems can forward these values to logging or metrics
/// backends.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct IncrementalTelemetry {
    pub dirty_nodes: usize,
    pub preserved_constraints: usize,
}

/// Tracks AST deltas and produces constraint diffs for incremental inference.
#[derive(Debug, Default)]
pub struct IncrementalConstraintBuilder {
    dirty_nodes: HashSet<AstId>,
    constraint_diff: ConstraintDiff,
    preserved_constraints: usize,
}

impl IncrementalConstraintBuilder {
    /// Creates a fresh builder with no pending updates.
    pub fn new() -> Self {
        Self::default()
    }

    /// Marks an AST node as dirty. Returns `true` when it was newly inserted.
    pub fn mark_dirty(&mut self, id: AstId) -> bool {
        self.dirty_nodes.insert(id)
    }

    /// Extends the dirty set with the provided iterator of node identifiers.
    pub fn extend_dirty<I>(&mut self, ids: I)
    where
        I: IntoIterator<Item = AstId>,
    {
        for id in ids {
            self.mark_dirty(id);
        }
    }

    /// Returns a view over the currently tracked dirty nodes.
    pub fn dirty_nodes(&self) -> impl Iterator<Item = &AstId> {
        self.dirty_nodes.iter()
    }

    /// Number of dirty nodes recorded so far.
    pub fn dirty_count(&self) -> usize {
        self.dirty_nodes.len()
    }

    /// Records that an existing constraint could be reused without
    /// reconstruction.
    pub fn record_preserved_constraint(&mut self) {
        self.preserved_constraints += 1;
    }

    /// Adds an edge that should be inserted into the constraint graph.
    pub fn add_edge(&mut self, edge: Edge) {
        self.constraint_diff.record_edge_addition(edge);
    }

    /// Adds an edge that should be removed from the constraint graph.
    pub fn remove_edge(&mut self, edge: Edge) {
        self.constraint_diff.record_edge_removal(edge);
    }

    /// Marks that the span for the specified node changed.
    pub fn update_span(&mut self, node: NodeId, span: Span) {
        self.constraint_diff.record_span_update(node, span);
    }

    /// Produces a shallow copy of the current diff.
    pub fn diff(&self) -> ConstraintDiff {
        self.constraint_diff.clone()
    }

    /// Consumes the diff and resets the internal buffer.
    pub fn take_diff(&mut self) -> ConstraintDiff {
        std::mem::take(&mut self.constraint_diff)
    }

    /// Clears dirty tracking while keeping cached telemetry counters.
    pub fn clear_dirty(&mut self) {
        self.dirty_nodes.clear();
    }

    /// Resets both dirty nodes and diff buffers.
    pub fn reset(&mut self) {
        self.dirty_nodes.clear();
        self.constraint_diff = ConstraintDiff::default();
        self.preserved_constraints = 0;
    }

    /// Returns current telemetry snapshot.
    pub fn telemetry(&self) -> IncrementalTelemetry {
        IncrementalTelemetry {
            dirty_nodes: self.dirty_nodes.len(),
            preserved_constraints: self.preserved_constraints,
        }
    }

    /// Applies recorded span updates directly to the provided graph metadata.
    /// Edge additions/removals must be handled by the caller according to their
    /// own locking discipline.
    pub fn apply_metadata_updates(&mut self, metadata: &mut SourceSpanTable) {
        for (node, span) in self.constraint_diff.updated_spans.iter().cloned() {
            metadata.insert(node, span);
        }
    }

    /// Applies metadata updates and then clears the diff span records.
    pub fn flush_metadata(&mut self, metadata: &mut SourceSpanTable) {
        self.apply_metadata_updates(metadata);
        self.constraint_diff.updated_spans.clear();
    }

    /// Convenience helper that rewrites metadata on the provided constraint
    /// graph using the pending span updates. Edge operations are intentionally
    /// left for higher layers because they may require additional bookkeeping
    /// (e.g. SCC cache invalidation).
    pub fn flush_into_graph(&mut self, graph: &mut ConstraintGraph) {
        let metadata = graph.metadata_mut();
        self.flush_metadata(metadata);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constraint::{ConstraintKind, ConstraintNode, TypeNode};
    use crate::types::{TypeId, TypeKind};

    fn sample_edge() -> Edge {
        let mut graph = ConstraintGraph::new();
        let t1 = graph.add_type_node(TypeNode::new(TypeId::new(0), TypeKind::default()));
        let _t2 = graph.add_type_node(TypeNode::new(TypeId::new(1), TypeKind::default()));
        let constraint = graph.add_constraint_node(ConstraintNode::new(ConstraintKind::Equality {
            left: TypeId::new(0),
            right: TypeId::new(1),
        }));
        Edge::new(
            NodeId::Type(t1),
            NodeId::Constraint(constraint),
            EdgeKind::Equality,
        )
    }

    #[test]
    fn mark_dirty_is_idempotent() {
        let mut builder = IncrementalConstraintBuilder::new();
        assert!(builder.mark_dirty(42));
        assert!(!builder.mark_dirty(42));
        assert_eq!(builder.dirty_count(), 1);
    }

    #[test]
    fn records_constraint_diff_changes() {
        let mut builder = IncrementalConstraintBuilder::new();
        let edge = sample_edge();
        builder.add_edge(edge);
        builder.remove_edge(edge);
        builder.update_span(edge.from, Span::new(1, 0, 1, 5));

        let diff = builder.diff();
        assert_eq!(diff.added_edges.len(), 1);
        assert_eq!(diff.removed_edges.len(), 1);
        assert_eq!(diff.updated_spans.len(), 1);
    }

    #[test]
    fn telemetry_tracks_preserved_constraints() {
        let mut builder = IncrementalConstraintBuilder::new();
        builder.mark_dirty(1);
        builder.record_preserved_constraint();
        builder.record_preserved_constraint();
        let telemetry = builder.telemetry();
        assert_eq!(telemetry.dirty_nodes, 1);
        assert_eq!(telemetry.preserved_constraints, 2);
    }

    #[test]
    fn flush_metadata_writes_to_graph() {
        let mut builder = IncrementalConstraintBuilder::new();
        let mut graph = ConstraintGraph::new();
        let t1 = graph.add_type_node(TypeNode::new(TypeId::new(0), TypeKind::default()));
        let span = Span::new(10, 2, 10, 8);
        builder.update_span(NodeId::Type(t1), span.clone());
        builder.flush_into_graph(&mut graph);

        let stored = graph
            .metadata()
            .get(&NodeId::Type(t1))
            .expect("span must exist");
        assert_eq!(stored.start_line, span.start_line);
    }

    #[test]
    fn take_diff_clears_pending_changes() {
        let mut builder = IncrementalConstraintBuilder::new();
        let edge = sample_edge();
        builder.add_edge(edge);
        assert!(!builder.take_diff().is_empty());
        assert!(builder.take_diff().is_empty());
    }
}
