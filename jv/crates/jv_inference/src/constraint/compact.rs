//! Compact adjacency representation for constraint graphs.
//!
//! The [`CompactConstraintGraph`] mirrors the data stored in [`ConstraintGraph`]
//! but encodes node connectivity using an index-based compressed sparse row
//! layout together with a bit-packed edge kind buffer. The structure is optimised
//! for fast traversal with minimal heap allocations which is particularly useful
//! when diffing large graphs during incremental inference.

use super::{ConstraintGraph, EdgeKind, NodeId};
use bitvec::prelude::*;

/// Compact adjacency structure derived from [`ConstraintGraph`].
#[derive(Debug, Clone)]
pub struct CompactConstraintGraph {
    type_count: u32,
    constraint_count: u32,
    /// Row offsets into `constraint_indices`. Length is `node_count + 1`.
    pub(crate) type_indices: Vec<u32>,
    /// Target node indices matching the compressed sparse row layout.
    pub(crate) constraint_indices: Vec<u32>,
    /// Two-bit packed edge kinds stored in insertion order.
    pub(crate) compressed_edges: BitVec<u32, Lsb0>,
}

impl CompactConstraintGraph {
    /// Builds a compact representation from the provided [`ConstraintGraph`].
    pub fn from_graph(graph: &ConstraintGraph) -> Self {
        let type_count = graph.type_node_count() as u32;
        let constraint_count = graph.constraint_node_count() as u32;
        let node_count = type_count + constraint_count;

        let mut type_indices = Vec::with_capacity(node_count as usize + 1);
        let mut constraint_indices = Vec::new();
        let mut compressed_edges = BitVec::<u32, Lsb0>::new();

        type_indices.push(0);

        for raw_index in 0..node_count {
            let node = Self::index_to_node(raw_index as usize, type_count);
            let start_len = constraint_indices.len() as u32;
            for edge in graph.edges_from(node) {
                let target_index = Self::node_index(edge.to, type_count);
                constraint_indices.push(target_index as u32);
                Self::encode_edge_kind(edge.kind, &mut compressed_edges);
            }
            type_indices.push(constraint_indices.len() as u32);

            // The CSR format expects monotonic offsets; ensure we persisted the
            // original position for nodes without outgoing edges.
            if start_len == constraint_indices.len() as u32 {
                // No outgoing edges, still keep the same offset for clarity.
                continue;
            }
        }

        Self {
            type_count,
            constraint_count,
            type_indices,
            constraint_indices,
            compressed_edges,
        }
    }

    /// Total number of nodes encoded in the compact graph.
    pub fn node_count(&self) -> usize {
        (self.type_count + self.constraint_count) as usize
    }

    /// Total number of edges stored in the compact graph.
    pub fn edge_count(&self) -> usize {
        self.constraint_indices.len()
    }

    /// Returns an iterator over the outgoing neighbours for the provided node.
    pub fn neighbours(&self, node: NodeId) -> impl Iterator<Item = (NodeId, EdgeKind)> + '_ {
        let type_count = self.type_count;
        let index = Self::node_index(node, type_count) as usize;
        let start = self.type_indices[index] as usize;
        let end = self.type_indices[index + 1] as usize;

        (start..end).map(move |edge_idx| {
            let target_raw = self.constraint_indices[edge_idx] as usize;
            let target = Self::index_to_node(target_raw, type_count);
            let kind = self.edge_kind_at(edge_idx);
            (target, kind)
        })
    }

    /// Retrieves the edge kind stored at the provided edge index.
    pub fn edge_kind_at(&self, edge_index: usize) -> EdgeKind {
        let bit_index = edge_index * 2;
        let first = *self
            .compressed_edges
            .get(bit_index)
            .expect("valid edge kind lower bit");
        let second = *self
            .compressed_edges
            .get(bit_index + 1)
            .expect("valid edge kind upper bit");
        Self::decode_edge_kind(first, second)
    }

    fn node_index(node: NodeId, type_count: u32) -> usize {
        match node {
            NodeId::Type(id) => id.to_raw(),
            NodeId::Constraint(id) => type_count as usize + id.to_raw(),
        }
    }

    fn index_to_node(index: usize, type_count: u32) -> NodeId {
        if index < type_count as usize {
            NodeId::Type(super::graph::TypeNodeId::new(index))
        } else {
            NodeId::Constraint(super::graph::ConstraintNodeId::new(
                index - type_count as usize,
            ))
        }
    }

    fn encode_edge_kind(kind: EdgeKind, buffer: &mut BitVec<u32, Lsb0>) {
        match kind {
            EdgeKind::Equality => {
                buffer.push(false);
                buffer.push(false);
            }
            EdgeKind::Assignment => {
                buffer.push(true);
                buffer.push(false);
            }
            EdgeKind::BoundSatisfaction => {
                buffer.push(false);
                buffer.push(true);
            }
        }
    }

    fn decode_edge_kind(lower: bool, upper: bool) -> EdgeKind {
        match (lower, upper) {
            (false, false) => EdgeKind::Equality,
            (true, false) => EdgeKind::Assignment,
            (false, true) => EdgeKind::BoundSatisfaction,
            (true, true) => EdgeKind::BoundSatisfaction,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constraint::{
        ConstraintKind, ConstraintNode, ConstraintNodeId, TypeNode, TypeNodeId,
    };
    use crate::types::{TypeId, TypeKind};

    fn build_sample_graph() -> ConstraintGraph {
        let mut graph = ConstraintGraph::new();
        let t1 = graph.add_type_node(TypeNode::new(TypeId::new(0), TypeKind::default()));
        let t2 = graph.add_type_node(TypeNode::new(TypeId::new(1), TypeKind::default()));
        let c = graph.add_constraint_node(ConstraintNode::new(ConstraintKind::Equality {
            left: TypeId::new(0),
            right: TypeId::new(1),
        }));

        graph.add_edge(NodeId::Type(t1), NodeId::Constraint(c), EdgeKind::Equality);
        graph.add_edge(
            NodeId::Constraint(c),
            NodeId::Type(t2),
            EdgeKind::Assignment,
        );
        graph.add_edge(
            NodeId::Type(t2),
            NodeId::Type(t1),
            EdgeKind::BoundSatisfaction,
        );

        graph
    }

    #[test]
    fn builds_compact_representation() {
        let graph = build_sample_graph();
        let compact = CompactConstraintGraph::from_graph(&graph);

        assert_eq!(compact.node_count(), 3);
        assert_eq!(compact.edge_count(), 3);

        let t1 = NodeId::Type(TypeNodeId::new(0));
        let mut neighbours: Vec<_> = compact.neighbours(t1).collect();
        assert_eq!(neighbours.len(), 1);
        assert_eq!(neighbours[0].1, EdgeKind::Equality);

        let constraint = NodeId::Constraint(ConstraintNodeId::new(0));
        neighbours = compact.neighbours(constraint).collect();
        assert_eq!(neighbours.len(), 1);
        assert_eq!(neighbours[0].1, EdgeKind::Assignment);

        let t2 = NodeId::Type(TypeNodeId::new(1));
        neighbours = compact.neighbours(t2).collect();
        assert_eq!(neighbours.len(), 1);
        assert_eq!(neighbours[0].1, EdgeKind::BoundSatisfaction);
    }

    #[test]
    fn edge_kind_round_trip() {
        let graph = build_sample_graph();
        let compact = CompactConstraintGraph::from_graph(&graph);

        let mut kinds: Vec<u8> = (0..compact.edge_count())
            .map(|idx| match compact.edge_kind_at(idx) {
                EdgeKind::Equality => 0,
                EdgeKind::Assignment => 1,
                EdgeKind::BoundSatisfaction => 2,
            })
            .collect();
        kinds.sort_unstable();

        assert_eq!(kinds, vec![0, 1, 2]);
    }
}
