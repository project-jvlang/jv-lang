//! Constraint graph representation and Tarjan SCC traversal utilities.
//!
//! The graph models both type-level nodes and constraint nodes. Each edge preserves
//! the original constraint semantics via [`EdgeKind`], and source spans are attached
//! through [`SourceSpanTable`] for high quality diagnostics.

use super::nullable::{predicate_requires_nullable, referenced_type_parameters};
use super::{GenericConstraint, GenericConstraintKind};
use crate::types::{TypeId, TypeKind};
use jv_ast::Span;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

/// Summary describing how where-clause constraints affected the constraint graph.
#[derive(Debug, Default, Clone)]
pub struct WhereConstraintSummary {
    recorded_constraints: usize,
    nullable_parameters: HashMap<TypeId, Span>,
}

impl WhereConstraintSummary {
    /// Returns the number of constraint records processed.
    pub fn recorded_constraints(&self) -> usize {
        self.recorded_constraints
    }

    /// Returns an iterator over nullable type parameters together with the span
    /// that introduced the nullability.
    pub fn nullable_parameters(&self) -> impl Iterator<Item = (TypeId, &Span)> {
        self.nullable_parameters
            .iter()
            .map(|(id, span)| (*id, span))
    }

    fn register_nullable(&mut self, parameter: TypeId, span: Span) {
        self.nullable_parameters.entry(parameter).or_insert(span);
    }

    fn increment(&mut self) {
        self.recorded_constraints += 1;
    }
}

/// Identifier assigned to type nodes within the constraint graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeNodeId(u32);

impl TypeNodeId {
    pub(crate) fn new(raw: usize) -> Self {
        Self(raw as u32)
    }

    /// Returns the raw index associated with the node.
    pub fn to_raw(self) -> usize {
        self.0 as usize
    }
}

/// Identifier assigned to constraint nodes within the graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ConstraintNodeId(u32);

impl ConstraintNodeId {
    pub(crate) fn new(raw: usize) -> Self {
        Self(raw as u32)
    }

    /// Returns the raw index associated with the node.
    pub fn to_raw(self) -> usize {
        self.0 as usize
    }
}

/// Unified node identifier used when traversing the graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeId {
    Type(TypeNodeId),
    Constraint(ConstraintNodeId),
}

/// Record describing a type node and the underlying inferred type.
#[derive(Debug, Clone)]
pub struct TypeNode {
    pub type_id: TypeId,
    pub ty: TypeKind,
}

impl TypeNode {
    pub fn new(type_id: TypeId, ty: TypeKind) -> Self {
        Self { type_id, ty }
    }
}

/// Categories of constraint nodes handled by the graph.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintKind {
    Equality { left: TypeId, right: TypeId },
    Assignment { from: TypeId, to: TypeId },
    BoundSatisfaction { subject: TypeId, bound: String },
    Custom(String),
}

/// Constraint node payload containing the high-level constraint semantics.
#[derive(Debug, Clone)]
pub struct ConstraintNode {
    pub kind: ConstraintKind,
}

impl ConstraintNode {
    pub fn new(kind: ConstraintKind) -> Self {
        Self { kind }
    }
}

/// Edge categories tracked in the constraint graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EdgeKind {
    Equality,
    Assignment,
    BoundSatisfaction,
}

/// Edge connecting two nodes in the constraint graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Edge {
    pub from: NodeId,
    pub to: NodeId,
    pub kind: EdgeKind,
}

impl Edge {
    pub fn new(from: NodeId, to: NodeId, kind: EdgeKind) -> Self {
        Self { from, to, kind }
    }
}

/// Table mapping nodes to their source spans for diagnostics.
#[derive(Debug, Clone, Default)]
pub struct SourceSpanTable {
    spans: HashMap<NodeId, Span>,
}

impl SourceSpanTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, node: NodeId, span: Span) {
        self.spans.insert(node, span);
    }

    pub fn get(&self, node: &NodeId) -> Option<&Span> {
        self.spans.get(node)
    }
}

/// Cache containing SCC information computed via Tarjan's algorithm.
#[derive(Debug, Clone)]
pub struct SccCache {
    components: Vec<Vec<NodeId>>,
    component_lookup: HashMap<NodeId, usize>,
}

impl SccCache {
    fn new(components: Vec<Vec<NodeId>>) -> Self {
        let mut component_lookup = HashMap::new();
        for (idx, component) in components.iter().enumerate() {
            for node in component {
                component_lookup.insert(*node, idx);
            }
        }
        Self {
            components,
            component_lookup,
        }
    }

    /// Total number of strongly connected components cached.
    pub fn len(&self) -> usize {
        self.components.len()
    }

    /// Returns true when no components have been recorded.
    pub fn is_empty(&self) -> bool {
        self.components.is_empty()
    }

    /// Returns the component containing the specified node.
    pub fn component_for(&self, node: NodeId) -> Option<&[NodeId]> {
        self.component_lookup
            .get(&node)
            .and_then(|idx| self.components.get(*idx))
            .map(|component| component.as_slice())
    }

    /// Returns an iterator over all components.
    pub fn components(&self) -> impl Iterator<Item = &[NodeId]> {
        self.components.iter().map(|component| component.as_slice())
    }
}

/// Constraint graph storing nodes, edges, and SCC metadata.
#[derive(Debug)]
pub struct ConstraintGraph {
    type_nodes: Vec<TypeNode>,
    constraint_nodes: Vec<ConstraintNode>,
    edges: Vec<Edge>,
    metadata: SourceSpanTable,
    scc_cache: RefCell<Option<SccCache>>,
    constraint_index: HashMap<(TypeId, String), ConstraintNodeId>,
    edge_index: HashSet<(NodeId, NodeId, EdgeKind)>,
}

impl ConstraintGraph {
    /// Creates an empty constraint graph.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the number of type nodes present in the graph.
    pub fn type_node_count(&self) -> usize {
        self.type_nodes.len()
    }

    /// Returns the number of constraint nodes present in the graph.
    pub fn constraint_node_count(&self) -> usize {
        self.constraint_nodes.len()
    }

    /// Adds a new type node to the graph.
    pub fn add_type_node(&mut self, node: TypeNode) -> TypeNodeId {
        self.type_nodes.push(node);
        self.invalidate_scc_cache();
        TypeNodeId::new(self.type_nodes.len() - 1)
    }

    /// Returns the node identifier for an existing type, if any.
    pub fn find_type_node(&self, type_id: TypeId) -> Option<TypeNodeId> {
        self.type_nodes
            .iter()
            .position(|node| node.type_id == type_id)
            .map(TypeNodeId::new)
    }

    /// Ensures a type node exists and optionally enriches it with a more
    /// specific [`TypeKind`].
    pub fn ensure_type_node(&mut self, type_id: TypeId, hint: Option<TypeKind>) -> TypeNodeId {
        if let Some(index) = self
            .type_nodes
            .iter()
            .position(|node| node.type_id == type_id)
        {
            if let Some(hint) = hint {
                let entry = &mut self.type_nodes[index];
                let needs_update = entry.ty.contains_unknown() && !hint.contains_unknown();
                if needs_update {
                    entry.ty = hint;
                }
            }
            return TypeNodeId::new(index);
        }

        let ty = hint.unwrap_or_default();
        self.add_type_node(TypeNode::new(type_id, ty))
    }

    /// Adds a new constraint node to the graph.
    pub fn add_constraint_node(&mut self, node: ConstraintNode) -> ConstraintNodeId {
        self.constraint_nodes.push(node);
        self.invalidate_scc_cache();
        ConstraintNodeId::new(self.constraint_nodes.len() - 1)
    }

    /// Records an edge between two nodes.
    pub fn add_edge(&mut self, from: NodeId, to: NodeId, kind: EdgeKind) {
        if self.edge_index.insert((from, to, kind)) {
            self.edges.push(Edge::new(from, to, kind));
            self.invalidate_scc_cache();
        }
    }

    /// Returns true when an edge with the given kind connects the specified nodes.
    pub fn has_edge_with_kind(&self, from: NodeId, to: NodeId, kind: EdgeKind) -> bool {
        self.edge_index.contains(&(from, to, kind))
    }

    /// Returns true when any edge connects the specified nodes irrespective of kind.
    pub fn has_edge_between(&self, from: NodeId, to: NodeId) -> bool {
        self.edge_index
            .iter()
            .any(|(edge_from, edge_to, _)| *edge_from == from && *edge_to == to)
    }

    /// Registers generic bound requirements into the graph and returns a summary that
    /// can be used by higher layers (e.g. diagnostics, null safety bridge).
    pub fn add_where_constraints(
        &mut self,
        constraints: &[GenericConstraint],
    ) -> WhereConstraintSummary {
        let mut summary = WhereConstraintSummary::default();

        for constraint in constraints {
            if let GenericConstraintKind::BoundRequirement {
                parameter,
                predicate,
                ..
            } = &constraint.kind
            {
                summary.increment();
                let type_node = self.ensure_type_node(*parameter, None);
                self.metadata
                    .insert(NodeId::Type(type_node), constraint.span.clone());

                let predicate_key = predicate.key();
                let constraint_node_id = match self
                    .constraint_index
                    .get(&(*parameter, predicate_key.clone()))
                {
                    Some(&id) => id,
                    None => {
                        let node = ConstraintNode::new(ConstraintKind::BoundSatisfaction {
                            subject: *parameter,
                            bound: predicate.describe(),
                        });
                        let id = self.add_constraint_node(node);
                        self.metadata
                            .insert(NodeId::Constraint(id), constraint.span.clone());
                        self.constraint_index
                            .insert((*parameter, predicate_key), id);
                        id
                    }
                };

                self.add_edge(
                    NodeId::Type(type_node),
                    NodeId::Constraint(constraint_node_id),
                    EdgeKind::BoundSatisfaction,
                );

                let mut referenced = Vec::new();
                referenced_type_parameters(predicate, &mut referenced);
                for referenced_id in referenced {
                    let referenced_node = self.ensure_type_node(referenced_id, None);
                    self.metadata
                        .insert(NodeId::Type(referenced_node), constraint.span.clone());
                    self.add_edge(
                        NodeId::Constraint(constraint_node_id),
                        NodeId::Type(referenced_node),
                        EdgeKind::BoundSatisfaction,
                    );
                }

                if predicate_requires_nullable(predicate, *parameter) {
                    summary.register_nullable(*parameter, constraint.span.clone());
                }
            }
        }

        summary
    }

    /// Returns all edges originating from the provided node.
    pub fn edges_from<'graph>(&'graph self, node: NodeId) -> impl Iterator<Item = Edge> + 'graph {
        self.edges
            .iter()
            .copied()
            .filter(move |edge| edge.from == node)
    }

    /// Returns a reference to the stored source metadata.
    pub fn metadata(&self) -> &SourceSpanTable {
        &self.metadata
    }

    /// Returns a mutable reference to the stored source metadata.
    pub fn metadata_mut(&mut self) -> &mut SourceSpanTable {
        &mut self.metadata
    }

    /// Returns the cached SCC data, computing it if necessary.
    pub fn strongly_connected_components(&self) -> SccCache {
        if let Some(cache) = self.scc_cache.borrow().clone() {
            return cache;
        }

        let cache = self.compute_scc();
        *self.scc_cache.borrow_mut() = Some(cache.clone());
        cache
    }

    /// Builds a simple adjacency list for all nodes.
    fn adjacency_map(&self) -> HashMap<NodeId, Vec<NodeId>> {
        let mut adjacency: HashMap<NodeId, Vec<NodeId>> = HashMap::new();
        for edge in &self.edges {
            adjacency.entry(edge.from).or_default().push(edge.to);
        }
        adjacency
    }

    fn compute_scc(&self) -> SccCache {
        let adjacency = self.adjacency_map();
        let mut state = TarjanState::new(adjacency);
        for node in self.all_nodes() {
            if !state.has_index(node) {
                state.strong_connect(node);
            }
        }
        SccCache::new(state.into_components())
    }

    fn all_nodes(&self) -> Vec<NodeId> {
        let mut nodes = Vec::with_capacity(self.type_nodes.len() + self.constraint_nodes.len());
        for idx in 0..self.type_nodes.len() {
            nodes.push(NodeId::Type(TypeNodeId::new(idx)));
        }
        for idx in 0..self.constraint_nodes.len() {
            nodes.push(NodeId::Constraint(ConstraintNodeId::new(idx)));
        }
        nodes
    }

    fn invalidate_scc_cache(&mut self) {
        self.scc_cache.borrow_mut().take();
    }

    /// Returns strongly connected components that represent cycles in the graph.
    pub fn circular_components(&self) -> Vec<Vec<NodeId>> {
        let cache = self.strongly_connected_components();
        let mut components = Vec::new();

        for component in cache.components() {
            if component.len() > 1 {
                let mut nodes = component.to_vec();
                nodes.sort_by_key(order_key);
                components.push(nodes);
            } else if let Some(&node) = component.first() {
                if self.has_edge_between(node, node) {
                    components.push(vec![node]);
                }
            }
        }

        components.sort_by(|left, right| order_key(&left[0]).cmp(&order_key(&right[0])));
        components
    }
}

impl Default for ConstraintGraph {
    fn default() -> Self {
        Self {
            type_nodes: Vec::new(),
            constraint_nodes: Vec::new(),
            edges: Vec::new(),
            metadata: SourceSpanTable::new(),
            scc_cache: RefCell::new(None),
            constraint_index: HashMap::new(),
            edge_index: HashSet::new(),
        }
    }
}

fn order_key(node: &NodeId) -> (u8, usize) {
    match node {
        NodeId::Type(id) => (0, id.to_raw()),
        NodeId::Constraint(id) => (1, id.to_raw()),
    }
}

struct TarjanState {
    next_index: usize,
    stack: Vec<NodeId>,
    on_stack: HashSet<NodeId>,
    indices: HashMap<NodeId, usize>,
    lowlinks: HashMap<NodeId, usize>,
    components: Vec<Vec<NodeId>>,
    adjacency: HashMap<NodeId, Vec<NodeId>>,
}

impl TarjanState {
    fn new(adjacency: HashMap<NodeId, Vec<NodeId>>) -> Self {
        Self {
            next_index: 0,
            stack: Vec::new(),
            on_stack: HashSet::new(),
            indices: HashMap::new(),
            lowlinks: HashMap::new(),
            components: Vec::new(),
            adjacency,
        }
    }

    fn has_index(&self, node: NodeId) -> bool {
        self.indices.contains_key(&node)
    }

    fn strong_connect(&mut self, node: NodeId) {
        self.indices.insert(node, self.next_index);
        self.lowlinks.insert(node, self.next_index);
        self.next_index += 1;

        self.stack.push(node);
        self.on_stack.insert(node);

        let neighbours = self.adjacency.get(&node).cloned().unwrap_or_default();
        for target in neighbours {
            if !self.has_index(target) {
                self.strong_connect(target);
                self.update_lowlink(node, target);
            } else if self.on_stack.contains(&target) {
                self.update_lowlink_index(node, target);
            }
        }

        if self.index_of(node) == self.lowlink_of(node) {
            let mut component = Vec::new();
            loop {
                let popped = self.stack.pop().expect("stack must contain node");
                self.on_stack.remove(&popped);
                component.push(popped);
                if popped == node {
                    break;
                }
            }
            self.components.push(component);
        }
    }

    fn update_lowlink(&mut self, node: NodeId, neighbour: NodeId) {
        let neighbour_lowlink = self.lowlink_of(neighbour);
        let entry = self
            .lowlinks
            .get_mut(&node)
            .expect("node must have lowlink");
        *entry = (*entry).min(neighbour_lowlink);
    }

    fn update_lowlink_index(&mut self, node: NodeId, neighbour: NodeId) {
        let neighbour_index = self.index_of(neighbour);
        let entry = self
            .lowlinks
            .get_mut(&node)
            .expect("node must have lowlink");
        *entry = (*entry).min(neighbour_index);
    }

    fn index_of(&self, node: NodeId) -> usize {
        self.indices[&node]
    }

    fn lowlink_of(&self, node: NodeId) -> usize {
        self.lowlinks[&node]
    }

    fn into_components(self) -> Vec<Vec<NodeId>> {
        self.components
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constraint::WhereConstraintResolver;
    use crate::types::SymbolId;
    use jv_ast::types::{QualifiedName, TypeAnnotation, WhereClause, WherePredicate};
    use std::collections::HashMap;

    fn dummy_span(line: usize) -> Span {
        Span::new(line, 0, line, 10)
    }

    #[test]
    fn scc_detects_single_cycle() {
        let mut graph = ConstraintGraph::new();
        let t1 = graph.add_type_node(TypeNode::new(TypeId::new(0), TypeKind::default()));
        let t2 = graph.add_type_node(TypeNode::new(TypeId::new(1), TypeKind::default()));

        let c1 = graph.add_constraint_node(ConstraintNode::new(ConstraintKind::Equality {
            left: TypeId::new(0),
            right: TypeId::new(1),
        }));

        graph.add_edge(NodeId::Type(t1), NodeId::Constraint(c1), EdgeKind::Equality);
        graph.add_edge(NodeId::Constraint(c1), NodeId::Type(t2), EdgeKind::Equality);
        graph.add_edge(NodeId::Type(t2), NodeId::Type(t1), EdgeKind::Assignment);

        let cache = graph.strongly_connected_components();
        assert_eq!(cache.len(), 1);
        let component = cache.component_for(NodeId::Type(t1)).expect("component");
        assert_eq!(component.len(), 3);
    }

    #[test]
    fn metadata_records_span() {
        let mut graph = ConstraintGraph::new();
        let t1 = graph.add_type_node(TypeNode::new(TypeId::new(0), TypeKind::default()));
        let span = dummy_span(1);
        graph.metadata_mut().insert(NodeId::Type(t1), span.clone());

        let fetched = graph.metadata().get(&NodeId::Type(t1)).expect("span");
        assert_eq!(fetched.start_line, 1);
        assert_eq!(fetched.end_line, 1);
    }

    #[test]
    fn scc_cache_is_invalidated_on_edge_addition() {
        let mut graph = ConstraintGraph::new();
        let t1 = graph.add_type_node(TypeNode::new(TypeId::new(0), TypeKind::default()));
        let t2 = graph.add_type_node(TypeNode::new(TypeId::new(1), TypeKind::default()));

        let cache_initial = graph.strongly_connected_components();
        assert_eq!(cache_initial.len(), 2);

        graph.add_edge(NodeId::Type(t1), NodeId::Type(t2), EdgeKind::Assignment);
        let cache_one_direction = graph.strongly_connected_components();
        assert_eq!(
            cache_one_direction
                .component_for(NodeId::Type(t1))
                .unwrap()
                .len(),
            1
        );
        assert_eq!(
            cache_one_direction
                .component_for(NodeId::Type(t2))
                .unwrap()
                .len(),
            1
        );

        graph.add_edge(NodeId::Type(t2), NodeId::Type(t1), EdgeKind::Assignment);
        let cache_cycle = graph.strongly_connected_components();
        assert_eq!(
            cache_cycle.component_for(NodeId::Type(t1)).unwrap().len(),
            2
        );
        assert_eq!(
            cache_cycle.component_for(NodeId::Type(t2)).unwrap().len(),
            2
        );
    }

    #[test]
    fn where_constraints_register_edges_and_nullability() {
        let mut params = HashMap::new();
        params.insert("T".into(), TypeId::new(10));
        params.insert("U".into(), TypeId::new(11));

        let clause = WhereClause {
            predicates: vec![
                WherePredicate::TraitBound {
                    type_param: "T".into(),
                    trait_name: QualifiedName::new(vec!["Comparable".into()], Span::dummy()),
                    type_args: vec![TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple(
                        "T".into(),
                    )))],
                    span: Span::dummy(),
                },
                WherePredicate::TraitBound {
                    type_param: "U".into(),
                    trait_name: QualifiedName::new(vec!["Serializer".into()], Span::dummy()),
                    type_args: vec![TypeAnnotation::Simple("T".into())],
                    span: Span::dummy(),
                },
            ],
            span: Span::dummy(),
        };

        let resolver = WhereConstraintResolver::new(SymbolId::from("pkg::Example"), &params);
        let constraints = resolver.from_clause(&clause);

        let mut graph = ConstraintGraph::new();
        let summary = graph.add_where_constraints(&constraints);

        assert_eq!(graph.constraint_node_count(), 2);
        assert_eq!(graph.type_node_count(), 2);
        assert_eq!(summary.recorded_constraints(), 2);

        let nullable: Vec<_> = summary.nullable_parameters().map(|(id, _)| id).collect();
        assert!(nullable.contains(&TypeId::new(10)));

        let t_node = graph
            .find_type_node(TypeId::new(10))
            .expect("type node for T");
        let edges_from_t: Vec<_> = graph.edges_from(NodeId::Type(t_node)).collect();
        let constraint_node = edges_from_t
            .iter()
            .find_map(|edge| match edge.to {
                NodeId::Constraint(id) if edge.kind == EdgeKind::BoundSatisfaction => Some(id),
                _ => None,
            })
            .expect("bound satisfaction edge");

        assert!(graph.has_edge_with_kind(
            NodeId::Constraint(constraint_node),
            NodeId::Type(t_node),
            EdgeKind::BoundSatisfaction,
        ));

        let cycles = graph.circular_components();
        assert!(cycles.iter().any(|component| component.len() >= 2));
    }
}
