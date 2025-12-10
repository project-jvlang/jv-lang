use crate::constraints::types::{Constraint, TypeVarId};
use jv_ast::types::TypeAnnotation;
use petgraph::Directed;
use petgraph::graph::Graph;
use petgraph::visit::EdgeRef;
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;

/// 制約グラフのノード。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ConstraintGraphNode {
    Type(TypeAnnotation),
    TypeVar(TypeVarId),
}

/// 制約グラフのエッジ。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ConstraintGraphEdge {
    Constraint(Constraint),
}

/// petgraph ベースの制約グラフ型を包むラッパー。
#[derive(Debug, Clone, Default)]
pub struct ConstraintGraph(pub Graph<ConstraintGraphNode, ConstraintGraphEdge, Directed>);

impl PartialEq for ConstraintGraph {
    fn eq(&self, other: &Self) -> bool {
        let self_nodes: BTreeSet<String> =
            self.0.node_weights().map(|n| format!("{:?}", n)).collect();
        let other_nodes: BTreeSet<String> =
            other.0.node_weights().map(|n| format!("{:?}", n)).collect();
        if self_nodes != other_nodes {
            return false;
        }

        let self_edges: BTreeSet<(String, String, String)> = self
            .0
            .edge_references()
            .map(|e| {
                let src = format!("{:?}", self.0[e.source()]);
                let dst = format!("{:?}", self.0[e.target()]);
                let w = format!("{:?}", e.weight());
                (src, dst, w)
            })
            .collect();
        let other_edges: BTreeSet<(String, String, String)> = other
            .0
            .edge_references()
            .map(|e| {
                let src = format!("{:?}", other.0[e.source()]);
                let dst = format!("{:?}", other.0[e.target()]);
                let w = format!("{:?}", e.weight());
                (src, dst, w)
            })
            .collect();
        self_edges == other_edges
    }
}

impl Eq for ConstraintGraph {}
