//! 型制約ソルバーの骨組み。
//!
//! 本フェーズでは状態機械の基本構造のみを用意し、詳細ロジックは後続で拡張する。

use crate::constraints::{Constraint, ConstraintGraph, ConstraintGraphEdge};

#[derive(Debug, Default)]
pub struct Solver {
    graph: ConstraintGraph,
}

impl Solver {
    pub fn new(graph: ConstraintGraph) -> Self {
        Self { graph }
    }

    /// 制約グラフを走査し、暫定的に処理を実行する。
    /// 現時点では全エッジを列挙するのみ。
    pub fn solve(&mut self) -> Vec<Constraint> {
        let mut constraints = Vec::new();
        for edge in self.graph.0.edge_references() {
            let c = match edge.weight() {
                ConstraintGraphEdge::Constraint(c) => c.clone(),
            };
            constraints.push(c);
        }
        constraints
    }
}
