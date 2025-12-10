//! 型制約ソルバーの骨組み。
//!
//! Collecting → Propagating → Checking → Done の状態遷移を持ち、詳細ロジックは後続で拡張する。

use crate::constraints::{Constraint, ConstraintGraph, ConstraintGraphEdge};

#[derive(Debug, Clone, PartialEq)]
pub enum SolverState {
    Collecting,
    Propagating,
    Checking,
    Done,
    Error(String),
}

impl Default for SolverState {
    fn default() -> Self {
        SolverState::Collecting
    }
}

#[derive(Debug, Default)]
pub struct Solver {
    graph: ConstraintGraph,
    constraints: Vec<Constraint>,
    state: SolverState,
}

impl Solver {
    pub fn new(graph: ConstraintGraph) -> Self {
        Self {
            graph,
            constraints: Vec::new(),
            state: SolverState::Collecting,
        }
    }

    pub fn state(&self) -> &SolverState {
        &self.state
    }

    fn collect(&mut self) {
        self.constraints.clear();
        for edge in self.graph.0.edge_references() {
            let c = match edge.weight() {
                ConstraintGraphEdge::Constraint(c) => c.clone(),
            };
            self.constraints.push(c);
        }
    }

    fn propagate(&mut self) {
        // 将来の単一化・伝播ロジックのフック。現状は no-op。
    }

    fn check(&mut self) {
        // 将来の整合性チェックをここに追加。現状は no-op。
    }

    /// 1 ステップ状態遷移を進める。
    pub fn step(&mut self) -> Result<(), String> {
        match &self.state {
            SolverState::Collecting => {
                self.collect();
                self.state = SolverState::Propagating;
                Ok(())
            }
            SolverState::Propagating => {
                self.propagate();
                self.state = SolverState::Checking;
                Ok(())
            }
            SolverState::Checking => {
                self.check();
                self.state = SolverState::Done;
                Ok(())
            }
            SolverState::Done => Ok(()),
            SolverState::Error(msg) => Err(msg.clone()),
        }
    }

    /// 状態が Done になるまで遷移し、収集済み制約を返す。
    pub fn solve(&mut self) -> Result<Vec<Constraint>, String> {
        self.run_to_fixpoint(32)
    }

    /// 状態を進め、収束するかエラーループを検出する。
    pub fn run_to_fixpoint(&mut self, max_steps: usize) -> Result<Vec<Constraint>, String> {
        let mut steps = 0;
        while !matches!(self.state, SolverState::Done | SolverState::Error(_)) {
            if steps >= max_steps {
                self.state = SolverState::Error("solver did not converge".into());
                break;
            }
            self.step()?;
            steps += 1;
        }

        match &self.state {
            SolverState::Done => Ok(self.constraints.clone()),
            SolverState::Error(msg) => Err(msg.clone()),
            _ => Ok(self.constraints.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constraints::{ConstraintGraphEdge, ConstraintGraphNode, TypeRef};
    use jv_ast::types::TypeAnnotation;

    fn graph_with_single_constraint() -> ConstraintGraph {
        let mut g = ConstraintGraph::default();
        let a =
            g.0.add_node(ConstraintGraphNode::Type(TypeAnnotation::Simple(
                "A".into(),
            )));
        let b =
            g.0.add_node(ConstraintGraphNode::Type(TypeAnnotation::Simple(
                "B".into(),
            )));
        let c = Constraint::Equal(
            TypeRef::Annotation(TypeAnnotation::Simple("A".into())),
            TypeRef::Annotation(TypeAnnotation::Simple("B".into())),
        );
        g.0.add_edge(a, b, ConstraintGraphEdge::Constraint(c));
        g
    }

    #[test]
    fn solver_transitions_through_states() {
        let graph = graph_with_single_constraint();
        let mut solver = Solver::new(graph);
        assert_eq!(solver.state(), &SolverState::Collecting);
        solver.step().unwrap();
        assert_eq!(solver.state(), &SolverState::Propagating);
        solver.step().unwrap();
        assert_eq!(solver.state(), &SolverState::Checking);
        solver.step().unwrap();
        assert_eq!(solver.state(), &SolverState::Done);
    }

    #[test]
    fn solver_collects_constraints_on_solve() {
        let graph = graph_with_single_constraint();
        let mut solver = Solver::new(graph);
        let constraints = solver.solve().unwrap();
        assert_eq!(constraints.len(), 1);
    }

    #[test]
    fn run_to_fixpoint_converges_with_limit() {
        let graph = graph_with_single_constraint();
        let mut solver = Solver::new(graph);
        let constraints = solver.run_to_fixpoint(10).unwrap();
        assert_eq!(constraints.len(), 1);
        assert_eq!(solver.state(), &SolverState::Done);
    }
}
