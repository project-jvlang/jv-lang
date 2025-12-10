use crate::constraints::{ConstraintGraphEdge, ConstraintGraphNode, TypeRef};
use crate::db::{Database, FileInput, constraint_graph, solve_types};
use crate::solver::{Solver, SolverState};
use jv_ast::types::TypeAnnotation;
use salsa::Setter;
use std::sync::Arc;

fn graph_with_single_constraint() -> crate::constraints::ConstraintGraph {
    let mut g = crate::constraints::ConstraintGraph::default();
    let a =
        g.0.add_node(ConstraintGraphNode::Type(TypeAnnotation::Simple(
            "A".into(),
        )));
    let b =
        g.0.add_node(ConstraintGraphNode::Type(TypeAnnotation::Simple(
            "B".into(),
        )));
    let c = crate::constraints::Constraint::Equal(
        TypeRef::Annotation(TypeAnnotation::Simple("A".into())),
        TypeRef::Annotation(TypeAnnotation::Simple("B".into())),
    );
    g.0.add_edge(a, b, ConstraintGraphEdge::Constraint(c));
    g
}

fn graph_with_two_constraints() -> crate::constraints::ConstraintGraph {
    let mut g = crate::constraints::ConstraintGraph::default();
    let a =
        g.0.add_node(ConstraintGraphNode::Type(TypeAnnotation::Simple(
            "A".into(),
        )));
    let b =
        g.0.add_node(ConstraintGraphNode::Type(TypeAnnotation::Simple(
            "B".into(),
        )));
    let c =
        g.0.add_node(ConstraintGraphNode::Type(TypeAnnotation::Simple(
            "C".into(),
        )));
    let c1 = crate::constraints::Constraint::Equal(
        TypeRef::Annotation(TypeAnnotation::Simple("A".into())),
        TypeRef::Annotation(TypeAnnotation::Simple("B".into())),
    );
    let c2 = crate::constraints::Constraint::Equal(
        TypeRef::Annotation(TypeAnnotation::Simple("B".into())),
        TypeRef::Annotation(TypeAnnotation::Simple("C".into())),
    );
    g.0.add_edge(a, b, ConstraintGraphEdge::Constraint(c1));
    g.0.add_edge(b, c, ConstraintGraphEdge::Constraint(c2));
    g
}

fn cyclic_graph() -> crate::constraints::ConstraintGraph {
    let mut g = crate::constraints::ConstraintGraph::default();
    let a =
        g.0.add_node(ConstraintGraphNode::Type(TypeAnnotation::Simple(
            "A".into(),
        )));
    let b =
        g.0.add_node(ConstraintGraphNode::Type(TypeAnnotation::Simple(
            "B".into(),
        )));
    let c = crate::constraints::Constraint::Equal(
        TypeRef::Annotation(TypeAnnotation::Simple("A".into())),
        TypeRef::Annotation(TypeAnnotation::Simple("B".into())),
    );
    g.0.add_edge(a, b, ConstraintGraphEdge::Constraint(c.clone()));
    g.0.add_edge(b, a, ConstraintGraphEdge::Constraint(c));
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
fn solver_handles_multiple_constraints() {
    let graph = graph_with_two_constraints();
    let mut solver = Solver::new(graph);
    let constraints = solver.solve().unwrap();
    assert_eq!(constraints.len(), 2);
    assert_eq!(solver.state(), &SolverState::Done);
}

#[test]
fn run_to_fixpoint_converges_with_limit() {
    let graph = graph_with_single_constraint();
    let mut solver = Solver::new(graph);
    let constraints = solver.run_to_fixpoint(10).unwrap();
    assert_eq!(constraints.len(), 1);
    assert_eq!(solver.state(), &SolverState::Done);
}

#[test]
fn run_to_fixpoint_errors_on_zero_limit() {
    let graph = cyclic_graph();
    let mut solver = Solver::new(graph);
    let result = solver.run_to_fixpoint(0);
    assert!(result.is_err());
    assert!(matches!(solver.state(), SolverState::Error(_)));
}

#[test]
fn solve_types_is_cached_and_recomputes_on_change() {
    let mut db = Database::new();
    let file = FileInput::new(
        &db,
        Arc::from("main.jv"),
        Arc::from("package main\nval x = 1"),
    );

    let first = solve_types(&db, file);
    let second = solve_types(&db, file);
    assert!(
        Arc::ptr_eq(&first, &second),
        "solve_types should be cached for unchanged input"
    );

    file.set_text(&mut db)
        .to(Arc::from("package main\nval x = 2"));
    let updated = solve_types(&db, file);
    assert!(
        !Arc::ptr_eq(&first, &updated),
        "changing input should invalidate solve_types cache"
    );

    let graph = constraint_graph(&db, file);
    assert!(graph.0.edge_count() >= 1);
}
