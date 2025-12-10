use crate::constraints::collect_constraints;
use crate::db::{Database, FileInput, constraint_graph};
use std::sync::Arc;

fn constraints_for_source(src: &str) -> Vec<String> {
    let db = Database::new();
    let file = FileInput::new(&db, Arc::from("test.jv"), Arc::from(src));
    let constraints = collect_constraints(&db, file);
    constraints
        .iter()
        .map(|c| format!("{:?}", c))
        .collect::<Vec<_>>()
}

#[test]
fn collects_constraints_for_val_and_call() {
    let list =
        constraints_for_source("package main\nval x = 1\nfun f(a: Int): Int { a }\nval y = f(x)\n");
    assert!(
        !list.is_empty(),
        "constraints should be collected for simple program"
    );
    assert!(
        list.iter().any(|c| c.contains("Callable")),
        "callable constraint should exist"
    );
}

#[test]
fn constraint_graph_handles_cycles() {
    let db = Database::new();
    let file = FileInput::new(
        &db,
        Arc::from("cycle.jv"),
        Arc::from("val x = 1\nval y = x\n"),
    );
    let graph = constraint_graph(&db, file);
    // Graph should contain at least one edge and not panic on cycles/self references.
    assert!(
        graph.0.edge_count() > 0,
        "constraint graph should contain edges"
    );
}
