use crate::db::{Database, FileInput, lower_to_hir};
use jv_ast::statement::Statement;
use std::sync::Arc;

#[test]
fn salsa_lowering_produces_expected_statements() {
    let source = "package demo\nval x = 1 + 2\n";
    let db = Database::new();
    let file = FileInput::new(&db, Arc::from("demo.jv"), Arc::from(source));

    let salsa_hir = lower_to_hir(&db, file);
    assert!(
        !salsa_hir.statements.is_empty(),
        "expected at least package + val statements"
    );
    match &salsa_hir.statements[0] {
        Statement::Package { name, .. } => assert_eq!(name, "demo"),
        other => panic!("expected package statement, got {:?}", other),
    }
    match &salsa_hir.statements[1] {
        Statement::ValDeclaration { name, .. } => assert_eq!(name, "x"),
        other => panic!("expected val declaration, got {:?}", other),
    }
}
