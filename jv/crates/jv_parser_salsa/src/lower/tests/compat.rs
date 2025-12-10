use crate::db::{Database, FileInput, lower_to_hir};
use jv_ast::statement::Statement;
use jv_parser_rowan::frontend::RowanPipeline;
use std::sync::Arc;

#[test]
fn salsa_and_rowan_lowerings_align_on_basic_input() {
    let source = "package demo\nval x = 1 + 2\n";
    let db = Database::new();
    let file = FileInput::new(&db, Arc::from("demo.jv"), Arc::from(source));

    let salsa_hir = lower_to_hir(&db, file);
    let rowan = RowanPipeline::default()
        .execute_with_debug(source)
        .expect("rowan pipeline")
        .statements()
        .to_vec();

    assert_eq!(salsa_hir.statements.len(), rowan.len());
    match (&salsa_hir.statements[0], &rowan[0]) {
        (Statement::Package { name: left, .. }, Statement::Package { name: right, .. }) => {
            assert_eq!(left, right);
        }
        other => panic!("package statements should match, got {:?}", other),
    }

    if salsa_hir.statements.len() > 1 {
        match (&salsa_hir.statements[1], &rowan[1]) {
            (
                Statement::ValDeclaration { name: left, .. },
                Statement::ValDeclaration { name: right, .. },
            ) => assert_eq!(left, right),
            other => panic!("val statements should match, got {:?}", other),
        }
    }
}
