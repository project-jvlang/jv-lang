use super::lower_source;
use jv_ast::expression::{Expression, LogBlockLevel};
use jv_ast::statement::{ConcurrencyConstruct, ResourceManagement, Statement};

#[test]
fn verifies_log_block_hir_mapping() {
    let lowered = lower_source("WARN { \"warn\" }");
    let expr = match lowered.statements.first() {
        Some(Statement::Expression { expr, .. }) => expr.clone(),
        other => panic!("expected expression statement, got {:?}", other),
    };
    match expr {
        Expression::LogBlock(block) => assert_eq!(block.level, LogBlockLevel::Warn),
        other => panic!("expected log block, got {:?}", other),
    }
}

#[test]
fn verifies_concurrency_mapping() {
    let lowered = lower_source("async { work() }");
    assert!(matches!(
        lowered.statements.first(),
        Some(Statement::Concurrency(ConcurrencyConstruct::Async { .. }))
    ));
}

#[test]
fn verifies_resource_mapping() {
    let lowered = lower_source("use file { }");
    assert!(matches!(
        lowered.statements.first(),
        Some(Statement::ResourceManagement(
            ResourceManagement::Use { .. }
        ))
    ));
}

#[test]
fn verifies_test_declaration_mapping() {
    let lowered = lower_source(r#"test "spec" { assertTrue() }"#);
    match lowered.statements.first() {
        Some(Statement::TestDeclaration(decl)) => {
            assert_eq!(decl.display_name, "spec");
            assert!(
                matches!(decl.body, Expression::Block { .. }),
                "body should be block expression"
            );
        }
        other => panic!("expected test declaration, got {:?}", other),
    }
}
