use super::lower_source;
use jv_ast::expression::{Expression, LogBlockLevel};
use jv_ast::statement::{ConcurrencyConstruct, ResourceManagement, Statement, TestDataset};

#[test]
fn parses_log_block_default_level() {
    let lowered = lower_source("LOG { \"start\" }");
    let expr = match lowered.statements.first().expect("statement exists") {
        Statement::Expression { expr, .. } => expr.clone(),
        other => panic!("expected expression statement, got {:?}", other),
    };

    match expr {
        Expression::LogBlock(block) => {
            assert_eq!(block.level, LogBlockLevel::Default);
            assert_eq!(block.items.len(), 1);
        }
        other => panic!("expected log block expression, got {:?}", other),
    }
}

#[test]
fn parses_spawn_and_async_blocks() {
    let lowered = lower_source("spawn { x }\nasync { y }");
    assert!(
        matches!(
            lowered.statements.first(),
            Some(Statement::Concurrency(ConcurrencyConstruct::Spawn { .. }))
        ),
        "expected spawn construct"
    );
    assert!(
        matches!(
            lowered.statements.get(1),
            Some(Statement::Concurrency(ConcurrencyConstruct::Async { .. }))
        ),
        "expected async construct"
    );
}

#[test]
fn parses_use_and_defer_blocks() {
    let lowered_use = lower_source(r#"use resource { }"#);
    assert!(
        lowered_use.statements.iter().any(|stmt| matches!(
            stmt,
            Statement::ResourceManagement(ResourceManagement::Use { .. })
        )),
        "expected use resource management"
    );

    let lowered_defer = lower_source(r#"defer { cleanup() }"#);
    assert!(
        lowered_defer.statements.iter().any(|stmt| matches!(
            stmt,
            Statement::ResourceManagement(ResourceManagement::Defer { .. })
        )),
        "expected defer resource management"
    );
}

#[test]
fn parses_test_block_with_dataset() {
    let lowered = lower_source(r#"test "sample" dataset cases {  }"#);
    let stmt = lowered
        .statements
        .first()
        .expect("test statement present")
        .clone();
    match stmt {
        Statement::TestDeclaration(decl) => {
            assert_eq!(decl.display_name, "sample");
            match decl.dataset {
                Some(TestDataset::Sample(sample)) => assert_eq!(sample.source, "cases"),
                other => panic!("expected sample dataset, got {:?}", other),
            }
        }
        other => panic!("expected test declaration, got {:?}", other),
    }
}
