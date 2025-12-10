use crate::db::{Database, FileInput, lower_to_hir};
use std::sync::Arc;

#[test]
fn lower_to_hir_uses_salsa_cache() {
    let db = Database::new();
    let file = FileInput::new(
        &db,
        Arc::from("cache.jv"),
        Arc::from("package demo\nval x = 1"),
    );

    let first = lower_to_hir(&db, file);
    let second = lower_to_hir(&db, file);
    assert!(
        Arc::ptr_eq(&first, &second),
        "salsa should reuse cached HIR results"
    );
}
