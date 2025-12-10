use super::lower_source;

#[test]
fn reports_missing_log_brace() {
    let lowered = lower_source("LOG \"oops\"");
    assert!(
        lowered
            .diagnostics
            .iter()
            .any(|d| d.message.contains("JV-DSL-002")),
        "expected JV-DSL-002 diagnostic, got {:?}",
        lowered.diagnostics
    );
}

#[test]
fn reports_missing_resource_for_use() {
    let lowered = lower_source("use { }");
    assert!(
        lowered
            .diagnostics
            .iter()
            .any(|d| d.message.contains("JV-DSL-004")),
        "expected JV-DSL-004 diagnostic"
    );
}

#[test]
fn reports_missing_test_name_and_body() {
    let lowered = lower_source("test { }");
    assert!(
        lowered
            .diagnostics
            .iter()
            .any(|d| d.message.contains("JV-DSL-006")),
        "expected JV-DSL-006 diagnostic"
    );
    assert!(
        lowered
            .diagnostics
            .iter()
            .any(|d| d.message.contains("JV-DSL-007")),
        "expected JV-DSL-007 diagnostic"
    );
}
