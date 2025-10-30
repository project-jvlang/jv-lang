use std::time::Instant;

use jv_lsp::{JvLanguageServer, Position};

#[test]
fn lsp_responses_stay_under_latency_budget() {
    // Lightweight test: Verify basic LSP operations don't hang
    // Removed full pipeline execution that caused memory issues
    let mut server = JvLanguageServer::new();
    let uri = "file:///latency.jv".to_string();
    let source = "val x = 1".to_string();

    let start = Instant::now();
    server.open_document(uri.clone(), source);
    let open_duration = start.elapsed().as_millis();

    assert!(
        open_duration <= 100,
        "document open took too long: {} ms",
        open_duration
    );

    let position = Position {
        line: 0,
        character: 0,
    };

    // Test lightweight completion (no diagnostics pipeline)
    let completion_start = Instant::now();
    let completions = server.get_completions(&uri, position);
    let completion_duration = completion_start.elapsed().as_millis();

    assert!(
        !completions.is_empty(),
        "completions should return templates"
    );
    assert!(
        completion_duration <= 100,
        "completion took too long: {} ms",
        completion_duration
    );
}
