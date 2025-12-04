use std::fs;
use std::path::PathBuf;
use std::time::{Duration, Instant};

use jv_lsp::ParserAdapter;

fn fixture(name: &str) -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/incremental")
        .join(name);
    fs::read_to_string(path).expect("fixture should exist")
}

fn measure_full_parse(adapter: &mut ParserAdapter, uri: &str, content: &str) -> Duration {
    let start = Instant::now();
    adapter
        .parse_full(uri, content)
        .expect("full parse should succeed");
    adapter
        .parse_full(uri, content)
        .expect("second full parse should succeed");
    start.elapsed()
}

#[test]
fn incremental_small_file_within_budget() {
    let base = fixture("small_file_edit.jv");
    let edited = base.replace("value050", "value050 + 1");
    let mut adapter = ParserAdapter::new();
    let uri = "file:///small";

    let full_time = measure_full_parse(&mut adapter, uri, &base);
    let start = Instant::now();
    adapter
        .parse_incremental(uri, &base, &edited)
        .expect("incremental parse should succeed");
    let incremental = start.elapsed();

    assert!(
        incremental <= Duration::from_millis(200),
        "incremental parse should stay under 200ms (was {:?})",
        incremental
    );
    assert!(
        incremental <= full_time + Duration::from_millis(20),
        "incremental parse should not exceed full parse baseline (full {:?}, incremental {:?})",
        full_time,
        incremental
    );
}

#[test]
fn incremental_medium_file_within_budget() {
    let base = fixture("medium_file_edit.jv");
    let edited = base.replace("metric250", "metric250 + metric249");
    let mut adapter = ParserAdapter::new();
    let uri = "file:///medium";

    let full_time = measure_full_parse(&mut adapter, uri, &base);
    let start = Instant::now();
    adapter
        .parse_incremental(uri, &base, &edited)
        .expect("incremental parse should succeed");
    let incremental = start.elapsed();

    assert!(
        incremental <= Duration::from_millis(200),
        "incremental parse should stay under 200ms (was {:?})",
        incremental
    );
    assert!(
        incremental <= full_time + Duration::from_millis(20),
        "incremental parse should not exceed full parse baseline (full {:?}, incremental {:?})",
        full_time,
        incremental
    );
}

#[test]
fn incremental_block_insertion_within_budget() {
    let base = fixture("block_insertion.jv");
    let edited = base.replace("val doubled", "val doubled = value * value");
    let mut adapter = ParserAdapter::new();
    let uri = "file:///block";

    let full_time = measure_full_parse(&mut adapter, uri, &base);
    let start = Instant::now();
    adapter
        .parse_incremental(uri, &base, &edited)
        .expect("incremental parse should succeed");
    let incremental = start.elapsed();

    assert!(
        incremental <= Duration::from_millis(200),
        "incremental parse should stay under 200ms (was {:?})",
        incremental
    );
    assert!(
        incremental <= full_time + Duration::from_millis(20),
        "incremental parse should not exceed full parse baseline (full {:?}, incremental {:?})",
        full_time,
        incremental
    );
}
