#![cfg(feature = "dsl-all")]

use std::fs;
use std::path::Path;

use jv_parser2::plugins::PluginRegistry;
use serde_json::Value;

fn read_fixture(suite: &str, name: &str) -> (String, Value) {
    let base = Path::new("tests/fixtures/dsl").join(suite);
    let input = fs::read_to_string(base.join(format!("{name}.jv"))).expect("fixture input missing");
    let expected = fs::read_to_string(base.join(format!("{name}.expected.json")))
        .expect("fixture expected missing");
    let expected_json: Value = serde_json::from_str(&expected).expect("invalid expected json");
    (input, expected_json)
}

fn split_block(input: &str) -> (&str, &str) {
    let start = input.find("|{").expect("missing block start");
    let end_rel = input[start + 2..]
        .find("}|")
        .expect("missing block end");
    let keyword = input[..start].trim().split_whitespace().last().unwrap_or_default();
    let body_end = start + 2 + end_rel;
    (keyword, input[start + 2..body_end].trim())
}

fn parse_with_registry(registry: &PluginRegistry, suite: &str, name: &str) -> Value {
    let (input, expected) = read_fixture(suite, name);
    let (keyword, body) = split_block(&input);
    let actual_block = registry
        .parse_block(keyword, body)
        .expect("parser error")
        .expect("plugin missing");
    let actual_json = serde_json::to_value(actual_block).expect("serialize block");
    assert_eq!(actual_json, expected);
    actual_json
}

fn parse_multi(registry: &PluginRegistry, suite: &str, name: &str) -> Value {
    let (input, expected) = read_fixture(suite, name);
    let mut blocks = Vec::new();
    let mut cursor = input.as_str();
    while let Some(start) = cursor.find("|{") {
        let keyword = cursor[..start]
            .trim_end()
            .split_whitespace()
            .last()
            .unwrap_or_default();
        let body_start = start + 2;
        let rest = &cursor[body_start..];
        if let Some(end_rel) = rest.find("}|") {
            let body = rest[..end_rel].trim();
            let block = registry
                .parse_block(keyword, body)
                .expect("parse error")
                .expect("plugin missing");
            blocks.push(serde_json::to_value(block).expect("serialize block"));
            cursor = &rest[end_rel + 2..];
        } else {
            break;
        }
    }
    let actual = Value::Array(blocks);
    assert_eq!(actual, expected);
    actual
}

#[test]
fn log_plugin_parses_levels() {
    let registry = PluginRegistry::new();
    parse_with_registry(&registry, "log", "basic");
}

#[test]
fn io_plugin_tracks_operations() {
    let registry = PluginRegistry::new();
    parse_with_registry(&registry, "io", "basic");
}

#[test]
fn lock_plugin_requires_body() {
    let registry = PluginRegistry::new();
    parse_with_registry(&registry, "lock", "basic");
}

#[test]
fn cron_plugin_extracts_schedule() {
    let registry = PluginRegistry::new();
    parse_with_registry(&registry, "cron", "basic");
}

#[test]
fn assert_plugin_collects_checks() {
    let registry = PluginRegistry::new();
    parse_with_registry(&registry, "assert", "basic");
}

#[test]
fn registry_handles_multiple_blocks() {
    let registry = PluginRegistry::new();
    parse_multi(&registry, "combo", "mixed");
}
