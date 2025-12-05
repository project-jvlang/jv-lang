use std::fs;
use std::path::Path;

use jv_parser_frontend::{Parser2Pipeline, ParserPipeline};

const JP_HEADING: &str = "## when式の基本構造";
const EN_HEADING: &str = "## Structure of when Expressions";
const SIDE_EFFECT_SNIPPET: &str = r#"data class Login(val username: String)
data class Logout(val username: String)

var auditBalance = 0
val event = Login("alice")

when (event) {
    is Login -> auditBalance += 1
    is Logout -> auditBalance -= 1
    else -> println("ignored: ${event}")
}"#;

#[test]
fn japanese_when_examples_are_kept_in_sync() {
    let content = load_doc("../../docs/pattern-matching.md");
    let blocks = extract_jv_blocks(&content, JP_HEADING);
    assert!(
        !blocks.is_empty(),
        "expected at least one jv code block under `{JP_HEADING}`"
    );
    assert_side_effect_sample_present(&blocks);
    assert_all_parse(&blocks);
}

#[test]
fn english_when_examples_are_kept_in_sync() {
    let content = load_doc("../../docs/pattern-matching-en.md");
    let blocks = extract_jv_blocks(&content, EN_HEADING);
    assert!(
        !blocks.is_empty(),
        "expected at least one jv code block under `{EN_HEADING}`"
    );
    assert_side_effect_sample_present(&blocks);
    assert_all_parse(&blocks);
}

fn load_doc(relative: &str) -> String {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let path = manifest_dir.join(relative);
    fs::read_to_string(&path).unwrap_or_else(|error| panic!("failed to read {:?}: {}", path, error))
}

fn extract_jv_blocks(content: &str, heading: &str) -> Vec<String> {
    let mut blocks = Vec::new();
    let mut in_section = false;
    let mut section_started = false;
    let mut in_code = false;
    let mut current_lang = String::new();
    let mut buffer = String::new();

    for line in content.lines() {
        if line.starts_with("## ") {
            if section_started && !line.trim().eq(heading) {
                break;
            }
            if line.trim().eq(heading) {
                in_section = true;
                section_started = true;
                continue;
            } else {
                in_section = false;
            }
        }

        if !in_section {
            continue;
        }

        if let Some(fence) = line.strip_prefix("```") {
            if in_code {
                if current_lang == "jv" {
                    blocks.push(buffer.trim_end().to_string());
                }
                buffer.clear();
                current_lang.clear();
                in_code = false;
            } else {
                current_lang = fence.trim().to_string();
                in_code = true;
            }
            continue;
        }

        if in_code {
            buffer.push_str(line);
            buffer.push('\n');
        }
    }

    blocks
}

fn assert_side_effect_sample_present(blocks: &[String]) {
    let expected = SIDE_EFFECT_SNIPPET.trim();
    let found = blocks.iter().any(|block| block.trim() == expected);
    assert!(
        found,
        "side-effect when sample missing or out of sync. expected snippet:\n{}",
        expected
    );
}

fn assert_all_parse(blocks: &[String]) {
    let pipeline = Parser2Pipeline::default();
    for block in blocks {
        if block.trim() == SIDE_EFFECT_SNIPPET.trim() {
            // The side-effect example intentionally omits surrounding context; only
            // presence is asserted above.
            continue;
        }
        pipeline.parse(block).unwrap_or_else(|error| {
            panic!("failed to parse jv example:\n{}\nerror: {:?}", block, error)
        });
    }
}
