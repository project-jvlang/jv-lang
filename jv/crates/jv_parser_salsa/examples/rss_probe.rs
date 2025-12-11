use jv_parser_frontend::ParserPipeline;
use jv_parser_salsa::pipeline::{ParseOptions, SalsaPipeline};
use jv_parser_rowan::frontend::RowanPipeline;
use std::fs;
use std::path::PathBuf;

fn current_rss_kb() -> Option<u64> {
    let status = fs::read_to_string("/proc/self/status").ok()?;
    status.lines().find_map(|line| {
        if let Some(rest) = line.strip_prefix("VmRSS:") {
            rest.split_whitespace()
                .next()
                .and_then(|num| num.parse::<u64>().ok())
        } else {
            None
        }
    })
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let corpus_path = manifest_dir
        .join("benches")
        .join("corpus")
        .join("synthetic")
        .join("synthetic-2000.jv");
    let source = fs::read_to_string(&corpus_path)?;

    let salsa = SalsaPipeline::new();
    let rowan = RowanPipeline::new();

    let before = current_rss_kb().unwrap_or(0);
    salsa
        .execute_with_options(
            &source,
            ParseOptions {
                generate_cst: false,
                generate_trivia_map: false,
            },
        )
        .map_err(|e| format!("salsa_fast parse failed: {e:?}"))?;
    let after_fast = current_rss_kb().unwrap_or(before);

    let delta_fast = after_fast.saturating_sub(before);

    let before_full = current_rss_kb().unwrap_or(0);
    salsa
        .execute_with_options(
            &source,
            ParseOptions {
                generate_cst: true,
                generate_trivia_map: true,
            },
        )
        .map_err(|e| format!("salsa_full parse failed: {e:?}"))?;
    let after_full = current_rss_kb().unwrap_or(before_full);
    let delta_full = after_full.saturating_sub(before_full);

    let before_rowan = current_rss_kb().unwrap_or(0);
    rowan
        .execute(&source)
        .map_err(|e| format!("rowan parse failed: {e:?}"))?;
    let after_rowan = current_rss_kb().unwrap_or(before_rowan);
    let delta_rowan = after_rowan.saturating_sub(before_rowan);

    println!(
        "RSS KiB delta (synthetic-2000): salsa_fast={} KiB, salsa_full={} KiB, rowan={} KiB",
        delta_fast, delta_full, delta_rowan
    );

    Ok(())
}
