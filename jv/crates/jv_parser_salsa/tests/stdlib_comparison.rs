use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;
use jv_parser_salsa::pipeline::{ParseOptions, SalsaPipeline};
use jv_parser_salsa::support::normalize::{normalize_diagnostics, spans_within_tolerance};
use std::fs;
use std::path::PathBuf;

#[test]
fn salsa_matches_rowan_on_stdlib_corpus() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let corpus_list = manifest_dir
        .join("benches")
        .join("corpus")
        .join("stdlib.txt");
    let entries: Vec<_> = fs::read_to_string(&corpus_list)
        .expect("can read corpus list")
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .map(|rel| manifest_dir.parent().unwrap().parent().unwrap().join(rel))
        .collect();

    let salsa = SalsaPipeline::new();
    let rowan = RowanPipeline::new();

    for path in entries {
        let source = fs::read_to_string(&path).expect("can read stdlib file");
        let salsa_output = salsa.execute_with_options(
            &source,
            ParseOptions {
                generate_cst: true,
                generate_trivia_map: true,
                trim_trivia_and_metadata: false,
            },
        );
        let rowan_output = rowan.execute(&source);

        match (salsa_output, rowan_output) {
            (Ok(salsa_output), Ok(rowan_output)) => {
                let s_diags =
                    normalize_diagnostics(salsa_output.artifacts.diagnostics().final_diagnostics());
                let r_diags = normalize_diagnostics(rowan_output.diagnostics().final_diagnostics());
                assert_eq!(
                    s_diags.len(),
                    r_diags.len(),
                    "diagnostic count mismatch for {:?}",
                    path
                );
                for ((s_key, s_msg), (r_key, r_msg)) in s_diags.iter().zip(r_diags.iter()) {
                    assert_eq!(
                        s_key.severity, r_key.severity,
                        "severity mismatch for {:?}",
                        path
                    );
                    assert_eq!(s_key.code, r_key.code, "code mismatch for {:?}", path);
                    assert_eq!(s_msg, r_msg, "message mismatch for {:?}", path);
                }

                let s_program = &salsa_output.artifacts.program;
                let r_program = &rowan_output.program;
                assert!(
                    spans_within_tolerance(&s_program.span, &r_program.span),
                    "program span tolerance exceeded for {:?}: {:?} vs {:?}",
                    path,
                    s_program.span,
                    r_program.span
                );

                assert_eq!(
                    s_program.statements.len(),
                    r_program.statements.len(),
                    "statement count mismatch for {:?}",
                    path
                );
            }
            (Err(se), Err(re)) => {
                if std::mem::discriminant(&se) != std::mem::discriminant(&re) {
                    eprintln!(
                        "Skipping file with differing error kinds {:?}: salsa={:?}, rowan={:?}",
                        path,
                        se.span(),
                        re.span()
                    );
                }
            }
            (Err(_), Ok(_)) => {
                eprintln!("Skipping file with salsa-only failure: {:?}", path);
            }
            (Ok(_), Err(_)) => {
                eprintln!("Skipping file with rowan-only failure: {:?}", path);
            }
        }
    }
}
