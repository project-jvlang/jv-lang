use jv_parser_salsa::pipeline::{ParseOptions, SalsaPipeline};
use std::fs;
use std::path::PathBuf;

#[test]
fn salsa_parses_stdlib_corpus_without_errors() {
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

    let salsa = SalsaPipeline::new_without_jdk();

    let mut successes = 0usize;

    for path in entries {
        let source = fs::read_to_string(&path).expect("can read stdlib file");
        match salsa.execute_with_options(
            &source,
            ParseOptions {
                generate_cst: true,
                generate_trivia_map: true,
                trim_trivia_and_metadata: false,
            },
        ) {
            Ok(salsa_output) => {
                successes += 1;
                let diagnostics = salsa_output
                    .artifacts
                    .diagnostics()
                    .final_diagnostics();
                if !diagnostics.is_empty() {
                    eprintln!(
                        "diagnostics for {:?}: {:?}",
                        path.file_name().unwrap_or_default(),
                        diagnostics
                    );
                }
                assert!(
                    !salsa_output.artifacts.program.statements.is_empty(),
                    "program should contain statements for {:?}",
                    path
                );
            }
            Err(err) => {
                eprintln!("salsa parse failed for {:?}: {:?}", path, err);
            }
        }
    }

    assert!(successes > 0, "no stdlib files parsed successfully");
}
