use std::{
    fs,
    path::{Path, PathBuf},
    process::ExitCode,
};

/// ベンチマークコーパスの存在と整合性を検証する。
fn main() -> ExitCode {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let corpus_dir = manifest_dir.join("benches").join("corpus");

    if let Err(error) = verify_stdlib_list(&manifest_dir, &corpus_dir) {
        eprintln!("stdlib corpus verification failed: {error}");
        return ExitCode::FAILURE;
    }

    if let Err(error) = verify_synthetic_files(&corpus_dir) {
        eprintln!("synthetic corpus verification failed: {error}");
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}

fn verify_stdlib_list(manifest_dir: &Path, corpus_dir: &Path) -> Result<(), String> {
    let list_path = corpus_dir.join("stdlib.txt");
    let content = fs::read_to_string(&list_path)
        .map_err(|err| format!("cannot read {list_path:?}: {err}"))?;

    for (idx, raw_line) in content.lines().enumerate() {
        let line = raw_line.trim();
        if line.is_empty() {
            continue;
        }
        let candidate = manifest_dir.join(line);
        if !candidate.exists() {
            return Err(format!(
                "missing stdlib entry at line {idx}: {line} ({candidate:?})"
            ));
        }
        if candidate.extension().and_then(|ext| ext.to_str()) != Some("jv") {
            return Err(format!("non-jv entry at line {idx}: {line}"));
        }
    }

    Ok(())
}

fn verify_synthetic_files(corpus_dir: &Path) -> Result<(), String> {
    let synthetic_dir = corpus_dir.join("synthetic");
    let expectations = [
        (100_usize, "synthetic-100.jv"),
        (500, "synthetic-500.jv"),
        (2000, "synthetic-2000.jv"),
    ];

    for (expected_lines, file_name) in expectations {
        let path = synthetic_dir.join(file_name);
        let content =
            fs::read_to_string(&path).map_err(|err| format!("cannot read {path:?}: {err}"))?;
        let line_count = content.lines().count();
        if line_count != expected_lines {
            return Err(format!(
                "expected {expected_lines} lines in {file_name}, found {line_count}"
            ));
        }
        if !content
            .lines()
            .any(|line| line.starts_with("fun synthetic_"))
        {
            return Err(format!(
                "{file_name} does not contain any function signatures"
            ));
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stdlib_list_matches_files() {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let corpus_dir = manifest_dir.join("benches").join("corpus");
        verify_stdlib_list(&manifest_dir, &corpus_dir).expect("stdlib corpus should be valid");
    }

    #[test]
    fn synthetic_files_have_expected_shape() {
        let corpus_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("benches")
            .join("corpus");
        verify_synthetic_files(&corpus_dir).expect("synthetic corpus should be valid");
    }
}
