use criterion::Criterion;
use jv_parser_frontend::{ParseError, ParserPipeline};
use jv_parser_rowan::frontend::RowanPipeline;
use jv_parser_salsa::pipeline::{ParseOptions, SalsaPipeline};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

#[derive(Clone, Copy, Debug)]
pub enum PipelineKind {
    /// Salsa パイプライン（CST/Trivia なし）
    SalsaFast,
    /// Salsa パイプライン（CST/Trivia あり）
    SalsaFull,
    /// Rowan ベースのリファレンスパイプライン
    Rowan,
}

pub struct PipelineSwitcher {
    salsa: SalsaPipeline,
    rowan: RowanPipeline,
}

impl PipelineSwitcher {
    pub fn new() -> Self {
        Self {
            salsa: SalsaPipeline::new(),
            rowan: RowanPipeline::new(),
        }
    }

    /// パイプラインを切り替えて実行する。診断件数を返す。
    pub fn run(&self, kind: PipelineKind, source: &str) -> Result<usize, ParseError> {
        match kind {
            PipelineKind::SalsaFast => self
                .salsa
                .execute_with_options(source, ParseOptions::default())
                .map(|out| out.artifacts.diagnostics.final_diagnostics().len()),
            PipelineKind::SalsaFull => self
                .salsa
                .execute_with_options(
                    source,
                    ParseOptions {
                        generate_cst: true,
                        generate_trivia_map: true,
                    },
                )
                .map(|out| out.artifacts.diagnostics.final_diagnostics().len()),
            PipelineKind::Rowan => self
                .rowan
                .execute(source)
                .map(|artifacts| artifacts.diagnostics.final_diagnostics().len()),
        }
    }
}

#[derive(Clone)]
pub struct CorpusEntry {
    pub name: String,
    pub source: String,
}

pub struct BenchCorpus {
    stdlib: Vec<CorpusEntry>,
    synthetic: Vec<CorpusEntry>,
}

impl BenchCorpus {
    pub fn load() -> Result<Self, String> {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let workspace_root = workspace_root(&manifest_dir);
        let corpus_root = manifest_dir.join("benches").join("corpus");

        let stdlib = load_stdlib(&workspace_root, &corpus_root)?;
        let synthetic = load_synthetic(&corpus_root)?;

        Ok(Self { stdlib, synthetic })
    }

    pub fn stdlib(&self) -> &[CorpusEntry] {
        &self.stdlib
    }

    pub fn synthetic(&self) -> &[CorpusEntry] {
        &self.synthetic
    }

    pub fn synthetic_by_lines(&self, expected_lines: usize) -> Option<&CorpusEntry> {
        self.synthetic
            .iter()
            .find(|entry| entry.source.lines().count() == expected_lines)
    }
}

fn load_stdlib(workspace_root: &Path, corpus_root: &Path) -> Result<Vec<CorpusEntry>, String> {
    let list_path = corpus_root.join("stdlib.txt");
    let content = fs::read_to_string(&list_path)
        .map_err(|err| format!("cannot read {list_path:?}: {err}"))?;

    let mut entries = Vec::new();
    for line in content.lines().map(str::trim).filter(|l| !l.is_empty()) {
        let abs_path = workspace_root.join(line);
        let source = fs::read_to_string(&abs_path)
            .map_err(|err| format!("cannot read stdlib file {abs_path:?}: {err}"))?;
        entries.push(CorpusEntry {
            name: line.to_string(),
            source,
        });
    }
    Ok(entries)
}

fn load_synthetic(corpus_root: &Path) -> Result<Vec<CorpusEntry>, String> {
    let synthetic_dir = corpus_root.join("synthetic");
    let mut entries = Vec::new();
    let files = ["synthetic-100.jv", "synthetic-500.jv", "synthetic-2000.jv"];
    for file in files {
        let path = synthetic_dir.join(file);
        let source =
            fs::read_to_string(&path).map_err(|err| format!("cannot read {path:?}: {err}"))?;
        entries.push(CorpusEntry {
            name: file.to_string(),
            source,
        });
    }
    Ok(entries)
}

fn workspace_root(manifest_dir: &Path) -> PathBuf {
    manifest_dir
        .parent()
        .and_then(|p| p.parent())
        .map(PathBuf::from)
        .unwrap_or_else(|| manifest_dir.to_path_buf())
}

/// ベンチ用 Criterion 設定。
pub fn criterion_config() -> Criterion {
    Criterion::default()
        .warm_up_time(Duration::from_secs(3))
        .measurement_time(Duration::from_secs(10))
        .sample_size(20)
}

/// /proc/self/status から現在の RSS(KiB) を取得する。
pub fn current_rss_kb() -> Option<u64> {
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

/// 変更行数に基づいてキャッシュヒット率を概算する。
pub fn estimate_hit_rate(before: &str, after: &str) -> f64 {
    let total_lines = after.lines().count().max(1);
    let changed = before
        .lines()
        .zip(after.lines())
        .filter(|(l, r)| l != r)
        .count();
    let delta = changed as f64 / total_lines as f64;
    (1.0_f64 - delta).max(0.0)
}

/// ベンチで共有するハーネスとコーパスを取得する。
pub fn bench_state() -> (PipelineSwitcher, BenchCorpus) {
    let harness = PipelineSwitcher::new();
    let corpus = BenchCorpus::load().expect("corpus should load");
    (harness, corpus)
}
