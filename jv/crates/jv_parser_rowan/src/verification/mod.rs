//! 仕様ベースの AST 検証ハーネス。

mod fixtures;
mod rules;

pub use fixtures::{
    BlockExpectation, DiagnosticsExpectation, ExpectationSpec, FixtureSpec, SpanExpectation,
    StatementExpectation, StatementKindKey,
};
pub use rules::{
    apply_expectations, LoweringDiagnosticSummary, ParserDiagnosticSummary, RuleViolation,
    StatementSummary,
};

use crate::lowering::lower_program;
use crate::parser::parse;
use crate::{JvLanguage, ParseBuilder};
use fixtures::FixtureSpec as Fixture;
use jv_lexer::{LexError, Lexer};
use rowan::SyntaxNode;
use serde::Serialize;
use std::path::{Path, PathBuf};
use thiserror::Error;
use walkdir::WalkDir;

/// ハーネス実行時に発生し得るエラー。
#[derive(Debug, Error)]
pub enum HarnessError {
    /// ワークスペースルートを特定できなかった。
    #[error("failed to locate workspace root from manifest path")]
    WorkspaceRoot,
    /// 仕様ディレクトリが存在しない。
    #[error("spec directory not found: {path}")]
    SpecDirectoryMissing {
        /// 存在が確認できなかったディレクトリ。
        path: PathBuf,
    },
    /// 仕様ディレクトリの走査に失敗した。
    #[error("failed to traverse spec directory {path}: {source}")]
    Walkdir {
        /// 走査対象ディレクトリ。
        path: PathBuf,
        /// 発生した walkdir エラー。
        source: walkdir::Error,
    },
    /// フィクスチャ定義ファイルを読み込めなかった。
    #[error("failed to read fixture {path}: {source}")]
    ReadFixture {
        /// フィクスチャ定義ファイルのパス。
        path: PathBuf,
        /// I/O エラー内容。
        source: std::io::Error,
    },
    /// フィクスチャ定義ファイルの解析に失敗した。
    #[error("failed to parse fixture {path}: {source}")]
    ParseFixture {
        /// フィクスチャ定義ファイルのパス。
        path: PathBuf,
        /// TOML 解析エラー。
        source: toml::de::Error,
    },
    /// フィクスチャが参照するソースファイルが見つからなかった。
    #[error("source file not found: {path} (referenced from {fixture})")]
    SourceNotFound {
        /// 見つからなかったソースファイル。
        path: PathBuf,
        /// 参照元フィクスチャ。
        fixture: PathBuf,
    },
    /// ソースファイルの読み込みに失敗した。
    #[error("failed to read source {path}: {source}")]
    ReadSource {
        /// 読み込みに失敗したソースパス。
        path: PathBuf,
        /// I/O エラー内容。
        source: std::io::Error,
    },
    /// 字句解析に失敗した。
    #[error("lexing failed for {path}: {source}")]
    Lex {
        /// 字句解析対象パス。
        path: PathBuf,
        /// レキサエラー。
        source: LexError,
    },
    /// レポートの JSON 変換に失敗した。
    #[error("failed to serialize verification report: {source}")]
    SerializeReport {
        /// JSON 化に失敗した理由。
        source: serde_json::Error,
    },
    /// レポートを書き出せなかった。
    #[error("failed to write verification report {path}: {source}")]
    WriteReport {
        /// 書き込み先パス。
        path: PathBuf,
        /// I/O エラー内容。
        source: std::io::Error,
    },
}

/// ハーネスの実行結果全体。
#[derive(Debug, Serialize)]
pub struct HarnessReport {
    /// 実行した仕様ディレクトリ（ワークスペース相対パス）。
    pub spec_root: String,
    /// フィクスチャごとの検証結果。
    pub fixtures: Vec<FixtureReport>,
}

impl HarnessReport {
    /// 失敗したフィクスチャ数。
    pub fn failures(&self) -> usize {
        self.fixtures
            .iter()
            .filter(|fixture| !fixture.violations.is_empty())
            .count()
    }

    /// すべてのフィクスチャが成功したか。
    pub fn is_success(&self) -> bool {
        self.failures() == 0
    }

    /// レポートを指定パスへ書き出す。
    pub fn write_report<P: AsRef<Path>>(&self, path: P) -> Result<(), HarnessError> {
        let path = path.as_ref();
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|source| HarnessError::WriteReport {
                path: parent.to_path_buf(),
                source,
            })?;
        }
        let json = serde_json::to_string_pretty(self)
            .map_err(|source| HarnessError::SerializeReport { source })?;
        std::fs::write(path, json).map_err(|source| HarnessError::WriteReport {
            path: path.to_path_buf(),
            source,
        })
    }

    /// デフォルトのレポート出力先（`target/parser_rowan_specs/report.json`）。
    pub fn default_report_path(workspace_root: &Path) -> PathBuf {
        workspace_root
            .join("target")
            .join("parser_rowan_specs")
            .join("report.json")
    }
}

/// フィクスチャ単位の検証結果。
#[derive(Debug, Serialize)]
pub struct FixtureReport {
    /// フィクスチャ定義ファイル（ワークスペース相対パス）。
    pub spec: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// 任意の説明文。
    pub description: Option<String>,
    /// 入力ソースのパス（ワークスペース相対）。
    pub source: String,
    /// パーサがエラー回復を行ったかどうか。
    pub parser_recovered: bool,
    /// 発生したパーサ診断一覧。
    pub parser_diagnostics: Vec<ParserDiagnosticSummary>,
    /// 発生したローワリング診断一覧。
    pub lowering_diagnostics: Vec<LoweringDiagnosticSummary>,
    /// 生成されたステートメントサマリ。
    pub statements: Vec<StatementSummary>,
    /// 期待値違反の一覧。
    pub violations: Vec<RuleViolation>,
}

/// 指定ディレクトリ配下のフィクスチャを実行する。
pub fn run_spec_directory(dir: &Path) -> Result<HarnessReport, HarnessError> {
    let workspace_root = workspace_root()?;
    let resolved_dir = if dir.is_absolute() {
        dir.to_path_buf()
    } else {
        workspace_root.join(dir)
    };

    if !resolved_dir.exists() {
        return Err(HarnessError::SpecDirectoryMissing { path: resolved_dir });
    }

    let spec_files = collect_spec_files(&resolved_dir)?;
    let mut fixtures = Vec::new();

    for spec_path in spec_files {
        let fixture = Fixture::load(&spec_path, &workspace_root)?;
        let report = evaluate_fixture(&fixture, &workspace_root)?;
        fixtures.push(report);
    }

    let spec_root = relative_path(&resolved_dir, &workspace_root);

    Ok(HarnessReport {
        spec_root,
        fixtures,
    })
}

fn collect_spec_files(dir: &Path) -> Result<Vec<PathBuf>, HarnessError> {
    let mut files = Vec::new();
    for entry in WalkDir::new(dir).into_iter() {
        let entry = entry.map_err(|source| HarnessError::Walkdir {
            path: dir.to_path_buf(),
            source,
        })?;
        if entry.file_type().is_file()
            && entry
                .path()
                .extension()
                .map(|ext| ext == "toml")
                .unwrap_or(false)
        {
            files.push(entry.into_path());
        }
    }
    files.sort();
    Ok(files)
}

fn evaluate_fixture(
    fixture: &Fixture,
    workspace_root: &Path,
) -> Result<FixtureReport, HarnessError> {
    let source = std::fs::read_to_string(&fixture.source_path).map_err(|source| {
        HarnessError::ReadSource {
            path: fixture.source_path.clone(),
            source,
        }
    })?;

    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|source| HarnessError::Lex {
        path: fixture.source_path.clone(),
        source,
    })?;

    let parse_output = parse(&tokens);
    let green = ParseBuilder::build_from_events(&parse_output.events, &tokens);
    let syntax: SyntaxNode<JvLanguage> = SyntaxNode::new_root(green);
    let lowering = lower_program(&syntax, &tokens);

    let mut violations = rules::apply_expectations(&fixture.expect, &lowering.statements);

    if let Some(message) = fixture
        .expect
        .parser_diagnostics
        .validate(parse_output.diagnostics.len())
    {
        violations.push(RuleViolation {
            rule: "parser_diagnostics".to_string(),
            message,
        });
    }

    if let Some(message) = fixture
        .expect
        .lowering_diagnostics
        .validate(lowering.diagnostics.len())
    {
        violations.push(RuleViolation {
            rule: "lowering_diagnostics".to_string(),
            message,
        });
    }

    let parser_diagnostics = parse_output
        .diagnostics
        .iter()
        .map(ParserDiagnosticSummary::from)
        .collect();
    let lowering_diagnostics = lowering
        .diagnostics
        .iter()
        .map(LoweringDiagnosticSummary::from)
        .collect();
    let statements = lowering
        .statements
        .iter()
        .map(StatementSummary::from_statement)
        .collect();

    let spec = relative_path(&fixture.file_path, workspace_root);
    let source = relative_path(&fixture.source_path, workspace_root);

    Ok(FixtureReport {
        spec,
        description: fixture.description.clone(),
        source,
        parser_recovered: parse_output.recovered,
        parser_diagnostics,
        lowering_diagnostics,
        statements,
        violations,
    })
}

fn relative_path(path: &Path, workspace_root: &Path) -> String {
    path.strip_prefix(workspace_root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

fn workspace_root() -> Result<PathBuf, HarnessError> {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest
        .ancestors()
        .nth(3)
        .map(Path::to_path_buf)
        .ok_or(HarnessError::WorkspaceRoot)
}
