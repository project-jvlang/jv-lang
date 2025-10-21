use std::path::{Path, PathBuf};

use serde::Deserialize;

use super::HarnessError;

/// 仕様フィクスチャのルート構造。
#[derive(Debug)]
pub struct FixtureSpec {
    /// フィクスチャ定義ファイルへのパス。
    pub file_path: PathBuf,
    /// 任意の説明文。
    pub description: Option<String>,
    /// 入力となる `.jv` ソースファイルへの絶対パス。
    pub source_path: PathBuf,
    /// 期待値定義。
    pub expect: ExpectationSpec,
}

/// `toml` から読み込む前段階のフィクスチャ構造。
#[derive(Debug, Deserialize)]
struct RawFixtureSpec {
    #[serde(default)]
    description: Option<String>,
    source: String,
    #[serde(default)]
    expect: ExpectationSpec,
}

impl FixtureSpec {
    /// フィクスチャ定義を読み込む。
    pub fn load(path: &Path, workspace_root: &Path) -> Result<Self, HarnessError> {
        let content =
            std::fs::read_to_string(path).map_err(|source| HarnessError::ReadFixture {
                path: path.to_path_buf(),
                source,
            })?;

        let raw: RawFixtureSpec =
            toml::from_str(&content).map_err(|source| HarnessError::ParseFixture {
                path: path.to_path_buf(),
                source,
            })?;

        let source_path = resolve_source_path(workspace_root, path, &raw.source)?;

        Ok(Self {
            file_path: path.to_path_buf(),
            description: raw.description,
            source_path,
            expect: raw.expect,
        })
    }
}

fn resolve_source_path(
    workspace_root: &Path,
    fixture_path: &Path,
    source: &str,
) -> Result<PathBuf, HarnessError> {
    let candidate = Path::new(source);
    if candidate.is_absolute() {
        return Ok(candidate.to_path_buf());
    }

    let by_fixture = fixture_path
        .parent()
        .map(|parent| parent.join(candidate))
        .unwrap_or_else(|| candidate.to_path_buf());
    if by_fixture.exists() {
        return Ok(by_fixture);
    }

    let from_workspace = workspace_root.join(candidate);
    if from_workspace.exists() {
        return Ok(from_workspace);
    }

    Err(HarnessError::SourceNotFound {
        path: candidate.to_path_buf(),
        fixture: fixture_path.to_path_buf(),
    })
}

/// 期待値定義。
#[derive(Debug, Clone, Deserialize, Default)]
#[serde(default)]
pub struct ExpectationSpec {
    /// 期待するトップレベルステートメント数。
    pub statement_count: Option<usize>,
    /// パーサ診断の許容条件。
    pub parser_diagnostics: DiagnosticsExpectation,
    /// ローワリング診断の許容条件。
    pub lowering_diagnostics: DiagnosticsExpectation,
    /// 各ステートメントに対する検証ルール。
    pub statements: Vec<StatementExpectation>,
}

/// 診断の許容条件。
#[derive(Debug, Clone, Deserialize)]
#[serde(default)]
pub struct DiagnosticsExpectation {
    #[serde(default)]
    /// 診断を許容するかどうか。
    pub allowed: bool,
    #[serde(default)]
    /// 許容する診断数の上限（None の場合は無制限）。
    pub max: Option<usize>,
}

impl Default for DiagnosticsExpectation {
    fn default() -> Self {
        Self {
            allowed: false,
            max: Some(0),
        }
    }
}

impl DiagnosticsExpectation {
    /// 実際の診断件数が期待値を満たしているか判定する。
    pub fn validate(&self, actual: usize) -> Option<String> {
        if !self.allowed {
            if actual == 0 {
                None
            } else {
                Some(format!("0 diagnostic(s) expected but got {}", actual))
            }
        } else if let Some(max) = self.max {
            if actual > max {
                Some(format!(
                    "expected at most {} diagnostic(s) but got {}",
                    max, actual
                ))
            } else {
                None
            }
        } else {
            None
        }
    }
}

/// ステートメント単位の期待値。
#[derive(Debug, Clone, Deserialize)]
pub struct StatementExpectation {
    /// 対象とするステートメントのインデックス。
    pub index: usize,
    /// 期待するステートメント種別。
    pub kind: StatementKindKey,
    #[serde(default)]
    /// 期待する名前（該当する場合）。
    pub name: Option<String>,
    #[serde(default)]
    /// スパンに関する期待値。
    pub span: Option<SpanExpectation>,
    #[serde(default)]
    /// ブロック内ステートメントに関する期待値。
    pub block: Option<BlockExpectation>,
}

/// ブロック内ステートメントの期待値。
#[derive(Debug, Clone, Deserialize, Default)]
#[serde(default)]
pub struct BlockExpectation {
    /// ブロックに出現するステートメント種別の列。
    pub statement_kinds: Vec<StatementKindKey>,
}

/// スパンの期待値。
#[derive(Debug, Clone, Deserialize, Default)]
#[serde(default)]
pub struct SpanExpectation {
    /// 開始行。
    pub start_line: Option<usize>,
    /// 開始列。
    pub start_column: Option<usize>,
    /// 終了行。
    pub end_line: Option<usize>,
    /// 終了列。
    pub end_column: Option<usize>,
}

/// ステートメント種別を人間可読な形で表現するキー。
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum StatementKindKey {
    Assignment,
    BreakStatement,
    ClassDeclaration,
    Comment,
    Concurrency,
    ContinueStatement,
    DataClassDeclaration,
    ExtensionFunction,
    Expression,
    ForIn,
    FunctionDeclaration,
    Import,
    InterfaceDeclaration,
    Package,
    ResourceManagement,
    ReturnStatement,
    ThrowStatement,
    ValDeclaration,
    VarDeclaration,
    WhenStatement,
    IfStatement,
    Unknown,
}
