pub mod logging;

/// 生成された構成ファイルを表す。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GeneratedArtifact {
    /// 出力ファイル名（例: `logback.xml`）。
    pub filename: &'static str,
    /// ファイル内容。UTF-8 テキストとして扱う。
    pub contents: String,
}
