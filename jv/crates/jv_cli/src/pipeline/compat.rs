use super::*;
use anyhow::Result;
use jv_build::{BuildSystem, CompatibilityReport, CompatibilityStatus};
use jv_checker::diagnostics::ToolingDiagnostic;
use std::path::Path;

pub fn preflight(build_system: &BuildSystem, input_path: &Path) -> Result<CompatibilityReport> {
    let report = build_system.analyze_compatibility()?;

    if let CompatibilityStatus::RequiresHigherTarget { required_major } = report.status {
        let required_label = report
            .required_release()
            .map(|release| format!("Java {}", release))
            .unwrap_or_else(|| format!("バイトコード major {}", required_major));

        let culprit = report
            .findings
            .iter()
            .find(|finding| finding.version.as_major() == Some(required_major))
            .or_else(|| report.findings.first());

        let artifact = culprit
            .map(|finding| finding.artifact.as_str())
            .unwrap_or("クラスパス項目");

        let diagnostic = ToolingDiagnostic {
            code: "JV2001",
            title: "依存ライブラリが要求する Java バージョンに未対応です",
            message: format!(
                "{} は {} 以上を必要とするため、ターゲット {} ではビルドできません。",
                artifact,
                required_label,
                report.target
            ),
            help: "jv.toml の [build].java_version または CLI オプション --target を必要なバージョンへ更新するか、互換性のある依存ライブラリへ置き換えてください。",
            span: None,
        };

        return Err(tooling_failure(input_path, diagnostic));
    }

    Ok(report)
}
