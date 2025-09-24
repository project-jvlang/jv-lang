use super::*;
use anyhow::Result;
use jv_build::{BuildSystem, CompatibilityReport, CompatibilityStatus};
use jv_checker::compat::diagnostics as compat_diagnostics;
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

        let target_label = format!("Java{}", report.target);
        let diagnostic = compat_diagnostics::requires_higher_target(
            artifact,
            &required_label,
            &target_label,
        );

        return Err(tooling_failure(input_path, diagnostic));
    }

    Ok(report)
}
