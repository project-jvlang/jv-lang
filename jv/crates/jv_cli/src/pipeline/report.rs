use anyhow::{Context, Result};
use chrono::Utc;
use jv_build::{
    CompatibilityEvidence, CompatibilityFinding, CompatibilityReport, CompatibilityStatus,
    DetectedVersion, JavaTarget,
};
use jv_pm::LoggingConfig;
use serde::Serialize;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct RenderedCompatibilityReport {
    pub table: String,
    pub summary: String,
    pub json_path: PathBuf,
    pub status: CompatibilityStatus,
    pub target: JavaTarget,
}

pub fn render_logging_overview(config: &LoggingConfig) -> String {
    let mut lines = Vec::new();
    lines.push("ロギング設定 / Logging Configuration".to_string());
    lines.push(format!("  フレームワーク Framework : {}", config.framework));
    lines.push(format!("  現在のログレベル Current : {}", config.log_level));
    lines.push(format!(
        "  既定ログレベル Default : {}",
        config.default_level
    ));

    let otel = &config.opentelemetry;
    lines.push(format!(
        "  OpenTelemetry 有効化     : {}",
        bool_with_label(otel.enabled)
    ));
    lines.push(format!(
        "    endpoint              : {}",
        otel.endpoint
            .as_deref()
            .filter(|value| !value.is_empty())
            .unwrap_or("-")
    ));
    lines.push(format!("    protocol              : {}", otel.protocol));
    lines.push(format!(
        "    trace_context         : {}",
        bool_with_label(otel.trace_context)
    ));
    lines.push(format!(
        "    resource entries      : {}",
        otel.resource.len()
    ));
    lines.push(format!(
        "    attribute entries     : {}",
        otel.attributes.len()
    ));

    lines.join("\n")
}

pub fn render(
    report: &CompatibilityReport,
    output_dir: &Path,
) -> Result<RenderedCompatibilityReport> {
    let table = render_table(report);
    let summary = render_summary(report);
    let json_path = write_json(report, output_dir)?;

    Ok(RenderedCompatibilityReport {
        table,
        summary,
        json_path,
        status: report.status,
        target: report.target,
    })
}

fn render_table(report: &CompatibilityReport) -> String {
    let mut lines = Vec::new();
    lines.push(format!("ターゲット Target: Java{}", report.target));
    lines.push(format!("検出結果 Findings: {}", report.findings.len()));
    lines.push(String::from(
        "| Artifact / アーティファクト | Version (種別) | Release | Evidence / 根拠 |",
    ));
    lines.push(String::from(
        "|-----------------------------|------------------|---------|------------------|",
    ));

    if report.findings.is_empty() {
        lines.push(String::from("| (no findings) | - | - | - |"));
    } else {
        for finding in &report.findings {
            lines.push(render_row(finding));
        }
    }

    if report.warnings.is_empty() {
        lines.push("警告なし / No compatibility warnings".to_string());
    } else {
        lines.push("Warnings:".to_string());
        for warning in &report.warnings {
            lines.push(format!("  - {}", warning));
        }
    }

    lines.join("\n")
}

fn render_row(finding: &CompatibilityFinding) -> String {
    let (version_label, release_label) = version_labels(finding.version);
    let evidence_label = evidence_label(finding.evidence);
    format!(
        "| {} | {} | {} | {} |",
        finding.artifact, version_label, release_label, evidence_label
    )
}

fn render_summary(report: &CompatibilityReport) -> String {
    let (status_jp, status_en) = status_labels(report.status);
    let mut lines = Vec::new();
    lines.push("互換性サマリ / Compatibility Summary".to_string());
    lines.push(format!("  ターゲット Target : Java{}", report.target));
    lines.push(format!(
        "  ステータス Status : {} ({})",
        status_jp, status_en
    ));

    match report.highest_required_major {
        Some(major) => {
            let release = report
                .required_release()
                .map(|release| format!("Java {}", release))
                .unwrap_or_else(|| format!("major {}", major));
            lines.push(format!("  要求バージョン Required : {}", release));
        }
        None => lines.push(String::from("  要求バージョン Required : -")),
    }

    lines.push(format!("  警告 Warnings : {}", report.warnings.len()));

    lines.join("\n")
}

fn write_json(report: &CompatibilityReport, output_dir: &Path) -> Result<PathBuf> {
    std::fs::create_dir_all(output_dir).with_context(|| {
        format!(
            "Failed to prepare output directory for compatibility report: {}",
            output_dir.display()
        )
    })?;

    let path = output_dir.join("compatibility.json");
    let file = File::create(&path).with_context(|| {
        format!(
            "Failed to create compatibility report JSON at {}",
            path.display()
        )
    })?;
    let mut writer = BufWriter::new(file);
    let json = JsonCompatibilityReport::from(report);

    serde_json::to_writer_pretty(&mut writer, &json).with_context(|| {
        format!(
            "Failed to serialize compatibility report JSON at {}",
            path.display()
        )
    })?;
    writer.flush().with_context(|| {
        format!(
            "Failed to flush compatibility report JSON at {}",
            path.display()
        )
    })?;

    Ok(path)
}

fn version_labels(version: DetectedVersion) -> (String, String) {
    match version {
        DetectedVersion::Release(release) => (
            format!("Java {} (release)", release),
            format!("Java {}", release),
        ),
        DetectedVersion::Major(major) => (
            format!("major {} (bytecode)", major),
            version
                .as_release()
                .map(|release| format!("Java {}", release))
                .unwrap_or_else(|| "-".to_string()),
        ),
    }
}

fn evidence_label(evidence: CompatibilityEvidence) -> &'static str {
    match evidence {
        CompatibilityEvidence::Manifest => "Manifest / マニフェスト",
        CompatibilityEvidence::Bytecode => "Bytecode / バイトコード",
    }
}

fn status_labels(status: CompatibilityStatus) -> (&'static str, &'static str) {
    match status {
        CompatibilityStatus::Compatible => ("互換性 OK", "Compatible"),
        CompatibilityStatus::RequiresHigherTarget { .. } => {
            ("ターゲット不足", "Requires higher target")
        }
    }
}

fn bool_with_label(value: bool) -> String {
    if value {
        "有効(true)".to_string()
    } else {
        "無効(false)".to_string()
    }
}

#[derive(Serialize)]
struct JsonCompatibilityReport {
    target: String,
    target_release: &'static str,
    status: JsonStatus,
    highest_required_major: Option<u16>,
    highest_required_release: Option<u16>,
    warnings: Vec<String>,
    findings: Vec<JsonFinding>,
    generated_at: String,
}

impl From<&CompatibilityReport> for JsonCompatibilityReport {
    fn from(report: &CompatibilityReport) -> Self {
        let status = JsonStatus::from(report.status);
        let findings = report.findings.iter().map(JsonFinding::from).collect();

        Self {
            target: report.target.to_string(),
            target_release: report.target.release_flag(),
            status,
            highest_required_major: report.highest_required_major,
            highest_required_release: report.required_release(),
            warnings: report.warnings.clone(),
            findings,
            generated_at: Utc::now().to_rfc3339(),
        }
    }
}

#[derive(Serialize)]
struct JsonStatus {
    code: &'static str,
    label: &'static str,
}

impl From<CompatibilityStatus> for JsonStatus {
    fn from(status: CompatibilityStatus) -> Self {
        let (code, label) = match status {
            CompatibilityStatus::Compatible => ("compatible", "Compatible"),
            CompatibilityStatus::RequiresHigherTarget { .. } => {
                ("requires_higher_target", "Requires higher target")
            }
        };

        Self { code, label }
    }
}

#[derive(Serialize)]
struct JsonFinding {
    artifact: String,
    evidence: &'static str,
    version_major: Option<u16>,
    version_release: Option<u16>,
    version_label: String,
}

impl From<&CompatibilityFinding> for JsonFinding {
    fn from(finding: &CompatibilityFinding) -> Self {
        let version_major = finding.version.as_major();
        let version_release = finding.version.as_release();
        let (label, _) = version_labels(finding.version);
        let evidence = evidence_label(finding.evidence);

        Self {
            artifact: finding.artifact.clone(),
            evidence,
            version_major,
            version_release,
            version_label: label,
        }
    }
}
