use std::io::Write;

use anyhow::{Context, Result};
use serde_json::json;

use super::ReconstructedAst;
use super::diagnostics::{DiagnosticsSummary, format_span};
use jv_ast::Span;

#[derive(Debug, Default, Clone, Copy)]
pub struct AstArtifactWriter;

impl AstArtifactWriter {
    pub fn new() -> Self {
        Self
    }

    pub fn write_json<W: Write>(&self, writer: &mut W, artifact: &ReconstructedAst) -> Result<()> {
        let summary = DiagnosticsSummary::from_artifact(artifact);
        let program_value = serde_json::to_value(&artifact.program)
            .context("failed to serialize reconstructed AST program")?;

        let warnings_value = artifact
            .warnings
            .iter()
            .map(warning_to_json)
            .collect::<Vec<_>>();

        let stats = summary.stats;
        let stats_value = json!({
            "total_nodes": stats.total_nodes,
            "reconstructed_nodes": stats.reconstructed_nodes,
            "placeholder_nodes": stats.placeholder_nodes,
            "elapsed_millis": stats.elapsed.as_millis(),
        });

        let mut by_kind = Vec::new();
        for (kind, count) in summary.warning_summary.iter() {
            by_kind.push(json!({ "kind": kind, "count": count }));
        }
        let summary_value = json!({
            "total": summary.warning_summary.total(),
            "by_kind": by_kind,
        });

        let root = json!({
            "program": program_value,
            "warnings": warnings_value,
            "stats": stats_value,
            "summary": summary_value,
        });

        serde_json::to_writer_pretty(&mut *writer, &root)
            .context("failed to stream reconstructed artifact as JSON")?;
        writer
            .write_all(b"\n")
            .context("failed to finalize JSON output")?;
        Ok(())
    }

    pub fn write_pretty<W: Write>(
        &self,
        writer: &mut W,
        artifact: &ReconstructedAst,
    ) -> Result<()> {
        let summary = DiagnosticsSummary::from_artifact(artifact);

        writeln!(writer, "=== Reconstruction Stats ===")?;
        writeln!(writer, "total_nodes: {}", summary.stats.total_nodes)?;
        writeln!(
            writer,
            "reconstructed_nodes: {}",
            summary.stats.reconstructed_nodes
        )?;
        writeln!(
            writer,
            "placeholder_nodes: {}",
            summary.stats.placeholder_nodes
        )?;
        writeln!(writer, "elapsed_ms: {}", summary.stats.elapsed.as_millis())?;
        writeln!(writer)?;

        writeln!(writer, "=== Warning Summary ===")?;
        if summary.warning_summary.is_empty() {
            writeln!(writer, "total: 0")?;
            writeln!(writer, "- none")?;
        } else {
            writeln!(writer, "total: {}", summary.warning_summary.total())?;
            for (kind, count) in summary.warning_summary.iter() {
                writeln!(writer, "- {}: {}", kind, count)?;
            }
        }
        writeln!(writer)?;

        writeln!(
            writer,
            "=== Warnings ({}) ===",
            summary.warning_summary.total()
        )?;
        if summary.warning_summary.is_empty() {
            writeln!(writer, "(none)")?;
        } else {
            for (idx, warning) in summary.warnings.iter().enumerate() {
                let span_text = warning
                    .span
                    .as_ref()
                    .map(format_span)
                    .unwrap_or_else(|| "-".to_string());
                writeln!(
                    writer,
                    "{:>4}. [{}] {} - {} ({})",
                    idx + 1,
                    warning.kind,
                    warning.node_path,
                    warning.message,
                    span_text
                )?;
            }
        }
        writeln!(writer)?;

        writeln!(writer, "=== Program ===")?;
        let program_json = serde_json::to_string_pretty(&artifact.program)
            .context("failed to serialize reconstructed program for text output")?;
        for line in program_json.lines() {
            writeln!(writer, "{line}")?;
        }
        Ok(())
    }
}

fn warning_to_json(warning: &super::ReconstructionWarning) -> serde_json::Value {
    let span_value = warning.span.as_ref().map(span_to_json);
    json!({
        "kind": warning.kind.as_str(),
        "node_path": &warning.node_path,
        "message": &warning.message,
        "span": span_value,
    })
}

fn span_to_json(span: &Span) -> serde_json::Value {
    json!({
        "start_line": span.start_line,
        "start_column": span.start_column,
        "end_line": span.end_line,
        "end_column": span.end_column,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::debug::{ReconstructionWarning, WarningKind};
    use jv_ast::Program;
    use jv_ast::Span;
    use std::time::Duration;

    fn build_artifact() -> ReconstructedAst {
        let program = Program {
            package: Some("example".to_string()),
            imports: Vec::new(),
            statements: Vec::new(),
            span: Span::dummy(),
        };
        let mut artifact = ReconstructedAst::new(program);
        artifact.stats.total_nodes = 10;
        artifact.stats.reconstructed_nodes = 9;
        artifact.stats.placeholder_nodes = 1;
        artifact.stats.elapsed = Duration::from_millis(25);

        artifact.warnings.push(ReconstructionWarning::new(
            Some(Span::new(1, 0, 1, 5)),
            "program.module",
            WarningKind::MissingMetadata,
            "missing span metadata",
        ));
        artifact.warnings.push(ReconstructionWarning::new(
            None,
            "program.module.fn",
            WarningKind::UnsupportedNode,
            "unsupported construct",
        ));
        artifact
    }

    #[test]
    fn writes_json_artifact() {
        let artifact = build_artifact();
        let mut buffer = Vec::new();
        AstArtifactWriter::new()
            .write_json(&mut buffer, &artifact)
            .expect("json writing should succeed");

        let value: serde_json::Value = serde_json::from_slice(&buffer).expect("valid json");
        assert!(value.get("program").is_some());
        let warnings = value
            .get("warnings")
            .and_then(|v| v.as_array())
            .expect("warnings array");
        assert_eq!(warnings.len(), 2);
        let summary_total = value
            .get("summary")
            .and_then(|summary| summary.get("total"))
            .and_then(|total| total.as_u64())
            .expect("summary total");
        assert_eq!(summary_total, 2);
    }

    #[test]
    fn writes_pretty_text() {
        let artifact = build_artifact();
        let mut buffer = Vec::new();
        AstArtifactWriter::new()
            .write_pretty(&mut buffer, &artifact)
            .expect("text writing should succeed");

        let output = String::from_utf8(buffer).expect("utf8");
        assert!(output.contains("=== Reconstruction Stats ==="));
        assert!(output.contains("- missing-metadata: 1"));
        assert!(output.contains("[unsupported-node]"));
        assert!(output.contains("\"package\": \"example\""));
    }
}
