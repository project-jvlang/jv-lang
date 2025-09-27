use std::collections::BTreeMap;

use super::{ReconstructedAst, ReconstructionStats, ReconstructionWarning};
use jv_ast::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WarningSummary {
    total: usize,
    by_kind: BTreeMap<String, usize>,
}

impl WarningSummary {
    pub fn from_warnings(warnings: &[ReconstructionWarning]) -> Self {
        let mut by_kind = BTreeMap::new();
        for warning in warnings {
            let key = warning.kind.as_str().to_string();
            *by_kind.entry(key).or_default() += 1;
        }

        Self {
            total: warnings.len(),
            by_kind,
        }
    }

    pub fn total(&self) -> usize {
        self.total
    }

    pub fn is_empty(&self) -> bool {
        self.total == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &usize)> {
        self.by_kind.iter()
    }

    pub fn count_for(&self, kind: &str) -> usize {
        self.by_kind.get(kind).copied().unwrap_or(0)
    }
}

#[derive(Debug)]
pub struct DiagnosticsSummary<'a> {
    pub stats: &'a ReconstructionStats,
    pub warnings: &'a [ReconstructionWarning],
    pub warning_summary: WarningSummary,
}

impl<'a> DiagnosticsSummary<'a> {
    pub fn new(stats: &'a ReconstructionStats, warnings: &'a [ReconstructionWarning]) -> Self {
        Self {
            stats,
            warnings,
            warning_summary: WarningSummary::from_warnings(warnings),
        }
    }

    pub fn from_artifact(artifact: &'a ReconstructedAst) -> Self {
        Self::new(&artifact.stats, &artifact.warnings)
    }
}

pub fn format_span(span: &Span) -> String {
    format!(
        "{}:{}-{}:{}",
        span.start_line, span.start_column, span.end_line, span.end_column
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::debug::{ReconstructionWarning, WarningKind};

    fn span() -> Span {
        Span::new(1, 2, 3, 4)
    }

    #[test]
    fn summarizes_warning_counts() {
        let warnings = vec![
            ReconstructionWarning::new(
                Some(span()),
                "root",
                WarningKind::MissingMetadata,
                "missing",
            ),
            ReconstructionWarning::new(None, "root", WarningKind::MissingMetadata, "still missing"),
            ReconstructionWarning::new(
                Some(span()),
                "other",
                WarningKind::UnsupportedNode,
                "unsupported",
            ),
        ];

        let summary = WarningSummary::from_warnings(&warnings);
        assert_eq!(summary.total(), 3);
        assert_eq!(summary.count_for("missing-metadata"), 2);
        assert_eq!(summary.count_for("unsupported-node"), 1);
        assert!(summary.count_for("placeholder-injected") == 0);

        let collected: Vec<_> = summary.iter().collect();
        assert_eq!(collected.len(), 2);
    }

    #[test]
    fn formats_span_consistently() {
        let formatted = format_span(&span());
        assert_eq!(formatted, "1:2-3:4");
    }
}
