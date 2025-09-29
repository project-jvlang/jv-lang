mod context;

use crate::inference::nullability::NullabilityAnalyzer;
use crate::{CheckError, InferenceSnapshot};
use jv_ast::Program;

pub use context::{NullSafetyContext, NullabilityKind, NullabilityLattice};

/// Aggregated outcome of the null safety pipeline.
#[derive(Default)]
pub struct NullSafetyReport {
    diagnostics: Vec<CheckError>,
    warnings: Vec<CheckError>,
}

impl NullSafetyReport {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_diagnostics(diagnostics: Vec<CheckError>) -> Self {
        Self {
            diagnostics,
            warnings: Vec::new(),
        }
    }

    pub fn diagnostics(&self) -> &[CheckError] {
        &self.diagnostics
    }

    pub fn warnings(&self) -> &[CheckError] {
        &self.warnings
    }

    pub fn into_diagnostics(self) -> Vec<CheckError> {
        self.diagnostics
    }

    pub fn push_warning(&mut self, warning: CheckError) {
        self.warnings.push(warning);
    }

    pub fn extend_diagnostics<I>(&mut self, diagnostics: I)
    where
        I: IntoIterator<Item = CheckError>,
    {
        self.diagnostics.extend(diagnostics);
    }
}

/// Coordinates context hydration, flow analysis, and diagnostics emission.
pub struct NullSafetyCoordinator<'snapshot> {
    snapshot: Option<&'snapshot InferenceSnapshot>,
}

impl<'snapshot> NullSafetyCoordinator<'snapshot> {
    pub fn new(snapshot: Option<&'snapshot InferenceSnapshot>) -> Self {
        Self { snapshot }
    }

    pub fn run(&self, program: &Program) -> NullSafetyReport {
        let mut report = NullSafetyReport::new();
        let context = NullSafetyContext::hydrate(self.snapshot);

        if context.is_degraded() {
            report.push_warning(CheckError::NullSafetyError(
                "型推論スナップショットが見つからないため、null安全解析を簡易モードで実行しました。".into(),
            ));
        }

        report.extend_diagnostics(NullabilityAnalyzer::analyze(program));
        report
    }
}
