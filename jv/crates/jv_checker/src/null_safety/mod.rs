mod context;
mod flow;
mod graph;
mod operators;
mod patterns;

use crate::inference::nullability::NullabilityAnalyzer;
use crate::{CheckError, InferenceSnapshot};
use flow::{build_graph, FlowSolver};
use jv_ast::Program;
pub use operators::{JavaLoweringHint, JavaLoweringStrategy};

pub use context::{NullSafetyContext, NullabilityKind, NullabilityLattice};

/// Aggregated outcome of the null safety pipeline.
#[derive(Default)]
pub struct NullSafetyReport {
    diagnostics: Vec<CheckError>,
    warnings: Vec<CheckError>,
    java_hints: Vec<JavaLoweringHint>,
}

impl NullSafetyReport {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_diagnostics(diagnostics: Vec<CheckError>) -> Self {
        Self {
            diagnostics,
            warnings: Vec::new(),
            java_hints: Vec::new(),
        }
    }

    pub fn diagnostics(&self) -> &[CheckError] {
        &self.diagnostics
    }

    pub fn warnings(&self) -> &[CheckError] {
        &self.warnings
    }

    pub fn java_hints(&self) -> &[JavaLoweringHint] {
        &self.java_hints
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

    pub fn extend_java_hints<I>(&mut self, hints: I)
    where
        I: IntoIterator<Item = JavaLoweringHint>,
    {
        self.java_hints.extend(hints);
    }

    pub fn take_diagnostics(&mut self) -> Vec<CheckError> {
        std::mem::take(&mut self.diagnostics)
    }

    pub fn take_java_hints(&mut self) -> Vec<JavaLoweringHint> {
        std::mem::take(&mut self.java_hints)
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

        let graph = build_graph(program);
        let analysis = FlowSolver::new(&graph, &context).solve();
        report.extend_diagnostics(analysis.diagnostics);
        report.extend_java_hints(analysis.java_hints);
        report.extend_diagnostics(NullabilityAnalyzer::analyze(program));
        report
    }
}
