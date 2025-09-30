mod context;
mod diagnostics;
mod flow;
mod graph;
mod operators;
mod patterns;

use crate::inference::nullability::NullabilityAnalyzer;
use crate::{CheckError, InferenceSnapshot};
use diagnostics::DiagnosticsEmitter;
use flow::{build_graph, FlowSolver};
use jv_ast::Program;
use jv_inference::service::TypeFactsSnapshot;
pub use operators::{JavaLoweringHint, JavaLoweringStrategy};

pub use context::{NullSafetyContext, NullabilityKind, NullabilityLattice};

/// Aggregated outcome of the null safety pipeline.
#[derive(Default)]
pub struct NullSafetyReport {
    diagnostics: Vec<CheckError>,
    warnings: Vec<CheckError>,
    java_hints: Vec<JavaLoweringHint>,
    type_facts: Option<TypeFactsSnapshot>,
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
            type_facts: None,
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

    pub fn set_type_facts(&mut self, snapshot: TypeFactsSnapshot) {
        self.type_facts = Some(snapshot);
    }

    pub fn take_type_facts(&mut self) -> Option<TypeFactsSnapshot> {
        self.type_facts.take()
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
        let graph = build_graph(program);
        let analysis = FlowSolver::new(&graph, &context).solve();
        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&analysis);
        report.extend_diagnostics(payload.errors);
        for warning in payload.warnings {
            report.push_warning(warning);
        }
        if let Some(facts) = payload.facts {
            report.set_type_facts(facts);
        }
        report.extend_java_hints(analysis.java_hints);
        report.extend_diagnostics(NullabilityAnalyzer::analyze(program));
        report
    }
}
