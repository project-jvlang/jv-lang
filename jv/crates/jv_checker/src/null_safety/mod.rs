mod annotations;
mod boundary;
mod context;
mod diagnostics;
mod flow;
mod graph;
mod operators;
mod pattern_bridge;
mod patterns;

use crate::inference::nullability::NullabilityAnalyzer;
use crate::pattern::PatternMatchService;
use crate::{CheckError, InferenceSnapshot};
use boundary::BoundaryChecker;
use diagnostics::DiagnosticsEmitter;
use flow::{FlowSolver, build_graph};
use jv_ast::Program;
use jv_inference::service::TypeFactsSnapshot;
pub use operators::{JavaLoweringHint, JavaLoweringStrategy};
use pattern_bridge::PatternFactsBridge;

use std::time::Instant;

pub use annotations::JavaNullabilityHint;
pub use context::{NullSafetyContext, NullabilityKind, NullabilityLattice};

/// Aggregated outcome of the null safety pipeline.
#[derive(Default)]
pub struct NullSafetyReport {
    diagnostics: Vec<CheckError>,
    warnings: Vec<CheckError>,
    java_hints: Vec<JavaLoweringHint>,
    type_facts: Option<TypeFactsSnapshot>,
    pattern_bridge_duration_ms: Option<f64>,
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
            pattern_bridge_duration_ms: None,
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

    pub fn take_warnings(&mut self) -> Vec<CheckError> {
        std::mem::take(&mut self.warnings)
    }

    pub fn set_type_facts(&mut self, snapshot: TypeFactsSnapshot) {
        self.type_facts = Some(snapshot);
    }

    pub fn take_type_facts(&mut self) -> Option<TypeFactsSnapshot> {
        self.type_facts.take()
    }

    pub fn set_pattern_bridge_duration_ms(&mut self, duration: f64) {
        self.pattern_bridge_duration_ms = Some(duration);
    }

    pub fn pattern_bridge_duration_ms(&self) -> Option<f64> {
        self.pattern_bridge_duration_ms
    }
}

/// Coordinates context hydration, pattern bridging, flow analysis, and diagnostics emission.
pub struct NullSafetyCoordinator<'service> {
    snapshot: Option<InferenceSnapshot>,
    pattern_service: Option<&'service mut PatternMatchService>,
}

impl<'service> NullSafetyCoordinator<'service> {
    pub fn new(
        snapshot: Option<InferenceSnapshot>,
        pattern_service: Option<&'service mut PatternMatchService>,
    ) -> Self {
        Self {
            snapshot,
            pattern_service,
        }
    }

    pub fn run(&mut self, program: &Program) -> NullSafetyReport {
        let mut report = NullSafetyReport::new();
        let snapshot_ref = self.snapshot.as_ref();
        let mut context = NullSafetyContext::hydrate(snapshot_ref);

        let mut bridge_duration = None;

        if let Some(service) = self.pattern_service.as_deref_mut() {
            let mut bridge = PatternFactsBridge::new();
            let started = Instant::now();
            let outcome = bridge.apply_program(program, service, &mut context);
            bridge_duration = Some(started.elapsed().as_secs_f64() * 1_000.0);
            if !outcome.diagnostics.is_empty() {
                report.extend_diagnostics(outcome.diagnostics);
            }
        }

        let graph = build_graph(program, &mut context);
        let analysis = FlowSolver::new(&graph, &context).solve();
        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&analysis);
        report.extend_diagnostics(payload.errors);
        for warning in payload.warnings {
            report.push_warning(warning);
        }
        for warning in BoundaryChecker::new(&context).evaluate(&analysis) {
            report.push_warning(warning);
        }
        if let Some(facts) = payload.facts {
            report.set_type_facts(facts);
        }
        report.extend_java_hints(analysis.java_hints);
        report.extend_diagnostics(NullabilityAnalyzer::analyze(program));
        if let Some(duration) = bridge_duration {
            report.set_pattern_bridge_duration_ms(duration);
        }
        report
    }
}
