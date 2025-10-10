use crate::types::{SymbolId, TypeId};
use jv_ast::types::{Kind, KindConstraint as AstKindConstraint};
use jv_ast::Span;
use std::collections::HashMap;

/// Kind information registered for a generic parameter or constructor symbol.
#[derive(Debug, Clone, Default)]
struct ParameterEntry {
    name: Option<String>,
    annotation: Option<Kind>,
}

/// Shared environment describing known parameter annotations and constructor kinds.
#[derive(Debug, Clone, Default)]
pub struct KindEnvironment {
    parameters: HashMap<TypeId, ParameterEntry>,
    constructors: HashMap<SymbolId, Kind>,
}

impl KindEnvironment {
    /// Registers a generic parameter with an optional kind annotation.
    pub fn declare_parameter(
        &mut self,
        parameter: TypeId,
        name: Option<String>,
        annotation: Option<Kind>,
    ) {
        let entry = self.parameters.entry(parameter).or_default();
        if entry.name.is_none() {
            entry.name = name;
        }
        if entry.annotation.is_none() {
            entry.annotation = annotation;
        }
    }

    /// Overrides the kind annotation associated with a parameter.
    pub fn update_parameter_annotation(&mut self, parameter: TypeId, annotation: Kind) {
        let entry = self.parameters.entry(parameter).or_default();
        entry.annotation = Some(annotation);
    }

    /// Registers the kind signature for a constructor/type symbol.
    pub fn register_constructor_kind(&mut self, symbol: SymbolId, kind: Kind) {
        self.constructors.insert(symbol, kind);
    }

    fn parameter_entry(&self, parameter: &TypeId) -> Option<&ParameterEntry> {
        self.parameters.get(parameter)
    }

    fn constructor_kind(&self, symbol: &SymbolId) -> Option<&Kind> {
        self.constructors.get(symbol)
    }
}

/// Argument supplied to a higher-kinded type slot.
#[derive(Debug, Clone, PartialEq)]
pub enum KindArgument {
    /// Explicit kind expression supplied by builders/tests.
    Kind(Kind),
    /// Reference to a previously registered constructor symbol.
    Constructor(SymbolId),
    /// Reference to another generic parameter whose kind should be reused.
    Parameter(TypeId),
}

/// Constraint representing a kind check for a specific application.
#[derive(Debug, Clone, PartialEq)]
pub struct KindConstraint {
    pub owner: SymbolId,
    pub parameter: TypeId,
    pub argument: KindArgument,
    pub span: Span,
}

impl KindConstraint {
    /// Creates a new kind application constraint.
    pub fn application(
        owner: impl Into<SymbolId>,
        parameter: TypeId,
        argument: KindArgument,
        span: Span,
    ) -> Self {
        Self {
            owner: owner.into(),
            parameter,
            argument,
            span,
        }
    }
}

#[derive(Debug, Clone, Default)]
struct ParameterState {
    name: Option<String>,
    annotation: Option<Kind>,
    resolved: Option<Kind>,
}

impl ParameterState {
    fn from_entry(entry: Option<&ParameterEntry>) -> Self {
        if let Some(entry) = entry {
            Self {
                name: entry.name.clone(),
                annotation: entry.annotation.clone(),
                resolved: entry.annotation.clone(),
            }
        } else {
            Self::default()
        }
    }

    fn expected(&self) -> Option<&Kind> {
        self.resolved.as_ref().or(self.annotation.as_ref())
    }
}

/// Diagnostic emitted when a kind mismatch is detected.
#[derive(Debug, Clone, PartialEq)]
pub struct KindSolverDiagnostic {
    pub owner: SymbolId,
    pub parameter: TypeId,
    pub parameter_name: Option<String>,
    pub expected: Kind,
    pub actual: Kind,
    pub span: Span,
}

/// Telemetry collected while evaluating kind constraints.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct KindSolverTelemetry {
    pub checks: u64,
    pub cache_hits: u64,
}

/// Result of running the kind solver for a batch of constraints.
#[derive(Debug, Clone, PartialEq)]
pub struct KindSolution {
    assignments: HashMap<TypeId, Kind>,
    diagnostics: Vec<KindSolverDiagnostic>,
    telemetry: KindSolverTelemetry,
}

impl KindSolution {
    pub fn assignments(&self) -> &HashMap<TypeId, Kind> {
        &self.assignments
    }

    pub fn diagnostics(&self) -> &[KindSolverDiagnostic] {
        &self.diagnostics
    }

    pub fn telemetry(&self) -> &KindSolverTelemetry {
        &self.telemetry
    }

    pub fn into_parts(
        self,
    ) -> (
        HashMap<TypeId, Kind>,
        Vec<KindSolverDiagnostic>,
        KindSolverTelemetry,
    ) {
        (self.assignments, self.diagnostics, self.telemetry)
    }
}

/// Kind solver limited to Phase 2 single-level higher-kinded types.
#[derive(Debug, Default)]
pub struct KindSolver;

impl KindSolver {
    pub fn new() -> Self {
        Self
    }

    /// Evaluates the provided constraints against the registered environment.
    pub fn solve(
        &self,
        environment: &KindEnvironment,
        constraints: &[KindConstraint],
    ) -> KindSolution {
        let mut states: HashMap<TypeId, ParameterState> = HashMap::new();
        for (parameter, entry) in &environment.parameters {
            states.insert(*parameter, ParameterState::from_entry(Some(entry)));
        }

        let mut diagnostics = Vec::new();
        let mut checks: u64 = 0;

        for constraint in constraints {
            states.entry(constraint.parameter).or_insert_with(|| {
                ParameterState::from_entry(environment.parameter_entry(&constraint.parameter))
            });

            let argument_kind = resolve_argument_kind(&states, environment, &constraint.argument);

            let Some(actual_kind) = argument_kind else {
                // Unable to determine the argument kind; defer until more information is available.
                continue;
            };

            checks += 1;

            let state = states
                .get_mut(&constraint.parameter)
                .expect("parameter state should exist");

            if let Some(expected_kind) = state.expected() {
                if !kinds_compatible(expected_kind, &actual_kind) {
                    diagnostics.push(KindSolverDiagnostic {
                        owner: constraint.owner.clone(),
                        parameter: constraint.parameter,
                        parameter_name: state.name.clone(),
                        expected: expected_kind.clone(),
                        actual: actual_kind.clone(),
                        span: constraint.span.clone(),
                    });
                }
            } else {
                state.resolved = Some(actual_kind.clone());
            }
        }

        let mut assignments = HashMap::new();
        for (parameter, state) in states {
            let resolved = state
                .resolved
                .or(state.annotation)
                .unwrap_or_else(|| Kind::Star);
            assignments.insert(parameter, resolved);
        }

        KindSolution {
            assignments,
            diagnostics,
            telemetry: KindSolverTelemetry {
                checks,
                cache_hits: 0,
            },
        }
    }
}

fn resolve_argument_kind(
    states: &HashMap<TypeId, ParameterState>,
    environment: &KindEnvironment,
    argument: &KindArgument,
) -> Option<Kind> {
    match argument {
        KindArgument::Kind(kind) => Some(kind.clone()),
        KindArgument::Constructor(symbol) => environment
            .constructor_kind(symbol)
            .cloned()
            .or_else(|| Some(Kind::Star)),
        KindArgument::Parameter(parameter) => states
            .get(parameter)
            .and_then(|state| state.expected().cloned()),
    }
}

fn kinds_compatible(expected: &Kind, actual: &Kind) -> bool {
    let expected_shape = KindShape::from(expected);
    let actual_shape = KindShape::from(actual);
    expected_shape == actual_shape
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum KindShape {
    Star,
    Arrow(Box<KindShape>, Box<KindShape>),
    Higher(Vec<KindShape>, Box<KindShape>),
}

impl From<&Kind> for KindShape {
    fn from(kind: &Kind) -> Self {
        match kind {
            Kind::Star => KindShape::Star,
            Kind::Arrow { parameter, result } => KindShape::Arrow(
                Box::new(KindShape::from(strip_constraints(parameter))),
                Box::new(KindShape::from(strip_constraints(result))),
            ),
            Kind::Higher { parameters, result } => {
                let params = parameters
                    .iter()
                    .map(|k| KindShape::from(strip_constraints(k)))
                    .collect();
                KindShape::Higher(params, Box::new(KindShape::from(strip_constraints(result))))
            }
            Kind::Constraint { base, .. } => KindShape::from(strip_constraints(base)),
        }
    }
}

fn strip_constraints(kind: &Kind) -> &Kind {
    match kind {
        Kind::Constraint { base, .. } => strip_constraints(base),
        other => other,
    }
}

/// Formats a kind into a human-readable signature (e.g. `* -> *`).
pub fn describe_kind(kind: &Kind) -> String {
    match kind {
        Kind::Star => "*".to_string(),
        Kind::Arrow { parameter, result } => {
            format!("{} -> {}", describe_kind(parameter), describe_kind(result))
        }
        Kind::Higher { parameters, result } => {
            let params = parameters
                .iter()
                .map(describe_kind)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({}) -> {}", params, describe_kind(result))
        }
        Kind::Constraint { base, constraints } => {
            let base_desc = describe_kind(base);
            if constraints.is_empty() {
                base_desc
            } else {
                let parts = constraints
                    .iter()
                    .map(|constraint: &AstKindConstraint| constraint.trait_name.qualified())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} where {}", base_desc, parts)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span() -> Span {
        Span::new(1, 0, 1, 5)
    }

    fn arrow(param: Kind, result: Kind) -> Kind {
        Kind::Arrow {
            parameter: Box::new(param),
            result: Box::new(result),
        }
    }

    #[test]
    fn infers_kind_for_unannotated_parameter() {
        let mut env = KindEnvironment::default();
        let param = TypeId::new(1);
        env.declare_parameter(param, Some("F".to_string()), None);

        let list_kind = arrow(Kind::Star, Kind::Star);
        let constraints = vec![KindConstraint::application(
            SymbolId::from("pkg::Functor"),
            param,
            KindArgument::Kind(list_kind.clone()),
            span(),
        )];

        let solver = KindSolver::new();
        let solution = solver.solve(&env, &constraints);

        assert_eq!(solution.telemetry().checks, 1);
        assert_eq!(solution.telemetry().cache_hits, 0);
        assert_eq!(solution.diagnostics().len(), 0);
        assert_eq!(solution.assignments().get(&param), Some(&list_kind));
    }

    #[test]
    fn detects_kind_mismatch() {
        let mut env = KindEnvironment::default();
        let param = TypeId::new(2);
        env.declare_parameter(
            param,
            Some("F".to_string()),
            Some(arrow(Kind::Star, Kind::Star)),
        );

        let constraints = vec![KindConstraint::application(
            SymbolId::from("pkg::Functor"),
            param,
            KindArgument::Kind(Kind::Star),
            span(),
        )];

        let solver = KindSolver::new();
        let solution = solver.solve(&env, &constraints);

        assert_eq!(solution.telemetry().checks, 1);
        assert_eq!(solution.telemetry().cache_hits, 0);
        assert_eq!(solution.diagnostics().len(), 1);
        let diagnostic = &solution.diagnostics()[0];
        assert_eq!(diagnostic.owner, SymbolId::from("pkg::Functor"));
        assert_eq!(diagnostic.parameter, param);
        assert_eq!(describe_kind(&diagnostic.expected), "* -> *");
        assert_eq!(describe_kind(&diagnostic.actual), "*");
    }

    #[test]
    fn reuses_constructor_kinds() {
        let mut env = KindEnvironment::default();
        let param = TypeId::new(3);
        env.declare_parameter(
            param,
            Some("F".to_string()),
            Some(arrow(Kind::Star, Kind::Star)),
        );
        env.register_constructor_kind(SymbolId::from("pkg::List"), arrow(Kind::Star, Kind::Star));

        let constraints = vec![KindConstraint::application(
            SymbolId::from("pkg::Functor"),
            param,
            KindArgument::Constructor(SymbolId::from("pkg::List")),
            span(),
        )];

        let solver = KindSolver::new();
        let solution = solver.solve(&env, &constraints);

        assert_eq!(solution.diagnostics().len(), 0);
        assert_eq!(solution.telemetry().checks, 1);
        assert_eq!(solution.telemetry().cache_hits, 0);
    }
}

use crate::solver::GenericSolverDiagnostic;

impl From<KindSolverDiagnostic> for GenericSolverDiagnostic {
    fn from(value: KindSolverDiagnostic) -> Self {
        GenericSolverDiagnostic::KindMismatch {
            symbol: value.owner,
            parameter: value.parameter,
            parameter_name: value.parameter_name,
            expected: value.expected,
            actual: value.actual,
            span: value.span,
        }
    }
}
