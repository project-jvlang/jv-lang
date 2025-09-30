use std::collections::{hash_map::Entry, HashMap};

use super::context::NullSafetyContext;
use super::flow::FlowAnalysisOutcome;
use super::NullabilityKind;
use crate::CheckError;
use jv_inference::service::{TypeFactsBuilder, TypeFactsSnapshot};
use jv_inference::types::{
    NullabilityFlag, TypeKind as FactsTypeKind, TypeVariant as FactsTypeVariant,
};
use jv_inference::TypeFacts;

const DEFAULT_ERROR_CODE: &str = "JV3002";

pub struct DiagnosticsPayload {
    pub errors: Vec<CheckError>,
    pub warnings: Vec<CheckError>,
    pub facts: Option<TypeFactsSnapshot>,
}

pub struct DiagnosticsEmitter<'ctx> {
    context: &'ctx NullSafetyContext<'ctx>,
}

impl<'ctx> DiagnosticsEmitter<'ctx> {
    pub fn new(context: &'ctx NullSafetyContext<'ctx>) -> Self {
        Self { context }
    }

    pub fn emit(&self, outcome: &FlowAnalysisOutcome) -> DiagnosticsPayload {
        let errors = outcome
            .diagnostics
            .iter()
            .map(|error| {
                let message = ensure_code(&error.to_string(), DEFAULT_ERROR_CODE);
                CheckError::NullSafetyError(message)
            })
            .collect::<Vec<_>>();

        let mut warnings = Vec::new();
        if self.context.is_degraded() {
            warnings.push(degraded_warning());
        }

        DiagnosticsPayload {
            errors,
            warnings,
            facts: self.collect_facts(outcome),
        }
    }

    fn collect_facts(&self, outcome: &FlowAnalysisOutcome) -> Option<TypeFactsSnapshot> {
        let overrides = aggregate_states(outcome);

        match (self.context.facts(), overrides.is_empty()) {
            (Some(snapshot), true) => return Some(snapshot.clone()),
            (None, true) => return None,
            _ => {}
        }

        let builder = match self.context.facts() {
            Some(snapshot) => {
                let mut builder = TypeFactsBuilder::from_snapshot(snapshot);
                let mut environment = snapshot.environment().values().clone();
                for (name, state) in overrides.iter() {
                    let entry = environment
                        .entry(name.clone())
                        .or_insert_with(|| FactsTypeKind::new(FactsTypeVariant::Unknown));
                    *entry = apply_nullability(entry.clone(), *state);
                }
                builder.set_environment(environment);
                builder
            }
            None => {
                let mut builder = TypeFactsBuilder::new();
                for (name, state) in overrides.iter() {
                    builder.environment_entry(
                        name.clone(),
                        FactsTypeKind::new(FactsTypeVariant::Unknown)
                            .with_nullability(to_flag(*state)),
                    );
                }
                builder
            }
        };

        Some(builder.build())
    }
}

fn aggregate_states(outcome: &FlowAnalysisOutcome) -> HashMap<String, NullabilityKind> {
    let mut aggregated = HashMap::new();

    for snapshot in outcome.states.values() {
        for (name, state) in snapshot.states.iter() {
            match aggregated.entry(name.clone()) {
                Entry::Occupied(mut slot) => {
                    let current: NullabilityKind = *slot.get();
                    slot.insert(current.join(*state));
                }
                Entry::Vacant(slot) => {
                    slot.insert(*state);
                }
            }
        }
    }

    aggregated
}

fn apply_nullability(ty: FactsTypeKind, state: NullabilityKind) -> FactsTypeKind {
    ty.with_nullability(to_flag(state))
}

fn to_flag(state: NullabilityKind) -> NullabilityFlag {
    match state {
        NullabilityKind::NonNull => NullabilityFlag::NonNull,
        NullabilityKind::Nullable => NullabilityFlag::Nullable,
        NullabilityKind::Unknown => NullabilityFlag::Unknown,
    }
}

fn ensure_code(message: &str, default_code: &str) -> String {
    if message.contains("JV3") {
        message.to_string()
    } else {
        format!("{default_code}: {message}")
    }
}

fn degraded_warning() -> CheckError {
    CheckError::NullSafetyError(
        "JV3005: 型推論スナップショットが見つからないため null 安全解析を簡易モードで実行しました。\nJV3005: Null safety analysis ran in degraded mode because the inference snapshot is missing.".into(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inference::TypeEnvironment;
    use crate::null_safety::graph::FlowStateSnapshot;
    use jv_inference::service::TypeFactsBuilder;
    use jv_inference::types::{TypeKind as FactsTypeKind, TypeVariant as FactsTypeVariant};

    #[test]
    fn degraded_mode_emits_warning() {
        let context = NullSafetyContext::hydrate(None);
        let outcome = FlowAnalysisOutcome::default();
        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        assert!(payload.errors.is_empty());
        assert_eq!(payload.warnings.len(), 1);
        assert!(payload.warnings[0].to_string().contains("JV3005"));
        assert!(payload.facts.is_none());
    }

    #[test]
    fn collects_flow_states_into_type_facts() {
        let mut env = TypeEnvironment::new();
        env.define_monotype("user_id", crate::inference::TypeKind::Primitive("Int"));

        let mut base_builder = TypeFactsBuilder::new();
        base_builder.environment_entry(
            "user_id",
            FactsTypeKind::new(FactsTypeVariant::Primitive("Int")),
        );
        let base_snapshot = base_builder.build();

        let context = NullSafetyContext::from_parts(Some(&base_snapshot), Some(&env));

        let mut outcome = FlowAnalysisOutcome::default();
        let mut state = FlowStateSnapshot::new();
        state.assign("user_id".into(), NullabilityKind::Nullable);
        outcome.states.insert(0, state);
        outcome
            .diagnostics
            .push(CheckError::NullSafetyError("value may be null".into()));

        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        let facts = payload.facts.expect("facts present");
        let environment = facts.environment().values();
        let ty = environment
            .get("user_id")
            .expect("user_id entry should exist");
        assert_eq!(ty.nullability(), NullabilityFlag::Nullable);
        assert!(!payload.errors.is_empty());
        assert!(payload.errors[0].to_string().contains(DEFAULT_ERROR_CODE));
    }
}
