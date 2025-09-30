use std::collections::HashMap;

use super::context::NullSafetyContext;
use super::flow::FlowAnalysisOutcome;
use super::NullabilityKind;
use crate::CheckError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoundaryKind {
    Jni,
    Foreign,
}

pub struct BoundaryChecker<'ctx> {
    context: &'ctx NullSafetyContext<'ctx>,
}

impl<'ctx> BoundaryChecker<'ctx> {
    pub fn new(context: &'ctx NullSafetyContext<'ctx>) -> Self {
        Self { context }
    }

    pub fn evaluate(&self, outcome: &FlowAnalysisOutcome) -> Vec<CheckError> {
        let aggregated = aggregate_states(outcome);
        let mut warnings = Vec::new();

        for (symbol, kind) in self.context.boundary_entries() {
            let state = aggregated
                .get(symbol.as_str())
                .copied()
                .unwrap_or(NullabilityKind::Unknown);

            if matches!(state, NullabilityKind::NonNull) {
                continue;
            }

            warnings.push(boundary_warning(symbol, kind, state));
        }

        warnings
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inference::TypeEnvironment;
    use crate::null_safety::flow::FlowAnalysisOutcome;
    use crate::null_safety::graph::FlowStateSnapshot;
    use jv_inference::service::TypeFactsBuilder;
    use jv_inference::types::{TypeKind as FactsTypeKind, TypeVariant as FactsTypeVariant};

    #[test]
    fn boundary_checker_emits_warning_for_nullable_jni_symbol() {
        let env = TypeEnvironment::new();
        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry(
            "nativeCall",
            FactsTypeKind::new(FactsTypeVariant::Primitive("java.lang.String")),
        );
        builder.add_java_annotation("nativeCall", "@JNI");
        let facts = builder.build();

        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env));

        let mut outcome = FlowAnalysisOutcome::default();
        let mut snapshot = FlowStateSnapshot::new();
        snapshot.assign("nativeCall".into(), NullabilityKind::Nullable);
        outcome.states.insert(0, snapshot);

        let warnings = BoundaryChecker::new(&context).evaluate(&outcome);

        assert!(warnings
            .iter()
            .any(|warning| warning.to_string().contains("JV3006")));
    }
}

fn aggregate_states(outcome: &FlowAnalysisOutcome) -> HashMap<String, NullabilityKind> {
    let mut aggregated = HashMap::new();

    for snapshot in outcome.states.values() {
        for (name, state) in snapshot.states.iter() {
            aggregated
                .entry(name.clone())
                .and_modify(|existing: &mut NullabilityKind| {
                    *existing = existing.join(*state);
                })
                .or_insert(*state);
        }
    }

    aggregated
}

fn boundary_warning(symbol: &str, kind: BoundaryKind, state: NullabilityKind) -> CheckError {
    let (kind_ja, kind_en) = match kind {
        BoundaryKind::Jni => ("JNI", "JNI"),
        BoundaryKind::Foreign => ("FFM", "FFM"),
    };

    let (state_ja, state_en) = match state {
        NullabilityKind::Nullable => ("Nullable", "nullable"),
        NullabilityKind::Platform => ("Platform", "platform"),
        NullabilityKind::Unknown => ("Unknown", "unknown"),
        NullabilityKind::NonNull => ("NonNull", "non-null"),
    };

    CheckError::NullSafetyError(format!(
        "JV3006: {kind_ja} 境界 `{symbol}` の null 契約を確認できません ({state_ja})。境界で引数と戻り値を手動で安全に検証するか、安全ラッパーを導入してください。\nJV3006: Unable to verify the {kind_en} boundary `{symbol}` (observed {state_en} state). Add explicit null checks or wrap the boundary defensively."
    ))
}
