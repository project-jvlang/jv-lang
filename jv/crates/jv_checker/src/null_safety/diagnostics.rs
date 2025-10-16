use std::collections::{hash_map::Entry, HashMap, HashSet};

use super::annotations::JavaNullabilityHint;
use super::context::{LateInitContractKind, NullSafetyContext};
use super::flow::FlowAnalysisOutcome;
use super::graph::FlowStateSnapshot;
use super::operators::{JavaLoweringHint, JavaLoweringStrategy, OperatorOperand};
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
        let mut overrides = aggregate_states(outcome);
        let mut errors = self.verify_exit_state(outcome.exit_state.as_ref(), &overrides);
        for error in &outcome.diagnostics {
            let message = ensure_code(&error.to_string(), DEFAULT_ERROR_CODE);
            errors.push(CheckError::NullSafetyError(message));
        }

        let mut warnings = Vec::new();
        if self.context.is_degraded() {
            warnings.push(degraded_warning());
        }
        warnings.extend(self.unknown_annotation_warnings());
        warnings.extend(self.reconcile_external_signatures(&mut overrides));
        warnings.extend(platform_warnings(&overrides));
        warnings.extend(redundant_operator_warnings(
            self.context,
            &overrides,
            &outcome.java_hints,
        ));

        DiagnosticsPayload {
            errors,
            warnings,
            facts: self.collect_facts(&overrides),
        }
    }

    fn collect_facts(
        &self,
        overrides: &HashMap<String, NullabilityKind>,
    ) -> Option<TypeFactsSnapshot> {
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

    fn verify_exit_state(
        &self,
        exit_state: Option<&FlowStateSnapshot>,
        aggregated: &HashMap<String, NullabilityKind>,
    ) -> Vec<CheckError> {
        let mut diagnostics = Vec::new();
        let Some(state) = exit_state else {
            let mut processed: HashSet<String> = HashSet::new();

            for (name, observed) in aggregated.iter() {
                self.evaluate_exit_state(name, *observed, &mut diagnostics);
                processed.insert(name.clone());
            }

            for (name, _) in self.context.contracts().iter() {
                if processed.contains(name) {
                    continue;
                }
                let observed = aggregated
                    .get(name.as_str())
                    .copied()
                    .unwrap_or(NullabilityKind::Unknown);
                self.evaluate_exit_state(name, observed, &mut diagnostics);
                processed.insert(name.clone());
            }

            for (name, _) in self.context.late_init_contracts().iter() {
                if processed.contains(name) {
                    continue;
                }
                let observed = aggregated
                    .get(name.as_str())
                    .copied()
                    .unwrap_or(NullabilityKind::Unknown);
                self.evaluate_exit_state(name, observed, &mut diagnostics);
                processed.insert(name.clone());
            }

            return diagnostics;
        };

        let mut processed: HashSet<String> = HashSet::new();

        for (name, _) in self.context.contracts().iter() {
            let mut observed = state
                .states
                .get(name.as_str())
                .copied()
                .or_else(|| aggregated.get(name.as_str()).copied())
                .unwrap_or(NullabilityKind::Unknown);

            if matches!(observed, NullabilityKind::Unknown) {
                if let Some(lattice_state) = self.context.lattice().get(name.as_str()) {
                    if !matches!(lattice_state, NullabilityKind::Unknown) {
                        observed = lattice_state;
                    }
                }
            }

            self.evaluate_exit_state(name.as_str(), observed, &mut diagnostics);
            processed.insert(name.clone());
        }

        for (name, _) in self.context.late_init_contracts().iter() {
            if processed.contains(name.as_str()) {
                continue;
            }

            let mut observed = state
                .states
                .get(name.as_str())
                .copied()
                .or_else(|| aggregated.get(name.as_str()).copied())
                .unwrap_or(NullabilityKind::Unknown);

            if matches!(observed, NullabilityKind::Unknown) {
                if let Some(lattice_state) = self.context.lattice().get(name.as_str()) {
                    if !matches!(lattice_state, NullabilityKind::Unknown) {
                        observed = lattice_state;
                    }
                }
            }

            self.evaluate_exit_state(name.as_str(), observed, &mut diagnostics);
            processed.insert(name.clone());
        }

        for (name, contract_state) in self.context.lattice().iter() {
            if !matches!(contract_state, NullabilityKind::NonNull) {
                continue;
            }

            if self.has_contract(name.as_str()) {
                continue;
            }

            let observed = state
                .states
                .get(name.as_str())
                .copied()
                .or_else(|| aggregated.get(name.as_str()).copied())
                .unwrap_or(NullabilityKind::Unknown);

            self.evaluate_exit_state(name.as_str(), observed, &mut diagnostics);
            processed.insert(name.clone());
        }

        for (name, observed) in state.states.iter() {
            if processed.contains(name.as_str()) {
                continue;
            }

            self.evaluate_exit_state(name, *observed, &mut diagnostics);
            processed.insert(name.clone());
        }

        for (name, observed) in aggregated.iter() {
            if processed.contains(name.as_str()) {
                continue;
            }

            self.evaluate_exit_state(name, *observed, &mut diagnostics);
            processed.insert(name.clone());
        }

        diagnostics
    }

    fn evaluate_exit_state(
        &self,
        name: &str,
        observed: NullabilityKind,
        diagnostics: &mut Vec<CheckError>,
    ) {
        let has_contract = self.has_contract(name);
        let requires_initialization = self.context.late_init().is_tracked(name);
        let degraded = self.context.is_degraded();
        let manifest_contract_kind = self.context.late_init_contracts().kind(name);
        let enforce_unknown = matches!(
            manifest_contract_kind,
            Some(LateInitContractKind::ImplicitInitialized)
        );
        match observed {
            NullabilityKind::NonNull => {}
            NullabilityKind::Nullable => {
                if has_contract {
                    if matches!(
                        manifest_contract_kind,
                        Some(LateInitContractKind::ImplicitInitialized)
                    ) {
                        // Implicit bindings with initialisers may legally remain nullable.
                    } else {
                        diagnostics.push(CheckError::NullSafetyError(exit_violation_message(name)));
                    }
                } else if requires_initialization && !degraded {
                    diagnostics.push(CheckError::NullSafetyError(late_init_message(name)));
                }
            }
            NullabilityKind::Unknown => {
                if has_contract && enforce_unknown && !degraded {
                    diagnostics.push(CheckError::NullSafetyError(exit_violation_message(name)));
                }
            }
            NullabilityKind::Platform => {
                if has_contract {
                    diagnostics.push(CheckError::NullSafetyError(exit_violation_message(name)));
                } else if requires_initialization && !degraded {
                    diagnostics.push(CheckError::NullSafetyError(late_init_message(name)));
                }
            }
        }
    }

    fn unknown_annotation_warnings(&self) -> Vec<CheckError> {
        self.context
            .unknown_java_annotations()
            .map(|(symbol, annotation)| unknown_annotation_warning(symbol, annotation))
            .collect()
    }

    fn reconcile_external_signatures(
        &self,
        overrides: &mut HashMap<String, NullabilityKind>,
    ) -> Vec<CheckError> {
        let mut warnings = Vec::new();

        for (symbol, meta) in self.context.java_metadata() {
            let Some(hint) = meta.nullability_hint() else {
                continue;
            };

            let Some(observed) = overrides.get_mut(symbol) else {
                continue;
            };

            if external_mismatch_detected(hint, *observed) {
                *observed = NullabilityKind::Unknown;
                warnings.push(external_mismatch_warning(symbol, hint));
            }
        }

        warnings
    }

    fn has_contract(&self, name: &str) -> bool {
        self.context.contracts().contains(name) || self.context.late_init_contracts().contains(name)
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
        NullabilityKind::Platform => NullabilityFlag::Unknown,
        NullabilityKind::Unknown => NullabilityFlag::Unknown,
    }
}

fn platform_warnings(overrides: &HashMap<String, NullabilityKind>) -> Vec<CheckError> {
    overrides
        .iter()
        .filter(|(_, state)| matches!(state, NullabilityKind::Platform))
        .map(|(name, _)| platform_warning(name))
        .collect()
}

fn platform_warning(name: &str) -> CheckError {
    CheckError::NullSafetyError(format!(
        "JV3005: プラットフォーム型 `{name}` の null 安全性が不明です。境界に注釈を追加するか、jv で型をラップしてください。\nJV3005: Null safety for platform type `{name}` is unknown. Add annotations at the boundary or wrap the type in jv."
    ))
}

fn unknown_annotation_warning(symbol: &str, annotation: &str) -> CheckError {
    CheckError::NullSafetyError(format!(
        "JV3005: `{annotation}` 注釈の null 意味が分からないため、シンボル `{symbol}` を危険側で扱います。注釈を標準の null 安全注釈へ置き換えるか、境界に安全なラッパーを追加してください。\nJV3005: Null semantics for annotation `{annotation}` on symbol `{symbol}` are unknown. Replace it with a recognised null-safety annotation or wrap the boundary defensively."
    ))
}

fn external_mismatch_detected(hint: JavaNullabilityHint, observed: NullabilityKind) -> bool {
    match hint {
        JavaNullabilityHint::Nullable => matches!(observed, NullabilityKind::NonNull),
        JavaNullabilityHint::NonNull | JavaNullabilityHint::NullMarked => {
            matches!(observed, NullabilityKind::Nullable)
        }
    }
}

fn external_mismatch_warning(symbol: &str, hint: JavaNullabilityHint) -> CheckError {
    let (expected_ja, expected_en) = match hint {
        JavaNullabilityHint::Nullable => ("@Nullable".to_string(), "@Nullable".to_string()),
        JavaNullabilityHint::NonNull => (
            "@NotNull/@NonNull".to_string(),
            "@NotNull/@NonNull".to_string(),
        ),
        JavaNullabilityHint::NullMarked => ("@NullMarked".to_string(), "@NullMarked".to_string()),
    };

    CheckError::NullSafetyError(format!(
        "JV3005: Java 境界 `{symbol}` は {expected_ja} として宣言されていますが、解析結果と矛盾しました。境界の注釈を確認し、必要であれば jv 側で null チェックを追加してください。\nJV3005: Java boundary `{symbol}` is declared as {expected_en} but the analysis observed conflicting nullability. Review the annotation or add defensive null handling in jv."
    ))
}

fn redundant_operator_warnings(
    context: &NullSafetyContext<'_>,
    overrides: &HashMap<String, NullabilityKind>,
    hints: &[JavaLoweringHint],
) -> Vec<CheckError> {
    let mut warnings = Vec::new();

    for hint in hints {
        if !matches!(
            hint.strategy,
            JavaLoweringStrategy::NullSafeMemberAccess
                | JavaLoweringStrategy::NullSafeIndexAccess
                | JavaLoweringStrategy::NotNullAssertion
        ) {
            continue;
        }

        let Some(operand) = &hint.operand else {
            continue;
        };

        if !operand_is_non_null(context, overrides, operand) {
            continue;
        }

        if let Some(message) = redundant_operator_message(hint, operand) {
            warnings.push(CheckError::NullSafetyError(message));
        }
    }

    warnings
}

fn operand_is_non_null(
    context: &NullSafetyContext<'_>,
    overrides: &HashMap<String, NullabilityKind>,
    operand: &OperatorOperand,
) -> bool {
    if matches!(operand.state(), NullabilityKind::NonNull) {
        return true;
    }

    if let Some(symbol) = operand.symbol.as_deref() {
        if matches!(
            context.lattice().get(symbol),
            Some(NullabilityKind::NonNull)
        ) {
            return true;
        }

        if matches!(overrides.get(symbol), Some(NullabilityKind::NonNull)) {
            return true;
        }
    }

    false
}

fn redundant_operator_message(
    hint: &JavaLoweringHint,
    operand: &OperatorOperand,
) -> Option<String> {
    let (target_ja, target_en) = operand
        .symbol
        .as_ref()
        .map(|name| (format!("`{name}`"), format!("`{name}`")))
        .unwrap_or_else(|| ("この値".to_string(), "this value".to_string()));

    let message = match hint.strategy {
        JavaLoweringStrategy::NullSafeMemberAccess => format!(
            "JV3001: 値 {target_ja} は non-null と判定されているため `?.` 演算子は冗長です。`.` アクセスへ書き換えてください。\nJV3001: Null-safe operator `?.` is redundant because {target_en} is already non-null. Replace it with a regular `.` access."
        ),
        JavaLoweringStrategy::NullSafeIndexAccess => format!(
            "JV3001: 値 {target_ja} は non-null と判定されているため `?[ ]` 演算子は冗長です。通常のインデックスアクセスへ置き換えてください。\nJV3001: Null-safe operator `?[ ]` is redundant because {target_en} is already non-null. Replace it with a standard index access."
        ),
        JavaLoweringStrategy::NotNullAssertion => format!(
            "JV3001: 値 {target_ja} は non-null と判定されているため `!!` 演算子は冗長です。演算子を削除するか Elvis 演算子 `?:` でフォールバックを提供してください。\nJV3001: Non-null assertion `!!` is redundant because {target_en} is already non-null. Drop the operator or provide a fallback using `?:`."
        ),
        _ => return None,
    };

    Some(message)
}

fn exit_violation_message(name: &str) -> String {
    format!(
        "JV3002: 値 `{name}` は non-null と宣言されていますが、スコープ終了時に null になる可能性があります。確実に non-null へ初期化するか、Nullable 型へ更新してください。\nJV3002: Value `{name}` declared non-null may be null when control leaves this scope. Ensure it is initialised with a non-null value or relax the type to nullable."
    )
}

fn late_init_message(name: &str) -> String {
    format!(
        "JV3003: 値 `{name}` は non-null と宣言されていますが、すべての経路で初期化されるとは限りません。即時に初期化するか `@LateInit` 注釈を追加し、安全な初期化手順を提供してください。\nJV3003: Value `{name}` is declared non-null but may bypass initialisation on some paths. Initialise it eagerly or add an `@LateInit` annotation with a safe initialisation strategy."
    )
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
    use crate::binding::{LateInitManifest, LateInitSeed};
    use crate::inference::PrimitiveType;
    use crate::inference::TypeEnvironment;
    use crate::null_safety::graph::FlowStateSnapshot;
    use crate::null_safety::operators::{JavaLoweringHint, JavaLoweringStrategy, OperatorOperand};
    use jv_ast::types::Span;
    use jv_ast::ValBindingOrigin;
    use jv_inference::service::TypeFactsBuilder;
    use jv_inference::types::{
        NullabilityFlag, TypeKind as FactsTypeKind, TypeVariant as FactsTypeVariant,
    };
    use std::collections::HashMap;

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
        env.define_monotype(
            "user_id",
            crate::inference::TypeKind::primitive(PrimitiveType::Int),
        );

        let mut base_builder = TypeFactsBuilder::new();
        base_builder.environment_entry(
            "user_id",
            FactsTypeKind::new(FactsTypeVariant::Primitive("Int")),
        );
        let base_snapshot = base_builder.build();

        let context = NullSafetyContext::from_parts(Some(&base_snapshot), Some(&env), None);

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

    #[test]
    fn platform_states_emit_jv3005_information_warning() {
        let env = TypeEnvironment::new();
        let facts = TypeFactsBuilder::new().build();
        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);

        let mut outcome = FlowAnalysisOutcome::default();
        let mut state = FlowStateSnapshot::new();
        state.assign("external_api".into(), NullabilityKind::Platform);
        outcome.states.insert(0, state);

        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        assert_eq!(payload.errors.len(), 0);
        assert_eq!(payload.warnings.len(), 1);
        assert!(payload.warnings[0].to_string().contains("JV3005"));
        assert!(payload.warnings[0].to_string().contains("external_api"));
    }

    #[test]
    fn unknown_java_annotations_trigger_warning() {
        let env = TypeEnvironment::new();
        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry(
            "mystery",
            FactsTypeKind::new(FactsTypeVariant::Primitive("java.lang.Object")),
        );
        builder.add_java_annotation("mystery", "@MaybeNull");
        let facts = builder.build();

        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);
        let outcome = FlowAnalysisOutcome::default();
        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        assert!(payload
            .warnings
            .iter()
            .any(|warning| warning.to_string().contains("JV3005")
                && warning.to_string().contains("MaybeNull")));
    }

    #[test]
    fn external_annotation_mismatch_emits_warning() {
        let env = TypeEnvironment::new();
        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry(
            "external",
            FactsTypeKind::new(FactsTypeVariant::Primitive("java.lang.String")),
        );
        builder.add_java_annotation("external", "@NotNull");
        let facts = builder.build();

        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);

        let mut outcome = FlowAnalysisOutcome::default();
        let mut state = FlowStateSnapshot::new();
        state.assign("external".into(), NullabilityKind::Nullable);
        outcome.states.insert(0, state);

        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        assert!(payload
            .warnings
            .iter()
            .any(|warning| warning.to_string().contains("JV3005")
                && warning.to_string().contains("external")));
    }

    #[test]
    fn redundant_null_safe_member_access_emits_warning() {
        let mut env = TypeEnvironment::new();
        env.define_monotype("user", crate::inference::TypeKind::reference("User"));

        let mut facts_builder = TypeFactsBuilder::new();
        facts_builder.environment_entry(
            "user",
            FactsTypeKind::new(FactsTypeVariant::Primitive("User"))
                .with_nullability(NullabilityFlag::NonNull),
        );
        let facts = facts_builder.build();
        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);

        let operand = OperatorOperand::new(NullabilityKind::NonNull, Some("user".into()));
        let hint = JavaLoweringHint::new(
            Span::dummy(),
            JavaLoweringStrategy::NullSafeMemberAccess,
            "hint",
            Some(operand),
        );

        let mut outcome = FlowAnalysisOutcome::default();
        outcome.java_hints.push(hint);

        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        assert_eq!(payload.errors.len(), 0);
        assert!(payload
            .warnings
            .iter()
            .any(|warning| warning.to_string().contains("JV3001")
                && warning.to_string().contains("?.")
                && warning.to_string().contains("user")));
    }

    #[test]
    fn redundant_not_null_assertion_emits_warning() {
        let mut env = TypeEnvironment::new();
        env.define_monotype(
            "value",
            crate::inference::TypeKind::reference("java.lang.String"),
        );

        let mut facts_builder = TypeFactsBuilder::new();
        facts_builder.environment_entry(
            "value",
            FactsTypeKind::new(FactsTypeVariant::Primitive("String"))
                .with_nullability(NullabilityFlag::NonNull),
        );
        let facts = facts_builder.build();
        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);

        let operand = OperatorOperand::new(NullabilityKind::NonNull, Some("value".into()));
        let hint = JavaLoweringHint::new(
            Span::dummy(),
            JavaLoweringStrategy::NotNullAssertion,
            "hint",
            Some(operand),
        );

        let mut outcome = FlowAnalysisOutcome::default();
        outcome.java_hints.push(hint);

        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        assert!(payload
            .warnings
            .iter()
            .any(|warning| warning.to_string().contains("JV3001")
                && warning.to_string().contains("!!")
                && warning.to_string().contains("value")));
    }

    #[test]
    fn exit_violation_emits_error() {
        let mut env = TypeEnvironment::new();
        env.define_monotype(
            "token",
            crate::inference::TypeKind::reference("java.lang.String"),
        );

        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry(
            "token",
            FactsTypeKind::new(FactsTypeVariant::Primitive("String"))
                .with_nullability(NullabilityFlag::NonNull),
        );
        let facts = builder.build();
        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);

        let mut outcome = FlowAnalysisOutcome::default();
        let mut exit_state = FlowStateSnapshot::new();
        exit_state.assign("token".into(), NullabilityKind::Nullable);
        outcome.exit_state = Some(exit_state);

        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        assert!(
            payload
                .errors
                .iter()
                .any(|error| error.to_string().contains("JV3002")
                    && error.to_string().contains("token")),
            "expected JV3002 exit diagnostic for token, got {:?}",
            payload
                .errors
                .iter()
                .map(|err| err.to_string())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn manifest_contract_unknown_exit_state_is_tolerated() {
        let mut env = TypeEnvironment::new();
        env.define_monotype(
            "token",
            crate::inference::TypeKind::reference("java.lang.String"),
        );

        let mut facts_builder = TypeFactsBuilder::new();
        facts_builder.environment_entry(
            "token",
            FactsTypeKind::new(FactsTypeVariant::Primitive("String"))
                .with_nullability(NullabilityFlag::NonNull),
        );
        let facts = facts_builder.build();

        let mut seeds = HashMap::new();
        seeds.insert(
            "token".to_string(),
            LateInitSeed {
                name: "token".into(),
                origin: ValBindingOrigin::ExplicitKeyword,
                has_initializer: true,
                explicit_late_init: false,
            },
        );
        let manifest = LateInitManifest::new(seeds);

        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), Some(&manifest));

        let mut outcome = FlowAnalysisOutcome::default();
        let mut exit_state = FlowStateSnapshot::new();
        exit_state.assign("token".into(), NullabilityKind::Unknown);
        outcome.exit_state = Some(exit_state);

        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        assert!(
            payload
                .errors
                .iter()
                .all(|error| !error.to_string().contains("JV3002")
                    && !error.to_string().contains("JV3003")),
            "unexpected JV3002/JV3003 diagnostics: {:?}",
            payload.errors
        );
    }

    #[test]
    fn manifest_contract_nullable_exit_state_emits_violation() {
        let mut env = TypeEnvironment::new();
        env.define_monotype(
            "token",
            crate::inference::TypeKind::reference("java.lang.String"),
        );

        let mut facts_builder = TypeFactsBuilder::new();
        facts_builder.environment_entry(
            "token",
            FactsTypeKind::new(FactsTypeVariant::Primitive("String"))
                .with_nullability(NullabilityFlag::NonNull),
        );
        let facts = facts_builder.build();

        let mut seeds = HashMap::new();
        seeds.insert(
            "token".to_string(),
            LateInitSeed {
                name: "token".into(),
                origin: ValBindingOrigin::ExplicitKeyword,
                has_initializer: true,
                explicit_late_init: false,
            },
        );
        let manifest = LateInitManifest::new(seeds);

        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), Some(&manifest));

        let mut outcome = FlowAnalysisOutcome::default();
        let mut exit_state = FlowStateSnapshot::new();
        exit_state.assign("token".into(), NullabilityKind::Nullable);
        outcome.exit_state = Some(exit_state);

        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        assert!(payload
            .errors
            .iter()
            .any(|error| error.to_string().contains("JV3002")));
    }

    #[test]
    fn exit_nonnull_state_is_ok() {
        let mut env = TypeEnvironment::new();
        env.define_monotype(
            "token",
            crate::inference::TypeKind::reference("java.lang.String"),
        );

        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry(
            "token",
            FactsTypeKind::new(FactsTypeVariant::Primitive("String"))
                .with_nullability(NullabilityFlag::NonNull),
        );
        let facts = builder.build();
        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);

        let mut outcome = FlowAnalysisOutcome::default();
        let mut exit_state = FlowStateSnapshot::new();
        exit_state.assign("token".into(), NullabilityKind::NonNull);
        outcome.exit_state = Some(exit_state);

        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        assert!(payload.errors.is_empty());
    }

    #[test]
    fn exit_unknown_emits_late_init_guidance() {
        let mut env = TypeEnvironment::new();
        env.define_monotype("session", crate::inference::TypeKind::reference("Session"));

        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry(
            "session",
            FactsTypeKind::new(FactsTypeVariant::Primitive("Session"))
                .with_nullability(NullabilityFlag::NonNull),
        );
        let facts = builder.build();
        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);

        let mut outcome = FlowAnalysisOutcome::default();
        outcome.exit_state = Some(FlowStateSnapshot::new());

        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        assert!(
            payload
                .errors
                .iter()
                .all(|error| !error.to_string().contains("JV3003")),
            "unknown states should no longer emit JV3003, got {:?}",
            payload
                .errors
                .iter()
                .map(|err| err.to_string())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn degraded_mode_skips_late_init_guidance() {
        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry(
            "session",
            FactsTypeKind::new(FactsTypeVariant::Primitive("Session"))
                .with_nullability(NullabilityFlag::NonNull),
        );
        let facts = builder.build();
        let context = NullSafetyContext::from_parts(Some(&facts), None, None);

        let mut outcome = FlowAnalysisOutcome::default();
        outcome.exit_state = Some(FlowStateSnapshot::new());

        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        assert!(payload
            .errors
            .iter()
            .all(|error| !error.to_string().contains("JV3003")));
    }

    #[test]
    fn annotated_late_init_suppresses_guidance() {
        let mut env = TypeEnvironment::new();
        env.define_monotype("session", crate::inference::TypeKind::reference("Session"));

        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry(
            "session",
            FactsTypeKind::new(FactsTypeVariant::Primitive("Session"))
                .with_nullability(NullabilityFlag::NonNull),
        );
        let facts = builder.build();
        let mut context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);
        context.late_init_mut().allow_late_init("session");

        let mut outcome = FlowAnalysisOutcome::default();
        outcome.exit_state = Some(FlowStateSnapshot::new());

        let emitter = DiagnosticsEmitter::new(&context);
        let payload = emitter.emit(&outcome);

        assert!(payload
            .errors
            .iter()
            .all(|error| !error.to_string().contains("JV3003")));
    }
}
