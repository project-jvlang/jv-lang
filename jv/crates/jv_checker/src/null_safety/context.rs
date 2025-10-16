use std::collections::{HashMap, HashSet};

use super::annotations::{lookup_nullability_hint, JavaNullabilityHint};
use crate::binding::LateInitManifest;
use crate::inference::{TypeEnvironment, TypeKind as CheckerTypeKind, TypeScheme};
use crate::pattern::{PatternMatchFacts, PatternTarget};
use crate::{InferenceSnapshot, TypeInferenceService};
use jv_ast::ValBindingOrigin;
use jv_inference::service::{TypeFacts, TypeFactsSnapshot};
use jv_inference::types::{
    NullabilityFlag, TypeKind as FactsTypeKind, TypeVariant as FactsTypeVariant,
};

/// Represents the three-valued nullability lattice used by the null safety pipeline.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NullabilityKind {
    NonNull,
    Nullable,
    Platform,
    Unknown,
}

impl NullabilityKind {
    pub fn join(self, other: Self) -> Self {
        use NullabilityKind::*;
        match (self, other) {
            (Nullable, _) | (_, Nullable) => Nullable,
            (Platform, _) | (_, Platform) => Platform,
            (Unknown, _) | (_, Unknown) => Unknown,
            _ => NonNull,
        }
    }

    fn from_checker_type(ty: &CheckerTypeKind) -> Self {
        use CheckerTypeKind::*;
        match ty {
            Optional(_) => NullabilityKind::Nullable,
            Unknown => NullabilityKind::Unknown,
            Variable(_) => NullabilityKind::Unknown,
            Primitive(_) | Boxed(_) | Reference(_) | Function(_, _) => NullabilityKind::NonNull,
        }
    }

    pub(super) fn from_facts_type(ty: &FactsTypeKind) -> Self {
        use FactsTypeVariant::*;

        if matches!(ty.variant(), Optional(_)) {
            return NullabilityKind::Nullable;
        }

        match ty.nullability() {
            NullabilityFlag::Nullable => NullabilityKind::Nullable,
            NullabilityFlag::Unknown => {
                if is_platform_candidate(ty) {
                    NullabilityKind::Platform
                } else {
                    NullabilityKind::Unknown
                }
            }
            NullabilityFlag::NonNull => match ty.variant() {
                Unknown | Variable(_) => NullabilityKind::Unknown,
                _ => NullabilityKind::NonNull,
            },
        }
    }

    fn from_checker_scheme(scheme: &TypeScheme) -> Self {
        if scheme.is_polymorphic() {
            return NullabilityKind::Unknown;
        }

        if scheme.ty.contains_unknown() {
            NullabilityKind::Unknown
        } else {
            NullabilityKind::from_checker_type(&scheme.ty)
        }
    }
}

fn is_platform_candidate(ty: &FactsTypeKind) -> bool {
    use FactsTypeVariant::*;

    match ty.variant() {
        Unknown | Variable(_) => false,
        Optional(_) => false, // handled earlier
        _ => true,
    }
}

/// Mapping from symbol identifiers to their inferred nullability states.
#[derive(Debug, Default, Clone)]
pub struct NullabilityLattice {
    symbols: HashMap<String, NullabilityKind>,
}

impl NullabilityLattice {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: impl Into<String>, state: NullabilityKind) {
        let name = name.into();
        match self.symbols.get(&name).copied() {
            Some(existing) => {
                let merged = existing.join(state);
                self.symbols.insert(name, merged);
            }
            None => {
                self.symbols.insert(name, state);
            }
        }
    }

    /// Overrides the recorded nullability state for the specified symbol.
    pub fn set(&mut self, name: impl Into<String>, state: NullabilityKind) {
        self.symbols.insert(name.into(), state);
    }

    pub fn get(&self, name: &str) -> Option<NullabilityKind> {
        self.symbols.get(name).copied()
    }

    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &NullabilityKind)> {
        self.symbols.iter()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NonNullContractOrigin {
    InferenceEnvironment,
    InferenceScheme,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NonNullContract {
    origin: NonNullContractOrigin,
}

impl NonNullContract {
    pub fn origin(&self) -> NonNullContractOrigin {
        self.origin
    }
}

#[derive(Debug, Default, Clone)]
pub struct NonNullContracts {
    entries: HashMap<String, NonNullContract>,
}

impl NonNullContracts {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, name: impl Into<String>, origin: NonNullContractOrigin) {
        let symbol = name.into();
        self.entries
            .entry(symbol)
            .or_insert(NonNullContract { origin });
    }

    pub fn contains(&self, name: &str) -> bool {
        self.entries.contains_key(name)
    }

    pub fn get(&self, name: &str) -> Option<&NonNullContract> {
        self.entries.get(name)
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &NonNullContract)> {
        self.entries.iter()
    }
}

#[derive(Debug, Default, Clone)]
pub struct LateInitRegistry {
    required: HashSet<String>,
    exempt: HashSet<String>,
}

impl LateInitRegistry {
    pub fn new(lattice: &NullabilityLattice, manifest: Option<&LateInitManifest>) -> Self {
        let mut required: HashSet<String> = lattice
            .iter()
            .filter_map(|(name, state)| {
                if matches!(state, NullabilityKind::NonNull | NullabilityKind::Nullable) {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect();

        let mut exempt = HashSet::new();

        if let Some(manifest) = manifest {
            for (name, seed) in manifest.iter() {
                if !required.contains(name) {
                    continue;
                }

                if seed.explicit_late_init {
                    exempt.remove(name);
                    continue;
                }

                let Some(state) = lattice.get(name) else {
                    continue;
                };

                if seed.origin == ValBindingOrigin::Implicit {
                    required.remove(name);
                    exempt.insert(name.clone());
                    continue;
                }

                if matches!(state, NullabilityKind::NonNull) && seed.has_initializer {
                    required.remove(name);
                    exempt.insert(name.clone());
                }
            }
        }

        Self { required, exempt }
    }

    pub fn is_tracked(&self, name: &str) -> bool {
        self.required.contains(name) && !self.exempt.contains(name)
    }

    pub fn allow_late_init(&mut self, name: impl Into<String>) {
        self.exempt.insert(name.into());
    }

    #[cfg(test)]
    pub fn tracked_names(&self) -> impl Iterator<Item = &String> {
        self.required.iter()
    }

    #[cfg(test)]
    pub fn exempt_names(&self) -> impl Iterator<Item = &String> {
        self.exempt.iter()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LateInitContractKind {
    ImplicitInitialized,
    InferredNonNull,
}

#[derive(Debug, Default, Clone)]
pub struct LateInitContracts {
    enforced: HashMap<String, LateInitContractKind>,
}

impl LateInitContracts {
    pub fn new(lattice: &NullabilityLattice, manifest: Option<&LateInitManifest>) -> Self {
        let mut enforced = HashMap::new();

        if let Some(manifest) = manifest {
            for (name, seed) in manifest.iter() {
                if seed.explicit_late_init {
                    continue;
                }

                if !seed.has_initializer {
                    continue;
                }

                let lattice_state = lattice.get(name);
                let is_nonnull_contract = matches!(lattice_state, Some(NullabilityKind::NonNull));
                let is_implicit_contract = matches!(seed.origin, ValBindingOrigin::Implicit);

                if is_implicit_contract {
                    enforced.insert(name.clone(), LateInitContractKind::ImplicitInitialized);
                    continue;
                }

                if is_nonnull_contract {
                    enforced.insert(name.clone(), LateInitContractKind::InferredNonNull);
                }
            }
        }

        Self { enforced }
    }

    pub fn contains(&self, name: &str) -> bool {
        self.enforced.contains_key(name)
    }

    pub fn kind(&self, name: &str) -> Option<LateInitContractKind> {
        self.enforced.get(name).copied()
    }

    pub fn is_empty(&self) -> bool {
        self.enforced.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, LateInitContractKind)> {
        self.enforced.iter().map(|(name, kind)| (name, *kind))
    }
}

/// Hydrates symbol tables and lattice state from inference snapshots for downstream flow analysis.
pub struct NullSafetyContext<'facts> {
    facts: Option<&'facts TypeFactsSnapshot>,
    lattice: NullabilityLattice,
    contracts: NonNullContracts,
    degraded: bool,
    late_init: LateInitRegistry,
    late_init_contracts: LateInitContracts,
    java_metadata: HashMap<String, JavaSymbolMetadata>,
    pattern_facts: HashMap<u64, PatternMatchFacts>,
}

impl<'facts> NullSafetyContext<'facts> {
    /// Builds the context from an optional inference snapshot, falling back to degraded mode.
    pub fn hydrate(snapshot: Option<&'facts InferenceSnapshot>) -> Self {
        match snapshot {
            Some(snapshot) => {
                let mut context = Self::from_parts(
                    Some(snapshot.type_facts()),
                    Some(snapshot.environment()),
                    Some(snapshot.late_init_manifest()),
                );
                for ((node_id, target), facts) in snapshot.pattern_facts() {
                    if *target == PatternTarget::Java25 {
                        context.register_pattern_facts(*node_id, facts.clone());
                    }
                }
                context
            }
            None => Self::fallback(),
        }
    }

    /// Public constructor used by tests and future tasks when facts are precomputed.
    pub fn from_parts(
        facts: Option<&'facts TypeFactsSnapshot>,
        environment: Option<&'facts TypeEnvironment>,
        manifest: Option<&LateInitManifest>,
    ) -> Self {
        let mut lattice = NullabilityLattice::new();
        let mut contracts = NonNullContracts::default();

        if let Some(env) = environment {
            for (name, scheme) in env.flattened_bindings() {
                let state = NullabilityKind::from_checker_scheme(&scheme);
                lattice.insert(name, state);
            }
        }

        if let Some(facts) = facts {
            hydrate_from_facts(facts, &mut lattice, &mut contracts);
        }

        let mut java_metadata = HashMap::new();

        if let Some(facts) = facts {
            hydrate_java_annotations(facts, &mut lattice, &mut java_metadata);
        }

        let late_init = LateInitRegistry::new(&lattice, manifest);
        let late_init_contracts = LateInitContracts::new(&lattice, manifest);

        Self {
            facts,
            lattice,
            contracts,
            degraded: facts.is_none() || environment.is_none(),
            late_init,
            late_init_contracts,
            java_metadata,
            pattern_facts: HashMap::new(),
        }
    }

    fn fallback() -> Self {
        Self {
            facts: None,
            lattice: NullabilityLattice::new(),
            contracts: NonNullContracts::default(),
            degraded: true,
            late_init: LateInitRegistry::default(),
            late_init_contracts: LateInitContracts::default(),
            java_metadata: HashMap::new(),
            pattern_facts: HashMap::new(),
        }
    }

    /// Returns hydrated type facts when available.
    pub fn facts(&self) -> Option<&'facts TypeFactsSnapshot> {
        self.facts
    }

    /// Returns a reference to the symbol lattice used for flow analysis.
    pub fn lattice(&self) -> &NullabilityLattice {
        &self.lattice
    }

    pub fn contracts(&self) -> &NonNullContracts {
        &self.contracts
    }

    /// Returns true when the context is operating in degraded mode due to missing inference data.
    pub fn is_degraded(&self) -> bool {
        self.degraded
    }

    /// Returns registry describing declarations that must be initialised on every path.
    pub fn late_init(&self) -> &LateInitRegistry {
        &self.late_init
    }

    pub fn late_init_mut(&mut self) -> &mut LateInitRegistry {
        &mut self.late_init
    }

    pub fn late_init_contracts(&self) -> &LateInitContracts {
        &self.late_init_contracts
    }

    /// Returns Java interop metadata collected during context hydration.
    pub fn java_metadata(&self) -> &HashMap<String, JavaSymbolMetadata> {
        &self.java_metadata
    }

    /// Returns iterator over unknown Java annotations discovered during hydration.
    pub fn unknown_java_annotations(&self) -> impl Iterator<Item = (&String, &String)> {
        self.java_metadata.iter().flat_map(|(symbol, meta)| {
            meta.unknown_annotations
                .iter()
                .map(move |annotation| (symbol, annotation))
        })
    }

    /// Returns boundary metadata derived from Java annotations.
    pub fn boundary_entries(
        &self,
    ) -> impl Iterator<Item = (&String, super::boundary::BoundaryKind)> {
        self.java_metadata
            .iter()
            .filter_map(|(symbol, meta)| meta.boundary.map(|kind| (symbol, kind)))
    }

    pub fn register_pattern_facts(&mut self, node_id: u64, facts: PatternMatchFacts) {
        self.pattern_facts.insert(node_id, facts);
    }

    pub fn pattern_facts(&self, node_id: u64) -> Option<&PatternMatchFacts> {
        self.pattern_facts.get(&node_id)
    }
}

fn hydrate_from_facts(
    facts: &TypeFactsSnapshot,
    lattice: &mut NullabilityLattice,
    contracts: &mut NonNullContracts,
) {
    for (name, ty) in facts.environment().values().iter() {
        let symbol = name.to_string();
        let state = NullabilityKind::from_facts_type(ty);
        lattice.insert(symbol.clone(), state);

        if matches!(state, NullabilityKind::NonNull) {
            contracts.insert(symbol, NonNullContractOrigin::InferenceEnvironment);
        }
    }

    for (name, scheme) in facts.all_schemes() {
        let symbol = name.to_string();
        let state = NullabilityKind::from_facts_type(scheme.body());
        lattice.insert(symbol.clone(), state);

        if matches!(state, NullabilityKind::NonNull) {
            contracts.insert(symbol, NonNullContractOrigin::InferenceScheme);
        }
    }
}

fn hydrate_java_annotations(
    facts: &TypeFactsSnapshot,
    lattice: &mut NullabilityLattice,
    metadata: &mut HashMap<String, JavaSymbolMetadata>,
) {
    for (symbol, annotations) in facts.java_annotations().iter() {
        if annotations.is_empty() {
            continue;
        }

        let mut meta = JavaSymbolMetadata::new();
        for annotation in annotations {
            meta.record_annotation(annotation);
        }

        if let Some(kind) = meta.nullability_kind() {
            lattice.set(symbol.clone(), kind);
        }

        if !meta.is_empty() {
            metadata.insert(symbol.clone(), meta);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JavaSymbolMetadata {
    annotations: Vec<String>,
    nullability_hint: Option<(JavaNullabilityHint, u8)>,
    unknown_annotations: Vec<String>,
    boundary: Option<super::boundary::BoundaryKind>,
}

impl JavaSymbolMetadata {
    fn new() -> Self {
        Self {
            annotations: Vec::new(),
            nullability_hint: None,
            unknown_annotations: Vec::new(),
            boundary: None,
        }
    }

    fn record_annotation(&mut self, annotation: &str) {
        let trimmed = annotation.trim().trim_start_matches('@');
        self.annotations.push(trimmed.to_string());

        let normalized = trimmed.to_ascii_lowercase();

        if let Some((hint, precedence)) = lookup_nullability_hint(&normalized) {
            match self.nullability_hint {
                Some((_, existing)) if existing > precedence => {}
                _ => self.nullability_hint = Some((hint, precedence)),
            }
            return;
        }

        match normalized.as_str() {
            "nullmarked" | "org.jspecify.annotations.nullmarked" => {
                if !matches!(
                    self.nullability_hint,
                    Some((JavaNullabilityHint::Nullable, _))
                ) {
                    self.nullability_hint = Some((JavaNullabilityHint::NullMarked, 50));
                }
            }
            "jni" | "jnifunction" | "jdk.jni" => {
                self.boundary = Some(super::boundary::BoundaryKind::Jni);
            }
            "ffm" | "foreignfunction" | "foreignmemory" | "jdk.incubator.foreign" => {
                self.boundary = Some(super::boundary::BoundaryKind::Foreign);
            }
            _ => self.unknown_annotations.push(trimmed.to_string()),
        }
    }

    fn nullability_kind(&self) -> Option<NullabilityKind> {
        match self.nullability_hint.map(|(hint, _)| hint) {
            Some(JavaNullabilityHint::Nullable) => Some(NullabilityKind::Nullable),
            Some(JavaNullabilityHint::NonNull | JavaNullabilityHint::NullMarked) => {
                Some(NullabilityKind::NonNull)
            }
            None => None,
        }
    }

    pub fn nullability_hint(&self) -> Option<JavaNullabilityHint> {
        self.nullability_hint.map(|(hint, _)| hint)
    }

    pub fn boundary(&self) -> Option<super::boundary::BoundaryKind> {
        self.boundary
    }

    pub fn annotations(&self) -> &[String] {
        &self.annotations
    }

    pub fn unknown_annotations(&self) -> &[String] {
        &self.unknown_annotations
    }

    fn is_empty(&self) -> bool {
        self.annotations.is_empty() && self.unknown_annotations.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binding::{LateInitManifest, LateInitSeed};
    use crate::inference::type_factory::TypeFactory;
    use crate::inference::types::TypeKind;
    use crate::inference::PrimitiveType;
    use crate::inference::TypeEnvironment;
    use jv_inference::service::TypeFactsBuilder;
    use jv_inference::service::TypeScheme as FactsTypeScheme;
    use jv_inference::types::{NullabilityFlag, TypeKind as FactsTypeKind, TypeVariant};

    fn checker_optional(inner: &'static str) -> TypeKind {
        let base =
            TypeFactory::from_annotation(inner).unwrap_or_else(|_| TypeKind::reference(inner));
        TypeKind::optional(base)
    }

    fn facts_optional(inner: &'static str) -> FactsTypeKind {
        FactsTypeKind::new(TypeVariant::Optional(Box::new(FactsTypeKind::new(
            TypeVariant::Primitive(inner),
        ))))
        .with_nullability(NullabilityFlag::Nullable)
    }

    #[test]
    fn hydrates_symbol_lattice_from_environment_and_facts() {
        let mut env = TypeEnvironment::new();
        env.define_monotype("user_id", TypeKind::primitive(PrimitiveType::Int));
        env.define_monotype("maybe_email", checker_optional("String"));

        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry("user_id", FactsTypeKind::new(TypeVariant::Primitive("Int")));
        builder.environment_entry("maybe_email", facts_optional("String"));
        builder.add_scheme(
            "User::lookup",
            FactsTypeScheme::new(
                vec![],
                FactsTypeKind::new(TypeVariant::Primitive("User"))
                    .with_nullability(NullabilityFlag::NonNull),
            ),
        );
        let facts = builder.build();

        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);

        assert_eq!(context.is_degraded(), false);
        assert_eq!(context.lattice().len(), 3);
        assert_eq!(
            context.lattice().get("user_id"),
            Some(NullabilityKind::Platform)
        );
        assert_eq!(
            context.lattice().get("maybe_email"),
            Some(NullabilityKind::Nullable)
        );
        assert_eq!(
            context.lattice().get("User::lookup"),
            Some(NullabilityKind::NonNull)
        );
        assert_eq!(context.contracts().len(), 1);
        assert!(context.contracts().contains("User::lookup"));
        assert!(!context.contracts().contains("user_id"));
        assert!(!context.contracts().contains("maybe_email"));
        assert_eq!(
            context
                .contracts()
                .get("User::lookup")
                .map(NonNullContract::origin),
            Some(NonNullContractOrigin::InferenceScheme)
        );
        assert!(context.late_init().is_tracked("User::lookup"));
    }

    #[test]
    fn missing_snapshot_sets_degraded_mode() {
        let context = NullSafetyContext::hydrate(None);
        assert!(context.is_degraded());
        assert!(context.lattice().is_empty());
    }

    #[test]
    fn marks_unknown_facts_as_platform_when_metadata_missing() {
        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry(
            "external",
            FactsTypeKind::new(TypeVariant::Primitive("java.lang.String")),
        );
        let facts = builder.build();

        let context = NullSafetyContext::from_parts(Some(&facts), None, None);
        assert!(context.is_degraded());
        assert_eq!(
            context.lattice().get("external"),
            Some(NullabilityKind::Platform)
        );
    }

    #[test]
    fn applies_java_annotations_to_lattice() {
        let env = TypeEnvironment::new();
        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry(
            "external",
            FactsTypeKind::new(TypeVariant::Primitive("java.lang.String")),
        );
        builder.add_java_annotation("external", "@NotNull");
        let facts = builder.build();

        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);

        assert_eq!(
            context.lattice().get("external"),
            Some(NullabilityKind::NonNull)
        );

        let metadata = context
            .java_metadata()
            .get("external")
            .expect("metadata present");
        assert_eq!(
            metadata.nullability_hint(),
            Some(JavaNullabilityHint::NonNull)
        );
    }

    #[test]
    fn late_init_manifest_filters_required_symbols() {
        let mut env = TypeEnvironment::new();
        env.define_monotype("implicitVal", TypeKind::primitive(PrimitiveType::Int));
        env.define_monotype(
            "explicitInitialized",
            TypeKind::primitive(PrimitiveType::Int),
        );
        env.define_monotype("explicitLate", TypeKind::primitive(PrimitiveType::Int));
        env.define_monotype("annotatedLate", TypeKind::primitive(PrimitiveType::Int));
        env.define_monotype(
            "nullableVar",
            TypeKind::Optional(Box::new(TypeKind::reference("java.lang.String"))),
        );

        let mut builder = TypeFactsBuilder::new();
        for name in [
            "implicitVal",
            "explicitInitialized",
            "explicitLate",
            "annotatedLate",
        ] {
            builder.environment_entry(
                name,
                FactsTypeKind::new(TypeVariant::Primitive("Int"))
                    .with_nullability(NullabilityFlag::NonNull),
            );
        }
        builder.environment_entry(
            "nullableVar",
            FactsTypeKind::new(TypeVariant::Optional(Box::new(FactsTypeKind::new(
                TypeVariant::Primitive("String"),
            ))))
            .with_nullability(NullabilityFlag::Nullable),
        );
        let facts = builder.build();

        let mut seeds = HashMap::new();
        seeds.insert(
            "implicitVal".to_string(),
            LateInitSeed {
                name: "implicitVal".to_string(),
                origin: ValBindingOrigin::Implicit,
                has_initializer: true,
                explicit_late_init: false,
            },
        );
        seeds.insert(
            "explicitInitialized".to_string(),
            LateInitSeed {
                name: "explicitInitialized".to_string(),
                origin: ValBindingOrigin::ExplicitKeyword,
                has_initializer: true,
                explicit_late_init: false,
            },
        );
        seeds.insert(
            "explicitLate".to_string(),
            LateInitSeed {
                name: "explicitLate".to_string(),
                origin: ValBindingOrigin::ExplicitKeyword,
                has_initializer: false,
                explicit_late_init: false,
            },
        );
        seeds.insert(
            "annotatedLate".to_string(),
            LateInitSeed {
                name: "annotatedLate".to_string(),
                origin: ValBindingOrigin::ExplicitKeyword,
                has_initializer: false,
                explicit_late_init: true,
            },
        );
        seeds.insert(
            "nullableVar".to_string(),
            LateInitSeed {
                name: "nullableVar".to_string(),
                origin: ValBindingOrigin::ExplicitKeyword,
                has_initializer: true,
                explicit_late_init: false,
            },
        );

        let manifest = LateInitManifest::new(seeds);

        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), Some(&manifest));

        assert!(!context.late_init().is_tracked("implicitVal"));
        assert!(!context.late_init().is_tracked("explicitInitialized"));
        assert!(context.late_init().is_tracked("explicitLate"));
        assert!(context.late_init().is_tracked("annotatedLate"));
        assert!(context.late_init().is_tracked("nullableVar"));
    }

    #[test]
    fn implicit_nullable_val_registers_contract() {
        let mut env = TypeEnvironment::new();
        env.define_monotype("maybe", checker_optional("String"));

        let mut facts_builder = TypeFactsBuilder::new();
        facts_builder.environment_entry("maybe", facts_optional("String"));
        let facts = facts_builder.build();

        let mut seeds = HashMap::new();
        seeds.insert(
            "maybe".to_string(),
            LateInitSeed {
                name: "maybe".to_string(),
                origin: ValBindingOrigin::Implicit,
                has_initializer: true,
                explicit_late_init: false,
            },
        );
        let manifest = LateInitManifest::new(seeds);

        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), Some(&manifest));

        assert!(
            context.late_init_contracts().contains("maybe"),
            "implicit val with initializer should form a contract even if nullable"
        );
    }

    #[test]
    fn unknown_annotations_are_exposed_for_diagnostics() {
        let env = TypeEnvironment::new();
        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry(
            "mystery",
            FactsTypeKind::new(TypeVariant::Primitive("java.lang.Object")),
        );
        builder.add_java_annotation("mystery", "@MaybeNull");
        let facts = builder.build();

        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);
        let unknowns: Vec<_> = context.unknown_java_annotations().collect();

        assert_eq!(unknowns.len(), 1);
        assert_eq!(unknowns[0].0, "mystery");
        assert_eq!(unknowns[0].1, "MaybeNull");
    }

    #[test]
    fn higher_precedence_nullability_wins() {
        let env = TypeEnvironment::new();
        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry(
            "symbol",
            FactsTypeKind::new(TypeVariant::Primitive("java.lang.Object")),
        );
        builder.add_java_annotation("symbol", "@org.springframework.lang.NonNull");
        builder.add_java_annotation("symbol", "@org.jetbrains.annotations.Nullable");
        let facts = builder.build();

        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);
        let metadata = context
            .java_metadata()
            .get("symbol")
            .expect("metadata present");

        assert_eq!(
            metadata.nullability_hint(),
            Some(JavaNullabilityHint::Nullable)
        );
        assert_eq!(
            context.lattice().get("symbol"),
            Some(NullabilityKind::Nullable)
        );
    }

    #[test]
    fn null_marked_does_not_override_explicit_nullable() {
        let env = TypeEnvironment::new();
        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry(
            "symbol",
            FactsTypeKind::new(TypeVariant::Primitive("java.lang.Object")),
        );
        builder.add_java_annotation("symbol", "@org.jspecify.annotations.NullMarked");
        builder.add_java_annotation("symbol", "@jakarta.annotation.Nullable");
        let facts = builder.build();

        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env), None);
        let metadata = context
            .java_metadata()
            .get("symbol")
            .expect("metadata present");

        assert_eq!(
            metadata.nullability_hint(),
            Some(JavaNullabilityHint::Nullable)
        );
    }

    #[test]
    fn join_prefers_nullable_then_platform_then_unknown() {
        use NullabilityKind::*;
        assert_eq!(Nullable.join(Platform), Nullable);
        assert_eq!(Platform.join(NonNull), Platform);
        assert_eq!(Unknown.join(NonNull), Unknown);
    }
}
