use std::collections::{HashMap, HashSet};

use crate::inference::{TypeEnvironment, TypeKind as CheckerTypeKind, TypeScheme};
use crate::{InferenceSnapshot, TypeInferenceService};
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
            Primitive(_) | Function(_, _) => NullabilityKind::NonNull,
        }
    }

    fn from_facts_type(ty: &FactsTypeKind) -> Self {
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

#[derive(Debug, Default, Clone)]
pub struct LateInitRegistry {
    required: HashSet<String>,
    exempt: HashSet<String>,
}

impl LateInitRegistry {
    pub fn new(lattice: &NullabilityLattice) -> Self {
        let required = lattice
            .iter()
            .filter_map(|(name, state)| {
                if matches!(state, NullabilityKind::NonNull) {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect();

        Self {
            required,
            exempt: HashSet::new(),
        }
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

/// Hydrates symbol tables and lattice state from inference snapshots for downstream flow analysis.
pub struct NullSafetyContext<'facts> {
    facts: Option<&'facts TypeFactsSnapshot>,
    lattice: NullabilityLattice,
    degraded: bool,
    late_init: LateInitRegistry,
}

impl<'facts> NullSafetyContext<'facts> {
    /// Builds the context from an optional inference snapshot, falling back to degraded mode.
    pub fn hydrate(snapshot: Option<&'facts InferenceSnapshot>) -> Self {
        match snapshot {
            Some(snapshot) => {
                Self::from_parts(Some(snapshot.type_facts()), Some(snapshot.environment()))
            }
            None => Self::fallback(),
        }
    }

    /// Public constructor used by tests and future tasks when facts are precomputed.
    pub fn from_parts(
        facts: Option<&'facts TypeFactsSnapshot>,
        environment: Option<&'facts TypeEnvironment>,
    ) -> Self {
        let mut lattice = NullabilityLattice::new();

        if let Some(env) = environment {
            for (name, scheme) in env.flattened_bindings() {
                let state = NullabilityKind::from_checker_scheme(&scheme);
                lattice.insert(name, state);
            }
        }

        if let Some(facts) = facts {
            for (name, ty) in facts.environment().values().iter() {
                let state = NullabilityKind::from_facts_type(ty);
                lattice.insert(name.clone(), state);
            }

            for (name, scheme) in facts.all_schemes() {
                let state = NullabilityKind::from_facts_type(&scheme.body);
                lattice.insert(name.to_string(), state);
            }
        }

        let late_init = LateInitRegistry::new(&lattice);

        Self {
            facts,
            lattice,
            degraded: facts.is_none() || environment.is_none(),
            late_init,
        }
    }

    fn fallback() -> Self {
        Self {
            facts: None,
            lattice: NullabilityLattice::new(),
            degraded: true,
            late_init: LateInitRegistry::default(),
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inference::types::TypeKind;
    use crate::inference::TypeEnvironment;
    use jv_inference::service::TypeFactsBuilder;
    use jv_inference::service::TypeScheme as FactsTypeScheme;
    use jv_inference::types::{NullabilityFlag, TypeKind as FactsTypeKind, TypeVariant};

    fn checker_optional(inner: &'static str) -> TypeKind {
        TypeKind::Optional(Box::new(TypeKind::Primitive(inner)))
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
        env.define_monotype("user_id", TypeKind::Primitive("Int"));
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

        let context = NullSafetyContext::from_parts(Some(&facts), Some(&env));

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

        let context = NullSafetyContext::from_parts(Some(&facts), None);
        assert!(context.is_degraded());
        assert_eq!(
            context.lattice().get("external"),
            Some(NullabilityKind::Platform)
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
