use super::{
    ConversionHelperCatalog, ConversionKind, ConversionMetadata, ConversionOutcome, HelperSpec,
    NullableGuard, NullableGuardReason,
};
use crate::inference::types::{PrimitiveType, TypeError, TypeKind};
use crate::java::JavaBoxingTable;
use jv_inference::registry::default_impl::{DefaultImplementationRegistry, ImplementationVariant};
use std::collections::BTreeSet;

/// 型変換規則を判定するエンジン。
#[derive(Debug, Default)]
pub struct ConversionRulesEngine;

impl ConversionRulesEngine {
    /// `from` から `to` への変換可能性を評価し、必要なメタデータを返す。
    pub fn analyze(from: &TypeKind, to: &TypeKind) -> ConversionOutcome {
        Self::analyze_with_catalog(from, to, None)
    }

    pub fn analyze_with_catalog(
        from: &TypeKind,
        to: &TypeKind,
        catalog: Option<&ConversionHelperCatalog>,
    ) -> ConversionOutcome {
        if matches!(from, TypeKind::Unknown) || matches!(to, TypeKind::Unknown) {
            return ConversionOutcome::Identity;
        }

        if from == to {
            return ConversionOutcome::Identity;
        }

        if matches!(from, TypeKind::Variable(_)) || matches!(to, TypeKind::Variable(_)) {
            return ConversionOutcome::Identity;
        }

        match (from, to) {
            (TypeKind::Optional(inner_from), TypeKind::Optional(inner_to)) => {
                Self::analyze_with_catalog(inner_from, inner_to, catalog)
            }
            (TypeKind::Optional(inner_from), target) => {
                match Self::analyze_with_catalog(inner_from, target, catalog) {
                    ConversionOutcome::Identity => ConversionOutcome::Allowed(
                        ConversionMetadata::new(ConversionKind::Identity).with_nullable_guard(
                            NullableGuard::new(NullableGuardReason::OptionalLift),
                        ),
                    ),
                    ConversionOutcome::Allowed(metadata) => {
                        if metadata.nullable_guard.is_some() {
                            ConversionOutcome::Allowed(metadata)
                        } else {
                            ConversionOutcome::Allowed(metadata.with_nullable_guard(
                                NullableGuard::new(NullableGuardReason::OptionalLift),
                            ))
                        }
                    }
                    ConversionOutcome::Rejected(err) => ConversionOutcome::Rejected(err),
                }
            }
            (source, TypeKind::Optional(inner_to)) => {
                // Optional<T> への流入。T を解決した上で Optional へ昇格する。
                match Self::analyze_with_catalog(source, inner_to, catalog) {
                    ConversionOutcome::Identity => ConversionOutcome::Allowed(
                        ConversionMetadata::new(ConversionKind::Identity),
                    ),
                    ConversionOutcome::Allowed(metadata) => ConversionOutcome::Allowed(metadata),
                    ConversionOutcome::Rejected(err) => ConversionOutcome::Rejected(err),
                }
            }
            (TypeKind::Primitive(from_prim), TypeKind::Primitive(to_prim)) => {
                Self::analyze_primitive_widening(*from_prim, *to_prim)
            }
            (TypeKind::Primitive(from_prim), TypeKind::Boxed(to_prim)) if from_prim == to_prim => {
                ConversionOutcome::Allowed(ConversionMetadata::new(ConversionKind::Boxing))
            }
            (TypeKind::Boxed(from_prim), TypeKind::Primitive(to_prim)) if from_prim == to_prim => {
                ConversionOutcome::Allowed(
                    ConversionMetadata::new(ConversionKind::Unboxing)
                        .with_nullable_guard(NullableGuard::new(NullableGuardReason::Unboxing)),
                )
            }
            (_, TypeKind::Reference(name)) if name == "java.lang.String" => {
                let owner = Self::string_helper_owner(from);
                let metadata = ConversionMetadata::new(ConversionKind::StringConversion)
                    .with_helper(HelperSpec::instance(owner, "toString"));
                ConversionOutcome::Allowed(metadata)
            }
            (TypeKind::Reference(left), TypeKind::Reference(right)) => {
                if Self::references_are_compatible(left, right) {
                    return ConversionOutcome::Identity;
                }
                ConversionOutcome::Rejected(TypeError::incompatible_conversion(
                    from.describe(),
                    to.describe(),
                ))
            }
            _ => {
                if let Some(lookup) = catalog {
                    if let Some(helper) = lookup.find_helper(from, to) {
                        return ConversionOutcome::Allowed(
                            ConversionMetadata::new(ConversionKind::MethodInvocation)
                                .with_helper(helper),
                        );
                    }
                }
                ConversionOutcome::Rejected(TypeError::incompatible_conversion(
                    from.describe(),
                    to.describe(),
                ))
            }
        }
    }

    fn analyze_primitive_widening(from: PrimitiveType, to: PrimitiveType) -> ConversionOutcome {
        if from == to {
            return ConversionOutcome::Identity;
        }

        if from.widening_targets().contains(&to) {
            ConversionOutcome::Allowed(ConversionMetadata::new(ConversionKind::WideningPrimitive))
        } else {
            ConversionOutcome::Rejected(TypeError::incompatible_conversion(
                TypeKind::primitive(from).describe(),
                TypeKind::primitive(to).describe(),
            ))
        }
    }

    fn references_are_compatible(left: &str, right: &str) -> bool {
        let registry = DefaultImplementationRegistry::shared();
        let left_targets = Self::collect_registry_targets(left, registry);
        let right_targets = Self::collect_registry_targets(right, registry);
        if !left_targets.is_disjoint(&right_targets) {
            return true;
        }
        false
    }

    fn collect_registry_targets(
        name: &str,
        registry: &DefaultImplementationRegistry,
    ) -> BTreeSet<String> {
        let mut targets = BTreeSet::new();
        targets.insert(Self::normalize_reference_name(name));

        if let Some(entry) =
            registry.resolve_interface_variant(name, ImplementationVariant::Mutable)
        {
            targets.insert(entry.target().to_string());
        }
        if let Some(entry) =
            registry.resolve_interface_variant(name, ImplementationVariant::Immutable)
        {
            targets.insert(entry.target().to_string());
        }
        if let Some(entry) = registry.resolve_abstract(name, None) {
            targets.insert(entry.target().to_string());
        }

        targets
    }

    fn normalize_reference_name(name: &str) -> String {
        name.trim().replace('/', ".")
    }

    fn string_helper_owner(ty: &TypeKind) -> String {
        match ty {
            TypeKind::Primitive(prim) | TypeKind::Boxed(prim) => {
                JavaBoxingTable::boxed_fqcn(*prim).to_string()
            }
            TypeKind::Reference(name) => name.clone(),
            TypeKind::Optional(inner) => Self::string_helper_owner(inner),
            TypeKind::Function(_, _) => "java.lang.Object".to_string(),
            TypeKind::Variable(_) | TypeKind::Unknown => "java.lang.Object".to_string(),
        }
    }
}
