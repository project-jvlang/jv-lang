use crate::inference::conversions::{ConversionOutcome, ConversionRulesEngine};
use crate::inference::environment::TypeEnvironment;
use crate::inference::types::TypeKind;

/// Evaluates assignability between two types, consulting the conversion catalog when available.
#[derive(Debug, Default, Clone, Copy)]
pub struct CompatibilityChecker;

impl CompatibilityChecker {
    pub fn analyze(env: &TypeEnvironment, from: &TypeKind, to: &TypeKind) -> ConversionOutcome {
        ConversionRulesEngine::analyze_with_catalog(from, to, env.conversion_catalog())
    }
}
