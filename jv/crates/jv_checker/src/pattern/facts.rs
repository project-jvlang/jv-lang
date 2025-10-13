//! Data structures shared across pattern matching analyses.

use std::collections::HashMap;

use jv_ast::Span;

/// Analysis facts produced by the pattern matcher.
///
/// The structure tracks missing cases discovered during exhaustiveness
/// analysis together with narrowing snapshots that downstream stages (null
/// safety, lowering, tooling) can consume.
#[derive(Debug, Clone, Default)]
pub struct PatternMatchFacts {
    missing_cases: Vec<MissingCase>,
    narrowing: NarrowingFacts,
}

impl PatternMatchFacts {
    /// Returns an empty fact set. Provided for readability at call sites.
    pub fn empty() -> Self {
        Self::default()
    }

    /// Constructs a fact set with the provided missing-case information.
    pub fn with_missing_cases(missing_cases: Vec<MissingCase>) -> Self {
        Self {
            missing_cases,
            narrowing: NarrowingFacts::default(),
        }
    }

    pub fn missing_cases(&self) -> &[MissingCase] {
        &self.missing_cases
    }

    pub fn into_missing_cases(self) -> Vec<MissingCase> {
        self.missing_cases
    }

    pub fn is_exhaustive(&self) -> bool {
        self.missing_cases.is_empty()
    }

    pub fn set_narrowing(&mut self, narrowing: NarrowingFacts) {
        self.narrowing = narrowing;
    }

    pub fn narrowing(&self) -> &NarrowingFacts {
        &self.narrowing
    }

    pub fn arm_narrowing(&self, arm_id: ArmId) -> Option<&NarrowingSnapshot> {
        self.narrowing.arm_facts.get(&arm_id)
    }

    pub fn fallback_narrowing(&self) -> Option<&NarrowingSnapshot> {
        self.narrowing.fallback.as_ref()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MissingCase {
    Boolean {
        missing: MissingBooleanCase,
        suggestion: MissingCaseSuggestion,
    },
    SealedVariant {
        type_name: String,
        variant: String,
        suggestion: MissingCaseSuggestion,
    },
    EnumConstant {
        enum_type: String,
        constant: String,
        suggestion: MissingCaseSuggestion,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MissingBooleanCase {
    True,
    False,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingCaseSuggestion {
    pub quick_fix_id: &'static str,
    pub label_en: String,
    pub label_ja: String,
}

impl MissingCaseSuggestion {
    pub fn new(
        quick_fix_id: &'static str,
        label_en: impl Into<String>,
        label_ja: impl Into<String>,
    ) -> Self {
        Self {
            quick_fix_id,
            label_en: label_en.into(),
            label_ja: label_ja.into(),
        }
    }
}

impl MissingCase {
    pub fn suggestion(&self) -> Option<&MissingCaseSuggestion> {
        match self {
            MissingCase::Boolean { suggestion, .. } => Some(suggestion),
            MissingCase::SealedVariant { suggestion, .. } => Some(suggestion),
            MissingCase::EnumConstant { suggestion, .. } => Some(suggestion),
        }
    }
}

/// Aggregated metrics describing cache behaviour for pattern analysis.
#[derive(Debug, Clone, Copy, Default)]
pub struct PatternCacheMetrics {
    pub hits: u64,
    pub misses: u64,
}

impl PatternCacheMetrics {
    pub fn record_hit(&mut self) {
        self.hits = self.hits.saturating_add(1);
    }

    pub fn record_miss(&mut self) {
        self.misses = self.misses.saturating_add(1);
    }
}

/// Identifier assigned to each `when` arm in source order.
pub type ArmId = usize;

/// Collection of branch narrowing facts for a `when` expression.
#[derive(Debug, Clone, Default)]
pub struct NarrowingFacts {
    arm_facts: HashMap<ArmId, NarrowingSnapshot>,
    fallback: Option<NarrowingSnapshot>,
}

impl NarrowingFacts {
    pub fn insert_arm(&mut self, arm_id: ArmId, snapshot: NarrowingSnapshot) {
        self.arm_facts.insert(arm_id, snapshot);
    }

    pub fn set_fallback(&mut self, snapshot: NarrowingSnapshot) {
        self.fallback = Some(snapshot);
    }

    pub fn arms(&self) -> impl Iterator<Item = (&ArmId, &NarrowingSnapshot)> {
        self.arm_facts.iter()
    }

    pub fn fallback(&self) -> Option<&NarrowingSnapshot> {
        self.fallback.as_ref()
    }
}

/// Nullability assumptions derived for a specific branch.
#[derive(Debug, Clone, Default)]
pub struct NarrowingSnapshot {
    on_match: Vec<NarrowedBinding>,
    on_mismatch: Vec<NarrowedBinding>,
    guard_span: Option<Span>,
    guard_evaluated: bool,
    span: Option<Span>,
}

impl NarrowingSnapshot {
    pub fn new(span: Option<Span>) -> Self {
        Self {
            on_match: Vec::new(),
            on_mismatch: Vec::new(),
            guard_span: None,
            guard_evaluated: false,
            span,
        }
    }

    pub fn push_on_match(&mut self, binding: NarrowedBinding) {
        self.on_match.push(binding);
    }

    pub fn push_on_mismatch(&mut self, binding: NarrowedBinding) {
        self.on_mismatch.push(binding);
    }

    pub fn set_guard_span(&mut self, span: Option<Span>) {
        self.guard_span = span;
    }

    pub fn mark_guard_evaluated(&mut self) {
        self.guard_evaluated = true;
    }

    pub fn on_match(&self) -> &[NarrowedBinding] {
        &self.on_match
    }

    pub fn on_mismatch(&self) -> &[NarrowedBinding] {
        &self.on_mismatch
    }

    pub fn guard_span(&self) -> Option<&Span> {
        self.guard_span.as_ref()
    }

    pub fn guard_evaluated(&self) -> bool {
        self.guard_evaluated
    }

    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct NarrowedBinding {
    pub variable: String,
    pub nullability: NarrowedNullability,
}

impl NarrowedBinding {
    pub fn new(variable: impl Into<String>, nullability: NarrowedNullability) -> Self {
        Self {
            variable: variable.into(),
            nullability,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NarrowedNullability {
    NonNull,
    Nullable,
    Unknown,
}

impl Default for NarrowedNullability {
    fn default() -> Self {
        NarrowedNullability::Unknown
    }
}
