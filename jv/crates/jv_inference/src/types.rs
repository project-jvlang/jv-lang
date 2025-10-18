//! Rich type representation used by the jv Hindley-Milner inference engine.
//!
//! The new representation extends the historical `TypeKind` enum that lived inside
//! `jv_checker` by adding nullability flags, generic bound metadata, and SAT-friendly
//! bookkeeping. A lightweight compatibility layer is provided so that existing call
//! sites relying on the old enum-style API can migrate incrementally.

use crate::solver::Variance;
use jv_ast::Span;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fmt;
use std::sync::atomic::{AtomicU32, Ordering};

pub use jv_ast::types::{
    PrimitiveBound as PrimitiveBoundMetadata, PrimitiveTypeName, PrimitiveTypeReference,
    RawTypeContinuation, RawTypeDirective,
};

/// Utility helpers for working with primitive type metadata.
pub trait PrimitiveTypeNameExt {
    /// Returns the canonical primitive family for the provided primitive.
    fn canonical_family(self) -> PrimitiveTypeName;
    /// Returns the human-readable label used within the inference engine.
    fn type_label(self) -> &'static str;
}

impl PrimitiveTypeNameExt for PrimitiveTypeName {
    fn canonical_family(self) -> PrimitiveTypeName {
        match self {
            PrimitiveTypeName::Char | PrimitiveTypeName::Short => PrimitiveTypeName::Int,
            other => other,
        }
    }

    fn type_label(self) -> &'static str {
        match self {
            PrimitiveTypeName::Int => "Int",
            PrimitiveTypeName::Long => "Long",
            PrimitiveTypeName::Short => "Short",
            PrimitiveTypeName::Byte => "Byte",
            PrimitiveTypeName::Float => "Float",
            PrimitiveTypeName::Double => "Double",
            PrimitiveTypeName::Boolean => "Boolean",
            PrimitiveTypeName::Char => "Char",
        }
    }
}

/// Global counter used to mint fresh [`TypeId`] values.
static NEXT_TYPE_ID: AtomicU32 = AtomicU32::new(1);

/// Identifier assigned to type variables during inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(u32);

impl TypeId {
    /// Creates a new type identifier from its raw numeric representation.
    pub fn new(raw: u32) -> Self {
        Self(raw)
    }

    /// Returns the raw numeric identifier.
    pub fn to_raw(self) -> u32 {
        self.0
    }

    /// Mints a fresh identifier using the global counter.
    pub fn fresh() -> Self {
        let raw = NEXT_TYPE_ID.fetch_add(1, Ordering::Relaxed);
        TypeId::new(raw)
    }

    #[cfg(test)]
    pub(crate) fn reset_counter(value: u32) {
        NEXT_TYPE_ID.store(value, Ordering::Relaxed);
    }
}

/// Identifier assigned to resolved symbols (functions, constructors, etc.).
///
/// A dedicated type is used instead of reusing [`TypeId`] so that symbol
/// tracking can evolve independently from type variable allocation. The newtype
/// ensures we do not accidentally mix the two identifier spaces.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolId(String);

impl SymbolId {
    /// Creates a symbol identifier from a fully-qualified name.
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    /// Creates a symbol identifier from path segments such as module and name.
    pub fn from_path<I, S>(segments: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        let mut iter = segments.into_iter().map(Into::into);
        let mut buffer = String::new();
        if let Some(first) = iter.next() {
            buffer.push_str(&first);
        }
        for segment in iter {
            if !buffer.is_empty() {
                buffer.push_str("::");
            }
            buffer.push_str(&segment);
        }
        Self(buffer)
    }

    /// Returns the underlying symbol representation as `&str`.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for SymbolId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl From<&str> for SymbolId {
    fn from(value: &str) -> Self {
        SymbolId::new(value)
    }
}

impl From<String> for SymbolId {
    fn from(value: String) -> Self {
        SymbolId::new(value)
    }
}

/// Nullability metadata threaded through the inference pipeline.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NullabilityFlag {
    /// Nullability is currently unknown.
    Unknown,
    /// The type is known to be non-null.
    NonNull,
    /// The type can evaluate to `null`.
    Nullable,
}

impl Default for NullabilityFlag {
    fn default() -> Self {
        NullabilityFlag::Unknown
    }
}

impl NullabilityFlag {
    /// Combines two flags, favouring `Nullable` over `Unknown`, and `Unknown` over `NonNull`.
    pub fn combine(self, other: Self) -> Self {
        use NullabilityFlag::*;
        match (self, other) {
            (Nullable, _) | (_, Nullable) => Nullable,
            (Unknown, _) | (_, Unknown) => Unknown,
            _ => NonNull,
        }
    }

    /// Returns true if the flag semantically allows null.
    pub fn allows_null(self) -> bool {
        matches!(self, NullabilityFlag::Nullable | NullabilityFlag::Unknown)
    }
}

/// Record field representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldType {
    pub name: String,
    pub ty: TypeKind,
}

impl FieldType {
    pub fn new(name: impl Into<String>, ty: TypeKind) -> Self {
        Self {
            name: name.into(),
            ty,
        }
    }
}

/// Core variant selector for [`TypeKind`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeVariant {
    Primitive(&'static str),
    Optional(Box<TypeKind>),
    Function(Vec<TypeKind>, Box<TypeKind>),
    Record { fields: Vec<FieldType> },
    Union { arms: Vec<TypeKind> },
    Variable(TypeId),
    Unknown,
}

impl Default for TypeVariant {
    fn default() -> Self {
        TypeVariant::Unknown
    }
}

/// Constraint bound predicate.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BoundPredicate {
    Trait(TraitBound),
    Interface(String),
    Capability(CapabilityBound),
    FunctionSignature(FunctionSignatureBound),
    Primitive(PrimitiveBoundConstraint),
    WhereClause(Vec<BoundPredicate>),
}

impl BoundPredicate {
    pub fn key(&self) -> String {
        match self {
            BoundPredicate::Trait(bound) => bound.key(),
            BoundPredicate::Interface(name) => name.clone(),
            BoundPredicate::Capability(bound) => bound.key(),
            BoundPredicate::FunctionSignature(signature) => signature.key(),
            BoundPredicate::Primitive(bound) => bound.key(),
            BoundPredicate::WhereClause(predicates) => {
                let inner: Vec<String> = predicates.iter().map(|p| p.key()).collect();
                format!("where({})", inner.join("&"))
            }
        }
    }

    /// Human-readable description used by diagnostics and debugging helpers.
    pub fn describe(&self) -> String {
        match self {
            BoundPredicate::Trait(bound) => bound.describe(),
            BoundPredicate::Interface(name) => format!("interface {name}"),
            BoundPredicate::Capability(bound) => bound.describe(),
            BoundPredicate::FunctionSignature(signature) => signature.describe(),
            BoundPredicate::Primitive(bound) => bound.describe(),
            BoundPredicate::WhereClause(predicates) => {
                let inner: Vec<String> = predicates.iter().map(|p| p.describe()).collect();
                inner.join(" & ")
            }
        }
    }
}

/// Trait-style bound with optional type arguments.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitBound {
    pub name: String,
    pub arguments: Vec<BoundTypeReference>,
}

impl TraitBound {
    pub fn new(name: impl Into<String>, arguments: Vec<BoundTypeReference>) -> Self {
        Self {
            name: name.into(),
            arguments,
        }
    }

    pub fn simple(name: impl Into<String>) -> Self {
        Self::new(name, Vec::new())
    }

    fn key(&self) -> String {
        if self.arguments.is_empty() {
            self.name.clone()
        } else {
            let args = self
                .arguments
                .iter()
                .map(|arg| arg.key())
                .collect::<Vec<_>>()
                .join(",");
            format!("{}<{}>", self.name, args)
        }
    }

    fn describe(&self) -> String {
        if self.arguments.is_empty() {
            self.name.clone()
        } else {
            let args = self
                .arguments
                .iter()
                .map(|arg| arg.describe())
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}<{}>", self.name, args)
        }
    }
}

/// Capability-style bound analogous to type class constraints.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CapabilityBound {
    pub name: String,
    pub target: BoundTypeReference,
    pub hints: CapabilityHints,
}

impl CapabilityBound {
    pub fn new(
        name: impl Into<String>,
        target: BoundTypeReference,
        hints: CapabilityHints,
    ) -> Self {
        Self {
            name: name.into(),
            target,
            hints,
        }
    }

    fn key(&self) -> String {
        format!(
            "capability:{}:{}:{}",
            self.name,
            self.target.key(),
            self.hints.key()
        )
    }

    fn describe(&self) -> String {
        let mut base = format!("{} for {}", self.name, self.target.describe());
        if let Some(preferred) = &self.hints.preferred_impl {
            base.push_str(&format!(" (preferred impl: {preferred})"));
        }
        if self.hints.inline_only {
            base.push_str(" [inline-only]");
        }
        base
    }
}

/// Optional metadata provided when resolving capabilities.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct CapabilityHints {
    pub preferred_impl: Option<String>,
    pub inline_only: bool,
}

impl CapabilityHints {
    fn key(&self) -> String {
        let preferred = self
            .preferred_impl
            .as_ref()
            .map(String::as_str)
            .unwrap_or("_");
        format!("pref={preferred}:inline={}", self.inline_only)
    }
}

/// Dispatch style used when binding capability implementations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DispatchKind {
    /// Static dispatch through explicitly referenced implementation functions.
    Static,
    /// Inline expansion of the implementation (e.g. constexpr-style helpers).
    Inline,
    /// Dispatch via default methods provided by interfaces.
    DefaultMethod,
}

impl DispatchKind {
    /// Returns true when the dispatch kind satisfies inline-only requirements.
    pub fn supports_inline(self) -> bool {
        matches!(self, DispatchKind::Inline)
    }
}

/// Result of resolving a capability requirement against the environment.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CapabilitySolution {
    pub implementor: SymbolId,
    pub binding_type: TypeKind,
    pub dispatch_kind: DispatchKind,
}

impl CapabilitySolution {
    pub fn new(implementor: SymbolId, binding_type: TypeKind, dispatch_kind: DispatchKind) -> Self {
        Self {
            implementor,
            binding_type,
            dispatch_kind,
        }
    }
}

/// Function signature-style bound used for advanced constraints.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignatureBound {
    pub parameters: Vec<BoundTypeReference>,
    pub return_type: Option<BoundTypeReference>,
}

impl FunctionSignatureBound {
    pub fn new(
        parameters: Vec<BoundTypeReference>,
        return_type: Option<BoundTypeReference>,
    ) -> Self {
        Self {
            parameters,
            return_type,
        }
    }

    fn key(&self) -> String {
        let params = self
            .parameters
            .iter()
            .map(|p| p.key())
            .collect::<Vec<_>>()
            .join(",");
        let ret = self
            .return_type
            .as_ref()
            .map(|ty| ty.key())
            .unwrap_or_else(|| "void".to_string());
        format!("fn({params})->{ret}")
    }

    fn describe(&self) -> String {
        let params = self
            .parameters
            .iter()
            .map(|p| p.describe())
            .collect::<Vec<_>>()
            .join(", ");
        let ret = self
            .return_type
            .as_ref()
            .map(|ty| ty.describe())
            .unwrap_or_else(|| "Unit".to_string());
        format!("fn({params}) -> {ret}")
    }
}

/// Canonicalized primitive bound information surfaced from where clauses.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrimitiveBoundConstraint {
    canonical: PrimitiveTypeName,
    aliases: Vec<PrimitiveTypeName>,
}

impl PrimitiveBoundConstraint {
    /// Constructs a new constraint with the provided canonical family and alias list.
    pub fn new(canonical: PrimitiveTypeName, aliases: Vec<PrimitiveTypeName>) -> Self {
        let mut unique = Vec::new();
        for alias in aliases {
            if alias != canonical && !unique.contains(&alias) {
                unique.push(alias);
            }
        }
        unique.sort_by_key(|alias| alias.type_label());
        Self {
            canonical,
            aliases: unique,
        }
    }

    /// Returns the canonical primitive family.
    pub fn canonical(&self) -> PrimitiveTypeName {
        self.canonical
    }

    /// Returns the list of alias families in deterministic order.
    pub fn alias_families(&self) -> &[PrimitiveTypeName] {
        &self.aliases
    }

    /// Provides the full set of permitted primitive families (canonical + aliases).
    pub fn allowed_families(&self) -> impl Iterator<Item = PrimitiveTypeName> + '_ {
        std::iter::once(self.canonical).chain(self.aliases.iter().copied())
    }

    /// Indicates whether the provided type label (e.g. "Int") satisfies the constraint.
    pub fn accepts_label(&self, label: &str) -> bool {
        self.allowed_labels().any(|candidate| candidate == label)
    }

    fn allowed_labels(&self) -> impl Iterator<Item = &'static str> + '_ {
        self.allowed_families()
            .map(PrimitiveTypeNameExt::type_label)
    }

    fn key(&self) -> String {
        let mut alias_labels: Vec<&'static str> = self
            .aliases
            .iter()
            .copied()
            .map(PrimitiveTypeNameExt::type_label)
            .collect();
        alias_labels.sort_unstable();
        if alias_labels.is_empty() {
            format!("primitive:{}", self.canonical.type_label())
        } else {
            format!(
                "primitive:{}[{}]",
                self.canonical.type_label(),
                alias_labels.join("|")
            )
        }
    }

    fn describe(&self) -> String {
        if self.aliases.is_empty() {
            format!("primitive {}", self.canonical.type_label())
        } else {
            let alias_names = self
                .aliases
                .iter()
                .map(|alias| alias.type_label())
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "primitive {} (aliases: {alias_names})",
                self.canonical.type_label()
            )
        }
    }
}

/// Type reference captured while translating predicates.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BoundTypeReference {
    TypeParameter {
        id: TypeId,
        name: String,
    },
    Named(String),
    Optional(Box<BoundTypeReference>),
    Array(Box<BoundTypeReference>),
    Generic {
        name: String,
        arguments: Vec<BoundTypeReference>,
    },
    Function {
        parameters: Vec<BoundTypeReference>,
        return_type: Option<Box<BoundTypeReference>>,
    },
}

impl BoundTypeReference {
    pub fn key(&self) -> String {
        match self {
            BoundTypeReference::TypeParameter { id, name } => {
                format!("param:{name}:{}", id.to_raw())
            }
            BoundTypeReference::Named(name) => format!("named:{name}"),
            BoundTypeReference::Optional(inner) => format!("optional<{}>", inner.key()),
            BoundTypeReference::Array(inner) => format!("array<{}>", inner.key()),
            BoundTypeReference::Generic { name, arguments } => {
                let args = arguments
                    .iter()
                    .map(|arg| arg.key())
                    .collect::<Vec<_>>()
                    .join(",");
                format!("generic:{name}<{}>", args)
            }
            BoundTypeReference::Function {
                parameters,
                return_type,
            } => {
                let params = parameters
                    .iter()
                    .map(|param| param.key())
                    .collect::<Vec<_>>()
                    .join(",");
                let ret = return_type
                    .as_ref()
                    .map(|ty| ty.key())
                    .unwrap_or_else(|| "void".to_string());
                format!("fn<{params}->{ret}>")
            }
        }
    }

    pub fn describe(&self) -> String {
        match self {
            BoundTypeReference::TypeParameter { name, .. } => name.clone(),
            BoundTypeReference::Named(name) => name.clone(),
            BoundTypeReference::Optional(inner) => format!("{}?", inner.describe()),
            BoundTypeReference::Array(inner) => format!("{}[]", inner.describe()),
            BoundTypeReference::Generic { name, arguments } => {
                if arguments.is_empty() {
                    name.clone()
                } else {
                    let args = arguments
                        .iter()
                        .map(|arg| arg.describe())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}<{}>", name, args)
                }
            }
            BoundTypeReference::Function {
                parameters,
                return_type,
            } => {
                let params = parameters
                    .iter()
                    .map(|param| param.describe())
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret = return_type
                    .as_ref()
                    .map(|ty| ty.describe())
                    .unwrap_or_else(|| "Unit".to_string());
                format!("fn({params}) -> {ret}")
            }
        }
    }
}

/// Generic parameter declaration annotated with bounds, variance, and defaults.
#[derive(Debug, Clone, PartialEq)]
pub struct GenericParameterInfo {
    pub name: String,
    pub bounds: Vec<BoundTypeReference>,
    pub variance: Option<Variance>,
    pub default: Option<BoundTypeReference>,
    pub span: Span,
}

impl GenericParameterInfo {
    pub fn new(
        name: impl Into<String>,
        bounds: Vec<BoundTypeReference>,
        variance: Option<Variance>,
        default: Option<BoundTypeReference>,
        span: Span,
    ) -> Self {
        Self {
            name: name.into(),
            bounds,
            variance,
            default,
            span,
        }
    }

    pub fn has_bounds(&self) -> bool {
        !self.bounds.is_empty()
    }

    pub fn variance(&self) -> Option<Variance> {
        self.variance
    }

    pub fn default(&self) -> Option<&BoundTypeReference> {
        self.default.as_ref()
    }
}

/// where句内の個別制約をIR向けに正規化した構造。
#[derive(Debug, Clone, PartialEq)]
pub struct GenericWherePredicate {
    pub type_param: String,
    pub predicate: BoundPredicate,
    pub span: Span,
}

impl GenericWherePredicate {
    pub fn new(type_param: impl Into<String>, predicate: BoundPredicate, span: Span) -> Self {
        Self {
            type_param: type_param.into(),
            predicate,
            span,
        }
    }
}

/// Optional where句メタデータ。
#[derive(Debug, Clone, PartialEq)]
pub struct GenericWhereClause {
    pub predicates: Vec<GenericWherePredicate>,
    pub primitive_bounds: Vec<PrimitiveBoundMetadata>,
    pub span: Span,
}

impl GenericWhereClause {
    pub fn new(
        predicates: Vec<GenericWherePredicate>,
        primitive_bounds: Vec<PrimitiveBoundMetadata>,
        span: Span,
    ) -> Self {
        Self {
            predicates,
            primitive_bounds,
            span,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.predicates.is_empty() && self.primitive_bounds.is_empty()
    }

    pub fn primitive_bounds(&self) -> &[PrimitiveBoundMetadata] {
        &self.primitive_bounds
    }
}

/// Full generic signature metadata threaded through the inference pipeline.
#[derive(Debug, Clone, PartialEq)]
pub struct GenericSignature {
    pub parameters: Vec<GenericParameterInfo>,
    pub where_clause: Option<GenericWhereClause>,
    pub raw_directives: Vec<RawTypeDirective>,
    pub span: Span,
}

impl GenericSignature {
    pub fn new(
        parameters: Vec<GenericParameterInfo>,
        where_clause: Option<GenericWhereClause>,
        raw_directives: Vec<RawTypeDirective>,
        span: Span,
    ) -> Self {
        Self {
            parameters,
            where_clause,
            raw_directives,
            span,
        }
    }

    pub fn parameters(&self) -> &[GenericParameterInfo] {
        &self.parameters
    }

    pub fn where_clause(&self) -> Option<&GenericWhereClause> {
        self.where_clause.as_ref()
    }

    pub fn has_raw_directives(&self) -> bool {
        !self.raw_directives.is_empty()
    }

    pub fn is_empty(&self) -> bool {
        self.parameters.is_empty()
            && self
                .where_clause
                .as_ref()
                .map(GenericWhereClause::is_empty)
                .unwrap_or(true)
            && self.raw_directives.is_empty()
    }
}

impl Default for GenericSignature {
    fn default() -> Self {
        Self {
            parameters: Vec::new(),
            where_clause: None,
            raw_directives: Vec::new(),
            span: Span::default(),
        }
    }
}

/// Individual constraint between a type parameter and a predicate.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BoundConstraint {
    pub type_param: TypeId,
    pub predicate: BoundPredicate,
}

impl BoundConstraint {
    pub fn new(type_param: TypeId, predicate: BoundPredicate) -> Self {
        Self {
            type_param,
            predicate,
        }
    }
}

/// SAT-friendly boolean matrix describing which predicates are satisfied per type parameter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BoundsMatrix {
    symbols: Vec<String>,
    rows: Vec<BoundsMatrixRow>,
}

impl BoundsMatrix {
    fn from_constraints(constraints: &[BoundConstraint]) -> Self {
        let mut symbols: Vec<String> = Vec::new();
        let mut rows: BTreeMap<TypeId, Vec<bool>> = BTreeMap::new();

        for constraint in constraints {
            let key = constraint.predicate.key();
            let symbol_index = match symbols.iter().position(|s| s == &key) {
                Some(index) => index,
                None => {
                    symbols.push(key.clone());
                    for assignments in rows.values_mut() {
                        assignments.push(false);
                    }
                    symbols.len() - 1
                }
            };

            let entry = rows
                .entry(constraint.type_param)
                .or_insert_with(|| vec![false; symbols.len()]);
            if entry.len() < symbols.len() {
                entry.resize(symbols.len(), false);
            }
            entry[symbol_index] = true;
        }

        let rows = rows
            .into_iter()
            .map(|(type_param, assignments)| BoundsMatrixRow {
                type_param,
                assignments,
            })
            .collect();

        Self { symbols, rows }
    }

    /// Returns the predicate symbols tracked by the matrix in deterministic order.
    pub fn symbols(&self) -> &[String] {
        &self.symbols
    }

    /// Returns the individual rows that map type parameters to boolean assignments.
    pub fn rows(&self) -> &[BoundsMatrixRow] {
        &self.rows
    }
}

/// Single row of the [`BoundsMatrix`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BoundsMatrixRow {
    pub type_param: TypeId,
    pub assignments: Vec<bool>,
}

/// Generic bounds metadata including SAT reduction support.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericBounds {
    constraints: Vec<BoundConstraint>,
    matrix: BoundsMatrix,
}

impl GenericBounds {
    pub fn new(constraints: Vec<BoundConstraint>) -> Self {
        let matrix = BoundsMatrix::from_constraints(&constraints);
        Self {
            constraints,
            matrix,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.constraints.is_empty()
    }

    pub fn constraints(&self) -> &[BoundConstraint] {
        &self.constraints
    }

    pub fn matrix(&self) -> &BoundsMatrix {
        &self.matrix
    }

    /// Produces a new bounds set filtered to the specified type parameters.
    pub fn filter_for(&self, params: &[TypeId]) -> Self {
        if self.is_empty() || params.is_empty() {
            return GenericBounds::default();
        }

        let wanted: HashSet<_> = params.iter().copied().collect();
        let filtered = self
            .constraints
            .iter()
            .filter(|constraint| wanted.contains(&constraint.type_param))
            .cloned()
            .collect::<Vec<_>>();

        if filtered.is_empty() {
            GenericBounds::default()
        } else {
            GenericBounds::new(filtered)
        }
    }

    /// Returns `true` when one of the contained predicates matches `predicate`.
    pub fn contains_predicate(&self, predicate: &BoundPredicate) -> bool {
        let target = predicate.key();
        self.constraints
            .iter()
            .any(|constraint| constraint.predicate.key() == target)
    }
}

impl Default for GenericBounds {
    fn default() -> Self {
        Self::new(Vec::new())
    }
}

/// Complete type representation used across the inference pipeline.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeKind {
    variant: TypeVariant,
    nullability: NullabilityFlag,
    bounds: Option<GenericBounds>,
}

impl Default for TypeKind {
    fn default() -> Self {
        Self {
            variant: TypeVariant::Unknown,
            nullability: NullabilityFlag::Unknown,
            bounds: None,
        }
    }
}

impl TypeKind {
    /// Constructs a type from a [`TypeVariant`].
    pub fn new(variant: TypeVariant) -> Self {
        Self {
            variant,
            ..TypeKind::default()
        }
    }

    /// Convenience constructor for plain type variables.
    pub fn variable(id: TypeId) -> Self {
        TypeKind::new(TypeVariant::Variable(id))
    }

    /// Accessor for the inner variant.
    pub fn variant(&self) -> &TypeVariant {
        &self.variant
    }

    /// Mutable accessor for callers that need to tweak the variant in place.
    pub fn variant_mut(&mut self) -> &mut TypeVariant {
        &mut self.variant
    }

    /// Returns the current nullability flag.
    pub fn nullability(&self) -> NullabilityFlag {
        self.nullability
    }

    /// Returns true if the type allows null values.
    pub fn allows_null(&self) -> bool {
        self.nullability.allows_null()
    }

    /// Overrides the nullability flag, returning self for chaining.
    pub fn with_nullability(mut self, flag: NullabilityFlag) -> Self {
        self.nullability = flag;
        self
    }

    /// Attaches generic bounds metadata to the type, returning self for chaining.
    pub fn with_bounds(mut self, bounds: GenericBounds) -> Self {
        self.bounds = Some(bounds);
        self
    }

    /// Returns the bounds metadata, if available.
    pub fn bounds(&self) -> Option<&GenericBounds> {
        self.bounds.as_ref()
    }

    /// Convenience constructor for optional types.
    pub fn optional(inner: TypeKind) -> Self {
        TypeKind::new(TypeVariant::Optional(Box::new(inner)))
            .with_nullability(NullabilityFlag::Nullable)
    }

    /// Convenience constructor for function types.
    pub fn function(params: Vec<TypeKind>, return_type: TypeKind) -> Self {
        TypeKind::new(TypeVariant::Function(params, Box::new(return_type)))
    }

    /// Convenience constructor for record types.
    pub fn record(fields: Vec<FieldType>) -> Self {
        TypeKind::new(TypeVariant::Record { fields })
    }

    /// Convenience constructor for union types.
    pub fn union(arms: Vec<TypeKind>) -> Self {
        TypeKind::new(TypeVariant::Union { arms })
    }

    /// Returns true if the type or any nested type contains `Unknown`.
    pub fn contains_unknown(&self) -> bool {
        match &self.variant {
            TypeVariant::Unknown => true,
            TypeVariant::Primitive(_) | TypeVariant::Variable(_) => matches!(
                self.bounds.as_ref().map(GenericBounds::is_empty),
                Some(false)
            ),
            TypeVariant::Optional(inner) => inner.contains_unknown(),
            TypeVariant::Function(params, ret) => {
                params.iter().any(TypeKind::contains_unknown) || ret.contains_unknown()
            }
            TypeVariant::Record { fields } => {
                fields.iter().any(|field| field.ty.contains_unknown())
            }
            TypeVariant::Union { arms } => arms.iter().any(TypeKind::contains_unknown),
        }
    }

    /// Collects free type variables into a sorted Vec for deterministic output.
    pub fn free_type_vars(&self) -> Vec<TypeId> {
        let mut vars = BTreeSet::new();
        self.collect_free_type_vars_into(&mut vars);
        vars.into_iter().collect()
    }

    fn collect_free_type_vars_into(&self, acc: &mut BTreeSet<TypeId>) {
        match &self.variant {
            TypeVariant::Primitive(_) | TypeVariant::Unknown => {}
            TypeVariant::Variable(id) => {
                acc.insert(*id);
            }
            TypeVariant::Optional(inner) => inner.collect_free_type_vars_into(acc),
            TypeVariant::Function(params, ret) => {
                for param in params {
                    param.collect_free_type_vars_into(acc);
                }
                ret.collect_free_type_vars_into(acc);
            }
            TypeVariant::Record { fields } => {
                for field in fields {
                    field.ty.collect_free_type_vars_into(acc);
                }
            }
            TypeVariant::Union { arms } => {
                for arm in arms {
                    arm.collect_free_type_vars_into(acc);
                }
            }
        }
    }
}

/// Compatibility helpers targeting the legacy `TypeKind` enum that lived in `jv_checker`.
pub mod legacy {
    use super::*;

    /// Legacy representation that mirrors the pre-refactor enum style API.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum LegacyTypeKind {
        Primitive(&'static str),
        Optional(Box<LegacyTypeKind>),
        Function(Vec<LegacyTypeKind>, Box<LegacyTypeKind>),
        Record { fields: Vec<LegacyFieldType> },
        Union { arms: Vec<LegacyTypeKind> },
        Variable(TypeId),
        Unknown,
    }

    /// Record field representation used by the legacy enum.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct LegacyFieldType {
        pub name: String,
        pub ty: LegacyTypeKind,
    }

    impl LegacyFieldType {
        pub fn new(name: impl Into<String>, ty: LegacyTypeKind) -> Self {
            Self {
                name: name.into(),
                ty,
            }
        }
    }

    impl From<LegacyTypeKind> for TypeKind {
        fn from(legacy: LegacyTypeKind) -> Self {
            match legacy {
                LegacyTypeKind::Primitive(name) => TypeKind::new(TypeVariant::Primitive(name)),
                LegacyTypeKind::Optional(inner) => TypeKind::optional(TypeKind::from(*inner)),
                LegacyTypeKind::Function(params, ret) => {
                    let params = params.into_iter().map(TypeKind::from).collect();
                    let ret = TypeKind::from(*ret);
                    TypeKind::function(params, ret)
                }
                LegacyTypeKind::Record { fields } => {
                    let fields = fields
                        .into_iter()
                        .map(|field| FieldType::new(field.name, TypeKind::from(field.ty)))
                        .collect();
                    TypeKind::record(fields)
                }
                LegacyTypeKind::Union { arms } => {
                    let arms = arms.into_iter().map(TypeKind::from).collect();
                    TypeKind::union(arms)
                }
                LegacyTypeKind::Variable(id) => TypeKind::new(TypeVariant::Variable(id)),
                LegacyTypeKind::Unknown => TypeKind::default(),
            }
        }
    }

    impl From<TypeKind> for LegacyTypeKind {
        fn from(kind: TypeKind) -> Self {
            match kind.variant {
                TypeVariant::Primitive(name) => LegacyTypeKind::Primitive(name),
                TypeVariant::Optional(inner) => {
                    LegacyTypeKind::Optional(Box::new(LegacyTypeKind::from(*inner)))
                }
                TypeVariant::Function(params, ret) => {
                    let params = params.into_iter().map(LegacyTypeKind::from).collect();
                    let ret = LegacyTypeKind::from(*ret);
                    LegacyTypeKind::Function(params, Box::new(ret))
                }
                TypeVariant::Record { fields } => {
                    let fields = fields
                        .into_iter()
                        .map(|field| {
                            LegacyFieldType::new(field.name, LegacyTypeKind::from(field.ty))
                        })
                        .collect();
                    LegacyTypeKind::Record { fields }
                }
                TypeVariant::Union { arms } => {
                    let arms = arms.into_iter().map(LegacyTypeKind::from).collect();
                    LegacyTypeKind::Union { arms }
                }
                TypeVariant::Variable(id) => LegacyTypeKind::Variable(id),
                TypeVariant::Unknown => LegacyTypeKind::Unknown,
            }
        }
    }

    impl LegacyTypeKind {
        /// Compatibility helper mirroring the legacy `free_type_vars` method.
        pub fn free_type_vars(&self) -> Vec<TypeId> {
            let modern: TypeKind = self.clone().into();
            modern.free_type_vars()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::legacy::{LegacyFieldType, LegacyTypeKind};

    #[test]
    fn collects_free_type_vars_in_sorted_order() {
        let t_param = TypeKind::new(TypeVariant::Variable(TypeId::new(2)));
        let t_fn = TypeKind::function(
            vec![TypeKind::new(TypeVariant::Variable(TypeId::new(1)))],
            TypeKind::new(TypeVariant::Primitive("Int")),
        );
        let record = TypeKind::record(vec![FieldType::new("value", t_fn.clone())]);
        let union = TypeKind::union(vec![record, t_param]);

        let vars = union.free_type_vars();
        assert_eq!(vars, vec![TypeId::new(1), TypeId::new(2)]);
    }

    #[test]
    fn nullability_combination_prefers_nullable() {
        use NullabilityFlag::*;
        assert_eq!(NonNull.combine(Nullable), Nullable);
        assert_eq!(Nullable.combine(Unknown), Nullable);
        assert_eq!(Unknown.combine(Unknown), Unknown);
        assert_eq!(NonNull.combine(NonNull), NonNull);
    }

    #[test]
    fn optional_constructor_marks_nullability() {
        let inner = TypeKind::new(TypeVariant::Primitive("String"));
        let optional = TypeKind::optional(inner);
        assert_eq!(optional.nullability(), NullabilityFlag::Nullable);
    }

    #[test]
    fn bounds_matrix_tracks_constraints() {
        let constraints = vec![
            BoundConstraint::new(
                TypeId::new(0),
                BoundPredicate::Trait(TraitBound::simple("Clone")),
            ),
            BoundConstraint::new(
                TypeId::new(0),
                BoundPredicate::Trait(TraitBound::simple("Debug")),
            ),
            BoundConstraint::new(
                TypeId::new(1),
                BoundPredicate::Trait(TraitBound::simple("Clone")),
            ),
            BoundConstraint::new(
                TypeId::new(1),
                BoundPredicate::WhereClause(vec![BoundPredicate::Trait(TraitBound::simple(
                    "Send",
                ))]),
            ),
        ];
        let bounds = GenericBounds::new(constraints);
        let matrix = bounds.matrix();

        assert_eq!(matrix.symbols().len(), 3);
        assert_eq!(matrix.symbols()[0], "Clone");
        assert_eq!(matrix.symbols()[1], "Debug");
        assert_eq!(matrix.symbols()[2], "where(Send)");

        let row0 = matrix.rows()[0].clone();
        assert_eq!(row0.type_param, TypeId::new(0));
        assert_eq!(row0.assignments, vec![true, true, false]);

        let row1 = matrix.rows()[1].clone();
        assert_eq!(row1.type_param, TypeId::new(1));
        assert_eq!(row1.assignments, vec![true, false, true]);
    }

    #[test]
    fn legacy_conversion_round_trip_preserves_structure() {
        let legacy = LegacyTypeKind::Function(
            vec![LegacyTypeKind::Variable(TypeId::new(7))],
            Box::new(LegacyTypeKind::Record {
                fields: vec![LegacyFieldType::new(
                    "value",
                    LegacyTypeKind::Optional(Box::new(LegacyTypeKind::Primitive("Int"))),
                )],
            }),
        );

        let modern: TypeKind = legacy.clone().into();
        let round_trip: LegacyTypeKind = modern.into();
        assert_eq!(legacy, round_trip);
    }
}
