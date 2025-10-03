//! Rich type representation used by the jv Hindley-Milner inference engine.
//!
//! The new representation extends the historical `TypeKind` enum that lived inside
//! `jv_checker` by adding nullability flags, generic bound metadata, and SAT-friendly
//! bookkeeping. A lightweight compatibility layer is provided so that existing call
//! sites relying on the old enum-style API can migrate incrementally.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

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
    Trait(String),
    Interface(String),
    WhereClause(Vec<BoundPredicate>),
}

impl BoundPredicate {
    fn key(&self) -> String {
        match self {
            BoundPredicate::Trait(name) | BoundPredicate::Interface(name) => name.clone(),
            BoundPredicate::WhereClause(predicates) => {
                let inner: Vec<String> = predicates.iter().map(|p| p.key()).collect();
                format!("where({})", inner.join("&"))
            }
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
            BoundConstraint::new(TypeId::new(0), BoundPredicate::Trait("Clone".to_string())),
            BoundConstraint::new(TypeId::new(0), BoundPredicate::Trait("Debug".to_string())),
            BoundConstraint::new(TypeId::new(1), BoundPredicate::Trait("Clone".to_string())),
            BoundConstraint::new(
                TypeId::new(1),
                BoundPredicate::WhereClause(vec![BoundPredicate::Trait("Send".to_string())]),
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
