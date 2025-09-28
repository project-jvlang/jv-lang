//! Service-layer abstractions used to expose inference results to the rest of the compiler.
//!
//! `TypeFacts` acts as the shared contract between the inference engine and downstream
//! consumers such as the checker, code generator, and LSP server. Concrete implementations
//! live outside this crate so that each consumer can decide how to persist and surface data.

use std::hash::Hash;

/// Alias for spans reported by the inference pipeline.
pub type FactSpan = jv_ast::Span;

/// Describes the read-only facts that the inference engine exposes once analysis completes.
///
/// The trait is intentionally generic so that downstream crates can project their own
/// representations of environments, schemes, and diagnostic payloads while still sharing
/// a common vocabulary.
pub trait TypeFacts {
    /// Type of node identifiers used to query facts.
    type NodeId: Copy + Eq + Hash;
    /// Concrete type environment snapshot.
    type Environment;
    /// Binding information for type variables.
    type Binding;
    /// Function or let-binding schemes that callers can instantiate.
    type Scheme;
    /// Fully resolved type expressions.
    type Type;

    /// Returns the global type environment recorded by the inference engine.
    fn environment(&self) -> &Self::Environment;

    /// Returns all type variable bindings captured during solving.
    fn bindings(&self) -> &[Self::Binding];

    /// Resolves the type scheme associated with a named binding, if available.
    fn scheme_for(&self, name: &str) -> Option<&Self::Scheme>;

    /// Enumerates all known schemes as `(name, scheme)` pairs for downstream iteration.
    fn all_schemes(&self) -> Vec<(&str, &Self::Scheme)>;

    /// Queries the resolved type for the specified AST node identifier.
    fn type_for_node(&self, node: Self::NodeId) -> Option<&Self::Type>;

    /// Retrieves the inferred type for the outermost expression or declaration, if present.
    fn root_type(&self) -> Option<&Self::Type>;
}
