//! Backwards-compatibility shim for Maven-compatible resolver strategy.
//!
//! The actual implementation lives in `maven_39.rs` to reflect the Maven 3.9
//! semantics and Jar handling pipeline. This module keeps the historical
//! `MavenCompatStrategy` name available.

pub use super::maven_39::MavenCompat39Strategy as MavenCompatStrategy;
