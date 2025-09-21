// jv_ast - Abstract Syntax Tree definitions for the jv language
//! This crate provides the Abstract Syntax Tree (AST) definitions for the jv programming language.
//!
//! The AST represents the structure of jv source code after parsing, including all expressions,
//! statements, types, and metadata needed for further processing by the compiler.

// Module declarations
pub mod expression;
pub mod statement;
pub mod types;
pub mod utils;

// Re-export all public types for convenient access
pub use expression::*;
pub use statement::*;
pub use types::*;
pub use utils::*;

#[cfg(test)]
mod tests;
