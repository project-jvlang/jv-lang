// jv_ast - Abstract Syntax Tree definitions for the jv language
//! This crate provides the Abstract Syntax Tree (AST) definitions for the jv programming language.
//!
//! The AST represents the structure of jv source code after parsing, including all expressions,
//! statements, types, and metadata needed for further processing by the compiler.

// Module declarations
pub mod annotation;
pub mod comments;
pub mod expression;
pub mod json;
pub mod statement;
pub mod strings;
pub mod types;
pub mod utils;

// Re-export all public types for convenient access
pub use annotation::*;
pub use comments::*;
pub use expression::*;
pub use json::*;
pub use statement::*;
pub use strings::*;
pub use types::*;
pub use utils::*;

#[cfg(test)]
mod tests;
