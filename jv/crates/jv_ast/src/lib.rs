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

// Span helper functions
impl Span {
    pub fn from_token_lexeme(line: usize, column: usize, lexeme: &str) -> Self {
        Self {
            start_line: line,
            start_column: column,
            end_line: line,
            end_column: column + lexeme.len(),
        }
    }

    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start_line: self.start_line,
            start_column: self.start_column,
            end_line: other.end_line,
            end_column: other.end_column,
        }
    }
}
