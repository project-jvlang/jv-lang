// jv_parser_syntax - Syntax parsing module for jv language
// Extracted from jv_parser for memory-efficient compilation

pub mod expressions;
pub mod json;
pub mod parameters;
pub mod patterns;
pub mod statements;
pub mod support;

// Re-export commonly used items
pub use statements::statement_parser;
