// jv_parser_syntax - Syntax parsing facade for jv language
// This crate re-exports specialized syntax parsing crates to keep compile units small.

pub use jv_parser_syntax_expressions::{
    expression_parser, expressions, json, json_expression_parser, patterns,
};
pub use jv_parser_syntax_statements::{parameters, statement_parser, statements};
pub use jv_parser_syntax_support as support;
pub use jv_parser_syntax_support::{merge_spans, span_from_token};
