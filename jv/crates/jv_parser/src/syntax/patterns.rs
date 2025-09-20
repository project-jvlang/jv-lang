use jv_ast::{Pattern, Span};

/// Temporary placeholder until dedicated pattern parsing is implemented.
pub(crate) fn placeholder_identifier_pattern() -> Pattern {
    Pattern::Identifier(
        "_temp_pattern".to_string(),
        Span {
            start_line: 0,
            start_column: 0,
            end_line: 0,
            end_column: 0,
        },
    )
}
