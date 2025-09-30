use jv_ast::Expression;

/// Placeholder normalizer that will host structural preprocessing for `when`
/// expressions. The struct is intentionally minimal for Task 2 and will be
/// extended in subsequent tasks with guard flattening and constructor mapping.
#[derive(Debug, Default)]
pub struct PatternNormalizer;

impl PatternNormalizer {
    pub fn new() -> Self {
        Self
    }

    #[allow(unused_variables)]
    pub fn normalize(&self, expression: &Expression) {
        // Normalization logic will be implemented in Task 3 when exhaustive
        // analysis is introduced.
    }
}
