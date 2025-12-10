//! AST から HIR へのローワリング層。

mod context;
mod expressions;
mod literals;
mod params;
mod statements;
mod types;

pub use context::{
    LoweringContext, LoweringDiagnostic, LoweringDiagnosticSeverity, LoweringResult,
};

/// パース結果を HIR へローワリングする。
pub fn lower(source: &str, parse: &crate::parser::ParseResult) -> LoweringResult {
    statements::lower_statements(source, parse)
}

#[cfg(test)]
mod tests;
