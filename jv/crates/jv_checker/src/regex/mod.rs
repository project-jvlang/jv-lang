//! Regex validation utilities used by the checker.

mod const_fold;
mod validator;

pub use const_fold::{PatternConstAnalyzer, PatternConstKind};
pub use validator::{RegexAnalysis, RegexValidator};
