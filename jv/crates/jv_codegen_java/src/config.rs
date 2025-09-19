use serde::{Deserialize, Serialize};

/// Configuration options that drive Java code generation behaviour.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct JavaCodeGenConfig {
    /// Indentation string used when pretty-printing generated Java.
    pub indent: String,
    /// Whether to emit additional null-safety checks around potentially nullable expressions.
    pub extra_null_checks: bool,
    /// Whether the generator should leverage modern Java 25 language features when available.
    pub use_modern_features: bool,
    /// Whether to include source comments that help with debugging emitted code.
    pub include_source_comments: bool,
}

impl Default for JavaCodeGenConfig {
    fn default() -> Self {
        Self {
            indent: "    ".to_string(),
            extra_null_checks: true,
            use_modern_features: true,
            include_source_comments: false,
        }
    }
}
