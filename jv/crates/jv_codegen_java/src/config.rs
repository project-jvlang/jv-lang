use jv_pm::JavaTarget;
use serde::{Deserialize, Serialize};

/// Configuration options that drive Java code generation behaviour.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct JavaCodeGenConfig {
    /// Indentation string used when pretty-printing generated Java.
    pub indent: String,
    /// Whether to emit additional null-safety checks around potentially nullable expressions.
    pub extra_null_checks: bool,
    /// Whether to include source comments that help with debugging emitted code.
    pub include_source_comments: bool,
    /// Target Java release that guides feature selection and fallbacks.
    pub target: JavaTarget,
    /// Class name used when wrapping script statements into an executable entrypoint.
    pub script_main_class: String,
}

impl Default for JavaCodeGenConfig {
    fn default() -> Self {
        Self {
            indent: "    ".to_string(),
            extra_null_checks: true,
            include_source_comments: false,
            target: JavaTarget::default(),
            script_main_class: "GeneratedMain".to_string(),
        }
    }
}

impl JavaCodeGenConfig {
    /// Construct a configuration tailored for a specific Java target while keeping other
    /// settings at their defaults.
    pub fn for_target(target: JavaTarget) -> Self {
        Self {
            target,
            ..Self::default()
        }
    }
}
