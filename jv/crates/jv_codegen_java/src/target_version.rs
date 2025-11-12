use jv_pm::JavaTarget;

/// Encapsulates target-specific feature flags and helper logic for the Java code generator.
#[derive(Debug, Clone, Copy)]
pub struct TargetedJavaEmitter {
    target: JavaTarget,
}

impl TargetedJavaEmitter {
    pub fn new(target: JavaTarget) -> Self {
        Self { target }
    }

    pub const fn target(&self) -> JavaTarget {
        self.target
    }

    pub const fn supports_sealed_types(&self) -> bool {
        matches!(self.target, JavaTarget::Java25)
    }

    pub const fn supports_collection_factories(&self) -> bool {
        matches!(self.target, JavaTarget::Java25)
    }

    pub const fn supports_pattern_switch(&self) -> bool {
        matches!(self.target, JavaTarget::Java25)
    }

    pub const fn supports_text_blocks(&self) -> bool {
        matches!(self.target, JavaTarget::Java25)
    }

    pub fn permits_clause(&self, permitted: &[String]) -> Option<String> {
        if !permitted.is_empty() && self.supports_sealed_types() {
            Some(format!(" permits {}", permitted.join(", ")))
        } else {
            None
        }
    }

    pub fn sealed_fallback_comment(&self, permitted: &[String]) -> Option<String> {
        if self.supports_sealed_types() || permitted.is_empty() {
            return None;
        }

        Some(format!(
            "// Fallback: sealed hierarchy permits [{}] emulated for Java {}",
            permitted.join(", "),
            self.target.as_str()
        ))
    }
}
