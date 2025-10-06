use super::*;
use jv_ir::JavaWildcardKind;

impl JavaCodeGenerator {
    /// Generate Java type signature from IR JavaType.
    ///
    /// This method handles all Java type representations including:
    /// - Primitive types
    /// - Reference types with generic arguments
    /// - Array types
    /// - Functional interface types
    /// - Wildcard types (? extends, ? super)
    /// - Void type
    pub fn generate_type(&self, java_type: &JavaType) -> Result<String, CodeGenError> {
        Ok(match java_type {
            JavaType::Primitive(name) => name.clone(),
            JavaType::Reference { name, generic_args } => {
                if generic_args.is_empty() {
                    name.clone()
                } else {
                    let mut rendered = Vec::new();
                    for arg in generic_args {
                        rendered.push(self.generate_type(arg)?);
                    }
                    format!("{}<{}>", name, rendered.join(", "))
                }
            }
            JavaType::Array {
                element_type,
                dimensions,
            } => {
                let base = self.generate_type(element_type)?;
                let suffix = "[]".repeat(*dimensions);
                format!("{}{}", base, suffix)
            }
            JavaType::Functional { interface_name, .. } => interface_name.clone(),
            JavaType::Wildcard { kind, bound } => match kind {
                JavaWildcardKind::Unbounded => "?".to_string(),
                JavaWildcardKind::Extends => {
                    let ty = bound
                        .as_ref()
                        .map(|inner| self.generate_type(inner))
                        .transpose()?
                        .unwrap_or_else(|| "Object".to_string());
                    format!("? extends {}", ty)
                }
                JavaWildcardKind::Super => {
                    let ty = bound
                        .as_ref()
                        .map(|inner| self.generate_type(inner))
                        .transpose()?
                        .unwrap_or_else(|| "Object".to_string());
                    format!("? super {}", ty)
                }
            },
            JavaType::Void => "void".to_string(),
        })
    }

    // Future extensions for java-generics-interop spec:
    // - generate_type_with_variance (Task 5: variance metadata handling)
    // - generate_raw_type_comment (Task 5: raw type continuation comments)
    // - emit_defensive_code (Task 5: defensive casts and null checks)
    // - emit_type_token (Task 4: type erasure helper)
}
