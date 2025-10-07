use super::*;
use jv_ast::Span;
use jv_ir::JavaWildcardKind;

const TYPE_TOKEN_ERROR_CODE: &str = "JV3201";

/// Planning result for handling Java's type erasure at code generation time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErasurePlan {
    /// A reusable Java expression that can serve as a runtime type token.
    ReuseTypeToken { token_expr: String },
    /// Require the user to provide an explicit type because no safe token exists.
    RequireExplicitType {
        message_key: &'static str,
        type_description: String,
    },
}

impl JavaCodeGenerator {
    /// Determine how to cope with Java's type erasure for the provided type.
    pub fn plan_erasure(&self, java_type: &JavaType) -> ErasurePlan {
        if let Some(token_expr) = Self::synthesize_type_token(java_type) {
            ErasurePlan::ReuseTypeToken { token_expr }
        } else {
            ErasurePlan::RequireExplicitType {
                message_key: TYPE_TOKEN_ERROR_CODE,
                type_description: Self::describe_type(java_type),
            }
        }
    }

    /// Emit a runtime type token or surface `JV3201` when no safe token can be generated.
    pub fn emit_type_token(
        &self,
        java_type: &JavaType,
        span: Option<Span>,
    ) -> Result<String, CodeGenError> {
        match self.plan_erasure(java_type) {
            ErasurePlan::ReuseTypeToken { token_expr } => Ok(token_expr),
            ErasurePlan::RequireExplicitType {
                message_key,
                type_description,
            } => Err(CodeGenError::GenericTypeError {
                message: format!(
                    "[{message_key}] cannot synthesize runtime type token for {type_description}"
                ),
                span,
            }),
        }
    }

    fn describe_type(java_type: &JavaType) -> String {
        match java_type {
            JavaType::Primitive(name) => name.clone(),
            JavaType::Reference { name, generic_args } => {
                if generic_args.is_empty() {
                    name.clone()
                } else {
                    let args: Vec<String> = generic_args.iter().map(Self::describe_type).collect();
                    format!("{}<{}>", name, args.join(", "))
                }
            }
            JavaType::Array {
                element_type,
                dimensions,
            } => {
                let suffix = "[]".repeat(*dimensions);
                format!("{}{}", Self::describe_type(element_type), suffix)
            }
            JavaType::Functional { interface_name, .. } => interface_name.clone(),
            JavaType::Wildcard { kind, bound } => match kind {
                JavaWildcardKind::Unbounded => "?".to_string(),
                JavaWildcardKind::Extends => {
                    let ty = bound
                        .as_ref()
                        .map(|inner| Self::describe_type(inner))
                        .unwrap_or_else(|| "Object".to_string());
                    format!("? extends {ty}")
                }
                JavaWildcardKind::Super => {
                    let ty = bound
                        .as_ref()
                        .map(|inner| Self::describe_type(inner))
                        .unwrap_or_else(|| "Object".to_string());
                    format!("? super {ty}")
                }
            },
            JavaType::Void => "void".to_string(),
        }
    }

    fn synthesize_type_token(java_type: &JavaType) -> Option<String> {
        Self::erasure_literal(java_type).map(|literal| format!("{literal}.class"))
    }

    fn erasure_literal(java_type: &JavaType) -> Option<String> {
        match java_type {
            JavaType::Primitive(name) => Some(name.clone()),
            JavaType::Reference { name, generic_args } => {
                if generic_args.is_empty() {
                    Some(name.clone())
                } else {
                    None
                }
            }
            JavaType::Array {
                element_type,
                dimensions,
            } => {
                let base = Self::erasure_literal(element_type)?;
                Some(format!("{}{}", base, "[]".repeat(*dimensions)))
            }
            JavaType::Functional { interface_name, .. } => Some(interface_name.clone()),
            JavaType::Wildcard { .. } => None,
            JavaType::Void => None,
        }
    }

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
