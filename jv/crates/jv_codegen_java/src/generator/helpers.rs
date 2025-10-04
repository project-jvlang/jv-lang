use super::*;
use jv_ir::JavaWildcardKind;

impl JavaCodeGenerator {
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

    pub fn generate_binary_op(&self, op: &BinaryOp) -> Result<String, CodeGenError> {
        Ok(match op {
            BinaryOp::Add => "+".to_string(),
            BinaryOp::Subtract => "-".to_string(),
            BinaryOp::Multiply => "*".to_string(),
            BinaryOp::Divide => "/".to_string(),
            BinaryOp::Modulo => "%".to_string(),
            BinaryOp::Equal => "==".to_string(),
            BinaryOp::NotEqual => "!=".to_string(),
            BinaryOp::Less => "<".to_string(),
            BinaryOp::LessEqual => "<=".to_string(),
            BinaryOp::Greater => ">".to_string(),
            BinaryOp::GreaterEqual => ">=".to_string(),
            BinaryOp::And => "&&".to_string(),
            BinaryOp::Or => "||".to_string(),
            BinaryOp::BitAnd => "&".to_string(),
            BinaryOp::BitOr => "|".to_string(),
            BinaryOp::BitXor => "^".to_string(),
            BinaryOp::PlusAssign => "+=".to_string(),
            BinaryOp::MinusAssign => "-=".to_string(),
            BinaryOp::MultiplyAssign => "*=".to_string(),
            BinaryOp::DivideAssign => "/=".to_string(),
            BinaryOp::RangeExclusive | BinaryOp::RangeInclusive => {
                return Err(CodeGenError::UnsupportedConstruct {
                    construct: "Range operators must be lowered before Java emission".to_string(),
                    span: None,
                })
            }
            BinaryOp::Elvis => {
                return Err(CodeGenError::UnsupportedConstruct {
                    construct: "Elvis operator requires specialised lowering".to_string(),
                    span: None,
                })
            }
        })
    }

    pub fn generate_modifiers(&self, modifiers: &IrModifiers) -> String {
        let mut parts = Vec::new();
        match modifiers.visibility {
            IrVisibility::Public => parts.push("public"),
            IrVisibility::Protected => parts.push("protected"),
            IrVisibility::Private => parts.push("private"),
            IrVisibility::Package => {}
        }
        if modifiers.is_abstract {
            parts.push("abstract");
        }
        if modifiers.is_sealed {
            if self.targeting.supports_sealed_types() {
                parts.push("sealed");
            } else if !modifiers.is_final {
                parts.push("final");
            }
        }
        if modifiers.is_static {
            parts.push("static");
        }
        if modifiers.is_final {
            parts.push("final");
        }
        if modifiers.is_synchronized {
            parts.push("synchronized");
        }
        if modifiers.is_native {
            parts.push("native");
        }
        if modifiers.is_strictfp {
            parts.push("strictfp");
        }
        parts.join(" ")
    }

    pub fn generate_parameter_modifiers(&self, modifiers: &IrModifiers) -> String {
        if modifiers.is_final {
            "final".to_string()
        } else {
            String::new()
        }
    }

    pub(super) fn literal_to_string(literal: &Literal) -> String {
        match literal {
            Literal::String(value) => format!("\"{}\"", Self::escape_string(value)),
            Literal::Number(value) => value.clone(),
            Literal::Boolean(value) => value.to_string(),
            Literal::Null => "null".to_string(),
            Literal::Character(value) => {
                let escaped = match value {
                    '\\' => "\\".to_string(),
                    '\n' => "\\n".to_string(),
                    '\r' => "\\r".to_string(),
                    '\t' => "\\t".to_string(),
                    '"' => "\"".to_string(),
                    '\'' => "\\'".to_string(),
                    ch => ch.to_string(),
                };
                format!("'{}'", escaped)
            }
        }
    }

    pub(super) fn escape_string(value: &str) -> String {
        value
            .replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
    }

    pub(super) fn push_lines(builder: &mut JavaSourceBuilder, text: &str) {
        for line in text.lines() {
            builder.push_line(line);
        }
    }

    pub(super) fn append_inline_comment(mut code: String, comment: &str) -> String {
        if code.is_empty() {
            return comment.to_string();
        }

        let mut lines: Vec<String> = code.lines().map(|line| line.to_string()).collect();

        if let Some(last_line) = lines.last_mut() {
            if !last_line.trim_end().is_empty() {
                last_line.push(' ');
            }
            last_line.push_str(comment);
        } else {
            lines.push(comment.to_string());
        }

        code.clear();
        for (index, line) in lines.iter().enumerate() {
            if index > 0 {
                code.push('\n');
            }
            code.push_str(line);
        }

        code
    }

    pub(super) fn is_default_only_case(case: &IrSwitchCase) -> bool {
        case.labels.len() == 1 && matches!(case.labels[0], IrCaseLabel::Default)
    }
}
