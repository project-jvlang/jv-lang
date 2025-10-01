use crate::error::CodeGenError;
use jv_ir::{IrCaseLabel, IrSwitchCase, IrTypeParameter, JavaType};
use std::collections::{HashMap, HashSet};

/// Maps IR and type-system constructs into Java-specific representations.
pub struct JavaTypeMapper;

impl JavaTypeMapper {
    /// Map primitive types to their Java equivalents.
    pub fn map_primitive_type(type_name: &str) -> Result<String, CodeGenError> {
        match type_name {
            "int" => Ok("int".to_string()),
            "bool" => Ok("boolean".to_string()),
            "string" => Ok("String".to_string()),
            "char" => Ok("char".to_string()),
            "byte" => Ok("byte".to_string()),
            "short" => Ok("short".to_string()),
            "long" => Ok("long".to_string()),
            "float" => Ok("float".to_string()),
            "double" => Ok("double".to_string()),
            "void" => Ok("void".to_string()),
            _ => Err(CodeGenError::UnsupportedType {
                type_name: type_name.to_string(),
                message: format!("Unsupported primitive type: {}", type_name),
            }),
        }
    }

    /// Map logical collection types into Java collection implementations.
    pub fn map_collection_type(
        type_name: &str,
        element_types: &[JavaType],
    ) -> Result<String, CodeGenError> {
        match type_name {
            "List" => {
                if element_types.len() != 1 {
                    return Err(CodeGenError::InvalidGenericArity {
                        expected: 1,
                        actual: element_types.len(),
                        type_name: "List".to_string(),
                    });
                }
                let element_type = Self::format_java_type(&element_types[0])?;
                Ok(format!("List<{}>", element_type))
            }
            "Set" => {
                if element_types.len() != 1 {
                    return Err(CodeGenError::InvalidGenericArity {
                        expected: 1,
                        actual: element_types.len(),
                        type_name: "Set".to_string(),
                    });
                }
                let element_type = Self::format_java_type(&element_types[0])?;
                Ok(format!("Set<{}>", element_type))
            }
            "Map" => {
                if element_types.len() != 2 {
                    return Err(CodeGenError::InvalidGenericArity {
                        expected: 2,
                        actual: element_types.len(),
                        type_name: "Map".to_string(),
                    });
                }
                let key_type = Self::format_java_type(&element_types[0])?;
                let value_type = Self::format_java_type(&element_types[1])?;
                Ok(format!("Map<{}, {}>", key_type, value_type))
            }
            "Array" => {
                if element_types.len() != 1 {
                    return Err(CodeGenError::InvalidGenericArity {
                        expected: 1,
                        actual: element_types.len(),
                        type_name: "Array".to_string(),
                    });
                }
                let element_type = Self::format_java_type(&element_types[0])?;
                Ok(format!("{}[]", element_type))
            }
            _ => Err(CodeGenError::UnsupportedType {
                type_name: type_name.to_string(),
                message: format!("Unsupported collection type: {}", type_name),
            }),
        }
    }

    /// Helper function to format JavaType into string representation
    fn format_java_type(java_type: &JavaType) -> Result<String, CodeGenError> {
        match java_type {
            JavaType::Primitive(name) => Self::map_primitive_type(name),
            JavaType::Reference { name, generic_args } => {
                if generic_args.is_empty() {
                    Ok(name.clone())
                } else {
                    let args: Result<Vec<_>, _> =
                        generic_args.iter().map(Self::format_java_type).collect();
                    Ok(format!("{}<{}>", name, args?.join(", ")))
                }
            }
            JavaType::Array {
                element_type,
                dimensions,
            } => {
                let element = Self::format_java_type(element_type)?;
                let brackets = "[]".repeat(*dimensions);
                Ok(format!("{}{}", element, brackets))
            }
            JavaType::Void => Ok("void".to_string()),
            JavaType::Functional { interface_name, .. } => Ok(interface_name.clone()),
        }
    }

    /// Render generic parameter definitions compatible with Java syntax.
    pub fn generate_generic_parameters(
        type_params: &[IrTypeParameter],
    ) -> Result<String, CodeGenError> {
        if type_params.is_empty() {
            return Ok(String::new());
        }

        let params: Result<Vec<_>, _> = type_params
            .iter()
            .map(|param| {
                let mut result = param.name.clone();
                if !param.bounds.is_empty() {
                    let bounds = Self::generate_type_bounds(&param.bounds)?;
                    result.push_str(&bounds);
                }
                Ok(result)
            })
            .collect();

        match params {
            Ok(p) => Ok(format!("<{}>", p.join(", "))),
            Err(e) => Err(e),
        }
    }

    /// Render generic type bounds clause.
    pub fn generate_type_bounds(bounds: &[JavaType]) -> Result<String, CodeGenError> {
        if bounds.is_empty() {
            return Ok(String::new());
        }

        let bound_strings: Result<Vec<_>, _> = bounds.iter().map(Self::format_java_type).collect();

        match bound_strings {
            Ok(bounds) => Ok(format!(" extends {}", bounds.join(" & "))),
            Err(e) => Err(e),
        }
    }
}

/// Builds Java expressions that ensure runtime null-safety guarantees.
pub struct NullSafetyGenerator;

impl NullSafetyGenerator {
    pub fn generate_null_check(expr: &str, java_type: &JavaType) -> String {
        match java_type {
            JavaType::Primitive(_) => {
                // Primitive types can't be null, no check needed
                expr.to_string()
            }
            JavaType::Reference { .. } | JavaType::Array { .. } => {
                // Generate null check for reference types
                format!("({} != null ? {} : null)", expr, expr)
            }
            JavaType::Void | JavaType::Functional { .. } => {
                // Void and functional types don't need null checks
                expr.to_string()
            }
        }
    }

    pub fn generate_optional_chaining(operations: &[String]) -> String {
        if operations.is_empty() {
            return "null".to_string();
        }

        if operations.len() == 1 {
            return format!("Optional.ofNullable({})", operations[0]);
        }

        // Chain multiple operations using Optional
        let mut result = format!("Optional.ofNullable({})", operations[0]);
        for op in &operations[1..] {
            result = format!("{}.map(x -> x.{})", result, op);
        }
        result + ".orElse(null)"
    }

    pub fn generate_elvis_operator(left_expr: &str, right_expr: &str) -> String {
        format!("({} != null ? {} : {})", left_expr, left_expr, right_expr)
    }
}

/// Emits Java 25 feature-specific constructs such as pattern matching and records.
pub struct Java25FeatureGenerator;

impl Java25FeatureGenerator {
    pub fn generate_pattern_switch(
        discriminant: &str,
        cases: &[IrSwitchCase],
    ) -> Result<String, CodeGenError> {
        if cases.is_empty() {
            return Err(CodeGenError::InvalidSwitchCases {
                message: "Switch expression must have at least one case".to_string(),
            });
        }

        let mut result = format!("switch ({}) {{\n", discriminant);

        for case in cases {
            for label in &case.labels {
                match label {
                    IrCaseLabel::Literal(value) => {
                        result.push_str(&format!("    case {:?} -> /* body */;\n", value));
                    }
                    IrCaseLabel::TypePattern {
                        type_name,
                        variable,
                    } => {
                        result.push_str(&format!(
                            "    case {} {} -> /* body */;\n",
                            type_name, variable
                        ));
                    }
                    IrCaseLabel::Range {
                        lower,
                        upper,
                        inclusive_end,
                        ..
                    } => {
                        result.push_str(&format!(
                            "    case RANGE[{:?}{}{:?}] -> /* body */;\n",
                            lower,
                            if *inclusive_end { "..=" } else { ".." },
                            upper
                        ));
                    }
                    IrCaseLabel::Default => {
                        result.push_str("    default -> /* body */;\n");
                    }
                }
            }
        }

        result.push('}');
        Ok(result)
    }

    pub fn generate_record_patterns(patterns: &[IrCaseLabel]) -> Result<String, CodeGenError> {
        let pattern_strings: Result<Vec<String>, CodeGenError> = patterns
            .iter()
            .map(|pattern| match pattern {
                IrCaseLabel::TypePattern {
                    type_name,
                    variable,
                } => Ok(format!("{} {}", type_name, variable)),
                IrCaseLabel::Literal(l) => Ok(format!("{:?}", l)),
                IrCaseLabel::Range {
                    lower,
                    upper,
                    inclusive_end,
                    ..
                } => Ok(format!(
                    "{:?}{}{:?}",
                    lower,
                    if *inclusive_end { "..=" } else { ".." },
                    upper
                )),
                IrCaseLabel::Default => Ok("_".to_string()),
            })
            .collect();

        match pattern_strings {
            Ok(patterns) => Ok(patterns.join(", ")),
            Err(_) => Err(CodeGenError::InvalidPattern {
                message: "Failed to generate record patterns".to_string(),
            }),
        }
    }

    pub fn generate_virtual_thread_creation(runnable_expr: &str) -> String {
        format!("Thread.ofVirtual().start({})", runnable_expr)
    }

    pub fn generate_text_block(content: &str) -> String {
        // Handle multiline strings as Java text blocks
        if content.contains('\n') {
            format!("\"\"\"\n{}\"\"\"\n", content)
        } else {
            // Single line strings remain as regular strings
            format!("\"{}\"", content.replace('"', "\\\""))
        }
    }
}

/// Tracks which imports need to be emitted for a compilation unit.
#[derive(Debug, Default, Clone)]
pub struct ImportManager {
    imports: HashMap<String, String>,
    java_lang_types: HashSet<String>,
}

impl ImportManager {
    pub fn new() -> Self {
        let mut java_lang_types = HashSet::new();
        java_lang_types.insert("String".to_string());
        java_lang_types.insert("Object".to_string());
        java_lang_types.insert("Integer".to_string());
        java_lang_types.insert("Boolean".to_string());
        java_lang_types.insert("Character".to_string());
        java_lang_types.insert("Double".to_string());
        java_lang_types.insert("Float".to_string());
        java_lang_types.insert("Long".to_string());
        java_lang_types.insert("Short".to_string());
        java_lang_types.insert("Byte".to_string());

        Self {
            imports: HashMap::new(),
            java_lang_types,
        }
    }

    pub fn add_import(&mut self, class_name: &str) {
        if self.java_lang_types.contains(class_name)
            || (!class_name.contains('.') && !class_name.starts_with("java."))
        {
            return;
        }
        self.imports
            .insert(class_name.to_string(), class_name.to_string());
    }

    pub fn add_standard_import(&mut self, import: StandardImport) {
        match import {
            StandardImport::CompletableFuture => {
                self.add_import("java.util.concurrent.CompletableFuture");
            }
            StandardImport::VirtualThread => {
                self.add_import("java.lang.Thread");
            }
            StandardImport::Collections => {
                self.add_import("java.util.*");
            }
            StandardImport::Optional => {
                self.add_import("java.util.Optional");
            }
        }
    }

    pub fn get_imports(&self) -> Vec<String> {
        let mut imports: Vec<_> = self.imports.values().cloned().collect();
        imports.sort();
        imports
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StandardImport {
    CompletableFuture,
    VirtualThread,
    Collections,
    Optional,
}
