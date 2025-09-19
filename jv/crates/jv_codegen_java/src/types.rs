use crate::error::CodeGenError;
use jv_ir::{IrCaseLabel, IrSwitchCase, IrTypeParameter, JavaType};
use std::collections::{HashMap, HashSet};

/// Maps IR and type-system constructs into Java-specific representations.
pub struct JavaTypeMapper;

impl JavaTypeMapper {
    /// Map primitive types to their Java equivalents.
    pub fn map_primitive_type(_type_name: &str) -> Result<String, CodeGenError> {
        panic!("not yet implemented: map_primitive_type")
    }

    /// Map logical collection types into Java collection implementations.
    pub fn map_collection_type(
        _type_name: &str,
        _element_types: &[JavaType],
    ) -> Result<String, CodeGenError> {
        panic!("not yet implemented: map_collection_type")
    }

    /// Render generic parameter definitions compatible with Java syntax.
    pub fn generate_generic_parameters(
        _type_params: &[IrTypeParameter],
    ) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_generic_parameters")
    }

    /// Render generic type bounds clause.
    pub fn generate_type_bounds(_bounds: &[JavaType]) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_type_bounds")
    }
}

/// Builds Java expressions that ensure runtime null-safety guarantees.
pub struct NullSafetyGenerator;

impl NullSafetyGenerator {
    pub fn generate_null_check(_expr: &str, _java_type: &JavaType) -> String {
        panic!("not yet implemented: generate_null_check")
    }

    pub fn generate_optional_chaining(_operations: &[String]) -> String {
        panic!("not yet implemented: generate_optional_chaining")
    }

    pub fn generate_elvis_operator(_left_expr: &str, _right_expr: &str) -> String {
        panic!("not yet implemented: generate_elvis_operator")
    }
}

/// Emits Java 25 feature-specific constructs such as pattern matching and records.
pub struct Java25FeatureGenerator;

impl Java25FeatureGenerator {
    pub fn generate_pattern_switch(
        _discriminant: &str,
        _cases: &[IrSwitchCase],
    ) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_pattern_switch")
    }

    pub fn generate_record_patterns(_patterns: &[IrCaseLabel]) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_record_patterns")
    }

    pub fn generate_virtual_thread_creation(_runnable_expr: &str) -> String {
        panic!("not yet implemented: generate_virtual_thread_creation")
    }

    pub fn generate_text_block(_content: &str) -> String {
        panic!("not yet implemented: generate_text_block")
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
