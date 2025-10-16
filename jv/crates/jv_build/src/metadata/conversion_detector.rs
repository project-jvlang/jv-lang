use crate::metadata::{JavaMethodSignature, TypeEntry};
use jv_ir::types::JavaType;

/// Kind of conversion detected for a helper method.
#[derive(Debug, Clone, PartialEq)]
pub enum ConversionMethodKind {
    DirectConversion,
    StaticFactory,
    Unboxing,
    CollectionTransform,
    FunctionalTransform,
}

/// Signature describing a detected conversion helper method.
#[derive(Debug, Clone)]
pub struct ConversionSignature {
    pub kind: ConversionMethodKind,
    pub source_type: JavaType,
    pub target_type: JavaType,
    pub method_name: String,
    pub is_static: bool,
    pub confidence: f32,
}

trait DetectionRule: Send + Sync {
    fn check_instance_method(
        &self,
        owner_type: &str,
        method_name: &str,
        signature: &JavaMethodSignature,
    ) -> Option<ConversionSignature>;

    fn check_static_method(
        &self,
        owner_type: &str,
        method_name: &str,
        signature: &JavaMethodSignature,
    ) -> Option<ConversionSignature>;
}

/// Rule detecting `to*` style conversions such as `toString`, `toArray`, etc.
struct ToMethodRule;

impl DetectionRule for ToMethodRule {
    fn check_instance_method(
        &self,
        owner_type: &str,
        method_name: &str,
        signature: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        if !method_name.starts_with("to") {
            return None;
        }

        let params_ok = signature.parameters.len() <= 1;
        if !params_ok {
            return None;
        }

        if method_name == "toString" {
            return Some(ConversionSignature {
                kind: ConversionMethodKind::DirectConversion,
                source_type: parse_owner_type(owner_type),
                target_type: JavaType::Reference {
                    name: "java.lang.String".to_string(),
                    generic_args: Vec::new(),
                },
                method_name: method_name.to_string(),
                is_static: false,
                confidence: 1.0,
            });
        }

        if type_differs_from_owner(owner_type, &signature.return_type) {
            return Some(ConversionSignature {
                kind: ConversionMethodKind::DirectConversion,
                source_type: parse_owner_type(owner_type),
                target_type: signature.return_type.clone(),
                method_name: method_name.to_string(),
                is_static: false,
                confidence: 0.9,
            });
        }

        None
    }

    fn check_static_method(
        &self,
        _: &str,
        _: &str,
        _: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        None
    }
}

/// Rule detecting `valueOf`, `parse*`, `of`, `from` style static factories.
struct StaticFactoryRule;

impl DetectionRule for StaticFactoryRule {
    fn check_instance_method(
        &self,
        _: &str,
        _: &str,
        _: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        None
    }

    fn check_static_method(
        &self,
        owner_type: &str,
        method_name: &str,
        signature: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        let is_factory = matches!(method_name, "valueOf" | "of" | "from" | "fromString")
            || method_name.starts_with("parse");

        if !is_factory || signature.parameters.len() != 1 {
            return None;
        }

        let param_type = signature.parameters[0].clone();
        let return_type = signature.return_type.clone();

        if return_type_matches_owner(owner_type, &return_type) {
            return Some(ConversionSignature {
                kind: ConversionMethodKind::StaticFactory,
                source_type: param_type,
                target_type: return_type,
                method_name: method_name.to_string(),
                is_static: true,
                confidence: 0.95,
            });
        }

        None
    }
}

/// Rule detecting `*Value` style unboxing helpers.
struct UnboxingRule;

impl DetectionRule for UnboxingRule {
    fn check_instance_method(
        &self,
        owner_type: &str,
        method_name: &str,
        signature: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        if !method_name.ends_with("Value") || !signature.parameters.is_empty() {
            return None;
        }

        if let JavaType::Primitive(prim_name) = &signature.return_type {
            let expected_owner = format!("java.lang.{}", capitalize(prim_name));
            if owner_type.ends_with(&expected_owner) {
                return Some(ConversionSignature {
                    kind: ConversionMethodKind::Unboxing,
                    source_type: parse_owner_type(owner_type),
                    target_type: signature.return_type.clone(),
                    method_name: method_name.to_string(),
                    is_static: false,
                    confidence: 1.0,
                });
            }
        }

        None
    }

    fn check_static_method(
        &self,
        _: &str,
        _: &str,
        _: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        None
    }
}

/// Rule detecting collection transforms such as `stream`, `iterator`, etc.
struct CollectionTransformRule;

impl DetectionRule for CollectionTransformRule {
    fn check_instance_method(
        &self,
        owner_type: &str,
        method_name: &str,
        signature: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        const KNOWN_TRANSFORMS: [&str; 8] = [
            "stream",
            "parallelStream",
            "iterator",
            "spliterator",
            "toArray",
            "entrySet",
            "keySet",
            "values",
        ];

        if !KNOWN_TRANSFORMS.contains(&method_name) {
            return None;
        }

        if signature.parameters.len() > 1 {
            return None;
        }

        Some(ConversionSignature {
            kind: ConversionMethodKind::CollectionTransform,
            source_type: parse_owner_type(owner_type),
            target_type: signature.return_type.clone(),
            method_name: method_name.to_string(),
            is_static: false,
            confidence: 0.95,
        })
    }

    fn check_static_method(
        &self,
        _: &str,
        _: &str,
        _: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        None
    }
}

/// Rule detecting functional transforms such as `map`, `flatMap`, `filter`, etc.
struct FunctionalTransformRule;

impl DetectionRule for FunctionalTransformRule {
    fn check_instance_method(
        &self,
        owner_type: &str,
        method_name: &str,
        signature: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        const FUNCTIONAL_NAMES: [&str; 5] = ["map", "flatMap", "filter", "reduce", "collect"];

        if !FUNCTIONAL_NAMES.contains(&method_name) {
            return None;
        }

        if !has_functional_parameter(signature) {
            return None;
        }

        Some(ConversionSignature {
            kind: ConversionMethodKind::FunctionalTransform,
            source_type: parse_owner_type(owner_type),
            target_type: signature.return_type.clone(),
            method_name: method_name.to_string(),
            is_static: false,
            confidence: 0.9,
        })
    }

    fn check_static_method(
        &self,
        _: &str,
        _: &str,
        _: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        None
    }
}

/// Detector combining all configured rules.
#[derive(Default)]
pub struct ConversionDetector {
    rules: Vec<Box<dyn DetectionRule>>,
}

impl ConversionDetector {
    pub fn new() -> Self {
        let rules: Vec<Box<dyn DetectionRule>> = vec![
            Box::new(ToMethodRule),
            Box::new(StaticFactoryRule),
            Box::new(UnboxingRule),
            Box::new(CollectionTransformRule),
            Box::new(FunctionalTransformRule),
        ];
        Self { rules }
    }

    pub fn detect_conversions(&self, type_entry: &TypeEntry) -> Vec<ConversionSignature> {
        let mut conversions = Vec::new();

        for (method_name, signature) in &type_entry.instance_methods {
            for rule in &self.rules {
                if let Some(conv) =
                    rule.check_instance_method(&type_entry.fqcn, method_name, signature)
                {
                    conversions.push(conv);
                }
            }
        }

        for (method_name, signature) in &type_entry.static_methods {
            for rule in &self.rules {
                if let Some(conv) =
                    rule.check_static_method(&type_entry.fqcn, method_name, signature)
                {
                    conversions.push(conv);
                }
            }
        }

        conversions
    }
}

fn has_functional_parameter(signature: &JavaMethodSignature) -> bool {
    signature.parameters.iter().any(|param| match param {
        JavaType::Functional { .. } => true,
        JavaType::Reference { name, .. } => {
            name.starts_with("java.util.function.") || name.starts_with("java.util.stream.")
        }
        _ => false,
    })
}

fn type_differs_from_owner(owner_type: &str, return_type: &JavaType) -> bool {
    match return_type {
        JavaType::Reference { name, .. } => name != owner_type,
        JavaType::Primitive(_) => true,
        JavaType::Array { .. } => true,
        JavaType::Functional { .. } => true,
        _ => false,
    }
}

fn return_type_matches_owner(owner_type: &str, return_type: &JavaType) -> bool {
    match return_type {
        JavaType::Reference { name, .. } => name == owner_type,
        JavaType::Primitive(prim) => {
            let expected_wrapper = format!("java.lang.{}", capitalize(prim));
            owner_type.ends_with(&expected_wrapper)
        }
        _ => false,
    }
}

fn parse_owner_type(owner_type: &str) -> JavaType {
    JavaType::Reference {
        name: owner_type.to_string(),
        generic_args: Vec::new(),
    }
}

fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().chain(chars).collect(),
        None => String::new(),
    }
}
