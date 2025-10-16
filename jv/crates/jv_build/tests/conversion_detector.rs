use jv_build::metadata::{
    ConversionDetector, ConversionMethodKind, JavaMethodSignature, TypeEntry,
};
use jv_ir::types::JavaType;

fn build_entry_with_method(owner: &str, method: &str, signature: JavaMethodSignature) -> TypeEntry {
    let mut entry = TypeEntry::new(owner.to_string(), "java.util".to_string(), None);
    entry.instance_methods.insert(method.to_string(), signature);
    entry
}

#[test]
fn detects_collection_transform_rule() {
    let signature = JavaMethodSignature {
        parameters: Vec::new(),
        return_type: JavaType::Reference {
            name: "java.util.stream.Stream".to_string(),
            generic_args: Vec::new(),
        },
    };
    let entry = build_entry_with_method("java.util.List", "stream", signature);
    let detector = ConversionDetector::new();
    let conversions = detector.detect_conversions(&entry);

    assert!(conversions.iter().any(|conv| {
        conv.method_name == "stream"
            && conv.kind == ConversionMethodKind::CollectionTransform
            && conv.target_type
                == JavaType::Reference {
                    name: "java.util.stream.Stream".to_string(),
                    generic_args: Vec::new(),
                }
    }));
}

#[test]
fn detects_functional_transform_rule() {
    let signature = JavaMethodSignature {
        parameters: vec![JavaType::Functional {
            interface_name: "java.util.function.Function".to_string(),
            param_types: vec![JavaType::Reference {
                name: "java.lang.String".to_string(),
                generic_args: Vec::new(),
            }],
            return_type: Box::new(JavaType::Reference {
                name: "java.lang.Integer".to_string(),
                generic_args: Vec::new(),
            }),
        }],
        return_type: JavaType::Reference {
            name: "java.util.stream.Stream".to_string(),
            generic_args: Vec::new(),
        },
    };
    let entry = build_entry_with_method("java.util.stream.Stream", "map", signature);
    let detector = ConversionDetector::new();
    let conversions = detector.detect_conversions(&entry);

    assert!(conversions.iter().any(|conv| {
        conv.method_name == "map"
            && conv.kind == ConversionMethodKind::FunctionalTransform
            && !conv.is_static
    }));
}
