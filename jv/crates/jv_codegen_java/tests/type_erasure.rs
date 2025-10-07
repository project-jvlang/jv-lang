use jv_codegen_java::generator::{ErasurePlan, JavaCodeGenerator};
use jv_ir::JavaType;

fn simple_generator() -> JavaCodeGenerator {
    JavaCodeGenerator::new()
}

#[test]
fn plan_erasure_reuses_token_for_raw_reference() {
    let generator = simple_generator();
    let raw_type = JavaType::Reference {
        name: "java.util.List".to_string(),
        generic_args: vec![],
    };

    let plan = generator.plan_erasure(&raw_type);
    assert_eq!(
        plan,
        ErasurePlan::ReuseTypeToken {
            token_expr: "java.util.List.class".to_string()
        }
    );

    let emitted = generator
        .emit_type_token(&raw_type, None)
        .expect("token for raw reference");
    assert_eq!(emitted, "java.util.List.class");
}

#[test]
fn plan_erasure_requires_explicit_type_for_generic_reference() {
    let generator = simple_generator();
    let generic_type = JavaType::Reference {
        name: "java.util.List".to_string(),
        generic_args: vec![JavaType::Reference {
            name: "java.lang.String".to_string(),
            generic_args: vec![],
        }],
    };

    match generator.plan_erasure(&generic_type) {
        ErasurePlan::RequireExplicitType {
            message_key,
            type_description,
        } => {
            assert_eq!(message_key, "JV3201");
            assert!(type_description.contains("java.util.List"));
        }
        other => panic!("expected RequireExplicitType, got {:?}", other),
    }

    let err = generator
        .emit_type_token(&generic_type, None)
        .expect_err("generic type should fail");
    assert!(err.to_string().contains("JV3201"));
}

#[test]
fn plan_erasure_handles_array_of_allowed_type() {
    let generator = simple_generator();
    let array_type = JavaType::Array {
        element_type: Box::new(JavaType::Reference {
            name: "java.lang.String".to_string(),
            generic_args: vec![],
        }),
        dimensions: 2,
    };

    let plan = generator.plan_erasure(&array_type);
    assert_eq!(
        plan,
        ErasurePlan::ReuseTypeToken {
            token_expr: "java.lang.String[][].class".to_string()
        }
    );
}
