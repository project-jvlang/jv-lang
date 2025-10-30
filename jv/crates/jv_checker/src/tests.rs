use super::*;
use crate::inference::environment::{TypeEnvironment, TypeScheme};
use crate::inference::types::TypeBinding;
use crate::inference::{PrimitiveType, TypeKind};
use crate::pattern::{self, PatternTarget};
use crate::regex::RegexValidator;
use fastrand::Rng;
use jv_ast::{
    Annotation, AnnotationName, BinaryMetadata, BinaryOp, Expression, Literal, Modifiers,
    Parameter, ParameterModifiers, Pattern, Program, RegexLiteral, Span, Statement, TypeAnnotation,
    ValBindingOrigin, WhenArm,
};
use jv_inference::TypeFacts;
use jv_inference::types::{NullabilityFlag, TypeVariant as FactsTypeVariant};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;
use std::collections::HashMap;

fn dummy_span() -> Span {
    Span::new(1, 0, 1, 5)
}

fn default_modifiers() -> Modifiers {
    Modifiers::default()
}

fn annotation(name: &str) -> Annotation {
    Annotation {
        name: AnnotationName::new(vec![name.to_string()], dummy_span()),
        arguments: Vec::new(),
        span: dummy_span(),
    }
}

fn collect_null_safety_messages(errors: &[CheckError]) -> Vec<String> {
    errors
        .iter()
        .filter_map(|error| match error {
            CheckError::NullSafetyError(message) => Some(message.clone()),
            _ => None,
        })
        .collect()
}

fn parse_program(source: &str) -> Program {
    RowanPipeline::default()
        .parse(source)
        .expect("source snippet should parse")
        .into_program()
}

fn random_identifier(rng: &mut Rng) -> String {
    const START: &[u8] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
    const CONT: &[u8] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789";

    let length = rng.usize(1..=8);
    let mut ident = String::with_capacity(length);
    ident.push(START[rng.usize(0..START.len())] as char);
    for _ in 1..length {
        ident.push(CONT[rng.usize(0..CONT.len())] as char);
    }
    ident
}

#[test]
fn convert_type_kind_exports_non_null_primitives() {
    let facts = convert_type_kind(&TypeKind::primitive(PrimitiveType::Int));
    assert_eq!(facts.nullability(), NullabilityFlag::NonNull);
    assert!(matches!(
        facts.variant(),
        FactsTypeVariant::Primitive(name) if *name == "Int"
    ));
}

#[test]
fn convert_type_kind_exports_non_null_functions() {
    let function = TypeKind::Function(
        vec![TypeKind::primitive(PrimitiveType::Boolean)],
        Box::new(TypeKind::primitive(PrimitiveType::Int)),
    );
    let facts = convert_type_kind(&function);
    assert_eq!(facts.nullability(), NullabilityFlag::NonNull);

    if let FactsTypeVariant::Function(params, ret) = facts.variant() {
        assert_eq!(params.len(), 1);
        assert!(matches!(
            params[0].variant(),
            FactsTypeVariant::Primitive(name) if *name == "Boolean"
        ));
        assert!(matches!(
            ret.variant(),
            FactsTypeVariant::Primitive(name) if *name == "Int"
        ));
    } else {
        panic!("expected function variant");
    }
}

#[test]
fn convert_type_kind_preserves_optional_nullability() {
    let optional = TypeKind::optional(TypeKind::primitive(PrimitiveType::Int));
    let facts = convert_type_kind(&optional);

    assert_eq!(facts.nullability(), NullabilityFlag::Nullable);
    if let FactsTypeVariant::Optional(inner) = facts.variant() {
        assert_eq!(inner.nullability(), NullabilityFlag::NonNull);
        assert!(matches!(
            inner.variant(),
            FactsTypeVariant::Primitive(name) if name == &"Int"
        ));
    } else {
        panic!("expected optional variant");
    }
}

#[test]
fn build_type_facts_propagates_environment_nullability() {
    let mut environment = TypeEnvironment::new();
    environment.define_monotype("user_id", TypeKind::primitive(PrimitiveType::Int));
    environment.define_monotype(
        "maybe_email",
        TypeKind::optional(TypeKind::reference("java.lang.String")),
    );

    let bindings: Vec<TypeBinding> = Vec::new();
    let function_schemes: HashMap<String, TypeScheme> = HashMap::new();

    let facts = build_type_facts(&environment, &bindings, &function_schemes, None);
    let env_facts = facts.environment().values();

    assert_eq!(
        env_facts
            .get("user_id")
            .expect("user_id fact")
            .nullability(),
        NullabilityFlag::NonNull
    );
    assert_eq!(
        env_facts
            .get("maybe_email")
            .expect("maybe_email fact")
            .nullability(),
        NullabilityFlag::Nullable
    );
}

#[test]
fn build_type_facts_propagates_result_type_nullability() {
    let environment = TypeEnvironment::new();
    let bindings: Vec<TypeBinding> = Vec::new();
    let function_schemes: HashMap<String, TypeScheme> = HashMap::new();
    let result_type = TypeKind::primitive(PrimitiveType::Boolean);

    let facts = build_type_facts(
        &environment,
        &bindings,
        &function_schemes,
        Some(&result_type),
    );

    let root = facts.root_type().expect("root type");
    assert_eq!(root.nullability(), NullabilityFlag::NonNull);
    assert!(matches!(
        root.variant(),
        FactsTypeVariant::Primitive(name) if name == &"Boolean"
    ));
}

#[test]
fn check_program_populates_inference_snapshot() {
    let span = dummy_span();
    let function_body = Expression::Binary {
        left: Box::new(Expression::Identifier("lhs".into(), span.clone())),
        op: BinaryOp::Add,
        right: Box::new(Expression::Identifier("rhs".into(), span.clone())),
        span: span.clone(),
        metadata: BinaryMetadata::default(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![
            Statement::ValDeclaration {
                name: "lhs".into(),
                binding: None,

                type_annotation: None,
                initializer: Expression::Literal(Literal::Number("1".into()), span.clone()),
                modifiers: default_modifiers(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: span.clone(),
            },
            Statement::ValDeclaration {
                name: "rhs".into(),
                binding: None,

                type_annotation: None,
                initializer: Expression::Literal(Literal::Number("2".into()), span.clone()),
                modifiers: default_modifiers(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: span.clone(),
            },
            Statement::FunctionDeclaration {
                name: "add".into(),
                type_parameters: Vec::new(),
                generic_signature: None,
                where_clause: None,
                parameters: vec![
                    Parameter {
                        name: "lhs".into(),
                        type_annotation: None,
                        default_value: None,
                        modifiers: ParameterModifiers::default(),
                        span: span.clone(),
                    },
                    Parameter {
                        name: "rhs".into(),
                        type_annotation: None,
                        default_value: None,
                        modifiers: ParameterModifiers::default(),
                        span: span.clone(),
                    },
                ],
                return_type: None,
                primitive_return: None,
                body: Box::new(function_body),
                modifiers: default_modifiers(),
                span: span.clone(),
            },
        ],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_ok(), "expected successful inference: {result:?}");

    let snapshot = checker
        .inference_snapshot()
        .expect("inference snapshot should be populated");
    assert!(snapshot.function_scheme("add").is_some());
    assert!(!snapshot.bindings().is_empty());

    let manifest = snapshot.late_init_manifest();
    let lhs_seed = manifest
        .get("lhs")
        .expect("lhs seed should be captured in snapshot");
    assert_eq!(lhs_seed.origin, ValBindingOrigin::ExplicitKeyword);
    assert!(lhs_seed.has_initializer);

    let rhs_seed = manifest
        .get("rhs")
        .expect("rhs seed should be captured in snapshot");
    assert_eq!(rhs_seed.origin, ValBindingOrigin::ExplicitKeyword);
    assert!(rhs_seed.has_initializer);
}

#[test]
fn binding_resolver_collects_late_init_metadata() {
    let span = dummy_span();

    let explicit_val = Statement::ValDeclaration {
        name: "explicitVal".into(),
        binding: None,

        type_annotation: None,
        initializer: Expression::Literal(Literal::Number("1".into()), span.clone()),
        modifiers: default_modifiers(),
        origin: ValBindingOrigin::ExplicitKeyword,
        span: span.clone(),
    };

    let implicit_val = Statement::ValDeclaration {
        name: "implicitVal".into(),
        binding: None,

        type_annotation: None,
        initializer: Expression::Literal(Literal::Number("2".into()), span.clone()),
        modifiers: default_modifiers(),
        origin: ValBindingOrigin::Implicit,
        span: span.clone(),
    };

    let mut late_init_modifiers = default_modifiers();
    late_init_modifiers.annotations.push(annotation("LateInit"));

    let late_var = Statement::VarDeclaration {
        name: "lateVar".into(),
        binding: None,
        type_annotation: None,
        initializer: None,
        modifiers: late_init_modifiers,
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![explicit_val, implicit_val, late_var],
        span,
    };

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("binding resolution should succeed");

    let manifest = checker.late_init_manifest();

    let explicit = manifest
        .get("explicitVal")
        .expect("explicit val seed should be recorded");
    assert_eq!(explicit.origin, ValBindingOrigin::ExplicitKeyword);
    assert!(explicit.has_initializer);
    assert!(!explicit.explicit_late_init);

    let implicit = manifest
        .get("implicitVal")
        .expect("implicit val seed should be recorded");
    assert_eq!(implicit.origin, ValBindingOrigin::Implicit);
    assert!(implicit.has_initializer);
    assert!(!implicit.explicit_late_init);

    let late_var = manifest
        .get("lateVar")
        .expect("late var seed should be recorded");
    assert_eq!(late_var.origin, ValBindingOrigin::ExplicitKeyword);
    assert!(!late_var.has_initializer);
    assert!(late_var.explicit_late_init);
}

#[test]
fn check_program_reports_type_error_on_mismatch() {
    let span = dummy_span();
    let mismatched_expr = Expression::Binary {
        left: Box::new(Expression::Literal(Literal::Boolean(true), span.clone())),
        op: BinaryOp::Add,
        right: Box::new(Expression::Literal(
            Literal::Number("1".into()),
            span.clone(),
        )),
        span: span.clone(),
        metadata: BinaryMetadata::default(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "x".into(),
            binding: None,

            type_annotation: None,
            initializer: mismatched_expr,
            modifiers: default_modifiers(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_err(), "type mismatch should yield an error");

    let errors = result.err().unwrap();
    assert!(
        matches!(errors.first(), Some(CheckError::TypeError(message)) if message.contains("type mismatch"))
    );
}

#[test]
fn override_annotation_on_method_is_allowed() {
    let span = dummy_span();
    let mut method_modifiers = Modifiers::default();
    method_modifiers.annotations.push(annotation("Override"));

    let method = Statement::FunctionDeclaration {
        name: "run".into(),
        type_parameters: Vec::new(),
        generic_signature: None,
        where_clause: None,
        parameters: Vec::new(),
        return_type: None,
        primitive_return: None,
        body: Box::new(Expression::Literal(
            Literal::Number("1".into()),
            span.clone(),
        )),
        modifiers: method_modifiers,
        span: span.clone(),
    };

    let class = Statement::ClassDeclaration {
        name: "Service".into(),
        type_parameters: Vec::new(),
        generic_signature: None,
        superclass: None,
        interfaces: Vec::new(),
        properties: Vec::new(),
        methods: vec![Box::new(method)],
        modifiers: Modifiers::default(),
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![class],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "override on method should be accepted: {result:?}"
    );
}

#[test]
fn override_annotation_on_field_is_rejected() {
    let span = dummy_span();
    let mut field_modifiers = Modifiers::default();
    field_modifiers.annotations.push(annotation("Override"));

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "count".into(),
            binding: None,

            type_annotation: None,
            initializer: Expression::Literal(Literal::Number("1".into()), span.clone()),
            modifiers: field_modifiers,
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_err(), "override on field should be rejected");

    let errors = result.err().unwrap();
    assert!(matches!(
        errors.first(),
        Some(CheckError::ValidationError { message, .. }) if message.contains("@Override") && message.contains("method")
    ));
}

#[test]
fn duplicate_reserved_annotation_is_reported() {
    let span = dummy_span();
    let mut modifiers = Modifiers::default();
    modifiers.annotations.push(annotation("Sample"));
    modifiers.annotations.push(annotation("Sample"));

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "fixture".into(),
            binding: None,

            type_annotation: None,
            initializer: Expression::Literal(Literal::String("data".into()), span.clone()),
            modifiers,
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_err(), "duplicate reserved annotation should fail");

    let errors = result.err().unwrap();
    assert!(matches!(
        errors.first(),
        Some(CheckError::ValidationError { message, .. }) if message.contains("@Sample") && message.contains("used once")
    ));
}

#[test]
fn reserved_annotation_shadowing_is_detected() {
    let span = dummy_span();
    let shadow = Annotation {
        name: AnnotationName::new(
            vec![
                "com".to_string(),
                "example".to_string(),
                "Sample".to_string(),
            ],
            span.clone(),
        ),
        arguments: Vec::new(),
        span: span.clone(),
    };

    let mut modifiers = Modifiers::default();
    modifiers.annotations.push(shadow);

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "fixture".into(),
            binding: None,

            type_annotation: None,
            initializer: Expression::Literal(Literal::String("data".into()), span.clone()),
            modifiers,
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_err(), "shadowing reserved annotation should fail");

    let errors = result.err().unwrap();
    assert!(matches!(
        errors.first(),
        Some(CheckError::ValidationError { message, .. }) if message.contains("reserved jv annotation")
    ));
}

#[test]
fn null_safety_violation_is_reported() {
    let span = dummy_span();
    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "greeting".into(),
            binding: None,

            type_annotation: Some(TypeAnnotation::Simple("String".into())),
            initializer: Expression::Literal(Literal::Null, span.clone()),
            modifiers: default_modifiers(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let diagnostics = checker.check_null_safety(&program, None);
    assert!(diagnostics.iter().any(
        |error| matches!(error, CheckError::NullSafetyError(message) if message.contains("null"))
    ));
}

#[test]
fn null_safety_reports_jv3108_without_jv3003_regression() {
    let span = dummy_span();

    let implicit_val = Statement::ValDeclaration {
        name: "implicitVal".into(),
        binding: None,

        type_annotation: None,
        initializer: Expression::Literal(Literal::Number("1".into()), span.clone()),
        modifiers: default_modifiers(),
        origin: ValBindingOrigin::Implicit,
        span: span.clone(),
    };

    let token_binding = Statement::ValDeclaration {
        name: "token".into(),
        binding: None,

        type_annotation: Some(TypeAnnotation::Simple("String".into())),
        initializer: Expression::Literal(Literal::String("hello".into()), span.clone()),
        modifiers: default_modifiers(),
        origin: ValBindingOrigin::ExplicitKeyword,
        span: span.clone(),
    };

    let when_expr = Expression::When {
        expr: Some(Box::new(Expression::Identifier(
            "token".into(),
            span.clone(),
        ))),
        arms: vec![WhenArm {
            pattern: Pattern::Literal(Literal::Null, span.clone()),
            guard: None,
            body: Expression::Literal(Literal::String("none".into()), span.clone()),
            span: span.clone(),
        }],
        else_arm: Some(Box::new(Expression::Identifier(
            "token".into(),
            span.clone(),
        ))),
        implicit_end: None,
        span: span.clone(),
    };

    let null_branch_label = Statement::ValDeclaration {
        name: "label".into(),
        binding: None,

        type_annotation: None,
        initializer: when_expr,
        modifiers: default_modifiers(),
        origin: ValBindingOrigin::ExplicitKeyword,
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![implicit_val, token_binding, null_branch_label],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("program should type-check");

    let snapshot = checker.inference_snapshot().cloned();
    let diagnostics = checker.check_null_safety(&program, snapshot.as_ref());
    let messages = collect_null_safety_messages(&diagnostics);

    assert_eq!(
        messages.len(),
        1,
        "expected a single null safety diagnostic, got: {messages:?}"
    );
    assert!(
        messages.iter().any(|message| message.contains("JV3108")),
        "expected JV3108 conflict, got: {messages:?}"
    );
    assert!(
        messages.iter().all(|message| !message.contains("JV3003")),
        "unexpected JV3003 regression detected: {messages:?}"
    );

    let snapshot = snapshot.expect("snapshot should be available for null safety");
    let manifest = snapshot.late_init_manifest();

    let implicit_seed = manifest
        .get("implicitVal")
        .expect("implicit val should be recorded in manifest");
    assert_eq!(implicit_seed.origin, ValBindingOrigin::Implicit);
    assert!(implicit_seed.has_initializer);

    let token_seed = manifest
        .get("token")
        .expect("token binding should be recorded in manifest");
    assert_eq!(token_seed.origin, ValBindingOrigin::ExplicitKeyword);
    assert!(token_seed.has_initializer);
}

fn sample_when_arm(span: &Span) -> WhenArm {
    WhenArm {
        pattern: Pattern::Literal(Literal::Number("1".into()), span.clone()),
        guard: None,
        body: Expression::Literal(Literal::Number("1".into()), span.clone()),
        span: span.clone(),
    }
}

#[test]
fn when_without_else_in_value_position_emits_validation_error() {
    let span = dummy_span();
    let when_expr = Expression::When {
        expr: None,
        arms: vec![sample_when_arm(&span)],
        else_arm: None,
        implicit_end: None,
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "result".into(),
            binding: None,

            type_annotation: None,
            initializer: when_expr,
            modifiers: default_modifiers(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let errors = checker
        .check_program(&program)
        .expect_err("when without else in value context should fail");

    assert!(
        matches!(
            errors.first(),
            Some(CheckError::ValidationError {
                message,
                span: Some(error_span),
            }) if message.contains("E_WHEN_002") && *error_span == span
        ),
        "expected E_WHEN_002 validation error with span info",
    );
}

#[test]
fn when_with_else_in_value_position_passes_validation() {
    let span = dummy_span();
    let when_expr = Expression::When {
        expr: None,
        arms: vec![sample_when_arm(&span)],
        else_arm: Some(Box::new(Expression::Literal(
            Literal::Number("0".into()),
            span.clone(),
        ))),
        implicit_end: None,
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "result".into(),
            binding: None,

            type_annotation: None,
            initializer: when_expr,
            modifiers: default_modifiers(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("when with explicit else should pass validation");

    let snapshot = checker
        .inference_snapshot()
        .expect("snapshot should include pattern facts");
    let node_id = pattern::node_identifier(&span);
    let facts = snapshot
        .pattern_fact(node_id, PatternTarget::Java25)
        .expect("pattern facts recorded for when expression");
    assert!(
        facts.is_exhaustive(),
        "else branch should render exhaustive"
    );
}

#[test]
fn when_boolean_true_false_without_else_passes_validation() {
    let span = dummy_span();
    let when_expr = Expression::When {
        expr: Some(Box::new(Expression::Identifier(
            "flag".into(),
            span.clone(),
        ))),
        arms: vec![
            WhenArm {
                pattern: Pattern::Literal(Literal::Boolean(true), span.clone()),
                guard: None,
                body: Expression::Literal(Literal::Number("1".into()), span.clone()),
                span: span.clone(),
            },
            WhenArm {
                pattern: Pattern::Literal(Literal::Boolean(false), span.clone()),
                guard: None,
                body: Expression::Literal(Literal::Number("0".into()), span.clone()),
                span: span.clone(),
            },
        ],
        else_arm: None,
        implicit_end: None,
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![
            Statement::ValDeclaration {
                name: "flag".into(),
                binding: None,

                type_annotation: None,
                initializer: Expression::Literal(Literal::Boolean(true), span.clone()),
                modifiers: default_modifiers(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: span.clone(),
            },
            Statement::ValDeclaration {
                name: "result".into(),
                binding: None,

                type_annotation: None,
                initializer: when_expr,
                modifiers: default_modifiers(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: span.clone(),
            },
        ],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("boolean when with true/false arms should pass without else");
}

#[test]
fn when_branch_nullability_is_exposed_via_inference_service() {
    let program = parse_program(
        r#"
fun provide(): String? = null

maybe = provide()
val fallback = "fallback"
label = when (maybe) {
    is String -> maybe
    else -> fallback
}
"#,
    );

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("sample program should type-check");

    let snapshot = checker
        .inference_snapshot()
        .expect("inference snapshot should be available");
    let normalized = checker
        .normalized_program()
        .expect("normalized program should be available");

    let (subject_name, arm_count, has_else, span) = normalized
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::ValDeclaration {
                name, initializer, ..
            } if name == "label" => {
                if let Expression::When {
                    expr,
                    arms,
                    else_arm,
                    span,
                    ..
                } = initializer
                {
                    let subject = expr.as_deref().and_then(|expr| match expr {
                        Expression::Identifier(name, _) => Some(name.as_str()),
                        _ => None,
                    });
                    Some((subject, arms.len(), else_arm.is_some(), span.clone()))
                } else {
                    None
                }
            }
            _ => None,
        })
        .expect("label declaration should contain when expression");

    let node_id = pattern::node_identifier(&span);
    let summary = snapshot.when_branch_nullability(
        node_id,
        subject_name,
        arm_count,
        has_else,
        PatternTarget::Java25,
    );

    assert_eq!(
        summary.arm_nullability(),
        &[NullabilityFlag::NonNull],
        "pattern facts should mark the matching arm as non-null"
    );
    assert_eq!(
        summary.fallback_nullability(),
        Some(NullabilityFlag::Unknown),
        "else branch defaults to unknown without explicit narrowing facts"
    );
}

#[test]
fn when_without_else_in_statement_position_is_allowed() {
    let span = dummy_span();
    let when_expr = Expression::When {
        expr: None,
        arms: vec![sample_when_arm(&span)],
        else_arm: None,
        implicit_end: None,
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::Expression {
            expr: when_expr,
            span: span.clone(),
        }],
        span,
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "statement-position when without else should be permitted"
    );
}

#[test]
fn implicit_assignment_becomes_val_declaration() {
    let span = dummy_span();
    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::Assignment {
            target: Expression::Identifier("total".into(), span.clone()),
            binding_pattern: None,

            value: Expression::Literal(Literal::Number("42".into()), span.clone()),
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "binding resolver should accept implicit assignment: {result:?}"
    );

    let normalized = checker
        .normalized_program()
        .expect("normalized program should be available");
    match normalized.statements.first() {
        Some(Statement::ValDeclaration { name, origin, .. }) => {
            assert_eq!(name, "total");
            assert_eq!(*origin, ValBindingOrigin::Implicit);
        }
        other => panic!("expected ValDeclaration, got {:?}", other),
    }

    let usage = checker.binding_usage();
    assert_eq!(usage.implicit, 1);
    assert_eq!(usage.explicit + usage.implicit_typed + usage.vars, 0);

    let manifest = checker.late_init_manifest();
    let seed = manifest
        .get("total")
        .expect("implicit assignment should register a LateInit seed");
    assert_eq!(seed.origin, ValBindingOrigin::Implicit);
    assert!(seed.has_initializer);
    assert!(!seed.explicit_late_init);
}

#[test]
fn task15_debug_implicit_binding_manifest_and_missing_jv3002() {
    let program = parse_program(
        r#"
fun provide(): String? = null

maybe = provide()
val fallback = "fallback"
label = when (maybe) {
    is String -> maybe
    else -> fallback
}

label
"#,
    );

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("program should type-check for debug reproduction");

    let normalized = checker
        .normalized_program()
        .expect("normalized program should exist");
    let maybe_origin = normalized
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::ValDeclaration { name, origin, .. } if name == "maybe" => Some(origin),
            _ => None,
        })
        .expect("implicit assignment should normalize to val declaration");
    assert_eq!(
        *maybe_origin,
        ValBindingOrigin::Implicit,
        "implicit assignment must retain implicit origin"
    );

    let manifest = checker.late_init_manifest();
    let maybe_seed = manifest
        .get("maybe")
        .expect("LateInit manifest should track implicit binding");
    assert_eq!(maybe_seed.origin, ValBindingOrigin::Implicit);
    assert!(maybe_seed.has_initializer);
    assert!(
        !maybe_seed.explicit_late_init,
        "implicit binding should not be marked as explicit late init"
    );

    let diagnostics = checker.check_null_safety(&program, None);
    let messages = collect_null_safety_messages(&diagnostics);
    assert!(
        messages.is_empty(),
        "expected implicit binding scenario to avoid null safety diagnostics, got: {messages:?}"
    );
}

#[test]
fn implicit_assignment_normalization_is_stable_under_random_inputs() {
    let mut rng = Rng::with_seed(0xA1CE_FACE);

    for iteration in 0..64 {
        let name = random_identifier(&mut rng);
        let first_value = rng.u32(0..=1_000_000).to_string();
        let second_value = (rng.u32(0..=1_000_000) + 1).to_string();

        let base_line = iteration * 3 + 1;

        let first_target_span = Span::new(base_line, 1, base_line, 1 + name.len());
        let first_value_span = Span::new(base_line, 5, base_line, 5 + first_value.len());
        let first_statement_span = Span::new(base_line, 1, base_line, 5 + first_value.len());

        let single_program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![Statement::Assignment {
                target: Expression::Identifier(name.clone(), first_target_span.clone()),
                binding_pattern: None,

                value: Expression::Literal(
                    Literal::Number(first_value.clone()),
                    first_value_span.clone(),
                ),
                span: first_statement_span.clone(),
            }],
            span: first_statement_span.clone(),
        };

        let mut checker = TypeChecker::new();
        let result = checker.check_program(&single_program);
        assert!(
            result.is_ok(),
            "implicit assignment should normalize without diagnostics: {result:?}"
        );

        let normalized = checker
            .normalized_program()
            .expect("normalized program should be recorded");
        match normalized.statements.first() {
            Some(Statement::ValDeclaration {
                name: normalized_name,
                binding: None,

                origin,
                span,
                initializer,
                ..
            }) => {
                assert_eq!(
                    normalized_name, &name,
                    "normalized binding name should match source identifier"
                );
                assert_eq!(
                    *origin,
                    ValBindingOrigin::Implicit,
                    "normalized binding origin should remain implicit"
                );
                assert_eq!(
                    span, &first_statement_span,
                    "normalized span should cover original assignment statement"
                );
                assert!(
                    matches!(
                        initializer,
                        Expression::Literal(Literal::Number(value), literal_span)
                            if value == &first_value && literal_span == &first_value_span
                    ),
                    "initializer literal should survive normalization with span preserved"
                );
            }
            other => panic!(
                "expected implicit assignment to normalize into val declaration, got {:?}",
                other
            ),
        }

        let manifest = checker.late_init_manifest();
        let seed = manifest
            .get(&name)
            .unwrap_or_else(|| panic!("late init manifest should track implicit binding {name}"));
        assert!(
            seed.has_initializer,
            "implicit binding must record initializer"
        );
        assert_eq!(
            seed.origin,
            ValBindingOrigin::Implicit,
            "late init manifest should remember implicit origin"
        );

        let second_target_span = Span::new(base_line + 1, 1, base_line + 1, 1 + name.len());
        let second_value_span = Span::new(base_line + 1, 5, base_line + 1, 5 + second_value.len());
        let second_statement_span =
            Span::new(base_line + 1, 1, base_line + 1, 5 + second_value.len());

        let double_program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![
                Statement::Assignment {
                    target: Expression::Identifier(name.clone(), first_target_span.clone()),
                    binding_pattern: None,

                    value: Expression::Literal(
                        Literal::Number(first_value.clone()),
                        first_value_span.clone(),
                    ),
                    span: first_statement_span.clone(),
                },
                Statement::Assignment {
                    target: Expression::Identifier(name.clone(), second_target_span.clone()),
                    binding_pattern: None,

                    value: Expression::Literal(
                        Literal::Number(second_value.clone()),
                        second_value_span.clone(),
                    ),
                    span: second_statement_span.clone(),
                },
            ],
            span: Span::new(base_line, 1, base_line + 1, 5 + second_value.len()),
        };

        let mut reassignment_checker = TypeChecker::new();
        let reassignment = reassignment_checker.check_program(&double_program);
        assert!(
            reassignment.is_err(),
            "reassignment program should emit diagnostic for immutable binding"
        );
        let errors = reassignment.err().unwrap();
        assert!(
            errors.iter().any(|error| matches!(
                error,
                CheckError::ValidationError { message, .. }
                    if message.contains("JV4201")
            )),
            "expected JV4201 for reassignment of binding {name}, got {errors:?}"
        );

        let self_value_span = Span::new(base_line + 2, 5, base_line + 2, 5 + name.len());
        let self_statement_span = Span::new(base_line + 2, 1, base_line + 2, 5 + name.len());

        let self_assign_program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![Statement::Assignment {
                target: Expression::Identifier(name.clone(), second_target_span.clone()),
                binding_pattern: None,

                value: Expression::Identifier(name.clone(), self_value_span.clone()),
                span: self_statement_span.clone(),
            }],
            span: self_statement_span.clone(),
        };

        let mut self_checker = TypeChecker::new();
        let self_result = self_checker.check_program(&self_assign_program);
        assert!(
            self_result.is_err(),
            "self assignment should be rejected for binding {name}"
        );
        let self_errors = self_result.err().unwrap();
        assert!(
            self_errors.iter().any(|error| matches!(
                error,
                CheckError::ValidationError { message, .. }
                    if message.contains("JV4202")
            )),
            "expected JV4202 for self assignment of binding {name}, got {self_errors:?}"
        );
    }
}

#[test]
fn implicit_self_assignment_emits_jv4202() {
    let span = dummy_span();
    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::Assignment {
            target: Expression::Identifier("total".into(), span.clone()),
            binding_pattern: None,

            value: Expression::Identifier("total".into(), span.clone()),
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(
        result.is_err(),
        "implicit self-assignment should produce JV4202 error"
    );
    let errors = result.err().unwrap();
    assert!(errors.iter().any(|error| matches!(
        error,
        CheckError::ValidationError { message, .. } if message.contains("JV4202")
    )));
}

#[test]
fn reassigning_immutable_binding_emits_jv4201() {
    let span = dummy_span();
    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![
            Statement::Assignment {
                target: Expression::Identifier("value".into(), span.clone()),
                binding_pattern: None,

                value: Expression::Literal(Literal::Number("1".into()), span.clone()),
                span: span.clone(),
            },
            Statement::Assignment {
                target: Expression::Identifier("value".into(), span.clone()),
                binding_pattern: None,

                value: Expression::Literal(Literal::Number("2".into()), span.clone()),
                span: span.clone(),
            },
        ],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_err(), "reassignment should produce JV4201 error");
    let errors = result.err().unwrap();
    assert!(errors.iter().any(|error| matches!(
        error,
        CheckError::ValidationError { message, .. } if message.contains("JV4201")
    )));
}

#[test]
fn mutable_var_allows_reassignment() {
    let span = dummy_span();
    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![
            Statement::VarDeclaration {
                name: "count".into(),
                binding: None,
                type_annotation: None,
                initializer: Some(Expression::Literal(
                    Literal::Number("0".into()),
                    span.clone(),
                )),
                modifiers: default_modifiers(),
                span: span.clone(),
            },
            Statement::Assignment {
                target: Expression::Identifier("count".into(), span.clone()),
                binding_pattern: None,

                value: Expression::Literal(Literal::Number("1".into()), span.clone()),
                span: span.clone(),
            },
        ],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "var reassignment should be permitted: {result:?}"
    );
}

#[test]
fn explicit_var_self_assignment_is_allowed() {
    let span = dummy_span();
    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![
            Statement::VarDeclaration {
                name: "count".into(),
                binding: None,
                type_annotation: None,
                initializer: Some(Expression::Literal(
                    Literal::Number("0".into()),
                    span.clone(),
                )),
                modifiers: default_modifiers(),
                span: span.clone(),
            },
            Statement::Assignment {
                target: Expression::Identifier("count".into(), span.clone()),
                binding_pattern: None,

                value: Expression::Identifier("count".into(), span.clone()),
                span: span.clone(),
            },
        ],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "explicit mutable binding should allow self-assignment: {result:?}"
    );
}

#[test]
fn regex_literal_infers_pattern_type() {
    let span = dummy_span();
    let literal = RegexLiteral {
        pattern: "\\d+".into(),
        raw: "/\\d+/".into(),
        span: span.clone(),
    };
    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "pattern".into(),
            binding: None,

            type_annotation: None,
            initializer: Expression::RegexLiteral(literal),
            modifiers: default_modifiers(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "regex literal should be accepted: {result:?}"
    );

    let snapshot = checker
        .inference_snapshot()
        .expect("snapshot should be produced");
    let scheme = snapshot
        .binding_scheme("pattern")
        .expect("binding scheme for pattern");
    assert_eq!(
        scheme.ty,
        TypeKind::reference("java.util.regex.Pattern"),
        "regex literal should infer Pattern type"
    );
    let analyses = snapshot.regex_analyses();
    assert_eq!(analyses.len(), 1);
    assert!(analyses[0].diagnostics.is_empty());
    assert_eq!(analyses[0].pattern, "\\d+");
}

#[test]
fn regex_validator_reports_unsupported_escape() {
    let span = dummy_span();
    let literal = RegexLiteral {
        pattern: "abc\\q".into(),
        raw: "/abc\\q/".into(),
        span: span.clone(),
    };
    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "pattern".into(),
            binding: None,

            type_annotation: None,
            initializer: Expression::RegexLiteral(literal),
            modifiers: default_modifiers(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_err(), "invalid escape should be rejected");
    let errors = result.err().unwrap();
    assert!(errors.iter().any(|error| matches!(
        error,
        CheckError::ValidationError { message, .. } if message.contains("JV5102")
    )));
}

#[test]
fn regex_validator_reports_unbalanced_groups() {
    let span = dummy_span();
    let literal = RegexLiteral {
        pattern: "(abc".into(),
        raw: "/(abc/".into(),
        span: span.clone(),
    };
    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "pattern".into(),
            binding: None,

            type_annotation: None,
            initializer: Expression::RegexLiteral(literal),
            modifiers: default_modifiers(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_err(), "unbalanced groups should be rejected");
    let errors = result.err().unwrap();
    assert!(errors.iter().any(|error| matches!(
        error,
        CheckError::ValidationError { message, .. } if message.contains("JV5101")
    )));
}

fn build_regex_program(pattern: &str) -> Program {
    let span = dummy_span();
    let literal = RegexLiteral {
        pattern: pattern.to_string(),
        raw: format!("/{pattern}/"),
        span: span.clone(),
    };
    Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "pattern".into(),
            binding: None,

            type_annotation: None,
            initializer: Expression::RegexLiteral(literal),
            modifiers: default_modifiers(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span,
        }],
        span: dummy_span(),
    }
}

fn run_regex_validator(pattern: &str) -> Vec<CheckError> {
    let program = build_regex_program(pattern);
    let mut validator = RegexValidator::new();
    validator.validate_program(&program)
}

fn sample_valid_pattern(rng: &mut Rng) -> String {
    sample_valid_pattern_with_depth(rng, 0)
}

fn sample_valid_pattern_with_depth(rng: &mut Rng, depth: u8) -> String {
    let segments = rng.usize(1..=6);
    let mut pattern = String::new();
    for _ in 0..segments {
        let choice_limit = if depth < 2 { 5 } else { 4 };
        let segment = match rng.usize(0..choice_limit) {
            0 => sample_literal_atom(rng),
            1 => sample_escape_atom(rng).to_string(),
            2 => sample_quantified(sample_literal_atom(rng), rng),
            3 => sample_quantified(sample_escape_atom(rng).to_string(), rng),
            _ => format!("({})", sample_valid_pattern_with_depth(rng, depth + 1)),
        };
        pattern.push_str(&segment);
    }
    pattern
}

fn sample_literal_atom(rng: &mut Rng) -> String {
    const LITERALS: &[char] = &[
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1',
        '2', '3', '4', '5', '6', '7', '8', '9', '_', '.', '-', '^',
    ];
    let len = rng.usize(1..=3);
    (0..len)
        .map(|_| {
            let idx = rng.usize(0..LITERALS.len());
            LITERALS[idx]
        })
        .collect()
}

fn sample_escape_atom(rng: &mut Rng) -> &'static str {
    const ESCAPES: &[&str] = &[
        "\\d", "\\D", "\\s", "\\S", "\\w", "\\W", "\\t", "\\n", "\\r", "\\b", "\\B", "\\+", "\\.",
        "\\?", "\\*", "\\Q", "\\E", "\\/", "\\\\",
    ];
    let idx = rng.usize(0..ESCAPES.len());
    ESCAPES[idx]
}

fn sample_quantified(atom: String, rng: &mut Rng) -> String {
    const QUANTIFIERS: &[&str] = &["+", "*", "?"];
    let idx = rng.usize(0..QUANTIFIERS.len());
    format!("{atom}{}", QUANTIFIERS[idx])
}

fn random_invalid_escape_char(rng: &mut Rng) -> char {
    const INVALID_ESCAPES: &[char] = &['q', 'h', '!', 'y'];
    let idx = rng.usize(0..INVALID_ESCAPES.len());
    INVALID_ESCAPES[idx]
}

#[test]
fn regex_validator_fuzzes_valid_patterns_without_errors() {
    let mut rng = Rng::with_seed(0xC0FFEE);
    for iteration in 0..256 {
        let pattern = sample_valid_pattern(&mut rng);
        let errors = run_regex_validator(&pattern);
        assert!(
            errors.is_empty(),
            "iteration {iteration}: unexpected errors for pattern {pattern:?}: {errors:?}"
        );
    }
}

#[test]
fn regex_validator_fuzzes_invalid_escape_detection() {
    let mut rng = Rng::with_seed(0xFEEDBEEF);
    for iteration in 0..256 {
        let mut pattern = sample_valid_pattern(&mut rng);
        let invalid = random_invalid_escape_char(&mut rng);
        pattern.push('\\');
        pattern.push(invalid);
        let errors = run_regex_validator(&pattern);
        assert!(
            errors.iter().any(|error| matches!(
                error,
                CheckError::ValidationError { message, .. } if message.contains("JV5102")
            )),
            "iteration {iteration}: expected JV5102 for pattern {pattern:?}, got {errors:?}"
        );
    }
}
