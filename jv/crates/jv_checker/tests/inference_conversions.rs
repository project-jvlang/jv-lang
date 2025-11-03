use jv_build::metadata::{ConversionCatalog, JavaMethodSignature, SymbolIndex, TypeEntry};
use jv_checker::InferenceTelemetry;
use jv_checker::inference::conversions::ConversionHelperCatalog;
use jv_checker::inference::{
    AppliedConversion, CompatibilityChecker, Constraint, ConstraintKind, ConstraintSet,
    ConstraintSolver, ConversionKind, ConversionOutcome, ConversionRulesEngine,
    NullableGuardReason, PrimitiveType, TypeEnvironment, TypeError, TypeFactory, TypeKind,
};
use jv_ir::types::JavaType;
use std::sync::Arc;

#[test]
fn primitive_type_mappings() {
    let int_kind = TypeFactory::from_annotation("Int").expect("Int should map to primitive");
    assert_eq!(int_kind, TypeKind::primitive(PrimitiveType::Int));

    let integer_kind =
        TypeFactory::from_annotation("Integer").expect("Integer should map to boxed primitive");
    assert_eq!(integer_kind, TypeKind::boxed(PrimitiveType::Int));

    let i_alias = TypeFactory::from_annotation("i").expect("i should map to primitive Int");
    assert_eq!(i_alias, TypeKind::primitive(PrimitiveType::Int));

    let short_alias =
        TypeFactory::from_annotation("i16").expect("i16 should map to primitive Short");
    assert_eq!(short_alias, TypeKind::primitive(PrimitiveType::Short));

    let long_alias = TypeFactory::from_annotation("i64").expect("i64 should map to primitive Long");
    assert_eq!(long_alias, TypeKind::primitive(PrimitiveType::Long));

    let float_alias = TypeFactory::from_annotation("f").expect("f should map to primitive Float");
    assert_eq!(float_alias, TypeKind::primitive(PrimitiveType::Float));

    let double_alias =
        TypeFactory::from_annotation("f64").expect("f64 should map to primitive Double");
    assert_eq!(double_alias, TypeKind::primitive(PrimitiveType::Double));

    let char_alias = TypeFactory::from_annotation("c").expect("c should map to primitive Char");
    assert_eq!(char_alias, TypeKind::primitive(PrimitiveType::Char));

    let bigdecimal_ref = TypeFactory::from_annotation("math.BigDecimal")
        .expect("math.BigDecimal should resolve to java.math.BigDecimal reference");
    assert_eq!(bigdecimal_ref, TypeKind::reference("java.math.BigDecimal"));

    let optional_int = TypeKind::optional_primitive(PrimitiveType::Int);
    assert!(matches!(
        optional_int,
        TypeKind::Optional(inner) if matches!(inner.as_ref(), TypeKind::Boxed(p) if *p == PrimitiveType::Int)
    ));

    let err = TypeFactory::primitive_from_java_name("intr")
        .expect_err("unknown primitive should produce an error");
    assert!(matches!(
        err,
        TypeError::UnknownPrimitive { identifier } if identifier == "intr"
    ));
}

#[test]
fn collection_annotation_normalization() {
    let deque_kind =
        TypeFactory::from_annotation("deque").expect("deque は java.util.Deque へ正規化されるはず");
    assert_eq!(deque_kind, TypeKind::reference("java.util.Deque"));

    let array_list_kind = TypeFactory::from_annotation("arraylist")
        .expect("arraylist は java.util.ArrayList へ正規化されるはず");
    assert_eq!(array_list_kind, TypeKind::reference("java.util.ArrayList"));

    let linked_hash_set_kind = TypeFactory::from_annotation("linkedhashset")
        .expect("linkedhashset は java.util.LinkedHashSet へ正規化されるはず");
    assert_eq!(
        linked_hash_set_kind,
        TypeKind::reference("java.util.LinkedHashSet")
    );

    let concurrent_hash_map_kind = TypeFactory::from_annotation("java.util.concurrenthashmap")
        .expect("java.util.concurrenthashmap も FQCN へ正規化されるはず");
    assert_eq!(
        concurrent_hash_map_kind,
        TypeKind::reference("java.util.concurrent.ConcurrentHashMap")
    );
}

#[test]
fn conversion_rules_detects_widening() {
    let outcome = ConversionRulesEngine::analyze(
        &TypeKind::primitive(PrimitiveType::Int),
        &TypeKind::primitive(PrimitiveType::Long),
    );

    match outcome {
        ConversionOutcome::Allowed(metadata) => {
            assert_eq!(metadata.kind, ConversionKind::WideningPrimitive);
            assert!(metadata.nullable_guard.is_none());
        }
        other => panic!("expected widening conversion, got {other:?}"),
    }
}

#[test]
fn conversion_rules_optional_requires_guard() {
    let source = TypeKind::optional(TypeKind::reference("java.lang.String"));
    let target = TypeKind::reference("java.lang.String");
    let outcome = ConversionRulesEngine::analyze(&source, &target);

    match outcome {
        ConversionOutcome::Allowed(metadata) => {
            let guard = metadata
                .nullable_guard
                .expect("optional conversion should request guard");
            assert_eq!(guard.reason, NullableGuardReason::OptionalLift);
        }
        other => panic!("expected optional guard metadata, got {other:?}"),
    }
}

#[test]
fn conversion_rules_allow_collections_sharing_defaults() {
    let source = TypeKind::reference("java.util.List");
    let target = TypeKind::reference("java.util.Collection");
    let outcome = ConversionRulesEngine::analyze(&source, &target);
    assert!(
        matches!(outcome, ConversionOutcome::Identity),
        "List と Collection は共通の具象クラスを共有するため許容されるべき"
    );
}

#[test]
fn conversion_rules_reject_conflicting_collection_defaults() {
    let source = TypeKind::reference("java.util.List");
    let target = TypeKind::reference("java.util.Set");
    let outcome = ConversionRulesEngine::analyze(&source, &target);
    match outcome {
        ConversionOutcome::Rejected(error) => {
            let message = error.to_string();
            assert!(
                message.contains("List") && message.contains("Set"),
                "List → Set の不整合診断が含まれるべき"
            );
        }
        other => panic!("List → Set は拒否されるべきだが {other:?} が返却された"),
    }
}

#[test]
fn constraint_solver_records_boxing_conversion() {
    let mut set = ConstraintSet::new();
    set.push(Constraint::new(ConstraintKind::Convertible {
        from: TypeKind::primitive(PrimitiveType::Int),
        to: TypeKind::boxed(PrimitiveType::Int),
    }));

    let solver = ConstraintSolver::new();
    let result = solver
        .solve(set)
        .expect("boxing conversion should be accepted");
    assert_eq!(result.conversions.len(), 1);
    let entry = &result.conversions[0];
    assert_eq!(entry.kind, ConversionKind::Boxing);
    assert!(!entry.warned);
    assert!(entry.nullable_guard.is_none());
}

#[test]
fn telemetry_counts_unboxing_and_guards() {
    let mut set = ConstraintSet::new();
    set.push(Constraint::new(ConstraintKind::Convertible {
        from: TypeKind::boxed(PrimitiveType::Int),
        to: TypeKind::primitive(PrimitiveType::Int),
    }));

    let solver = ConstraintSolver::new();
    let result = solver
        .solve(set)
        .expect("unboxing conversion should be accepted");
    let mut telemetry = InferenceTelemetry::default();
    telemetry.record_conversions(&result.conversions);

    assert_eq!(telemetry.unboxing_conversions, 1);
    assert_eq!(telemetry.nullable_guards_generated, 1);
    assert_eq!(telemetry.conversion_events.len(), 1);
    assert_eq!(telemetry.nullable_guards.len(), 1);
    assert_eq!(
        telemetry.nullable_guards[0].reason,
        NullableGuardReason::Unboxing
    );
    assert!(telemetry.catalog_hits.is_empty());
}

#[test]
fn telemetry_records_conversion_events_and_catalog_hits() {
    let mut set = ConstraintSet::new();
    set.push(Constraint::new(ConstraintKind::Convertible {
        from: TypeKind::primitive(PrimitiveType::Int),
        to: TypeKind::reference("java.lang.String"),
    }));

    let catalog = Arc::new(helper_catalog());
    let mut solver = ConstraintSolver::new();
    solver.set_conversion_catalog(Some(Arc::clone(&catalog)));

    let result = solver
        .solve(set)
        .expect("string conversion via helper should succeed");
    let mut telemetry = InferenceTelemetry::default();
    telemetry.record_conversions(&result.conversions);

    assert_eq!(telemetry.conversion_events.len(), 1);
    let event = &telemetry.conversion_events[0];
    assert_eq!(event.kind, ConversionKind::StringConversion);
    let helper = event
        .helper_method
        .as_ref()
        .expect("helper metadata should be recorded");
    assert_eq!(helper.owner, "java.lang.Integer");
    assert_eq!(helper.method, "toString");
    assert!(!helper.is_static);
    assert!(!event.warned);
    assert!(event.nullable_guard.is_none());

    assert_eq!(telemetry.catalog_hits.len(), 1);
    let hit = &telemetry.catalog_hits[0];
    assert_eq!(hit.owner, "java.lang.Integer");
    assert_eq!(hit.method, "toString");
    assert!(!hit.is_static);
    assert!(telemetry.nullable_guards.is_empty());
}

#[test]
fn conversion_diagnostic_reports_boxing_event() {
    let conversion = AppliedConversion::new(
        TypeKind::primitive(PrimitiveType::Int),
        TypeKind::boxed(PrimitiveType::Int),
        ConversionKind::Boxing,
        None,
        None,
        false,
    );

    let diagnostic = jv_checker::inference::diagnostics::conversion_diagnostic(&conversion)
        .expect("diagnostic produced");
    assert_eq!(diagnostic.code, "JV_TYPE_001");
    assert!(diagnostic.message.contains("Implicitly boxed"));
}

#[test]
fn optional_reference_conversion_requests_guard() {
    let source = TypeKind::optional(TypeKind::reference("java.lang.String"));
    let target = TypeKind::reference("java.lang.String");

    let outcome = ConversionRulesEngine::analyze(&source, &target);
    match outcome {
        ConversionOutcome::Allowed(metadata) => {
            let guard = metadata
                .nullable_guard
                .expect("optional conversion should request a nullable guard");
            assert_eq!(guard.reason, NullableGuardReason::OptionalLift);
        }
        other => panic!("expected optional lift conversion metadata, got {other:?}"),
    }
}

#[test]
fn collection_transform_conversion_integrates_with_solver_and_telemetry() {
    let mut set = ConstraintSet::new();
    set.push(Constraint::new(ConstraintKind::Convertible {
        from: TypeKind::reference("java.util.List"),
        to: TypeKind::reference("java.util.stream.Stream"),
    }));

    let catalog = Arc::new(collection_catalog());
    let mut solver = ConstraintSolver::new();
    solver.set_conversion_catalog(Some(Arc::clone(&catalog)));

    let solve_result = solver
        .solve(set)
        .expect("collection transform should be resolved via helper catalog");

    assert_eq!(solve_result.conversions.len(), 1);
    let conversion = &solve_result.conversions[0];
    assert_eq!(conversion.kind, ConversionKind::MethodInvocation);
    let helper = conversion
        .helper_method
        .as_ref()
        .expect("method invocation conversion should carry helper metadata");
    assert_eq!(helper.method, "stream");
    assert!(!helper.is_static);

    let mut telemetry = InferenceTelemetry::default();
    telemetry.record_conversions(&solve_result.conversions);
    assert_eq!(telemetry.method_invocation_conversions, 1);
    assert_eq!(telemetry.conversion_events.len(), 1);
    assert_eq!(telemetry.catalog_hits.len(), 1);
    assert_eq!(
        telemetry.catalog_hits[0].method, "stream",
        "telemetry should record the helper method for collection transforms"
    );
}

fn catalog_from_entries(entries: Vec<TypeEntry>) -> ConversionHelperCatalog {
    let mut index = SymbolIndex::new(Some(25));
    for entry in entries {
        index.add_type(entry);
    }
    let catalog = ConversionCatalog::from_symbol_index(&index);
    ConversionHelperCatalog::new(Arc::new(catalog))
}

fn helper_catalog() -> ConversionHelperCatalog {
    let mut entry = TypeEntry::new(
        "java.lang.String".to_string(),
        "java.lang".to_string(),
        None,
    );
    entry.static_methods.insert(
        "valueOf".to_string(),
        JavaMethodSignature {
            parameters: vec![JavaType::Primitive("int".to_string())],
            return_type: JavaType::Reference {
                name: "java.lang.String".to_string(),
                generic_args: Vec::new(),
            },
        },
    );

    catalog_from_entries(vec![entry])
}

fn collection_catalog() -> ConversionHelperCatalog {
    let mut entry = TypeEntry::new("java.util.List".to_string(), "java.util".to_string(), None);
    entry.instance_methods.insert(
        "stream".to_string(),
        JavaMethodSignature {
            parameters: Vec::new(),
            return_type: JavaType::Reference {
                name: "java.util.stream.Stream".to_string(),
                generic_args: Vec::new(),
            },
        },
    );

    catalog_from_entries(vec![entry])
}

fn functional_catalog() -> ConversionHelperCatalog {
    let mut entry = TypeEntry::new(
        "java.util.stream.Stream".to_string(),
        "java.util.stream".to_string(),
        None,
    );
    entry.instance_methods.insert(
        "map".to_string(),
        JavaMethodSignature {
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
                name: "java.util.Optional".to_string(),
                generic_args: Vec::new(),
            },
        },
    );

    catalog_from_entries(vec![entry])
}

#[test]
fn string_conversion_prefers_instance_helper() {
    let catalog = helper_catalog();
    let outcome = ConversionRulesEngine::analyze_with_catalog(
        &TypeKind::primitive(PrimitiveType::Int),
        &TypeKind::reference("java.lang.String"),
        Some(&catalog),
    );

    match outcome {
        ConversionOutcome::Allowed(metadata) => {
            assert_eq!(metadata.kind, ConversionKind::StringConversion);
            let helper = metadata.helper.expect("helper metadata expected");
            assert_eq!(helper.owner, "java.lang.Integer");
            assert_eq!(helper.method, "toString");
            assert!(!helper.is_static);
        }
        other => panic!("expected string conversion metadata, got {other:?}"),
    }
}

#[test]
fn conversion_catalog_detects_collection_transform() {
    let catalog = collection_catalog();
    let outcome = ConversionRulesEngine::analyze_with_catalog(
        &TypeKind::reference("java.util.List"),
        &TypeKind::reference("java.util.stream.Stream"),
        Some(&catalog),
    );

    match outcome {
        ConversionOutcome::Allowed(metadata) => {
            assert_eq!(metadata.kind, ConversionKind::MethodInvocation);
            let helper = metadata.helper.expect("helper metadata expected");
            assert_eq!(helper.method, "stream");
            assert!(!helper.is_static);
        }
        other => panic!("expected method invocation conversion, got {other:?}"),
    }
}

#[test]
fn conversion_catalog_detects_functional_transform() {
    let catalog = functional_catalog();
    let outcome = ConversionRulesEngine::analyze_with_catalog(
        &TypeKind::reference("java.util.stream.Stream"),
        &TypeKind::reference("java.util.Optional"),
        Some(&catalog),
    );

    match outcome {
        ConversionOutcome::Allowed(metadata) => {
            assert_eq!(metadata.kind, ConversionKind::MethodInvocation);
            let helper = metadata.helper.expect("helper metadata expected");
            assert_eq!(helper.method, "map");
            assert!(!helper.is_static);
        }
        other => panic!("expected method invocation conversion, got {other:?}"),
    }
}

#[test]
fn compatibility_consults_environment_catalog() {
    let catalog = Arc::new(collection_catalog());
    let mut env = TypeEnvironment::new();
    env.set_conversion_catalog(Some(Arc::clone(&catalog)));

    let outcome = CompatibilityChecker::analyze(
        &env,
        &TypeKind::reference("java.util.List"),
        &TypeKind::reference("java.util.stream.Stream"),
    );

    match outcome {
        ConversionOutcome::Allowed(metadata) => {
            assert_eq!(metadata.kind, ConversionKind::MethodInvocation);
        }
        other => panic!("expected method invocation conversion, got {other:?}"),
    }
}
