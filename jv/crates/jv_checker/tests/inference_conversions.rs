use jv_checker::inference::{
    Constraint, ConstraintKind, ConstraintSet, ConstraintSolver, ConversionKind, ConversionOutcome,
    ConversionRulesEngine, InferenceTelemetry, NullableGuardReason, PrimitiveType, TypeError,
    TypeFactory, TypeKind,
};

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
    assert_eq!(entry.metadata.kind, ConversionKind::Boxing);
    assert!(!entry.warned);
    assert!(entry.metadata.nullable_guard.is_none());
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
}
