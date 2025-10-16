use std::sync::Arc;

use jv_build::metadata::{ConversionCatalog, JavaMethodSignature, SymbolIndex, TypeEntry};
use jv_checker::{
    inference::{
        conversions::ConversionHelperCatalog, Constraint, ConstraintKind, ConstraintSet,
        ConstraintSolver, ConversionKind as InferenceConversionKind, NullableGuardReason,
        PrimitiveType, TypeKind,
    },
    InferenceTelemetry,
};
use jv_ir::types::JavaType;

fn build_conversion_catalog() -> Arc<ConversionHelperCatalog> {
    let mut index = SymbolIndex::new(Some(25));

    let mut string_entry = TypeEntry::new(
        "java.lang.Integer".to_string(),
        "java.lang".to_string(),
        None,
    );
    string_entry.instance_methods.insert(
        "toString".to_string(),
        JavaMethodSignature {
            parameters: Vec::new(),
            return_type: JavaType::Reference {
                name: "java.lang.String".to_string(),
                generic_args: Vec::new(),
            },
        },
    );
    index.add_type(string_entry);

    let mut list_entry = TypeEntry::new(
        "java.util.List".to_string(),
        "java.util".to_string(),
        None,
    );
    list_entry.instance_methods.insert(
        "stream".to_string(),
        JavaMethodSignature {
            parameters: Vec::new(),
            return_type: JavaType::Reference {
                name: "java.util.stream.Stream".to_string(),
                generic_args: Vec::new(),
            },
        },
    );
    index.add_type(list_entry);

    let stream_entry =
        TypeEntry::new("java.util.stream.Stream".to_string(), "java.util.stream".to_string(), None);
    index.add_type(stream_entry);

    let catalog = ConversionCatalog::from_symbol_index(&index);
    Arc::new(ConversionHelperCatalog::new(Arc::new(catalog)))
}

#[test]
fn phase_four_conversion_pipeline_records_telemetry() {
    let helper_catalog = build_conversion_catalog();

    let mut constraints = ConstraintSet::new();
    constraints.push(Constraint::new(ConstraintKind::Convertible {
        from: TypeKind::optional(TypeKind::reference("java.lang.String")),
        to: TypeKind::reference("java.lang.String"),
    }));
    constraints.push(Constraint::new(ConstraintKind::Convertible {
        from: TypeKind::primitive(PrimitiveType::Int),
        to: TypeKind::reference("java.lang.String"),
    }));
    constraints.push(Constraint::new(ConstraintKind::Convertible {
        from: TypeKind::reference("java.util.List"),
        to: TypeKind::reference("java.util.stream.Stream"),
    }));

    let mut solver = ConstraintSolver::new();
    solver.set_conversion_catalog(Some(Arc::clone(&helper_catalog)));
    let solve_result = solver
        .solve(constraints)
        .expect("conversion constraints should be solvable");

    assert_eq!(solve_result.conversions.len(), 3);

    let mut telemetry = InferenceTelemetry::default();
    telemetry.record_conversions(&solve_result.conversions);

    assert_eq!(
        telemetry.method_invocation_conversions, 1,
        "collection transform should rely on helper invocation"
    );
    assert_eq!(
        telemetry.string_conversions, 1,
        "string conversions should be counted once"
    );
    assert!(
        telemetry.nullable_guards_generated >= 1,
        "optional lift should request a nullable guard"
    );
    assert_eq!(
        telemetry.catalog_hits.len(),
        2,
        "helper lookups for string conversion and collection transform should be recorded"
    );
    assert!(
        telemetry
            .catalog_hits
            .iter()
            .any(|helper| helper.method == "stream"),
        "collection transform helper should be recorded"
    );

    let kinds: Vec<_> = telemetry.conversion_events.iter().map(|event| event.kind).collect();
    assert!(
        kinds.contains(&InferenceConversionKind::MethodInvocation),
        "method invocation conversion event should be recorded"
    );
    assert!(
        kinds.contains(&InferenceConversionKind::StringConversion),
        "string conversion event should be recorded"
    );
    assert!(
        telemetry
            .nullable_guards
            .iter()
            .any(|guard| guard.reason == NullableGuardReason::OptionalLift),
        "nullable guard reasons should include OptionalLift"
    );
}
