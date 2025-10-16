use std::sync::Arc;
use std::time::Instant;

use jv_build::metadata::{ConversionCatalog, JavaMethodSignature, SymbolIndex, TypeEntry};
use jv_checker::inference::{
    conversions::ConversionHelperCatalog, Constraint, ConstraintKind, ConstraintSet, ConstraintSolver,
    PrimitiveType, TypeKind,
};

const BASELINE_MS: f64 = 80.0;

fn helper_catalog() -> Arc<ConversionHelperCatalog> {
    let mut index = SymbolIndex::new(Some(25));

    let mut integer_entry = TypeEntry::new(
        "java.lang.Integer".to_string(),
        "java.lang".to_string(),
        None,
    );
    integer_entry.instance_methods.insert(
        "toString".to_string(),
        JavaMethodSignature {
            parameters: Vec::new(),
            return_type: jv_ir::types::JavaType::Reference {
                name: "java.lang.String".to_string(),
                generic_args: Vec::new(),
            },
        },
    );
    index.add_type(integer_entry);

    let mut list_entry = TypeEntry::new(
        "java.util.List".to_string(),
        "java.util".to_string(),
        None,
    );
    list_entry.instance_methods.insert(
        "stream".to_string(),
        JavaMethodSignature {
            parameters: Vec::new(),
            return_type: jv_ir::types::JavaType::Reference {
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

fn build_constraints() -> ConstraintSet {
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
    constraints
}

#[test]
#[ignore]
fn java_inference_regression_guard() {
    let catalog = helper_catalog();

    {
        let mut warmup_solver = ConstraintSolver::new();
        warmup_solver.set_conversion_catalog(Some(Arc::clone(&catalog)));
        warmup_solver
            .solve(build_constraints())
            .expect("warm-up solve should succeed");
    }

    let mut solver = ConstraintSolver::new();
    solver.set_conversion_catalog(Some(catalog));
    let start = Instant::now();
    solver
        .solve(build_constraints())
        .expect("benchmark solve should succeed");
    let elapsed_ms = start.elapsed().as_secs_f64() * 1_000.0;
    let budget_ms = BASELINE_MS * 1.10;

    assert!(
        elapsed_ms <= budget_ms,
        "constraint solving regression: {elapsed_ms:.2}ms (budget {budget_ms:.2}ms)"
    );
}
