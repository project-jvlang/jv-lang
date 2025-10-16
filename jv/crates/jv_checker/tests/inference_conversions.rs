use jv_checker::inference::{PrimitiveType, TypeError, TypeFactory, TypeKind};

#[test]
fn primitive_type_mappings() {
    let int_kind = TypeFactory::from_annotation("Int").expect("Int should map to primitive");
    assert_eq!(int_kind, TypeKind::primitive(PrimitiveType::Int));

    let integer_kind =
        TypeFactory::from_annotation("Integer").expect("Integer should map to boxed primitive");
    assert_eq!(integer_kind, TypeKind::boxed(PrimitiveType::Int));

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
