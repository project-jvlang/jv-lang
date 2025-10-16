use jv_checker::inference::{PrimitiveType, TypeError, TypeFactory, TypeKind};

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
