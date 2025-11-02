use super::*;

fn pattern_preview(key: &PatternConstKey) -> &str {
    key.preview.as_str()
}

#[test]
fn pattern_const_records_capture_static_literals_and_functions() {
    let program = parse_program(
        r#"
val cache = /\w+/

fun build(): java.util.regex.Pattern = /(?<id>\d+)/

class Holder {
    val pattern = /prefix-.*/
}
"#,
    );

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("sample program with regex literals should type-check");

    let snapshot = checker
        .inference_snapshot()
        .expect("pattern test should produce inference snapshot");
    let scheme = snapshot
        .binding_scheme("cache")
        .expect("cache binding should exist");
    assert!(
        matches!(scheme.ty, TypeKind::Reference(ref name) if name == "java.util.regex.Pattern"),
        "cache binding should infer Pattern type, got {:?}",
        scheme.ty
    );

    let build_scheme = snapshot
        .function_scheme("build")
        .expect("function scheme for build should exist");
    match &build_scheme.ty {
        TypeKind::Function(params, ret) => {
            assert!(
                params.is_empty(),
                "build() should not accept parameters, got {:?}",
                params
            );
            assert!(
                matches!(ret.as_ref(), TypeKind::Reference(name) if name == "java.util.regex.Pattern"),
                "build() should return Pattern, got {:?}",
                ret
            );
        }
        other => panic!("build() should be a function type, got {:?}", other),
    }

    let records = checker.pattern_const_records();
    assert_eq!(
        records.len(),
        3,
        "three regex literals should be recorded as pattern consts: {:?}",
        records
    );
    for record in records {
        assert_eq!(
            record.kind,
            PatternConstKind::Static,
            "regex literal should be classified as static const"
        );
        let key = record
            .key
            .as_ref()
            .expect("static regex literal should produce const key");
        assert_eq!(
            key.hash.len(),
            16,
            "const key hash should be truncated to 16 bytes"
        );
        assert!(
            !pattern_preview(key).is_empty(),
            "const key preview should capture leading pattern text"
        );
    }

    let analyses = snapshot.regex_analyses();
    assert_eq!(
        analyses.len(),
        3,
        "regex analyses should mirror the three literals: {:?}",
        analyses
    );
    for analysis in analyses {
        assert_eq!(
            analysis.const_kind,
            PatternConstKind::Static,
            "validator should mark literal as static const"
        );
        assert!(
            analysis.const_key.is_some(),
            "validator analysis should surface const key metadata"
        );
        assert_eq!(
            analysis.origin.span.start_line, analysis.span.start_line,
            "analysis origin should reference the same source line: {:?} vs {:?}",
            analysis.origin.span, analysis.span
        );
    }
}

#[test]
fn pattern_expectations_propagate_through_blocks() {
    let program = parse_program(
        r#"
fun produce(flag: Boolean): java.util.regex.Pattern =
    when (flag) {
        true -> /static-\d+/
        else -> {
            val local = /fallback-[a-z]+/
            local
        }
    }
"#,
    );

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("pattern returning function should type-check");

    let snapshot = checker
        .inference_snapshot()
        .expect("pattern expectation test should produce snapshot");
    let scheme = snapshot
        .function_scheme("produce")
        .expect("produce() scheme should exist");
    match &scheme.ty {
        TypeKind::Function(params, ret) => {
            assert_eq!(
                params.len(),
                1,
                "produce() should accept a single boolean parameter"
            );
            match params.first() {
                Some(TypeKind::Primitive(PrimitiveType::Boolean))
                | Some(TypeKind::Boxed(PrimitiveType::Boolean)) => {}
                other => panic!("first parameter should be boolean, got {:?}", other),
            }
            assert!(
                matches!(ret.as_ref(), TypeKind::Reference(name) if name == "java.util.regex.Pattern"),
                "produce() should return Pattern, got {:?}",
                ret
            );
        }
        other => panic!("produce() should be a function type, got {:?}", other),
    }
}
