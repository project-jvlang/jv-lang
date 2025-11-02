use jv_ast::{Span, Statement};
use jv_build::metadata::{JavaMethodSignature, ModuleEntry, PackageEntry, SymbolIndex, TypeEntry};
use jv_checker::imports::{
    ImportError, ImportResolutionService, ResolvedImportKind,
    from_error as import_diagnostic_from_error, missing_module,
};
use jv_ir::types::JavaType;
use jv_pm::JavaTarget;
use std::collections::HashSet;
use std::sync::Arc;

fn dummy_span() -> Span {
    Span::new(1, 0, 1, 5)
}

fn with_type(
    index: &mut SymbolIndex,
    fqcn: &str,
    module: Option<&str>,
    static_members: &[(&str, JavaMethodSignature)],
) {
    let package = fqcn
        .rsplit_once('.')
        .map(|(pkg, _)| pkg.to_string())
        .unwrap_or_else(|| "".to_string());

    if let Some(module_name) = module {
        index
            .modules
            .entry(module_name.to_string())
            .or_insert_with(|| ModuleEntry {
                name: module_name.to_string(),
                exports: HashSet::new(),
            })
            .exports
            .insert(package.clone());
    }

    let mut entry = TypeEntry::new(
        fqcn.to_string(),
        package.clone(),
        module.map(|m| m.to_string()),
    );
    for (name, signature) in static_members {
        entry.add_static_method((*name).to_string(), signature.clone());
    }

    index.types.insert(fqcn.to_string(), entry);

    index
        .packages
        .entry(package.clone())
        .or_insert_with(|| PackageEntry {
            name: package.clone(),
            module: module.map(|m| m.to_string()),
            types: HashSet::new(),
        })
        .types
        .insert(fqcn.to_string());
}

fn base_index() -> Arc<SymbolIndex> {
    let mut index = SymbolIndex::default();
    with_type(&mut index, "java.util.List", Some("java.base"), &[]);
    with_type(
        &mut index,
        "java.lang.Math",
        Some("java.base"),
        &[(
            "max",
            JavaMethodSignature {
                parameters: vec![
                    JavaType::Primitive("int".into()),
                    JavaType::Primitive("int".into()),
                ],
                return_type: JavaType::Primitive("int".into()),
            },
        )],
    );

    Arc::new(index)
}

#[test]
fn resolves_fully_qualified_type_import() {
    let service = ImportResolutionService::new(base_index(), JavaTarget::Java25);
    let stmt = Statement::Import {
        path: "java.util.List".into(),
        alias: None,
        is_wildcard: false,
        span: dummy_span(),
    };

    let resolved = service.resolve(&stmt).expect("import resolved");
    assert!(matches!(
        resolved.kind,
        ResolvedImportKind::Type { ref fqcn } if fqcn == "java.util.List"
    ));
    assert_eq!(resolved.module_dependency, Some("java.base".into()));
}

#[test]
fn resolves_static_member_import() {
    let service = ImportResolutionService::new(base_index(), JavaTarget::Java25);
    let stmt = Statement::Import {
        path: "java.lang.Math.max".into(),
        alias: None,
        is_wildcard: false,
        span: dummy_span(),
    };

    let resolved = service.resolve(&stmt).expect("import resolved");
    assert!(matches!(
        resolved.kind,
        ResolvedImportKind::StaticMember { ref owner, ref member }
        if owner == "java.lang.Math" && member == "max"
    ));
}

#[test]
fn resolves_package_wildcard_import() {
    let service = ImportResolutionService::new(base_index(), JavaTarget::Java25);
    let stmt = Statement::Import {
        path: "java.util".into(),
        alias: None,
        is_wildcard: true,
        span: dummy_span(),
    };

    let resolved = service.resolve(&stmt).expect("wildcard resolved");
    assert!(matches!(
        resolved.kind,
        ResolvedImportKind::Package { ref name } if name == "java.util"
    ));

    let expanded = service.expand_wildcard("java.util");
    assert!(expanded.iter().any(|entry| matches!(
        entry.kind,
        ResolvedImportKind::Type { ref fqcn } if fqcn == "java.util.List"
    )));
}

#[test]
fn expands_static_wildcard_for_type_members() {
    let service = ImportResolutionService::new(base_index(), JavaTarget::Java25);
    let expanded = service.expand_wildcard("java.lang.Math");

    assert!(
        expanded.iter().any(|entry| matches!(
            entry.kind,
            ResolvedImportKind::StaticMember { ref owner, ref member }
                if owner == "java.lang.Math" && member == "max"
        )),
        "expected java.lang.Math.max to be part of wildcard expansion"
    );
}

#[test]
fn unknown_import_diagnostic_contains_candidates() {
    let span = dummy_span();
    let error = ImportError::UnknownSymbol {
        path: "foo.Bar".into(),
        span: span.clone(),
        candidates: vec!["foo.BarBaz".into(), "foo.bar.Bar".into()],
    };

    let diagnostic = import_diagnostic_from_error(&error).expect("diagnostic");
    assert_eq!(diagnostic.code, "JV4100");
    assert_eq!(diagnostic.span, Some(span));
    assert!(diagnostic.message.contains("foo.BarBaz"));
    assert!(
        diagnostic
            .suggestions
            .iter()
            .any(|suggestion| suggestion.contains("import foo.BarBaz"))
    );
}

#[test]
fn ambiguous_import_diagnostic_lists_candidates() {
    let mut index = SymbolIndex::default();
    with_type(
        &mut index,
        "com.example.logging.Logger",
        Some("example.logging"),
        &[],
    );
    with_type(&mut index, "org.slf4j.Logger", Some("org.slf4j"), &[]);

    let service = ImportResolutionService::new(Arc::new(index), JavaTarget::Java25);
    let span = dummy_span();
    let stmt = Statement::Import {
        path: "Logger".into(),
        alias: None,
        is_wildcard: false,
        span: span.clone(),
    };

    let error = service.resolve(&stmt).expect_err("expected ambiguity");
    match &error {
        ImportError::AmbiguousSymbol { candidates, .. } => {
            assert!(
                candidates
                    .iter()
                    .any(|entry| entry == "com.example.logging.Logger")
            );
            assert!(candidates.iter().any(|entry| entry == "org.slf4j.Logger"));
        }
        other => panic!("unexpected error: {other:?}"),
    }

    let diagnostic = import_diagnostic_from_error(&error).expect("diagnostic");
    assert_eq!(diagnostic.code, "JV4101");
    assert_eq!(diagnostic.span, Some(span));
    assert!(diagnostic.message.contains("org.slf4j.Logger"));
    assert!(
        diagnostic
            .suggestions
            .iter()
            .any(|suggestion| suggestion.contains("import com.example.logging.Logger"))
    );
}

#[test]
fn missing_module_diagnostic_suggests_add_modules() {
    let span = dummy_span();
    let diagnostic = missing_module("java.sql", "java.sql.DriverManager", &span);

    assert_eq!(diagnostic.code, "JV4102");
    assert_eq!(diagnostic.span, Some(span));
    assert!(
        diagnostic
            .suggestions
            .iter()
            .any(|suggestion| suggestion.contains("--add-modules java.sql"))
    );
}
