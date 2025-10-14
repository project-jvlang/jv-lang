use jv_ast::{Span, Statement};
use jv_build::metadata::{JavaMethodSignature, ModuleEntry, PackageEntry, SymbolIndex, TypeEntry};
use jv_checker::imports::{ImportResolutionService, ResolvedImportKind};
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
