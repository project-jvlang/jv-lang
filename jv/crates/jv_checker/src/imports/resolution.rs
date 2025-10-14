use jv_ast::{Span, Statement};
use jv_build::metadata::{SymbolIndex, TypeEntry};
use jv_pm::JavaTarget;
use std::collections::HashSet;
use std::sync::Arc;
use thiserror::Error;

/// Kind of entity an import statement resolves to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedImportKind {
    Type { fqcn: String },
    Package { name: String },
    StaticMember { owner: String, member: String },
    Module { name: String },
}

/// Normalised import metadata consumed by downstream checker components.
#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedImport {
    pub source_span: Span,
    pub original_path: String,
    pub alias: Option<String>,
    pub is_wildcard: bool,
    pub kind: ResolvedImportKind,
    pub module_dependency: Option<String>,
}

impl ResolvedImport {
    fn new(
        span: Span,
        path: impl Into<String>,
        alias: Option<String>,
        is_wildcard: bool,
        kind: ResolvedImportKind,
        module_dependency: Option<String>,
    ) -> Self {
        Self {
            source_span: span,
            original_path: path.into(),
            alias,
            is_wildcard,
            kind,
            module_dependency,
        }
    }
}

/// Errors produced when normalising `import` statements.
#[derive(Debug, Clone, PartialEq, Error)]
pub enum ImportError {
    #[error("expected import statement")]
    InvalidStatement,
    #[error("import path is empty")]
    EmptyImport { span: Span },
    #[error("unknown symbol '{path}'")]
    UnknownSymbol {
        path: String,
        span: Span,
        candidates: Vec<String>,
    },
    #[error("ambiguous import '{path}'")]
    AmbiguousSymbol {
        path: String,
        span: Span,
        candidates: Vec<String>,
    },
}

/// Service that resolves AST import statements using the metadata gathered by `SymbolIndex`.
#[derive(Debug, Clone)]
pub struct ImportResolutionService {
    index: Arc<SymbolIndex>,
    target: JavaTarget,
}

impl ImportResolutionService {
    pub fn new(index: Arc<SymbolIndex>, target: JavaTarget) -> Self {
        Self { index, target }
    }

    /// Resolve an AST import statement into its canonical representation.
    pub fn resolve(&self, statement: &Statement) -> Result<ResolvedImport, ImportError> {
        match statement {
            Statement::Import {
                path,
                alias,
                is_wildcard,
                span,
            } => {
                if path.trim().is_empty() {
                    return Err(ImportError::EmptyImport { span: span.clone() });
                }

                if *is_wildcard {
                    self.resolve_wildcard(path, alias.clone(), span.clone())
                } else {
                    self.resolve_plain(path, alias.clone(), span.clone())
                }
            }
            _ => Err(ImportError::InvalidStatement),
        }
    }

    /// Expand a wildcard import into concrete resolved imports.
    ///
    /// - For package wildcards, produces one `ResolvedImportKind::Type` per contained type.
    /// - For static-star imports, produces one `ResolvedImportKind::StaticMember` per static member.
    pub fn expand_wildcard(&self, path: &str) -> Vec<ResolvedImport> {
        if let Some(ty) = self.index.lookup_type(path) {
            return self.expand_static_star(ty);
        }

        if let Some(package) = self.index.packages.get(path) {
            return self.expand_package_star(package);
        }

        if !path.contains('.') {
            let matches = self.types_by_simple_name(path);
            if matches.len() == 1 {
                return self.expand_static_star(matches[0]);
            }
        }

        Vec::new()
    }

    /// Return the Java module dependency (when targeting Java 25) for a fully qualified type.
    pub fn required_module(&self, fqcn: &str) -> Option<String> {
        self.index
            .lookup_type(fqcn)
            .and_then(|ty| self.module_dependency_from_type(ty))
    }

    fn resolve_plain(
        &self,
        path: &str,
        alias: Option<String>,
        span: Span,
    ) -> Result<ResolvedImport, ImportError> {
        if let Some(ty) = self.index.lookup_type(path) {
            return Ok(ResolvedImport::new(
                span,
                path,
                alias,
                false,
                ResolvedImportKind::Type {
                    fqcn: ty.fqcn.clone(),
                },
                self.module_dependency_from_type(ty),
            ));
        }

        if let Some(resolved) = self.resolve_static_member(path, alias.clone(), span.clone())? {
            return Ok(resolved);
        }

        if let Some(resolved) = self.resolve_by_simple_name(path, alias.clone(), span.clone())? {
            return Ok(resolved);
        }

        if let Some(module) = self.index.modules.get(path) {
            return Ok(ResolvedImport::new(
                span,
                path,
                alias,
                false,
                ResolvedImportKind::Module {
                    name: module.name.clone(),
                },
                None,
            ));
        }

        Err(ImportError::UnknownSymbol {
            path: path.to_string(),
            span,
            candidates: self.collect_candidates(simple_name_hint(path)),
        })
    }

    fn resolve_wildcard(
        &self,
        path: &str,
        alias: Option<String>,
        span: Span,
    ) -> Result<ResolvedImport, ImportError> {
        if let Some(ty) = self.index.lookup_type(path) {
            return Ok(ResolvedImport::new(
                span,
                path,
                alias,
                true,
                ResolvedImportKind::StaticMember {
                    owner: ty.fqcn.clone(),
                    member: "*".to_string(),
                },
                self.module_dependency_from_type(ty),
            ));
        }

        if let Some(package) = self.index.packages.get(path) {
            return Ok(ResolvedImport::new(
                span,
                path,
                alias,
                true,
                ResolvedImportKind::Package {
                    name: package.name.clone(),
                },
                self.module_dependency_from_package(package),
            ));
        }

        if !path.contains('.') {
            let matches = self.types_by_simple_name(path);
            return match matches.as_slice() {
                [] => Err(ImportError::UnknownSymbol {
                    path: path.to_string(),
                    span,
                    candidates: Vec::new(),
                }),
                [ty] => Ok(ResolvedImport::new(
                    span,
                    path,
                    alias,
                    true,
                    ResolvedImportKind::StaticMember {
                        owner: ty.fqcn.clone(),
                        member: "*".to_string(),
                    },
                    self.module_dependency_from_type(ty),
                )),
                many => Err(ImportError::AmbiguousSymbol {
                    path: path.to_string(),
                    span,
                    candidates: many.iter().map(|ty| ty.fqcn.clone()).collect(),
                }),
            };
        }

        Err(ImportError::UnknownSymbol {
            path: path.to_string(),
            span,
            candidates: Vec::new(),
        })
    }

    fn resolve_static_member(
        &self,
        path: &str,
        alias: Option<String>,
        span: Span,
    ) -> Result<Option<ResolvedImport>, ImportError> {
        let mut segments: Vec<&str> = path.split('.').collect();
        if segments.len() < 2 {
            return Ok(None);
        }

        let member = segments
            .pop()
            .expect("segments.contains at least one element")
            .to_string();
        let owner = segments.join(".");
        if owner.is_empty() {
            return Ok(None);
        }

        if let Some(static_member) = self.index.lookup_static(&owner, &member) {
            let module_dependency = self
                .index
                .lookup_type(&static_member.owner)
                .and_then(|ty| self.module_dependency_from_type(ty));

            return Ok(Some(ResolvedImport::new(
                span,
                path,
                alias,
                false,
                ResolvedImportKind::StaticMember {
                    owner: static_member.owner,
                    member: static_member.member,
                },
                module_dependency,
            )));
        }

        Ok(None)
    }

    fn resolve_by_simple_name(
        &self,
        name: &str,
        alias: Option<String>,
        span: Span,
    ) -> Result<Option<ResolvedImport>, ImportError> {
        let matches = self.types_by_simple_name(name);

        match matches.len() {
            0 => Ok(None),
            1 => {
                let ty = matches[0];
                Ok(Some(ResolvedImport::new(
                    span,
                    name,
                    alias,
                    false,
                    ResolvedImportKind::Type {
                        fqcn: ty.fqcn.clone(),
                    },
                    self.module_dependency_from_type(ty),
                )))
            }
            _ => Err(ImportError::AmbiguousSymbol {
                path: name.to_string(),
                span,
                candidates: matches.iter().map(|ty| ty.fqcn.clone()).collect(),
            }),
        }
    }

    fn expand_static_star(&self, ty: &TypeEntry) -> Vec<ResolvedImport> {
        let mut outputs = Vec::with_capacity(ty.static_fields.len() + ty.static_methods.len());
        let module_dependency = self.module_dependency_from_type(ty);
        let span = Span::dummy();
        let base_path = format!("{}.*", ty.fqcn);

        for member in ty.static_fields.keys() {
            outputs.push(ResolvedImport::new(
                span.clone(),
                &base_path,
                None,
                false,
                ResolvedImportKind::StaticMember {
                    owner: ty.fqcn.clone(),
                    member: member.clone(),
                },
                module_dependency.clone(),
            ));
        }

        for member in ty.static_methods.keys() {
            outputs.push(ResolvedImport::new(
                span.clone(),
                &base_path,
                None,
                false,
                ResolvedImportKind::StaticMember {
                    owner: ty.fqcn.clone(),
                    member: member.clone(),
                },
                module_dependency.clone(),
            ));
        }

        outputs
    }

    fn expand_package_star(
        &self,
        package: &jv_build::metadata::PackageEntry,
    ) -> Vec<ResolvedImport> {
        let mut outputs = Vec::with_capacity(package.types.len());
        let module_dependency = self.module_dependency_from_package(package);
        let span = Span::dummy();
        let base_path = format!("{}.*", package.name);

        for fqcn in package.types.iter() {
            if let Some(ty) = self.index.lookup_type(fqcn) {
                outputs.push(ResolvedImport::new(
                    span.clone(),
                    &base_path,
                    None,
                    false,
                    ResolvedImportKind::Type {
                        fqcn: ty.fqcn.clone(),
                    },
                    module_dependency
                        .clone()
                        .or_else(|| self.module_dependency_from_type(ty)),
                ));
            }
        }

        outputs
    }

    fn module_dependency_from_type(&self, ty: &TypeEntry) -> Option<String> {
        if !self.target.enables_modern_features() {
            return None;
        }
        ty.module.clone()
    }

    fn module_dependency_from_package(
        &self,
        package: &jv_build::metadata::PackageEntry,
    ) -> Option<String> {
        if !self.target.enables_modern_features() {
            return None;
        }
        package.module.clone()
    }

    fn types_by_simple_name(&self, name: &str) -> Vec<&TypeEntry> {
        self.index
            .types
            .values()
            .filter(|ty| simple_name(&ty.fqcn) == name)
            .collect()
    }

    fn collect_candidates(&self, hint: &str) -> Vec<String> {
        if hint.is_empty() {
            return Vec::new();
        }

        let mut seen = HashSet::new();
        let mut results = Vec::new();
        for fqcn in self.index.types.keys() {
            let simple = simple_name(fqcn);
            if simple == hint && seen.insert(fqcn.clone()) {
                results.push(fqcn.clone());
            }
            if results.len() >= 8 {
                break;
            }
        }
        results
    }
}

fn simple_name(fqcn: &str) -> &str {
    fqcn.rsplit_once('.').map(|(_, tail)| tail).unwrap_or(fqcn)
}

fn simple_name_hint(path: &str) -> &str {
    simple_name(path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_build::metadata::{
        JavaMethodSignature, ModuleEntry, PackageEntry, SymbolIndex, TypeEntry,
    };
    use jv_ir::types::JavaType;
    use std::collections::HashSet;

    fn index_with_type(fqcn: &str, module: Option<&str>, statics: &[&str]) -> SymbolIndex {
        let mut index = SymbolIndex::default();

        let package = fqcn
            .rsplit_once('.')
            .map(|(pkg, _)| pkg.to_string())
            .unwrap_or_else(|| "".to_string());

        let mut type_entry = TypeEntry::new(
            fqcn.to_string(),
            package.clone(),
            module.map(ToOwned::to_owned),
        );
        for member in statics {
            type_entry.add_static_method(
                (*member).to_string(),
                JavaMethodSignature {
                    parameters: Vec::new(),
                    return_type: JavaType::Void,
                },
            );
        }

        if let Some(module_name) = module {
            index.modules.insert(
                module_name.to_string(),
                ModuleEntry {
                    name: module_name.to_string(),
                    exports: HashSet::from([package.clone()]),
                },
            );
        }

        let fqcn = type_entry.fqcn.clone();
        index.types.insert(fqcn.clone(), type_entry);

        index
            .packages
            .entry(package.clone())
            .or_insert_with(|| PackageEntry {
                name: package.clone(),
                module: module.map(|m| m.to_string()),
                types: HashSet::new(),
            })
            .types
            .insert(fqcn);
        index
    }

    fn dummy_span() -> Span {
        Span::new(1, 0, 1, 5)
    }

    #[test]
    fn resolve_type_import_by_fully_qualified_name() {
        let index = Arc::new(index_with_type(
            "java.time.LocalDate",
            Some("java.base"),
            &[],
        ));
        let service = ImportResolutionService::new(index, JavaTarget::Java25);
        let stmt = Statement::Import {
            path: "java.time.LocalDate".into(),
            alias: None,
            is_wildcard: false,
            span: dummy_span(),
        };

        let resolved = service.resolve(&stmt).expect("resolved import");
        assert!(matches!(
            resolved.kind,
            ResolvedImportKind::Type { ref fqcn } if fqcn == "java.time.LocalDate"
        ));
        assert_eq!(resolved.module_dependency, Some("java.base".into()));
    }

    #[test]
    fn resolve_type_import_by_simple_name() {
        let index = Arc::new(index_with_type("java.util.List", Some("java.base"), &[]));
        let service = ImportResolutionService::new(index, JavaTarget::Java25);
        let stmt = Statement::Import {
            path: "List".into(),
            alias: None,
            is_wildcard: false,
            span: dummy_span(),
        };

        let resolved = service.resolve(&stmt).expect("resolved import");
        assert!(matches!(
            resolved.kind,
            ResolvedImportKind::Type { ref fqcn } if fqcn == "java.util.List"
        ));
    }

    #[test]
    fn resolve_static_member_import() {
        let mut index = SymbolIndex::default();
        index.modules.insert(
            "java.base".into(),
            ModuleEntry {
                name: "java.base".into(),
                exports: HashSet::from(["java.lang".into()]),
            },
        );

        let mut type_entry = TypeEntry::new(
            "java.lang.Math".into(),
            "java.lang".into(),
            Some("java.base".into()),
        );
        type_entry.add_static_method(
            "max".into(),
            JavaMethodSignature {
                parameters: vec![
                    JavaType::Primitive("int".into()),
                    JavaType::Primitive("int".into()),
                ],
                return_type: JavaType::Primitive("int".into()),
            },
        );

        index.types.insert("java.lang.Math".into(), type_entry);
        index
            .packages
            .entry("java.lang".into())
            .or_insert_with(|| PackageEntry {
                name: "java.lang".into(),
                module: Some("java.base".into()),
                types: HashSet::new(),
            })
            .types
            .insert("java.lang.Math".into());
        let service = ImportResolutionService::new(Arc::new(index), JavaTarget::Java25);

        let stmt = Statement::Import {
            path: "java.lang.Math.max".into(),
            alias: None,
            is_wildcard: false,
            span: dummy_span(),
        };

        let resolved = service.resolve(&stmt).expect("resolved import");
        assert!(matches!(
            resolved.kind,
            ResolvedImportKind::StaticMember { ref owner, ref member }
            if owner == "java.lang.Math" && member == "max"
        ));
        assert_eq!(resolved.module_dependency, Some("java.base".into()));
    }

    #[test]
    fn resolve_package_wildcard_import() {
        let mut index = SymbolIndex::default();
        index.modules.insert(
            "java.base".into(),
            ModuleEntry {
                name: "java.base".into(),
                exports: HashSet::from(["java.util".into()]),
            },
        );

        let mut package_types = HashSet::new();
        for fqcn in ["java.util.List", "java.util.Set"] {
            let fqcn_string = fqcn.to_string();
            index.types.insert(
                fqcn_string.clone(),
                TypeEntry::new(
                    fqcn_string.clone(),
                    "java.util".into(),
                    Some("java.base".into()),
                ),
            );
            package_types.insert(fqcn_string);
        }

        index.packages.insert(
            "java.util".into(),
            PackageEntry {
                name: "java.util".into(),
                module: Some("java.base".into()),
                types: package_types,
            },
        );

        let service = ImportResolutionService::new(Arc::new(index), JavaTarget::Java25);
        let stmt = Statement::Import {
            path: "java.util".into(),
            alias: None,
            is_wildcard: true,
            span: dummy_span(),
        };

        let resolved = service.resolve(&stmt).expect("resolved import");
        assert!(matches!(
            resolved.kind,
            ResolvedImportKind::Package { ref name } if name == "java.util"
        ));
        assert_eq!(resolved.module_dependency, Some("java.base".into()));

        let expanded = service.expand_wildcard("java.util");
        let fqcn_set: HashSet<_> = expanded
            .into_iter()
            .filter_map(|imp| {
                if let ResolvedImportKind::Type { fqcn } = imp.kind {
                    Some(fqcn)
                } else {
                    None
                }
            })
            .collect();
        assert!(fqcn_set.contains("java.util.List"));
        assert!(fqcn_set.contains("java.util.Set"));
    }

    #[test]
    fn unknown_import_returns_error_with_candidates() {
        let mut index = SymbolIndex::default();
        index.types.insert(
            "java.util.List".into(),
            TypeEntry::new(
                "java.util.List".into(),
                "java.util".into(),
                Some("java.base".into()),
            ),
        );
        index.packages.insert(
            "java.util".into(),
            PackageEntry {
                name: "java.util".into(),
                module: Some("java.base".into()),
                types: HashSet::from(["java.util.List".into()]),
            },
        );

        let service = ImportResolutionService::new(Arc::new(index), JavaTarget::Java25);
        let stmt = Statement::Import {
            path: "java.util.Optional".into(),
            alias: None,
            is_wildcard: false,
            span: dummy_span(),
        };

        let error = service
            .resolve(&stmt)
            .expect_err("expected resolution error");
        match error {
            ImportError::UnknownSymbol { path, .. } => assert_eq!(path, "java.util.Optional"),
            other => panic!("unexpected error: {:?}", other),
        }
    }
}
