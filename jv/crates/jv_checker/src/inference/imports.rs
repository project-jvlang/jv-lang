use crate::imports::{ResolvedImport, ResolvedImportKind};
use crate::inference::environment::{TypeEnvironment, TypeScheme};
use crate::inference::type_factory::TypeFactory;
use crate::inference::types::TypeKind;
use jv_build::metadata::{JavaMethodSignature, StaticMemberKind, SymbolIndex};
use jv_ir::types::JavaType;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// Maintains namespace information derived from resolved imports and bridges it into the
/// type environment on demand.
#[derive(Debug)]
pub struct ImportRegistry {
    symbol_index: Arc<SymbolIndex>,
    type_aliases: HashMap<String, String>,
    package_wildcards: HashSet<String>,
    static_wildcards: HashSet<String>,
}

impl ImportRegistry {
    pub fn new(symbol_index: Arc<SymbolIndex>) -> Self {
        Self {
            symbol_index,
            type_aliases: HashMap::new(),
            package_wildcards: HashSet::new(),
            static_wildcards: HashSet::new(),
        }
    }

    /// Registers explicit (non-wildcard) imports into the provided environment and records
    /// wildcard metadata for later lookups.
    pub fn register_imports(&mut self, env: &mut TypeEnvironment, imports: &[ResolvedImport]) {
        for import in imports {
            if import.is_wildcard {
                self.register_wildcard(import);
                continue;
            }

            match &import.kind {
                ResolvedImportKind::Type { fqcn } => {
                    let alias = import
                        .alias
                        .clone()
                        .unwrap_or_else(|| simple_name(&import.original_path).to_string());
                    self.register_type_alias(env, alias, fqcn.clone());
                }
                ResolvedImportKind::StaticMember { owner, member } => {
                    if member == "*" {
                        self.static_wildcards.insert(owner.clone());
                        continue;
                    }
                    if let Some(scheme) = self.static_member_scheme(owner, member) {
                        let alias = import.alias.clone().unwrap_or_else(|| member.to_string());
                        env.define_scheme(alias.clone(), scheme);
                    }
                }
                ResolvedImportKind::Package { .. } => {}
                ResolvedImportKind::Module { .. } => {}
            }
        }
    }

    /// Attempts to resolve an identifier via previously registered imports. When a wildcard
    /// import matches the requested symbol, the binding is materialized and stored in the
    /// environment so subsequent lookups become fast.
    pub fn resolve_identifier(
        &mut self,
        env: &mut TypeEnvironment,
        name: &str,
    ) -> Option<TypeKind> {
        if let Some(scheme) = env.lookup(name).cloned() {
            return Some(env.instantiate(&scheme));
        }

        if let Some(fqcn) = self.type_aliases.get(name).cloned() {
            let ty = TypeKind::Reference(fqcn.clone());
            env.define_scheme(name.to_string(), TypeScheme::monotype(ty.clone()));
            return Some(ty);
        }

        if let Some(ty) = self.resolve_type_from_wildcard(env, name) {
            return Some(ty);
        }

        if let Some(ty) = self.resolve_static_from_wildcard(env, name) {
            return Some(ty);
        }

        None
    }

    fn register_wildcard(&mut self, import: &ResolvedImport) {
        match &import.kind {
            ResolvedImportKind::Package { name } => {
                self.package_wildcards.insert(name.clone());
            }
            ResolvedImportKind::StaticMember { owner, member } if member == "*" => {
                self.static_wildcards.insert(owner.clone());
            }
            _ => {}
        }
    }

    fn register_type_alias(&mut self, env: &mut TypeEnvironment, alias: String, fqcn: String) {
        self.type_aliases.insert(alias.clone(), fqcn.clone());
        let ty = TypeKind::reference(fqcn);
        env.define_scheme(alias, TypeScheme::monotype(ty));
    }

    fn resolve_type_from_wildcard(
        &mut self,
        env: &mut TypeEnvironment,
        name: &str,
    ) -> Option<TypeKind> {
        for package in &self.package_wildcards {
            let candidate = format!("{package}.{name}");
            if self.symbol_index.lookup_type(&candidate).is_some() {
                let ty = TypeKind::reference(candidate.clone());
                self.type_aliases
                    .insert(name.to_string(), candidate.clone());
                env.define_scheme(name.to_string(), TypeScheme::monotype(ty.clone()));
                return Some(ty);
            }
        }
        None
    }

    fn resolve_static_from_wildcard(
        &mut self,
        env: &mut TypeEnvironment,
        name: &str,
    ) -> Option<TypeKind> {
        for owner in &self.static_wildcards {
            if let Some(member) = self.symbol_index.lookup_static(owner, name) {
                match member.kind {
                    StaticMemberKind::Field { ref ty } => {
                        let scheme = TypeScheme::monotype(java_type_to_type_kind(ty));
                        env.define_scheme(name.to_string(), scheme);
                    }
                    StaticMemberKind::Method { ref signature } => {
                        let scheme = TypeScheme::monotype(method_signature_to_type(signature));
                        env.define_scheme(name.to_string(), scheme);
                    }
                }
                if let Some(scheme) = env.lookup(name).cloned() {
                    return Some(env.instantiate(&scheme));
                }
            }
        }
        None
    }

    fn static_member_scheme(&self, owner: &str, member: &str) -> Option<TypeScheme> {
        let static_info = self.symbol_index.lookup_static(owner, member)?;
        match static_info.kind {
            StaticMemberKind::Field { ref ty } => {
                Some(TypeScheme::monotype(java_type_to_type_kind(ty)))
            }
            StaticMemberKind::Method { ref signature } => {
                Some(TypeScheme::monotype(method_signature_to_type(signature)))
            }
        }
    }
}

fn method_signature_to_type(signature: &JavaMethodSignature) -> TypeKind {
    let params = signature
        .parameters
        .iter()
        .map(java_type_to_type_kind)
        .collect::<Vec<_>>();
    let return_ty = java_type_to_type_kind(&signature.return_type);
    TypeKind::function(params, return_ty)
}

fn java_type_to_type_kind(java_type: &JavaType) -> TypeKind {
    match java_type {
        JavaType::Primitive(name) => {
            TypeFactory::primitive_from_java_name(name).unwrap_or(TypeKind::Unknown)
        }
        JavaType::Reference { name, .. } => {
            if let Some(kind) = TypeFactory::boxed_from_fqcn(name) {
                kind
            } else {
                TypeKind::reference(name.clone())
            }
        }
        JavaType::Array {
            element_type,
            dimensions,
        } => {
            let element = java_type_to_type_kind(element_type);
            let mut base = match element {
                TypeKind::Primitive(primitive) => primitive.java_name().to_string(),
                TypeKind::Boxed(primitive) => primitive.boxed_fqcn().to_string(),
                TypeKind::Reference(name) => name,
                _ => return TypeKind::Unknown,
            };
            for _ in 0..*dimensions {
                base.push_str("[]");
            }
            TypeKind::reference(base)
        }
        JavaType::Functional { interface_name, .. } => TypeKind::reference(interface_name.clone()),
        JavaType::Wildcard { .. } => TypeKind::Unknown,
        JavaType::Void => TypeKind::reference("Unit"),
    }
}

fn simple_name(path: &str) -> &str {
    path.rsplit('.').next().unwrap_or(path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_build::metadata::{ModuleEntry, PackageEntry, SymbolIndex, TypeEntry};
    use std::collections::HashSet;

    fn index_with_type(fqcn: &str) -> SymbolIndex {
        let mut index = SymbolIndex::default();

        let package = fqcn
            .rsplit_once('.')
            .map(|(pkg, _)| pkg.to_string())
            .unwrap_or_default();

        index.types.insert(
            fqcn.to_string(),
            TypeEntry::new(fqcn.to_string(), package.clone(), Some("java.base".into())),
        );

        index.packages.insert(
            package.clone(),
            PackageEntry {
                name: package.clone(),
                module: Some("java.base".into()),
                types: HashSet::from([fqcn.to_string()]),
            },
        );

        index.modules.insert(
            "java.base".into(),
            ModuleEntry {
                name: "java.base".into(),
                exports: HashSet::from([package]),
            },
        );

        index
    }

    #[test]
    fn registers_explicit_type_alias() {
        let index = Arc::new(index_with_type("java.time.LocalDate"));
        let mut registry = ImportRegistry::new(index);
        let mut env = TypeEnvironment::new();

        let import = ResolvedImport {
            source_span: jv_ast::Span::dummy(),
            original_path: "java.time.LocalDate".into(),
            alias: None,
            is_wildcard: false,
            kind: ResolvedImportKind::Type {
                fqcn: "java.time.LocalDate".into(),
            },
            module_dependency: Some("java.base".into()),
        };

        registry.register_imports(&mut env, &[import]);
        let scheme = env.lookup("LocalDate").expect("alias registered");
        assert_eq!(scheme.ty, TypeKind::Reference("java.time.LocalDate".into()));
    }

    #[test]
    fn resolves_package_wildcard_on_demand() {
        let index = Arc::new(index_with_type("java.util.List"));
        let mut registry = ImportRegistry::new(index);
        let mut env = TypeEnvironment::new();

        let import = ResolvedImport {
            source_span: jv_ast::Span::dummy(),
            original_path: "java.util".into(),
            alias: None,
            is_wildcard: true,
            kind: ResolvedImportKind::Package {
                name: "java.util".into(),
            },
            module_dependency: Some("java.base".into()),
        };

        registry.register_imports(&mut env, &[import]);
        let ty = registry
            .resolve_identifier(&mut env, "List")
            .expect("wildcard resolved");
        assert_eq!(ty, TypeKind::Reference("java.util.List".into()));
    }
}
