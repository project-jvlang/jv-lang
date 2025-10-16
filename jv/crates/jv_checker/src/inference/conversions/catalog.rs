use super::HelperSpec;
use crate::inference::types::TypeKind;
use crate::java::JavaBoxingTable;
use jv_build::metadata::{ConversionCatalog, HelperMethod};
use jv_ir::types::JavaType;
use std::sync::Arc;

/// Thin adapter that exposes catalog lookups in terms of `TypeKind` and `HelperSpec`.
#[derive(Debug, Clone)]
pub struct ConversionHelperCatalog {
    inner: Arc<ConversionCatalog>,
}

impl ConversionHelperCatalog {
    pub fn new(inner: Arc<ConversionCatalog>) -> Self {
        Self { inner }
    }

    pub fn find_helper(&self, from: &TypeKind, to: &TypeKind) -> Option<HelperSpec> {
        let source = type_kind_to_java(from)?;
        let target = type_kind_to_java(to)?;
        self.inner.find_helper(&source, &target).map(helper_to_spec)
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn as_arc(&self) -> Arc<ConversionCatalog> {
        Arc::clone(&self.inner)
    }
}

fn helper_to_spec(helper: HelperMethod) -> HelperSpec {
    if helper.is_static {
        HelperSpec::static_method(helper.owner, helper.method)
    } else {
        HelperSpec::instance(helper.owner, helper.method)
    }
}

fn type_kind_to_java(kind: &TypeKind) -> Option<JavaType> {
    match kind {
        TypeKind::Primitive(primitive) => {
            Some(JavaType::Primitive(primitive.java_name().to_string()))
        }
        TypeKind::Boxed(primitive) => Some(JavaType::Reference {
            name: JavaBoxingTable::boxed_fqcn(*primitive).to_string(),
            generic_args: Vec::new(),
        }),
        TypeKind::Reference(name) => Some(JavaType::Reference {
            name: name.clone(),
            generic_args: Vec::new(),
        }),
        TypeKind::Optional(inner) => type_kind_to_java(inner),
        TypeKind::Function(_, _) | TypeKind::Variable(_) | TypeKind::Unknown => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inference::types::PrimitiveType;
    use jv_build::metadata::{ConversionCatalog, SymbolIndex, TypeEntry};
    use jv_ir::types::JavaType;

    fn build_catalog() -> ConversionHelperCatalog {
        let mut entry = TypeEntry::new(
            "java.lang.String".to_string(),
            "java.lang".to_string(),
            None,
        );
        entry.static_methods.insert(
            "valueOf".to_string(),
            jv_build::metadata::JavaMethodSignature {
                parameters: vec![JavaType::Primitive("int".to_string())],
                return_type: JavaType::Reference {
                    name: "java.lang.String".to_string(),
                    generic_args: Vec::new(),
                },
            },
        );

        let mut index = SymbolIndex::new(Some(25));
        index.add_type(entry);
        let catalog = ConversionCatalog::from_symbol_index(&index);
        ConversionHelperCatalog::new(Arc::new(catalog))
    }

    #[test]
    fn resolves_static_helper() {
        let catalog = build_catalog();
        let helper = catalog.find_helper(
            &TypeKind::primitive(PrimitiveType::Int),
            &TypeKind::reference("java.lang.String"),
        );
        let helper = helper.expect("expected helper for String.valueOf");
        assert_eq!(helper.owner, "java.lang.String");
        assert_eq!(helper.method, "valueOf");
        assert!(helper.is_static);
    }
}
