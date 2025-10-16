use crate::metadata::conversion_detector::ConversionDetector;
use crate::metadata::{JavaMethodSignature, SymbolIndex, TypeEntry};
use blake3::Hasher;
use jv_ir::types::{JavaType, JavaWildcardKind};
use jv_pm::JavaTarget;
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

const DEFAULT_CACHE_CAPACITY: usize = 8;

/// Catalog of conversion helper methods harvested from the `SymbolIndex`.
#[derive(Debug, Clone, Default)]
pub struct ConversionCatalog {
    entries: HashMap<String, Vec<ConversionRecord>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ConversionRecord {
    target: JavaType,
    helper: HelperMethod,
}

/// Description of a helper method that enables a conversion.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HelperMethod {
    pub owner: String,
    pub method: String,
    pub is_static: bool,
}

impl HelperMethod {
    pub fn instance(owner: impl Into<String>, method: impl Into<String>) -> Self {
        Self {
            owner: owner.into(),
            method: method.into(),
            is_static: false,
        }
    }

    pub fn static_method(owner: impl Into<String>, method: impl Into<String>) -> Self {
        Self {
            owner: owner.into(),
            method: method.into(),
            is_static: true,
        }
    }
}

impl ConversionCatalog {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_symbol_index(index: &SymbolIndex) -> Self {
        let mut catalog = ConversionCatalog::new();
        let detector = ConversionDetector::new();
        for entry in index.types.values() {
            catalog.collect_from_entry(entry, &detector);
        }
        catalog
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn find_helper(&self, from: &JavaType, to: &JavaType) -> Option<HelperMethod> {
        let key = java_type_key(from);
        self.entries.get(&key).and_then(|records| {
            records
                .iter()
                .find(|record| record.target == *to)
                .map(|record| record.helper.clone())
        })
    }

    fn collect_from_entry(&mut self, entry: &TypeEntry, detector: &ConversionDetector) {
        for (name, signature) in &entry.static_methods {
            if let Some((source, target, helper)) = detect_static_conversion(entry, name, signature)
            {
                self.record_conversion(source, target, helper);
            }
        }

        self.apply_detected_conversions(entry, detector);

        if entry.instance_methods.contains_key("toString") {
            let source = JavaType::Reference {
                name: entry.fqcn.clone(),
                generic_args: Vec::new(),
            };
            let target = JavaType::Reference {
                name: "java.lang.String".to_string(),
                generic_args: Vec::new(),
            };
            let helper = HelperMethod::instance(entry.fqcn.clone(), "toString");
            self.record_conversion(source, target, helper);
        }
    }

    fn record_conversion(&mut self, source: JavaType, target: JavaType, helper: HelperMethod) {
        let key = java_type_key(&source);
        let records = self.entries.entry(key).or_default();
        let record = ConversionRecord { target, helper };
        if !records.contains(&record) {
            records.push(record);
        }
    }

    fn apply_detected_conversions(&mut self, entry: &TypeEntry, detector: &ConversionDetector) {
        for signature in detector.detect_conversions(entry) {
            if signature.confidence < 0.8 {
                continue;
            }

            let helper = if signature.is_static {
                HelperMethod::static_method(entry.fqcn.clone(), signature.method_name.clone())
            } else {
                HelperMethod::instance(entry.fqcn.clone(), signature.method_name.clone())
            };

            self.record_conversion(
                signature.source_type.clone(),
                signature.target_type.clone(),
                helper,
            );
        }
    }
}

fn detect_static_conversion(
    entry: &TypeEntry,
    name: &str,
    signature: &JavaMethodSignature,
) -> Option<(JavaType, JavaType, HelperMethod)> {
    if name == "valueOf" && signature.parameters.len() == 1 {
        let source = signature.parameters[0].clone();
        let target = signature.return_type.clone();
        return Some((
            source,
            target,
            HelperMethod::static_method(entry.fqcn.clone(), name.to_string()),
        ));
    }

    if name.starts_with("parse") && signature.parameters.len() == 1 {
        let source = signature.parameters[0].clone();
        let target = signature.return_type.clone();
        return Some((
            source,
            target,
            HelperMethod::static_method(entry.fqcn.clone(), name.to_string()),
        ));
    }

    if name == "toString" && signature.parameters.len() == 1 {
        let target = &signature.return_type;
        if is_string_type(target) {
            let source = signature.parameters[0].clone();
            return Some((
                source,
                target.clone(),
                HelperMethod::static_method(entry.fqcn.clone(), name.to_string()),
            ));
        }
    }

    None
}

fn is_string_type(ty: &JavaType) -> bool {
    matches!(
        ty,
        JavaType::Reference { name, .. } if name == "java.lang.String"
    )
}

fn java_type_key(ty: &JavaType) -> String {
    match ty {
        JavaType::Primitive(name) => format!("P:{name}"),
        JavaType::Reference { name, generic_args } => {
            if generic_args.is_empty() {
                format!("R:{name}")
            } else {
                let mut args = String::new();
                for (index, arg) in generic_args.iter().enumerate() {
                    if index > 0 {
                        args.push(',');
                    }
                    args.push_str(&java_type_key(arg));
                }
                format!("R:{name}<{}>", args)
            }
        }
        JavaType::Array {
            element_type,
            dimensions,
        } => format!("A:{};{}", java_type_key(element_type), dimensions),
        JavaType::Functional {
            interface_name,
            param_types,
            return_type,
        } => {
            let mut params = String::new();
            for (index, param) in param_types.iter().enumerate() {
                if index > 0 {
                    params.push(',');
                }
                params.push_str(&java_type_key(param));
            }
            format!(
                "F:{interface_name}({})->{}",
                params,
                java_type_key(return_type)
            )
        }
        JavaType::Wildcard { kind, bound } => {
            let kind_label = match kind {
                JavaWildcardKind::Unbounded => "*",
                JavaWildcardKind::Extends => "+",
                JavaWildcardKind::Super => "-",
            };
            let bound_label = bound
                .as_ref()
                .map(|ty| java_type_key(ty.as_ref()))
                .unwrap_or_default();
            format!("W:{kind_label}:{bound_label}")
        }
        JavaType::Void => "V:void".to_string(),
    }
}

fn signature_params_key(params: &[JavaType]) -> String {
    if params.is_empty() {
        return String::new();
    }
    let mut buffer = String::new();
    for (index, param) in params.iter().enumerate() {
        if index > 0 {
            buffer.push(';');
        }
        buffer.push_str(&java_type_key(param));
    }
    buffer
}

fn symbol_index_hash(index: &SymbolIndex) -> String {
    let mut fragments = Vec::new();
    if let Some(release) = index.jdk_release {
        fragments.push(format!("release:{release}"));
    }

    let mut type_keys: Vec<_> = index.types.keys().collect();
    type_keys.sort();
    for key in type_keys {
        if let Some(entry) = index.types.get(key) {
            fragments.push(format!("type:{key}"));
            if let Some(module) = &entry.module {
                fragments.push(format!("module:{module}"));
            }

            let mut static_methods: Vec<_> = entry.static_methods.iter().collect();
            static_methods.sort_by(|left, right| left.0.cmp(right.0));
            for (name, signature) in static_methods {
                fragments.push(format!(
                    "sm:{name}({})->{}",
                    signature_params_key(&signature.parameters),
                    java_type_key(&signature.return_type)
                ));
            }

            let mut instance_methods: Vec<_> = entry.instance_methods.iter().collect();
            instance_methods.sort_by(|left, right| left.0.cmp(right.0));
            for (name, signature) in instance_methods {
                fragments.push(format!(
                    "im:{name}({})->{}",
                    signature_params_key(&signature.parameters),
                    java_type_key(&signature.return_type)
                ));
            }
        }
    }

    let mut hasher = Hasher::new();
    for fragment in fragments {
        hasher.update(fragment.as_bytes());
    }
    hasher.finalize().to_hex().to_string()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CatalogCacheKey {
    target: JavaTarget,
    index_hash: String,
}

impl CatalogCacheKey {
    pub fn new(target: JavaTarget, index: &SymbolIndex) -> Self {
        let hash = symbol_index_hash(index);
        Self {
            target,
            index_hash: hash,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CatalogAccess {
    pub catalog: Arc<ConversionCatalog>,
    pub hit: bool,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct CatalogCacheStats {
    pub hits: u64,
    pub misses: u64,
}

#[derive(Debug, Clone)]
pub struct ConversionCatalogCache {
    capacity: usize,
    entries: HashMap<CatalogCacheKey, Arc<ConversionCatalog>>,
    order: VecDeque<CatalogCacheKey>,
    hits: u64,
    misses: u64,
}

impl ConversionCatalogCache {
    pub fn with_capacity(capacity: usize) -> Self {
        let size = capacity.max(1);
        Self {
            capacity: size,
            entries: HashMap::new(),
            order: VecDeque::new(),
            hits: 0,
            misses: 0,
        }
    }

    pub fn new() -> Self {
        Self::with_capacity(DEFAULT_CACHE_CAPACITY)
    }

    pub fn access(&mut self, target: JavaTarget, index: &SymbolIndex) -> CatalogAccess {
        let key = CatalogCacheKey::new(target, index);
        if let Some(catalog) = self.entries.get(&key).cloned() {
            self.hits += 1;
            self.bump_key(&key);
            return CatalogAccess { catalog, hit: true };
        }

        let catalog = Arc::new(ConversionCatalog::from_symbol_index(index));
        self.insert(key.clone(), Arc::clone(&catalog));
        self.misses += 1;
        CatalogAccess {
            catalog,
            hit: false,
        }
    }

    pub fn stats(&self) -> CatalogCacheStats {
        CatalogCacheStats {
            hits: self.hits,
            misses: self.misses,
        }
    }

    fn insert(&mut self, key: CatalogCacheKey, catalog: Arc<ConversionCatalog>) {
        if self.entries.len() >= self.capacity {
            if let Some(oldest) = self.order.pop_front() {
                self.entries.remove(&oldest);
            }
        }
        self.order.push_back(key.clone());
        self.entries.insert(key, catalog);
    }

    fn bump_key(&mut self, key: &CatalogCacheKey) {
        if let Some(position) = self.order.iter().position(|entry| entry == key) {
            self.order.remove(position);
        }
        self.order.push_back(key.clone());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_entry() -> TypeEntry {
        let mut entry = TypeEntry::new(
            "java.lang.Integer".to_string(),
            "java.lang".to_string(),
            None,
        );
        entry.static_methods.insert(
            "valueOf".to_string(),
            JavaMethodSignature {
                parameters: vec![JavaType::Primitive("int".to_string())],
                return_type: JavaType::Reference {
                    name: "java.lang.Integer".to_string(),
                    generic_args: Vec::new(),
                },
            },
        );
        entry.instance_methods.insert(
            "toString".to_string(),
            JavaMethodSignature {
                parameters: Vec::new(),
                return_type: JavaType::Reference {
                    name: "java.lang.String".to_string(),
                    generic_args: Vec::new(),
                },
            },
        );
        entry
    }

    #[test]
    fn builds_catalog_from_index() {
        let mut index = SymbolIndex::new(Some(25));
        index.add_type(sample_entry());

        let catalog = ConversionCatalog::from_symbol_index(&index);
        assert!(!catalog.is_empty());

        let source = JavaType::Primitive("int".to_string());
        let target = JavaType::Reference {
            name: "java.lang.Integer".to_string(),
            generic_args: Vec::new(),
        };
        let helper = catalog
            .find_helper(&source, &target)
            .expect("valueOf conversion should exist");
        assert_eq!(helper.owner, "java.lang.Integer");
        assert_eq!(helper.method, "valueOf");
        assert!(helper.is_static);
    }

    #[test]
    fn cache_records_hit_and_miss() {
        let mut index = SymbolIndex::new(Some(25));
        index.add_type(sample_entry());
        let mut cache = ConversionCatalogCache::with_capacity(2);

        let access1 = cache.access(JavaTarget::Java25, &index);
        assert!(!access1.hit);
        let access2 = cache.access(JavaTarget::Java25, &index);
        assert!(access2.hit);

        let stats = cache.stats();
        assert_eq!(stats.hits, 1);
        assert_eq!(stats.misses, 1);
    }
}
