use jv_build::metadata::JavaMethodSignature;
use jv_build::metadata::{
    CatalogAccess, ConversionCatalog, ConversionCatalogCache, SymbolIndex, TypeEntry,
};
use jv_ir::types::JavaType;
use jv_pm::JavaTarget;

fn sample_type_entry(fqcn: &str) -> TypeEntry {
    let mut entry = TypeEntry::new(fqcn.to_string(), "com.example".to_string(), None);
    entry.static_methods.insert(
        "valueOf".to_string(),
        JavaMethodSignature {
            parameters: vec![JavaType::Primitive("int".to_string())],
            return_type: JavaType::Reference {
                name: fqcn.to_string(),
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
fn harvests_conversions_for_many_types() {
    let mut index = SymbolIndex::new(Some(25));
    for i in 0..60 {
        let fqcn = format!("com.example.Type{i}");
        index.add_type(sample_type_entry(&fqcn));
    }

    let catalog = ConversionCatalog::from_symbol_index(&index);
    let mut harvested = 0usize;
    for i in 0..60 {
        let target = JavaType::Reference {
            name: format!("com.example.Type{i}"),
            generic_args: Vec::new(),
        };
        let helper = catalog.find_helper(&JavaType::Primitive("int".to_string()), &target);
        if helper.is_some() {
            harvested += 1;
        }
    }

    assert!(
        harvested >= 50,
        "expected at least 50 conversions, observed {harvested}"
    );
}

#[test]
fn lru_cache_eviction_and_metrics() {
    let mut index_a = SymbolIndex::new(Some(25));
    index_a.add_type(sample_type_entry("com.example.TypeA"));

    let mut index_b = SymbolIndex::new(Some(25));
    index_b.add_type(sample_type_entry("com.example.TypeB"));

    let mut index_c = SymbolIndex::new(Some(25));
    index_c.add_type(sample_type_entry("com.example.TypeC"));

    let mut cache = ConversionCatalogCache::with_capacity(2);

    let CatalogAccess { hit, .. } = cache.access(JavaTarget::Java25, &index_a);
    assert!(!hit);
    let CatalogAccess { hit, .. } = cache.access(JavaTarget::Java25, &index_b);
    assert!(!hit);
    let CatalogAccess { hit, .. } = cache.access(JavaTarget::Java25, &index_a);
    assert!(hit);

    let CatalogAccess { hit, .. } = cache.access(JavaTarget::Java25, &index_c);
    assert!(!hit, "introducing third entry should trigger miss");

    let CatalogAccess { hit, catalog } = cache.access(JavaTarget::Java25, &index_b);
    assert!(!hit, "entry B should have been evicted by LRU policy");

    let helper = catalog.find_helper(
        &JavaType::Primitive("int".to_string()),
        &JavaType::Reference {
            name: "com.example.TypeB".to_string(),
            generic_args: Vec::new(),
        },
    );
    assert!(helper.is_some());

    let stats = cache.stats();
    assert_eq!(stats.hits, 1);
    assert_eq!(stats.misses, 4);
}
