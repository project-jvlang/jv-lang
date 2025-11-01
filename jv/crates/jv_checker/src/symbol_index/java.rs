use jv_build::metadata::{ModuleEntry, SymbolIndex, TypeEntry};
use once_cell::sync::Lazy;
use std::collections::HashMap;

/// フォールバックとして扱う Java 標準ライブラリの型情報。
#[derive(Debug, Clone, Copy)]
struct JavaFallbackType {
    fqcn: &'static str,
    package: &'static str,
    simple: &'static str,
    module: Option<&'static str>,
}

/// 外部公開用のフォールバックメタデータ。
#[derive(Debug, Clone, Copy)]
pub struct JavaFallbackMetadata {
    pub fqcn: &'static str,
    pub package: &'static str,
    pub simple: &'static str,
    pub module: Option<&'static str>,
}

const FALLBACK_TYPES: &[JavaFallbackType] = &[
    JavaFallbackType {
        fqcn: "java.util.stream.Stream",
        package: "java.util.stream",
        simple: "Stream",
        module: Some("java.base"),
    },
    JavaFallbackType {
        fqcn: "java.time.LocalDate",
        package: "java.time",
        simple: "LocalDate",
        module: Some("java.base"),
    },
];

static SIMPLE_NAME_MAP: Lazy<HashMap<&'static str, &'static JavaFallbackType>> =
    Lazy::new(|| FALLBACK_TYPES.iter().map(|ty| (ty.simple, ty)).collect());

static FQCN_MAP: Lazy<HashMap<&'static str, &'static JavaFallbackType>> =
    Lazy::new(|| FALLBACK_TYPES.iter().map(|ty| (ty.fqcn, ty)).collect());

static PACKAGE_MAP: Lazy<HashMap<&'static str, Vec<&'static JavaFallbackType>>> = Lazy::new(|| {
    let mut map: HashMap<&'static str, Vec<&'static JavaFallbackType>> = HashMap::new();
    for ty in FALLBACK_TYPES {
        map.entry(ty.package).or_default().push(ty);
    }
    map
});

/// シンボルインデックスにフォールバック型を登録する。
///
/// 実行環境の JDK メタデータに該当クラスが含まれていない場合でも、
/// 代表的な型が解決できるようにする。
pub fn ensure_fallback_types(index: &mut SymbolIndex) {
    for ty in FALLBACK_TYPES {
        if index.lookup_type(ty.fqcn).is_some() {
            continue;
        }

        let entry = TypeEntry::new(
            ty.fqcn.to_string(),
            ty.package.to_string(),
            ty.module.map(|module| module.to_string()),
        );
        index.add_type(entry);

        if let Some(module) = ty.module {
            index
                .modules
                .entry(module.to_string())
                .and_modify(|existing| {
                    existing.exports.insert(ty.package.to_string());
                })
                .or_insert_with(|| {
                    let mut module_entry = ModuleEntry::new(module.to_string());
                    module_entry.exports.insert(ty.package.to_string());
                    module_entry
                });
        }
    }
}

fn to_metadata(entry: &JavaFallbackType) -> JavaFallbackMetadata {
    JavaFallbackMetadata {
        fqcn: entry.fqcn,
        package: entry.package,
        simple: entry.simple,
        module: entry.module,
    }
}

/// シンプル名からフォールバック型の FQCN を取得する。
pub fn fallback_fqcn(simple_name: &str) -> Option<&'static str> {
    SIMPLE_NAME_MAP.get(simple_name).map(|ty| ty.fqcn)
}

/// シンプル名からフォールバックメタデータを取得する。
pub fn fallback_metadata_by_simple(simple_name: &str) -> Option<JavaFallbackMetadata> {
    SIMPLE_NAME_MAP
        .get(simple_name)
        .map(|entry| to_metadata(entry))
}

/// FQCN からフォールバックメタデータを取得する。
pub fn fallback_metadata_by_fqcn(fqcn: &str) -> Option<JavaFallbackMetadata> {
    FQCN_MAP.get(fqcn).map(|entry| to_metadata(entry))
}

/// パッケージ名とシンプル名に対応するフォールバック FQCN を返す。
pub fn fallback_fqcn_in_package(package: &str, simple: &str) -> Option<&'static str> {
    PACKAGE_MAP.get(package).and_then(|entries| {
        entries
            .iter()
            .find(|ty| ty.simple == simple)
            .map(|ty| ty.fqcn)
    })
}

/// パッケージ名とシンプル名に対応するフォールバックメタデータを返す。
pub fn fallback_metadata_in_package(package: &str, simple: &str) -> Option<JavaFallbackMetadata> {
    PACKAGE_MAP.get(package).and_then(|entries| {
        entries
            .iter()
            .find(|ty| ty.simple == simple)
            .map(|entry| to_metadata(entry))
    })
}
