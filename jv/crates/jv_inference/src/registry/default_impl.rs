//! Doublebrace 初期化向けのデフォルト実装レジストリ。
//!
//! インターフェースや抽象クラスに対して、コンパイラが安全に選択できる
//! 具象クラスを解決するためのテーブルを保持する。既定値はコンパイラに
//! 内蔵したマッピングを使用し、`JV_TYPE_DEFAULTS_PATH` で示された TOML
//! ファイルが存在する場合は開発者向けオーバーライドとして上書きする。

use jv_build::metadata::SymbolIndex;
use once_cell::sync::OnceCell;
use serde::Deserialize;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

const OVERRIDE_ENV: &str = "JV_TYPE_DEFAULTS_PATH";
const DEFAULT_OVERRIDE_RELATIVE: &str = ".config/jv/internal/defaults.toml";

static SHARED: OnceCell<DefaultImplementationRegistry> = OnceCell::new();

/// 登録済みのデフォルト実装を参照するためのレジストリ。
#[derive(Debug, Clone)]
pub struct DefaultImplementationRegistry {
    interface_defaults: HashMap<String, DefaultImplementationSource>,
    abstract_defaults: HashMap<String, DefaultImplementationSource>,
    load_errors: Vec<RegistryLoadError>,
}

impl DefaultImplementationRegistry {
    /// グローバルなレジストリを取得する。初回アクセス時にロードを行い、その後は再利用する。
    pub fn shared() -> &'static Self {
        SHARED.get_or_init(|| Self::load_from_env())
    }

    /// 環境変数やデフォルトパスを考慮してレジストリを構築する。
    pub fn load_from_env() -> Self {
        let override_path = env::var_os(OVERRIDE_ENV).map(PathBuf::from);
        let mut registry = Self::load(override_path.as_deref());

        // 環境変数が設定されていない場合のみ、既定パスを試みる。
        if override_path.is_none() {
            if let Some(path) = default_override_path() {
                registry.try_apply_override(&path);
            }
        }

        registry
    }

    /// 任意のオーバーライドパスを指定してレジストリを構築する（テスト向け）。
    pub fn load(override_path: Option<&Path>) -> Self {
        let mut registry = Self::with_builtins();
        if let Some(path) = override_path {
            registry.try_apply_override(path);
        }
        registry
    }

    /// 指定インターフェースに対する既定の具象クラスを返す。
    pub fn resolve_interface(&self, fqcn: &str) -> Option<&DefaultImplementationSource> {
        let key = normalize_fqcn(fqcn);
        self.interface_defaults.get(&key)
    }

    /// 指定抽象クラスに対する既定候補を返す。`SymbolIndex` が与えられた場合は存在確認も行う。
    pub fn resolve_abstract(
        &self,
        fqcn: &str,
        symbol_index: Option<&SymbolIndex>,
    ) -> Option<&DefaultImplementationSource> {
        let key = normalize_fqcn(fqcn);
        let candidate = self.abstract_defaults.get(&key)?;

        if let Some(index) = symbol_index {
            if index.lookup_type(candidate.target()).is_none() {
                return None;
            }
        }

        Some(candidate)
    }

    /// ロード時に発生した警告やエラーを返す。
    pub fn load_errors(&self) -> &[RegistryLoadError] {
        &self.load_errors
    }

    fn with_builtins() -> Self {
        let mut registry = Self {
            interface_defaults: HashMap::new(),
            abstract_defaults: HashMap::new(),
            load_errors: Vec::new(),
        };

        for (iface, impl_fqcn) in BUILTIN_INTERFACE_DEFAULTS.iter() {
            registry.insert_interface(
                iface.to_string(),
                impl_fqcn.to_string(),
                RegistryEntryOrigin::BuiltIn,
            );
        }

        for (abstract_ty, impl_fqcn) in BUILTIN_ABSTRACT_DEFAULTS.iter() {
            registry.insert_abstract(
                abstract_ty.to_string(),
                impl_fqcn.to_string(),
                RegistryEntryOrigin::BuiltIn,
            );
        }

        registry
    }

    fn try_apply_override(&mut self, path: &Path) {
        match self.load_override_file(path) {
            Ok(overrides) => {
                for (iface, target) in overrides.interfaces {
                    self.insert_interface(
                        iface,
                        target,
                        RegistryEntryOrigin::Override {
                            source: path_to_string(path),
                        },
                    );
                }
                for (abstract_ty, target) in overrides.abstract_classes {
                    self.insert_abstract(
                        abstract_ty,
                        target,
                        RegistryEntryOrigin::Override {
                            source: path_to_string(path),
                        },
                    );
                }
            }
            Err(error) => self.load_errors.push(error),
        }
    }

    fn load_override_file(&self, path: &Path) -> Result<OverrideFile, RegistryLoadError> {
        if !path.exists() {
            return Err(RegistryLoadError::new(
                format!("オーバーライドファイルが見つかりません: {}", path.display()),
                Some(path_to_string(path)),
            ));
        }

        match fs::read_to_string(path) {
            Ok(contents) => toml::from_str(&contents).map_err(|err| {
                RegistryLoadError::new(
                    format!("TOML の解析に失敗しました: {err}"),
                    Some(path_to_string(path)),
                )
            }),
            Err(err) => Err(RegistryLoadError::new(
                format!("オーバーライドファイルの読み込みに失敗しました: {err}"),
                Some(path_to_string(path)),
            )),
        }
    }

    fn insert_interface(&mut self, iface: String, target: String, origin: RegistryEntryOrigin) {
        if iface.trim().is_empty() || target.trim().is_empty() {
            self.load_errors.push(RegistryLoadError::new(
                "空の FQCN は登録できません".to_string(),
                None,
            ));
            return;
        }

        self.interface_defaults.insert(
            normalize_fqcn(&iface),
            DefaultImplementationSource::new(target, origin),
        );
    }

    fn insert_abstract(
        &mut self,
        abstract_ty: String,
        target: String,
        origin: RegistryEntryOrigin,
    ) {
        if abstract_ty.trim().is_empty() || target.trim().is_empty() {
            self.load_errors.push(RegistryLoadError::new(
                "空の FQCN は登録できません".to_string(),
                None,
            ));
            return;
        }

        self.abstract_defaults.insert(
            normalize_fqcn(&abstract_ty),
            DefaultImplementationSource::new(target, origin),
        );
    }
}

#[derive(Debug, Clone, Deserialize)]
struct OverrideFile {
    #[serde(default)]
    interfaces: HashMap<String, String>,
    #[serde(default, alias = "abstracts")]
    abstract_classes: HashMap<String, String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefaultImplementationSource {
    target: String,
    origin: RegistryEntryOrigin,
}

impl DefaultImplementationSource {
    fn new(target: String, origin: RegistryEntryOrigin) -> Self {
        Self {
            target: normalize_fqcn(&target),
            origin,
        }
    }

    /// 解決先の FQCN を返す。
    pub fn target(&self) -> &str {
        &self.target
    }

    /// この設定の由来を返す。
    pub fn origin(&self) -> &RegistryEntryOrigin {
        &self.origin
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegistryEntryOrigin {
    BuiltIn,
    Override { source: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegistryLoadError {
    message: String,
    source: Option<String>,
}

impl RegistryLoadError {
    pub fn new(message: String, source: Option<String>) -> Self {
        Self { message, source }
    }

    /// エラーメッセージ本文を返す。
    pub fn message(&self) -> &str {
        &self.message
    }

    /// エラー発生元のパス（存在する場合）を返す。
    pub fn source(&self) -> Option<&str> {
        self.source.as_deref()
    }
}

fn normalize_fqcn(input: &str) -> String {
    input.trim().replace('/', ".")
}

fn default_override_path() -> Option<PathBuf> {
    let home = env::var_os("HOME")?;
    let mut path = PathBuf::from(home);
    path.push(DEFAULT_OVERRIDE_RELATIVE);
    Some(path)
}

fn path_to_string(path: &Path) -> String {
    path.to_string_lossy().to_string()
}

const BUILTIN_INTERFACE_DEFAULTS: &[(&str, &str)] = &[
    ("java.util.Collection", "java.util.ArrayList"),
    ("java.util.List", "java.util.ArrayList"),
    ("java.util.Set", "java.util.LinkedHashSet"),
    ("java.util.Map", "java.util.LinkedHashMap"),
    ("java.util.Queue", "java.util.ArrayDeque"),
    ("java.util.Deque", "java.util.ArrayDeque"),
    (
        "java.util.concurrent.ConcurrentMap",
        "java.util.concurrent.ConcurrentHashMap",
    ),
];

const BUILTIN_ABSTRACT_DEFAULTS: &[(&str, &str)] = &[
    ("java.util.AbstractCollection", "java.util.ArrayList"),
    ("java.util.AbstractList", "java.util.ArrayList"),
    ("java.util.AbstractSequentialList", "java.util.LinkedList"),
    ("java.util.AbstractSet", "java.util.LinkedHashSet"),
    ("java.util.AbstractMap", "java.util.LinkedHashMap"),
];

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn builtin_list_resolves_to_array_list() {
        let registry = DefaultImplementationRegistry::load(None);
        let record = registry
            .resolve_interface("java.util.List")
            .expect("List のデフォルト実装が見つかるはず");
        assert_eq!(record.target(), "java.util.ArrayList");
        assert!(matches!(record.origin(), RegistryEntryOrigin::BuiltIn));
    }

    #[test]
    fn override_file_replaces_defaults() {
        let dir = tempfile::tempdir().expect("テンポラリディレクトリ作成");
        let path = dir.path().join("defaults.toml");
        let mut file = fs::File::create(&path).expect("TOML ファイル作成");
        writeln!(
            file,
            "[interfaces]\n\"java.util.List\" = \"java.util.LinkedList\"\n[abstract_classes]\n\"java.util.AbstractList\" = \"java.util.LinkedList\"\n"
        )
        .expect("TOML 書き込み");

        let registry = DefaultImplementationRegistry::load(Some(path.as_path()));
        let record = registry
            .resolve_interface("java.util.List")
            .expect("List のオーバーライドが読み込まれるはず");
        assert_eq!(record.target(), "java.util.LinkedList");
        assert!(matches!(
            record.origin(),
            RegistryEntryOrigin::Override { .. }
        ));

        let abstract_record = registry
            .resolve_abstract("java.util.AbstractList", None)
            .expect("抽象クラスのオーバーライドが読み込まれるはず");
        assert_eq!(abstract_record.target(), "java.util.LinkedList");
    }

    #[test]
    fn invalid_override_reports_error() {
        let dir = tempfile::tempdir().expect("テンポラリディレクトリ作成");
        let path = dir.path().join("broken.toml");
        fs::write(&path, "this is not toml").expect("破損ファイル作成");

        let registry = DefaultImplementationRegistry::load(Some(path.as_path()));
        assert!(
            registry
                .load_errors()
                .iter()
                .any(|err| err.message().contains("解析に失敗")),
            "TOML 解析失敗メッセージが記録されるはず"
        );
    }
}
