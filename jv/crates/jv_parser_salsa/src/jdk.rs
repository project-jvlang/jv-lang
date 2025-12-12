use jv_build::JavaTarget;
use jv_build::metadata::{
    BuildContext, IndexError, SymbolIndex, SymbolIndexBuilder, SymbolIndexCache,
};
use std::env;
use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock};
use thiserror::Error;

/// デフォルトの JDK モジュールイメージの相対パス。
pub const DEFAULT_JDK_MODULES_RELATIVE: &str = "toolchains/jdk25/lib/modules";
/// JDK モジュールイメージのパスを上書きする環境変数。
pub const MODULES_ENV: &str = "JV_BENCH_JDK_MODULES";
/// JDK ロードを明示的にスキップする環境変数。
pub const SKIP_ENV: &str = "JV_BENCH_SKIP_JDK_MODULES";

/// JDK モジュールのロードモード。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JdkLoadMode {
    /// 環境変数でスキップ指定がない限りロードする。
    Auto,
    /// 明示的にロードをスキップする。
    Skip,
}

/// ロードされた JDK モジュールのメタデータ。
#[derive(Debug, Clone)]
pub struct JdkModules {
    symbol_index: Arc<SymbolIndex>,
    modules_path: PathBuf,
}

impl JdkModules {
    pub fn symbol_index(&self) -> &SymbolIndex {
        &self.symbol_index
    }

    pub fn symbol_index_arc(&self) -> Arc<SymbolIndex> {
        self.symbol_index.clone()
    }

    pub fn modules_path(&self) -> &Path {
        &self.modules_path
    }
}

/// JDK モジュールロード時のエラー。
#[derive(Debug, Error)]
pub enum JdkLoadError {
    #[error(
        "JDK modules image not found at {path:?}. Set {MODULES_ENV}=<path>/lib/modules or \
         provision {DEFAULT_JDK_MODULES_RELATIVE}."
    )]
    Missing { path: PathBuf },
    #[error("JDK modules path {path:?} is not a file; expected a lib/modules jimage.")]
    NotAFile { path: PathBuf },
    #[error("Failed to index JDK modules from {path:?}: {source}")]
    Index {
        path: PathBuf,
        #[source]
        source: IndexError,
    },
}

/// 環境に基づくデフォルトのロードモードを返す。
pub fn default_load_mode() -> JdkLoadMode {
    if env::var_os(SKIP_ENV).is_some() {
        JdkLoadMode::Skip
    } else {
        JdkLoadMode::Auto
    }
}

/// デフォルトの JDK モジュールパスを返す。
pub fn default_modules_path() -> PathBuf {
    workspace_root().join(DEFAULT_JDK_MODULES_RELATIVE)
}

/// 実際に使用する JDK モジュールパスを解決する。
pub fn resolve_modules_path() -> Result<PathBuf, JdkLoadError> {
    let base = workspace_root();
    let candidates = if let Ok(value) = env::var(MODULES_ENV) {
        candidate_paths(PathBuf::from(value), &base)
    } else {
        candidate_paths(PathBuf::from(DEFAULT_JDK_MODULES_RELATIVE), &base)
    };

    for path in &candidates {
        if !path.exists() {
            continue;
        }
        if !path.is_file() {
            return Err(JdkLoadError::NotAFile { path: path.clone() });
        }
        return Ok(path.clone());
    }

    let first = candidates
        .into_iter()
        .next()
        .unwrap_or_else(default_modules_path);
    Err(JdkLoadError::Missing { path: first })
}

/// JDK モジュールをプリロードし、同プロセス内で共有する。
pub fn preload_jdk_modules(mode: JdkLoadMode) -> Result<Option<Arc<JdkModules>>, JdkLoadError> {
    if matches!(mode, JdkLoadMode::Skip) {
        return Ok(None);
    }
    if env::var_os(SKIP_ENV).is_some() {
        return Ok(None);
    }

    if let Some(cached) = JDK_MODULES.get() {
        return Ok(Some(cached.clone()));
    }

    let modules = Arc::new(load_jdk_modules()?);
    let _ = JDK_MODULES.set(modules.clone());
    Ok(Some(modules))
}

fn load_jdk_modules() -> Result<JdkModules, JdkLoadError> {
    let modules_path = resolve_modules_path()?;
    let context = BuildContext {
        target: JavaTarget::Java25,
        java_home: modules_path
            .parent()
            .and_then(|p| p.parent())
            .map(PathBuf::from),
        classpath: Vec::new(),
        module_path: vec![modules_path.clone()],
    };

    let cache = SymbolIndexCache::with_default_location();
    let builder = SymbolIndexBuilder::new(&context);
    let index = builder
        .build_with_cache(&cache)
        .map_err(|source| JdkLoadError::Index {
            path: modules_path.clone(),
            source,
        })?;

    Ok(JdkModules {
        symbol_index: Arc::new(index),
        modules_path,
    })
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")))
}

static JDK_MODULES: OnceLock<Arc<JdkModules>> = OnceLock::new();

fn candidate_paths(candidate: PathBuf, base: &Path) -> Vec<PathBuf> {
    if candidate.is_absolute() {
        return vec![candidate];
    }

    let mut paths = vec![base.join(&candidate)];
    if let Some(parent) = base.parent() {
        let alt = parent.join(candidate);
        if alt != paths[0] {
            paths.push(alt);
        }
    }
    paths
}
