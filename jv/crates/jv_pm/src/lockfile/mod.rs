use std::collections::{BTreeMap, BTreeSet, HashMap, btree_map::Entry};
use std::fs;
use std::path::Path;

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use thiserror::Error;

use crate::{
    DependencyScope, Manifest, PackageInfo, ResolutionSource, ResolvedDependencies,
    ResolverAlgorithmKind, VersionDecision,
};

/// 現行の`jv.lock`スキーマバージョン。
pub const LOCKFILE_VERSION: u32 = 1;

/// `jv.lock`全体の表現。
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Lockfile {
    pub version: u32,
    pub manifest: LockfileManifestSnapshot,
    #[serde(default)]
    pub packages: Vec<LockedPackage>,
}

/// ロック生成時点のマニフェスト情報。
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LockfileManifestSnapshot {
    pub name: String,
    pub version: String,
    pub hash: String,
    #[serde(default)]
    pub dependencies: Vec<LockfileManifestDependency>,
}

impl LockfileManifestSnapshot {
    pub fn from_manifest(manifest: &Manifest) -> Self {
        let mut dependencies: Vec<LockfileManifestDependency> = manifest
            .package
            .dependencies
            .iter()
            .map(|(name, requirement)| LockfileManifestDependency {
                name: name.clone(),
                requirement: requirement.clone(),
                scope: DependencyScope::Main,
            })
            .collect();

        dependencies.sort_by(|left, right| {
            left.scope
                .cmp(&right.scope)
                .then(left.name.cmp(&right.name))
        });

        let hash = compute_manifest_hash(&manifest.package, &dependencies);

        Self {
            name: manifest.package.name.clone(),
            version: manifest.package.version.clone(),
            hash,
            dependencies,
        }
    }

    pub fn from_resolved(resolved: &ResolvedDependencies) -> Self {
        let mut dependencies: Vec<LockfileManifestDependency> = resolved
            .dependencies
            .iter()
            .map(|dependency| LockfileManifestDependency {
                name: dependency.name.clone(),
                requirement: dependency.requested.clone(),
                scope: dependency.scope,
            })
            .collect();

        dependencies.sort_by(|left, right| {
            left.scope
                .cmp(&right.scope)
                .then(left.name.cmp(&right.name))
        });

        let manifest_name = format!("wrapper-{}", resolved.strategy);
        let manifest_version = algorithm_label(resolved.algorithm).to_string();

        let package_info = PackageInfo {
            name: manifest_name.clone(),
            version: manifest_version.clone(),
            description: None,
            dependencies: HashMap::new(),
        };

        let hash = compute_manifest_hash(&package_info, &dependencies);

        Self {
            name: manifest_name,
            version: manifest_version,
            hash,
            dependencies,
        }
    }
}

/// マニフェストの依存スナップショット。
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LockfileManifestDependency {
    pub name: String,
    pub requirement: String,
    #[serde(default)]
    pub scope: DependencyScope,
}

/// ロック対象のパッケージ情報。
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LockedPackage {
    pub name: String,
    pub version: String,
    #[serde(default)]
    pub source: LockedSource,
    #[serde(default)]
    pub checksum: Option<String>,
    #[serde(default)]
    pub dependencies: Vec<LockedDependency>,
}

/// 依存の参照情報。
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LockedDependency {
    pub name: String,
    pub version: String,
    pub scope: DependencyScope,
}

/// パッケージが取得されたソース。
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum LockedSource {
    Manifest,
    Lockfile,
    Registry,
    Unknown,
}

impl Default for LockedSource {
    fn default() -> Self {
        LockedSource::Unknown
    }
}

impl From<&ResolutionSource> for LockedSource {
    fn from(value: &ResolutionSource) -> Self {
        match value {
            ResolutionSource::Manifest => LockedSource::Manifest,
            ResolutionSource::Lockfile => LockedSource::Lockfile,
            ResolutionSource::Registry => LockedSource::Registry,
        }
    }
}

impl From<ResolutionSource> for LockedSource {
    fn from(value: ResolutionSource) -> Self {
        LockedSource::from(&value)
    }
}

/// Lockfile関連のエラー。
#[derive(Debug, Error)]
pub enum LockfileError {
    #[error("Lockfileの読み書きに失敗しました: {0}")]
    Io(#[from] std::io::Error),
    #[error("Lockfileの解析に失敗しました: {0}")]
    ParseToml(#[from] toml::de::Error),
    #[error("Lockfileの整形に失敗しました: {0}")]
    SerializeToml(#[from] toml::ser::Error),
    #[error("未対応のLockfileバージョン {found} (サポート対象: {supported})")]
    UnsupportedVersion { found: u32, supported: u32 },
    #[error("Lockfileに依存 '{name}' が重複しています")]
    DuplicatePackage { name: String },
    #[error("依存 '{name}' のバージョンが空です")]
    EmptyPackageVersion { name: String },
    #[error("依存 '{package}' -> '{dependency}' のバージョンが空です")]
    EmptyDependencyVersion { package: String, dependency: String },
    #[error("依存 '{name}' の解決結果に確定バージョンが見つかりません")]
    NonExactVersion { name: String },
    #[error("依存 '{name}' の解決結果が複数のバージョンを指しています: {previous} と {next}")]
    ConflictingPackageVersion {
        name: String,
        previous: String,
        next: String,
    },
}

/// バージョン更新の差分。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VersionChange {
    pub previous: String,
    pub current: String,
}

/// 依存要件変更の差分。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependencyRequirementChange {
    pub name: String,
    pub previous_requirement: String,
    pub previous_scope: DependencyScope,
    pub current_requirement: String,
    pub current_scope: DependencyScope,
}

/// マニフェストとLockfileの差分。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LockfileDiff {
    pub hash_mismatch: bool,
    pub previous_hash: String,
    pub current_hash: String,
    pub version_changed: Option<VersionChange>,
    pub added_dependencies: Vec<LockfileManifestDependency>,
    pub removed_dependencies: Vec<LockfileManifestDependency>,
    pub updated_dependencies: Vec<DependencyRequirementChange>,
}

impl Default for LockfileDiff {
    fn default() -> Self {
        Self {
            hash_mismatch: false,
            previous_hash: String::new(),
            current_hash: String::new(),
            version_changed: None,
            added_dependencies: Vec::new(),
            removed_dependencies: Vec::new(),
            updated_dependencies: Vec::new(),
        }
    }
}

impl LockfileDiff {
    pub fn is_empty(&self) -> bool {
        !self.hash_mismatch
            && self.version_changed.is_none()
            && self.added_dependencies.is_empty()
            && self.removed_dependencies.is_empty()
            && self.updated_dependencies.is_empty()
    }

    pub fn render_messages(&self) -> Vec<String> {
        if self.is_empty() {
            return Vec::new();
        }

        let mut lines = Vec::new();

        if self.hash_mismatch {
            lines.push(format!(
                "manifestハッシュが変更されました: {} -> {}",
                self.previous_hash, self.current_hash
            ));
        }

        if let Some(change) = &self.version_changed {
            lines.push(format!(
                "パッケージバージョンが変更されました: {} -> {}",
                change.previous, change.current
            ));
        }

        for added in &self.added_dependencies {
            lines.push(format!(
                "依存が追加されました: {} (要求: {}, scope: {})",
                added.name,
                added.requirement,
                scope_label(added.scope)
            ));
        }

        for removed in &self.removed_dependencies {
            lines.push(format!(
                "依存が削除されました: {} (以前の要求: {}, scope: {})",
                removed.name,
                removed.requirement,
                scope_label(removed.scope)
            ));
        }

        for updated in &self.updated_dependencies {
            lines.push(format!(
                "依存が更新されました: {} (要求: {} -> {}, scope: {} -> {})",
                updated.name,
                updated.previous_requirement,
                updated.current_requirement,
                scope_label(updated.previous_scope),
                scope_label(updated.current_scope)
            ));
        }

        lines
    }
}

/// Lockfile操作のエントリポイント。
pub struct LockfileService;

impl LockfileService {
    pub fn generate(
        manifest: &Manifest,
        resolved: &ResolvedDependencies,
    ) -> Result<Lockfile, LockfileError> {
        let manifest_snapshot = LockfileManifestSnapshot::from_manifest(manifest);
        let mut package_map: BTreeMap<String, LockedPackage> = BTreeMap::new();
        let mut root_dependencies: Vec<LockedDependency> = Vec::new();

        for dependency in &resolved.dependencies {
            let version = match &dependency.decision {
                VersionDecision::Exact(value) => value.clone(),
                _ => {
                    return Err(LockfileError::NonExactVersion {
                        name: dependency.name.clone(),
                    });
                }
            };

            root_dependencies.push(LockedDependency {
                name: dependency.name.clone(),
                version: version.clone(),
                scope: dependency.scope,
            });

            match package_map.entry(dependency.name.clone()) {
                Entry::Vacant(entry) => {
                    entry.insert(LockedPackage {
                        name: dependency.name.clone(),
                        version: version.clone(),
                        source: LockedSource::from(&dependency.source),
                        checksum: None,
                        dependencies: Vec::new(),
                    });
                }
                Entry::Occupied(mut entry) => {
                    let existing = entry.get_mut();
                    if existing.version != version {
                        return Err(LockfileError::ConflictingPackageVersion {
                            name: dependency.name.clone(),
                            previous: existing.version.clone(),
                            next: version,
                        });
                    }

                    if matches!(existing.source, LockedSource::Unknown) {
                        existing.source = LockedSource::from(&dependency.source);
                    }
                }
            }
        }

        root_dependencies.sort_by(|left, right| {
            left.scope
                .cmp(&right.scope)
                .then(left.name.cmp(&right.name))
        });

        let mut packages: Vec<LockedPackage> = package_map.into_values().collect();
        packages.push(LockedPackage {
            name: manifest.package.name.clone(),
            version: manifest.package.version.clone(),
            source: LockedSource::Manifest,
            checksum: None,
            dependencies: root_dependencies,
        });
        packages.sort_by(|left, right| left.name.cmp(&right.name));

        let lockfile = Lockfile {
            version: LOCKFILE_VERSION,
            manifest: manifest_snapshot,
            packages,
        };

        Self::validate(&lockfile)?;
        Ok(lockfile)
    }

    pub fn generate_from_resolved(
        resolved: &ResolvedDependencies,
    ) -> Result<Lockfile, LockfileError> {
        let manifest_snapshot = LockfileManifestSnapshot::from_resolved(resolved);
        let mut package_map: BTreeMap<String, LockedPackage> = BTreeMap::new();
        let mut root_dependencies: Vec<LockedDependency> = Vec::new();

        for dependency in &resolved.dependencies {
            let version = match &dependency.decision {
                VersionDecision::Exact(value) => value.clone(),
                _ => {
                    return Err(LockfileError::NonExactVersion {
                        name: dependency.name.clone(),
                    });
                }
            };

            root_dependencies.push(LockedDependency {
                name: dependency.name.clone(),
                version: version.clone(),
                scope: dependency.scope,
            });

            match package_map.entry(dependency.name.clone()) {
                Entry::Vacant(entry) => {
                    entry.insert(LockedPackage {
                        name: dependency.name.clone(),
                        version: version.clone(),
                        source: LockedSource::from(&dependency.source),
                        checksum: None,
                        dependencies: Vec::new(),
                    });
                }
                Entry::Occupied(mut entry) => {
                    let existing = entry.get_mut();
                    if existing.version != version {
                        return Err(LockfileError::ConflictingPackageVersion {
                            name: dependency.name.clone(),
                            previous: existing.version.clone(),
                            next: version,
                        });
                    }

                    if matches!(existing.source, LockedSource::Unknown) {
                        existing.source = LockedSource::from(&dependency.source);
                    }
                }
            }
        }

        root_dependencies.sort_by(|left, right| {
            left.scope
                .cmp(&right.scope)
                .then(left.name.cmp(&right.name))
        });

        let mut packages: Vec<LockedPackage> = package_map.into_values().collect();
        packages.push(LockedPackage {
            name: manifest_snapshot.name.clone(),
            version: manifest_snapshot.version.clone(),
            source: LockedSource::Lockfile,
            checksum: None,
            dependencies: root_dependencies,
        });
        packages.sort_by(|left, right| left.name.cmp(&right.name));

        let lockfile = Lockfile {
            version: LOCKFILE_VERSION,
            manifest: manifest_snapshot,
            packages,
        };

        Self::validate(&lockfile)?;
        Ok(lockfile)
    }

    pub fn save(path: impl AsRef<Path>, lockfile: &Lockfile) -> Result<(), LockfileError> {
        Self::validate(lockfile)?;
        let toml = toml::to_string_pretty(lockfile)?;
        let path = path.as_ref();
        if let Some(parent) = path.parent() {
            if !parent.as_os_str().is_empty() {
                fs::create_dir_all(parent)?;
            }
        }
        fs::write(path, toml)?;
        Ok(())
    }

    pub fn load(path: impl AsRef<Path>) -> Result<Lockfile, LockfileError> {
        let content = fs::read_to_string(path)?;
        let lockfile: Lockfile = toml::from_str(&content)?;
        Self::validate(&lockfile)?;
        Ok(lockfile)
    }

    pub fn validate(lockfile: &Lockfile) -> Result<(), LockfileError> {
        if lockfile.version != LOCKFILE_VERSION {
            return Err(LockfileError::UnsupportedVersion {
                found: lockfile.version,
                supported: LOCKFILE_VERSION,
            });
        }

        let mut seen = BTreeSet::new();
        for package in &lockfile.packages {
            if !seen.insert(package.name.clone()) {
                return Err(LockfileError::DuplicatePackage {
                    name: package.name.clone(),
                });
            }

            if package.version.trim().is_empty() {
                return Err(LockfileError::EmptyPackageVersion {
                    name: package.name.clone(),
                });
            }

            for dependency in &package.dependencies {
                if dependency.version.trim().is_empty() {
                    return Err(LockfileError::EmptyDependencyVersion {
                        package: package.name.clone(),
                        dependency: dependency.name.clone(),
                    });
                }
            }
        }

        Ok(())
    }

    pub fn diff(manifest: &Manifest, lockfile: &Lockfile) -> LockfileDiff {
        let current_snapshot = LockfileManifestSnapshot::from_manifest(manifest);
        let mut diff = LockfileDiff::default();
        diff.previous_hash = lockfile.manifest.hash.clone();
        diff.current_hash = current_snapshot.hash.clone();
        diff.hash_mismatch = diff.previous_hash != diff.current_hash;

        if !diff.hash_mismatch {
            return diff;
        }

        if lockfile.manifest.version != current_snapshot.version {
            diff.version_changed = Some(VersionChange {
                previous: lockfile.manifest.version.clone(),
                current: current_snapshot.version.clone(),
            });
        }

        let mut previous: BTreeMap<&str, &LockfileManifestDependency> = BTreeMap::new();
        for dependency in &lockfile.manifest.dependencies {
            previous.insert(dependency.name.as_str(), dependency);
        }

        let mut current: BTreeMap<&str, &LockfileManifestDependency> = BTreeMap::new();
        for dependency in &current_snapshot.dependencies {
            current.insert(dependency.name.as_str(), dependency);
        }

        for (name, dependency) in &current {
            match previous.remove(name) {
                Some(original) => {
                    if original.requirement != dependency.requirement
                        || original.scope != dependency.scope
                    {
                        diff.updated_dependencies.push(DependencyRequirementChange {
                            name: (*name).to_string(),
                            previous_requirement: original.requirement.clone(),
                            previous_scope: original.scope,
                            current_requirement: dependency.requirement.clone(),
                            current_scope: dependency.scope,
                        });
                    }
                }
                None => diff.added_dependencies.push((*dependency).clone()),
            }
        }

        for leftover in previous.into_values() {
            diff.removed_dependencies.push(leftover.clone());
        }

        diff
    }
}

fn compute_manifest_hash(
    package: &PackageInfo,
    dependencies: &[LockfileManifestDependency],
) -> String {
    let mut hasher = Sha256::new();
    hasher.update(package.name.as_bytes());
    hasher.update(b"\n");
    hasher.update(package.version.as_bytes());
    hasher.update(b"\n");

    for dependency in dependencies {
        hasher.update(scope_label(dependency.scope).as_bytes());
        hasher.update(b"/");
        hasher.update(dependency.name.as_bytes());
        hasher.update(b"=");
        hasher.update(dependency.requirement.as_bytes());
        hasher.update(b"\n");
    }

    let digest = hasher.finalize();
    format!("{:x}", digest)
}

fn scope_label(scope: DependencyScope) -> &'static str {
    match scope {
        DependencyScope::Main => "main",
        DependencyScope::Dev => "dev",
        DependencyScope::Build => "build",
    }
}

fn algorithm_label(algorithm: ResolverAlgorithmKind) -> &'static str {
    match algorithm {
        ResolverAlgorithmKind::PubGrub => "pubgrub",
        ResolverAlgorithmKind::BreadthFirst => "breadth-first",
        ResolverAlgorithmKind::MavenCompat => "maven-compat",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    use tempfile::tempdir;

    use crate::{
        LoggingConfig, RepositorySection, ResolutionSource, ResolutionStats, ResolvedDependency,
        ResolverAlgorithmKind,
    };

    fn sample_manifest() -> Manifest {
        Manifest {
            package: PackageInfo {
                name: "demo".to_string(),
                version: "0.1.0".to_string(),
                description: None,
                dependencies: HashMap::from([
                    ("serde".to_string(), "^1.0.188".to_string()),
                    ("tokio".to_string(), "1.37".to_string()),
                ]),
            },
            project: Default::default(),
            repositories: RepositorySection::default(),
            mirrors: Vec::new(),
            build: None,
            logging: LoggingConfig::default(),
            maven: Default::default(),
        }
    }

    fn sample_resolved() -> ResolvedDependencies {
        ResolvedDependencies {
            strategy: "pubgrub".to_string(),
            algorithm: ResolverAlgorithmKind::PubGrub,
            dependencies: vec![
                ResolvedDependency {
                    name: "serde".to_string(),
                    requested: "^1.0.188".to_string(),
                    decision: VersionDecision::Exact("1.0.188".to_string()),
                    scope: DependencyScope::Main,
                    source: ResolutionSource::Registry,
                    local_artifact: None,
                },
                ResolvedDependency {
                    name: "tokio".to_string(),
                    requested: "1.37".to_string(),
                    decision: VersionDecision::Exact("1.37.0".to_string()),
                    scope: DependencyScope::Dev,
                    source: ResolutionSource::Registry,
                    local_artifact: None,
                },
            ],
            diagnostics: Vec::new(),
            stats: ResolutionStats {
                elapsed_ms: 10,
                total_dependencies: 2,
                decided_dependencies: 2,
            },
        }
    }

    #[test]
    fn generate_lockfile_includes_root_package() {
        let manifest = sample_manifest();
        let resolved = sample_resolved();

        let lockfile = LockfileService::generate(&manifest, &resolved).expect("generate lockfile");

        assert_eq!(lockfile.version, LOCKFILE_VERSION);
        assert_eq!(lockfile.manifest.name, "demo");
        assert_eq!(lockfile.manifest.version, "0.1.0");
        assert!(!lockfile.manifest.hash.is_empty());

        let root = lockfile
            .packages
            .iter()
            .find(|package| matches!(package.source, LockedSource::Manifest))
            .expect("root package entry");

        assert_eq!(root.name, "demo");
        assert_eq!(root.dependencies.len(), 2);
        assert_eq!(root.dependencies[0].name, "serde");
        assert_eq!(root.dependencies[0].scope, DependencyScope::Main);
        assert_eq!(root.dependencies[1].name, "tokio");
        assert_eq!(root.dependencies[1].scope, DependencyScope::Dev);

        let serde_pkg = lockfile
            .packages
            .iter()
            .find(|package| package.name == "serde")
            .expect("serde package entry");
        assert_eq!(serde_pkg.version, "1.0.188");
    }

    #[test]
    fn diff_detects_manifest_changes() {
        let mut manifest = sample_manifest();
        let resolved = sample_resolved();
        let lockfile = LockfileService::generate(&manifest, &resolved).unwrap();

        manifest.package.version = "0.2.0".to_string();
        manifest.package.dependencies.remove("tokio");
        manifest
            .package
            .dependencies
            .insert("serde".to_string(), "^1.0.190".to_string());
        manifest
            .package
            .dependencies
            .insert("reqwest".to_string(), "0.12".to_string());

        let diff = LockfileService::diff(&manifest, &lockfile);

        assert!(diff.hash_mismatch);
        assert_eq!(diff.previous_hash.len(), 64);
        assert_eq!(diff.current_hash.len(), 64);

        let version_change = diff.version_changed.as_ref().expect("version change");
        assert_eq!(version_change.previous, "0.1.0");
        assert_eq!(version_change.current, "0.2.0");

        assert_eq!(
            diff.added_dependencies
                .iter()
                .map(|dep| dep.name.as_str())
                .collect::<Vec<_>>(),
            vec!["reqwest"]
        );

        assert_eq!(
            diff.removed_dependencies
                .iter()
                .map(|dep| dep.name.as_str())
                .collect::<Vec<_>>(),
            vec!["tokio"]
        );

        assert!(
            diff.updated_dependencies
                .iter()
                .any(|change| change.name == "serde" && change.current_requirement == "^1.0.190")
        );

        let messages = diff.render_messages();
        assert!(!messages.is_empty());
    }

    #[test]
    fn save_and_load_roundtrip() {
        let manifest = sample_manifest();
        let resolved = sample_resolved();
        let lockfile = LockfileService::generate(&manifest, &resolved).unwrap();

        let temp = tempdir().unwrap();
        let path = temp.path().join("jv.lock");

        LockfileService::save(&path, &lockfile).unwrap();
        let loaded = LockfileService::load(&path).unwrap();

        assert_eq!(lockfile, loaded);
    }

    #[test]
    fn validate_rejects_unsupported_version() {
        let manifest = sample_manifest();
        let resolved = sample_resolved();
        let mut lockfile = LockfileService::generate(&manifest, &resolved).unwrap();
        lockfile.version = LOCKFILE_VERSION + 1;

        let error = LockfileService::validate(&lockfile).unwrap_err();
        match error {
            LockfileError::UnsupportedVersion { found, supported } => {
                assert_eq!(found, LOCKFILE_VERSION + 1);
                assert_eq!(supported, LOCKFILE_VERSION);
            }
            _ => panic!("unexpected error: {error}"),
        }
    }

    #[test]
    fn generate_fails_on_non_exact_versions() {
        let manifest = sample_manifest();
        let mut resolved = sample_resolved();
        if let Some(first) = resolved.dependencies.first_mut() {
            first.decision = VersionDecision::Range("^1".to_string());
        }

        let error = LockfileService::generate(&manifest, &resolved).unwrap_err();
        matches!(error, LockfileError::NonExactVersion { .. });
    }

    #[test]
    fn generate_from_resolved_includes_root_package() {
        let resolved = sample_resolved();
        let lockfile =
            LockfileService::generate_from_resolved(&resolved).expect("generate lockfile");

        assert_eq!(lockfile.manifest.name, "wrapper-pubgrub");
        assert_eq!(lockfile.manifest.version, "pubgrub");

        let root = lockfile
            .packages
            .iter()
            .find(|package| package.name == lockfile.manifest.name)
            .expect("root package entry");

        assert_eq!(root.dependencies.len(), 2);
        assert_eq!(root.dependencies[0].name, "serde");
        assert_eq!(root.dependencies[0].scope, DependencyScope::Main);
        assert_eq!(root.dependencies[1].name, "tokio");
        assert_eq!(root.dependencies[1].scope, DependencyScope::Dev);

        let serde_pkg = lockfile
            .packages
            .iter()
            .find(|package| package.name == "serde")
            .expect("serde package entry");
        assert_eq!(serde_pkg.version, "1.0.188");
    }

    #[test]
    fn generate_from_resolved_fails_on_non_exact_versions() {
        let mut resolved = sample_resolved();
        if let Some(first) = resolved.dependencies.first_mut() {
            first.decision = VersionDecision::Range("^1".to_string());
        }

        let error = LockfileService::generate_from_resolved(&resolved).unwrap_err();
        matches!(error, LockfileError::NonExactVersion { .. });
    }
}
