//! Resolver subsystem for jv_pm.
//!
//! This module provides the resolver dispatcher, strategy abstraction, and
//! supporting data structures required to convert a `Manifest` into
//! deterministically ordered `ResolvedDependencies`.

use crate::{Manifest, MavenMirrorConfig, MavenRepositoryConfig, PackageInfo};
use indexmap::IndexMap;
use semver::{Version, VersionReq};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;
use thiserror::Error;

pub mod strategies;

use strategies::{BreadthFirstStrategy, MavenCompatStrategy, PubGrubStrategy};

/// Dispatcher that keeps track of every available resolver strategy and
/// exposes a single entry point for manifest resolution.
#[derive(Clone)]
pub struct ResolverDispatcher {
    registry: IndexMap<String, StrategyEntry>,
    aliases: HashMap<String, String>,
    default_strategy: String,
}

#[derive(Clone)]
struct StrategyEntry {
    strategy: Arc<dyn ResolverStrategy>,
    info: ResolverStrategyInfo,
}

impl ResolverDispatcher {
    /// Creates a dispatcher pre-populated with the built-in strategies
    /// described in the Phase 2 design document.
    pub fn with_default_strategies() -> Self {
        let mut dispatcher = Self {
            registry: IndexMap::new(),
            aliases: HashMap::new(),
            default_strategy: String::new(),
        };

        dispatcher.register_strategy(Arc::new(PubGrubStrategy::new()));
        dispatcher.register_strategy(Arc::new(BreadthFirstStrategy::new()));
        dispatcher.register_strategy(Arc::new(MavenCompatStrategy::new()));
        dispatcher
    }

    /// Registers a new strategy. If the strategy declares `is_default`, it will
    /// become the dispatcher default.
    pub fn register_strategy(&mut self, strategy: Arc<dyn ResolverStrategy>) {
        let info = strategy.metadata();
        let canonical = info.name.clone();
        let entry = StrategyEntry {
            strategy,
            info: info.clone(),
        };

        self.registry.insert(canonical.clone(), entry);
        self.aliases.insert(canonical.clone(), canonical.clone());
        for alias in &info.aliases {
            self.aliases.insert(alias.clone(), canonical.clone());
        }

        if info.is_default {
            self.default_strategy = canonical;
        } else if self.default_strategy.is_empty() {
            self.default_strategy = canonical;
        }
    }

    /// Returns the canonical strategy name considered as default.
    pub fn default_strategy(&self) -> &str {
        &self.default_strategy
    }

    /// Lists metadata for every registered strategy.
    pub fn list_strategies(&self) -> Vec<ResolverStrategyInfo> {
        let mut entries = self
            .registry
            .values()
            .map(|entry| entry.info.clone())
            .collect::<Vec<_>>();
        entries.sort_by(|a, b| a.name.cmp(&b.name));
        entries
    }

    /// Retrieves metadata for the requested strategy name or alias.
    pub fn strategy_info(&self, name: &str) -> Option<ResolverStrategyInfo> {
        self.resolve_strategy_key(name)
            .ok()
            .and_then(|canonical| self.registry.get(canonical))
            .map(|entry| entry.info.clone())
    }

    /// Resolves the provided manifest using the requested (or default)
    /// strategy. The resulting dependencies are always stable-sorted.
    pub fn resolve_manifest(
        &self,
        manifest: &Manifest,
        options: ResolverOptions,
    ) -> Result<ResolvedDependencies, ResolverError> {
        let requested = options
            .strategy
            .as_deref()
            .unwrap_or_else(|| self.default_strategy());
        let canonical = self.resolve_strategy_key(requested)?;
        let entry = self
            .registry
            .get(canonical)
            .ok_or_else(|| ResolverError::UnknownStrategy(requested.to_string()))?;

        let provider = ManifestDependencyProvider::new(manifest);
        let mut resolved = entry.strategy.resolve(manifest, &provider, &options)?;
        resolved.strategy = entry.info.name.clone();
        resolved.algorithm = entry.info.algorithm;
        resolved.ensure_stable_order();
        Ok(resolved)
    }

    fn resolve_strategy_key<'a>(&'a self, requested: &str) -> Result<&'a str, ResolverError> {
        if let Some(entry) = self.registry.get_key_value(requested) {
            return Ok(entry.0.as_str());
        }

        self.aliases
            .get(requested)
            .map(|value| value.as_str())
            .ok_or_else(|| ResolverError::UnknownStrategy(requested.to_string()))
    }
}

impl Default for ResolverDispatcher {
    fn default() -> Self {
        Self::with_default_strategies()
    }
}

/// Metadata describing a resolver strategy.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ResolverStrategyInfo {
    pub name: String,
    pub display_name: String,
    pub description: String,
    pub algorithm: ResolverAlgorithmKind,
    pub stability: StrategyStability,
    pub is_default: bool,
    pub deterministic: bool,
    pub supports_offline: bool,
    pub emits_conflict_reasons: bool,
    pub aliases: Vec<String>,
    pub conflict_policy: String,
}

/// High-level algorithm type for a strategy. The enum is serde-friendly so the
/// CLI can emit JSON metadata without extra adapters.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum ResolverAlgorithmKind {
    PubGrub,
    BreadthFirst,
    MavenCompat,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum StrategyStability {
    Experimental,
    Stable,
}

/// Runtime configuration switches passed to the resolver.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ResolverOptions {
    pub strategy: Option<String>,
    pub allow_prerelease: bool,
    pub include_dev_dependencies: bool,
    pub max_depth: usize,
    /// Optional Maven-specific context required for strategies that perform
    /// artifact downloads or pom.xml resolution. This is populated by the CLI
    /// layer and skipped during serialization to avoid leaking filesystem
    /// details into persisted configs.
    #[serde(skip)]
    pub maven_context: Option<MavenResolverContext>,
}

/// Maven resolution context shared across strategies that need repository
/// and filesystem information to download artifacts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MavenResolverContext {
    pub project_root: PathBuf,
    pub local_repository: PathBuf,
    pub repositories: Vec<MavenRepositoryConfig>,
    pub mirrors: Vec<MavenMirrorConfig>,
}

impl ResolverOptions {
    pub fn with_strategy(mut self, strategy: impl Into<String>) -> Self {
        self.strategy = Some(strategy.into());
        self
    }

    pub fn with_maven_context(mut self, context: MavenResolverContext) -> Self {
        self.maven_context = Some(context);
        self
    }
}

impl Default for ResolverOptions {
    fn default() -> Self {
        Self {
            strategy: None,
            allow_prerelease: false,
            include_dev_dependencies: false,
            max_depth: 64,
            maven_context: None,
        }
    }
}

/// Dependency as requested by the manifest.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RequestedDependency {
    pub name: String,
    pub requirement: String,
    pub scope: DependencyScope,
}

/// Dependency scope (runtime/main, dev/test, or build time).
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Default)]
#[serde(rename_all = "snake_case")]
pub enum DependencyScope {
    #[default]
    Main,
    Dev,
    Build,
}

/// Final resolution artifact returned to the caller.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct ResolvedDependencies {
    pub strategy: String,
    pub algorithm: ResolverAlgorithmKind,
    pub dependencies: Vec<ResolvedDependency>,
    pub diagnostics: Vec<ResolutionDiagnostic>,
    pub stats: ResolutionStats,
}

impl ResolvedDependencies {
    fn ensure_stable_order(&mut self) {
        self.dependencies.sort_by(|left, right| {
            left.name
                .cmp(&right.name)
                .then(left.decision.order_tag().cmp(&right.decision.order_tag()))
                .then(left.scope.cmp(&right.scope))
        });
    }
}

/// Information about an individual resolved dependency.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ResolvedDependency {
    pub name: String,
    pub requested: String,
    pub decision: VersionDecision,
    pub scope: DependencyScope,
    pub source: ResolutionSource,
    /// Absolute path to the locally cached artifact (e.g., `.jv/repository/...`).
    /// Strategies that do not download artifacts should leave this as `None`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub local_artifact: Option<String>,
}

/// How a dependency version was decided.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum VersionDecision {
    Exact(String),
    Range(String),
    Unspecified,
}

impl VersionDecision {
    fn order_tag(&self) -> (u8, &str) {
        match self {
            VersionDecision::Exact(value) => (0, value.as_str()),
            VersionDecision::Range(value) => (1, value.as_str()),
            VersionDecision::Unspecified => (2, ""),
        }
    }

    fn is_exact(&self) -> bool {
        matches!(self, VersionDecision::Exact(_))
    }
}

/// Where the resolved dependency information originated from.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum ResolutionSource {
    Manifest,
    Lockfile,
    Registry,
}

/// Diagnostic message emitted during resolution.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct ResolutionDiagnostic {
    pub code: &'static str,
    pub message: String,
    pub level: ResolutionDiagnosticLevel,
}

impl ResolutionDiagnostic {
    pub fn info(code: &'static str, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            level: ResolutionDiagnosticLevel::Info,
        }
    }

    pub fn warning(code: &'static str, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            level: ResolutionDiagnosticLevel::Warning,
        }
    }

    pub fn error(code: &'static str, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            level: ResolutionDiagnosticLevel::Error,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum ResolutionDiagnosticLevel {
    Info,
    Warning,
    Error,
}

/// Statistics describing a resolver run.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ResolutionStats {
    pub elapsed_ms: u128,
    pub total_dependencies: usize,
    pub decided_dependencies: usize,
}

impl ResolutionStats {
    fn new(elapsed_ms: u128, total: usize, decided: usize) -> Self {
        Self {
            elapsed_ms,
            total_dependencies: total,
            decided_dependencies: decided,
        }
    }
}

/// Resolver failure conditions surfaced to the caller.
#[derive(Debug, Error)]
pub enum ResolverError {
    #[error("Unknown resolver strategy '{0}'")]
    UnknownStrategy(String),
    #[error("Manifest declares no dependencies to resolve")]
    EmptyDependencies,
    #[error("Dependency '{dependency}' has an invalid requirement: {requirement}")]
    InvalidRequirement {
        dependency: String,
        requirement: String,
    },
    #[error("Resolver strategy reported a conflict for '{dependency}': {details}")]
    Conflict { dependency: String, details: String },
}

/// Trait implemented by every resolver strategy.
pub trait ResolverStrategy: Send + Sync {
    fn metadata(&self) -> ResolverStrategyInfo;

    fn resolve(
        &self,
        manifest: &Manifest,
        provider: &dyn DependencyProvider,
        options: &ResolverOptions,
    ) -> Result<ResolvedDependencies, ResolverError>;
}

/// Abstraction for obtaining dependency information. The manifest provider is
/// sufficient for Phase 2, but the abstraction allows the resolver to switch to
/// alternative sources (lockfile, registry) without touching strategy code.
pub trait DependencyProvider {
    fn root_package(&self) -> &PackageInfo;
    fn direct_dependencies(&self) -> Vec<RequestedDependency>;
}

/// Dependency provider backed by an in-memory `Manifest`.
pub struct ManifestDependencyProvider<'a> {
    manifest: &'a Manifest,
}

impl<'a> ManifestDependencyProvider<'a> {
    pub fn new(manifest: &'a Manifest) -> Self {
        Self { manifest }
    }
}

impl<'a> DependencyProvider for ManifestDependencyProvider<'a> {
    fn root_package(&self) -> &PackageInfo {
        &self.manifest.package
    }

    fn direct_dependencies(&self) -> Vec<RequestedDependency> {
        self.manifest
            .package
            .dependencies
            .iter()
            .map(|(name, requirement)| RequestedDependency {
                name: name.clone(),
                requirement: requirement.clone(),
                scope: DependencyScope::Main,
            })
            .collect()
    }
}

/// Execution profile describing how a simple linear resolution should behave.
#[derive(Debug, Clone)]
pub struct LinearResolutionProfile {
    pub heuristic: &'static str,
    pub treat_range_as_pending: bool,
}

/// Shared helper used by built-in strategies. The implementation keeps the
/// surface area intentionally small so that more sophisticated resolvers can
/// replace it without modifying the dispatcher or CLI.
pub(crate) fn execute_linear_resolution(
    manifest: &Manifest,
    provider: &dyn DependencyProvider,
    options: &ResolverOptions,
    info: &ResolverStrategyInfo,
    profile: LinearResolutionProfile,
) -> Result<ResolvedDependencies, ResolverError> {
    let start = Instant::now();
    let mut dependencies = provider.direct_dependencies();
    if dependencies.is_empty() {
        return Err(ResolverError::EmptyDependencies);
    }

    dependencies.sort_by(|a, b| a.name.cmp(&b.name));

    let mut resolved = Vec::with_capacity(dependencies.len());
    let mut diagnostics = vec![ResolutionDiagnostic::info(
        "JVPM1100",
        format!(
            "{strategy} 戦略: {heuristic} ヒューリスティックを使用 (root={root}, dev={dev})",
            strategy = info.name,
            heuristic = profile.heuristic,
            root = manifest.package.name,
            dev = options.include_dev_dependencies,
        ),
    )];

    for dependency in dependencies {
        let decision = classify_requirement(&dependency.name, &dependency.requirement)?;
        resolved.push(ResolvedDependency {
            name: dependency.name.clone(),
            requested: dependency.requirement.clone(),
            decision,
            scope: dependency.scope,
            source: ResolutionSource::Manifest,
            local_artifact: None,
        });
    }

    if profile.treat_range_as_pending {
        for dependency in &mut resolved {
            if matches!(dependency.decision, VersionDecision::Range(_)) {
                diagnostics.push(ResolutionDiagnostic::warning(
                    "JVPM1201",
                    format!(
                        "{name} のバージョン範囲 {req} は解決保留です (strategy={strategy})",
                        name = dependency.name,
                        req = dependency.requested,
                        strategy = info.name
                    ),
                ));
                dependency.decision = VersionDecision::Unspecified;
            }
        }
    }

    let elapsed_ms = start.elapsed().as_millis();
    let decided = resolved
        .iter()
        .filter(|dependency| dependency.decision.is_exact())
        .count();
    let total = resolved.len();

    Ok(ResolvedDependencies {
        strategy: info.name.clone(),
        algorithm: info.algorithm,
        dependencies: resolved,
        diagnostics,
        stats: ResolutionStats::new(elapsed_ms, total, decided),
    })
}

fn classify_requirement(name: &str, requirement: &str) -> Result<VersionDecision, ResolverError> {
    let trimmed = requirement.trim();
    if trimmed.is_empty() {
        return Err(ResolverError::InvalidRequirement {
            dependency: name.to_string(),
            requirement: requirement.to_string(),
        });
    }

    let exact_candidate = trimmed.trim_start_matches('=').trim();
    if let Ok(version) = Version::parse(exact_candidate) {
        return Ok(VersionDecision::Exact(version.to_string()));
    }

    if VersionReq::parse(trimmed).is_ok() {
        return Ok(VersionDecision::Range(trimmed.to_string()));
    }

    Err(ResolverError::InvalidRequirement {
        dependency: name.to_string(),
        requirement: requirement.to_string(),
    })
}
