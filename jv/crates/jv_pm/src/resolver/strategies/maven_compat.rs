use crate::Manifest;
use crate::resolver::{
    DependencyProvider, LinearResolutionProfile, ResolvedDependencies, ResolverAlgorithmKind,
    ResolverError, ResolverOptions, ResolverStrategy, ResolverStrategyInfo, StrategyStability,
    execute_linear_resolution,
};

#[derive(Debug, Default)]
pub struct MavenCompatStrategy;

impl MavenCompatStrategy {
    pub fn new() -> Self {
        Self
    }
}

impl ResolverStrategy for MavenCompatStrategy {
    fn metadata(&self) -> ResolverStrategyInfo {
        ResolverStrategyInfo {
            name: "maven".into(),
            display_name: "Maven-compatible".into(),
            description: "Mavenのバージョン範囲評価と互換順序を模倣するフォールバック戦略".into(),
            algorithm: ResolverAlgorithmKind::MavenCompat,
            stability: StrategyStability::Experimental,
            is_default: false,
            deterministic: true,
            supports_offline: false,
            emits_conflict_reasons: false,
            aliases: vec!["maven-compat".into(), "legacy".into()],
            conflict_policy: "first-wins".into(),
        }
    }

    fn resolve(
        &self,
        manifest: &Manifest,
        provider: &dyn DependencyProvider,
        options: &ResolverOptions,
    ) -> Result<ResolvedDependencies, ResolverError> {
        let info = self.metadata();
        execute_linear_resolution(
            manifest,
            provider,
            options,
            &info,
            LinearResolutionProfile {
                heuristic: "nearest-wins",
                treat_range_as_pending: true,
            },
        )
    }
}
