use crate::Manifest;
use crate::resolver::{
    DependencyProvider, LinearResolutionProfile, ResolvedDependencies, ResolverAlgorithmKind,
    ResolverError, ResolverOptions, ResolverStrategy, ResolverStrategyInfo, StrategyStability,
    execute_linear_resolution,
};

#[derive(Debug, Default)]
pub struct PubGrubStrategy;

impl PubGrubStrategy {
    pub fn new() -> Self {
        Self
    }
}

impl ResolverStrategy for PubGrubStrategy {
    fn metadata(&self) -> ResolverStrategyInfo {
        ResolverStrategyInfo {
            name: "pubgrub".into(),
            display_name: "PubGrub".into(),
            description: "競合駆動節学習を備えた決定論的なPubGrubベースの解決戦略".into(),
            algorithm: ResolverAlgorithmKind::PubGrub,
            stability: StrategyStability::Stable,
            is_default: true,
            deterministic: true,
            supports_offline: false,
            emits_conflict_reasons: true,
            aliases: vec!["pg".into(), "default".into()],
            conflict_policy: "learned-clauses".into(),
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
                heuristic: "constraint-first",
                treat_range_as_pending: false,
            },
        )
    }
}
