use crate::Manifest;
use crate::resolver::{
    DependencyProvider, LinearResolutionProfile, ResolvedDependencies, ResolverAlgorithmKind,
    ResolverError, ResolverOptions, ResolverStrategy, ResolverStrategyInfo, StrategyStability,
    execute_linear_resolution,
};

#[derive(Debug, Default)]
pub struct BreadthFirstStrategy;

impl BreadthFirstStrategy {
    pub fn new() -> Self {
        Self
    }
}

impl ResolverStrategy for BreadthFirstStrategy {
    fn metadata(&self) -> ResolverStrategyInfo {
        ResolverStrategyInfo {
            name: "breadth-first".into(),
            display_name: "Breadth-first".into(),
            description: "幅優先展開で直接依存を優先し、決定論的な順序を保証する解決戦略".into(),
            algorithm: ResolverAlgorithmKind::BreadthFirst,
            stability: StrategyStability::Experimental,
            is_default: false,
            deterministic: true,
            supports_offline: true,
            emits_conflict_reasons: true,
            aliases: vec!["bf".into()],
            conflict_policy: "queue-order".into(),
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
                heuristic: "breadth-first",
                treat_range_as_pending: false,
            },
        )
    }
}
