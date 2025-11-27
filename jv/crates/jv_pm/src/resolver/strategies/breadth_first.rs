use super::maven_39::hydrate_resolved_dependencies_with_jars;
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
        let mut resolved = execute_linear_resolution(
            manifest,
            provider,
            options,
            &info,
            LinearResolutionProfile {
                heuristic: "breadth-first",
                treat_range_as_pending: false,
            },
        )?;

        if let Some(context) = options.maven_context.as_ref() {
            hydrate_resolved_dependencies_with_jars(None, context, &mut resolved)?;
        } else {
            tracing::warn!(
                "MavenResolverContext が設定されていないため Jar ダウンロードをスキップします"
            );
        }

        Ok(resolved)
    }
}
