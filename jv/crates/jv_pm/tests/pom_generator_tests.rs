use std::collections::HashMap;

use jv_pm::{
    DependencyScope, LoggingConfig, Manifest, MavenProjectMetadata, PackageInfo, PomGenerator,
    ProjectSection, RepositorySection, ResolutionSource, ResolutionStats, ResolvedDependencies,
    ResolvedDependency, ResolverAlgorithmKind, VersionDecision,
};

#[test]
fn pom_generator_outputs_manifest_dependencies_only() {
    let manifest = Manifest {
        package: PackageInfo {
            name: "example-app".to_string(),
            version: "0.1.0".to_string(),
            description: None,
            dependencies: HashMap::from([(
                "org.example:root-lib".to_string(),
                "1.0.0".to_string(),
            )]),
        },
        project: ProjectSection::default(),
        repositories: RepositorySection::default(),
        mirrors: Vec::new(),
        build: None,
        logging: LoggingConfig::default(),
        maven: MavenProjectMetadata {
            group_id: "com.example".to_string(),
            artifact_id: Some("example-app".to_string()),
            packaging: Some("jar".to_string()),
            description: None,
            url: None,
        },
    };

    // resolved には多数の依存が含まれていても、生成する pom.xml は manifest の直接依存のみを出力する。
    let resolved = ResolvedDependencies {
        strategy: "test".to_string(),
        algorithm: ResolverAlgorithmKind::MavenCompat,
        dependencies: vec![
            ResolvedDependency {
                name: "org.example:root-lib".to_string(),
                requested: "1.0.0".to_string(),
                decision: VersionDecision::Exact("1.0.0".to_string()),
                scope: DependencyScope::Main,
                source: ResolutionSource::Registry,
                local_artifact: None,
            },
            ResolvedDependency {
                name: "org.extra:transitive-lib".to_string(),
                requested: "2.3.4".to_string(),
                decision: VersionDecision::Exact("2.3.4".to_string()),
                scope: DependencyScope::Main,
                source: ResolutionSource::Registry,
                local_artifact: None,
            },
        ],
        diagnostics: Vec::new(),
        stats: ResolutionStats {
            elapsed_ms: 0,
            total_dependencies: 0,
            decided_dependencies: 0,
        },
    };

    let pom = PomGenerator::new(&manifest, &resolved)
        .generate()
        .expect("pom generation should succeed");

    assert!(
        pom.contains("org.example") && pom.contains("root-lib") && pom.contains("1.0.0"),
        "pom.xml should contain manifest dependency"
    );
    assert!(
        !pom.contains("org.extra:transitive-lib"),
        "pom.xml must not include transitive dependencies"
    );
}
