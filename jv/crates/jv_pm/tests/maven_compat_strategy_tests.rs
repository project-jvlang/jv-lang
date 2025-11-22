use std::fs;
use std::path::PathBuf;

use tempfile::tempdir;

use jv_pm::registry::ChecksumAlgorithm;
use jv_pm::resolver::strategies::maven_39::hydrate_resolved_dependencies_with_jars;
use jv_pm::resolver::{
    DependencyScope, MavenResolverContext, ResolutionDiagnostic, ResolutionStats,
    ResolvedDependencies, ResolvedDependency, ResolverAlgorithmKind, VersionDecision,
};

fn empty_resolved() -> ResolvedDependencies {
    ResolvedDependencies {
        strategy: "maven".to_string(),
        algorithm: ResolverAlgorithmKind::MavenCompat,
        dependencies: Vec::new(),
        diagnostics: Vec::new(),
        stats: ResolutionStats {
            elapsed_ms: 0,
            total_dependencies: 0,
            decided_dependencies: 0,
        },
    }
}

#[test]
fn test_hydrate_resolved_dependencies_with_jars_empty() {
    let temp = tempdir().expect("temp dir");
    let context = MavenResolverContext {
        project_root: temp.path().to_path_buf(),
        local_repository: temp.path().join(".jv").join("repository"),
        repositories: Vec::new(),
        mirrors: Vec::new(),
        base_dependencies: Vec::new(),
    };

    let mut resolved = empty_resolved();
    hydrate_resolved_dependencies_with_jars(None, &context, &mut resolved)
        .expect("hydrate empty dependencies");
    assert!(resolved.dependencies.is_empty());
}

fn write_fake_jar(local_repo: &PathBuf, coords: (&str, &str, &str)) -> PathBuf {
    let (group, artifact, version) = coords;
    let jar_path = local_repo
        .join(group.replace('.', "/"))
        .join(artifact)
        .join(version)
        .join(format!("{artifact}-{version}.jar"));

    if let Some(parent) = jar_path.parent() {
        fs::create_dir_all(parent).expect("create parent dirs");
    }

    let bytes = b"demo-jar-bytes";
    fs::write(&jar_path, bytes).expect("write jar");

    let algorithm = ChecksumAlgorithm::Sha256;
    let checksum_path = jar_path.with_extension(format!("jar.{}", algorithm.extension()));
    let checksum = algorithm.compute(bytes);
    fs::write(checksum_path, format!("{checksum}\n")).expect("write checksum");

    jar_path
}

#[test]
fn test_hydrate_resolved_dependencies_with_jars_single() {
    let temp = tempdir().expect("temp dir");
    let local_repo = temp.path().join(".jv").join("repository");
    let jar_path = write_fake_jar(&local_repo, ("org.example", "demo", "1.0.0"));

    let context = MavenResolverContext {
        project_root: temp.path().to_path_buf(),
        local_repository: local_repo.clone(),
        repositories: Vec::new(),
        mirrors: Vec::new(),
        base_dependencies: Vec::new(),
    };

    let dependency = ResolvedDependency {
        name: "org.example:demo".to_string(),
        requested: "1.0.0".to_string(),
        decision: VersionDecision::Exact("1.0.0".to_string()),
        scope: DependencyScope::Main,
        source: jv_pm::resolver::ResolutionSource::Registry,
        local_artifact: None,
    };

    let mut resolved = ResolvedDependencies {
        strategy: "maven".to_string(),
        algorithm: ResolverAlgorithmKind::MavenCompat,
        dependencies: vec![dependency],
        diagnostics: vec![ResolutionDiagnostic::info("TEST", "hydrate single jar")],
        stats: ResolutionStats {
            elapsed_ms: 1,
            total_dependencies: 1,
            decided_dependencies: 1,
        },
    };

    let runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("runtime");

    hydrate_resolved_dependencies_with_jars(Some(&runtime), &context, &mut resolved)
        .expect("hydrate single dependency");

    let artifact_path = resolved
        .dependencies
        .first()
        .and_then(|dep| dep.local_artifact.clone())
        .expect("local artifact to be populated");

    assert_eq!(
        PathBuf::from(artifact_path),
        jar_path,
        "local_artifact should point to the verified jar"
    );
}
