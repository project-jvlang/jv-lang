use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use tempfile::tempdir;
use tokio::runtime::Builder as RuntimeBuilder;

use jv_pm::cache::DependencyCache;
use jv_pm::registry::{ArtifactCoordinates, ChecksumAlgorithm};
use jv_pm::resolver::strategies::maven_39::{
    ClosureOptions, MavenDependencyResolver, compare_maven_versions,
    hydrate_resolved_dependencies_with_jars, version_satisfies_range,
};
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
        "local_artifact が検証済み Jar を指していること"
    );
}

fn store_pom(
    cache: &DependencyCache,
    coords: &ArtifactCoordinates,
    deps: &[(&str, &str, &str, Option<&str>, bool)],
) {
    let mut deps_xml = String::new();
    if !deps.is_empty() {
        deps_xml.push_str("<dependencies>");
        for (group, artifact, version, scope, optional) in deps {
            deps_xml.push_str("<dependency>");
            deps_xml.push_str(&format!("<groupId>{group}</groupId>"));
            deps_xml.push_str(&format!("<artifactId>{artifact}</artifactId>"));
            deps_xml.push_str(&format!("<version>{version}</version>"));
            if let Some(scope) = scope {
                deps_xml.push_str(&format!("<scope>{scope}</scope>"));
            }
            if *optional {
                deps_xml.push_str("<optional>true</optional>");
            }
            deps_xml.push_str("</dependency>");
        }
        deps_xml.push_str("</dependencies>");
    }

    let pom = format!(
        r#"
            <project>
              <modelVersion>4.0.0</modelVersion>
              <groupId>{}</groupId>
              <artifactId>{}</artifactId>
              <version>{}</version>
              {deps}
            </project>
        "#,
        coords.group_id,
        coords.artifact_id,
        coords.version,
        deps = deps_xml
    );
    cache.store_pom(coords, &pom).expect("store pom");
}

fn ids(coords: &[ArtifactCoordinates]) -> HashSet<String> {
    coords
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect()
}

fn coords(group: &str, artifact: &str, version: &str) -> ArtifactCoordinates {
    ArtifactCoordinates::new(group.to_string(), artifact.to_string(), version.to_string())
}

#[test]
fn test_nearest_conflict_resolver_depth_wins() {
    // Maven: toolchains/apache-maven-3.9.11/maven-compat/src/test/java/org/apache/maven/repository/legacy/resolver/conflict/NearestConflictResolverTest.java
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = coords("org.example", "root", "1.0.0");
    let direct_old = coords("org.example", "lib-a", "1.0.0");
    let transitive_new = coords("org.example", "lib-a", "2.0.0");
    let intermediate = coords("org.example", "middle", "1.0.0");

    store_pom(
        cache.as_ref(),
        &root,
        &[
            (
                &direct_old.group_id,
                &direct_old.artifact_id,
                &direct_old.version,
                None,
                false,
            ),
            (
                &intermediate.group_id,
                &intermediate.artifact_id,
                &intermediate.version,
                None,
                false,
            ),
        ],
    );
    store_pom(
        cache.as_ref(),
        &intermediate,
        &[(
            &transitive_new.group_id,
            &transitive_new.artifact_id,
            &transitive_new.version,
            None,
            false,
        )],
    );
    store_pom(cache.as_ref(), &direct_old, &[]);
    store_pom(cache.as_ref(), &transitive_new, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::base())
        .expect("closure");
    let set = ids(&closure);

    assert!(
        set.contains("org.example:lib-a:1.0.0"),
        "nearest（浅い経路）が優先される"
    );
    assert!(
        !set.contains("org.example:lib-a:2.0.0"),
        "深い経路の lib-a は除外される"
    );
    // 深さが浅い a:1.0 が transitive の a:2.0 より優先されることを確認（dependency 列挙順を逆にしても同じ結果）
    let reversed_root = coords("org.example", "root-rev", "1.0.0");
    let reversed_runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let reversed_resolver =
        MavenDependencyResolver::new(&reversed_runtime, cache.clone(), Vec::new());

    // root には B(→a2) と a1 の両方が並ぶ（B が先頭で列挙されても a1 が勝つ）
    let b1 = coords("org.example", "lib-b", "1.0.0");
    store_pom(
        cache.as_ref(),
        &reversed_root,
        &[
            (&b1.group_id, &b1.artifact_id, &b1.version, None, false),
            (
                &direct_old.group_id,
                &direct_old.artifact_id,
                &direct_old.version,
                None,
                false,
            ),
        ],
    );
    store_pom(
        cache.as_ref(),
        &b1,
        &[(
            &transitive_new.group_id,
            &transitive_new.artifact_id,
            &transitive_new.version,
            None,
            false,
        )],
    );
    store_pom(cache.as_ref(), &reversed_root, &[]);

    let reversed_closure = reversed_resolver
        .resolve_closure_with_options(&[reversed_root], ClosureOptions::base())
        .expect("closure");
    let reversed_set = ids(&reversed_closure);
    assert!(
        reversed_set.contains("org.example:lib-a:1.0.0"),
        "transitive が先に列挙されても浅い経路が優先される"
    );
    assert!(
        !reversed_set.contains("org.example:lib-a:2.0.0"),
        "nearest 戦略では深いバージョンが落とされる"
    );
}

#[test]
fn test_newest_conflict_resolver_version_wins() {
    // Maven: toolchains/apache-maven-3.9.11/maven-compat/src/test/java/org/apache/maven/repository/legacy/resolver/conflict/NewestConflictResolverTest.java
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = coords("org.example", "root", "1.0.0");
    let a_v1 = coords("org.example", "lib-a", "1.0.0");
    let a_v2 = coords("org.example", "lib-a", "2.0.0");
    store_pom(
        cache.as_ref(),
        &root,
        &[
            (
                &a_v1.group_id,
                &a_v1.artifact_id,
                &a_v1.version,
                None,
                false,
            ),
            (
                &a_v2.group_id,
                &a_v2.artifact_id,
                &a_v2.version,
                None,
                false,
            ),
        ],
    );
    store_pom(cache.as_ref(), &a_v1, &[]);
    store_pom(cache.as_ref(), &a_v2, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::newest_conflict())
        .expect("closure");
    let versions = closure
        .iter()
        .filter(|c| c.group_id == "org.example" && c.artifact_id == "lib-a")
        .map(|c| c.version.clone())
        .collect::<HashSet<_>>();

    assert_eq!(
        versions,
        HashSet::from(["2.0.0".to_string()]),
        "newest 戦略では最大バージョンのみが残る"
    );
}

#[test]
fn test_oldest_conflict_resolver_version_wins() {
    // Maven: toolchains/apache-maven-3.9.11/maven-compat/src/test/java/org/apache/maven/repository/legacy/resolver/conflict/OldestConflictResolverTest.java
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = coords("org.example", "root", "1.0.0");
    let a_v1 = coords("org.example", "lib-a", "1.0.0");
    let a_v2 = coords("org.example", "lib-a", "2.0.0");
    // 同一深さで列挙順が決着することを確認
    store_pom(
        cache.as_ref(),
        &root,
        &[
            (
                &a_v1.group_id,
                &a_v1.artifact_id,
                &a_v1.version,
                None,
                false,
            ),
            (
                &a_v2.group_id,
                &a_v2.artifact_id,
                &a_v2.version,
                None,
                false,
            ),
        ],
    );
    store_pom(cache.as_ref(), &a_v1, &[]);
    store_pom(cache.as_ref(), &a_v2, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::oldest_conflict())
        .expect("closure");
    let set = ids(&closure);

    assert!(
        set.contains("org.example:lib-a:1.0.0"),
        "oldest 戦略では最初に遭遇した古い版が残る"
    );
    assert!(
        !set.contains("org.example:lib-a:2.0.0"),
        "新しい版は oldest 戦略では落とされる"
    );
}

#[test]
fn test_farthest_conflict_resolver_depth_loses() {
    // Maven: toolchains/apache-maven-3.9.11/maven-compat/src/test/java/org/apache/maven/repository/legacy/resolver/conflict/FarthestConflictResolverTest.java
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = coords("org.example", "root", "1.0.0");
    let shallow = coords("org.example", "lib-a", "1.0.0");
    let far = coords("org.example", "lib-a", "2.0.0");
    let intermediate = coords("org.example", "middle", "1.0.0");

    store_pom(
        cache.as_ref(),
        &root,
        &[
            (
                &shallow.group_id,
                &shallow.artifact_id,
                &shallow.version,
                None,
                false,
            ),
            (
                &intermediate.group_id,
                &intermediate.artifact_id,
                &intermediate.version,
                None,
                false,
            ),
        ],
    );
    store_pom(
        cache.as_ref(),
        &intermediate,
        &[(&far.group_id, &far.artifact_id, &far.version, None, false)],
    );
    store_pom(cache.as_ref(), &shallow, &[]);
    store_pom(cache.as_ref(), &far, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::farthest_conflict())
        .expect("closure");
    let set = ids(&closure);

    assert!(
        set.contains("org.example:lib-a:2.0.0"),
        "farthest 戦略では深い経路のバージョンが採用される"
    );
    assert!(
        !set.contains("org.example:lib-a:1.0.0"),
        "浅い依存は深い依存に置き換わる"
    );
}

#[test]
fn test_graph_conflict_resolver_compile_scope() {
    // Maven: toolchains/apache-maven-3.9.11/maven-compat/src/test/java/org/apache/maven/repository/metadata/DefaultGraphConflictResolverTest.java (compile scope)
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = coords("org.example", "root", "1.0.0");
    let compile_dep = coords("org.example", "compile-lib", "1.0.0");
    let provided_dep = coords("org.example", "provided-lib", "1.0.0");
    store_pom(
        cache.as_ref(),
        &root,
        &[
            (
                &compile_dep.group_id,
                &compile_dep.artifact_id,
                &compile_dep.version,
                None,
                false,
            ),
            (
                &provided_dep.group_id,
                &provided_dep.artifact_id,
                &provided_dep.version,
                Some("provided"),
                false,
            ),
        ],
    );
    store_pom(cache.as_ref(), &compile_dep, &[]);
    store_pom(cache.as_ref(), &provided_dep, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::base())
        .expect("closure");
    let set = ids(&closure);
    assert!(set.contains("org.example:compile-lib:1.0.0"));
    assert!(
        !set.contains("org.example:provided-lib:1.0.0"),
        "provided は base クローズでは除外される"
    );
}

#[test]
fn test_graph_conflict_resolver_runtime_scope() {
    // Maven: toolchains/apache-maven-3.9.11/maven-compat/src/test/java/org/apache/maven/repository/metadata/DefaultGraphConflictResolverTest.java (runtime scope)
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = coords("org.example", "root", "1.0.0");
    let runtime_dep = coords("org.example", "runtime-lib", "1.0.0");
    store_pom(
        cache.as_ref(),
        &root,
        &[(
            &runtime_dep.group_id,
            &runtime_dep.artifact_id,
            &runtime_dep.version,
            Some("runtime"),
            false,
        )],
    );
    store_pom(cache.as_ref(), &runtime_dep, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::base())
        .expect("closure");
    let set = ids(&closure);
    assert!(
        set.contains("org.example:runtime-lib:1.0.0"),
        "runtime は base クローズで保持される"
    );
}

#[test]
fn test_graph_conflict_resolver_test_scope() {
    // Maven: toolchains/apache-maven-3.9.11/maven-compat/src/test/java/org/apache/maven/repository/metadata/DefaultGraphConflictResolverTest.java (test scope)
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = coords("org.example", "root", "1.0.0");
    let test_dep = coords("org.example", "test-lib", "1.0.0");
    store_pom(
        cache.as_ref(),
        &root,
        &[(
            &test_dep.group_id,
            &test_dep.artifact_id,
            &test_dep.version,
            Some("test"),
            false,
        )],
    );
    store_pom(cache.as_ref(), &test_dep, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::base())
        .expect("closure");
    let set = ids(&closure);
    assert!(
        !set.contains("org.example:test-lib:1.0.0"),
        "test スコープは base クローズで除外される"
    );
}

#[test]
fn test_artifact_collector_transitive_dependencies() {
    // Maven: toolchains/apache-maven-3.9.11/maven-compat/src/test/java/org/apache/maven/repository/legacy/resolver/DefaultArtifactCollectorTest.java
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = coords("org.example", "root", "1.0.0");
    let b = coords("org.example", "lib-b", "1.0.0");
    let e = coords("org.example", "lib-e", "1.0.0");
    let c_new = coords("org.example", "lib-c", "2.0.0");
    let c_old = coords("org.example", "lib-c", "1.0.0");
    let d = coords("org.example", "lib-d", "1.0.0");
    let f = coords("org.example", "lib-f", "1.0.0");
    store_pom(
        cache.as_ref(),
        &root,
        &[
            (&b.group_id, &b.artifact_id, &b.version, None, false),
            (&e.group_id, &e.artifact_id, &e.version, None, false),
        ],
    );
    store_pom(
        cache.as_ref(),
        &b,
        &[(
            &c_new.group_id,
            &c_new.artifact_id,
            &c_new.version,
            None,
            false,
        )],
    );
    store_pom(
        cache.as_ref(),
        &e,
        &[(
            &c_old.group_id,
            &c_old.artifact_id,
            &c_old.version,
            None,
            false,
        )],
    );
    store_pom(
        cache.as_ref(),
        &c_new,
        &[(&d.group_id, &d.artifact_id, &d.version, None, false)],
    );
    store_pom(
        cache.as_ref(),
        &c_old,
        &[(&f.group_id, &f.artifact_id, &f.version, None, false)],
    );
    store_pom(cache.as_ref(), &d, &[]);
    store_pom(cache.as_ref(), &f, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::base())
        .expect("closure");
    let set = ids(&closure);
    assert!(set.contains("org.example:lib-d:1.0.0"));
    assert!(set.contains("org.example:lib-f:1.0.0"));
    assert!(
        set.contains("org.example:lib-c:1.0.0"),
        "nearest は最初に遭遇した（古い）lib-c を保持する"
    );
    assert!(
        !set.contains("org.example:lib-c:2.0.0"),
        "近い経路で決まったバージョンがある場合、新しい lib-c は除外される"
    );
}

#[test]
fn test_comparable_version_ordering() {
    // Maven: toolchains/apache-maven-3.9.11/maven-artifact/src/test/java/org/apache/maven/artifact/versioning/ComparableVersionTest.java
    assert_eq!(
        std::cmp::Ordering::Less,
        compare_maven_versions("1.0.0", "1.1.0")
    );
    assert_eq!(
        std::cmp::Ordering::Less,
        compare_maven_versions("1.1.0", "2.0.0")
    );
    assert_eq!(
        std::cmp::Ordering::Equal,
        compare_maven_versions("2.0.0", "2.0.0")
    );
    assert_eq!(
        std::cmp::Ordering::Less,
        compare_maven_versions("1.0-alpha", "1.0-beta")
    );
    assert_eq!(
        std::cmp::Ordering::Less,
        compare_maven_versions("1.0-beta", "1.0-rc")
    );
    assert_eq!(
        std::cmp::Ordering::Less,
        compare_maven_versions("1.0-rc", "1.0-SNAPSHOT")
    );
    assert_eq!(
        std::cmp::Ordering::Less,
        compare_maven_versions("1.0-SNAPSHOT", "1.0")
    );
    assert_eq!(
        std::cmp::Ordering::Equal,
        compare_maven_versions("1.0.Final", "1.0")
    );
    assert_eq!(
        std::cmp::Ordering::Greater,
        compare_maven_versions("1.0-sp", "1.0")
    );
}

#[test]
fn test_version_range_evaluation() {
    // Maven: toolchains/apache-maven-3.9.11/maven-artifact/src/test/java/org/apache/maven/artifact/versioning/VersionRangeTest.java
    assert!(
        version_satisfies_range("0.9", "(,1.0]"),
        "0.9 は上限1.0以下として許容される"
    );
    assert!(
        version_satisfies_range("1.0", "[1.0]"),
        "単一バージョン指定に合致する"
    );
    assert!(
        version_satisfies_range("1.2", "[1.2,1.3]"),
        "閉区間に 1.2 が含まれる"
    );
    assert!(
        !version_satisfies_range("1.4", "[1.2,1.3]"),
        "区間外の値は除外される"
    );
    assert!(
        version_satisfies_range("1.5", "[1.5,)"),
        "上限なし区間に 1.5 が含まれる"
    );
    assert!(
        !version_satisfies_range("1.1", "(,1.0],[1.2,)"),
        "連結区間の穴 (1.0,1.2) に入る 1.1 は除外される"
    );
    assert!(
        version_satisfies_range("1.0-SNAPSHOT", "[1.0,1.1-SNAPSHOT]"),
        "上限を含む場合、スナップショット境界も許容される"
    );
    assert!(
        !version_satisfies_range("1.0-SNAPSHOT", "[1.0,)"),
        "リリース基準の下限はスナップショットを含まない"
    );
}

#[test]
fn test_default_artifact_version_comparison() {
    // Maven: toolchains/apache-maven-3.9.11/maven-artifact/src/test/java/org/apache/maven/artifact/versioning/DefaultArtifactVersionTest.java
    assert_eq!(
        std::cmp::Ordering::Equal,
        compare_maven_versions("1.2.3", "1.2.3")
    );
    assert_eq!(
        std::cmp::Ordering::Less,
        compare_maven_versions("1.2.3", "1.2.4")
    );
    assert_eq!(
        std::cmp::Ordering::Greater,
        compare_maven_versions("1.3.0", "1.2.9")
    );
    assert_eq!(
        std::cmp::Ordering::Less,
        compare_maven_versions("1.2.3-rc1", "1.2.3")
    );
    assert_eq!(
        std::cmp::Ordering::Less,
        compare_maven_versions("1.2.3-SNAPSHOT", "1.2.3")
    );
    assert_eq!(
        std::cmp::Ordering::Greater,
        compare_maven_versions("1.2.3-sp1", "1.2.3")
    );
}
