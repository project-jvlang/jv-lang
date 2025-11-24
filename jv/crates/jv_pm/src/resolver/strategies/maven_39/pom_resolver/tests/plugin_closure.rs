use super::store_pom;
use super::*;
use std::sync::Arc;
use tempfile::tempdir;
use tokio::runtime::Builder as RuntimeBuilder;

#[test]
fn plugin_closure_excludes_optional_and_test_scopes() {
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let plugin_root = ArtifactCoordinates::new(
        "org.example".to_string(),
        "plugin-root".to_string(),
        "1.0.0".to_string(),
    );
    let dep_main = ArtifactCoordinates::new(
        "org.sample".to_string(),
        "main-lib".to_string(),
        "1.0.0".to_string(),
    );
    let dep_opt = ArtifactCoordinates::new(
        "org.sample".to_string(),
        "opt-lib".to_string(),
        "1.0.0".to_string(),
    );
    let dep_test = ArtifactCoordinates::new(
        "org.sample".to_string(),
        "test-lib".to_string(),
        "1.0.0".to_string(),
    );

    store_pom(
        &cache,
        &plugin_root,
        &[
            (
                &dep_main.group_id,
                &dep_main.artifact_id,
                &dep_main.version,
                None,
                false,
            ),
            (
                &dep_opt.group_id,
                &dep_opt.artifact_id,
                &dep_opt.version,
                None,
                true,
            ),
            (
                &dep_test.group_id,
                &dep_test.artifact_id,
                &dep_test.version,
                Some("test"),
                false,
            ),
        ],
    );
    store_pom(&cache, &dep_main, &[]);
    store_pom(&cache, &dep_opt, &[]);
    store_pom(&cache, &dep_test, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[plugin_root.clone()], ClosureOptions::plugin_download())
        .expect("resolve plugin closure");

    let names: Vec<String> = closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();
    assert!(
        names.contains(&format!(
            "{}:{}:{}",
            dep_main.group_id, dep_main.artifact_id, dep_main.version
        )),
        "main dependency should be expanded"
    );
    assert!(
        !names.contains(&format!(
            "{}:{}:{}",
            dep_opt.group_id, dep_opt.artifact_id, dep_opt.version
        )),
        "optional dependency should be skipped for plugin download"
    );
    assert!(
        !names.contains(&format!(
            "{}:{}:{}",
            dep_test.group_id, dep_test.artifact_id, dep_test.version
        )),
        "test-scope dependency should be skipped for plugin download"
    );
    assert!(
        names.len() >= 2,
        "closure should include root plus main dependency (optional/test are skipped)"
    );
}

#[test]
fn plugin_closure_keeps_multiple_versions_and_parses_poms() {
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let v1 = ArtifactCoordinates::new(
        "org.sample".to_string(),
        "multi-lib".to_string(),
        "1.0.0".to_string(),
    );
    let v2 = ArtifactCoordinates::new(
        "org.sample".to_string(),
        "multi-lib".to_string(),
        "2.0.0".to_string(),
    );
    // 依存を持たない POM をキャッシュに登録しておく
    store_pom(&cache, &v1, &[]);
    store_pom(&cache, &v2, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[v1.clone(), v2.clone()], ClosureOptions::plugin_download())
        .expect("resolve multi-version closure");

    let mut versions = closure
        .iter()
        .filter(|c| c.group_id == "org.sample" && c.artifact_id == "multi-lib")
        .map(|c| c.version.clone())
        .collect::<Vec<_>>();
    versions.sort();
    versions.dedup();

    assert_eq!(
        versions,
        vec!["1.0.0".to_string(), "2.0.0".to_string()],
        "allow_multiple_versions 経路で seeds が事前に落とされないことを確認"
    );
}
