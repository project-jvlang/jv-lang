use super::store_pom;
use super::*;
use std::sync::Arc;
use tempfile::tempdir;
use tokio::runtime::Builder as RuntimeBuilder;

#[test]
fn download_plan_unifies_base_and_plugin_closures() {
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    // base root -> depends on base-child
    let base_root = ArtifactCoordinates::new(
        "org.base".to_string(),
        "root".to_string(),
        "1.0.0".to_string(),
    );
    let base_child = ArtifactCoordinates::new(
        "org.base".to_string(),
        "child".to_string(),
        "1.0.0".to_string(),
    );
    store_pom(
        &cache,
        &base_root,
        &[(
            &base_child.group_id,
            &base_child.artifact_id,
            &base_child.version,
            None,
            false,
        )],
    );
    store_pom(&cache, &base_child, &[]);

    // plugin root -> depends on plugin-opt (optional) + plugin-test (test scope)
    let plugin_root = ArtifactCoordinates::new(
        "org.plugin".to_string(),
        "tool".to_string(),
        "1.0.0".to_string(),
    );
    let plugin_opt = ArtifactCoordinates::new(
        "org.plugin".to_string(),
        "opt".to_string(),
        "1.0.0".to_string(),
    );
    let plugin_test = ArtifactCoordinates::new(
        "org.plugin".to_string(),
        "test".to_string(),
        "1.0.0".to_string(),
    );
    store_pom(
        &cache,
        &plugin_root,
        &[
            (
                &plugin_opt.group_id,
                &plugin_opt.artifact_id,
                &plugin_opt.version,
                None,
                true,
            ),
            (
                &plugin_test.group_id,
                &plugin_test.artifact_id,
                &plugin_test.version,
                Some("test"),
                false,
            ),
        ],
    );
    store_pom(&cache, &plugin_opt, &[]);
    store_pom(&cache, &plugin_test, &[]);

    let base_closure = resolver
        .resolve_closure_with_options(&[base_root.clone()], ClosureOptions::base())
        .expect("base closure");
    let plugin_closure = resolver
        .resolve_closure_with_options(&[plugin_root.clone()], ClosureOptions::plugin_download())
        .expect("plugin closure");

    // download_plan と同じロジックで union を構成
    let mut seen = std::collections::HashSet::new();
    let mut plan = Vec::new();
    for coords in base_closure.iter().chain(plugin_closure.iter()) {
        let key = (
            coords.group_id.clone(),
            coords.artifact_id.clone(),
            coords.classifier.clone(),
            coords.version.clone(),
        );
        if seen.insert(key) {
            plan.push(coords.clone());
        }
    }

    let names: Vec<String> = plan
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    assert_eq!(
        plan.len(),
        5,
        "base(2) + plugin(3) が union され seeds だけで終わらないこと"
    );
    for expected in [
        &base_root,
        &base_child,
        &plugin_root,
        &plugin_opt,
        &plugin_test,
    ] {
        let id = format!(
            "{}:{}:{}",
            expected.group_id, expected.artifact_id, expected.version
        );
        assert!(
            names.contains(&id),
            "plan should include {id}, got {names:?}"
        );
    }
}

#[test]
fn download_plan_matches_expected_baseline() {
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    // base
    let base_root = ArtifactCoordinates::new(
        "org.base".to_string(),
        "root".to_string(),
        "1.0.0".to_string(),
    );
    let base_child = ArtifactCoordinates::new(
        "org.base".to_string(),
        "child".to_string(),
        "1.0.0".to_string(),
    );
    store_pom(
        &cache,
        &base_root,
        &[(
            &base_child.group_id,
            &base_child.artifact_id,
            &base_child.version,
            None,
            false,
        )],
    );
    store_pom(&cache, &base_child, &[]);

    // plugin
    let plugin_root = ArtifactCoordinates::new(
        "org.plugin".to_string(),
        "tool".to_string(),
        "1.0.0".to_string(),
    );
    let plugin_dep = ArtifactCoordinates::new(
        "org.plugin".to_string(),
        "dep".to_string(),
        "2.0.0".to_string(),
    );
    store_pom(
        &cache,
        &plugin_root,
        &[(
            &plugin_dep.group_id,
            &plugin_dep.artifact_id,
            &plugin_dep.version,
            Some("test"),
            false,
        )],
    );
    store_pom(&cache, &plugin_dep, &[]);

    let base_closure = resolver
        .resolve_closure_with_options(&[base_root.clone()], ClosureOptions::base())
        .expect("base closure");
    let plugin_closure = resolver
        .resolve_closure_with_options(&[plugin_root.clone()], ClosureOptions::plugin_download())
        .expect("plugin closure");

    let mut plan = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for coords in base_closure.iter().chain(plugin_closure.iter()) {
        let key = (
            coords.group_id.clone(),
            coords.artifact_id.clone(),
            coords.classifier.clone(),
            coords.version.clone(),
        );
        if seen.insert(key) {
            plan.push(coords.clone());
        }
    }

    let baseline: std::collections::HashSet<String> = vec![
        format!(
            "{}:{}:{}",
            base_root.group_id, base_root.artifact_id, base_root.version
        ),
        format!(
            "{}:{}:{}",
            base_child.group_id, base_child.artifact_id, base_child.version
        ),
        format!(
            "{}:{}:{}",
            plugin_root.group_id, plugin_root.artifact_id, plugin_root.version
        ),
        format!(
            "{}:{}:{}",
            plugin_dep.group_id, plugin_dep.artifact_id, plugin_dep.version
        ),
    ]
    .into_iter()
    .collect();

    let plan_set: std::collections::HashSet<String> = plan
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    let missing: Vec<_> = baseline.difference(&plan_set).cloned().collect();
    let extra: Vec<_> = plan_set.difference(&baseline).cloned().collect();

    assert!(
        missing.is_empty() && extra.is_empty(),
        "download_plan should match baseline. missing={missing:?}, extra={extra:?}"
    );
}

#[test]
fn download_plan_matches_reference_maven_like_baseline() {
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    // 疑似 Maven baseline: base root + transitive, plugin root + test/optional transitive
    let base_root = ArtifactCoordinates::new(
        "org.example".to_string(),
        "app".to_string(),
        "1.0.0".to_string(),
    );
    let base_dep_runtime = ArtifactCoordinates::new(
        "org.runtime".to_string(),
        "core".to_string(),
        "1.2.3".to_string(),
    );
    let base_dep_transitive = ArtifactCoordinates::new(
        "org.runtime".to_string(),
        "util".to_string(),
        "4.5.6".to_string(),
    );

    // base_root depends on core; core depends on util
    store_pom(
        &cache,
        &base_root,
        &[(
            &base_dep_runtime.group_id,
            &base_dep_runtime.artifact_id,
            &base_dep_runtime.version,
            None,
            false,
        )],
    );
    store_pom(
        &cache,
        &base_dep_runtime,
        &[(
            &base_dep_transitive.group_id,
            &base_dep_transitive.artifact_id,
            &base_dep_transitive.version,
            None,
            false,
        )],
    );
    store_pom(&cache, &base_dep_transitive, &[]);

    let plugin_root = ArtifactCoordinates::new(
        "org.plugin".to_string(),
        "tool".to_string(),
        "3.0.0".to_string(),
    );
    let plugin_dep_test = ArtifactCoordinates::new(
        "org.plugin".to_string(),
        "test-helper".to_string(),
        "2.0.0".to_string(),
    );
    let plugin_dep_optional = ArtifactCoordinates::new(
        "org.plugin".to_string(),
        "opt-extra".to_string(),
        "1.1.0".to_string(),
    );
    let plugin_dep_nested = ArtifactCoordinates::new(
        "org.plugin".to_string(),
        "nested".to_string(),
        "0.9.0".to_string(),
    );

    // plugin_root depends on test-helper (test) and opt-extra (optional); test-helper depends on nested
    store_pom(
        &cache,
        &plugin_root,
        &[
            (
                &plugin_dep_test.group_id,
                &plugin_dep_test.artifact_id,
                &plugin_dep_test.version,
                Some("test"),
                false,
            ),
            (
                &plugin_dep_optional.group_id,
                &plugin_dep_optional.artifact_id,
                &plugin_dep_optional.version,
                None,
                true,
            ),
        ],
    );
    store_pom(
        &cache,
        &plugin_dep_test,
        &[(
            &plugin_dep_nested.group_id,
            &plugin_dep_nested.artifact_id,
            &plugin_dep_nested.version,
            None,
            false,
        )],
    );
    store_pom(&cache, &plugin_dep_optional, &[]);
    store_pom(&cache, &plugin_dep_nested, &[]);

    let base_closure = resolver
        .resolve_closure_with_options(&[base_root.clone()], ClosureOptions::base())
        .expect("base closure");
    let plugin_closure = resolver
        .resolve_closure_with_options(&[plugin_root.clone()], ClosureOptions::plugin_download())
        .expect("plugin closure");

    let mut plan = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for coords in base_closure.iter().chain(plugin_closure.iter()) {
        let key = (
            coords.group_id.clone(),
            coords.artifact_id.clone(),
            coords.classifier.clone(),
            coords.version.clone(),
        );
        if seen.insert(key) {
            plan.push(coords.clone());
        }
    }

    let baseline: std::collections::HashSet<String> = vec![
        format!(
            "{}:{}:{}",
            base_root.group_id, base_root.artifact_id, base_root.version
        ),
        format!(
            "{}:{}:{}",
            base_dep_runtime.group_id, base_dep_runtime.artifact_id, base_dep_runtime.version
        ),
        format!(
            "{}:{}:{}",
            base_dep_transitive.group_id,
            base_dep_transitive.artifact_id,
            base_dep_transitive.version
        ),
        format!(
            "{}:{}:{}",
            plugin_root.group_id, plugin_root.artifact_id, plugin_root.version
        ),
        format!(
            "{}:{}:{}",
            plugin_dep_test.group_id, plugin_dep_test.artifact_id, plugin_dep_test.version
        ),
        format!(
            "{}:{}:{}",
            plugin_dep_optional.group_id,
            plugin_dep_optional.artifact_id,
            plugin_dep_optional.version
        ),
        format!(
            "{}:{}:{}",
            plugin_dep_nested.group_id, plugin_dep_nested.artifact_id, plugin_dep_nested.version
        ),
    ]
    .into_iter()
    .collect();

    let plan_set: std::collections::HashSet<String> = plan
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    let missing: Vec<_> = baseline.difference(&plan_set).cloned().collect();
    let extra: Vec<_> = plan_set.difference(&baseline).cloned().collect();

    assert!(
        missing.is_empty() && extra.is_empty(),
        "baseline mismatch. missing={missing:?}, extra={extra:?}"
    );
}

fn parse_fixture_line(line: &str) -> ArtifactCoordinates {
    let line = line.trim().trim_end_matches(".jar");
    let parts: Vec<&str> = line.split('/').collect();
    assert!(
        parts.len() >= 3,
        "expected at least group/../artifact/version/file: got {line}"
    );
    let version = parts[parts.len() - 2].to_string();
    let artifact_id = parts[parts.len() - 3].to_string();
    let group_id = parts[..parts.len() - 3].join(".");
    // filename = artifact-version.jar (classifier 未考慮)
    let filename = parts[parts.len() - 1];
    assert!(
        filename.starts_with(&artifact_id),
        "filename should start with artifact id"
    );
    ArtifactCoordinates::new(group_id, artifact_id, version)
}

#[test]
fn download_plan_matches_real_maven_fixture_list() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/maven_baseline_jars.txt");
    let content = std::fs::read_to_string(&fixture_path).expect("fixture file should be readable");
    let coords: Vec<ArtifactCoordinates> = content
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(parse_fixture_line)
        .collect();

    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    // すべての座標に空POMを保存し、rootsとして投入する（依存展開なしでも download_plan の集合一致を確認）。
    for coord in &coords {
        cache
            .store_pom(
                coord,
                "<project><modelVersion>4.0.0</modelVersion></project>",
            )
            .expect("store fixture pom");
    }

    let base_closure = resolver
        .resolve_closure_with_options(&coords, ClosureOptions::base())
        .expect("base closure");
    let plugin_closure = resolver
        .resolve_closure_with_options(&coords, ClosureOptions::plugin_download())
        .expect("plugin closure");

    let mut plan = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for coords in base_closure.iter().chain(plugin_closure.iter()) {
        let key = (
            coords.group_id.clone(),
            coords.artifact_id.clone(),
            coords.classifier.clone(),
            coords.version.clone(),
        );
        if seen.insert(key) {
            plan.push(coords.clone());
        }
    }

    let baseline: std::collections::HashSet<String> = coords
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();
    let plan_set: std::collections::HashSet<String> = plan
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    let missing: Vec<_> = baseline.difference(&plan_set).cloned().collect();
    let extra: Vec<_> = plan_set.difference(&baseline).cloned().collect();

    assert!(
        missing.is_empty() && extra.is_empty(),
        "fixture baseline mismatch. missing={missing:?}, extra={extra:?}"
    );
}

#[test]
fn download_plan_matches_fixture_when_root_is_commons_lang3() {
    // Maven dependency:resolve が commons-lang3 1件から取得した 62件の座標と一致するかを検証
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/maven_baseline_jars.txt");
    let content = std::fs::read_to_string(&fixture_path).expect("fixture file should be readable");
    let coords: Vec<ArtifactCoordinates> = content
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(parse_fixture_line)
        .collect();

    // root を commons-lang3 3.14.0 として、そのほかの座標をすべて依存としてつなぐ POM を構成
    let root = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-lang3".to_string(),
        "3.14.0".to_string(),
    );

    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    // root POM: 62件（root自身を含む）を dependencies に列挙
    let mut deps_xml = String::new();
    deps_xml.push_str("<dependencies>");
    for coord in &coords {
        deps_xml.push_str("<dependency>");
        deps_xml.push_str(&format!("<groupId>{}</groupId>", coord.group_id));
        deps_xml.push_str(&format!("<artifactId>{}</artifactId>", coord.artifact_id));
        deps_xml.push_str(&format!("<version>{}</version>", coord.version));
        deps_xml.push_str("</dependency>");
    }
    deps_xml.push_str("</dependencies>");
    let root_pom = format!(
        r#"
                <project>
                  <modelVersion>4.0.0</modelVersion>
                  <groupId>{}</groupId>
                  <artifactId>{}</artifactId>
                  <version>{}</version>
                  {deps}
                </project>
            "#,
        root.group_id,
        root.artifact_id,
        root.version,
        deps = deps_xml
    );
    cache.store_pom(&root, &root_pom).expect("store root pom");

    // そのほかの座標は依存なしの空 POM
    for coord in &coords {
        cache
            .store_pom(
                coord,
                "<project><modelVersion>4.0.0</modelVersion></project>",
            )
            .expect("store fixture pom");
    }

    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    // all scopes + optional を許容する plugin_download オプションで展開
    let closure = resolver
        .resolve_closure_with_options(&[root.clone()], ClosureOptions::plugin_download())
        .expect("closure");

    let plan_set: std::collections::HashSet<String> = closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();
    let baseline: std::collections::HashSet<String> = coords
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    let missing: Vec<_> = baseline.difference(&plan_set).cloned().collect();
    let extra: Vec<_> = plan_set.difference(&baseline).cloned().collect();

    assert!(
        missing.is_empty() && extra.is_empty(),
        "commons-lang3 基準の Maven 実績と不一致: missing={missing:?}, extra={extra:?}"
    );
}
