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

    // そのほかの座標は依存なしの空 POM（root と同一 GA+version は上書きしない）
    for coord in &coords {
        if coord.group_id == root.group_id
            && coord.artifact_id == root.artifact_id
            && coord.version == root.version
        {
            continue;
        }
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

fn find_coord<'a>(
    coords: &'a [ArtifactCoordinates],
    ga: &str,
    version: &str,
) -> Option<&'a ArtifactCoordinates> {
    coords.iter().find(|c| {
        c.version == version
            && ga
                .split_once(':')
                .map(|(g, a)| g == c.group_id && a == c.artifact_id)
                .unwrap_or(false)
    })
}

#[test]
fn download_plan_includes_key_maven_baseline_coords() {
    // Maven公式 dependency:resolve の実績から、欠落しやすい代表的な座標が含まれることを追加検証する
    // 参照: toolchains/apache-maven-3.9.11/maven-compat/src/test/java/org/apache/maven/repository/legacy/resolver/DefaultArtifactCollectorTest.java 相当の解決挙動
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/maven_baseline_jars.txt");
    let content = std::fs::read_to_string(&fixture_path).expect("fixture file should be readable");
    let coords: Vec<ArtifactCoordinates> = content
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(parse_fixture_line)
        .collect();

    // 欠落が報告されている代表的な座標群
    let required = [
        ("org.apache.httpcomponents:httpcore", "4.4.14"),
        ("org.apache.httpcomponents:httpclient", "4.5.13"),
        ("commons-codec:commons-codec", "1.16.1"),
        ("commons-io:commons-io", "2.15.1"),
        ("org.codehaus.plexus:plexus-utils", "4.0.1"),
        ("com.github.luben:zstd-jni", "1.5.5-11"),
    ];

    let mut required_coords = Vec::new();
    for (ga, ver) in required {
        let coord = find_coord(&coords, ga, ver)
            .unwrap_or_else(|| panic!("fixture should contain {ga}:{ver} but was not found"));
        required_coords.push(coord.clone());
    }

    // root を commons-lang3 とし、fixture 座標をすべて依存に列挙する POM を用意
    let root = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-lang3".to_string(),
        "3.14.0".to_string(),
    );

    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));

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

    for coord in &coords {
        if coord.group_id == root.group_id
            && coord.artifact_id == root.artifact_id
            && coord.version == root.version
        {
            continue;
        }
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

    let closure = resolver
        .resolve_closure_with_options(&[root.clone()], ClosureOptions::plugin_download())
        .expect("closure");
    let plan_set: std::collections::HashSet<String> = closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    for coord in required_coords {
        let id = format!("{}:{}:{}", coord.group_id, coord.artifact_id, coord.version);
        assert!(
            plan_set.contains(&id),
            "download plan should include required baseline artifact {id}"
        );
    }
}

#[test]
fn plugin_download_includes_optional_and_test_dependencies() {
    // optional/test は base クローズでは除外され、plugin_download では含まれることを検証する
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = ArtifactCoordinates::new(
        "org.example".to_string(),
        "app".to_string(),
        "1.0.0".to_string(),
    );
    let optional_dep = ArtifactCoordinates::new(
        "org.example".to_string(),
        "opt-lib".to_string(),
        "1.0.0".to_string(),
    );
    let test_dep = ArtifactCoordinates::new(
        "org.example".to_string(),
        "test-lib".to_string(),
        "1.0.0".to_string(),
    );

    store_pom(
        &cache,
        &root,
        &[
            (
                &optional_dep.group_id,
                &optional_dep.artifact_id,
                &optional_dep.version,
                None,
                true,
            ),
            (
                &test_dep.group_id,
                &test_dep.artifact_id,
                &test_dep.version,
                Some("test"),
                false,
            ),
        ],
    );
    store_pom(&cache, &optional_dep, &[]);
    store_pom(&cache, &test_dep, &[]);

    let base_closure = resolver
        .resolve_closure_with_options(&[root.clone()], ClosureOptions::base())
        .expect("base");
    let plugin_closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::plugin_download())
        .expect("plugin");

    let base_ids: std::collections::HashSet<String> = base_closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();
    let plugin_ids: std::collections::HashSet<String> = plugin_closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    assert!(
        !base_ids.contains(&format!(
            "{}:{}:{}",
            optional_dep.group_id, optional_dep.artifact_id, optional_dep.version
        )),
        "base closure should exclude optional dependency"
    );
    assert!(
        !base_ids.contains(&format!(
            "{}:{}:{}",
            test_dep.group_id, test_dep.artifact_id, test_dep.version
        )),
        "base closure should exclude test scope dependency"
    );
    assert!(
        plugin_ids.contains(&format!(
            "{}:{}:{}",
            optional_dep.group_id, optional_dep.artifact_id, optional_dep.version
        )),
        "plugin_download should include optional dependency"
    );
    assert!(
        plugin_ids.contains(&format!(
            "{}:{}:{}",
            test_dep.group_id, test_dep.artifact_id, test_dep.version
        )),
        "plugin_download should include test scope dependency"
    );
}

#[test]
fn plugin_download_applies_dependency_management_version() {
    // dependencyManagement が指定する版が plugin_download でも強制されることを検証
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = ArtifactCoordinates::new(
        "org.example".to_string(),
        "app".to_string(),
        "1.0.0".to_string(),
    );
    let managed = ArtifactCoordinates::new(
        "org.example".to_string(),
        "codec".to_string(),
        "2.0.0".to_string(),
    );
    let older = ArtifactCoordinates::new(
        "org.example".to_string(),
        "codec".to_string(),
        "1.0.0".to_string(),
    );

    // root dependencyManagement で codec=2.0.0 を指定し、子 POM が 1.0.0 を参照しても 2.0.0 で解決されることを確認
    let dm = format!(
        r#"
        <dependencyManagement>
          <dependencies>
            <dependency>
              <groupId>{}</groupId>
              <artifactId>{}</artifactId>
              <version>{}</version>
            </dependency>
          </dependencies>
        </dependencyManagement>
        "#,
        managed.group_id, managed.artifact_id, managed.version
    );
    let root_pom = format!(
        r#"
        <project>
          <modelVersion>4.0.0</modelVersion>
          <groupId>{root_group}</groupId>
          <artifactId>{root_artifact}</artifactId>
          <version>{root_version}</version>
          {dm}
          <dependencies>
            <dependency>
              <groupId>{dep_group}</groupId>
              <artifactId>{dep_artifact}</artifactId>
              <version>{dep_version}</version>
            </dependency>
          </dependencies>
        </project>
        "#,
        root_group = root.group_id,
        root_artifact = root.artifact_id,
        root_version = root.version,
        dm = dm,
        dep_group = older.group_id,
        dep_artifact = older.artifact_id,
        dep_version = older.version
    );
    cache.store_pom(&root, &root_pom).expect("store root");
    cache
        .store_pom(
            &older,
            "<project><modelVersion>4.0.0</modelVersion></project>",
        )
        .expect("store older");
    cache
        .store_pom(
            &managed,
            "<project><modelVersion>4.0.0</modelVersion></project>",
        )
        .expect("store managed");

    let plugin_closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::plugin_download())
        .expect("plugin");
    let mut versions = std::collections::HashSet::new();
    for c in plugin_closure {
        if c.group_id == managed.group_id && c.artifact_id == managed.artifact_id {
            versions.insert(c.version.clone());
        }
    }
    assert!(
        versions.contains(&managed.version),
        "managed version should be present in closure"
    );
    assert!(
        !versions.contains(&older.version),
        "older unmanaged version should be excluded when dependencyManagement is present"
    );
}

#[test]
fn plugin_download_deduplicates_ga_by_nearest_version() {
    // plugin_download は Maven 実績に合わせ、依存管理が無い場合は同一GAの複数版を許容する。
    // ここでは同一GAの 1.0 / 2.0 を投入し、両方が残ることを確認する。
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = ArtifactCoordinates::new(
        "org.example".to_string(),
        "app".to_string(),
        "1.0.0".to_string(),
    );
    let newer = ArtifactCoordinates::new(
        "org.example".to_string(),
        "codec".to_string(),
        "2.0.0".to_string(),
    );
    let older = ArtifactCoordinates::new(
        "org.example".to_string(),
        "codec".to_string(),
        "1.0.0".to_string(),
    );

    // root depends on both versions (順序: older, newer)。依存管理が無いので両方が共存する。
    store_pom(
        &cache,
        &root,
        &[
            (
                &older.group_id,
                &older.artifact_id,
                &older.version,
                None,
                false,
            ),
            (
                &newer.group_id,
                &newer.artifact_id,
                &newer.version,
                None,
                false,
            ),
        ],
    );
    store_pom(&cache, &older, &[]);
    store_pom(&cache, &newer, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::plugin_download())
        .expect("closure");
    let versions: std::collections::HashSet<String> = closure
        .iter()
        .filter(|c| c.group_id == newer.group_id && c.artifact_id == newer.artifact_id)
        .map(|c| c.version.clone())
        .collect();

    assert!(
        versions.contains(&older.version) && versions.contains(&newer.version),
        "plugin_download should keep both versions when no dependencyManagement is present"
    );
}

#[test]
fn plugin_download_baseline_count_matches_fixture() {
    // baseline 62 件を期待することを明示し、差分をカウントで検知する
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/maven_baseline_jars.txt");
    let content = std::fs::read_to_string(&fixture_path).expect("fixture file should be readable");
    let coords: Vec<ArtifactCoordinates> = content
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(parse_fixture_line)
        .collect();

    let root = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-lang3".to_string(),
        "3.14.0".to_string(),
    );

    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));

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
    for coord in &coords {
        if coord.group_id == root.group_id
            && coord.artifact_id == root.artifact_id
            && coord.version == root.version
        {
            continue;
        }
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

    let closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::plugin_download())
        .expect("closure");

    assert_eq!(
        closure.len(),
        coords.len(),
        "plugin_download closure count should match Maven baseline ({}), got {}",
        coords.len(),
        closure.len()
    );
}

#[test]
fn parses_fixture_root_pom_dependencies() {
    // 依存が POM パーサで正しく抽出されることを確認し、0 件になる回 regresion を明示検出する
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/maven_baseline_jars.txt");
    let content = std::fs::read_to_string(&fixture_path).expect("fixture file should be readable");
    let coords: Vec<ArtifactCoordinates> = content
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(parse_fixture_line)
        .collect();

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
    let pom = format!(
        r#"
        <project>
          <modelVersion>4.0.0</modelVersion>
          <groupId>org.apache.commons</groupId>
          <artifactId>commons-lang3</artifactId>
          <version>3.14.0</version>
          {deps}
        </project>
        "#,
        deps = deps_xml
    );

    let deps = parse_direct_dependencies(&pom).expect("parse fixture pom");
    let names: std::collections::HashSet<String> = deps
        .iter()
        .map(|d| format!("{}:{}", d.name, d.requirement))
        .collect();
    assert_eq!(
        deps.len(),
        coords.len(),
        "fixture dependencies should be parsed completely"
    );

    // 代表的な座標が抽出されていることを確認
    for (ga, ver) in [
        ("org.apache.commons:commons-lang3", "3.14.0"),
        ("org.apache.httpcomponents:httpcore", "4.4.14"),
        ("com.github.luben:zstd-jni", "1.5.5-11"),
    ] {
        let key = format!("{ga}:{ver}");
        assert!(
            names.contains(&key),
            "expected dependency {key} to be parsed from fixture pom"
        );
    }
}
