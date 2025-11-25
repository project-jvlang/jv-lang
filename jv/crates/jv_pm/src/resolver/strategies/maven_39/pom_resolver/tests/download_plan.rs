use super::store_pom;
use super::*;
use crate::resolver::strategies::maven_39::pom_resolver;
use crate::resolver::strategies::maven_39::pom_resolver::append_managed_artifacts;
use crate::resolver::strategies::maven_39::resolve_plugin_closure;
use crate::wrapper::plugins;
use std::collections::HashSet;
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
        3,
        "base(2) + plugin(root)。optional/test はスキップされる"
    );
    for expected in [&base_root, &base_child, &plugin_root] {
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

    // plugin（test 依存はスキップされるため root のみ）
    let plugin_root = ArtifactCoordinates::new(
        "org.plugin".to_string(),
        "tool".to_string(),
        "1.0.0".to_string(),
    );
    store_pom(&cache, &plugin_root, &[]);

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
    // test/optional は探索しないため root のみ
    store_pom(&cache, &plugin_root, &[]);

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

/// wrapper の download_plan 構築（base_closure + plugin_closure + plugin seeds + managed_artifacts）が
/// フィクスチャ 62 件と一致することを検証する。
#[test]
fn wrapper_download_plan_matches_fixture_with_plugin_seeds() {
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

    // root POM にフィクスチャ全件を列挙（compile スコープで直接依存にして閉包を再現）
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

    // ほかの座標は空POMを格納して存在チェックを満たす。
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

    // プラグイン seeds も解決できるよう空POMを用意
    for plugin in plugins::standard_plugins() {
        cache
            .store_pom(
                &plugin.to_artifact(),
                "<project><modelVersion>4.0.0</modelVersion></project>",
            )
            .expect("store plugin pom");
    }

    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let base_closure = pom_resolver::resolve_union_per_root(&resolver, &[root.clone()], ClosureOptions::base())
        .expect("base closure");
    let plugin_roots: Vec<ArtifactCoordinates> = plugins::standard_plugins()
        .iter()
        .map(|p| p.to_artifact())
        .collect();
    let plugin_closure = resolve_plugin_closure(&resolver, &plugin_roots).expect("plugin closure");

    let managed: Vec<ArtifactCoordinates> = plugins::managed_artifacts().to_vec();

    let mut download_plan: Vec<ArtifactCoordinates> = Vec::new();
    let mut seen: std::collections::HashSet<(String, String, Option<String>, String)> =
        std::collections::HashSet::new();
    pom_resolver::append_artifacts(&mut download_plan, &base_closure, &mut seen);
    pom_resolver::append_artifacts(&mut download_plan, &plugin_closure, &mut seen);
    pom_resolver::append_artifacts(&mut download_plan, &plugin_roots, &mut seen);
    append_managed_artifacts(&mut download_plan, &managed, &mut seen);

    let plan_set: std::collections::HashSet<String> = download_plan
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
        "wrapper download_plan should match Maven baseline (missing={missing:?}, extra={extra:?})"
    );
}

/// RED: プラグインのコンパイル依存（例: flexmark-all）を download_plan に含めてはならない。
/// プラグイン POM に重い依存を仕込んだ場合でも、baseline(62件)から膨張しないことを検証する。
#[test]
fn wrapper_download_plan_does_not_include_plugin_transitives_red() {
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

    let flexmark = ArtifactCoordinates::new(
        "com.vladsch.flexmark".to_string(),
        "flexmark-all".to_string(),
        "0.42.14".to_string(),
    );

    // baseline 座標は空POM、プラグイン座標だけ flexmark を依存に付与。
    for coord in &coords {
        let deps_xml = if coord.group_id == "org.apache.maven.plugins" {
            format!(
                r#"
                <dependencies>
                  <dependency>
                    <groupId>{}</groupId>
                    <artifactId>{}</artifactId>
                    <version>{}</version>
                  </dependency>
                </dependencies>
            "#,
                flexmark.group_id, flexmark.artifact_id, flexmark.version
            )
        } else {
            "<dependencies></dependencies>".to_string()
        };
        let pom = format!(
            r#"<project><modelVersion>4.0.0</modelVersion>{}</project>"#,
            deps_xml
        );
        cache.store_pom(coord, &pom).expect("store fixture pom");
    }
    cache
        .store_pom(
            &flexmark,
            "<project><modelVersion>4.0.0</modelVersion></project>",
        )
        .expect("store flexmark pom");

    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let base_closure = pom_resolver::resolve_union_per_root(&resolver, &coords, ClosureOptions::base())
        .expect("base closure");
    let plugin_roots: Vec<ArtifactCoordinates> = coords
        .iter()
        .filter(|c| c.group_id == "org.apache.maven.plugins")
        .cloned()
        .collect();
    let plugin_closure = resolve_plugin_closure(&resolver, &plugin_roots).expect("plugin closure");

    let mut download_plan: Vec<ArtifactCoordinates> = Vec::new();
    let mut seen: std::collections::HashSet<(String, String, Option<String>, String)> =
        std::collections::HashSet::new();
    pom_resolver::append_artifacts(&mut download_plan, &base_closure, &mut seen);
    pom_resolver::append_artifacts(&mut download_plan, &plugin_closure, &mut seen);
    pom_resolver::append_artifacts(&mut download_plan, &plugin_roots, &mut seen);

    let plan_set: std::collections::HashSet<String> = download_plan
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
        "plugin transitives should not bloat download_plan; missing={missing:?}, extra={extra:?}"
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
fn wrapper_base_closure_should_match_commons_lang3_fixture_but_drops_scopes() {
    // wrapper-default と同じ ClosureOptions::base() で commons-lang3 を起点にすると、provided/test が落ちて missing が出ることを検出する RED テスト。
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

    // root POM に fixture 全件を dependencies として列挙（commons-lang3 自身を含む）
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

    // wrapper-default での実装に合わせ、provided/test を辿り複数版を許容するオプションを使用する
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
        "wrapper base closure should match Maven baseline but missing={missing:?}, extra={extra:?}"
    );
}

#[test]
fn base_closure_includes_provided_and_skips_test_scope() {
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = ArtifactCoordinates::new(
        "org.example".to_string(),
        "root".to_string(),
        "1.0.0".to_string(),
    );
    let provided = ArtifactCoordinates::new(
        "org.example".to_string(),
        "provided-lib".to_string(),
        "1.0.0".to_string(),
    );
    let test_dep = ArtifactCoordinates::new(
        "org.example".to_string(),
        "test-lib".to_string(),
        "1.0.0".to_string(),
    );
    let compile_dep = ArtifactCoordinates::new(
        "org.example".to_string(),
        "compile-lib".to_string(),
        "1.0.0".to_string(),
    );

    store_pom(
        &cache,
        &root,
        &[
            (
                &provided.group_id,
                &provided.artifact_id,
                &provided.version,
                Some("provided"),
                false,
            ),
            (
                &test_dep.group_id,
                &test_dep.artifact_id,
                &test_dep.version,
                Some("test"),
                false,
            ),
            (
                &compile_dep.group_id,
                &compile_dep.artifact_id,
                &compile_dep.version,
                None,
                false,
            ),
        ],
    );
    store_pom(&cache, &provided, &[]);
    store_pom(&cache, &test_dep, &[]);
    store_pom(&cache, &compile_dep, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[root.clone()], ClosureOptions::base())
        .expect("base closure");

    let ids: std::collections::HashSet<String> = closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    assert!(
        ids.contains(&format!(
            "{}:{}:{}",
            compile_dep.group_id, compile_dep.artifact_id, compile_dep.version
        )),
        "base closure should include compile scope dep"
    );
    assert!(
        ids.contains(&format!(
            "{}:{}:{}",
            provided.group_id, provided.artifact_id, provided.version
        )),
        "base closure (dependency:resolve 相当) は provided を含める"
    );
    assert!(
        !ids.contains(&format!(
            "{}:{}:{}",
            test_dep.group_id, test_dep.artifact_id, test_dep.version
        )),
        "base closure should exclude test scope dep"
    );
}

#[test]
fn base_closure_keeps_multiple_versions_with_keep_all_conflict() {
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
    let v1 = ArtifactCoordinates::new(
        "org.example".to_string(),
        "lib".to_string(),
        "1.0.0".to_string(),
    );
    let v2 = ArtifactCoordinates::new(
        "org.example".to_string(),
        "lib".to_string(),
        "2.0.0".to_string(),
    );

    // root depends on v1; v1 depends on v2. KeepAll/multi-version should retain both.
    store_pom(
        &cache,
        &root,
        &[(&v1.group_id, &v1.artifact_id, &v1.version, None, false)],
    );
    store_pom(
        &cache,
        &v1,
        &[(&v2.group_id, &v2.artifact_id, &v2.version, None, false)],
    );
    store_pom(&cache, &v2, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[root.clone()], ClosureOptions::base())
        .expect("base closure");

    let ids: std::collections::HashSet<String> = closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    assert!(
        ids.contains(&format!(
            "{}:{}:{}",
            v1.group_id, v1.artifact_id, v1.version
        )),
        "keep-all should keep shallow version"
    );
    assert!(
        ids.contains(&format!(
            "{}:{}:{}",
            v2.group_id, v2.artifact_id, v2.version
        )),
        "keep-all should also retain deeper version of same GA"
    );
}

#[test]
fn wrapper_closure_matches_commons_lang3_fixture_multiple_versions() {
    // wrapper が Maven 実績どおり provided/test を辿り複数版を保持できることを確認する（commons-lang3 フィクスチャ 62件と一致）。
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

    // root POM にフィクスチャ全件を列挙
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
        .expect("wrapper closure");

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
        "wrapper closure should reproduce Maven fixture (missing={missing:?}, extra={extra:?})"
    );
}

/// RED: plugin seeds を使わない現行挙動では、Maven baseline に含まれる標準プラグイン JAR が download_plan から欠落することを検出する。
#[test]
fn wrapper_download_plan_missing_standard_plugins_without_plugin_seeds_red() {
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/maven_baseline_jars.txt");
    let content = std::fs::read_to_string(&fixture_path).expect("fixture file should be readable");
    let coords: Vec<ArtifactCoordinates> = content
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(parse_fixture_line)
        .collect();

    // commons-lang3 を root とし、他の座標は空POMで格納しておく
    let root = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-lang3".to_string(),
        "3.14.0".to_string(),
    );
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    for coord in &coords {
        let pom = if coord == &root {
            "<project><modelVersion>4.0.0</modelVersion><dependencies></dependencies></project>"
        } else {
            "<project><modelVersion>4.0.0</modelVersion></project>"
        };
        cache.store_pom(coord, pom).expect("store pom");
    }

    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    // plugin seeds を投入しない現在の実装に合わせて download_plan を構築
    let base_closure = resolver
        .resolve_closure_with_options(&[root.clone()], ClosureOptions::plugin_download())
        .expect("base closure");
    let mut download_plan = base_closure.clone();
    let mut seen: std::collections::HashSet<(String, String, Option<String>, String)> =
        download_plan
            .iter()
            .map(|c| {
                (
                    c.group_id.clone(),
                    c.artifact_id.clone(),
                    c.classifier.clone(),
                    c.version.clone(),
                )
            })
            .collect();
    append_managed_artifacts(
        &mut download_plan,
        crate::wrapper::plugins::managed_artifacts(),
        &mut seen,
    );

    let set: std::collections::HashSet<String> = download_plan
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    // baseline に含まれる標準プラグインの一部が欠落していることをREDで検出する
    let missing_plugins: Vec<String> = vec![
        "org.apache.maven.plugins:maven-compiler-plugin:3.13.0",
        "org.apache.maven.plugins:maven-surefire-plugin:3.2.5",
        "org.apache.maven.plugins:maven-jar-plugin:3.4.1",
    ]
    .into_iter()
    .filter(|k| !set.contains(*k))
    .map(|s| s.to_string())
    .collect();

    assert!(
        !missing_plugins.is_empty(),
        "standard plugins should be missing without plugin seeds, but all were present unexpectedly"
    );
}

/// base オプションが Maven 実績フィクスチャ 62 件と一致することを確認する。
#[test]
fn base_closure_matches_commons_lang3_fixture() {
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

    // root POM にフィクスチャ全件を列挙する（provided/test/複数版を含む Maven 実績を再現）
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
        .resolve_closure_with_options(&[root.clone()], ClosureOptions::base())
        .expect("base closure");

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
        "base closure should match Maven fixture: missing={missing:?}, extra={extra:?}"
    );
}

#[test]
fn wrapper_closure_includes_provided_and_skips_test_dependencies_for_commons_lang3() {
    // wrapper が Maven 実績どおり provided を seeds に含め、test を除外することを検証する。
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-lang3".to_string(),
        "3.14.0".to_string(),
    );
    let provided = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-text".to_string(),
        "1.12.0".to_string(),
    );
    let test_dep = ArtifactCoordinates::new(
        "org.junit.jupiter".to_string(),
        "junit-jupiter".to_string(),
        "5.10.0".to_string(),
    );

    store_pom(
        &cache,
        &root,
        &[
            (
                &provided.group_id,
                &provided.artifact_id,
                &provided.version,
                Some("provided"),
                false,
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
    store_pom(&cache, &provided, &[]);
    store_pom(&cache, &test_dep, &[]);

    let closure =
        resolve_plugin_closure(&resolver, &[root.clone()]).expect("wrapper closure");
    let ids: std::collections::HashSet<String> = closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    assert!(
        ids.contains(&format!(
            "{}:{}:{}",
            root.group_id, root.artifact_id, root.version
        )),
        "root version should remain"
    );
    assert!(
        !ids.contains(&format!(
            "{}:{}:{}",
            provided.group_id, provided.artifact_id, provided.version
        )),
        "provided scope should be excluded for plugin downloads"
    );
    assert!(
        !ids.contains(&format!(
            "{}:{}:{}",
            test_dep.group_id, test_dep.artifact_id, test_dep.version
        )),
        "wrapper should skip test dependency"
    );
}

#[test]
fn plugin_download_excludes_provided_and_deduplicates_versions_for_commons_lang3() {
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-lang3".to_string(),
        "3.14.0".to_string(),
    );
    let older = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-lang3".to_string(),
        "3.8.1".to_string(),
    );
    let provided = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-text".to_string(),
        "1.12.0".to_string(),
    );
    let test_dep = ArtifactCoordinates::new(
        "org.example".to_string(),
        "junit-lite".to_string(),
        "1.0.0".to_string(),
    );

    // root depends on: same GA older version (to check dedup), provided, test.
    let deps = [
        (
            &*older.group_id,
            &*older.artifact_id,
            &*older.version,
            None,
            false,
        ),
        (
            &*provided.group_id,
            &*provided.artifact_id,
            &*provided.version,
            Some("provided"),
            false,
        ),
        (
            &*test_dep.group_id,
            &*test_dep.artifact_id,
            &*test_dep.version,
            Some("test"),
            false,
        ),
    ];
    store_pom(&cache, &root, &deps);
    store_pom(&cache, &older, &[]);
    store_pom(&cache, &provided, &[]);
    store_pom(&cache, &test_dep, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[root.clone()], ClosureOptions::plugin_download())
        .expect("plugin_download closure");

    let ids: std::collections::HashSet<String> = closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    assert!(
        ids.contains(&format!(
            "{}:{}:{}",
            root.group_id, root.artifact_id, root.version
        )),
        "current root version should remain"
    );
    assert!(
        ids.contains(&format!(
            "{}:{}:{}",
            older.group_id, older.artifact_id, older.version
        )),
        "allowlisted secondary version should remain for commons-lang3"
    );
    assert!(
        !ids.contains(&format!(
            "{}:{}:{}",
            test_dep.group_id, test_dep.artifact_id, test_dep.version
        )),
        "plugin_download should skip test scope dependencies"
    );
}

#[test]
fn plugin_download_skips_optional_and_test_dependencies() {
    // optional と test を除外することを検証する
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

    let plugin_closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::plugin_download())
        .expect("plugin");

    let plugin_ids: std::collections::HashSet<String> = plugin_closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    assert!(
        !plugin_ids.contains(&format!(
            "{}:{}:{}",
            optional_dep.group_id, optional_dep.artifact_id, optional_dep.version
        )),
        "plugin_download should skip optional dependency"
    );
    assert!(
        !plugin_ids.contains(&format!(
            "{}:{}:{}",
            test_dep.group_id, test_dep.artifact_id, test_dep.version
        )),
        "plugin_download should skip test scope dependency"
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
fn plugin_download_filters_to_baseline_versions_for_commons_ga() {
    // dependencyManagement で上書きされた版に収束し、余剰版（commons-text 1.10/1.11 や commons-lang3 3.12/3.13）が入らないことを検証
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = ArtifactCoordinates::new(
        "org.example".to_string(),
        "plugin".to_string(),
        "1.0.0".to_string(),
    );
    let lang_old = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-lang3".to_string(),
        "3.12.0".to_string(),
    );
    let lang_baseline = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-lang3".to_string(),
        "3.14.0".to_string(),
    );
    let text_old = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-text".to_string(),
        "1.11.0".to_string(),
    );
    let text_baseline = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-text".to_string(),
        "1.12.0".to_string(),
    );

    let root_pom = format!(
        r#"
        <project>
          <modelVersion>4.0.0</modelVersion>
          <groupId>{}</groupId>
          <artifactId>{}</artifactId>
          <version>{}</version>
          <dependencyManagement>
            <dependencies>
              <dependency>
                <groupId>{}</groupId>
                <artifactId>{}</artifactId>
                <version>{}</version>
              </dependency>
              <dependency>
                <groupId>{}</groupId>
                <artifactId>{}</artifactId>
                <version>{}</version>
              </dependency>
            </dependencies>
          </dependencyManagement>
          <dependencies>
            <dependency>
              <groupId>{}</groupId>
              <artifactId>{}</artifactId>
              <version>{}</version>
            </dependency>
            <dependency>
              <groupId>{}</groupId>
              <artifactId>{}</artifactId>
              <version>{}</version>
            </dependency>
          </dependencies>
        </project>
        "#,
        root.group_id,
        root.artifact_id,
        root.version,
        lang_baseline.group_id,
        lang_baseline.artifact_id,
        lang_baseline.version,
        text_baseline.group_id,
        text_baseline.artifact_id,
        text_baseline.version,
        lang_old.group_id,
        lang_old.artifact_id,
        lang_old.version,
        text_old.group_id,
        text_old.artifact_id,
        text_old.version
    );
    cache.store_pom(&root, &root_pom).expect("store root pom");

    // 依存の POM は空でよい（存在確認のみ）
    for coords in [&lang_old, &lang_baseline, &text_old, &text_baseline] {
        cache
            .store_pom(
                coords,
                "<project><modelVersion>4.0.0</modelVersion></project>",
            )
            .expect("store dep");
    }

    let closure = resolver
        .resolve_closure_with_options(&[root.clone()], ClosureOptions::plugin_download())
        .expect("closure");
    let versions: std::collections::HashMap<(String, String), std::collections::HashSet<String>> =
        closure.iter().fold(
            std::collections::HashMap::new(),
            |mut acc, c| {
                acc.entry((c.group_id.clone(), c.artifact_id.clone()))
                    .or_default()
                    .insert(c.version.clone());
                acc
            },
        );

    let lang_versions = versions
        .get(&(lang_baseline.group_id.clone(), lang_baseline.artifact_id.clone()))
        .cloned()
        .unwrap_or_default();
    let text_versions = versions
        .get(&(text_baseline.group_id.clone(), text_baseline.artifact_id.clone()))
        .cloned()
        .unwrap_or_default();

    assert!(
        lang_versions == std::collections::HashSet::from([lang_baseline.version.clone()]),
        "commons-lang3 should resolve to baseline version only, got {:?}",
        lang_versions
    );
    assert!(
        text_versions == std::collections::HashSet::from([text_baseline.version.clone()]),
        "commons-text should resolve to baseline version only, got {:?}",
        text_versions
    );
}

#[test]
fn plugin_download_deduplicates_ga_by_nearest_version() {
    // PluginDependenciesResolver 挙動: 管理版が無ければ最近接のみ残し、GA は 1 版に収束する。
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

    // root depends on both versions (順序: older, newer)。Nearest で 1 版に収束する。
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

    let closure =
        resolve_plugin_closure(&resolver, &[root]).expect("closure with plugin filtering applied");
    let versions: std::collections::HashSet<String> = closure
        .iter()
        .filter(|c| c.group_id == newer.group_id && c.artifact_id == newer.artifact_id)
        .map(|c| c.version.clone())
        .collect();

    assert_eq!(
        versions,
        std::collections::HashSet::from([older.version.clone()]),
        "Nearest conflict should keep the first/nearest version when no managed version is present"
    );
}

#[test]
fn plugin_runtime_download_skips_provided_scope_dependencies() {
    // Maven 本体が提供する provided 依存はプラグイン実行時には取得しない（ローカルMavenが保持するためダウンロード不要）。
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let plugin_root = ArtifactCoordinates::new(
        "org.example".to_string(),
        "tool".to_string(),
        "1.0.0".to_string(),
    );
    let provided_dep = ArtifactCoordinates::new(
        "org.example".to_string(),
        "provided-lib".to_string(),
        "1.0.0".to_string(),
    );
    let compile_dep = ArtifactCoordinates::new(
        "org.example".to_string(),
        "compile-lib".to_string(),
        "1.0.0".to_string(),
    );

    store_pom(
        &cache,
        &plugin_root,
        &[
            (
                &provided_dep.group_id,
                &provided_dep.artifact_id,
                &provided_dep.version,
                Some("provided"),
                false,
            ),
            (
                &compile_dep.group_id,
                &compile_dep.artifact_id,
                &compile_dep.version,
                None,
                false,
            ),
        ],
    );
    store_pom(&cache, &provided_dep, &[]);
    store_pom(&cache, &compile_dep, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[plugin_root], ClosureOptions::plugin_runtime_nearest())
        .expect("closure");
    let ids: HashSet<String> = closure
        .iter()
        .map(|c| format!("{}:{}", c.group_id, c.artifact_id))
        .collect();
    assert!(
        ids.contains("org.example:compile-lib"),
        "compile-scope dependency should be kept"
    );
    assert!(
        !ids.contains("org.example:provided-lib"),
        "provided-scope dependency should be skipped for plugin runtime (Maven本体で提供されるためダウンロード不要)"
    );
}

#[test]
fn plugin_execution_full_deduplicates_versions_and_skips_provided() {
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let plugin_root = ArtifactCoordinates::new(
        "org.example".to_string(),
        "tooling".to_string(),
        "1.0.0".to_string(),
    );
    let provided_dep = ArtifactCoordinates::new(
        "org.example".to_string(),
        "provided-lib".to_string(),
        "1.0.0".to_string(),
    );
    let nearest = ArtifactCoordinates::new(
        "org.example".to_string(),
        "shared-lib".to_string(),
        "2.0.0".to_string(),
    );
    let deeper_newer = ArtifactCoordinates::new(
        "org.example".to_string(),
        "shared-lib".to_string(),
        "3.0.0".to_string(),
    );

    // root -> nearest (compile) -> deeper_newer (compile), plus one provided dependency
    store_pom(
        &cache,
        &plugin_root,
        &[
            (
                &nearest.group_id,
                &nearest.artifact_id,
                &nearest.version,
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
    store_pom(
        &cache,
        &nearest,
        &[(
            &deeper_newer.group_id,
            &deeper_newer.artifact_id,
            &deeper_newer.version,
            None,
            false,
        )],
    );
    store_pom(&cache, &deeper_newer, &[]);
    store_pom(&cache, &provided_dep, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[plugin_root.clone()], ClosureOptions::plugin_execution_full())
        .expect("execution closure");

    let ids: HashSet<String> = closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    assert!(
        ids.contains(&format!(
            "{}:{}:{}",
            nearest.group_id, nearest.artifact_id, nearest.version
        )),
        "nearest compile dependency should be retained"
    );
    assert!(
        !ids.contains(&format!(
            "{}:{}:{}",
            deeper_newer.group_id, deeper_newer.artifact_id, deeper_newer.version
        )),
        "KeepAll をやめて最近接優先にしたため、深い階層の別バージョンは除外される"
    );
    assert!(
        !ids.contains(&format!(
            "{}:{}:{}",
            provided_dep.group_id, provided_dep.artifact_id, provided_dep.version
        )),
        "provided 依存は Maven 本体が提供するため download plan から除外する"
    );
}

#[test]
fn plugin_closure_skips_transitives_for_non_dependency_plugins() {
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let dependency_plugin = ArtifactCoordinates::new(
        "org.apache.maven.plugins".to_string(),
        "maven-dependency-plugin".to_string(),
        "3.7.0".to_string(),
    );
    let reporting_dep = ArtifactCoordinates::new(
        "org.example".to_string(),
        "reporting-lib".to_string(),
        "1.0.0".to_string(),
    );
    store_pom(
        &cache,
        &dependency_plugin,
        &[(
            &reporting_dep.group_id,
            &reporting_dep.artifact_id,
            &reporting_dep.version,
            None,
            false,
        )],
    );
    store_pom(&cache, &reporting_dep, &[]);

    let jar_plugin = ArtifactCoordinates::new(
        "org.apache.maven.plugins".to_string(),
        "maven-jar-plugin".to_string(),
        "3.4.1".to_string(),
    );
    let jar_helper = ArtifactCoordinates::new(
        "org.example".to_string(),
        "jar-helper".to_string(),
        "1.0.0".to_string(),
    );
    store_pom(
        &cache,
        &jar_plugin,
        &[(
            &jar_helper.group_id,
            &jar_helper.artifact_id,
            &jar_helper.version,
            None,
            false,
        )],
    );
    store_pom(&cache, &jar_helper, &[]);

    let closure = resolve_plugin_closure(&resolver, &[dependency_plugin, jar_plugin])
        .expect("plugin closure");
    let ids: HashSet<String> = closure
        .iter()
        .map(|c| format!("{}:{}", c.group_id, c.artifact_id))
        .collect();

    assert!(ids.contains("org.apache.maven.plugins:maven-jar-plugin"));
    assert!(ids.contains("org.example:reporting-lib"));
    assert!(
        ids.contains("org.example:jar-helper"),
        "compile 依存は含める（provided/test のみ除外）"
    );
}

#[test]
fn resolve_union_per_root_keeps_versions_without_global_explosion() {
    // 異なる root が同一 GA の別バージョンを要求する場合でも、最近接で 1 版に収束させて爆発を防ぐ。
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let plugin_v1 = ArtifactCoordinates::new(
        "org.example".to_string(),
        "tool".to_string(),
        "1.0.0".to_string(),
    );
    let plugin_v2 = ArtifactCoordinates::new(
        "org.example".to_string(),
        "tool".to_string(),
        "2.0.0".to_string(),
    );
    let shared_v1 = ArtifactCoordinates::new(
        "org.example".to_string(),
        "shared".to_string(),
        "1.0.0".to_string(),
    );
    let shared_v2 = ArtifactCoordinates::new(
        "org.example".to_string(),
        "shared".to_string(),
        "2.0.0".to_string(),
    );

    // plugin それぞれが専用の shared バージョンを要求する。
    store_pom(
        &cache,
        &plugin_v1,
        &[(
            &shared_v1.group_id,
            &shared_v1.artifact_id,
            &shared_v1.version,
            None,
            false,
        )],
    );
    store_pom(
        &cache,
        &plugin_v2,
        &[(
            &shared_v2.group_id,
            &shared_v2.artifact_id,
            &shared_v2.version,
            None,
            false,
        )],
    );
    store_pom(&cache, &shared_v1, &[]);
    store_pom(&cache, &shared_v2, &[]);

    // 単純にまとめて解決しても最近接 1 版に収束する。
    let combined =
        resolve_plugin_closure(&resolver, &[plugin_v1.clone(), plugin_v2.clone()]).expect("combined");
    let combined_shared: HashSet<_> = combined
        .iter()
        .filter(|c| c.group_id == "org.example" && c.artifact_id == "shared")
        .map(|c| c.version.clone())
        .collect();
    assert_eq!(combined_shared.len(), 1, "shared GA should deduplicate to a single version");

    // root ごとに解決し直しても shared のバージョンは 1 つに統一される。
    let union =
        resolve_plugin_closure(&resolver, &[plugin_v1.clone(), plugin_v2.clone()]).expect("union");
    let resolved: HashSet<String> = union
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();
    assert!(resolved.contains("org.example:tool:1.0.0"));
    assert!(resolved.contains("org.example:tool:2.0.0"));
    let shared_versions: Vec<_> = resolved
        .iter()
        .filter(|id| id.starts_with("org.example:shared"))
        .cloned()
        .collect();
    assert_eq!(
        shared_versions.len(),
        1,
        "shared GA should be deduped across roots"
    );
}

#[test]
fn plugin_download_excludes_plugin_roots_from_download_plan() {
    // wrapper 実績との差分要因: plugin_roots を seeds に入れると 281件に膨張する。
    // download_plan_size が plugin_roots を含めない場合に 62 に収束することを期待する RED テスト。
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    // commons-lang3 フィクスチャ座標を root にし、依存を自力展開せず download_plan の件数だけを見る。
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
    let deps_xml = coords
        .iter()
        .map(|c| {
            format!(
                "<dependency><groupId>{}</groupId><artifactId>{}</artifactId><version>{}</version></dependency>",
                c.group_id, c.artifact_id, c.version
            )
        })
        .collect::<Vec<_>>()
        .join("");
    let root_pom = format!(
        "<project><modelVersion>4.0.0</modelVersion><groupId>{}</groupId><artifactId>{}</artifactId><version>{}</version><dependencies>{}</dependencies></project>",
        root.group_id, root.artifact_id, root.version, deps_xml
    );
    cache.store_pom(&root, &root_pom).expect("store root pom");
    for coord in &coords {
        if coord == &root {
            continue;
        }
        cache
            .store_pom(
                coord,
                "<project><modelVersion>4.0.0</modelVersion></project>",
            )
            .expect("store fixture pom");
    }

    // plugin_download だが、実装で plugin_roots を seeds に含めないことを期待（provided/test を含めた baseline 件数を維持）。
    let closure =
        resolve_plugin_closure(&resolver, &[root]).expect("plugin closure with filtering");

    assert_eq!(
        closure.len(),
        coords.len(),
        "plugin_roots を seeds に含めずとも baseline 件数と一致する想定"
    );
}

#[test]
fn plugin_download_excludes_provided_scope_dependencies() {
    // PluginDependenciesResolver 相当: provided は除外する（commons-text は含めない）
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-lang3".to_string(),
        "3.14.0".to_string(),
    );
    let provided = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-text".to_string(),
        "1.12.0".to_string(),
    );

    let root_pom = format!(
        r#"
        <project>
          <modelVersion>4.0.0</modelVersion>
          <groupId>{}</groupId>
          <artifactId>{}</artifactId>
          <version>{}</version>
          <dependencies>
            <dependency>
              <groupId>{}</groupId>
              <artifactId>{}</artifactId>
              <version>{}</version>
              <scope>provided</scope>
            </dependency>
          </dependencies>
        </project>
        "#,
        root.group_id,
        root.artifact_id,
        root.version,
        provided.group_id,
        provided.artifact_id,
        provided.version
    );
    cache.store_pom(&root, &root_pom).expect("store root");
    cache
        .store_pom(
            &provided,
            "<project><modelVersion>4.0.0</modelVersion></project>",
        )
        .expect("store provided");

    let closure =
        resolve_plugin_closure(&resolver, &[root]).expect("closure with plugin filtering");
    let ids: std::collections::HashSet<String> = closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    assert!(
        !ids.contains(&format!(
            "{}:{}:{}",
            provided.group_id, provided.artifact_id, provided.version
        )),
        "provided scope dependencies should be skipped for plugin download"
    );
}

#[test]
fn plugin_download_skips_provided_and_test_scope() {
    // regression guard: plugin_download では provided/test を除外する（PluginDependenciesResolver 相当）
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
    let provided_dep = ArtifactCoordinates::new(
        "org.example".to_string(),
        "provided-lib".to_string(),
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
                &provided_dep.group_id,
                &provided_dep.artifact_id,
                &provided_dep.version,
                Some("provided"),
                false,
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
    store_pom(&cache, &provided_dep, &[]);
    store_pom(&cache, &test_dep, &[]);

    let closure = resolver
        .resolve_closure_with_options(&[root], ClosureOptions::plugin_download())
        .expect("closure");
    let ids: std::collections::HashSet<String> = closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    assert!(
        !ids.contains(&format!(
            "{}:{}:{}",
            provided_dep.group_id, provided_dep.artifact_id, provided_dep.version
        )),
        "provided is excluded for plugin downloads"
    );
    assert!(
        !ids.contains(&format!(
            "{}:{}:{}",
            test_dep.group_id, test_dep.artifact_id, test_dep.version
        )),
        "test scope is excluded for plugin downloads"
    );
}

#[test]
fn plugin_download_baseline_count_matches_fixture() {
    // provided/test を除外するため baseline 62 から1件減る前提
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
        "dependency:resolve 実績に合わせ baseline 62 件と一致すること"
    );
}

/// RED: プラグイン seed が不要な推移依存（ここでは古い commons-lang3 3.7.0）を巻き込むことを検出する。
#[test]
fn plugin_seed_brings_in_unexpected_transitive_versions_red() {
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    // root: commons-lang3 3.14.0（dependency:resolve 基準）
    let root = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-lang3".to_string(),
        "3.14.0".to_string(),
    );
    store_pom(&cache, &root, &[]);

    // plugin seed: maven-compiler-plugin が古い commons-lang3 3.7.0 を依存に持つと仮定
    let plugin = ArtifactCoordinates::new(
        "org.apache.maven.plugins".to_string(),
        "maven-compiler-plugin".to_string(),
        "3.13.0".to_string(),
    );
    let old_lang = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-lang3".to_string(),
        "3.7.0".to_string(),
    );
    store_pom(
        &cache,
        &plugin,
        &[(
            &old_lang.group_id,
            &old_lang.artifact_id,
            &old_lang.version,
            None,
            false,
        )],
    );
    store_pom(&cache, &old_lang, &[]);

    // 実装では plugin_roots が seeds に含まれるため、plugin_download が余計な古い版を拾うと想定。
    let plugin_closure = resolver
        .resolve_closure_with_options(&[plugin.clone()], ClosureOptions::plugin_seed())
        .expect("plugin closure");
    let set: std::collections::HashSet<String> = plugin_closure
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    assert!(
        !set.contains(&format!(
            "{}:{}:{}",
            old_lang.group_id, old_lang.artifact_id, old_lang.version
        )),
        "plugin seeds should NOT introduce unrelated commons-lang3 3.7.0, but it was present: {set:?}"
    );
}

/// RED: embedded managed_artifacts（commons-text 1.12.0）が download_plan に入らず missing を生むことを検出する。
#[test]
fn managed_artifacts_should_be_included_in_download_plan_red() {
    let temp = tempdir().expect("temp dir");
    let cache = Arc::new(DependencyCache::with_dir(temp.path().join("cache")).expect("cache"));
    let runtime = RuntimeBuilder::new_current_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), Vec::new());

    let root = ArtifactCoordinates::new(
        "org.apache.commons".to_string(),
        "commons-lang3".to_string(),
        "3.14.0".to_string(),
    );
    store_pom(&cache, &root, &[]);

    // managed_artifacts の POM を空で格納（ダウンロード不要で解決できるようにする）
    let managed = crate::wrapper::plugins::managed_artifacts();
    for coords in managed {
        cache
            .store_pom(
                coords,
                "<project><modelVersion>4.0.0</modelVersion></project>",
            )
            .expect("store managed pom");
    }

    let closure = resolver
        .resolve_closure_with_options(&[root.clone()], ClosureOptions::plugin_download())
        .expect("closure");

    let mut download_plan = closure.clone();
    let mut seen: std::collections::HashSet<(String, String, Option<String>, String)> =
        download_plan
            .iter()
            .map(|c| {
                (
                    c.group_id.clone(),
                    c.artifact_id.clone(),
                    c.classifier.clone(),
                    c.version.clone(),
                )
            })
            .collect();
    append_managed_artifacts(&mut download_plan, managed, &mut seen);

    let set: std::collections::HashSet<String> = download_plan
        .iter()
        .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
        .collect();

    for coords in managed {
        let key = format!(
            "{}:{}:{}",
            coords.group_id, coords.artifact_id, coords.version
        );
        assert!(
            set.contains(&key),
            "managed artifact {key} should be included in download plan but was missing: {set:?}"
        );
    }
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
