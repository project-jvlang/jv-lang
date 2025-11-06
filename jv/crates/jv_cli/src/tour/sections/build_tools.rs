use std::collections::HashMap;
use std::io::Write;

use anyhow::Result;
use jv_build::{BuildConfig, JavaTarget, SampleConfig};
use jv_pm::{BuildInfo, Manifest, PackageInfo, PackageManager, ProjectSection};

/// Render the Build Tools learning module demonstrating the full workflow.
pub fn render<W: Write>(writer: &mut W) -> Result<()> {
    writeln!(writer, "--- ビルドツール体験セクション ---")?;
    writeln!(
        writer,
        "jv CLI のビルドツール群を使ってプロジェクト初期化から javac 連携までを 5 ステップで体験します。"
    )?;
    writeln!(
        writer,
        "Hands-on では jv.toml の設定、依存管理、JDK 25 固定、そして `jv build` の内部で発行される `javac --release 25` を追跡します。"
    )?;

    let manifest = sample_manifest();
    let manifest_toml = render_manifest(&manifest);
    let build_config = sample_build_config();
    let package_manager = PackageManager::new("https://registry.jvlang.dev".into());
    let dependency_summary = describe_dependency_resolution(&package_manager, &manifest);

    // STEP 1: jv init
    writeln!(writer, "\n[STEP 1] プロジェクト初期化")?;
    writeln!(writer, "コマンド例: jv init hello-jv-tour")?;
    writeln!(
        writer,
        "- 空ディレクトリで `jv init` を実行すると、基本的な `jv.toml` と `src/main.jv` が生成されます。"
    )?;
    writeln!(
        writer,
        "- `package.name` にはプロジェクト名が入り、バージョンは `0.1.0` で初期化されます。"
    )?;

    // STEP 2: manifest editing
    writeln!(writer, "\n[STEP 2] 依存関係と JDK バージョンの設定")?;
    writeln!(writer, "コマンド例: jv add junit:junit 5.10.2")?;
    writeln!(
        writer,
        "- `jv add` で JUnit を追加すると依存関係セクションが更新され、同時に Java 25 固定の build 設定を追記します。"
    )?;
    writeln!(writer, "- 更新後の `jv.toml`:")?;
    writeln!(writer, "```toml")?;
    writeln!(writer, "{}", manifest_toml.trim())?;
    writeln!(writer, "```")?;

    // STEP 3: project structure and sample tests
    writeln!(writer, "\n[STEP 3] プロジェクト構造とサンプルテストの追加")?;
    writeln!(writer, "- 推奨ディレクトリ構成:")?;
    writeln!(writer, "```")?;
    writeln!(writer, "hello-jv-tour/")?;
    writeln!(writer, "  jv.toml")?;
    writeln!(writer, "  src/")?;
    writeln!(writer, "    main.jv")?;
    writeln!(writer, "  tests/")?;
    writeln!(writer, "    main_test.jv")?;
    writeln!(writer, "```")?;
    writeln!(
        writer,
        "- `tests/main_test.jv` に以下のような JUnit ベースの検証を追加します。"
    )?;
    writeln!(writer, "```jv")?;
    writeln!(
        writer,
        "{}",
        r#"@test fun greets_user() {
    val message = buildGreeting("Tour")
    assertEquals("Hello, Tour!", message)
}"#
    )?;
    writeln!(writer, "```")?;

    // STEP 4: dependency resolution preview
    writeln!(writer, "\n[STEP 4] 依存解決とビルド準備")?;
    writeln!(writer, "コマンド例: jv build --prepare")?;
    writeln!(
        writer,
        "- `jv build` は内部でパッケージマネージャーを呼び出し、依存関係をダウンロードおよび検証します。"
    )?;
    writeln!(writer, "- 解析結果: {}", dependency_summary)?;
    writeln!(
        writer,
        "- 解決済みアーティファクトは `target/jv/hello-jv-tour/deps` に配置され、`javac` のクラスパスに反映されます。"
    )?;

    // STEP 5: javac integration preview
    writeln!(writer, "\n[STEP 5] javac --release 25 を用いたビルド実行")?;
    writeln!(writer, "コマンド例: jv build")?;
    writeln!(
        writer,
        "- `jv build` は `jv_build` クレートを経由して `javac --release 25` を発行し、Javaソースを `target/jv/hello-jv-tour/java` に生成します。"
    )?;
    writeln!(
        writer,
        "- 発行されるコマンド例: {}",
        javac_command_preview(
            &build_config,
            &[
                "target/jv/hello-jv-tour/java/HelloJvTour.java",
                "target/jv/hello-jv-tour/java/HelloJvTourTest.java",
            ]
        )
    )?;
    writeln!(
        writer,
        "- ビルド完了後は `java -cp target/jv/hello-jv-tour/classes:...` でテスト実行が可能です。"
    )?;

    writeln!(
        writer,
        "\n✅ これで jv プロジェクトの初期化からビルドまでの流れを把握できました。"
    )?;
    writeln!(
        writer,
        "学習メニューに戻って他のセクションやミニプロジェクトで習得内容を活用しましょう。"
    )?;

    Ok(())
}

fn sample_manifest() -> Manifest {
    let mut dependencies = HashMap::new();
    dependencies.insert("junit".to_string(), "5.10.2".to_string());

    Manifest {
        package: PackageInfo {
            name: "hello-jv-tour".to_string(),
            version: "0.1.0".to_string(),
            description: Some("Hands-on project for the jv language tour".to_string()),
            dependencies,
        },
        project: ProjectSection::default(),
        build: Some(BuildInfo {
            java_version: JavaTarget::Java25,
            ..BuildInfo::default()
        }),
    }
}

fn sample_build_config() -> BuildConfig {
    let mut config = BuildConfig::with_target(JavaTarget::Java25);
    config.output_dir = "target/jv/hello-jv-tour/classes".to_string();
    config.classpath = vec!["target/jv/hello-jv-tour/deps/junit-5.10.2.jar".to_string()];
    config.sample = SampleConfig::default();
    config
}

fn render_manifest(manifest: &Manifest) -> String {
    let mut output = String::new();
    output.push_str("[package]\n");
    output.push_str(&format!("name = \"{}\"\n", manifest.package.name));
    output.push_str(&format!("version = \"{}\"\n", manifest.package.version));
    if let Some(description) = &manifest.package.description {
        output.push_str(&format!("description = \"{}\"\n", description));
    }

    if !manifest.package.dependencies.is_empty() {
        output.push_str("\n[dependencies]\n");
        let mut deps: Vec<_> = manifest.package.dependencies.iter().collect();
        deps.sort_by(|(a, _), (b, _)| a.cmp(b));
        for (name, version) in deps {
            output.push_str(&format!("{} = \"{}\"\n", name, version));
        }
    }

    if let Some(build) = &manifest.build {
        output.push_str("\n[build]\n");
        output.push_str(&format!("java_version = \"{}\"\n", build.java_version));
    }

    output
}

fn describe_dependency_resolution(manager: &PackageManager, manifest: &Manifest) -> String {
    match manager.resolve_dependencies(manifest) {
        Ok(packages) if packages.is_empty() => {
            "依存関係はローカルキャッシュから解決されました".to_string()
        }
        Ok(packages) => format!("{} 件の依存関係を解決しました", packages.len()),
        Err(error) => format!("依存関係の解決に失敗しました: {}", error),
    }
}

fn javac_command_preview(config: &BuildConfig, sources: &[&str]) -> String {
    let mut parts = vec!["javac".to_string()];
    parts.extend(config.compiler_options.iter().cloned());
    parts.push("-d".to_string());
    parts.push(config.output_dir.clone());

    if !config.classpath.is_empty() {
        parts.push("-cp".to_string());
        parts.push(config.classpath.join(":"));
    }

    parts.extend(sources.iter().map(|path| path.to_string()));
    parts.join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn render_contains_manifest_and_javac_preview() {
        let mut buffer = Vec::new();
        render(&mut buffer).expect("render should succeed");
        let output = String::from_utf8(buffer).expect("output must be valid UTF-8");

        assert!(output.contains("jv init hello-jv-tour"));
        assert!(output.contains("[build]\njava_version = \"25\""));
        assert!(output.contains("javac --release 25"));
    }
}
