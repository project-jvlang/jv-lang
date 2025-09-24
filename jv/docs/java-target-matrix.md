# Javaターゲットマトリクス / Java Target Matrix

Javaターゲットを 21 と 25 のどちらに切り替えるかをまとめた運用ガイドです。テストスイートと CLI の振る舞いがターゲットに追従することを確認しながら利用してください。

This guide explains how to switch between the Java 21 and Java 25 targets, how compatibility reporting behaves, and which safeguards the test suite enforces.

## ターゲットの選択 / Selecting Targets

- `jv.toml` で既定ターゲットを設定できます。未指定の場合は `Java25` が利用され、最新機能とフォールバック検知が有効になります。
- You can pick the default target inside `jv.toml`. When omitted, the tool falls back to `Java25`, enabling modern features and compatibility tracking.

```toml
[build]
java_version = "21" # or "25"
```

- CLI では `--target` フラグが設定ファイルを上書きします。CI でのスポットチェックや互換性検証に便利です。
- The CLI `--target` flag overrides the manifest setting, which keeps CI scenarios deterministic.

```sh
jv build sample.jv --java-only --target 21
```

- ターゲットによって `--release` フラグやコード生成経路が切り替わり、Java 21 では `Arrays.asList(...).stream().toList()`、Java 25 では `List.of(...)` が生成されます。
- Build configuration updates the `--release` flag and generation strategy automatically: Java 21 emits `Arrays.asList(...).stream().toList()` while Java 25 keeps `List.of(...)`.

## 互換性レポートの読み方 / Reading Compatibility Reports

- すべてのビルドは `out/compatibility.json`（`--output` 変更時はその配下）と CLI サマリを出力します。`status.code` が `requires_higher_target` の場合は `JavaTarget` を引き上げるか依存性を調整してください。
- Each build writes `out/compatibility.json` (or the directory from `--output`) and prints a bilingual summary. When `status.code` equals `requires_higher_target`, raise the Java target or adjust dependencies.

- `warnings` 配列にはフォールバック適用理由が記録され、CLI/LSP では `JV2002` 診断として流れます。JSON 上の `highest_required_major` / `highest_required_release` を合わせて確認しましょう。
- The `warnings` array records why fallbacks were applied, and the CLI/LSP surface them as `JV2002`. Inspect `highest_required_major` / `highest_required_release` in the JSON payload for quick triage.

## テストとベンチ / Tests and Benchmarks

- 単体テスト: `cargo test -p jv_build java_target_matrix` でビルド設定と互換性アナライザの振る舞いを検証できます。
- Unit tests: run `cargo test -p jv_build java_target_matrix` to validate the build configuration and analyzer behaviour.

- 統合テスト: `cargo test --test java_target_matrix` は CLI のターゲット切り替えと JSON レポートをカバーします。
- Integration tests: `cargo test --test java_target_matrix` ensures the CLI toggles between targets and writes the compatibility JSON correctly.

- ベンチマーク: `cargo bench -p jv_codegen_java --bench target_matrix` で Java 21/25 のコード生成コストを比較できます（短時間で完了する軽量計測です）。
- Benchmarks: `cargo bench -p jv_codegen_java --bench target_matrix` compares code generation cost for Java 21 and Java 25; the run is lightweight for CI use.

## 既知の制約 / Known Constraints

- Java 21 では sealed クラスやパターンマッチのフォールバックコメントが挿入されます。生成コードが明示的に final 化されることに注意してください。
- Java 21 builds insert fallback comments and replace sealed types with `final` hierarchies; expect explicit finals in the output.

- `BuildConfig` のクラスパスは現在マニフェストから自動連携されません。互換性検証に外部 JAR を含める場合は CLI フラグやビルドスクリプトでパスを指定してください。
- The build configuration does not yet hydrate classpath entries from the manifest. Provide external JAR paths via CLI flags or build scripts when you need compatibility analysis.

- 互換性レポートはローカルファイル検査に限定されています。ネットワーク依存の解析は実行されないため、リポジトリに JAR をコミットするかキャッシュしてください。
- Compatibility analysis only inspects local artifacts; it never fetches dependencies over the network, so ensure required JARs are available locally or cached in advance.
