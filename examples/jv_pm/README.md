# jv_pm Phase 2 デモプロジェクト

このディレクトリは `docs/jv_pm.md` で紹介しているチュートリアル用の最小プロジェクトです。`jv add` / `jv remove` / `jv resolver` / `jv repo` と、`jv build` 完了時の自動エクスポート機能を検証する際の出発点として利用できます。

## 手順概要

1. 任意の作業場所へコピーします。
   ```bash
   cp -r examples/jv_pm my-jvpm-demo
   cd my-jvpm-demo
   ```
2. 依存関係を追加します。
   ```bash
   jv add org.example:demo:1.0.0 --non-interactive
   ```
3. 生成された `jv.lock` と `target/java-project` を確認します。
   ```bash
   jv build --java-only src/main.jv
   ls target/java-project
   ```
4. 依存解決戦略やリポジトリ設定を試します。
   ```bash
   jv resolver list
   jv repo list --global
   jv repo mirror central https://cache.example.com --name corp-cache --global
   ```
5. OUTPUT_DIR を変更したい場合は `jv.toml` の `project.output.directory` を編集し、再度 `jv build` を実行します。

詳細な説明やサンプル出力は `docs/jv_pm.md` と `jv help` コマンドを参照してください。
