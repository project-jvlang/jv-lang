# CLI Project Structure / CLIプロジェクト構造

## Overview / 概要
The `cli-project-structure` initiative introduces a manifest-driven project discovery flow for the `jv` CLI. `jv.toml` now governs root detection, source enumeration, and output management so commands can run from any subdirectory without extra flags.
`cli-project-structure` イニシアティブは、`jv` CLI にマニフェスト主導のプロジェクト解決フローを導入します。`jv.toml` がルート検出・ソース列挙・出力管理を統制し、どのサブディレクトリからでも追加フラグなしでコマンドを実行できます。

## Output Management / 出力管理
- **Target-specific directories / ターゲット別ディレクトリ**: Build artifacts are written under `<project>/<output>/<javaXX>`. For example, `out/java/java25` for Java 25 and `out/java/java21` for Java 21 builds.
- **Clean builds / クリーンビルド**: Use `--clean` to remove the target directory before compiling. Manifest defaults via `project.output.clean = true` are also honoured.
- **Automatic cleanup / 自動クリーンアップ**: If the build pipeline fails, partially generated outputs are removed to keep the workspace tidy.

## Usage / 使い方
```bash
# Build with manifest defaults / マニフェスト既定でビルド
jv build

# Build to a custom directory and clean before compilation / カスタム出力先 + クリーンビルド
jv build --output ./dist --clean
```

After a successful build, the CLI reports the resolved output directory in both Japanese and English.
ビルド成功時には、CLI が解決された出力ディレクトリを日英両方で表示します。
