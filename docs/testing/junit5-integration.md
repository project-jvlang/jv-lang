# JUnit 5 テスト DSL & CLI ワークフロー

テスト DSL で記述した `.jv` ファイルを JUnit 5 互換の Java 25/21 プロジェクトへ変換するための実務ガイドです。構文ルール、命名正規化、`jv test` コマンド、@Sample データ管理までを 1 ドキュメントに集約しました。

## 1. テスト DSL の全体像

| 要素 | 役割 | Java 変換例 |
| ---- | ---- | ------------ |
| `test "display" { ... }` | 単体テスト宣言 | `@Test` + `@DisplayName` を付与した `void` メソッド |
| `test "display" [ ... ] (params) { ... }` | パラメータ化テスト | `@ParameterizedTest` + `@MethodSource` + `Stream<Arguments>` |
| `@Sample("cases.json", mode = SampleMode.Load)` | 外部データ参照 | `IrSampleDeclaration` → `static final List<Record>` / ローダー |
| `expr == expected` / `expr != expected` | アサーション簡略記法 | `Assertions.assertEquals` / `assertNotEquals` |
| `booleanExpr` | 真理値アサーション | `Assertions.assertTrue(booleanExpr)` |
| `@BeforeEach`, `@AfterEach` など | ライフサイクルアノテーション | Java コードへ透過的にコピー |

> **Tip**: `jv_parser_rowan` → `jv_ir` → `jv_codegen_java` → `jv_cli` の 4 段構成で一貫した Span / DisplayName を保持します。

## 2. DSL 構文サンプル

### 2.1 単体テスト
```jv
test "simple equality" {
    val sum = 40 + 2
    sum == 42
}
```
- `test` キーワードとダブルクォート付ディスプレイ名は必須。
- 本体はブロック式。最後の式がアサーション記法でない場合でも、`assertTrue` への自動変換を試みます。
- コメントや `@Disabled` など任意のアノテーションをブロック前に付与可能です。

### 2.2 パラメータ化テスト
```jv
test "addition dataset" [
    ["carry" 11 17 28]
    ["negative" -5 3 -2]
] (label: String, lhs: Int, rhs: Int, expected: Int) {
    val sum = lhs + rhs
    sum == expected
}
```
1. 角括弧内で行を並べると `Stream.of(Arguments.of(...))` へ展開されます。
2. パラメータ数と列数は厳密一致 (`JV5301`)。行数 0 はエラー (`JV5304`)。
3. `CallArgumentStyle` は空白区切りまたはカンマ区切りの両方に対応。

### 2.3 @Sample データセット
```jv
test "orders from S3" [@Sample("s3://bucket/orders.json", mode = SampleMode.Load)] (order) {
    order.total >= 0
}
```
- `@Sample` は既存のサンプル変換パイプラインを再利用。`sample.cache_dir` は `target/generated-tests/.sample-cache` をデフォルトに CLI が設定します。
- `mode = SampleMode.Load` ならランタイムロード用ローダークラスを生成。`Embed` なら `static final List<...>` へ埋め込み。
- ネットワークアクセスは `jv test --sample-network allow`（`jv build` と同じフラグ）で制御します。

### 2.4 アサーション記法
- `lhs == rhs` → `Assertions.assertEquals(rhs, lhs)`
- `lhs != rhs` → `Assertions.assertNotEquals(rhs, lhs)`
- 任意の boolean 式 → `Assertions.assertTrue(expr)`
- 上記以外（void 式や非 boolean 値）はそのまま残ります。非 boolean 値は `JV5305` で警告。

## 3. 命名正規化ルール

`jv_ir::naming::test_identifiers` で共通処理を行います。

1. DisplayName を Unicode NFKD 正規化。
2. ASCII 以外の文字を除去 → `identifier_components` で単語分割。
3. 非英数字を `_` に置換し、連続 `_` を折り畳み。
4. 先頭が数字なら `_` を追加。
5. `test_<slug>` をベースに `Span` 情報を含む 32bit ハッシュを `_deadbeef` 形式で付与。
6. データセットは `test_<slug>_source_<hash>`。テストメソッドと MethodSource のハッシュは DisplayName + Span が一致する限り同一です。

例: `test "足し算 ケース"` → `test_at_san_kesu_4fd2a6e1`、データセットは `test_at_san_kesu_source_4fd2a6e1`。

## 4. `jv test` コマンド

```
jv test [<entrypoint>|<dir>] [OPTIONS]
```

| オプション | 説明 |
| ---------- | ---- |
| `--clean` | `target/generated-tests` を削除してから再生成 |
| `--target <21|25>` | Java ターゲット (デフォルト: 25)。出力先は `target/generated-tests/java<target>` |
| `--maven <path>` | 既定以外の Maven 実行ファイルを使用 |
| `--sample-network allow|deny` | @Sample のネットワークアクセス方針（`jv build` と同様） |
| `--entrypoint <file>` | manifest とは別の `.jv` をテストエントリに指定 |

実行時フロー:
1. プロジェクト探索 → manifest 読み込み (`jv_cli::pipeline`).
2. テスト DSL を Java へコンパイルし、`target/generated-tests/java{target}` へ出力。
3. `JV_GENERATED_TESTS` と `JV_TEST_TARGET` 環境変数を設定した上で `mvn test` を起動。
4. Java 25/21 用ツールチェーンは `toolchains/jdk25` / `toolchains/jdk21`、もしくは `JAVA25_HOME` / `JAVA21_HOME` で自動検出。
5. 生成済みサンプルは `target/generated-tests/.sample-cache` 配下で共有。

> **重要**: `toolchains/maven` が同梱されているため、CI では `PATH=$MAVEN_HOME/bin:$JAVA*_HOME/bin:$PATH` を設定するだけで再現可能です。

### 4.1 代表的なワークフロー

```bash
# Java 25 をデフォルト設定で実行
jv test

# Java 21 向けに生成し、カスタム Maven を利用
jv test --target 21 --maven "$HOME/.maven/bin/mvn"

# テスト DSL だけをクリーンビルドし、生成物を確認
jv test --clean --target 25 && tree target/generated-tests/java25
```

### 4.2 Maven 連携で渡される環境変数

| 変数 | 内容 |
| ---- | ---- |
| `JAVA_HOME` | 選択されたターゲット（21 or 25）の toolchain パス |
| `JV_GENERATED_TESTS` | 生成された Java ソースディレクトリ |
| `JV_TEST_TARGET` | 文字列 `21` or `25` |
| `MAVEN_HOME` | `--maven` 省略時は `toolchains/maven` を自動設定 |

Surefire には `-Djv.generated.tests=<path>` が渡されるため、`pom.xml` 側で `build-helper-maven-plugin` にこのプロパティを指定すれば生成コードが即テストソースに含まれます（サンプルは CLI ユニットテスト `jv/tests/junit5_integration.rs` を参照）。

## 5. ベストプラクティス

1. **Span の一貫性**: DisplayName を変えずにテストを複製するとハッシュが重なるため、`test "Case"` を複数回使用する場合は別行へ貼り付けるだけで行番号がハッシュに影響します。
2. **`@Sample` とキャッシュ**: `mode = Load` で大容量データを参照する際は `--sample-cache-dir`（`jv build` 同等）を `jv.toml` で指定し、CI ごとにキャッシュを切り替えます。
3. **Maven プロファイル**: Java 21/25 を同じ pom で回す場合は `mvn -Djv.generated.tests=... test` のプロパティだけで済むよう、`pom.xml` に `<testSourceDirectory>${jv.generated.tests}</testSourceDirectory>` を設定します。
4. **静的解析**: 生成された Java を IDE で開く際は `target/generated-tests/java25` をソースルートに追加。`jv test --clean` で再生成すると差分が明確になります。

## 6. トラブルシューティング

| 症状 | 対処 |
| ---- | ---- |
| `JV5301` / `JV5304` | データセット行の列数をパラメータ数に合わせる。行が 0 の場合は 1 行以上追加。 |
| `JV5305` | boolean 以外の式が末尾に残っているため、`assertEquals` などの関数を明示。 |
| `JV5306` | 現行バージョンでは `@Sample` + テスト DSL のローワリングが未実装。`InlineArray` へ変換するか、次期リリースを待つ。 |
| `Java 21 toolchain not found` | `toolchains/jdk21` を配置するか `JAVA21_HOME` をエクスポート。CI では `scripts/setup-toolchains.sh` を参照。 |
| Maven 実行に失敗 | `--maven` で明示パスを指定するか、`toolchains/maven/bin` を `PATH` の先頭に追加。 |

## 7. 参考リソース

- `docs/cli-reference.md` – `jv test` コマンド詳細
- `jv/tests/junit5_integration.rs` – CLI E2E テスト実装例
- `.spec-workflow/specs/junit5-integration/design.md` – アーキテクチャ設計文書
- `jv/crates/jv_codegen_java/src/tests/junit5.rs` – 生成コードの期待スナップショット
- `jv/crates/jv_parser_rowan/tests/test_dsl.rs` – DSL パーサーフィクスチャ

これらの資料を併用することで、テスト DSL → Java → Maven 実行までの一連のフローを短時間で検証できます。
