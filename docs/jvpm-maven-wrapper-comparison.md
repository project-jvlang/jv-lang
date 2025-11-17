# Maven Wrapper（`mvn`/`mvnw`）と `jvpm` Wrapper モードの実測比較（レスポンス時間重視）

本資料は実際のコマンドを `/usr/bin/time -p` で計測した結果をもとに、従来の Maven Wrapper（または `mvn`）による依存解決と、`jvpm` の Wrapper モードによる依存追加のレスポンス時間および同期対象の違いを整理したものです。両方とも Maven 中心のワークフローですが、jvpm は `WrapperContext`/`WrapperPipeline`/`WrapperUpdateSummary` を介して Maven ファイルと `jv.lock` を同期する点に違いがあります。

## 1. 計測環境
- **OS**: WSL2 (Linux)、`mvn -v` 出力によれば Apache Maven 3.9.11、Java 25.0.1 (GraalVM)。
- **jvpm バイナリ**: `jv/target/debug/jvpm` を `cargo build --bin jvpm`（ビルドに約62秒）で生成。このバイナリを使って Wrapper モードの実行時間を計測。以降の `jvpm` 実行では再コンパイルを避けるため `target/debug/jvpm` を直接呼び出した。
- **測定手順**: `/usr/bin/time -p` をコマンドの前に置き、`real` を比較指標とした。ネットワークアクセスがあるため `mvn dependency:resolve` および `jvpm add ...` は Apache Maven 中央リポジトリへの通信を伴う（実行前に CI/開発環境でキャッシュがない状態で再現）。

## 2. 測定対象コマンド

| フロー | 計測コマンド | `real` | 備考 |
| --- | --- | --- | --- |
| Maven（既存 `pom.xml`） | `/tmp/jvpm-wrapper-SFy8Qh/maven` ディレクトリで `mvn -B dependency:resolve` | `7.10 s` | Maven 自身の `Total time: 2.745 s`（ビルドログ）も併記。`pom.xml` は事前に `junit-jupiter-api` を依存に持つ静的構成。 |
| `jvpm` Wrapper モード | `/tmp/jvpm-wrapper-SFy8Qh/jvpm` で `target/debug/jvpm add org.junit.jupiter:junit-jupiter-api` | `8.21 s` | `WrapperContext` が `pom.xml`/`settings.xml` を生成し、`WrapperPipeline` → `WrapperIntegrationStrategy` → `jv.lock` 更新までを一気通貫。コマンド出力に「更新: pom.xml / settings.xml / jv.lock」が含まれている。 |

## 3. 実測結果の読み取り

- `mvn` コマンドは既存 `pom.xml` で実行しているため、依存解決だけに集中し `real 7.10 s` で完了。`Total time: 2.745 s` という Maven の内部計測値と比べると、`real` にはプロセスの起動・JVM 読み込みのオーバーヘッドも含まれている。既存のプロジェクトではこの流れが最短だが、`pom.xml` 生成や `settings.xml` の同期はユーザーが手動で行う必要がある。
- `jvpm` はわずか 1 秒強長く `8.21 s` だが、その間にテンプレート生成（`pom.xml`/`settings.xml`）、依存解決、`jv.lock` 生成、および `WrapperUpdateSummary` による変更点通知を完了させている。ログには「依存関係 'org.junit.jupiter:junit-jupiter-api' を追加しました。更新: pom.xml / settings.xml / jv.lock」と表示されるため、結果として `pom` ファイルの初期作成にも対応している。
- したがって、`jvpm` モードは単一コマンドで Maven とのファイル同期と再現性（`jv.lock`）を確保しつつ `~8.2 s` で返ってくる。一方 Maven 単体は `pom` や `settings` の整備を別工程とするため依存解決自体はやや高速だが、Wrapper モードが狙う「テンプレート生成＋ロックファイル＋ファイル同期」の一貫性には届かない。

## 4. 再現手順

1. `/tmp/jvpm-wrapper-SFy8Qh/maven` を作り、`pom.xml` に `org.junit.jupiter:junit-jupiter-api:5.9.2` を定義（`mvn -B dependency:resolve`）。`/usr/bin/time -p mvn -B dependency:resolve` で `real 7.10 s` を記録した。
2. `cd /tmp/jvpm-wrapper-SFy8Qh/jvpm`（`pom.xml`/`jv.toml`なし）で `cargo build --bin jvpm` で `target/debug/jvpm` を生成し、`/usr/bin/time -p target/debug/jvpm add org.junit.jupiter:junit-jupiter-api` を実行すると `real 8.21 s`。コマンド出力に `pom.xml/settings.xml/jv.lock` の更新が含まれる。
3. 両コマンド共に Maven 中央リポジトリからのダウンロードを含むため、初回実行時はネットワークへアクセスする点を踏まえてほしい。

## 5. 補足

- `jvpm` Wrapper モードでは `WrapperCommandFilter` などによる `jv` 固有コマンドの拒否も入るため、無効なコマンドを与えた場合のレスポンスで時間の無駄は少ない（今回の測定では正しい `add` に限定）。
- 両者とも Maven キャッシュは `~/.m2/repository` を利用するため、二度目以降は実行時間が短縮される。ただし `jvpm` は `jv.lock` に戦略名やキャッシュ済みの情報を刻むことで、ファイル同期やコマンドの再現性を維持している点が差別化要素となる。

## 6. 継続的な計測と改善アクション

`scripts/performance/jvpm-wrapper-timing.sh` は `mvn -B dependency:resolve` と `target/debug/jvpm add <dependency>` をそれぞれ最初から 3 回実行し、`/usr/bin/time -p` の `real`/`user`/`sys` と `WrapperUpdateSummary` を含む CLI 出力を `target/performance/jvpm-wrapper-timing/measurements-<timestamp>.log` に追記することで、初回（キャッシュなし）と再実行時（キャッシュあり）の差を連続的に記録できるようにしています。依存名は引数（デフォルトは `org.apache.commons:commons-lang3:3.14.0`）で差し替えられ、ツールチップを `tee` するためリアルタイムでもログを確認できます。

測定ログには `WrapperPipeline` → `WrapperIntegrationStrategy` → `WrapperUpdateSummary` に並ぶ出力が含まれており、`pom.xml/settings.xml/jv.lock` の更新状況と `real` 時間をセットで照合できます。再測定は変更のたびに 2 時間スパンで、「計測 → 分析 → 改善 → 再計測」のサイクルを回すことで、Maven との差を 50% 以下に圧縮することを目標とします。

改善案（今後 2 時間以内の短期タスクとして想定）:

1. **Resolver キャッシュのウォームアップ**  
   `DependencyCache::global` と `ResolverDispatcher` の初期化を `WrapperPipeline` 起動前に段階的に動かして、キャッシュヒット率と `wrapper-pipeline-dependency-resolution` の `real` 値を低減する。
2. **pom.xml / settings.xml のバッチ更新**  
   `WrapperIntegrationStrategy` で `MavenIntegrationFiles` を一括生成し、`sync::sync_maven_artifacts` でまとめて書き出すことで `wrapper-pipeline-artifact-generation` の時間を縮める（ファイルごとの `fs::write` による `real` オーバーヘッドを減らす）。
3. **jvpm 起動の軽量化**  
   CLI 全体での `tokio` ランタイム生成や `WrapperContext` のファイル検出を遅延化し、`wrapper-context-detect` と `wrapper-context-template-generation` のログから見える遅延をカットする。必要に応じて `DependencyCache` などのマネージャーをシングルトン化し、`bench` で 50% ターゲットに近づくかを測定後に判断する。

改善を適用するたびにスクリプト（およびログ）を再実行し、`real` 時間の 50% 削減と `WrapperUpdateSummary` 出力の安定性を両立させることで、Phase 7 の目標に合致したレスポンスと再現性を確保してください。
