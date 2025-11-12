# ロギングフレームワーク統合コード生成

## 概要
`LogInvocationPlan` を Java コードへ展開する際、利用するロギングフレームワークに応じて最適なメソッド呼び出しとプレースホルダー整形を行う。共通のエントリポイントは `LoggingFrameworkStrategy` トレイトで定義し、`StrategyRegistry` が `LoggingFrameworkKind` から適切な実装を取得する。

## 戦略ディスパッチ
- `Slf4jStrategy` / `Log4j2Strategy`
  - `{}` プレースホルダーを採用し、可変長引数でメッセージと引数を渡す。
  - TRACE/DEBUG レベルでは `isTraceEnabled` / `isDebugEnabled` ガードを挿入する。
- `JbossStrategy`
  - `tracef` などのフォーマット付きメソッドを使用し、`%s` プレースホルダーを維持する。
- `CommonsStrategy`
  - 事前に文字列結合を行った結果を単一引数で渡し、必要に応じて `String.valueOf` で非文字列を補完する。
- `JulStrategy`
  - `Logger.log(Level, pattern, args...)` を利用し、`{0}` 形式の番号付きプレースホルダーを生成する。
  - ガードは `isLoggable(Level)` により実装する。

## ログメッセージ処理
`analyze_message` ヘルパーが `LogMessage` を解析し、

- リテラルのみ
- 文字列フォーマット（`StringFormat`）
- 任意式

の種類に応じて戦略共通の補助関数へ渡す。文字列フォーマットの場合は `%s` プレースホルダーをセグメントへ分解し、各戦略での置換に対応する。Commons Logging では連結式を生成し、JUL では `{0}` ベースのフォーマット文字列を構築する。

## ログ項目の展開
`emit_log_plan` が以下の順序で処理を行う:
1. `LogGuardKind` が存在する場合は対応するガード式で `if` ブロックを生成。
2. `LogInvocationItem::Statement` はログ出力前にそのまま展開。
3. `LogInvocationItem::Message` は戦略実装へ委譲してログ呼び出しを生成。
4. `LogInvocationItem::Nested` は再帰的に `emit_log_plan` を呼び出す。

## テスト
`src/tests/logging.rs` に戦略ごとの挙動を検証するユニットテストを追加し、

- SLF4J のガードと `{}` プレースホルダー
- JBoss Logging の `%s` フォーマット
- Commons Logging の文字列結合
- JUL の `Level` 判定と `{0}` プレースホルダー

を確認している。テストは `cargo test -p jv_codegen_java logging` で実行可能。
