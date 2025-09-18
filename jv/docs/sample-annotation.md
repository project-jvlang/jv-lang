# @Sample – サンプルデータ取得と自動型推論（設計）

**日本語** | [English](sample-annotation-en.md)

## 目的
`@Sample("users.json") val users` のような宣言から、サンプルデータを元に静的型を自動推論し、Java 25 ソース生成まで一気通貫で行う。利用者は小さなサンプルを置くだけで、安全に型付きのコードと初期データを得られる。

## 概要
- 対応スキーマ: ファイルパス（相対/絶対）、`file://`、`https://`、`s3://`
- 動作モード: `mode` オプションで切替（デフォルト: `Embed`）
  - `Embed`（デフォルト）: コンパイル時にサンプルデータを取得・解析し、生成コードへ埋め込む。
  - `Load`: 実行時にサンプルデータをロードするコード（ローダ）を生成する。
- 追加オプション（任意）:
  - `format`: `json|csv|tsv|auto`（デフォルト `auto`。拡張子/Content-Typeで推測）
  - `sha256`: コンテンツ検証用ハッシュ（リプレイ/改竄対策）
  - `limitBytes`: 埋め込み上限（デフォルト 1 MiB、`Embed`時のみ）

例:
```
@Sample("users.json") val users             // => mode=Embed（デフォルト）
@Sample("https://example.com/users.json", mode=Load) val users
@Sample(source="s3://bucket/key.json", mode=Embed, sha256="<hex>") val users
```

## 型推論
- JSON オブジェクト → `record`（不変）または通常クラス（可変）を生成（jv では data class → Java record を基本とする）。
- JSON 配列 → `List<T>` として推論。要素のユニオンは構造の共通部分で最小上界を合成（フィールド欠如は `Optional<T>`/nullable で表現）。
- プリミティブ推論:
  - 整数: `int`/`long`/`BigInteger`（範囲で選択）
  - 小数: `double`/`BigDecimal`（桁数/精度で選択）
  - 文字列/真偽/Null → `String`/`boolean`/`Optional<T>`（または nullable）
- CSV/TSV は 1 行目ヘッダをフィールド名、以降をレコード列として推論。型は列全体から決定。

## Embed（コンパイル時埋め込み）
- コンパイル時にデータ取得→パース→Java ソースへ直列化し埋め込む。
- 大容量対策: `limitBytes` 超過でビルド失敗 or 自動で `Load` にフォールバック（`--sample-embed-fallback=load|fail`）。
- 生成物のイメージ:
  - 推論スキーマに対応する `record`/クラス群
  - `public static final List<User> users = List.of( … );` のような初期化コード
- ネットワーク既定: デフォルト禁止。`--sample-network=allow` または `jv.toml` で明示許可。`https://`/`s3://` 利用時は `sha256` 推奨。

## Load（実行時ロード）
- 生成コードは以下のローダを用いる:
  - `file://`/パス: `java.nio.file.Files`
  - `https://`/`http://`: Java 11+ `HttpClient`
  - `s3://`: デフォルトは未同梱（ゼロ依存方針のため）。`jv_s3` オプション拡張がある場合のみ組み込み。推奨は事前にプリサイン URL を発行し `https://` を使用。
- 例外/再試行: 簡易リトライ（指数バックオフ）と `IOException`/`HttpTimeoutException` を明示スロー。
- キャッシュ: 任意で `~/.cache/jv/sample/<sha256>` にディスクキャッシュ（`--sample-cache=on|off`）。

## セキュリティ
- 既定: ネットワーク取得は無効（`Embed`/`Load` いずれも）。
- `sha256` ピン留めをサポート（不一致はエラー）。
- `s3://` は認証情報取り扱いのため、デフォルト同梱なし。必要時のみオプション拡張で明示導入。

## 設定/CLI
- アノテーション:
  - `@Sample(source: String, mode: Embed|Load = Embed, format: String = "auto", sha256: String? = null, limitBytes: Int? = null)`
- `jv.toml`（例）:
```
[sample]
mode = "embed"               # default: embed|load
allow_network = false        # default: false
embed_max_bytes = 1048576    # default: 1 MiB
fallback_on_oversize = "fail"  # fail|load
cache = true
```
- CLI フラグ（`jv build`）:
  - `--sample-mode=embed|load`（既定を上書き）
  - `--sample-network=allow|deny`（デフォルト deny）
  - `--sample-embed-max-bytes=N`
  - `--sample-embed-fallback=load|fail`
  - `--sample-cache=on|off`

## 実装方針（段階）
1) 解析器/AST にアノテーション表現を導入（`@Name(args)` を `val/var/class` 等へ付与）。
2) IR で `@Sample` を検出→型推論フェーズへサンプルソースを引き渡し。
3) 型推論器（JSON/CSV）を実装し、レコード/クラス定義と初期化表現を生成。
4) `Embed` 経路: 初期化式を Java AST に埋め込み。
5) `Load` 経路: ローダ（file/http）を最小依存で生成し、デシリアライズ→型へマッピング。
6) CLI/設定連携・セキュリティ（ネットワーク許可/ハッシュ検証/容量制限/キャッシュ）。

## エラー設計
- ソース未到達/404/権限エラー: 具体的な URI とヒントを含む診断を出力。
- 型不一致（配列要素の著しい不整合）: 最小上界推論の失敗理由を提示し、`--sample-infer=loose|strict` で緩和可能に。
- 埋め込みサイズ超過: 推奨対策（サブセット化、`Load` への切替、`embed_max_bytes` 増加）。

## 例
```
// ローカル JSON をデフォルト（Embed）で埋め込み
@Sample("data/users.json") val users

// 実行時に HTTP からロード
@Sample("https://example.com/users.json", mode=Load) val users

// s3 をコンパイル時に取得・ハッシュ検証し埋め込み
@Sample(source="s3://my-bucket/users.json", mode=Embed, sha256="ab12…") val users
```

---
本仕様はドキュメントおよび CLI の合意インターフェースを先行定義するもので、言語側（AST/IR/コード生成）の実装は段階的に追加します。