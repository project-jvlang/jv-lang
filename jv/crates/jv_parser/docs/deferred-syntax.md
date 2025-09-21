# 延期構文インベントリ / Deferred Syntax Inventory

jv_parser クレートで一時的に延期されている構文サポートの最新状態を共有するためのドキュメントです。
Spec `.spec-workflow/specs/jv-parser-red-phase-handling` で定義したインベントリと同期した内容のみを掲載します。

<!-- deferred-syntax:begin -->
現時点では延期中の構文はありません。
No syntax is currently deferred.
<!-- deferred-syntax:end -->

## 更新手順 / Update Workflow

1. `.spec-workflow/specs/jv-parser-red-phase-handling/requirements.md` のインベントリを確認し、延期構文が増減した場合は `doc_sync.rs` 内の `INVENTORY` を更新します。
2. `render_markdown_table()` を実行して得られる出力と本ドキュメントのマーカー区間が一致するように編集します。
3. `cargo test -p jv_parser` を実行し、`doc_sync` テストが成功することを確認します。
4. ドキュメントを更新したら、README などのリンク先が最新であるか併せて見直してください。

> Maintainer: Parser Core Team (parser-core@jv.dev)
