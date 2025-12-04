//! AST統合モジュール。
//!
//! 互換性メモ:
//! - `jv_ast` のデータモデルをそのまま再利用し、差分は薄いアダプタで吸収する。
//! - バイトオフセットベースの `crate::span::Span` を `jv_ast::Span`（行・桁情報）へ変換する。
//! - AST本体は `Arena` 上に確保し、パースごとに再利用できるようにする。
//! - IR 変換は従来通り `jv_ir::transform_program` を呼び出すラッパーで提供する。
//! - 未移植のDSLノードは既存の `jv_ast` 定義に追従し、今後の拡張で追加する。

pub mod builder;
pub mod to_ir;
