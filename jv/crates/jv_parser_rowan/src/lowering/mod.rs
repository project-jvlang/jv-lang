//! Rowan構文木から`jv_ast`ステートメントへ写像するローワリング層。
//!
//! 現段階では既存Chumskyベースのステートメントパーサを再利用しつつ、
//! Rowanノードから必要なトークン列・スパン情報・補助メタデータを抽出する。
//! 今後の段階ではRowanネイティブな式/型ローワリングへ段階的に移行する。

mod helpers;
mod statements;

pub use statements::{
    lower_program, LoweringDiagnostic, LoweringDiagnosticSeverity, LoweringResult,
};
