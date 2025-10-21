//! Rowan ベースの構文木生成基盤。
//!
//! 本クレートは `rowan` を用いて jv 言語のロスレス構文木を構築するための
//! 最低限の足場を提供する。Lexer が生成した `jv_lexer::Token` 列を受け取り、
//! `ParseBuilder` が `GreenNodeBuilder` を薄くラップしてノードを積み上げる。
//! `SyntaxKind` と `TokenKind` は package/import/val/var ステートメントを中心とした
//! シグネチャをカバーし、後続の Statement 戦略やローワリング層が拡張しやすいように
//! 設計されている。

#![warn(missing_docs)]

use jv_lexer::Token;
use rowan::{GreenNode, GreenNodeBuilder, Language};

/// Rowan ベースのイベントパーサ機能。
pub mod parser;
/// Rowan 構文種別定義。
pub mod syntax;

pub use parser::{parse, DiagnosticSeverity, ParseEvent, ParseOutput, ParserDiagnostic, TokenSpan};
pub use syntax::{SyntaxKind, TokenKind};

#[cfg(test)]
mod tests;

/// Rowan 上で jv 構文木を識別するための言語マーカー。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum JvLanguage {}

impl Language for JvLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::from_raw(raw)
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

/// Rowan の `GreenNode` への型エイリアス。
pub type GreenTree = GreenNode;

/// `rowan::GreenNodeBuilder` の薄いラッパー。
///
/// - ノード開始/終了のマーカーを強制し、構文木の整合性を保つ。
/// - `jv_lexer::Token` から直接トークンを追加できる。
#[derive(Debug, Default)]
pub struct ParseBuilder {
    builder: GreenNodeBuilder<'static>,
}

impl ParseBuilder {
    /// 新しいビルダーを作成する。
    pub fn new() -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
        }
    }

    /// ノードの開始を記録する。
    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(kind.into());
    }

    /// ノードの終了を記録する。
    pub fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    /// 既存のトークンを構文木へ追加する。
    ///
    /// 返り値は付与された `TokenKind`。上位のパーサ戦略が同種トークンを
    /// まとめて扱いたい場合に利用できる。
    pub fn push_token(&mut self, token: &Token) -> TokenKind {
        let kind = TokenKind::from_token(token);
        self.builder
            .token(kind.to_syntax().into(), token.lexeme.as_str());
        kind
    }

    /// 任意のテキストトークンを追加する。
    pub fn push_raw_token(&mut self, kind: TokenKind, text: &str) {
        self.builder.token(kind.to_syntax().into(), text);
    }

    /// 内部の `GreenNodeBuilder` への可変参照を取得する。
    ///
    /// Rowan 固有の操作が必要な際に利用する。
    pub fn builder(&mut self) -> &mut GreenNodeBuilder<'static> {
        &mut self.builder
    }

    /// 構築した Green ツリーを返す。
    ///
    /// 呼び出し後、このビルダーは利用できない。
    pub fn finish(self) -> GreenTree {
        self.builder.finish()
    }
}
