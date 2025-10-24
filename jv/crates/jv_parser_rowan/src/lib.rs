//! Rowan ベースの構文木生成基盤。
//!
//! 本クレートは `rowan` を用いて jv 言語のロスレス構文木を構築するための
//! 最低限の足場を提供する。Lexer が生成した `jv_lexer::Token` 列を受け取り、
//! `ParseBuilder` が `GreenNodeBuilder` を薄くラップしてノードを積み上げる。
//! `SyntaxKind` と `TokenKind` は package/import/val/var ステートメントを中心とした
//! シグネチャをカバーし、後続の Statement 戦略やローワリング層が拡張しやすいように
//! 設計されている。
//!
//! このクレートの公開 API は Cargo feature `rowan-parser` が有効な場合にのみ利用できる。

#![warn(missing_docs)]

#[cfg(feature = "rowan-parser")]
use crate::parser::ParseEvent as ParserEvent;
#[cfg(feature = "rowan-parser")]
use jv_lexer::Token;
#[cfg(feature = "rowan-parser")]
use rowan::{GreenNode, GreenNodeBuilder, Language};

#[cfg(feature = "rowan-parser")]
/// Rowan パイプライン統合。
pub mod frontend;
#[cfg(feature = "rowan-parser")]
/// Rowanノードからjv_astへのローワリング層。
pub mod lowering;
#[cfg(feature = "rowan-parser")]
/// Rowan ベースのイベントパーサ機能。
pub mod parser;
#[cfg(feature = "rowan-parser")]
/// Rowan 構文種別定義。
pub mod syntax;
#[cfg(feature = "rowan-parser")]
/// Rowan AST 検証ハーネス。
pub mod verification;
#[cfg(feature = "rowan-parser")]
mod support;

#[cfg(feature = "rowan-parser")]
pub use parser::{parse, DiagnosticSeverity, ParseEvent, ParseOutput, ParserDiagnostic, TokenSpan};
#[cfg(feature = "rowan-parser")]
pub use syntax::{SyntaxKind, TokenKind};

#[cfg(all(test, feature = "rowan-parser"))]
mod tests;

/// Rowan 上で jv 構文木を識別するための言語マーカー。
#[cfg(feature = "rowan-parser")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum JvLanguage {}

#[cfg(feature = "rowan-parser")]
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
#[cfg(feature = "rowan-parser")]
pub type GreenTree = GreenNode;

/// `rowan::GreenNodeBuilder` の薄いラッパー。
///
/// - ノード開始/終了のマーカーを強制し、構文木の整合性を保つ。
/// - `jv_lexer::Token` から直接トークンを追加できる。
#[cfg(feature = "rowan-parser")]
#[derive(Debug, Default)]
pub struct ParseBuilder {
    builder: GreenNodeBuilder<'static>,
}

#[cfg(feature = "rowan-parser")]
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

    /// 構文解析イベント列から Green ツリーを構築する。
    ///
    /// Parser が生成した `ParseEvent` と元のトークン列を入力とし、
    /// `rowan::GreenNode` を返す。イベント列に含まれる `ParseEvent::Error`
    /// は診断用途のためのメタ情報なので、木構築時には無視する。
    pub fn build_from_events(events: &[ParserEvent], tokens: &[Token]) -> GreenTree {
        let mut builder = Self::new();
        builder.apply_events(events, tokens);
        builder.finish()
    }

    /// 構文解析イベント列を順に適用する。
    fn apply_events(&mut self, events: &[ParserEvent], tokens: &[Token]) {
        let mut open_nodes = 0usize;
        for event in events {
            match event {
                ParserEvent::StartNode { kind } => {
                    self.start_node(*kind);
                    open_nodes = open_nodes.saturating_add(1);
                }
                ParserEvent::FinishNode => {
                    if open_nodes == 0 {
                        // 不整合なイベント列の場合は安全側で無視する。
                        continue;
                    }
                    self.finish_node();
                    open_nodes -= 1;
                }
                ParserEvent::Token { token_index, .. } => {
                    if open_nodes == 0 {
                        // ルート外に出力された EOF などは木に含めない。
                        continue;
                    }
                    if let Some(token) = tokens.get(*token_index) {
                        self.push_token(token);
                    } else {
                        debug_assert!(
                            *token_index < tokens.len(),
                            "token index out of bounds during tree construction"
                        );
                    }
                }
                ParserEvent::Error { .. } => {
                    // エラーイベントは診断用。構文木の構築には影響しない。
                }
            }
        }
    }
}
