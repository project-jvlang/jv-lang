//! パーサー本体とサブモジュール。

mod checkpoint;
pub mod expression;
pub mod incremental;
pub mod pattern;
pub mod recovery;
pub mod statement;
pub mod types;

use crate::{
    allocator::Arena,
    ast::builder::{AstBuilder, AstRoot},
    context::JvContext,
    diagnostics::Diagnostic,
    lexer::Lexer,
    span::Span,
    token::{Token, TokenKind},
};

pub use checkpoint::Checkpoint;

/// パース結果。
#[derive(Default, Clone)]
pub struct ParseResult<'alloc> {
    pub ast: Option<AstRoot<'alloc>>,
    pub diagnostics: Vec<Diagnostic>,
    pub recovered: bool,
}

/// レキサー上で構文解析を行う再帰下降パーサー。
pub struct Parser<'src, 'alloc>
where
    'src: 'alloc,
{
    pub(crate) lexer: Lexer<'src>,
    arena: &'alloc Arena,
    ast_builder: AstBuilder<'src, 'alloc>,
    diagnostics: Vec<Diagnostic>,
    tokens: Vec<Token>,
    position: usize,
    context: JvContext,
    recovered: bool,
    recovery_metrics: crate::parser::recovery::RecoveryMetrics,
}

impl<'src, 'alloc> Parser<'src, 'alloc>
where
    'src: 'alloc,
{
    pub fn new(lexer: Lexer<'src>, arena: &'alloc Arena) -> Self {
        let builder = AstBuilder::new(arena, lexer.source_text());
        Self {
            lexer,
            arena,
            ast_builder: builder,
            diagnostics: Vec::new(),
            tokens: Vec::new(),
            position: 0,
            context: JvContext::empty(),
            recovered: false,
            recovery_metrics: crate::parser::recovery::RecoveryMetrics::default(),
        }
    }

    /// プログラム全体をパースする。
    pub fn parse(&mut self) -> ParseResult<'_> {
        let program = statement::parse_program(self);
        let ast = program.map(|prog| self.ast_builder.alloc_program(prog));
        ParseResult {
            ast,
            diagnostics: self.diagnostics.clone(),
            recovered: self.recovery_metrics.recovered > 0 || self.recovered,
        }
    }

    /// チェックポイントを取得する。
    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint::new(
            self.lexer.current_offset(),
            self.position,
            self.tokens.len(),
            self.diagnostics.len(),
            self.context,
            self.recovered,
            self.lexer.current,
            self.lexer.mode,
        )
    }

    /// 指定のチェックポイントまで復元する。
    pub fn rewind(&mut self, checkpoint: Checkpoint) {
        self.position = checkpoint.token_index;
        self.tokens.truncate(checkpoint.tokens_len);
        self.diagnostics.truncate(checkpoint.diagnostics_len);
        self.context = checkpoint.context;
        self.recovered = checkpoint.recovered;
        self.lexer.set_state(
            checkpoint.cursor,
            checkpoint.lexer_current,
            checkpoint.lexer_mode,
        );
    }

    /// 現在のトークンを取得する（必要に応じてレクシング）。
    pub(crate) fn current(&mut self) -> Token {
        self.token_at(self.position)
            .unwrap_or_else(|| Token::new(TokenKind::Eof, Span::new(0, 0)))
    }

    /// 次のトークンを先読みする（消費しない）。
    pub(crate) fn peek_next(&mut self) -> Option<Token> {
        self.token_at(self.position + 1)
    }

    /// 次のトークンへ進める。
    pub(crate) fn advance(&mut self) -> Token {
        let token = self.current();
        self.position = self.position.saturating_add(1);
        token
    }

    /// 指定位置のトークンを返す。足りない場合はレクシングする。
    pub(crate) fn token_at(&mut self, index: usize) -> Option<Token> {
        while self.tokens.len() <= index {
            let mut next = self.lexer.next_token();
            while matches!(
                next.kind,
                TokenKind::Whitespace
                    | TokenKind::Newline
                    | TokenKind::LineComment
                    | TokenKind::BlockComment
                    | TokenKind::JavaDocComment
                    | TokenKind::Semicolon
            ) {
                next = self.lexer.next_token();
            }
            let is_eof = next.kind == TokenKind::Eof;
            self.tokens.push(next);
            if is_eof {
                break;
            }
        }
        self.tokens.get(index).copied()
    }

    /// 指定のトークン種別にマッチするか確認し、成功時は進める。
    pub(crate) fn consume_if(&mut self, kind: TokenKind) -> bool {
        if self.current().kind == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    /// トークン種別を期待し、満たさない場合は診断を追加する。
    pub(crate) fn expect(&mut self, kind: TokenKind, message: &str) -> bool {
        if self.current().kind == kind {
            self.advance();
            true
        } else {
            let span = self.current().span;
            self.diagnostics.push(Diagnostic::new(message, span));
            false
        }
    }

    /// 診断を追加する。
    pub(crate) fn push_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// 診断の参照を取得する（テスト用）。
    pub fn diagnostics(&self) -> &Vec<Diagnostic> {
        &self.diagnostics
    }

    /// Span に対応するソース文字列を取得する（UTF-8）。
    pub(crate) fn lexeme(&self, span: crate::span::Span) -> Option<&'src str> {
        self.lexer.lexeme(span)
    }

    /// 現在のコンテキストを一時的に追加し、元の値を返す。
    pub(crate) fn push_context(&mut self, ctx: JvContext) -> JvContext {
        let prev = self.context;
        self.context |= ctx;
        prev
    }

    /// コンテキストを復元する。
    pub(crate) fn restore_context(&mut self, prev: JvContext) {
        self.context = prev;
    }

    /// Arena参照を返す（AST構築用）。
    pub(crate) fn arena(&self) -> &'alloc Arena {
        self.arena
    }

    /// ASTビルダーへの参照を返す。
    pub(crate) fn ast_builder(&self) -> &AstBuilder<'src, 'alloc> {
        &self.ast_builder
    }

    /// バイトスパンをASTスパンへ変換する。
    pub(crate) fn ast_span(&self, span: Span) -> jv_ast::Span {
        self.ast_builder.span(span)
    }

    /// `jv_ast::Span` をバイトスパンへ逆変換する。
    pub(crate) fn span_from_ast(&self, span: &jv_ast::Span) -> Span {
        self.ast_builder.span_to_byte(span)
    }

    /// パーサーが回復を行ったかどうかをマーキングする。
    pub(crate) fn mark_recovered(&mut self) {
        self.recovered = true;
    }
}
