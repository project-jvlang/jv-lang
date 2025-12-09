use crate::lexer::TokenKind;

use super::{
    expression,
    recovery::recover_statement,
    strategies,
    DiagnosticSeverity, OwnedToken, ParseEvent, ParseOutput, ParserDiagnostic, SyntaxKind,
};

/// パーサ内部状態を保持するコンテキスト。
pub struct ParserContext {
    pub(crate) tokens: Vec<OwnedToken>,
    pub(crate) cursor: usize,
    pub(crate) events: Vec<ParseEvent>,
    pub(crate) diagnostics: Vec<ParserDiagnostic>,
    pub(crate) recovered: bool,
    pub(crate) paren_depth: usize,
    pub(crate) brace_depth: usize,
    pub(crate) bracket_depth: usize,
    pub(crate) angle_depth: usize,
}

impl ParserContext {
    /// 新しいコンテキストを生成する。
    pub fn new(tokens: Vec<OwnedToken>) -> Self {
        Self {
            tokens,
            cursor: 0,
            events: Vec::new(),
            diagnostics: Vec::new(),
            recovered: false,
            paren_depth: 0,
            brace_depth: 0,
            bracket_depth: 0,
            angle_depth: 0,
        }
    }

    /// ルートノードの解析を実行する。
    pub fn parse(&mut self) -> ParseOutput {
        self.start_node(SyntaxKind::Root);
        self.start_node(SyntaxKind::StatementList);
        self.parse_statement_list(None);
        self.finish_node(); // StatementList
        self.finish_node(); // Root
        self.into_output()
    }

    /// 解析結果へ変換する。
    pub fn into_output(&self) -> ParseOutput {
        ParseOutput::new(
            self.events.clone(),
            self.diagnostics.clone(),
            self.recovered,
        )
    }

    /// ステートメント列を解析する。
    /// ステートメント列を解析する。terminator が指定されている場合、そのトークンに遭遇した時点で終了する。
    pub(crate) fn parse_statement_list(&mut self, terminator: Option<TokenKind>) {
        while !self.is_eof() {
            if let Some(term) = terminator {
                if self.peek_kind() == Some(term) {
                    break;
                }
            }
            let before = self.cursor;
            if !self.parse_single_statement() {
                if before == self.cursor {
                    // 進まない場合はエラーとしてリカバリ。
                    self.error("予期しないトークンです");
                    recover_statement(self);
                }
            }
        }
    }

    /// 単一ステートメントを解析する。
    fn parse_single_statement(&mut self) -> bool {
        let lookahead = match self.peek_kind() {
            Some(kind) => kind,
            None => return false,
        };

        if lookahead == TokenKind::Semicolon {
            self.bump();
            return true;
        }

        for strategy in strategies::registry() {
            if strategy.matches(self, lookahead) {
                let before = self.cursor;
                let parsed = strategy.parse(self);
                if parsed || self.cursor > before {
                    return parsed;
                }
            }
        }

        false
    }

    /// 現在のトークンを返す。
    pub fn current(&self) -> Option<&OwnedToken> {
        self.tokens.get(self.cursor)
    }

    /// n 個先を参照する。
    pub fn peek(&self, n: usize) -> Option<&OwnedToken> {
        self.tokens.get(self.cursor + n)
    }

    /// 直近のトークン種別を参照する。
    pub fn peek_kind(&self) -> Option<TokenKind> {
        self.current().map(|tok| tok.kind)
    }

    /// EOF かどうか。
    pub fn is_eof(&self) -> bool {
        matches!(self.peek_kind(), None | Some(TokenKind::Eof))
    }

    /// トークンを前進させ、イベントとして記録する。
    pub fn bump(&mut self) -> bool {
        if self.is_eof() {
            return false;
        }

        let idx = self.cursor;
        if let Some(tok) = self.current() {
            let kind = tok.kind;
            self.record_depth(kind);
            self.events.push(ParseEvent::Token {
                kind: SyntaxKind::from(kind),
                token_index: idx,
            });
        }

        self.cursor += 1;
        true
    }

    /// 範囲付きでトークンを消費する。
    pub fn bump_while<F: Fn(TokenKind) -> bool>(&mut self, pred: F) {
        while let Some(kind) = self.peek_kind() {
            if pred(kind) {
                self.bump();
            } else {
                break;
            }
        }
    }

    /// ノード開始をイベントに追加する。
    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.events.push(ParseEvent::StartNode { kind });
    }

    /// ノード終了をイベントに追加する。
    pub fn finish_node(&mut self) {
        self.events.push(ParseEvent::FinishNode);
    }

    /// 診断を追加する。
    pub fn error(&mut self, message: impl Into<String>) {
        let span = self.current_span();
        let message = message.into();
        self.events.push(ParseEvent::Error {
            message: message.clone(),
            span,
        });
        self.diagnostics.push(ParserDiagnostic::new(
            message,
            DiagnosticSeverity::Error,
            span,
        ));
    }

    /// 現在位置のスパンを返す。
    pub fn current_span(&self) -> super::TokenSpan {
        let start = self.cursor;
        let end = (self.cursor + 1).min(self.tokens.len());
        super::TokenSpan::new(start, end)
    }

    /// 深さカウンタを更新する。
    fn record_depth(&mut self, kind: TokenKind) {
        match kind {
            TokenKind::LeftParen => self.paren_depth += 1,
            TokenKind::RightParen => self.paren_depth = self.paren_depth.saturating_sub(1),
            TokenKind::LeftBrace => self.brace_depth += 1,
            TokenKind::RightBrace => self.brace_depth = self.brace_depth.saturating_sub(1),
            TokenKind::LeftBracket => self.bracket_depth += 1,
            TokenKind::RightBracket => self.bracket_depth = self.bracket_depth.saturating_sub(1),
            TokenKind::Less => self.angle_depth += 1,
            TokenKind::Greater => self.angle_depth = self.angle_depth.saturating_sub(1),
            _ => {}
        }
    }

    /// Pratt パーサーのエントリポイント。
    pub fn parse_expression(&mut self) -> bool {
        expression::parse_expression_bp(self, 0)
    }

    /// `{ ... }` ブロックを解析する。開始の `{` は呼び出し元で確認済みである前提。
    pub fn parse_block(&mut self) -> bool {
        if self.peek_kind() != Some(TokenKind::LeftBrace) {
            return false;
        }
        self.start_node(SyntaxKind::Block);
        self.bump(); // consume '{'
        self.parse_statement_list(Some(TokenKind::RightBrace));
        if self.peek_kind() == Some(TokenKind::RightBrace) {
            self.bump();
        } else {
            self.error("ブロックが `}` で閉じられていません");
            recover_statement(self);
        }
        self.finish_node();
        true
    }
}
