use jv_lexer::Token;

use crate::syntax::{SyntaxKind, TokenKind};

use super::{strategies, DiagnosticSeverity, ParseEvent, ParseOutput, ParserDiagnostic, TokenSpan};

/// デフォルトの同期トークン集合。
const SYNC_TOKENS: &[TokenKind] = &[
    TokenKind::Semicolon,
    TokenKind::Newline,
    TokenKind::RightBrace,
    TokenKind::PackageKw,
    TokenKind::ImportKw,
    TokenKind::ValKw,
    TokenKind::VarKw,
    TokenKind::FunKw,
    TokenKind::ClassKw,
    TokenKind::DataKw,
    TokenKind::IfKw,
    TokenKind::ElseKw,
    TokenKind::WhenKw,
    TokenKind::ForKw,
    TokenKind::WhileKw,
    TokenKind::DoKw,
    TokenKind::ReturnKw,
    TokenKind::ThrowKw,
    TokenKind::BreakKw,
    TokenKind::ContinueKw,
    TokenKind::Eof,
];

/// パーサ内部状態。
pub(crate) struct ParserContext<'tokens> {
    pub(crate) tokens: &'tokens [Token],
    cursor: usize,
    events: Vec<ParseEvent>,
    diagnostics: Vec<ParserDiagnostic>,
    recovered: bool,
    block_depth: usize,
}

impl<'tokens> ParserContext<'tokens> {
    /// 新しいコンテキストを生成する。
    pub(crate) fn new(tokens: &'tokens [Token]) -> Self {
        Self {
            tokens,
            cursor: 0,
            events: Vec::new(),
            diagnostics: Vec::new(),
            recovered: false,
            block_depth: 0,
        }
    }

    /// ルートノードの解析を実行する。
    pub(crate) fn parse(mut self) -> ParseOutput {
        self.start_node(SyntaxKind::Root);
        self.start_node(SyntaxKind::StatementList);
        self.parse_statement_list(None);
        self.finish_node(); // StatementList
        self.finish_node();

        // 残余のトリビアや EOF を取り込む。
        self.consume_trivia();
        if self.peek_significant_kind() == Some(TokenKind::Eof) {
            self.bump_raw();
        }

        self.into_output()
    }

    /// 解析結果へ変換する。
    pub(crate) fn into_output(self) -> ParseOutput {
        ParseOutput::new(self.events, self.diagnostics, self.recovered)
    }

    /// ステートメント列を解析する。
    fn parse_statement_list(&mut self, terminator: Option<TokenKind>) {
        loop {
            if self.consume_trivia() {
                continue;
            }

            if self.is_eof() {
                break;
            }

            if let Some(term) = terminator {
                if self.peek_significant_kind() == Some(term) {
                    break;
                }
            }

            let before = self.cursor;
            if !self.parse_single_statement() {
                // 進捗がない場合は安全に抜ける。
                if self.cursor == before {
                    if self.is_eof() {
                        break;
                    }
                    self.bump_raw();
                }
            }
        }
    }

    /// 単一ステートメントを解析する。
    fn parse_single_statement(&mut self) -> bool {
        let lookahead = match self.peek_significant_kind() {
            Some(kind) => kind,
            None => return false,
        };

        if lookahead == TokenKind::Semicolon {
            self.bump_raw();
            return true;
        }

        for strategy in strategies::registry() {
            if strategy.matches(self, lookahead) {
                let before = self.cursor;
                let consumed = strategy.parse(self);
                self.consume_statement_terminators();
                if !consumed && self.cursor == before {
                    let message = format!(
                        "{:?} ステートメントがトークンを消費しませんでした",
                        strategy.name()
                    );
                    self.recover_statement(message, before);
                }
                return true;
            }
        }

        self.unexpected_statement(lookahead);
        true
    }

    /// ステートメント終端トークンを読み飛ばす。
    fn consume_statement_terminators(&mut self) {
        loop {
            let mut progressed = false;
            progressed |= self.consume_trivia();
            if self.peek_significant_kind() == Some(TokenKind::Semicolon) {
                self.bump_raw();
                progressed = true;
            }
            if !progressed {
                break;
            }
        }
    }

    /// 予期しないトークンを処理する。
    fn unexpected_statement(&mut self, lookahead: TokenKind) {
        let start = self.cursor;
        let message = format!(
            "トークン {:?} からステートメントを構築できません",
            lookahead
        );
        self.recover_statement(message, start);
    }

    /// ブロック（`{}`）を解析する。
    pub(crate) fn parse_block(&mut self) -> bool {
        self.parse_braced_statements(SyntaxKind::Block)
    }

    /// クラスボディを解析する。
    pub(crate) fn parse_class_body(&mut self) -> bool {
        self.parse_braced_statements(SyntaxKind::ClassBody)
    }

    fn parse_braced_statements(&mut self, outer_kind: SyntaxKind) -> bool {
        self.consume_trivia();
        if self.peek_significant_kind() != Some(TokenKind::LeftBrace) {
            return false;
        }

        let start = self.cursor;
        self.start_node(outer_kind);
        self.bump_raw(); // LeftBrace

        self.block_depth = self.block_depth.saturating_add(1);
        self.start_node(SyntaxKind::StatementList);
        self.parse_statement_list(Some(TokenKind::RightBrace));
        self.finish_node(); // StatementList

        self.consume_trivia();
        if self.peek_significant_kind() == Some(TokenKind::RightBrace) {
            self.bump_raw();
        } else {
            self.recover_statement("'}' が必要です", start);
        }

        self.block_depth = self.block_depth.saturating_sub(1);
        self.finish_node(); // outer_kind
        true
    }

    /// バインディングパターンを解析する。
    pub(crate) fn parse_binding_pattern(&mut self) -> bool {
        self.start_node(SyntaxKind::BindingPattern);
        let ok = self.bump_expected(TokenKind::Identifier, "識別子が必要です");
        self.finish_node();
        ok
    }

    /// 型注釈を解析する。
    pub(crate) fn parse_optional_type_annotation(&mut self) -> bool {
        self.consume_trivia();
        if self.peek_significant_kind() != Some(TokenKind::Colon) {
            return false;
        }

        self.start_node(SyntaxKind::TypeAnnotation);
        self.bump_raw(); // colon
        self.parse_expression_until(
            &[
                TokenKind::Assign,
                TokenKind::Comma,
                TokenKind::RightParen,
                TokenKind::Semicolon,
                TokenKind::RightBrace,
            ],
            false,
        );
        self.finish_node();
        true
    }

    /// 初期化子を解析する。
    pub(crate) fn parse_initializer_clause(&mut self, required: bool) -> bool {
        self.consume_trivia();
        if self.peek_significant_kind() != Some(TokenKind::Assign) {
            if required {
                let start = self.cursor;
                self.recover_statement("初期化子が必要です", start);
            }
            return false;
        }

        self.start_node(SyntaxKind::InitializerClause);
        self.bump_raw(); // '='
        self.parse_expression_until(
            &[
                TokenKind::Semicolon,
                TokenKind::Newline,
                TokenKind::RightBrace,
            ],
            true,
        );
        self.finish_node();
        true
    }

    /// 修飾名を解析する。
    pub(crate) fn parse_qualified_name(&mut self, outer_kind: SyntaxKind) -> bool {
        self.consume_trivia();
        if self.peek_significant_kind() != Some(TokenKind::Identifier) {
            return false;
        }

        self.start_node(outer_kind);
        self.start_node(SyntaxKind::QualifiedName);

        loop {
            self.start_node(SyntaxKind::QualifiedNameSegment);
            self.bump_raw();
            self.finish_node();

            self.consume_trivia();
            if self.peek_significant_kind() == Some(TokenKind::Dot) {
                self.bump_raw();
                self.consume_trivia();
                if self.peek_significant_kind() != Some(TokenKind::Identifier) {
                    break;
                }
                continue;
            }
            break;
        }

        self.finish_node(); // QualifiedName
        self.finish_node(); // outer
        true
    }

    /// 式ノードを構築する。
    pub(crate) fn parse_expression_until(
        &mut self,
        terminators: &[TokenKind],
        respect_statement_boundaries: bool,
    ) -> bool {
        self.consume_trivia();
        let start = self.cursor;
        self.start_node(SyntaxKind::Expression);

        let mut depth_paren = 0usize;
        let mut depth_brace = 0usize;
        let mut depth_bracket = 0usize;

        while let Some(token) = self.current_token() {
            let kind = TokenKind::from_token(token);
            if kind == TokenKind::Eof {
                break;
            }
            let at_top_level = depth_paren == 0 && depth_brace == 0 && depth_bracket == 0;
            if at_top_level && terminators.contains(&kind) {
                break;
            }
            if respect_statement_boundaries && at_top_level && SYNC_TOKENS.contains(&kind) {
                break;
            }

            match kind {
                TokenKind::LeftParen => depth_paren += 1,
                TokenKind::RightParen => {
                    if depth_paren == 0 {
                        break;
                    }
                    depth_paren -= 1;
                }
                TokenKind::LeftBrace => depth_brace += 1,
                TokenKind::RightBrace => {
                    if depth_brace == 0 {
                        break;
                    }
                    depth_brace -= 1;
                }
                TokenKind::LeftBracket => depth_bracket += 1,
                TokenKind::RightBracket => {
                    if depth_bracket == 0 {
                        break;
                    }
                    depth_bracket -= 1;
                }
                _ => {}
            }

            self.bump_raw();
        }

        let consumed = self.cursor > start;
        self.finish_node();
        consumed
    }

    /// トリビアを消費する。
    pub(crate) fn consume_trivia(&mut self) -> bool {
        let mut consumed = false;
        while let Some(token) = self.current_token() {
            let kind = TokenKind::from_token(token);
            if !kind.is_trivia() {
                break;
            }
            self.bump_raw();
            consumed = true;
        }
        consumed
    }

    /// 次の有効トークン種別を取得する。
    pub(crate) fn peek_significant_kind(&self) -> Option<TokenKind> {
        self.peek_significant_kind_n(0).map(|(_, kind)| kind)
    }

    pub(crate) fn peek_significant_kind_n(&self, n: usize) -> Option<(usize, TokenKind)> {
        let mut index = self.cursor;
        let mut skipped = 0usize;
        while index < self.tokens.len() {
            let token = &self.tokens[index];
            let kind = TokenKind::from_token(token);
            if kind.is_trivia() {
                index += 1;
                continue;
            }
            if skipped == n {
                return Some((index, kind));
            }
            skipped += 1;
            index += 1;
        }
        None
    }

    /// EoF かどうか。
    pub(crate) fn is_eof(&self) -> bool {
        self.peek_significant_kind()
            .map(|kind| kind == TokenKind::Eof)
            .unwrap_or(true)
    }

    /// 現在のトークンを取得する。
    pub(crate) fn current_token(&self) -> Option<&'tokens Token> {
        self.tokens.get(self.cursor)
    }

    /// 現在位置を取得する。
    pub(crate) fn position(&self) -> usize {
        self.cursor
    }

    /// トークンを 1 つ消費する。
    pub(crate) fn bump_raw(&mut self) -> Option<&'tokens Token> {
        let idx = self.cursor;
        if idx >= self.tokens.len() {
            return None;
        }
        let token = &self.tokens[idx];
        let kind = TokenKind::from_token(token).to_syntax();
        self.events.push(ParseEvent::Token {
            kind,
            token_index: idx,
        });
        self.cursor += 1;
        Some(token)
    }

    /// 指定したトークンを期待して消費する。
    pub(crate) fn bump_expected(&mut self, expected: TokenKind, message: &str) -> bool {
        self.consume_trivia();
        if self.peek_significant_kind() == Some(expected) {
            self.bump_raw();
            true
        } else {
            let span = self.make_span(self.cursor, self.cursor);
            self.push_diagnostic(ParserDiagnostic::new(
                message,
                DiagnosticSeverity::Error,
                span,
            ));
            false
        }
    }

    /// 指定したトークンが続く場合に消費する。
    pub(crate) fn bump_if(&mut self, expected: TokenKind) -> bool {
        self.consume_trivia();
        if self.peek_significant_kind() == Some(expected) {
            self.bump_raw();
            true
        } else {
            false
        }
    }

    /// エラーノードを伴う回復を実行する。
    pub(crate) fn recover_statement(&mut self, message: impl Into<String>, start: usize) {
        let message = message.into();
        self.recovered = true;
        let error_kind = if self.block_depth > 0 {
            SyntaxKind::BlockError
        } else {
            SyntaxKind::Error
        };

        self.start_node(error_kind);
        let initial_cursor = self.cursor;

        while let Some(token) = self.current_token() {
            let kind = TokenKind::from_token(token);
            if SYNC_TOKENS.contains(&kind) {
                match kind {
                    TokenKind::Newline | TokenKind::Semicolon => {
                        self.bump_raw();
                    }
                    _ => {}
                }
                break;
            }

            if kind == TokenKind::Eof {
                self.bump_raw();
                break;
            }

            self.bump_raw();
        }

        self.finish_node();
        if self.cursor == initial_cursor {
            if let Some(token) = self.current_token() {
                let kind = TokenKind::from_token(token);
                if matches!(
                    kind,
                    TokenKind::RightBrace
                        | TokenKind::RightParen
                        | TokenKind::RightBracket
                        | TokenKind::Comma
                        | TokenKind::Semicolon
                        | TokenKind::Newline
                        | TokenKind::Eof
                ) {
                    self.bump_raw();
                }
            }
        }
        let end = self.cursor.max(start);
        let span = self.make_span(start, end);
        self.push_error_event(&message, span);
        self.push_diagnostic(ParserDiagnostic::new(
            message,
            DiagnosticSeverity::Error,
            span,
        ));
    }

    fn push_error_event(&mut self, message: &str, span: TokenSpan) {
        self.events.push(ParseEvent::Error {
            message: message.to_string(),
            span,
        });
    }

    fn push_diagnostic(&mut self, diagnostic: ParserDiagnostic) {
        self.diagnostics.push(diagnostic);
    }

    fn make_span(&self, start: usize, end: usize) -> TokenSpan {
        let len = self.tokens.len();
        let clamped_start = start.min(len);
        let clamped_end = end.min(len);
        TokenSpan::new(clamped_start, clamped_end.max(clamped_start))
    }

    /// ノードの開始を記録する。
    pub(crate) fn start_node(&mut self, kind: SyntaxKind) {
        self.events.push(ParseEvent::StartNode { kind });
    }

    /// ノードの終了を記録する。
    pub(crate) fn finish_node(&mut self) {
        self.events.push(ParseEvent::FinishNode);
    }
}
