use jv_lexer::Token;

use crate::syntax::{SyntaxKind, TokenKind};

use super::{DiagnosticSeverity, ParseEvent, ParseOutput, ParserDiagnostic, TokenSpan, strategies};

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
    TokenKind::WhenKw,
    TokenKind::ForKw,
    TokenKind::ReturnKw,
    TokenKind::ThrowKw,
    TokenKind::BreakKw,
    TokenKind::ContinueKw,
    TokenKind::UseKw,
    TokenKind::DeferKw,
    TokenKind::SpawnKw,
    TokenKind::Eof,
];

const MAX_LOG_BLOCK_DEPTH: usize = 2;

#[derive(Default)]
struct WhenBlockState {
    brace_depth: usize,
}

#[derive(Default)]
struct ExpressionState {
    pending_when: bool,
    when_blocks: Vec<WhenBlockState>,
}

impl ExpressionState {
    fn register_when(&mut self) {
        self.pending_when = true;
    }

    fn start_when_block(&mut self) {
        self.pending_when = false;
        self.when_blocks.push(WhenBlockState { brace_depth: 1 });
    }

    fn has_pending_when(&self) -> bool {
        self.pending_when
    }

    fn on_left_brace(&mut self) {
        if let Some(block) = self.when_blocks.last_mut() {
            block.brace_depth = block.brace_depth.saturating_add(1);
        }
    }

    fn on_right_brace(&mut self) {
        if let Some(block) = self.when_blocks.last_mut() {
            if block.brace_depth <= 1 {
                self.when_blocks.pop();
            } else {
                block.brace_depth -= 1;
            }
        }
    }

    fn is_in_when_context(&self) -> bool {
        self.pending_when || !self.when_blocks.is_empty()
    }

    fn reset(&mut self) {
        self.pending_when = false;
        self.when_blocks.clear();
    }
}

/// パーサ内部状態。
pub(crate) struct ParserContext<'tokens> {
    pub(crate) tokens: &'tokens [Token],
    cursor: usize,
    events: Vec<ParseEvent>,
    diagnostics: Vec<ParserDiagnostic>,
    recovered: bool,
    block_depth: usize,
    expression_states: Vec<ExpressionState>,
    log_block_depth: usize,
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
            expression_states: Vec::new(),
            log_block_depth: 0,
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
            if self.consume_whitespace() {
                continue;
            }

            if self.is_eof() {
                break;
            }

            if self.consume_comment_statement() {
                continue;
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
            progressed |= self.consume_whitespace();
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

    pub(crate) fn parse_log_block_expression(&mut self, keyword: TokenKind) -> Option<usize> {
        debug_assert!(matches!(
            keyword,
            TokenKind::LogKw
                | TokenKind::TraceKw
                | TokenKind::DebugKw
                | TokenKind::InfoKw
                | TokenKind::WarnKw
                | TokenKind::ErrorKw
        ));

        let start = self.position();
        self.consume_trivia();
        self.start_node(SyntaxKind::LogBlockExpression);

        let keyword_line = self.bump_raw().map(|token| token.line).unwrap_or_default();

        let previous_depth = self.log_block_depth;
        self.log_block_depth = self.log_block_depth.saturating_add(1);
        if self.log_block_depth > MAX_LOG_BLOCK_DEPTH {
            let message = "ログブロックのネストは1段までです";
            self.report_error(message, start, self.cursor);
        }

        let mut last_line = Some(keyword_line);

        self.consume_trivia();
        if self.peek_significant_kind() == Some(TokenKind::LeftBrace) {
            let parsed = self.parse_braced_statements(SyntaxKind::Block);
            if parsed {
                if let Some(token) = self.tokens.get(self.cursor.saturating_sub(1)) {
                    last_line = Some(token.line);
                }
            }
        } else {
            let message = format!(
                "{} ブロックは'{{'で開始する必要があります",
                Self::log_keyword_label(keyword)
            );
            self.report_error(message, start, self.cursor);
        }

        self.log_block_depth = previous_depth;
        self.finish_node();
        last_line
    }

    fn log_keyword_label(keyword: TokenKind) -> &'static str {
        match keyword {
            TokenKind::LogKw => "LOG",
            TokenKind::TraceKw => "TRACE",
            TokenKind::DebugKw => "DEBUG",
            TokenKind::InfoKw => "INFO",
            TokenKind::WarnKw => "WARN",
            TokenKind::ErrorKw => "ERROR",
            _ => "LOG",
        }
    }

    /// バインディングパターンを解析する。
    pub(crate) fn parse_binding_pattern(&mut self) -> bool {
        self.consume_trivia();
        let Some(lookahead) = self.peek_significant_kind() else {
            return false;
        };

        if !matches!(
            lookahead,
            TokenKind::Identifier | TokenKind::LeftBracket | TokenKind::LeftParen
        ) {
            return false;
        }

        self.start_node(SyntaxKind::BindingPattern);
        let ok = match lookahead {
            TokenKind::Identifier => {
                self.bump_raw();
                true
            }
            TokenKind::LeftBracket => self.parse_binding_list_pattern(),
            TokenKind::LeftParen => self.parse_binding_tuple_pattern(),
            _ => false,
        };
        self.finish_node();
        ok
    }

    fn parse_binding_list_pattern(&mut self) -> bool {
        self.start_node(SyntaxKind::BindingListPattern);
        self.bump_raw(); // '['

        loop {
            self.consume_trivia();
            match self.peek_significant_kind() {
                Some(TokenKind::RightBracket) => {
                    self.bump_raw();
                    break;
                }
                Some(TokenKind::Eof) | None => {
                    self.finish_node();
                    return false;
                }
                _ => {}
            }

            if !self.parse_binding_pattern() {
                self.finish_node();
                return false;
            }

            self.consume_trivia();
            match self.peek_significant_kind() {
                Some(TokenKind::Comma) | Some(TokenKind::LayoutComma) => {
                    self.bump_raw();
                }
                Some(TokenKind::RightBracket) => {}
                Some(TokenKind::Eof) | None => {
                    self.finish_node();
                    return false;
                }
                _ => {}
            }
        }

        self.finish_node();
        true
    }

    fn parse_binding_tuple_pattern(&mut self) -> bool {
        self.start_node(SyntaxKind::BindingTuplePattern);
        self.bump_raw(); // '('

        loop {
            self.consume_trivia();
            match self.peek_significant_kind() {
                Some(TokenKind::RightParen) => {
                    self.bump_raw();
                    break;
                }
                Some(TokenKind::Eof) | None => {
                    self.finish_node();
                    return false;
                }
                _ => {}
            }

            if !self.parse_binding_pattern() {
                self.finish_node();
                return false;
            }

            self.consume_trivia();
            match self.peek_significant_kind() {
                Some(TokenKind::Comma) | Some(TokenKind::LayoutComma) => {
                    self.bump_raw();
                }
                Some(TokenKind::RightParen) => {}
                Some(TokenKind::Eof) | None => {
                    self.finish_node();
                    return false;
                }
                _ => {}
            }
        }

        self.finish_node();
        true
    }

    /// 関数・コンストラクタパラメータ修飾子を解析する。
    pub(crate) fn parse_parameter_modifiers(&mut self) -> bool {
        self.consume_trivia();

        let mut consumed_any = false;
        let mut list_started = false;

        loop {
            self.consume_trivia();

            let Some((index, kind)) = self.peek_significant_kind_n(0) else {
                break;
            };

            let recognized = match kind {
                TokenKind::ValKw | TokenKind::VarKw => true,
                TokenKind::Identifier => {
                    let token = &self.tokens[index];
                    matches!(token.lexeme.as_str(), "mut" | "ref")
                }
                _ => false,
            };

            if !recognized {
                break;
            }

            if !list_started {
                self.start_node(SyntaxKind::ParameterModifierList);
                list_started = true;
            }

            self.start_node(SyntaxKind::ParameterModifier);
            self.bump_raw();
            self.finish_node();

            consumed_any = true;
        }

        if list_started {
            self.finish_node();
        }

        consumed_any
    }

    /// 型注釈を解析する。
    pub(crate) fn parse_optional_type_annotation(&mut self) -> bool {
        self.consume_trivia();
        if self.peek_significant_kind() != Some(TokenKind::Colon) {
            return false;
        }

        self.start_node(SyntaxKind::TypeAnnotation);
        self.bump_raw(); // colon
        self.start_node(SyntaxKind::Expression);
        self.parse_type_expression_until(&[
            TokenKind::Assign,
            TokenKind::LeftBrace,
            TokenKind::Semicolon,
            TokenKind::RightBrace,
            TokenKind::Comma,
            TokenKind::RightParen,
            TokenKind::WhereKw,
        ]);
        self.finish_node();
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
        self.consume_trivia();
        self.finish_node();
        true
    }

    /// 関数パラメータのデフォルト値を解析する。
    pub(crate) fn parse_optional_parameter_default(&mut self) -> bool {
        self.consume_trivia();
        if self.peek_significant_kind() != Some(TokenKind::Assign) {
            return false;
        }

        self.start_node(SyntaxKind::InitializerClause);
        self.bump_raw(); // '='
        self.parse_expression_until(
            &[
                TokenKind::Comma,
                TokenKind::LayoutComma,
                TokenKind::RightParen,
            ],
            false,
        );
        self.consume_trivia();
        self.finish_node();
        true
    }

    /// 修飾名を解析する。
    pub(crate) fn parse_qualified_name(&mut self, outer_kind: SyntaxKind) -> bool {
        self.consume_whitespace();
        if self.peek_significant_kind() != Some(TokenKind::Identifier) {
            return false;
        }

        self.start_node(outer_kind);
        self.start_node(SyntaxKind::QualifiedName);

        loop {
            self.start_node(SyntaxKind::QualifiedNameSegment);
            self.bump_raw();
            self.finish_node();

            self.consume_whitespace();
            if self.peek_significant_kind() == Some(TokenKind::Dot) {
                self.bump_raw();
                self.consume_whitespace();
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
        self.expression_states.push(ExpressionState::default());
        self.consume_trivia();
        let start = self.cursor;
        self.start_node(SyntaxKind::Expression);

        let mut depth_paren = 0usize;
        let mut depth_brace = 0usize;
        let mut depth_bracket = 0usize;
        let mut depth_angle = 0usize;
        let mut last_line: Option<usize> = None;
        let mut last_significant_kind: Option<TokenKind> = None;
        let mut second_last_significant_kind: Option<TokenKind> = None;

        while let Some(token) = self.current_token() {
            let kind = TokenKind::from_token(token);
            if kind == TokenKind::Eof {
                break;
            }
            let at_top_level = depth_paren == 0 && depth_brace == 0 && depth_bracket == 0;
            if at_top_level && terminators.contains(&kind) {
                break;
            }
            let mut should_break_on_sync = false;
            if respect_statement_boundaries && at_top_level {
                if depth_angle == 0 {
                    if let Some(prev_line) = last_line {
                        if token.line > prev_line {
                            let continuation_kind = self.peek_significant_kind();
                            let continuation = continuation_kind.map_or(false, |kind| {
                                matches!(
                                    kind,
                                    TokenKind::Dot
                                        | TokenKind::NullSafe
                                        | TokenKind::DoubleColon
                                        | TokenKind::Arrow
                                )
                            });
                            if !continuation {
                                should_break_on_sync = true;
                            }
                        }
                    }
                }
                if depth_angle == 0 && kind == TokenKind::Comma {
                    should_break_on_sync = true;
                }
                if kind == TokenKind::WhenKw {
                    if let Some(state) = self.expression_states.last_mut() {
                        state.register_when();
                    }
                } else if kind == TokenKind::ElseKw {
                    let inside_when = self
                        .expression_states
                        .last()
                        .map(ExpressionState::is_in_when_context)
                        .unwrap_or(false);
                    if !inside_when {
                        should_break_on_sync = true;
                    }
                } else if matches!(
                    kind,
                    TokenKind::LineComment | TokenKind::BlockComment | TokenKind::DocComment
                ) {
                    should_break_on_sync = true;
                } else if SYNC_TOKENS.contains(&kind) {
                    should_break_on_sync = true;
                }
            }

            if should_break_on_sync {
                break;
            }

            let mut started_when_block = false;
            if respect_statement_boundaries && at_top_level && kind == TokenKind::LeftBrace {
                if let Some(state) = self.expression_states.last_mut() {
                    if state.has_pending_when() {
                        state.start_when_block();
                        started_when_block = true;
                    }
                }
            }

            if at_top_level
                && matches!(
                    kind,
                    TokenKind::LogKw
                        | TokenKind::TraceKw
                        | TokenKind::DebugKw
                        | TokenKind::InfoKw
                        | TokenKind::WarnKw
                        | TokenKind::ErrorKw
                )
            {
                let consumed_line = self.parse_log_block_expression(kind);
                if let Some(line) = consumed_line {
                    if line > 0 {
                        last_line = Some(line);
                    }
                    second_last_significant_kind = last_significant_kind;
                    last_significant_kind = Some(TokenKind::RightBrace);
                } else {
                    second_last_significant_kind = last_significant_kind;
                    last_significant_kind = Some(kind);
                }
                continue;
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
                        if respect_statement_boundaries {
                            if let Some(state) = self.expression_states.last_mut() {
                                state.on_right_brace();
                            }
                        }
                        break;
                    }
                    depth_brace -= 1;
                    if respect_statement_boundaries {
                        if let Some(state) = self.expression_states.last_mut() {
                            state.on_right_brace();
                        }
                    }
                }
                TokenKind::LeftBracket => depth_bracket += 1,
                TokenKind::RightBracket => {
                    if depth_bracket == 0 {
                        break;
                    }
                    depth_bracket -= 1;
                }
                TokenKind::Less => {
                    if depth_angle > 0 {
                        depth_angle += 1;
                    } else if self.is_generic_argument_sequence(
                        self.cursor,
                        last_significant_kind,
                        second_last_significant_kind,
                    ) {
                        depth_angle = 1;
                    }
                }
                TokenKind::Greater => {
                    if depth_angle > 0 {
                        depth_angle = depth_angle.saturating_sub(1);
                    }
                }
                _ => {}
            }

            if respect_statement_boundaries && kind == TokenKind::LeftBrace && !started_when_block {
                if let Some(state) = self.expression_states.last_mut() {
                    state.on_left_brace();
                }
            }

            self.bump_raw();
            last_line = Some(token.line);
            if !kind.is_trivia() {
                second_last_significant_kind = last_significant_kind;
                last_significant_kind = Some(kind);
            }
        }

        let consumed = self.cursor > start;
        self.finish_node();
        self.expression_states.pop();
        consumed
    }

    fn is_generic_argument_sequence(
        &self,
        start_index: usize,
        prev_significant: Option<TokenKind>,
        prev_prev_significant: Option<TokenKind>,
    ) -> bool {
        match prev_significant {
            Some(TokenKind::Identifier)
            | Some(TokenKind::RightParen)
            | Some(TokenKind::RightBracket)
            | Some(TokenKind::Greater) => {}
            _ => return false,
        }

        if matches!(
            prev_prev_significant,
            Some(TokenKind::DataKw)
                | Some(TokenKind::ClassKw)
                | Some(TokenKind::FunKw)
                | Some(TokenKind::ValKw)
                | Some(TokenKind::VarKw)
        ) {
            return false;
        }

        let mut depth = 1usize;
        let mut index = start_index + 1;
        while index < self.tokens.len() {
            let token = &self.tokens[index];
            let kind = TokenKind::from_token(token);
            if kind.is_trivia() {
                index += 1;
                continue;
            }

            match kind {
                TokenKind::Less => depth = depth.saturating_add(1),
                TokenKind::Greater => {
                    if depth == 0 {
                        return false;
                    }
                    depth -= 1;
                    if depth == 0 {
                        let mut lookahead = index + 1;
                        while lookahead < self.tokens.len() {
                            let next_kind = TokenKind::from_token(&self.tokens[lookahead]);
                            if next_kind.is_trivia() {
                                lookahead += 1;
                                continue;
                            }
                            return matches!(next_kind, TokenKind::LeftParen);
                        }
                        return false;
                    }
                }
                TokenKind::Semicolon
                | TokenKind::Assign
                | TokenKind::RightBrace
                | TokenKind::Newline => return false,
                _ => {}
            }

            index += 1;
        }
        false
    }

    fn reset_active_expression_state(&mut self) {
        if let Some(state) = self.expression_states.last_mut() {
            state.reset();
        }
    }

    /// トリビアを消費する。
    pub(crate) fn consume_trivia(&mut self) -> bool {
        let mut consumed = false;
        consumed |= self.consume_whitespace();
        while let Some(token) = self.current_token() {
            let kind = TokenKind::from_token(token);
            if !kind.is_comment() {
                break;
            }
            self.bump_raw();
            consumed = true;
        }
        consumed
    }

    /// ホワイトスペース・改行のみを消費する。
    pub(crate) fn consume_whitespace(&mut self) -> bool {
        let mut consumed = false;
        while let Some(token) = self.current_token() {
            let kind = TokenKind::from_token(token);
            if !matches!(kind, TokenKind::Whitespace | TokenKind::Newline) {
                break;
            }
            self.bump_raw();
            consumed = true;
        }
        consumed
    }

    fn consume_comment_statement(&mut self) -> bool {
        let Some(token) = self.current_token() else {
            return false;
        };
        let kind = TokenKind::from_token(token);
        if !matches!(kind, TokenKind::LineComment | TokenKind::BlockComment) {
            return false;
        }
        self.start_node(SyntaxKind::CommentStatement);
        self.bump_raw();
        self.finish_node();
        true
    }

    pub(crate) fn parse_type_expression_until(&mut self, terminators: &[TokenKind]) {
        let mut angle_depth = 0usize;
        let mut paren_depth = 0usize;
        let mut bracket_depth = 0usize;

        loop {
            self.consume_trivia();
            let Some(kind) = self.peek_significant_kind() else {
                break;
            };

            if angle_depth == 0
                && paren_depth == 0
                && bracket_depth == 0
                && terminators.contains(&kind)
            {
                break;
            }

            match kind {
                TokenKind::Less => angle_depth = angle_depth.saturating_add(1),
                TokenKind::Greater => {
                    if angle_depth > 0 {
                        angle_depth -= 1;
                    }
                }
                TokenKind::LeftParen => paren_depth = paren_depth.saturating_add(1),
                TokenKind::RightParen => {
                    if paren_depth > 0 {
                        paren_depth -= 1;
                    }
                }
                TokenKind::LeftBracket => bracket_depth = bracket_depth.saturating_add(1),
                TokenKind::RightBracket => {
                    if bracket_depth > 0 {
                        bracket_depth -= 1;
                    }
                }
                _ => {}
            }

            self.bump_raw();
        }
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
        self.reset_active_expression_state();
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

    /// 現在位置付近で診断を報告する。
    pub(crate) fn report_error(&mut self, message: impl Into<String>, start: usize, end: usize) {
        let message = message.into();
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
