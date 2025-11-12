use jv_lexer::{Token, TokenType};

use crate::{
    frontend::DIAGNOSTIC_JV_UNIT_005_DEFAULT_MARKER_FORBIDDEN,
    syntax::{SyntaxKind, TokenKind},
};

use super::{strategies, DiagnosticSeverity, ParseEvent, ParseOutput, ParserDiagnostic, TokenSpan};

/// デフォルトの同期トークン集合。
const SYNC_TOKENS: &[TokenKind] = &[
    TokenKind::Semicolon,
    TokenKind::Newline,
    TokenKind::RightBrace,
    TokenKind::Arrow,
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
/// ジェネリック引数シーケンスの先読み最大トークン数（無限ループ防止）
const MAX_GENERIC_LOOKAHEAD: usize = 100;
/// 式のネスト最大深度（メモリ消費制限）
const MAX_EXPRESSION_DEPTH: usize = 50;

#[derive(Default)]
struct WhenBlockState {
    brace_depth: usize,
}

/// 式解析時の `when` ブロック状態を保持し、`->` を誤った同期境界として扱わないようにする。
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

    fn inside_when_block(&self) -> bool {
        !self.when_blocks.is_empty()
    }

    fn reset(&mut self) {
        self.pending_when = false;
        self.when_blocks.clear();
    }
}

#[derive(Clone, Copy)]
enum UnitSuffixDescriptor {
    SimpleWithAt,
    SimpleWithoutAt,
    BracketWithAt,
    BracketWithoutAt,
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
    unit_type_annotation_depth: usize,
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
            unit_type_annotation_depth: 0,
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
        let terminators = [
            TokenKind::Assign,
            TokenKind::LeftBrace,
            TokenKind::Semicolon,
            TokenKind::RightBrace,
            TokenKind::Comma,
            TokenKind::RightParen,
            TokenKind::WhereKw,
        ];
        let unit_suffix = self.preview_unit_type_annotation_suffix(&terminators);

        if let Some(descriptor) = unit_suffix {
            self.start_node(SyntaxKind::UnitTypeAnnotation);
            self.unit_type_annotation_depth = self.unit_type_annotation_depth.saturating_add(1);
            self.start_node(SyntaxKind::Expression);
            self.parse_type_expression_until(&terminators);
            self.finish_node();
            self.unit_type_annotation_depth = self.unit_type_annotation_depth.saturating_sub(1);
            self.parse_unit_type_annotation_suffix(descriptor);
            self.finish_node();
        } else {
            self.start_node(SyntaxKind::Expression);
            self.parse_type_expression_until(&terminators);
            self.finish_node();
        }
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
        // ネストの深さ制限チェック（スタックオーバーフロー・メモリ不足防止）
        if self.expression_states.len() >= MAX_EXPRESSION_DEPTH {
            let message = format!("式のネストが深すぎます（最大{}段）", MAX_EXPRESSION_DEPTH);
            self.report_error(message, self.cursor, self.cursor);
            return false;
        }

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
            if kind == TokenKind::IfKw {
                let message = "JV3103: jv 言語では `if`/`else` 式はサポートされていません。`when` を使用してください。";
                self.report_error(message, self.cursor, self.cursor + 1);
                self.bump_raw();
                continue;
            }
            let at_top_level = depth_paren == 0 && depth_brace == 0 && depth_bracket == 0;
            if at_top_level && terminators.contains(&kind) {
                break;
            }
            if self.unit_type_annotation_depth > 0
                && last_significant_kind.is_some()
                && at_top_level
                && depth_angle == 0
            {
                if kind == TokenKind::At {
                    break;
                }
                if kind == TokenKind::Whitespace {
                    if let Some((_, next_kind)) = self.peek_significant_kind_from(self.cursor + 1) {
                        if matches!(next_kind, TokenKind::Identifier | TokenKind::LeftBracket) {
                            break;
                        }
                    }
                }
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
                } else if matches!(
                    kind,
                    TokenKind::LineComment | TokenKind::BlockComment | TokenKind::DocComment
                ) {
                    should_break_on_sync = true;
                } else if SYNC_TOKENS.contains(&kind) {
                    // `when` 式のアーム内では `->` はステートメント境界にならない。
                    let allow_arrow_within_when = kind == TokenKind::Arrow
                        && self
                            .expression_states
                            .last()
                            .map(|state| state.inside_when_block())
                            .unwrap_or(false);
                    if !allow_arrow_within_when {
                        should_break_on_sync = true;
                    }
                }
            }

            if should_break_on_sync {
                break;
            }

            if matches!(
                kind,
                TokenKind::NumberLiteral | TokenKind::StringLiteral | TokenKind::CharacterLiteral
            ) {
                let literal_index = self.cursor;
                if let Some(descriptor) = self.detect_unit_suffix(literal_index) {
                    self.start_node(SyntaxKind::UnitLiteral);

                    let literal_token = self
                        .bump_raw()
                        .expect("literal token should be available when building unit literal");
                    let mut last_kind_consumed = TokenKind::from_token(literal_token);
                    let mut last_line_consumed = literal_token.line;

                    self.consume_inline_whitespace();

                    match descriptor {
                        UnitSuffixDescriptor::SimpleWithAt => {
                            if let Some(token) = self.current_token() {
                                debug_assert!(
                                    TokenKind::from_token(token) == TokenKind::At,
                                    "単位識別子の前には `@` トークンが必要です"
                                );
                            }
                            if let Some(token) = self.bump_raw() {
                                last_kind_consumed = TokenKind::from_token(token);
                                last_line_consumed = token.line;
                            }
                            self.consume_inline_whitespace();
                            if let Some(token) = self.current_token() {
                                debug_assert!(
                                    Self::token_supports_unit_symbol(token),
                                    "`@` の直後には単位識別子が必要です"
                                );
                            }
                            if let Some(token) = self.bump_raw() {
                                last_kind_consumed = TokenKind::from_token(token);
                                last_line_consumed = token.line;
                            }
                        }
                        UnitSuffixDescriptor::SimpleWithoutAt => {
                            if let Some(token) = self.current_token() {
                                debug_assert!(
                                    Self::token_supports_unit_symbol(token),
                                    "単位リテラルの末尾には単位識別子が必要です"
                                );
                            }
                            if let Some(token) = self.bump_raw() {
                                last_kind_consumed = TokenKind::from_token(token);
                                last_line_consumed = token.line;
                            }
                        }
                        UnitSuffixDescriptor::BracketWithAt => {
                            if let Some(token) = self.current_token() {
                                debug_assert!(
                                    TokenKind::from_token(token) == TokenKind::At,
                                    "角括弧付き単位の前には `@` トークンが必要です"
                                );
                            }
                            if let Some(token) = self.bump_raw() {
                                last_kind_consumed = TokenKind::from_token(token);
                                last_line_consumed = token.line;
                            }
                            self.consume_inline_whitespace();
                            let mut depth = 0usize;
                            while let Some(token) = self.current_token() {
                                let kind_now = TokenKind::from_token(token);
                                let line_now = token.line;
                                self.bump_raw();
                                if kind_now == TokenKind::LeftBracket {
                                    depth = depth.saturating_add(1);
                                } else if kind_now == TokenKind::RightBracket {
                                    depth = depth.saturating_sub(1);
                                    if depth == 0 {
                                        last_kind_consumed = kind_now;
                                        last_line_consumed = line_now;
                                        break;
                                    }
                                }
                                last_kind_consumed = kind_now;
                                last_line_consumed = line_now;
                            }
                        }
                        UnitSuffixDescriptor::BracketWithoutAt => {
                            let mut depth = 0usize;
                            while let Some(token) = self.current_token() {
                                let kind_now = TokenKind::from_token(token);
                                let line_now = token.line;
                                self.bump_raw();
                                if kind_now == TokenKind::LeftBracket {
                                    depth = depth.saturating_add(1);
                                } else if kind_now == TokenKind::RightBracket {
                                    depth = depth.saturating_sub(1);
                                    if depth == 0 {
                                        last_kind_consumed = kind_now;
                                        last_line_consumed = line_now;
                                        break;
                                    }
                                }
                                last_kind_consumed = kind_now;
                                last_line_consumed = line_now;
                            }
                        }
                    }

                    self.finish_node();

                    last_line = Some(last_line_consumed);
                    second_last_significant_kind = last_significant_kind;
                    last_significant_kind = Some(last_kind_consumed);
                    continue;
                }
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
        let max_index = (start_index + MAX_GENERIC_LOOKAHEAD).min(self.tokens.len());

        while index < max_index {
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
                        let max_lookahead = lookahead.saturating_add(10).min(self.tokens.len());
                        while lookahead < max_lookahead {
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

    pub(crate) fn consume_inline_whitespace(&mut self) -> bool {
        let mut consumed = false;
        while let Some(token) = self.current_token() {
            if TokenKind::from_token(token) == TokenKind::Whitespace {
                self.bump_raw();
                consumed = true;
            } else {
                if token.leading_trivia.spaces > 0 {
                    consumed = true;
                }
                break;
            }
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
        let mut saw_significant = false;
        let mut pending_whitespace = false;
        let mut last_significant_kind: Option<TokenKind> = None;

        loop {
            let Some(token) = self.current_token() else {
                break;
            };
            let kind = TokenKind::from_token(token);

            if kind.is_trivia() {
                if matches!(kind, TokenKind::Whitespace)
                    && saw_significant
                    && angle_depth == 0
                    && paren_depth == 0
                    && bracket_depth == 0
                {
                    pending_whitespace = true;
                } else if matches!(kind, TokenKind::Newline)
                    && saw_significant
                    && angle_depth == 0
                    && paren_depth == 0
                    && bracket_depth == 0
                {
                    break;
                }
                self.bump_raw();
                continue;
            }

            let at_top_level = angle_depth == 0 && paren_depth == 0 && bracket_depth == 0;

            if self.unit_type_annotation_depth > 0
                && last_significant_kind.is_some()
                && at_top_level
            {
                if kind == TokenKind::At {
                    break;
                }
                if kind == TokenKind::Whitespace {
                    if let Some((_, next_kind)) =
                        self.peek_significant_kind_from(self.cursor + 1)
                    {
                        if matches!(next_kind, TokenKind::Identifier | TokenKind::LeftBracket) {
                            break;
                        }
                    }
                }
            }

            if at_top_level && terminators.contains(&kind) {
                break;
            }
            if at_top_level && self.unit_type_annotation_depth > 0 && saw_significant {
                if kind == TokenKind::At {
                    break;
                }
                if pending_whitespace
                    && (Self::token_supports_unit_symbol(token) || kind == TokenKind::LeftBracket)
                {
                    break;
                }
            }

            pending_whitespace = false;
            saw_significant = true;

            match kind {
                TokenKind::Less => angle_depth = angle_depth.saturating_add(1),
                TokenKind::Greater => {
                    if angle_depth > 0 {
                        angle_depth -= 1;
                    }
                }
                TokenKind::LeftParen => paren_depth = paren_depth.saturating_add(1),
                TokenKind::RightParen => {
                    if paren_depth == 0 {
                        break;
                    }
                    paren_depth -= 1;
                }
                TokenKind::LeftBracket => bracket_depth = bracket_depth.saturating_add(1),
                TokenKind::RightBracket => {
                    if bracket_depth == 0 {
                        break;
                    }
                    bracket_depth -= 1;
                }
                _ => {}
            }

            self.bump_raw();
            if !kind.is_trivia() {
                last_significant_kind = Some(kind);
            }
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

    pub(crate) fn peek_significant_token_n(&self, n: usize) -> Option<&'tokens Token> {
        self.peek_significant_kind_n(n)
            .and_then(|(index, _)| self.tokens.get(index))
    }

    fn peek_significant_kind_from(&self, start_index: usize) -> Option<(usize, TokenKind)> {
        let mut index = start_index;
        while index < self.tokens.len() {
            let token = &self.tokens[index];
            let kind = TokenKind::from_token(token);
            if kind.is_trivia() {
                index += 1;
                continue;
            }
            return Some((index, kind));
        }
        None
    }

    /// 指定インデックスの `@` トークン直後にホワイトスペースが存在するかを判定する。
    fn detect_unit_suffix(&self, literal_index: usize) -> Option<UnitSuffixDescriptor> {
        let mut index = literal_index.saturating_add(1);
        let len = self.tokens.len();
        let mut saw_whitespace = false;
        let literal_line = self
            .tokens
            .get(literal_index)
            .map(|token| token.line)
            .unwrap_or(0);

        while index < len {
            let token = &self.tokens[index];
            if token.line != literal_line {
                return None;
            }
            let kind = TokenKind::from_token(token);
            match kind {
                TokenKind::Whitespace => {
                    saw_whitespace = true;
                    index += 1;
                }
                TokenKind::Newline => return None,
                TokenKind::At => {
                    index += 1;
                    while index < len {
                        let token_after = &self.tokens[index];
                        let kind_after = TokenKind::from_token(token_after);
                        match kind_after {
                            TokenKind::Whitespace => index += 1,
                            TokenKind::Newline => return None,
                            TokenKind::LeftBracket => {
                                if self.find_matching_right_bracket(index).is_some() {
                                    return Some(UnitSuffixDescriptor::BracketWithAt);
                                } else {
                                    return None;
                                }
                            }
                            _ if Self::token_supports_unit_symbol(token_after) => {
                                return Some(UnitSuffixDescriptor::SimpleWithAt);
                            }
                            _ => return None,
                        }
                    }
                    return None;
                }
                TokenKind::LeftBracket => {
                    if saw_whitespace {
                        return None;
                    }
                    if self.find_matching_right_bracket(index).is_some() {
                        return Some(UnitSuffixDescriptor::BracketWithoutAt);
                    } else {
                        return None;
                    }
                }
                _ if Self::token_supports_unit_symbol(token) => {
                    if saw_whitespace {
                        return None;
                    }
                    return Some(UnitSuffixDescriptor::SimpleWithoutAt);
                }
                _ => return None,
            }
        }

        None
    }

    fn find_matching_right_bracket(&self, start_index: usize) -> Option<usize> {
        let mut depth = 0usize;
        let len = self.tokens.len();
        let mut index = start_index;

        while index < len {
            let token = &self.tokens[index];
            let kind = TokenKind::from_token(token);
            match kind {
                TokenKind::LeftBracket => depth = depth.saturating_add(1),
                TokenKind::RightBracket => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        return Some(index);
                    }
                }
                TokenKind::Newline => return None,
                _ => {}
            }
            index += 1;
        }

        None
    }
    #[allow(dead_code)]
    pub(crate) fn has_whitespace_after_at(&self, index: usize) -> bool {
        let Some(token) = self.tokens.get(index) else {
            return false;
        };
        if TokenKind::from_token(token) != TokenKind::At {
            return false;
        }

        let next_index = match index.checked_add(1) {
            Some(next) => next,
            None => return false,
        };
        let Some(next_token) = self.tokens.get(next_index) else {
            return false;
        };

        let has_explicit_whitespace =
            matches!(TokenKind::from_token(next_token), TokenKind::Whitespace);
        let has_trivia_gap =
            next_token.leading_trivia.spaces > 0 || next_token.leading_trivia.newlines > 0;
        let token_width = token.lexeme.chars().count().max(1);
        let token_end_column = token.column + token_width;
        let has_inline_gap = next_token.line == token.line && next_token.column > token_end_column;

        has_explicit_whitespace || has_trivia_gap || has_inline_gap
    }

    /// 現在のカーソル位置が `@` であり直後にホワイトスペースが存在するかを判定する。
    #[allow(dead_code)]
    pub(crate) fn cursor_has_whitespace_after_at(&self) -> bool {
        self.has_whitespace_after_at(self.cursor)
    }

    fn token_supports_unit_symbol(token: &Token) -> bool {
        TokenKind::from_token(token) == TokenKind::Identifier
            || matches!(token.token_type, TokenType::Invalid(_))
    }

    fn preview_unit_type_annotation_suffix(
        &self,
        terminators: &[TokenKind],
    ) -> Option<UnitSuffixDescriptor> {
        let mut index = self.cursor;
        let len = self.tokens.len();
        let mut depth_paren = 0usize;
        let mut depth_brace = 0usize;
        let mut depth_bracket = 0usize;
        let mut depth_angle = 0usize;
        let mut saw_significant = false;
        let mut pending_whitespace = false;

        while index < len {
            let token = &self.tokens[index];
            let kind = TokenKind::from_token(token);

            if kind.is_trivia() {
                if saw_significant
                    && matches!(kind, TokenKind::Whitespace)
                    && depth_paren == 0
                    && depth_brace == 0
                    && depth_bracket == 0
                    && depth_angle == 0
                {
                    pending_whitespace = true;
                }
                if saw_significant
                    && matches!(kind, TokenKind::Newline)
                    && depth_paren == 0
                    && depth_brace == 0
                    && depth_bracket == 0
                    && depth_angle == 0
                {
                    break;
                }
                index += 1;
                continue;
            }

            if depth_paren == 0
                && depth_brace == 0
                && depth_bracket == 0
                && depth_angle == 0
                && terminators.contains(&kind)
            {
                break;
            }

            if depth_paren == 0
                && depth_brace == 0
                && depth_bracket == 0
                && depth_angle == 0
                && saw_significant
            {
                if kind == TokenKind::At {
                    if let Some((next_index, next_kind)) =
                        self.peek_significant_kind_from(index + 1)
                    {
                        if next_kind == TokenKind::LeftBracket {
                            return Some(UnitSuffixDescriptor::BracketWithAt);
                        }
                        if let Some(token_after) = self.tokens.get(next_index) {
                            if Self::token_supports_unit_symbol(token_after) {
                                return Some(UnitSuffixDescriptor::SimpleWithAt);
                            }
                        }
                        return None;
                    } else {
                        return None;
                    }
                } else if pending_whitespace {
                    if kind == TokenKind::LeftBracket {
                        return Some(UnitSuffixDescriptor::BracketWithoutAt);
                    }
                    if Self::token_supports_unit_symbol(token) {
                        return Some(UnitSuffixDescriptor::SimpleWithoutAt);
                    }
                    return None;
                }
            }

            match kind {
                TokenKind::LeftParen => depth_paren = depth_paren.saturating_add(1),
                TokenKind::RightParen => {
                    if depth_paren == 0 {
                        break;
                    }
                    depth_paren = depth_paren.saturating_sub(1);
                }
                TokenKind::LeftBrace => depth_brace = depth_brace.saturating_add(1),
                TokenKind::RightBrace => {
                    if depth_brace == 0 {
                        break;
                    }
                    depth_brace = depth_brace.saturating_sub(1);
                }
                TokenKind::LeftBracket => depth_bracket = depth_bracket.saturating_add(1),
                TokenKind::RightBracket => {
                    if depth_bracket == 0 {
                        break;
                    }
                    depth_bracket = depth_bracket.saturating_sub(1);
                }
                TokenKind::Less => depth_angle = depth_angle.saturating_add(1),
                TokenKind::Greater => {
                    if depth_angle > 0 {
                        depth_angle = depth_angle.saturating_sub(1);
                    }
                }
                _ => {}
            }

            saw_significant = true;
            pending_whitespace = false;
            index += 1;
        }

        None
    }

    fn parse_unit_type_annotation_suffix(&mut self, descriptor: UnitSuffixDescriptor) -> bool {
        let start = self.cursor;
        let _ = self.consume_inline_whitespace();

        let parsed = match descriptor {
            UnitSuffixDescriptor::SimpleWithAt => {
                if !self.bump_expected(TokenKind::At, "`@` で単位名を示してください") {
                    return false;
                }
                let _ = self.consume_inline_whitespace();
                if !self.bump_unit_identifier_or_invalid("単位名として識別子を指定してください")
                {
                    return false;
                }
                true
            }
            UnitSuffixDescriptor::SimpleWithoutAt => {
                if !self.bump_unit_identifier_or_invalid("単位名として識別子を指定してください")
                {
                    return false;
                }
                true
            }
            UnitSuffixDescriptor::BracketWithAt => {
                if !self.bump_expected(TokenKind::At, "`@` で単位名を示してください") {
                    return false;
                }
                let _ = self.consume_inline_whitespace();
                self.consume_unit_bracket_symbol(start)
            }
            UnitSuffixDescriptor::BracketWithoutAt => self.consume_unit_bracket_symbol(start),
        };

        if parsed {
            self.report_forbidden_unit_default_marker_if_present();
        }

        parsed
    }

    fn consume_unit_bracket_symbol(&mut self, start: usize) -> bool {
        if !self.bump_expected(TokenKind::LeftBracket, "単位記号は `[` で開始してください")
        {
            return false;
        }
        let mut depth = 1usize;
        while let Some(token) = self.current_token() {
            let kind = TokenKind::from_token(token);
            self.bump_raw();
            match kind {
                TokenKind::LeftBracket => depth = depth.saturating_add(1),
                TokenKind::RightBracket => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        return true;
                    }
                }
                TokenKind::Newline | TokenKind::Eof => {
                    let end = self.cursor;
                    self.report_error(
                        "単位記号は同じ行で `]` を記述して閉じてください",
                        start,
                        end,
                    );
                    return false;
                }
                _ => {}
            }
        }
        let end = self.cursor;
        self.report_error("単位記号は `]` で閉じる必要があります", start, end);
        false
    }

    fn report_forbidden_unit_default_marker_if_present(&mut self) {
        let mut index = self.cursor;
        while index < self.tokens.len() {
            let token = &self.tokens[index];
            let kind = TokenKind::from_token(token);
            if kind == TokenKind::Whitespace {
                index += 1;
                continue;
            }
            if kind == TokenKind::Newline {
                break;
            }
            if kind == TokenKind::Bang {
                self.report_error(
                    DIAGNOSTIC_JV_UNIT_005_DEFAULT_MARKER_FORBIDDEN,
                    index,
                    index + 1,
                );
            }
            break;
        }
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

    fn bump_unit_identifier_or_invalid(&mut self, message: &str) -> bool {
        self.consume_trivia();
        if let Some(token) = self.current_token() {
            if Self::token_supports_unit_symbol(token) {
                self.bump_raw();
                return true;
            }
        }
        let span = self.make_span(self.cursor, self.cursor);
        self.push_diagnostic(ParserDiagnostic::new(
            message,
            DiagnosticSeverity::Error,
            span,
        ));
        false
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
                    TokenKind::Newline | TokenKind::Semicolon | TokenKind::Arrow => {
                        // Arrow may have triggered error recovery, so always advance to avoid infinite loops.
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

#[cfg(test)]
mod tests {
    use super::{ParserContext, UnitSuffixDescriptor};
    use crate::frontend::DIAGNOSTIC_JV_UNIT_005_DEFAULT_MARKER_FORBIDDEN;
    use crate::syntax::TokenKind;
    use jv_lexer::{Lexer, Token, TokenTrivia, TokenType};

    fn make_token(token_type: TokenType, lexeme: &str) -> Token {
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            line: 1,
            column: 0,
            leading_trivia: TokenTrivia::default(),
            diagnostic: None,
            metadata: Vec::new(),
        }
    }

    fn assert_token_kind(token_type: TokenType, lexeme: &str, expected: TokenKind) {
        let token = make_token(token_type, lexeme);
        let actual = TokenKind::from_token(&token);
        assert_eq!(
            actual, expected,
            "token {:?} should map to {:?}, got {:?}",
            token.token_type, expected, actual
        );
    }

    #[test]
    fn rowan_symbols_match_token_kind() {
        use TokenKind::*;

        let keyword_cases = vec![
            (TokenType::Package, "package", PackageKw),
            (TokenType::Import, "import", ImportKw),
            (TokenType::Val, "val", ValKw),
            (TokenType::Var, "var", VarKw),
            (TokenType::Fun, "fun", FunKw),
            (TokenType::Class, "class", ClassKw),
            (TokenType::Data, "data", DataKw),
            (TokenType::When, "when", WhenKw),
            (TokenType::Where, "where", WhereKw),
            (TokenType::If, "if", IfKw),
            (TokenType::Else, "else", ElseKw),
            (TokenType::For, "for", ForKw),
            (TokenType::In, "in", InKw),
            (TokenType::While, "while", WhileKw),
            (TokenType::Do, "do", DoKw),
            (TokenType::Return, "return", ReturnKw),
            (TokenType::Throw, "throw", ThrowKw),
            (TokenType::Break, "break", BreakKw),
            (TokenType::Continue, "continue", ContinueKw),
            (TokenType::True, "true", TrueKw),
            (TokenType::False, "false", FalseKw),
            (TokenType::Null, "null", NullKw),
            (TokenType::Log, "LOG", LogKw),
            (TokenType::Trace, "TRACE", TraceKw),
            (TokenType::Debug, "DEBUG", DebugKw),
            (TokenType::Info, "INFO", InfoKw),
            (TokenType::Warn, "WARN", WarnKw),
            (TokenType::Error, "ERROR", ErrorKw),
        ];

        let operator_cases = vec![
            (TokenType::Assign, "=", Assign),
            (TokenType::Plus, "+", Plus),
            (TokenType::Minus, "-", Minus),
            (TokenType::Multiply, "*", Star),
            (TokenType::Divide, "/", Slash),
            (TokenType::Modulo, "%", Percent),
            (TokenType::Equal, "==", EqualEqual),
            (TokenType::NotEqual, "!=", NotEqual),
            (TokenType::Less, "<", Less),
            (TokenType::LessEqual, "<=", LessEqual),
            (TokenType::Greater, ">", Greater),
            (TokenType::GreaterEqual, ">=", GreaterEqual),
            (TokenType::And, "&&", AndAnd),
            (TokenType::Or, "||", OrOr),
            (TokenType::Not, "!", Bang),
            (TokenType::RangeExclusive, "..", RangeExclusive),
            (TokenType::RangeInclusive, "..=", RangeInclusive),
            (TokenType::Question, "?", Question),
            (TokenType::NullSafe, "?.", NullSafe),
            (TokenType::Elvis, "?:", Elvis),
            (TokenType::Arrow, "->", Arrow),
            (TokenType::FatArrow, "=>", FatArrow),
            (TokenType::DoubleColon, "::", DoubleColon),
        ];

        let punctuation_cases = vec![
            (TokenType::LeftParen, "(", LeftParen),
            (TokenType::RightParen, ")", RightParen),
            (TokenType::LeftBrace, "{", LeftBrace),
            (TokenType::RightBrace, "}", RightBrace),
            (TokenType::LeftBracket, "[", LeftBracket),
            (TokenType::RightBracket, "]", RightBracket),
            (TokenType::Comma, ",", Comma),
            (TokenType::LayoutComma, ",", LayoutComma),
            (TokenType::Dot, ".", Dot),
            (TokenType::Semicolon, ";", Semicolon),
            (TokenType::Colon, ":", Colon),
            (TokenType::At, "@", At),
        ];

        let literal_cases = vec![
            (
                TokenType::String("\"text\"".into()),
                "\"text\"",
                StringLiteral,
            ),
            (
                TokenType::StringInterpolation("${value}".into()),
                "${value}",
                StringLiteral,
            ),
            (TokenType::Number("42".into()), "42", NumberLiteral),
            (TokenType::Character('a'), "'a'", CharacterLiteral),
            (TokenType::Identifier("name".into()), "name", Identifier),
            (TokenType::Boolean(true), "true", BooleanLiteral),
            (TokenType::RegexLiteral(".*".into()), "/.*/", RegexLiteral),
        ];

        let trivia_cases = vec![
            (TokenType::Whitespace(" ".into()), " ", Whitespace),
            (TokenType::Newline, "\n", Newline),
            (TokenType::LineComment("// a".into()), "// a", LineComment),
            (
                TokenType::BlockComment("/* a */".into()),
                "/* a */",
                BlockComment,
            ),
            (
                TokenType::JavaDocComment("/** a */".into()),
                "/** a */",
                DocComment,
            ),
        ];

        let misc_cases = vec![
            (TokenType::StringStart, "\"$", StringStart),
            (TokenType::StringMid, "}", StringMid),
            (TokenType::StringEnd, "\"", StringEnd),
            (TokenType::Eof, "", Eof),
            (TokenType::Invalid("???".into()), "???", Unknown),
        ];

        for (token_type, lexeme, expected) in keyword_cases
            .into_iter()
            .chain(operator_cases)
            .chain(punctuation_cases)
            .chain(literal_cases)
            .chain(trivia_cases)
            .chain(misc_cases)
        {
            assert_token_kind(token_type, lexeme, expected);
        }

        assert_token_kind(TokenType::Identifier("use".into()), "use", UseKw);
        assert_token_kind(TokenType::Identifier("defer".into()), "defer", DeferKw);
        assert_token_kind(TokenType::Identifier("spawn".into()), "spawn", SpawnKw);
    }

    #[test]
    fn detects_unit_definition_block() {
        let tokens = vec![
            make_token(TokenType::At, "@"),
            make_token(TokenType::Identifier("Length".into()), "Length"),
            make_token(TokenType::LeftParen, "("),
            make_token(TokenType::Identifier("Double".into()), "Double"),
            make_token(TokenType::RightParen, ")"),
            make_token(TokenType::Identifier("m".into()), "m"),
            make_token(TokenType::LeftBrace, "{"),
            make_token(TokenType::Identifier("基準".into()), "基準"),
            make_token(TokenType::Colon, ":"),
            make_token(TokenType::Assign, "="),
            make_token(TokenType::Identifier("1".into()), "1"),
            make_token(TokenType::RightBrace, "}"),
        ];

        let mut ctx = ParserContext::new(tokens.as_slice());
        ctx.parse_statement_list(None);

        assert!(ctx
            .diagnostics
            .iter()
            .all(|diag| diag.message != DIAGNOSTIC_JV_UNIT_005_DEFAULT_MARKER_FORBIDDEN));
    }

    #[test]
    fn detects_whitespace_after_at() {
        let tokens = vec![
            make_token(TokenType::At, "@"),
            make_token(TokenType::Whitespace(" ".into()), " "),
            make_token(TokenType::Identifier("unit".into()), "unit"),
        ];
        let ctx = ParserContext::new(tokens.as_slice());
        assert!(ctx.has_whitespace_after_at(0));
        assert!(ctx.cursor_has_whitespace_after_at());
    }

    #[test]
    fn detects_absence_of_whitespace_after_at() {
        let tokens = vec![
            make_token(TokenType::At, "@"),
            make_token(TokenType::Identifier("unit".into()), "unit"),
        ];
        let ctx = ParserContext::new(tokens.as_slice());
        assert!(!ctx.has_whitespace_after_at(0));
        assert!(!ctx.cursor_has_whitespace_after_at());
    }

    fn lex_tokens(source: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(source.to_owned());
        lexer.tokenize().expect("字句解析に成功すること")
    }

    #[test]
    fn detects_real_world_whitespace_after_at() {
        let tokens = lex_tokens("@ 単位(Double) m {");
        let ctx = ParserContext::new(tokens.as_slice());
        assert!(
            ctx.has_whitespace_after_at(0),
            "実際のトークン列で空白ありケースを検出できること"
        );
        assert!(ctx.cursor_has_whitespace_after_at());
    }

    #[test]
    fn detects_missing_whitespace_with_lexer_tokens() {
        let tokens = lex_tokens("@単位(Double) m {");
        let ctx = ParserContext::new(tokens.as_slice());
        assert!(
            !ctx.has_whitespace_after_at(0),
            "実際のトークン列で空白なしケースを検出できること"
        );
        assert!(!ctx.cursor_has_whitespace_after_at());
    }

    #[test]
    fn ignores_non_at_tokens() {
        let tokens = vec![
            make_token(TokenType::Identifier("leading".into()), "leading"),
            make_token(TokenType::Whitespace(" ".into()), " "),
            make_token(TokenType::At, "@"),
            make_token(TokenType::Whitespace("\t".into()), "\t"),
            make_token(TokenType::Identifier("unit".into()), "unit"),
        ];
        let mut ctx = ParserContext::new(tokens.as_slice());
        assert!(!ctx.cursor_has_whitespace_after_at());

        ctx.bump_raw();
        assert!(!ctx.cursor_has_whitespace_after_at());

        ctx.bump_raw();
        assert!(ctx.cursor_has_whitespace_after_at());
        assert!(ctx.has_whitespace_after_at(ctx.position()));
    }

    #[test]
    fn 型注釈でアット付き単位を検出できる() {
        let tokens = vec![
            make_token(TokenType::Identifier("Int".into()), "Int"),
            make_token(TokenType::At, "@"),
            make_token(TokenType::Identifier("m".into()), "m"),
        ];
        let ctx = ParserContext::new(tokens.as_slice());
        let descriptor = ctx.preview_unit_type_annotation_suffix(&[
            TokenKind::Assign,
            TokenKind::LeftBrace,
            TokenKind::Semicolon,
            TokenKind::RightBrace,
            TokenKind::Comma,
            TokenKind::RightParen,
            TokenKind::WhereKw,
        ]);
        assert!(matches!(
            descriptor,
            Some(UnitSuffixDescriptor::SimpleWithAt)
        ));
    }

    #[test]
    fn 型注釈でスペース区切り単位を検出できる() {
        let tokens = vec![
            make_token(TokenType::Identifier("Int".into()), "Int"),
            make_token(TokenType::Whitespace(" ".into()), " "),
            make_token(TokenType::Identifier("m".into()), "m"),
        ];
        let ctx = ParserContext::new(tokens.as_slice());
        let descriptor = ctx.preview_unit_type_annotation_suffix(&[
            TokenKind::Assign,
            TokenKind::LeftBrace,
            TokenKind::Semicolon,
            TokenKind::RightBrace,
            TokenKind::Comma,
            TokenKind::RightParen,
            TokenKind::WhereKw,
        ]);
        assert!(matches!(
            descriptor,
            Some(UnitSuffixDescriptor::SimpleWithoutAt)
        ));
    }

    #[test]
    fn 型注釈で角括弧付き単位を検出できる() {
        let tokens = vec![
            make_token(TokenType::Identifier("Int".into()), "Int"),
            make_token(TokenType::Whitespace(" ".into()), " "),
            make_token(TokenType::LeftBracket, "["),
            make_token(TokenType::Identifier("degC".into()), "degC"),
            make_token(TokenType::RightBracket, "]"),
        ];
        let ctx = ParserContext::new(tokens.as_slice());
        let descriptor = ctx.preview_unit_type_annotation_suffix(&[
            TokenKind::Assign,
            TokenKind::LeftBrace,
            TokenKind::Semicolon,
            TokenKind::RightBrace,
            TokenKind::Comma,
            TokenKind::RightParen,
            TokenKind::WhereKw,
        ]);
        assert!(matches!(
            descriptor,
            Some(UnitSuffixDescriptor::BracketWithoutAt)
        ));
    }

    #[test]
    fn 型注釈で単位がない場合は検出しない() {
        let tokens = vec![make_token(TokenType::Identifier("Int".into()), "Int")];
        let ctx = ParserContext::new(tokens.as_slice());
        let descriptor = ctx.preview_unit_type_annotation_suffix(&[
            TokenKind::Assign,
            TokenKind::LeftBrace,
            TokenKind::Semicolon,
            TokenKind::RightBrace,
            TokenKind::Comma,
            TokenKind::RightParen,
            TokenKind::WhereKw,
        ]);
        assert!(descriptor.is_none());
    }

    #[test]
    fn parse_expression_until_keeps_parsing_when_arrow_within_return() {
        let source = r#"
            return when (value) {
                is Int -> "int"
                else -> "other"
            }
        "#;

        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer
            .tokenize()
            .expect("lexing should succeed for parser tests");
        let arrow_index = tokens
            .iter()
            .position(|token| TokenKind::from_token(token) == TokenKind::Arrow)
            .expect("fixture should contain an arrow token");

        let mut context = ParserContext::new(&tokens);
        assert!(
            context.bump_expected(TokenKind::ReturnKw, "test should consume return keyword"),
            "return keyword should be present"
        );

        let terminators = [
            TokenKind::Semicolon,
            TokenKind::Newline,
            TokenKind::RightBrace,
        ];
        let consumed = context.parse_expression_until(&terminators, true);
        assert!(
            consumed,
            "when expression body should be parsed as return payload"
        );
        assert!(
            context.position() > arrow_index,
            "cursor must advance past the when arrow to avoid syncing on it"
        );
    }
}
