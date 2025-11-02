use jv_lexer::{Token, TokenType};

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

const FORBIDDEN_IF_DIAGNOSTIC: &str = "JV3103: if expressions are not supported; use when instead.\nJV3103: if式はサポートされていません。when式を使用してください。\nQuick Fix: when.convert.if -> when { 条件 -> 真分岐; else -> 偽分岐 } (例: if (x > 0) a else b => when { x > 0 -> a; else -> b })\nQuick Fix: when.convert.if -> when { condition -> thenBranch; else -> elseBranch } (Example: if (x > 0) a else b => when { x > 0 -> a; else -> b })";

#[derive(Default)]
struct WhenBlockState {
    brace_depth: usize,
}

fn is_short_mode_identifier(token: &Token) -> bool {
    match &token.token_type {
        TokenType::Identifier(value) if value.len() == 1 => matches!(
            value.as_bytes()[0],
            b'a' | b'A' | b'f' | b'F' | b'm' | b'M' | b's' | b'S' | b'i' | b'I'
        ),
        _ => false,
    }
}

fn consume_explicit_mode_tokens(
    collected: &[(usize, TokenKind, &Token)],
    index: &mut usize,
) -> bool {
    let len = collected.len();
    *index += 1;
    while *index < len {
        if collected[*index].1 == TokenKind::RightBracket {
            *index += 1;
            return true;
        }
        *index += 1;
    }
    false
}

fn flag_token_content(token: &Token) -> Option<String> {
    let value = match &token.token_type {
        TokenType::Identifier(value)
        | TokenType::String(value)
        | TokenType::StringInterpolation(value) => value.clone(),
        _ => return None,
    };

    if value.chars().all(|ch| {
        matches!(
            ch,
            'i' | 'I'
                | 'm'
                | 'M'
                | 's'
                | 'S'
                | 'u'
                | 'U'
                | 'd'
                | 'D'
                | 'x'
                | 'X'
                | 'l'
                | 'L'
                | 'c'
                | 'C'
        )
    }) {
        Some(value)
    } else {
        None
    }
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

            if respect_statement_boundaries && at_top_level {
                if let Some((line, consumed_kind, prev_kind)) = self.try_start_regex_command_node()
                {
                    last_line = Some(line);
                    if let Some(prev) = prev_kind {
                        second_last_significant_kind = Some(prev);
                    } else {
                        second_last_significant_kind = last_significant_kind;
                    }
                    last_significant_kind = Some(consumed_kind);
                    continue;
                }
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

            if at_top_level && kind == TokenKind::IfKw {
                let span = TokenSpan::new(self.cursor, self.cursor.saturating_add(1));
                self.push_diagnostic(ParserDiagnostic::new(
                    FORBIDDEN_IF_DIAGNOSTIC,
                    DiagnosticSeverity::Error,
                    span,
                ));
                self.consume_forbidden_if_expression(terminators, respect_statement_boundaries);
                self.reset_active_expression_state();
                break;
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

    fn consume_forbidden_if_expression(
        &mut self,
        terminators: &[TokenKind],
        respect_statement_boundaries: bool,
    ) {
        if self.peek_significant_kind() != Some(TokenKind::IfKw) {
            return;
        }

        self.bump_raw(); // 'if'

        let mut depth_paren = 0usize;
        let mut depth_brace = 0usize;
        let mut depth_bracket = 0usize;

        while let Some(token) = self.current_token() {
            let kind = TokenKind::from_token(token);

            if kind == TokenKind::Eof {
                self.bump_raw();
                break;
            }

            if kind.is_trivia() {
                self.bump_raw();
                continue;
            }

            let at_top_level = depth_paren == 0 && depth_brace == 0 && depth_bracket == 0;

            if at_top_level && terminators.contains(&kind) {
                break;
            }

            if respect_statement_boundaries && at_top_level && SYNC_TOKENS.contains(&kind) {
                break;
            }

            match kind {
                TokenKind::LeftParen => depth_paren = depth_paren.saturating_add(1),
                TokenKind::RightParen => {
                    if depth_paren > 0 {
                        depth_paren -= 1;
                    }
                }
                TokenKind::LeftBrace => depth_brace = depth_brace.saturating_add(1),
                TokenKind::RightBrace => {
                    if depth_brace > 0 {
                        depth_brace -= 1;
                    } else {
                        break;
                    }
                }
                TokenKind::LeftBracket => depth_bracket = depth_bracket.saturating_add(1),
                TokenKind::RightBracket => {
                    if depth_bracket > 0 {
                        depth_bracket -= 1;
                    } else {
                        break;
                    }
                }
                _ => {}
            }

            self.bump_raw();
        }
    }

    fn try_start_regex_command_node(&mut self) -> Option<(usize, TokenKind, Option<TokenKind>)> {
        let original_cursor = self.cursor;
        let mut collected: Vec<(usize, TokenKind, &Token)> = Vec::new();
        let mut raw_index = self.cursor;
        let mut brace_depth = 0usize;
        let mut paren_depth = 0usize;
        let mut bracket_depth = 0usize;

        while raw_index < self.tokens.len() && collected.len() < 64 {
            let token = &self.tokens[raw_index];
            let kind = TokenKind::from_token(token);
            if matches!(kind, TokenKind::Eof) {
                break;
            }

            if kind.is_trivia() {
                raw_index += 1;
                continue;
            }

            if !collected.is_empty() {
                if kind == TokenKind::Semicolon
                    && brace_depth == 0
                    && paren_depth == 0
                    && bracket_depth == 0
                {
                    break;
                }
                if kind == TokenKind::RightBrace && brace_depth == 0 {
                    break;
                }
            }

            collected.push((raw_index, kind, token));

            match kind {
                TokenKind::LeftBrace => brace_depth = brace_depth.saturating_add(1),
                TokenKind::RightBrace => brace_depth = brace_depth.saturating_sub(1),
                TokenKind::LeftParen => paren_depth = paren_depth.saturating_add(1),
                TokenKind::RightParen => paren_depth = paren_depth.saturating_sub(1),
                TokenKind::LeftBracket => bracket_depth = bracket_depth.saturating_add(1),
                TokenKind::RightBracket => bracket_depth = bracket_depth.saturating_sub(1),
                _ => {}
            }

            raw_index += 1;
        }

        if collected.is_empty() {
            return None;
        }

        let len = collected.len();
        let mut idx = 0usize;
        let token_starts_new_statement = |kind: TokenKind| -> bool {
            matches!(
                kind,
                TokenKind::ValKw
                    | TokenKind::VarKw
                    | TokenKind::FunKw
                    | TokenKind::ClassKw
                    | TokenKind::WhenKw
                    | TokenKind::ForKw
                    | TokenKind::ReturnKw
                    | TokenKind::ThrowKw
                    | TokenKind::BreakKw
                    | TokenKind::ContinueKw
                    | TokenKind::UseKw
                    | TokenKind::DeferKw
                    | TokenKind::SpawnKw
                    | TokenKind::PackageKw
                    | TokenKind::ImportKw
                    | TokenKind::Semicolon
                    | TokenKind::Eof
            )
        };
        let token_is_type_cast = |index: usize| -> bool {
            index < len
                && matches!(collected[index].1, TokenKind::Identifier)
                && collected[index].2.lexeme.eq_ignore_ascii_case("as")
        };
        let boundary_due_to_layout = |index: usize| -> bool {
            if index >= len {
                return false;
            }
            let (_, kind, token) = collected[index];
            if kind == TokenKind::RightBrace {
                return false;
            }
            if token.leading_trivia.newlines > 0 && token.column == 1 {
                let next_is_slash = index + 1 < len && collected[index + 1].1 == TokenKind::Slash;
                return !next_is_slash;
            }
            false
        };

        if let Some((_, kind, token)) = collected.get(idx) {
            if *kind == TokenKind::Identifier && is_short_mode_identifier(token) {
                idx += 1;
            } else if *kind == TokenKind::LeftBracket {
                if !consume_explicit_mode_tokens(&collected, &mut idx) {
                    self.cursor = original_cursor;
                    return None;
                }
            } else if *kind != TokenKind::Slash {
                self.cursor = original_cursor;
                return None;
            }
        }

        if idx >= len || collected[idx].1 != TokenKind::Slash {
            self.cursor = original_cursor;
            return None;
        }
        idx += 1;
        if idx >= len {
            self.cursor = original_cursor;
            return None;
        }

        let subject_start = idx;
        let mut pattern_index = None;
        while idx < len {
            if matches!(collected[idx].2.token_type, TokenType::RegexLiteral(_)) {
                pattern_index = Some(idx);
                break;
            }
            idx += 1;
        }

        let Some(pattern_index) = pattern_index else {
            self.cursor = original_cursor;
            return None;
        };

        if subject_start == pattern_index {
            self.cursor = original_cursor;
            return None;
        }

        idx = pattern_index + 1;

        if idx >= len
            || boundary_due_to_layout(idx)
            || token_starts_new_statement(collected[idx].1)
            || token_is_type_cast(idx)
        {
            // command terminates immediately after pattern
        } else if collected[idx].1 == TokenKind::Slash {
            idx += 1;
            let mut replacement_end = idx;
            let mut brace_depth = 0usize;
            let mut paren_depth = 0usize;
            let mut bracket_depth = 0usize;
            let mut ended_by_closing_slash = false;

            while replacement_end < len {
                let (_, kind, _) = collected[replacement_end];

                if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 {
                    if kind == TokenKind::Slash {
                        ended_by_closing_slash = true;
                        break;
                    }
                    if boundary_due_to_layout(replacement_end) {
                        break;
                    }
                    if token_starts_new_statement(kind) {
                        break;
                    }
                    if token_is_type_cast(replacement_end) {
                        break;
                    }
                }

                match kind {
                    TokenKind::LeftBrace => brace_depth = brace_depth.saturating_add(1),
                    TokenKind::RightBrace => {
                        if brace_depth == 0 {
                            break;
                        }
                        brace_depth -= 1;
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

                replacement_end += 1;
            }

            if ended_by_closing_slash {
                idx = replacement_end + 1;

                if idx < len
                    && !boundary_due_to_layout(idx)
                    && !token_starts_new_statement(collected[idx].1)
                    && !token_is_type_cast(idx)
                    && flag_token_content(collected[idx].2).is_some()
                {
                    idx += 1;
                }
            } else {
                idx = replacement_end;
            }
        } else if idx < len {
            let mut brace_depth = 0usize;
            let mut paren_depth = 0usize;
            let mut bracket_depth = 0usize;

            while idx < len {
                let (_, kind, _) = collected[idx];

                if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 {
                    if kind == TokenKind::Slash {
                        break;
                    }
                    if boundary_due_to_layout(idx) {
                        break;
                    }
                    if token_starts_new_statement(kind) {
                        break;
                    }
                    if token_is_type_cast(idx) {
                        break;
                    }
                }

                match kind {
                    TokenKind::LeftBrace => brace_depth = brace_depth.saturating_add(1),
                    TokenKind::RightBrace => {
                        if brace_depth == 0 {
                            break;
                        }
                        brace_depth -= 1;
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

                idx += 1;
            }

            if idx < len && collected[idx].1 == TokenKind::Slash {
                idx += 1;

                if idx < len
                    && !boundary_due_to_layout(idx)
                    && !token_starts_new_statement(collected[idx].1)
                    && !token_is_type_cast(idx)
                    && flag_token_content(collected[idx].2).is_some()
                {
                    idx += 1;
                }
            }
        }

        let consumed = idx;
        if consumed == 0 || consumed > collected.len() {
            self.cursor = original_cursor;
            return None;
        }

        let last_entry = &collected[consumed - 1];
        let last_raw_index = last_entry.0;

        self.start_node(SyntaxKind::RegexCommand);
        while self.cursor <= last_raw_index {
            self.bump_raw();
        }
        self.finish_node();

        let last_kind = last_entry.1;
        let last_line = last_entry.2.line;
        let prev_kind = if consumed >= 2 {
            Some(collected[consumed - 2].1)
        } else {
            None
        };

        Some((last_line, last_kind, prev_kind))
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
