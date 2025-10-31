use std::collections::{HashMap, HashSet};

use crate::Position;
use jv_ast::{
    Argument, Expression, Program, RegexCommand, RegexCommandMode, RegexCommandModeOrigin,
    RegexReplacement, RegexTemplateSegment, Span, Statement, StringPart, TryCatchClause,
};
use jv_checker::inference::{RegexCommandIssue, RegexCommandTyping};
use jv_lexer::{Token, TokenType};

#[derive(Clone, Debug, Default)]
pub struct RegexCommandIndex {
    entries: Vec<RegexCommandEntry>,
}

#[derive(Clone, Debug)]
struct RegexCommandEntry {
    span: Span,
    mode_span: Option<Span>,
    flags_span: Option<Span>,
    mode: RegexCommandMode,
    mode_origin: RegexCommandModeOrigin,
    raw_flags: Option<String>,
    issues: Vec<RegexCommandIssue>,
}

#[derive(Clone, Debug)]
pub struct RegexCommandDiagnostic {
    pub span: Span,
    pub code: String,
    pub message: String,
    pub suggestions: Vec<String>,
}

impl RegexCommandIndex {
    pub fn build(program: &Program, tokens: &[Token], typings: &[RegexCommandTyping]) -> Self {
        let mut typing_map: HashMap<SpanKey, Vec<RegexCommandIssue>> = HashMap::new();
        for typing in typings {
            typing_map.insert(SpanKey::from_span(&typing.span), typing.diagnostics.clone());
        }

        let mut collector = CommandCollector {
            tokens,
            typing_map,
            entries: Vec::new(),
        };
        collector.visit_program(program);

        Self {
            entries: collector.entries,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn diagnostics(&self) -> Vec<RegexCommandDiagnostic> {
        let mut diagnostics = Vec::new();
        for entry in &self.entries {
            for issue in &entry.issues {
                diagnostics.push(RegexCommandDiagnostic {
                    span: entry.span.clone(),
                    code: issue.code.clone(),
                    message: issue.message.clone(),
                    suggestions: quick_fix_suggestions(entry, &issue.code),
                });
            }
        }
        diagnostics
    }

    pub fn completions(&self, position: &Position) -> Vec<String> {
        let mut items = Vec::new();
        for entry in &self.entries {
            if let Some(mode_span) = entry.mode_span.as_ref() {
                if position_in_span(position, mode_span) {
                    items.extend(mode_completion_items());
                }
            }

            if let Some(flags_span) = entry.flags_span.as_ref() {
                if position_in_span(position, flags_span) {
                    let existing: HashSet<char> = entry
                        .raw_flags
                        .as_deref()
                        .unwrap_or("")
                        .chars()
                        .map(|ch| ch.to_ascii_lowercase())
                        .collect();
                    items.extend(flag_completion_items(&existing));
                }
            }
        }
        items
    }

    pub fn hover(&self, position: &Position) -> Option<(Span, String)> {
        for entry in &self.entries {
            if entry.mode != RegexCommandMode::Split {
                continue;
            }

            let anchor = entry.mode_span.as_ref().unwrap_or(&entry.span);
            if !position_in_span(position, anchor) && !position_in_span(position, &entry.span) {
                continue;
            }

            let mut lines = Vec::new();
            lines.push("RegexCommand `Split` モード".to_string());
            lines.push(
                "`Pattern.split(subject, -1)` を生成し、戻り値は `String[]` です。".to_string(),
            );
            lines.push("大きな入力では配列が肥大化するため、必要に応じて `i` モードで逐次処理することを検討してください。".to_string());
            return Some((anchor.clone(), lines.join("\n")));
        }

        None
    }
}

struct CommandCollector<'a> {
    tokens: &'a [Token],
    typing_map: HashMap<SpanKey, Vec<RegexCommandIssue>>,
    entries: Vec<RegexCommandEntry>,
}

impl<'a> CommandCollector<'a> {
    fn visit_program(&mut self, program: &Program) {
        for statement in &program.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::ValDeclaration { initializer, .. } => {
                self.visit_expression(initializer);
            }
            Statement::VarDeclaration {
                initializer: Some(expr),
                ..
            } => {
                self.visit_expression(expr);
            }
            Statement::VarDeclaration { .. } => {}
            Statement::FunctionDeclaration {
                parameters, body, ..
            } => {
                for param in parameters {
                    if let Some(default) = &param.default_value {
                        self.visit_expression(default);
                    }
                }
                self.visit_expression(body);
            }
            Statement::ClassDeclaration {
                properties,
                methods,
                ..
            } => {
                for property in properties {
                    if let Some(initializer) = &property.initializer {
                        self.visit_expression(initializer);
                    }
                    if let Some(getter) = &property.getter {
                        self.visit_expression(getter);
                    }
                    if let Some(setter) = &property.setter {
                        self.visit_expression(setter);
                    }
                }
                for method in methods {
                    self.visit_statement(method);
                }
            }
            Statement::InterfaceDeclaration {
                properties,
                methods,
                ..
            } => {
                for property in properties {
                    if let Some(initializer) = &property.initializer {
                        self.visit_expression(initializer);
                    }
                }
                for method in methods {
                    self.visit_statement(method);
                }
            }
            Statement::ExtensionFunction(extension) => {
                self.visit_statement(extension.function.as_ref());
            }
            Statement::Expression { expr, .. } => self.visit_expression(expr),
            Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.visit_expression(expr);
                }
            }
            Statement::Throw { expr, .. } => self.visit_expression(expr),
            Statement::Assignment { target, value, .. } => {
                self.visit_expression(target);
                self.visit_expression(value);
            }
            Statement::ForIn(statement) => {
                self.visit_expression(&statement.iterable);
                self.visit_loop_strategy(&statement.strategy);
                self.visit_expression(&statement.body);
            }
            Statement::Break(_) | Statement::Continue(_) | Statement::Comment(_) => {}
            Statement::Import { .. } | Statement::Package { .. } => {}
            Statement::Concurrency(construct) => self.visit_concurrency(construct),
            Statement::ResourceManagement(resource) => self.visit_resource_management(resource),
            Statement::DataClassDeclaration { .. } => {}
        }
    }

    fn visit_concurrency(&mut self, construct: &jv_ast::ConcurrencyConstruct) {
        match construct {
            jv_ast::ConcurrencyConstruct::Spawn { body, .. }
            | jv_ast::ConcurrencyConstruct::Async { body, .. } => self.visit_expression(body),
            jv_ast::ConcurrencyConstruct::Await { expr, .. } => self.visit_expression(expr),
        }
    }

    fn visit_resource_management(&mut self, resource: &jv_ast::ResourceManagement) {
        match resource {
            jv_ast::ResourceManagement::Use { resource, body, .. } => {
                self.visit_expression(resource);
                self.visit_expression(body);
            }
            jv_ast::ResourceManagement::Defer { body, .. } => self.visit_expression(body),
        }
    }

    fn visit_loop_strategy(&mut self, strategy: &jv_ast::LoopStrategy) {
        if let jv_ast::LoopStrategy::NumericRange(range) = strategy {
            self.visit_expression(&range.start);
            self.visit_expression(&range.end);
        }
    }

    fn visit_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Literal(_, _)
            | Expression::RegexLiteral(_)
            | Expression::Identifier(_, _)
            | Expression::This(_)
            | Expression::Super(_) => {}
            Expression::RegexCommand(command) => {
                self.push_command(command);
                self.visit_expression(&command.subject);
                if let Some(replacement) = &command.replacement {
                    self.visit_replacement(replacement);
                }
            }
            Expression::Binary { left, right, .. } => {
                self.visit_expression(left);
                self.visit_expression(right);
            }
            Expression::Unary { operand, .. } => self.visit_expression(operand),
            Expression::Call { function, args, .. } => {
                self.visit_expression(function);
                for argument in args {
                    self.visit_argument(argument);
                }
            }
            Expression::MemberAccess { object, .. }
            | Expression::NullSafeMemberAccess { object, .. } => self.visit_expression(object),
            Expression::IndexAccess { object, index, .. }
            | Expression::NullSafeIndexAccess { object, index, .. } => {
                self.visit_expression(object);
                self.visit_expression(index);
            }
            Expression::TypeCast { expr, .. } => self.visit_expression(expr),
            Expression::StringInterpolation { parts, .. } => {
                for part in parts {
                    if let StringPart::Expression(inner) = part {
                        self.visit_expression(inner);
                    }
                }
            }
            Expression::MultilineString(literal) => {
                for part in &literal.parts {
                    if let StringPart::Expression(inner) = part {
                        self.visit_expression(inner);
                    }
                }
            }
            Expression::JsonLiteral(_) => {}
            Expression::When {
                expr,
                arms,
                else_arm,
                ..
            } => {
                if let Some(cond) = expr {
                    self.visit_expression(cond);
                }
                for arm in arms {
                    self.visit_expression(&arm.body);
                }
                if let Some(otherwise) = else_arm {
                    self.visit_expression(otherwise);
                }
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.visit_expression(condition);
                self.visit_expression(then_branch);
                if let Some(else_branch) = else_branch {
                    self.visit_expression(else_branch);
                }
            }
            Expression::Block { statements, .. } => {
                for stmt in statements {
                    self.visit_statement(stmt);
                }
            }
            Expression::Array { elements, .. } => {
                for element in elements {
                    self.visit_expression(element);
                }
            }
            Expression::Lambda {
                parameters, body, ..
            } => {
                for param in parameters {
                    if let Some(default) = &param.default_value {
                        self.visit_expression(default);
                    }
                }
                self.visit_expression(body);
            }
            Expression::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                self.visit_expression(body);
                for clause in catch_clauses {
                    self.visit_catch_clause(clause);
                }
                if let Some(finally_expr) = finally_block {
                    self.visit_expression(finally_expr);
                }
            }
        }
    }

    fn visit_argument(&mut self, argument: &Argument) {
        match argument {
            Argument::Positional(expr) => self.visit_expression(expr),
            Argument::Named { value, .. } => self.visit_expression(value),
        }
    }

    fn visit_replacement(&mut self, replacement: &RegexReplacement) {
        match replacement {
            RegexReplacement::Literal(literal) => {
                for segment in &literal.template_segments {
                    if let RegexTemplateSegment::Expression(expr) = segment {
                        self.visit_expression(expr);
                    }
                }
            }
            RegexReplacement::Expression(expr) => self.visit_expression(expr),
            RegexReplacement::Lambda(lambda) => {
                for param in &lambda.params {
                    if let Some(default) = &param.default_value {
                        self.visit_expression(default);
                    }
                }
                self.visit_expression(&lambda.body);
            }
        }
    }

    fn visit_catch_clause(&mut self, clause: &TryCatchClause) {
        if let Some(parameter) = &clause.parameter {
            if let Some(default) = &parameter.default_value {
                self.visit_expression(default);
            }
        }
        self.visit_expression(&clause.body);
    }

    fn push_command(&mut self, command: &RegexCommand) {
        let key = SpanKey::from_span(&command.span);
        let issues = self.typing_map.get(&key).cloned().unwrap_or_default();

        let regions = compute_regions(command, self.tokens);

        self.entries.push(RegexCommandEntry {
            span: command.span.clone(),
            mode_span: regions.mode_span.or_else(|| {
                find_token_at_span(self.tokens, &command.span)
                    .map(|idx| span_from_token(&self.tokens[idx]))
            }),
            flags_span: regions.flags_span,
            mode: command.mode,
            mode_origin: command.mode_origin,
            raw_flags: command.raw_flags.clone(),
            issues,
        });
    }
}

fn quick_fix_suggestions(entry: &RegexCommandEntry, code: &str) -> Vec<String> {
    match code {
        "JV_REGEX_I001" => match entry.mode_origin {
            RegexCommandModeOrigin::ShortMode => vec![
                "Quick Fix: regex.command.mode.remove -> 先頭の短縮モード記号を削除し、デフォルトの matches モードを利用する".to_string(),
                "Quick Fix: regex.command.mode.expand -> `[match]` を用いてモードを明示し、フラグの `m` と区別する".to_string(),
            ],
            RegexCommandModeOrigin::ExplicitToken => vec![
                "Quick Fix: regex.command.mode.rewrite -> `[match]` の代わりにモードを省略し、フラグ `m` との混同を避ける".to_string(),
            ],
            _ => vec![
                "Quick Fix: regex.command.mode.annotate -> 先頭に `[match]` を追加してモードとフラグを区別する".to_string(),
            ],
        },
        "JV_REGEX_I002" => vec![
            "Quick Fix: regex.command.mode.explicit-match -> 先頭に `m` モードを記述して判定目的であることを明示する".to_string(),
            "Quick Fix: regex.command.replacement.add-empty -> 置換部 `//` を挿入して置換モードであることを示す".to_string(),
        ],
        "JV_REGEX_I003" => match entry.mode {
            RegexCommandMode::Split => vec![
                "Quick Fix: regex.command.mode.iterate -> `s` モードではなく `i` モードでストリーム処理に切り替える".to_string(),
            ],
            RegexCommandMode::Iterate => vec![
                "Quick Fix: regex.command.stream.remove-collect -> `as List` などの全件収集をやめて逐次処理する".to_string(),
            ],
            _ => vec![
                "Quick Fix: regex.command.stream.limit -> `limit` やフィルタで処理量を抑制する".to_string(),
            ],
        },
        "JV_REGEX_I004" => vec![
            "Quick Fix: regex.command.stream.materialize -> `as List` を追加して `Stream<MatchResult>` を一度だけ評価する".to_string(),
        ],
        "JV_REGEX_I005" => vec![
            "Quick Fix: regex.command.stream.cache -> `.as List` を付与して結果を使い回せるリストへ変換する".to_string(),
        ],
        _ => Vec::new(),
    }
}

fn mode_completion_items() -> Vec<String> {
    MODE_COMPLETIONS
        .iter()
        .map(|completion| format!("regex mode: {} · {}", completion.key, completion.summary))
        .collect()
}

fn flag_completion_items(existing: &HashSet<char>) -> Vec<String> {
    FLAG_COMPLETIONS
        .iter()
        .filter(|candidate| !existing.contains(&candidate.flag))
        .map(|candidate| format!("regex flag: {} · {}", candidate.flag, candidate.summary))
        .collect()
}

struct CommandRegions {
    mode_span: Option<Span>,
    flags_span: Option<Span>,
}

fn compute_regions(command: &RegexCommand, tokens: &[Token]) -> CommandRegions {
    let Some(start_index) = find_token_at_span(tokens, &command.span) else {
        return CommandRegions {
            mode_span: None,
            flags_span: None,
        };
    };

    let segments = detect_segments(tokens, start_index);
    let mode_span = segments
        .mode_range
        .and_then(|(start, end)| span_across(tokens, start, end));
    let flags_span = segments
        .flags_index
        .map(|idx| span_from_token(&tokens[idx]));

    CommandRegions {
        mode_span,
        flags_span,
    }
}

struct SegmentsInfo {
    mode_range: Option<(usize, usize)>,
    flags_index: Option<usize>,
}

fn detect_segments(tokens: &[Token], start: usize) -> SegmentsInfo {
    let len = tokens.len();
    if start >= len {
        return SegmentsInfo {
            mode_range: None,
            flags_index: None,
        };
    }

    let mut idx = start;
    let mut mode_range = None;

    if parse_short_mode_token(&tokens[idx]).is_some() {
        mode_range = Some((idx, idx + 1));
        idx += 1;
    } else if let Some(end_idx) = parse_explicit_mode(tokens, idx) {
        mode_range = Some((idx, end_idx));
        idx = end_idx;
    }

    if idx >= len || !matches!(tokens[idx].token_type, TokenType::Divide) {
        return SegmentsInfo {
            mode_range,
            flags_index: None,
        };
    }

    let prefix_span = if let Some(range) = mode_range {
        span_across(tokens, range.0, range.1)
    } else {
        Some(span_from_token(&tokens[idx]))
    };

    if mode_range.is_none() {
        mode_range = prefix_span.map(|_| (idx, idx + 1));
    }

    idx += 1;
    if idx >= len {
        return SegmentsInfo {
            mode_range,
            flags_index: None,
        };
    }

    while idx < len && !matches!(tokens[idx].token_type, TokenType::RegexLiteral(_)) {
        idx += 1;
    }

    if idx >= len {
        return SegmentsInfo {
            mode_range,
            flags_index: None,
        };
    }

    idx += 1;

    if idx >= len {
        return SegmentsInfo {
            mode_range,
            flags_index: None,
        };
    }

    if matches!(tokens[idx].token_type, TokenType::Divide) {
        idx += 1;
        while idx < len && !matches!(tokens[idx].token_type, TokenType::Divide) {
            idx += 1;
        }
        if idx < len && matches!(tokens[idx].token_type, TokenType::Divide) {
            idx += 1;
            if idx < len && extract_flag_string(&tokens[idx]).is_some() {
                return SegmentsInfo {
                    mode_range,
                    flags_index: Some(idx),
                };
            }
        }
    } else {
        while idx < len && !matches!(tokens[idx].token_type, TokenType::Divide) {
            idx += 1;
        }
        if idx < len && matches!(tokens[idx].token_type, TokenType::Divide) {
            idx += 1;
            if idx < len && extract_flag_string(&tokens[idx]).is_some() {
                return SegmentsInfo {
                    mode_range,
                    flags_index: Some(idx),
                };
            }
        }
    }

    SegmentsInfo {
        mode_range,
        flags_index: None,
    }
}

fn parse_short_mode_token(token: &Token) -> Option<RegexCommandMode> {
    if let TokenType::Identifier(value) = &token.token_type {
        if value.len() == 1 {
            return match value.as_bytes()[0] {
                b'a' | b'A' => Some(RegexCommandMode::All),
                b'f' | b'F' => Some(RegexCommandMode::First),
                b'm' | b'M' => Some(RegexCommandMode::Match),
                b's' | b'S' => Some(RegexCommandMode::Split),
                b'i' | b'I' => Some(RegexCommandMode::Iterate),
                _ => None,
            };
        }
    }
    None
}

fn parse_explicit_mode(tokens: &[Token], start: usize) -> Option<usize> {
    let len = tokens.len();
    if start >= len || !matches!(tokens[start].token_type, TokenType::LeftBracket) {
        return None;
    }

    let mut idx = start + 1;
    while idx < len && !matches!(tokens[idx].token_type, TokenType::RightBracket) {
        idx += 1;
    }

    if idx >= len {
        return None;
    }

    Some(idx + 1)
}

fn extract_flag_string(token: &Token) -> Option<String> {
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

fn span_across(tokens: &[Token], start: usize, end: usize) -> Option<Span> {
    if start >= end || end == 0 || end > tokens.len() {
        return None;
    }
    let first = span_from_token(&tokens[start]);
    let last = span_from_token(&tokens[end - 1]);
    Some(first.merge(&last))
}

fn span_from_token(token: &Token) -> Span {
    Span::from_token_lexeme(token.line, token.column, &token.lexeme)
}

fn find_token_at_span(tokens: &[Token], span: &Span) -> Option<usize> {
    tokens
        .iter()
        .position(|token| token.line == span.start_line && token.column == span.start_column)
}

fn position_in_span(position: &Position, span: &Span) -> bool {
    let line = position.line as usize + 1;
    let column = position.character as usize + 1;

    if line < span.start_line || line > span.end_line {
        return false;
    }

    if span.start_line == span.end_line {
        return column >= span.start_column && column < span.end_column;
    }

    if line == span.start_line {
        return column >= span.start_column;
    }

    if line == span.end_line {
        return column < span.end_column;
    }

    true
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct SpanKey {
    start_line: usize,
    start_column: usize,
    end_line: usize,
    end_column: usize,
}

impl SpanKey {
    fn from_span(span: &Span) -> Self {
        Self {
            start_line: span.start_line,
            start_column: span.start_column,
            end_line: span.end_line,
            end_column: span.end_column,
        }
    }
}

struct ModeCompletion {
    key: &'static str,
    summary: &'static str,
}

const MODE_COMPLETIONS: &[ModeCompletion] = &[
    ModeCompletion {
        key: "a",
        summary: "全件置換（replaceAll）",
    },
    ModeCompletion {
        key: "f",
        summary: "先頭のみ置換（replaceFirst）",
    },
    ModeCompletion {
        key: "m",
        summary: "一致判定（matches）",
    },
    ModeCompletion {
        key: "s",
        summary: "区切り配列 `String[]` を返却（Pattern.split）",
    },
    ModeCompletion {
        key: "i",
        summary: "一致結果を `Stream<MatchResult>` で逐次処理",
    },
];

struct FlagCompletion {
    flag: char,
    summary: &'static str,
}

const FLAG_COMPLETIONS: &[FlagCompletion] = &[
    FlagCompletion {
        flag: 'i',
        summary: "大文字小文字を無視（CASE_INSENSITIVE）",
    },
    FlagCompletion {
        flag: 'm',
        summary: "複数行モード（MULTILINE）",
    },
    FlagCompletion {
        flag: 's',
        summary: "`.` で改行も一致（DOTALL）",
    },
    FlagCompletion {
        flag: 'u',
        summary: "Unicode 大文字小文字対応（UNICODE_CASE）",
    },
    FlagCompletion {
        flag: 'd',
        summary: "Unix 行モード（UNIX_LINES）",
    },
    FlagCompletion {
        flag: 'x',
        summary: "コメント・空白を無視（COMMENTS）",
    },
    FlagCompletion {
        flag: 'l',
        summary: "リテラルモードでメタ文字を無効化（LITERAL）",
    },
    FlagCompletion {
        flag: 'c',
        summary: "正規化等価マッチ（CANON_EQ）",
    },
];
