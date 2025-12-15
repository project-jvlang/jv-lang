use super::context::{LoweringContext, LoweringDiagnostic};
use super::types::lower_type_annotation;
use crate::dsl::log::lower_log_block;
use crate::lexer::Span as TokenSpan;
use crate::lexer::TokenKind;
use crate::parser::OwnedToken;
use jv_ast::expression::{Argument, CallArgumentMetadata, Expression, StringPart};
use jv_ast::expression::{
    CallArgumentStyle, Parameter, ParameterModifiers, SequenceDelimiter, WhenArm,
};
use jv_ast::json::{JsonEntry, JsonLiteral, JsonValue, NumberGrouping};
use jv_ast::types::{BinaryOp, Literal, RegexLiteral, TypeAnnotation, UnaryOp};
use jv_lexer::{JsonConfidence, LayoutMode, Lexer, StringInterpolationSegment, TokenMetadata};

/// トークンスライスを前提にした簡易 Pratt パーサ。
pub fn lower_expression(
    ctx: &mut LoweringContext<'_>,
    tokens: &[OwnedToken],
) -> Option<Expression> {
    lower_expression_with_depth(ctx, tokens, 0)
}

fn lower_expression_with_depth(
    ctx: &mut LoweringContext<'_>,
    tokens: &[OwnedToken],
    depth: usize,
) -> Option<Expression> {
    let mut cursor = Cursor::new(tokens);
    parse_expression_bp(ctx, &mut cursor, 0, depth)
}

/// 指定した区切りトークンまでのスライスを返す（最初の出現位置基準）。
pub fn slice_until<F>(
    tokens: &[OwnedToken],
    delimiter: TokenKind,
    kind_fn: F,
) -> Option<(&[OwnedToken], &[OwnedToken])>
where
    F: Fn(&OwnedToken) -> TokenKind,
{
    let idx = tokens.iter().position(|t| kind_fn(t) == delimiter)?;
    Some(tokens.split_at(idx + 1))
}

struct Cursor<'a> {
    tokens: &'a [OwnedToken],
    pos: usize,
}

impl<'a> Cursor<'a> {
    fn new(tokens: &'a [OwnedToken]) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&'a OwnedToken> {
        self.tokens.get(self.pos)
    }

    fn peek_at(&self, n: usize) -> Option<&'a OwnedToken> {
        self.tokens.get(self.pos + n)
    }

    fn peek_kind(&self) -> Option<TokenKind> {
        self.peek().map(|t| t.kind)
    }

    fn bump(&mut self) -> Option<&'a OwnedToken> {
        let tok = self.tokens.get(self.pos);
        if tok.is_some() {
            self.pos += 1;
        }
        tok
    }

    fn remaining(&self) -> &'a [OwnedToken] {
        if self.pos >= self.tokens.len() {
            &[]
        } else {
            &self.tokens[self.pos..]
        }
    }

    fn skip_to_end(&mut self) {
        self.pos = self.tokens.len();
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn bump_while<F: Fn(TokenKind) -> bool>(&mut self, pred: F) {
        while let Some(kind) = self.peek_kind() {
            if !pred(kind) {
                break;
            }
            self.bump();
        }
    }
}

fn parse_expression_bp(
    ctx: &mut LoweringContext<'_>,
    cursor: &mut Cursor<'_>,
    min_bp: u8,
    depth: usize,
) -> Option<Expression> {
    cursor.bump_while(|kind| {
        matches!(
            kind,
            TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::LayoutComma
                | TokenKind::FieldNameLabel
                | TokenKind::LineComment
                | TokenKind::BlockComment
                | TokenKind::JavaDocComment
        )
    });
    let mut lhs = parse_prefix(ctx, cursor, depth)?;

    loop {
        cursor.bump_while(|kind| {
            matches!(
                kind,
                TokenKind::Whitespace
                    | TokenKind::Newline
                    | TokenKind::LayoutComma
                    | TokenKind::FieldNameLabel
                    | TokenKind::LineComment
                    | TokenKind::BlockComment
                    | TokenKind::JavaDocComment
            )
        });
        let Some(op_tok) = cursor.peek().cloned() else {
            break;
        };

        if op_tok.kind == TokenKind::Identifier && op_tok.lexeme_eq("as") {
            let (l_bp, _r_bp) = (4u8, 5u8);
            if l_bp < min_bp {
                break;
            }
            cursor.bump(); // consume `as`
            cursor.bump_while(|kind| {
                matches!(
                    kind,
                    TokenKind::Whitespace
                        | TokenKind::Newline
                        | TokenKind::LayoutComma
                        | TokenKind::FieldNameLabel
                        | TokenKind::LineComment
                        | TokenKind::BlockComment
                        | TokenKind::JavaDocComment
                )
            });
            let (ty_tokens, consumed) = take_type_tokens(cursor.remaining());
            let target = lower_type_annotation(ctx, ty_tokens)
                .unwrap_or_else(|| TypeAnnotation::Simple(String::new()));
            cursor.pos = cursor.pos.saturating_add(consumed);
            let span = if let Some(last) = ty_tokens.last() {
                lhs.span().merge(&ctx.span_for_token(last))
            } else {
                lhs.span().clone()
            };
            lhs = Expression::TypeCast {
                expr: Box::new(lhs),
                target,
                span,
            };
            continue;
        }
        let (l_bp, r_bp, op) = match infix_binding_power(&op_tok) {
            Some(info) => info,
            None => break,
        };

        if l_bp < min_bp {
            break;
        }

        cursor.bump(); // operator
        let rhs = match parse_expression_bp(ctx, cursor, r_bp, depth) {
            Some(expr) => expr,
            None => {
                ctx.push_diagnostic(LoweringDiagnostic::error(
                    "二項演算子の右辺を解釈できませんでした",
                    Some(ctx.span_for_token(&op_tok)),
                ));
                break;
            }
        };
        let span = {
            let left_span = lhs.span().clone();
            let right_span = rhs.span().clone();
            left_span.merge(&right_span)
        };
        lhs = Expression::Binary {
            left: Box::new(lhs),
            op,
            right: Box::new(rhs),
            span,
            metadata: Default::default(),
        };
    }

    Some(lhs)
}

fn parse_prefix(
    ctx: &mut LoweringContext<'_>,
    cursor: &mut Cursor<'_>,
    depth: usize,
) -> Option<Expression> {
    let tok = cursor.bump()?;
    match tok.kind {
        TokenKind::When => Some(parse_when_expression(ctx, cursor, tok, depth)),
        TokenKind::Log
        | TokenKind::Trace
        | TokenKind::Debug
        | TokenKind::Info
        | TokenKind::Warn
        | TokenKind::Error => {
            let mut collected = vec![tok.clone()];
            collected.extend_from_slice(cursor.remaining());
            let expr = lower_log_block(ctx, &collected, depth);
            cursor.skip_to_end();
            expr
        }
        TokenKind::Number => Some(Expression::Literal(
            Literal::Number(tok.lexeme_string()),
            ctx.span_for_token(tok),
        )),
        TokenKind::StringStart => Some(parse_string_interpolation(ctx, cursor, tok, depth)),
        TokenKind::String => {
            if tok.metadata.iter().any(|meta| matches!(meta, TokenMetadata::StringInterpolation { .. }))
            {
                Some(lower_string_interpolation(ctx, tok, depth))
            } else {
                Some(Expression::Literal(
                    Literal::String(tok.lexeme_string()),
                    ctx.span_for_token(tok),
                ))
            }
        }
        TokenKind::StringInterpolation => Some(lower_string_interpolation(ctx, tok, depth)),
        TokenKind::RegexLiteral => {
            let span = ctx.span_for_token(tok);
            let (raw, pattern) = tok
                .metadata
                .iter()
                .find_map(|meta| match meta {
                    TokenMetadata::RegexLiteral { raw, pattern } => {
                        Some((raw.clone(), pattern.clone()))
                    }
                    _ => None,
                })
                .unwrap_or_else(|| {
                    let raw = tok.lexeme_string();
                    (raw.clone(), raw)
                });
            Some(Expression::RegexLiteral(RegexLiteral {
                pattern,
                raw,
                span,
                origin: None,
                const_key: None,
                template_segments: Vec::new(),
            }))
        }
        TokenKind::Character => {
            let ch = tok.lexeme.chars().next().unwrap_or_default();
            Some(Expression::Literal(
                Literal::Character(ch),
                ctx.span_for_token(tok),
            ))
        }
        TokenKind::BooleanTrue => Some(Expression::Literal(
            Literal::Boolean(true),
            ctx.span_for_token(tok),
        )),
        TokenKind::BooleanFalse => Some(Expression::Literal(
            Literal::Boolean(false),
            ctx.span_for_token(tok),
        )),
        TokenKind::Null => Some(Expression::Literal(Literal::Null, ctx.span_for_token(tok))),
        TokenKind::Identifier => Some(Expression::Identifier(
            tok.lexeme_string(),
            ctx.span_for_token(tok),
        )),
        TokenKind::ImplicitParam => Some(Expression::Identifier(
            tok.lexeme_string(),
            ctx.span_for_token(tok),
        )),
        TokenKind::Underscore => Some(Expression::Identifier(
            "_".to_string(),
            ctx.span_for_token(tok),
        )),
        TokenKind::Minus | TokenKind::Plus | TokenKind::Not => {
            let op = match tok.kind {
                TokenKind::Minus => UnaryOp::Minus,
                TokenKind::Plus => UnaryOp::Plus,
                TokenKind::Not => UnaryOp::Not,
                _ => UnaryOp::Plus,
            };
            let operand = parse_expression_bp(ctx, cursor, 9, depth)
                .unwrap_or_else(|| Expression::Literal(Literal::Null, ctx.span_for_token(tok)));
            let span = ctx.span_for_token(tok).merge(operand.span());
            Some(Expression::Unary {
                op,
                operand: Box::new(operand),
                span,
            })
        }
        TokenKind::Question => {
            ctx.push_diagnostic(LoweringDiagnostic::error(
                "JV3103: unexpected `?` unary form / `?` 単項演算子はサポートされていません。",
                Some(ctx.span_for_token(tok)),
            ));
            Some(Expression::Literal(Literal::Null, ctx.span_for_token(tok)))
        }
        TokenKind::If => {
            let message = "JV3103: `if` expressions are not supported / `if` 式はサポートされていません。\n条件分岐は `when` 式を使用してください。Quick Fix: when.convert.if. / Use a `when` expression for branching. Quick Fix: when.convert.if. (--explain JV3103)";
            let span = ctx.span_for_token(tok);
            ctx.push_diagnostic(LoweringDiagnostic::error(message, Some(span.clone())));
            consume_expression_like_region(cursor);
            Some(Expression::Literal(Literal::Null, span))
        }
        TokenKind::LeftParen => {
            let open_span = ctx.span_for_token(tok);
            let mut elements = Vec::new();
            let mut fields = Vec::new();
            let mut pending_labels: Vec<(TokenKind, String, jv_ast::Span)> = Vec::new();

            loop {
                loop {
                    cursor.bump_while(|kind| {
                        matches!(
                            kind,
                            TokenKind::Whitespace
                                | TokenKind::Newline
                                | TokenKind::LayoutComma
                                | TokenKind::FieldNameLabel
                        )
                    });
                    let Some(next) = cursor.peek() else {
                        break;
                    };
                    match next.kind {
                        TokenKind::LineComment
                        | TokenKind::BlockComment
                        | TokenKind::JavaDocComment => {
                            let span = ctx.span_for_token(next);
                            pending_labels.push((next.kind, next.lexeme_string(), span));
                            cursor.bump();
                        }
                        TokenKind::FieldNameLabel => {
                            let span = ctx.span_for_token(next);
                            pending_labels.push((next.kind, next.lexeme_string(), span));
                            cursor.bump();
                        }
                        _ => break,
                    }
                }
                if cursor.is_eof() || cursor.peek_kind() == Some(TokenKind::RightParen) {
                    break;
                }

                let start_tok = cursor.peek().cloned();
                let expr = parse_expression_bp(ctx, cursor, 0, depth)?;
                let mut trailing_labels = collect_trailing_tuple_labels(ctx, cursor);
                trailing_labels.extend(collect_carried_tuple_labels(ctx, cursor, expr.span()));
                if let Some(token) = start_tok {
                    let span = expr.span().clone();
                    let mut meta =
                        jv_ast::expression::TupleFieldMeta::empty(fields.len() + 1, span.clone());
                    if let Expression::Identifier(name, _) = &expr {
                        meta.identifier_hint = Some(name.clone());
                    }
                    for (kind, raw, comment_span) in pending_labels.drain(..) {
                        let cleaned = clean_comment_text(kind, &raw);
                        if cleaned.is_empty() {
                            continue;
                        }
                        match kind {
                            TokenKind::LineComment => {
                                meta.primary_label.get_or_insert(cleaned);
                            }
                            TokenKind::FieldNameLabel => {
                                meta.primary_label.get_or_insert(cleaned);
                            }
                            TokenKind::BlockComment | TokenKind::JavaDocComment => {
                                meta.secondary_labels.push(jv_ast::expression::LabeledSpan {
                                    name: cleaned,
                                    span: comment_span,
                                });
                            }
                            _ => {}
                        }
                    }
                    for (kind, raw, comment_span) in trailing_labels {
                        let cleaned = clean_comment_text(kind, &raw);
                        if cleaned.is_empty() {
                            continue;
                        }
                        match kind {
                            TokenKind::LineComment => {
                                meta.primary_label.get_or_insert(cleaned);
                            }
                            TokenKind::FieldNameLabel => {
                                meta.primary_label.get_or_insert(cleaned);
                            }
                            TokenKind::BlockComment | TokenKind::JavaDocComment => {
                                meta.secondary_labels.push(jv_ast::expression::LabeledSpan {
                                    name: cleaned,
                                    span: comment_span,
                                });
                            }
                            _ => {}
                        }
                    }
                    for comment in token
                        .leading_trivia
                        .passthrough_comments
                        .iter()
                        .chain(token.leading_trivia.jv_comments.iter())
                    {
                        let kind = match comment.kind {
                            jv_lexer::SourceCommentKind::Line => TokenKind::LineComment,
                            jv_lexer::SourceCommentKind::Block => TokenKind::BlockComment,
                        };
                        let cleaned = clean_comment_text(kind, &comment.text);
                        if cleaned.is_empty() {
                            continue;
                        }
                        let span = jv_ast::Span::new(
                            comment.line,
                            comment.column,
                            comment.line,
                            comment.column.saturating_add(cleaned.len()),
                        );
                        match kind {
                            TokenKind::LineComment => {
                                meta.primary_label.get_or_insert(cleaned);
                            }
                            TokenKind::BlockComment => {
                                if !meta
                                    .secondary_labels
                                    .iter()
                                    .any(|label| label.name == cleaned)
                                {
                                    meta.secondary_labels.push(jv_ast::expression::LabeledSpan {
                                        name: cleaned,
                                        span,
                                    });
                                }
                            }
                            _ => {}
                        }
                    }
                    fields.push(meta);
                }
                elements.push(expr);
            }

            let close_span = if cursor.peek_kind() == Some(TokenKind::RightParen) {
                let close = cursor.bump().map(|t| ctx.span_for_token(t));
                close.unwrap_or_else(|| open_span.clone())
            } else {
                open_span.clone()
            };
            let span = open_span.merge(&close_span);

            if elements.is_empty() {
                ctx.push_diagnostic(LoweringDiagnostic::error(
                    "空のタプルや句読点のみのタプルリテラルはサポートされません",
                    Some(span.clone()),
                ));
                Some(Expression::Tuple {
                    elements,
                    fields,
                    context: Default::default(),
                    span,
                })
            } else if elements.len() == 1 {
                elements.into_iter().next()
            } else {
                Some(Expression::Tuple {
                    elements,
                    fields,
                    context: Default::default(),
                    span,
                })
            }
        }
        TokenKind::LeftBracket => {
            let open_span = ctx.span_for_token(tok);
            if should_parse_json_array_literal(cursor.remaining()) {
                return Some(Expression::JsonLiteral(parse_json_array_literal(
                    ctx, cursor, open_span,
                )));
            }
            let mut elements = Vec::new();
            let mut used_commas = false;
            let mut emitted_comma_error = false;

            loop {
                cursor.bump_while(|kind| {
                    matches!(
                        kind,
                        TokenKind::Whitespace
                            | TokenKind::Newline
                            | TokenKind::LayoutComma
                            | TokenKind::FieldNameLabel
                            | TokenKind::LineComment
                            | TokenKind::BlockComment
                            | TokenKind::JavaDocComment
                    )
                });
                if cursor.is_eof() || cursor.peek_kind() == Some(TokenKind::RightBracket) {
                    break;
                }

                let expr = parse_expression_bp(ctx, cursor, 0, depth)?;
                elements.push(expr);

                cursor.bump_while(|kind| {
                    matches!(
                        kind,
                        TokenKind::Whitespace
                            | TokenKind::Newline
                            | TokenKind::LayoutComma
                            | TokenKind::FieldNameLabel
                            | TokenKind::LineComment
                            | TokenKind::BlockComment
                            | TokenKind::JavaDocComment
                    )
                });
                if cursor.peek_kind() == Some(TokenKind::Comma) {
                    if !emitted_comma_error {
                        if let Some(comma) = cursor.peek() {
                            ctx.push_diagnostic(LoweringDiagnostic::error(
                                "JV2101: 配列リテラルでカンマ区切りはサポートされません。要素は空白または改行で区切ってください。",
                                Some(ctx.span_for_token(comma)),
                            ));
                        }
                        emitted_comma_error = true;
                    }
                    used_commas = true;
                    cursor.bump();
                }
            }

            let close_span = if cursor.peek_kind() == Some(TokenKind::RightBracket) {
                let close = cursor.bump().map(|t| ctx.span_for_token(t));
                close.unwrap_or_else(|| open_span.clone())
            } else {
                open_span.clone()
            };
            let span = open_span.merge(&close_span);
            let delimiter = if used_commas {
                SequenceDelimiter::Comma
            } else {
                SequenceDelimiter::Whitespace
            };
            Some(Expression::Array {
                elements,
                delimiter,
                span,
            })
        }
        TokenKind::LeftBrace => Some(parse_brace_expression(ctx, cursor, tok, depth)),
        _ => {
            ctx.push_diagnostic(LoweringDiagnostic::error(
                "式を解釈できませんでした",
                Some(ctx.span_for_token(tok)),
            ));
            None
        }
    }
    .map(|expr| parse_postfix(ctx, cursor, expr, depth))
}

fn parse_string_interpolation(
    ctx: &mut LoweringContext<'_>,
    cursor: &mut Cursor<'_>,
    start: &OwnedToken,
    depth: usize,
) -> Expression {
    let open_span = ctx.span_for_token(start);
    let mut last_span = open_span.clone();
    let mut parts = Vec::new();

    let first = start.lexeme_string();
    if !first.is_empty() {
        parts.push(StringPart::Text(first));
    }

    loop {
        cursor.bump_while(|kind| matches!(kind, TokenKind::Whitespace | TokenKind::FieldNameLabel));

        if cursor.is_eof() {
            break;
        }

        if !matches!(
            cursor.peek_kind(),
            Some(TokenKind::StringMid | TokenKind::StringEnd)
        ) {
            if let Some(expr) = parse_expression_bp(ctx, cursor, 0, depth) {
                last_span = expr.span().clone();
                parts.push(StringPart::Expression(expr));
            } else {
                break;
            }
        }

        match cursor.peek_kind() {
            Some(TokenKind::StringMid) => {
                let mid = cursor.bump().expect("peeked StringMid");
                last_span = ctx.span_for_token(mid);
                let literal = mid.lexeme_string();
                if !literal.is_empty() {
                    parts.push(StringPart::Text(literal));
                }
            }
            Some(TokenKind::StringEnd) => {
                let end = cursor.bump().expect("peeked StringEnd");
                last_span = ctx.span_for_token(end);
                let literal = end.lexeme_string();
                if !literal.is_empty() {
                    parts.push(StringPart::Text(literal));
                }
                break;
            }
            _ => break,
        }
    }

    let span = open_span.merge(&last_span);
    Expression::StringInterpolation { parts, span }
}

fn consume_expression_like_region(cursor: &mut Cursor<'_>) {
    let mut brace_depth: isize = 0;
    let mut paren_depth: isize = 0;
    let mut bracket_depth: isize = 0;
    let mut consumed_any = false;
    while let Some(tok) = cursor.peek() {
        if consumed_any
            && tok.leading_trivia.newlines > 0
            && brace_depth == 0
            && paren_depth == 0
            && bracket_depth == 0
        {
            break;
        }
        match tok.kind {
            TokenKind::LeftBrace => brace_depth += 1,
            TokenKind::RightBrace => {
                if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 {
                    break;
                }
                brace_depth -= 1;
            }
            TokenKind::LeftParen => paren_depth += 1,
            TokenKind::RightParen => paren_depth -= 1,
            TokenKind::LeftBracket => bracket_depth += 1,
            TokenKind::RightBracket => bracket_depth -= 1,
            TokenKind::Semicolon | TokenKind::LayoutComma | TokenKind::Newline
                if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 =>
            {
                break;
            }
            TokenKind::Eof => break,
            _ => {}
        }
        cursor.bump();
        consumed_any = true;
    }
}

fn parse_postfix(
    ctx: &mut LoweringContext<'_>,
    cursor: &mut Cursor<'_>,
    mut expr: Expression,
    depth: usize,
) -> Expression {
    let mut pending_type_arguments: Vec<TypeAnnotation> = Vec::new();
    loop {
        cursor.bump_while(|kind| {
            matches!(
                kind,
                TokenKind::Whitespace
                    | TokenKind::Newline
                    | TokenKind::LayoutComma
                    | TokenKind::FieldNameLabel
                    | TokenKind::LineComment
                    | TokenKind::BlockComment
                    | TokenKind::JavaDocComment
            )
        });
        match cursor.peek_kind() {
            Some(TokenKind::Less) => {
                if should_parse_type_args(cursor) {
                    pending_type_arguments = consume_type_arguments(ctx, cursor);
                    continue;
                }
                break;
            }
            Some(TokenKind::LeftParen) => {
                cursor.bump();
                let mut args = Vec::new();
                let mut used_commas = false;
                let mut emitted_comma_error = false;
                loop {
                    cursor.bump_while(|kind| {
                        matches!(
                            kind,
                            TokenKind::Whitespace
                                | TokenKind::Newline
                                | TokenKind::LayoutComma
                                | TokenKind::FieldNameLabel
                                | TokenKind::LineComment
                                | TokenKind::BlockComment
                                | TokenKind::JavaDocComment
                        )
                    });
                    if cursor.is_eof() || cursor.peek_kind() == Some(TokenKind::RightParen) {
                        break;
                    }
                    if let Some(arg) = parse_expression_bp(ctx, cursor, 0, depth) {
                        args.push(Argument::Positional(arg));
                    }
                    cursor.bump_while(|kind| {
                        matches!(
                            kind,
                            TokenKind::Whitespace
                                | TokenKind::Newline
                                | TokenKind::LayoutComma
                                | TokenKind::FieldNameLabel
                                | TokenKind::LineComment
                                | TokenKind::BlockComment
                                | TokenKind::JavaDocComment
                        )
                    });
                    if cursor.peek_kind() == Some(TokenKind::Comma) {
                        if !emitted_comma_error {
                            if let Some(comma) = cursor.peek() {
                                ctx.push_diagnostic(LoweringDiagnostic::error(
                                    "JV2102: 関数呼び出しでカンマ区切りはサポートされません。位置引数は空白または改行で区切ってください。",
                                    Some(ctx.span_for_token(comma)),
                                ));
                            }
                            emitted_comma_error = true;
                        }
                        cursor.bump();
                        used_commas = true;
                    }
                }
                if cursor.peek_kind() == Some(TokenKind::RightParen) {
                    cursor.bump();
                }
                let span = expr.span().clone();
                let style = if used_commas {
                    CallArgumentStyle::Comma
                } else if args.len() >= 2 {
                    CallArgumentStyle::Whitespace
                } else {
                    CallArgumentStyle::Comma
                };
                expr = Expression::Call {
                    function: Box::new(expr),
                    args,
                    type_arguments: std::mem::take(&mut pending_type_arguments),
                    argument_metadata: CallArgumentMetadata {
                        style,
                        used_commas,
                        ..Default::default()
                    },
                    span,
                };
            }
            Some(TokenKind::Dot) => {
                cursor.bump();
                let property = match cursor.peek_kind() {
                    Some(TokenKind::Identifier | TokenKind::ImplicitParam) => cursor
                        .bump()
                        .map(|tok| tok.lexeme_string())
                        .unwrap_or_default(),
                    Some(TokenKind::Underscore) => {
                        let underscore = cursor.bump().expect("peeked underscore");
                        if cursor.peek_kind() == Some(TokenKind::Number) {
                            let number = cursor.bump().expect("peeked number");
                            format!("_{}", number.lexeme_string())
                        } else {
                            ctx.push_diagnostic(LoweringDiagnostic::error(
                                "メンバ名が必要です",
                                Some(ctx.span_for_token(underscore)),
                            ));
                            break;
                        }
                    }
                    _ => break,
                };
                let span = expr.span().clone();
                expr = Expression::MemberAccess {
                    object: Box::new(expr),
                    property,
                    span,
                };
            }
            Some(TokenKind::NullSafe) => {
                cursor.bump();
                let property = match cursor.peek_kind() {
                    Some(TokenKind::Identifier | TokenKind::ImplicitParam) => cursor
                        .bump()
                        .map(|tok| tok.lexeme_string())
                        .unwrap_or_default(),
                    Some(TokenKind::Underscore) => {
                        let underscore = cursor.bump().expect("peeked underscore");
                        if cursor.peek_kind() == Some(TokenKind::Number) {
                            let number = cursor.bump().expect("peeked number");
                            format!("_{}", number.lexeme_string())
                        } else {
                            ctx.push_diagnostic(LoweringDiagnostic::error(
                                "null 安全アクセスのターゲットが必要です",
                                Some(ctx.span_for_token(underscore)),
                            ));
                            break;
                        }
                    }
                    _ => break,
                };
                let span = expr.span().clone();
                expr = Expression::NullSafeMemberAccess {
                    object: Box::new(expr),
                    property,
                    span,
                };
            }
            Some(TokenKind::LeftBracket) => {
                cursor.bump();
                let index = parse_expression_bp(ctx, cursor, 0, depth)
                    .unwrap_or_else(|| Expression::Literal(Literal::Null, expr.span().clone()));
                if cursor.peek_kind() == Some(TokenKind::RightBracket) {
                    cursor.bump();
                }
                let span = expr.span().merge(index.span());
                expr = Expression::IndexAccess {
                    object: Box::new(expr),
                    index: Box::new(index),
                    span,
                };
            }
            Some(TokenKind::LeftBrace) => {
                let open = cursor.bump().expect("peeked LeftBrace");
                let lambda = parse_brace_expression(ctx, cursor, open, depth);
                let span = expr.span().merge(lambda.span());

                expr = match expr {
                    Expression::Call {
                        function,
                        mut args,
                        type_arguments,
                        mut argument_metadata,
                        ..
                    } => {
                        args.push(Argument::Positional(lambda));
                        argument_metadata.style = CallArgumentStyle::Whitespace;
                        Expression::Call {
                            function,
                            args,
                            type_arguments,
                            argument_metadata,
                            span,
                        }
                    }
                    other => Expression::Call {
                        function: Box::new(other),
                        args: vec![Argument::Positional(lambda)],
                        type_arguments: Vec::new(),
                        argument_metadata: CallArgumentMetadata::with_style(
                            CallArgumentStyle::Whitespace,
                        ),
                        span,
                    },
                };
            }
            _ => break,
        }
    }
    expr
}

fn should_parse_type_args(cursor: &Cursor<'_>) -> bool {
    let Some(close_idx) = find_matching_angle(cursor.remaining()) else {
        return false;
    };
    if close_idx == 0 {
        return false;
    }
    matches!(
        cursor.peek_at(close_idx + 1).map(|tok| tok.kind),
        Some(
            TokenKind::LeftParen
                | TokenKind::Dot
                | TokenKind::LeftBracket
                | TokenKind::LeftBrace
                | TokenKind::Comma
                | TokenKind::RightParen
        )
    )
}

fn find_matching_angle(tokens: &[OwnedToken]) -> Option<usize> {
    let mut depth: isize = 0;
    let mut i = 0usize;
    while let Some(tok) = tokens.get(i) {
        match tok.kind {
            TokenKind::Less => depth += 1,
            TokenKind::Greater => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                } else if depth < 0 {
                    return None;
                }
            }
            TokenKind::Eof | TokenKind::Semicolon => return None,
            _ => {}
        }
        i += 1;
    }
    None
}

fn consume_type_arguments(
    ctx: &mut LoweringContext<'_>,
    cursor: &mut Cursor<'_>,
) -> Vec<TypeAnnotation> {
    let mut type_arguments = Vec::new();
    let Some(close_idx) = find_matching_angle(cursor.remaining()) else {
        return type_arguments;
    };
    let slice = cursor.remaining();
    let inner = slice.get(1..close_idx).unwrap_or(&[]);
    let mut start = 0usize;
    let mut angle: isize = 0;
    let mut paren: isize = 0;
    let mut bracket: isize = 0;
    for (idx, tok) in inner.iter().enumerate() {
        match tok.kind {
            TokenKind::Less => angle += 1,
            TokenKind::Greater => angle -= 1,
            TokenKind::LeftParen => paren += 1,
            TokenKind::RightParen => paren -= 1,
            TokenKind::LeftBracket => bracket += 1,
            TokenKind::RightBracket => bracket -= 1,
            TokenKind::Comma if angle == 0 && paren == 0 && bracket == 0 => {
                let part = trim_trivia(inner.get(start..idx).unwrap_or(&[]));
                if let Some(ty) = lower_type_annotation(ctx, part) {
                    type_arguments.push(ty);
                }
                start = idx + 1;
            }
            _ => {}
        }
    }
    let tail = trim_trivia(inner.get(start..).unwrap_or(&[]));
    if let Some(ty) = lower_type_annotation(ctx, tail) {
        type_arguments.push(ty);
    }
    cursor.pos = cursor.pos.saturating_add(close_idx + 1);
    type_arguments
}

fn parse_brace_expression(
    ctx: &mut LoweringContext<'_>,
    cursor: &mut Cursor<'_>,
    open: &OwnedToken,
    _depth: usize,
) -> Expression {
    let open_span = ctx.span_for_token(open);
    if is_potential_json_start(open) {
        return Expression::JsonLiteral(parse_json_object_literal(ctx, cursor, open_span));
    }
    let start = cursor.pos;
    let mut brace_depth: usize = 0;

    while let Some(tok) = cursor.peek() {
        match tok.kind {
            TokenKind::LeftBrace => {
                brace_depth += 1;
                cursor.bump();
            }
            TokenKind::RightBrace => {
                if brace_depth == 0 {
                    break;
                }
                brace_depth = brace_depth.saturating_sub(1);
                cursor.bump();
            }
            TokenKind::Eof => break,
            _ => {
                cursor.bump();
            }
        }
    }

    let end = cursor.pos;
    let close_span = if cursor.peek_kind() == Some(TokenKind::RightBrace) {
        cursor
            .bump()
            .map(|tok| ctx.span_for_token(tok))
            .unwrap_or_else(|| open_span.clone())
    } else {
        open_span.clone()
    };
    let span = open_span.merge(&close_span);

    let content = cursor.tokens.get(start..end).unwrap_or(&[]);
    let Some(arrow_idx) = find_top_level_arrow(content) else {
        // `value -> expr` 形式がない `{ ... }` は、呼び出し側の文脈でラムダとして扱われることが多い。
        // 旧パーサー互換のため、暗黙 1 引数ラムダとしてローワリングする。
        let body = lower_expression(ctx, content)
            .unwrap_or_else(|| Expression::Literal(Literal::Null, span.clone()));
        let implicit = Parameter {
            name: "_".to_string(),
            type_annotation: None,
            default_value: None,
            modifiers: ParameterModifiers::default(),
            span: open_span.clone(),
        };
        return Expression::Lambda {
            parameters: vec![implicit],
            body: Box::new(body),
            span,
        };
    };

    let params = parse_lambda_parameters(ctx, &content[..arrow_idx]);
    let body_tokens = content.get(arrow_idx + 1..).unwrap_or(&[]);
    let body = lower_lambda_body(ctx, body_tokens, &span);
    Expression::Lambda {
        parameters: params,
        body: Box::new(body),
        span,
    }
}

fn lower_lambda_body(
    ctx: &mut LoweringContext<'_>,
    tokens: &[OwnedToken],
    fallback_span: &jv_ast::Span,
) -> Expression {
    let tokens = trim_trivia(tokens);
    if let Some(assign_idx) = find_top_level_assign(tokens) {
        let (lhs, rhs_with_assign) = tokens.split_at(assign_idx);
        let rhs = rhs_with_assign.get(1..).unwrap_or(&[]);
        let target = lower_expression(ctx, lhs)
            .unwrap_or_else(|| Expression::Literal(Literal::Null, fallback_span.clone()));
        let value = lower_expression(ctx, rhs)
            .unwrap_or_else(|| Expression::Literal(Literal::Null, fallback_span.clone()));
        let span = target.span().merge(value.span());
        let statement = jv_ast::Statement::Assignment {
            target,
            binding_pattern: None,
            value,
            span: span.clone(),
        };
        Expression::Block {
            statements: vec![statement],
            span,
        }
    } else {
        lower_expression(ctx, tokens)
            .unwrap_or_else(|| Expression::Literal(Literal::Null, fallback_span.clone()))
    }
}

fn find_top_level_assign(tokens: &[OwnedToken]) -> Option<usize> {
    let mut brace_depth: isize = 0;
    let mut paren_depth: isize = 0;
    let mut bracket_depth: isize = 0;
    for (idx, tok) in tokens.iter().enumerate() {
        match tok.kind {
            TokenKind::LeftBrace => brace_depth += 1,
            TokenKind::RightBrace => brace_depth -= 1,
            TokenKind::LeftParen => paren_depth += 1,
            TokenKind::RightParen => paren_depth -= 1,
            TokenKind::LeftBracket => bracket_depth += 1,
            TokenKind::RightBracket => bracket_depth -= 1,
            TokenKind::Assign if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 => {
                return Some(idx);
            }
            _ => {}
        }
    }
    None
}

fn is_potential_json_start(token: &OwnedToken) -> bool {
    token.metadata.iter().any(|meta| match meta {
        TokenMetadata::PotentialJsonStart { confidence } => {
            !matches!(confidence, JsonConfidence::Low)
        }
        _ => false,
    })
}

fn should_parse_json_array_literal(tokens: &[OwnedToken]) -> bool {
    let mut brace_depth: isize = 0;
    let mut paren_depth: isize = 0;
    let mut bracket_depth: isize = 0;
    let mut saw_comma = false;
    let mut saw_string = false;

    for tok in tokens {
        match tok.kind {
            TokenKind::LeftBrace => brace_depth += 1,
            TokenKind::RightBrace => brace_depth -= 1,
            TokenKind::LeftParen => paren_depth += 1,
            TokenKind::RightParen => paren_depth -= 1,
            TokenKind::LeftBracket => bracket_depth += 1,
            TokenKind::RightBracket => {
                if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 {
                    break;
                }
                bracket_depth -= 1;
            }
            TokenKind::Comma if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 => {
                saw_comma = true;
            }
            TokenKind::String | TokenKind::StringInterpolation
                if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 =>
            {
                saw_string = true;
            }
            _ => {}
        }
    }

    saw_comma && saw_string
}

fn parse_json_object_literal(
    ctx: &mut LoweringContext<'_>,
    cursor: &mut Cursor<'_>,
    open_span: jv_ast::Span,
) -> JsonLiteral {
    let mut entries = Vec::new();

    loop {
        cursor.bump_while(is_json_trivia);
        if cursor.is_eof() || cursor.peek_kind() == Some(TokenKind::RightBrace) {
            break;
        }

        let Some(key_tok) = cursor.bump() else {
            break;
        };
        let key = match key_tok.kind {
            TokenKind::String | TokenKind::Identifier => key_tok.lexeme_string(),
            _ => {
                ctx.push_diagnostic(LoweringDiagnostic::error(
                    "JSON オブジェクトのキーは文字列または識別子である必要があります",
                    Some(ctx.span_for_token(key_tok)),
                ));
                key_tok.lexeme_string()
            }
        };
        cursor.bump_while(is_json_trivia);
        if cursor.peek_kind() == Some(TokenKind::Colon) {
            cursor.bump();
        } else {
            ctx.push_diagnostic(LoweringDiagnostic::error(
                "JSON オブジェクトのキーの後ろに `:` が必要です",
                Some(ctx.span_for_token(key_tok)),
            ));
        }
        cursor.bump_while(is_json_trivia);
        let value = parse_json_value(ctx, cursor).unwrap_or_else(|| JsonValue::Null {
            span: ctx.span_for_token(key_tok),
        });
        let entry_span = ctx.span_for_token(key_tok).merge(json_value_span(&value));
        entries.push(JsonEntry {
            key,
            comments: Vec::new(),
            value,
            span: entry_span,
        });
        cursor.bump_while(is_json_trivia);
        if cursor.peek_kind() == Some(TokenKind::Comma) {
            cursor.bump();
        }
    }

    let close_span = if cursor.peek_kind() == Some(TokenKind::RightBrace) {
        cursor
            .bump()
            .map(|tok| ctx.span_for_token(tok))
            .unwrap_or_else(|| open_span.clone())
    } else {
        open_span.clone()
    };
    let span = open_span.merge(&close_span);
    JsonLiteral {
        value: JsonValue::Object {
            entries,
            span: span.clone(),
        },
        leading_comments: Vec::new(),
        trailing_comments: Vec::new(),
        span,
        inferred_schema: None,
    }
}

fn parse_json_array_literal(
    ctx: &mut LoweringContext<'_>,
    cursor: &mut Cursor<'_>,
    open_span: jv_ast::Span,
) -> JsonLiteral {
    let mut elements = Vec::new();
    let mut saw_comma = false;

    loop {
        cursor.bump_while(is_json_trivia);
        if cursor.is_eof() || cursor.peek_kind() == Some(TokenKind::RightBracket) {
            break;
        }

        if let Some(value) = parse_json_value(ctx, cursor) {
            elements.push(value);
        } else if let Some(tok) = cursor.bump() {
            ctx.push_diagnostic(LoweringDiagnostic::error(
                "JSON 配列要素を解釈できませんでした",
                Some(ctx.span_for_token(tok)),
            ));
        } else {
            break;
        }

        cursor.bump_while(is_json_trivia);
        if cursor.peek_kind() == Some(TokenKind::Comma) {
            saw_comma = true;
            cursor.bump();
        }
    }

    let close_span = if cursor.peek_kind() == Some(TokenKind::RightBracket) {
        cursor
            .bump()
            .map(|tok| ctx.span_for_token(tok))
            .unwrap_or_else(|| open_span.clone())
    } else {
        open_span.clone()
    };
    let span = open_span.merge(&close_span);
    let delimiter = if saw_comma {
        SequenceDelimiter::Comma
    } else {
        SequenceDelimiter::Whitespace
    };
    JsonLiteral {
        value: JsonValue::Array {
            elements,
            delimiter,
            span: span.clone(),
        },
        leading_comments: Vec::new(),
        trailing_comments: Vec::new(),
        span,
        inferred_schema: None,
    }
}

fn parse_json_value(ctx: &mut LoweringContext<'_>, cursor: &mut Cursor<'_>) -> Option<JsonValue> {
    cursor.bump_while(is_json_trivia);
    let tok = cursor.bump()?;
    let span = ctx.span_for_token(tok);
    match tok.kind {
        TokenKind::LeftBrace => {
            let literal = parse_json_object_literal(ctx, cursor, span.clone());
            Some(literal.value)
        }
        TokenKind::LeftBracket => {
            let literal = parse_json_array_literal(ctx, cursor, span.clone());
            Some(literal.value)
        }
        TokenKind::String | TokenKind::StringInterpolation => Some(JsonValue::String {
            value: tok.lexeme_string(),
            span,
        }),
        TokenKind::Number => Some(JsonValue::Number {
            literal: tok.lexeme_string(),
            grouping: number_grouping_from_metadata(tok),
            span,
        }),
        TokenKind::BooleanTrue => Some(JsonValue::Boolean { value: true, span }),
        TokenKind::BooleanFalse => Some(JsonValue::Boolean { value: false, span }),
        TokenKind::Null => Some(JsonValue::Null { span }),
        _ => {
            ctx.push_diagnostic(LoweringDiagnostic::error(
                "JSON 値を解釈できませんでした",
                Some(span.clone()),
            ));
            Some(JsonValue::Null { span })
        }
    }
}

fn number_grouping_from_metadata(tok: &OwnedToken) -> NumberGrouping {
    let grouping = tok.metadata.iter().find_map(|meta| match meta {
        TokenMetadata::NumberLiteral(info) => Some(info.grouping),
        _ => None,
    });
    match grouping {
        Some(jv_lexer::NumberGroupingKind::Comma) => NumberGrouping::Comma,
        Some(jv_lexer::NumberGroupingKind::Underscore) => NumberGrouping::Underscore,
        Some(jv_lexer::NumberGroupingKind::Mixed) => NumberGrouping::Mixed,
        _ => NumberGrouping::None,
    }
}

fn json_value_span(value: &JsonValue) -> &jv_ast::Span {
    match value {
        JsonValue::Object { span, .. }
        | JsonValue::Array { span, .. }
        | JsonValue::String { span, .. }
        | JsonValue::Number { span, .. }
        | JsonValue::Boolean { span, .. }
        | JsonValue::Null { span } => span,
    }
}

fn is_json_trivia(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Whitespace
            | TokenKind::Newline
            | TokenKind::LayoutComma
            | TokenKind::FieldNameLabel
            | TokenKind::LineComment
            | TokenKind::BlockComment
            | TokenKind::JavaDocComment
    )
}

fn find_top_level_arrow(tokens: &[OwnedToken]) -> Option<usize> {
    let mut brace_depth: isize = 0;
    let mut paren_depth: isize = 0;
    let mut bracket_depth: isize = 0;
    for (idx, tok) in tokens.iter().enumerate() {
        match tok.kind {
            TokenKind::LeftBrace => brace_depth += 1,
            TokenKind::RightBrace => brace_depth -= 1,
            TokenKind::LeftParen => paren_depth += 1,
            TokenKind::RightParen => paren_depth -= 1,
            TokenKind::LeftBracket => bracket_depth += 1,
            TokenKind::RightBracket => bracket_depth -= 1,
            TokenKind::Arrow if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 => {
                return Some(idx);
            }
            _ => {}
        }
    }
    None
}

fn parse_lambda_parameters(ctx: &mut LoweringContext<'_>, tokens: &[OwnedToken]) -> Vec<Parameter> {
    let mut params = Vec::new();
    let tokens = trim_trivia(tokens);
    if tokens.is_empty() {
        return params;
    }

    let inner = if tokens
        .first()
        .is_some_and(|t| t.kind == TokenKind::LeftParen)
    {
        let mut depth: isize = 0;
        let mut end_idx = None;
        for (idx, tok) in tokens.iter().enumerate() {
            match tok.kind {
                TokenKind::LeftParen => depth += 1,
                TokenKind::RightParen => {
                    depth -= 1;
                    if depth == 0 {
                        end_idx = Some(idx);
                        break;
                    }
                }
                _ => {}
            }
        }
        let end_idx = end_idx.unwrap_or(tokens.len().saturating_sub(1));
        tokens.get(1..end_idx).unwrap_or(&[])
    } else {
        tokens
    };

    for tok in trim_trivia(inner) {
        if tok.kind == TokenKind::Identifier {
            params.push(Parameter {
                name: tok.lexeme_string(),
                type_annotation: None,
                default_value: None,
                modifiers: ParameterModifiers::default(),
                span: ctx.span_for_token(tok),
            });
        }
    }

    params
}

fn trim_trivia(tokens: &[OwnedToken]) -> &[OwnedToken] {
    let mut start = 0usize;
    let mut end = tokens.len();
    while start < end
        && matches!(
            tokens[start].kind,
            TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::LayoutComma
                | TokenKind::FieldNameLabel
                | TokenKind::LineComment
                | TokenKind::BlockComment
                | TokenKind::JavaDocComment
        )
    {
        start += 1;
    }
    while end > start
        && matches!(
            tokens[end - 1].kind,
            TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::LayoutComma
                | TokenKind::FieldNameLabel
                | TokenKind::LineComment
                | TokenKind::BlockComment
                | TokenKind::JavaDocComment
        )
    {
        end -= 1;
    }
    tokens.get(start..end).unwrap_or(&[])
}

fn parse_when_expression(
    ctx: &mut LoweringContext<'_>,
    cursor: &mut Cursor<'_>,
    when_tok: &OwnedToken,
    depth: usize,
) -> Expression {
    let when_span = ctx.span_for_token(when_tok);

    cursor.bump_while(|kind| {
        matches!(
            kind,
            TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::LayoutComma
                | TokenKind::FieldNameLabel
                | TokenKind::LineComment
                | TokenKind::BlockComment
                | TokenKind::JavaDocComment
        )
    });

    let mut subject: Option<Box<Expression>> = None;
    if cursor.peek_kind() != Some(TokenKind::LeftBrace) {
        let (subject_tokens, consumed) = take_until_left_brace(cursor.remaining());
        if !subject_tokens.is_empty() {
            subject = lower_expression_with_depth(ctx, subject_tokens, depth).map(Box::new);
        }
        cursor.pos = cursor.pos.saturating_add(consumed);
        cursor.bump_while(|kind| {
            matches!(
                kind,
                TokenKind::Whitespace
                    | TokenKind::Newline
                    | TokenKind::LayoutComma
                    | TokenKind::FieldNameLabel
                    | TokenKind::LineComment
                    | TokenKind::BlockComment
                    | TokenKind::JavaDocComment
            )
        });
    }

    let Some(open) = cursor.bump() else {
        return Expression::Literal(Literal::Null, when_span);
    };
    if open.kind != TokenKind::LeftBrace {
        return Expression::Literal(Literal::Null, when_span);
    }
    let open_brace_span = ctx.span_for_token(open);

    let mut arms = Vec::new();
    let mut else_arm: Option<Box<Expression>> = None;

    loop {
        cursor.bump_while(|kind| {
            matches!(
                kind,
                TokenKind::Whitespace
                    | TokenKind::Newline
                    | TokenKind::LayoutComma
                    | TokenKind::Semicolon
                    | TokenKind::FieldNameLabel
                    | TokenKind::LineComment
                    | TokenKind::BlockComment
                    | TokenKind::JavaDocComment
            )
        });

        if cursor.is_eof() || cursor.peek_kind() == Some(TokenKind::RightBrace) {
            break;
        }

        let remaining = cursor.remaining();
        let Some(arrow_offset) = find_top_level_when_arrow(remaining) else {
            break;
        };
        let pattern_tokens = remaining.get(..arrow_offset).unwrap_or(&[]);
        cursor.pos = cursor.pos.saturating_add(arrow_offset);
        let _ = cursor.bump(); // arrow

        let (body_tokens, consumed) = take_until_when_branch_end(cursor.remaining());
        cursor.pos = cursor.pos.saturating_add(consumed);

        let body = lower_expression_with_depth(ctx, body_tokens, depth)
            .unwrap_or_else(|| Expression::Literal(Literal::Null, when_span.clone()));

        if is_else_pattern(pattern_tokens) {
            else_arm = Some(Box::new(body));
            continue;
        }

        if let Some((pattern, guard)) = lower_when_pattern(ctx, pattern_tokens, depth) {
            arms.push(WhenArm {
                pattern,
                guard,
                body,
                span: when_span.clone(),
            });
        }
    }

    let close_span = if cursor.peek_kind() == Some(TokenKind::RightBrace) {
        cursor
            .bump()
            .map(|tok| ctx.span_for_token(tok))
            .unwrap_or(open_brace_span.clone())
    } else {
        open_brace_span
    };

    let span = when_span.merge(&close_span);
    Expression::When {
        expr: subject,
        arms,
        else_arm,
        implicit_end: None,
        span,
    }
}

fn is_else_pattern(tokens: &[OwnedToken]) -> bool {
    let tokens = trim_trivia(tokens);
    tokens.len() == 1 && tokens[0].kind == TokenKind::Else
}

fn lower_when_pattern(
    ctx: &mut LoweringContext<'_>,
    tokens: &[OwnedToken],
    depth: usize,
) -> Option<(jv_ast::types::Pattern, Option<Expression>)> {
    let tokens = trim_trivia(tokens);
    if tokens.is_empty() {
        return None;
    }

    let has_comma = tokens.iter().any(|t| t.kind == TokenKind::Comma);
    let expr = lower_expression_with_depth(ctx, tokens, depth)?;
    match expr.clone() {
        Expression::Identifier(name, span) if name == "_" && !has_comma => {
            Some((jv_ast::types::Pattern::Wildcard(span), None))
        }
        Expression::Identifier(name, span) if !has_comma => {
            Some((jv_ast::types::Pattern::Identifier(name, span), None))
        }
        Expression::Literal(lit, span) if !has_comma => {
            Some((jv_ast::types::Pattern::Literal(lit, span), None))
        }
        Expression::Binary {
            op: BinaryOp::And,
            left,
            right,
            ..
        } if !has_comma
            && matches!(
                left.as_ref(),
                Expression::Literal(Literal::Boolean(_) | Literal::Null, _)
            ) =>
        {
            let Expression::Literal(lit, lit_span) = *left else {
                unreachable!()
            };
            Some((jv_ast::types::Pattern::Literal(lit, lit_span), Some(*right)))
        }
        _ => None,
    }
}

fn find_top_level_when_arrow(tokens: &[OwnedToken]) -> Option<usize> {
    let mut brace_depth: isize = 0;
    let mut paren_depth: isize = 0;
    let mut bracket_depth: isize = 0;
    for (idx, tok) in tokens.iter().enumerate() {
        match tok.kind {
            TokenKind::LeftBrace => brace_depth += 1,
            TokenKind::RightBrace => brace_depth -= 1,
            TokenKind::LeftParen => paren_depth += 1,
            TokenKind::RightParen => paren_depth -= 1,
            TokenKind::LeftBracket => bracket_depth += 1,
            TokenKind::RightBracket => bracket_depth -= 1,
            TokenKind::Arrow | TokenKind::FatArrow
                if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 =>
            {
                return Some(idx);
            }
            _ => {}
        }
    }
    None
}

fn take_until_left_brace(tokens: &[OwnedToken]) -> (&[OwnedToken], usize) {
    let mut paren_depth: isize = 0;
    let mut bracket_depth: isize = 0;
    for (idx, tok) in tokens.iter().enumerate() {
        match tok.kind {
            TokenKind::LeftParen => paren_depth += 1,
            TokenKind::RightParen => paren_depth -= 1,
            TokenKind::LeftBracket => bracket_depth += 1,
            TokenKind::RightBracket => bracket_depth -= 1,
            TokenKind::LeftBrace if paren_depth == 0 && bracket_depth == 0 => {
                return (tokens.get(..idx).unwrap_or(&[]), idx);
            }
            _ => {}
        }
    }
    (tokens, tokens.len())
}

fn take_until_when_branch_end(tokens: &[OwnedToken]) -> (&[OwnedToken], usize) {
    let mut brace_depth: isize = 0;
    let mut paren_depth: isize = 0;
    let mut bracket_depth: isize = 0;
    for (idx, tok) in tokens.iter().enumerate() {
        match tok.kind {
            TokenKind::LeftBrace => brace_depth += 1,
            TokenKind::RightBrace if brace_depth > 0 => brace_depth -= 1,
            TokenKind::LeftParen => paren_depth += 1,
            TokenKind::RightParen => paren_depth -= 1,
            TokenKind::LeftBracket => bracket_depth += 1,
            TokenKind::RightBracket => bracket_depth -= 1,
            TokenKind::Newline | TokenKind::LayoutComma | TokenKind::Semicolon
                if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 =>
            {
                return (tokens.get(..idx).unwrap_or(&[]), idx + 1);
            }
            TokenKind::RightBrace if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 => {
                return (tokens.get(..idx).unwrap_or(&[]), idx);
            }
            _ => {}
        }
    }
    (tokens, tokens.len())
}

fn take_type_tokens(tokens: &[OwnedToken]) -> (&[OwnedToken], usize) {
    let mut angle_depth: isize = 0;
    let mut end = 0usize;

    for (idx, tok) in tokens.iter().enumerate() {
        match tok.kind {
            TokenKind::Less => {
                angle_depth += 1;
                end = idx + 1;
            }
            TokenKind::Greater => {
                angle_depth = angle_depth.saturating_sub(1);
                end = idx + 1;
            }
            TokenKind::Identifier
            | TokenKind::Dot
            | TokenKind::Question
            | TokenKind::Comma
            | TokenKind::LeftBracket
            | TokenKind::RightBracket => {
                end = idx + 1;
            }
            TokenKind::Whitespace
            | TokenKind::Newline
            | TokenKind::LayoutComma
            | TokenKind::FieldNameLabel
            | TokenKind::LineComment
            | TokenKind::BlockComment
            | TokenKind::JavaDocComment => {
                end = idx + 1;
            }
            TokenKind::RightParen
            | TokenKind::RightBrace
            | TokenKind::Semicolon
            | TokenKind::Arrow
            | TokenKind::FatArrow
            | TokenKind::Eof
                if angle_depth == 0 =>
            {
                break;
            }
            _ if angle_depth == 0 => break,
            _ => {
                end = idx + 1;
            }
        }
    }

    let slice = tokens.get(..end).unwrap_or(&[]);
    (trim_trivia(slice), end)
}

fn infix_binding_power(token: &OwnedToken) -> Option<(u8, u8, BinaryOp)> {
    let (l, r, op) = match token.kind {
        TokenKind::Or => (1, 2, BinaryOp::Or),
        TokenKind::And => (2, 3, BinaryOp::And),
        TokenKind::Elvis => (3, 4, BinaryOp::Elvis),
        TokenKind::Equal => (4, 5, BinaryOp::Equal),
        TokenKind::NotEqual => (4, 5, BinaryOp::NotEqual),
        TokenKind::Identifier if token.lexeme_eq("is") => (4, 5, BinaryOp::Is),
        TokenKind::Less => (5, 6, BinaryOp::Less),
        TokenKind::LessEqual => (5, 6, BinaryOp::LessEqual),
        TokenKind::Greater => (5, 6, BinaryOp::Greater),
        TokenKind::GreaterEqual => (5, 6, BinaryOp::GreaterEqual),
        TokenKind::RangeExclusive => (5, 6, BinaryOp::RangeExclusive),
        TokenKind::RangeInclusive => (5, 6, BinaryOp::RangeInclusive),
        TokenKind::Plus => (6, 7, BinaryOp::Add),
        TokenKind::Minus => (6, 7, BinaryOp::Subtract),
        TokenKind::Multiply => (7, 8, BinaryOp::Multiply),
        TokenKind::Divide => (7, 8, BinaryOp::Divide),
        TokenKind::Modulo => (7, 8, BinaryOp::Modulo),
        _ => return None,
    };
    Some((l, r, op))
}

fn clean_comment_text(kind: TokenKind, raw: &str) -> String {
    let trimmed = raw.trim();
    match kind {
        TokenKind::LineComment => trimmed
            .strip_prefix("//")
            .unwrap_or(trimmed)
            .trim()
            .to_string(),
        TokenKind::JavaDocComment | TokenKind::BlockComment => trimmed
            .strip_prefix("/**")
            .or_else(|| trimmed.strip_prefix("/*"))
            .unwrap_or(trimmed)
            .strip_suffix("*/")
            .unwrap_or(trimmed)
            .trim()
            .to_string(),
        _ => trimmed.to_string(),
    }
}

fn collect_trailing_tuple_labels(
    ctx: &LoweringContext<'_>,
    cursor: &mut Cursor<'_>,
) -> Vec<(TokenKind, String, jv_ast::Span)> {
    let mut labels = Vec::new();
    loop {
        cursor.bump_while(|kind| matches!(kind, TokenKind::Whitespace));
        let Some(next) = cursor.peek() else {
            break;
        };
        match next.kind {
            TokenKind::FieldNameLabel => {
                labels.push((next.kind, next.lexeme_string(), ctx.span_for_token(next)));
                cursor.bump();
                continue;
            }
            TokenKind::LineComment | TokenKind::BlockComment | TokenKind::JavaDocComment => {
                labels.push((next.kind, next.lexeme_string(), ctx.span_for_token(next)));
                cursor.bump();
                continue;
            }
            _ => break,
        }
    }
    labels
}

fn collect_carried_tuple_labels(
    _ctx: &LoweringContext<'_>,
    cursor: &Cursor<'_>,
    current_span: &jv_ast::Span,
) -> Vec<(TokenKind, String, jv_ast::Span)> {
    let mut labels = Vec::new();
    let Some(next) = cursor.remaining().iter().find(|tok| {
        !matches!(
            tok.kind,
            TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::LayoutComma
                | TokenKind::FieldNameLabel
                | TokenKind::LineComment
                | TokenKind::BlockComment
                | TokenKind::JavaDocComment
        )
    }) else {
        return labels;
    };

    for comment in next
        .leading_trivia
        .passthrough_comments
        .iter()
        .chain(next.leading_trivia.jv_comments.iter())
    {
        if comment.line != current_span.end_line {
            continue;
        }
        let kind = match comment.kind {
            jv_lexer::SourceCommentKind::Line => TokenKind::LineComment,
            jv_lexer::SourceCommentKind::Block => TokenKind::BlockComment,
        };
        let cleaned = comment.text.trim().to_string();
        if cleaned.is_empty() {
            continue;
        }
        let span = jv_ast::Span::new(
            comment.line,
            comment.column,
            comment.line,
            comment.column.saturating_add(cleaned.len()),
        );
        labels.push((kind, cleaned, span));
    }

    labels
}

fn lower_string_interpolation(
    ctx: &mut LoweringContext<'_>,
    tok: &OwnedToken,
    depth: usize,
) -> Expression {
    let span = ctx.span_for_token(tok);
    let segments = tok.metadata.iter().find_map(|meta| match meta {
        TokenMetadata::StringInterpolation { segments } => Some(segments),
        _ => None,
    });

    let Some(segments) = segments else {
        return Expression::Literal(Literal::String(tok.lexeme_string()), span);
    };

    let mut parts = Vec::with_capacity(segments.len());
    for segment in segments {
        match segment {
            StringInterpolationSegment::Literal(text) => parts.push(StringPart::Text(text.clone())),
            StringInterpolationSegment::Expression(raw) => {
                let expr = lower_interpolation_expression(ctx, raw, depth.saturating_add(1))
                    .unwrap_or_else(|| Expression::Identifier(raw.clone(), span.clone()));
                parts.push(StringPart::Expression(expr));
            }
        }
    }

    Expression::StringInterpolation { parts, span }
}

fn lower_interpolation_expression(
    ctx: &mut LoweringContext<'_>,
    raw: &str,
    depth: usize,
) -> Option<Expression> {
    let mut lexer = Lexer::with_layout_mode(raw.to_string(), LayoutMode::Enabled);
    let tokens = lexer.tokenize().ok()?;
    let owned_tokens = tokens
        .into_iter()
        .filter(|tok| !matches!(tok.token_type, jv_lexer::TokenType::Eof))
        .map(|tok| OwnedToken {
            kind: crate::lexer::kind_from_token_type(&tok.token_type),
            span: TokenSpan { start: 0, end: 0 },
            lexeme: tok.lexeme,
            leading_trivia: tok.leading_trivia,
            metadata: tok.metadata,
            diagnostic: tok.diagnostic,
        })
        .collect::<Vec<_>>();

    lower_expression_with_depth(ctx, &owned_tokens, depth)
}
