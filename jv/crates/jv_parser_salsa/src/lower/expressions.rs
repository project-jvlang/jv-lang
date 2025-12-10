use super::context::{LoweringContext, LoweringDiagnostic};
use crate::lexer::TokenKind;
use crate::parser::OwnedToken;
use jv_ast::expression::{Argument, CallArgumentMetadata, Expression};
use jv_ast::types::{BinaryOp, Literal, UnaryOp};

/// トークンスライスを前提にした簡易 Pratt パーサ。
pub fn lower_expression(
    ctx: &mut LoweringContext<'_>,
    tokens: &[OwnedToken],
) -> Option<Expression> {
    let mut cursor = Cursor::new(tokens);
    parse_expression_bp(ctx, &mut cursor, 0)
}

/// 指定した区切りトークンまでのスライスを返す（最初の出現位置基準）。
pub fn slice_until<'a, F>(
    tokens: &'a [OwnedToken],
    delimiter: TokenKind,
    kind_fn: F,
) -> Option<(&'a [OwnedToken], &'a [OwnedToken])>
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

    fn is_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }
}

fn parse_expression_bp(
    ctx: &mut LoweringContext<'_>,
    cursor: &mut Cursor<'_>,
    min_bp: u8,
) -> Option<Expression> {
    let mut lhs = parse_prefix(ctx, cursor)?;

    loop {
        let op_tok = match cursor.peek() {
            Some(tok) => tok.clone(),
            None => break,
        };

        let (l_bp, r_bp, op) = match infix_binding_power(&op_tok) {
            Some(info) => info,
            None => break,
        };

        if l_bp < min_bp {
            break;
        }

        cursor.bump(); // operator
        let rhs = match parse_expression_bp(ctx, cursor, r_bp) {
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

fn parse_prefix(ctx: &mut LoweringContext<'_>, cursor: &mut Cursor<'_>) -> Option<Expression> {
    let tok = cursor.bump()?;
    match tok.kind {
        TokenKind::Number => Some(Expression::Literal(
            Literal::Number(tok.lexeme.clone()),
            ctx.span_for_token(tok),
        )),
        TokenKind::String | TokenKind::StringInterpolation => Some(Expression::Literal(
            Literal::String(tok.lexeme.clone()),
            ctx.span_for_token(tok),
        )),
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
            tok.lexeme.clone(),
            ctx.span_for_token(tok),
        )),
        TokenKind::Minus | TokenKind::Plus | TokenKind::Not => {
            let op = match tok.kind {
                TokenKind::Minus => UnaryOp::Minus,
                TokenKind::Plus => UnaryOp::Plus,
                TokenKind::Not => UnaryOp::Not,
                _ => UnaryOp::Plus,
            };
            let operand = parse_expression_bp(ctx, cursor, 9)
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
        TokenKind::LeftParen => {
            let inner = parse_expression_bp(ctx, cursor, 0);
            // consume ')'
            if cursor.peek_kind() == Some(TokenKind::RightParen) {
                cursor.bump();
            }
            // タプル検出などは簡略化し単一式とする。
            inner
        }
        TokenKind::LeftBrace => {
            // ブロック: StatementList は上位で処理されるため、ここでは空ブロックにする。
            Some(Expression::Block {
                statements: Vec::new(),
                span: ctx.span_for_token(tok),
            })
        }
        _ => {
            ctx.push_diagnostic(LoweringDiagnostic::error(
                "式を解釈できませんでした",
                Some(ctx.span_for_token(tok)),
            ));
            None
        }
    }
    .map(|expr| parse_postfix(ctx, cursor, expr))
}

fn parse_postfix(
    ctx: &mut LoweringContext<'_>,
    cursor: &mut Cursor<'_>,
    mut expr: Expression,
) -> Expression {
    loop {
        match cursor.peek_kind() {
            Some(TokenKind::LeftParen) => {
                cursor.bump();
                let mut args = Vec::new();
                let mut used_commas = false;
                while !cursor.is_eof() && cursor.peek_kind() != Some(TokenKind::RightParen) {
                    if let Some(arg) = parse_expression_bp(ctx, cursor, 0) {
                        args.push(Argument::Positional(arg));
                    }
                    if cursor.peek_kind() == Some(TokenKind::Comma) {
                        cursor.bump();
                        used_commas = true;
                    } else {
                        break;
                    }
                }
                if cursor.peek_kind() == Some(TokenKind::RightParen) {
                    cursor.bump();
                }
                let span = expr.span().clone();
                expr = Expression::Call {
                    function: Box::new(expr),
                    args,
                    type_arguments: Vec::new(),
                    argument_metadata: CallArgumentMetadata {
                        used_commas,
                        ..Default::default()
                    },
                    span,
                };
            }
            Some(TokenKind::Dot) => {
                cursor.bump();
                if let Some(ident) = cursor.bump() {
                    let span = expr.span().merge(&ctx.span_for_token(ident));
                    expr = Expression::MemberAccess {
                        object: Box::new(expr),
                        property: ident.lexeme.clone(),
                        span,
                    };
                } else {
                    break;
                }
            }
            Some(TokenKind::NullSafe) => {
                cursor.bump();
                if let Some(ident) = cursor.bump() {
                    let span = expr.span().merge(&ctx.span_for_token(ident));
                    expr = Expression::NullSafeMemberAccess {
                        object: Box::new(expr),
                        property: ident.lexeme.clone(),
                        span,
                    };
                } else {
                    break;
                }
            }
            Some(TokenKind::LeftBracket) => {
                cursor.bump();
                let index = parse_expression_bp(ctx, cursor, 0)
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
            _ => break,
        }
    }
    expr
}

fn infix_binding_power(token: &OwnedToken) -> Option<(u8, u8, BinaryOp)> {
    let (l, r, op) = match token.kind {
        TokenKind::Or => (1, 2, BinaryOp::Or),
        TokenKind::And => (2, 3, BinaryOp::And),
        TokenKind::Elvis => (3, 4, BinaryOp::Elvis),
        TokenKind::Equal => (4, 5, BinaryOp::Equal),
        TokenKind::NotEqual => (4, 5, BinaryOp::NotEqual),
        TokenKind::Identifier if token.lexeme == "is" => (4, 5, BinaryOp::Is),
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
