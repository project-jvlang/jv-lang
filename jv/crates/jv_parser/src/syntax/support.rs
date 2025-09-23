use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{
    ConcurrencyConstruct, Expression, ResourceManagement, Span, Statement, TypeAnnotation,
};
use jv_lexer::{Token, TokenType};

pub(crate) fn token_val() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Val))
}

pub(crate) fn token_var() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Var))
}

pub(crate) fn token_fun() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Fun))
}

pub(crate) fn token_data() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Data))
}

pub(crate) fn token_class() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Class))
}

pub(crate) fn token_return() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| match &token.token_type {
        TokenType::Return => true,
        TokenType::Identifier(name) => name == "return",
        _ => false,
    })
}

pub(crate) fn token_use() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    keyword("use")
}

pub(crate) fn token_defer() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    keyword("defer")
}

pub(crate) fn token_spawn() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    keyword("spawn")
}

pub(crate) fn token_when() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::When))
}

pub(crate) fn token_else() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Else))
}

pub(crate) fn token_assign() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Assign))
}

pub(crate) fn token_left_paren() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone
{
    filter(|token: &Token| matches!(token.token_type, TokenType::LeftParen))
}

pub(crate) fn token_right_paren() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone
{
    filter(|token: &Token| matches!(token.token_type, TokenType::RightParen))
}

pub(crate) fn token_left_brace() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone
{
    filter(|token: &Token| matches!(token.token_type, TokenType::LeftBrace))
}

pub(crate) fn token_right_brace() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone
{
    filter(|token: &Token| matches!(token.token_type, TokenType::RightBrace))
}

pub(crate) fn token_comma() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Comma))
}

pub(crate) fn token_layout_comma() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone
{
    filter(|token: &Token| matches!(token.token_type, TokenType::LayoutComma))
}

pub(crate) fn token_colon() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Colon))
}

pub(crate) fn token_at() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::At))
}

pub(crate) fn token_dot() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Dot))
}

pub(crate) fn token_null_safe() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::NullSafe))
}

pub(crate) fn token_arrow() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Arrow))
}

pub(crate) fn token_plus() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Plus))
}

pub(crate) fn token_minus() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Minus))
}

pub(crate) fn token_multiply() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Multiply))
}

pub(crate) fn token_divide() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Divide))
}

pub(crate) fn token_modulo() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Modulo))
}

pub(crate) fn token_equal() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Equal))
}

pub(crate) fn token_not_equal() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::NotEqual))
}

pub(crate) fn token_less() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Less))
}

pub(crate) fn token_less_equal() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone
{
    filter(|token: &Token| matches!(token.token_type, TokenType::LessEqual))
}

pub(crate) fn token_greater() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Greater))
}

pub(crate) fn token_greater_equal(
) -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::GreaterEqual))
}

pub(crate) fn token_and() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::And))
}

pub(crate) fn token_or() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Or))
}

pub(crate) fn token_not() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Not))
}

pub(crate) fn token_question() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Question))
}

pub(crate) fn token_elvis() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Elvis))
}

pub(crate) fn token_if() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::If))
}

pub(crate) fn token_left_bracket() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone
{
    filter(|token: &Token| matches!(token.token_type, TokenType::LeftBracket))
}

pub(crate) fn token_right_bracket(
) -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::RightBracket))
}

pub(crate) fn token_string_start() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone
{
    filter(|token: &Token| matches!(token.token_type, TokenType::StringStart))
}

pub(crate) fn token_string_mid() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone
{
    filter(|token: &Token| matches!(token.token_type, TokenType::StringMid))
}

pub(crate) fn token_string_end() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone
{
    filter(|token: &Token| matches!(token.token_type, TokenType::StringEnd))
}

pub(crate) fn identifier() -> impl ChumskyParser<Token, String, Error = Simple<Token>> + Clone {
    filter_map(|span, token: Token| match token.token_type {
        TokenType::Identifier(name) => Ok(name),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    })
}

pub(crate) fn identifier_with_span(
) -> impl ChumskyParser<Token, (String, Span), Error = Simple<Token>> + Clone {
    filter_map(|span, token: Token| {
        let token_span = span_from_token(&token);
        match token.token_type {
            TokenType::Identifier(name) => Ok((name, token_span)),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        }
    })
}

pub(crate) fn span_from_token(token: &Token) -> Span {
    Span {
        start_line: token.line,
        start_column: token.column,
        end_line: token.line,
        end_column: token.column + token.lexeme.len(),
    }
}

pub(crate) fn merge_spans(start: &Span, end: &Span) -> Span {
    Span {
        start_line: start.start_line,
        start_column: start.start_column,
        end_line: end.end_line,
        end_column: end.end_column,
    }
}

pub(crate) fn expression_span(expr: &Expression) -> Span {
    match expr {
        Expression::Literal(_, span) => span.clone(),
        Expression::Identifier(_, span) => span.clone(),
        Expression::Binary { span, .. } => span.clone(),
        Expression::Unary { span, .. } => span.clone(),
        Expression::Call { span, .. } => span.clone(),
        Expression::MemberAccess { span, .. } => span.clone(),
        Expression::NullSafeMemberAccess { span, .. } => span.clone(),
        Expression::StringInterpolation { span, .. } => span.clone(),
        Expression::When { span, .. } => span.clone(),
        Expression::If { span, .. } => span.clone(),
        Expression::IndexAccess { span, .. } => span.clone(),
        Expression::NullSafeIndexAccess { span, .. } => span.clone(),
        Expression::Block { span, .. } => span.clone(),
        Expression::Array { span, .. } => span.clone(),
        Expression::Lambda { span, .. } => span.clone(),
        Expression::Try { span, .. } => span.clone(),
        Expression::This(span) => span.clone(),
        Expression::Super(span) => span.clone(),
    }
}

pub(crate) fn statement_span(stmt: &Statement) -> Span {
    match stmt {
        Statement::ValDeclaration { span, .. } => span.clone(),
        Statement::VarDeclaration { span, .. } => span.clone(),
        Statement::FunctionDeclaration { span, .. } => span.clone(),
        Statement::DataClassDeclaration { span, .. } => span.clone(),
        Statement::Expression { span, .. } => span.clone(),
        Statement::Return { span, .. } => span.clone(),
        Statement::Assignment { span, .. } => span.clone(),
        Statement::ClassDeclaration { span, .. } => span.clone(),
        Statement::InterfaceDeclaration { span, .. } => span.clone(),
        Statement::ExtensionFunction(ef) => ef.span.clone(),
        Statement::Import { span, .. } => span.clone(),
        Statement::While { span, .. } => span.clone(),
        Statement::For { span, .. } => span.clone(),
        Statement::Break(span) => span.clone(),
        Statement::Continue(span) => span.clone(),
        Statement::Package { span, .. } => span.clone(),
        Statement::Concurrency(cc) => match cc {
            ConcurrencyConstruct::Spawn { span, .. } => span.clone(),
            ConcurrencyConstruct::Async { span, .. } => span.clone(),
            ConcurrencyConstruct::Await { span, .. } => span.clone(),
        },
        Statement::ResourceManagement(rm) => match rm {
            ResourceManagement::Use { span, .. } => span.clone(),
            ResourceManagement::Defer { span, .. } => span.clone(),
        },
    }
}

pub(crate) fn keyword(
    expected: &'static str,
) -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(move |token: &Token| match &token.token_type {
        TokenType::Identifier(name) => name == expected,
        _ => false,
    })
}

pub(crate) fn type_annotation_simple(
) -> impl ChumskyParser<Token, TypeAnnotation, Error = Simple<Token>> + Clone {
    identifier().map(TypeAnnotation::Simple)
}
