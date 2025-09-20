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

pub(crate) fn token_colon() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Colon))
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

pub(crate) fn identifier() -> impl ChumskyParser<Token, String, Error = Simple<Token>> + Clone {
    filter_map(|span, token: Token| match token.token_type {
        TokenType::Identifier(name) => Ok(name),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
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
