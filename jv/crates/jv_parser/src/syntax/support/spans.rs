use jv_ast::{ConcurrencyConstruct, Expression, ResourceManagement, Span, Statement};
use jv_lexer::Token;

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
        Expression::RegexLiteral(literal) => literal.span.clone(),
        Expression::Identifier(_, span) => span.clone(),
        Expression::Binary { span, .. } => span.clone(),
        Expression::Unary { span, .. } => span.clone(),
        Expression::Call { span, .. } => span.clone(),
        Expression::MemberAccess { span, .. } => span.clone(),
        Expression::NullSafeMemberAccess { span, .. } => span.clone(),
        Expression::StringInterpolation { span, .. } => span.clone(),
        Expression::MultilineString(literal) => literal.span.clone(),
        Expression::JsonLiteral(literal) => literal.span.clone(),
        Expression::When { span, .. } => span.clone(),
        Expression::If { span, .. } => span.clone(),
        Expression::IndexAccess { span, .. } => span.clone(),
        Expression::NullSafeIndexAccess { span, .. } => span.clone(),
        Expression::TypeCast { span, .. } => span.clone(),
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
        Statement::Throw { span, .. } => span.clone(),
        Statement::Assignment { span, .. } => span.clone(),
        Statement::ClassDeclaration { span, .. } => span.clone(),
        Statement::InterfaceDeclaration { span, .. } => span.clone(),
        Statement::ExtensionFunction(ef) => ef.span.clone(),
        Statement::Import { span, .. } => span.clone(),
        Statement::ForIn(for_in) => for_in.span.clone(),
        Statement::Break(span) => span.clone(),
        Statement::Continue(span) => span.clone(),
        Statement::Package { span, .. } => span.clone(),
        Statement::Comment(comment) => comment.span.clone(),
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
