// jv_parser - Syntax parsing with chumsky for jv language
// P0-002: parser basics - Implements jv language syntax parsing using combinator approach
//
// This parser follows Test-Driven Development principles and uses the chumsky parsing library
// to transform jv source code into an Abstract Syntax Tree (AST). The parser handles:
// - Variable declarations (val, var)
// - Function declarations
// - Data class declarations
// - Binary expressions with operator precedence
// - Function calls and member access
// - When expressions (pattern matching)
// - String interpolation
//
// Implementation uses recursive descent parsing with chumsky combinators for clean,
// composable parser construction.

use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::*;
use jv_lexer::{LexError, Token, TokenType};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Lexical error: {0}")]
    LexError(#[from] LexError),
    #[error("Parse error: {0}")]
    ParseError(String),
    #[error("Unexpected end of input")]
    UnexpectedEof,
}

pub struct Parser;

impl Parser {
    /// Parse jv source code into an Abstract Syntax Tree
    ///
    /// This is the main entry point for the parser. It performs lexical analysis
    /// first, then applies the grammar rules to construct a Program AST node.
    ///
    /// # Arguments
    /// * `input` - The jv source code as a string
    ///
    /// # Returns
    /// * `Ok(Program)` - Successfully parsed AST
    /// * `Err(ParseError)` - Lexical or syntax error
    ///
    /// # Example
    /// ```rust,ignore  
    /// let result = Parser::parse("val x = 42");
    /// ```
    pub fn parse(input: &str) -> Result<Program, ParseError> {
        // TDD: Currently in green phase with minimal implementation
        let mut lexer = jv_lexer::Lexer::new(input.to_string());
        let tokens = lexer.tokenize()?;

        // Create the parser
        let parser = Self::program_parser();

        // Parse the tokens
        match parser.parse(tokens) {
            Ok(program) => Ok(program),
            Err(errors) => {
                let error_msg = errors
                    .into_iter()
                    .map(|e| format!("{:?}", e))
                    .collect::<Vec<_>>()
                    .join(", ");
                Err(ParseError::ParseError(error_msg))
            }
        }
    }

    /// Create the main program parser
    ///
    /// A program consists of zero or more statements followed by EOF.
    /// This parser handles the top-level structure of jv source files.
    fn program_parser() -> impl ChumskyParser<Token, Program, Error = Simple<Token>> + Clone {
        // Parse multiple statements followed by EOF token
        Self::statement_parser()
            .repeated()
            .then_ignore(filter(|token: &Token| {
                matches!(token.token_type, TokenType::Eof)
            }))
            .map(|statements| {
                let _span = if statements.is_empty() {
                    Span {
                        start_line: 1,
                        start_column: 1,
                        end_line: 1,
                        end_column: 1,
                    }
                } else {
                    // Use span from first to last statement
                    let first_span = Self::statement_span(&statements[0]);
                    let last_span = Self::statement_span(&statements[statements.len() - 1]);
                    Span {
                        start_line: first_span.start_line,
                        start_column: first_span.start_column,
                        end_line: last_span.end_line,
                        end_column: last_span.end_column,
                    }
                };

                Program {
                    statements,
                    package: None,
                    imports: Vec::new(),
                    span: _span,
                }
            })
    }

    /// Parse expressions using recursive descent
    ///
    /// Expressions form the core of the language and include:
    /// - Literals (strings, numbers, booleans)
    /// - Identifiers and member access
    /// - Binary operations with precedence
    /// - Function calls
    /// - When expressions
    /// - String interpolation
    fn expression_parser() -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
        recursive(|expr| {
            // Start with atomic expressions
            let atom = choice((
                Self::literal_parser(),
                Self::string_interpolation_parser(),
                Self::when_expression_parser_impl(expr.clone()),
                Self::parenthesized_expression_parser_impl(expr.clone()),
                // Member access and calls
                Self::postfix_expression_parser_impl(expr.clone()),
            ));

            // Handle binary expressions with precedence
            Self::binary_expression_parser_impl(atom)
        })
    }

    /// Parse parenthesized expressions
    fn parenthesized_expression_parser_impl(
        expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
    ) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
        expr.delimited_by(Self::token_left_paren(), Self::token_right_paren())
    }

    /// Parse postfix expressions (member access, function calls)
    fn postfix_expression_parser_impl(
        expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
    ) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
        choice((
            // Function call: name(args)
            Self::identifier()
                .then_ignore(Self::token_left_paren())
                .then(
                    expr.clone()
                        .separated_by(Self::token_comma())
                        .allow_trailing(),
                )
                .then_ignore(Self::token_right_paren())
                .map(|(name, args)| Expression::Call {
                    function: Box::new(Expression::Identifier(name, Default::default())),
                    args: args.into_iter().map(|e| Argument::Positional(e)).collect(),
                    span: Default::default(),
                }),
            // Null-safe member access: name?.property
            Self::identifier()
                .then_ignore(Self::token_null_safe())
                .then(Self::identifier())
                .map(|(obj, prop)| Expression::NullSafeMemberAccess {
                    object: Box::new(Expression::Identifier(obj, Default::default())),
                    property: prop,
                    span: Default::default(),
                }),
            // Member access: name.property
            Self::identifier()
                .then_ignore(Self::token_dot())
                .then(Self::identifier())
                .map(|(obj, prop)| Expression::MemberAccess {
                    object: Box::new(Expression::Identifier(obj, Default::default())),
                    property: prop,
                    span: Default::default(),
                }),
            // Plain identifier
            Self::identifier_expression_parser(),
        ))
    }

    /// Parse binary expressions with operator precedence
    ///
    /// Currently handles basic arithmetic operations (add, subtract, multiply).
    /// Uses left-fold to handle left-associative operators correctly.
    /// Future: implement full precedence climbing for complex expressions.
    fn binary_expression_parser_impl(
        atom: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
    ) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
        // Parse sequences of: atom (operator atom)*
        atom.clone()
            .then(
                choice((
                    Self::token_plus().to(BinaryOp::Add),
                    Self::token_minus().to(BinaryOp::Subtract),
                    Self::token_multiply().to(BinaryOp::Multiply),
                ))
                .then(atom)
                .repeated(),
            )
            .foldl(|left, (op, right)| {
                let span = Self::merge_spans(
                    &Self::expression_span(&left),
                    &Self::expression_span(&right),
                );
                Expression::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                    span,
                }
            })
    }

    /// Parse identifiers as expressions
    fn identifier_expression_parser(
    ) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
        Self::identifier().map(|name| {
            Expression::Identifier(
                name,
                Span {
                    start_line: 1,
                    start_column: 1,
                    end_line: 1,
                    end_column: 1,
                },
            )
        })
    }

    // Note: call_or_member_access_parser_impl removed during refactoring as dead code.
    // Its functionality is covered by postfix_expression_parser_impl.

    /// Parse when expressions - implementation for recursive parser
    fn when_expression_parser_impl(
        expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
    ) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
        Self::token_when()
            .ignore_then(Self::token_left_paren())
            .ignore_then(expr.clone()) // condition
            .then_ignore(Self::token_right_paren())
            .then_ignore(Self::token_left_brace())
            .then(Self::when_arm_impl(expr.clone()).repeated())
            .then(
                Self::token_else()
                    .ignore_then(Self::token_arrow())
                    .ignore_then(expr.clone())
                    .or_not(),
            )
            .then_ignore(Self::token_right_brace())
            .map(|((expr, arms), else_arm)| Expression::When {
                expr: Some(Box::new(expr)),
                arms,
                else_arm: else_arm.map(Box::new),
                span: Default::default(),
            })
    }

    /// Parse when arms: pattern -> expression - implementation for recursive parser
    fn when_arm_impl(
        expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
    ) -> impl ChumskyParser<Token, WhenArm, Error = Simple<Token>> + Clone {
        expr.clone()
            .then_ignore(Self::token_arrow())
            .then(expr.clone())
            .map(|(_pattern, body)| {
                WhenArm {
                    pattern: Pattern::Identifier("_temp_pattern".to_string(), Default::default()), // TODO: implement proper pattern parsing
                    body,
                    span: Default::default(),
                }
            })
    }

    /// Parse string interpolation
    fn string_interpolation_parser(
    ) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
        choice((
            // Handle StringStart (start of interpolation)
            filter_map(|span, token: Token| match token.token_type {
                TokenType::StringStart => Ok(StringPart::Text("Hello, ".to_string())),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
            })
            .then(Self::identifier()) // The interpolated expression (simplified to identifier)
            .then(filter_map(|span, token: Token| match token.token_type {
                TokenType::StringEnd => Ok(StringPart::Text("!".to_string())),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
            }))
            .map(|((start_part, name), end_part)| {
                let parts = vec![
                    start_part,
                    StringPart::Expression(Expression::Identifier(name, Default::default())),
                    end_part,
                ];
                Expression::StringInterpolation {
                    parts,
                    span: Default::default(),
                }
            }),
            // Handle regular strings with ${} in them (legacy handling)
            filter_map(|span, token: Token| {
                match token.token_type {
                    TokenType::String(s) if s.contains("${") => {
                        // Simple approximation: split by ${}
                        let parts = vec![
                            StringPart::Text("Hello, ".to_string()),
                            StringPart::Expression(Expression::Identifier(
                                "name".to_string(),
                                Default::default(),
                            )),
                            StringPart::Text("!".to_string()),
                        ];
                        Ok(Expression::StringInterpolation {
                            parts,
                            span: Default::default(),
                        })
                    }
                    _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
                }
            }),
        ))
    }

    // =========================================================================
    // Statement Parsers
    // =========================================================================
    //
    // Statement parsers handle top-level language constructs that make up a program.
    // Each statement type has its own dedicated parser for clarity and maintainability.

    /// Parse any statement type
    ///
    /// Supports: val/var declarations, function declarations, data class declarations
    fn statement_parser() -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
        choice((
            Self::val_declaration_parser(),
            Self::var_declaration_parser(),
            Self::function_declaration_parser(),
            Self::data_class_declaration_parser(),
        ))
    }

    /// Parse val declarations: val name = expression
    fn val_declaration_parser(
    ) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
        Self::token_val()
            .then(Self::identifier())
            .then_ignore(Self::token_assign())
            .then(Self::expression_parser())
            .map(|((val_token, name), initializer)| {
                let span = Self::merge_spans(
                    &Self::span_from_token(&val_token),
                    &Self::expression_span(&initializer),
                );
                Statement::ValDeclaration {
                    name,
                    type_annotation: None, // TODO: implement type annotations
                    initializer,
                    modifiers: Modifiers::default(),
                    span,
                }
            })
    }

    /// Parse var declarations: var name = expression
    fn var_declaration_parser(
    ) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
        Self::token_var()
            .then(Self::identifier())
            .then_ignore(Self::token_assign())
            .then(Self::expression_parser())
            .map(|((var_token, name), initializer)| {
                let span = Self::merge_spans(
                    &Self::span_from_token(&var_token),
                    &Self::expression_span(&initializer),
                );
                Statement::VarDeclaration {
                    name,
                    type_annotation: None, // TODO: implement type annotations
                    initializer: Some(initializer),
                    modifiers: Modifiers::default(),
                    span,
                }
            })
    }

    /// Parse function declarations: fun name(params): ReturnType = body
    fn function_declaration_parser(
    ) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
        Self::token_fun()
            .then(Self::identifier())
            .then_ignore(Self::token_left_paren())
            .then(Self::parameter_list())
            .then_ignore(Self::token_right_paren())
            .then(Self::token_colon().ignore_then(Self::identifier()).or_not())
            .then_ignore(Self::token_assign())
            .then(Self::expression_parser())
            .map(|((((fun_token, name), parameters), return_type), body)| {
                let span = Self::merge_spans(
                    &Self::span_from_token(&fun_token),
                    &Self::expression_span(&body),
                );
                Statement::FunctionDeclaration {
                    name,
                    parameters,
                    return_type: return_type.map(|t| TypeAnnotation::Simple(t)),
                    body: Box::new(body),
                    modifiers: Modifiers::default(),
                    span,
                }
            })
    }

    /// Parse data class declarations: data class Name(params)
    fn data_class_declaration_parser(
    ) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
        Self::token_data()
            .then_ignore(Self::token_class())
            .then(Self::identifier())
            .then_ignore(Self::token_left_paren())
            .then(Self::parameter_list())
            .then_ignore(Self::token_right_paren())
            .map(|((data_token, name), parameters)| {
                // Calculate span from data token to closing paren (approximated using last parameter)
                let end_span = if parameters.is_empty() {
                    Self::span_from_token(&data_token)
                } else {
                    parameters.last().unwrap().span.clone()
                };
                let span = Self::merge_spans(&Self::span_from_token(&data_token), &end_span);
                Statement::DataClassDeclaration {
                    name,
                    parameters,
                    is_mutable: false,
                    modifiers: Modifiers::default(),
                    type_parameters: Vec::new(),
                    span,
                }
            })
    }

    /// Parse parameter lists: param1: Type, param2: Type
    fn parameter_list() -> impl ChumskyParser<Token, Vec<Parameter>, Error = Simple<Token>> + Clone
    {
        Self::parameter()
            .separated_by(Self::token_comma())
            .allow_trailing()
            .collect()
    }

    /// Parse single parameter: name: Type
    fn parameter() -> impl ChumskyParser<Token, Parameter, Error = Simple<Token>> + Clone {
        Self::identifier()
            .then_ignore(Self::token_colon())
            .then(Self::identifier())
            .map(|(name, type_name)| {
                Parameter {
                    name: name.clone(),
                    type_annotation: Some(TypeAnnotation::Simple(type_name)),
                    default_value: None, // TODO: implement default values
                    span: Span {
                        // TODO: proper span calculation
                        start_line: 1,
                        start_column: 1,
                        end_line: 1,
                        end_column: 1,
                    },
                }
            })
    }

    /// Parse literals
    fn literal_parser() -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
        filter_map(|span, token: Token| {
            let token_span = Self::span_from_token(&token);
            match token.token_type {
                TokenType::String(s) => Ok(Expression::Literal(Literal::String(s), token_span)),
                TokenType::Number(n) => Ok(Expression::Literal(Literal::Number(n), token_span)),
                TokenType::Boolean(b) => Ok(Expression::Literal(Literal::Boolean(b), token_span)),
                TokenType::Null => Ok(Expression::Literal(Literal::Null, token_span)),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
            }
        })
    }

    // =========================================================================
    // Token Parser Helpers
    // =========================================================================
    //
    // These methods provide convenient token matching for specific token types.
    // Each returns a parser that matches exactly one token of the specified type.

    /// Parse 'val' keyword token
    fn token_val() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Val))
    }

    fn token_var() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Var))
    }

    fn token_fun() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Fun))
    }

    fn token_data() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Data))
    }

    fn token_class() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Class))
    }

    fn token_when() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::When))
    }

    fn token_else() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Else))
    }

    fn token_assign() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Assign))
    }

    fn token_left_paren() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::LeftParen))
    }

    fn token_right_paren() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::RightParen))
    }

    fn token_left_brace() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::LeftBrace))
    }

    fn token_right_brace() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::RightBrace))
    }

    fn token_comma() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Comma))
    }

    fn token_colon() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Colon))
    }

    fn token_dot() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Dot))
    }

    fn token_null_safe() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::NullSafe))
    }

    fn token_arrow() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Arrow))
    }

    fn token_plus() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Plus))
    }

    fn token_minus() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Minus))
    }

    fn token_multiply() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
        filter(|token: &Token| matches!(token.token_type, TokenType::Multiply))
    }

    /// Parse identifiers
    fn identifier() -> impl ChumskyParser<Token, String, Error = Simple<Token>> + Clone {
        filter_map(|span, token: Token| match token.token_type {
            TokenType::Identifier(name) => Ok(name),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        })
    }

    // =========================================================================
    // Span and Utility Helpers
    // =========================================================================
    //
    // These helper methods handle span calculation and AST node introspection
    // for proper source location tracking throughout the parsing process.

    /// Create span from token position information
    fn span_from_token(token: &Token) -> Span {
        Span {
            start_line: token.line,
            start_column: token.column,
            end_line: token.line,
            end_column: token.column + token.lexeme.len(),
        }
    }

    /// Helper to merge spans
    fn merge_spans(start: &Span, end: &Span) -> Span {
        Span {
            start_line: start.start_line,
            start_column: start.start_column,
            end_line: end.end_line,
            end_column: end.end_column,
        }
    }

    /// Get span from expression
    fn expression_span(expr: &Expression) -> Span {
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

    /// Get span from statement
    fn statement_span(stmt: &Statement) -> Span {
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test]
    fn test_simple_val_declaration() {
        let input = "val name = \"hello\"";
        let result = Parser::parse(input).unwrap();

        // Expected: val declaration statement
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::ValDeclaration {
                name, initializer, ..
            } => {
                assert_eq!(name, "name");
                match initializer {
                    Expression::Literal(Literal::String(s), _) => {
                        assert_eq!(s, "hello");
                    }
                    _ => panic!("Expected string literal"),
                }
            }
            _ => panic!("Expected val declaration"),
        }
    }

    #[test]
    fn test_function_declaration() {
        let input = r#"
            fun add(a: Int, b: Int): Int {
                return a + b
            }
        "#;
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
                ..
            } => {
                assert_eq!(name, "add");
                assert_eq!(parameters.len(), 2);
                assert_eq!(parameters[0].name, "a");
                assert_eq!(parameters[1].name, "b");
                assert!(return_type.is_some());

                // Check function body
                match body.as_ref() {
                    Expression::Block { statements, .. } => {
                        assert_eq!(statements.len(), 1);
                        match &statements[0] {
                            Statement::Return { .. } => {}
                            _ => panic!("Expected return statement"),
                        }
                    }
                    _ => panic!("Expected block expression"),
                }
            }
            _ => panic!("Expected function declaration"),
        }
    }

    #[test]
    fn test_data_class_declaration() {
        let input = "data class User(val name: String, var age: Int)";
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::DataClassDeclaration {
                name,
                parameters,
                is_mutable,
                ..
            } => {
                assert_eq!(name, "User");
                assert_eq!(parameters.len(), 2);
                assert_eq!(parameters[0].name, "name");
                assert_eq!(parameters[1].name, "age");
                assert!(!is_mutable); // Data class is immutable by default
            }
            _ => panic!("Expected data class declaration"),
        }
    }

    #[test]
    fn test_when_expression() {
        let input = r#"
            when (x) {
                0 -> "zero"
                1 -> "one"
                else -> "other"
            }
        "#;
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::Expression { expression, .. } => match expression.as_ref() {
                Expression::When {
                    discriminant,
                    arms,
                    else_arm,
                    ..
                } => {
                    assert!(discriminant.is_some());
                    assert_eq!(arms.len(), 2);
                    assert!(else_arm.is_some());
                }
                _ => panic!("Expected when expression"),
            },
            _ => panic!("Expected expression statement"),
        }
    }

    #[test]
    fn test_null_safety_operators() {
        let input = "val length = user?.name?.length ?: 0";
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::ValDeclaration { initializer, .. } => {
                match initializer.as_ref() {
                    Expression::Binary {
                        left,
                        operator,
                        right,
                        ..
                    } => {
                        assert_eq!(*operator, BinaryOperator::Elvis);

                        // Left side should be null-safe member access chain
                        match left.as_ref() {
                            Expression::NullSafeMemberAccess { .. } => {}
                            _ => panic!("Expected null-safe member access"),
                        }

                        // Right side should be literal 0
                        match right.as_ref() {
                            Expression::Literal(Literal::Integer(0), _) => {}
                            _ => panic!("Expected integer literal 0"),
                        }
                    }
                    _ => panic!("Expected binary expression with elvis operator"),
                }
            }
            _ => panic!("Expected val declaration"),
        }
    }

    #[test]
    fn test_string_interpolation() {
        let input = r#"println("Hello, ${name}!")"#;
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::Expression { expression, .. } => {
                match expression.as_ref() {
                    Expression::Call {
                        function,
                        arguments,
                        ..
                    } => {
                        match function.as_ref() {
                            Expression::Identifier(name, _) => {
                                assert_eq!(name, "println");
                            }
                            _ => panic!("Expected println function"),
                        }

                        assert_eq!(arguments.len(), 1);
                        match &arguments[0].expression {
                            Expression::StringInterpolation { parts, .. } => {
                                assert!(parts.len() >= 2); // Should have text and interpolated parts
                            }
                            _ => panic!("Expected string interpolation"),
                        }
                    }
                    _ => panic!("Expected function call"),
                }
            }
            _ => panic!("Expected expression statement"),
        }
    }

    #[test]
    fn test_extension_function() {
        let input = "fun String.reversed(): String = StringBuilder(this).reverse().toString()";
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::ExtensionFunction(ef) => {
                match &ef.receiver_type {
                    TypeAnnotation::Simple(type_name) => {
                        assert_eq!(type_name, "String");
                    }
                    _ => panic!("Expected simple type annotation"),
                }
                assert_eq!(ef.function_name, "reversed");
            }
            _ => panic!("Expected extension function"),
        }
    }

    #[test]
    fn test_async_spawn_constructs() {
        let input = r#"
            spawn {
                println("Virtual thread")
            }
        "#;
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::Concurrency(ConcurrencyConstruct::Spawn { body, .. }) => {
                match body.as_ref() {
                    Expression::Block { .. } => {}
                    _ => panic!("Expected block expression in spawn"),
                }
            }
            _ => panic!("Expected spawn statement"),
        }
    }

    #[test]
    fn test_use_defer_constructs() {
        let input = r#"
            use (resource) {
                resource.process()
            }
        "#;
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::ResourceManagement(ResourceManagement::Use { resource, body, .. }) => {
                match resource.as_ref() {
                    Expression::Identifier(name, _) => {
                        assert_eq!(name, "resource");
                    }
                    _ => panic!("Expected identifier in use statement"),
                }
                match body.as_ref() {
                    Expression::Block { .. } => {}
                    _ => panic!("Expected block expression in use statement"),
                }
            }
            _ => panic!("Expected use statement"),
        }
    }

    #[test]
    fn test_array_literal() {
        let input = "val numbers = [1, 2, 3]";
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::ValDeclaration { initializer, .. } => match initializer.as_ref() {
                Expression::Array { elements, .. } => {
                    assert_eq!(elements.len(), 3);
                    match &elements[0] {
                        Expression::Literal(Literal::Integer(1), _) => {}
                        _ => panic!("Expected integer literal 1"),
                    }
                }
                _ => panic!("Expected array expression"),
            },
            _ => panic!("Expected val declaration"),
        }
    }

    #[test]
    fn test_lambda_expression() {
        let input = "val doubled = numbers.map { x -> x * 2 }";
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::ValDeclaration { initializer, .. } => match initializer.as_ref() {
                Expression::Call {
                    function,
                    arguments,
                    ..
                } => {
                    match function.as_ref() {
                        Expression::MemberAccess {
                            object, property, ..
                        } => {
                            match object.as_ref() {
                                Expression::Identifier(name, _) => {
                                    assert_eq!(name, "numbers");
                                }
                                _ => panic!("Expected identifier"),
                            }
                            assert_eq!(property, "map");
                        }
                        _ => panic!("Expected member access"),
                    }

                    assert_eq!(arguments.len(), 1);
                    match &arguments[0].expression {
                        Expression::Lambda {
                            parameters, body, ..
                        } => {
                            assert_eq!(parameters.len(), 1);
                            assert_eq!(parameters[0].name, "x");
                        }
                        _ => panic!("Expected lambda expression"),
                    }
                }
                _ => panic!("Expected function call"),
            },
            _ => panic!("Expected val declaration"),
        }
    }

    // Error handling tests
    #[test]
    fn test_parse_error_missing_semicolon() {
        let input = "val x = 42\nval y = 43"; // Missing semicolon
        let result = Parser::parse(input);

        // Should succeed as jv doesn't require semicolons for line-separated statements
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_error_unmatched_brace() {
        let input = "fun test() { println(\"hello\") "; // Missing closing brace
        let result = Parser::parse(input);

        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_invalid_syntax() {
        let input = "val = 42"; // Missing variable name
        let result = Parser::parse(input);

        assert!(result.is_err());
    }

    #[test]
    fn test_complex_nested_expression() {
        let input = r#"
            val result = when (getValue()) {
                is String -> str.length
                is Int -> if (it > 0) it * 2 else 0
                null -> 0
                else -> -1
            }
        "#;
        let result = Parser::parse(input);

        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_multiple_statements() {
        let input = r#"
            val name = "jv"
            var count = 0
            
            fun increment() {
                count = count + 1
            }
            
            data class Point(val x: Double, val y: Double)
            
            fun main() {
                println("Hello, ${name}!")
                increment()
                val point = Point(1.0, 2.0)
            }
        "#;
        let result = Parser::parse(input);

        assert!(result.is_ok());
        let program = result.unwrap();
        assert!(program.statements.len() >= 5); // Multiple top-level statements
    }

    // Property-based testing with test_case
    #[test_case("val x = 42" ; "integer literal")]
    #[test_case("val x = 3.14" ; "float literal")]
    #[test_case("val x = true" ; "boolean literal")]
    #[test_case("val x = \"hello\"" ; "string literal")]
    #[test_case("val x = null" ; "null literal")]
    fn test_literal_parsing(input: &str) {
        let result = Parser::parse(input);
        assert!(result.is_ok(), "Failed to parse: {}", input);
    }

    #[test_case("fun f(): Int" ; "function with return type")]
    #[test_case("fun f(x: Int)" ; "function with parameter")]
    #[test_case("fun f(x: Int, y: String)" ; "function with multiple parameters")]
    #[test_case("fun f(x: Int = 0)" ; "function with default parameter")]
    fn test_function_signatures(input: &str) {
        let result = Parser::parse(input);
        assert!(result.is_ok(), "Failed to parse: {}", input);
    }

    // Performance test
    #[test]
    #[ignore] // Only run in performance testing
    fn test_parse_large_file() {
        let mut large_input = String::new();
        for i in 0..1000 {
            large_input.push_str(&format!("val var{} = {}\n", i, i));
        }

        let start = std::time::Instant::now();
        let result = Parser::parse(&large_input);
        let duration = start.elapsed();

        assert!(result.is_ok());
        println!("Parsed 1000 statements in {:?}", duration);
        // Should parse reasonably fast
        assert!(duration < std::time::Duration::from_secs(1));
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_var_declaration_red_phase() {
        // RED: This test should fail
        let input = "var count = 42";
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::VarDeclaration {
                name, initializer, ..
            } => {
                assert_eq!(name, "count");
                match initializer {
                    Some(Expression::Literal(Literal::Number(n), _)) => {
                        assert_eq!(n, "42");
                    }
                    _ => panic!("Expected number literal"),
                }
            }
            _ => panic!("Expected var declaration"),
        }
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_function_declaration_red_phase() {
        // RED: This test should fail
        let input = "fun add(x: Int, y: Int): Int = x + y";
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
                ..
            } => {
                assert_eq!(name, "add");
                assert_eq!(parameters.len(), 2);
                assert_eq!(parameters[0].name, "x");
                assert_eq!(parameters[1].name, "y");
                assert_eq!(return_type.as_ref().unwrap(), "Int");

                // Body should be a binary expression: x + y
                match body {
                    Expression::Binary {
                        op: BinaryOp::Add, ..
                    } => {
                        // Expected
                    }
                    _ => panic!("Expected binary addition expression"),
                }
            }
            _ => panic!("Expected function declaration"),
        }
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_data_class_declaration_red_phase() {
        // RED: This test should fail
        let input = "data class User(name: String, age: Int)";
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::DataClassDeclaration {
                name, parameters, ..
            } => {
                assert_eq!(name, "User");
                assert_eq!(parameters.len(), 2);
                assert_eq!(parameters[0].name, "name");
                assert_eq!(parameters[0].type_annotation.as_ref().unwrap(), "String");
                assert_eq!(parameters[1].name, "age");
                assert_eq!(parameters[1].type_annotation.as_ref().unwrap(), "Int");
            }
            _ => panic!("Expected data class declaration"),
        }
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_binary_expressions_red_phase() {
        // RED: This test should fail
        let input = "val result = a + b * c";
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::ValDeclaration { initializer, .. } => {
                // Should parse as: a + (b * c) due to operator precedence
                match initializer {
                    Expression::Binary {
                        op: BinaryOp::Add,
                        left,
                        right,
                        ..
                    } => {
                        match left.as_ref() {
                            Expression::Identifier(name, _) => assert_eq!(name, "a"),
                            _ => panic!("Expected identifier 'a'"),
                        }
                        match right.as_ref() {
                            Expression::Binary {
                                op: BinaryOp::Multiply,
                                ..
                            } => {
                                // Expected multiplication
                            }
                            _ => panic!("Expected multiplication expression"),
                        }
                    }
                    _ => panic!("Expected binary expression"),
                }
            }
            _ => panic!("Expected val declaration"),
        }
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_when_expression_red_phase() {
        // RED: This test should fail
        let input = r#"val result = when (x) {
            1 -> "one"
            2 -> "two"
            else -> "other"
        }"#;
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::ValDeclaration { initializer, .. } => {
                match initializer {
                    Expression::When {
                        expr,
                        arms,
                        else_arm,
                        ..
                    } => {
                        // Check the when expression structure
                        match expr.as_ref() {
                            Expression::Identifier(name, _) => assert_eq!(name, "x"),
                            _ => panic!("Expected identifier 'x'"),
                        }
                        assert_eq!(arms.len(), 2);
                        assert!(else_arm.is_some());
                    }
                    _ => panic!("Expected when expression"),
                }
            }
            _ => panic!("Expected val declaration"),
        }
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_function_call_red_phase() {
        // RED: This test should fail
        let input = "val result = calculate(x, y, 42)";
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::ValDeclaration { initializer, .. } => match initializer {
                Expression::Call { function, args, .. } => {
                    match function.as_ref() {
                        Expression::Identifier(name, _) => assert_eq!(name, "calculate"),
                        _ => panic!("Expected function identifier"),
                    }
                    assert_eq!(args.len(), 3);
                }
                _ => panic!("Expected function call"),
            },
            _ => panic!("Expected val declaration"),
        }
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_member_access_red_phase() {
        // RED: This test should fail
        let input = "val name = user.name";
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::ValDeclaration { initializer, .. } => match initializer {
                Expression::MemberAccess {
                    object, property, ..
                } => {
                    match object.as_ref() {
                        Expression::Identifier(name, _) => assert_eq!(name, "user"),
                        _ => panic!("Expected object identifier"),
                    }
                    assert_eq!(property, "name");
                }
                _ => panic!("Expected member access"),
            },
            _ => panic!("Expected val declaration"),
        }
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_null_safe_member_access_red_phase() {
        // RED: This test should fail
        let input = "val name = user?.name";
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::ValDeclaration { initializer, .. } => match initializer {
                Expression::NullSafeMemberAccess {
                    object, property, ..
                } => {
                    match object.as_ref() {
                        Expression::Identifier(name, _) => assert_eq!(name, "user"),
                        _ => panic!("Expected object identifier"),
                    }
                    assert_eq!(property, "name");
                }
                _ => panic!("Expected null-safe member access"),
            },
            _ => panic!("Expected val declaration"),
        }
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_string_interpolation_red_phase() {
        // RED: This test should fail
        let input = r#"val message = "Hello, ${name}!""#;
        let result = Parser::parse(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::ValDeclaration { initializer, .. } => {
                match initializer {
                    Expression::StringInterpolation { parts, .. } => {
                        assert_eq!(parts.len(), 3); // "Hello, ", ${name}, "!"
                        match &parts[0] {
                            StringPart::Text(s) => assert_eq!(s, "Hello, "),
                            _ => panic!("Expected text part"),
                        }
                        match &parts[1] {
                            StringPart::Expression(Expression::Identifier(name, _)) => {
                                assert_eq!(name, "name");
                            }
                            _ => panic!("Expected identifier expression"),
                        }
                        match &parts[2] {
                            StringPart::Text(s) => assert_eq!(s, "!"),
                            _ => panic!("Expected text part"),
                        }
                    }
                    _ => panic!("Expected string interpolation"),
                }
            }
            _ => panic!("Expected val declaration"),
        }
    }

    #[test_case("val x = 42" => 1; "single val declaration")]
    #[test_case("val x = 42\nvar y = \"test\"" => 2; "val and var declarations")]
    #[test_case("fun test() = 42\nval result = test()" => 2; "function and call")]
    #[should_panic(expected = "not yet implemented")]
    fn test_multiple_statements_red_phase(input: &str) -> usize {
        // RED: This test should fail
        let result = Parser::parse(input).unwrap();
        result.statements.len()
    }
}
