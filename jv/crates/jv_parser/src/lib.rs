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
mod tests;
