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

//! 延期構文の最新状況は `docs/deferred-syntax.md` を参照してください。
//! The up-to-date deferred syntax inventory lives in `docs/deferred-syntax.md`.

use chumsky::error::{Simple, SimpleReason};
use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::*;
use jv_lexer::{LexError, Token, TokenType};
use thiserror::Error;

mod preprocess;
mod syntax;

use crate::syntax::support::{merge_spans, span_from_token};

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Lexical error: {0}")]
    LexError(#[from] LexError),
    #[error("Parse error at {span:?}: {message}")]
    Syntax { message: String, span: Span },
    #[error("Unexpected end of input at {span:?}")]
    UnexpectedEof { span: Span },
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
        let mut lexer = jv_lexer::Lexer::new(input.to_string());
        let preprocess_result = preprocess::run(lexer.tokenize()?);
        let (tokens, diagnostics, halted_stage) = preprocess_result.into_parts();

        if let Some(stage_name) = halted_stage {
            if let Some(diagnostic) = diagnostics.first() {
                let span = diagnostic.span().cloned().unwrap_or_else(Span::dummy);
                let message = format!("[{}] {}", stage_name, diagnostic.message());
                return Err(ParseError::Syntax { message, span });
            } else {
                return Err(ParseError::Syntax {
                    message: format!("Stage 0 preprocessing halted at {}", stage_name),
                    span: Span::dummy(),
                });
            }
        }

        let parser_tokens = tokens.clone();

        let parser = Self::program_parser();

        match parser.parse(parser_tokens) {
            Ok(program) => Ok(program),
            Err(errors) => {
                let format_simple = |error: &Simple<Token>| match error.reason() {
                    SimpleReason::Custom(message) => message.clone(),
                    _ => format!("{:?}", error),
                };

                let mut errors_iter = errors.into_iter();

                if let Some(error) = errors_iter.next() {
                    let error_span = simple_error_span(&error, &tokens);
                    let mut message = format_simple(&error);
                    for extra in errors_iter {
                        message.push_str("; ");
                        message.push_str(&format_simple(&extra));
                    }

                    let is_custom = matches!(error.reason(), SimpleReason::Custom(_));
                    let is_eof = match error.found() {
                        None => !is_custom,
                        Some(token) => matches!(&token.token_type, TokenType::Eof),
                    };

                    if is_eof {
                        Err(ParseError::UnexpectedEof { span: error_span })
                    } else {
                        Err(ParseError::Syntax {
                            message,
                            span: error_span,
                        })
                    }
                } else {
                    let span = tokens
                        .last()
                        .map(|token| span_from_token(token))
                        .unwrap_or_else(Span::dummy);
                    Err(ParseError::UnexpectedEof { span })
                }
            }
        }
    }

    /// A program consists of zero or more statements followed by EOF.
    fn program_parser() -> impl ChumskyParser<Token, Program, Error = Simple<Token>> + Clone {
        syntax::statement_parser()
            .repeated()
            .then_ignore(filter(|token: &Token| {
                matches!(token.token_type, TokenType::Eof)
            }))
            .map(|statements| {
                let span = if statements.is_empty() {
                    Span {
                        start_line: 1,
                        start_column: 1,
                        end_line: 1,
                        end_column: 1,
                    }
                } else {
                    let first_span = syntax::statement_span(&statements[0]);
                    let last_span = syntax::statement_span(statements.last().unwrap());
                    syntax::merge_spans(&first_span, &last_span)
                };

                let package = statements.iter().find_map(|statement| match statement {
                    Statement::Package { name, .. } => Some(name.clone()),
                    _ => None,
                });

                let imports = statements
                    .iter()
                    .filter_map(|statement| match statement {
                        Statement::Import { .. } => Some(statement.clone()),
                        _ => None,
                    })
                    .collect();

                Program {
                    package,
                    imports,
                    statements,
                    span,
                }
            })
    }
}

impl ParseError {
    pub fn span(&self) -> Option<&Span> {
        match self {
            ParseError::LexError(_) => None,
            ParseError::Syntax { span, .. } => Some(span),
            ParseError::UnexpectedEof { span } => Some(span),
        }
    }
}

fn simple_error_span(error: &Simple<Token>, tokens: &[Token]) -> Span {
    if tokens.is_empty() {
        return Span::dummy();
    }

    let range = error.span();
    let tokens_len = tokens.len();

    let start_index = range.start.min(tokens_len - 1);
    let raw_end = if range.end == range.start {
        range.start
    } else {
        range.end.saturating_sub(1)
    };
    let end_index = raw_end.min(tokens_len - 1).max(start_index);

    let start_token = tokens
        .get(start_index)
        .unwrap_or_else(|| tokens.last().unwrap());
    let end_token = tokens
        .get(end_index)
        .unwrap_or_else(|| tokens.last().unwrap());

    let start_span = span_from_token(start_token);
    let end_span = span_from_token(end_token);

    merge_spans(&start_span, &end_span)
}

#[cfg(test)]
mod tests;
