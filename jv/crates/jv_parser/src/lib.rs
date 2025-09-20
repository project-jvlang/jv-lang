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

mod syntax;

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
        let mut lexer = jv_lexer::Lexer::new(input.to_string());
        let tokens = lexer.tokenize()?;

        let parser = Self::program_parser();

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

                Program {
                    statements,
                    package: None,
                    imports: Vec::new(),
                    span,
                }
            })
    }
}

#[cfg(test)]
mod tests;
