// jv_parser - Facade crate for jv language parser
//
// This crate re-exports all parser functionality from specialized subcrates:
// - jv_parser_syntax: Syntax parsing combinators
// - jv_parser_preprocess: Token preprocessing pipeline
// - jv_parser_semantics: Semantic analysis passes
// - jv_parser_frontend: Frontend API and diagnostics
//
// The subcrate split reduces memory usage during compilation by breaking up
// large monomorphized parser combinator types across multiple compilation units.

//! 延期構文の最新状況は \`docs/deferred-syntax.md\` を参照してください。
//! The up-to-date deferred syntax inventory lives in \`docs/deferred-syntax.md\`.

use chumsky::error::{Simple, SimpleReason};
use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::*;
use jv_lexer::{LexError, Token, TokenType};
use thiserror::Error;

// Re-export subcrate modules
pub use jv_parser_frontend as frontend;
pub use jv_parser_preprocess as preprocess;
pub use jv_parser_semantics as semantics;
pub use jv_parser_syntax as syntax;

// Re-export commonly used types
pub use jv_parser_frontend::{
    DiagnosticContext, DiagnosticFormatter, FrontendDiagnostics, FrontendOutput,
};
pub use jv_parser_preprocess::PreprocessResult;
pub use jv_parser_semantics::SemanticsResult;
pub use jv_parser_syntax::statement_parser;

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
    pub fn parse(input: &str) -> Result<FrontendOutput, ParseError> {
        let mut lexer = jv_lexer::Lexer::new(input.to_string());
        let preprocess_result = jv_parser_preprocess::run(lexer.tokenize()?);
        let (tokens, preprocess_diagnostics, preprocess_halted_stage) =
            preprocess_result.into_parts();

        if let Some(stage_name) = preprocess_halted_stage {
            if let Some(diagnostic) = preprocess_diagnostics.first() {
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
            Ok(program) => {
                let semantics_result = jv_parser_semantics::run(&tokens, program);

                if let Some(stage_name) = semantics_result.halted_stage {
                    if let Some(diagnostic) = semantics_result.staged_diagnostics.first() {
                        let span = diagnostic.span().cloned().unwrap_or_else(Span::dummy);
                        let message = format!("[{}] {}", stage_name, diagnostic.message());
                        return Err(ParseError::Syntax { message, span });
                    } else {
                        return Err(ParseError::Syntax {
                            message: format!("Stage 2 semantics halted at {}", stage_name),
                            span: Span::dummy(),
                        });
                    }
                }

                let semantics_halted_stage = semantics_result.halted_stage;
                let semantics_diagnostics = semantics_result.staged_diagnostics;
                let program = semantics_result.program;

                let formatter = DiagnosticFormatter::default();
                let context = DiagnosticContext::new(
                    &tokens,
                    &[],
                    &preprocess_diagnostics,
                    preprocess_halted_stage,
                    &semantics_diagnostics,
                    semantics_halted_stage,
                );
                let final_diagnostics = formatter.format(context);

                let diagnostics = FrontendDiagnostics::new(
                    final_diagnostics,
                    preprocess_diagnostics,
                    preprocess_halted_stage,
                    semantics_diagnostics,
                    semantics_halted_stage,
                );

                Ok(FrontendOutput::new(program, tokens, diagnostics))
            }
            Err(errors) => {
                use jv_parser_syntax::support::{merge_spans, span_from_token};

                let format_simple = |error: &Simple<Token>| match error.reason() {
                    SimpleReason::Custom(message) => message.clone(),
                    _ => format!("{:?}", error),
                };

                let simple_error_span = |error: &Simple<Token>, tokens: &[Token]| -> Span {
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
                    Err(ParseError::Syntax {
                        message: "Unknown parse error".to_string(),
                        span: Span::dummy(),
                    })
                }
            }
        }
    }

    fn program_parser() -> impl ChumskyParser<Token, Program, Error = Simple<Token>> + Clone {
        use jv_parser_syntax::statement_parser;

        statement_parser()
            .repeated()
            .map(|statements| Program { statements })
            .then_ignore(end())
    }
}

impl ParseError {
    pub fn span(&self) -> &Span {
        match self {
            ParseError::Syntax { span, .. } | ParseError::UnexpectedEof { span } => span,
            ParseError::LexError(_) => &Span::dummy(),
        }
    }
}

#[cfg(test)]
mod tests;
