use crate::{ParseError, Parser};
use jv_ast::{Program, Statement};

/// Convenience helpers shared across parser tests.
pub(crate) mod helpers {
    use super::*;

    /// Parse the provided source code and return the resulting AST program.
    /// Panics with a descriptive message when parsing fails.
    pub(crate) fn parse_program(input: &str) -> Program {
        Parser::parse(input).unwrap_or_else(|err| panic!("failed to parse `{}`: {:?}", input, err))
    }

    /// Parse the provided source code and return the raw parser result.
    /// Useful for negative tests that expect an error.
    pub(crate) fn parse_program_result(input: &str) -> Result<Program, ParseError> {
        Parser::parse(input)
    }

    /// Borrow the first statement from a program, panicking if none exist.
    pub(crate) fn first_statement(program: &Program) -> &Statement {
        program
            .statements
            .first()
            .expect("expected at least one top-level statement")
    }
}

pub(crate) use helpers::{first_statement, parse_program, parse_program_result};
