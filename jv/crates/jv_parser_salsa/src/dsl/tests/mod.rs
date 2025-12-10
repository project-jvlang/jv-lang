use crate::lexer::Lexer;
use crate::lower::{LoweringResult, lower};
use crate::parser::parse;

fn lower_source(source: &str) -> LoweringResult {
    let lexer = Lexer::new(source).expect("lexing should succeed");
    let tokens = lexer.collect_owned_tokens();
    let parse_result = parse(tokens);
    lower(source, &parse_result)
}

mod blocks;
mod compat;
mod errors;
mod verification;
