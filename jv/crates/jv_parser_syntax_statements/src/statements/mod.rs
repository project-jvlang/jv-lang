use chumsky::error::Simple;
use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::Statement;
use jv_lexer::Token;

use jv_parser_syntax_expressions::expression_parser;
use jv_parser_syntax_support::expression_level_block_parser;

mod control;
mod declarations;
mod signatures;

pub fn statement_parser() -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    recursive(|statement| {
        let expr = expression_parser(
            expression_level_block_parser(statement.clone()),
            statement.clone(),
        );

        let comment_stmt = declarations::comment_statement_parser();
        let package_stmt = attempt_statement_parser(declarations::package_declaration_parser());
        let import_stmt = attempt_statement_parser(declarations::import_statement_parser());
        let val_decl = attempt_statement_parser(declarations::val_declaration_parser(expr.clone()));
        let implicit_typed_val_decl = attempt_statement_parser(
            declarations::implicit_typed_val_declaration_parser(expr.clone()),
        );
        let var_decl = attempt_statement_parser(declarations::var_declaration_parser(expr.clone()));
        let assignment = control::assignment_statement_parser(expr.clone());
        let function_decl =
            declarations::function_declaration_parser(statement.clone(), expr.clone());
        let class_decl = attempt_statement_parser(declarations::class_declaration_parser(
            statement.clone(),
            expr.clone(),
        ));
        let data_class_decl = declarations::data_class_declaration_parser(expr.clone());
        let use_stmt = control::use_statement_parser(statement.clone(), expr.clone());
        let defer_stmt = control::defer_statement_parser(statement.clone());
        let spawn_stmt = control::spawn_statement_parser(statement.clone());
        let return_stmt = control::return_statement_parser(expr.clone());
        let throw_stmt = control::throw_statement_parser(expr.clone());
        let for_in_stmt = control::for_in_statement_parser(statement.clone(), expr.clone());
        let legacy_loop = control::legacy_loop_parser();
        let expression_stmt = control::expression_statement_parser(expr);

        choice((
            comment_stmt,
            package_stmt,
            import_stmt,
            val_decl,
            implicit_typed_val_decl,
            var_decl,
            assignment,
            function_decl,
            class_decl,
            data_class_decl,
            use_stmt,
            defer_stmt,
            spawn_stmt,
            for_in_stmt,
            return_stmt,
            throw_stmt,
            legacy_loop,
            expression_stmt,
        ))
        .boxed()
    })
}

fn attempt_statement_parser<P>(
    parser: P,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone
where
    P: ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
{
    parser
}
