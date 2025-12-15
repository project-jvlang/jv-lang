use crate::lexer::Lexer;
use crate::lower::lower;
use crate::parser::parse;
use jv_ast::expression::Expression;
use jv_ast::types::Literal;

fn lower_source(source: &str) -> Vec<Expression> {
    let lexer = Lexer::new(source).expect("lexing should succeed");
    let tokens = lexer.collect_owned_tokens();
    let parse_result = parse(tokens);
    let lowering = lower(source, &parse_result);
    lowering
        .statements
        .into_iter()
        .filter_map(|stmt| match stmt {
            jv_ast::Statement::Expression { expr, .. } => Some(expr),
            jv_ast::Statement::ValDeclaration { initializer, .. } => Some(initializer),
            _ => None,
        })
        .collect()
}

#[test]
fn lowers_binary_expression_with_precedence() {
    let exprs = lower_source("val x = 1 + 2 * 3;");
    let expr = exprs.first().expect("should lower expression");
    match expr {
        Expression::Binary {
            op, left, right, ..
        } => {
            assert_eq!(*op, jv_ast::types::BinaryOp::Add);
            assert!(
                matches!(left.as_ref(), Expression::Literal(Literal::Number(n), _) if n == "1")
            );
            match right.as_ref() {
                Expression::Binary { op: inner_op, .. } => {
                    assert_eq!(*inner_op, jv_ast::types::BinaryOp::Multiply);
                }
                other => panic!("expected multiply on RHS, got {:?}", other),
            }
        }
        other => panic!("expected binary expression, got {:?}", other),
    }
}

#[test]
fn lowers_call_and_member_access() {
    let exprs = lower_source("foo.bar(1 2)");
    let expr = exprs.first().expect("should lower expression");
    match expr {
        Expression::Call { function, args, .. } => {
            match function.as_ref() {
                Expression::MemberAccess { property, .. } => assert_eq!(property, "bar"),
                other => panic!("expected member access, got {:?}", other),
            }
            assert_eq!(args.len(), 2);
        }
        other => panic!("expected call expression, got {:?}", other),
    }
}
