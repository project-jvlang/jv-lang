use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{
    Annotation, AnnotationArgument, AnnotationName, AnnotationValue, BinaryOp, CommentKind,
    CommentStatement, CommentVisibility, ConcurrencyConstruct, Expression, ExtensionFunction,
    ForInStatement, Literal, LoopBinding, LoopStrategy, Modifiers, NumericRangeLoop, Parameter,
    ResourceManagement, Span, Statement, TypeAnnotation, Visibility,
};
use jv_lexer::{Token, TokenType};

use super::expressions;
use super::parameters::parameter_list;
use super::support::{
    expression_span, identifier, identifier_with_span, keyword as support_keyword, merge_spans,
    span_from_token, statement_span, token_any_comma, token_assign, token_at, token_class,
    token_colon, token_data, token_defer, token_do_keyword, token_dot, token_for, token_fun,
    token_in_keyword, token_left_brace, token_left_paren, token_return, token_right_brace,
    token_right_paren, token_spawn, token_use, token_val, token_var, token_while_keyword,
    type_annotation_simple,
};

pub(crate) fn statement_parser(
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    recursive(|statement| {
        let expr = expressions::expression_parser();

        let comment_stmt = comment_statement_parser();
        let val_decl = val_declaration_parser(expr.clone());
        let var_decl = var_declaration_parser(expr.clone());
        let assignment = assignment_statement_parser(expr.clone());
        let function_decl = function_declaration_parser(statement.clone(), expr.clone());
        let data_class_decl = data_class_declaration_parser(expr.clone());
        let use_stmt = use_statement_parser(statement.clone(), expr.clone());
        let defer_stmt = defer_statement_parser(statement.clone());
        let spawn_stmt = spawn_statement_parser(statement.clone());
        let return_stmt = return_statement_parser(expr.clone());
        let for_in_stmt = for_in_statement_parser(statement.clone(), expr.clone());
        let legacy_loop = legacy_loop_parser();
        let expression_stmt = expression_statement_parser(expr);

        choice((
            comment_stmt,
            val_decl,
            var_decl,
            assignment,
            function_decl,
            data_class_decl,
            use_stmt,
            defer_stmt,
            spawn_stmt,
            for_in_stmt,
            return_stmt,
            legacy_loop,
            expression_stmt,
        ))
        .boxed()
    })
}

fn comment_statement_parser() -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone
{
    filter_map(|span, token: Token| {
        let token_span = span_from_token(&token);
        match token.token_type.clone() {
            TokenType::LineComment(text) => {
                let visibility = if is_jv_only_line_comment(&text) {
                    CommentVisibility::JvOnly
                } else {
                    CommentVisibility::Passthrough
                };
                let rendered = format!("/{}", text);
                Ok(Statement::Comment(CommentStatement {
                    kind: CommentKind::Line,
                    visibility,
                    text: rendered,
                    span: token_span,
                }))
            }
            TokenType::BlockComment(text) => Ok(Statement::Comment(CommentStatement {
                kind: CommentKind::Block,
                visibility: CommentVisibility::Passthrough,
                text: format!("/*{}*/", text),
                span: token_span,
            })),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        }
    })
}

fn is_jv_only_line_comment(raw: &str) -> bool {
    let mut chars = raw.chars();
    let first = chars.next();
    let second = chars.next();
    matches!(
        (first, second),
        (Some('/'), Some('/')) | (Some('/'), Some('*'))
    )
}

fn val_declaration_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    modifiers_parser()
        .then_ignore(token_val())
        .then(identifier())
        .then(type_annotation_clause())
        .then_ignore(token_assign())
        .then(expr)
        .map(|(((modifiers, name), type_annotation), initializer)| {
            let span = expression_span(&initializer);
            Statement::ValDeclaration {
                name,
                type_annotation,
                initializer,
                modifiers,
                span,
            }
        })
}

fn var_declaration_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    modifiers_parser()
        .then_ignore(token_var())
        .then(identifier())
        .then(type_annotation_clause())
        .then(token_assign().ignore_then(expr).or_not())
        .map(|(((modifiers, name), type_annotation), initializer)| {
            let span = initializer
                .as_ref()
                .map(expression_span)
                .unwrap_or_else(Span::dummy);
            Statement::VarDeclaration {
                name,
                type_annotation,
                initializer,
                modifiers,
                span,
            }
        })
}

fn assignment_statement_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    assignment_target_parser()
        .then_ignore(token_assign())
        .then(expr)
        .map(|(target, value)| {
            let span = merge_spans(&expression_span(&target), &expression_span(&value));
            Statement::Assignment {
                target,
                value,
                span,
            }
        })
}

fn function_declaration_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    modifiers_parser()
        .then_ignore(token_fun())
        .then(function_signature(expr.clone()))
        .then(function_body(statement, expr).or_not())
        .map(|((modifiers, signature), body)| {
            let body_expr = body.unwrap_or_else(|| Expression::Block {
                statements: Vec::new(),
                span: Span::dummy(),
            });
            let span = expression_span(&body_expr);

            if let Some(receiver) = signature.receiver_type {
                let inner_body = body_expr.clone();
                let inner = Statement::FunctionDeclaration {
                    name: signature.name,
                    parameters: signature.parameters,
                    return_type: signature.return_type,
                    body: Box::new(inner_body),
                    modifiers: modifiers.clone(),
                    span: span.clone(),
                };

                Statement::ExtensionFunction(ExtensionFunction {
                    receiver_type: receiver,
                    function: Box::new(inner),
                    span,
                })
            } else {
                Statement::FunctionDeclaration {
                    name: signature.name,
                    parameters: signature.parameters,
                    return_type: signature.return_type,
                    body: Box::new(body_expr),
                    modifiers,
                    span,
                }
            }
        })
}

fn data_class_declaration_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    modifiers_parser()
        .then_ignore(token_data())
        .then_ignore(token_class())
        .then(identifier())
        .then_ignore(token_left_paren())
        .then(parameter_list(expr))
        .then_ignore(token_right_paren())
        .map(
            |((modifiers, name), parameters)| Statement::DataClassDeclaration {
                name,
                parameters,
                is_mutable: false,
                modifiers,
                type_parameters: Vec::new(),
                span: Span::dummy(),
            },
        )
}

fn use_statement_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_use()
        .ignore_then(token_left_paren())
        .ignore_then(expr)
        .then_ignore(token_right_paren())
        .then(block_expression_parser(statement.clone()))
        .map(|(resource, body)| {
            let span = merge_spans(&expression_span(&resource), &expression_span(&body));
            Statement::ResourceManagement(ResourceManagement::Use {
                resource: Box::new(resource),
                body: Box::new(body.clone()),
                span,
            })
        })
}

fn defer_statement_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_defer()
        .ignore_then(block_expression_parser(statement.clone()))
        .map(|body| {
            Statement::ResourceManagement(ResourceManagement::Defer {
                body: Box::new(body.clone()),
                span: expression_span(&body),
            })
        })
}

fn spawn_statement_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_spawn()
        .ignore_then(block_expression_parser(statement.clone()))
        .map(|body| {
            Statement::Concurrency(ConcurrencyConstruct::Spawn {
                body: Box::new(body.clone()),
                span: expression_span(&body),
            })
        })
}

fn return_statement_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_return()
        .then(expr.or_not())
        .map(|(ret_token, value)| {
            let span = value
                .as_ref()
                .map(expression_span)
                .unwrap_or_else(|| span_from_token(&ret_token));
            Statement::Return { value, span }
        })
}

fn expression_statement_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    expr.map(|expression| {
        let span = expression_span(&expression);
        Statement::Expression {
            expr: expression,
            span,
        }
    })
}

fn assignment_target_parser() -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone
{
    identifier()
        .then(token_dot().ignore_then(identifier()).repeated())
        .map(|(base, properties)| {
            let mut expr = Expression::Identifier(base, Span::dummy());
            for property in properties {
                expr = Expression::MemberAccess {
                    object: Box::new(expr),
                    property,
                    span: Span::dummy(),
                };
            }
            expr
        })
}

fn loop_binding_parser() -> impl ChumskyParser<Token, LoopBinding, Error = Simple<Token>> + Clone {
    identifier_with_span()
        .then(type_annotation_clause())
        .map(|((name, span), type_annotation)| LoopBinding {
            name,
            type_annotation,
            span,
        })
}

fn infer_loop_strategy(iterable: &Expression) -> LoopStrategy {
    match iterable {
        Expression::Binary {
            left,
            right,
            op: BinaryOp::RangeExclusive,
            span,
        } => LoopStrategy::NumericRange(NumericRangeLoop {
            start: (*left.clone()),
            end: (*right.clone()),
            inclusive: false,
            span: span.clone(),
        }),
        Expression::Binary {
            left,
            right,
            op: BinaryOp::RangeInclusive,
            span,
        } => LoopStrategy::NumericRange(NumericRangeLoop {
            start: (*left.clone()),
            end: (*right.clone()),
            inclusive: true,
            span: span.clone(),
        }),
        _ => LoopStrategy::Iterable,
    }
}

fn block_expression_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_left_brace()
        .ignore_then(statement.repeated())
        .then_ignore(token_right_brace())
        .map(|statements| {
            let span = if statements.is_empty() {
                Span::dummy()
            } else {
                let start = statement_span(&statements[0]);
                let end = statement_span(statements.last().unwrap());
                merge_spans(&start, &end)
            };

            Expression::Block { statements, span }
        })
}

fn function_body(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    block_expression_parser(statement.clone()).or(token_assign().ignore_then(expr))
}

fn for_in_statement_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_for()
        .map(|token| span_from_token(&token))
        .then(
            token_left_paren()
                .ignore_then(loop_binding_parser())
                .then_ignore(token_in_keyword())
                .then(expr.clone())
                .then_ignore(token_right_paren()),
        )
        .then(block_expression_parser(statement.clone()))
        .map(|((for_span, (binding, iterable)), body)| {
            let strategy = infer_loop_strategy(&iterable);
            let body_span = expression_span(&body);
            let span = merge_spans(&for_span, &body_span);

            Statement::ForIn(ForInStatement {
                binding,
                iterable,
                strategy,
                body: Box::new(body),
                span,
            })
        })
}

fn legacy_loop_parser() -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    choice((token_while_keyword(), token_do_keyword())).try_map(|token, span| {
        let keyword = match token.token_type {
            TokenType::While => "while",
            TokenType::Do => "do-while",
            _ => "legacy loop",
        };
        let message = format!(
            "E_LOOP_001: `{}` loops are no longer supported. Use `for (item in ...)` with ranges or iterables instead.",
            keyword
        );
        Err(Simple::custom(span, message))
    })
}

fn function_signature(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, FunctionSignature, Error = Simple<Token>> + Clone {
    let receiver_and_name = type_annotation_simple()
        .then_ignore(token_dot())
        .then(identifier())
        .map(|(receiver, name)| (Some(receiver), name))
        .or(identifier().map(|name| (None, name)));

    receiver_and_name
        .then_ignore(token_left_paren())
        .then(parameter_list(expr.clone()))
        .then_ignore(token_right_paren())
        .then(type_annotation_clause())
        .map(
            |(((receiver, name), parameters), return_type)| FunctionSignature {
                receiver_type: receiver,
                name,
                parameters,
                return_type,
            },
        )
}

fn type_annotation_clause(
) -> impl ChumskyParser<Token, Option<TypeAnnotation>, Error = Simple<Token>> + Clone {
    token_colon().ignore_then(type_annotation_simple()).or_not()
}

fn modifiers_parser() -> impl ChumskyParser<Token, Modifiers, Error = Simple<Token>> + Clone {
    annotation_parser()
        .repeated()
        .then(modifier_keyword().repeated())
        .map(|(annotations, keywords)| {
            let mut modifiers = Modifiers::default();
            modifiers.annotations = annotations;
            for keyword in keywords {
                match keyword {
                    ModifierToken::Visibility(vis) => modifiers.visibility = vis,
                    ModifierToken::Abstract => modifiers.is_abstract = true,
                    ModifierToken::Final => modifiers.is_final = true,
                    ModifierToken::Static => modifiers.is_static = true,
                    ModifierToken::Override => modifiers.is_override = true,
                    ModifierToken::Open => modifiers.is_open = true,
                }
            }
            modifiers
        })
}

fn annotation_parser() -> impl ChumskyParser<Token, Annotation, Error = Simple<Token>> + Clone {
    recursive(|annotation_parser_ref| {
        let annotation_value = recursive(|value_parser_ref| {
            let literal = filter_map(|span, token: Token| {
                let token_span = span_from_token(&token);
                match token.token_type {
                    TokenType::String(value) => Ok((
                        AnnotationValue::Literal(Literal::String(value)),
                        token_span,
                    )),
                    TokenType::Number(value) => Ok((
                        AnnotationValue::Literal(Literal::Number(value)),
                        token_span,
                    )),
                    TokenType::Boolean(value) => Ok((
                        AnnotationValue::Literal(Literal::Boolean(value)),
                        token_span,
                    )),
                    TokenType::Null => Ok((AnnotationValue::Literal(Literal::Null), token_span)),
                    _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
                }
            });

            let enum_or_class = qualified_name_with_span().then(
                token_dot()
                    .ignore_then(token_class().map(|token| span_from_token(&token)))
                    .or_not(),
            )
            .map(|((segments, name_span), maybe_class_span)| {
                if let Some(class_span) = maybe_class_span {
                    let span = merge_spans(&name_span, &class_span);
                    (
                        AnnotationValue::ClassLiteral {
                            type_path: segments,
                        },
                        span,
                    )
                } else {
                    let mut type_path = segments.clone();
                    let constant = type_path.pop().unwrap_or_default();
                    (
                        AnnotationValue::EnumConstant {
                            type_path,
                            constant,
                        },
                        name_span,
                    )
                }
            });

            let nested = annotation_parser_ref.clone().map(|annotation: Annotation| {
                let span = annotation.span.clone();
                (
                    AnnotationValue::NestedAnnotation(Box::new(annotation)),
                    span,
                )
            });

            let array = token_left_brace()
                .map(|token| span_from_token(&token))
                .then(
                    value_parser_ref
                        .clone()
                        .separated_by(token_any_comma())
                        .allow_trailing(),
                )
                .then(token_right_brace().map(|token| span_from_token(&token)))
                .map(|((left_span, values), right_span)| {
                    let elements = values.into_iter().map(|(value, _)| value).collect();
                    let span = merge_spans(&left_span, &right_span);
                    (AnnotationValue::Array(elements), span)
                });

            choice((literal, array, nested, enum_or_class.clone()))
        });

        token_at()
            .map(|token| span_from_token(&token))
            .then(qualified_name_with_span())
            .then(annotation_argument_list(annotation_value).or_not())
            .map(|((at_span, (segments, name_span)), maybe_args)| {
                let (arguments, end_span) =
                    maybe_args.unwrap_or_else(|| (Vec::new(), name_span.clone()));
                let span = merge_spans(&at_span, &end_span);
                Annotation {
                    name: AnnotationName::new(segments, name_span),
                    arguments,
                    span,
                }
            })
    })
}

fn annotation_argument_list(
    value_parser: impl ChumskyParser<Token, (AnnotationValue, Span), Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, (Vec<AnnotationArgument>, Span), Error = Simple<Token>> + Clone {
    token_left_paren()
        .map(|token| span_from_token(&token))
        .then(
            annotation_argument(value_parser)
                .separated_by(token_any_comma())
                .allow_trailing(),
        )
        .then(token_right_paren().map(|token| span_from_token(&token)))
        .map(|((_left_span, arguments), right_span)| (arguments, right_span))
}

fn annotation_argument(
    value_parser: impl ChumskyParser<Token, (AnnotationValue, Span), Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, AnnotationArgument, Error = Simple<Token>> + Clone {
    let positional = value_parser
        .clone()
        .map(|(value, span)| AnnotationArgument::Positional { value, span });

    let named = identifier_with_span()
        .then_ignore(token_assign())
        .then(value_parser)
        .map(|((name, name_span), (value, value_span))| {
            let span = merge_spans(&name_span, &value_span);
            AnnotationArgument::Named { name, value, span }
        });

    choice((named, positional))
}

fn qualified_name_with_span(
) -> impl ChumskyParser<Token, (Vec<String>, Span), Error = Simple<Token>> + Clone {
    identifier_with_span()
        .then(
            token_dot()
                .ignore_then(identifier_with_span())
                .repeated(),
        )
        .map(|((first_name, first_span), rest)| {
            let mut segments = Vec::with_capacity(rest.len() + 1);
            segments.push(first_name);
            let mut span = first_span;
            for (segment, segment_span) in rest {
                span = merge_spans(&span, &segment_span);
                segments.push(segment);
            }
            (segments, span)
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::Parser as ChumskyParser;
    use jv_lexer::TokenTrivia;

    fn make_token(token_type: TokenType, lexeme: &str, column: usize) -> Token {
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            line: 1,
            column,
            leading_trivia: TokenTrivia::default(),
            diagnostic: None,
            metadata: Vec::new(),
        }
    }

    #[test]
    fn annotation_parser_supports_positional_and_named_arguments() {
        let parser = annotation_parser();

        let tokens = vec![
            make_token(TokenType::At, "@", 1),
            make_token(TokenType::Identifier("Sample".to_string()), "Sample", 2),
            make_token(TokenType::LeftParen, "(", 8),
            make_token(
                TokenType::String("examples/users.json".to_string()),
                "examples/users.json",
                9,
            ),
            make_token(TokenType::Comma, ",", 31),
            make_token(TokenType::Identifier("mode".to_string()), "mode", 33),
            make_token(TokenType::Assign, "=", 37),
            make_token(TokenType::Identifier("Load".to_string()), "Load", 38),
            make_token(TokenType::RightParen, ")", 42),
        ];

        let annotation = parser.parse(tokens).expect("parse annotation");

        assert_eq!(annotation.name.simple_name(), "Sample");
        assert_eq!(annotation.arguments.len(), 2);

        match &annotation.arguments[0] {
            AnnotationArgument::Positional { value, .. } => match value {
                AnnotationValue::Literal(Literal::String(path)) => {
                    assert_eq!(path, "examples/users.json")
                }
                other => panic!("expected string literal, found {:?}", other),
            },
            other => panic!("expected positional literal, found {:?}", other),
        }

        match &annotation.arguments[1] {
            AnnotationArgument::Named { name, value, .. } => {
                assert_eq!(name, "mode");
                match value {
                    AnnotationValue::EnumConstant { type_path, constant } => {
                        assert!(type_path.is_empty());
                        assert_eq!(constant, "Load");
                    }
                    other => panic!("expected identifier expression, found {:?}", other),
                }
            }
            other => panic!("expected named argument, found {:?}", other),
        }

        assert_eq!(annotation.span.start_column, 1);
        assert!(annotation.span.end_column >= 42);
    }

    #[test]
    fn modifiers_parser_collects_annotations_and_keywords() {
        let parser = modifiers_parser();

        let tokens = vec![
            make_token(TokenType::At, "@", 1),
            make_token(TokenType::Identifier("Sample".to_string()), "Sample", 2),
            make_token(TokenType::LeftParen, "(", 8),
            make_token(TokenType::String("data.json".to_string()), "data.json", 9),
            make_token(TokenType::RightParen, ")", 21),
            make_token(TokenType::Identifier("public".to_string()), "public", 23),
            make_token(TokenType::Identifier("final".to_string()), "final", 30),
        ];

        let modifiers = parser.parse(tokens).expect("parse modifiers");

        assert_eq!(modifiers.annotations.len(), 1);
        assert_eq!(modifiers.annotations[0].name.simple_name(), "Sample");
        assert_eq!(modifiers.visibility, Visibility::Public);
        assert!(modifiers.is_final);
        assert_eq!(modifiers.annotations[0].arguments.len(), 1);
        match &modifiers.annotations[0].arguments[0] {
            AnnotationArgument::Positional { value, .. } => match value {
                AnnotationValue::Literal(Literal::String(path)) => {
                    assert_eq!(path, "data.json");
                }
                other => panic!("expected string literal, found {:?}", other),
            },
            other => panic!("expected positional argument, found {:?}", other),
        }
    }

    #[test]
    fn annotation_parser_supports_enum_and_class_literals() {
        let parser = annotation_parser();

        let tokens = vec![
            make_token(TokenType::At, "@", 1),
            make_token(TokenType::Identifier("Meta".to_string()), "Meta", 2),
            make_token(TokenType::LeftParen, "(", 6),
            make_token(TokenType::Identifier("status".to_string()), "status", 7),
            make_token(TokenType::Assign, "=", 13),
            make_token(TokenType::Identifier("com".to_string()), "com", 15),
            make_token(TokenType::Dot, ".", 18),
            make_token(TokenType::Identifier("example".to_string()), "example", 19),
            make_token(TokenType::Dot, ".", 26),
            make_token(TokenType::Identifier("Status".to_string()), "Status", 27),
            make_token(TokenType::Dot, ".", 33),
            make_token(TokenType::Identifier("ACTIVE".to_string()), "ACTIVE", 34),
            make_token(TokenType::Comma, ",", 40),
            make_token(TokenType::Identifier("clazz".to_string()), "clazz", 42),
            make_token(TokenType::Assign, "=", 47),
            make_token(TokenType::Identifier("java".to_string()), "java", 49),
            make_token(TokenType::Dot, ".", 53),
            make_token(TokenType::Identifier("lang".to_string()), "lang", 54),
            make_token(TokenType::Dot, ".", 58),
            make_token(TokenType::Identifier("String".to_string()), "String", 59),
            make_token(TokenType::Dot, ".", 65),
            make_token(TokenType::Class, "class", 66),
            make_token(TokenType::RightParen, ")", 71),
        ];

        let annotation = parser.parse(tokens).expect("parse annotation with enum/class");

        assert_eq!(annotation.arguments.len(), 2);

        match &annotation.arguments[0] {
            AnnotationArgument::Named { name, value, .. } => {
                assert_eq!(name, "status");
                match value {
                    AnnotationValue::EnumConstant { type_path, constant } => {
                        assert_eq!(type_path, &vec![
                            "com".to_string(),
                            "example".to_string(),
                            "Status".to_string(),
                        ]);
                        assert_eq!(constant, "ACTIVE");
                    }
                    other => panic!("expected enum constant, found {:?}", other),
                }
            }
            other => panic!("unexpected argument {:?}", other),
        }

        match &annotation.arguments[1] {
            AnnotationArgument::Named { name, value, .. } => {
                assert_eq!(name, "clazz");
                match value {
                    AnnotationValue::ClassLiteral { type_path } => {
                        assert_eq!(type_path, &vec![
                            "java".to_string(),
                            "lang".to_string(),
                            "String".to_string(),
                        ]);
                    }
                    other => panic!("expected class literal, found {:?}", other),
                }
            }
            other => panic!("unexpected argument {:?}", other),
        }
    }

    #[test]
    fn annotation_parser_supports_array_and_nested_annotations() {
        let parser = annotation_parser();

        let tokens = vec![
            make_token(TokenType::At, "@", 1),
            make_token(TokenType::Identifier("Tag".to_string()), "Tag", 2),
            make_token(TokenType::LeftParen, "(", 5),
            make_token(TokenType::Identifier("values".to_string()), "values", 6),
            make_token(TokenType::Assign, "=", 12),
            make_token(TokenType::LeftBrace, "{", 14),
            make_token(TokenType::String("alpha".to_string()), "\"alpha\"", 15),
            make_token(TokenType::Comma, ",", 22),
            make_token(TokenType::String("beta".to_string()), "\"beta\"", 24),
            make_token(TokenType::RightBrace, "}", 30),
            make_token(TokenType::Comma, ",", 31),
            make_token(TokenType::Identifier("nested".to_string()), "nested", 33),
            make_token(TokenType::Assign, "=", 39),
            make_token(TokenType::At, "@", 41),
            make_token(TokenType::Identifier("Qualifier".to_string()), "Qualifier", 42),
            make_token(TokenType::LeftParen, "(", 51),
            make_token(TokenType::Identifier("value".to_string()), "value", 52),
            make_token(TokenType::Assign, "=", 57),
            make_token(TokenType::String("id".to_string()), "\"id\"", 59),
            make_token(TokenType::RightParen, ")", 63),
            make_token(TokenType::RightParen, ")", 64),
        ];

        let annotation = parser.parse(tokens).expect("parse annotation with array/nested");

        assert_eq!(annotation.arguments.len(), 2);

        match &annotation.arguments[0] {
            AnnotationArgument::Named { name, value, .. } => {
                assert_eq!(name, "values");
                match value {
                    AnnotationValue::Array(elements) => {
                        assert_eq!(elements.len(), 2);
                        match &elements[0] {
                            AnnotationValue::Literal(Literal::String(text)) => {
                                assert_eq!(text, "alpha");
                            }
                            other => panic!("unexpected array element {:?}", other),
                        }
                    }
                    other => panic!("expected array, found {:?}", other),
                }
            }
            other => panic!("unexpected argument {:?}", other),
        }

        match &annotation.arguments[1] {
            AnnotationArgument::Named { name, value, .. } => {
                assert_eq!(name, "nested");
                match value {
                    AnnotationValue::NestedAnnotation(inner) => {
                        assert_eq!(inner.name.simple_name(), "Qualifier");
                    }
                    other => panic!("expected nested annotation, found {:?}", other),
                }
            }
            other => panic!("unexpected argument {:?}", other),
        }
    }
}

fn modifier_keyword() -> impl ChumskyParser<Token, ModifierToken, Error = Simple<Token>> + Clone {
    choice((
        support_keyword("public")
            .to(())
            .map(|_| ModifierToken::Visibility(Visibility::Public)),
        support_keyword("private")
            .to(())
            .map(|_| ModifierToken::Visibility(Visibility::Private)),
        support_keyword("internal")
            .to(())
            .map(|_| ModifierToken::Visibility(Visibility::Internal)),
        support_keyword("protected")
            .to(())
            .map(|_| ModifierToken::Visibility(Visibility::Protected)),
        support_keyword("abstract")
            .to(())
            .map(|_| ModifierToken::Abstract),
        support_keyword("final")
            .to(())
            .map(|_| ModifierToken::Final),
        support_keyword("static")
            .to(())
            .map(|_| ModifierToken::Static),
        support_keyword("override")
            .to(())
            .map(|_| ModifierToken::Override),
        support_keyword("open").to(()).map(|_| ModifierToken::Open),
    ))
}

#[derive(Debug, Clone)]
struct FunctionSignature {
    receiver_type: Option<TypeAnnotation>,
    name: String,
    parameters: Vec<Parameter>,
    return_type: Option<TypeAnnotation>,
}

#[derive(Debug, Clone)]
enum ModifierToken {
    Visibility(Visibility),
    Abstract,
    Final,
    Static,
    Override,
    Open,
}
