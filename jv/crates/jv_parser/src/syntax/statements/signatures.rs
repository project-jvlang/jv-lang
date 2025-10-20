use chumsky::error::Simple;
use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{
    Annotation, AnnotationArgument, AnnotationName, AnnotationValue, Expression, GenericParameter,
    GenericSignature, Literal, Modifiers, Parameter, PrimitiveBound, PrimitiveReturnMetadata,
    PrimitiveTypeName, PrimitiveTypeReference, PrimitiveTypeSource, QualifiedName,
    RawTypeContinuation, RawTypeDirective, Span, TypeAnnotation, VarianceMarker, Visibility,
    WhereClause, WherePredicate,
};
use jv_lexer::{SourceCommentTrivia, Token, TokenTrivia, TokenType};

use crate::syntax::parameters::parameter_list;
use crate::syntax::support::{
    identifier, identifier_with_span, keyword as support_keyword, merge_spans,
    qualified_name_with_span, span_from_token, token_any_comma, token_assign, token_at,
    token_class, token_colon, token_dot, token_greater, token_left_brace, token_left_paren,
    token_less, token_question, token_right_brace, token_right_paren, token_where_keyword,
    type_annotation,
};

fn optional_type_arguments(
) -> impl ChumskyParser<Token, Vec<TypeAnnotation>, Error = Simple<Token>> + Clone {
    token_less()
        .ignore_then(
            type_annotation()
                .separated_by(token_any_comma())
                .allow_trailing(),
        )
        .then_ignore(token_greater())
        .or_not()
        .map(|args| args.unwrap_or_default())
}

fn receiver_type_parser() -> impl ChumskyParser<Token, TypeAnnotation, Error = Simple<Token>> + Clone
{
    identifier()
        .then(optional_type_arguments())
        .then(token_question().or_not())
        .map(|((name, type_args), nullable)| {
            let base = if type_args.is_empty() {
                TypeAnnotation::Simple(name)
            } else {
                TypeAnnotation::Generic { name, type_args }
            };

            if nullable.is_some() {
                TypeAnnotation::Nullable(Box::new(base))
            } else {
                base
            }
        })
}

pub(super) fn function_signature(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, FunctionSignature, Error = Simple<Token>> + Clone {
    let receiver_and_name = receiver_type_parser()
        .then_ignore(token_dot())
        .then(identifier())
        .map(|(receiver, name)| (Some(receiver), name, None))
        .boxed();

    let primitive_return_and_name = identifier_with_span()
        .then(identifier())
        .try_map(|((candidate, candidate_span), name), span| {
            let segments = [candidate.clone()];
            if let Some(reference) = primitive_reference_from_segments(&segments, &candidate_span) {
                if matches!(reference.source, PrimitiveTypeSource::PrimitiveKeyword) {
                    return Ok((None, name, Some(PrimitiveReturnMetadata { reference })));
                }
            }
            Err(Simple::expected_input_found(span, Vec::new(), None))
        })
        .boxed();

    let name_only = identifier().map(|name| (None, name, None)).boxed();

    type_parameter_list()
        .or_not()
        .then(
            receiver_and_name
                .or(primitive_return_and_name)
                .or(name_only),
        )
        .then_ignore(token_left_paren())
        .then(parameter_list(expr.clone()))
        .then_ignore(token_right_paren())
        .then(type_annotation_clause())
        .then(where_clause_parser())
        .map(
            |(
                (((generics, (receiver, name, primitive_return)), parameters), return_type),
                where_clause,
            )| {
                let (type_parameters, generic_signature) = match generics {
                    Some(generics) => {
                        let names: Vec<String> = generics
                            .params
                            .iter()
                            .map(|param| param.name.clone())
                            .collect();
                        let signature = GenericSignature {
                            parameters: generics.params,
                            const_parameters: Vec::new(),
                            where_clause: where_clause.clone(),
                            raw_directives: generics.raw_directives,
                            span: generics.span,
                        };
                        (names, Some(signature))
                    }
                    None => (Vec::new(), None),
                };

                FunctionSignature {
                    receiver_type: receiver,
                    name,
                    type_parameters,
                    generic_signature,
                    parameters,
                    return_type,
                    primitive_return,
                    where_clause,
                }
            },
        )
}

pub(super) fn type_parameter_list(
) -> impl ChumskyParser<Token, ParsedGenerics, Error = Simple<Token>> + Clone {
    token_less()
        .map(|token| {
            let span = span_from_token(&token);
            (span, collect_raw_directives_from_token(&token))
        })
        .then(
            generic_parameter()
                .separated_by(token_any_comma())
                .allow_trailing(),
        )
        .then(token_greater().map(|token| {
            let span = span_from_token(&token);
            (span, collect_raw_directives_from_token(&token))
        }))
        .map(
            |(((left_span, mut directives), params), (right_span, mut closing_directives))| {
                directives.append(&mut closing_directives);
                ParsedGenerics {
                    params,
                    raw_directives: directives,
                    span: merge_spans(&left_span, &right_span),
                }
            },
        )
}

pub(super) fn extract_type_parameters_and_signature(
    generics: Option<ParsedGenerics>,
) -> (Vec<String>, Option<GenericSignature>) {
    match generics {
        Some(parsed) => {
            let ParsedGenerics {
                params,
                raw_directives,
                span,
            } = parsed;

            let type_parameters = params
                .iter()
                .map(|param| param.name.clone())
                .collect::<Vec<_>>();

            let signature = GenericSignature {
                parameters: params,
                const_parameters: Vec::new(),
                where_clause: None,
                raw_directives,
                span,
            };

            (type_parameters, Some(signature))
        }
        None => (Vec::new(), None),
    }
}

fn generic_parameter() -> impl ChumskyParser<Token, GenericParameter, Error = Simple<Token>> + Clone
{
    variance_marker()
        .then(identifier_with_span())
        .then(
            token_colon()
                .ignore_then(type_annotation())
                .map(|annotation| vec![annotation])
                .or_not()
                .map(|maybe| maybe.unwrap_or_default()),
        )
        .then(token_assign().ignore_then(type_annotation()).or_not())
        .map(
            |(((variance, (name, name_span)), bounds), default)| GenericParameter {
                name,
                bounds,
                variance,
                default,
                kind: None,
                span: name_span,
            },
        )
}

fn variance_marker(
) -> impl ChumskyParser<Token, Option<VarianceMarker>, Error = Simple<Token>> + Clone {
    filter_map(|span, token: Token| match token.token_type {
        TokenType::Identifier(ref name) if name == "out" => Ok(VarianceMarker::Covariant),
        TokenType::Identifier(ref name) if name == "in" => Ok(VarianceMarker::Contravariant),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    })
    .or_not()
}

fn collect_raw_directives_from_token(token: &Token) -> Vec<RawTypeDirective> {
    collect_raw_directives_from_trivia(&token.leading_trivia)
}

fn collect_raw_directives_from_trivia(trivia: &TokenTrivia) -> Vec<RawTypeDirective> {
    let mut directives = Vec::new();

    for comment in trivia
        .passthrough_comments
        .iter()
        .chain(trivia.jv_comments.iter())
    {
        if let Some(directive) = raw_directive_from_comment(comment) {
            directives.push(directive);
        }
    }

    directives
        .into_iter()
        .filter(|directive| !directive.owner.segments.is_empty())
        .collect()
}

fn primitive_reference_from_segments(
    segments: &[String],
    span: &Span,
) -> Option<PrimitiveTypeReference> {
    classify_primitive_segments(segments).map(|(primitive, source)| PrimitiveTypeReference {
        primitive,
        source,
        raw_path: segments.to_vec(),
        span: span.clone(),
    })
}

fn classify_primitive_segments(
    segments: &[String],
) -> Option<(PrimitiveTypeName, PrimitiveTypeSource)> {
    match segments.len() {
        1 => match segments[0].as_str() {
            "int" => Some((
                PrimitiveTypeName::Int,
                PrimitiveTypeSource::PrimitiveKeyword,
            )),
            "long" => Some((
                PrimitiveTypeName::Long,
                PrimitiveTypeSource::PrimitiveKeyword,
            )),
            "short" => Some((
                PrimitiveTypeName::Short,
                PrimitiveTypeSource::PrimitiveKeyword,
            )),
            "byte" => Some((
                PrimitiveTypeName::Byte,
                PrimitiveTypeSource::PrimitiveKeyword,
            )),
            "double" => Some((
                PrimitiveTypeName::Double,
                PrimitiveTypeSource::PrimitiveKeyword,
            )),
            "float" => Some((
                PrimitiveTypeName::Float,
                PrimitiveTypeSource::PrimitiveKeyword,
            )),
            "boolean" => Some((
                PrimitiveTypeName::Boolean,
                PrimitiveTypeSource::PrimitiveKeyword,
            )),
            "char" => Some((
                PrimitiveTypeName::Char,
                PrimitiveTypeSource::PrimitiveKeyword,
            )),
            "Integer" | "Int" => {
                Some((PrimitiveTypeName::Int, PrimitiveTypeSource::BoxedIdentifier))
            }
            "Long" => Some((
                PrimitiveTypeName::Long,
                PrimitiveTypeSource::BoxedIdentifier,
            )),
            "Short" => Some((
                PrimitiveTypeName::Short,
                PrimitiveTypeSource::BoxedIdentifier,
            )),
            "Byte" => Some((
                PrimitiveTypeName::Byte,
                PrimitiveTypeSource::BoxedIdentifier,
            )),
            "Double" => Some((
                PrimitiveTypeName::Double,
                PrimitiveTypeSource::BoxedIdentifier,
            )),
            "Float" => Some((
                PrimitiveTypeName::Float,
                PrimitiveTypeSource::BoxedIdentifier,
            )),
            "Boolean" => Some((
                PrimitiveTypeName::Boolean,
                PrimitiveTypeSource::BoxedIdentifier,
            )),
            "Character" | "Char" => Some((
                PrimitiveTypeName::Char,
                PrimitiveTypeSource::BoxedIdentifier,
            )),
            _ => None,
        },
        3 if segments[0] == "java" && segments[1] == "lang" => match segments[2].as_str() {
            "Integer" => Some((
                PrimitiveTypeName::Int,
                PrimitiveTypeSource::QualifiedBoxedIdentifier,
            )),
            "Long" => Some((
                PrimitiveTypeName::Long,
                PrimitiveTypeSource::QualifiedBoxedIdentifier,
            )),
            "Short" => Some((
                PrimitiveTypeName::Short,
                PrimitiveTypeSource::QualifiedBoxedIdentifier,
            )),
            "Byte" => Some((
                PrimitiveTypeName::Byte,
                PrimitiveTypeSource::QualifiedBoxedIdentifier,
            )),
            "Double" => Some((
                PrimitiveTypeName::Double,
                PrimitiveTypeSource::QualifiedBoxedIdentifier,
            )),
            "Float" => Some((
                PrimitiveTypeName::Float,
                PrimitiveTypeSource::QualifiedBoxedIdentifier,
            )),
            "Boolean" => Some((
                PrimitiveTypeName::Boolean,
                PrimitiveTypeSource::QualifiedBoxedIdentifier,
            )),
            "Character" => Some((
                PrimitiveTypeName::Char,
                PrimitiveTypeSource::QualifiedBoxedIdentifier,
            )),
            _ => None,
        },
        _ => None,
    }
}

fn collect_primitive_bounds(predicates: &[WherePredicate]) -> Vec<PrimitiveBound> {
    predicates
        .iter()
        .filter_map(|predicate| match predicate {
            WherePredicate::TraitBound {
                type_param,
                trait_name,
                type_args,
                span,
            } if type_args.is_empty() => {
                primitive_reference_from_segments(&trait_name.segments, &trait_name.span).map(
                    |reference| PrimitiveBound {
                        type_param: type_param.clone(),
                        reference,
                        compatible_aliases: Vec::new(),
                        span: span.clone(),
                    },
                )
            }
            _ => None,
        })
        .collect()
}

fn raw_directive_from_comment(comment: &SourceCommentTrivia) -> Option<RawTypeDirective> {
    let content = normalize_comment_text(&comment.text);
    let trimmed = content.trim();

    let (mode, payload) = if let Some(rest) = trimmed.strip_prefix("jv:raw-allow") {
        (RawTypeContinuation::AllowWithComment, rest.trim())
    } else if let Some(rest) = trimmed.strip_prefix("jv:raw-default") {
        (RawTypeContinuation::DefaultPolicy, rest.trim())
    } else {
        return None;
    };

    if payload.is_empty() {
        return None;
    }

    let segments: Vec<String> = payload
        .split('.')
        .map(|segment| segment.trim())
        .filter(|segment| !segment.is_empty())
        .map(|segment| segment.to_string())
        .collect();

    if segments.is_empty() {
        return None;
    }

    let span = Span::new(
        comment.line,
        comment.column,
        comment.line,
        comment.column + comment.text.len(),
    );

    Some(RawTypeDirective {
        owner: QualifiedName::new(segments, span.clone()),
        span,
        mode,
    })
}

fn normalize_comment_text(raw: &str) -> String {
    let trimmed = raw.trim();
    if let Some(stripped) = trimmed.strip_prefix("//") {
        stripped.trim_start_matches('*').trim().to_string()
    } else if let Some(stripped) = trimmed.strip_prefix("/*") {
        stripped.trim_end_matches("*/").trim().to_string()
    } else {
        trimmed.to_string()
    }
}

fn where_clause_parser(
) -> impl ChumskyParser<Token, Option<WhereClause>, Error = Simple<Token>> + Clone {
    token_where_keyword()
        .map(|token| span_from_token(&token))
        .then(
            where_predicate_parser()
                .separated_by(token_any_comma())
                .at_least(1)
                .allow_trailing(),
        )
        .map(|(where_span, predicates)| {
            let clause_span = predicates
                .last()
                .map(|predicate| predicate.span().clone())
                .map(|end_span| merge_spans(&where_span, &end_span))
                .unwrap_or(where_span.clone());
            let primitive_bounds = collect_primitive_bounds(&predicates);

            WhereClause {
                predicates,
                primitive_bounds,
                span: clause_span,
            }
        })
        .or_not()
}

fn where_predicate_parser(
) -> impl ChumskyParser<Token, WherePredicate, Error = Simple<Token>> + Clone {
    identifier_with_span()
        .then_ignore(token_colon())
        .then(trait_bound_predicate())
        .map(
            |((type_param, type_span), (trait_name, type_args, trait_span))| {
                let span = merge_spans(&type_span, &trait_span);
                WherePredicate::TraitBound {
                    type_param,
                    trait_name,
                    type_args,
                    span,
                }
            },
        )
}

fn trait_bound_predicate(
) -> impl ChumskyParser<Token, (QualifiedName, Vec<TypeAnnotation>, Span), Error = Simple<Token>> + Clone
{
    qualified_name_with_span()
        .map(|(segments, span)| (QualifiedName::new(segments, span.clone()), span))
        .then(
            token_less()
                .map(|token| span_from_token(&token))
                .then(
                    type_annotation()
                        .separated_by(token_any_comma())
                        .allow_trailing(),
                )
                .then(token_greater().map(|token| span_from_token(&token)))
                .map(|((lt_span, args), gt_span)| (args, merge_spans(&lt_span, &gt_span)))
                .or_not(),
        )
        .map(|((name, name_span), maybe_args)| {
            if let Some((args, args_span)) = maybe_args {
                (name, args, merge_spans(&name_span, &args_span))
            } else {
                (name, Vec::new(), name_span)
            }
        })
}

pub(super) fn type_annotation_clause(
) -> impl ChumskyParser<Token, Option<TypeAnnotation>, Error = Simple<Token>> + Clone {
    token_colon().ignore_then(type_annotation()).or_not()
}

pub(super) fn modifiers_parser(
) -> impl ChumskyParser<Token, Modifiers, Error = Simple<Token>> + Clone {
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
                    TokenType::String(value) => {
                        Ok((AnnotationValue::Literal(Literal::String(value)), token_span))
                    }
                    TokenType::Number(value) => {
                        Ok((AnnotationValue::Literal(Literal::Number(value)), token_span))
                    }
                    TokenType::Boolean(value) => Ok((
                        AnnotationValue::Literal(Literal::Boolean(value)),
                        token_span,
                    )),
                    TokenType::Null => Ok((AnnotationValue::Literal(Literal::Null), token_span)),
                    _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
                }
            });

            let enum_or_class = qualified_name_with_span()
                .then(
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

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::Parser as ChumskyParser;
    use jv_lexer::{SourceCommentKind, SourceCommentTrivia, TokenTrivia};

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
                    AnnotationValue::EnumConstant {
                        type_path,
                        constant,
                    } => {
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
    pub(super) fn modifiers_parser_collects_annotations_and_keywords() {
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

        let annotation = parser
            .parse(tokens)
            .expect("parse annotation with enum/class");

        assert_eq!(annotation.arguments.len(), 2);

        match &annotation.arguments[0] {
            AnnotationArgument::Named { name, value, .. } => {
                assert_eq!(name, "status");
                match value {
                    AnnotationValue::EnumConstant {
                        type_path,
                        constant,
                    } => {
                        assert_eq!(
                            type_path,
                            &vec![
                                "com".to_string(),
                                "example".to_string(),
                                "Status".to_string(),
                            ]
                        );
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
                        assert_eq!(
                            type_path,
                            &vec!["java".to_string(), "lang".to_string(), "String".to_string(),]
                        );
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
            make_token(
                TokenType::Identifier("Qualifier".to_string()),
                "Qualifier",
                42,
            ),
            make_token(TokenType::LeftParen, "(", 51),
            make_token(TokenType::Identifier("value".to_string()), "value", 52),
            make_token(TokenType::Assign, "=", 57),
            make_token(TokenType::String("id".to_string()), "\"id\"", 59),
            make_token(TokenType::RightParen, ")", 63),
            make_token(TokenType::RightParen, ")", 64),
        ];

        let annotation = parser
            .parse(tokens)
            .expect("parse annotation with array/nested");

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

    #[test]
    pub(super) fn type_parameter_list_extracts_raw_directive() {
        let mut less = make_token(TokenType::Less, "<", 1);
        less.leading_trivia
            .passthrough_comments
            .push(SourceCommentTrivia {
                kind: SourceCommentKind::Line,
                text: "// jv:raw-allow java.util.List".to_string(),
                line: 1,
                column: 1,
            });

        let identifier = make_token(TokenType::Identifier("T".to_string()), "T", 2);
        let greater = make_token(TokenType::Greater, ">", 3);

        let parser = type_parameter_list();
        let parsed = parser
            .parse(vec![less, identifier, greater])
            .expect("generic parameters should parse");

        assert_eq!(parsed.params.len(), 1);
        assert_eq!(parsed.params[0].name, "T");
        assert_eq!(parsed.raw_directives.len(), 1);
        let directive = &parsed.raw_directives[0];
        assert_eq!(directive.owner.qualified(), "java.util.List");
        assert!(matches!(
            directive.mode,
            RawTypeContinuation::AllowWithComment
        ));
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

pub(super) struct ParsedGenerics {
    params: Vec<GenericParameter>,
    raw_directives: Vec<RawTypeDirective>,
    span: Span,
}

pub(super) struct FunctionSignature {
    pub(super) receiver_type: Option<TypeAnnotation>,
    pub(super) name: String,
    pub(super) type_parameters: Vec<String>,
    pub(super) generic_signature: Option<GenericSignature>,
    pub(super) parameters: Vec<Parameter>,
    pub(super) return_type: Option<TypeAnnotation>,
    pub(super) primitive_return: Option<PrimitiveReturnMetadata>,
    pub(super) where_clause: Option<WhereClause>,
}

enum ModifierToken {
    Visibility(Visibility),
    Abstract,
    Final,
    Static,
    Override,
    Open,
}
