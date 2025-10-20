use jv_ast::{
    types::{PrimitiveTypeName, PrimitiveTypeReference, PrimitiveTypeSource, RawTypeContinuation},
    QualifiedName, RawTypeDirective, Span,
};
use jv_lexer::{Token, TokenTrivia};

pub fn collect_raw_directives_from_token(token: &Token) -> Vec<RawTypeDirective> {
    collect_raw_directives_from_trivia(&token.leading_trivia)
}

pub fn collect_raw_directives_from_trivia(trivia: &TokenTrivia) -> Vec<RawTypeDirective> {
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

pub fn raw_directive_from_comment(
    comment: &jv_lexer::SourceCommentTrivia,
) -> Option<RawTypeDirective> {
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

pub fn primitive_reference_from_segments(
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

pub fn classify_primitive_segments(
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
