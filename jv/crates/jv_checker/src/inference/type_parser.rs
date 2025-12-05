//! 型注釈から `TypeKind` への変換を担当し、タプル型注釈を解析するヘルパ。
//!
//! 3.2 タスクでは `(Int String)` のようなタプル注釈を解析し、要素ごとの型に
//! 分解できるようにすることが求められている。本モジュールは注釈のパースと
//! 構造化された `TupleTypeDescriptor` の生成を担う。

use crate::inference::type_factory::TypeFactory;
use crate::inference::types::{TypeError, TypeKind};
use jv_ast::{Statement, TypeAnnotation};
use jv_ast::types::{TupleTypeDescriptor, TupleTypeElement};
use jv_parser_frontend::{Parser2Pipeline, ParserPipeline};

/// 型注釈を `TypeKind` に変換する。
pub fn parse_type_annotation(annotation: &TypeAnnotation) -> Result<TypeKind, TypeError> {
    if let Some(descriptor) = parse_tuple_descriptor(annotation)? {
        let mut element_types = Vec::with_capacity(descriptor.elements.len());
        for element in &descriptor.elements {
            element_types.push(parse_type_annotation(&element.ty)?);
        }
        return Ok(TypeKind::tuple(element_types));
    }

    match annotation {
        TypeAnnotation::Simple(name) => TypeFactory::from_annotation(name),
        TypeAnnotation::Generic { name, type_args } => {
            for arg in type_args {
                // タプル型がネストしている場合に備え、各引数も解析しておく。
                let _ = parse_type_annotation(arg)?;
            }
            TypeFactory::from_annotation(name)
        }
        TypeAnnotation::Nullable(inner) => {
            let inner_ty = parse_type_annotation(inner)?;
            Ok(TypeKind::optional(inner_ty))
        }
        TypeAnnotation::Function {
            params,
            return_type,
        } => {
            let mut param_types = Vec::with_capacity(params.len());
            for param in params {
                param_types.push(parse_type_annotation(param)?);
            }
            let ret_ty = parse_type_annotation(return_type)?;
            Ok(TypeKind::function(param_types, ret_ty))
        }
        TypeAnnotation::Unit { base, .. } => parse_type_annotation(base),
        _ => Ok(TypeKind::Unknown),
    }
}

/// タプル型として解釈できる場合は `TupleTypeDescriptor` を返す。
pub fn parse_tuple_descriptor(
    annotation: &TypeAnnotation,
) -> Result<Option<TupleTypeDescriptor>, TypeError> {
    match annotation {
        TypeAnnotation::Simple(raw) => parse_tuple_from_simple(raw),
        _ => Ok(None),
    }
}

fn parse_tuple_from_simple(raw: &str) -> Result<Option<TupleTypeDescriptor>, TypeError> {
    let trimmed = raw.trim();
    if !(trimmed.starts_with('(') && trimmed.ends_with(')')) {
        return Ok(None);
    }

    // 関数型 `(A, B) -> C` を誤ってタプル扱いしないための簡易チェック。
    if trimmed.contains("->") {
        return Ok(None);
    }

    let inner = &trimmed[1..trimmed.len() - 1];
    let parts = split_tuple_elements(inner)
        .map_err(|reason| TypeError::invalid_tuple(raw.to_string(), reason))?;

    if parts.len() < 2 {
        return Err(TypeError::invalid_tuple(
            raw.to_string(),
            "タプル型には2要素以上が必要です".to_string(),
        ));
    }

    let mut elements = Vec::with_capacity(parts.len());
    for segment in parts {
        let annotation = lower_tuple_element(raw, &segment)
            .map_err(|reason| TypeError::invalid_tuple(raw.to_string(), reason))?;
        elements.push(TupleTypeElement::new(annotation, None));
    }

    Ok(Some(TupleTypeDescriptor::new(elements, None)))
}

fn split_tuple_elements(value: &str) -> Result<Vec<String>, String> {
    let mut elements = Vec::new();
    let mut current = String::new();
    let mut paren_depth = 0;
    let mut angle_depth = 0;
    let mut square_depth = 0;

    let mut chars = value.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '(' => {
                paren_depth += 1;
                current.push(ch);
            }
            ')' => {
                if paren_depth == 0 {
                    return Err("括弧の対応関係が不正です".to_string());
                }
                paren_depth -= 1;
                current.push(ch);
            }
            '<' => {
                angle_depth += 1;
                current.push(ch);
            }
            '>' => {
                if angle_depth == 0 {
                    return Err("ジェネリック引数の括弧が不正です".to_string());
                }
                angle_depth -= 1;
                current.push(ch);
            }
            '[' => {
                square_depth += 1;
                current.push(ch);
            }
            ']' => {
                if square_depth == 0 {
                    return Err("配列型の括弧が不正です".to_string());
                }
                square_depth -= 1;
                current.push(ch);
            }
            ',' => {
                if paren_depth == 0 && angle_depth == 0 && square_depth == 0 {
                    flush_segment(&mut current, &mut elements);
                } else {
                    current.push(ch);
                }
            }
            ch if ch.is_whitespace() => {
                if paren_depth == 0 && angle_depth == 0 && square_depth == 0 {
                    flush_segment(&mut current, &mut elements);
                } else if !current.is_empty() {
                    current.push(ch);
                }
            }
            _ => {
                current.push(ch);
            }
        }
    }

    if paren_depth != 0 || angle_depth != 0 || square_depth != 0 {
        return Err("括弧のネストが閉じられていません".to_string());
    }

    flush_segment(&mut current, &mut elements);

    if elements.is_empty() {
        return Err("タプル要素が見つかりません".to_string());
    }

    Ok(elements)
}

fn flush_segment(buffer: &mut String, elements: &mut Vec<String>) {
    let trimmed = buffer.trim();
    if !trimmed.is_empty() {
        elements.push(trimmed.to_string());
    }
    buffer.clear();
}

fn lower_tuple_element(tuple_source: &str, segment: &str) -> Result<TypeAnnotation, String> {
    let trimmed = segment.trim();
    if trimmed.is_empty() {
        return Err(format!(
            "要素 `{segment}` が空です (元の注釈 `{tuple_source}`)"
        ));
    }

    // Parser2Pipeline を使用して型注釈をパースする。
    // `val _: TYPE = null` という形式でラップしてパースし、型注釈を抽出する。
    let wrapper = format!("val _: {trimmed} = null");
    let pipeline = Parser2Pipeline::default();

    let output = pipeline.parse(&wrapper).map_err(|error| {
        format!(
            "要素 `{segment}` の解析に失敗しました (注釈 `{tuple_source}`): {error}"
        )
    })?;

    let program = output.into_program();
    let stmt = program.statements.into_iter().next().ok_or_else(|| {
        format!(
            "要素 `{segment}` を型として解釈できませんでした (注釈 `{tuple_source}`)"
        )
    })?;

    match stmt {
        Statement::ValDeclaration {
            type_annotation: Some(ty),
            ..
        } => Ok(ty),
        _ => Err(format!(
            "要素 `{segment}` を型として解釈できませんでした (注釈 `{tuple_source}`)"
        )),
    }
}
