use crate::inference::conversions::AppliedConversion;
use crate::inference::types::{PrimitiveType, TypeKind};
use jv_ast::Span;
use jv_ast::strings::MultilineStringLiteral;

/// 文脈適応で適用される変換の種類。
#[derive(Debug, Clone, PartialEq)]
pub enum ContextAdaptation {
    CharToString(CharToStringAdaptation),
}

/// `Char` → `String` 変換を挿入する際のメタデータ。
#[derive(Debug, Clone, PartialEq)]
pub struct CharToStringAdaptation {
    pub span: Span,
}

/// Raw文字列リテラルに対する初期型を判定する。
/// `'''a'''` や `'a'` のように単一UTF-16コード単位で構成される場合は `Char` を割り当てる。
pub fn infer_multiline_literal_type(literal: &MultilineStringLiteral) -> TypeKind {
    if is_raw_char_literal(literal) {
        TypeKind::primitive(PrimitiveType::Char)
    } else {
        TypeKind::reference("java.lang.String")
    }
}

/// 推論ソルバが記録した変換の中から文脈適応に相当するものを抽出する。
pub fn collect_context_adaptations(conversions: &[AppliedConversion]) -> Vec<ContextAdaptation> {
    conversions
        .iter()
        .filter_map(|conversion| {
            if is_char_to_string_conversion(conversion) {
                conversion.source_span.as_ref().map(|span| {
                    ContextAdaptation::CharToString(CharToStringAdaptation { span: span.clone() })
                })
            } else {
                None
            }
        })
        .collect()
}

fn is_char_to_string_conversion(conversion: &AppliedConversion) -> bool {
    is_charish(&conversion.from_type) && is_stringish(&conversion.to_type)
}

fn is_charish(ty: &TypeKind) -> bool {
    match ty {
        TypeKind::Primitive(PrimitiveType::Char) => true,
        TypeKind::Optional(inner) => is_charish(inner),
        _ => false,
    }
}

fn is_stringish(ty: &TypeKind) -> bool {
    match ty {
        TypeKind::Reference(name) if name == "java.lang.String" => true,
        TypeKind::Optional(inner) => is_stringish(inner),
        _ => false,
    }
}

fn is_raw_char_literal(literal: &MultilineStringLiteral) -> bool {
    if !literal.parts.is_empty() {
        return false;
    }

    if literal.raw_flavor.is_none() {
        return false;
    }

    literal.normalized.encode_utf16().count() == 1
}
