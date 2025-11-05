use super::{RawUnitCatalog, UnitDefinitionRaw, UnitMemberRaw};
use crate::CheckError;
use crate::diagnostics;
use crate::diagnostics::unit_semantics;
use crate::inference::type_factory::TypeFactory;
use crate::inference::types::{TypeError, TypeKind};
use jv_ast::{Span, TypeAnnotation};
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::fmt;

/// 単位カテゴリごとの許可能力を表す。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnitCategorySpec {
    pub name: &'static str,
    pub allowed_capabilities: &'static [BaseTypeCapability],
    pub requires_default_marker: bool,
    pub kind: UnitCategoryKind,
}

impl UnitCategorySpec {
    pub fn allows(&self, capability: BaseTypeCapability) -> bool {
        self.allowed_capabilities
            .iter()
            .any(|allowed| *allowed == capability)
    }
}

/// カテゴリ種別。Custom は警告対象。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnitCategoryKind {
    Standard,
    Custom,
}

/// 数値系能力の内訳。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericCapability {
    Integral,
    Decimal,
}

/// 単位が要求する基底型の能力。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BaseTypeCapability {
    Numeric(NumericCapability),
    StringLike,
    Binary,
    Temporal,
}

impl fmt::Display for BaseTypeCapability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BaseTypeCapability::Numeric(NumericCapability::Integral) => write!(f, "整数値"),
            BaseTypeCapability::Numeric(NumericCapability::Decimal) => write!(f, "高精度小数"),
            BaseTypeCapability::StringLike => write!(f, "文字列/テキスト"),
            BaseTypeCapability::Binary => write!(f, "バイナリ列"),
            BaseTypeCapability::Temporal => write!(f, "日時/暦情報"),
        }
    }
}

/// `!` マーカー付き単位の情報。
#[derive(Debug, Clone, PartialEq)]
pub struct DefaultUnit {
    pub symbol: String,
    pub span: Span,
}

/// 検証済み単位定義。
#[derive(Debug, Clone, PartialEq)]
pub struct UnitDefinitionValidated {
    pub definition: UnitDefinitionRaw,
    pub category: UnitCategorySpec,
    pub base_type: TypeKind,
    pub base_capability: BaseTypeCapability,
    pub default_unit: Option<DefaultUnit>,
}

/// 検証後のカタログ。
#[derive(Debug, Clone, Default, PartialEq)]
pub struct ValidatedCatalog {
    pub definitions: Vec<UnitDefinitionValidated>,
    pub defaults: HashMap<String, DefaultUnit>,
}

/// カテゴリ検証ロジック。
#[derive(Debug, Default)]
pub struct UnitSchemaValidator;

impl UnitSchemaValidator {
    pub fn validate(
        catalog: RawUnitCatalog,
        diagnostics: &mut Vec<CheckError>,
    ) -> ValidatedCatalog {
        let mut validated = Vec::new();
        let mut defaults = HashMap::new();
        let mut category_records: HashMap<String, CategoryRecord> = HashMap::new();

        for definition in catalog.definitions {
            let category_name = definition.category.clone();
            let definition_span = definition.span.clone();

            let Some(spec) = lookup_category(&category_name) else {
                emit_unknown_category(&category_name, &definition_span, diagnostics);
                continue;
            };

            if spec.kind == UnitCategoryKind::Custom {
                emit_custom_warning(&category_name, &definition_span, diagnostics);
            }

            let type_kind = match resolve_type_annotation(&definition.base_type) {
                Ok(kind) => kind,
                Err(error) => {
                    emit_base_type_error(
                        describe_type_annotation(&definition.base_type),
                        error.message(),
                        &definition_span,
                        diagnostics,
                    );
                    continue;
                }
            };

            let capability = match capability_from_type(&type_kind) {
                Some(capability) => capability,
                None => {
                    emit_base_type_error(
                        type_kind.describe(),
                        "指定された型は単位カテゴリでサポートされる能力に分類できません。"
                            .to_string(),
                        &definition_span,
                        diagnostics,
                    );
                    continue;
                }
            };

            if !spec.allows(capability) {
                emit_base_type_error(
                    type_kind.describe(),
                    format!(
                        "カテゴリ `{}` では {} を基底型として使用できません。",
                        spec.name, capability
                    ),
                    &definition_span,
                    diagnostics,
                );
                continue;
            }

            let markers = collect_default_markers(&definition);
            let mut definition_default = None;

            let record = category_records
                .entry(category_name.clone())
                .or_insert_with(|| CategoryRecord::new(spec, definition_span.clone()));

            for marker in markers {
                if let Some(existing) = &record.default_marker {
                    emit_duplicate_default(&category_name, &marker, existing, diagnostics);
                    continue;
                }

                defaults.insert(category_name.clone(), marker.clone());
                record.default_marker = Some(marker.clone());
                definition_default.get_or_insert(marker.clone());
            }

            validated.push(UnitDefinitionValidated {
                definition,
                category: spec,
                base_type: type_kind,
                base_capability: capability,
                default_unit: definition_default,
            });
        }

        for record in category_records.values() {
            if record.default_marker.is_none() && record.spec.requires_default_marker {
                emit_missing_default(record.spec.name, &record.first_span, diagnostics);
            }
        }

        ValidatedCatalog {
            definitions: validated,
            defaults,
        }
    }
}

struct CategoryRecord {
    spec: UnitCategorySpec,
    first_span: Span,
    default_marker: Option<DefaultUnit>,
}

impl CategoryRecord {
    fn new(spec: UnitCategorySpec, span: Span) -> Self {
        Self {
            spec,
            first_span: span,
            default_marker: None,
        }
    }
}

#[derive(Debug)]
enum TypeResolutionError {
    TypeFactory(TypeError),
    Unsupported,
}

impl TypeResolutionError {
    fn message(&self) -> String {
        match self {
            TypeResolutionError::TypeFactory(error) => {
                format!("型注釈を解析できません: {error}")
            }
            TypeResolutionError::Unsupported => {
                "この型注釈は単位の基底型として解釈できません。".to_string()
            }
        }
    }
}

fn resolve_type_annotation(annotation: &TypeAnnotation) -> Result<TypeKind, TypeResolutionError> {
    match annotation {
        TypeAnnotation::Simple(name) => {
            TypeFactory::from_annotation(name).map_err(TypeResolutionError::TypeFactory)
        }
        TypeAnnotation::Nullable(inner) => resolve_type_annotation(inner).map(TypeKind::optional),
        TypeAnnotation::Unit { base, .. } => resolve_type_annotation(base),
        _ => Err(TypeResolutionError::Unsupported),
    }
}

fn capability_from_type(kind: &TypeKind) -> Option<BaseTypeCapability> {
    match kind {
        TypeKind::Primitive(primitive) => {
            if matches_integral_primitive(*primitive) {
                Some(BaseTypeCapability::Numeric(NumericCapability::Integral))
            } else {
                None
            }
        }
        TypeKind::Boxed(primitive) => {
            if matches_integral_primitive(*primitive) {
                Some(BaseTypeCapability::Numeric(NumericCapability::Integral))
            } else {
                None
            }
        }
        TypeKind::Reference(name) => {
            if is_decimal_reference(name) {
                Some(BaseTypeCapability::Numeric(NumericCapability::Decimal))
            } else if is_string_like(name) {
                Some(BaseTypeCapability::StringLike)
            } else if is_binary_like(name) {
                Some(BaseTypeCapability::Binary)
            } else if is_temporal_reference(name) {
                Some(BaseTypeCapability::Temporal)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn matches_integral_primitive(primitive: crate::JavaPrimitive) -> bool {
    matches!(
        primitive,
        crate::JavaPrimitive::Int
            | crate::JavaPrimitive::Long
            | crate::JavaPrimitive::Short
            | crate::JavaPrimitive::Byte
    )
}

fn is_decimal_reference(name: &str) -> bool {
    name.eq_ignore_ascii_case("java.math.BigDecimal")
}

fn is_string_like(name: &str) -> bool {
    matches_ignore_case(
        name,
        &[
            "java.lang.String",
            "java.lang.CharSequence",
            "java.lang.StringBuilder",
        ],
    )
}

fn is_binary_like(name: &str) -> bool {
    matches_ignore_case(
        name,
        &[
            "ByteArray",
            "kotlin.ByteArray",
            "java.nio.ByteBuffer",
            "java.lang.ByteBuffer",
        ],
    )
}

fn is_temporal_reference(name: &str) -> bool {
    let lowered = name.to_ascii_lowercase();
    lowered.starts_with("java.time.")
        || lowered.starts_with("java.time.chrono.")
        || matches_ignore_case(
            name,
            &[
                "LocalDate",
                "LocalDateTime",
                "Chronology",
                "TemporalAccessor",
            ],
        )
}

fn matches_ignore_case(candidate: &str, options: &[&str]) -> bool {
    options
        .iter()
        .any(|option| option.eq_ignore_ascii_case(candidate))
}

fn collect_default_markers(definition: &UnitDefinitionRaw) -> Vec<DefaultUnit> {
    let mut markers = Vec::new();
    if definition.symbol.has_default_marker {
        markers.push(DefaultUnit {
            symbol: definition.symbol.name.clone(),
            span: definition.symbol.span.clone(),
        });
    }

    for member in &definition.members {
        match member {
            UnitMemberRaw::SymbolDecl(symbol) => {
                if symbol.has_default_marker {
                    markers.push(DefaultUnit {
                        symbol: symbol.name.clone(),
                        span: symbol.span.clone(),
                    });
                }
            }
            UnitMemberRaw::Dependency(dependency) => {
                if dependency.symbol.has_default_marker {
                    markers.push(DefaultUnit {
                        symbol: dependency.symbol.name.clone(),
                        span: dependency.symbol.span.clone(),
                    });
                }
            }
            _ => {}
        }
    }

    markers
}

fn emit_unknown_category(category: &str, span: &Span, diagnostics: &mut Vec<CheckError>) {
    let descriptor =
        unit_semantics::descriptor("JV_UNIT_SEM_001").expect("JV_UNIT_SEM_001 must be registered");
    emit_validation_message(
        descriptor,
        format!("`{category}` は許可された単位カテゴリではありません。"),
        span,
        diagnostics,
    );
}

fn emit_custom_warning(category: &str, span: &Span, diagnostics: &mut Vec<CheckError>) {
    let descriptor =
        unit_semantics::descriptor("JV_UNIT_SEM_005").expect("JV_UNIT_SEM_005 must be registered");
    emit_validation_message(
        descriptor,
        format!(
            "`{category}` カテゴリは暫定機能です。標準カテゴリへ移行できないか確認してください。"
        ),
        span,
        diagnostics,
    );
}

fn emit_base_type_error(
    type_label: String,
    reason: String,
    span: &Span,
    diagnostics: &mut Vec<CheckError>,
) {
    let descriptor =
        unit_semantics::descriptor("JV_UNIT_SEM_002").expect("JV_UNIT_SEM_002 must be registered");
    emit_validation_message(
        descriptor,
        format!("基底型 `{type_label}` が無効です。{reason}"),
        span,
        diagnostics,
    );
}

fn emit_duplicate_default(
    category: &str,
    latest: &DefaultUnit,
    previous: &DefaultUnit,
    diagnostics: &mut Vec<CheckError>,
) {
    let descriptor =
        unit_semantics::descriptor("JV_UNIT_SEM_010").expect("JV_UNIT_SEM_010 must be registered");
    emit_validation_message(
        descriptor,
        format!(
            "カテゴリ `{category}` でデフォルト単位 `{}` と `{}` が重複しています。",
            previous.symbol, latest.symbol
        ),
        &latest.span,
        diagnostics,
    );
}

fn emit_missing_default(category: &str, span: &Span, diagnostics: &mut Vec<CheckError>) {
    let descriptor =
        unit_semantics::descriptor("JV_UNIT_SEM_011").expect("JV_UNIT_SEM_011 must be registered");
    emit_validation_message(
        descriptor,
        format!("カテゴリ `{category}` には `!` マーカー付きの単位が必要です。"),
        span,
        diagnostics,
    );
}

fn emit_validation_message(
    descriptor: &'static diagnostics::DiagnosticDescriptor,
    detail: String,
    span: &Span,
    diagnostics: &mut Vec<CheckError>,
) {
    let mut lines = vec![format!("{}: {}", descriptor.code, descriptor.title)];
    if !detail.is_empty() {
        lines.push(detail);
    }
    if !descriptor.help.is_empty() {
        lines.push(descriptor.help.to_string());
    }

    diagnostics.push(CheckError::ValidationError {
        message: lines.join("\n"),
        span: Some(span.clone()),
    });
}

fn describe_type_annotation(annotation: &TypeAnnotation) -> String {
    match annotation {
        TypeAnnotation::Simple(name) => name.clone(),
        TypeAnnotation::Nullable(inner) => format!("{}?", describe_type_annotation(inner)),
        TypeAnnotation::Generic { name, type_args } => {
            let args = type_args
                .iter()
                .map(describe_type_annotation)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{name}<{args}>")
        }
        TypeAnnotation::Function { .. } => "fn(...)".to_string(),
        TypeAnnotation::Array(inner) => format!("{}[]", describe_type_annotation(inner)),
        TypeAnnotation::Unit { base, .. } => describe_type_annotation(base),
    }
}

fn lookup_category(name: &str) -> Option<UnitCategorySpec> {
    UNIT_CATEGORY_SPECS.get(name).copied()
}

static UNIT_CATEGORY_SPECS: Lazy<HashMap<&'static str, UnitCategorySpec>> = Lazy::new(|| {
    let mut specs = HashMap::new();
    specs.insert(
        "Currency",
        UnitCategorySpec {
            name: "Currency",
            allowed_capabilities: &[
                BaseTypeCapability::Numeric(NumericCapability::Integral),
                BaseTypeCapability::Numeric(NumericCapability::Decimal),
            ],
            requires_default_marker: true,
            kind: UnitCategoryKind::Standard,
        },
    );
    specs.insert(
        "Encoding",
        UnitCategorySpec {
            name: "Encoding",
            allowed_capabilities: &[BaseTypeCapability::StringLike, BaseTypeCapability::Binary],
            requires_default_marker: true,
            kind: UnitCategoryKind::Standard,
        },
    );
    specs.insert(
        "Calendar",
        UnitCategorySpec {
            name: "Calendar",
            allowed_capabilities: &[BaseTypeCapability::Temporal],
            requires_default_marker: true,
            kind: UnitCategoryKind::Standard,
        },
    );
    specs.insert(
        "Tax",
        UnitCategorySpec {
            name: "Tax",
            allowed_capabilities: &[BaseTypeCapability::Numeric(NumericCapability::Decimal)],
            requires_default_marker: true,
            kind: UnitCategoryKind::Standard,
        },
    );
    specs.insert(
        "Custom",
        UnitCategorySpec {
            name: "Custom",
            allowed_capabilities: &[
                BaseTypeCapability::Numeric(NumericCapability::Integral),
                BaseTypeCapability::Numeric(NumericCapability::Decimal),
                BaseTypeCapability::StringLike,
                BaseTypeCapability::Binary,
                BaseTypeCapability::Temporal,
            ],
            requires_default_marker: true,
            kind: UnitCategoryKind::Custom,
        },
    );
    specs
});
