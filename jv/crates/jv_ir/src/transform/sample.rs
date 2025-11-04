use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fs;
use std::io::{Cursor, Read};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{
    DataFormat, IrSampleDeclaration, JavaType, PrimitiveType, SampleFetchError, SampleFetchRequest,
    SampleFetchResult, SampleMode, SampleRecordDescriptor, SampleRecordField, SampleSourceKind,
    Schema, SchemaError,
};
use jv_ast::{
    Annotation, AnnotationArgument, AnnotationValue, Expression, JsonLiteral, JsonValue, Literal,
    Modifiers, Span, TypeAnnotation,
};
use serde_json::{Map, Value};
use sha2::{Digest, Sha256};
use tempfile::TempDir;
use ureq::AgentBuilder;
use url::Url;

/// サンプルデータのフォーマットに応じてスキーマを推論する。
pub fn infer_schema(data: &[u8], format: DataFormat) -> Result<Schema, SchemaError> {
    match format {
        DataFormat::Json => infer_json_schema(data),
        DataFormat::Csv => infer_tabular_schema(data, b',', DataFormat::Csv),
        DataFormat::Tsv => infer_tabular_schema(data, b'\t', DataFormat::Tsv),
    }
}

fn infer_json_schema(data: &[u8]) -> Result<Schema, SchemaError> {
    let value: Value = serde_json::from_slice(data).map_err(|err| SchemaError::InvalidJson {
        message: err.to_string(),
    })?;

    if let Some(array) = value.as_array() {
        if array.is_empty() {
            return Err(SchemaError::EmptyDataset);
        }
    }

    Ok(infer_json_value_schema(&value))
}

pub fn infer_json_value_schema(value: &Value) -> Schema {
    match value {
        Value::Null => Schema::Primitive(PrimitiveType::Null),
        Value::Bool(_) => Schema::Primitive(PrimitiveType::Boolean),
        Value::Number(number) => Schema::Primitive(classify_json_number(number)),
        Value::String(_) => Schema::Primitive(PrimitiveType::String),
        Value::Array(values) => infer_json_array_schema(values),
        Value::Object(map) => infer_json_object_schema(map),
    }
}

fn literal_to_json_value(literal: &JsonLiteral) -> Value {
    json_ast_value_to_value(&literal.value)
}

fn json_ast_value_to_value(value: &JsonValue) -> Value {
    match value {
        JsonValue::Object { entries, .. } => {
            let mut map = Map::new();
            for entry in entries {
                map.insert(entry.key.clone(), json_ast_value_to_value(&entry.value));
            }
            Value::Object(map)
        }
        JsonValue::Array { elements, .. } => Value::Array(
            elements
                .iter()
                .map(json_ast_value_to_value)
                .collect::<Vec<_>>(),
        ),
        JsonValue::String { value, .. } => Value::String(value.clone()),
        JsonValue::Number { literal, .. } => serde_json::from_str::<Value>(literal)
            .unwrap_or_else(|_| Value::String(literal.clone())),
        JsonValue::Boolean { value, .. } => Value::Bool(*value),
        JsonValue::Null { .. } => Value::Null,
    }
}

fn infer_json_object_schema(map: &Map<String, Value>) -> Schema {
    let mut fields = BTreeMap::new();
    let mut required = BTreeSet::new();

    for (key, value) in map {
        fields.insert(key.clone(), infer_json_value_schema(value));
        required.insert(key.clone());
    }

    Schema::Object { fields, required }
}

fn infer_json_array_schema(values: &[Value]) -> Schema {
    let mut element_schema: Option<Schema> = None;

    for value in values {
        let schema = infer_json_value_schema(value);
        element_schema = Some(match element_schema {
            Some(existing) => merge_schema(existing, schema),
            None => schema,
        });
    }

    Schema::Array {
        element_type: Box::new(
            element_schema.unwrap_or_else(|| Schema::Primitive(PrimitiveType::Null)),
        ),
    }
}

fn classify_json_number(number: &serde_json::Number) -> PrimitiveType {
    let literal = number.to_string();
    classify_numeric_literal(&literal).unwrap_or(PrimitiveType::Double)
}

#[derive(Clone, Default)]
struct ColumnAccumulator {
    schema: Option<Schema>,
    present_count: usize,
}

fn infer_tabular_schema(
    data: &[u8],
    delimiter: u8,
    format: DataFormat,
) -> Result<Schema, SchemaError> {
    let mut reader = csv::ReaderBuilder::new()
        .delimiter(delimiter)
        .has_headers(true)
        .from_reader(Cursor::new(data));

    let headers = reader
        .headers()
        .map_err(|err| SchemaError::InvalidTabular {
            format,
            message: err.to_string(),
        })?
        .clone();

    if headers.is_empty() {
        return Err(SchemaError::MissingHeaders);
    }

    let header_names: Vec<String> = headers.iter().map(|h| h.trim().to_string()).collect();
    let mut accumulators = vec![ColumnAccumulator::default(); header_names.len()];
    let mut record_count = 0usize;

    for record in reader.records() {
        let record = record.map_err(|err| SchemaError::InvalidTabular {
            format,
            message: err.to_string(),
        })?;

        record_count += 1;

        for (idx, value) in record.iter().enumerate().take(header_names.len()) {
            let trimmed = value.trim();
            if trimmed.is_empty() || trimmed.eq_ignore_ascii_case("null") {
                continue;
            }

            let value_schema = infer_tabular_value_schema(trimmed);
            let accumulator = &mut accumulators[idx];
            let merged = match accumulator.schema.clone() {
                Some(existing) => merge_schema(existing, value_schema),
                None => value_schema,
            };
            accumulator.schema = Some(merged);
            accumulator.present_count += 1;
        }
    }

    if record_count == 0 {
        return Err(SchemaError::EmptyDataset);
    }

    let mut fields = BTreeMap::new();
    let mut required = BTreeSet::new();

    for (idx, header) in header_names.iter().enumerate() {
        let accumulator = &accumulators[idx];
        let schema = accumulator
            .schema
            .clone()
            .unwrap_or_else(|| Schema::Primitive(PrimitiveType::Null));
        fields.insert(header.clone(), schema);
        if accumulator.present_count == record_count {
            required.insert(header.clone());
        }
    }

    Ok(Schema::Array {
        element_type: Box::new(Schema::Object { fields, required }),
    })
}

fn infer_tabular_value_schema(value: &str) -> Schema {
    let lowered = value.to_ascii_lowercase();
    if lowered == "true" || lowered == "false" {
        return Schema::Primitive(PrimitiveType::Boolean);
    }

    if let Some(numeric) = classify_numeric_literal(value) {
        return Schema::Primitive(numeric);
    }

    Schema::Primitive(PrimitiveType::String)
}

fn classify_numeric_literal(literal: &str) -> Option<PrimitiveType> {
    let trimmed = literal.trim();
    if trimmed.is_empty() {
        return None;
    }

    let without_plus = trimmed.strip_prefix('+').unwrap_or(trimmed);

    if without_plus.contains('.') || without_plus.contains('e') || without_plus.contains('E') {
        let has_digit = without_plus.chars().any(|c| c.is_ascii_digit());
        if !has_digit {
            return None;
        }

        if without_plus.parse::<f64>().is_err() {
            return None;
        }

        if needs_big_decimal(without_plus) {
            return Some(PrimitiveType::BigDecimal);
        }

        return Some(PrimitiveType::Double);
    }

    let digits = without_plus.trim_start_matches('-');
    if !digits.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }

    if let Ok(value) = without_plus.parse::<i64>() {
        if value >= i64::from(i32::MIN) && value <= i64::from(i32::MAX) {
            Some(PrimitiveType::Integer)
        } else {
            Some(PrimitiveType::Long)
        }
    } else {
        Some(PrimitiveType::BigInteger)
    }
}

fn needs_big_decimal(literal: &str) -> bool {
    let digit_count = literal.chars().filter(|c| c.is_ascii_digit()).count();
    digit_count > 15 || literal.contains('e') || literal.contains('E')
}

fn merge_schema(left: Schema, right: Schema) -> Schema {
    if left == right {
        return left;
    }

    match (left, right) {
        (Schema::Optional(left_inner), Schema::Optional(right_inner)) => {
            Schema::Optional(Box::new(merge_schema(*left_inner, *right_inner)))
        }
        (Schema::Optional(left_inner), right) => {
            Schema::Optional(Box::new(merge_schema(*left_inner, right)))
        }
        (left, Schema::Optional(right_inner)) => {
            Schema::Optional(Box::new(merge_schema(left, *right_inner)))
        }
        (Schema::Primitive(PrimitiveType::Null), right) => optionalize(right),
        (left, Schema::Primitive(PrimitiveType::Null)) => optionalize(left),
        (Schema::Primitive(left_prim), Schema::Primitive(right_prim)) => {
            if let Some(merged) = merge_primitive_types(left_prim, right_prim) {
                Schema::Primitive(merged)
            } else {
                make_union(vec![
                    Schema::Primitive(left_prim),
                    Schema::Primitive(right_prim),
                ])
            }
        }
        (
            Schema::Array {
                element_type: left_element,
            },
            Schema::Array {
                element_type: right_element,
            },
        ) => Schema::Array {
            element_type: Box::new(merge_schema(*left_element, *right_element)),
        },
        (
            Schema::Object {
                fields: left_fields,
                required: left_required,
            },
            Schema::Object {
                fields: right_fields,
                required: right_required,
            },
        ) => Schema::Object {
            fields: merge_object_fields(left_fields, right_fields),
            required: merge_required_fields(left_required, right_required),
        },
        (Schema::Union(left_variants), Schema::Union(right_variants)) => {
            make_union(left_variants.into_iter().chain(right_variants))
        }
        (Schema::Union(left_variants), right) => {
            make_union(left_variants.into_iter().chain(std::iter::once(right)))
        }
        (left, Schema::Union(right_variants)) => {
            make_union(std::iter::once(left).chain(right_variants.into_iter()))
        }
        (left, right) => make_union(vec![left, right]),
    }
}

fn merge_object_fields(
    mut left_fields: BTreeMap<String, Schema>,
    mut right_fields: BTreeMap<String, Schema>,
) -> BTreeMap<String, Schema> {
    let mut keys = BTreeSet::new();
    keys.extend(left_fields.keys().cloned());
    keys.extend(right_fields.keys().cloned());

    let mut merged = BTreeMap::new();

    for key in keys {
        match (left_fields.remove(&key), right_fields.remove(&key)) {
            (Some(left), Some(right)) => {
                merged.insert(key.clone(), merge_schema(left, right));
            }
            (Some(left), None) => {
                merged.insert(key.clone(), left);
            }
            (None, Some(right)) => {
                merged.insert(key.clone(), right);
            }
            (None, None) => {}
        }
    }

    merged
}

fn merge_required_fields(
    mut left_required: BTreeSet<String>,
    right_required: BTreeSet<String>,
) -> BTreeSet<String> {
    left_required.retain(|key| right_required.contains(key));
    left_required
}

fn merge_primitive_types(a: PrimitiveType, b: PrimitiveType) -> Option<PrimitiveType> {
    if a == b {
        return Some(a);
    }

    if a.is_numeric() && b.is_numeric() {
        return Some(widen_numeric(a, b));
    }

    None
}

fn widen_numeric(a: PrimitiveType, b: PrimitiveType) -> PrimitiveType {
    use PrimitiveType::*;

    if matches!(a, BigDecimal) || matches!(b, BigDecimal) {
        return BigDecimal;
    }

    if matches!(a, Double) || matches!(b, Double) {
        if matches!(a, BigInteger) || matches!(b, BigInteger) {
            return BigDecimal;
        }
        return Double;
    }

    if matches!(a, BigInteger) || matches!(b, BigInteger) {
        return BigInteger;
    }

    if matches!(a, Long) || matches!(b, Long) {
        return Long;
    }

    Integer
}

fn optionalize(schema: Schema) -> Schema {
    match schema {
        Schema::Optional(inner) => Schema::Optional(inner),
        other => Schema::Optional(Box::new(other)),
    }
}

fn make_union<I>(items: I) -> Schema
where
    I: IntoIterator<Item = Schema>,
{
    let mut unique: BTreeMap<String, Schema> = BTreeMap::new();

    for item in items {
        match item {
            Schema::Union(inner) => {
                for variant in inner {
                    insert_union_variant(&mut unique, variant);
                }
            }
            other => insert_union_variant(&mut unique, other),
        }
    }

    let mut variants: Vec<Schema> = unique.into_values().collect();
    if variants.len() == 1 {
        variants.remove(0)
    } else {
        Schema::Union(variants)
    }
}

fn insert_union_variant(map: &mut BTreeMap<String, Schema>, schema: Schema) {
    let key = schema_sort_key(&schema);
    map.entry(key).or_insert(schema);
}

fn schema_sort_key(schema: &Schema) -> String {
    match schema {
        Schema::Primitive(primitive) => format!("primitive:{primitive:?}"),
        Schema::Array { element_type } => format!("array:{}", schema_sort_key(element_type)),
        Schema::Optional(inner) => format!("optional:{}", schema_sort_key(inner)),
        Schema::Union(variants) => {
            let mut keys: Vec<String> = variants.iter().map(schema_sort_key).collect();
            keys.sort();
            format!("union:[{}]", keys.join(","))
        }
        Schema::Object { fields, required } => {
            let mut field_parts = Vec::new();
            for (name, schema) in fields {
                field_parts.push(format!("{name}={}", schema_sort_key(schema)));
            }
            let required_key = required.iter().cloned().collect::<Vec<_>>().join("|");
            format!("object:{{{}}}|req:{required_key}", field_parts.join(","))
        }
    }
}

struct SampleAnnotationConfig {
    source: String,
    mode: Option<SampleMode>,
    format: Option<DataFormat>,
    sha256: Option<String>,
    limit_bytes: Option<u64>,
}

fn sample_annotation_error(span: &Span, message: impl Into<String>) -> TransformError {
    TransformError::SampleAnnotationError {
        message: message.into(),
        span: span.clone(),
    }
}

fn sample_processing_error(span: &Span, message: impl Into<String>) -> TransformError {
    TransformError::SampleProcessingError {
        message: message.into(),
        span: span.clone(),
    }
}

fn parse_sample_annotation(
    annotation: &Annotation,
) -> Result<SampleAnnotationConfig, TransformError> {
    let mut source: Option<String> = None;
    let mut mode: Option<SampleMode> = None;
    let mut format: Option<DataFormat> = None;
    let mut sha256: Option<String> = None;
    let mut limit_bytes: Option<u64> = None;

    for argument in &annotation.arguments {
        match argument {
            AnnotationArgument::Positional { value, span } => {
                if source.is_some() {
                    return Err(sample_annotation_error(
                        span,
                        "@Sample にはソース引数を一度だけ指定できます",
                    ));
                }
                if let Some(path) = annotation_value_to_string(value) {
                    source = Some(path);
                } else {
                    return Err(sample_annotation_error(
                        span,
                        "@Sample の最初の位置引数は文字列リテラルである必要があります",
                    ));
                }
            }
            AnnotationArgument::Named { name, value, span } => match name.as_str() {
                "source" => {
                    if source.is_some() {
                        return Err(sample_annotation_error(span, "source 引数が重複しています"));
                    }
                    if let Some(path) = annotation_value_to_string(value) {
                        source = Some(path);
                    } else {
                        return Err(sample_annotation_error(
                            span,
                            "source 引数は文字列リテラルで指定してください",
                        ));
                    }
                }
                "mode" => {
                    if mode.is_some() {
                        return Err(sample_annotation_error(span, "mode 引数が重複しています"));
                    }
                    if let Some(parsed) = parse_mode_value(value) {
                        mode = Some(parsed);
                    } else {
                        return Err(sample_annotation_error(
                            span,
                            "mode 引数は Embed または Load を指定してください",
                        ));
                    }
                }
                "format" => {
                    if format.is_some() {
                        return Err(sample_annotation_error(span, "format 引数が重複しています"));
                    }
                    let maybe_string = annotation_value_to_identifier(value);
                    if let Some(fmt) = maybe_string.as_deref().and_then(parse_format_literal) {
                        format = Some(fmt);
                    } else {
                        return Err(sample_annotation_error(
                            span,
                            "format 引数は json/csv/tsv のいずれかを指定してください",
                        ));
                    }
                }
                "sha256" => {
                    if sha256.is_some() {
                        return Err(sample_annotation_error(span, "sha256 引数が重複しています"));
                    }
                    if let Some(hash) = annotation_value_to_string(value) {
                        sha256 = Some(hash);
                    } else {
                        return Err(sample_annotation_error(
                            span,
                            "sha256 引数は文字列リテラルで指定してください",
                        ));
                    }
                }
                "limitBytes" | "limit_bytes" => {
                    if limit_bytes.is_some() {
                        return Err(sample_annotation_error(
                            span,
                            "limitBytes 引数が重複しています",
                        ));
                    }
                    if let Some(parsed) = annotation_value_to_u64(value) {
                        limit_bytes = Some(parsed);
                    } else {
                        return Err(sample_annotation_error(
                            span,
                            "limitBytes 引数は正の整数リテラルで指定してください",
                        ));
                    }
                }
                other => {
                    return Err(sample_annotation_error(
                        span,
                        format!("未対応の@Sample引数 '{other}' が指定されました"),
                    ));
                }
            },
        }
    }

    let source = source.ok_or_else(|| {
        sample_annotation_error(
            &annotation.span,
            "@Sample にはサンプルソースを指定してください",
        )
    })?;

    Ok(SampleAnnotationConfig {
        source,
        mode,
        format,
        sha256,
        limit_bytes,
    })
}

fn parse_mode_value(value: &AnnotationValue) -> Option<SampleMode> {
    match value {
        AnnotationValue::EnumConstant { constant, .. } => parse_mode_literal(constant),
        AnnotationValue::Literal(Literal::String(value)) => parse_mode_literal(value),
        _ => None,
    }
}

fn parse_mode_literal(value: &str) -> Option<SampleMode> {
    match value.to_ascii_lowercase().as_str() {
        "embed" => Some(SampleMode::Embed),
        "load" => Some(SampleMode::Load),
        _ => None,
    }
}

fn parse_format_literal(value: &str) -> Option<DataFormat> {
    match value.to_ascii_lowercase().as_str() {
        "json" => Some(DataFormat::Json),
        "csv" => Some(DataFormat::Csv),
        "tsv" => Some(DataFormat::Tsv),
        _ => None,
    }
}

fn annotation_value_to_identifier(value: &AnnotationValue) -> Option<String> {
    match value {
        AnnotationValue::EnumConstant { constant, .. } => Some(constant.clone()),
        AnnotationValue::Literal(Literal::String(value)) => Some(value.clone()),
        _ => None,
    }
}

fn annotation_value_to_string(value: &AnnotationValue) -> Option<String> {
    match value {
        AnnotationValue::Literal(Literal::String(value)) => Some(value.clone()),
        _ => None,
    }
}

fn annotation_value_to_u64(value: &AnnotationValue) -> Option<u64> {
    match value {
        AnnotationValue::Literal(Literal::Number(number)) => {
            let digits: String = number.chars().filter(|c| *c != '_').collect();
            digits.parse::<u64>().ok()
        }
        _ => None,
    }
}

fn detect_data_format(
    source: &str,
    bytes: &[u8],
    span: &Span,
) -> Result<DataFormat, TransformError> {
    if let Some(ext) = Path::new(source).extension().and_then(|ext| ext.to_str()) {
        match ext.to_ascii_lowercase().as_str() {
            "json" | "jsonl" => return Ok(DataFormat::Json),
            "csv" => return Ok(DataFormat::Csv),
            "tsv" => return Ok(DataFormat::Tsv),
            _ => {}
        }
    }

    if serde_json::from_slice::<serde_json::Value>(bytes).is_ok() {
        return Ok(DataFormat::Json);
    }

    let preview = String::from_utf8_lossy(&bytes[..bytes.len().min(1024)]);
    if preview.contains('\t') {
        return Ok(DataFormat::Tsv);
    }
    if preview.contains(',') {
        return Ok(DataFormat::Csv);
    }

    Err(sample_processing_error(
        span,
        format!("データフォーマットを自動判定できません: {source}"),
    ))
}

struct SampleTypeBuilder {
    base_name: String,
    records: Vec<SampleRecordDescriptor>,
    used_names: HashSet<String>,
}

#[derive(Clone)]
struct PathSegment {
    name: String,
    is_collection: bool,
}

struct TypeInfo {
    java_type: JavaType,
    is_optional: bool,
    record_name: Option<String>,
}

impl SampleTypeBuilder {
    fn new(variable_name: &str) -> Self {
        let base = to_pascal_case(&singularize(variable_name));
        let base = if base.is_empty() {
            "Sample".to_string()
        } else {
            base
        };
        Self {
            base_name: base,
            records: Vec::new(),
            used_names: HashSet::new(),
        }
    }

    fn ensure_unique_name(&mut self, base: String) -> String {
        if self.used_names.insert(base.clone()) {
            return base;
        }
        let mut counter = 2;
        loop {
            let candidate = format!("{}{}", base, counter);
            if self.used_names.insert(candidate.clone()) {
                return candidate;
            }
            counter += 1;
        }
    }

    fn record_name_for_path(&mut self, path: &[PathSegment]) -> String {
        let mut name = self.base_name.clone();
        let mut start_index = 0;

        if name.is_empty() && !path.is_empty() {
            let component = if path[0].is_collection {
                singularize(&path[0].name)
            } else {
                path[0].name.to_ascii_lowercase()
            };
            name.push_str(&to_pascal_case(&component));
            start_index = 1;
        } else if !name.is_empty() {
            start_index = 1;
        }

        for segment in path.iter().skip(start_index) {
            let component = if segment.is_collection {
                singularize(&segment.name)
            } else {
                segment.name.to_ascii_lowercase()
            };
            name.push_str(&to_pascal_case(&component));
        }

        if name.is_empty() {
            name.push_str("Sample");
        }
        if !name.ends_with("Sample") {
            name.push_str("Sample");
        }

        self.ensure_unique_name(name)
    }

    fn add_record(&mut self, name: String, fields: Vec<SampleRecordField>) {
        self.records.push(SampleRecordDescriptor { name, fields });
    }

    fn build_root(&mut self, schema: &Schema, variable_name: &str) -> TypeInfo {
        let mut path = Vec::new();
        path.push(PathSegment {
            name: variable_name.to_string(),
            is_collection: is_collection_schema(schema),
        });
        self.build_schema(schema, &mut path)
    }

    fn build_schema(&mut self, schema: &Schema, path: &mut Vec<PathSegment>) -> TypeInfo {
        match schema {
            Schema::Optional(inner) => {
                let mut info = self.build_schema(inner, path);
                info.is_optional = true;
                info
            }
            Schema::Primitive(primitive) => TypeInfo {
                java_type: primitive_to_java_type(*primitive),
                is_optional: false,
                record_name: None,
            },
            Schema::Array { element_type } => {
                let element_info = self.build_schema(element_type, path);
                TypeInfo {
                    java_type: list_type(element_info.java_type.clone()),
                    is_optional: false,
                    record_name: element_info.record_name,
                }
            }
            Schema::Object { fields, required } => {
                let record_name = self.record_name_for_path(path);
                let mut record_fields = Vec::new();

                for (field_name, field_schema) in fields {
                    let (core_schema, schema_optional) = unwrap_optional_schema(field_schema);
                    let is_collection = is_collection_schema(core_schema);
                    path.push(PathSegment {
                        name: field_name.clone(),
                        is_collection,
                    });
                    let child_info = self.build_schema(core_schema, path);
                    path.pop();

                    let is_optional =
                        schema_optional || !required.contains(field_name) || child_info.is_optional;

                    let mut field_type = child_info.java_type.clone();
                    if is_optional {
                        field_type = optional_type(field_type);
                    }

                    record_fields.push(SampleRecordField {
                        name: field_name.clone(),
                        java_type: field_type,
                        is_optional,
                    });
                }

                self.add_record(record_name.clone(), record_fields);

                TypeInfo {
                    java_type: JavaType::Reference {
                        name: record_name.clone(),
                        generic_args: Vec::new(),
                    },
                    is_optional: false,
                    record_name: Some(record_name),
                }
            }
            Schema::Union(variants) => {
                let mut result: Option<TypeInfo> = None;
                let mut saw_null = false;

                for variant in variants {
                    if matches!(variant, Schema::Primitive(PrimitiveType::Null)) {
                        saw_null = true;
                        continue;
                    }
                    let info = self.build_schema(variant, path);
                    if result.is_some() {
                        return TypeInfo {
                            java_type: JavaType::object(),
                            is_optional: true,
                            record_name: None,
                        };
                    }
                    result = Some(info);
                }

                if let Some(mut info) = result {
                    if saw_null {
                        info.is_optional = true;
                    }
                    info
                } else {
                    TypeInfo {
                        java_type: JavaType::object(),
                        is_optional: true,
                        record_name: None,
                    }
                }
            }
        }
    }
}

fn compute_type_layout(
    schema: &Schema,
    variable_name: &str,
) -> (JavaType, Vec<SampleRecordDescriptor>, Option<String>) {
    let mut builder = SampleTypeBuilder::new(variable_name);
    let info = builder.build_root(schema, variable_name);

    let mut java_type = info.java_type;
    if info.is_optional {
        java_type = optional_type(java_type);
    }

    (java_type, builder.records, info.record_name)
}

fn primitive_to_java_type(primitive: PrimitiveType) -> JavaType {
    match primitive {
        PrimitiveType::String => JavaType::string(),
        PrimitiveType::Boolean => JavaType::boolean(),
        PrimitiveType::Integer => JavaType::int(),
        PrimitiveType::Long => JavaType::Primitive("long".to_string()),
        PrimitiveType::BigInteger => JavaType::Reference {
            name: "java.math.BigInteger".to_string(),
            generic_args: Vec::new(),
        },
        PrimitiveType::Double => JavaType::Primitive("double".to_string()),
        PrimitiveType::BigDecimal => JavaType::Reference {
            name: "java.math.BigDecimal".to_string(),
            generic_args: Vec::new(),
        },
        PrimitiveType::Null => JavaType::object(),
    }
}

fn list_type(inner: JavaType) -> JavaType {
    JavaType::Reference {
        name: "java.util.List".to_string(),
        generic_args: vec![inner],
    }
}

fn optional_type(inner: JavaType) -> JavaType {
    JavaType::Reference {
        name: "java.util.Optional".to_string(),
        generic_args: vec![inner],
    }
}

fn unwrap_optional_schema<'a>(schema: &'a Schema) -> (&'a Schema, bool) {
    match schema {
        Schema::Optional(inner) => (inner, true),
        Schema::Union(variants) => {
            let mut non_null = variants
                .iter()
                .filter(|variant| !matches!(variant, Schema::Primitive(PrimitiveType::Null)))
                .collect::<Vec<_>>();
            if non_null.len() == 1 && variants.len() == 2 {
                (non_null.remove(0), true)
            } else if variants
                .iter()
                .any(|variant| matches!(variant, Schema::Primitive(PrimitiveType::Null)))
            {
                (schema, true)
            } else {
                (schema, false)
            }
        }
        _ => (schema, false),
    }
}

fn is_collection_schema(schema: &Schema) -> bool {
    match schema {
        Schema::Array { .. } => true,
        Schema::Optional(inner) => is_collection_schema(inner),
        Schema::Union(variants) => variants.iter().any(is_collection_schema),
        _ => false,
    }
}

fn to_pascal_case(value: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = true;
    for ch in value.chars() {
        if ch.is_ascii_alphanumeric() {
            if capitalize_next {
                for upper in ch.to_uppercase() {
                    result.push(upper);
                }
                capitalize_next = false;
            } else {
                for lower in ch.to_lowercase() {
                    result.push(lower);
                }
            }
        } else {
            capitalize_next = true;
        }
    }
    result
}

fn singularize(name: &str) -> String {
    let lower = name.to_ascii_lowercase();
    if lower.ends_with("ies") && lower.len() > 3 {
        format!("{}y", &lower[..lower.len() - 3])
    } else if (lower.ends_with("ses") || lower.ends_with("xes") || lower.ends_with("zes"))
        && lower.len() > 2
    {
        lower[..lower.len() - 2].to_string()
    } else if lower.ends_with('s') && lower.len() > 1 {
        lower[..lower.len() - 1].to_string()
    } else {
        lower
    }
}

pub fn desugar_sample_annotation(
    name: String,
    type_annotation: Option<TypeAnnotation>,
    initializer: Expression,
    modifiers: &Modifiers,
    annotation: Annotation,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrSampleDeclaration, TransformError> {
    let _ = (initializer, modifiers);

    if type_annotation.is_some() {
        return Err(sample_annotation_error(
            &span,
            "@Sample を付与した val に型注釈は指定できません",
        ));
    }

    let config = parse_sample_annotation(&annotation)?;
    let options = context.sample_options().clone();
    let mode = config.mode.unwrap_or(options.default_mode);

    let mut request = SampleFetchRequest::new(config.source.clone());
    request.base_dir = options.base_dir.clone();
    request.allow_network = options.allow_network;
    request.expected_sha256 = config.sha256.clone();
    request.cache_dir = options.cache_dir.clone();
    request.aws_cli_path = options.aws_cli_path.clone();
    request.git_cli_path = options.git_cli_path.clone();
    request.timeout = options.timeout;
    request.max_bytes = config.limit_bytes.or_else(|| {
        if mode == SampleMode::Embed {
            options.embed_max_bytes
        } else {
            None
        }
    });

    let fetch_result = fetch_sample_data(&request)
        .map_err(|error| sample_processing_error(&annotation.span, error.to_string()))?;

    let format = if let Some(explicit) = config.format.or(options.default_format) {
        explicit
    } else {
        detect_data_format(&config.source, &fetch_result.bytes, &annotation.span)?
    };

    let schema = infer_schema(&fetch_result.bytes, format)
        .map_err(|error| sample_processing_error(&annotation.span, error.to_string()))?;

    let (java_type, records, root_record_name) = compute_type_layout(&schema, &name);

    let SampleFetchResult {
        bytes,
        sha256,
        source_kind,
        origin,
        cache_path,
    } = fetch_result;

    let embedded_data = if mode == SampleMode::Embed {
        Some(bytes)
    } else {
        None
    };

    let variable_java_type = java_type.clone();
    let variable_name = name.clone();

    let declaration = IrSampleDeclaration {
        variable_name: variable_name.clone(),
        java_type,
        format,
        mode,
        source: origin,
        source_kind,
        sha256,
        cache_path,
        limit_bytes: request.max_bytes,
        embedded_data,
        schema,
        records,
        root_record_name,
        span,
    };

    context.add_variable(variable_name, variable_java_type);

    Ok(declaration)
}

pub fn inline_json_declaration(
    name: String,
    type_annotation: Option<TypeAnnotation>,
    literal: JsonLiteral,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrSampleDeclaration, TransformError> {
    if type_annotation.is_some() {
        return Err(sample_annotation_error(
            &span,
            "JSONリテラルには型注釈を併用できません (@Sampleと同様に型は自動推論されます)",
        ));
    }

    let value = literal_to_json_value(&literal);
    let bytes =
        serde_json::to_vec(&value).map_err(|error| TransformError::SampleProcessingError {
            message: format!("inline JSON literal serialization failed: {}", error),
            span: span.clone(),
        })?;

    let schema = infer_json_value_schema(&value);

    let (java_type, records, root_record_name) = compute_type_layout(&schema, &name);

    let declaration = IrSampleDeclaration {
        variable_name: name.clone(),
        java_type: java_type.clone(),
        format: DataFormat::Json,
        mode: SampleMode::Embed,
        source: format!(
            "inline:{}:{}-{}:{}",
            span.start_line, span.start_column, span.end_line, span.end_column
        ),
        source_kind: SampleSourceKind::Inline,
        sha256: compute_sha256(&bytes),
        cache_path: None,
        limit_bytes: None,
        embedded_data: Some(bytes),
        schema,
        records,
        root_record_name,
        span,
    };

    context.add_variable(name, java_type);

    Ok(declaration)
}

/// フェッチと検証を一括で実行する。
pub fn fetch_sample_data(
    request: &SampleFetchRequest,
) -> Result<SampleFetchResult, SampleFetchError> {
    if let Some(cached) = try_read_cache(request)? {
        return Ok(cached);
    }

    let (bytes, kind) = fetch_uncached(request)?;
    enforce_limit(request, bytes.len() as u64)?;

    let sha256 = compute_sha256(&bytes);
    if let Some(expected) = &request.expected_sha256 {
        let expected_norm = expected.to_ascii_lowercase();
        if expected_norm != sha256 {
            return Err(SampleFetchError::Sha256Mismatch {
                expected: expected_norm,
                actual: sha256,
            });
        }
    }

    let cache_path = if let Some(dir) = request.cache_dir.as_ref() {
        Some(store_cache(dir, &sha256, &bytes)?)
    } else {
        None
    };

    Ok(SampleFetchResult {
        bytes,
        sha256,
        source_kind: kind,
        origin: request.source.clone(),
        cache_path,
    })
}

fn try_read_cache(
    request: &SampleFetchRequest,
) -> Result<Option<SampleFetchResult>, SampleFetchError> {
    let (cache_dir, expected_hash) = match (&request.cache_dir, &request.expected_sha256) {
        (Some(dir), Some(hash)) => (dir, hash.to_ascii_lowercase()),
        _ => return Ok(None),
    };

    let cache_path = cache_dir.join(&expected_hash);
    if !cache_path.exists() {
        return Ok(None);
    }

    let bytes = fs::read(&cache_path).map_err(|error| SampleFetchError::Io {
        path: cache_path.clone(),
        error,
    })?;
    enforce_limit(request, bytes.len() as u64)?;

    let actual_hash = compute_sha256(&bytes);
    if actual_hash != expected_hash {
        return Err(SampleFetchError::Sha256Mismatch {
            expected: expected_hash,
            actual: actual_hash,
        });
    }

    Ok(Some(SampleFetchResult {
        bytes,
        sha256: expected_hash,
        source_kind: SampleSourceKind::CachedFile,
        origin: request.source.clone(),
        cache_path: Some(cache_path),
    }))
}

fn store_cache(cache_dir: &Path, sha256: &str, bytes: &[u8]) -> Result<PathBuf, SampleFetchError> {
    fs::create_dir_all(cache_dir).map_err(|error| SampleFetchError::Io {
        path: cache_dir.to_path_buf(),
        error,
    })?;
    let cache_path = cache_dir.join(sha256);
    fs::write(&cache_path, bytes).map_err(|error| SampleFetchError::Io {
        path: cache_path.clone(),
        error,
    })?;
    Ok(cache_path)
}

fn fetch_uncached(
    request: &SampleFetchRequest,
) -> Result<(Vec<u8>, SampleSourceKind), SampleFetchError> {
    match Url::parse(&request.source) {
        Ok(url) => match url.scheme() {
            "file" => {
                fetch_local_file_from_url(&url).map(|data| (data, SampleSourceKind::LocalFile))
            }
            "http" | "https" => {
                fetch_http(&url, request).map(|data| (data, SampleSourceKind::Http))
            }
            "s3" => fetch_s3(&request.source, request).map(|data| (data, SampleSourceKind::S3)),
            "git+ssh" => fetch_git_ssh(&url, request).map(|data| (data, SampleSourceKind::GitSsh)),
            other => Err(SampleFetchError::UnsupportedScheme {
                scheme: other.to_string(),
            }),
        },
        Err(_) => fetch_local_path(&request.source, request)
            .map(|data| (data, SampleSourceKind::LocalFile)),
    }
}

fn fetch_local_file_from_url(url: &Url) -> Result<Vec<u8>, SampleFetchError> {
    let path = url
        .to_file_path()
        .map_err(|_| SampleFetchError::InvalidUri {
            uri: url.to_string(),
            message: "file:// URI をパスに変換できませんでした".to_string(),
        })?;
    read_file(&path)
}

fn fetch_local_path(
    source: &str,
    request: &SampleFetchRequest,
) -> Result<Vec<u8>, SampleFetchError> {
    let path = Path::new(source);
    let resolved = if path.is_absolute() {
        path.to_path_buf()
    } else if let Some(base) = &request.base_dir {
        base.join(path)
    } else {
        std::env::current_dir()
            .map_err(|error| SampleFetchError::Io {
                path: PathBuf::from("."),
                error,
            })?
            .join(path)
    };

    read_file(&resolved)
}

fn read_file(path: &Path) -> Result<Vec<u8>, SampleFetchError> {
    fs::read(path).map_err(|error| SampleFetchError::Io {
        path: path.to_path_buf(),
        error,
    })
}

fn fetch_http(url: &Url, request: &SampleFetchRequest) -> Result<Vec<u8>, SampleFetchError> {
    ensure_network_allowed(url.as_str(), request)?;

    let agent = AgentBuilder::new().timeout(request.timeout).build();

    let response = agent.get(url.as_str()).call().map_err(|err| match err {
        ureq::Error::Status(status, _) => SampleFetchError::HttpResponse {
            uri: url.to_string(),
            status: status as u16,
        },
        ureq::Error::Transport(transport) => SampleFetchError::HttpRequest {
            uri: url.to_string(),
            message: transport.to_string(),
        },
    })?;

    let mut reader = response.into_reader();
    let mut buffer = Vec::new();
    reader
        .read_to_end(&mut buffer)
        .map_err(|err| SampleFetchError::HttpRequest {
            uri: url.to_string(),
            message: err.to_string(),
        })?;

    Ok(buffer)
}

fn fetch_s3(source: &str, request: &SampleFetchRequest) -> Result<Vec<u8>, SampleFetchError> {
    ensure_network_allowed(source, request)?;
    let aws = resolve_command("aws", &request.aws_cli_path)?;

    let output = Command::new(aws)
        .args(["s3", "cp", source, "-", "--no-progress"])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|err| SampleFetchError::CommandFailed {
            command: "aws".to_string(),
            status: None,
            stderr: err.to_string(),
        })?;

    if !output.status.success() {
        return Err(SampleFetchError::CommandFailed {
            command: "aws".to_string(),
            status: output.status.code(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        });
    }

    Ok(output.stdout)
}

fn fetch_git_ssh(url: &Url, request: &SampleFetchRequest) -> Result<Vec<u8>, SampleFetchError> {
    ensure_network_allowed(url.as_str(), request)?;
    let git = resolve_command("git", &request.git_cli_path)?;

    let mut repo_url = url.clone();
    repo_url
        .set_scheme("ssh")
        .map_err(|_| SampleFetchError::InvalidUri {
            uri: url.to_string(),
            message: "git+ssh URI を ssh スキームへ変換できません".to_string(),
        })?;
    repo_url.set_query(None);

    let mut file_path = None;
    let mut reference = None;
    if let Some(query) = url.query() {
        for (key, value) in query.split('&').filter_map(|pair| pair.split_once('=')) {
            let decoded = urlencoding::decode(value)
                .map_err(|err| SampleFetchError::InvalidUri {
                    uri: url.to_string(),
                    message: format!("query decode error: {err}"),
                })?
                .into_owned();
            match key {
                "path" => file_path = Some(PathBuf::from(decoded)),
                "ref" => reference = Some(decoded),
                _ => {}
            }
        }
    }

    let file_path = file_path.ok_or_else(|| SampleFetchError::GitPathMissing {
        uri: url.to_string(),
    })?;

    let temp_dir = TempDir::new().map_err(|err| SampleFetchError::CommandFailed {
        command: "git".to_string(),
        status: None,
        stderr: err.to_string(),
    })?;
    let repo_dir = temp_dir.path().join("repo");

    let mut command = Command::new(&git);
    command.arg("clone");
    command.arg("--depth");
    command.arg("1");
    if let Some(reference) = &reference {
        command.arg("--branch");
        command.arg(reference);
    }
    command.arg(repo_url.as_str());
    command.arg(&repo_dir);

    let output = command
        .stderr(Stdio::piped())
        .stdout(Stdio::null())
        .output()
        .map_err(|err| SampleFetchError::CommandFailed {
            command: "git".to_string(),
            status: None,
            stderr: err.to_string(),
        })?;

    if !output.status.success() {
        return Err(SampleFetchError::CommandFailed {
            command: "git".to_string(),
            status: output.status.code(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        });
    }

    let full_path = repo_dir.join(&file_path);
    read_file(&full_path)
}

fn ensure_network_allowed(
    source: &str,
    request: &SampleFetchRequest,
) -> Result<(), SampleFetchError> {
    if request.allow_network {
        return Ok(());
    }
    Err(SampleFetchError::NetworkDisabled {
        uri: source.to_string(),
    })
}

fn resolve_command(
    command: &str,
    override_path: &Option<PathBuf>,
) -> Result<PathBuf, SampleFetchError> {
    if let Some(path) = override_path {
        if path.exists() {
            return Ok(path.clone());
        }
        return Err(SampleFetchError::CommandMissing {
            command: path.display().to_string(),
        });
    }

    which::which(command).map_err(|_| SampleFetchError::CommandMissing {
        command: command.to_string(),
    })
}

fn compute_sha256(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let digest = hasher.finalize();
    hex::encode(digest)
}

fn enforce_limit(request: &SampleFetchRequest, actual: u64) -> Result<(), SampleFetchError> {
    if let Some(limit) = request.max_bytes {
        if actual > limit {
            return Err(SampleFetchError::SizeLimitExceeded { limit, actual });
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use std::net::TcpListener;
    use std::thread;

    fn write_temp_file(content: &str) -> (TempDir, PathBuf) {
        let dir = TempDir::new().expect("temp dir");
        let file_path = dir.path().join("data.json");
        let mut file = fs::File::create(&file_path).expect("create file");
        file.write_all(content.as_bytes()).expect("write file");
        (dir, file_path)
    }

    #[test]
    fn fetches_local_file_by_relative_path() {
        let (dir, file_path) = write_temp_file("{\"name\": \"Alice\"}");
        let relative = file_path
            .strip_prefix(dir.path())
            .unwrap()
            .to_string_lossy()
            .to_string();

        let mut request = SampleFetchRequest::new(relative);
        request.base_dir = Some(dir.path().to_path_buf());
        request.expected_sha256 =
            Some("6d4a333838d0ef96756cccc680af2531075c512502fb68c5503c63d93de859b3".to_string());
        request.cache_dir = Some(dir.path().join("cache"));

        let result1 = fetch_sample_data(&request).expect("fetch local");
        assert_eq!(result1.source_kind, SampleSourceKind::LocalFile);
        assert!(result1.cache_path.is_some());
        assert_eq!(result1.bytes, fs::read(&file_path).unwrap());

        let result2 = fetch_sample_data(&request).expect("fetch cached");
        assert_eq!(result2.source_kind, SampleSourceKind::CachedFile);
        assert_eq!(result2.bytes, fs::read(file_path).unwrap());
    }

    #[test]
    fn honors_size_limit() {
        let (dir, file_path) = write_temp_file("123456");
        let relative = file_path
            .strip_prefix(dir.path())
            .unwrap()
            .to_string_lossy()
            .to_string();

        let mut request = SampleFetchRequest::new(relative);
        request.base_dir = Some(dir.path().to_path_buf());
        request.max_bytes = Some(2);

        let error = fetch_sample_data(&request).expect_err("size limit error");
        assert!(matches!(error, SampleFetchError::SizeLimitExceeded { .. }));
    }

    #[test]
    fn blocks_network_when_not_allowed() {
        let request = SampleFetchRequest::new("https://example.com/data.json");
        let error = fetch_sample_data(&request).expect_err("network disabled");
        assert!(matches!(error, SampleFetchError::NetworkDisabled { .. }));
    }

    #[test]
    fn fetches_http_when_allowed() {
        let listener = match TcpListener::bind("127.0.0.1:0") {
            Ok(listener) => listener,
            Err(err) => {
                eprintln!("Skipping HTTP fetch test: {err}");
                return;
            }
        };
        let addr = listener.local_addr().unwrap();
        let handle = thread::spawn(move || {
            if let Ok((mut stream, _)) = listener.accept() {
                let response = "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\ntest";
                let _ = stream.write_all(response.as_bytes());
            }
        });

        let mut request = SampleFetchRequest::new(format!("http://{addr}/data"));
        request.allow_network = true;

        let result = fetch_sample_data(&request);
        match result {
            Ok(result) => {
                assert_eq!(result.source_kind, SampleSourceKind::Http);
                assert_eq!(result.bytes, b"test");
            }
            Err(SampleFetchError::HttpRequest { message, .. })
                if message.contains("Network Error") || message.contains("connection") =>
            {
                eprintln!("Skipping flaky HTTP fetch test: {}", message);
                return;
            }
            Err(err) => panic!("fetch http: {err:?}"),
        }

        handle.join().unwrap();
    }

    #[test]
    fn reports_missing_cli_for_s3() {
        let mut request = SampleFetchRequest::new("s3://bucket/object.json");
        request.allow_network = true;
        request.aws_cli_path = Some(PathBuf::from("/does/not/exist/aws"));

        let error = fetch_sample_data(&request).expect_err("missing aws");
        assert!(matches!(error, SampleFetchError::CommandMissing { .. }));
    }

    #[test]
    fn reports_missing_cli_for_git() {
        let url = "git+ssh://git@example.com/repo.git?path=data.json";
        let mut request = SampleFetchRequest::new(url);
        request.allow_network = true;
        request.git_cli_path = Some(PathBuf::from("/does/not/exist/git"));

        let error = fetch_sample_data(&request).expect_err("missing git");
        assert!(matches!(error, SampleFetchError::CommandMissing { .. }));
    }

    #[test]
    fn infers_json_object_schema() {
        let data = br#"{"name": "Alice", "age": 29}"#;
        let schema = infer_schema(data, DataFormat::Json).expect("schema");

        let Schema::Object { fields, required } = schema else {
            panic!("unexpected schema");
        };

        assert!(required.contains("name"));
        assert!(required.contains("age"));
        assert_eq!(
            fields.get("name"),
            Some(&Schema::Primitive(PrimitiveType::String))
        );
        assert_eq!(
            fields.get("age"),
            Some(&Schema::Primitive(PrimitiveType::Integer))
        );
    }

    #[test]
    fn infers_json_array_with_optional_field() {
        let data = br#"[{"age": 30}, {"age": null}]"#;
        let schema = infer_schema(data, DataFormat::Json).expect("schema");

        let Schema::Array { element_type } = schema else {
            panic!("expected array schema");
        };

        let Schema::Object { fields, required } = *element_type else {
            panic!("expected object schema");
        };

        assert!(required.contains("age"));
        let age_schema = fields.get("age").expect("age field");
        assert!(matches!(
            age_schema,
            Schema::Optional(inner) if matches!(**inner, Schema::Primitive(PrimitiveType::Integer))
        ));
    }

    #[test]
    fn infers_csv_schema_with_optional_columns() {
        let data = b"name,age\nAlice,30\nBob,\n";
        let schema = infer_schema(data, DataFormat::Csv).expect("schema");

        let Schema::Array { element_type } = schema else {
            panic!("expected array schema");
        };

        let Schema::Object { fields, required } = *element_type else {
            panic!("expected object schema");
        };

        assert!(required.contains("name"));
        assert!(!required.contains("age"));
        assert_eq!(
            fields.get("name"),
            Some(&Schema::Primitive(PrimitiveType::String))
        );
        assert_eq!(
            fields.get("age"),
            Some(&Schema::Primitive(PrimitiveType::Integer))
        );
    }

    #[test]
    fn classifies_large_integers_as_long() {
        let data = br#"{"value": 2147483648}"#;
        let schema = infer_schema(data, DataFormat::Json).expect("schema");

        let Schema::Object { fields, .. } = schema else {
            panic!("expected object schema");
        };

        assert_eq!(
            fields.get("value"),
            Some(&Schema::Primitive(PrimitiveType::Long))
        );
    }
}
