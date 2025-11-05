use crate::builder::JavaSourceBuilder;
use jv_ast::Span;
use jv_ast::expression::TupleFieldMeta;
use jv_ir::{TupleRecordPlan, TupleRecordStrategy};
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct TupleRecordComponent {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Clone)]
pub struct TupleRecord {
    pub name: String,
    pub components: Vec<TupleRecordComponent>,
    pub strategy: TupleRecordStrategy,
}

impl TupleRecord {
    pub fn component_names(&self) -> Vec<String> {
        self.components
            .iter()
            .map(|component| component.name.clone())
            .collect()
    }
}

pub fn collect_tuple_records(plans: &[TupleRecordPlan]) -> Vec<TupleRecord> {
    let mut seen = HashSet::new();
    let mut records = Vec::new();

    for plan in plans {
        if plan.arity == 0 {
            continue;
        }
        let name = match plan.strategy {
            TupleRecordStrategy::Specific => plan
                .specific_name
                .as_ref()
                .cloned()
                .unwrap_or_else(|| plan.generic_name.clone()),
            TupleRecordStrategy::Generic => plan.generic_name.clone(),
        };

        if !seen.insert(name.clone()) {
            continue;
        }

        let components = build_components(plan);
        records.push(TupleRecord {
            name,
            components,
            strategy: plan.strategy,
        });
    }

    records
}

fn build_components(plan: &TupleRecordPlan) -> Vec<TupleRecordComponent> {
    let mut components = Vec::with_capacity(plan.arity);
    for index in 0..plan.arity {
        let meta = plan
            .fields
            .get(index)
            .cloned()
            .unwrap_or_else(|| fallback_meta(index));

        let name = match plan.strategy {
            TupleRecordStrategy::Generic => format!("_{}", index + 1),
            TupleRecordStrategy::Specific => field_name_from_meta(&meta, index),
        };

        let hint = plan
            .type_hints
            .get(index)
            .map(|hint| hint.as_str())
            .unwrap_or("Unknown");
        let ty = java_type_from_hint(hint);

        components.push(TupleRecordComponent { name, ty });
    }
    components
}

fn fallback_meta(index: usize) -> TupleFieldMeta {
    TupleFieldMeta::empty(index + 1, Span::dummy())
}

fn field_name_from_meta(meta: &TupleFieldMeta, position: usize) -> String {
    if let Some(label) = primary_label(meta) {
        return label;
    }
    let fallback = if meta.fallback_index == 0 {
        position + 1
    } else {
        meta.fallback_index
    };
    format!("_{}", fallback)
}

fn primary_label(meta: &TupleFieldMeta) -> Option<String> {
    if let Some(label) = meta.primary_label.as_ref() {
        let trimmed = label.trim();
        if !trimmed.is_empty() {
            return Some(trimmed.to_string());
        }
    }

    for candidate in &meta.secondary_labels {
        let trimmed = candidate.name.trim();
        if !trimmed.is_empty() {
            return Some(trimmed.to_string());
        }
    }

    if let Some(hint) = meta.identifier_hint.as_ref() {
        let trimmed = hint.trim();
        if !trimmed.is_empty() {
            return Some(trimmed.to_string());
        }
    }

    None
}

fn java_type_from_hint(hint: &str) -> String {
    let normalized = hint.trim();
    if normalized.is_empty() || normalized.eq_ignore_ascii_case("Unknown") {
        return "Object".to_string();
    }

    match_normalized_type(normalized)
}

fn match_normalized_type(hint: &str) -> String {
    if hint.eq_ignore_ascii_case("Int") {
        "int".to_string()
    } else if hint.eq_ignore_ascii_case("Long") {
        "long".to_string()
    } else if hint.eq_ignore_ascii_case("Short") {
        "short".to_string()
    } else if hint.eq_ignore_ascii_case("Byte") {
        "byte".to_string()
    } else if hint.eq_ignore_ascii_case("Double") {
        "double".to_string()
    } else if hint.eq_ignore_ascii_case("Float") {
        "float".to_string()
    } else if hint.eq_ignore_ascii_case("Boolean") {
        "boolean".to_string()
    } else if hint.eq_ignore_ascii_case("Char") || hint.eq_ignore_ascii_case("Character") {
        "char".to_string()
    } else if hint.eq_ignore_ascii_case("String") {
        "String".to_string()
    } else if hint.eq_ignore_ascii_case("Void") {
        "Void".to_string()
    } else if hint.eq_ignore_ascii_case("Unit") {
        "Void".to_string()
    } else {
        hint.to_string()
    }
}

pub fn render_tuple_record(record: &TupleRecord, indent: &str) -> String {
    let mut builder = JavaSourceBuilder::new(indent.to_string());
    let components = record
        .components
        .iter()
        .map(|component| format!("{} {}", component.ty, component.name))
        .collect::<Vec<_>>()
        .join(", ");
    builder.push_line(&format!(
        "public record {}({}) {{}}",
        record.name, components
    ));
    builder.build()
}
