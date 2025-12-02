use crate::builder::JavaSourceBuilder;
use jv_ast::Span;
use jv_ast::expression::TupleFieldMeta;
use jv_ir::{TupleRecordPlan, TupleRecordStrategy};
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct TupleRecordComponent {
    pub name: String,
    pub ty: String,
    pub aliases: Vec<String>,
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

pub fn component_names_for_plan(plan: &TupleRecordPlan) -> Vec<String> {
    build_components(plan)
        .into_iter()
        .map(|component| component.name)
        .collect()
}

fn build_components(plan: &TupleRecordPlan) -> Vec<TupleRecordComponent> {
    let mut components = Vec::with_capacity(plan.arity);
    for index in 0..plan.arity {
        let meta = plan
            .fields
            .get(index)
            .cloned()
            .unwrap_or_else(|| fallback_meta(index));

        let fallback_name = format!("_{}", index + 1);
        let name = match plan.strategy {
            TupleRecordStrategy::Generic => fallback_name.clone(),
            TupleRecordStrategy::Specific => field_name_from_meta(&meta, index),
        };

        let mut aliases = aliases_from_meta(&meta, &name, &fallback_name);
        if !aliases.contains(&fallback_name) {
            aliases.push(fallback_name.clone());
        }
        aliases.retain(|alias| !alias.is_empty());

        let hint = plan
            .type_hints
            .get(index)
            .map(|hint| hint.as_str())
            .unwrap_or("Unknown");
        let ty = java_type_from_hint(hint);

        components.push(TupleRecordComponent { name, ty, aliases });
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

fn aliases_from_meta(meta: &TupleFieldMeta, chosen: &str, fallback: &str) -> Vec<String> {
    let mut aliases = HashSet::new();

    if let Some(label) = primary_label(meta) {
        if label != chosen {
            aliases.insert(label);
        }
    }

    if let Some(hint) = meta.identifier_hint.as_ref() {
        let trimmed = hint.trim();
        if !trimmed.is_empty() && trimmed != chosen {
            aliases.insert(trimmed.to_string());
        }
    }

    for candidate in &meta.secondary_labels {
        let trimmed = candidate.name.trim();
        if !trimmed.is_empty() && trimmed != chosen && trimmed != fallback {
            aliases.insert(trimmed.to_string());
        }
    }

    aliases.into_iter().collect()
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
    } else if hint.eq_ignore_ascii_case("Bool") {
        "boolean".to_string()
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
    let has_aliases = record
        .components
        .iter()
        .any(|component| !component.aliases.is_empty());

    if !has_aliases {
        builder.push_line(&format!(
            "public record {}({}) {{}}",
            record.name, components
        ));
        return builder.build();
    }

    builder.push_line(&format!("public record {}({}) {{", record.name, components));
    builder.indent();
    for component in &record.components {
        for alias in &component.aliases {
            if alias == &component.name {
                continue;
            }
            builder.push_line(&format!(
                "public {} {}() {{ return this.{}(); }}",
                component.ty, alias, component.name
            ));
        }
    }
    builder.dedent();
    builder.push_line("}");
    builder.build()
}
