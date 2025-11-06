use jv_ast::expression::TupleFieldMeta;
use jv_ast::Span;
use jv_inference::service::TypeFactsSnapshot;
use jv_inference::types::{TypeId, TypeKind, TypeVariant};
use jv_inference::TypeFacts;
use jv_ir::types::{JavaType, TupleRecordPlan, TupleRecordStrategy};
use jv_ir::TransformContext;
use std::collections::HashMap;
use tracing::debug;

/// Preloads TypeFacts-derived signatures and record components into the lowering context so
/// transformations can reflect inferred types when source annotations are absent.
pub fn preload_type_facts_into_context(
    context: &mut TransformContext,
    facts: &TypeFactsSnapshot,
) {
    let binding_map = facts
        .bindings()
        .iter()
        .map(|binding| (binding.id, binding.ty.clone()))
        .collect::<HashMap<TypeId, TypeKind>>();

    let mut registered_functions = 0usize;
    for (name, scheme) in facts.all_schemes() {
        if let TypeVariant::Function(params, ret) = scheme.body().variant() {
            let parameter_types = params
                .iter()
                .map(|kind| {
                    let resolved = resolve_type_kind(kind, &binding_map);
                    java_type_from_type_kind(&resolved).unwrap_or_else(JavaType::object)
                })
                .collect::<Vec<_>>();
            let return_kind = resolve_type_kind(ret.as_ref(), &binding_map);
            let return_type = java_type_from_type_kind(&return_kind);

            debug!(
                target: "jv::transform::facts",
                function = %name,
                params = parameter_types.len(),
                "registered signature hint from TypeFacts"
            );

            context.preload_function_signature(
                name.to_string(),
                parameter_types,
                return_type,
            );
            registered_functions += 1;
        }
    }

    let mut registered_records = 0usize;
    for (name, kind) in facts.environment().values() {
        let resolved_kind = resolve_type_kind(kind, &binding_map);
        if let Some(java_type) = java_type_from_type_kind(&resolved_kind) {
            context
                .type_info
                .insert(name.clone(), java_type.clone());
        }

        match resolved_kind.variant() {
            TypeVariant::Function(params, ret) => {
                let parameter_types = params
                    .iter()
                    .map(|param| {
                        java_type_from_type_kind(param).unwrap_or_else(JavaType::object)
                    })
                    .collect::<Vec<_>>();
                let return_type = java_type_from_type_kind(ret.as_ref());
                context.preload_function_signature(name.clone(), parameter_types, return_type);
                registered_functions += 1;
            }
            TypeVariant::Record { fields } => {
                let components = fields
                    .iter()
                    .map(|field| {
                        let java_type =
                            java_type_from_type_kind(&field.ty).unwrap_or_else(JavaType::object);
                        (field.name.clone(), java_type)
                    })
                    .collect::<Vec<_>>();

                if components.is_empty() {
                    continue;
                }

                let canonical = name.replace("::", ".");
                context.register_record_components(canonical, components);
                debug!(
                    target: "jv::transform::facts",
                    record = %name,
                    "registered record components from TypeFacts"
                );
                registered_records += 1;
            }
            _ => {}
        }
    }

    if registered_functions > 0 || registered_records > 0 {
        debug!(
            target: "jv::transform::facts",
            functions = registered_functions,
            records = registered_records,
            "preloaded TypeFacts hints into TransformContext"
        );
    }
}

fn java_type_from_type_kind(kind: &TypeKind) -> Option<JavaType> {
    match kind.variant() {
        TypeVariant::Primitive(name) => Some(java_type_from_name(name)),
        TypeVariant::Optional(inner) => {
            let inner_type = java_type_from_type_kind(inner)?;
            Some(JavaType::Reference {
                name: "java.util.Optional".to_string(),
                generic_args: vec![inner_type],
            })
        }
        TypeVariant::Function(_, _) => Some(JavaType::Reference {
            name: "java.util.function.Function".to_string(),
            generic_args: Vec::new(),
        }),
        TypeVariant::Record { .. } | TypeVariant::Union { .. } => Some(JavaType::object()),
        TypeVariant::Variable(_) => Some(JavaType::object()),
        TypeVariant::Unknown => None,
    }
}

fn java_type_from_name(name: &str) -> JavaType {
    match name {
        "int" | "Int" => JavaType::Primitive("int".to_string()),
        "long" | "Long" => JavaType::Primitive("long".to_string()),
        "short" | "Short" => JavaType::Primitive("short".to_string()),
        "byte" | "Byte" => JavaType::Primitive("byte".to_string()),
        "float" | "Float" => JavaType::Primitive("float".to_string()),
        "double" | "Double" => JavaType::Primitive("double".to_string()),
        "boolean" | "Boolean" => JavaType::Primitive("boolean".to_string()),
        "char" | "Char" => JavaType::Primitive("char".to_string()),
        "String" => JavaType::Reference {
            name: "java.lang.String".to_string(),
            generic_args: Vec::new(),
        },
        other => JavaType::Reference {
            name: other.replace("::", "."),
            generic_args: Vec::new(),
        },
    }
}

fn resolve_type_kind(kind: &TypeKind, bindings: &HashMap<TypeId, TypeKind>) -> TypeKind {
    match kind.variant() {
        TypeVariant::Primitive(_) => kind.clone(),
        TypeVariant::Optional(inner) => {
            let resolved_inner = resolve_type_kind(inner, bindings);
            rebuild_kind_like(kind, TypeVariant::Optional(Box::new(resolved_inner)))
        }
        TypeVariant::Function(params, ret) => {
            let resolved_params = params
                .iter()
                .map(|param| resolve_type_kind(param, bindings))
                .collect();
            let resolved_ret = resolve_type_kind(ret, bindings);
            rebuild_kind_like(
                kind,
                TypeVariant::Function(resolved_params, Box::new(resolved_ret)),
            )
        }
        TypeVariant::Record { fields } => {
            let resolved_fields = fields
                .iter()
                .map(|field| {
                    let mut resolved_field = field.clone();
                    resolved_field.ty = resolve_type_kind(&field.ty, bindings);
                    resolved_field
                })
                .collect();
            rebuild_kind_like(kind, TypeVariant::Record { fields: resolved_fields })
        }
        TypeVariant::Union { arms } => {
            let resolved_arms = arms
                .iter()
                .map(|arm| resolve_type_kind(arm, bindings))
                .collect();
            rebuild_kind_like(kind, TypeVariant::Union { arms: resolved_arms })
        }
        TypeVariant::Variable(id) => bindings
            .get(id)
            .map(|resolved| resolve_type_kind(resolved, bindings))
            .unwrap_or_else(|| kind.clone()),
        TypeVariant::Unknown => kind.clone(),
    }
}

fn rebuild_kind_like(source: &TypeKind, variant: TypeVariant) -> TypeKind {
    let mut rebuilt = TypeKind::new(variant).with_nullability(source.nullability());
    if let Some(bounds) = source.bounds().cloned() {
        rebuilt = rebuilt.with_bounds(bounds);
    }
    rebuilt
}

/// Registers tuple record plan metadata so IR transformation can resolve tuple component types.
pub fn preload_tuple_plans_into_context(
    context: &mut TransformContext,
    plans: &[TupleRecordPlan],
) {
    for plan in plans {
        if plan.arity == 0 {
            continue;
        }

        let record_name = match plan.strategy {
            TupleRecordStrategy::Specific => plan
                .specific_name
                .as_ref()
                .cloned()
                .unwrap_or_else(|| plan.generic_name.clone()),
            TupleRecordStrategy::Generic => plan.generic_name.clone(),
        };

        let component_names = component_names_for_plan(plan);
        let type_hints = plan
            .type_hints
            .iter()
            .map(|hint| java_type_from_tuple_hint(hint))
            .collect::<Vec<_>>();

        for usage in &plan.usage_sites {
            context.register_tuple_plan_usage(
                usage.span.clone(),
                Some(record_name.clone()),
                component_names.clone(),
                type_hints.clone(),
            );
        }
    }
}

fn component_names_for_plan(plan: &TupleRecordPlan) -> Vec<String> {
    let mut components = Vec::with_capacity(plan.arity);
    for index in 0..plan.arity {
        let meta = plan
            .fields
            .get(index)
            .cloned()
            .unwrap_or_else(|| TupleFieldMeta::empty(index + 1, Span::dummy()));
        let name = match plan.strategy {
            TupleRecordStrategy::Generic => format!("_{}", index + 1),
            TupleRecordStrategy::Specific => field_name_from_meta(&meta, index),
        };
        components.push(name);
    }
    components
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

fn java_type_from_tuple_hint(hint: &str) -> Option<JavaType> {
    let normalized = hint.trim();
    if normalized.is_empty() || normalized.eq_ignore_ascii_case("Unknown") {
        return None;
    }
    Some(java_type_from_name(normalized))
}
