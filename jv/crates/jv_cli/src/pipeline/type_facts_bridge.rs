use jv_ast::expression::TupleFieldMeta;
use jv_ast::Span;
use jv_inference::service::TypeFactsSnapshot;
use jv_inference::types::{TypeId, TypeKind, TypeVariant};
use jv_inference::TypeFacts;
use jv_ir::types::{IrProgram, IrStatement, JavaType, TupleRecordPlan, TupleRecordStrategy, TupleUsageKind};
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
        "boolean" | "Boolean" | "Bool" => JavaType::Primitive("boolean".to_string()),
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

        let (component_names, component_spans) = component_metadata_for_plan(plan);
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
                component_spans.clone(),
                type_hints.clone(),
            );

            if usage.kind == TupleUsageKind::FunctionReturn {
                if let Some(owner) = &usage.owner {
                    context.register_tuple_return(owner, &record_name, component_names.clone());
                }
            }
        }
    }
}

fn component_metadata_for_plan(plan: &TupleRecordPlan) -> (Vec<String>, Vec<Option<Span>>) {
    let mut components = Vec::with_capacity(plan.arity);
    let mut spans = Vec::with_capacity(plan.arity);
    for index in 0..plan.arity {
        let (name, span) = match plan.fields.get(index) {
            Some(meta) => {
                let label = match plan.strategy {
                    TupleRecordStrategy::Generic => format!("_{}", index + 1),
                    TupleRecordStrategy::Specific => field_name_from_meta(meta, index),
                };
                (label, Some(meta.span.clone()))
            }
            None => {
                let fallback = format!("_{}", index + 1);
                (fallback, None)
            }
        };
        components.push(name);
        spans.push(span);
    }
    (components, spans)
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

/// Map tuple-returning関数の戻り型を、計画済みのタプルレコード名に置き換える。
pub fn apply_tuple_return_types(program: &mut IrProgram) {
    let mut mapping: HashMap<String, String> = HashMap::new();
    for plan in &program.tuple_record_plans {
        let record_name = plan
            .specific_name
            .as_ref()
            .cloned()
            .unwrap_or_else(|| plan.generic_name.clone());
        for usage in &plan.usage_sites {
            if usage.kind == TupleUsageKind::FunctionReturn {
                if let Some(owner) = &usage.owner {
                    mapping.insert(owner.clone(), record_name.clone());
                }
            }
        }
    }

    if mapping.is_empty() {
        return;
    }

    fn rewrite(statement: &mut IrStatement, mapping: &HashMap<String, String>) {
        match statement {
            IrStatement::MethodDeclaration { name, return_type, .. } => {
                if let Some(record) = mapping.get(name) {
                    *return_type = JavaType::Reference {
                        name: record.clone(),
                        generic_args: vec![],
                    };
                }
            }
            IrStatement::ClassDeclaration {
                fields,
                methods,
                nested_classes,
                ..
            } => {
                for field in fields.iter_mut() {
                    rewrite(field, mapping);
                }
                for method in methods.iter_mut() {
                    rewrite(method, mapping);
                }
                for class in nested_classes.iter_mut() {
                    rewrite(class, mapping);
                }
            }
            IrStatement::InterfaceDeclaration {
                methods,
                default_methods,
                fields,
                nested_types,
                ..
            } => {
                for method in methods.iter_mut() {
                    rewrite(method, mapping);
                }
                for method in default_methods.iter_mut() {
                    rewrite(method, mapping);
                }
                for field in fields.iter_mut() {
                    rewrite(field, mapping);
                }
                for nested in nested_types.iter_mut() {
                    rewrite(nested, mapping);
                }
            }
            IrStatement::RecordDeclaration { methods, .. } => {
                for method in methods.iter_mut() {
                    rewrite(method, mapping);
                }
            }
            IrStatement::Block { statements, .. } => {
                for stmt in statements.iter_mut() {
                    rewrite(stmt, mapping);
                }
            }
            IrStatement::If { then_stmt, else_stmt, .. } => {
                rewrite(then_stmt, mapping);
                if let Some(else_stmt) = else_stmt.as_mut() {
                    rewrite(else_stmt, mapping);
                }
            }
            IrStatement::While { body, .. } => rewrite(body, mapping),
            IrStatement::ForEach { body, .. } => rewrite(body, mapping),
            IrStatement::For { body, .. } => rewrite(body, mapping),
            IrStatement::Try { body, catch_clauses, finally_block, .. } => {
                rewrite(body, mapping);
                for clause in catch_clauses.iter_mut() {
                    rewrite(&mut clause.body, mapping);
                }
                if let Some(finally_stmt) = finally_block.as_mut() {
                    rewrite(finally_stmt, mapping);
                }
            }
            IrStatement::TryWithResources { body, catch_clauses, finally_block, .. } => {
                rewrite(body, mapping);
                for clause in catch_clauses.iter_mut() {
                    rewrite(&mut clause.body, mapping);
                }
                if let Some(finally_stmt) = finally_block.as_mut() {
                    rewrite(finally_stmt, mapping);
                }
            }
            IrStatement::Switch { .. } => {}
            _ => {}
        }
    }

    for decl in program.type_declarations.iter_mut() {
        rewrite(decl, &mapping);
    }
}
