use std::collections::HashSet;

use jv_ast::{Span, types::PrimitiveTypeName};
use jv_inference::service::{TypeFacts, TypeFactsSnapshot, TypeLevelValue};
use jv_inference::solver::Variance;
use jv_inference::types::{BoundPredicate, GenericBounds, SymbolId, TypeId, TypeKind, TypeVariant};
use jv_ir::{
    GenericMetadataMap, IrExpression, IrGenericMetadata, IrModifiers, IrProgram, IrStatement,
    IrTypeLevelValue, IrTypeParameter, IrVariance, JavaType, PrimitiveSpecializationHint,
    SequencePipeline, SequenceSource, SequenceStage,
};

/// Enriches the generated IR program with generic metadata sourced from type inference facts.
pub fn apply_type_facts(program: &mut IrProgram, facts: &TypeFactsSnapshot) {
    let package = program.package.as_deref();
    let mut metadata = std::mem::take(&mut program.generic_metadata);
    metadata.clear();
    let mut path = Vec::new();

    for declaration in &mut program.type_declarations {
        apply_to_statement(declaration, facts, package, &mut metadata, &mut path);
    }

    program.generic_metadata = metadata;
}

fn apply_to_statement(
    statement: &mut IrStatement,
    facts: &TypeFactsSnapshot,
    package: Option<&str>,
    metadata: &mut GenericMetadataMap,
    path: &mut Vec<String>,
) {
    match statement {
        IrStatement::ClassDeclaration {
            name,
            type_parameters,
            modifiers,
            methods,
            nested_classes,
            ..
        } => {
            path.push(name.clone());
            enrich_type_parameters(
                package,
                path.as_slice(),
                type_parameters,
                modifiers,
                facts,
                metadata,
            );
            for method in methods.iter_mut() {
                apply_to_statement(method, facts, package, metadata, path);
            }
            for nested in nested_classes {
                apply_to_statement(nested, facts, package, metadata, path);
            }
            path.pop();
        }
        IrStatement::InterfaceDeclaration {
            name,
            type_parameters,
            modifiers,
            methods,
            default_methods,
            nested_types,
            ..
        } => {
            path.push(name.clone());
            enrich_type_parameters(
                package,
                path.as_slice(),
                type_parameters,
                modifiers,
                facts,
                metadata,
            );
            for method in methods.iter_mut() {
                apply_to_statement(method, facts, package, metadata, path);
            }
            for method in default_methods.iter_mut() {
                apply_to_statement(method, facts, package, metadata, path);
            }
            for nested in nested_types {
                apply_to_statement(nested, facts, package, metadata, path);
            }
            path.pop();
        }
        IrStatement::RecordDeclaration {
            name,
            type_parameters,
            modifiers,
            methods,
            ..
        } => {
            path.push(name.clone());
            enrich_type_parameters(
                package,
                path.as_slice(),
                type_parameters,
                modifiers,
                facts,
                metadata,
            );
            for method in methods.iter_mut() {
                apply_to_statement(method, facts, package, metadata, path);
            }
            path.pop();
        }
        IrStatement::MethodDeclaration {
            name,
            type_parameters,
            modifiers,
            body,
            ..
        } => {
            path.push(name.clone());

            let primitive_hint = {
                let params_view = type_parameters.as_slice();
                find_primitive_specialization_hint(package, path.as_slice(), params_view, facts)
            };

            if let (Some(expr), Some(hint)) = (body.as_mut(), primitive_hint.as_ref()) {
                apply_hint_to_expression(expr, hint);
            }

            if !type_parameters.is_empty() {
                enrich_type_parameters(
                    package,
                    path.as_slice(),
                    type_parameters,
                    modifiers,
                    facts,
                    metadata,
                );
            }

            if let (Some(expr), Some(hint)) = (body.as_mut(), primitive_hint.as_ref()) {
                apply_hint_to_expression(expr, hint);
            }

            path.pop();
        }
        _ => {}
    }
}

fn enrich_type_parameters(
    package: Option<&str>,
    path: &[String],
    type_parameters: &mut [IrTypeParameter],
    modifiers: &mut IrModifiers,
    facts: &TypeFactsSnapshot,
    metadata: &mut GenericMetadataMap,
) {
    let Some((scheme, generics, symbol_name)) = find_scheme(facts, package, path) else {
        return;
    };

    let mut collected_permits: HashSet<String> = modifiers.permitted_types.iter().cloned().collect();

    let mut metadata_entry = IrGenericMetadata::default();

    if !type_parameters.is_empty() && !generics.is_empty() {
        for (param, type_id) in type_parameters.iter_mut().zip(generics.iter()) {
            if let Some(bounds) = scheme
                .bounds_for(*type_id)
                .or_else(|| facts.recorded_bounds(*type_id))
            {
                param.bounds = convert_bounds(bounds);
            }

            if let Some(variance) = scheme
                .variance_for(*type_id)
                .copied()
                .or_else(|| facts.recorded_variance(*type_id))
            {
                param.variance = map_variance(variance);
            }

            if let Some(kind) = facts.kind_for(*type_id) {
                param.kind = Some(kind.clone());
                metadata_entry
                    .type_parameter_kinds
                    .insert(param.name.clone(), kind.clone());
            }

            if let Some(permits) = facts.sealed_permits_for(*type_id) {
                let rendered = permits
                    .iter()
                    .filter_map(type_kind_to_type_name)
                    .collect::<Vec<_>>();
                if !rendered.is_empty() {
                    param.permits = rendered.clone();
                    collected_permits.extend(rendered);
                }
            }
        }
    }

    if !collected_permits.is_empty() {
        modifiers.is_sealed = true;
        modifiers.permitted_types = collected_permits.into_iter().collect();
        modifiers.permitted_types.sort();
    }

    let symbol = SymbolId::from(symbol_name.as_str());

    if let Some(bindings) = facts.const_bindings_for(&symbol) {
        for (name, value) in bindings {
            metadata_entry.const_parameter_values.insert(
                name.clone(),
                convert_type_level_value(value),
            );
        }
    }

    if let Some(results) = facts.type_level_results_for(&symbol) {
        for (slot, value) in results {
            metadata_entry.type_level_bindings.insert(
                slot.clone(),
                convert_type_level_value(value),
            );
        }
    }

    if !metadata_entry.type_parameter_kinds.is_empty()
        || !metadata_entry.const_parameter_values.is_empty()
        || !metadata_entry.type_level_bindings.is_empty()
    {
        let key = metadata_key(package, path);
        metadata.insert(key, metadata_entry);
    }
}

pub(crate) fn find_primitive_specialization_hint(
    package: Option<&str>,
    path: &[String],
    type_parameters: &[IrTypeParameter],
    facts: &TypeFactsSnapshot,
) -> Option<PrimitiveSpecializationHint> {
    if type_parameters.is_empty() {
        return None;
    }

    let (scheme, generics, _) = find_scheme(facts, package, path)?;

    for (param, type_id) in type_parameters.iter().zip(generics.iter()) {
        let bounds = scheme
            .bounds_for(*type_id)
            .or_else(|| facts.recorded_bounds(*type_id));

        let Some(bounds) = bounds else {
            continue;
        };

        for constraint in bounds.constraints() {
            if constraint.type_param != *type_id {
                continue;
            }

            if let BoundPredicate::Primitive(bound) = &constraint.predicate {
                let aliases = bound.alias_families().iter().copied().collect();
                return Some(PrimitiveSpecializationHint {
                    type_param: param.name.clone(),
                    canonical: bound.canonical(),
                    aliases,
                    span: Span::default(),
                });
            }
        }
    }

    None
}

fn apply_hint_to_statement(statement: &mut IrStatement, hint: &PrimitiveSpecializationHint) {
    match statement {
        IrStatement::Expression { expr, .. } => apply_hint_to_expression(expr, hint),
        IrStatement::Return { value: Some(expr), .. } => apply_hint_to_expression(expr, hint),
        IrStatement::VariableDeclaration { initializer, .. } => {
            if let Some(expr) = initializer.as_mut() {
                apply_hint_to_expression(expr, hint);
            }
        }
        IrStatement::If {
            condition,
            then_stmt,
            else_stmt,
            ..
        } => {
            apply_hint_to_expression(condition, hint);
            apply_hint_to_statement(then_stmt, hint);
            if let Some(else_branch) = else_stmt.as_mut() {
                apply_hint_to_statement(else_branch, hint);
            }
        }
        IrStatement::While { condition, body, .. } => {
            apply_hint_to_expression(condition, hint);
            apply_hint_to_statement(body, hint);
        }
        IrStatement::ForEach { iterable, body, .. } => {
            apply_hint_to_expression(iterable, hint);
            apply_hint_to_statement(body, hint);
        }
        IrStatement::For {
            init,
            condition,
            update,
            body,
            ..
        } => {
            if let Some(initializer) = init.as_mut() {
                apply_hint_to_statement(initializer, hint);
            }
            if let Some(cond) = condition.as_mut() {
                apply_hint_to_expression(cond, hint);
            }
            if let Some(update_expr) = update.as_mut() {
                apply_hint_to_expression(update_expr, hint);
            }
            apply_hint_to_statement(body, hint);
        }
        IrStatement::Switch {
            discriminant,
            cases,
            ..
        } => {
            apply_hint_to_expression(discriminant, hint);
            for case in cases.iter_mut() {
                apply_hint_to_case(case, hint);
            }
        }
        IrStatement::Try {
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            apply_hint_to_statement(body, hint);
            for clause in catch_clauses.iter_mut() {
                apply_hint_to_statement(&mut clause.body, hint);
            }
            if let Some(finally_stmt) = finally_block.as_mut() {
                apply_hint_to_statement(finally_stmt, hint);
            }
        }
        IrStatement::TryWithResources {
            resources,
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            for resource in resources.iter_mut() {
                apply_hint_to_expression(&mut resource.initializer, hint);
            }
            apply_hint_to_statement(body, hint);
            for clause in catch_clauses.iter_mut() {
                apply_hint_to_statement(&mut clause.body, hint);
            }
            if let Some(finally_stmt) = finally_block.as_mut() {
                apply_hint_to_statement(finally_stmt, hint);
            }
        }
        IrStatement::Throw { expr, .. } => apply_hint_to_expression(expr, hint),
        IrStatement::Commented { statement, .. } => apply_hint_to_statement(statement, hint),
        _ => {}
    }
}

fn apply_hint_to_case(case: &mut jv_ir::types::IrSwitchCase, hint: &PrimitiveSpecializationHint) {
    for label in case.labels.iter_mut() {
        apply_hint_to_case_label(label, hint);
    }
    if let Some(guard) = case.guard.as_mut() {
        apply_hint_to_expression(guard, hint);
    }
    apply_hint_to_expression(&mut case.body, hint);
}

fn apply_hint_to_case_label(label: &mut jv_ir::types::IrCaseLabel, hint: &PrimitiveSpecializationHint) {
    use jv_ir::types::IrCaseLabel;
    match label {
        IrCaseLabel::Range { lower, upper, .. } => {
            apply_hint_to_expression(lower, hint);
            apply_hint_to_expression(upper, hint);
        }
        IrCaseLabel::TypePattern { deconstruction, .. } => {
            if let Some(pattern) = deconstruction.as_mut() {
                apply_hint_to_deconstruction_pattern(pattern, hint);
            }
        }
        _ => {}
    }
}

fn apply_hint_to_deconstruction_pattern(
    pattern: &mut jv_ir::types::IrDeconstructionPattern,
    hint: &PrimitiveSpecializationHint,
) {
    use jv_ir::types::IrDeconstructionComponent;
    for component in pattern.components.iter_mut() {
        if let IrDeconstructionComponent::Type { pattern: Some(inner), .. } = component {
            apply_hint_to_deconstruction_pattern(inner, hint);
        }
    }
}

fn apply_hint_to_expression(expr: &mut IrExpression, hint: &PrimitiveSpecializationHint) {
    match expr {
        IrExpression::SequencePipeline { pipeline, .. } => {
            if let Some(terminal) = pipeline.terminal.as_mut() {
                if terminal.specialization_hint.is_none() {
                    terminal.specialization_hint = Some(hint.clone());
                }
            }
            pipeline.apply_specialization_hint();
            apply_hint_to_sequence_pipeline(pipeline, hint);
        }
        IrExpression::Block { statements, .. } => {
            for statement in statements.iter_mut() {
                apply_hint_to_statement(statement, hint);
            }
        }
        IrExpression::Lambda { body, .. } => apply_hint_to_expression(body, hint),
        IrExpression::MethodCall { receiver, args, .. } => {
            if let Some(receiver) = receiver.as_mut() {
                apply_hint_to_expression(receiver, hint);
            }
            for arg in args.iter_mut() {
                apply_hint_to_expression(arg, hint);
            }
        }
        IrExpression::FieldAccess { receiver, .. } => apply_hint_to_expression(receiver, hint),
        IrExpression::ArrayAccess { array, index, .. } => {
            apply_hint_to_expression(array, hint);
            apply_hint_to_expression(index, hint);
        }
        IrExpression::Binary { left, right, .. } => {
            apply_hint_to_expression(left, hint);
            apply_hint_to_expression(right, hint);
        }
        IrExpression::Unary { operand, .. } => apply_hint_to_expression(operand, hint),
        IrExpression::Assignment { target, value, .. } => {
            apply_hint_to_expression(target, hint);
            apply_hint_to_expression(value, hint);
        }
        IrExpression::Conditional {
            condition,
            then_expr,
            else_expr,
            ..
        } => {
            apply_hint_to_expression(condition, hint);
            apply_hint_to_expression(then_expr, hint);
            apply_hint_to_expression(else_expr, hint);
        }
        IrExpression::ArrayCreation {
            dimensions,
            initializer,
            ..
        } => {
            for dimension in dimensions.iter_mut() {
                if let Some(expr) = dimension.as_mut() {
                    apply_hint_to_expression(expr, hint);
                }
            }
            if let Some(elements) = initializer.as_mut() {
                for element in elements.iter_mut() {
                    apply_hint_to_expression(element, hint);
                }
            }
        }
        IrExpression::ObjectCreation { args, .. } => {
            for arg in args.iter_mut() {
                apply_hint_to_expression(arg, hint);
            }
        }
        IrExpression::Switch { discriminant, cases, .. } => {
            apply_hint_to_expression(discriminant, hint);
            for case in cases.iter_mut() {
                apply_hint_to_case(case, hint);
            }
        }
        IrExpression::Cast { expr: inner, .. } => apply_hint_to_expression(inner, hint),
        IrExpression::InstanceOf { expr: inner, .. } => apply_hint_to_expression(inner, hint),
        IrExpression::NullSafeOperation {
            expr: inner,
            operation,
            default_value,
            ..
        } => {
            apply_hint_to_expression(inner, hint);
            apply_hint_to_expression(operation, hint);
            if let Some(default_expr) = default_value.as_mut() {
                apply_hint_to_expression(default_expr, hint);
            }
        }
        IrExpression::StringFormat { args, .. } => {
            for arg in args.iter_mut() {
                apply_hint_to_expression(arg, hint);
            }
        }
        IrExpression::CompletableFuture { args, .. }
        | IrExpression::VirtualThread { args, .. } => {
            for arg in args.iter_mut() {
                apply_hint_to_expression(arg, hint);
            }
        }
        IrExpression::TryWithResources {
            resources,
            body,
            ..
        } => {
            for resource in resources.iter_mut() {
                apply_hint_to_expression(&mut resource.initializer, hint);
            }
            apply_hint_to_expression(body, hint);
        }
        IrExpression::RegexPattern { .. }
        | IrExpression::Literal(..)
        | IrExpression::Identifier { .. }
        | IrExpression::This { .. }
        | IrExpression::Super { .. } => {}
    }
}

fn apply_hint_to_sequence_pipeline(
    pipeline: &mut SequencePipeline,
    hint: &PrimitiveSpecializationHint,
) {
    apply_hint_to_sequence_source(&mut pipeline.source, hint);

    for stage in pipeline.stages.iter_mut() {
        apply_hint_to_sequence_stage(stage, hint);
    }

    if let Some(terminal) = pipeline.terminal.as_mut() {
        apply_hint_to_sequence_terminal(terminal, hint);
    }

    pipeline.recompute_element_types();
}

fn apply_hint_to_sequence_source(
    source: &mut SequenceSource,
    hint: &PrimitiveSpecializationHint,
) {
    match source {
        SequenceSource::Collection { expr, .. }
        | SequenceSource::Array { expr, .. }
        | SequenceSource::JavaStream { expr, .. } => {
            apply_hint_to_expression(expr, hint);
        }
        SequenceSource::ListLiteral { elements, .. } => {
            for element in elements.iter_mut() {
                apply_hint_to_expression(element, hint);
            }
        }
    }
}

fn apply_hint_to_sequence_stage(
    stage: &mut SequenceStage,
    hint: &PrimitiveSpecializationHint,
) {
    match stage {
        SequenceStage::Map { lambda, .. }
        | SequenceStage::FlatMap { lambda, .. } => apply_hint_to_expression(lambda, hint),
        SequenceStage::Filter { predicate, .. } => apply_hint_to_expression(predicate, hint),
        SequenceStage::Take { count, .. }
        | SequenceStage::Drop { count, .. } => apply_hint_to_expression(count, hint),
        SequenceStage::Sorted { comparator, .. } => {
            if let Some(comparator) = comparator.as_mut() {
                apply_hint_to_expression(comparator, hint);
            }
        }
    }
}

fn apply_hint_to_sequence_terminal(
    terminal: &mut jv_ir::sequence_pipeline::SequenceTerminal,
    hint: &PrimitiveSpecializationHint,
) {
    use jv_ir::sequence_pipeline::SequenceTerminalKind;

    match &mut terminal.kind {
        SequenceTerminalKind::Fold { initial, accumulator } => {
            apply_hint_to_expression(initial, hint);
            apply_hint_to_expression(accumulator, hint);
        }
        SequenceTerminalKind::Reduce { accumulator } => {
            apply_hint_to_expression(accumulator, hint);
        }
        SequenceTerminalKind::GroupBy { key_selector } => {
            apply_hint_to_expression(key_selector, hint);
        }
        SequenceTerminalKind::Associate { pair_selector } => {
            apply_hint_to_expression(pair_selector, hint);
        }
        SequenceTerminalKind::ForEach { action } => apply_hint_to_expression(action, hint),
        SequenceTerminalKind::ToList
        | SequenceTerminalKind::ToSet
        | SequenceTerminalKind::Count
        | SequenceTerminalKind::Sum => {}
    }
}

fn find_scheme<'a>(
    facts: &'a TypeFactsSnapshot,
    package: Option<&str>,
    path: &[String],
) -> Option<(&'a jv_inference::service::TypeScheme, Vec<TypeId>, String)> {
    for candidate in symbol_candidates(package, path) {
        if let Some(scheme) = facts.scheme_for(&candidate) {
            let generics = scheme.generics().to_vec();
            return Some((scheme, generics, candidate));
        }
    }
    None
}

fn symbol_candidates(package: Option<&str>, path: &[String]) -> Vec<String> {
    let mut candidates = Vec::new();
    if path.is_empty() {
        return candidates;
    }

    let name = path.last().cloned().unwrap_or_default();
    push_candidate(&mut candidates, name.clone());

    let colon_join = path.join("::");
    push_candidate(&mut candidates, colon_join.clone());

    if path.len() > 1 {
        push_candidate(&mut candidates, path.join("."));
        push_candidate(&mut candidates, path.join("$"));
    }

    if let Some(pkg) = package {
        if !pkg.is_empty() {
            let pkg_colon = pkg.replace('.', "::");
            let pkg_dot = pkg;
            push_candidate(&mut candidates, format!("{}::{}", pkg_colon, colon_join));
            let dot_join = path.join(".");
            push_candidate(&mut candidates, format!("{}.{}", pkg_dot, dot_join));
            let dollar_join = path.join("$");
            let pkg_dollar = pkg.replace('.', "$");
            push_candidate(&mut candidates, format!("{}${}", pkg_dollar, dollar_join));
        }
    }

    candidates
}

fn push_candidate(candidates: &mut Vec<String>, candidate: String) {
    if !candidate.is_empty() && !candidates.iter().any(|existing| existing == &candidate) {
        candidates.push(candidate);
    }
}

fn metadata_key(package: Option<&str>, path: &[String]) -> String {
    let mut segments: Vec<String> = Vec::new();
    if let Some(pkg) = package {
        if !pkg.is_empty() {
            segments.extend(pkg.split('.').map(|segment| segment.to_string()));
        }
    }
    segments.extend(path.iter().cloned());
    segments.join("::")
}

fn convert_type_level_value(value: &TypeLevelValue) -> IrTypeLevelValue {
    match value {
        TypeLevelValue::Int(v) => IrTypeLevelValue::Int(*v),
        TypeLevelValue::Bool(v) => IrTypeLevelValue::Bool(*v),
        TypeLevelValue::String(v) => IrTypeLevelValue::String(v.clone()),
    }
}

fn convert_bounds(bounds: &GenericBounds) -> Vec<JavaType> {
    let mut rendered = Vec::new();
    for constraint in bounds.constraints() {
        rendered.extend(java_types_from_predicate(&constraint.predicate));
    }
    dedup_types(rendered)
}

fn java_types_from_predicate(predicate: &BoundPredicate) -> Vec<JavaType> {
    match predicate {
        BoundPredicate::Trait(bound) => vec![JavaType::Reference {
            name: to_java_name(&bound.name),
            generic_args: Vec::new(),
        }],
        BoundPredicate::Interface(name) => vec![JavaType::Reference {
            name: to_java_name(name),
            generic_args: Vec::new(),
        }],
        BoundPredicate::Capability(capability) => vec![JavaType::Reference {
            name: to_java_name(&capability.name),
            generic_args: Vec::new(),
        }],
        BoundPredicate::Primitive(bound) => vec![JavaType::Primitive(
            match bound.canonical() {
                PrimitiveTypeName::Int => "int",
                PrimitiveTypeName::Long => "long",
                PrimitiveTypeName::Short => "short",
                PrimitiveTypeName::Byte => "byte",
                PrimitiveTypeName::Float => "float",
                PrimitiveTypeName::Double => "double",
                PrimitiveTypeName::Boolean => "boolean",
                PrimitiveTypeName::Char => "char",
            }
            .to_string(),
        )],
        BoundPredicate::FunctionSignature(_) => Vec::new(),
        BoundPredicate::WhereClause(predicates) => predicates
            .iter()
            .flat_map(java_types_from_predicate)
            .collect(),
    }
}

fn type_kind_to_type_name(kind: &TypeKind) -> Option<String> {
    java_type_from_type_kind(kind).map(|java_type| render_java_type(&java_type))
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
        "boolean" | "Boolean" => JavaType::Primitive("boolean".to_string()),
        "double" | "Double" => JavaType::Primitive("double".to_string()),
        "float" | "Float" => JavaType::Primitive("float".to_string()),
        "long" | "Long" => JavaType::Primitive("long".to_string()),
        "short" | "Short" => JavaType::Primitive("short".to_string()),
        "byte" | "Byte" => JavaType::Primitive("byte".to_string()),
        "char" | "Char" => JavaType::Primitive("char".to_string()),
        "String" => JavaType::Reference {
            name: "java.lang.String".to_string(),
            generic_args: Vec::new(),
        },
        other => JavaType::Reference {
            name: to_java_name(other),
            generic_args: Vec::new(),
        },
    }
}

fn dedup_types(types: Vec<JavaType>) -> Vec<JavaType> {
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for ty in types {
        let key = render_java_type(&ty);
        if seen.insert(key) {
            result.push(ty);
        }
    }
    result
}

fn render_java_type(java_type: &JavaType) -> String {
    match java_type {
        JavaType::Primitive(name) => name.clone(),
        JavaType::Reference { name, generic_args } => {
            if generic_args.is_empty() {
                name.clone()
            } else {
                let args = generic_args
                    .iter()
                    .map(render_java_type)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", name, args)
            }
        }
        JavaType::Array {
            element_type,
            dimensions,
        } => {
            let base = render_java_type(element_type);
            let suffix = "[]".repeat(*dimensions);
            format!("{}{}", base, suffix)
        }
        JavaType::Functional { interface_name, .. } => interface_name.clone(),
        JavaType::Wildcard { kind, bound } => match kind {
            jv_ir::JavaWildcardKind::Unbounded => "?".to_string(),
            jv_ir::JavaWildcardKind::Extends => {
                let ty = bound
                    .as_ref()
                    .map(|inner| render_java_type(inner))
                    .unwrap_or_else(|| "Object".to_string());
                format!("? extends {}", ty)
            }
            jv_ir::JavaWildcardKind::Super => {
                let ty = bound
                    .as_ref()
                    .map(|inner| render_java_type(inner))
                    .unwrap_or_else(|| "Object".to_string());
                format!("? super {}", ty)
            }
        },
        JavaType::Void => "void".to_string(),
    }
}

fn to_java_name(name: &str) -> String {
    name.replace("::", ".")
}

fn map_variance(variance: Variance) -> IrVariance {
    match variance {
        Variance::Covariant => IrVariance::Covariant,
        Variance::Contravariant => IrVariance::Contravariant,
        Variance::Invariant => IrVariance::Invariant,
        Variance::Bivariant => IrVariance::Bivariant,
    }
}
