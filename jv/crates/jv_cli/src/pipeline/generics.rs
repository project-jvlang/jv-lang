use std::collections::HashSet;

use jv_inference::service::{TypeFacts, TypeFactsSnapshot};
use jv_inference::solver::Variance;
use jv_inference::types::{BoundPredicate, GenericBounds, TypeId, TypeKind, TypeVariant};
use jv_ir::{IrModifiers, IrProgram, IrStatement, IrTypeParameter, IrVariance, JavaType};

/// Enriches the generated IR program with generic metadata sourced from type inference facts.
pub fn apply_type_facts(program: &mut IrProgram, facts: &TypeFactsSnapshot) {
    let package = program.package.as_deref();
    for declaration in &mut program.type_declarations {
        apply_to_statement(declaration, facts, package);
    }
}

fn apply_to_statement(statement: &mut IrStatement, facts: &TypeFactsSnapshot, package: Option<&str>) {
    match statement {
        IrStatement::ClassDeclaration {
            name,
            type_parameters,
            modifiers,
            nested_classes,
            ..
        }
        | IrStatement::InterfaceDeclaration {
            name,
            type_parameters,
            modifiers,
            nested_types: nested_classes,
            ..
        } => {
            enrich_type_parameters(package, name, type_parameters, modifiers, facts);
            for nested in nested_classes {
                apply_to_statement(nested, facts, package);
            }
        }
        IrStatement::RecordDeclaration {
            name,
            type_parameters,
            modifiers,
            ..
        } => {
            enrich_type_parameters(package, name, type_parameters, modifiers, facts);
        }
        _ => {}
    }
}

fn enrich_type_parameters(
    package: Option<&str>,
    name: &str,
    type_parameters: &mut [IrTypeParameter],
    modifiers: &mut IrModifiers,
    facts: &TypeFactsSnapshot,
) {
    let Some((scheme, generics)) = find_scheme(facts, package, name) else {
        return;
    };

    if type_parameters.is_empty() || generics.is_empty() {
        return;
    }

    let mut collected_permits: HashSet<String> = modifiers.permitted_types.iter().cloned().collect();

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

    if !collected_permits.is_empty() {
        modifiers.is_sealed = true;
        modifiers.permitted_types = collected_permits.into_iter().collect();
        modifiers.permitted_types.sort();
    }
}

fn find_scheme<'a>(
    facts: &'a TypeFactsSnapshot,
    package: Option<&str>,
    name: &str,
) -> Option<(&'a jv_inference::service::TypeScheme, Vec<TypeId>)> {
    for candidate in symbol_candidates(package, name) {
        if let Some(scheme) = facts.scheme_for(&candidate) {
            let generics = scheme.generics().to_vec();
            return Some((scheme, generics));
        }
    }
    None
}

fn symbol_candidates(package: Option<&str>, name: &str) -> Vec<String> {
    let mut candidates = Vec::new();
    let bare = name.to_string();
    candidates.push(bare.clone());
    if let Some(pkg) = package {
        if !pkg.is_empty() {
            let pkg_path = pkg.replace('.', "::");
            candidates.push(format!("{}::{}", pkg_path, name));
            candidates.push(format!("{}.{}", pkg, name));
        }
    }
    candidates
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
