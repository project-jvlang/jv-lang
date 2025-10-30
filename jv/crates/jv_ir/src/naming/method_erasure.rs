use crate::context::{RegisteredMethodCall, RegisteredMethodDeclaration, TransformContext};
use crate::types::{
    IrCatchClause, IrExpression, IrResolvedMethodTarget, IrResource, IrStatement, IrSwitchCase,
    JavaType, JavaWildcardKind,
};
use hex::encode;
use jv_ast::Span;
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, HashMap, HashSet};
use tracing::trace;

pub fn apply_method_erasure(type_declarations: &mut [IrStatement], context: &TransformContext) {
    if context.method_declarations.is_empty() {
        return;
    }

    let registry = MethodRegistry::from_context(context);
    let resolution = registry.resolve();

    if resolution.is_empty() {
        return;
    }

    for statement in type_declarations.iter_mut() {
        apply_statement(statement, &resolution);
    }
}

#[derive(Default)]
struct MethodResolution {
    declaration_updates: HashMap<SpanKey, MethodRename>,
    call_updates: HashMap<SpanKey, MethodRename>,
}

impl MethodResolution {
    fn is_empty(&self) -> bool {
        self.declaration_updates.is_empty() && self.call_updates.is_empty()
    }

    fn declaration(&self, span: &Span) -> Option<&MethodRename> {
        self.declaration_updates.get(&SpanKey::from(span))
    }

    fn call(&self, span: &Span) -> Option<&MethodRename> {
        self.call_updates.get(&SpanKey::from(span))
    }
}

#[derive(Clone, Debug)]
struct MethodRename {
    java_name: String,
    owner: Option<String>,
    original_name: String,
    erased_parameters: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SpanKey {
    start_line: usize,
    start_column: usize,
    end_line: usize,
    end_column: usize,
}

impl From<&Span> for SpanKey {
    fn from(span: &Span) -> Self {
        Self {
            start_line: span.start_line,
            start_column: span.start_column,
            end_line: span.end_line,
            end_column: span.end_column,
        }
    }
}

struct MethodRegistry {
    declarations: Vec<MethodDeclRecord>,
    calls: Vec<MethodCallRecord>,
}

impl MethodRegistry {
    fn from_context(context: &TransformContext) -> Self {
        let declarations = context
            .method_declarations
            .iter()
            .map(MethodDeclRecord::from)
            .collect();

        let calls = context
            .method_calls
            .iter()
            .map(MethodCallRecord::from)
            .collect();

        Self {
            declarations,
            calls,
        }
    }

    fn resolve(mut self) -> MethodResolution {
        let mut collisions: BTreeMap<CollisionKey, Vec<usize>> = BTreeMap::new();

        for (index, decl) in self.declarations.iter().enumerate() {
            let key = CollisionKey::new(&decl.owner, &decl.original_name, &decl.erased_parameters);
            collisions.entry(key).or_default().push(index);
        }

        for indices in collisions.values() {
            if indices.len() <= 1 {
                continue;
            }

            let mut used_names = HashSet::with_capacity(indices.len());
            for &idx in indices {
                let record = &mut self.declarations[idx];

                if used_names.insert(record.base_java_name.clone()) {
                    record.final_java_name = record.base_java_name.clone();
                    continue;
                }

                let suffix = compute_signature_fingerprint(record);
                let mut candidate = format!("{}${}", record.base_java_name, suffix);
                let mut counter: u32 = 1;

                while !used_names.insert(candidate.clone()) {
                    counter += 1;
                    candidate = format!("{}${}_{}", record.base_java_name, suffix, counter);
                }

                if candidate != record.base_java_name {
                    trace!(
                        owner = record.owner.as_deref().unwrap_or("<global>"),
                        method = %record.original_name,
                        new_name = %candidate,
                        "deterministic rename for erased overload"
                    );
                }

                record.final_java_name = candidate;
            }
        }

        let mut resolution = MethodResolution::default();
        let mut groups: HashMap<GroupKey, Vec<usize>> = HashMap::new();

        for (idx, record) in self.declarations.iter().enumerate() {
            let span_key = SpanKey::from(&record.span);
            let rename = MethodRename {
                java_name: record.final_java_name.clone(),
                owner: record.owner.clone(),
                original_name: record.original_name.clone(),
                erased_parameters: record.erased_parameters.clone(),
            };

            resolution.declaration_updates.insert(span_key, rename);
            groups
                .entry(GroupKey::new(
                    record.owner.clone(),
                    record.original_name.clone(),
                ))
                .or_default()
                .push(idx);
        }

        for call in &self.calls {
            let group_key = GroupKey::new(call.owner.clone(), call.method_name.clone());
            if let Some(candidate_indices) = groups.get(&group_key) {
                if let Some(idx) = self.select_candidate(candidate_indices, call) {
                    let decl = &self.declarations[idx];
                    let rename = MethodRename {
                        java_name: decl.final_java_name.clone(),
                        owner: decl.owner.clone(),
                        original_name: decl.original_name.clone(),
                        erased_parameters: decl.erased_parameters.clone(),
                    };

                    resolution
                        .call_updates
                        .insert(SpanKey::from(&call.span), rename);
                    continue;
                }
            }

            let rename = MethodRename {
                java_name: call.method_name.clone(),
                owner: call.owner.clone(),
                original_name: call.method_name.clone(),
                erased_parameters: call.erased_parameters.clone(),
            };

            resolution
                .call_updates
                .insert(SpanKey::from(&call.span), rename);
        }

        resolution
    }

    fn select_candidate(
        &self,
        candidate_indices: &[usize],
        call: &MethodCallRecord,
    ) -> Option<usize> {
        if candidate_indices.is_empty() {
            return None;
        }

        let mut matching: Vec<usize> = candidate_indices
            .iter()
            .copied()
            .filter(|idx| self.declarations[*idx].erased_parameters == call.erased_parameters)
            .collect();

        if matching.is_empty() {
            matching = candidate_indices.to_vec();
        }

        if matching.len() == 1 {
            return matching.into_iter().next();
        }

        for idx in &matching {
            let record = &self.declarations[*idx];
            if parameters_match(&record.parameter_types, &call.argument_types) {
                return Some(*idx);
            }
        }

        if let Some(idx) = matching.into_iter().next() {
            let record = &self.declarations[idx];
            trace!(
                owner = record.owner.as_deref().unwrap_or("<global>"),
                method = %record.original_name,
                "fallback mapping for ambiguous call"
            );
            return Some(idx);
        }

        None
    }
}

#[derive(Clone)]
struct MethodDeclRecord {
    owner: Option<String>,
    original_name: String,
    base_java_name: String,
    final_java_name: String,
    parameter_types: Vec<JavaType>,
    return_type: JavaType,
    is_static: bool,
    span: Span,
    erased_parameters: Vec<String>,
}

impl<'a> From<&'a RegisteredMethodDeclaration> for MethodDeclRecord {
    fn from(decl: &'a RegisteredMethodDeclaration) -> Self {
        let erased_parameters = decl.parameter_types.iter().map(erase_java_type).collect();

        Self {
            owner: decl.owner.clone(),
            original_name: decl.name.clone(),
            base_java_name: decl.java_name.clone(),
            final_java_name: decl.java_name.clone(),
            parameter_types: decl.parameter_types.clone(),
            return_type: decl.return_type.clone(),
            is_static: decl.is_static,
            span: decl.span.clone(),
            erased_parameters,
        }
    }
}

#[derive(Clone)]
struct MethodCallRecord {
    owner: Option<String>,
    method_name: String,
    argument_types: Vec<JavaType>,
    span: Span,
    erased_parameters: Vec<String>,
}

impl<'a> From<&'a RegisteredMethodCall> for MethodCallRecord {
    fn from(call: &'a RegisteredMethodCall) -> Self {
        let erased_parameters = call.argument_types.iter().map(erase_java_type).collect();

        Self {
            owner: call.owner.clone(),
            method_name: call.original_name.clone(),
            argument_types: call.argument_types.clone(),
            span: call.span.clone(),
            erased_parameters,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct CollisionKey {
    owner: Option<String>,
    method_name: String,
    erased_parameters: Vec<String>,
}

impl CollisionKey {
    fn new(owner: &Option<String>, method_name: &str, erased_parameters: &[String]) -> Self {
        Self {
            owner: owner.clone(),
            method_name: method_name.to_string(),
            erased_parameters: erased_parameters.to_vec(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct GroupKey {
    owner: Option<String>,
    method_name: String,
}

impl GroupKey {
    fn new(owner: Option<String>, method_name: String) -> Self {
        Self { owner, method_name }
    }
}

fn apply_statement(statement: &mut IrStatement, resolution: &MethodResolution) {
    match statement {
        IrStatement::Comment { .. }
        | IrStatement::SampleDeclaration(_)
        | IrStatement::Import(_)
        | IrStatement::Package { .. }
        | IrStatement::Break { .. }
        | IrStatement::Continue { .. } => {}
        IrStatement::Commented { statement, .. } => apply_statement(statement, resolution),
        IrStatement::VariableDeclaration { initializer, .. } => {
            if let Some(expr) = initializer {
                apply_expression(expr, resolution);
            }
        }
        IrStatement::MethodDeclaration {
            java_name,
            body,
            span,
            ..
        } => {
            if let Some(update) = resolution.declaration(span) {
                *java_name = Some(update.java_name.clone());
            }
            if let Some(expr) = body {
                apply_expression(expr, resolution);
            }
        }
        IrStatement::ClassDeclaration {
            fields,
            methods,
            nested_classes,
            ..
        } => {
            for field in fields {
                apply_statement(field, resolution);
            }
            for method in methods {
                apply_statement(method, resolution);
            }
            for nested in nested_classes {
                apply_statement(nested, resolution);
            }
        }
        IrStatement::InterfaceDeclaration {
            methods,
            default_methods,
            fields,
            nested_types,
            ..
        } => {
            for method in methods {
                apply_statement(method, resolution);
            }
            for method in default_methods {
                apply_statement(method, resolution);
            }
            for field in fields {
                apply_statement(field, resolution);
            }
            for nested in nested_types {
                apply_statement(nested, resolution);
            }
        }
        IrStatement::RecordDeclaration { methods, .. } => {
            for method in methods {
                apply_statement(method, resolution);
            }
        }
        IrStatement::FieldDeclaration { initializer, .. } => {
            if let Some(expr) = initializer {
                apply_expression(expr, resolution);
            }
        }
        IrStatement::Expression { expr, .. } => apply_expression(expr, resolution),
        IrStatement::Return { value, .. } => {
            if let Some(expr) = value {
                apply_expression(expr, resolution);
            }
        }
        IrStatement::If {
            condition,
            then_stmt,
            else_stmt,
            ..
        } => {
            apply_expression(condition, resolution);
            apply_statement(then_stmt, resolution);
            if let Some(else_stmt) = else_stmt {
                apply_statement(else_stmt, resolution);
            }
        }
        IrStatement::While {
            condition, body, ..
        } => {
            apply_expression(condition, resolution);
            apply_statement(body, resolution);
        }
        IrStatement::ForEach { iterable, body, .. } => {
            apply_expression(iterable, resolution);
            apply_statement(body, resolution);
        }
        IrStatement::For {
            init,
            condition,
            update,
            body,
            ..
        } => {
            if let Some(init_stmt) = init {
                apply_statement(init_stmt, resolution);
            }
            if let Some(expr) = condition {
                apply_expression(expr, resolution);
            }
            if let Some(expr) = update {
                apply_expression(expr, resolution);
            }
            apply_statement(body, resolution);
        }
        IrStatement::Switch {
            discriminant,
            cases,
            ..
        } => {
            apply_expression(discriminant, resolution);
            for case in cases {
                apply_switch_case(case, resolution);
            }
        }
        IrStatement::Try {
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            apply_statement(body, resolution);
            for clause in catch_clauses {
                apply_catch_clause(clause, resolution);
            }
            if let Some(finally_stmt) = finally_block {
                apply_statement(finally_stmt, resolution);
            }
        }
        IrStatement::TryWithResources {
            resources,
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            for resource in resources {
                apply_resource(resource, resolution);
            }
            apply_statement(body, resolution);
            for clause in catch_clauses {
                apply_catch_clause(clause, resolution);
            }
            if let Some(finally_stmt) = finally_block {
                apply_statement(finally_stmt, resolution);
            }
        }
        IrStatement::Throw { expr, .. } => apply_expression(expr, resolution),
        IrStatement::Block { statements, .. } => {
            for stmt in statements {
                apply_statement(stmt, resolution);
            }
        }
    }
}

fn apply_switch_case(case: &mut IrSwitchCase, resolution: &MethodResolution) {
    if let Some(guard) = &mut case.guard {
        apply_expression(guard, resolution);
    }
    apply_expression(&mut case.body, resolution);
}

fn apply_catch_clause(clause: &mut IrCatchClause, resolution: &MethodResolution) {
    apply_statement(&mut clause.body, resolution);
}

fn apply_resource(resource: &mut IrResource, resolution: &MethodResolution) {
    apply_expression(&mut resource.initializer, resolution);
}

fn apply_expression(expr: &mut IrExpression, resolution: &MethodResolution) {
    match expr {
        IrExpression::MethodCall {
            receiver,
            args,
            java_name,
            resolved_target,
            span,
            ..
        } => {
            if let Some(update) = resolution.call(span) {
                *java_name = Some(update.java_name.clone());
                let target = resolved_target.get_or_insert_with(IrResolvedMethodTarget::default);
                if target.original_name.is_none() {
                    target.original_name = Some(update.original_name.clone());
                }
                target.java_name = Some(update.java_name.clone());
                target.owner = update.owner.clone();
                target.erased_parameters = update.erased_parameters.clone();
            }
            if let Some(receiver_expr) = receiver {
                apply_expression(receiver_expr, resolution);
            }
            for arg in args {
                apply_expression(arg, resolution);
            }
        }
        IrExpression::FieldAccess { receiver, .. } => {
            apply_expression(receiver, resolution);
        }
        IrExpression::ArrayAccess { array, index, .. } => {
            apply_expression(array, resolution);
            apply_expression(index, resolution);
        }
        IrExpression::Binary { left, right, .. } => {
            apply_expression(left, resolution);
            apply_expression(right, resolution);
        }
        IrExpression::RegexMatch { subject, pattern, .. } => {
            apply_expression(subject, resolution);
            apply_expression(pattern, resolution);
        }
        IrExpression::Unary { operand, .. } => apply_expression(operand, resolution),
        IrExpression::Assignment { target, value, .. } => {
            apply_expression(target, resolution);
            apply_expression(value, resolution);
        }
        IrExpression::CharToString(conversion) => {
            apply_expression(&mut conversion.value, resolution);
        }
        IrExpression::Conditional {
            condition,
            then_expr,
            else_expr,
            ..
        } => {
            apply_expression(condition, resolution);
            apply_expression(then_expr, resolution);
            apply_expression(else_expr, resolution);
        }
        IrExpression::Block { statements, .. } => {
            for stmt in statements {
                apply_statement(stmt, resolution);
            }
        }
        IrExpression::ArrayCreation {
            dimensions,
            initializer,
            ..
        } => {
            for dimension in dimensions {
                if let Some(expr) = dimension {
                    apply_expression(expr, resolution);
                }
            }
            if let Some(items) = initializer {
                for item in items {
                    apply_expression(item, resolution);
                }
            }
        }
        IrExpression::ObjectCreation { args, .. } => {
            for arg in args {
                apply_expression(arg, resolution);
            }
        }
        IrExpression::Lambda { body, .. } => apply_expression(body, resolution),
        IrExpression::Switch {
            discriminant,
            cases,
            ..
        } => {
            apply_expression(discriminant, resolution);
            for case in cases {
                apply_switch_case(case, resolution);
            }
        }
        IrExpression::Cast { expr, .. } => apply_expression(expr, resolution),
        IrExpression::InstanceOf { expr, .. } => apply_expression(expr, resolution),
        IrExpression::NullSafeOperation {
            expr: inner,
            operation,
            default_value,
            ..
        } => {
            apply_expression(inner, resolution);
            apply_expression(operation, resolution);
            if let Some(default_expr) = default_value {
                apply_expression(default_expr, resolution);
            }
        }
        IrExpression::StringFormat { args, .. }
        | IrExpression::CompletableFuture { args, .. }
        | IrExpression::VirtualThread { args, .. } => {
            for arg in args {
                apply_expression(arg, resolution);
            }
        }
        IrExpression::TryWithResources {
            resources, body, ..
        } => {
            for resource in resources {
                apply_resource(resource, resolution);
            }
            apply_expression(body, resolution);
        }
        IrExpression::SequencePipeline { .. }
        | IrExpression::Literal(..)
        | IrExpression::RegexPattern { .. }
        | IrExpression::Identifier { .. }
        | IrExpression::This { .. }
        | IrExpression::Super { .. } => {}
    }
}

fn compute_signature_fingerprint(record: &MethodDeclRecord) -> String {
    let mut hasher = Sha256::new();
    if let Some(owner) = &record.owner {
        hasher.update(owner.as_bytes());
    }
    hasher.update(record.original_name.as_bytes());
    hasher.update(if record.is_static { b"S" } else { b"I" });

    for param in &record.parameter_types {
        feed_type(&mut hasher, param);
    }
    feed_type(&mut hasher, &record.return_type);

    let digest = hasher.finalize();
    encode(&digest[..4])
}

fn feed_type(hasher: &mut Sha256, java_type: &JavaType) {
    match java_type {
        JavaType::Primitive(name) => {
            hasher.update(b"P");
            hasher.update(name.as_bytes());
        }
        JavaType::Reference { name, generic_args } => {
            hasher.update(b"R");
            hasher.update(name.as_bytes());
            hasher.update(&(generic_args.len() as u32).to_le_bytes());
            for arg in generic_args {
                feed_type(hasher, arg);
            }
        }
        JavaType::Array {
            element_type,
            dimensions,
        } => {
            hasher.update(b"A");
            hasher.update(&(*dimensions as u32).to_le_bytes());
            feed_type(hasher, element_type);
        }
        JavaType::Functional {
            interface_name,
            param_types,
            return_type,
        } => {
            hasher.update(b"F");
            hasher.update(interface_name.as_bytes());
            hasher.update(&(param_types.len() as u32).to_le_bytes());
            for param in param_types {
                feed_type(hasher, param);
            }
            feed_type(hasher, return_type);
        }
        JavaType::Wildcard { kind, bound } => {
            hasher.update(b"W");
            hasher.update(match kind {
                JavaWildcardKind::Unbounded => b"U" as &[u8],
                JavaWildcardKind::Extends => b"E",
                JavaWildcardKind::Super => b"S",
            });
            if let Some(inner) = bound {
                feed_type(hasher, inner);
            }
        }
        JavaType::Void => hasher.update(b"V"),
    }
}

fn parameters_match(params: &[JavaType], args: &[JavaType]) -> bool {
    if params.len() != args.len() {
        return false;
    }

    params
        .iter()
        .zip(args.iter())
        .all(|(param, arg)| type_matches(param, arg))
}

fn type_matches(param: &JavaType, arg: &JavaType) -> bool {
    match param {
        JavaType::Primitive(name) => match arg {
            JavaType::Primitive(other) => name == other,
            _ => false,
        },
        JavaType::Reference { name, generic_args } => {
            if is_type_parameter_name(name) {
                return true;
            }

            match arg {
                JavaType::Reference {
                    name: arg_name,
                    generic_args: arg_generics,
                } => {
                    if normalize_type_name(name) != normalize_type_name(arg_name) {
                        return false;
                    }

                    if generic_args.is_empty() {
                        return true;
                    }

                    if arg_generics.len() != generic_args.len() {
                        return true;
                    }

                    generic_args
                        .iter()
                        .zip(arg_generics.iter())
                        .all(|(param_arg, call_arg)| type_matches(param_arg, call_arg))
                }
                JavaType::Functional { interface_name, .. } => {
                    normalize_type_name(name) == normalize_type_name(interface_name)
                }
                JavaType::Wildcard { bound, .. } => bound
                    .as_ref()
                    .map(|inner| type_matches(param, inner))
                    .unwrap_or(true),
                _ => false,
            }
        }
        JavaType::Array { element_type, .. } => match arg {
            JavaType::Array {
                element_type: other_element,
                ..
            } => type_matches(element_type, other_element),
            _ => false,
        },
        JavaType::Functional {
            interface_name,
            param_types,
            return_type,
        } => match arg {
            JavaType::Functional {
                interface_name: other_name,
                param_types: other_params,
                return_type: other_return,
            } => {
                normalize_type_name(interface_name) == normalize_type_name(other_name)
                    && param_types.len() == other_params.len()
                    && param_types
                        .iter()
                        .zip(other_params.iter())
                        .all(|(param_arg, call_arg)| type_matches(param_arg, call_arg))
                    && type_matches(return_type, other_return)
            }
            JavaType::Reference { name, .. } => {
                normalize_type_name(interface_name) == normalize_type_name(name)
            }
            _ => false,
        },
        JavaType::Wildcard { bound, .. } => match bound {
            Some(inner) => type_matches(inner, arg),
            None => true,
        },
        JavaType::Void => matches!(arg, JavaType::Void),
    }
}

fn normalize_type_name(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}

fn is_type_parameter_name(name: &str) -> bool {
    !name.contains('.') && name.chars().all(|ch| ch.is_ascii_uppercase())
}

fn erase_java_type(java_type: &JavaType) -> String {
    match java_type {
        JavaType::Primitive(name) => name.clone(),
        JavaType::Reference { name, .. } => name.clone(),
        JavaType::Array {
            element_type,
            dimensions,
        } => format!(
            "{}{}",
            erase_java_type(element_type),
            "[]".repeat(*dimensions)
        ),
        JavaType::Functional { interface_name, .. } => interface_name.clone(),
        JavaType::Wildcard { bound, .. } => bound
            .as_ref()
            .map(|inner| erase_java_type(inner))
            .unwrap_or_else(|| "java.lang.Object".to_string()),
        JavaType::Void => "void".to_string(),
    }
}
