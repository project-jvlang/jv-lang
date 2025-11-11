use std::collections::{HashMap, HashSet};

use jv_ast::{CallArgumentStyle, Literal, Span};

use crate::error::TransformError;
use crate::types::{
    ClassId, IrExpression, IrModifiers, IrRegexReplacement, IrRegexTemplateSegment, IrStatement,
    IrVisibility, JavaType, LogInvocationItem, LogInvocationPlan, LoggerFieldId,
    LoggingFrameworkKind, LoggingMetadata,
};

/// ロガーフィールドを各クラスへ割り当て、重複や衝突を解消する。
pub fn attach_logger_fields(
    package: Option<&str>,
    declarations: &mut [IrStatement],
    metadata: &mut LoggingMetadata,
) -> Result<(), TransformError> {
    if metadata.logger_fields.is_empty() {
        return Ok(());
    }

    let mut spec_index: HashMap<LoggerFieldId, usize> = metadata
        .logger_fields
        .iter()
        .enumerate()
        .map(|(idx, spec)| (spec.id, idx))
        .collect();

    let mut used_ids: HashSet<LoggerFieldId> = HashSet::new();
    let mut duplicates: HashSet<LoggerFieldId> = HashSet::new();
    let mut path = Vec::new();

    for declaration in declarations.iter_mut() {
        process_declaration(
            declaration,
            package,
            &mut path,
            metadata,
            &mut spec_index,
            metadata.framework.clone(),
            &mut used_ids,
            &mut duplicates,
        )?;
    }

    for declaration in declarations.iter() {
        collect_plan_ids(declaration, &mut used_ids);
    }

    metadata.logger_fields.retain(|spec| {
        if duplicates.contains(&spec.id) {
            return false;
        }
        used_ids.contains(&spec.id)
    });

    Ok(())
}

fn process_declaration(
    stmt: &mut IrStatement,
    package: Option<&str>,
    path: &mut Vec<String>,
    metadata: &mut LoggingMetadata,
    spec_index: &mut HashMap<LoggerFieldId, usize>,
    framework: LoggingFrameworkKind,
    used_ids: &mut HashSet<LoggerFieldId>,
    duplicates: &mut HashSet<LoggerFieldId>,
) -> Result<(), TransformError> {
    match stmt {
        IrStatement::ClassDeclaration {
            name,
            fields,
            methods,
            nested_classes,
            span,
            ..
        } => {
            path.push(name.clone());
            process_class(
                package, path, fields, methods, span, metadata, spec_index, &framework, used_ids,
                duplicates,
            )?;
            for nested in nested_classes.iter_mut() {
                process_declaration(
                    nested,
                    package,
                    path,
                    metadata,
                    spec_index,
                    framework.clone(),
                    used_ids,
                    duplicates,
                )?;
            }
            path.pop();
        }
        IrStatement::Commented { statement, .. } => {
            process_declaration(
                statement, package, path, metadata, spec_index, framework, used_ids, duplicates,
            )?;
        }
        _ => {}
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn process_class(
    package: Option<&str>,
    path: &mut Vec<String>,
    fields: &mut Vec<IrStatement>,
    methods: &mut Vec<IrStatement>,
    class_span: &Span,
    metadata: &mut LoggingMetadata,
    spec_index: &mut HashMap<LoggerFieldId, usize>,
    framework: &LoggingFrameworkKind,
    used_ids: &mut HashSet<LoggerFieldId>,
    duplicates: &mut HashSet<LoggerFieldId>,
) -> Result<(), TransformError> {
    let class_id = ClassId {
        package: package.map(|pkg| pkg.to_string()),
        local_name: path.clone(),
    };

    let mut collector = LoggerPlanCollector::new(class_id.clone(), used_ids);

    for field in fields.iter_mut() {
        collector.visit_statement(field);
    }

    for method in methods.iter_mut() {
        collector.visit_statement(method);
    }

    let canonical_id = collector.canonical_id();
    let primary_span = collector.primary_span().cloned();
    for duplicate in collector.into_duplicates() {
        duplicates.insert(duplicate);
    }

    let Some(logger_id) = canonical_id else {
        return Ok(());
    };

    let Some(&spec_idx) = spec_index.get(&logger_id) else {
        return Ok(());
    };

    let fqcn = class_fqcn_from_id(&class_id);
    if let Some(spec) = metadata.logger_fields.get_mut(spec_idx) {
        spec.class_id = Some(class_id.clone());
        spec.owner_hint = Some(fqcn.clone());
    }

    let field_name = metadata.logger_fields[spec_idx].field_name.clone();
    if let Some(conflict_span) = find_field_conflict(fields, &field_name) {
        return Err(TransformError::ScopeError {
            message: format!(
                "クラス '{}' には既にロガーフィールド '{}' が定義されています",
                fqcn, field_name
            ),
            span: conflict_span,
        });
    }

    let field_span = primary_span.unwrap_or_else(|| class_span.clone());
    let field_statement =
        build_logger_field_statement(framework, &class_id, &field_name, &field_span)?;

    fields.insert(0, field_statement);

    Ok(())
}

fn find_field_conflict(fields: &[IrStatement], target: &str) -> Option<Span> {
    for field in fields {
        match field {
            IrStatement::FieldDeclaration { name, span, .. } if name == target => {
                return Some(span.clone());
            }
            IrStatement::Commented { statement, .. } => {
                if let Some(span) = find_field_conflict(std::slice::from_ref(statement), target) {
                    return Some(span);
                }
            }
            _ => {}
        }
    }
    None
}

fn build_logger_field_statement(
    framework: &LoggingFrameworkKind,
    class_id: &ClassId,
    field_name: &str,
    span: &Span,
) -> Result<IrStatement, TransformError> {
    let (java_type, initializer) = build_logger_initializer(framework, class_id, span)?;
    let mut modifiers = IrModifiers::default();
    modifiers.visibility = IrVisibility::Private;
    modifiers.is_static = true;
    modifiers.is_final = true;

    Ok(IrStatement::FieldDeclaration {
        name: field_name.to_string(),
        java_type,
        initializer: Some(initializer),
        modifiers,
        span: span.clone(),
    })
}

fn build_logger_initializer(
    framework: &LoggingFrameworkKind,
    class_id: &ClassId,
    span: &Span,
) -> Result<(JavaType, IrExpression), TransformError> {
    let fqcn = class_fqcn_from_id(class_id);
    match framework {
        LoggingFrameworkKind::Slf4j => {
            let logger_type = JavaType::Reference {
                name: "org.slf4j.Logger".to_string(),
                generic_args: vec![],
            };
            let factory_name = "org.slf4j.LoggerFactory".to_string();
            let factory_type = JavaType::Reference {
                name: factory_name.clone(),
                generic_args: vec![],
            };
            let receiver = IrExpression::Identifier {
                name: factory_name,
                java_type: factory_type,
                span: span.clone(),
            };
            let call = IrExpression::MethodCall {
                receiver: Some(Box::new(receiver)),
                method_name: "getLogger".to_string(),
                java_name: None,
                resolved_target: None,
                args: vec![class_literal_expression(class_id, span)],
                argument_style: CallArgumentStyle::Comma,
                java_type: logger_type.clone(),
                span: span.clone(),
            };
            Ok((logger_type, call))
        }
        LoggingFrameworkKind::Log4j2 => {
            let logger_type = JavaType::Reference {
                name: "org.apache.logging.log4j.Logger".to_string(),
                generic_args: vec![],
            };
            let manager_name = "org.apache.logging.log4j.LogManager".to_string();
            let manager_type = JavaType::Reference {
                name: manager_name.clone(),
                generic_args: vec![],
            };
            let receiver = IrExpression::Identifier {
                name: manager_name,
                java_type: manager_type,
                span: span.clone(),
            };
            let call = IrExpression::MethodCall {
                receiver: Some(Box::new(receiver)),
                method_name: "getLogger".to_string(),
                java_name: None,
                resolved_target: None,
                args: vec![class_literal_expression(class_id, span)],
                argument_style: CallArgumentStyle::Comma,
                java_type: logger_type.clone(),
                span: span.clone(),
            };
            Ok((logger_type, call))
        }
        LoggingFrameworkKind::JbossLogging => {
            let logger_name = "org.jboss.logging.Logger".to_string();
            let logger_type = JavaType::Reference {
                name: logger_name.clone(),
                generic_args: vec![],
            };
            let receiver = IrExpression::Identifier {
                name: logger_name,
                java_type: logger_type.clone(),
                span: span.clone(),
            };
            let call = IrExpression::MethodCall {
                receiver: Some(Box::new(receiver)),
                method_name: "getLogger".to_string(),
                java_name: None,
                resolved_target: None,
                args: vec![class_literal_expression(class_id, span)],
                argument_style: CallArgumentStyle::Comma,
                java_type: logger_type.clone(),
                span: span.clone(),
            };
            Ok((logger_type, call))
        }
        LoggingFrameworkKind::CommonsLogging => {
            let logger_type = JavaType::Reference {
                name: "org.apache.commons.logging.Log".to_string(),
                generic_args: vec![],
            };
            let factory_name = "org.apache.commons.logging.LogFactory".to_string();
            let factory_type = JavaType::Reference {
                name: factory_name.clone(),
                generic_args: vec![],
            };
            let receiver = IrExpression::Identifier {
                name: factory_name,
                java_type: factory_type,
                span: span.clone(),
            };
            let call = IrExpression::MethodCall {
                receiver: Some(Box::new(receiver)),
                method_name: "getLog".to_string(),
                java_name: None,
                resolved_target: None,
                args: vec![class_literal_expression(class_id, span)],
                argument_style: CallArgumentStyle::Comma,
                java_type: logger_type.clone(),
                span: span.clone(),
            };
            Ok((logger_type, call))
        }
        LoggingFrameworkKind::Jul => {
            let logger_name = "java.util.logging.Logger".to_string();
            let logger_type = JavaType::Reference {
                name: logger_name.clone(),
                generic_args: vec![],
            };
            let receiver = IrExpression::Identifier {
                name: logger_name,
                java_type: logger_type.clone(),
                span: span.clone(),
            };
            let call = IrExpression::MethodCall {
                receiver: Some(Box::new(receiver)),
                method_name: "getLogger".to_string(),
                java_name: None,
                resolved_target: None,
                args: vec![IrExpression::Literal(Literal::String(fqcn), span.clone())],
                argument_style: CallArgumentStyle::Comma,
                java_type: logger_type.clone(),
                span: span.clone(),
            };
            Ok((logger_type, call))
        }
        LoggingFrameworkKind::Custom { identifier } => Err(TransformError::UnsupportedConstruct {
            construct: format!(
                "カスタムロギングフレームワーク '{}' の Logger フィールド生成",
                identifier.clone().unwrap_or_else(|| "unknown".to_string())
            ),
            span: span.clone(),
        }),
    }
}

fn class_literal_expression(class_id: &ClassId, span: &Span) -> IrExpression {
    let class_name = class_fqcn_from_id(class_id);
    let class_type = JavaType::Reference {
        name: class_name.clone(),
        generic_args: vec![],
    };
    let identifier = IrExpression::Identifier {
        name: class_name,
        java_type: class_type,
        span: span.clone(),
    };
    IrExpression::FieldAccess {
        receiver: Box::new(identifier),
        field_name: "class".to_string(),
        java_type: JavaType::Reference {
            name: "java.lang.Class".to_string(),
            generic_args: vec![],
        },
        span: span.clone(),
        is_record_component: false,
    }
}

fn class_fqcn_from_id(class_id: &ClassId) -> String {
    class_fqcn(class_id.package.as_deref(), &class_id.local_name)
}

fn class_fqcn(package: Option<&str>, local: &[String]) -> String {
    let local_path = local.join(".");
    match package {
        Some(pkg) if !pkg.is_empty() => format!("{pkg}.{local_path}"),
        _ => local_path,
    }
}

struct LoggerPlanCollector<'a> {
    class_id: ClassId,
    canonical: Option<LoggerFieldId>,
    primary_span: Option<Span>,
    duplicates: Vec<LoggerFieldId>,
    used_ids: &'a mut HashSet<LoggerFieldId>,
}

impl<'a> LoggerPlanCollector<'a> {
    fn new(class_id: ClassId, used_ids: &'a mut HashSet<LoggerFieldId>) -> Self {
        Self {
            class_id,
            canonical: None,
            primary_span: None,
            duplicates: Vec::new(),
            used_ids,
        }
    }

    fn canonical_id(&self) -> Option<LoggerFieldId> {
        self.canonical
    }

    fn primary_span(&self) -> Option<&Span> {
        self.primary_span.as_ref()
    }

    fn into_duplicates(self) -> Vec<LoggerFieldId> {
        self.duplicates
    }

    fn visit_statement(&mut self, stmt: &mut IrStatement) {
        match stmt {
            IrStatement::VariableDeclaration { initializer, .. }
            | IrStatement::FieldDeclaration { initializer, .. } => {
                if let Some(expr) = initializer {
                    self.visit_expression(expr);
                }
            }
            IrStatement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.visit_expression(expr);
                }
            }
            IrStatement::Expression { expr, .. } => {
                self.visit_expression(expr);
            }
            IrStatement::Block { statements, .. } => {
                for stmt in statements {
                    self.visit_statement(stmt);
                }
            }
            IrStatement::If {
                condition,
                then_stmt,
                else_stmt,
                ..
            } => {
                self.visit_expression(condition);
                self.visit_statement(then_stmt);
                if let Some(otherwise) = else_stmt.as_deref_mut() {
                    self.visit_statement(otherwise);
                }
            }
            IrStatement::While {
                condition, body, ..
            } => {
                self.visit_expression(condition);
                self.visit_statement(body);
            }
            IrStatement::For {
                init,
                condition,
                update,
                body,
                ..
            } => {
                if let Some(init_stmt) = init.as_deref_mut() {
                    self.visit_statement(init_stmt);
                }
                if let Some(cond) = condition {
                    self.visit_expression(cond);
                }
                if let Some(update_expr) = update {
                    self.visit_expression(update_expr);
                }
                self.visit_statement(body);
            }
            IrStatement::ForEach { iterable, body, .. } => {
                self.visit_expression(iterable);
                self.visit_statement(body);
            }
            IrStatement::Switch {
                discriminant,
                cases,
                ..
            } => {
                self.visit_expression(discriminant);
                for case in cases {
                    self.visit_expression(&mut case.body);
                }
            }
            IrStatement::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                self.visit_statement(body);
                for clause in catch_clauses {
                    self.visit_statement(&mut clause.body);
                }
                if let Some(finally_stmt) = finally_block.as_deref_mut() {
                    self.visit_statement(finally_stmt);
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
                    self.visit_expression(&mut resource.initializer);
                }
                self.visit_statement(body);
                for clause in catch_clauses {
                    self.visit_statement(&mut clause.body);
                }
                if let Some(finally_stmt) = finally_block.as_deref_mut() {
                    self.visit_statement(finally_stmt);
                }
            }
            IrStatement::Throw { expr, .. } => {
                self.visit_expression(expr);
            }
            IrStatement::MethodDeclaration { body, .. } => {
                if let Some(expr) = body {
                    self.visit_expression(expr);
                }
            }
            IrStatement::Commented { statement, .. } => {
                self.visit_statement(statement);
            }
            IrStatement::Comment { .. }
            | IrStatement::Import(_)
            | IrStatement::Package { .. }
            | IrStatement::Break { .. }
            | IrStatement::Continue { .. }
            | IrStatement::InterfaceDeclaration { .. }
            | IrStatement::RecordDeclaration { .. }
            | IrStatement::SampleDeclaration(_)
            | IrStatement::ClassDeclaration { .. } => {}
        }
    }

    fn visit_expression(&mut self, expr: &mut IrExpression) {
        match expr {
            IrExpression::Literal(_, _)
            | IrExpression::Identifier { .. }
            | IrExpression::This { .. }
            | IrExpression::Super { .. } => {}
            IrExpression::MethodCall { receiver, args, .. } => {
                if let Some(target) = receiver.as_deref_mut() {
                    self.visit_expression(target);
                }
                for arg in args {
                    self.visit_expression(arg);
                }
            }
            IrExpression::FieldAccess { receiver, .. } => {
                self.visit_expression(receiver);
            }
            IrExpression::ArrayAccess { array, index, .. } => {
                self.visit_expression(array);
                self.visit_expression(index);
            }
            IrExpression::Binary { left, right, .. } => {
                self.visit_expression(left);
                self.visit_expression(right);
            }
            IrExpression::Unary { operand, .. } => {
                self.visit_expression(operand);
            }
            IrExpression::Assignment { target, value, .. } => {
                self.visit_expression(target);
                self.visit_expression(value);
            }
            IrExpression::Conditional {
                condition,
                then_expr,
                else_expr,
                ..
            } => {
                self.visit_expression(condition);
                self.visit_expression(then_expr);
                self.visit_expression(else_expr);
            }
            IrExpression::Block { statements, .. } => {
                for stmt in statements {
                    self.visit_statement(stmt);
                }
            }
            IrExpression::ArrayCreation {
                dimensions,
                initializer,
                ..
            } => {
                for dim in dimensions {
                    if let Some(expr) = dim {
                        self.visit_expression(expr);
                    }
                }
                if let Some(values) = initializer {
                    for value in values {
                        self.visit_expression(value);
                    }
                }
            }
            IrExpression::ObjectCreation { args, .. } => {
                for arg in args {
                    self.visit_expression(arg);
                }
            }
            IrExpression::Lambda { body, .. } => self.visit_expression(body),
            IrExpression::SequencePipeline { .. } => {}
            IrExpression::Switch {
                discriminant,
                cases,
                ..
            } => {
                self.visit_expression(discriminant);
                for case in cases {
                    self.visit_expression(&mut case.body);
                }
            }
            IrExpression::InstanceOf { expr: inner, .. } => self.visit_expression(inner),
            IrExpression::Cast { expr: inner, .. } => self.visit_expression(inner),
            IrExpression::TryWithResources {
                resources, body, ..
            } => {
                for resource in resources {
                    self.visit_expression(&mut resource.initializer);
                }
                self.visit_expression(body);
            }
            IrExpression::NullSafeOperation {
                expr,
                operation,
                default_value,
                ..
            } => {
                self.visit_expression(expr);
                self.visit_expression(operation);
                if let Some(default) = default_value {
                    self.visit_expression(default);
                }
            }
            IrExpression::StringFormat { args, .. }
            | IrExpression::CompletableFuture { args, .. }
            | IrExpression::VirtualThread { args, .. } => {
                for arg in args {
                    self.visit_expression(arg);
                }
            }
            IrExpression::LogInvocation { plan, .. } => self.visit_plan(plan),
            IrExpression::RegexCommand {
                subject,
                pattern_expr,
                replacement,
                ..
            } => {
                self.visit_expression(subject);
                if let Some(pattern_expr) = pattern_expr {
                    self.visit_expression(pattern_expr);
                }
                if let Some(replacement) = replacement {
                    self.visit_regex_replacement(replacement);
                }
            }
            IrExpression::RegexPattern { .. } | IrExpression::TextBlock { .. } => {}
        }
    }

    fn visit_regex_replacement(&mut self, replacement: &mut IrRegexReplacement) {
        match replacement {
            IrRegexReplacement::Literal(literal) => {
                for segment in &mut literal.segments {
                    if let IrRegexTemplateSegment::Expression(expr) = segment {
                        self.visit_expression(expr);
                    }
                }
            }
            IrRegexReplacement::Expression(expr) => self.visit_expression(expr),
        }
    }

    fn visit_plan(&mut self, plan: &mut LogInvocationPlan) {
        if self.canonical.is_none() {
            self.canonical = Some(plan.logger_field);
            self.primary_span = Some(plan.span.clone());
        } else if let Some(canonical) = self.canonical {
            if plan.logger_field != canonical {
                self.duplicates.push(plan.logger_field);
                self.used_ids.remove(&plan.logger_field);
                plan.logger_field = canonical;
            }
        }

        plan.class_id = Some(self.class_id.clone());
        self.used_ids.insert(plan.logger_field);

        for item in plan.items.iter_mut() {
            match item {
                LogInvocationItem::Statement(stmt) => self.visit_statement(stmt),
                LogInvocationItem::Message(message) => {
                    self.visit_expression(&mut message.expression);
                }
                LogInvocationItem::Nested(nested) => self.visit_plan(nested),
            }
        }
    }
}

fn collect_plan_ids(stmt: &IrStatement, used: &mut HashSet<LoggerFieldId>) {
    match stmt {
        IrStatement::VariableDeclaration { initializer, .. }
        | IrStatement::FieldDeclaration { initializer, .. } => {
            if let Some(expr) = initializer {
                collect_plan_ids_from_expression(expr, used);
            }
        }
        IrStatement::Return { value, .. } => {
            if let Some(expr) = value {
                collect_plan_ids_from_expression(expr, used);
            }
        }
        IrStatement::Expression { expr, .. } => collect_plan_ids_from_expression(expr, used),
        IrStatement::Block { statements, .. } => {
            for stmt in statements {
                collect_plan_ids(stmt, used);
            }
        }
        IrStatement::If {
            condition,
            then_stmt,
            else_stmt,
            ..
        } => {
            collect_plan_ids_from_expression(condition, used);
            collect_plan_ids(then_stmt, used);
            if let Some(otherwise) = else_stmt.as_deref() {
                collect_plan_ids(otherwise, used);
            }
        }
        IrStatement::While {
            condition, body, ..
        } => {
            collect_plan_ids_from_expression(condition, used);
            collect_plan_ids(body, used);
        }
        IrStatement::For {
            init,
            condition,
            update,
            body,
            ..
        } => {
            if let Some(init_stmt) = init.as_deref() {
                collect_plan_ids(init_stmt, used);
            }
            if let Some(cond) = condition {
                collect_plan_ids_from_expression(cond, used);
            }
            if let Some(update_expr) = update {
                collect_plan_ids_from_expression(update_expr, used);
            }
            collect_plan_ids(body, used);
        }
        IrStatement::ForEach { iterable, body, .. } => {
            collect_plan_ids_from_expression(iterable, used);
            collect_plan_ids(body, used);
        }
        IrStatement::Switch {
            discriminant,
            cases,
            ..
        } => {
            collect_plan_ids_from_expression(discriminant, used);
            for case in cases {
                collect_plan_ids_from_expression(&case.body, used);
            }
        }
        IrStatement::Try {
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            collect_plan_ids(body, used);
            for clause in catch_clauses {
                collect_plan_ids(&clause.body, used);
            }
            if let Some(finally_stmt) = finally_block.as_deref() {
                collect_plan_ids(finally_stmt, used);
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
                collect_plan_ids_from_expression(&resource.initializer, used);
            }
            collect_plan_ids(body, used);
            for clause in catch_clauses {
                collect_plan_ids(&clause.body, used);
            }
            if let Some(finally_stmt) = finally_block.as_deref() {
                collect_plan_ids(finally_stmt, used);
            }
        }
        IrStatement::Throw { expr, .. } => {
            collect_plan_ids_from_expression(expr, used);
        }
        IrStatement::MethodDeclaration { body, .. } => {
            if let Some(expr) = body {
                collect_plan_ids_from_expression(expr, used);
            }
        }
        IrStatement::Commented { statement, .. } => collect_plan_ids(statement, used),
        IrStatement::ClassDeclaration {
            fields,
            methods,
            nested_classes,
            ..
        } => {
            for field in fields {
                collect_plan_ids(field, used);
            }
            for method in methods {
                collect_plan_ids(method, used);
            }
            for nested in nested_classes {
                collect_plan_ids(nested, used);
            }
        }
        IrStatement::InterfaceDeclaration { .. }
        | IrStatement::RecordDeclaration { .. }
        | IrStatement::Import(_)
        | IrStatement::Package { .. }
        | IrStatement::SampleDeclaration(_)
        | IrStatement::Comment { .. }
        | IrStatement::Break { .. }
        | IrStatement::Continue { .. } => {}
    }
}

fn collect_plan_ids_from_expression(expr: &IrExpression, used: &mut HashSet<LoggerFieldId>) {
    match expr {
        IrExpression::MethodCall { receiver, args, .. } => {
            if let Some(target) = receiver.as_deref() {
                collect_plan_ids_from_expression(target, used);
            }
            for arg in args {
                collect_plan_ids_from_expression(arg, used);
            }
        }
        IrExpression::FieldAccess { receiver, .. } => {
            collect_plan_ids_from_expression(receiver, used);
        }
        IrExpression::ArrayAccess { array, index, .. } => {
            collect_plan_ids_from_expression(array, used);
            collect_plan_ids_from_expression(index, used);
        }
        IrExpression::Binary { left, right, .. } => {
            collect_plan_ids_from_expression(left, used);
            collect_plan_ids_from_expression(right, used);
        }
        IrExpression::Unary { operand, .. } => collect_plan_ids_from_expression(operand, used),
        IrExpression::Assignment { target, value, .. } => {
            collect_plan_ids_from_expression(target, used);
            collect_plan_ids_from_expression(value, used);
        }
        IrExpression::Conditional {
            condition,
            then_expr,
            else_expr,
            ..
        } => {
            collect_plan_ids_from_expression(condition, used);
            collect_plan_ids_from_expression(then_expr, used);
            collect_plan_ids_from_expression(else_expr, used);
        }
        IrExpression::Block { statements, .. } => {
            for stmt in statements {
                collect_plan_ids(stmt, used);
            }
        }
        IrExpression::ArrayCreation {
            dimensions,
            initializer,
            ..
        } => {
            for dim in dimensions {
                if let Some(expr) = dim {
                    collect_plan_ids_from_expression(expr, used);
                }
            }
            if let Some(values) = initializer {
                for value in values {
                    collect_plan_ids_from_expression(value, used);
                }
            }
        }
        IrExpression::ObjectCreation { args, .. } => {
            for arg in args {
                collect_plan_ids_from_expression(arg, used);
            }
        }
        IrExpression::Lambda { body, .. } => collect_plan_ids_from_expression(body, used),
        IrExpression::Switch {
            discriminant,
            cases,
            ..
        } => {
            collect_plan_ids_from_expression(discriminant, used);
            for case in cases {
                collect_plan_ids_from_expression(&case.body, used);
            }
        }
        IrExpression::Cast { expr: inner, .. } => collect_plan_ids_from_expression(inner, used),
        IrExpression::TryWithResources {
            resources, body, ..
        } => {
            for resource in resources {
                collect_plan_ids_from_expression(&resource.initializer, used);
            }
            collect_plan_ids_from_expression(body, used);
        }
        IrExpression::NullSafeOperation {
            expr,
            operation,
            default_value,
            ..
        } => {
            collect_plan_ids_from_expression(expr, used);
            collect_plan_ids_from_expression(operation, used);
            if let Some(default) = default_value {
                collect_plan_ids_from_expression(default, used);
            }
        }
        IrExpression::StringFormat { args, .. }
        | IrExpression::CompletableFuture { args, .. }
        | IrExpression::VirtualThread { args, .. } => {
            for arg in args {
                collect_plan_ids_from_expression(arg, used);
            }
        }
        IrExpression::LogInvocation { plan, .. } => {
            collect_plan_ids_from_plan(plan, used);
        }
        IrExpression::RegexCommand {
            subject,
            pattern_expr,
            replacement,
            ..
        } => {
            collect_plan_ids_from_expression(subject, used);
            if let Some(pattern_expr) = pattern_expr {
                collect_plan_ids_from_expression(pattern_expr, used);
            }
            if let Some(replacement) = replacement {
                collect_plan_ids_from_regex_replacement(replacement, used);
            }
        }
        IrExpression::InstanceOf { expr: inner, .. } => {
            collect_plan_ids_from_expression(inner, used);
        }
        IrExpression::SequencePipeline { .. }
        | IrExpression::RegexPattern { .. }
        | IrExpression::TextBlock { .. }
        | IrExpression::Literal(_, _)
        | IrExpression::Identifier { .. }
        | IrExpression::This { .. }
        | IrExpression::Super { .. } => {}
    }
}

fn collect_plan_ids_from_regex_replacement(
    replacement: &IrRegexReplacement,
    used: &mut HashSet<LoggerFieldId>,
) {
    match replacement {
        IrRegexReplacement::Literal(literal) => {
            for segment in &literal.segments {
                if let IrRegexTemplateSegment::Expression(expr) = segment {
                    collect_plan_ids_from_expression(expr, used);
                }
            }
        }
        IrRegexReplacement::Expression(expr) => collect_plan_ids_from_expression(expr, used),
    }
}

fn collect_plan_ids_from_plan(plan: &LogInvocationPlan, used: &mut HashSet<LoggerFieldId>) {
    used.insert(plan.logger_field);
    for item in &plan.items {
        match item {
            LogInvocationItem::Statement(stmt) => collect_plan_ids(stmt, used),
            LogInvocationItem::Message(message) => {
                collect_plan_ids_from_expression(&message.expression, used);
            }
            LogInvocationItem::Nested(nested) => collect_plan_ids_from_plan(nested, used),
        }
    }
}
