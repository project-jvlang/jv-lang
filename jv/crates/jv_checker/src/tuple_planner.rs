//! Tuple record planning utilities for multi-return tuples.
//!
//! This module scans the normalized AST to identify tuple literals and records how they
//! are used throughout the program. The gathered information is converted into
//! `TupleRecordPlan` structures that downstream stages (type facts, IR, codegen) can
//! consume to decide whether a dedicated record type should be generated or a generic
//! `TupleN_*` record can be reused.

use crate::inference::type_parser;
use jv_ast::expression::{
    Argument, Expression, LogBlock, LogItem, RegexCommand, RegexReplacement,
    RegexTemplateSegment, StringPart, TryCatchClause, TupleFieldMeta, WhenArm,
};
use jv_ast::types::{Span, TupleTypeDescriptor, TypeAnnotation};
use jv_ast::{
    ConcurrencyConstruct, ForInStatement, Program, ResourceManagement, Statement, TestDataset,
    TestDeclaration, UnitTypeDefinition, UnitTypeMember,
};
use jv_ir::{TupleRecordPlan, TupleRecordStrategy, TupleUsageContext, TupleUsageKind};
use std::collections::{HashMap, HashSet};

const UNKNOWN_HINT: &str = "Unknown";

/// Planner responsible for analysing tuple literals inside a program.
#[derive(Default)]
pub struct TuplePlanner {
    function_stack: Vec<FunctionContext>,
    plans: HashMap<TupleKey, TuplePlanAccumulator>,
}

impl TuplePlanner {
    /// Builds tuple record plans for the provided program.
    pub fn plan_program(program: &Program) -> Vec<TupleRecordPlan> {
        let mut planner = Self::default();
        planner.visit_program(program);
        planner.into_plans()
    }

    fn visit_program(&mut self, program: &Program) {
        for statement in &program.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Comment(_) | Statement::Import { .. } | Statement::Package { .. } => {}
            Statement::ValDeclaration {
                name,
                type_annotation,
                initializer,
                ..
            } => {
                let descriptor = type_annotation
                    .as_ref()
                    .and_then(|ann| parse_tuple_descriptor(ann));
                let context = ExpressionContext::with_usage(
                    TupleUsageKind::BindingInitializer,
                    Some(name.clone()),
                    descriptor,
                );
                self.visit_expression(initializer, &context);
            }
            Statement::VarDeclaration {
                name,
                type_annotation,
                initializer,
                ..
            } => {
                if let Some(expr) = initializer {
                    let descriptor = type_annotation
                        .as_ref()
                        .and_then(|ann| parse_tuple_descriptor(ann));
                    let context = ExpressionContext::with_usage(
                        TupleUsageKind::BindingInitializer,
                        Some(name.clone()),
                        descriptor,
                    );
                    self.visit_expression(expr, &context);
                }
            }
            Statement::FunctionDeclaration {
                name,
                return_type,
                body,
                ..
            } => {
                let descriptor = return_type
                    .as_ref()
                    .and_then(|ann| parse_tuple_descriptor(ann));
                self.function_stack
                    .push(FunctionContext::new(name.clone(), descriptor));
                self.visit_expression(body, &ExpressionContext::general());
                self.function_stack.pop();
            }
            Statement::ClassDeclaration {
                properties,
                methods,
                ..
            } => {
                for property in properties {
                    if let Some(initializer) = &property.initializer {
                        self.visit_expression(initializer, &ExpressionContext::general());
                    }
                    if let Some(getter) = &property.getter {
                        self.visit_expression(getter, &ExpressionContext::general());
                    }
                    if let Some(setter) = &property.setter {
                        self.visit_expression(setter, &ExpressionContext::general());
                    }
                }
                for method in methods {
                    self.visit_statement(method);
                }
            }
            Statement::InterfaceDeclaration {
                methods,
                properties,
                ..
            } => {
                for property in properties {
                    if let Some(initializer) = &property.initializer {
                        self.visit_expression(initializer, &ExpressionContext::general());
                    }
                }
                for method in methods {
                    self.visit_statement(method);
                }
            }
            Statement::DataClassDeclaration { .. } => {}
            Statement::ExtensionFunction(ext) => {
                self.visit_statement(&ext.function);
            }
            Statement::UnitTypeDefinition(definition) => {
                self.visit_unit_type_definition(definition);
            }
            Statement::TestDeclaration(declaration) => {
                self.visit_test_declaration(declaration);
            }
            Statement::Expression { expr, .. } => {
                self.visit_expression(expr, &ExpressionContext::general());
            }
            Statement::Return {
                value: Some(expr), ..
            } => {
                if let Some(function) = self.function_stack.last() {
                    let context = ExpressionContext::with_usage(
                        TupleUsageKind::FunctionReturn,
                        Some(function.name.clone()),
                        function.return_descriptor.clone(),
                    );
                    self.visit_expression(expr, &context);
                } else {
                    self.visit_expression(expr, &ExpressionContext::general());
                }
            }
            Statement::Return { value: None, .. } => {}
            Statement::Throw { expr, .. } => {
                self.visit_expression(expr, &ExpressionContext::general());
            }
            Statement::Assignment { target, value, .. } => {
                self.visit_expression(target, &ExpressionContext::general());
                self.visit_expression(
                    value,
                    &ExpressionContext::with_usage(TupleUsageKind::AssignmentValue, None, None),
                );
            }
            Statement::ForIn(for_in) => self.visit_for_in(for_in),
            Statement::Break(_) | Statement::Continue(_) => {}
            Statement::Concurrency(construct) => self.visit_concurrency(construct),
            Statement::ResourceManagement(resource) => self.visit_resource_management(resource),
        }
    }

    fn visit_for_in(&mut self, for_in: &ForInStatement) {
        self.visit_expression(&for_in.iterable, &ExpressionContext::general());
        self.visit_expression(&for_in.body, &ExpressionContext::general());
    }

    fn visit_concurrency(&mut self, construct: &ConcurrencyConstruct) {
        match construct {
            ConcurrencyConstruct::Spawn { body, .. } | ConcurrencyConstruct::Async { body, .. } => {
                self.visit_expression(body, &ExpressionContext::general());
            }
            ConcurrencyConstruct::Await { expr, .. } => {
                self.visit_expression(expr, &ExpressionContext::general());
            }
        }
    }

    fn visit_resource_management(&mut self, resource: &ResourceManagement) {
        match resource {
            ResourceManagement::Use { resource, body, .. } => {
                self.visit_expression(resource, &ExpressionContext::general());
                self.visit_expression(body, &ExpressionContext::general());
            }
            ResourceManagement::Defer { body, .. } => {
                self.visit_expression(body, &ExpressionContext::general());
            }
        }
    }

    fn visit_unit_type_definition(&mut self, definition: &UnitTypeDefinition) {
        for member in &definition.members {
            match member {
                UnitTypeMember::Dependency(dependency) => {
                    if let Some(value) = &dependency.value {
                        self.visit_expression(value, &ExpressionContext::general());
                    }
                }
                UnitTypeMember::Conversion(block) => {
                    for statement in &block.body {
                        self.visit_statement(statement);
                    }
                }
                UnitTypeMember::NestedStatement(statement) => {
                    self.visit_statement(statement);
                }
            }
        }
    }

    fn visit_test_declaration(&mut self, declaration: &TestDeclaration) {
        if let Some(dataset) = &declaration.dataset {
            self.visit_test_dataset(dataset);
        }
        self.visit_expression(&declaration.body, &ExpressionContext::general());
    }

    fn visit_test_dataset(&mut self, dataset: &TestDataset) {
        if let TestDataset::InlineArray { rows, .. } = dataset {
            for row in rows {
                for value in &row.values {
                    self.visit_expression(value, &ExpressionContext::general());
                }
            }
        }
    }

    fn visit_log_block(&mut self, block: &LogBlock) {
        for item in &block.items {
            match item {
                LogItem::Statement(statement) => self.visit_statement(statement),
                LogItem::Expression(expr) => {
                    self.visit_expression(expr, &ExpressionContext::general());
                }
                LogItem::Nested(nested) => self.visit_log_block(nested),
            }
        }
    }

    fn visit_regex_command(&mut self, command: &RegexCommand) {
        self.visit_expression(&command.subject, &ExpressionContext::general());
        if let Some(pattern_expr) = &command.pattern_expr {
            self.visit_expression(pattern_expr, &ExpressionContext::general());
        }
        if let Some(replacement) = &command.replacement {
            self.visit_regex_replacement(replacement);
        }
    }

    fn visit_regex_replacement(&mut self, replacement: &RegexReplacement) {
        match replacement {
            RegexReplacement::Literal(literal) => {
                for segment in &literal.template_segments {
                    if let RegexTemplateSegment::Expression(expr) = segment {
                        self.visit_expression(expr, &ExpressionContext::general());
                    }
                }
            }
            RegexReplacement::Lambda(lambda) => {
                for parameter in &lambda.params {
                    if let Some(default) = &parameter.default_value {
                        self.visit_expression(default, &ExpressionContext::general());
                    }
                }
                self.visit_expression(&lambda.body, &ExpressionContext::general());
            }
            RegexReplacement::Expression(expr) => {
                self.visit_expression(expr, &ExpressionContext::general());
            }
        }
    }

    fn visit_expression(&mut self, expression: &Expression, context: &ExpressionContext) {
        match expression {
            Expression::Literal(_, _)
            | Expression::Identifier(_, _)
            | Expression::This(_)
            | Expression::Super(_)
            | Expression::RegexLiteral(_)
            | Expression::JsonLiteral(_)
            | Expression::MultilineString(_) => {}
            Expression::Binary { left, right, .. } => {
                self.visit_expression(left, &ExpressionContext::general());
                self.visit_expression(right, &ExpressionContext::general());
            }
            Expression::Unary { operand, .. } => {
                self.visit_expression(operand, &ExpressionContext::general());
            }
            Expression::Call {
                function,
                args,
                type_arguments: _,
                ..
            } => {
                self.visit_expression(function, &ExpressionContext::general());
                for arg in args {
                    match arg {
                        Argument::Positional(expr) => {
                            self.visit_expression(expr, &ExpressionContext::general());
                        }
                        Argument::Named { value, .. } => {
                            self.visit_expression(value, &ExpressionContext::general());
                        }
                    }
                }
            }
            Expression::MemberAccess { object, .. }
            | Expression::NullSafeMemberAccess { object, .. } => {
                self.visit_expression(object, &ExpressionContext::general());
            }
            Expression::IndexAccess { object, index, .. }
            | Expression::NullSafeIndexAccess { object, index, .. } => {
                self.visit_expression(object, &ExpressionContext::general());
                self.visit_expression(index, &ExpressionContext::general());
            }
            Expression::TypeCast { expr, .. } => {
                self.visit_expression(expr, &ExpressionContext::general());
            }
            Expression::StringInterpolation { parts, .. } => {
                for part in parts {
                    if let StringPart::Expression(expr) = part {
                        self.visit_expression(expr, &ExpressionContext::general());
                    }
                }
            }
            Expression::When {
                expr,
                arms,
                else_arm,
                ..
            } => {
                if let Some(scrutinee) = expr {
                    self.visit_expression(scrutinee, &ExpressionContext::general());
                }
                for arm in arms {
                    self.visit_when_arm(arm);
                }
                if let Some(fallback) = else_arm {
                    self.visit_expression(fallback, &ExpressionContext::general());
                }
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.visit_expression(condition, &ExpressionContext::general());
                self.visit_expression(then_branch, &ExpressionContext::general());
                if let Some(branch) = else_branch {
                    self.visit_expression(branch, &ExpressionContext::general());
                }
            }
            Expression::Block { statements, .. } => {
                for statement in statements {
                    self.visit_statement(statement);
                }
            }
            Expression::Array { elements, .. } => {
                for element in elements {
                    self.visit_expression(element, &ExpressionContext::general());
                }
            }
            Expression::Tuple {
                elements,
                fields,
                span,
                ..
            } => {
                let arity = elements.len();
                if arity >= 2 {
                    self.record_tuple(arity, fields, span, context);
                }
                for element in elements {
                    self.visit_expression(element, &ExpressionContext::general());
                }
            }
            Expression::Lambda {
                parameters, body, ..
            } => {
                for parameter in parameters {
                    if let Some(default) = &parameter.default_value {
                        self.visit_expression(default, &ExpressionContext::general());
                    }
                }
                self.visit_expression(body, &ExpressionContext::general());
            }
            Expression::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                self.visit_expression(body, &ExpressionContext::general());
                for clause in catch_clauses {
                    self.visit_try_clause(clause);
                }
                if let Some(finally) = finally_block {
                    self.visit_expression(finally, &ExpressionContext::general());
                }
            }
            Expression::RegexCommand(command) => {
                self.visit_regex_command(command);
            }
            Expression::UnitLiteral { value, .. } => {
                self.visit_expression(value, &ExpressionContext::general());
            }
            Expression::LogBlock(block) => self.visit_log_block(block),
        }
    }

    fn visit_when_arm(&mut self, arm: &WhenArm) {
        if let Some(guard) = &arm.guard {
            self.visit_expression(guard, &ExpressionContext::general());
        }
        self.visit_expression(&arm.body, &ExpressionContext::general());
    }

    fn visit_try_clause(&mut self, clause: &TryCatchClause) {
        if let Some(parameter) = &clause.parameter {
            if let Some(default) = &parameter.default_value {
                self.visit_expression(default, &ExpressionContext::general());
            }
        }
        self.visit_expression(&clause.body, &ExpressionContext::general());
    }

    fn record_tuple(
        &mut self,
        arity: usize,
        fields: &[TupleFieldMeta],
        span: &Span,
        context: &ExpressionContext,
    ) {
        let normalized_fields = normalize_fields(fields, arity, span);
        let key = TupleKey::new(arity, &normalized_fields);
        let accumulator = self
            .plans
            .entry(key)
            .or_insert_with(|| TuplePlanAccumulator::new(normalized_fields.clone()));

        accumulator.merge_fields(&normalized_fields);
        let usage_kind = context.usage.unwrap_or(TupleUsageKind::Expression);
        accumulator.usage_sites.push(TupleUsageContext {
            kind: usage_kind,
            owner: context.owner.clone(),
            span: span.clone(),
        });
        if usage_kind == TupleUsageKind::FunctionReturn {
            if let Some(owner) = &context.owner {
                accumulator.function_owners.insert(owner.clone());
            }
        }

        if let Some(descriptor) = context.descriptor.as_ref() {
            if descriptor.arity() == arity {
                for (index, element) in descriptor.elements.iter().enumerate() {
                    let hint = annotation_hint(&element.ty);
                    accumulator.update_type_hint(index, hint);
                }
            }
        }
    }

    fn into_plans(self) -> Vec<TupleRecordPlan> {
        let mut plans = Vec::with_capacity(self.plans.len());
        for (key, accumulator) in self.plans {
            let strategy = if accumulator.function_owners.len() == 1
                && accumulator
                    .usage_sites
                    .iter()
                    .all(|usage| usage.kind == TupleUsageKind::FunctionReturn)
            {
                TupleRecordStrategy::Specific
            } else {
                TupleRecordStrategy::Generic
            };

            let specific_name = if strategy == TupleRecordStrategy::Specific {
                accumulator
                    .function_owners
                    .iter()
                    .next()
                    .map(|name| format!("{}_Result", to_pascal_case(name)))
            } else {
                None
            };
            let generic_name = build_generic_name(key.arity, &accumulator.type_hints);

            plans.push(TupleRecordPlan {
                arity: key.arity,
                strategy,
                specific_name,
                generic_name,
                fields: accumulator.fields,
                type_hints: accumulator.type_hints,
                usage_sites: accumulator.usage_sites,
            });
        }

        plans.sort_by(|lhs, rhs| {
            let left = lhs.specific_name.as_ref().unwrap_or(&lhs.generic_name);
            let right = rhs.specific_name.as_ref().unwrap_or(&rhs.generic_name);
            left.cmp(right)
        });

        plans
    }
}

#[derive(Debug, Clone)]
struct FunctionContext {
    name: String,
    return_descriptor: Option<TupleTypeDescriptor>,
}

impl FunctionContext {
    fn new(name: String, return_descriptor: Option<TupleTypeDescriptor>) -> Self {
        Self {
            name,
            return_descriptor,
        }
    }
}

#[derive(Debug, Clone, Default)]
struct ExpressionContext {
    usage: Option<TupleUsageKind>,
    owner: Option<String>,
    descriptor: Option<TupleTypeDescriptor>,
}

impl ExpressionContext {
    fn general() -> Self {
        Self::default()
    }

    fn with_usage(
        usage: TupleUsageKind,
        owner: Option<String>,
        descriptor: Option<TupleTypeDescriptor>,
    ) -> Self {
        Self {
            usage: Some(usage),
            owner,
            descriptor,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TupleKey {
    arity: usize,
    primary_labels: Vec<Option<String>>,
}

impl TupleKey {
    fn new(arity: usize, fields: &[TupleFieldMeta]) -> Self {
        let primary_labels = fields
            .iter()
            .map(|meta| {
                meta.primary_label
                    .as_ref()
                    .map(|label| label.trim().to_string())
            })
            .collect::<Vec<_>>();
        Self {
            arity,
            primary_labels,
        }
    }
}

#[derive(Debug, Clone)]
struct TuplePlanAccumulator {
    fields: Vec<TupleFieldMeta>,
    type_hints: Vec<String>,
    usage_sites: Vec<TupleUsageContext>,
    function_owners: HashSet<String>,
}

impl TuplePlanAccumulator {
    fn new(fields: Vec<TupleFieldMeta>) -> Self {
        let arity = fields.len();
        Self {
            fields,
            type_hints: vec![UNKNOWN_HINT.to_string(); arity],
            usage_sites: Vec::new(),
            function_owners: HashSet::new(),
        }
    }

    fn merge_fields(&mut self, fields: &[TupleFieldMeta]) {
        for (slot, new_meta) in self.fields.iter_mut().zip(fields.iter()) {
            if slot.primary_label.is_none() {
                slot.primary_label = new_meta.primary_label.clone();
            }
            if slot.identifier_hint.is_none() {
                slot.identifier_hint = new_meta.identifier_hint.clone();
            }
            if slot.secondary_labels.is_empty() && !new_meta.secondary_labels.is_empty() {
                slot.secondary_labels = new_meta.secondary_labels.clone();
            }
        }
    }

    fn update_type_hint(&mut self, index: usize, hint: String) {
        if index >= self.type_hints.len() {
            return;
        }
        if hint.is_empty() || hint == UNKNOWN_HINT {
            return;
        }
        if self.type_hints[index] == UNKNOWN_HINT {
            self.type_hints[index] = hint;
        }
    }
}

fn parse_tuple_descriptor(annotation: &TypeAnnotation) -> Option<TupleTypeDescriptor> {
    match type_parser::parse_tuple_descriptor(annotation) {
        Ok(Some(descriptor)) => Some(descriptor),
        _ => None,
    }
}

fn normalize_fields(fields: &[TupleFieldMeta], arity: usize, span: &Span) -> Vec<TupleFieldMeta> {
    let mut normalized = Vec::with_capacity(arity);
    for index in 0..arity {
        if let Some(meta) = fields.get(index) {
            normalized.push(meta.clone());
        } else {
            normalized.push(TupleFieldMeta::empty(index + 1, span.clone()));
        }
    }
    normalized
}

fn annotation_hint(annotation: &TypeAnnotation) -> String {
    let raw = flatten_annotation(annotation);
    let sanitized = raw.replace([':', '.', '<', '>', ',', '?', '[', ']', '-', ' '], "_");
    to_pascal_case(&sanitized)
}

fn flatten_annotation(annotation: &TypeAnnotation) -> String {
    match annotation {
        TypeAnnotation::Simple(name) => name.clone(),
        TypeAnnotation::Nullable(inner) => {
            format!("{}Optional", flatten_annotation(inner))
        }
        TypeAnnotation::Generic { name, type_args } => {
            if type_args.is_empty() {
                name.clone()
            } else {
                let args = type_args
                    .iter()
                    .map(flatten_annotation)
                    .collect::<Vec<_>>()
                    .join("");
                format!("{name}{args}")
            }
        }
        TypeAnnotation::Function { .. } => "Function".to_string(),
        TypeAnnotation::Array(inner) => format!("{}Array", flatten_annotation(inner)),
        TypeAnnotation::Unit { base, .. } => flatten_annotation(base),
    }
}

fn build_generic_name(arity: usize, type_hints: &[String]) -> String {
    let mut name = format!("Tuple{arity}");
    for hint in type_hints {
        name.push('_');
        if hint.is_empty() {
            name.push_str(UNKNOWN_HINT);
        } else {
            name.push_str(hint);
        }
    }
    name
}

fn to_pascal_case(value: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = true;

    for ch in value.chars() {
        if ch.is_ascii_alphanumeric() {
            if capitalize_next {
                result.push(ch.to_ascii_uppercase());
                capitalize_next = false;
            } else {
                result.push(ch.to_ascii_lowercase());
            }
        } else {
            capitalize_next = true;
        }
    }

    if result.is_empty() {
        UNKNOWN_HINT.to_string()
    } else {
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::expression::TupleContextFlags;
    use jv_ast::{Literal, Modifiers, Parameter, ParameterModifiers, ValBindingOrigin};

    #[test]
    fn plans_specific_record_for_function_return() {
        let span = dummy_span();
        let return_expr = tuple_expr(vec![identifier("left"), identifier("right")]);
        let return_stmt = Statement::Return {
            value: Some(return_expr),
            span: span.clone(),
        };
        let body = Expression::Block {
            statements: vec![return_stmt],
            span: span.clone(),
        };
        let parameters = vec![
            Parameter {
                name: "left".into(),
                type_annotation: None,
                default_value: None,
                modifiers: ParameterModifiers::default(),
                span: span.clone(),
            },
            Parameter {
                name: "right".into(),
                type_annotation: None,
                default_value: None,
                modifiers: ParameterModifiers::default(),
                span: span.clone(),
            },
        ];
        let function = Statement::FunctionDeclaration {
            name: "divmod".into(),
            type_parameters: Vec::new(),
            generic_signature: None,
            where_clause: None,
            parameters,
            return_type: None,
            primitive_return: None,
            body: Box::new(body),
            modifiers: Modifiers::default(),
            span: span.clone(),
        };
        let program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![function],
            span,
        };

        let plans = TuplePlanner::plan_program(&program);
        assert_eq!(plans.len(), 1);

        let plan = &plans[0];
        assert_eq!(plan.strategy, TupleRecordStrategy::Specific);
        assert_eq!(plan.specific_name.as_deref(), Some("Divmod_Result"));
        assert_eq!(plan.generic_name, "Tuple2_Unknown_Unknown");
        assert_eq!(plan.arity, 2);
        assert_eq!(
            plan.type_hints,
            vec!["Unknown".to_string(), "Unknown".to_string()]
        );
        assert_eq!(plan.usage_sites.len(), 1);
        assert_eq!(plan.usage_sites[0].kind, TupleUsageKind::FunctionReturn);
    }

    #[test]
    fn plans_generic_record_for_shared_tuple() {
        let span = dummy_span();
        let first_tuple = tuple_expr(vec![number("1"), number("2")]);
        let first = Statement::ValDeclaration {
            name: "first".into(),
            binding: None,
            type_annotation: None,
            initializer: first_tuple,
            modifiers: Modifiers::default(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        };

        let second_tuple = tuple_expr(vec![number("1"), number("2")]);
        let second = Statement::ValDeclaration {
            name: "second".into(),
            binding: None,
            type_annotation: None,
            initializer: second_tuple,
            modifiers: Modifiers::default(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        };

        let program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![first, second],
            span,
        };

        let plans = TuplePlanner::plan_program(&program);
        // Both tuple literals share the same generic tuple pattern.
        assert_eq!(plans.len(), 1);
        let plan = &plans[0];
        assert_eq!(plan.strategy, TupleRecordStrategy::Generic);
        assert_eq!(plan.generic_name, "Tuple2_Unknown_Unknown");
        assert_eq!(plan.usage_sites.len(), 2);
    }

    fn tuple_expr(elements: Vec<Expression>) -> Expression {
        let span = dummy_span();
        let fields = elements
            .iter()
            .enumerate()
            .map(|(index, _)| TupleFieldMeta::empty(index + 1, span.clone()))
            .collect();
        Expression::Tuple {
            elements,
            fields,
            context: TupleContextFlags::default(),
            span,
        }
    }

    fn identifier(name: &str) -> Expression {
        Expression::Identifier(name.into(), dummy_span())
    }

    fn number(value: &str) -> Expression {
        Expression::Literal(Literal::Number(value.into()), dummy_span())
    }

    fn dummy_span() -> Span {
        Span::new(1, 0, 1, 0)
    }
}
