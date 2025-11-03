use crate::CheckError;
use crate::null_safety::{NullSafetyContext, NullabilityKind};
use crate::pattern::{
    self, NarrowedNullability, NarrowingSnapshot, PatternMatchFacts, PatternMatchService,
    PatternTarget,
};
use jv_ast::expression::{Parameter, StringPart};
use jv_ast::statement::{Property, TestDataset};
use jv_ast::types::TypeAnnotation;
use jv_ast::{Expression, Program, Statement};
use jv_inference::types::TypeVariant as FactsTypeVariant;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct BridgeOutcome {
    pub diagnostics: Vec<CheckError>,
    pub assumptions_applied: usize,
}

impl BridgeOutcome {
    fn merge(&mut self, other: BridgeOutcome) {
        self.diagnostics.extend(other.diagnostics);
        self.assumptions_applied += other.assumptions_applied;
    }
}

pub struct PatternFactsBridge {
    target: PatternTarget,
    scopes: Vec<HashMap<String, NullabilityKind>>,
}

impl PatternFactsBridge {
    pub fn new() -> Self {
        Self {
            target: PatternTarget::default(),
            scopes: Vec::new(),
        }
    }

    pub fn apply_program(
        &mut self,
        program: &Program,
        service: &mut PatternMatchService,
        context: &mut NullSafetyContext,
    ) -> BridgeOutcome {
        let mut outcome = BridgeOutcome::default();
        for statement in &program.statements {
            outcome.merge(self.visit_statement(statement, service, context));
        }
        outcome
    }

    fn visit_statement(
        &mut self,
        statement: &Statement,
        service: &mut PatternMatchService,
        context: &mut NullSafetyContext,
    ) -> BridgeOutcome {
        match statement {
            Statement::ValDeclaration { initializer, .. } => {
                self.visit_expression(initializer, service, context)
            }
            Statement::VarDeclaration { initializer, .. } => initializer
                .as_ref()
                .map(|expr| self.visit_expression(expr, service, context))
                .unwrap_or_default(),
            Statement::FunctionDeclaration {
                name,
                parameters,
                body,
                ..
            } => {
                let scope = self.build_parameter_scope(name, parameters, context);
                self.scopes.push(scope);
                let outcome = self.visit_expression(body, service, context);
                self.scopes.pop();
                outcome
            }
            Statement::TestDeclaration(test) => {
                let mut outcome = BridgeOutcome::default();
                if let Some(dataset) = &test.dataset {
                    outcome.merge(self.visit_test_dataset(dataset, service, context));
                }
                outcome.merge(self.visit_expression(&test.body, service, context));
                outcome
            }
            Statement::ClassDeclaration {
                properties,
                methods,
                ..
            }
            | Statement::InterfaceDeclaration {
                properties,
                methods,
                ..
            } => {
                let mut outcome = BridgeOutcome::default();
                for property in properties {
                    outcome.merge(self.visit_property(property, service, context));
                }
                for method in methods {
                    outcome.merge(self.visit_statement(method, service, context));
                }
                outcome
            }
            Statement::ExtensionFunction(extension) => {
                self.visit_statement(&extension.function, service, context)
            }
            Statement::Expression { expr, .. } => self.visit_expression(expr, service, context),
            Statement::Return { value, .. } => value
                .as_ref()
                .map(|expr| self.visit_expression(expr, service, context))
                .unwrap_or_default(),
            Statement::Throw { expr, .. } => self.visit_expression(expr, service, context),
            Statement::Assignment { target, value, .. } => {
                let mut outcome = self.visit_expression(target, service, context);
                outcome.merge(self.visit_expression(value, service, context));
                outcome
            }
            Statement::ForIn(for_in) => {
                let mut outcome = self.visit_expression(&for_in.iterable, service, context);
                outcome.merge(self.visit_expression(&for_in.body, service, context));
                outcome
            }
            Statement::Concurrency(construct) => match construct {
                jv_ast::statement::ConcurrencyConstruct::Spawn { body, .. }
                | jv_ast::statement::ConcurrencyConstruct::Async { body, .. } => {
                    self.visit_expression(body, service, context)
                }
                jv_ast::statement::ConcurrencyConstruct::Await { expr, .. } => {
                    self.visit_expression(expr, service, context)
                }
            },
            Statement::ResourceManagement(resource) => match resource {
                jv_ast::statement::ResourceManagement::Use { resource, body, .. } => {
                    let mut outcome = self.visit_expression(resource, service, context);
                    outcome.merge(self.visit_expression(body, service, context));
                    outcome
                }
                jv_ast::statement::ResourceManagement::Defer { body, .. } => {
                    self.visit_expression(body, service, context)
                }
            },
            Statement::DataClassDeclaration { .. }
            | Statement::Break(_)
            | Statement::Continue(_)
            | Statement::Import { .. }
            | Statement::Package { .. }
            | Statement::Comment(_) => BridgeOutcome::default(),
        }
    }

    fn visit_test_dataset(
        &mut self,
        dataset: &TestDataset,
        service: &mut PatternMatchService,
        context: &mut NullSafetyContext,
    ) -> BridgeOutcome {
        match dataset {
            TestDataset::InlineArray { rows, .. } => {
                let mut outcome = BridgeOutcome::default();
                for row in rows {
                    for value in &row.values {
                        outcome.merge(self.visit_expression(value, service, context));
                    }
                }
                outcome
            }
            TestDataset::Sample(_) => BridgeOutcome::default(),
        }
    }

    fn visit_property(
        &mut self,
        property: &Property,
        service: &mut PatternMatchService,
        context: &mut NullSafetyContext,
    ) -> BridgeOutcome {
        let mut outcome = BridgeOutcome::default();
        if let Some(initializer) = &property.initializer {
            outcome.merge(self.visit_expression(initializer, service, context));
        }
        if let Some(getter) = &property.getter {
            outcome.merge(self.visit_expression(getter, service, context));
        }
        if let Some(setter) = &property.setter {
            outcome.merge(self.visit_expression(setter, service, context));
        }
        outcome
    }

    fn visit_expression(
        &mut self,
        expression: &Expression,
        service: &mut PatternMatchService,
        context: &mut NullSafetyContext,
    ) -> BridgeOutcome {
        match expression {
            Expression::RegexLiteral(_) => BridgeOutcome::default(),
            Expression::Literal(_, _) | Expression::Identifier(_, _) => BridgeOutcome::default(),
            Expression::Unary { operand, .. } => self.visit_expression(operand, service, context),
            Expression::Binary { left, right, .. } => {
                let mut outcome = self.visit_expression(left, service, context);
                outcome.merge(self.visit_expression(right, service, context));
                outcome
            }
            Expression::Call { function, args, .. } => {
                let mut outcome = self.visit_expression(function, service, context);
                for argument in args {
                    match argument {
                        jv_ast::expression::Argument::Positional(expr) => {
                            outcome.merge(self.visit_expression(expr, service, context));
                        }
                        jv_ast::expression::Argument::Named { value, .. } => {
                            outcome.merge(self.visit_expression(value, service, context));
                        }
                    }
                }
                outcome
            }
            Expression::MemberAccess { object, .. }
            | Expression::NullSafeMemberAccess { object, .. } => {
                self.visit_expression(object, service, context)
            }
            Expression::IndexAccess { object, index, .. }
            | Expression::NullSafeIndexAccess { object, index, .. } => {
                let mut outcome = self.visit_expression(object, service, context);
                outcome.merge(self.visit_expression(index, service, context));
                outcome
            }
            Expression::TypeCast { expr, .. } => self.visit_expression(expr, service, context),
            Expression::Array { elements, .. } => {
                let mut outcome = BridgeOutcome::default();
                for element in elements {
                    outcome.merge(self.visit_expression(element, service, context));
                }
                outcome
            }
            Expression::Lambda {
                parameters, body, ..
            } => {
                let scope = self.build_lambda_scope(parameters);
                if !scope.is_empty() {
                    self.scopes.push(scope);
                    let outcome = self.visit_expression(body, service, context);
                    self.scopes.pop();
                    outcome
                } else {
                    self.visit_expression(body, service, context)
                }
            }
            Expression::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                let mut outcome = self.visit_expression(body, service, context);
                for clause in catch_clauses {
                    outcome.merge(self.visit_expression(&clause.body, service, context));
                }
                if let Some(finally_expr) = finally_block.as_deref() {
                    outcome.merge(self.visit_expression(finally_expr, service, context));
                }
                outcome
            }
            Expression::StringInterpolation { parts, .. } => {
                let mut outcome = BridgeOutcome::default();
                for part in parts {
                    if let StringPart::Expression(expr) = part {
                        outcome.merge(self.visit_expression(expr, service, context));
                    }
                }
                outcome
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let mut outcome = self.visit_expression(condition, service, context);
                outcome.merge(self.visit_expression(then_branch, service, context));
                if let Some(else_expr) = else_branch.as_deref() {
                    outcome.merge(self.visit_expression(else_expr, service, context));
                }
                outcome
            }
            Expression::Block { statements, .. } => {
                let mut outcome = BridgeOutcome::default();
                for statement in statements {
                    outcome.merge(self.visit_statement(statement, service, context));
                }
                outcome
            }
            Expression::When {
                expr: subject,
                arms,
                else_arm,
                implicit_end,
                ..
            } => {
                let mut outcome = BridgeOutcome::default();
                let (facts, node_id) = if let Some(span) = pattern::expression_span(expression) {
                    let node_id = pattern::node_identifier(span);
                    if let Some(existing) = context.pattern_facts(node_id) {
                        (existing.clone(), Some(node_id))
                    } else {
                        let analyzed = service.analyze(expression, self.target);
                        context.register_pattern_facts(node_id, analyzed.clone());
                        (analyzed, Some(node_id))
                    }
                } else {
                    (service.analyze(expression, self.target), None)
                };
                if let Some(id) = node_id {
                    outcome.assumptions_applied += facts.narrowing().arms().count();
                    outcome
                        .diagnostics
                        .extend(self.detect_conflicts(&facts, context));
                    context.register_pattern_facts(id, facts.clone());
                }
                if node_id.is_none() {
                    outcome.assumptions_applied += facts.narrowing().arms().count();
                    outcome
                        .diagnostics
                        .extend(self.detect_conflicts(&facts, context));
                }
                if let Some(subject_expr) = subject.as_deref() {
                    outcome.merge(self.visit_expression(subject_expr, service, context));
                }
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        outcome.merge(self.visit_expression(guard, service, context));
                    }
                    outcome.merge(self.visit_expression(&arm.body, service, context));
                }
                if let Some(else_expr) = else_arm.as_deref() {
                    outcome.merge(self.visit_expression(else_expr, service, context));
                }
                if let Some(jv_ast::expression::ImplicitWhenEnd::Unit { .. }) = implicit_end {
                    // Nothing additional to traverse for implicit unit branch.
                }
                outcome
            }
            Expression::MultilineString { .. }
            | Expression::JsonLiteral { .. }
            | Expression::This(_)
            | Expression::Super(_) => BridgeOutcome::default(),
        }
    }

    fn build_parameter_scope(
        &self,
        function_name: &str,
        parameters: &[Parameter],
        context: &NullSafetyContext,
    ) -> HashMap<String, NullabilityKind> {
        let mut scope = HashMap::new();

        if let Some(facts) = context.facts() {
            if let Some(signature) = facts.function_signature(function_name) {
                if let FactsTypeVariant::Function(param_types, _) = signature.body.variant() {
                    for (parameter, ty) in parameters.iter().zip(param_types.iter()) {
                        scope.insert(parameter.name.clone(), NullabilityKind::from_facts_type(ty));
                    }
                }
            }
        }

        for parameter in parameters {
            if scope.contains_key(&parameter.name) {
                continue;
            }
            if let Some(annotation) = &parameter.type_annotation {
                let state = state_from_annotation(annotation);
                scope.insert(parameter.name.clone(), state);
            }
        }

        scope
    }

    fn build_lambda_scope(&self, parameters: &[Parameter]) -> HashMap<String, NullabilityKind> {
        let mut scope = HashMap::new();
        for parameter in parameters {
            if let Some(annotation) = &parameter.type_annotation {
                scope.insert(parameter.name.clone(), state_from_annotation(annotation));
            }
        }
        scope
    }

    fn lookup_state(&self, name: &str, context: &NullSafetyContext) -> Option<NullabilityKind> {
        for scope in self.scopes.iter().rev() {
            if let Some(state) = scope.get(name) {
                return Some(self.apply_contract_override(name, *state, context));
            }
        }

        let recorded = context.lattice().get(name);
        if context.contracts().contains(name) {
            Some(NullabilityKind::NonNull)
        } else {
            recorded
        }
    }

    fn apply_contract_override(
        &self,
        name: &str,
        state: NullabilityKind,
        context: &NullSafetyContext,
    ) -> NullabilityKind {
        if context.contracts().contains(name) {
            NullabilityKind::NonNull
        } else {
            state
        }
    }

    fn detect_conflicts(
        &self,
        facts: &PatternMatchFacts,
        context: &NullSafetyContext,
    ) -> Vec<CheckError> {
        let mut diagnostics = Vec::new();

        for snapshot in facts.narrowing().arms().map(|(_, snap)| snap) {
            diagnostics.extend(self.conflicts_for_snapshot(snapshot, context));
        }

        if let Some(fallback) = facts.fallback_narrowing() {
            diagnostics.extend(self.conflicts_for_snapshot(fallback, context));
        }

        diagnostics
    }

    fn conflicts_for_snapshot(
        &self,
        snapshot: &NarrowingSnapshot,
        context: &NullSafetyContext,
    ) -> Vec<CheckError> {
        snapshot
            .on_match()
            .iter()
            .filter_map(|binding| {
                if !matches!(binding.nullability, NarrowedNullability::Nullable) {
                    return None;
                }
                let recorded = self.lookup_state(&binding.variable, context)?;
                if matches!(recorded, NullabilityKind::NonNull) {
                    Some(CheckError::NullSafetyError(conflict_message(
                        &binding.variable,
                    )))
                } else {
                    None
                }
            })
            .collect()
    }
}

fn conflict_message(variable: &str) -> String {
    format!(
        "JV3108: `{variable}` は non-null と推論されていますが、when 分岐で null と比較されています。分岐を削除するか型を nullable に変更してください。\nJV3108: `{variable}` is inferred as non-null but the when expression compares it against null. Remove the branch or update the type to be nullable.\nQuick Fix: when.remove.null-branch -> `{variable}` の null 分岐を削除\nQuick Fix: when.remove.null-branch -> remove the null arm for `{variable}` or declare the type as nullable"
    )
}

fn state_from_annotation(annotation: &TypeAnnotation) -> NullabilityKind {
    match annotation {
        TypeAnnotation::Nullable(_) => NullabilityKind::Nullable,
        _ => NullabilityKind::NonNull,
    }
}
