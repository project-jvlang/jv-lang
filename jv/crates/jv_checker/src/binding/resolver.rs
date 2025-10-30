use std::collections::{HashMap, HashSet};

use crate::CheckError;
use jv_ast::Span;
use jv_ast::{
    Argument, ConcurrencyConstruct, Expression, ExtensionFunction, Modifiers, Program,
    ResourceManagement, Statement, TryCatchClause, ValBindingOrigin,
};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct BindingUsageSummary {
    pub explicit: usize,
    pub implicit: usize,
    pub implicit_typed: usize,
    pub vars: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LateInitSeed {
    pub name: String,
    pub origin: ValBindingOrigin,
    pub has_initializer: bool,
    pub explicit_late_init: bool,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LateInitManifest {
    pub items: HashMap<String, LateInitSeed>,
}

impl LateInitManifest {
    pub fn new(items: HashMap<String, LateInitSeed>) -> Self {
        Self { items }
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn get(&self, name: &str) -> Option<&LateInitSeed> {
        self.items.get(name)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &LateInitSeed)> {
        self.items.iter()
    }
}

#[derive(Debug)]
pub struct BindingResolution {
    pub program: Program,
    pub diagnostics: Vec<CheckError>,
    pub usage: BindingUsageSummary,
    pub late_init_manifest: LateInitManifest,
}

pub fn resolve_bindings(program: &Program) -> BindingResolution {
    BindingResolver::new().resolve(program)
}

struct BindingResolver {
    scopes: Vec<HashMap<String, BindingKind>>,
    diagnostics: Vec<CheckError>,
    usage: BindingUsageSummary,
    late_init_seeds: HashMap<String, LateInitSeed>,
    label_stack: Vec<LabelEntry>,
    label_scopes: Vec<HashSet<String>>,
}

#[derive(Clone)]
enum BindingKind {
    Immutable {
        _origin: ValBindingOrigin,
        _span: Span,
    },
    Mutable {
        _span: Span,
    },
}

#[derive(Debug, Clone)]
struct LabelEntry {
    name: String,
    kind: LabelKind,
    span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LabelKind {
    Loop,
    Block,
    When,
    Lambda,
}

impl BindingResolver {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            diagnostics: Vec::new(),
            usage: BindingUsageSummary::default(),
            late_init_seeds: HashMap::new(),
            label_stack: Vec::new(),
            label_scopes: vec![HashSet::new()],
        }
    }

    fn resolve(mut self, program: &Program) -> BindingResolution {
        let mut normalized = program.clone();
        self.enter_scope();
        for import in &program.imports {
            self.register_import_binding(import);
        }
        normalized.statements = self.resolve_statements(program.statements.clone());
        self.exit_scope();

        BindingResolution {
            program: normalized,
            diagnostics: self.diagnostics,
            usage: self.usage,
            late_init_manifest: LateInitManifest::new(self.late_init_seeds),
        }
    }

    fn record_late_init_seed(
        &mut self,
        name: String,
        origin: ValBindingOrigin,
        has_initializer: bool,
        modifiers: &Modifiers,
    ) {
        let seed = LateInitSeed {
            name: name.clone(),
            origin,
            has_initializer,
            explicit_late_init: has_late_init_annotation(modifiers),
        };
        self.late_init_seeds.insert(name, seed);
    }

    fn resolve_statements(&mut self, statements: Vec<Statement>) -> Vec<Statement> {
        self.enter_label_scope();
        let resolved = statements
            .into_iter()
            .map(|statement| self.resolve_statement(statement))
            .collect();
        self.exit_label_scope();
        resolved
    }

    fn resolve_statement(&mut self, statement: Statement) -> Statement {
        match statement {
            Statement::ValDeclaration {
                name,
                binding,
                type_annotation,
                initializer,
                modifiers,
                origin,
                span,
            } => {
                let initializer = self.resolve_expression(initializer);
                self.declare_immutable(name.clone(), origin, span.clone(), true);
                self.record_late_init_seed(name.clone(), origin, true, &modifiers);
                Statement::ValDeclaration {
                    name,
                    binding,
                    type_annotation,
                    initializer,
                    modifiers,
                    origin,
                    span,
                }
            }
            Statement::VarDeclaration {
                name,
                binding,
                type_annotation,
                initializer,
                modifiers,
                span,
            } => {
                let (has_initializer, initializer) = match initializer {
                    Some(expr) => {
                        let resolved = self.resolve_expression(expr);
                        (true, Some(resolved))
                    }
                    None => (false, None),
                };
                self.declare_mutable(name.clone(), span.clone(), true);
                self.record_late_init_seed(
                    name.clone(),
                    ValBindingOrigin::ExplicitKeyword,
                    has_initializer,
                    &modifiers,
                );
                Statement::VarDeclaration {
                    name,
                    binding,
                    type_annotation,
                    initializer,
                    modifiers,
                    span,
                }
            }
            Statement::Assignment {
                target,
                value,
                span,
                binding_pattern: _,
            } => self.resolve_assignment(target, value, span),
            Statement::Expression { expr, span } => Statement::Expression {
                expr: self.resolve_expression(expr),
                span,
            },
            Statement::FunctionDeclaration {
                name,
                type_parameters,
                generic_signature,
                where_clause,
                mut parameters,
                return_type,
                primitive_return,
                body,
                modifiers,
                span,
            } => {
                for param in parameters.iter_mut() {
                    if let Some(default) = param.default_value.take() {
                        param.default_value = Some(self.resolve_expression(default));
                    }
                }

                self.enter_scope();
                for param in &parameters {
                    self.declare_immutable(
                        param.name.clone(),
                        ValBindingOrigin::ExplicitKeyword,
                        param.span.clone(),
                        false,
                    );
                }
                let body = Box::new(self.resolve_expression(*body));
                self.exit_scope();

                Statement::FunctionDeclaration {
                    name,
                    type_parameters,
                    generic_signature,
                    where_clause,
                    parameters,
                    return_type,
                    primitive_return,
                    body,
                    modifiers,
                    span,
                }
            }
            Statement::ClassDeclaration {
                name,
                type_parameters,
                generic_signature,
                superclass,
                interfaces,
                mut properties,
                mut methods,
                modifiers,
                span,
            } => {
                for property in properties.iter_mut() {
                    if let Some(initializer) = property.initializer.take() {
                        property.initializer = Some(self.resolve_expression(initializer));
                    }
                }
                for method in methods.iter_mut() {
                    let resolved = self.resolve_statement((**method).clone());
                    **method = resolved;
                }
                Statement::ClassDeclaration {
                    name,
                    type_parameters,
                    generic_signature,
                    superclass,
                    interfaces,
                    properties,
                    methods,
                    modifiers,
                    span,
                }
            }
            Statement::DataClassDeclaration {
                name,
                mut parameters,
                type_parameters,
                generic_signature,
                is_mutable,
                modifiers,
                span,
            } => {
                for param in parameters.iter_mut() {
                    if let Some(default) = param.default_value.take() {
                        param.default_value = Some(self.resolve_expression(default));
                    }
                }
                Statement::DataClassDeclaration {
                    name,
                    parameters,
                    type_parameters,
                    generic_signature,
                    is_mutable,
                    modifiers,
                    span,
                }
            }
            Statement::InterfaceDeclaration {
                name,
                type_parameters,
                generic_signature,
                superinterfaces,
                mut methods,
                mut properties,
                modifiers,
                span,
            } => {
                for method in methods.iter_mut() {
                    let resolved = self.resolve_statement((**method).clone());
                    **method = resolved;
                }
                for property in properties.iter_mut() {
                    if let Some(initializer) = property.initializer.take() {
                        property.initializer = Some(self.resolve_expression(initializer));
                    }
                }
                Statement::InterfaceDeclaration {
                    name,
                    type_parameters,
                    generic_signature,
                    superinterfaces,
                    methods,
                    properties,
                    modifiers,
                    span,
                }
            }
            Statement::ExtensionFunction(extension) => {
                let extension = self.resolve_extension_function(extension);
                Statement::ExtensionFunction(extension)
            }
            Statement::Return { label, value, span } => {
                if let Some(ref name) = label {
                    self.validate_return_label(name, &span);
                }
                Statement::Return {
                    label,
                    value: value.map(|expr| self.resolve_expression(expr)),
                    span,
                }
            }
            Statement::Throw { expr, span } => Statement::Throw {
                expr: self.resolve_expression(expr),
                span,
            },
            Statement::ForIn(mut for_in) => {
                let label_active =
                    self.activate_label(&for_in.label, LabelKind::Loop, &for_in.span);
                for_in.iterable = self.resolve_expression(for_in.iterable);
                self.enter_scope();
                self.enter_label_scope();
                self.declare_immutable(
                    for_in.binding.name.clone(),
                    ValBindingOrigin::Implicit,
                    for_in.binding.span.clone(),
                    false,
                );
                for_in.body = Box::new(self.resolve_expression(*for_in.body));
                self.exit_label_scope();
                self.exit_scope();
                self.deactivate_label(label_active);
                Statement::ForIn(for_in)
            }
            Statement::Break { label, span } => {
                if let Some(ref name) = label {
                    self.validate_break_label(name, &span);
                }
                Statement::Break { label, span }
            }
            Statement::Continue { label, span } => {
                if let Some(ref name) = label {
                    self.validate_continue_label(name, &span);
                }
                Statement::Continue { label, span }
            }
            Statement::Concurrency(construct) => {
                Statement::Concurrency(self.resolve_concurrency(construct))
            }
            Statement::ResourceManagement(resource) => {
                Statement::ResourceManagement(self.resolve_resource_management(resource))
            }
            Statement::Import { .. } | Statement::Package { .. } | Statement::Comment(_) => {
                statement
            }
        }
    }

    fn resolve_extension_function(
        &mut self,
        mut extension: ExtensionFunction,
    ) -> ExtensionFunction {
        extension.function = Box::new(self.resolve_statement(*extension.function));
        extension
    }

    fn resolve_assignment(
        &mut self,
        target: Expression,
        value: Expression,
        span: Span,
    ) -> Statement {
        match target {
            Expression::Identifier(name, target_span) => {
                let value = self.resolve_expression(value);
                match self.lookup(&name) {
                    Some(BindingKind::Immutable { .. }) => {
                        self.diagnostics.push(CheckError::ValidationError {
                            message: reassignment_message(&name),
                            span: Some(span.clone()),
                        });
                        Statement::Assignment {
                            target: Expression::Identifier(name, target_span),
                            binding_pattern: None,

                            value,
                            span,
                        }
                    }
                    Some(BindingKind::Mutable { .. }) => Statement::Assignment {
                        target: Expression::Identifier(name, target_span),
                        binding_pattern: None,

                        value,
                        span,
                    },
                    None => {
                        if self.is_self_reference(&name, &value) {
                            self.diagnostics.push(CheckError::ValidationError {
                                message: missing_initializer_message(&name),
                                span: Some(span.clone()),
                            });
                            Statement::Assignment {
                                target: Expression::Identifier(name, target_span),
                                binding_pattern: None,

                                value,
                                span,
                            }
                        } else {
                            let origin = ValBindingOrigin::Implicit;
                            self.declare_immutable(name.clone(), origin, target_span.clone(), true);
                            let modifiers = Modifiers::default();
                            self.record_late_init_seed(name.clone(), origin, true, &modifiers);
                            Statement::ValDeclaration {
                                name,
                                binding: None,
                                type_annotation: None,
                                initializer: value,
                                modifiers,
                                origin,
                                span,
                            }
                        }
                    }
                }
            }
            other_target => {
                let target = self.resolve_expression(other_target);
                let value = self.resolve_expression(value);
                Statement::Assignment {
                    target,
                    value,
                    span,
                    binding_pattern: None,
                }
            }
        }
    }

    fn resolve_expression(&mut self, expression: Expression) -> Expression {
        match expression {
            Expression::RegexLiteral(literal) => Expression::RegexLiteral(literal),
            Expression::Binary {
                left,
                op,
                right,
                span,
            } => Expression::Binary {
                left: Box::new(self.resolve_expression(*left)),
                op,
                right: Box::new(self.resolve_expression(*right)),
                span,
            },
            Expression::Unary { op, operand, span } => Expression::Unary {
                op,
                operand: Box::new(self.resolve_expression(*operand)),
                span,
            },
            Expression::Call {
                function,
                args,
                type_arguments,
                argument_metadata,
                span,
            } => Expression::Call {
                function: Box::new(self.resolve_expression(*function)),
                args: args
                    .into_iter()
                    .map(|arg| self.resolve_argument(arg))
                    .collect(),
                type_arguments,
                argument_metadata,
                span,
            },
            Expression::MemberAccess {
                object,
                property,
                span,
            } => Expression::MemberAccess {
                object: Box::new(self.resolve_expression(*object)),
                property,
                span,
            },
            Expression::NullSafeMemberAccess {
                object,
                property,
                span,
            } => Expression::NullSafeMemberAccess {
                object: Box::new(self.resolve_expression(*object)),
                property,
                span,
            },
            Expression::TypeCast { expr, target, span } => Expression::TypeCast {
                expr: Box::new(self.resolve_expression(*expr)),
                target,
                span,
            },
            Expression::IndexAccess {
                object,
                index,
                span,
            } => Expression::IndexAccess {
                object: Box::new(self.resolve_expression(*object)),
                index: Box::new(self.resolve_expression(*index)),
                span,
            },
            Expression::NullSafeIndexAccess {
                object,
                index,
                span,
            } => Expression::NullSafeIndexAccess {
                object: Box::new(self.resolve_expression(*object)),
                index: Box::new(self.resolve_expression(*index)),
                span,
            },
            Expression::StringInterpolation { parts, span } => Expression::StringInterpolation {
                parts: parts
                    .into_iter()
                    .map(|part| match part {
                        jv_ast::StringPart::Expression(expr) => {
                            jv_ast::StringPart::Expression(self.resolve_expression(expr))
                        }
                        other => other,
                    })
                    .collect(),
                span,
            },
            Expression::When {
                expr,
                arms,
                else_arm,
                implicit_end,
                span,
                label,
            } => {
                let label_active = self.activate_label(&label, LabelKind::When, &span);
                let expr = expr.map(|inner| Box::new(self.resolve_expression(*inner)));
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        self.enter_scope();
                        self.enter_label_scope();
                        let guard = arm.guard.map(|g| self.resolve_expression(g));
                        let body = self.resolve_expression(arm.body);
                        self.exit_label_scope();
                        self.exit_scope();
                        jv_ast::WhenArm {
                            pattern: arm.pattern,
                            guard,
                            body,
                            span: arm.span,
                        }
                    })
                    .collect();
                let else_arm = else_arm.map(|expr| Box::new(self.resolve_expression(*expr)));
                self.deactivate_label(label_active);
                Expression::When {
                    expr,
                    arms,
                    else_arm,
                    implicit_end,
                    label,
                    span,
                }
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => Expression::If {
                condition: Box::new(self.resolve_expression(*condition)),
                then_branch: Box::new(self.resolve_expression(*then_branch)),
                else_branch: else_branch.map(|expr| Box::new(self.resolve_expression(*expr))),
                span,
            },
            Expression::Block {
                statements,
                span,
                label,
            } => {
                let label_active = self.activate_label(&label, LabelKind::Block, &span);
                self.enter_scope();
                self.enter_label_scope();
                let statements = self.resolve_statements(statements);
                self.exit_label_scope();
                self.exit_scope();
                self.deactivate_label(label_active);
                Expression::Block {
                    statements,
                    label,
                    span,
                }
            }
            Expression::Array {
                elements,
                delimiter,
                span,
            } => Expression::Array {
                elements: elements
                    .into_iter()
                    .map(|element| self.resolve_expression(element))
                    .collect(),
                delimiter,
                span,
            },
            Expression::Lambda {
                parameters,
                body,
                span,
                label,
            } => {
                let parameters = parameters
                    .into_iter()
                    .map(|mut param| {
                        if let Some(default) = param.default_value.take() {
                            param.default_value = Some(self.resolve_expression(default));
                        }
                        param
                    })
                    .collect::<Vec<_>>();
                let label_active = self.activate_label(&label, LabelKind::Lambda, &span);
                self.enter_scope();
                self.enter_label_scope();
                for param in &parameters {
                    self.declare_immutable(
                        param.name.clone(),
                        ValBindingOrigin::ExplicitKeyword,
                        param.span.clone(),
                        false,
                    );
                }
                let body = Box::new(self.resolve_expression(*body));
                self.exit_label_scope();
                self.exit_scope();
                self.deactivate_label(label_active);
                Expression::Lambda {
                    parameters,
                    body,
                    label,
                    span,
                }
            }
            Expression::Try {
                body,
                catch_clauses,
                finally_block,
                span,
            } => Expression::Try {
                body: Box::new(self.resolve_expression(*body)),
                catch_clauses: catch_clauses
                    .into_iter()
                    .map(|clause| self.resolve_catch_clause(clause))
                    .collect(),
                finally_block: finally_block.map(|expr| Box::new(self.resolve_expression(*expr))),
                span,
            },
            Expression::Literal(_, _)
            | Expression::Identifier(_, _)
            | Expression::MultilineString(_)
            | Expression::JsonLiteral(_)
            | Expression::This(_)
            | Expression::Super(_) => expression,
        }
    }

    fn resolve_argument(&mut self, argument: Argument) -> Argument {
        match argument {
            Argument::Positional(expr) => Argument::Positional(self.resolve_expression(expr)),
            Argument::Named { name, value, span } => Argument::Named {
                name,
                value: self.resolve_expression(value),
                span,
            },
        }
    }

    fn resolve_catch_clause(&mut self, mut clause: TryCatchClause) -> TryCatchClause {
        self.enter_scope();
        if let Some(parameter) = clause.parameter.as_ref() {
            self.declare_immutable(
                parameter.name.clone(),
                ValBindingOrigin::ExplicitKeyword,
                parameter.span.clone(),
                false,
            );
        }
        clause.body = Box::new(self.resolve_expression(*clause.body));
        self.exit_scope();
        clause
    }

    fn resolve_concurrency(&mut self, construct: ConcurrencyConstruct) -> ConcurrencyConstruct {
        match construct {
            ConcurrencyConstruct::Spawn { body, span } => ConcurrencyConstruct::Spawn {
                body: Box::new(self.resolve_expression(*body)),
                span,
            },
            ConcurrencyConstruct::Async { body, span } => ConcurrencyConstruct::Async {
                body: Box::new(self.resolve_expression(*body)),
                span,
            },
            ConcurrencyConstruct::Await { expr, span } => ConcurrencyConstruct::Await {
                expr: Box::new(self.resolve_expression(*expr)),
                span,
            },
        }
    }

    fn resolve_resource_management(&mut self, resource: ResourceManagement) -> ResourceManagement {
        match resource {
            ResourceManagement::Use {
                resource,
                body,
                span,
            } => ResourceManagement::Use {
                resource: Box::new(self.resolve_expression(*resource)),
                body: Box::new(self.resolve_expression(*body)),
                span,
            },
            ResourceManagement::Defer { body, span } => ResourceManagement::Defer {
                body: Box::new(self.resolve_expression(*body)),
                span,
            },
        }
    }

    fn declare_immutable(
        &mut self,
        name: String,
        origin: ValBindingOrigin,
        span: Span,
        count_usage: bool,
    ) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name.clone(),
                BindingKind::Immutable {
                    _origin: origin,
                    _span: span,
                },
            );
        }
        if count_usage {
            match origin {
                ValBindingOrigin::ExplicitKeyword => self.usage.explicit += 1,
                ValBindingOrigin::Implicit => self.usage.implicit += 1,
                ValBindingOrigin::ImplicitTyped => self.usage.implicit_typed += 1,
            }
        }
    }

    fn declare_mutable(&mut self, name: String, span: Span, count_usage: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.clone(), BindingKind::Mutable { _span: span });
        }
        if count_usage {
            self.usage.vars += 1;
        }
    }

    fn register_import_binding(&mut self, import: &Statement) {
        if let Statement::Import {
            path,
            alias,
            is_wildcard,
            span,
        } = import
        {
            if *is_wildcard {
                return;
            }

            let binding_name = if let Some(alias) = alias {
                alias.clone()
            } else {
                path.split('.').last().unwrap_or("").to_string()
            };

            if binding_name.is_empty() {
                return;
            }

            self.declare_immutable(
                binding_name,
                ValBindingOrigin::Implicit,
                span.clone(),
                false,
            );
        }
    }

    fn lookup(&self, name: &str) -> Option<&BindingKind> {
        for scope in self.scopes.iter().rev() {
            if let Some(kind) = scope.get(name) {
                return Some(kind);
            }
        }
        None
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn enter_label_scope(&mut self) {
        self.label_scopes.push(HashSet::new());
    }

    fn exit_label_scope(&mut self) {
        if self.label_scopes.len() > 1 {
            self.label_scopes.pop();
        }
    }

    fn declare_label_name(&mut self, name: &str) -> bool {
        if let Some(scope) = self.label_scopes.last_mut() {
            if scope.contains(name) {
                return false;
            }
            scope.insert(name.to_string());
            true
        } else {
            let mut scope = HashSet::new();
            scope.insert(name.to_string());
            self.label_scopes.push(scope);
            true
        }
    }

    fn activate_label(&mut self, label: &Option<String>, kind: LabelKind, span: &Span) -> bool {
        if let Some(name) = label {
            if !self.declare_label_name(name) {
                self.report_validation_error(label_redeclared_message(name), Some(span.clone()));
            }
            self.label_stack.push(LabelEntry {
                name: name.clone(),
                kind,
                span: span.clone(),
            });
            true
        } else {
            false
        }
    }

    fn deactivate_label(&mut self, active: bool) {
        if active {
            self.label_stack.pop();
        }
    }

    fn lookup_label(&self, name: &str) -> Option<&LabelEntry> {
        self.label_stack
            .iter()
            .rev()
            .find(|entry| entry.name == name)
    }

    fn validate_break_label(&mut self, name: &str, span: &Span) {
        match self.lookup_label(name) {
            Some(entry)
                if matches!(
                    entry.kind,
                    LabelKind::Loop | LabelKind::Block | LabelKind::When
                ) => {}
            Some(_) | None => {
                self.report_validation_error(label_undefined_message(name), Some(span.clone()));
            }
        }
    }

    fn validate_continue_label(&mut self, name: &str, span: &Span) {
        match self.lookup_label(name) {
            Some(entry) if matches!(entry.kind, LabelKind::Loop) => {}
            Some(_) => {
                self.report_validation_error(
                    label_non_loop_continue_message(name),
                    Some(span.clone()),
                );
            }
            None => {
                self.report_validation_error(label_undefined_message(name), Some(span.clone()));
            }
        }
    }

    fn validate_return_label(&mut self, name: &str, span: &Span) {
        match self.lookup_label(name) {
            Some(entry) if matches!(entry.kind, LabelKind::Lambda) => {}
            Some(_) => {
                self.report_validation_error(
                    label_non_lambda_return_message(name),
                    Some(span.clone()),
                );
            }
            None => {
                self.report_validation_error(label_undefined_message(name), Some(span.clone()));
            }
        }
    }

    fn report_validation_error(&mut self, message: String, span: Option<Span>) {
        self.diagnostics
            .push(CheckError::ValidationError { message, span });
    }

    fn is_self_reference(&self, name: &str, expr: &Expression) -> bool {
        matches!(expr, Expression::Identifier(other, _) if other == name)
    }
}

fn has_late_init_annotation(modifiers: &Modifiers) -> bool {
    modifiers.annotations.iter().any(|annotation| {
        annotation
            .name
            .simple_name()
            .eq_ignore_ascii_case("LateInit")
    })
}

fn reassignment_message(name: &str) -> String {
    format!(
        "JV4201: `{}` は不変変数のため再代入できません。再代入が必要な場合は `var` で宣言してください。\nJV4201: Cannot reassign immutable binding `{}`. Use `var` if mutation is required.",
        name, name
    )
}

fn missing_initializer_message(name: &str) -> String {
    format!(
        "JV4202: 暗黙宣言 `{}` には初期化子が必要です。`identifier = value` の形式で値を割り当ててください。\nJV4202: Implicit binding `{}` must provide an initializer. Assign a value using `identifier = value`.",
        name, name
    )
}

fn label_undefined_message(label: &str) -> String {
    format!(
        "E-LABEL-UNDEFINED: `#{}` はこのスコープで利用できるラベルではありません。対応するラベルを宣言するか名前を確認してください。\nE-LABEL-UNDEFINED: Label `#{}` is not available in this scope. Declare the matching label or verify the name.",
        label, label
    )
}

fn label_non_loop_continue_message(label: &str) -> String {
    format!(
        "E-LABEL-NON_LOOP_CONTINUE: `continue #{}` はループラベルのみを対象にできます。`#{}` はループを指していません。\nE-LABEL-NON_LOOP_CONTINUE: `continue #{}` can only target loop labels; `#{}` does not refer to a loop.",
        label, label, label, label
    )
}

fn label_non_lambda_return_message(label: &str) -> String {
    format!(
        "E-LABEL-NON_LAMBDA_RETURN: `return #{}` はラムダ式に付与したラベルだけを指定できます。`#{}` はラムダを指していません。\nE-LABEL-NON_LAMBDA_RETURN: `return #{}` may only target lambda labels; `#{}` does not resolve to a lambda.",
        label, label, label, label
    )
}

fn label_redeclared_message(label: &str) -> String {
    format!(
        "E-LABEL-REDECLARED: ラベル `#{}` は同じスコープ内ですでに宣言されています。別名を使用してください。\nE-LABEL-REDECLARED: Label `#{}` is already declared in this scope. Use a different name.",
        label, label
    )
}
