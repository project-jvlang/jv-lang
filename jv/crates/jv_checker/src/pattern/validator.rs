use super::{
    MissingBooleanCase, MissingCase, PatternMatchFacts, PatternMatchService, PatternTarget,
};
use crate::CheckError;
use jv_ast::{
    Argument, ConcurrencyConstruct, Expression, ForInStatement, Literal, LogBlock, LogItem,
    LoopStrategy, NumericRangeLoop, Pattern, Program, Property, ResourceManagement, Span,
    Statement, StringPart, TypeAnnotation, WhenArm,
    statement::{UnitTypeDefinition, UnitTypeMember},
};

pub(super) fn validate_program(
    service: &mut PatternMatchService,
    program: &Program,
) -> Vec<CheckError> {
    WhenUsageValidator::validate(service, program)
}

struct WhenUsageValidator<'a> {
    service: &'a mut PatternMatchService,
    errors: Vec<CheckError>,
}

impl<'a> WhenUsageValidator<'a> {
    fn validate(service: &'a mut PatternMatchService, program: &Program) -> Vec<CheckError> {
        let mut validator = Self {
            service,
            errors: Vec::new(),
        };
        validator.visit_program(program);
        validator.errors
    }

    fn visit_program(&mut self, program: &Program) {
        for statement in &program.statements {
            self.visit_statement(statement, false);
        }
    }

    fn visit_statement(&mut self, statement: &Statement, expects_value: bool) {
        match statement {
            Statement::ValDeclaration { initializer, .. } => {
                self.visit_expression(initializer, true);
            }
            Statement::VarDeclaration { initializer, .. } => {
                if let Some(expr) = initializer {
                    self.visit_expression(expr, true);
                }
            }
            Statement::FunctionDeclaration {
                parameters,
                return_type,
                body,
                ..
            } => {
                for parameter in parameters {
                    if let Some(default) = &parameter.default_value {
                        self.visit_expression(default, true);
                    }
                }
                let body_expects_value = return_type
                    .as_ref()
                    .map(|annotation| !type_annotation_is_unit(annotation))
                    .unwrap_or(false);
                self.visit_expression(body, body_expects_value);
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
                for property in properties {
                    self.visit_property(property);
                }
                for method in methods {
                    self.visit_statement(method, false);
                }
            }
            Statement::ExtensionFunction(extension) => {
                self.visit_statement(&extension.function, false);
            }
            Statement::Expression { expr, .. } => {
                self.visit_expression(expr, expects_value);
            }
            Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.visit_expression(expr, true);
                }
            }
            Statement::Throw { expr, .. } => {
                self.visit_expression(expr, true);
            }
            Statement::Assignment { target, value, .. } => {
                self.visit_expression(target, true);
                self.visit_expression(value, true);
            }
            Statement::ForIn(for_in) => self.visit_for_in(for_in),
            Statement::Concurrency(construct) => self.visit_concurrency_construct(construct),
            Statement::ResourceManagement(resource) => self.visit_resource_management(resource),
            Statement::DataClassDeclaration { parameters, .. } => {
                for parameter in parameters {
                    if let Some(default) = &parameter.default_value {
                        self.visit_expression(default, true);
                    }
                }
            }
            Statement::UnitTypeDefinition(definition) => {
                self.visit_unit_definition(definition);
            }
            Statement::Import { .. }
            | Statement::Package { .. }
            | Statement::Break(_)
            | Statement::Continue(_)
            | Statement::Comment(_) => {}
        }
    }

    fn visit_unit_definition(&mut self, definition: &UnitTypeDefinition) {
        for member in &definition.members {
            match member {
                UnitTypeMember::Dependency(dependency) => {
                    if let Some(expr) = dependency.value.as_ref() {
                        self.visit_expression(expr, true);
                    }
                }
                UnitTypeMember::Conversion(block) => {
                    for statement in &block.body {
                        self.visit_statement(statement, false);
                    }
                }
                UnitTypeMember::NestedStatement(statement) => {
                    self.visit_statement(statement, false);
                }
            }
        }
    }

    fn visit_expression(&mut self, expression: &Expression, expects_value: bool) {
        match expression {
            Expression::RegexLiteral(..)
            | Expression::RegexCommand(_)
            | Expression::Literal(..)
            | Expression::Identifier(..)
            | Expression::This(..)
            | Expression::Super(..) => {}
            Expression::Binary { left, right, .. } => {
                self.visit_expression(left, true);
                self.visit_expression(right, true);
            }
            Expression::Unary { operand, .. } => {
                self.visit_expression(operand, true);
            }
            Expression::Call { function, args, .. } => {
                self.visit_expression(function, true);
                for arg in args {
                    match arg {
                        Argument::Positional(expr) => self.visit_expression(expr, true),
                        Argument::Named { value, .. } => self.visit_expression(value, true),
                    }
                }
            }
            Expression::MemberAccess { object, .. }
            | Expression::NullSafeMemberAccess { object, .. } => {
                self.visit_expression(object, true);
            }
            Expression::IndexAccess { object, index, .. }
            | Expression::NullSafeIndexAccess { object, index, .. } => {
                self.visit_expression(object, true);
                self.visit_expression(index, true);
            }
            Expression::TypeCast { expr, .. } => self.visit_expression(expr, true),
            Expression::StringInterpolation { parts, .. } => {
                for part in parts {
                    if let StringPart::Expression(expr) = part {
                        self.visit_expression(expr, true);
                    }
                }
            }
            Expression::UnitLiteral { value, .. } => {
                self.visit_expression(value, expects_value);
            }
            Expression::MultilineString { .. } | Expression::JsonLiteral { .. } => {}
            Expression::Array { elements, .. } => {
                for element in elements {
                    self.visit_expression(element, true);
                }
            }
            Expression::Lambda {
                parameters, body, ..
            } => {
                for parameter in parameters {
                    if let Some(default) = &parameter.default_value {
                        self.visit_expression(default, true);
                    }
                }
                self.visit_expression(body, true);
            }
            Expression::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                self.visit_expression(body, expects_value);
                for clause in catch_clauses {
                    self.visit_expression(&clause.body, expects_value);
                }
                if let Some(finally_expr) = finally_block.as_deref() {
                    self.visit_expression(finally_expr, expects_value);
                }
            }
            Expression::Block { statements, .. } => {
                for (index, statement) in statements.iter().enumerate() {
                    let is_last = index == statements.len().saturating_sub(1);
                    let stmt_expects_value = expects_value && is_last;
                    self.visit_statement(statement, stmt_expects_value);
                }
            }
            Expression::LogBlock(block) => self.visit_log_block(block),
            Expression::If {
                condition,
                then_branch,
                else_branch,
                span,
                ..
            } => {
                self.visit_expression(condition, true);
                self.visit_expression(then_branch, expects_value);
                if let Some(else_expr) = else_branch {
                    self.visit_expression(else_expr, expects_value);
                } else if expects_value {
                    self.record_missing_else(span);
                }
            }
            Expression::When {
                expr,
                arms,
                else_arm,
                implicit_end,
                span,
                ..
            } => {
                if let Some(subject) = expr {
                    self.visit_expression(subject, true);
                }
                for arm in arms {
                    self.visit_when_arm(arm, expects_value);
                }
                let has_subject = expr.is_some();
                let missing_else = expects_value && else_arm.is_none() && implicit_end.is_none();
                if let Some(else_expr) = else_arm {
                    self.visit_expression(else_expr, expects_value);
                } else if missing_else && !(has_subject && arms_cover_boolean_literals(arms)) {
                    self.record_missing_else(span);
                }
                self.analyze_when_expression(expression, span, expects_value, has_subject);
            }
        }
    }

    fn visit_log_block(&mut self, block: &LogBlock) {
        for item in &block.items {
            match item {
                LogItem::Statement(statement) => self.visit_statement(statement, false),
                LogItem::Expression(expr) => self.visit_expression(expr, false),
                LogItem::Nested(nested) => self.visit_log_block(nested),
            }
        }
    }

    fn visit_when_arm(&mut self, arm: &WhenArm, expects_value: bool) {
        self.visit_pattern(&arm.pattern);
        if let Some(guard) = &arm.guard {
            self.visit_expression(guard, true);
        }
        self.visit_expression(&arm.body, expects_value);
    }

    fn visit_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Constructor { patterns, .. } => {
                for nested in patterns {
                    self.visit_pattern(nested);
                }
            }
            Pattern::Range { start, end, .. } => {
                self.visit_expression(start, true);
                self.visit_expression(end, true);
            }
            Pattern::Guard {
                pattern: nested,
                condition,
                ..
            } => {
                self.visit_pattern(nested);
                self.visit_expression(condition, true);
            }
            Pattern::Literal(..) | Pattern::Identifier(..) | Pattern::Wildcard(..) => {}
        }
    }

    fn visit_property(&mut self, property: &Property) {
        if let Some(initializer) = &property.initializer {
            self.visit_expression(initializer, true);
        }
        if let Some(getter) = &property.getter {
            self.visit_expression(getter, true);
        }
        if let Some(setter) = &property.setter {
            self.visit_expression(setter, false);
        }
    }

    fn visit_for_in(&mut self, for_in: &ForInStatement) {
        self.visit_expression(&for_in.iterable, true);
        self.visit_loop_strategy(&for_in.strategy);
        self.visit_expression(&for_in.body, false);
    }

    fn visit_loop_strategy(&mut self, strategy: &LoopStrategy) {
        if let LoopStrategy::NumericRange(NumericRangeLoop { start, end, .. }) = strategy {
            self.visit_expression(start, true);
            self.visit_expression(end, true);
        }
    }

    fn visit_concurrency_construct(&mut self, construct: &ConcurrencyConstruct) {
        match construct {
            ConcurrencyConstruct::Spawn { body, .. } | ConcurrencyConstruct::Async { body, .. } => {
                self.visit_expression(body, false);
            }
            ConcurrencyConstruct::Await { expr, .. } => {
                self.visit_expression(expr, true);
            }
        }
    }

    fn visit_resource_management(&mut self, resource: &ResourceManagement) {
        match resource {
            ResourceManagement::Use { resource, body, .. } => {
                self.visit_expression(resource, true);
                self.visit_expression(body, false);
            }
            ResourceManagement::Defer { body, .. } => {
                self.visit_expression(body, false);
            }
        }
    }

    fn record_missing_else(&mut self, span: &Span) {
        let message = "E_WHEN_002: when expression used as a value must declare an `else` branch. 値コンテキストでは `else` を追加し、docs/language-guide.md#when-expression / docs/language-guide-en.md#when-expression を参照してください.".to_string();
        self.errors.push(CheckError::ValidationError {
            message,
            span: Some(span.clone()),
        });
    }

    fn analyze_when_expression(
        &mut self,
        expression: &Expression,
        span: &Span,
        expects_value: bool,
        has_subject: bool,
    ) {
        let facts = self.service.analyze(expression, PatternTarget::Java25);
        if expects_value && has_subject {
            self.emit_exhaustiveness_diagnostics(span, &facts);
        }
    }

    fn emit_exhaustiveness_diagnostics(&mut self, span: &Span, facts: &PatternMatchFacts) {
        if facts.is_exhaustive() {
            return;
        }

        for case in facts.missing_cases() {
            match case {
                MissingCase::Boolean {
                    missing,
                    suggestion,
                } => {
                    let (label_en, label_ja) = boolean_labels(*missing);
                    let message = format!(
                        "JV3100: when 式が boolean の `{label_ja}` ケースを網羅していません。欠落した分岐を追加してください。\nJV3100: `when` expression is missing the `{label_en}` branch; add an arm covering it.\nQuick Fix: {quick_fix} -> {quick_fix_label_ja}\nQuick Fix: {quick_fix} -> {quick_fix_label_en}",
                        quick_fix = suggestion.quick_fix_id,
                        quick_fix_label_ja = suggestion.label_ja,
                        quick_fix_label_en = suggestion.label_en,
                    );
                    self.errors.push(CheckError::ValidationError {
                        message,
                        span: Some(span.clone()),
                    });
                }
                MissingCase::SealedVariant {
                    type_name,
                    variant,
                    suggestion,
                } => {
                    let message = format!(
                        "JV3100: sealed 型 `{type_name}` の `{variant}` ケースを網羅していません。分岐を追加してください。\nJV3100: `when` expression is missing the `{variant}` branch of sealed type `{type_name}`; add an arm covering it.\nQuick Fix: {quick_fix} -> {quick_fix_label_ja}\nQuick Fix: {quick_fix} -> {quick_fix_label_en}",
                        quick_fix = suggestion.quick_fix_id,
                        quick_fix_label_ja = suggestion.label_ja,
                        quick_fix_label_en = suggestion.label_en,
                    );
                    self.errors.push(CheckError::ValidationError {
                        message,
                        span: Some(span.clone()),
                    });
                }
                MissingCase::EnumConstant {
                    enum_type,
                    constant,
                    suggestion,
                } => {
                    let message = format!(
                        "JV3100: enum 型 `{enum_type}` の `{constant}` ケースを網羅していません。分岐を追加してください。\nJV3100: `when` expression is missing the `{constant}` branch of enum `{enum_type}`; add an arm covering it.\nQuick Fix: {quick_fix} -> {quick_fix_label_ja}\nQuick Fix: {quick_fix} -> {quick_fix_label_en}",
                        quick_fix = suggestion.quick_fix_id,
                        quick_fix_label_ja = suggestion.label_ja,
                        quick_fix_label_en = suggestion.label_en,
                    );
                    self.errors.push(CheckError::ValidationError {
                        message,
                        span: Some(span.clone()),
                    });
                }
            }
        }
    }
}

fn type_annotation_is_unit(annotation: &TypeAnnotation) -> bool {
    match annotation {
        TypeAnnotation::Simple(name) => name == "Unit",
        TypeAnnotation::Nullable(inner) => type_annotation_is_unit(inner),
        _ => false,
    }
}

fn arms_cover_boolean_literals(arms: &[WhenArm]) -> bool {
    let mut covers_true = false;
    let mut covers_false = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Boolean(true), _) => covers_true = true,
            Pattern::Literal(Literal::Boolean(false), _) => covers_false = true,
            _ => {}
        }
        if covers_true && covers_false {
            return true;
        }
    }
    covers_true && covers_false
}

fn boolean_labels(case: MissingBooleanCase) -> (&'static str, &'static str) {
    match case {
        MissingBooleanCase::True => ("true", "true"),
        MissingBooleanCase::False => ("false", "false"),
    }
}
