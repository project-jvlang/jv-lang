use jv_ast::{
    Argument, Expression, JsonLiteral, JsonValue, Parameter, Pattern, Program, SequenceDelimiter,
    Span, Statement,
};

pub fn collect_sequence_warnings(program: &Program) -> Vec<String> {
    let mut collector = SequenceWarningCollector::default();
    collector.visit_program(program);
    collector.warnings
}

#[derive(Default)]
struct SequenceWarningCollector {
    warnings: Vec<String>,
    emitted_as_sequence_hint: bool,
    emitted_java_fallback_hint: bool,
}

enum SequenceKind {
    Array,
    Call,
    JsonArray,
}

impl SequenceWarningCollector {
    fn visit_program(&mut self, program: &Program) {
        for statement in &program.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Comment(_) => {}
            Statement::ValDeclaration { initializer, .. } => self.visit_expression(initializer),
            Statement::VarDeclaration { initializer, .. } => {
                if let Some(expr) = initializer {
                    self.visit_expression(expr);
                }
            }
            Statement::FunctionDeclaration {
                parameters, body, ..
            } => {
                for parameter in parameters {
                    self.visit_parameter(parameter);
                }
                self.visit_expression(body);
            }
            Statement::ClassDeclaration {
                properties,
                methods,
                ..
            } => {
                for property in properties {
                    if let Some(initializer) = &property.initializer {
                        self.visit_expression(initializer);
                    }
                    if let Some(getter) = &property.getter {
                        self.visit_expression(getter);
                    }
                    if let Some(setter) = &property.setter {
                        self.visit_expression(setter);
                    }
                }
                for method in methods {
                    self.visit_statement(method);
                }
            }
            Statement::DataClassDeclaration { parameters, .. } => {
                for parameter in parameters {
                    self.visit_parameter(parameter);
                }
            }
            Statement::InterfaceDeclaration {
                methods,
                properties,
                ..
            } => {
                for property in properties {
                    if let Some(initializer) = &property.initializer {
                        self.visit_expression(initializer);
                    }
                    if let Some(getter) = &property.getter {
                        self.visit_expression(getter);
                    }
                    if let Some(setter) = &property.setter {
                        self.visit_expression(setter);
                    }
                }
                for method in methods {
                    self.visit_statement(method);
                }
            }
            Statement::ExtensionFunction(extension) => {
                self.visit_statement(&extension.function);
            }
            Statement::Expression { expr, .. } => self.visit_expression(expr),
            Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.visit_expression(expr);
                }
            }
            Statement::Throw { expr, .. } => {
                self.visit_expression(expr);
            }
            Statement::Assignment { target, value, .. } => {
                self.visit_expression(target);
                self.visit_expression(value);
            }
            Statement::ForIn(statement) => {
                self.visit_expression(&statement.iterable);
                self.visit_loop_strategy(&statement.strategy);
                self.visit_expression(&statement.body);
            }
            Statement::Concurrency(construct) => self.visit_concurrency(construct),
            Statement::ResourceManagement(resource) => self.visit_resource_management(resource),
            Statement::Import { .. }
            | Statement::Package { .. }
            | Statement::Break { .. }
            | Statement::Continue { .. } => {}
        }
    }

    fn visit_concurrency(&mut self, construct: &jv_ast::ConcurrencyConstruct) {
        match construct {
            jv_ast::ConcurrencyConstruct::Spawn { body, .. }
            | jv_ast::ConcurrencyConstruct::Async { body, .. } => self.visit_expression(body),
            jv_ast::ConcurrencyConstruct::Await { expr, .. } => self.visit_expression(expr),
        }
    }

    fn visit_resource_management(&mut self, resource: &jv_ast::ResourceManagement) {
        match resource {
            jv_ast::ResourceManagement::Use { resource, body, .. } => {
                self.visit_expression(resource);
                self.visit_expression(body);
            }
            jv_ast::ResourceManagement::Defer { body, .. } => self.visit_expression(body),
        }
    }

    fn visit_loop_strategy(&mut self, strategy: &jv_ast::LoopStrategy) {
        if let jv_ast::LoopStrategy::NumericRange(range) = strategy {
            self.visit_expression(&range.start);
            self.visit_expression(&range.end);
        }
    }

    fn visit_parameter(&mut self, parameter: &Parameter) {
        if let Some(default) = &parameter.default_value {
            self.visit_expression(default);
        }
    }

    fn visit_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Literal(_, _)
            | Expression::RegexLiteral(_)
            | Expression::Identifier(_, _)
            | Expression::This(_)
            | Expression::Super(_) => {}
            Expression::Binary { left, right, .. } => {
                self.visit_expression(left);
                self.visit_expression(right);
            }
            Expression::Unary { operand, .. } => self.visit_expression(operand),
            Expression::Call {
                function,
                args,
                type_arguments: _,
                argument_metadata,
                span,
            } => {
                self.visit_expression(function);
                for argument in args {
                    self.visit_argument(argument);
                }
                if let Expression::MemberAccess {
                    property,
                    span: member_span,
                    ..
                } = function.as_ref()
                {
                    let method = property.as_str();
                    if method == "asSequence" {
                        self.emit_as_sequence_hint(member_span);
                    } else if method == "toList" || method == "toSet" {
                        self.emit_java_fallback_hint(member_span, method);
                    }
                }
                if argument_metadata.used_commas {
                    self.emit_warning(span, SequenceKind::Call);
                }
            }
            Expression::MemberAccess { object, .. }
            | Expression::NullSafeMemberAccess { object, .. } => self.visit_expression(object),
            Expression::IndexAccess { object, index, .. }
            | Expression::NullSafeIndexAccess { object, index, .. } => {
                self.visit_expression(object);
                self.visit_expression(index);
            }
            Expression::TypeCast { expr, .. } => self.visit_expression(expr),
            Expression::StringInterpolation { parts, .. } => {
                for part in parts {
                    if let jv_ast::StringPart::Expression(expr) = part {
                        self.visit_expression(expr);
                    }
                }
            }
            Expression::MultilineString(_) => {}
            Expression::JsonLiteral(literal) => self.visit_json_literal(literal),
            Expression::When {
                expr,
                arms,
                else_arm,
                ..
            } => {
                if let Some(subject) = expr {
                    self.visit_expression(subject);
                }
                for arm in arms {
                    self.visit_pattern(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.visit_expression(guard);
                    }
                    self.visit_expression(&arm.body);
                }
                if let Some(expr) = else_arm {
                    self.visit_expression(expr);
                }
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.visit_expression(condition);
                self.visit_expression(then_branch);
                if let Some(expr) = else_branch {
                    self.visit_expression(expr);
                }
            }
            Expression::Block { statements, .. } => {
                for statement in statements {
                    self.visit_statement(statement);
                }
            }
            Expression::Array {
                elements,
                delimiter,
                span,
            } => {
                if *delimiter == SequenceDelimiter::Comma && elements.len() > 1 {
                    self.emit_warning(span, SequenceKind::Array);
                }
                for element in elements {
                    self.visit_expression(element);
                }
            }
            Expression::Lambda {
                parameters, body, ..
            } => {
                for parameter in parameters {
                    self.visit_parameter(parameter);
                }
                self.visit_expression(body);
            }
            Expression::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                self.visit_expression(body);
                for clause in catch_clauses {
                    if let Some(parameter) = &clause.parameter {
                        self.visit_parameter(parameter);
                    }
                    self.visit_expression(&clause.body);
                }
                if let Some(finally) = finally_block {
                    self.visit_expression(finally);
                }
            }
        }
    }

    fn visit_argument(&mut self, argument: &Argument) {
        match argument {
            Argument::Positional(expr) => self.visit_expression(expr),
            Argument::Named { value, .. } => self.visit_expression(value),
        }
    }

    fn visit_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Literal(_, _) | Pattern::Identifier(_, _) | Pattern::Wildcard(_) => {}
            Pattern::Constructor { patterns, .. } => {
                for inner in patterns {
                    self.visit_pattern(inner);
                }
            }
            Pattern::Range { start, end, .. } => {
                self.visit_expression(start);
                self.visit_expression(end);
            }
            Pattern::Guard {
                pattern, condition, ..
            } => {
                self.visit_pattern(pattern);
                self.visit_expression(condition);
            }
        }
    }

    fn visit_json_literal(&mut self, literal: &JsonLiteral) {
        self.visit_json_value(&literal.value);
    }

    fn visit_json_value(&mut self, value: &JsonValue) {
        match value {
            JsonValue::Object { entries, .. } => {
                for entry in entries {
                    self.visit_json_value(&entry.value);
                }
            }
            JsonValue::Array {
                elements,
                delimiter,
                span,
            } => {
                if *delimiter == SequenceDelimiter::Comma && elements.len() > 1 {
                    self.emit_warning(span, SequenceKind::JsonArray);
                }
                for element in elements {
                    self.visit_json_value(element);
                }
            }
            JsonValue::String { .. }
            | JsonValue::Number { .. }
            | JsonValue::Boolean { .. }
            | JsonValue::Null { .. } => {}
        }
    }

    fn emit_warning(&mut self, span: &Span, kind: SequenceKind) {
        let (code, context) = match kind {
            SequenceKind::Array => ("JV2101", "array literal"),
            SequenceKind::Call => ("JV2102", "argument list"),
            SequenceKind::JsonArray => ("JV2101", "JSON array literal"),
        };

        if span.start_line > 0 {
            self.warnings.push(format!(
                "{}: comma-separated {} at {}:{}; whitespace-only layout is required.",
                code, context, span.start_line, span.start_column
            ));
        } else {
            self.warnings.push(format!(
                "{}: comma-separated {} detected; whitespace-only layout is required.",
                code, context
            ));
        }
    }

    fn emit_as_sequence_hint(&mut self, span: &Span) {
        if self.emitted_as_sequence_hint {
            return;
        }
        self.emitted_as_sequence_hint = true;
        if span.start_line > 0 {
            self.warnings.push(format!(
                "SEQ1001: `asSequence()` は不要です。Iterable から直接 map/filter を呼び出すと自動で遅延Sequenceが開始されます ({}:{})。",
                span.start_line, span.start_column
            ));
        } else {
            self.warnings.push(
                "SEQ1001: `asSequence()` は不要です。Iterable から直接 map/filter を呼び出すと自動で遅延Sequenceが開始されます。"
                    .to_string(),
            );
        }
    }

    fn emit_java_fallback_hint(&mut self, span: &Span, method: &str) {
        if self.emitted_java_fallback_hint {
            return;
        }
        self.emitted_java_fallback_hint = true;
        let fallback = if method == "toSet" {
            "Collectors.toSet()"
        } else {
            "Collectors.toList()"
        };
        if span.start_line > 0 {
            self.warnings.push(format!(
                "SEQ1002: `{method}()` は Java 25 で `.stream().{method}()`、Java 21 で `{fallback}` にデシュガリングされます ({}:{})。",
                span.start_line, span.start_column
            ));
        } else {
            self.warnings.push(format!(
                "SEQ1002: `{method}()` は Java 25 で `.stream().{method}()`、Java 21 で `{fallback}` にデシュガリングされます。",
            ));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_parser_frontend::ParserPipeline;
    use jv_parser_rowan::frontend::RowanPipeline;

    fn parse(source: &str) -> Program {
        RowanPipeline::default()
            .parse(source)
            .expect("source should parse for sequence warnings")
            .into_program()
    }

    #[test]
    fn warns_on_as_sequence_call() {
        let program = parse(
            r#"
numbers = [1 2 3]
numbers.asSequence().map { value -> value }.toList()
"#,
        );
        let warnings = collect_sequence_warnings(&program);
        assert!(warnings.iter().any(|warning| warning.contains("SEQ1001")));
    }

    #[test]
    fn warns_on_to_list_java_fallback() {
        let program = parse(
            r#"
numbers = [1 2 3]
numbers.map { value -> value * 2 }.toList()
"#,
        );
        let warnings = collect_sequence_warnings(&program);
        assert!(warnings.iter().any(|warning| warning.contains("SEQ1002")));
    }
}
