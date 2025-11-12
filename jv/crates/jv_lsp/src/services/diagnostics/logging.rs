use crate::{Diagnostic, DiagnosticSeverity, span_to_range};
use jv_ast::{Argument, Expression, LogBlock, LogItem, Statement, TestDataset};

const MAX_ALLOWED_LOG_BLOCK_DEPTH: usize = 2;
const LOG_NESTING_DIAGNOSTIC_CODE: &str = "JV4601";
const LOG_MISSING_MESSAGE_CODE: &str = "JV4602";

pub fn collect_logging_diagnostics(program: &jv_ast::Program) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    for statement in &program.statements {
        collect_from_statement(statement, 0, &mut diagnostics);
    }
    diagnostics
}

fn collect_from_statement(statement: &Statement, depth: usize, diagnostics: &mut Vec<Diagnostic>) {
    match statement {
        Statement::ValDeclaration { initializer, .. } => {
            collect_from_expression(initializer, depth, diagnostics);
        }
        Statement::VarDeclaration { initializer, .. } => {
            if let Some(expr) = initializer {
                collect_from_expression(expr, depth, diagnostics);
            }
        }
        Statement::FunctionDeclaration {
            body, parameters, ..
        } => {
            for parameter in parameters {
                if let Some(default) = &parameter.default_value {
                    collect_from_expression(default, depth, diagnostics);
                }
            }
            collect_from_expression(body, depth, diagnostics);
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
                if let Some(initializer) = &property.initializer {
                    collect_from_expression(initializer, depth, diagnostics);
                }
                if let Some(getter) = &property.getter {
                    collect_from_expression(getter, depth, diagnostics);
                }
                if let Some(setter) = &property.setter {
                    collect_from_expression(setter, depth, diagnostics);
                }
            }
            for method in methods {
                collect_from_statement(method, depth, diagnostics);
            }
        }
        Statement::DataClassDeclaration { parameters, .. } => {
            for parameter in parameters {
                if let Some(default) = &parameter.default_value {
                    collect_from_expression(default, depth, diagnostics);
                }
            }
        }
        Statement::TestDeclaration(declaration) => {
            if let Some(dataset) = &declaration.dataset {
                collect_from_test_dataset(dataset, depth, diagnostics);
            }
            collect_from_expression(&declaration.body, depth, diagnostics);
        }
        Statement::ExtensionFunction(extension) => {
            collect_from_statement(&extension.function, depth, diagnostics);
        }
        Statement::Expression { expr, .. } => {
            collect_from_expression(expr, depth, diagnostics);
        }
        Statement::Return { value, .. } => {
            if let Some(expr) = value {
                collect_from_expression(expr, depth, diagnostics);
            }
        }
        Statement::Throw { expr, .. } => {
            collect_from_expression(expr, depth, diagnostics);
        }
        Statement::Assignment { value, .. } => {
            collect_from_expression(value, depth, diagnostics);
        }
        Statement::ForIn(for_in) => {
            collect_from_expression(&for_in.iterable, depth, diagnostics);
            collect_from_expression(&for_in.body, depth, diagnostics);
        }
        Statement::Concurrency(construct) => match construct {
            jv_ast::ConcurrencyConstruct::Spawn { body, .. }
            | jv_ast::ConcurrencyConstruct::Async { body, .. } => {
                collect_from_expression(body, depth, diagnostics);
            }
            jv_ast::ConcurrencyConstruct::Await { expr, .. } => {
                collect_from_expression(expr, depth, diagnostics);
            }
        },
        Statement::ResourceManagement(resource) => match resource {
            jv_ast::ResourceManagement::Use { resource, body, .. } => {
                collect_from_expression(resource, depth, diagnostics);
                collect_from_expression(body, depth, diagnostics);
            }
            jv_ast::ResourceManagement::Defer { body, .. } => {
                collect_from_expression(body, depth, diagnostics);
            }
        },
        Statement::UnitTypeDefinition(definition) => {
            for member in &definition.members {
                match member {
                    jv_ast::statement::UnitTypeMember::Dependency(dependency) => {
                        if let Some(value) = dependency.value.as_ref() {
                            collect_from_expression(value, depth, diagnostics);
                        }
                    }
                    jv_ast::statement::UnitTypeMember::Conversion(block) => {
                        for statement in &block.body {
                            collect_from_statement(statement, depth, diagnostics);
                        }
                    }
                    jv_ast::statement::UnitTypeMember::NestedStatement(statement) => {
                        collect_from_statement(statement, depth, diagnostics);
                    }
                }
            }
        }
        Statement::Package { .. }
        | Statement::Import { .. }
        | Statement::Comment(_)
        | Statement::Break(..)
        | Statement::Continue(..) => {}
    }
}

fn collect_from_test_dataset(
    dataset: &TestDataset,
    depth: usize,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let TestDataset::InlineArray { rows, .. } = dataset {
        for row in rows {
            for value in &row.values {
                collect_from_expression(value, depth, diagnostics);
            }
        }
    }
}

fn collect_from_expression(expr: &Expression, depth: usize, diagnostics: &mut Vec<Diagnostic>) {
    match expr {
        Expression::LogBlock(block) => collect_from_log_block(block, depth + 1, diagnostics),
        Expression::Call { function, args, .. } => {
            collect_from_expression(function, depth, diagnostics);
            for argument in args {
                match argument {
                    Argument::Positional(value) => {
                        collect_from_expression(value, depth, diagnostics)
                    }
                    Argument::Named { value, .. } => {
                        collect_from_expression(value, depth, diagnostics)
                    }
                }
            }
        }
        Expression::Binary { left, right, .. } => {
            collect_from_expression(left, depth, diagnostics);
            collect_from_expression(right, depth, diagnostics);
        }
        Expression::Unary { operand, .. } | Expression::TypeCast { expr: operand, .. } => {
            collect_from_expression(operand, depth, diagnostics);
        }
        Expression::MemberAccess { object, .. }
        | Expression::NullSafeMemberAccess { object, .. } => {
            collect_from_expression(object, depth, diagnostics);
        }
        Expression::IndexAccess { object, index, .. }
        | Expression::NullSafeIndexAccess { object, index, .. } => {
            collect_from_expression(object, depth, diagnostics);
            collect_from_expression(index, depth, diagnostics);
        }
        Expression::Array { elements, .. } => {
            for element in elements {
                collect_from_expression(element, depth, diagnostics);
            }
        }
        Expression::Tuple { elements, .. } => {
            for element in elements {
                collect_from_expression(element, depth, diagnostics);
            }
        }
        Expression::UnitLiteral { value, .. } => {
            collect_from_expression(value, depth, diagnostics);
        }
        Expression::Lambda {
            parameters, body, ..
        } => {
            for parameter in parameters {
                if let Some(default) = &parameter.default_value {
                    collect_from_expression(default, depth, diagnostics);
                }
            }
            collect_from_expression(body, depth, diagnostics);
        }
        Expression::Block { statements, .. } => {
            for statement in statements {
                collect_from_statement(statement, depth, diagnostics);
            }
        }
        Expression::When {
            expr,
            arms,
            else_arm,
            ..
        } => {
            if let Some(condition) = expr {
                collect_from_expression(condition, depth, diagnostics);
            }
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_from_expression(guard, depth, diagnostics);
                }
                collect_from_expression(&arm.body, depth, diagnostics);
            }
            if let Some(else_branch) = else_arm {
                collect_from_expression(else_branch, depth, diagnostics);
            }
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_from_expression(condition, depth, diagnostics);
            collect_from_expression(then_branch, depth, diagnostics);
            if let Some(branch) = else_branch {
                collect_from_expression(branch, depth, diagnostics);
            }
        }
        Expression::Try {
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            collect_from_expression(body, depth, diagnostics);
            for clause in catch_clauses {
                collect_from_expression(&clause.body, depth, diagnostics);
            }
            if let Some(finally_block) = finally_block {
                collect_from_expression(finally_block, depth, diagnostics);
            }
        }
        Expression::StringInterpolation { parts, .. } => {
            for part in parts {
                if let jv_ast::StringPart::Expression(expr) = part {
                    collect_from_expression(expr, depth, diagnostics);
                }
            }
        }
        Expression::Literal(_, _)
        | Expression::Identifier(_, _)
        | Expression::RegexLiteral(_)
        | Expression::RegexCommand(_)
        | Expression::JsonLiteral(_)
        | Expression::MultilineString(_)
        | Expression::This(_)
        | Expression::Super(_) => {}
    }
}

fn collect_from_log_block(block: &LogBlock, depth: usize, diagnostics: &mut Vec<Diagnostic>) {
    if depth > MAX_ALLOWED_LOG_BLOCK_DEPTH {
        diagnostics.push(Diagnostic {
            range: span_to_range(&block.span),
            severity: Some(DiagnosticSeverity::Error),
            message: "ログブロックのネストは1段までです。\nLog blocks support at most a single nested level."
                .to_string(),
            code: Some(LOG_NESTING_DIAGNOSTIC_CODE.to_string()),
            source: Some("jv-lsp".to_string()),
            help: Some("内部のログブロックを関数へ切り出すか、別のログブロックに分割してください。".to_string()),
            suggestions: vec!["ネストを浅くする: LOG { ... } を通常のブロックへ移動する".to_string()],
            strategy: Some("Immediate".to_string()),
        });
    }

    let has_message = block
        .items
        .iter()
        .any(|item| matches!(item, LogItem::Expression(_)));
    if !has_message {
        diagnostics.push(Diagnostic {
            range: span_to_range(&block.span),
            severity: Some(DiagnosticSeverity::Warning),
            message: "ログブロックにメッセージ式が存在しません。\nAdd at least one log message expression."
                .to_string(),
            code: Some(LOG_MISSING_MESSAGE_CODE.to_string()),
            source: Some("jv-lsp".to_string()),
            help: Some("LOG { \"message\" } のような文字列式を追加してください。".to_string()),
            suggestions: vec!["LOG ブロック内に \"message\" 形式の式を追加する".to_string()],
            strategy: Some("Immediate".to_string()),
        });
    }

    for item in &block.items {
        match item {
            LogItem::Nested(inner) => collect_from_log_block(inner, depth + 1, diagnostics),
            LogItem::Statement(stmt) => collect_from_statement(stmt, depth, diagnostics),
            LogItem::Expression(expr) => collect_from_expression(expr, depth, diagnostics),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_parser_frontend::ParserPipeline;
    use jv_parser_rowan::frontend::RowanPipeline;

    fn parse_program(source: &str) -> jv_ast::Program {
        RowanPipeline::default()
            .parse(source)
            .expect("source should parse for logging diagnostics")
            .into_program()
    }

    #[test]
    fn unit_syntax_nodes_do_not_emit_logging_diagnostics() {
        let program = parse_program(
            r#"
@ 温度(Double) ℃ {
    基準 := 273.15
}

val reading = 42 @ ℃
LOG {
    "temperature reading"
    reading
}
"#,
        );

        let diagnostics = collect_logging_diagnostics(&program);
        assert!(
            diagnostics.is_empty(),
            "expected no diagnostics, got {diagnostics:?}"
        );
    }
}
