use super::*;

struct VariableInfo<'a> {
    is_final: bool,
    modifiers: &'a IrModifiers,
}

fn find_variable_in_program<'a>(program: &'a IrProgram, name: &str) -> VariableInfo<'a> {
    find_variable_in_statements(&program.type_declarations, name)
        .unwrap_or_else(|| panic!("variable `{name}` not found in IR program"))
}

fn find_variable_in_statements<'a>(
    statements: &'a [IrStatement],
    name: &str,
) -> Option<VariableInfo<'a>> {
    statements
        .iter()
        .find_map(|statement| find_variable_in_statement(statement, name))
}

fn find_variable_in_statement<'a>(
    statement: &'a IrStatement,
    name: &str,
) -> Option<VariableInfo<'a>> {
    match statement {
        IrStatement::VariableDeclaration {
            name: var_name,
            is_final,
            modifiers,
            ..
        } if var_name == name => Some(VariableInfo {
            is_final: *is_final,
            modifiers,
        }),
        IrStatement::VariableDeclaration { .. } => None,
        IrStatement::Commented { statement, .. } => find_variable_in_statement(statement, name),
        IrStatement::MethodDeclaration { body, .. } => body
            .as_ref()
            .and_then(|expr| find_variable_in_expression(expr, name)),
        IrStatement::ClassDeclaration {
            fields,
            methods,
            nested_classes,
            ..
        } => find_variable_in_statements(fields, name)
            .or_else(|| find_variable_in_statements(methods, name))
            .or_else(|| find_variable_in_statements(nested_classes, name)),
        IrStatement::InterfaceDeclaration {
            methods,
            default_methods,
            fields,
            nested_types,
            ..
        } => find_variable_in_statements(methods, name)
            .or_else(|| find_variable_in_statements(default_methods, name))
            .or_else(|| find_variable_in_statements(fields, name))
            .or_else(|| find_variable_in_statements(nested_types, name)),
        IrStatement::RecordDeclaration { methods, .. } => {
            find_variable_in_statements(methods, name)
        }
        IrStatement::FieldDeclaration { initializer, .. } => initializer
            .as_ref()
            .and_then(|expr| find_variable_in_expression(expr, name)),
        IrStatement::Expression { expr, .. } => find_variable_in_expression(expr, name),
        IrStatement::Return { value, .. } => value
            .as_ref()
            .and_then(|expr| find_variable_in_expression(expr, name)),
        IrStatement::If {
            condition,
            then_stmt,
            else_stmt,
            ..
        } => find_variable_in_expression(condition, name)
            .or_else(|| find_variable_in_statement(then_stmt, name))
            .or_else(|| {
                else_stmt
                    .as_ref()
                    .and_then(|stmt| find_variable_in_statement(stmt, name))
            }),
        IrStatement::While {
            condition, body, ..
        } => find_variable_in_expression(condition, name)
            .or_else(|| find_variable_in_statement(body, name)),
        IrStatement::ForEach { iterable, body, .. } => find_variable_in_expression(iterable, name)
            .or_else(|| find_variable_in_statement(body, name)),
        IrStatement::For {
            init,
            condition,
            update,
            body,
            ..
        } => init
            .as_deref()
            .and_then(|stmt| find_variable_in_statement(stmt, name))
            .or_else(|| {
                condition
                    .as_ref()
                    .and_then(|expr| find_variable_in_expression(expr, name))
            })
            .or_else(|| {
                update
                    .as_ref()
                    .and_then(|expr| find_variable_in_expression(expr, name))
            })
            .or_else(|| find_variable_in_statement(body, name)),
        IrStatement::Switch {
            discriminant,
            cases,
            ..
        } => find_variable_in_expression(discriminant, name).or_else(|| {
            cases.iter().find_map(|case| {
                case.guard
                    .as_ref()
                    .and_then(|expr| find_variable_in_expression(expr, name))
                    .or_else(|| find_variable_in_expression(&case.body, name))
            })
        }),
        IrStatement::Try {
            body,
            catch_clauses,
            finally_block,
            ..
        } => find_variable_in_statement(body, name)
            .or_else(|| {
                catch_clauses
                    .iter()
                    .find_map(|clause| find_variable_in_statement(&clause.body, name))
            })
            .or_else(|| {
                finally_block
                    .as_deref()
                    .and_then(|stmt| find_variable_in_statement(stmt, name))
            }),
        IrStatement::TryWithResources {
            resources,
            body,
            catch_clauses,
            finally_block,
            ..
        } => resources
            .iter()
            .find_map(|resource| find_variable_in_expression(&resource.initializer, name))
            .or_else(|| find_variable_in_statement(body, name))
            .or_else(|| {
                catch_clauses
                    .iter()
                    .find_map(|clause| find_variable_in_statement(&clause.body, name))
            })
            .or_else(|| {
                finally_block
                    .as_deref()
                    .and_then(|stmt| find_variable_in_statement(stmt, name))
            }),
        IrStatement::Throw { expr, .. } => find_variable_in_expression(expr, name),
        IrStatement::Block { statements, .. } => find_variable_in_statements(statements, name),
        IrStatement::SampleDeclaration(_)
        | IrStatement::Import(_)
        | IrStatement::Package { .. }
        | IrStatement::Comment { .. }
        | IrStatement::Break { .. }
        | IrStatement::Continue { .. } => None,
    }
}

fn find_variable_in_expression<'a>(expr: &'a IrExpression, name: &str) -> Option<VariableInfo<'a>> {
    match expr {
        IrExpression::Block { statements, .. } => find_variable_in_statements(statements, name),
        IrExpression::Lambda { body, .. } => find_variable_in_expression(body, name),
        IrExpression::MethodCall { receiver, args, .. } => receiver
            .as_ref()
            .and_then(|recv| find_variable_in_expression(recv, name))
            .or_else(|| {
                args.iter()
                    .find_map(|arg| find_variable_in_expression(arg, name))
            }),
        IrExpression::FieldAccess { receiver, .. } => find_variable_in_expression(receiver, name),
        IrExpression::ArrayAccess { array, index, .. } => find_variable_in_expression(array, name)
            .or_else(|| find_variable_in_expression(index, name)),
        IrExpression::Binary { left, right, .. } => find_variable_in_expression(left, name)
            .or_else(|| find_variable_in_expression(right, name)),
        IrExpression::Unary { operand, .. } => find_variable_in_expression(operand, name),
        IrExpression::Assignment { target, value, .. } => find_variable_in_expression(target, name)
            .or_else(|| find_variable_in_expression(value, name)),
        IrExpression::Conditional {
            condition,
            then_expr,
            else_expr,
            ..
        } => find_variable_in_expression(condition, name)
            .or_else(|| find_variable_in_expression(then_expr, name))
            .or_else(|| find_variable_in_expression(else_expr, name)),
        IrExpression::ArrayCreation {
            dimensions,
            initializer,
            ..
        } => {
            for dimension in dimensions {
                if let Some(dimension) = dimension {
                    if let Some(found) = find_variable_in_expression(dimension, name) {
                        return Some(found);
                    }
                }
            }
            if let Some(elements) = initializer {
                for element in elements {
                    if let Some(found) = find_variable_in_expression(element, name) {
                        return Some(found);
                    }
                }
            }
            None
        }
        IrExpression::ObjectCreation { args, .. } => args
            .iter()
            .find_map(|arg| find_variable_in_expression(arg, name)),
        IrExpression::Switch {
            discriminant,
            cases,
            ..
        } => {
            if let Some(found) = find_variable_in_expression(discriminant, name) {
                return Some(found);
            }
            for case in cases {
                if let Some(found) = case
                    .guard
                    .as_ref()
                    .and_then(|expr| find_variable_in_expression(expr, name))
                {
                    return Some(found);
                }
                if let Some(found) = find_variable_in_expression(&case.body, name) {
                    return Some(found);
                }
            }
            None
        }
        IrExpression::Cast { expr: inner, .. } | IrExpression::InstanceOf { expr: inner, .. } => {
            find_variable_in_expression(inner, name)
        }
        IrExpression::NullSafeOperation {
            expr: inner,
            operation,
            default_value,
            ..
        } => find_variable_in_expression(inner, name)
            .or_else(|| find_variable_in_expression(operation, name))
            .or_else(|| {
                default_value
                    .as_ref()
                    .and_then(|value| find_variable_in_expression(value, name))
            }),
        IrExpression::TryWithResources {
            resources, body, ..
        } => {
            for resource in resources {
                if let Some(found) = find_variable_in_expression(&resource.initializer, name) {
                    return Some(found);
                }
            }
            find_variable_in_expression(body, name)
        }
        IrExpression::CompletableFuture { args, .. }
        | IrExpression::VirtualThread { args, .. }
        | IrExpression::StringFormat { args, .. } => args
            .iter()
            .find_map(|arg| find_variable_in_expression(arg, name)),
        _ => None,
    }
}

fn method_body<'a>(program: &'a IrProgram, method_name: &str) -> &'a IrExpression {
    program
        .type_declarations
        .iter()
        .find_map(|statement| match statement {
            IrStatement::MethodDeclaration {
                name,
                body: Some(body),
                ..
            } if name == method_name => Some(body),
            _ => None,
        })
        .unwrap_or_else(|| panic!("method `{method_name}` not found in IR program"))
}

#[test]
fn top_level_val_variants_are_final() {
    let source = r#"
val explicit = 1
typed: Int = 2
implicit = 3
var mutable = 4
"#;

    let program = parse_program(source);
    let mut context = TransformContext::new();
    let ir_program = transform_program_with_context(program, &mut context)
        .expect("IR transformation should succeed");

    let explicit = find_variable_in_program(&ir_program, "explicit");
    assert!(explicit.is_final, "explicit val should be final");
    assert!(
        explicit.modifiers.is_final,
        "explicit val modifiers should be final"
    );

    let typed = find_variable_in_program(&ir_program, "typed");
    assert!(typed.is_final, "typed implicit val should be final");
    assert!(
        typed.modifiers.is_final,
        "typed implicit val modifiers should be final"
    );

    let implicit = find_variable_in_program(&ir_program, "implicit");
    assert!(implicit.is_final, "implicit val should be final");
    assert!(
        implicit.modifiers.is_final,
        "implicit val modifiers should be final"
    );

    let mutable = find_variable_in_program(&ir_program, "mutable");
    assert!(!mutable.is_final, "var should not be final");
    assert!(
        !mutable.modifiers.is_final,
        "var modifiers should not mark final"
    );
}

#[test]
fn function_scope_val_variants_are_final() {
    let source = r#"
fun sample() {
    val explicit = 1
    typed: Int = 2
    implicit = 3
    var mutable = 4
}
"#;

    let program = parse_program(source);
    let mut context = TransformContext::new();
    let ir_program = transform_program_with_context(program, &mut context)
        .expect("IR transformation should succeed");

    let body = method_body(&ir_program, "sample");

    let explicit = find_variable_in_expression(body, "explicit")
        .expect("explicit val should exist in function body");
    assert!(
        explicit.is_final,
        "explicit val inside function should be final"
    );
    assert!(explicit.modifiers.is_final);

    let typed = find_variable_in_expression(body, "typed")
        .expect("typed implicit val should exist in function body");
    assert!(
        typed.is_final,
        "typed implicit val should be final inside function"
    );
    assert!(typed.modifiers.is_final);

    let implicit = find_variable_in_expression(body, "implicit")
        .expect("implicit val should exist in function body");
    assert!(
        implicit.is_final,
        "implicit val should be final inside function"
    );
    assert!(implicit.modifiers.is_final);

    let mutable =
        find_variable_in_expression(body, "mutable").expect("var should exist in function body");
    assert!(
        !mutable.is_final,
        "var inside function should remain mutable"
    );
    assert!(!mutable.modifiers.is_final);
}

#[test]
fn nested_scopes_preserve_val_finality() {
    let source = r#"
fun nested() {
    when (true) {
        true -> {
            inner = 1
            inner_typed: Int = 2
            var mutable_inner = 3
            when (inner) {
                else -> {
                    deep = 4
                }
            }
        }
        else -> { }
    }
}
"#;

    let program = parse_program(source);
    let mut context = TransformContext::new();
    let ir_program = transform_program_with_context(program, &mut context)
        .expect("IR transformation should succeed");

    let body = method_body(&ir_program, "nested");

    let inner = find_variable_in_expression(body, "inner")
        .expect("implicit val in nested block should exist");
    assert!(
        inner.is_final,
        "implicit val in nested block should be final"
    );

    let inner_typed = find_variable_in_expression(body, "inner_typed")
        .expect("typed implicit val should exist in nested block");
    assert!(
        inner_typed.is_final,
        "typed implicit val should be final in nested block"
    );

    let deep = find_variable_in_expression(body, "deep").expect("deeply nested val should exist");
    assert!(deep.is_final, "deeply nested implicit val should be final");

    let mutable_inner = find_variable_in_expression(body, "mutable_inner")
        .expect("var should exist in nested block");
    assert!(
        !mutable_inner.is_final,
        "var in nested block should remain mutable"
    );
}
