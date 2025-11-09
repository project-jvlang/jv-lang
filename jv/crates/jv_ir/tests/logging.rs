use jv_ast::{
    Expression, Literal, LogBlock, LogItem, Modifiers, Program, Span, Statement, ValBindingOrigin,
    expression::LogBlockLevel,
};
use jv_ir::{
    IrExpression, IrProgram, IrStatement, LogGuardKind, LogInvocationItem, LogLevel,
    TransformContext, transform_program_with_context,
};

fn dummy_span() -> Span {
    Span::dummy()
}

fn string_literal(value: &str) -> Expression {
    Expression::Literal(Literal::String(value.to_string()), dummy_span())
}

fn build_program(log_block: LogBlock) -> Program {
    let span = dummy_span();
    Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::FunctionDeclaration {
            name: "main".to_string(),
            type_parameters: Vec::new(),
            generic_signature: None,
            where_clause: None,
            parameters: Vec::new(),
            return_type: None,
            primitive_return: None,
            body: Box::new(Expression::Block {
                statements: vec![Statement::Expression {
                    expr: Expression::LogBlock(log_block),
                    span: span.clone(),
                }],
                span: span.clone(),
            }),
            modifiers: Modifiers::default(),
            span: span.clone(),
        }],
        span,
    }
}

fn extract_log_plan(ir_program: &IrProgram) -> Option<&jv_ir::LogInvocationPlan> {
    ir_program.type_declarations.iter().find_map(|statement| {
        if let IrStatement::MethodDeclaration {
            name,
            body: Some(body),
            ..
        } = statement
        {
            if name == "main" {
                if let IrExpression::Block { statements, .. } = body {
                    return statements.iter().find_map(|stmt| {
                        if let IrStatement::Expression { expr, .. } = stmt {
                            if let IrExpression::LogInvocation { plan, .. } = expr {
                                return Some(plan.as_ref());
                            }
                        }
                        None
                    });
                }
            }
        }
        None
    })
}

#[test]
fn debug_log_block_emits_guard_and_logger_metadata() {
    let span = dummy_span();
    let log_block = LogBlock {
        level: LogBlockLevel::Debug,
        items: vec![
            LogItem::Statement(Statement::ValDeclaration {
                name: "userId".to_string(),
                binding: None,
                type_annotation: None,
                initializer: Expression::Literal(Literal::Number("42".to_string()), span.clone()),
                modifiers: Modifiers::default(),
                origin: ValBindingOrigin::Implicit,
                span: span.clone(),
            }),
            LogItem::Expression(string_literal("debug message")),
        ],
        span: span.clone(),
    };

    let program = build_program(log_block);
    let mut context = TransformContext::new();
    context.logging_options_mut().active_level = LogLevel::Debug;
    let ir_program = transform_program_with_context(program, &mut context)
        .expect("ローワリングに成功するはずです");

    let plan = extract_log_plan(&ir_program).expect("ログ呼び出しが生成されるはずです");
    assert_eq!(plan.level, LogLevel::Debug);
    assert_eq!(
        plan.guard_kind,
        Some(LogGuardKind::DebugEnabled),
        "DEBUG レベルではガードが必要です"
    );
    assert!(
        matches!(plan.items.first(), Some(LogInvocationItem::Statement(_))),
        "最初の項目はステートメントとしてローワリングされる必要があります"
    );
    assert!(
        matches!(plan.items.last(), Some(LogInvocationItem::Message(_))),
        "末尾の項目はログメッセージとして記録される必要があります"
    );
    assert_eq!(
        ir_program.logging.logger_fields.len(),
        1,
        "ロガーフィールドは1件だけ割り当てられるはずです"
    );
}

#[test]
fn trace_block_is_filtered_below_active_threshold() {
    let log_block = LogBlock {
        level: LogBlockLevel::Trace,
        items: vec![LogItem::Expression(string_literal("trace message"))],
        span: dummy_span(),
    };

    let program = build_program(log_block);
    let mut context = TransformContext::new();
    context.logging_options_mut().active_level = LogLevel::Error;

    let ir_program = transform_program_with_context(program, &mut context)
        .expect("ローワリングに成功するはずです");

    assert!(
        extract_log_plan(&ir_program).is_none(),
        "アクティブレベルより低いログは生成されるべきではありません"
    );
    assert!(
        ir_program.logging.logger_fields.is_empty(),
        "ログが生成されなければフィールドも追加されないはずです"
    );
}
