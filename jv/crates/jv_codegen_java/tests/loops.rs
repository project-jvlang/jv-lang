use jv_ast::{BinaryOp, Literal, Span};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator};
use jv_ir::{
    IrExpression, IrForEachKind, IrForLoopMetadata, IrModifiers, IrNumericRangeLoop, IrStatement,
    JavaType,
};
use jv_pm::JavaTarget;

fn dummy_span() -> Span {
    Span::dummy()
}

fn identifier(name: &str, ty: &JavaType, span: Span) -> IrExpression {
    IrExpression::Identifier {
        name: name.to_string(),
        java_type: ty.clone(),
        span,
    }
}

fn number_literal(value: &str, span: Span) -> IrExpression {
    IrExpression::Literal(Literal::Number(value.to_string()), span)
}

fn empty_block(span: Span) -> IrStatement {
    IrStatement::Block {
        label: None,
        statements: Vec::new(),
        span,
    }
}

fn numeric_range_for(inclusive: bool) -> IrStatement {
    let span = dummy_span();
    let binding_type = JavaType::int();
    let end_type = JavaType::int();

    let init = IrStatement::VariableDeclaration {
        name: "idx".to_string(),
        java_type: binding_type.clone(),
        initializer: Some(number_literal("0", span.clone())),
        is_final: false,
        modifiers: IrModifiers::default(),
        span: span.clone(),
    };

    let condition_op = if inclusive {
        BinaryOp::LessEqual
    } else {
        BinaryOp::Less
    };

    let condition = IrExpression::Binary {
        left: Box::new(identifier("idx", &binding_type, span.clone())),
        op: condition_op,
        right: Box::new(identifier("__jv_range_end_0", &end_type, span.clone())),
        java_type: JavaType::boolean(),
        span: span.clone(),
    };

    let update = IrExpression::Assignment {
        target: Box::new(identifier("idx", &binding_type, span.clone())),
        value: Box::new(IrExpression::Binary {
            left: Box::new(identifier("idx", &binding_type, span.clone())),
            op: BinaryOp::Add,
            right: Box::new(number_literal("1", span.clone())),
            java_type: binding_type.clone(),
            span: span.clone(),
        }),
        java_type: binding_type.clone(),
        span: span.clone(),
    };

    IrStatement::For {
        label: None,
        init: Some(Box::new(init)),
        condition: Some(condition),
        update: Some(update),
        body: Box::new(empty_block(span.clone())),
        metadata: Some(IrForLoopMetadata::NumericRange(IrNumericRangeLoop {
            binding: "idx".to_string(),
            binding_type: binding_type.clone(),
            end_variable: "__jv_range_end_0".to_string(),
            end_type,
            inclusive,
            span,
        })),
        span: dummy_span(),
    }
}

fn foreach_statement(kind: IrForEachKind) -> IrStatement {
    let span = dummy_span();
    let item_type = JavaType::string();
    let iterable_type = JavaType::object();
    IrStatement::ForEach {
        label: None,
        variable: "item".to_string(),
        variable_type: item_type.clone(),
        iterable: identifier("items", &iterable_type, span.clone()),
        body: Box::new(empty_block(span.clone())),
        iterable_kind: kind,
        span,
    }
}

#[test]
fn numeric_range_loop_uses_increment_operator_java25() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_statement(&numeric_range_for(false))
        .expect("code generation should succeed");
    let header = rendered.lines().next().expect("loop should render header");
    assert_eq!(
        header, "for (int idx = 0; idx < __jv_range_end_0; idx++) {",
        "exclusive range should use < and idx++"
    );
}

#[test]
fn inclusive_range_loop_uses_less_equal() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_statement(&numeric_range_for(true))
        .expect("code generation should succeed");
    let header = rendered.lines().next().expect("loop should render header");
    assert!(
        header.contains("<="),
        "inclusive range should render <= comparison"
    );
    assert!(
        header.ends_with("idx++) {"),
        "inclusive range should still use increment operator"
    );
}

#[test]
fn numeric_range_loop_preserves_legacy_java21_output() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let rendered = generator
        .generate_statement(&numeric_range_for(false))
        .expect("code generation should succeed");
    let header = rendered.lines().next().expect("loop should render header");
    assert!(
        header.contains("idx < __jv_range_end_0"),
        "legacy target should continue using < comparison"
    );
    assert!(
        header.contains("idx++)"),
        "legacy target should render compact increment"
    );
}

#[test]
fn iterable_foreach_uses_enhanced_for() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_statement(&foreach_statement(IrForEachKind::Iterable))
        .expect("foreach generation");
    let header = rendered
        .lines()
        .next()
        .expect("foreach should render header");
    assert_eq!(
        header, "for (String item : items) {",
        "iterable loops should render enhanced for"
    );
}

#[test]
fn lazy_sequence_foreach_wraps_with_cleanup() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let rendered = generator
        .generate_statement(&foreach_statement(IrForEachKind::LazySequence {
            needs_cleanup: true,
        }))
        .expect("lazy sequence generation");
    assert!(
        rendered.contains("final var __jvSequence = items;"),
        "lazy sequence should cache iterable expression"
    );
    assert!(
        rendered.contains("if (__jvSequence instanceof AutoCloseable closeable)"),
        "lazy sequence should guard cleanup with AutoCloseable"
    );
    assert!(
        rendered.contains("closeable.close();"),
        "lazy sequence cleanup should invoke close()"
    );
}

#[test]
fn labeled_foreach_outputs_java_label() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let mut stmt = foreach_statement(IrForEachKind::Iterable);
    if let IrStatement::ForEach { label, .. } = &mut stmt {
        *label = Some("#外側".to_string());
    } else {
        panic!("for-each以外のステートメントが生成されました");
    }
    let rendered = generator
        .generate_statement(&stmt)
        .expect("ラベル付きfor-eachの生成に失敗しました");
    let header = rendered.lines().next().expect("先頭行が必要です");
    assert_eq!(
        header, "外側: for (String item : items) {",
        "ハッシュラベルがJava標準ラベルに変換されるべきです"
    );
}

#[test]
fn labeled_lazy_sequence_foreach_preserves_label_inside_try() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let mut stmt = foreach_statement(IrForEachKind::LazySequence {
        needs_cleanup: true,
    });
    if let IrStatement::ForEach { label, .. } = &mut stmt {
        *label = Some("#監視".to_string());
    }
    let rendered = generator
        .generate_statement(&stmt)
        .expect("ラベル付きlazy sequenceの生成に失敗しました");
    let lines: Vec<_> = rendered.lines().collect();
    assert!(
        lines
            .iter()
            .any(|line| line.trim_start().starts_with("監視: for")),
        "tryブロック内のforヘッダに正規化されたラベルが含まれるべきです:\n{rendered}"
    );
}

#[test]
fn labeled_numeric_for_loop_outputs_label() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let mut stmt = numeric_range_for(false);
    if let IrStatement::For { label, .. } = &mut stmt {
        *label = Some("#外側ループ".to_string());
    }
    let rendered = generator
        .generate_statement(&stmt)
        .expect("ラベル付きforループの生成に失敗しました");
    let header = rendered.lines().next().expect("先頭行が必要です");
    assert!(
        header.starts_with("外側ループ: for (int idx = 0;"),
        "伝搬したラベルがforヘッダに出力されるべきです: {header}"
    );
}

#[test]
fn labeled_block_renders_prefixed_brace() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let stmt = IrStatement::Block {
        label: Some("#外側ブロック".to_string()),
        statements: vec![],
        span: dummy_span(),
    };
    let rendered = generator
        .generate_statement(&stmt)
        .expect("ラベル付きブロックの生成に失敗しました");
    let mut lines = rendered.lines();
    assert_eq!(
        lines.next().unwrap_or_default(),
        "外側ブロック: {",
        "ブロックの先頭にラベルが付与されるべきです"
    );
}

#[test]
fn labeled_break_and_continue_strip_hash_prefix() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let break_stmt = IrStatement::Break {
        label: Some("#終了".to_string()),
        span: dummy_span(),
    };
    let continue_stmt = IrStatement::Continue {
        label: Some("#継続".to_string()),
        span: dummy_span(),
    };
    let break_rendered = generator
        .generate_statement(&break_stmt)
        .expect("break生成に失敗しました");
    let continue_rendered = generator
        .generate_statement(&continue_stmt)
        .expect("continue生成に失敗しました");
    assert_eq!(
        break_rendered, "break 終了;",
        "break文ではハッシュが除去された識別子のみ出力するべきです"
    );
    assert_eq!(
        continue_rendered, "continue 継続;",
        "continue文でもハッシュが除去された識別子のみ出力するべきです"
    );
}

#[test]
fn lambda_return_ignores_label_metadata() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let span = dummy_span();
    let lambda_body = IrExpression::Block {
        statements: vec![IrStatement::Return {
            value: Some(IrExpression::Identifier {
                name: "value".to_string(),
                java_type: JavaType::int(),
                span: span.clone(),
            }),
            span: span.clone(),
        }],
        java_type: JavaType::int(),
        span: span.clone(),
    };
    let lambda = IrExpression::Lambda {
        functional_interface: "java.util.function.IntSupplier".to_string(),
        param_names: Vec::new(),
        param_types: Vec::new(),
        body: Box::new(lambda_body),
        java_type: JavaType::int(),
        span,
    };
    let rendered = generator
        .generate_expression(&lambda)
        .expect("ラムダ式の生成に失敗しました");
    assert!(
        rendered.contains("return value;"),
        "ラムダのreturnは平坦なreturnステートメントになるべきです: {rendered}"
    );
    assert!(
        !rendered.contains('#'),
        "ラムダ内のreturnではハッシュ付きラベルが出力されるべきではありません: {rendered}"
    );
}
