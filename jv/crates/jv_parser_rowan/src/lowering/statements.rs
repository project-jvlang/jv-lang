use super::helpers::{
    collect_annotation_texts, first_identifier_text, JvSyntaxNode, LoweringContext,
};
use crate::syntax::SyntaxKind;
use jv_ast::expression::Parameter;
use jv_ast::statement::{ForInStatement, LoopBinding, LoopStrategy, Property, ValBindingOrigin};
use jv_ast::types::{Literal, Modifiers, TypeAnnotation};
use jv_ast::{Expression, Span, Statement};
use jv_lexer::{Token, TokenType};

/// ローワリング結果。
#[derive(Debug)]
pub struct LoweringResult {
    /// 正常にローワリングされたステートメント列。
    pub statements: Vec<Statement>,
    /// ローワリング中に発生した診断。
    pub diagnostics: Vec<LoweringDiagnostic>,
}

impl LoweringResult {
    /// 診断が存在しないか判定する。
    pub fn is_success(&self) -> bool {
        self.diagnostics.is_empty()
    }
}

/// ローワリング診断の深刻度。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoweringDiagnosticSeverity {
    /// エラー。
    Error,
    /// 警告。
    Warning,
}

/// ローワリング診断情報。
#[derive(Debug, Clone)]
pub struct LoweringDiagnostic {
    /// メッセージ本文。
    pub message: String,
    /// 深刻度。
    pub severity: LoweringDiagnosticSeverity,
    /// 対象スパン。
    pub span: Option<jv_ast::Span>,
    /// 対象ノード種別。
    pub node_kind: SyntaxKind,
    /// 同定に利用できる識別子（存在する場合）。
    pub identifier: Option<String>,
    /// 関連するアノテーションテキスト。
    pub annotations: Vec<String>,
}

impl LoweringDiagnostic {
    fn new(
        severity: LoweringDiagnosticSeverity,
        message: impl Into<String>,
        span: Option<jv_ast::Span>,
        node_kind: SyntaxKind,
        identifier: Option<String>,
        annotations: Vec<String>,
    ) -> Self {
        Self {
            message: message.into(),
            severity,
            span,
            node_kind,
            identifier,
            annotations,
        }
    }
}

/// Rowan構文木からASTステートメントを生成する。
pub fn lower_program(root: &JvSyntaxNode, tokens: &[Token]) -> LoweringResult {
    let context = LoweringContext::new(tokens);
    let mut statements = Vec::new();
    let mut diagnostics = Vec::new();

    for child in root.children() {
        if child.kind().is_token() {
            continue;
        }

        if matches!(child.kind(), SyntaxKind::Error | SyntaxKind::BlockError) {
            let diagnostic = LoweringDiagnostic::new(
                LoweringDiagnosticSeverity::Error,
                "Rowanパーサでエラーノードが生成されました",
                context.span_for(&child),
                child.kind(),
                first_identifier_text(&child),
                collect_annotation_texts(&child),
            );
            diagnostics.push(diagnostic);
            continue;
        }

        if !is_top_level_statement(child.kind()) {
            continue;
        }

        match lower_single_statement(&context, &child, &mut diagnostics) {
            Ok(statement) => statements.push(statement),
            Err(diagnostic) => diagnostics.push(diagnostic),
        }
    }

    LoweringResult {
        statements,
        diagnostics,
    }
}

fn is_top_level_statement(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::PackageDeclaration
            | SyntaxKind::ImportDeclaration
            | SyntaxKind::ValDeclaration
            | SyntaxKind::VarDeclaration
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::IfStatement
            | SyntaxKind::WhenStatement
            | SyntaxKind::ForStatement
            | SyntaxKind::WhileStatement
            | SyntaxKind::DoWhileStatement
            | SyntaxKind::ReturnStatement
            | SyntaxKind::ThrowStatement
            | SyntaxKind::BreakStatement
            | SyntaxKind::ContinueStatement
            | SyntaxKind::Expression
    )
}

fn lower_single_statement(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    if context.tokens_for(node).is_empty() {
        return Err(LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "Rowanノードに対応するトークン列が空です",
            None,
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        ));
    }

    match node.kind() {
        SyntaxKind::PackageDeclaration => lower_package(context, node),
        SyntaxKind::ImportDeclaration => lower_import(context, node),
        SyntaxKind::ValDeclaration => lower_value(context, node, true, diagnostics),
        SyntaxKind::VarDeclaration => lower_value(context, node, false, diagnostics),
        SyntaxKind::FunctionDeclaration => lower_function(context, node, diagnostics),
        SyntaxKind::ClassDeclaration => lower_class(context, node, diagnostics),
        SyntaxKind::ForStatement => lower_for(context, node, diagnostics),
        SyntaxKind::ReturnStatement => lower_return(context, node, diagnostics),
        SyntaxKind::ThrowStatement => lower_throw(context, node, diagnostics),
        SyntaxKind::BreakStatement => lower_break(context, node),
        SyntaxKind::ContinueStatement => lower_continue(context, node),
        SyntaxKind::IfStatement
        | SyntaxKind::WhenStatement
        | SyntaxKind::WhileStatement
        | SyntaxKind::DoWhileStatement
        | SyntaxKind::Expression => lower_expression_statement(context, node, diagnostics),
        kind => Err(LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            format!(
                "Rowanノード {:?} のローワリングはまだ実装されていません",
                kind
            ),
            context.span_for(node),
            kind,
            first_identifier_text(node),
            collect_annotation_texts(node),
        )),
    }
}

fn lower_package(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Result<Statement, LoweringDiagnostic> {
    let name_node = child_node(node, SyntaxKind::PackageName)
        .and_then(|n| child_node(&n, SyntaxKind::QualifiedName))
        .ok_or_else(|| {
            missing_child_diagnostic(
                context,
                node,
                "package 名が必要です",
                SyntaxKind::PackageName,
            )
        })?;

    let segments = qualified_name_segments(context, &name_node).ok_or_else(|| {
        LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "package 名の解析に失敗しました",
            context.span_for(node),
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        )
    })?;

    Ok(Statement::Package {
        name: segments.join("."),
        span: context.span_for(node).unwrap_or_else(jv_ast::Span::dummy),
    })
}

fn lower_import(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Result<Statement, LoweringDiagnostic> {
    let path_node = child_node(node, SyntaxKind::ImportPath)
        .and_then(|n| child_node(&n, SyntaxKind::QualifiedName))
        .ok_or_else(|| {
            missing_child_diagnostic(
                context,
                node,
                "import パスが必要です",
                SyntaxKind::ImportPath,
            )
        })?;

    let segments = qualified_name_segments(context, &path_node).ok_or_else(|| {
        LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "import パスの解析に失敗しました",
            context.span_for(node),
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        )
    })?;

    let path_tokens = context.tokens_for(node);
    let is_wildcard = path_tokens
        .iter()
        .rev()
        .find(|token| {
            !matches!(
                token.token_type,
                TokenType::Whitespace(_) | TokenType::Newline
            )
        })
        .map(|token| matches!(token.token_type, TokenType::Multiply))
        .unwrap_or(false);

    Ok(Statement::Import {
        path: segments.join("."),
        alias: None,
        is_wildcard,
        span: context.span_for(node).unwrap_or_else(jv_ast::Span::dummy),
    })
}

fn lower_value(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    is_val: bool,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let binding = child_node(node, SyntaxKind::BindingPattern).ok_or_else(|| {
        missing_child_diagnostic(context, node, "バインディング", SyntaxKind::BindingPattern)
    })?;

    let name_tokens = context.tokens_for(&binding);
    let identifier_token = name_tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::Identifier(_)))
        .ok_or_else(|| {
            LoweringDiagnostic::new(
                LoweringDiagnosticSeverity::Error,
                "識別子が必要です",
                context.span_for(&binding),
                binding.kind(),
                first_identifier_text(&binding),
                collect_annotation_texts(&binding),
            )
        })?;

    let name = match &identifier_token.token_type {
        TokenType::Identifier(text) => text.clone(),
        _ => identifier_token.lexeme.clone(),
    };

    let type_annotation = child_node(node, SyntaxKind::TypeAnnotation)
        .and_then(|type_node| child_node(&type_node, SyntaxKind::Expression))
        .and_then(|expr| simple_type_from_expression(context, &expr));

    let initializer_clause = child_node(node, SyntaxKind::InitializerClause);

    let initializer = match initializer_clause {
        Some(clause) => match child_node(&clause, SyntaxKind::Expression) {
            Some(expr_node) => Some(lower_expression_shallow(context, &expr_node, diagnostics)?),
            None => {
                return Err(LoweringDiagnostic::new(
                    LoweringDiagnosticSeverity::Error,
                    "初期化子の解析に失敗しました",
                    context.span_for(&clause),
                    clause.kind(),
                    first_identifier_text(&clause),
                    collect_annotation_texts(&clause),
                ))
            }
        },
        None => None,
    };

    let initializer = if is_val {
        Some(initializer.ok_or_else(|| {
            LoweringDiagnostic::new(
                LoweringDiagnosticSeverity::Error,
                "val 宣言には初期化子が必要です",
                context.span_for(node),
                node.kind(),
                Some(name.clone()),
                collect_annotation_texts(node),
            )
        })?)
    } else {
        initializer
    };

    let span = context.span_for(node).unwrap_or_else(jv_ast::Span::dummy);
    let modifiers = Modifiers::default();

    if is_val {
        Ok(Statement::ValDeclaration {
            name,
            type_annotation,
            initializer: initializer.unwrap(),
            modifiers,
            origin: ValBindingOrigin::ExplicitKeyword,
            span,
        })
    } else {
        Ok(Statement::VarDeclaration {
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
        })
    }
}

fn simple_type_from_expression(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Option<TypeAnnotation> {
    let tokens = context.tokens_for(node);
    if tokens.is_empty() {
        return None;
    }
    Some(TypeAnnotation::Simple(join_tokens(&tokens)))
}

fn lower_expression_shallow(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Expression, LoweringDiagnostic> {
    let tokens = context.tokens_for(node);
    if tokens.is_empty() {
        return Err(LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "式が空です",
            context.span_for(node),
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        ));
    }

    let span = context.span_for(node).unwrap_or_else(jv_ast::Span::dummy);
    if tokens.len() == 1 {
        let token = tokens[0];
        let expr = match &token.token_type {
            TokenType::Number(value) => Expression::Literal(Literal::Number(value.clone()), span),
            TokenType::Boolean(value) => Expression::Literal(Literal::Boolean(*value), span),
            TokenType::String(value) => Expression::Literal(Literal::String(value.clone()), span),
            TokenType::Null => Expression::Literal(Literal::Null, span),
            TokenType::Identifier(text) => Expression::Identifier(text.clone(), span),
            _ => Expression::Identifier(token.lexeme.clone(), span),
        };
        return Ok(expr);
    }

    push_diagnostic(
        diagnostics,
        LoweringDiagnosticSeverity::Warning,
        "式ローワリングは暫定サポートのためテキストをそのまま識別子として扱います",
        context,
        node,
    );

    Ok(Expression::Identifier(join_tokens(&tokens), span))
}

fn qualified_name_segments(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Option<Vec<String>> {
    let mut segments = Vec::new();
    for segment in node
        .children()
        .filter(|child| child.kind() == SyntaxKind::QualifiedNameSegment)
    {
        let tokens = context.tokens_for(&segment);
        let text = tokens
            .iter()
            .find_map(|token| match &token.token_type {
                TokenType::Identifier(value) => Some(value.clone()),
                _ => None,
            })
            .or_else(|| tokens.first().map(|token| token.lexeme.clone()))?;
        segments.push(text);
    }
    if segments.is_empty() {
        None
    } else {
        Some(segments)
    }
}

fn child_node(node: &JvSyntaxNode, kind: SyntaxKind) -> Option<JvSyntaxNode> {
    node.children().find(|child| child.kind() == kind)
}

fn missing_child_diagnostic(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    message: &str,
    expected_kind: SyntaxKind,
) -> LoweringDiagnostic {
    LoweringDiagnostic::new(
        LoweringDiagnosticSeverity::Error,
        message,
        context.span_for(node),
        expected_kind,
        first_identifier_text(node),
        collect_annotation_texts(node),
    )
}

fn join_tokens(tokens: &[&Token]) -> String {
    tokens
        .iter()
        .map(|token| token.lexeme.as_str())
        .collect::<String>()
}

fn push_diagnostic(
    diagnostics: &mut Vec<LoweringDiagnostic>,
    severity: LoweringDiagnosticSeverity,
    message: impl Into<String>,
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) {
    diagnostics.push(LoweringDiagnostic::new(
        severity,
        message,
        context.span_for(node),
        node.kind(),
        first_identifier_text(node),
        collect_annotation_texts(node),
    ));
}

fn lower_function(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let tokens = context.tokens_for(node);
    let span = context.span_for(node).unwrap_or_else(Span::dummy);

    let name = extract_identifier(&tokens).ok_or_else(|| {
        LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "関数名を特定できませんでした",
            context.span_for(node),
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        )
    })?;

    let (parameters, _) = child_node(node, SyntaxKind::FunctionParameterList)
        .map(|list| lower_parameters(context, &list, diagnostics))
        .unwrap_or_default();

    let return_type = child_node(node, SyntaxKind::FunctionReturnType)
        .and_then(|ret| child_node(&ret, SyntaxKind::Expression))
        .and_then(|expr| simple_type_from_expression(context, &expr));

    let fallback_span = span.clone();
    let body = if let Some(block) = child_node(node, SyntaxKind::Block) {
        Expression::Block {
            statements: Vec::new(),
            span: context.span_for(&block).unwrap_or_else(Span::dummy),
        }
    } else {
        node.children()
            .find(|child| child.kind() == SyntaxKind::Expression)
            .map(|expr| lower_expression_shallow(context, &expr, diagnostics))
            .transpose()?
            .unwrap_or_else(|| Expression::Identifier(name.clone(), fallback_span))
    };

    Ok(Statement::FunctionDeclaration {
        name,
        type_parameters: Vec::new(),
        generic_signature: None,
        where_clause: None,
        parameters,
        return_type,
        primitive_return: None,
        body: Box::new(body),
        modifiers: Modifiers::default(),
        span,
    })
}

fn lower_class(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let tokens = context.tokens_for(node);
    let span = context.span_for(node).unwrap_or_else(Span::dummy);

    let name = extract_identifier(&tokens).ok_or_else(|| {
        LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "クラス名を特定できませんでした",
            context.span_for(node),
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        )
    })?;

    let (constructor_params, has_mutable) = child_node(node, SyntaxKind::FunctionParameterList)
        .map(|list| lower_parameters(context, &list, diagnostics))
        .unwrap_or_default();

    let (properties, methods) = child_node(node, SyntaxKind::ClassBody)
        .map(|body| lower_class_members(context, &body, diagnostics))
        .transpose()?
        .unwrap_or_default();

    let modifiers = Modifiers::default();

    let is_data = tokens
        .iter()
        .any(|token| matches!(token.token_type, TokenType::Data));

    if is_data {
        return Ok(Statement::DataClassDeclaration {
            name,
            parameters: constructor_params,
            type_parameters: Vec::new(),
            generic_signature: None,
            is_mutable: has_mutable,
            modifiers,
            span,
        });
    }

    Ok(Statement::ClassDeclaration {
        name,
        type_parameters: Vec::new(),
        generic_signature: None,
        superclass: None,
        interfaces: Vec::new(),
        properties,
        methods,
        modifiers,
        span,
    })
}

fn lower_class_members(
    context: &LoweringContext<'_>,
    body: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<(Vec<Property>, Vec<Box<Statement>>), LoweringDiagnostic> {
    let mut properties = Vec::new();
    let mut methods = Vec::new();

    for child in body.children() {
        if child.kind().is_token() {
            continue;
        }
        process_class_member(
            context,
            &child,
            diagnostics,
            &mut properties,
            &mut methods,
        )?;
    }

    Ok((properties, methods))
}

fn process_class_member(
    context: &LoweringContext<'_>,
    member: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
    properties: &mut Vec<Property>,
    methods: &mut Vec<Box<Statement>>,
) -> Result<(), LoweringDiagnostic> {
    match member.kind() {
        SyntaxKind::ValDeclaration => {
            if let Some(property) = lower_property(context, member, true, diagnostics)? {
                properties.push(property);
            }
        }
        SyntaxKind::VarDeclaration => {
            if let Some(property) = lower_property(context, member, false, diagnostics)? {
                properties.push(property);
            }
        }
        SyntaxKind::FunctionDeclaration => {
            let statement = lower_function(context, member, diagnostics)?;
            methods.push(Box::new(statement));
        }
        SyntaxKind::StatementList => {
            for stmt in member.children() {
                if stmt.kind().is_token() {
                    continue;
                }
                process_class_member(context, &stmt, diagnostics, properties, methods)?;
            }
        }
        other => {
            push_diagnostic(
                diagnostics,
                LoweringDiagnosticSeverity::Warning,
                format!(
                    "クラス内で未対応のノード {:?} を検出したためスキップしました",
                    other
                ),
                context,
                member,
            );
        }
    }
    Ok(())
}

fn lower_property(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    is_val: bool,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Option<Property>, LoweringDiagnostic> {
    let binding = match child_node(node, SyntaxKind::BindingPattern) {
        Some(binding) => binding,
        None => {
            push_diagnostic(
                diagnostics,
                LoweringDiagnosticSeverity::Error,
                "プロパティのバインディングが見つかりません",
                context,
                node,
            );
            return Ok(None);
        }
    };

    let span = context.span_for(node).unwrap_or_else(Span::dummy);
    let name = match context
        .tokens_for(&binding)
        .into_iter()
        .find_map(|token| match &token.token_type {
            TokenType::Identifier(text) => Some(text.clone()),
            _ => None,
        }) {
        Some(name) => name,
        None => {
            push_diagnostic(
                diagnostics,
                LoweringDiagnosticSeverity::Error,
                "プロパティ名を特定できませんでした",
                context,
                &binding,
            );
            return Ok(None);
        }
    };

    let type_annotation = child_node(node, SyntaxKind::TypeAnnotation)
        .and_then(|type_node| child_node(&type_node, SyntaxKind::Expression))
        .and_then(|expr| simple_type_from_expression(context, &expr));

    let initializer = match child_node(node, SyntaxKind::InitializerClause)
        .and_then(|clause| child_node(&clause, SyntaxKind::Expression))
    {
        Some(expr) => Some(lower_expression_shallow(context, &expr, diagnostics)?),
        None => None,
    };

    Ok(Some(Property {
        name,
        type_annotation,
        initializer,
        is_mutable: !is_val,
        modifiers: Modifiers::default(),
        getter: None,
        setter: None,
        span,
    }))
}

fn lower_for(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let binding_node = child_node(node, SyntaxKind::BindingPattern).ok_or_else(|| {
        missing_child_diagnostic(
            context,
            node,
            "for 文にはバインディングが必要です",
            SyntaxKind::BindingPattern,
        )
    })?;

    let binding_tokens = context.tokens_for(&binding_node);
    let binding_name = extract_identifier(&binding_tokens).ok_or_else(|| {
        LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "for 文のバインディング識別子が見つかりません",
            context.span_for(&binding_node),
            binding_node.kind(),
            first_identifier_text(&binding_node),
            collect_annotation_texts(&binding_node),
        )
    })?;

    let binding_span = context.span_for(&binding_node).unwrap_or_else(Span::dummy);
    let loop_binding = LoopBinding {
        name: binding_name,
        type_annotation: None,
        span: binding_span,
    };

    let iterable_expr = node
        .children()
        .find(|child| child.kind() == SyntaxKind::Expression)
        .map(|expr| lower_expression_shallow(context, &expr, diagnostics))
        .transpose()?
        .ok_or_else(|| {
            LoweringDiagnostic::new(
                LoweringDiagnosticSeverity::Error,
                "for 文のイテレータ式が見つかりません",
                context.span_for(node),
                node.kind(),
                first_identifier_text(node),
                collect_annotation_texts(node),
            )
        })?;

    let body_span = child_node(node, SyntaxKind::Block)
        .and_then(|block| context.span_for(&block))
        .unwrap_or_else(|| context.span_for(node).unwrap_or_else(Span::dummy));

    let body = child_node(node, SyntaxKind::Block)
        .map(|block| Expression::Block {
            statements: Vec::new(),
            span: context.span_for(&block).unwrap_or_else(Span::dummy),
        })
        .unwrap_or_else(|| {
            push_diagnostic(
                diagnostics,
                LoweringDiagnosticSeverity::Warning,
                "for 文の本体ブロックが存在しないため空ブロックとして処理します",
                context,
                node,
            );
            Expression::Block {
                statements: Vec::new(),
                span: body_span,
            }
        });

    Ok(Statement::ForIn(ForInStatement {
        binding: loop_binding,
        iterable: iterable_expr,
        strategy: LoopStrategy::Iterable,
        body: Box::new(body),
        span: context.span_for(node).unwrap_or_else(Span::dummy),
    }))
}

fn lower_return(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let value = child_node(node, SyntaxKind::Expression)
        .map(|expr| lower_expression_shallow(context, &expr, diagnostics))
        .transpose()?;

    Ok(Statement::Return {
        value,
        span: context.span_for(node).unwrap_or_else(Span::dummy),
    })
}

fn lower_throw(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let expr_node = child_node(node, SyntaxKind::Expression).ok_or_else(|| {
        missing_child_diagnostic(context, node, "throw 文には式が必要です", SyntaxKind::Expression)
    })?;

    let expr = lower_expression_shallow(context, &expr_node, diagnostics)?;
    Ok(Statement::Throw {
        expr,
        span: context.span_for(node).unwrap_or_else(Span::dummy),
    })
}

fn lower_break(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Result<Statement, LoweringDiagnostic> {
    Ok(Statement::Break(
        context.span_for(node).unwrap_or_else(Span::dummy),
    ))
}

fn lower_continue(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Result<Statement, LoweringDiagnostic> {
    Ok(Statement::Continue(
        context.span_for(node).unwrap_or_else(Span::dummy),
    ))
}

fn lower_expression_statement(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let expr = lower_expression_shallow(context, node, diagnostics)?;
    Ok(Statement::Expression {
        expr,
        span: context.span_for(node).unwrap_or_else(Span::dummy),
    })
}

fn extract_identifier(tokens: &[&Token]) -> Option<String> {
    tokens.iter().find_map(|token| match &token.token_type {
        TokenType::Identifier(text) => Some(text.clone()),
        _ => None,
    })
}

fn lower_parameters(
    context: &LoweringContext<'_>,
    list: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> (Vec<Parameter>, bool) {
    let mut params = Vec::new();
    let mut has_mutable = false;

    for child in list.children() {
        if child.kind() != SyntaxKind::FunctionParameter {
            continue;
        }

        let tokens = context.tokens_for(&child);
        if tokens
            .iter()
            .any(|token| matches!(token.token_type, TokenType::Var))
        {
            has_mutable = true;
        }

        let binding = child_node(&child, SyntaxKind::BindingPattern);
        let name = binding
            .as_ref()
            .and_then(|binding| {
                context.tokens_for(&binding).into_iter().find_map(|token| match &token.token_type {
                    TokenType::Identifier(text) => Some(text.clone()),
                    _ => None,
                })
            })
            .unwrap_or_else(|| {
                push_diagnostic(
                    diagnostics,
                    LoweringDiagnosticSeverity::Error,
                    "パラメータ名を特定できませんでした",
                    context,
                    &child,
                );
                String::new()
            });

        if name.is_empty() {
            continue;
        }

        let type_annotation = child_node(&child, SyntaxKind::TypeAnnotation)
            .and_then(|type_node| child_node(&type_node, SyntaxKind::Expression))
            .and_then(|expr| simple_type_from_expression(context, &expr));

        let span = context.span_for(&child).unwrap_or_else(Span::dummy);

        params.push(Parameter {
            name,
            type_annotation,
            default_value: None,
            span,
        });
    }

    (params, has_mutable)
}
