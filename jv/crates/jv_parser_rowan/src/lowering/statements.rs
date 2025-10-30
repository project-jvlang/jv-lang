use super::helpers::{
    collect_annotation_texts, first_identifier_text, JvSyntaxNode, LoweringContext,
};
use crate::support::{
    literals::regex_literal_from_token,
    spans::{expression_span, merge_spans, span_from_token},
};
use crate::syntax::SyntaxKind;
use jv_ast::comments::{CommentKind, CommentStatement, CommentVisibility};
use jv_ast::expression::{
    Argument, BinaryMetadata, CallArgumentMetadata, CallArgumentStyle, IsTestKind, IsTestMetadata,
    Parameter, ParameterModifiers, ParameterProperty, RegexGuardStrategy, StringPart, WhenArm,
};
use jv_ast::json::{
    JsonComment, JsonCommentKind, JsonEntry, JsonLiteral, JsonValue, NumberGrouping,
};
use jv_ast::statement::{
    ConcurrencyConstruct, ExtensionFunction, ForInStatement, LoopBinding, LoopStrategy,
    NumericRangeLoop, Property, ResourceManagement, ValBindingOrigin,
};
use jv_ast::strings::{MultilineKind, MultilineStringLiteral, RawStringFlavor};
use jv_ast::types::{
    BinaryOp, GenericParameter, GenericSignature, Literal, Modifiers, Pattern, RegexLiteral,
    TypeAnnotation, UnaryOp, VarianceMarker,
};
use jv_ast::{BindingPatternKind, Expression, SequenceDelimiter, Span, Statement};
use jv_lexer::{
    JsonCommentTrivia, JsonCommentTriviaKind, JsonConfidence, LayoutMode, Lexer,
    NumberGroupingKind, RawStringFlavor as LexerRawStringFlavor, StringDelimiterKind,
    StringInterpolationSegment, StringLiteralMetadata, Token, TokenMetadata, TokenTrivia,
    TokenType,
};
use jv_type_inference_java::{lower_type_annotation_from_tokens, TypeLoweringErrorKind};

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

    collect_statements_from_children(&context, root, &mut statements, &mut diagnostics);

    LoweringResult {
        statements,
        diagnostics,
    }
}

fn collect_statements_from_children(
    context: &LoweringContext<'_>,
    parent: &JvSyntaxNode,
    statements: &mut Vec<Statement>,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) {
    for child in parent.children() {
        if child.kind().is_token() {
            continue;
        }

        if child.kind() == SyntaxKind::StatementList {
            collect_statements_from_children(context, &child, statements, diagnostics);
            continue;
        }

        process_candidate(context, child, statements, diagnostics);
    }
}

fn is_top_level_statement(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::PackageDeclaration
            | SyntaxKind::ImportDeclaration
            | SyntaxKind::CommentStatement
            | SyntaxKind::ValDeclaration
            | SyntaxKind::VarDeclaration
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::WhenStatement
            | SyntaxKind::ForStatement
            | SyntaxKind::ReturnStatement
            | SyntaxKind::ThrowStatement
            | SyntaxKind::BreakStatement
            | SyntaxKind::ContinueStatement
            | SyntaxKind::UseStatement
            | SyntaxKind::DeferStatement
            | SyntaxKind::SpawnStatement
            | SyntaxKind::AssignmentStatement
            | SyntaxKind::Expression
    )
}

fn process_candidate(
    context: &LoweringContext<'_>,
    node: JvSyntaxNode,
    statements: &mut Vec<Statement>,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) {
    if node.kind().is_token() {
        return;
    }

    if matches!(node.kind(), SyntaxKind::Error | SyntaxKind::BlockError) {
        let diagnostic = LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "Rowanパーサでエラーノードが生成されました",
            context.span_for(&node),
            node.kind(),
            first_identifier_text(&node),
            collect_annotation_texts(&node),
        );
        diagnostics.push(diagnostic);
        return;
    }

    if !is_top_level_statement(node.kind()) {
        return;
    }

    match lower_single_statement(context, &node, diagnostics) {
        Ok(statement) => statements.push(statement),
        Err(diagnostic) => diagnostics.push(diagnostic),
    }
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
        SyntaxKind::CommentStatement => lower_comment(context, node),
        SyntaxKind::AssignmentStatement => lower_assignment(context, node, diagnostics),
        SyntaxKind::ValDeclaration => lower_value(context, node, true, diagnostics),
        SyntaxKind::VarDeclaration => lower_value(context, node, false, diagnostics),
        SyntaxKind::FunctionDeclaration => lower_function(context, node, diagnostics),
        SyntaxKind::ClassDeclaration => lower_class(context, node, diagnostics),
        SyntaxKind::ForStatement => lower_for(context, node, diagnostics),
        SyntaxKind::ReturnStatement => lower_return(context, node, diagnostics),
        SyntaxKind::ThrowStatement => lower_throw(context, node, diagnostics),
        SyntaxKind::BreakStatement => lower_break(context, node),
        SyntaxKind::ContinueStatement => lower_continue(context, node),
        SyntaxKind::UseStatement => lower_use(context, node, diagnostics),
        SyntaxKind::DeferStatement => lower_defer(context, node, diagnostics),
        SyntaxKind::SpawnStatement => lower_spawn(context, node, diagnostics),
        SyntaxKind::WhenStatement | SyntaxKind::Expression => {
            lower_expression_statement(context, node, diagnostics)
        }
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

    let node_tokens = context.tokens_for(node);
    let alias = import_alias_from_tokens(&node_tokens);
    let is_wildcard = node_tokens
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
        alias,
        is_wildcard,
        span: context.span_for(node).unwrap_or_else(jv_ast::Span::dummy),
    })
}

fn import_alias_from_tokens(tokens: &[&Token]) -> Option<String> {
    let mut awaiting_alias_name = false;
    let mut previous_was_dot = false;

    for token in tokens {
        match &token.token_type {
            TokenType::Whitespace(_) | TokenType::Newline => continue,
            TokenType::LineComment(_)
            | TokenType::BlockComment(_)
            | TokenType::JavaDocComment(_) => continue,
            TokenType::Dot => {
                previous_was_dot = true;
                if awaiting_alias_name {
                    awaiting_alias_name = false;
                }
            }
            TokenType::Identifier(value) => {
                if awaiting_alias_name {
                    return Some(value.clone());
                }

                if value == "as" && !previous_was_dot {
                    awaiting_alias_name = true;
                } else {
                    previous_was_dot = false;
                }
            }
            _ => {
                previous_was_dot = false;
                if awaiting_alias_name {
                    awaiting_alias_name = false;
                }
            }
        }
    }

    None
}

fn lower_comment(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Result<Statement, LoweringDiagnostic> {
    let tokens = context.tokens_for(node);
    let comment_token = tokens.first().ok_or_else(|| {
        LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "コメントノードからトークンを取得できませんでした",
            context.span_for(node),
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        )
    })?;

    let span = context.span_for(node).unwrap_or_else(Span::dummy);
    let statement = match &comment_token.token_type {
        TokenType::LineComment(raw) => {
            let visibility = if is_jv_only_line_comment(raw) {
                CommentVisibility::JvOnly
            } else {
                CommentVisibility::Passthrough
            };
            Statement::Comment(CommentStatement {
                kind: CommentKind::Line,
                visibility,
                text: render_line_comment(raw),
                span,
            })
        }
        TokenType::BlockComment(raw) => Statement::Comment(CommentStatement {
            kind: CommentKind::Block,
            visibility: CommentVisibility::Passthrough,
            text: format!("/*{}*/", raw),
            span,
        }),
        other => {
            return Err(LoweringDiagnostic::new(
                LoweringDiagnosticSeverity::Error,
                format!(
                    "コメントローワリングで未対応のトークン {:?} を検出しました",
                    other
                ),
                Some(span),
                node.kind(),
                None,
                Vec::new(),
            ))
        }
    };

    Ok(statement)
}

fn render_line_comment(raw: &str) -> String {
    if raw.starts_with("/*") {
        format!("//{}", &raw[1..])
    } else if raw.starts_with('/') {
        raw.to_string()
    } else {
        format!("/{}", raw)
    }
}

fn is_jv_only_line_comment(raw: &str) -> bool {
    let trimmed = raw.trim_start();
    trimmed.starts_with("///") || trimmed.starts_with("//*") || raw.contains("*//")
}

fn lower_assignment(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let target_node = child_node(node, SyntaxKind::AssignmentTarget).ok_or_else(|| {
        missing_child_diagnostic(
            context,
            node,
            "代入ターゲットを解析できませんでした",
            SyntaxKind::AssignmentTarget,
        )
    })?;

    let (target, binding_pattern) =
        if let Some(pattern_node) = child_node(&target_node, SyntaxKind::BindingPattern) {
            let pattern = lower_binding_pattern(context, &pattern_node)?;
            let expr = binding_pattern_primary_expression(&pattern);
            (expr, Some(pattern))
        } else {
            (lower_assignment_target(context, &target_node)?, None)
        };

    let value_node = node
        .children()
        .find(|child| child.kind() == SyntaxKind::Expression)
        .ok_or_else(|| {
            missing_child_diagnostic(
                context,
                node,
                "代入式の右辺が見つかりません",
                SyntaxKind::Expression,
            )
        })?;

    let value = lower_expression(context, &value_node)?;
    let span = context.span_for(node).unwrap_or_else(Span::dummy);

    if let Some(pattern) = binding_pattern.clone() {
        if let Some(type_node) = child_node(&target_node, SyntaxKind::TypeAnnotation) {
            if let Some(annotation) =
                lower_type_annotation_container(context, &type_node, node, diagnostics)
            {
                if let Some(name) = pattern.first_identifier() {
                    let modifiers = Modifiers::default();
                    return Ok(Statement::ValDeclaration {
                        name: name.to_string(),
                        binding: Some(pattern),
                        type_annotation: Some(annotation),
                        initializer: value,
                        modifiers,
                        origin: ValBindingOrigin::ImplicitTyped,
                        span,
                    });
                }
            }
        }
    }

    Ok(Statement::Assignment {
        target,
        binding_pattern,
        value,
        span,
    })
}

fn lower_use(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let resource_node = child_node(node, SyntaxKind::Expression).ok_or_else(|| {
        missing_child_diagnostic(
            context,
            node,
            "`use` 文のリソース式が必要です",
            SyntaxKind::Expression,
        )
    })?;

    let resource = lower_expression(context, &resource_node)?;

    let block_node = child_node(node, SyntaxKind::Block).ok_or_else(|| {
        missing_child_diagnostic(
            context,
            node,
            "`use` 文にはブロックが必要です",
            SyntaxKind::Block,
        )
    })?;

    let body = lower_block_expression(context, &block_node, diagnostics);
    let resource_span = expression_span(&resource);
    let body_span = expression_span(&body);
    let span = merge_spans(&resource_span, &body_span);

    Ok(Statement::ResourceManagement(ResourceManagement::Use {
        resource: Box::new(resource),
        body: Box::new(body),
        span,
    }))
}

fn lower_defer(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let block_node = child_node(node, SyntaxKind::Block).ok_or_else(|| {
        missing_child_diagnostic(
            context,
            node,
            "`defer` 文にはブロックが必要です",
            SyntaxKind::Block,
        )
    })?;

    let body = lower_block_expression(context, &block_node, diagnostics);
    let span = expression_span(&body);

    Ok(Statement::ResourceManagement(ResourceManagement::Defer {
        body: Box::new(body),
        span,
    }))
}

fn lower_spawn(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let block_node = child_node(node, SyntaxKind::Block).ok_or_else(|| {
        missing_child_diagnostic(
            context,
            node,
            "`spawn` 文にはブロックが必要です",
            SyntaxKind::Block,
        )
    })?;

    let body = lower_block_expression(context, &block_node, diagnostics);
    let span = expression_span(&body);

    Ok(Statement::Concurrency(ConcurrencyConstruct::Spawn {
        body: Box::new(body),
        span,
    }))
}

fn lower_assignment_target(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Result<Expression, LoweringDiagnostic> {
    let tokens = context.tokens_for(node);
    let filtered = tokens
        .iter()
        .copied()
        .filter(|token| !is_trivia_token(token))
        .collect::<Vec<_>>();

    let mut iter = filtered.into_iter();
    let first = iter.next().ok_or_else(|| {
        LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "代入ターゲットが空です",
            context.span_for(node),
            node.kind(),
            None,
            Vec::new(),
        )
    })?;

    let (base_name, base_span) = match &first.token_type {
        TokenType::Identifier(text) => (text.clone(), span_from_token(first)),
        other => {
            return Err(LoweringDiagnostic::new(
                LoweringDiagnosticSeverity::Error,
                format!(
                    "代入ターゲットは識別子で始まる必要がありますが {:?} が見つかりました",
                    other
                ),
                context.span_for(node),
                node.kind(),
                None,
                Vec::new(),
            ))
        }
    };

    let mut expr = Expression::Identifier(base_name, base_span.clone());

    while let Some(token) = iter.next() {
        match &token.token_type {
            TokenType::Dot => {
                let property_token = iter.next().ok_or_else(|| {
                    LoweringDiagnostic::new(
                        LoweringDiagnosticSeverity::Error,
                        "`.` の後には識別子が必要です",
                        context.span_for(node),
                        node.kind(),
                        None,
                        Vec::new(),
                    )
                })?;

                let property_name = match &property_token.token_type {
                    TokenType::Identifier(name) => name.clone(),
                    other => {
                        return Err(LoweringDiagnostic::new(
                            LoweringDiagnosticSeverity::Error,
                            format!(
                                "`.` の後には識別子が必要ですが {:?} が見つかりました",
                                other
                            ),
                            context.span_for(node),
                            node.kind(),
                            None,
                            Vec::new(),
                        ))
                    }
                };

                let previous_span = expression_span(&expr);
                let property_span = span_from_token(property_token);
                let span = merge_spans(&previous_span, &property_span);
                expr = Expression::MemberAccess {
                    object: Box::new(expr),
                    property: property_name,
                    span,
                };
            }
            other => {
                return Err(LoweringDiagnostic::new(
                    LoweringDiagnosticSeverity::Error,
                    format!(
                        "代入ターゲットで想定外のトークン {:?} を検出しました",
                        other
                    ),
                    context.span_for(node),
                    node.kind(),
                    None,
                    Vec::new(),
                ))
            }
        }
    }

    Ok(expr)
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

    let binding_pattern = lower_binding_pattern(context, &binding)?;
    let name = binding_pattern
        .first_identifier()
        .map(|text| text.to_string())
        .unwrap_or_else(|| join_tokens(&context.tokens_for(&binding)));

    let type_annotation = child_node(node, SyntaxKind::TypeAnnotation).and_then(|type_node| {
        lower_type_annotation_container(context, &type_node, node, diagnostics)
    });

    let initializer_clause = child_node(node, SyntaxKind::InitializerClause);

    let initializer = match initializer_clause {
        Some(clause) => match child_node(&clause, SyntaxKind::Expression) {
            Some(expr_node) => Some(lower_expression(context, &expr_node)?),
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
            binding: Some(binding_pattern.clone()),
            type_annotation,
            initializer: initializer.unwrap(),
            modifiers,
            origin: ValBindingOrigin::ExplicitKeyword,
            span,
        })
    } else {
        Ok(Statement::VarDeclaration {
            name,
            binding: Some(binding_pattern),
            type_annotation,
            initializer,
            modifiers,
            span,
        })
    }
}

fn lower_type_annotation_container(
    context: &LoweringContext<'_>,
    container_node: &JvSyntaxNode,
    owner: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Option<TypeAnnotation> {
    let expr = match child_node(container_node, SyntaxKind::Expression) {
        Some(expr) => expr,
        None => {
            diagnostics.push(LoweringDiagnostic::new(
                LoweringDiagnosticSeverity::Error,
                "型注釈の式が見つかりません",
                context.span_for(container_node),
                container_node.kind(),
                first_identifier_text(owner),
                collect_annotation_texts(owner),
            ));
            return None;
        }
    };

    let tokens = context.tokens_for(&expr);
    let owned_tokens: Vec<Token> = tokens.into_iter().cloned().collect();

    match lower_type_annotation_from_tokens(&owned_tokens) {
        Ok(lowered) => Some(lowered.into_annotation()),
        Err(error) => {
            let span = error
                .span()
                .cloned()
                .or_else(|| context.span_for(&expr))
                .or_else(|| context.span_for(container_node))
                .or_else(|| context.span_for(owner));
            diagnostics.push(LoweringDiagnostic::new(
                LoweringDiagnosticSeverity::Error,
                error.message().to_string(),
                span,
                container_node.kind(),
                first_identifier_text(owner),
                collect_annotation_texts(owner),
            ));
            None
        }
    }
}

fn lower_block_expression(
    context: &LoweringContext<'_>,
    block: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Expression {
    let span = context.span_for(block).unwrap_or_else(Span::dummy);
    let mut statements = Vec::new();
    collect_statements_from_children(context, block, &mut statements, diagnostics);
    Expression::Block { statements, span }
}

fn lower_expression(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Result<Expression, LoweringDiagnostic> {
    let tokens = context.tokens_for(node);
    lower_expression_from_tokens(context, node, tokens)
}

fn lower_expression_from_tokens(
    context: &LoweringContext<'_>,
    span_node: &JvSyntaxNode,
    tokens: Vec<&Token>,
) -> Result<Expression, LoweringDiagnostic> {
    if tokens.is_empty() {
        return Err(LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "式が空です",
            context.span_for(span_node),
            span_node.kind(),
            first_identifier_text(span_node),
            collect_annotation_texts(span_node),
        ));
    }

    let filtered: Vec<&Token> = tokens
        .iter()
        .copied()
        .filter(|token| !is_trivia_token(token) && !matches!(token.token_type, TokenType::Eof))
        .collect();

    if filtered.is_empty() {
        return Err(LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "式が解析対象のトークンを含みません",
            context.span_for(span_node),
            span_node.kind(),
            first_identifier_text(span_node),
            collect_annotation_texts(span_node),
        ));
    }

    match expression_parser::parse_expression(&filtered) {
        Ok(expr) => Ok(expr),
        Err(err) => {
            let expression_parser::ExpressionError { message, span } = err;
            if message.contains("JV2101") {
                let span = span
                    .or_else(|| context.span_for(span_node))
                    .unwrap_or_else(Span::dummy);
                Err(LoweringDiagnostic::new(
                    LoweringDiagnosticSeverity::Error,
                    message,
                    Some(span),
                    span_node.kind(),
                    first_identifier_text(span_node),
                    collect_annotation_texts(span_node),
                ))
            } else {
                let span = span
                    .or_else(|| context.span_for(span_node))
                    .unwrap_or_else(Span::dummy);
                let fallback = Expression::Identifier(join_tokens(&filtered), span);
                Ok(fallback)
            }
        }
    }
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

fn token_requires_followup(token_type: &TokenType) -> bool {
    matches!(
        token_type,
        TokenType::Plus
            | TokenType::Minus
            | TokenType::Multiply
            | TokenType::Divide
            | TokenType::Modulo
            | TokenType::Equal
            | TokenType::NotEqual
            | TokenType::Less
            | TokenType::LessEqual
            | TokenType::And
            | TokenType::Or
            | TokenType::RangeExclusive
            | TokenType::RangeInclusive
            | TokenType::Elvis
            | TokenType::Assign
            | TokenType::Dot
            | TokenType::NullSafe
            | TokenType::DoubleColon
            | TokenType::Comma
            | TokenType::LayoutComma
            | TokenType::LeftParen
            | TokenType::LeftBracket
            | TokenType::LeftBrace
            | TokenType::Arrow
            | TokenType::FatArrow
            | TokenType::Colon
            | TokenType::Semicolon
            | TokenType::At
            | TokenType::Question
    )
}

fn is_trivia_token(token: &Token) -> bool {
    matches!(
        token.token_type,
        TokenType::Whitespace(_)
            | TokenType::Newline
            | TokenType::LineComment(_)
            | TokenType::BlockComment(_)
            | TokenType::JavaDocComment(_)
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_lexer::Lexer;

    fn parse_expression_from_source(source: &str) -> Expression {
        let mut lexer = Lexer::with_layout_mode(source.to_string(), LayoutMode::Enabled);
        let tokens = lexer.tokenize().expect("tokenize expression");
        let filtered: Vec<&Token> = tokens
            .iter()
            .filter(|token| !matches!(token.token_type, TokenType::Eof))
            .collect();
        expression_parser::parse_expression(&filtered).expect("parse expression")
    }

    #[test]
    fn call_with_whitespace_arguments_parses() {
        let expr = parse_expression_from_source("operation(accumulator value)");
        match expr {
            Expression::Call { args, .. } => {
                assert_eq!(args.len(), 2, "expected two positional arguments");
            }
            other => panic!("expected call expression, got {:?}", other),
        }
    }

    #[test]
    fn parses_as_type_cast_expression() {
        let expr = parse_expression_from_source("value as Character");
        match expr {
            Expression::TypeCast { .. } => {}
            other => panic!("expected type cast expression, got {:?}", other),
        }
    }

    #[test]
    fn call_arguments_split_across_newlines_parse() {
        let expr = parse_expression_from_source("operation(\n    accumulator\n    value\n)");
        match expr {
            Expression::Call { args, .. } => assert_eq!(args.len(), 2),
            other => panic!("expected call expression, got {:?}", other),
        }
    }

    #[test]
    fn value_is_type_test_parses() {
        let expr = parse_expression_from_source("value is Character");
        match expr {
            Expression::Binary { op, metadata, .. }
                if matches!(op, BinaryOp::Is)
                    && matches!(
                        metadata.is_test.as_ref().map(|m| &m.kind),
                        Some(IsTestKind::Type)
                    ) => {}
            other => panic!("expected `is` binary expression, got {:?}", other),
        }
    }

    #[test]
    fn regex_literal_is_expression_carries_metadata() {
        let expr = parse_expression_from_source("text is /[a-z]+/");
        match expr {
            Expression::Binary { metadata, .. } => {
                let Some(is_test) = metadata.is_test else {
                    panic!("expected is-test metadata for regex literal");
                };
                assert!(matches!(is_test.kind, IsTestKind::RegexLiteral));
                assert!(
                    is_test.regex.is_some(),
                    "正規表現リテラル情報が欠落しています"
                );
                assert!(is_test.pattern_expr.is_none());
                assert!(matches!(is_test.guard_strategy, RegexGuardStrategy::None));
            }
            other => panic!("expected binary expression, got {:?}", other),
        }
    }

    #[test]
    fn pattern_expression_is_expression_carries_metadata() {
        let expr = parse_expression_from_source("subject is provider().pattern()");
        match expr {
            Expression::Binary { metadata, .. } => {
                let Some(is_test) = metadata.is_test else {
                    panic!("expected is-test metadata for pattern expression");
                };
                assert!(matches!(is_test.kind, IsTestKind::PatternExpression));
                assert!(is_test.regex.is_none());
                assert!(
                    is_test.pattern_expr.is_some(),
                    "パターン式が保存されていません"
                );
            }
            other => panic!("expected binary expression, got {:?}", other),
        }
    }

    #[test]
    fn stream_fold_lambda_parses() {
        let source = r#"this.forEach { value ->
    accumulator = operation(
        accumulator
        value
    )
}"#;
        let expr = parse_expression_from_source(source);
        match expr {
            Expression::Call { args, .. } => {
                assert_eq!(args.len(), 1);
            }
            other => panic!(
                "expected call expression with lambda argument, got {:?}",
                other
            ),
        }
    }
}

mod expression_parser {
    use super::*;

    #[derive(Debug)]
    #[allow(dead_code)]
    pub(super) struct ExpressionError {
        pub message: String,
        pub span: Option<Span>,
    }

    impl ExpressionError {
        fn new(message: impl Into<String>, span: Option<Span>) -> Self {
            Self {
                message: message.into(),
                span,
            }
        }
    }

    pub(super) fn parse_expression(tokens: &[&Token]) -> Result<Expression, ExpressionError> {
        let mut parser = ExpressionParser::new(tokens);
        let parsed = parser.parse_expression_bp(0)?;
        parser.consume_postfix_if_any(parsed)
    }

    struct ExpressionParser<'a> {
        tokens: &'a [&'a Token],
        pos: usize,
        pending_type_arguments: Option<Vec<TypeAnnotation>>,
    }

    #[derive(Clone)]
    struct ParsedExpr {
        expr: Expression,
        start: usize,
        end: usize,
    }

    impl ParsedExpr {
        fn span(&self) -> Span {
            expression_span(&self.expr)
        }
    }

    #[derive(Clone, Copy)]
    enum Associativity {
        Left,
    }

    struct BinaryInfo {
        op: BinaryOp,
        precedence: u8,
        associativity: Associativity,
    }

    fn has_layout_break(token: &Token) -> bool {
        token.leading_trivia.newlines > 0
    }

    #[derive(Clone, Copy)]
    enum SeparatorKind {
        Comma,
        Layout,
    }

    impl<'a> ExpressionParser<'a> {
        fn new(tokens: &'a [&'a Token]) -> Self {
            Self {
                tokens,
                pos: 0,
                pending_type_arguments: None,
            }
        }

        fn parse_expression_bp(&mut self, min_prec: u8) -> Result<ParsedExpr, ExpressionError> {
            let mut left = self.parse_prefix()?;

            loop {
                if let Some(kind) = self.peek_postfix_kind() {
                    left = self.parse_postfix(left, kind)?;
                    continue;
                }

                if self.peek_is_as_keyword() {
                    left = self.parse_type_cast(left)?;
                    continue;
                }

                let Some(info) = self.peek_binary_info() else {
                    break;
                };

                if info.precedence < min_prec {
                    break;
                }

                let (op_token, _op_index) =
                    self.advance_with_index_or_error("演算子を取得できませんでした")?;
                let op_span = span_from_token(op_token);

                let next_min_prec = if matches!(info.associativity, Associativity::Left) {
                    info.precedence + 1
                } else {
                    info.precedence
                };

                let right = self.parse_expression_bp(next_min_prec)?;
                left = self.make_binary(left, right, info.op, op_span)?;
            }

            Ok(left)
        }

        fn consume_postfix_if_any(
            &mut self,
            mut expr: ParsedExpr,
        ) -> Result<Expression, ExpressionError> {
            while let Some(kind) = self.peek_postfix_kind() {
                expr = self.parse_postfix(expr, kind)?;
            }

            if self.peek_token().is_some() {
                let span = self.span_at(self.pos);
                return Err(ExpressionError::new(
                    "式の末尾に解釈できないトークンがあります",
                    span,
                ));
            }

            Ok(expr.expr)
        }

        fn parse_prefix(&mut self) -> Result<ParsedExpr, ExpressionError> {
            let Some((token, index)) = self.peek_with_index() else {
                return Err(ExpressionError::new("式が必要です", None));
            };

            match &token.token_type {
                TokenType::Plus => {
                    let op_span = span_from_token(token);
                    self.pos += 1;
                    let operand = self.parse_expression_bp(PREFIX_PRECEDENCE)?;
                    let span = merge_spans(&op_span, &operand.span());
                    let expr = Expression::Unary {
                        op: UnaryOp::Plus,
                        operand: Box::new(operand.expr),
                        span: span.clone(),
                    };
                    Ok(ParsedExpr {
                        expr,
                        start: index,
                        end: operand.end,
                    })
                }
                TokenType::Minus => {
                    let op_span = span_from_token(token);
                    self.pos += 1;
                    let operand = self.parse_expression_bp(PREFIX_PRECEDENCE)?;
                    let span = merge_spans(&op_span, &operand.span());
                    let expr = Expression::Unary {
                        op: UnaryOp::Minus,
                        operand: Box::new(operand.expr),
                        span: span.clone(),
                    };
                    Ok(ParsedExpr {
                        expr,
                        start: index,
                        end: operand.end,
                    })
                }
                TokenType::Not => {
                    let op_span = span_from_token(token);
                    self.pos += 1;
                    let operand = self.parse_expression_bp(PREFIX_PRECEDENCE)?;
                    let span = merge_spans(&op_span, &operand.span());
                    let expr = Expression::Unary {
                        op: UnaryOp::Not,
                        operand: Box::new(operand.expr),
                        span: span.clone(),
                    };
                    Ok(ParsedExpr {
                        expr,
                        start: index,
                        end: operand.end,
                    })
                }
                TokenType::LeftBrace => {
                    if has_high_json_confidence(token) {
                        self.parse_json_object_expression(index)
                    } else {
                        self.parse_brace_expression()
                    }
                }
                TokenType::LeftBracket => self.parse_array_expression(index),
                TokenType::When => self.parse_when_expression(),
                _ => self.parse_primary(),
            }
        }

        fn parse_primary(&mut self) -> Result<ParsedExpr, ExpressionError> {
            let Some((token, index)) = self.advance_with_index() else {
                return Err(ExpressionError::new("式が必要です", None));
            };

            match &token.token_type {
                TokenType::Number(value) => {
                    let span = span_from_token(token);
                    Ok(ParsedExpr {
                        expr: Expression::Literal(Literal::Number(value.clone()), span.clone()),
                        start: index,
                        end: index + 1,
                    })
                }
                TokenType::Boolean(value) => {
                    let span = span_from_token(token);
                    Ok(ParsedExpr {
                        expr: Expression::Literal(Literal::Boolean(*value), span.clone()),
                        start: index,
                        end: index + 1,
                    })
                }
                TokenType::True => {
                    let span = span_from_token(token);
                    Ok(ParsedExpr {
                        expr: Expression::Literal(Literal::Boolean(true), span.clone()),
                        start: index,
                        end: index + 1,
                    })
                }
                TokenType::False => {
                    let span = span_from_token(token);
                    Ok(ParsedExpr {
                        expr: Expression::Literal(Literal::Boolean(false), span.clone()),
                        start: index,
                        end: index + 1,
                    })
                }
                TokenType::String(_) | TokenType::StringInterpolation(_) => {
                    let span = span_from_token(token);
                    let has_interpolation = token.metadata.iter().any(|metadata| {
                        matches!(metadata, TokenMetadata::StringInterpolation { .. })
                    });

                    let expr = if has_interpolation {
                        let (expr, _) = parse_string_token_with_metadata(token, span.clone())?;
                        expr
                    } else {
                        let value = match &token.token_type {
                            TokenType::String(value) | TokenType::StringInterpolation(value) => {
                                value.clone()
                            }
                            _ => token.lexeme.clone(),
                        };
                        Expression::Literal(Literal::String(value), span.clone())
                    };

                    Ok(ParsedExpr {
                        expr,
                        start: index,
                        end: index + 1,
                    })
                }
                TokenType::Character(value) => {
                    let span = span_from_token(token);
                    Ok(ParsedExpr {
                        expr: Expression::Literal(Literal::Character(*value), span.clone()),
                        start: index,
                        end: index + 1,
                    })
                }
                TokenType::StringStart => {
                    return self.parse_string_segments(index);
                }
                TokenType::Null => {
                    let span = span_from_token(token);
                    Ok(ParsedExpr {
                        expr: Expression::Literal(Literal::Null, span.clone()),
                        start: index,
                        end: index + 1,
                    })
                }
                TokenType::Identifier(name) => {
                    let span = span_from_token(token);
                    Ok(ParsedExpr {
                        expr: Expression::Identifier(name.clone(), span.clone()),
                        start: index,
                        end: index + 1,
                    })
                }
                TokenType::LeftParen => {
                    let close_index = self.find_matching_paren(index).ok_or_else(|| {
                        ExpressionError::new("')' が必要です", self.span_at(index))
                    })?;
                    let inner_tokens = &self.tokens[index + 1..close_index];
                    let expr = Self::parse_nested_expression(inner_tokens)?;
                    self.pos = close_index + 1;
                    Ok(ParsedExpr {
                        expr: expr.expr,
                        start: index,
                        end: close_index + 1,
                    })
                }
                TokenType::LeftBrace => unreachable!(
                    "LeftBrace should be handled in parse_prefix via parse_brace_expression"
                ),
                other => Err(ExpressionError::new(
                    format!("未対応のトークン {:?} が式中に存在します", other),
                    Some(span_from_index(self.tokens, index)),
                )),
            }
        }

        fn parse_json_object_expression(
            &mut self,
            start_index: usize,
        ) -> Result<ParsedExpr, ExpressionError> {
            let closing_index = self
                .find_matching_brace(start_index)
                .ok_or_else(|| ExpressionError::new("'}' が必要です", self.span_at(start_index)))?;
            let slice = &self.tokens[start_index..=closing_index];
            let json = parse_json_literal_tokens(slice)?;
            self.pos = closing_index + 1;
            Ok(ParsedExpr {
                expr: Expression::JsonLiteral(json),
                start: start_index,
                end: closing_index + 1,
            })
        }

        fn parse_array_expression(
            &mut self,
            start_index: usize,
        ) -> Result<ParsedExpr, ExpressionError> {
            let closing_index = self
                .find_matching_bracket(start_index)
                .ok_or_else(|| ExpressionError::new("']' が必要です", self.span_at(start_index)))?;
            let inner = &self.tokens[start_index + 1..closing_index];
            let span = span_for_range(self.tokens, start_index, closing_index + 1);

            if inner.is_empty()
                || inner.iter().all(|token| {
                    is_trivia_token(token) || matches!(token.token_type, TokenType::LayoutComma)
                })
            {
                self.pos = closing_index + 1;
                let expr = Expression::Array {
                    elements: Vec::new(),
                    delimiter: SequenceDelimiter::Comma,
                    span: span.clone(),
                };
                return Ok(ParsedExpr {
                    expr,
                    start: start_index,
                    end: closing_index + 1,
                });
            }

            let mut parts: Vec<&[&Token]> = Vec::new();
            let mut separators: Vec<SeparatorKind> = Vec::new();
            let mut start = 0usize;
            let mut depth_paren = 0usize;
            let mut depth_brace = 0usize;
            let mut depth_bracket = 0usize;

            for (offset, token) in inner.iter().enumerate() {
                let at_top_level = depth_paren == 0 && depth_brace == 0 && depth_bracket == 0;

                if at_top_level
                    && has_layout_break(token)
                    && !matches!(token.token_type, TokenType::Comma | TokenType::LayoutComma)
                    && start < offset
                {
                    parts.push(&inner[start..offset]);
                    separators.push(SeparatorKind::Layout);
                    start = offset;
                }

                if at_top_level
                    && start < offset
                    && token.leading_trivia.newlines == 0
                    && token.leading_trivia.spaces > 0
                {
                    if let Some(prev_token) = inner.get(offset - 1) {
                        if !token_requires_followup(&prev_token.token_type) {
                            parts.push(&inner[start..offset]);
                            separators.push(SeparatorKind::Layout);
                            start = offset;
                        }
                    }
                }

                match token.token_type {
                    TokenType::LeftParen => depth_paren += 1,
                    TokenType::RightParen if depth_paren > 0 => depth_paren -= 1,
                    TokenType::LeftBrace => depth_brace += 1,
                    TokenType::RightBrace if depth_brace > 0 => depth_brace -= 1,
                    TokenType::LeftBracket => depth_bracket += 1,
                    TokenType::RightBracket if depth_bracket > 0 => depth_bracket -= 1,
                    TokenType::Comma if at_top_level => {
                        parts.push(&inner[start..offset]);
                        separators.push(SeparatorKind::Comma);
                        start = offset + 1;
                        continue;
                    }
                    TokenType::LayoutComma if at_top_level => {
                        parts.push(&inner[start..offset]);
                        separators.push(SeparatorKind::Layout);
                        start = offset + 1;
                        continue;
                    }
                    _ => {}
                }
            }

            let mut trailing_separator: Option<SeparatorKind> = None;
            if start < inner.len() {
                parts.push(&inner[start..]);
            } else if start == inner.len() && !separators.is_empty() {
                trailing_separator = separators.pop();
            }

            if !parts.is_empty() && parts.len() != separators.len() + 1 {
                return Err(ExpressionError::new(
                    "配列リテラルの要素区切りが不正です",
                    Some(span.clone()),
                ));
            }

            let mut elements = Vec::with_capacity(parts.len());
            let mut saw_layout = false;
            let mut saw_comma = false;

            for (idx, part) in parts.iter().enumerate() {
                let filtered: Vec<&Token> = part
                    .iter()
                    .copied()
                    .filter(|token| {
                        !is_trivia_token(token)
                            && !matches!(token.token_type, TokenType::LayoutComma)
                    })
                    .collect();

                if filtered.is_empty() {
                    return Err(ExpressionError::new(
                        array_comma_error_message(),
                        Some(span.clone()),
                    ));
                }

                let parsed = Self::parse_nested_expression(filtered.as_slice())?;
                elements.push(parsed.expr);

                if let Some(separator) = separators.get(idx) {
                    match separator {
                        SeparatorKind::Comma => saw_comma = true,
                        SeparatorKind::Layout => saw_layout = true,
                    }
                }
            }

            if let Some(separator) = trailing_separator {
                match separator {
                    SeparatorKind::Comma => saw_comma = true,
                    SeparatorKind::Layout => saw_layout = true,
                }
            }

            if saw_comma {
                return Err(ExpressionError::new(
                    array_comma_error_message(),
                    Some(span),
                ));
            }

            let delimiter = if saw_layout {
                SequenceDelimiter::Whitespace
            } else {
                SequenceDelimiter::Comma
            };

            self.pos = closing_index + 1;
            Ok(ParsedExpr {
                expr: Expression::Array {
                    elements,
                    delimiter,
                    span: span.clone(),
                },
                start: start_index,
                end: closing_index + 1,
            })
        }

        fn parse_brace_expression(&mut self) -> Result<ParsedExpr, ExpressionError> {
            let start_index = self.pos;
            self.pos += 1; // consume '{'

            let closing_index = self
                .find_matching_brace(start_index)
                .ok_or_else(|| ExpressionError::new("'}' が必要です", self.span_at(start_index)))?;

            let inner = &self.tokens[start_index + 1..closing_index];
            let arrow_index = Self::find_top_level_arrow(inner);

            let span = span_for_range(self.tokens, start_index, closing_index + 1);

            let parsed = if let Some(relative_arrow) = arrow_index {
                let arrow_absolute = start_index + 1 + relative_arrow;
                let parameter_tokens = &self.tokens[start_index + 1..arrow_absolute];
                let body_tokens = &self.tokens[arrow_absolute + 1..closing_index];

                let parameters = self.parse_lambda_parameters(parameter_tokens)?;

                let body_expr = if body_tokens.is_empty() {
                    Expression::Block {
                        statements: Vec::new(),
                        span: span.clone(),
                    }
                } else {
                    match Self::parse_nested_expression(body_tokens) {
                        Ok(expr) => expr.expr,
                        Err(_) => {
                            self.parse_lambda_body_as_block(body_tokens, arrow_absolute + 1)?
                        }
                    }
                };

                ParsedExpr {
                    expr: Expression::Lambda {
                        parameters,
                        body: Box::new(body_expr),
                        span: span.clone(),
                    },
                    start: start_index,
                    end: closing_index + 1,
                }
            } else {
                let body_expr = if inner.is_empty() {
                    Expression::Block {
                        statements: Vec::new(),
                        span: span.clone(),
                    }
                } else {
                    match Self::parse_nested_expression(inner) {
                        Ok(expr) => expr.expr,
                        Err(_) => self.parse_lambda_body_as_block(inner, start_index + 1)?,
                    }
                };

                ParsedExpr {
                    expr: Expression::Lambda {
                        parameters: Vec::new(),
                        body: Box::new(body_expr),
                        span: span.clone(),
                    },
                    start: start_index,
                    end: closing_index + 1,
                }
            };

            self.pos = closing_index + 1;
            Ok(parsed)
        }

        fn parse_lambda_body_as_block(
            &self,
            body_tokens: &[&'a Token],
            body_start: usize,
        ) -> Result<Expression, ExpressionError> {
            let mut statements = Vec::new();
            let ranges = Self::split_lambda_body_statements(body_tokens);
            for (rel_start, rel_end) in ranges {
                if rel_start >= rel_end {
                    continue;
                }
                let slice = &body_tokens[rel_start..rel_end];
                let absolute_start = body_start + rel_start;
                let statement = self.parse_lambda_statement(slice, absolute_start)?;
                statements.push(statement);
            }

            let span = span_for_range(self.tokens, body_start, body_start + body_tokens.len());
            Ok(Expression::Block { statements, span })
        }

        fn split_lambda_body_statements(tokens: &[&Token]) -> Vec<(usize, usize)> {
            let mut ranges = Vec::new();
            let mut start = 0usize;
            let mut depth_paren = 0isize;
            let mut depth_brace = 0isize;
            let mut depth_bracket = 0isize;

            for (idx, token) in tokens.iter().enumerate() {
                match token.token_type {
                    TokenType::LeftParen => depth_paren += 1,
                    TokenType::RightParen => depth_paren = depth_paren.saturating_sub(1),
                    TokenType::LeftBrace => depth_brace += 1,
                    TokenType::RightBrace => depth_brace = depth_brace.saturating_sub(1),
                    TokenType::LeftBracket => depth_bracket += 1,
                    TokenType::RightBracket => depth_bracket = depth_bracket.saturating_sub(1),
                    _ => {}
                }

                if idx + 1 < tokens.len()
                    && depth_paren == 0
                    && depth_brace == 0
                    && depth_bracket == 0
                {
                    let next = tokens[idx + 1];
                    if next.leading_trivia.newlines > 0
                        && !matches!(
                            next.token_type,
                            TokenType::RightParen | TokenType::RightBrace | TokenType::RightBracket
                        )
                    {
                        ranges.push((start, idx + 1));
                        start = idx + 1;
                    }
                }
            }

            if start < tokens.len() {
                ranges.push((start, tokens.len()));
            }

            ranges
                .into_iter()
                .filter(|(range_start, range_end)| {
                    let slice = &tokens[*range_start..*range_end];
                    !slice.is_empty()
                        && !slice
                            .iter()
                            .all(|token| matches!(token.token_type, TokenType::StringEnd))
                })
                .collect()
        }

        fn parse_lambda_statement(
            &self,
            slice: &[&'a Token],
            absolute_start: usize,
        ) -> Result<Statement, ExpressionError> {
            if slice.is_empty() {
                return Err(ExpressionError::new("ラムダのステートメントが空です", None));
            }

            let absolute_end = absolute_start + slice.len();
            let span = span_for_range(self.tokens, absolute_start, absolute_end);
            let first = slice[0];

            match first.token_type {
                TokenType::Val => self.parse_val_statement(slice, absolute_start),
                TokenType::Return => {
                    let expr_slice = &slice[1..];
                    let value = if expr_slice.is_empty() {
                        None
                    } else {
                        Some(Self::parse_nested_expression(expr_slice)?.expr)
                    };
                    Ok(Statement::Return { value, span })
                }
                _ => {
                    if let Some(assign_index) = Self::find_top_level_assign(slice) {
                        let target_tokens = &slice[..assign_index];
                        let value_tokens = &slice[assign_index + 1..];
                        if target_tokens.is_empty() || value_tokens.is_empty() {
                            return Err(ExpressionError::new(
                                "代入ステートメントの構文が不正です",
                                Some(span.clone()),
                            ));
                        }
                        let target = Self::parse_nested_expression(target_tokens)?.expr;
                        let value = Self::parse_nested_expression(value_tokens)?.expr;
                        Ok(Statement::Assignment {
                            target,
                            binding_pattern: None,
                            value,
                            span,
                        })
                    } else {
                        let expr = Self::parse_nested_expression(slice)?.expr;
                        Ok(Statement::Expression { expr, span })
                    }
                }
            }
        }

        fn parse_val_statement(
            &self,
            slice: &[&'a Token],
            absolute_start: usize,
        ) -> Result<Statement, ExpressionError> {
            let absolute_end = absolute_start + slice.len();
            let span = span_for_range(self.tokens, absolute_start, absolute_end);

            if slice.len() < 3 {
                return Err(ExpressionError::new(
                    "`val` 宣言の構文が不正です",
                    Some(span.clone()),
                ));
            }

            let name_token = slice[1];
            let name = match &name_token.token_type {
                TokenType::Identifier(id) => id.clone(),
                _ => {
                    return Err(ExpressionError::new(
                        "`val` 宣言には識別子が必要です",
                        Some(span_from_token(name_token)),
                    ))
                }
            };

            let mut cursor = 2usize;
            let mut type_annotation: Option<TypeAnnotation> = None;

            if cursor < slice.len() && matches!(slice[cursor].token_type, TokenType::Colon) {
                cursor += 1;
                let mut type_end = cursor;
                while type_end < slice.len()
                    && !matches!(slice[type_end].token_type, TokenType::Assign)
                {
                    type_end += 1;
                }

                let type_tokens = &slice[cursor..type_end];
                if !type_tokens.is_empty() {
                    let owned: Vec<Token> =
                        type_tokens.iter().map(|token| (*token).clone()).collect();
                    match lower_type_annotation_from_tokens(&owned) {
                        Ok(lowered) => type_annotation = Some(lowered.into_annotation()),
                        Err(error) => {
                            return Err(ExpressionError::new(
                                error.message().to_string(),
                                error.span().cloned(),
                            ))
                        }
                    }
                }
                cursor = type_end;
            }

            if cursor >= slice.len() || !matches!(slice[cursor].token_type, TokenType::Assign) {
                return Err(ExpressionError::new(
                    "`val` 宣言には `=` が必要です",
                    Some(span.clone()),
                ));
            }

            let initializer_tokens = &slice[cursor + 1..];
            if initializer_tokens.is_empty() {
                return Err(ExpressionError::new(
                    "`val` 宣言の初期化式が必要です",
                    Some(span.clone()),
                ));
            }

            let initializer = Self::parse_nested_expression(initializer_tokens)?.expr;

            Ok(Statement::ValDeclaration {
                name,
                binding: None,
                type_annotation,
                initializer,
                modifiers: Modifiers::default(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span,
            })
        }

        fn find_top_level_assign(slice: &[&Token]) -> Option<usize> {
            let mut depth_paren = 0isize;
            let mut depth_brace = 0isize;
            let mut depth_bracket = 0isize;
            for (idx, token) in slice.iter().enumerate() {
                match token.token_type {
                    TokenType::LeftParen => depth_paren += 1,
                    TokenType::RightParen => depth_paren = depth_paren.saturating_sub(1),
                    TokenType::LeftBrace => depth_brace += 1,
                    TokenType::RightBrace => depth_brace = depth_brace.saturating_sub(1),
                    TokenType::LeftBracket => depth_bracket += 1,
                    TokenType::RightBracket => depth_bracket = depth_bracket.saturating_sub(1),
                    TokenType::Assign
                        if depth_paren == 0 && depth_brace == 0 && depth_bracket == 0 =>
                    {
                        return Some(idx);
                    }
                    _ => {}
                }
            }
            None
        }

        fn parse_when_expression(&mut self) -> Result<ParsedExpr, ExpressionError> {
            let (when_token, when_index) = self.advance_with_index().ok_or_else(|| {
                ExpressionError::new("`when` 式の先頭トークンを取得できませんでした", None)
            })?;
            let when_span = span_from_token(when_token);

            while matches!(
                self.peek_token().map(|token| &token.token_type),
                Some(TokenType::LayoutComma)
            ) {
                self.pos += 1;
            }

            let mut subject_expr: Option<Expression> = None;

            if matches!(
                self.peek_token().map(|token| &token.token_type),
                Some(TokenType::LeftParen)
            ) {
                let open_index = self.pos;
                let close_index = self.find_matching_paren(open_index).ok_or_else(|| {
                    ExpressionError::new(
                        "`when` 条件を閉じる ')' が必要です",
                        self.span_at(open_index),
                    )
                })?;
                let inner_tokens = &self.tokens[open_index + 1..close_index];
                if inner_tokens.is_empty() {
                    return Err(ExpressionError::new(
                        "`when` 条件式が必要です",
                        self.span_at(open_index),
                    ));
                }
                subject_expr = Some(Self::parse_nested_expression(inner_tokens)?.expr);
                self.pos = close_index + 1;
            } else {
                let brace_index = self.find_next_left_brace(self.pos).ok_or_else(|| {
                    ExpressionError::new(
                        "`when` ブロックを開始する '{' が必要です",
                        self.span_at(self.pos),
                    )
                })?;
                if brace_index > self.pos {
                    let subject_tokens = &self.tokens[self.pos..brace_index];
                    if subject_tokens.is_empty() {
                        return Err(ExpressionError::new(
                            "`when` 条件式が必要です",
                            self.span_at(self.pos),
                        ));
                    }
                    subject_expr = Some(Self::parse_nested_expression(subject_tokens)?.expr);
                }
                self.pos = brace_index;
            }

            while matches!(
                self.peek_token().map(|token| &token.token_type),
                Some(TokenType::LayoutComma)
            ) {
                self.pos += 1;
            }

            let brace_index = self.pos;
            let brace_token = self.peek_token().ok_or_else(|| {
                ExpressionError::new(
                    "`when` ブロックを開始する '{' が必要です",
                    self.span_at(self.pos),
                )
            })?;
            if brace_token.token_type != TokenType::LeftBrace {
                return Err(ExpressionError::new(
                    "`when` ブロックを開始する '{' が必要です",
                    Some(span_from_token(brace_token)),
                ));
            }

            let closing_index = self.find_matching_brace(brace_index).ok_or_else(|| {
                ExpressionError::new(
                    "`when` ブロックが閉じられていません",
                    self.span_at(brace_index),
                )
            })?;
            self.pos = brace_index + 1;

            let mut arms = Vec::new();
            let mut else_arm: Option<Expression> = None;

            while self.pos < closing_index {
                if matches!(self.tokens[self.pos].token_type, TokenType::LayoutComma) {
                    self.pos += 1;
                    continue;
                }

                if matches!(self.tokens[self.pos].token_type, TokenType::Else) {
                    if else_arm.is_some() {
                        return Err(ExpressionError::new(
                            "`else` 分岐は複数定義できません",
                            Some(span_from_token(self.tokens[self.pos])),
                        ));
                    }
                    self.pos += 1;
                    if self.pos >= closing_index
                        || self.tokens[self.pos].token_type != TokenType::Arrow
                    {
                        return Err(ExpressionError::new(
                            "`else` 分岐には `->` が必要です",
                            self.span_at(self.pos),
                        ));
                    }
                    self.pos += 1;

                    while matches!(
                        self.tokens.get(self.pos).map(|token| &token.token_type),
                        Some(TokenType::LayoutComma)
                    ) {
                        self.pos += 1;
                    }

                    if self.pos >= closing_index {
                        return Err(ExpressionError::new(
                            "`else` 分岐の式が必要です",
                            self.span_at(self.pos),
                        ));
                    }

                    let slice = &self.tokens[self.pos..closing_index];
                    let parsed_body = {
                        let mut nested = ExpressionParser::new(slice);
                        match nested.parse_expression_bp(0) {
                            Ok(parsed) => parsed,
                            Err(err) => {
                                if matches!(
                                    slice.first().map(|token| &token.token_type),
                                    Some(TokenType::LeftBrace)
                                ) {
                                    let body_close =
                                        self.find_matching_brace(self.pos).ok_or_else(|| {
                                            ExpressionError::new(
                                                "'}' が必要です",
                                                self.span_at(self.pos),
                                            )
                                        })?;
                                    let block_tokens = &self.tokens[self.pos + 1..body_close];
                                    let block_expr = self
                                        .parse_lambda_body_as_block(block_tokens, self.pos + 1)?;
                                    ParsedExpr {
                                        expr: block_expr,
                                        start: 0,
                                        end: body_close - self.pos + 1,
                                    }
                                } else {
                                    return Err(err);
                                }
                            }
                        }
                    };
                    if parsed_body.end == 0 {
                        return Err(ExpressionError::new(
                            "`else` 分岐の式が必要です",
                            self.span_at(self.pos),
                        ));
                    }
                    let body_expr = parsed_body.expr;
                    else_arm = Some(body_expr);
                    self.pos += parsed_body.end;
                    while self.pos < closing_index
                        && matches!(self.tokens[self.pos].token_type, TokenType::LayoutComma)
                    {
                        self.pos += 1;
                    }
                    continue;
                }

                let arrow_index = self
                    .find_top_level_arrow_in_range(self.pos, closing_index)
                    .ok_or_else(|| {
                        ExpressionError::new(
                            "`when` 分岐に `->` が存在しません",
                            self.span_at(self.pos),
                        )
                    })?;

                if arrow_index <= self.pos {
                    return Err(ExpressionError::new(
                        "`when` 分岐のパターンが空です",
                        Some(span_from_token(self.tokens[self.pos])),
                    ));
                }

                let pattern_tokens = &self.tokens[self.pos..arrow_index];
                let (pattern, guard, pattern_span) =
                    Self::parse_when_pattern(pattern_tokens, subject_expr.is_some())?;
                self.pos = arrow_index + 1;

                while matches!(
                    self.tokens.get(self.pos).map(|token| &token.token_type),
                    Some(TokenType::LayoutComma)
                ) {
                    self.pos += 1;
                }

                if self.pos >= closing_index {
                    return Err(ExpressionError::new(
                        "`when` 分岐の式が必要です",
                        self.span_at(self.pos),
                    ));
                }

                let slice = &self.tokens[self.pos..closing_index];
                let parsed_body = {
                    let mut nested = ExpressionParser::new(slice);
                    match nested.parse_expression_bp(0) {
                        Ok(parsed) => parsed,
                        Err(err) => {
                            if matches!(
                                slice.first().map(|token| &token.token_type),
                                Some(TokenType::LeftBrace)
                            ) {
                                let body_close =
                                    self.find_matching_brace(self.pos).ok_or_else(|| {
                                        ExpressionError::new(
                                            "'}' が必要です",
                                            self.span_at(self.pos),
                                        )
                                    })?;
                                let block_tokens = &self.tokens[self.pos + 1..body_close];
                                let block_expr =
                                    self.parse_lambda_body_as_block(block_tokens, self.pos + 1)?;
                                ParsedExpr {
                                    expr: block_expr,
                                    start: 0,
                                    end: body_close - self.pos + 1,
                                }
                            } else {
                                return Err(err);
                            }
                        }
                    }
                };
                if parsed_body.end == 0 {
                    return Err(ExpressionError::new(
                        "`when` 分岐の式が必要です",
                        self.span_at(self.pos),
                    ));
                }
                let body_expr = parsed_body.expr;
                let body_end = self.pos + parsed_body.end;
                let body_span = expression_span(&body_expr);
                let arm_span = merge_spans(&pattern_span, &body_span);

                arms.push(WhenArm {
                    pattern,
                    guard,
                    body: body_expr,
                    span: arm_span,
                });

                self.pos = body_end;
                while self.pos < closing_index
                    && matches!(self.tokens[self.pos].token_type, TokenType::LayoutComma)
                {
                    self.pos += 1;
                }
            }

            if arms.is_empty() && else_arm.is_none() {
                return Err(ExpressionError::new(
                    "`when` 式には少なくとも1つの分岐が必要です",
                    Some(when_span),
                ));
            }

            self.pos = closing_index + 1;
            let end_span = span_from_token(self.tokens[closing_index]);
            let span = merge_spans(&when_span, &end_span);

            Ok(ParsedExpr {
                expr: Expression::When {
                    expr: subject_expr.map(Box::new),
                    arms,
                    else_arm: else_arm.map(Box::new),
                    implicit_end: None,
                    span: span.clone(),
                },
                start: when_index,
                end: closing_index + 1,
            })
        }

        fn find_next_left_brace(&self, start: usize) -> Option<usize> {
            self.tokens
                .iter()
                .enumerate()
                .skip(start)
                .find_map(|(idx, token)| {
                    if matches!(token.token_type, TokenType::LeftBrace) {
                        Some(idx)
                    } else {
                        None
                    }
                })
        }

        fn find_top_level_arrow_in_range(&self, start: usize, end: usize) -> Option<usize> {
            let mut depth_paren = 0usize;
            let mut depth_brace = 0usize;
            let mut depth_bracket = 0usize;
            for idx in start..end {
                match self.tokens[idx].token_type {
                    TokenType::LeftParen => depth_paren += 1,
                    TokenType::RightParen => {
                        if depth_paren > 0 {
                            depth_paren -= 1;
                        }
                    }
                    TokenType::LeftBrace => depth_brace += 1,
                    TokenType::RightBrace => {
                        if depth_brace > 0 {
                            depth_brace -= 1;
                        } else {
                            break;
                        }
                    }
                    TokenType::LeftBracket => depth_bracket += 1,
                    TokenType::RightBracket => {
                        if depth_bracket > 0 {
                            depth_bracket -= 1;
                        }
                    }
                    TokenType::Arrow
                        if depth_paren == 0 && depth_brace == 0 && depth_bracket == 0 =>
                    {
                        return Some(idx);
                    }
                    _ => {}
                }
            }
            None
        }

        fn parse_when_pattern(
            tokens: &[&'a Token],
            has_subject: bool,
        ) -> Result<(Pattern, Option<Expression>, Span), ExpressionError> {
            let filtered = Self::filter_layout(tokens);
            if filtered.is_empty() {
                return Err(ExpressionError::new(
                    "`when` 分岐のパターンが必要です",
                    None,
                ));
            }

            if !has_subject {
                let condition = Self::parse_nested_expression(filtered.as_slice())?.expr;
                let condition_span = expression_span(&condition);
                let wildcard_span = condition_span.clone();
                return Ok((
                    Pattern::Wildcard(wildcard_span),
                    Some(condition),
                    condition_span,
                ));
            }

            let (base_slice, guard_slice) = Self::split_guard_tokens(filtered.as_slice())?;
            if base_slice.is_empty() {
                return Err(ExpressionError::new(
                    "`when` 分岐のパターンが必要です",
                    Some(span_for_range_slice(filtered.as_slice())),
                ));
            }

            let mut pattern = Self::parse_base_pattern(base_slice)?;
            if let Some(guard_tokens) = guard_slice {
                let guard_filtered = Self::filter_layout(guard_tokens);
                if guard_filtered.is_empty() {
                    return Err(ExpressionError::new(
                        "`when` ガード式が空です",
                        Some(span_for_range_slice(base_slice)),
                    ));
                }
                let guard_expr = Self::parse_nested_expression(guard_filtered.as_slice())?.expr;
                let base_span = pattern_span(&pattern);
                let guard_span = expression_span(&guard_expr);
                let span = merge_spans(&base_span, &guard_span);
                pattern = Pattern::Guard {
                    pattern: Box::new(pattern),
                    condition: guard_expr,
                    span,
                };
            }

            let span_for_arm = pattern_span(&pattern);
            let (final_pattern, guard) = split_when_guard(pattern);
            Ok((final_pattern, guard, span_for_arm))
        }

        fn parse_base_pattern(tokens: &[&'a Token]) -> Result<Pattern, ExpressionError> {
            let filtered = Self::filter_layout(tokens);
            if filtered.is_empty() {
                return Err(ExpressionError::new(
                    "`when` 分岐のパターンが必要です",
                    None,
                ));
            }

            let first = filtered[0];
            match &first.token_type {
                TokenType::Identifier(name) if name == "_" => {
                    let span = span_from_token(first);
                    Ok(Pattern::Wildcard(span))
                }
                TokenType::Identifier(name) if name == "is" => {
                    if filtered.len() < 2 {
                        return Err(ExpressionError::new(
                            "`is` パターンには型名が必要です",
                            Some(span_from_token(first)),
                        ));
                    }
                    let type_token = filtered[1];
                    let type_name = match &type_token.token_type {
                        TokenType::Identifier(value) => value.clone(),
                        _ => {
                            return Err(ExpressionError::new(
                                "`is` パターンには識別子が必要です",
                                Some(span_from_token(type_token)),
                            ))
                        }
                    };
                    let span = merge_spans(&span_from_token(first), &span_from_token(type_token));
                    Ok(Pattern::Constructor {
                        name: type_name,
                        patterns: Vec::new(),
                        span,
                    })
                }
                TokenType::In => {
                    let range_tokens = filtered.get(1..).ok_or_else(|| {
                        ExpressionError::new(
                            "`in` パターンには範囲式が必要です",
                            Some(span_from_token(first)),
                        )
                    })?;
                    let range_expr = Self::parse_nested_expression(range_tokens)?.expr;
                    match range_expr {
                        Expression::Binary {
                            left,
                            op: BinaryOp::RangeExclusive,
                            right,
                            span,
                            ..
                        } => Ok(Pattern::Range {
                            start: left,
                            end: right,
                            inclusive_end: false,
                            span,
                        }),
                        Expression::Binary {
                            left,
                            op: BinaryOp::RangeInclusive,
                            right,
                            span,
                            ..
                        } => Ok(Pattern::Range {
                            start: left,
                            end: right,
                            inclusive_end: true,
                            span,
                        }),
                        other => Err(ExpressionError::new(
                            format!("範囲式が必要ですが {:?} が見つかりました", other),
                            Some(expression_span(&other)),
                        )),
                    }
                }
                TokenType::String(value) => {
                    let span = span_from_token(first);
                    Ok(Pattern::Literal(Literal::String(value.clone()), span))
                }
                TokenType::Number(value) => {
                    let span = span_from_token(first);
                    Ok(Pattern::Literal(Literal::Number(value.clone()), span))
                }
                TokenType::Character(value) => {
                    let span = span_from_token(first);
                    Ok(Pattern::Literal(Literal::Character(value.clone()), span))
                }
                TokenType::Boolean(value) => {
                    let span = span_from_token(first);
                    Ok(Pattern::Literal(Literal::Boolean(value.clone()), span))
                }
                TokenType::Null => {
                    let span = span_from_token(first);
                    Ok(Pattern::Literal(Literal::Null, span))
                }
                TokenType::RegexLiteral(_) => {
                    let span = span_from_token(first);
                    let literal = regex_literal_from_token(first, span.clone());
                    let literal_span = literal.span.clone();
                    Ok(Pattern::Literal(Literal::Regex(literal), literal_span))
                }
                TokenType::Identifier(name) => {
                    if filtered.len() == 1 {
                        let span = span_from_token(first);
                        return Ok(Pattern::Identifier(name.clone(), span));
                    }

                    if !matches!(
                        filtered.get(1).map(|token| &token.token_type),
                        Some(TokenType::LeftParen)
                    ) {
                        return Err(ExpressionError::new(
                            format!("`{}` パターンの構文が解釈できません", name),
                            Some(span_from_token(filtered[1])),
                        ));
                    }

                    let open_index = 1;
                    let close_index = find_matching_paren_in_slice(&filtered, open_index)
                        .ok_or_else(|| {
                            ExpressionError::new(
                                "`when` コンストラクタパターンの ')' が必要です",
                                Some(span_from_token(filtered[open_index])),
                            )
                        })?;
                    let inner_tokens = &filtered[open_index + 1..close_index];
                    let arguments = Self::parse_constructor_arguments(inner_tokens)?;
                    let span = merge_spans(
                        &span_from_token(first),
                        &span_from_token(filtered[close_index]),
                    );
                    Ok(Pattern::Constructor {
                        name: name.clone(),
                        patterns: arguments,
                        span,
                    })
                }
                other => Err(ExpressionError::new(
                    format!("未対応のパターントークン {:?}", other),
                    Some(span_from_token(first)),
                )),
            }
        }

        fn parse_constructor_arguments(
            tokens: &[&'a Token],
        ) -> Result<Vec<Pattern>, ExpressionError> {
            let mut args = Vec::new();
            let mut start = 0usize;
            let mut depth_paren = 0usize;
            let mut depth_brace = 0usize;
            let mut depth_bracket = 0usize;

            for (index, token) in tokens.iter().enumerate() {
                match token.token_type {
                    TokenType::LeftParen => depth_paren += 1,
                    TokenType::RightParen => {
                        if depth_paren > 0 {
                            depth_paren -= 1;
                        }
                    }
                    TokenType::LeftBrace => depth_brace += 1,
                    TokenType::RightBrace => {
                        if depth_brace > 0 {
                            depth_brace -= 1;
                        }
                    }
                    TokenType::LeftBracket => depth_bracket += 1,
                    TokenType::RightBracket => {
                        if depth_bracket > 0 {
                            depth_bracket -= 1;
                        }
                    }
                    TokenType::Comma | TokenType::LayoutComma
                        if depth_paren == 0 && depth_brace == 0 && depth_bracket == 0 =>
                    {
                        let slice = &tokens[start..index];
                        if !slice.is_empty() {
                            let (pattern, guard, _) = Self::parse_when_pattern(slice, true)?;
                            if guard.is_some() {
                                return Err(ExpressionError::new(
                                    "コンストラクタ引数パターンでガードは使用できません",
                                    Some(span_for_range_slice(slice)),
                                ));
                            }
                            args.push(pattern);
                        }
                        start = index + 1;
                    }
                    _ => {}
                }
            }

            if start < tokens.len() {
                let slice = &tokens[start..];
                if !slice.is_empty() {
                    let (pattern, guard, _) = Self::parse_when_pattern(slice, true)?;
                    if guard.is_some() {
                        return Err(ExpressionError::new(
                            "コンストラクタ引数パターンでガードは使用できません",
                            Some(span_for_range_slice(slice)),
                        ));
                    }
                    args.push(pattern);
                }
            }

            Ok(args)
        }

        fn split_guard_tokens<'slice>(
            tokens: &'slice [&'a Token],
        ) -> Result<(&'slice [&'a Token], Option<&'slice [&'a Token]>), ExpressionError> {
            let mut depth_paren = 0usize;
            let mut depth_brace = 0usize;
            let mut depth_bracket = 0usize;
            for (idx, token) in tokens.iter().enumerate() {
                match token.token_type {
                    TokenType::LeftParen => depth_paren += 1,
                    TokenType::RightParen => {
                        if depth_paren > 0 {
                            depth_paren -= 1;
                        }
                    }
                    TokenType::LeftBrace => depth_brace += 1,
                    TokenType::RightBrace => {
                        if depth_brace > 0 {
                            depth_brace -= 1;
                        }
                    }
                    TokenType::LeftBracket => depth_bracket += 1,
                    TokenType::RightBracket => {
                        if depth_bracket > 0 {
                            depth_bracket -= 1;
                        }
                    }
                    TokenType::And
                        if depth_paren == 0 && depth_brace == 0 && depth_bracket == 0 =>
                    {
                        if idx == 0 {
                            return Err(ExpressionError::new(
                                "`when` ガードの前にパターンが必要です",
                                Some(span_from_token(token)),
                            ));
                        }
                        let base = &tokens[..idx];
                        let guard = &tokens[idx + 1..];
                        return Ok((base, Some(guard)));
                    }
                    _ => {}
                }
            }
            Ok((tokens, None))
        }

        fn filter_layout(tokens: &[&'a Token]) -> Vec<&'a Token> {
            tokens
                .iter()
                .copied()
                .filter(|token| !matches!(token.token_type, TokenType::LayoutComma))
                .collect()
        }

        fn parse_postfix(
            &mut self,
            left: ParsedExpr,
            kind: PostfixKind,
        ) -> Result<ParsedExpr, ExpressionError> {
            match kind {
                PostfixKind::MemberAccess => self.parse_member_access(left),
                PostfixKind::NullSafeMemberAccess => self.parse_null_safe_member(left),
                PostfixKind::Index => self.parse_index_access(left, false),
                PostfixKind::NullSafeIndex => self.parse_index_access(left, true),
                PostfixKind::TypeArguments => self.parse_type_arguments_postfix(left),
                PostfixKind::Call => self.parse_parenthesized_call(left),
                PostfixKind::TrailingLambda => self.parse_trailing_lambda_call(left),
            }
        }

        fn parse_string_segments(
            &mut self,
            start_index: usize,
        ) -> Result<ParsedExpr, ExpressionError> {
            let start_token = self
                .tokens
                .get(start_index)
                .ok_or_else(|| ExpressionError::new("文字列リテラルが必要です", None))?;

            let initial_span = span_from_token(start_token);
            let (mut expr, token_consumption) =
                parse_string_token_with_metadata(start_token, initial_span.clone())?;

            let interpolation_expr_count = count_interpolation_expression_segments(start_token);
            let string_mid_count = interpolation_expr_count.saturating_sub(1);
            let end_index = start_index
                .saturating_add(token_consumption)
                .saturating_add(string_mid_count)
                .saturating_add(2)
                .min(self.tokens.len());
            self.pos = end_index;

            let span = span_for_range(self.tokens, start_index, end_index.max(start_index + 1));
            match &mut expr {
                Expression::StringInterpolation {
                    span: expr_span, ..
                } => {
                    *expr_span = span.clone();
                }
                Expression::MultilineString(literal) => {
                    literal.span = span.clone();
                }
                _ => {}
            }

            Ok(ParsedExpr {
                expr,
                start: start_index,
                end: end_index,
            })
        }

        fn parse_member_access(&mut self, left: ParsedExpr) -> Result<ParsedExpr, ExpressionError> {
            let (_, _dot_index) = self.advance_with_index_or_error("'.' が必要です")?;
            let (token, name_index) = self.advance_with_index_or_error("メンバー名が必要です")?;
            let property = match &token.token_type {
                TokenType::Identifier(value) => value.clone(),
                _ => {
                    return Err(ExpressionError::new(
                        "メンバーアクセスには識別子が必要です",
                        Some(span_from_token(token)),
                    ))
                }
            };

            let span = merge_spans(&left.span(), &span_from_token(token));
            let expr = Expression::MemberAccess {
                object: Box::new(left.expr),
                property,
                span: span.clone(),
            };
            Ok(ParsedExpr {
                expr,
                start: left.start,
                end: name_index + 1,
            })
        }

        fn parse_null_safe_member(
            &mut self,
            left: ParsedExpr,
        ) -> Result<ParsedExpr, ExpressionError> {
            let (_, _op_index) = self.advance_with_index_or_error("'?.' が必要です")?;
            let (token, name_index) = self.advance_with_index_or_error("メンバー名が必要です")?;
            let property = match &token.token_type {
                TokenType::Identifier(value) => value.clone(),
                _ => {
                    return Err(ExpressionError::new(
                        "メンバーアクセスには識別子が必要です",
                        Some(span_from_token(token)),
                    ))
                }
            };

            let span = merge_spans(&left.span(), &span_from_token(token));
            let expr = Expression::NullSafeMemberAccess {
                object: Box::new(left.expr),
                property,
                span: span.clone(),
            };
            Ok(ParsedExpr {
                expr,
                start: left.start,
                end: name_index + 1,
            })
        }

        fn parse_index_access(
            &mut self,
            left: ParsedExpr,
            null_safe: bool,
        ) -> Result<ParsedExpr, ExpressionError> {
            let question_index = if null_safe {
                let (_, idx) = self.advance_with_index_or_error("'?[' が必要です")?;
                Some(idx)
            } else {
                None
            };

            let (open_token, open_index) = self.advance_with_index_or_error("'[' が必要です")?;
            if !matches!(open_token.token_type, TokenType::LeftBracket) {
                return Err(ExpressionError::new(
                    "'[' が必要です",
                    Some(span_from_token(open_token)),
                ));
            }

            let close_index = self
                .find_matching_bracket(open_index)
                .ok_or_else(|| ExpressionError::new("']' が必要です", self.span_at(open_index)))?;
            let inner_tokens = &self.tokens[open_index + 1..close_index];
            let index_expr = if inner_tokens.is_empty() {
                return Err(ExpressionError::new(
                    "インデックス式が必要です",
                    self.span_at(open_index),
                ));
            } else {
                Self::parse_nested_expression(inner_tokens)?.expr
            };
            self.pos = close_index + 1;

            let start_index = question_index.unwrap_or(left.start);
            let span = span_for_range(self.tokens, start_index, close_index + 1);
            let expr = if null_safe {
                Expression::NullSafeIndexAccess {
                    object: Box::new(left.expr),
                    index: Box::new(index_expr),
                    span: span.clone(),
                }
            } else {
                Expression::IndexAccess {
                    object: Box::new(left.expr),
                    index: Box::new(index_expr),
                    span: span.clone(),
                }
            };

            Ok(ParsedExpr {
                expr,
                start: left.start,
                end: close_index + 1,
            })
        }

        fn parse_type_arguments_postfix(
            &mut self,
            mut left: ParsedExpr,
        ) -> Result<ParsedExpr, ExpressionError> {
            if self.pending_type_arguments.is_some() {
                return Err(ExpressionError::new(
                    "型引数リストが重複しています",
                    self.span_at(self.pos),
                ));
            }

            let (end_index, ranges) = self.extract_type_argument_ranges(self.pos)?;
            if ranges.is_empty() {
                return Err(ExpressionError::new(
                    "型引数リストが空です",
                    self.span_at(self.pos),
                ));
            }

            let mut annotations = Vec::with_capacity(ranges.len());
            for (start, end) in ranges {
                let owned_tokens: Vec<Token> = self.tokens[start..end]
                    .iter()
                    .map(|token| (*token).clone())
                    .collect();

                let has_non_trivia = owned_tokens.iter().any(|token| !is_trivia_token(token));

                if !has_non_trivia {
                    return Err(ExpressionError::new(
                        "型引数が空です",
                        self.span_at(start).or_else(|| self.span_at(end)),
                    ));
                }

                match lower_type_annotation_from_tokens(&owned_tokens) {
                    Ok(lowered) => annotations.push(lowered.into_annotation()),
                    Err(error) => {
                        let span = error
                            .span()
                            .cloned()
                            .or_else(|| Some(span_for_range(self.tokens, start, end)));
                        return Err(ExpressionError::new(error.message().to_string(), span));
                    }
                }
            }

            let next_index = end_index + 1;
            self.pos = next_index;
            while let Some(token) = self.tokens.get(self.pos) {
                match token.token_type {
                    TokenType::Whitespace(_)
                    | TokenType::Newline
                    | TokenType::LayoutComma
                    | TokenType::LineComment(_)
                    | TokenType::BlockComment(_)
                    | TokenType::JavaDocComment(_) => self.pos += 1,
                    _ => break,
                }
            }

            self.pending_type_arguments = Some(annotations);
            left.end = next_index;
            Ok(left)
        }

        fn parse_parenthesized_call(
            &mut self,
            left: ParsedExpr,
        ) -> Result<ParsedExpr, ExpressionError> {
            let (_, open_index) = self.advance_with_index_or_error("'(' が必要です")?;
            let close_index = self
                .find_matching_paren(open_index)
                .ok_or_else(|| ExpressionError::new("')' が必要です", self.span_at(open_index)))?;

            let saved_type_arguments = self.pending_type_arguments.take();
            let (arguments, metadata, args_span_end) =
                self.parse_arguments(open_index, close_index)?;
            self.pos = close_index + 1;

            let call_span = span_for_range(self.tokens, left.start, args_span_end);
            let type_arguments = saved_type_arguments.unwrap_or_default();
            let expr = Expression::Call {
                function: Box::new(left.expr),
                args: arguments,
                type_arguments,
                argument_metadata: metadata,
                span: call_span.clone(),
            };

            Ok(ParsedExpr {
                expr,
                start: left.start,
                end: close_index + 1,
            })
        }

        fn parse_trailing_lambda_call(
            &mut self,
            left: ParsedExpr,
        ) -> Result<ParsedExpr, ExpressionError> {
            let lambda_parsed = self.parse_brace_expression()?;
            let lambda_end = lambda_parsed.end;
            let raw_lambda_expr = lambda_parsed.expr;
            let start = left.start;
            let call_span = span_for_range(self.tokens, start, lambda_end);
            let base_expr = left.expr;
            let lambda_expr = match raw_lambda_expr {
                Expression::Lambda { .. } => raw_lambda_expr,
                other => {
                    let span = other.span().clone();
                    Expression::Lambda {
                        parameters: Vec::new(),
                        body: Box::new(other),
                        span,
                    }
                }
            };

            if let Expression::Call {
                function,
                mut args,
                type_arguments,
                argument_metadata,
                ..
            } = base_expr
            {
                args.push(Argument::Positional(lambda_expr));
                let expr = Expression::Call {
                    function,
                    args,
                    type_arguments,
                    argument_metadata,
                    span: call_span.clone(),
                };
                return Ok(ParsedExpr {
                    expr,
                    start,
                    end: lambda_end,
                });
            }

            let mut metadata = CallArgumentMetadata::with_style(CallArgumentStyle::Whitespace);
            metadata.used_commas = false;
            let expr = Expression::Call {
                function: Box::new(base_expr),
                args: vec![Argument::Positional(lambda_expr)],
                type_arguments: Vec::new(),
                argument_metadata: metadata,
                span: call_span.clone(),
            };
            Ok(ParsedExpr {
                expr,
                start,
                end: lambda_end,
            })
        }

        fn parse_arguments(
            &self,
            open_index: usize,
            close_index: usize,
        ) -> Result<(Vec<Argument>, CallArgumentMetadata, usize), ExpressionError> {
            let inner = &self.tokens[open_index + 1..close_index];
            if inner.is_empty() {
                let metadata = CallArgumentMetadata::with_style(CallArgumentStyle::Comma);
                return Ok((Vec::new(), metadata, close_index + 1));
            }

            let mut parts = Vec::new();
            let mut separators = Vec::new();
            let mut start = 0usize;
            let mut depth_paren = 0usize;
            let mut depth_brace = 0usize;
            let mut depth_bracket = 0usize;

            for (offset, token) in inner.iter().enumerate() {
                let at_top_level = depth_paren == 0 && depth_brace == 0 && depth_bracket == 0;

                if at_top_level
                    && has_layout_break(token)
                    && !matches!(token.token_type, TokenType::Comma | TokenType::LayoutComma)
                    && start < offset
                {
                    parts.push(&inner[start..offset]);
                    separators.push(SeparatorKind::Layout);
                    start = offset;
                }

                if at_top_level
                    && start < offset
                    && token.leading_trivia.newlines == 0
                    && token.leading_trivia.spaces > 0
                {
                    if let Some(prev_token) = inner.get(offset - 1) {
                        if !token_requires_followup(&prev_token.token_type) {
                            parts.push(&inner[start..offset]);
                            separators.push(SeparatorKind::Layout);
                            start = offset;
                        }
                    }
                }

                match token.token_type {
                    TokenType::LeftParen => depth_paren += 1,
                    TokenType::RightParen if depth_paren > 0 => depth_paren -= 1,
                    TokenType::LeftBrace => depth_brace += 1,
                    TokenType::RightBrace if depth_brace > 0 => depth_brace -= 1,
                    TokenType::LeftBracket => depth_bracket += 1,
                    TokenType::RightBracket if depth_bracket > 0 => depth_bracket -= 1,
                    TokenType::Comma if at_top_level => {
                        if start != offset {
                            parts.push(&inner[start..offset]);
                            separators.push(SeparatorKind::Comma);
                        }
                        start = offset + 1;
                        continue;
                    }
                    TokenType::LayoutComma if at_top_level => {
                        if start != offset {
                            parts.push(&inner[start..offset]);
                            separators.push(SeparatorKind::Layout);
                        }
                        start = offset + 1;
                        continue;
                    }
                    _ => {}
                }
            }

            let mut trailing_separator: Option<SeparatorKind> = None;
            if start < inner.len() {
                parts.push(&inner[start..]);
            } else if start == inner.len() && !separators.is_empty() {
                trailing_separator = separators.pop();
            }

            let mut arguments = Vec::new();
            for slice in parts {
                if slice.is_empty() {
                    continue;
                }
                arguments.push(self.parse_argument_slice(slice)?);
            }

            let mut saw_comma = false;
            let mut saw_layout = false;
            for separator in &separators {
                match separator {
                    SeparatorKind::Comma => saw_comma = true,
                    SeparatorKind::Layout => saw_layout = true,
                }
            }

            if let Some(separator) = trailing_separator {
                match separator {
                    SeparatorKind::Comma => saw_comma = true,
                    SeparatorKind::Layout => saw_layout = true,
                }
            }

            let style = if saw_layout && !saw_comma {
                CallArgumentStyle::Whitespace
            } else {
                CallArgumentStyle::Comma
            };
            let mut metadata = CallArgumentMetadata::with_style(style);
            metadata.used_commas = saw_comma;

            Ok((arguments, metadata, close_index + 1))
        }

        fn parse_argument_slice(&self, slice: &[&Token]) -> Result<Argument, ExpressionError> {
            let mut depth_paren = 0usize;
            let mut depth_brace = 0usize;
            let mut depth_bracket = 0usize;
            let mut assign_index = None;

            for (idx, token) in slice.iter().enumerate() {
                match token.token_type {
                    TokenType::LeftParen => depth_paren += 1,
                    TokenType::RightParen if depth_paren > 0 => depth_paren -= 1,
                    TokenType::LeftBrace => depth_brace += 1,
                    TokenType::RightBrace if depth_brace > 0 => depth_brace -= 1,
                    TokenType::LeftBracket => depth_bracket += 1,
                    TokenType::RightBracket if depth_bracket > 0 => depth_bracket -= 1,
                    TokenType::Assign
                        if depth_paren == 0 && depth_brace == 0 && depth_bracket == 0 =>
                    {
                        assign_index = Some(idx);
                        break;
                    }
                    _ => {}
                }
            }

            if let Some(idx) = assign_index {
                let name_token = slice
                    .first()
                    .ok_or_else(|| ExpressionError::new("引数名が必要です", None))?;
                let name = match &name_token.token_type {
                    TokenType::Identifier(value) => value.clone(),
                    _ => {
                        return Err(ExpressionError::new(
                            "名前付き引数には識別子が必要です",
                            Some(span_from_token(name_token)),
                        ))
                    }
                };
                let value_tokens = &slice[idx + 1..];
                if value_tokens.is_empty() {
                    return Err(ExpressionError::new(
                        "名前付き引数の値が必要です",
                        Some(span_from_token(slice[idx])),
                    ));
                }
                let value_expr = Self::parse_nested_expression(value_tokens)?.expr;
                let span = merge_spans(&span_from_token(name_token), &expression_span(&value_expr));
                return Ok(Argument::Named {
                    name,
                    value: value_expr,
                    span,
                });
            }

            let expr = Self::parse_nested_expression(slice)?.expr;
            Ok(Argument::Positional(expr))
        }

        fn parse_lambda_parameters(
            &self,
            tokens: &[&Token],
        ) -> Result<Vec<Parameter>, ExpressionError> {
            let filtered: Vec<&Token> = tokens
                .iter()
                .copied()
                .filter(|token| {
                    !matches!(
                        token.token_type,
                        TokenType::Whitespace(_) | TokenType::Newline
                    )
                })
                .collect();

            if filtered.is_empty() {
                return Ok(Vec::new());
            }

            if matches!(filtered[0].token_type, TokenType::LeftParen) {
                return self.parse_parenthesized_parameters(&filtered);
            }

            if filtered.len() == 1 {
                return match &filtered[0].token_type {
                    TokenType::Identifier(name) => {
                        let span = span_from_token(filtered[0]);
                        Ok(vec![Parameter {
                            name: name.clone(),
                            type_annotation: None,
                            default_value: None,
                            modifiers: ParameterModifiers::default(),
                            span,
                        }])
                    }
                    _ => Err(ExpressionError::new(
                        "ラムダパラメータには識別子が必要です",
                        Some(span_from_token(filtered[0])),
                    )),
                };
            }

            let mut params = Vec::new();
            let mut current: Vec<&Token> = Vec::new();
            for token in filtered.iter().copied() {
                if matches!(token.token_type, TokenType::LayoutComma) {
                    self.push_parameters_from_slice(&mut params, &current)?;
                    current.clear();
                } else {
                    current.push(token);
                }
            }
            self.push_parameters_from_slice(&mut params, &current)?;

            if params.is_empty() {
                Err(ExpressionError::new(
                    "ラムダパラメータの解析に失敗しました",
                    tokens.first().map(|t| span_from_token(t)),
                ))
            } else {
                Ok(params)
            }
        }

        fn parse_parenthesized_parameters(
            &self,
            tokens: &[&Token],
        ) -> Result<Vec<Parameter>, ExpressionError> {
            if tokens.len() < 2 {
                return Err(ExpressionError::new(
                    "ラムダパラメータの括弧が正しく閉じられていません",
                    tokens.first().map(|t| span_from_token(t)),
                ));
            }

            let mut params = Vec::new();
            let mut current = Vec::new();
            let mut depth = 0usize;
            for token in &tokens[1..tokens.len() - 1] {
                if depth == 0
                    && has_layout_break(token)
                    && !matches!(token.token_type, TokenType::Comma | TokenType::LayoutComma)
                    && !current.is_empty()
                {
                    self.push_parameters_from_slice(&mut params, &current)?;
                    current.clear();
                }

                match token.token_type {
                    TokenType::LeftParen => depth += 1,
                    TokenType::RightParen if depth > 0 => depth -= 1,
                    TokenType::Comma | TokenType::LayoutComma if depth == 0 => {
                        self.push_parameters_from_slice(&mut params, &current)?;
                        current.clear();
                        continue;
                    }
                    _ => {}
                }
                current.push(*token);
            }

            self.push_parameters_from_slice(&mut params, &current)?;

            Ok(params)
        }

        fn push_parameters_from_slice(
            &self,
            params: &mut Vec<Parameter>,
            slice: &[&Token],
        ) -> Result<(), ExpressionError> {
            if slice.is_empty() {
                return Ok(());
            }

            let filtered: Vec<&Token> = slice
                .iter()
                .copied()
                .filter(|token| {
                    !matches!(
                        token.token_type,
                        TokenType::Whitespace(_) | TokenType::Newline
                    )
                })
                .collect();

            if filtered.is_empty() {
                return Ok(());
            }

            let has_colon = filtered
                .iter()
                .any(|token| matches!(token.token_type, TokenType::Colon));
            let all_identifiers = filtered.iter().all(|token| {
                matches!(
                    token.token_type,
                    TokenType::Identifier(_) | TokenType::LayoutComma
                )
            });

            if !has_colon && all_identifiers {
                for token in filtered.iter().copied() {
                    if let TokenType::Identifier(name) = &token.token_type {
                        params.push(Parameter {
                            name: name.clone(),
                            type_annotation: None,
                            default_value: None,
                            modifiers: ParameterModifiers::default(),
                            span: span_from_token(token),
                        });
                    }
                }
                return Ok(());
            }

            params.push(self.make_parameter_from_slice(&filtered)?);
            Ok(())
        }

        fn make_parameter_from_slice(
            &self,
            slice: &[&Token],
        ) -> Result<Parameter, ExpressionError> {
            if slice.is_empty() {
                return Err(ExpressionError::new("パラメータが空です", None));
            }

            let name_token = slice[0];
            let name = match &name_token.token_type {
                TokenType::Identifier(value) => value.clone(),
                _ => {
                    return Err(ExpressionError::new(
                        "パラメータ名には識別子が必要です",
                        Some(span_from_token(name_token)),
                    ))
                }
            };

            let type_annotation = slice
                .iter()
                .enumerate()
                .find(|(_, token)| matches!(token.token_type, TokenType::Colon))
                .map(|(idx, _)| {
                    let ty_tokens = &slice[idx + 1..];
                    TypeAnnotation::Simple(join_tokens(ty_tokens))
                });

            let span = span_for_range_slice(slice);
            Ok(Parameter {
                name,
                type_annotation,
                default_value: None,
                modifiers: ParameterModifiers::default(),
                span,
            })
        }

        fn make_binary(
            &self,
            left: ParsedExpr,
            right: ParsedExpr,
            op: BinaryOp,
            op_span: Span,
        ) -> Result<ParsedExpr, ExpressionError> {
            let span = span_for_range(self.tokens, left.start, right.end);
            match op {
                BinaryOp::Add
                | BinaryOp::Subtract
                | BinaryOp::Multiply
                | BinaryOp::Divide
                | BinaryOp::Modulo
                | BinaryOp::Equal
                | BinaryOp::NotEqual
                | BinaryOp::Less
                | BinaryOp::LessEqual
                | BinaryOp::Greater
                | BinaryOp::GreaterEqual
                | BinaryOp::And
                | BinaryOp::Or
                | BinaryOp::Is
                | BinaryOp::RangeExclusive
                | BinaryOp::RangeInclusive
                | BinaryOp::Elvis => {}
                _ => {
                    return Err(ExpressionError::new(
                        format!("未対応の2項演算子 {:?}", op),
                        Some(op_span.clone()),
                    ));
                }
            }

            let metadata = if matches!(op, BinaryOp::Is) {
                BinaryMetadata {
                    is_test: detect_is_test_metadata(&right.expr, &span),
                }
            } else {
                BinaryMetadata::default()
            };

            let expr = Expression::Binary {
                left: Box::new(left.expr),
                op,
                right: Box::new(right.expr),
                span: span.clone(),
                metadata,
            };
            Ok(ParsedExpr {
                expr,
                start: left.start,
                end: right.end,
            })
        }

        fn peek_postfix_kind(&self) -> Option<PostfixKind> {
            let token = self.peek_token()?;
            if matches!(token.token_type, TokenType::Less) && self.can_parse_type_arguments() {
                return Some(PostfixKind::TypeArguments);
            }
            match token.token_type {
                TokenType::Dot => Some(PostfixKind::MemberAccess),
                TokenType::NullSafe => Some(PostfixKind::NullSafeMemberAccess),
                TokenType::LeftBracket => Some(PostfixKind::Index),
                TokenType::Question => {
                    if matches!(
                        self.tokens.get(self.pos + 1).map(|token| &token.token_type),
                        Some(TokenType::LeftBracket)
                    ) {
                        Some(PostfixKind::NullSafeIndex)
                    } else {
                        None
                    }
                }
                TokenType::LeftParen => Some(PostfixKind::Call),
                TokenType::LeftBrace => Some(PostfixKind::TrailingLambda),
                _ => None,
            }
        }

        fn can_parse_type_arguments(&self) -> bool {
            match self.extract_type_argument_ranges(self.pos) {
                Ok((end_index, ranges)) => {
                    if ranges.is_empty() {
                        return false;
                    }
                    matches!(
                        self.next_significant_kind(end_index),
                        Some(TokenType::LeftParen)
                    )
                }
                Err(_) => false,
            }
        }

        fn extract_type_argument_ranges(
            &self,
            start_index: usize,
        ) -> Result<(usize, Vec<(usize, usize)>), ExpressionError> {
            let Some(start_token) = self.tokens.get(start_index) else {
                return Err(ExpressionError::new("`<` が必要です", None));
            };

            if !matches!(start_token.token_type, TokenType::Less) {
                return Err(ExpressionError::new(
                    "型引数リストの開始トークンが不正です",
                    Some(span_from_token(start_token)),
                ));
            }

            let mut ranges = Vec::new();
            let mut depth_angle = 1usize;
            let mut depth_paren = 0usize;
            let mut depth_brace = 0usize;
            let mut depth_bracket = 0usize;
            let mut index = start_index + 1;
            let mut part_start = index;

            while index < self.tokens.len() {
                let token = self.tokens[index];
                match token.token_type {
                    TokenType::Less => depth_angle += 1,
                    TokenType::Greater => {
                        if depth_angle == 0 {
                            return Err(ExpressionError::new(
                                "型引数リストで `>` が余分に存在します",
                                Some(span_from_token(token)),
                            ));
                        }
                        depth_angle -= 1;
                        if depth_angle == 0 {
                            ranges.push((part_start, index));
                            return Ok((index, ranges));
                        }
                    }
                    TokenType::Comma | TokenType::LayoutComma
                        if depth_angle == 1
                            && depth_paren == 0
                            && depth_brace == 0
                            && depth_bracket == 0 =>
                    {
                        ranges.push((part_start, index));
                        part_start = index + 1;
                    }
                    TokenType::LeftParen => depth_paren += 1,
                    TokenType::RightParen => {
                        if depth_paren > 0 {
                            depth_paren -= 1;
                        }
                    }
                    TokenType::LeftBrace => depth_brace += 1,
                    TokenType::RightBrace => {
                        if depth_brace > 0 {
                            depth_brace -= 1;
                        }
                    }
                    TokenType::LeftBracket => depth_bracket += 1,
                    TokenType::RightBracket => {
                        if depth_bracket > 0 {
                            depth_bracket -= 1;
                        }
                    }
                    _ => {}
                }
                index += 1;
            }

            Err(ExpressionError::new(
                "`>` が必要です",
                self.span_at(start_index),
            ))
        }

        fn next_significant_kind(&self, index: usize) -> Option<&TokenType> {
            let mut idx = index + 1;
            while let Some(token) = self.tokens.get(idx) {
                match token.token_type {
                    TokenType::Whitespace(_)
                    | TokenType::Newline
                    | TokenType::LayoutComma
                    | TokenType::LineComment(_)
                    | TokenType::BlockComment(_)
                    | TokenType::JavaDocComment(_) => idx += 1,
                    _ => return Some(&token.token_type),
                }
            }
            None
        }

        fn peek_is_as_keyword(&self) -> bool {
            matches!(
                self.peek_token().map(|token| &token.token_type),
                Some(TokenType::Identifier(text)) if text == "as"
            )
        }

        fn parse_type_cast(&mut self, left: ParsedExpr) -> Result<ParsedExpr, ExpressionError> {
            let (as_token, _as_index) = self.advance_with_index_or_error("`as` が必要です")?;
            let as_span = span_from_token(as_token);

            while let Some(token) = self.tokens.get(self.pos) {
                match token.token_type {
                    TokenType::Whitespace(_)
                    | TokenType::Newline
                    | TokenType::LayoutComma
                    | TokenType::LineComment(_)
                    | TokenType::BlockComment(_)
                    | TokenType::JavaDocComment(_) => self.pos += 1,
                    _ => break,
                }
            }

            let start_index = self.pos;
            if start_index >= self.tokens.len() {
                return Err(ExpressionError::new(
                    "`as` の後に型注釈が必要です",
                    Some(as_span),
                ));
            }

            let mut end = start_index;
            let mut last_success: Option<(usize, TypeAnnotation, Option<Span>)> = None;
            let mut last_error: Option<(String, Option<Span>)> = None;

            while end < self.tokens.len() {
                end += 1;
                let owned_tokens: Vec<Token> = self.tokens[start_index..end]
                    .iter()
                    .map(|token| (*token).clone())
                    .collect();

                if owned_tokens.is_empty() {
                    continue;
                }

                match lower_type_annotation_from_tokens(&owned_tokens) {
                    Ok(lowered) => {
                        let annotation_span = lowered.span().cloned();
                        last_success = Some((end, lowered.into_annotation(), annotation_span));

                        if let Some(next_kind) = self.tokens.get(end).map(|token| &token.token_type)
                        {
                            if Self::is_type_annotation_boundary_kind(next_kind) {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    Err(error) => {
                        last_error = Some((error.message().to_string(), error.span().cloned()));
                        if let Some(_) = last_success {
                            if error.kind() == TypeLoweringErrorKind::Parse
                                && error.message().contains("余分なトークン")
                            {
                                break;
                            }
                        }
                    }
                }
            }

            let (end_index, annotation, annotation_span_opt) = match last_success {
                Some(result) => result,
                None => {
                    let span = last_error
                        .as_ref()
                        .and_then(|(_, span)| span.clone())
                        .or_else(|| self.span_at(start_index))
                        .or(Some(as_span));
                    let message = last_error
                        .map(|(msg, _)| msg)
                        .unwrap_or_else(|| "`as` の後に型注釈が必要です".to_string());
                    return Err(ExpressionError::new(message, span));
                }
            };

            self.pos = end_index;

            let annotation_span = annotation_span_opt
                .unwrap_or_else(|| span_for_range(self.tokens, start_index, end_index));
            let cast_span = merge_spans(&left.span(), &annotation_span);

            let expr = Expression::TypeCast {
                expr: Box::new(left.expr),
                target: annotation,
                span: cast_span.clone(),
            };

            Ok(ParsedExpr {
                expr,
                start: left.start,
                end: end_index,
            })
        }

        fn is_type_annotation_boundary_kind(kind: &TokenType) -> bool {
            matches!(
                kind,
                TokenType::Comma
                    | TokenType::LayoutComma
                    | TokenType::RightParen
                    | TokenType::RightBracket
                    | TokenType::RightBrace
                    | TokenType::Arrow
                    | TokenType::FatArrow
                    | TokenType::Semicolon
                    | TokenType::Dot
                    | TokenType::NullSafe
                    | TokenType::DoubleColon
                    | TokenType::Question
                    | TokenType::Colon
                    | TokenType::Plus
                    | TokenType::Minus
                    | TokenType::Multiply
                    | TokenType::Divide
                    | TokenType::Modulo
                    | TokenType::Equal
                    | TokenType::NotEqual
                    | TokenType::Less
                    | TokenType::LessEqual
                    | TokenType::Greater
                    | TokenType::GreaterEqual
                    | TokenType::And
                    | TokenType::Or
                    | TokenType::Elvis
                    | TokenType::RangeExclusive
                    | TokenType::RangeInclusive
                    | TokenType::Assign
                    | TokenType::LeftBrace
                    | TokenType::LeftBracket
            )
        }

        fn peek_binary_info(&self) -> Option<BinaryInfo> {
            let token = self.peek_token()?;
            binary_info_for_token(token)
        }

        fn parse_nested_expression(tokens: &[&Token]) -> Result<ParsedExpr, ExpressionError> {
            let mut nested = ExpressionParser::new(tokens);
            let parsed = nested.parse_expression_bp(0)?;
            nested
                .consume_postfix_if_any(parsed)
                .map(|expr| ParsedExpr {
                    expr,
                    start: 0,
                    end: tokens.len(),
                })
        }

        fn peek_token(&self) -> Option<&Token> {
            self.tokens.get(self.pos).copied()
        }

        fn peek_with_index(&self) -> Option<(&Token, usize)> {
            self.tokens.get(self.pos).map(|token| (*token, self.pos))
        }

        fn advance_with_index(&mut self) -> Option<(&Token, usize)> {
            let index = self.pos;
            let token = self.tokens.get(self.pos)?;
            self.pos += 1;
            Some((*token, index))
        }

        fn span_at(&self, index: usize) -> Option<Span> {
            self.tokens.get(index).map(|token| span_from_token(token))
        }

        fn advance_with_index_or_error(
            &mut self,
            message: &str,
        ) -> Result<(&Token, usize), ExpressionError> {
            match self.advance_with_index() {
                Some(pair) => Ok(pair),
                None => Err(ExpressionError::new(message, None)),
            }
        }

        fn find_matching_paren(&self, open_index: usize) -> Option<usize> {
            let mut depth = 0isize;
            for (idx, token) in self.tokens.iter().enumerate().skip(open_index) {
                match token.token_type {
                    TokenType::LeftParen => depth += 1,
                    TokenType::RightParen => {
                        depth -= 1;
                        if depth == 0 {
                            return Some(idx);
                        }
                    }
                    _ => {}
                }
            }
            None
        }

        fn find_matching_brace(&self, open_index: usize) -> Option<usize> {
            let mut depth = 0isize;
            for (idx, token) in self.tokens.iter().enumerate().skip(open_index) {
                match token.token_type {
                    TokenType::LeftBrace => depth += 1,
                    TokenType::RightBrace => {
                        depth -= 1;
                        if depth == 0 {
                            return Some(idx);
                        }
                    }
                    _ => {}
                }
            }
            None
        }

        fn find_matching_bracket(&self, open_index: usize) -> Option<usize> {
            let mut depth = 0isize;
            for (idx, token) in self.tokens.iter().enumerate().skip(open_index) {
                match token.token_type {
                    TokenType::LeftBracket => depth += 1,
                    TokenType::RightBracket => {
                        depth -= 1;
                        if depth == 0 {
                            return Some(idx);
                        }
                    }
                    _ => {}
                }
            }
            None
        }

        fn find_top_level_arrow(tokens: &[&Token]) -> Option<usize> {
            let mut depth_paren = 0usize;
            let mut depth_brace = 0usize;
            let mut depth_bracket = 0usize;
            for (idx, token) in tokens.iter().enumerate() {
                match token.token_type {
                    TokenType::LeftParen => depth_paren += 1,
                    TokenType::RightParen if depth_paren > 0 => depth_paren -= 1,
                    TokenType::LeftBrace => depth_brace += 1,
                    TokenType::RightBrace if depth_brace > 0 => depth_brace -= 1,
                    TokenType::LeftBracket => depth_bracket += 1,
                    TokenType::RightBracket if depth_bracket > 0 => depth_bracket -= 1,
                    TokenType::Arrow
                        if depth_paren == 0 && depth_brace == 0 && depth_bracket == 0 =>
                    {
                        return Some(idx);
                    }
                    _ => {}
                }
            }
            None
        }
    }

    fn array_comma_error_message() -> String {
        "JV2101: 配列リテラルでカンマ区切りはサポートされません。空白または改行のみで要素を分けてください。\nJV2101: Array literals do not support comma separators. Use whitespace or newlines between elements.\nQuick Fix: arrays.whitespace.remove-commas -> [a b c]（例: [1, 2, 3] => [1 2 3])\nQuick Fix: arrays.whitespace.remove-commas -> [a b c] (Example: [1, 2, 3] => [1 2 3])\nDoc: docs/whitespace-arrays.md".to_string()
    }

    #[derive(Clone, Copy)]
    enum PostfixKind {
        MemberAccess,
        NullSafeMemberAccess,
        Index,
        NullSafeIndex,
        TypeArguments,
        Call,
        TrailingLambda,
    }

    const PREFIX_PRECEDENCE: u8 = 8;

    fn span_from_index(tokens: &[&Token], index: usize) -> Span {
        tokens
            .get(index)
            .map(|token| span_from_token(token))
            .unwrap_or_else(Span::dummy)
    }

    fn binary_info_for_token(token: &Token) -> Option<BinaryInfo> {
        match &token.token_type {
            TokenType::Plus => Some(BinaryInfo {
                op: BinaryOp::Add,
                precedence: 6,
                associativity: Associativity::Left,
            }),
            TokenType::Minus => Some(BinaryInfo {
                op: BinaryOp::Subtract,
                precedence: 6,
                associativity: Associativity::Left,
            }),
            TokenType::Multiply => Some(BinaryInfo {
                op: BinaryOp::Multiply,
                precedence: 7,
                associativity: Associativity::Left,
            }),
            TokenType::Divide => Some(BinaryInfo {
                op: BinaryOp::Divide,
                precedence: 7,
                associativity: Associativity::Left,
            }),
            TokenType::Modulo => Some(BinaryInfo {
                op: BinaryOp::Modulo,
                precedence: 7,
                associativity: Associativity::Left,
            }),
            TokenType::Equal => Some(BinaryInfo {
                op: BinaryOp::Equal,
                precedence: 4,
                associativity: Associativity::Left,
            }),
            TokenType::NotEqual => Some(BinaryInfo {
                op: BinaryOp::NotEqual,
                precedence: 4,
                associativity: Associativity::Left,
            }),
            TokenType::Less => Some(BinaryInfo {
                op: BinaryOp::Less,
                precedence: 5,
                associativity: Associativity::Left,
            }),
            TokenType::LessEqual => Some(BinaryInfo {
                op: BinaryOp::LessEqual,
                precedence: 5,
                associativity: Associativity::Left,
            }),
            TokenType::Greater => Some(BinaryInfo {
                op: BinaryOp::Greater,
                precedence: 5,
                associativity: Associativity::Left,
            }),
            TokenType::GreaterEqual => Some(BinaryInfo {
                op: BinaryOp::GreaterEqual,
                precedence: 5,
                associativity: Associativity::Left,
            }),
            TokenType::Identifier(name) if name == "is" => Some(BinaryInfo {
                op: BinaryOp::Is,
                precedence: 4,
                associativity: Associativity::Left,
            }),
            TokenType::And => Some(BinaryInfo {
                op: BinaryOp::And,
                precedence: 2,
                associativity: Associativity::Left,
            }),
            TokenType::Or => Some(BinaryInfo {
                op: BinaryOp::Or,
                precedence: 1,
                associativity: Associativity::Left,
            }),
            TokenType::RangeExclusive => Some(BinaryInfo {
                op: BinaryOp::RangeExclusive,
                precedence: 5,
                associativity: Associativity::Left,
            }),
            TokenType::RangeInclusive => Some(BinaryInfo {
                op: BinaryOp::RangeInclusive,
                precedence: 5,
                associativity: Associativity::Left,
            }),
            TokenType::Elvis => Some(BinaryInfo {
                op: BinaryOp::Elvis,
                precedence: 3,
                associativity: Associativity::Left,
            }),
            _ => None,
        }
    }

    fn span_for_range(tokens: &[&Token], start: usize, end: usize) -> Span {
        if start >= end || end == 0 {
            return span_from_token(tokens[start.min(tokens.len().saturating_sub(1))]);
        }
        let start_span = span_from_token(tokens[start]);
        let end_span = span_from_token(tokens[end - 1]);
        merge_spans(&start_span, &end_span)
    }

    fn count_interpolation_expression_segments(token: &Token) -> usize {
        token
            .metadata
            .iter()
            .find_map(|metadata| match metadata {
                TokenMetadata::StringInterpolation { segments } => Some(
                    segments
                        .iter()
                        .filter(|segment| {
                            matches!(segment, StringInterpolationSegment::Expression(_))
                        })
                        .count(),
                ),
                _ => None,
            })
            .unwrap_or(0)
    }

    fn parse_string_token_with_metadata(
        token: &Token,
        span: Span,
    ) -> Result<(Expression, usize), ExpressionError> {
        let segments = token.metadata.iter().find_map(|metadata| match metadata {
            TokenMetadata::StringInterpolation { segments } => Some(segments.clone()),
            _ => None,
        });
        let metadata = string_literal_metadata(token);

        if let Some(segments) = segments {
            let mut parts = Vec::new();
            let mut normalized = String::new();
            let mut token_consumption = 0usize;

            for segment in segments {
                match segment {
                    StringInterpolationSegment::Literal(text) => {
                        if !text.is_empty() {
                            parts.push(StringPart::Text(text.clone()));
                        }
                        normalized.push_str(&text);
                    }
                    StringInterpolationSegment::Expression(expr_source) => {
                        let (expr, consumed) =
                            parse_expression_from_text(&expr_source, span.clone())?;
                        token_consumption = token_consumption.saturating_add(consumed);
                        parts.push(StringPart::Expression(expr));
                    }
                }
            }

            if let Some(meta) = metadata {
                if let Some(kind) = multiline_kind_from_metadata(meta) {
                    let literal = build_multiline_literal(
                        kind,
                        normalized,
                        token.lexeme.clone(),
                        parts,
                        span.clone(),
                        metadata,
                    );
                    return Ok((Expression::MultilineString(literal), token_consumption));
                }
            }

            return Ok((
                Expression::StringInterpolation { parts, span },
                token_consumption,
            ));
        }

        let value = match &token.token_type {
            TokenType::StringInterpolation(value) | TokenType::String(value) => value.clone(),
            _ => token.lexeme.clone(),
        };

        if let Some(meta) = metadata {
            if let Some(kind) = multiline_kind_from_metadata(meta) {
                let literal = build_multiline_literal(
                    kind,
                    value.clone(),
                    token.lexeme.clone(),
                    Vec::new(),
                    span.clone(),
                    metadata,
                );
                return Ok((Expression::MultilineString(literal), 0));
            }
        }

        Ok((Expression::Literal(Literal::String(value), span), 0))
    }

    fn parse_expression_from_text(
        source: &str,
        span: Span,
    ) -> Result<(Expression, usize), ExpressionError> {
        let mut lexer = Lexer::with_layout_mode(source.to_string(), LayoutMode::Disabled);
        let tokens = lexer.tokenize().map_err(|_| {
            ExpressionError::new("文字列補間式の字句解析に失敗しました", Some(span.clone()))
        })?;

        let filtered: Vec<&Token> = tokens
            .iter()
            .filter(|token| !matches!(token.token_type, TokenType::Eof | TokenType::LayoutComma))
            .collect();

        if filtered.is_empty() {
            return Err(ExpressionError::new("文字列補間式が空です", Some(span)));
        }

        let expr = parse_expression(filtered.as_slice())?;
        Ok((expr, filtered.len()))
    }

    fn parse_json_literal_tokens(tokens: &[&Token]) -> Result<JsonLiteral, ExpressionError> {
        if tokens.is_empty() {
            return Err(ExpressionError::new("JSON リテラルが空です", None));
        }

        let mut parser = JsonParser::new(tokens);
        parser.parse_literal()
    }

    fn string_literal_metadata(token: &Token) -> Option<&StringLiteralMetadata> {
        token.metadata.iter().find_map(|metadata| match metadata {
            TokenMetadata::StringLiteral(data) => Some(data),
            _ => None,
        })
    }

    fn multiline_kind_from_metadata(metadata: &StringLiteralMetadata) -> Option<MultilineKind> {
        match metadata.delimiter {
            StringDelimiterKind::TripleQuote => Some(MultilineKind::TripleQuote),
            StringDelimiterKind::BacktickBlock => Some(MultilineKind::Backtick),
            StringDelimiterKind::SingleQuote if metadata.is_raw => Some(MultilineKind::RawSingle),
            StringDelimiterKind::TripleSingleQuote if metadata.is_raw => {
                Some(MultilineKind::RawTriple)
            }
            _ => None,
        }
    }

    fn build_multiline_literal(
        kind: MultilineKind,
        normalized: String,
        raw: String,
        parts: Vec<StringPart>,
        span: Span,
        metadata: Option<&StringLiteralMetadata>,
    ) -> MultilineStringLiteral {
        MultilineStringLiteral {
            kind,
            normalized,
            raw,
            parts,
            indent: None,
            raw_flavor: metadata.and_then(raw_flavor_from_metadata),
            span,
        }
    }

    fn raw_flavor_from_metadata(metadata: &StringLiteralMetadata) -> Option<RawStringFlavor> {
        metadata.raw_flavor.map(|flavor| match flavor {
            LexerRawStringFlavor::SingleLine => RawStringFlavor::SingleLine,
            LexerRawStringFlavor::MultiLine => RawStringFlavor::MultiLine,
        })
    }

    fn has_high_json_confidence(token: &Token) -> bool {
        token.metadata.iter().any(|metadata| match metadata {
            TokenMetadata::PotentialJsonStart { confidence } => {
                matches!(confidence, JsonConfidence::High | JsonConfidence::Medium)
            }
            _ => false,
        })
    }

    fn collect_json_comments(trivia: &TokenTrivia) -> Vec<JsonComment> {
        trivia
            .json_comments
            .iter()
            .map(|comment| JsonComment {
                kind: match comment.kind {
                    JsonCommentTriviaKind::Line => JsonCommentKind::Line,
                    JsonCommentTriviaKind::Block => JsonCommentKind::Block,
                },
                text: comment.text.clone(),
                span: comment_span(comment),
            })
            .collect()
    }

    fn comment_span(comment: &JsonCommentTrivia) -> Span {
        let width = comment.text.chars().count().max(1);
        Span::new(
            comment.line,
            comment.column,
            comment.line,
            comment.column + width,
        )
    }

    fn number_grouping_from_token(token: &Token) -> NumberGrouping {
        token
            .metadata
            .iter()
            .find_map(|metadata| match metadata {
                TokenMetadata::NumberLiteral(info) => Some(match info.grouping {
                    NumberGroupingKind::None => NumberGrouping::None,
                    NumberGroupingKind::Comma => NumberGrouping::Comma,
                    NumberGroupingKind::Underscore => NumberGrouping::Underscore,
                    NumberGroupingKind::Mixed => NumberGrouping::Mixed,
                }),
                _ => None,
            })
            .unwrap_or_default()
    }

    fn json_value_span(value: &JsonValue) -> Span {
        match value {
            JsonValue::Object { span, .. }
            | JsonValue::Array { span, .. }
            | JsonValue::String { span, .. }
            | JsonValue::Number { span, .. }
            | JsonValue::Boolean { span, .. }
            | JsonValue::Null { span } => span.clone(),
        }
    }

    fn build_json_literal_from_object(
        left: &Token,
        entries: Vec<JsonEntry>,
        right: &Token,
    ) -> JsonLiteral {
        let left_span = span_from_token(left);
        let right_span = span_from_token(right);
        let span = merge_spans(&left_span, &right_span);

        JsonLiteral {
            value: JsonValue::Object {
                entries,
                span: span.clone(),
            },
            leading_comments: collect_json_comments(&left.leading_trivia),
            trailing_comments: collect_json_comments(&right.leading_trivia),
            span,
            inferred_schema: None,
        }
    }

    fn build_json_literal_from_array(
        left: &Token,
        elements: Vec<JsonValue>,
        right: &Token,
    ) -> JsonLiteral {
        let left_span = span_from_token(left);
        let right_span = span_from_token(right);
        let span = merge_spans(&left_span, &right_span);

        JsonLiteral {
            value: JsonValue::Array {
                elements,
                delimiter: SequenceDelimiter::Comma,
                span: span.clone(),
            },
            leading_comments: collect_json_comments(&left.leading_trivia),
            trailing_comments: collect_json_comments(&right.leading_trivia),
            span,
            inferred_schema: None,
        }
    }

    struct JsonParser<'a> {
        tokens: &'a [&'a Token],
        pos: usize,
    }

    impl<'a> JsonParser<'a> {
        fn new(tokens: &'a [&'a Token]) -> Self {
            Self { tokens, pos: 0 }
        }

        fn parse_literal(&mut self) -> Result<JsonLiteral, ExpressionError> {
            self.skip_trivia();
            let token = self
                .peek()
                .ok_or_else(|| ExpressionError::new("JSON リテラルが空です", None))?;

            match token.token_type {
                TokenType::LeftBrace => self.parse_object_literal(),
                TokenType::LeftBracket => self.parse_array_literal(),
                _ => Err(ExpressionError::new(
                    "JSON リテラルの開始トークンが不正です",
                    Some(span_from_token(token)),
                )),
            }
        }

        fn parse_object_literal(&mut self) -> Result<JsonLiteral, ExpressionError> {
            let (left, entries, right) = self.parse_object_components()?;
            Ok(build_json_literal_from_object(left, entries, right))
        }

        fn parse_array_literal(&mut self) -> Result<JsonLiteral, ExpressionError> {
            let (left, elements, right) = self.parse_array_components()?;
            Ok(build_json_literal_from_array(left, elements, right))
        }

        fn parse_object_components(
            &mut self,
        ) -> Result<(&'a Token, Vec<JsonEntry>, &'a Token), ExpressionError> {
            self.skip_trivia();
            let left =
                self.expect_token(|ty| matches!(ty, TokenType::LeftBrace), "'{' が必要です")?;
            self.skip_trivia();

            let mut entries = Vec::new();

            if matches!(self.peek_type(), Some(TokenType::RightBrace)) {
                let right = self
                    .advance()
                    .ok_or_else(|| ExpressionError::new("'}' が必要です", None))?;
                return Ok((left, entries, right));
            }

            loop {
                let entry = self.parse_entry()?;
                entries.push(entry);
                self.skip_trivia();

                match self.peek_type() {
                    Some(TokenType::Comma) | Some(TokenType::LayoutComma) => {
                        self.pos += 1;
                        self.skip_trivia();
                        if matches!(self.peek_type(), Some(TokenType::RightBrace)) {
                            break;
                        }
                    }
                    Some(TokenType::RightBrace) => break,
                    _ => {
                        let span = self.peek().map(|token| span_from_token(token));
                        return Err(ExpressionError::new(
                            "JSON オブジェクトの要素区切りが不正です",
                            span,
                        ));
                    }
                }
            }

            self.skip_trivia();
            let right =
                self.expect_token(|ty| matches!(ty, TokenType::RightBrace), "'}' が必要です")?;
            Ok((left, entries, right))
        }

        fn parse_array_components(
            &mut self,
        ) -> Result<(&'a Token, Vec<JsonValue>, &'a Token), ExpressionError> {
            self.skip_trivia();
            let left =
                self.expect_token(|ty| matches!(ty, TokenType::LeftBracket), "'[' が必要です")?;
            self.skip_trivia();

            let mut elements = Vec::new();

            if matches!(self.peek_type(), Some(TokenType::RightBracket)) {
                let right = self
                    .advance()
                    .ok_or_else(|| ExpressionError::new("']' が必要です", None))?;
                return Ok((left, elements, right));
            }

            loop {
                let value = self.parse_value()?;
                elements.push(value);
                self.skip_trivia();

                match self.peek_type() {
                    Some(TokenType::Comma) | Some(TokenType::LayoutComma) => {
                        self.pos += 1;
                        self.skip_trivia();
                        if matches!(self.peek_type(), Some(TokenType::RightBracket)) {
                            break;
                        }
                    }
                    Some(TokenType::RightBracket) => break,
                    _ => {
                        let span = self.peek().map(|token| span_from_token(token));
                        return Err(ExpressionError::new(
                            "JSON 配列の要素区切りが不正です",
                            span,
                        ));
                    }
                }
            }

            self.skip_trivia();
            let right =
                self.expect_token(|ty| matches!(ty, TokenType::RightBracket), "']' が必要です")?;
            Ok((left, elements, right))
        }

        fn parse_entry(&mut self) -> Result<JsonEntry, ExpressionError> {
            self.skip_trivia();
            let key_token = self
                .advance()
                .ok_or_else(|| ExpressionError::new("JSON オブジェクトのキーが必要です", None))?;

            let key = match &key_token.token_type {
                TokenType::String(value) => value.clone(),
                TokenType::Identifier(value) => value.clone(),
                _ => {
                    return Err(ExpressionError::new(
                        "JSON オブジェクトのキーは識別子または文字列である必要があります",
                        Some(span_from_token(key_token)),
                    ))
                }
            };

            self.skip_trivia();
            let colon = self
                .advance()
                .ok_or_else(|| ExpressionError::new("':' が必要です", None))?;
            if !matches!(colon.token_type, TokenType::Colon) {
                return Err(ExpressionError::new(
                    "':' が必要です",
                    Some(span_from_token(colon)),
                ));
            }

            self.skip_trivia();
            let value = self.parse_value()?;

            let key_span = span_from_token(key_token);
            let value_span = json_value_span(&value);
            let span = merge_spans(&key_span, &value_span);
            let comments = collect_json_comments(&key_token.leading_trivia);

            Ok(JsonEntry {
                key,
                comments,
                value,
                span,
            })
        }

        fn parse_value(&mut self) -> Result<JsonValue, ExpressionError> {
            self.skip_trivia();
            let token = self
                .peek()
                .ok_or_else(|| ExpressionError::new("JSON 値が必要です", None))?;

            match token.token_type {
                TokenType::LeftBrace => {
                    let (left, entries, right) = self.parse_object_components()?;
                    let left_span = span_from_token(left);
                    let right_span = span_from_token(right);
                    let span = merge_spans(&left_span, &right_span);
                    Ok(JsonValue::Object { entries, span })
                }
                TokenType::LeftBracket => {
                    let (left, elements, right) = self.parse_array_components()?;
                    let left_span = span_from_token(left);
                    let right_span = span_from_token(right);
                    let span = merge_spans(&left_span, &right_span);
                    Ok(JsonValue::Array {
                        elements,
                        delimiter: SequenceDelimiter::Comma,
                        span,
                    })
                }
                TokenType::String(_) => {
                    let token = self.advance().unwrap();
                    let value = match &token.token_type {
                        TokenType::String(content) => content.clone(),
                        _ => unreachable!(),
                    };
                    let span = span_from_token(token);
                    Ok(JsonValue::String { value, span })
                }
                TokenType::Number(_) => {
                    let token = self.advance().unwrap();
                    let literal = match &token.token_type {
                        TokenType::Number(value) => value.clone(),
                        _ => unreachable!(),
                    };
                    let grouping = number_grouping_from_token(token);
                    let span = span_from_token(token);
                    Ok(JsonValue::Number {
                        literal,
                        grouping,
                        span,
                    })
                }
                TokenType::Boolean(value) => {
                    let token = self.advance().unwrap();
                    let span = span_from_token(token);
                    Ok(JsonValue::Boolean { value, span })
                }
                TokenType::True | TokenType::False => {
                    let token = self.advance().unwrap();
                    let span = span_from_token(token);
                    let value = matches!(token.token_type, TokenType::True);
                    Ok(JsonValue::Boolean { value, span })
                }
                TokenType::Null => {
                    let token = self.advance().unwrap();
                    let span = span_from_token(token);
                    Ok(JsonValue::Null { span })
                }
                _ => Err(ExpressionError::new(
                    "JSON 値が不正です",
                    Some(span_from_token(token)),
                )),
            }
        }

        fn skip_trivia(&mut self) {
            while let Some(token) = self.peek() {
                match token.token_type {
                    TokenType::Whitespace(_)
                    | TokenType::Newline
                    | TokenType::LineComment(_)
                    | TokenType::BlockComment(_)
                    | TokenType::JavaDocComment(_) => self.pos += 1,
                    _ => break,
                }
            }
        }

        fn expect_token<F>(
            &mut self,
            mut predicate: F,
            message: &str,
        ) -> Result<&'a Token, ExpressionError>
        where
            F: FnMut(&TokenType) -> bool,
        {
            let token = self
                .advance()
                .ok_or_else(|| ExpressionError::new(message, None))?;
            if predicate(&token.token_type) {
                Ok(token)
            } else {
                Err(ExpressionError::new(message, Some(span_from_token(token))))
            }
        }

        fn peek(&self) -> Option<&'a Token> {
            self.tokens.get(self.pos).copied()
        }

        fn peek_type(&self) -> Option<&TokenType> {
            self.peek().map(|token| &token.token_type)
        }

        fn advance(&mut self) -> Option<&'a Token> {
            let token = self.tokens.get(self.pos).copied();
            if token.is_some() {
                self.pos += 1;
            }
            token
        }
    }
}

fn detect_is_test_metadata(right: &Expression, span: &Span) -> Option<IsTestMetadata> {
    if let Some(literal) = extract_regex_literal(right) {
        return Some(IsTestMetadata {
            kind: IsTestKind::RegexLiteral,
            regex: Some(literal),
            pattern_expr: None,
            diagnostics: Vec::new(),
            guard_strategy: RegexGuardStrategy::None,
            span: span.clone(),
        });
    }

    if expression_looks_like_type_reference(right) {
        return Some(IsTestMetadata {
            kind: IsTestKind::Type,
            regex: None,
            pattern_expr: None,
            diagnostics: Vec::new(),
            guard_strategy: RegexGuardStrategy::None,
            span: span.clone(),
        });
    }

    Some(IsTestMetadata {
        kind: IsTestKind::PatternExpression,
        regex: None,
        pattern_expr: Some(Box::new(right.clone())),
        diagnostics: Vec::new(),
        guard_strategy: RegexGuardStrategy::None,
        span: span.clone(),
    })
}

fn extract_regex_literal(expr: &Expression) -> Option<RegexLiteral> {
    match expr {
        Expression::RegexLiteral(literal) => Some(literal.clone()),
        Expression::Literal(Literal::Regex(literal), _) => Some(literal.clone()),
        _ => None,
    }
}

fn expression_looks_like_type_reference(expr: &Expression) -> bool {
    match expr {
        Expression::Identifier(name, _) => is_probably_type_name(name),
        Expression::MemberAccess {
            object, property, ..
        } => {
            is_probably_type_name(property)
                && matches!(
                    object.as_ref(),
                    Expression::Identifier(_, _) | Expression::MemberAccess { .. }
                )
        }
        _ => false,
    }
}

fn is_probably_type_name(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(first) if first.is_uppercase() => true,
        _ => false,
    }
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

fn split_when_guard(pattern: Pattern) -> (Pattern, Option<Expression>) {
    match pattern {
        Pattern::Guard {
            pattern, condition, ..
        } => (*pattern, Some(condition)),
        other => (other, None),
    }
}

fn pattern_span(pattern: &Pattern) -> Span {
    match pattern {
        Pattern::Literal(_, span)
        | Pattern::Identifier(_, span)
        | Pattern::Wildcard(span)
        | Pattern::Constructor { span, .. }
        | Pattern::Range { span, .. }
        | Pattern::Guard { span, .. } => span.clone(),
    }
}

fn find_matching_paren_in_slice(tokens: &[&Token], open_index: usize) -> Option<usize> {
    let mut depth = 0isize;
    for (idx, token) in tokens.iter().enumerate().skip(open_index) {
        match token.token_type {
            TokenType::LeftParen => depth += 1,
            TokenType::RightParen => {
                depth -= 1;
                if depth == 0 {
                    return Some(idx);
                }
            }
            _ => {}
        }
    }
    None
}

fn span_for_range_slice(slice: &[&Token]) -> Span {
    let start = slice
        .first()
        .map(|token| span_from_token(token))
        .unwrap_or_else(Span::dummy);
    let end = slice
        .last()
        .map(|token| span_from_token(token))
        .unwrap_or_else(Span::dummy);
    merge_spans(&start, &end)
}

fn collect_function_signature_tokens<'a>(tokens: &'a [&'a Token]) -> Vec<&'a Token> {
    let mut fun_index: Option<usize> = None;

    for (idx, token) in tokens.iter().enumerate() {
        if matches!(token.token_type, TokenType::Fun) {
            fun_index = Some(idx);
        }
    }

    let Some(fun) = fun_index else {
        return Vec::new();
    };

    let mut result = Vec::new();
    let mut idx = fun + 1;
    let mut generic_depth = 0usize;

    while idx < tokens.len() {
        let token = tokens[idx];
        match token.token_type {
            TokenType::Whitespace(_) | TokenType::Newline => {
                idx += 1;
                continue;
            }
            TokenType::LeftParen if generic_depth == 0 => break,
            TokenType::LeftBrace | TokenType::Assign | TokenType::Arrow | TokenType::Where => {
                if generic_depth == 0 {
                    break;
                }
            }
            TokenType::Colon if generic_depth == 0 => break,
            TokenType::Less => {
                generic_depth = generic_depth.saturating_add(1);
            }
            TokenType::Greater => {
                if generic_depth == 0 {
                    break;
                }
                generic_depth = generic_depth.saturating_sub(1);
            }
            _ => {}
        }

        result.push(token);
        idx += 1;
    }

    result
}

fn split_generic_segment<'a>(
    tokens: &'a [&'a Token],
) -> (Option<&'a [&'a Token]>, &'a [&'a Token]) {
    if tokens
        .first()
        .map(|token| matches!(token.token_type, TokenType::Less))
        .unwrap_or(false)
    {
        let mut depth = 0usize;
        for (idx, token) in tokens.iter().enumerate() {
            match token.token_type {
                TokenType::Less => depth = depth.saturating_add(1),
                TokenType::Greater => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        let generics = &tokens[..=idx];
                        let rest = &tokens[idx + 1..];
                        return (Some(generics), rest);
                    }
                }
                _ => {}
            }
        }
    }
    (None, tokens)
}

fn extract_type_parameter_names(tokens: Option<&[&Token]>) -> Vec<String> {
    let mut names = Vec::new();
    let Some(tokens) = tokens else {
        return names;
    };

    let mut depth = 0usize;
    let mut expect_name = true;
    for token in tokens {
        match token.token_type {
            TokenType::Less => {
                depth = depth.saturating_add(1);
                expect_name = true;
            }
            TokenType::Greater => {
                if depth > 0 {
                    depth -= 1;
                }
                expect_name = depth == 1;
            }
            TokenType::Comma | TokenType::LayoutComma => {
                if depth == 1 {
                    expect_name = true;
                }
            }
            TokenType::Colon if depth == 1 => {
                expect_name = false;
            }
            TokenType::Identifier(ref name) if depth == 1 && expect_name => {
                names.push(name.clone());
                expect_name = false;
            }
            _ => {}
        }
    }
    names
}

fn lower_generic_signature_tokens(tokens: Option<&[&Token]>) -> Option<GenericSignature> {
    let tokens = tokens?;
    if tokens.len() < 3 {
        return None;
    }

    let mut parameters = Vec::new();
    let mut index = 1usize;
    let end = tokens.len() - 1;

    while index < end {
        while index < end
            && matches!(
                tokens[index].token_type,
                TokenType::Comma | TokenType::LayoutComma
            )
        {
            index += 1;
        }

        if index >= end {
            break;
        }

        let param_start_index = index;

        let variance = match &tokens[index].token_type {
            TokenType::Identifier(text) if text == "out" => {
                index += 1;
                Some(VarianceMarker::Covariant)
            }
            TokenType::Identifier(text) if text == "in" => {
                index += 1;
                Some(VarianceMarker::Contravariant)
            }
            _ => None,
        };

        if index >= end {
            break;
        }

        let name_token = tokens[index];
        let name = match &name_token.token_type {
            TokenType::Identifier(text) => text.clone(),
            _ => break,
        };
        index += 1;

        let mut bounds = Vec::new();

        if index < end && matches!(tokens[index].token_type, TokenType::Colon) {
            index += 1;
            loop {
                let (annotation_tokens, next_index) =
                    collect_type_annotation_tokens(tokens, index, end);
                if annotation_tokens.is_empty() {
                    index = next_index;
                    break;
                }

                match lower_type_annotation_from_tokens(&annotation_tokens) {
                    Ok(lowered) => bounds.push(lowered.annotation().clone()),
                    Err(_) => return None,
                }

                index = next_index;

                break;
            }
        }

        let default = if index < end && matches!(tokens[index].token_type, TokenType::Assign) {
            index += 1;
            let (default_tokens, next_index) = collect_type_annotation_tokens(tokens, index, end);
            index = next_index;
            if default_tokens.is_empty() {
                None
            } else {
                match lower_type_annotation_from_tokens(&default_tokens) {
                    Ok(lowered) => Some(lowered.annotation().clone()),
                    Err(_) => None,
                }
            }
        } else {
            None
        };

        let param_end_index = if index > param_start_index {
            index - 1
        } else {
            param_start_index
        };
        let start_span = span_from_token(tokens[param_start_index]);
        let end_span = span_from_token(tokens[param_end_index.min(end - 1)]);
        let param_span = merge_spans(&start_span, &end_span);

        parameters.push(GenericParameter {
            name,
            bounds,
            variance,
            default,
            kind: None,
            span: param_span,
        });
    }

    if parameters.is_empty() {
        return None;
    }

    let signature_span = merge_spans(
        &span_from_token(tokens.first().unwrap()),
        &span_from_token(tokens.last().unwrap()),
    );

    Some(GenericSignature {
        parameters,
        const_parameters: Vec::new(),
        where_clause: None,
        raw_directives: Vec::new(),
        span: signature_span,
    })
}

fn collect_type_annotation_tokens(
    tokens: &[&Token],
    mut index: usize,
    end: usize,
) -> (Vec<Token>, usize) {
    let mut collected = Vec::new();
    let mut depth = 0usize;

    while index < end {
        let token = tokens[index];
        match token.token_type {
            TokenType::Comma | TokenType::LayoutComma if depth == 0 => break,
            TokenType::Assign if depth == 0 => break,
            TokenType::Where if depth == 0 => break,
            TokenType::Less | TokenType::LeftParen | TokenType::LeftBracket => {
                depth = depth.saturating_add(1);
            }
            TokenType::Greater | TokenType::RightParen | TokenType::RightBracket => {
                depth = depth.saturating_sub(1);
            }
            _ => {}
        }

        collected.push(token.clone());
        index += 1;
    }

    (collected, index)
}

fn locate_function_name_index(tokens: &[&Token]) -> Option<usize> {
    let mut depth = 0usize;
    for (index, token) in tokens.iter().enumerate().rev() {
        match token.token_type {
            TokenType::Greater => depth = depth.saturating_add(1),
            TokenType::Less => {
                if depth == 0 {
                    continue;
                }
                depth = depth.saturating_sub(1);
            }
            _ if depth > 0 => continue,
            TokenType::Identifier(_) => return Some(index),
            _ => continue,
        }
    }
    None
}

fn receiver_token_slice<'a>(tokens: &'a [&'a Token], name_index: usize) -> Option<&'a [&'a Token]> {
    if name_index == 0 {
        return None;
    }

    let mut depth = 0usize;
    for idx in (0..name_index).rev() {
        match tokens[idx].token_type {
            TokenType::Greater => depth = depth.saturating_add(1),
            TokenType::Less => {
                if depth > 0 {
                    depth = depth.saturating_sub(1);
                }
            }
            _ if depth > 0 => continue,
            TokenType::Dot => return Some(&tokens[..idx]),
            _ => {}
        }
    }

    None
}

fn clone_tokens(slice: &[&Token]) -> Vec<Token> {
    slice.iter().map(|token| (*token).clone()).collect()
}

fn lower_function(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let tokens = context.tokens_for(node);
    let span = context.span_for(node).unwrap_or_else(Span::dummy);

    let return_type_token_ptrs: Vec<*const Token> =
        child_node(node, SyntaxKind::FunctionReturnType)
            .map(|ret| {
                context
                    .tokens_for(&ret)
                    .into_iter()
                    .map(|token| token as *const Token)
                    .collect()
            })
            .unwrap_or_default();

    let mut deprecated_arrow_index: Option<usize> = None;
    let mut depth_paren = 0usize;
    let mut depth_brace = 0usize;
    let mut depth_bracket = 0usize;

    for (idx, token) in tokens.iter().enumerate() {
        let token = *token;
        match token.token_type {
            TokenType::LeftParen => depth_paren = depth_paren.saturating_add(1),
            TokenType::RightParen => {
                if depth_paren > 0 {
                    depth_paren -= 1;
                }
            }
            TokenType::LeftBrace => {
                if depth_paren == 0 && depth_brace == 0 && depth_bracket == 0 {
                    break;
                }
                depth_brace = depth_brace.saturating_add(1);
            }
            TokenType::RightBrace => {
                if depth_brace > 0 {
                    depth_brace -= 1;
                }
            }
            TokenType::LeftBracket => depth_bracket = depth_bracket.saturating_add(1),
            TokenType::RightBracket => {
                if depth_bracket > 0 {
                    depth_bracket -= 1;
                }
            }
            TokenType::Assign => break,
            TokenType::Arrow => {
                let ptr = token as *const Token;
                let belongs_to_return_type =
                    return_type_token_ptrs.iter().any(|&ret_ptr| ret_ptr == ptr);
                if depth_paren == 0
                    && depth_brace == 0
                    && depth_bracket == 0
                    && !belongs_to_return_type
                {
                    deprecated_arrow_index = Some(idx);
                    break;
                }
            }
            TokenType::Eof => break,
            _ => {}
        }
    }

    if let Some(idx) = deprecated_arrow_index {
        let arrow_span = Some(span_from_token(tokens[idx]));
        diagnostics.push(LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "関数宣言では `->` 式ボディはサポートされません。`=` を使用してください",
            arrow_span,
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        ));
    }

    let signature_tokens = collect_function_signature_tokens(&tokens);
    let (generic_segment, signature_tail) = split_generic_segment(signature_tokens.as_slice());
    let generic_signature = lower_generic_signature_tokens(generic_segment);
    let type_parameters = if let Some(signature) = &generic_signature {
        signature
            .parameters
            .iter()
            .map(|param| param.name.clone())
            .collect()
    } else {
        extract_type_parameter_names(generic_segment)
    };

    let name_index = locate_function_name_index(signature_tail).ok_or_else(|| {
        LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "関数名を特定できませんでした",
            context.span_for(node),
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        )
    })?;

    let name_token = signature_tail[name_index];
    let name = if let TokenType::Identifier(text) = &name_token.token_type {
        text.clone()
    } else {
        return Err(LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "関数名を特定できませんでした",
            context.span_for(node),
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        ));
    };

    let receiver_type = receiver_token_slice(signature_tail, name_index).and_then(|slice| {
        if slice.is_empty() {
            return None;
        }
        let owned_tokens = clone_tokens(slice);
        match lower_type_annotation_from_tokens(&owned_tokens) {
            Ok(lowered) => Some(lowered.into_annotation()),
            Err(error) => {
                diagnostics.push(LoweringDiagnostic::new(
                    LoweringDiagnosticSeverity::Error,
                    error.message().to_string(),
                    Some(span_for_range_slice(slice)),
                    node.kind(),
                    Some(name.clone()),
                    collect_annotation_texts(node),
                ));
                None
            }
        }
    });

    let (parameters, _) = child_node(node, SyntaxKind::FunctionParameterList)
        .map(|list| lower_parameters(context, &list, diagnostics))
        .unwrap_or_default();

    let return_type = child_node(node, SyntaxKind::FunctionReturnType)
        .and_then(|ret| lower_type_annotation_container(context, &ret, node, diagnostics));

    let fallback_span = span.clone();
    let body = if let Some(block) = child_node(node, SyntaxKind::Block) {
        lower_block_expression(context, &block, diagnostics)
    } else {
        node.children()
            .find(|child| child.kind() == SyntaxKind::Expression)
            .map(|expr| lower_expression(context, &expr))
            .transpose()?
            .unwrap_or_else(|| Expression::Identifier(name.clone(), fallback_span))
    };

    let function_statement = Statement::FunctionDeclaration {
        name: name.clone(),
        type_parameters,
        generic_signature,
        where_clause: None,
        parameters,
        return_type,
        primitive_return: None,
        body: Box::new(body),
        modifiers: Modifiers::default(),
        span: span.clone(),
    };

    if let Some(receiver_type) = receiver_type {
        Ok(Statement::ExtensionFunction(ExtensionFunction {
            receiver_type,
            function: Box::new(function_statement),
            span,
        }))
    } else {
        Ok(function_statement)
    }
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
        process_class_member(context, &child, diagnostics, &mut properties, &mut methods)?;
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
    let binding_pattern = match lower_binding_pattern(context, &binding) {
        Ok(pattern) => pattern,
        Err(diag) => {
            diagnostics.push(diag);
            return Ok(None);
        }
    };

    let name = binding_pattern
        .first_identifier()
        .map(|text| text.to_string())
        .unwrap_or_else(|| join_tokens(&context.tokens_for(&binding)));

    let type_annotation = child_node(node, SyntaxKind::TypeAnnotation).and_then(|type_node| {
        lower_type_annotation_container(context, &type_node, node, diagnostics)
    });

    let initializer = match child_node(node, SyntaxKind::InitializerClause)
        .and_then(|clause| child_node(&clause, SyntaxKind::Expression))
    {
        Some(expr) => Some(lower_expression(context, &expr)?),
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

    let binding_pattern = lower_binding_pattern(context, &binding_node)?;
    let binding_name = binding_pattern
        .first_identifier()
        .map(|text| text.to_string())
        .unwrap_or_else(|| join_tokens(&context.tokens_for(&binding_node)));
    let binding_span = context.span_for(&binding_node).unwrap_or_else(Span::dummy);
    let loop_binding = LoopBinding {
        name: binding_name,
        pattern: Some(binding_pattern),
        type_annotation: None,
        span: binding_span,
    };

    let iterable_expr = node
        .children()
        .find(|child| child.kind() == SyntaxKind::Expression)
        .map(|expr| lower_expression(context, &expr))
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

    let body = match child_node(node, SyntaxKind::Block) {
        Some(block) => lower_block_expression(context, &block, diagnostics),
        None => {
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
        }
    };

    let strategy = infer_loop_strategy(&iterable_expr);

    Ok(Statement::ForIn(ForInStatement {
        binding: loop_binding,
        iterable: iterable_expr,
        strategy,
        body: Box::new(body),
        span: context.span_for(node).unwrap_or_else(Span::dummy),
    }))
}

fn lower_return(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    _diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let value = child_node(node, SyntaxKind::Expression)
        .map(|expr| lower_expression(context, &expr))
        .transpose()?;

    Ok(Statement::Return {
        value,
        span: context.span_for(node).unwrap_or_else(Span::dummy),
    })
}

fn lower_throw(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
    _diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let expr_node = child_node(node, SyntaxKind::Expression).ok_or_else(|| {
        missing_child_diagnostic(
            context,
            node,
            "throw 文には式が必要です",
            SyntaxKind::Expression,
        )
    })?;

    let expr = lower_expression(context, &expr_node)?;
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

fn infer_loop_strategy(iterable: &Expression) -> LoopStrategy {
    match iterable {
        Expression::Binary {
            left,
            op,
            right,
            span,
            ..
        } if matches!(op, BinaryOp::RangeExclusive | BinaryOp::RangeInclusive) => {
            let start = (*left.as_ref()).clone();
            let end = (*right.as_ref()).clone();
            LoopStrategy::NumericRange(NumericRangeLoop {
                start,
                end,
                inclusive: matches!(op, BinaryOp::RangeInclusive),
                span: span.clone(),
            })
        }
        _ => LoopStrategy::Iterable,
    }
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
    _diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let expr = lower_expression(context, node)?;
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

fn lower_parameter_modifiers(
    context: &LoweringContext<'_>,
    parameter: &JvSyntaxNode,
    diagnostics: &mut Vec<LoweringDiagnostic>,
) -> ParameterModifiers {
    let mut modifiers = ParameterModifiers::default();
    let mut property_seen: Option<ParameterProperty> = None;

    let Some(list) = child_node(parameter, SyntaxKind::ParameterModifierList) else {
        return modifiers;
    };

    for modifier in list
        .children()
        .filter(|child| child.kind() == SyntaxKind::ParameterModifier)
    {
        let tokens = context.tokens_for(&modifier);
        let Some(token) = tokens.iter().copied().find(|token| !is_trivia_token(token)) else {
            continue;
        };

        match &token.token_type {
            TokenType::Val => {
                let new_property = ParameterProperty::Val;
                if let Some(previous) = property_seen {
                    let message = if previous == new_property {
                        "`val` 修飾子が重複しています"
                    } else {
                        "`val` と `var` を同時に指定することはできません"
                    };
                    push_diagnostic(
                        diagnostics,
                        LoweringDiagnosticSeverity::Error,
                        message,
                        context,
                        &modifier,
                    );
                }
                modifiers.property = new_property;
                property_seen = Some(new_property);
            }
            TokenType::Var => {
                let new_property = ParameterProperty::Var;
                if let Some(previous) = property_seen {
                    let message = if previous == new_property {
                        "`var` 修飾子が重複しています"
                    } else {
                        "`val` と `var` を同時に指定することはできません"
                    };
                    push_diagnostic(
                        diagnostics,
                        LoweringDiagnosticSeverity::Error,
                        message,
                        context,
                        &modifier,
                    );
                }
                modifiers.property = new_property;
                property_seen = Some(new_property);
            }
            TokenType::Identifier(name) => match name.as_str() {
                "mut" => modifiers.is_mut = true,
                "ref" => modifiers.is_ref = true,
                other => {
                    push_diagnostic(
                        diagnostics,
                        LoweringDiagnosticSeverity::Error,
                        format!("未対応のパラメータ修飾子 `{}` を検出しました", other),
                        context,
                        &modifier,
                    );
                }
            },
            _ => {
                push_diagnostic(
                    diagnostics,
                    LoweringDiagnosticSeverity::Error,
                    format!("未対応のパラメータ修飾子 `{}` を検出しました", token.lexeme),
                    context,
                    &modifier,
                );
            }
        }
    }

    modifiers
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

        let modifiers = lower_parameter_modifiers(context, &child, diagnostics);
        if matches!(modifiers.property, ParameterProperty::Var) {
            has_mutable = true;
        }

        let binding = child_node(&child, SyntaxKind::BindingPattern);
        let name = binding
            .as_ref()
            .and_then(|binding| {
                context
                    .tokens_for(&binding)
                    .into_iter()
                    .find_map(|token| match &token.token_type {
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

        let type_annotation =
            child_node(&child, SyntaxKind::TypeAnnotation).and_then(|type_node| {
                lower_type_annotation_container(context, &type_node, &child, diagnostics)
            });

        let span = context.span_for(&child).unwrap_or_else(Span::dummy);

        params.push(Parameter {
            name,
            type_annotation,
            default_value: None,
            modifiers,
            span,
        });
    }

    (params, has_mutable)
}

fn lower_binding_pattern(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Result<BindingPatternKind, LoweringDiagnostic> {
    match node.kind() {
        SyntaxKind::BindingPattern => lower_binding_pattern_node(context, node),
        SyntaxKind::BindingListPattern | SyntaxKind::BindingTuplePattern => {
            lower_binding_collection_pattern(context, node)
        }
        other => Err(LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            format!(
                "BindingPattern ノードを期待しましたが {:?} を受け取りました",
                other
            ),
            context.span_for(node),
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        )),
    }
}

fn lower_binding_pattern_node(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Result<BindingPatternKind, LoweringDiagnostic> {
    for child in node.children() {
        match child.kind() {
            SyntaxKind::BindingListPattern | SyntaxKind::BindingTuplePattern => {
                return lower_binding_collection_pattern(context, &child);
            }
            _ => continue,
        }
    }

    lower_identifier_binding(context, node)
}

fn lower_binding_collection_pattern(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Result<BindingPatternKind, LoweringDiagnostic> {
    let mut elements = Vec::new();
    for child in node.children() {
        if child.kind() == SyntaxKind::BindingPattern {
            elements.push(lower_binding_pattern(context, &child)?);
        }
    }

    let span = context.span_for(node).unwrap_or_else(Span::dummy);

    match node.kind() {
        SyntaxKind::BindingListPattern => Ok(BindingPatternKind::List { elements, span }),
        SyntaxKind::BindingTuplePattern => Ok(BindingPatternKind::Tuple { elements, span }),
        _ => Err(LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "リスト/タプル以外のバインディングパターンを処理できません",
            context.span_for(node),
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        )),
    }
}

fn lower_identifier_binding(
    context: &LoweringContext<'_>,
    node: &JvSyntaxNode,
) -> Result<BindingPatternKind, LoweringDiagnostic> {
    let tokens = context.tokens_for(node);
    let identifier_token = tokens.iter().find(|token| match token.token_type {
        TokenType::Identifier(_) => true,
        _ => false,
    });

    let identifier_token = match identifier_token {
        Some(token) => token,
        None => {
            return Err(LoweringDiagnostic::new(
                LoweringDiagnosticSeverity::Error,
                "識別子バインディングを特定できませんでした",
                context.span_for(node),
                node.kind(),
                first_identifier_text(node),
                collect_annotation_texts(node),
            ))
        }
    };

    let span = context
        .span_for(node)
        .unwrap_or_else(|| span_from_token(identifier_token));

    match &identifier_token.token_type {
        TokenType::Identifier(text) if text == "_" => Ok(BindingPatternKind::Wildcard { span }),
        TokenType::Identifier(text) => Ok(BindingPatternKind::Identifier {
            name: text.clone(),
            span,
        }),
        _ => Err(LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "バインディングパターンが識別子以外のトークンで始まりました",
            context.span_for(node),
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        )),
    }
}

fn binding_pattern_primary_expression(pattern: &BindingPatternKind) -> Expression {
    let span = pattern.span();
    if let Some(name) = pattern.first_identifier() {
        Expression::Identifier(name.to_string(), span)
    } else {
        Expression::Identifier("_".to_string(), span)
    }
}
