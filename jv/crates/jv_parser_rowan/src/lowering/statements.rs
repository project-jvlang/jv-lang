use super::helpers::{
    collect_annotation_texts, first_identifier_text, JvSyntaxNode, LoweringContext,
};
use crate::syntax::SyntaxKind;
use jv_ast::comments::{CommentKind, CommentStatement, CommentVisibility};
use jv_ast::expression::{Argument, CallArgumentMetadata, CallArgumentStyle, Parameter, WhenArm};
use jv_ast::statement::{
    ConcurrencyConstruct, ForInStatement, LoopBinding, LoopStrategy, Property, ResourceManagement,
    ValBindingOrigin,
};
use jv_ast::types::{BinaryOp, Literal, Modifiers, Pattern, TypeAnnotation, UnaryOp};
use jv_ast::{Expression, Span, Statement};
use jv_lexer::{Token, TokenType};
use jv_parser_syntax_support::support::{
    parsers::regex_literal_from_token,
    spans::{expression_span, merge_spans, span_from_token},
};

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
    _diagnostics: &mut Vec<LoweringDiagnostic>,
) -> Result<Statement, LoweringDiagnostic> {
    let target_node = child_node(node, SyntaxKind::AssignmentTarget).ok_or_else(|| {
        missing_child_diagnostic(
            context,
            node,
            "代入ターゲットを解析できませんでした",
            SyntaxKind::AssignmentTarget,
        )
    })?;

    let target = lower_assignment_target(context, &target_node)?;

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

    Ok(Statement::Assignment {
        target,
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
    _diagnostics: &mut Vec<LoweringDiagnostic>,
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

    let filtered: Vec<&Token> = tokens
        .iter()
        .copied()
        .filter(|token| !is_trivia_token(token) && !matches!(token.token_type, TokenType::Eof))
        .collect();

    if filtered.is_empty() {
        return Err(LoweringDiagnostic::new(
            LoweringDiagnosticSeverity::Error,
            "式が解析対象のトークンを含みません",
            context.span_for(node),
            node.kind(),
            first_identifier_text(node),
            collect_annotation_texts(node),
        ));
    }

    match expression_parser::parse_expression(&filtered) {
        Ok(expr) => Ok(expr),
        Err(err) => {
            let span = err
                .span
                .or_else(|| context.span_for(node))
                .unwrap_or_else(Span::dummy);
            let fallback = Expression::Identifier(join_tokens(&filtered), span);
            Ok(fallback)
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

    impl<'a> ExpressionParser<'a> {
        fn new(tokens: &'a [&'a Token]) -> Self {
            Self { tokens, pos: 0 }
        }

        fn parse_expression_bp(&mut self, min_prec: u8) -> Result<ParsedExpr, ExpressionError> {
            let mut left = self.parse_prefix()?;

            loop {
                if let Some(kind) = self.peek_postfix_kind() {
                    left = self.parse_postfix(left, kind)?;
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
                TokenType::LeftBrace => self.parse_brace_expression(),
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
                TokenType::String(value) => {
                    let span = span_from_token(token);
                    Ok(ParsedExpr {
                        expr: Expression::Literal(Literal::String(value.clone()), span.clone()),
                        start: index,
                        end: index + 1,
                    })
                }
                TokenType::StringInterpolation(value) => {
                    let span = span_from_token(token);
                    Ok(ParsedExpr {
                        expr: Expression::Literal(Literal::String(value.clone()), span.clone()),
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
                    Self::parse_nested_expression(body_tokens)?.expr
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
                ParsedExpr {
                    expr: Expression::Block {
                        statements: Vec::new(),
                        span: span.clone(),
                    },
                    start: start_index,
                    end: closing_index + 1,
                }
            };

            self.pos = closing_index + 1;
            Ok(parsed)
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
                        nested.parse_expression_bp(0)?
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
                    nested.parse_expression_bp(0)?
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
                PostfixKind::Call => self.parse_parenthesized_call(left),
                PostfixKind::TrailingLambda => self.parse_trailing_lambda_call(left),
            }
        }

        fn parse_string_segments(
            &mut self,
            start_index: usize,
        ) -> Result<ParsedExpr, ExpressionError> {
            let mut end_index = start_index;
            let mut combined = String::new();

            if let Some(token) = self.tokens.get(start_index) {
                combined.push_str(token.lexeme.as_str());
            }

            while let Some(token) = self.tokens.get(end_index + 1) {
                combined.push_str(token.lexeme.as_str());
                end_index += 1;
                if matches!(token.token_type, TokenType::StringEnd) {
                    break;
                }
            }

            if !matches!(
                self.tokens.get(end_index).map(|token| &token.token_type),
                Some(TokenType::StringEnd)
            ) {
                return Err(ExpressionError::new(
                    "文字列リテラルが閉じられていません",
                    self.span_at(end_index),
                ));
            }

            self.pos = end_index + 1;
            let span = span_for_range(self.tokens, start_index, end_index + 1);
            let expr = Expression::Literal(Literal::String(combined), span.clone());
            Ok(ParsedExpr {
                expr,
                start: start_index,
                end: end_index + 1,
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

        fn parse_parenthesized_call(
            &mut self,
            left: ParsedExpr,
        ) -> Result<ParsedExpr, ExpressionError> {
            let (_, open_index) = self.advance_with_index_or_error("'(' が必要です")?;
            let close_index = self
                .find_matching_paren(open_index)
                .ok_or_else(|| ExpressionError::new("')' が必要です", self.span_at(open_index)))?;

            let (arguments, metadata, args_span_end) =
                self.parse_arguments(open_index, close_index)?;
            self.pos = close_index + 1;

            let call_span = span_for_range(self.tokens, left.start, args_span_end);
            let expr = Expression::Call {
                function: Box::new(left.expr),
                args: arguments,
                type_arguments: Vec::new(),
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
            let lambda = self.parse_brace_expression()?;
            let call_span = span_for_range(self.tokens, left.start, lambda.end);
            let mut metadata = CallArgumentMetadata::with_style(CallArgumentStyle::Whitespace);
            metadata.used_commas = false;
            let expr = Expression::Call {
                function: Box::new(left.expr),
                args: vec![Argument::Positional(lambda.expr)],
                type_arguments: Vec::new(),
                argument_metadata: metadata,
                span: call_span.clone(),
            };
            Ok(ParsedExpr {
                expr,
                start: left.start,
                end: lambda.end,
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
                match token.token_type {
                    TokenType::LeftParen => depth_paren += 1,
                    TokenType::RightParen if depth_paren > 0 => depth_paren -= 1,
                    TokenType::LeftBrace => depth_brace += 1,
                    TokenType::RightBrace if depth_brace > 0 => depth_brace -= 1,
                    TokenType::LeftBracket => depth_bracket += 1,
                    TokenType::RightBracket if depth_bracket > 0 => depth_bracket -= 1,
                    TokenType::Comma | TokenType::LayoutComma
                        if depth_paren == 0 && depth_brace == 0 && depth_bracket == 0 =>
                    {
                        if start != offset {
                            parts.push(&inner[start..offset]);
                            separators.push(&token.token_type);
                        }
                        start = offset + 1;
                    }
                    _ => {}
                }
            }

            if start < inner.len() {
                parts.push(&inner[start..]);
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
            for token_type in separators {
                match token_type {
                    TokenType::Comma => saw_comma = true,
                    TokenType::LayoutComma => saw_layout = true,
                    _ => {}
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
            if tokens.is_empty() {
                return Ok(Vec::new());
            }

            if matches!(tokens[0].token_type, TokenType::LeftParen) {
                return self.parse_parenthesized_parameters(tokens);
            }

            if tokens.len() == 1 {
                return match &tokens[0].token_type {
                    TokenType::Identifier(name) => {
                        let span = span_from_token(tokens[0]);
                        Ok(vec![Parameter {
                            name: name.clone(),
                            type_annotation: None,
                            default_value: None,
                            span,
                        }])
                    }
                    _ => Err(ExpressionError::new(
                        "ラムダパラメータには識別子が必要です",
                        Some(span_from_token(tokens[0])),
                    )),
                };
            }

            Err(ExpressionError::new(
                "ラムダパラメータの解析に失敗しました",
                tokens.first().map(|t| span_from_token(t)),
            ))
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
                match token.token_type {
                    TokenType::LeftParen => depth += 1,
                    TokenType::RightParen if depth > 0 => depth -= 1,
                    TokenType::Comma if depth == 0 => {
                        if !current.is_empty() {
                            params.push(self.make_parameter_from_slice(&current)?);
                            current.clear();
                        }
                        continue;
                    }
                    _ => {}
                }
                current.push(*token);
            }

            if !current.is_empty() {
                params.push(self.make_parameter_from_slice(&current)?);
            }

            Ok(params)
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
                | BinaryOp::RangeExclusive
                | BinaryOp::RangeInclusive
                | BinaryOp::Elvis => {}
                BinaryOp::Is => {
                    return Err(ExpressionError::new(
                        "`is` 演算子はまだサポートされていません",
                        Some(op_span.clone()),
                    ))
                }
                _ => {
                    return Err(ExpressionError::new(
                        format!("未対応の2項演算子 {:?}", op),
                        Some(op_span.clone()),
                    ));
                }
            }

            let expr = Expression::Binary {
                left: Box::new(left.expr),
                op,
                right: Box::new(right.expr),
                span: span.clone(),
            };
            Ok(ParsedExpr {
                expr,
                start: left.start,
                end: right.end,
            })
        }

        fn peek_postfix_kind(&self) -> Option<PostfixKind> {
            let token = self.peek_token()?;
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

        fn peek_binary_info(&self) -> Option<BinaryInfo> {
            let token = self.peek_token()?;
            binary_info_for_token(&token.token_type)
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

    #[derive(Clone, Copy)]
    enum PostfixKind {
        MemberAccess,
        NullSafeMemberAccess,
        Index,
        NullSafeIndex,
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

    fn binary_info_for_token(token: &TokenType) -> Option<BinaryInfo> {
        match token {
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
        lower_block_expression(context, &block, diagnostics)
    } else {
        node.children()
            .find(|child| child.kind() == SyntaxKind::Expression)
            .map(|expr| lower_expression(context, &expr))
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
    let name =
        match context
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
