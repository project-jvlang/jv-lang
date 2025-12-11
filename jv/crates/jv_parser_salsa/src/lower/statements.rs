use super::context::{LoweringContext, LoweringDiagnostic, LoweringResult};
use super::expressions::{lower_expression, slice_until};
use super::params::parse_parameters;
use super::types::lower_type_annotation;
use crate::dsl::concurrent::ensure_concurrency_body;
use crate::dsl::resource::{ensure_defer_body, ensure_use_resource};
use crate::dsl::test as dsl_test;
use crate::parser::cst::{CstBuilder, CstElement, CstNode};
use crate::parser::{ParseEvent, ParseResult, SyntaxKind};
use jv_ast::BindingPatternKind;
use jv_ast::expression::{Expression, WhenArm};
use jv_ast::statement::{
    ConcurrencyConstruct, Property, ResourceManagement, Statement, TestDataset, TestSampleMetadata,
    ValBindingOrigin,
};
use jv_ast::types::{Modifiers, Pattern, WhereClause};

/// パース結果を基にステートメント群をローワリングする。
pub fn lower_statements(source: &str, parse: &ParseResult) -> LoweringResult {
    let mut ctx = LoweringContext::new(source, &parse.tokens);
    let cst = match build_cst(parse) {
        Some(root) => root,
        None => {
            return LoweringResult {
                statements: Vec::new(),
                diagnostics: vec![LoweringDiagnostic::error("CST の構築に失敗しました", None)],
                token_spans: ctx.token_spans(),
            };
        }
    };

    let mut statements = Vec::new();
    lower_node(&mut ctx, &cst, &mut statements);

    let token_spans = ctx.token_spans();
    let diagnostics = ctx.into_diagnostics();

    LoweringResult {
        statements,
        diagnostics,
        token_spans,
    }
}

fn build_cst(parse: &ParseResult) -> Option<CstNode> {
    let mut builder = CstBuilder::new();
    for event in &parse.output.events {
        match event {
            ParseEvent::StartNode { kind } => builder.start_node(kind.clone()),
            ParseEvent::FinishNode => builder.finish_node(),
            ParseEvent::Token { token_index, .. } => {
                if let Some(tok) = parse.tokens.get(*token_index).cloned() {
                    builder.push_token(tok);
                }
            }
            ParseEvent::Error { .. } => {}
        }
    }
    builder.build()
}

fn lower_node(ctx: &mut LoweringContext<'_>, node: &CstNode, out: &mut Vec<Statement>) {
    match node.kind {
        SyntaxKind::Root | SyntaxKind::StatementList => {
            for child in node.children.iter().filter_map(|c| match c {
                CstElement::Node(n) => Some(n),
                _ => None,
            }) {
                lower_node(ctx, child, out);
            }
        }
        SyntaxKind::Error => {
            let tokens = collect_tokens(node);
            let span = span_for_node(ctx, node);
            if tokens.iter().any(|t| t.kind == crate::lexer::TokenKind::If) {
                ctx.push_diagnostic(LoweringDiagnostic::error(
                    "JV3103: `if` expressions are not supported / `if` 式はサポートされていません。",
                    Some(span),
                ));
            } else if tokens
                .iter()
                .any(|t| t.kind == crate::lexer::TokenKind::While || t.lexeme == "do")
            {
                ctx.push_diagnostic(LoweringDiagnostic::error(
                    "E_LOOP_001: `while`/`do-while` loops have been removed from the language / `while`/`do-while` ループはサポートされていません。",
                    Some(span),
                ));
            }
        }
        SyntaxKind::PackageDeclaration => {
            if let Some(stmt) = lower_package(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::ImportDeclaration => {
            if let Some(stmt) = lower_import(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::ValDeclaration => {
            if let Some(stmt) = lower_val(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::VarDeclaration => {
            if let Some(stmt) = lower_var(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::FunctionDeclaration => {
            if let Some(stmt) = lower_function(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::ClassDeclaration | SyntaxKind::DataClassDeclaration => {
            if let Some(stmt) = lower_class_like(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::ReturnStatement => {
            if let Some(stmt) = lower_return(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::IfStatement => {
            let span = span_for_node(ctx, node);
            ctx.push_diagnostic(LoweringDiagnostic::warning(
                "JV3103: `if` expressions are not supported / `if` 式はサポートされていません。",
                Some(span),
            ));
        }
        SyntaxKind::WhileStatement => {
            let span = span_for_node(ctx, node);
            ctx.push_diagnostic(LoweringDiagnostic::error(
                "E_LOOP_001: `while`/`do-while` loops have been removed from the language / `while`/`do-while` ループはサポートされていません。",
                Some(span),
            ));
        }
        SyntaxKind::BreakStatement => {
            let span = span_for_node(ctx, node);
            out.push(Statement::Break(span));
        }
        SyntaxKind::ContinueStatement => {
            let span = span_for_node(ctx, node);
            out.push(Statement::Continue(span));
        }
        SyntaxKind::WhenStatement => {
            if let Some(stmt) = lower_when(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::ForStatement => {
            if let Some(stmt) = lower_for(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::UseStatement => {
            if let Some(stmt) = lower_use(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::DeferStatement => {
            if let Some(stmt) = lower_defer(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::SpawnStatement | SyntaxKind::AsyncStatement => {
            if let Some(stmt) = lower_concurrency(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::AssignmentStatement => {
            if let Some(stmt) = lower_assignment(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::LogBlockExpression => {
            let tokens = collect_tokens(node);
            if let Some(expr) = lower_expression(ctx, &tokens) {
                let span = expr.span().clone();
                out.push(Statement::Expression { expr, span });
            }
        }
        SyntaxKind::TestDeclaration => {
            if let Some(stmt) = lower_test(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::Expression => {
            if let Some(expr) = lower_expression(ctx, &collect_tokens(node)) {
                let span = expr.span().clone();
                out.push(Statement::Expression { expr, span });
            }
        }
        _ => {}
    }
}

fn lower_package(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let idents = collect_identifiers(node);
    if idents.is_empty() {
        return None;
    }
    Some(Statement::Package {
        name: idents.join("."),
        span: span_for_node(ctx, node),
    })
}

fn lower_import(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let mut path_parts = Vec::new();
    let mut is_wildcard = false;
    for child in &node.children {
        if let CstElement::Node(child_node) = child {
            if child_node.kind == SyntaxKind::ImportPath {
                for tok in collect_tokens(child_node) {
                    match tok.kind {
                        crate::lexer::TokenKind::Identifier => path_parts.push(tok.lexeme),
                        crate::lexer::TokenKind::Multiply => is_wildcard = true,
                        _ => {}
                    }
                }
            }
        }
    }

    let mut alias = None;
    for child in &node.children {
        if let CstElement::Node(child_node) = child {
            if matches!(
                child_node.kind,
                SyntaxKind::ImportClause | SyntaxKind::ImportAlias
            ) {
                if let Some(first_ident) = collect_identifiers(child_node).first() {
                    alias = Some(first_ident.clone());
                }
            }
        }
    }

    Some(Statement::Import {
        path: path_parts.join("."),
        alias,
        is_wildcard,
        span: span_for_node(ctx, node),
    })
}

fn lower_val(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let name = tokens
        .iter()
        .find(|tok| tok.kind == crate::lexer::TokenKind::Identifier)
        .map(|tok| tok.lexeme.clone())?;

    let type_tokens = slice_until(&tokens, crate::lexer::TokenKind::Colon, |t| t.kind);
    let type_annotation = if let Some((_, after)) = type_tokens {
        let end = find_end_of_type(after);
        lower_type_annotation(ctx, &after[..end])
    } else {
        None
    };

    let initializer_tokens = slice_until(&tokens, crate::lexer::TokenKind::Assign, |t| t.kind)
        .and_then(|(_, after)| {
            if after.is_empty() {
                None
            } else {
                Some(after.to_vec())
            }
        });
    let initializer = initializer_tokens
        .as_ref()
        .and_then(|slice| lower_expression(ctx, slice))
        .unwrap_or_else(|| {
            let span = span_for_node(ctx, node);
            Expression::Literal(jv_ast::types::Literal::Null, span)
        });

    Some(Statement::ValDeclaration {
        name,
        binding: None,
        type_annotation,
        initializer,
        modifiers: Modifiers::default(),
        origin: ValBindingOrigin::ExplicitKeyword,
        span: span_for_node(ctx, node),
    })
}

fn lower_var(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let name = tokens
        .iter()
        .find(|tok| tok.kind == crate::lexer::TokenKind::Identifier)
        .map(|tok| tok.lexeme.clone())?;

    let type_tokens = slice_until(&tokens, crate::lexer::TokenKind::Colon, |t| t.kind);
    let type_annotation = if let Some((_, after)) = type_tokens {
        let end = find_end_of_type(after);
        lower_type_annotation(ctx, &after[..end])
    } else {
        None
    };

    let initializer_tokens = slice_until(&tokens, crate::lexer::TokenKind::Assign, |t| t.kind)
        .and_then(|(_, after)| {
            if after.is_empty() {
                None
            } else {
                Some(after.to_vec())
            }
        });
    let initializer = initializer_tokens
        .as_ref()
        .and_then(|slice| lower_expression(ctx, slice));

    Some(Statement::VarDeclaration {
        name,
        binding: None,
        type_annotation,
        initializer,
        modifiers: Modifiers::default(),
        span: span_for_node(ctx, node),
    })
}

fn lower_function(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let body_idx = tokens
        .iter()
        .position(|t| t.kind == crate::lexer::TokenKind::LeftBrace)
        .unwrap_or(tokens.len());
    let header_tokens = &tokens[..body_idx];

    let name_pos = header_tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::Identifier)?;
    let name = header_tokens.get(name_pos)?.lexeme.clone();

    // 型パラメータ抽出。
    let type_parameters = {
        let mut params = Vec::new();
        if let Some(lt_idx) = header_tokens
            .iter()
            .skip(name_pos + 1)
            .position(|t| t.kind == crate::lexer::TokenKind::Less)
        {
            let lt_idx = name_pos + 1 + lt_idx;
            let mut depth = 1usize;
            let mut idx = lt_idx + 1;
            while idx < header_tokens.len() && depth > 0 {
                match header_tokens[idx].kind {
                    crate::lexer::TokenKind::Less => depth += 1,
                    crate::lexer::TokenKind::Greater => depth = depth.saturating_sub(1),
                    crate::lexer::TokenKind::Identifier if depth == 1 => {
                        params.push(header_tokens[idx].lexeme.clone())
                    }
                    _ => {}
                }
                idx += 1;
            }
        }
        params
    };

    // パラメータ抽出。
    let params = {
        let open = header_tokens
            .iter()
            .position(|t| t.kind == crate::lexer::TokenKind::LeftParen)
            .unwrap_or(header_tokens.len());
        let close = find_matching_paren(header_tokens, open).unwrap_or(header_tokens.len());
        if close > open + 1 {
            parse_parameters(ctx, &header_tokens[open + 1..close])
        } else {
            Vec::new()
        }
    };

    let return_type = {
        let colon_idx = header_tokens
            .iter()
            .position(|t| t.kind == crate::lexer::TokenKind::Colon);
        colon_idx.and_then(|idx| {
            let slice = &header_tokens[idx + 1..];
            if slice.is_empty() {
                None
            } else {
                lower_type_annotation(ctx, slice)
            }
        })
    };

    // where 句（内容は未解析だが存在を保持）
    let where_clause = header_tokens
        .iter()
        .enumerate()
        .find(|(_, t)| t.kind == crate::lexer::TokenKind::Where)
        .map(|(idx, _)| {
            let end_idx = tokens
                .iter()
                .enumerate()
                .skip(idx)
                .find(|(_, t)| t.kind == crate::lexer::TokenKind::LeftBrace)
                .map(|(i, _)| i)
                .unwrap_or(tokens.len().saturating_sub(1));
            let start = tokens
                .get(idx)
                .cloned()
                .unwrap_or_else(|| tokens.first().cloned().unwrap());
            let end = tokens
                .get(end_idx)
                .cloned()
                .unwrap_or_else(|| tokens.last().cloned().unwrap());
            WhereClause {
                predicates: Vec::new(),
                primitive_bounds: Vec::new(),
                span: ctx.span_for_range(&start, &end),
            }
        });

    let body = if let Some(block) = find_child(node, SyntaxKind::Block) {
        lower_block_expr(ctx, block)
    } else {
        Expression::Block {
            statements: Vec::new(),
            span: span_for_node(ctx, node),
        }
    };

    Some(Statement::FunctionDeclaration {
        name,
        type_parameters,
        generic_signature: None,
        where_clause,
        parameters: params,
        return_type,
        primitive_return: None,
        body: Box::new(body),
        modifiers: Modifiers::default(),
        span: span_for_node(ctx, node),
    })
}

fn lower_class_like(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let name_idx = tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::Identifier)?;
    let name = tokens.get(name_idx)?.lexeme.clone();
    let span = span_for_node(ctx, node);

    let brace_idx = tokens
        .iter()
        .position(|t| t.kind == crate::lexer::TokenKind::LeftBrace)
        .unwrap_or(tokens.len());
    let semicolon_idx = tokens
        .iter()
        .position(|t| t.kind == crate::lexer::TokenKind::Semicolon)
        .unwrap_or(tokens.len());
    let where_idx = tokens
        .iter()
        .position(|t| t.kind == crate::lexer::TokenKind::Where)
        .unwrap_or(tokens.len());
    let inherit_end = brace_idx.min(semicolon_idx).min(where_idx);

    // 型パラメータ抽出（簡易: `<...>` 内の Identifier を収集）
    let type_parameters = {
        let mut params = Vec::new();
        if let Some(lt_idx) = tokens
            .iter()
            .skip(name_idx + 1)
            .position(|t| t.kind == crate::lexer::TokenKind::Less)
        {
            let lt_idx = name_idx + 1 + lt_idx;
            let mut depth = 1usize;
            let mut idx = lt_idx + 1;
            while idx < tokens.len() && depth > 0 {
                match tokens[idx].kind {
                    crate::lexer::TokenKind::Less => depth += 1,
                    crate::lexer::TokenKind::Greater => depth = depth.saturating_sub(1),
                    crate::lexer::TokenKind::Identifier if depth == 1 => {
                        params.push(tokens[idx].lexeme.clone())
                    }
                    _ => {}
                }
                idx += 1;
            }
        }
        params
    };

    // コンストラクタパラメータ（データクラス/プライマリコンストラクタを簡易対応）。
    let (params, after_params_idx) = {
        let open = tokens
            .iter()
            .skip(name_idx + 1)
            .position(|t| t.kind == crate::lexer::TokenKind::LeftParen)
            .map(|i| name_idx + 1 + i);
        if let Some(open_idx) = open {
            let close = find_matching_paren(&tokens, open_idx).unwrap_or(tokens.len());
            let params = if close > open_idx + 1 {
                parse_parameters(ctx, &tokens[open_idx + 1..close])
            } else {
                Vec::new()
            };
            (params, close + 1)
        } else {
            (Vec::new(), name_idx + 1)
        }
    };

    // 継承/実装リスト: コンストラクタ引数をスキップした後の `:` から `{` まで。`:` が無ければ空。
    let colon_idx = tokens
        .iter()
        .enumerate()
        .skip(after_params_idx)
        .find(|(_, t)| t.kind == crate::lexer::TokenKind::Colon)
        .map(|(i, _)| i);
    let mut superclass = None;
    let mut interfaces = Vec::new();
    if let Some(colon) = colon_idx {
        if colon + 1 < inherit_end {
            let inheritance_slice = &tokens[colon + 1..inherit_end];
            let mut type_slices = split_types(inheritance_slice);
            if let Some(first) = type_slices.get(0) {
                if !first.is_empty() {
                    superclass = lower_type_annotation(ctx, first);
                }
                type_slices.remove(0);
            }
            for ty_slice in type_slices {
                if let Some(ty) = lower_type_annotation(ctx, ty_slice) {
                    interfaces.push(ty);
                }
            }
        }
    }

    // クラスボディのメンバーをローワリング。
    let mut properties = Vec::new();
    let mut methods = Vec::new();
    if let Some(block) = find_child(node, SyntaxKind::Block) {
        for child in block.children.iter().filter_map(|c| match c {
            CstElement::Node(n) if n.kind == SyntaxKind::StatementList => Some(n),
            _ => None,
        }) {
            let mut lowered = Vec::new();
            lower_node(ctx, child, &mut lowered);
            for stmt in lowered {
                match stmt {
                    Statement::ValDeclaration {
                        name,
                        type_annotation,
                        initializer,
                        modifiers,
                        span,
                        ..
                    } => properties.push(Property {
                        name,
                        type_annotation,
                        initializer: Some(initializer),
                        is_mutable: false,
                        modifiers,
                        getter: None,
                        setter: None,
                        span,
                    }),
                    Statement::VarDeclaration {
                        name,
                        type_annotation,
                        initializer,
                        modifiers,
                        span,
                        ..
                    } => properties.push(Property {
                        name,
                        type_annotation,
                        initializer,
                        is_mutable: true,
                        modifiers,
                        getter: None,
                        setter: None,
                        span,
                    }),
                    Statement::FunctionDeclaration { .. } => methods.push(Box::new(stmt)),
                    Statement::ClassDeclaration { .. }
                    | Statement::DataClassDeclaration { .. }
                    | Statement::TestDeclaration(_) => methods.push(Box::new(stmt)),
                    other => ctx.push_diagnostic(LoweringDiagnostic::warning(
                        "LWR001: クラス本体内の未対応メンバーをスキップしました。",
                        Some(other.span().clone()),
                    )),
                }
            }
        }
    }

    if node.kind == SyntaxKind::DataClassDeclaration {
        Some(Statement::DataClassDeclaration {
            name,
            parameters: params,
            type_parameters,
            generic_signature: None,
            is_mutable: false,
            modifiers: Modifiers::default(),
            span,
        })
    } else {
        Some(Statement::ClassDeclaration {
            name,
            type_parameters,
            generic_signature: None,
            superclass,
            interfaces,
            properties,
            methods,
            modifiers: Modifiers::default(),
            span,
        })
    }
}

fn lower_return(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let after = tokens
        .iter()
        .skip_while(|tok| tok.kind != crate::lexer::TokenKind::Return)
        .skip(1)
        .cloned()
        .collect::<Vec<_>>();
    let value = if after.is_empty() {
        None
    } else {
        lower_expression(ctx, &after)
    };
    Some(Statement::Return {
        value,
        span: span_for_node(ctx, node),
    })
}

fn lower_when(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let brace_idx = tokens
        .iter()
        .position(|t| t.kind == crate::lexer::TokenKind::LeftBrace)
        .unwrap_or(tokens.len());
    let condition = lower_expression(ctx, &tokens[1..brace_idx])?;

    let mut arms = Vec::new();
    let mut else_arm = None;
    for child in &node.children {
        if let CstElement::Node(branch) = child {
            if branch.kind != SyntaxKind::WhenBranch {
                continue;
            }
            let branch_tokens = collect_tokens(branch);
            let arrow_idx = branch_tokens
                .iter()
                .position(|t| {
                    matches!(
                        t.kind,
                        crate::lexer::TokenKind::Arrow | crate::lexer::TokenKind::FatArrow
                    )
                })
                .unwrap_or(branch_tokens.len());
            let (pat_tokens, body_tokens) = branch_tokens.split_at(arrow_idx);
            let body_tokens = body_tokens.get(1..).unwrap_or(&[]);

            if pat_tokens
                .iter()
                .any(|t| t.lexeme.eq_ignore_ascii_case("else"))
            {
                else_arm = lower_expression(ctx, body_tokens).map(Box::new);
                continue;
            }

            let (pattern, guard) = lower_pattern(ctx, pat_tokens)
                .unwrap_or_else(|| (Pattern::Wildcard(span_for_node(ctx, branch)), None));
            let body = lower_expression(ctx, body_tokens).unwrap_or_else(|| {
                Expression::Literal(jv_ast::types::Literal::Null, span_for_node(ctx, branch))
            });
            arms.push(WhenArm {
                pattern,
                guard,
                body,
                span: span_for_node(ctx, branch),
            });
        }
    }

    let span = span_for_node(ctx, node);
    Some(Statement::Expression {
        span: span.clone(),
        expr: Expression::When {
            expr: Some(Box::new(condition)),
            arms,
            else_arm,
            implicit_end: None,
            span: span.clone(),
        },
    })
}

fn lower_for(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let in_idx = tokens
        .iter()
        .position(|t| t.kind == crate::lexer::TokenKind::In)
        .unwrap_or(tokens.len());
    let binding_slice = &tokens[1..in_idx];
    let (pattern_tokens, type_tokens) = if let Some(colon_idx) = binding_slice
        .iter()
        .position(|t| t.kind == crate::lexer::TokenKind::Colon)
    {
        (
            &binding_slice[..colon_idx],
            Some(&binding_slice[colon_idx + 1..]),
        )
    } else {
        (binding_slice, None)
    };
    let binding_pattern = lower_binding_pattern(ctx, pattern_tokens);
    let type_annotation = type_tokens.and_then(|ty_tokens| {
        if ty_tokens.is_empty() {
            None
        } else {
            let end = find_end_of_type(ty_tokens);
            lower_type_annotation(ctx, &ty_tokens[..end])
        }
    });

    let iterable_tokens = tokens.get(in_idx + 1..).unwrap_or(&[]);
    let iterable = lower_expression(ctx, iterable_tokens)?;

    let body = find_child(node, SyntaxKind::Block)
        .map(|block| lower_block_expr(ctx, block))
        .unwrap_or_else(|| Expression::Block {
            statements: Vec::new(),
            span: span_for_node(ctx, node),
        });

    let span = span_for_node(ctx, node);
    let binding_span = span.clone();
    Some(Statement::ForIn(jv_ast::statement::ForInStatement {
        binding: jv_ast::statement::LoopBinding {
            name: binding_pattern
                .as_ref()
                .and_then(|p| p.first_identifier().map(|s| s.to_string()))
                .unwrap_or_else(|| "_".to_string()),
            pattern: binding_pattern.clone(),
            type_annotation,
            span: binding_span,
        },
        iterable,
        strategy: jv_ast::statement::LoopStrategy::Iterable,
        body: Box::new(body),
        span,
    }))
}

fn lower_test(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let keyword = tokens
        .first()
        .cloned()
        .unwrap_or_else(|| crate::parser::OwnedToken {
            kind: crate::lexer::TokenKind::Invalid,
            span: crate::lexer::Span { start: 0, end: 0 },
            lexeme: "test".into(),
            leading_trivia: Default::default(),
            metadata: Vec::new(),
            diagnostic: None,
        });
    let name = dsl_test::extract_test_name(&tokens, ctx.source());
    dsl_test::ensure_test_name(ctx, &keyword, &name);
    let block_node = find_child(node, SyntaxKind::Block);
    let has_body_tokens = block_node
        .map(|b| {
            collect_tokens(b).into_iter().any(|t| {
                !matches!(
                    t.kind,
                    crate::lexer::TokenKind::LeftBrace
                        | crate::lexer::TokenKind::RightBrace
                        | crate::lexer::TokenKind::Newline
                )
            })
        })
        .unwrap_or(false);
    dsl_test::ensure_test_body(ctx, &keyword, has_body_tokens);
    let body = block_node
        .map(|block| lower_block_expr(ctx, block))
        .unwrap_or_else(|| Expression::Block {
            statements: Vec::new(),
            span: span_for_node(ctx, node),
        });
    let dataset = dsl_test::extract_dataset(&tokens, ctx.source()).map(|name| {
        TestDataset::Sample(TestSampleMetadata {
            source: name,
            arguments: Vec::new(),
            span: span_for_node(ctx, node),
        })
    });
    let span = span_for_node(ctx, node);
    Some(Statement::TestDeclaration(
        jv_ast::statement::TestDeclaration {
            display_name: name.clone().unwrap_or_default(),
            normalized: name,
            dataset,
            parameters: Vec::new(),
            annotations: Vec::new(),
            body,
            span,
        },
    ))
}

fn lower_use(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let keyword = tokens
        .iter()
        .find(|tok| tok.lexeme == "use")
        .cloned()
        .unwrap_or_else(|| {
            tokens
                .first()
                .cloned()
                .unwrap_or_else(|| crate::parser::OwnedToken {
                    kind: crate::lexer::TokenKind::Invalid,
                    span: crate::lexer::Span { start: 0, end: 0 },
                    lexeme: "use".into(),
                    leading_trivia: Default::default(),
                    metadata: Vec::new(),
                    diagnostic: None,
                })
        });
    let brace_pos = tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::LeftBrace)
        .unwrap_or(tokens.len());
    let use_pos = tokens
        .iter()
        .position(|tok| tok.lexeme == "use")
        .unwrap_or(0);
    let after_use: Vec<_> = if brace_pos > use_pos + 1 {
        tokens[use_pos + 1..brace_pos].to_vec()
    } else {
        Vec::new()
    };
    let has_resource = !after_use.is_empty();
    ensure_use_resource(ctx, &keyword, has_resource);

    let resource = lower_expression(ctx, &after_use).unwrap_or_else(|| {
        Expression::Literal(jv_ast::types::Literal::Null, span_for_node(ctx, node))
    });
    let body = find_child(node, SyntaxKind::Block)
        .map(|block| lower_block_expr(ctx, block))
        .unwrap_or_else(|| Expression::Block {
            statements: Vec::new(),
            span: span_for_node(ctx, node),
        });
    Some(Statement::ResourceManagement(ResourceManagement::Use {
        resource: Box::new(resource.clone()),
        body: Box::new(body),
        span: span_for_node(ctx, node),
    }))
}

fn lower_defer(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let keyword = tokens
        .iter()
        .find(|tok| tok.lexeme == "defer")
        .cloned()
        .unwrap_or_else(|| {
            tokens
                .first()
                .cloned()
                .unwrap_or_else(|| crate::parser::OwnedToken {
                    kind: crate::lexer::TokenKind::Invalid,
                    span: crate::lexer::Span { start: 0, end: 0 },
                    lexeme: "defer".into(),
                    leading_trivia: Default::default(),
                    metadata: Vec::new(),
                    diagnostic: None,
                })
        });
    let brace_pos = tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::LeftBrace)
        .unwrap_or(tokens.len());
    let defer_pos = tokens
        .iter()
        .position(|tok| tok.lexeme == "defer")
        .unwrap_or(0);
    let after_defer: Vec<_> = if brace_pos > defer_pos + 1 {
        tokens[defer_pos + 1..brace_pos].to_vec()
    } else {
        Vec::new()
    };
    let has_body_tokens = !after_defer.is_empty()
        || find_child(node, SyntaxKind::Block)
            .map(|b| !collect_tokens(b).is_empty())
            .unwrap_or(false);
    ensure_defer_body(ctx, &keyword, has_body_tokens);

    let body = find_child(node, SyntaxKind::Block)
        .map(|block| lower_block_expr(ctx, block))
        .or_else(|| lower_expression(ctx, &after_defer))
        .unwrap_or_else(|| Expression::Block {
            statements: Vec::new(),
            span: span_for_node(ctx, node),
        });
    Some(Statement::ResourceManagement(ResourceManagement::Defer {
        body: Box::new(body),
        span: span_for_node(ctx, node),
    }))
}

fn lower_concurrency(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let kind = node.kind.clone();
    let tokens = collect_tokens(node);
    let body_tokens = tokens.iter().skip(1).cloned().collect::<Vec<_>>();
    let has_body = !body_tokens.is_empty()
        || find_child(node, SyntaxKind::Block)
            .map(|b| !collect_tokens(b).is_empty())
            .unwrap_or(false);
    if let Some(keyword) = tokens.first() {
        ensure_concurrency_body(ctx, keyword, has_body);
    }
    let body = find_child(node, SyntaxKind::Block)
        .map(|block| lower_block_expr(ctx, block))
        .or_else(|| lower_expression(ctx, &body_tokens))
        .unwrap_or_else(|| Expression::Block {
            statements: Vec::new(),
            span: span_for_node(ctx, node),
        });
    let span = span_for_node(ctx, node);
    let construct = if kind == SyntaxKind::SpawnStatement {
        ConcurrencyConstruct::Spawn {
            body: Box::new(body),
            span,
        }
    } else {
        ConcurrencyConstruct::Async {
            body: Box::new(body),
            span,
        }
    };
    Some(Statement::Concurrency(construct))
}

fn lower_assignment(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let assign_idx = tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::Assign)?;
    let (lhs, rhs) = tokens.split_at(assign_idx);
    let rhs = &rhs[1..];
    let target = lower_expression(ctx, lhs).unwrap_or_else(|| {
        Expression::Literal(jv_ast::types::Literal::Null, span_for_node(ctx, node))
    });
    let value = lower_expression(ctx, rhs)?;
    Some(Statement::Assignment {
        target,
        binding_pattern: None,
        value,
        span: span_for_node(ctx, node),
    })
}

fn lower_block_expr(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Expression {
    let mut statements = Vec::new();
    for child in node.children.iter().filter_map(|c| match c {
        CstElement::Node(n) => Some(n),
        _ => None,
    }) {
        if child.kind == SyntaxKind::StatementList {
            lower_node(ctx, child, &mut statements);
        }
    }
    Expression::Block {
        statements,
        span: span_for_node(ctx, node),
    }
}

fn lower_pattern(
    ctx: &mut LoweringContext<'_>,
    tokens: &[crate::parser::OwnedToken],
) -> Option<(Pattern, Option<Expression>)> {
    if tokens.is_empty() {
        return None;
    }

    let expr = lower_expression(ctx, tokens)?;
    let has_comma = tokens
        .iter()
        .any(|t| t.kind == crate::lexer::TokenKind::Comma);
    match expr.clone() {
        Expression::Identifier(name, span) if !has_comma => {
            Some((Pattern::Identifier(name, span), None))
        }
        Expression::Literal(lit, span) if !has_comma => Some((Pattern::Literal(lit, span), None)),
        Expression::Binary {
            op: jv_ast::types::BinaryOp::Is,
            left,
            ..
        } => {
            // `x is Type` のような型パターンはパターン本体を保持しつつ guard に条件を積む。
            let base_pattern = match *left {
                Expression::Identifier(name, span) => Pattern::Identifier(name, span),
                Expression::Literal(lit, span) => Pattern::Literal(lit, span),
                other => {
                    ctx.push_diagnostic(LoweringDiagnostic::warning(
                        "LWR002: 型パターンの左辺をパターンとして解釈できませんでした。ワイルドカードとして扱います。",
                        Some(other.span().clone()),
                    ));
                    Pattern::Wildcard(
                        tokens
                            .first()
                            .map(|t| ctx.span_for_token(t))
                            .unwrap_or_else(|| {
                                ctx.span_from_raw(crate::lexer::Span { start: 0, end: 0 })
                            }),
                    )
                }
            };
            Some((base_pattern, Some(expr)))
        }
        _ => {
            if has_comma {
                // 複数パターンは全体を guard として保持する。
                Some((
                    Pattern::Wildcard(
                        tokens
                            .first()
                            .map(|t| ctx.span_for_token(t))
                            .unwrap_or_else(|| {
                                ctx.span_from_raw(crate::lexer::Span { start: 0, end: 0 })
                            }),
                    ),
                    Some(expr),
                ))
            } else {
                ctx.push_diagnostic(LoweringDiagnostic::warning(
                    "LWR003: when パターンを解釈できませんでした。ワイルドカードとして扱います。",
                    tokens.first().map(|t| ctx.span_for_token(t)),
                ));
                Some((
                    Pattern::Wildcard(
                        tokens
                            .first()
                            .map(|t| ctx.span_for_token(t))
                            .unwrap_or_else(|| {
                                ctx.span_from_raw(crate::lexer::Span { start: 0, end: 0 })
                            }),
                    ),
                    None,
                ))
            }
        }
    }
}

fn lower_binding_pattern(
    ctx: &mut LoweringContext<'_>,
    tokens: &[crate::parser::OwnedToken],
) -> Option<BindingPatternKind> {
    if tokens.is_empty() {
        return None;
    }

    let fallback_span = tokens
        .first()
        .map(|t| ctx.span_for_token(t))
        .unwrap_or_else(|| ctx.span_from_raw(crate::lexer::Span { start: 0, end: 0 }));

    let (pattern, _) = lower_pattern(ctx, tokens)?;
    match pattern {
        Pattern::Identifier(name, span) => Some(BindingPatternKind::identifier(name, span)),
        Pattern::Wildcard(span) => Some(BindingPatternKind::wildcard(span)),
        _other => {
            ctx.push_diagnostic(LoweringDiagnostic::warning(
                "このバインディングパターンは未対応のため `_` として扱います。",
                Some(fallback_span.clone()),
            ));
            Some(BindingPatternKind::wildcard(fallback_span))
        }
    }
}

fn collect_tokens(node: &CstNode) -> Vec<crate::parser::OwnedToken> {
    let mut tokens = Vec::new();
    for child in &node.children {
        match child {
            CstElement::Token(tok) => tokens.push(tok.clone()),
            CstElement::Node(n) => tokens.extend(collect_tokens(n)),
        }
    }
    tokens
}

fn collect_identifiers(node: &CstNode) -> Vec<String> {
    collect_tokens(node)
        .into_iter()
        .filter(|tok| tok.kind == crate::lexer::TokenKind::Identifier)
        .map(|tok| tok.lexeme)
        .collect()
}

fn span_for_node(ctx: &LoweringContext<'_>, node: &CstNode) -> jv_ast::Span {
    let tokens = collect_tokens(node);
    let start = tokens
        .first()
        .cloned()
        .unwrap_or_else(|| crate::parser::OwnedToken {
            kind: crate::lexer::TokenKind::Invalid,
            span: crate::lexer::Span { start: 0, end: 0 },
            lexeme: String::new(),
            leading_trivia: Default::default(),
            metadata: Vec::new(),
            diagnostic: None,
        });
    let end = tokens.last().unwrap_or(&start).clone();
    ctx.span_for_range(&start, &end)
}

fn find_child<'a>(node: &'a CstNode, kind: SyntaxKind) -> Option<&'a CstNode> {
    node.children.iter().find_map(|child| match child {
        CstElement::Node(n) if n.kind == kind => Some(n),
        _ => None,
    })
}

fn split_types(tokens: &[crate::parser::OwnedToken]) -> Vec<&[crate::parser::OwnedToken]> {
    let mut slices = Vec::new();
    let mut start = 0usize;
    let mut depth = 0usize;
    let mut angle = 0usize;
    for (idx, tok) in tokens.iter().enumerate() {
        match tok.kind {
            crate::lexer::TokenKind::LeftParen
            | crate::lexer::TokenKind::LeftBracket
            | crate::lexer::TokenKind::LeftBrace => depth += 1,
            crate::lexer::TokenKind::RightParen
            | crate::lexer::TokenKind::RightBracket
            | crate::lexer::TokenKind::RightBrace => depth = depth.saturating_sub(1),
            crate::lexer::TokenKind::Less => angle += 1,
            crate::lexer::TokenKind::Greater => angle = angle.saturating_sub(1),
            crate::lexer::TokenKind::Comma if depth == 0 && angle == 0 => {
                if start < idx {
                    slices.push(&tokens[start..idx]);
                }
                start = idx + 1;
            }
            _ => {}
        }
    }
    if start < tokens.len() {
        slices.push(&tokens[start..]);
    }
    slices
}

fn find_matching_paren(tokens: &[crate::parser::OwnedToken], open_idx: usize) -> Option<usize> {
    if open_idx >= tokens.len() || tokens[open_idx].kind != crate::lexer::TokenKind::LeftParen {
        return None;
    }
    let mut depth = 0usize;
    for (idx, tok) in tokens.iter().enumerate().skip(open_idx + 1) {
        match tok.kind {
            crate::lexer::TokenKind::LeftParen => depth += 1,
            crate::lexer::TokenKind::RightParen => {
                if depth == 0 {
                    return Some(idx);
                }
                depth = depth.saturating_sub(1);
            }
            _ => {}
        }
    }
    None
}

fn find_end_of_type(tokens: &[crate::parser::OwnedToken]) -> usize {
    let mut depth = 0usize;
    let mut angle = 0usize;
    for (idx, tok) in tokens.iter().enumerate() {
        match tok.kind {
            crate::lexer::TokenKind::LeftBracket => depth += 1,
            crate::lexer::TokenKind::RightBracket => {
                if depth == 0 {
                    return idx;
                }
                depth = depth.saturating_sub(1);
            }
            crate::lexer::TokenKind::Less => angle += 1,
            crate::lexer::TokenKind::Greater => {
                if angle == 0 {
                    return idx;
                }
                angle = angle.saturating_sub(1);
            }
            crate::lexer::TokenKind::LeftParen => depth += 1,
            crate::lexer::TokenKind::RightParen => {
                if depth == 0 {
                    return idx;
                }
                depth = depth.saturating_sub(1);
            }
            crate::lexer::TokenKind::Comma
            | crate::lexer::TokenKind::Assign
            | crate::lexer::TokenKind::Semicolon
            | crate::lexer::TokenKind::RightBrace
                if depth == 0 && angle == 0 =>
            {
                return idx;
            }
            _ => {}
        }
    }
    tokens.len()
}
