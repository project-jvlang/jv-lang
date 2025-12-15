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
use jv_ast::expression::{Expression, LogBlock, LogBlockLevel, LogItem, WhenArm};
use jv_ast::statement::{
    ConcurrencyConstruct, Property, ResourceManagement, Statement, TestDataset, TestDatasetRow,
    TestParameter, TestSampleMetadata, ValBindingOrigin,
};
use jv_ast::types::{
    GenericParameter, GenericSignature, Modifiers, Pattern, TypeAnnotation, UnitSymbol, WhereClause,
};

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
                .any(|t| t.kind == crate::lexer::TokenKind::While || t.lexeme_eq("do"))
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
            if let Some(expr) = lower_log_block_expression(ctx, node) {
                let span = expr.span().clone();
                out.push(Statement::Expression { expr, span });
            }
        }
        SyntaxKind::TestDeclaration => {
            if let Some(stmt) = lower_test(ctx, node) {
                out.push(stmt);
            }
        }
        SyntaxKind::UnitTypeDefinition => {
            if let Some(stmt) = lower_unit_type_definition(ctx, node) {
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

fn lower_log_block_expression(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Expression> {
    let tokens = collect_tokens(node);
    let keyword = tokens.first()?;
    let level = match keyword.kind {
        crate::lexer::TokenKind::Log => LogBlockLevel::Default,
        crate::lexer::TokenKind::Trace => LogBlockLevel::Trace,
        crate::lexer::TokenKind::Debug => LogBlockLevel::Debug,
        crate::lexer::TokenKind::Info => LogBlockLevel::Info,
        crate::lexer::TokenKind::Warn => LogBlockLevel::Warn,
        crate::lexer::TokenKind::Error => LogBlockLevel::Error,
        _ => return None,
    };

    let mut statements = Vec::new();
    if let Some(block) = find_child(node, SyntaxKind::Block) {
        for child in block.children.iter().filter_map(|c| match c {
            CstElement::Node(n) => Some(n),
            _ => None,
        }) {
            if child.kind == SyntaxKind::StatementList {
                lower_node(ctx, child, &mut statements);
            }
        }
    } else {
        ctx.push_diagnostic(LoweringDiagnostic::error(
            "JV-DSL-002: LOG ブロックは `{` で開始する必要があります",
            Some(ctx.span_for_token(keyword)),
        ));
    }

    let mut items = Vec::new();
    for stmt in statements {
        match stmt {
            Statement::Expression { expr, .. } => match expr {
                Expression::LogBlock(block) => items.push(LogItem::Nested(block)),
                other => items.push(LogItem::Expression(other)),
            },
            other => items.push(LogItem::Statement(other)),
        }
    }

    let span = span_for_node(ctx, node);
    let block = LogBlock { level, items, span };

    fn find_over_nested_span(block: &LogBlock, depth: usize) -> Option<jv_ast::Span> {
        for item in &block.items {
            let LogItem::Nested(inner) = item else {
                continue;
            };
            if depth >= 1 {
                return Some(inner.span.clone());
            }
            if let Some(span) = find_over_nested_span(inner, depth + 1) {
                return Some(span);
            }
        }
        None
    }

    if let Some(offending_span) = find_over_nested_span(&block, 0) {
        ctx.push_diagnostic(LoweringDiagnostic::error(
            "JV-DSL-001: ログブロックのネストは1段までです",
            Some(offending_span),
        ));
    }

    Some(Expression::LogBlock(block))
}

fn lower_import(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let mut path_parts = Vec::new();
    let mut is_wildcard = false;
    for child in &node.children {
        if let CstElement::Node(child_node) = child
            && child_node.kind == SyntaxKind::ImportPath
        {
            for tok in collect_tokens(child_node) {
                match tok.kind {
                    crate::lexer::TokenKind::Identifier => path_parts.push(tok.lexeme),
                    crate::lexer::TokenKind::Multiply => is_wildcard = true,
                    _ => {}
                }
            }
        }
    }

    let alias = find_descendant(node, SyntaxKind::ImportAlias)
        .and_then(|alias_node| collect_identifiers(alias_node).first().cloned());

    Some(Statement::Import {
        path: path_parts.join("."),
        alias,
        is_wildcard,
        span: span_for_node(ctx, node),
    })
}

fn lower_unit_type_definition(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let category = tokens
        .iter()
        .skip_while(|tok| tok.kind != crate::lexer::TokenKind::At)
        .skip(1)
        .find(|tok| tok.kind == crate::lexer::TokenKind::Identifier)
        .map(|tok| tok.lexeme_string())
        .unwrap_or_else(|| "Unit".to_string());

    let left_paren_idx = tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::LeftParen);
    let right_paren_idx = tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::RightParen);
    let base_type = if let (Some(l), Some(r)) = (left_paren_idx, right_paren_idx)
        && r > l
    {
        lower_type_annotation(ctx, &tokens[l + 1..r])
            .unwrap_or_else(|| TypeAnnotation::Simple("Any".to_string()))
    } else {
        TypeAnnotation::Simple("Any".to_string())
    };

    let symbol_token = right_paren_idx
        .and_then(|idx| tokens.get(idx + 1..))
        .and_then(|slice| {
            slice.iter().find(|tok| {
                !matches!(
                    tok.kind,
                    crate::lexer::TokenKind::Whitespace
                        | crate::lexer::TokenKind::Newline
                        | crate::lexer::TokenKind::LayoutComma
                        | crate::lexer::TokenKind::FieldNameLabel
                        | crate::lexer::TokenKind::LineComment
                        | crate::lexer::TokenKind::BlockComment
                        | crate::lexer::TokenKind::JavaDocComment
                        | crate::lexer::TokenKind::LeftBrace
                )
            })
        });
    let symbol_name = symbol_token
        .map(|tok| tok.lexeme_string())
        .unwrap_or_else(|| "_".to_string());
    let symbol_span = symbol_token
        .map(|tok| ctx.span_for_token(tok))
        .unwrap_or_else(|| span_for_node(ctx, node));
    let name = UnitSymbol {
        name: symbol_name,
        is_bracketed: false,
        has_default_marker: false,
        span: symbol_span,
    };

    Some(Statement::UnitTypeDefinition(
        jv_ast::statement::UnitTypeDefinition {
            category,
            base_type,
            name,
            members: Vec::new(),
            span: span_for_node(ctx, node),
        },
    ))
}

fn lower_val(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let assign_idx = tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::Assign)
        .unwrap_or(tokens.len());
    let header_tokens = tokens.get(..assign_idx).unwrap_or(&tokens);

    let pattern_tokens = slice_until(header_tokens, crate::lexer::TokenKind::Colon, |t| t.kind)
        .map(|(before, _)| before)
        .unwrap_or(header_tokens);
    let pattern_tokens = pattern_tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::Val)
        .and_then(|idx| pattern_tokens.get(idx + 1..))
        .unwrap_or(pattern_tokens);
    let binding_pattern = lower_binding_pattern(ctx, trim_trivia(pattern_tokens));
    let (name, binding) = match binding_pattern {
        Some(BindingPatternKind::Identifier { name, .. }) => (name, None),
        Some(pattern) => (
            pattern.first_identifier().unwrap_or("_").to_string(),
            Some(pattern),
        ),
        None => (
            header_tokens
                .iter()
                .find(|tok| tok.kind == crate::lexer::TokenKind::Identifier)
                .map(|tok| tok.lexeme_string())?,
            None,
        ),
    };

    let type_tokens = slice_until(header_tokens, crate::lexer::TokenKind::Colon, |t| t.kind);
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
    let mut initializer = find_descendant(node, SyntaxKind::WhenStatement)
        .and_then(|when_node| lower_when_expression(ctx, when_node))
        .or_else(|| {
            initializer_tokens
                .as_ref()
                .and_then(|slice| lower_expression(ctx, slice))
        })
        .unwrap_or_else(|| {
            let span = span_for_node(ctx, node);
            Expression::Literal(jv_ast::types::Literal::Null, span)
        });

    if let Some(binding_pattern) = binding.as_ref() {
        if let Expression::Tuple {
            elements, context, ..
        } = &mut initializer
        {
            context.in_destructuring_pattern = true;
            let pattern_len = match binding_pattern {
                BindingPatternKind::Tuple { elements, .. }
                | BindingPatternKind::List { elements, .. } => Some(elements.len()),
                _ => None,
            };
            if let Some(pattern_len) = pattern_len {
                if pattern_len > elements.len() {
                    ctx.push_diagnostic(LoweringDiagnostic::error(
                        "分割代入の要素が不足しています",
                        Some(span_for_node(ctx, node)),
                    ));
                } else if pattern_len < elements.len() {
                    ctx.push_diagnostic(LoweringDiagnostic::error(
                        "分割代入の要素が多すぎます",
                        Some(span_for_node(ctx, node)),
                    ));
                }
            }
        }
    }

    Some(Statement::ValDeclaration {
        name,
        binding,
        type_annotation,
        initializer,
        modifiers: Modifiers::default(),
        origin: ValBindingOrigin::ExplicitKeyword,
        span: span_for_node(ctx, node),
    })
}

fn lower_var(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let assign_idx = tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::Assign)
        .unwrap_or(tokens.len());
    let header_tokens = tokens.get(..assign_idx).unwrap_or(&tokens);

    let pattern_tokens = slice_until(header_tokens, crate::lexer::TokenKind::Colon, |t| t.kind)
        .map(|(before, _)| before)
        .unwrap_or(header_tokens);
    let pattern_tokens = pattern_tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::Var)
        .and_then(|idx| pattern_tokens.get(idx + 1..))
        .unwrap_or(pattern_tokens);
    let binding_pattern = lower_binding_pattern(ctx, trim_trivia(pattern_tokens));
    let (name, binding) = match binding_pattern {
        Some(BindingPatternKind::Identifier { name, .. }) => (name, None),
        Some(pattern) => (
            pattern.first_identifier().unwrap_or("_").to_string(),
            Some(pattern),
        ),
        None => (
            header_tokens
                .iter()
                .find(|tok| tok.kind == crate::lexer::TokenKind::Identifier)
                .map(|tok| tok.lexeme_string())?,
            None,
        ),
    };

    let type_tokens = slice_until(header_tokens, crate::lexer::TokenKind::Colon, |t| t.kind);
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
    let initializer = find_descendant(node, SyntaxKind::WhenStatement)
        .and_then(|when_node| lower_when_expression(ctx, when_node))
        .or_else(|| {
            initializer_tokens
                .as_ref()
                .and_then(|slice| lower_expression(ctx, slice))
        });

    Some(Statement::VarDeclaration {
        name,
        binding,
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
        .enumerate()
        .find_map(|(idx, tok)| {
            if matches!(
                tok.kind,
                crate::lexer::TokenKind::LeftBrace
                    | crate::lexer::TokenKind::Assign
                    | crate::lexer::TokenKind::FatArrow
            ) {
                Some(idx)
            } else {
                None
            }
        })
        .unwrap_or(tokens.len());
    let header_tokens = &tokens[..body_idx];

    let paren_idx = header_tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::LeftParen)
        .unwrap_or(header_tokens.len());
    let name_pos = header_tokens
        .get(..paren_idx)
        .unwrap_or(header_tokens)
        .iter()
        .rposition(|tok| tok.kind == crate::lexer::TokenKind::Identifier)?;
    let name = header_tokens.get(name_pos)?.lexeme_string();

    // 型パラメータ抽出（`fun <T, R: Bound>` を簡易対応）。
    let (type_parameters, generic_signature) = {
        let mut names = Vec::new();
        let mut parameters = Vec::new();

        let fun_idx = header_tokens
            .iter()
            .position(|t| t.kind == crate::lexer::TokenKind::Fun)
            .unwrap_or(0);
        let lt_idx = fun_idx.saturating_add(1);
        if header_tokens
            .get(lt_idx)
            .is_some_and(|t| t.kind == crate::lexer::TokenKind::Less)
        {
            let mut depth = 1usize;
            let mut idx = lt_idx.saturating_add(1);
            let mut segment_start = idx;
            while idx < header_tokens.len() && depth > 0 {
                match header_tokens[idx].kind {
                    crate::lexer::TokenKind::Less => depth += 1,
                    crate::lexer::TokenKind::Greater => {
                        depth = depth.saturating_sub(1);
                        if depth == 0 {
                            let segment = header_tokens.get(segment_start..idx).unwrap_or(&[]);
                            if let Some((name, param)) = parse_generic_parameter(ctx, segment) {
                                names.push(name);
                                parameters.push(param);
                            }
                            break;
                        }
                    }
                    crate::lexer::TokenKind::Comma if depth == 1 => {
                        let segment = header_tokens.get(segment_start..idx).unwrap_or(&[]);
                        if let Some((name, param)) = parse_generic_parameter(ctx, segment) {
                            names.push(name);
                            parameters.push(param);
                        }
                        segment_start = idx + 1;
                    }
                    _ => {}
                }
                idx += 1;
            }
        }

        let generic_signature = (!parameters.is_empty()).then(|| GenericSignature {
            parameters,
            const_parameters: Vec::new(),
            where_clause: None,
            raw_directives: Vec::new(),
            span: span_for_node(ctx, node),
        });

        (names, generic_signature)
    };

    // パラメータ抽出。
    let (params, params_close) = {
        let open = header_tokens
            .iter()
            .position(|t| t.kind == crate::lexer::TokenKind::LeftParen)
            .unwrap_or(header_tokens.len());
        let close = find_matching_paren(header_tokens, open).unwrap_or(header_tokens.len());
        let params = if close > open + 1 {
            parse_parameters(ctx, &header_tokens[open + 1..close])
        } else {
            Vec::new()
        };
        let close = (open < header_tokens.len()
            && close < header_tokens.len()
            && header_tokens
                .get(open)
                .is_some_and(|t| t.kind == crate::lexer::TokenKind::LeftParen)
            && header_tokens
                .get(close)
                .is_some_and(|t| t.kind == crate::lexer::TokenKind::RightParen))
        .then_some(close);
        (params, close)
    };

    let return_type = {
        // 戻り値型はパラメータリストの `)` 後の `:` を起点として解析する。
        // タプル戻り値 `(Int Int)` のように戻り値側にも `)` が含まれる場合、
        // 末尾の `)` を起点にすると `:` を見失うため、パラメータの閉じ括弧を優先する。
        let anchor = params_close.unwrap_or(name_pos);
        let colon_idx = header_tokens
            .iter()
            .enumerate()
            .skip(anchor + 1)
            .find_map(|(idx, tok)| {
                matches!(
                    tok.kind,
                    crate::lexer::TokenKind::Colon | crate::lexer::TokenKind::FieldNameLabel
                )
                .then_some(idx)
            });

        colon_idx.and_then(|idx| {
            let slice = trim_trivia(&header_tokens[idx + 1..]);
            if slice.is_empty() {
                None
            } else {
                let end = find_end_of_type(slice);
                lower_type_annotation(ctx, &slice[..end])
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
    } else if let Some(start) = tokens.iter().position(|t| {
        matches!(
            t.kind,
            crate::lexer::TokenKind::Assign | crate::lexer::TokenKind::FatArrow
        )
    }) {
        find_descendant(node, SyntaxKind::WhenStatement)
            .and_then(|when_node| lower_when_expression(ctx, when_node))
            .or_else(|| lower_expression(ctx, tokens.get(start + 1..).unwrap_or(&[])))
            .unwrap_or_else(|| {
                Expression::Literal(jv_ast::types::Literal::Null, span_for_node(ctx, node))
            })
    } else {
        Expression::Block {
            statements: Vec::new(),
            span: span_for_node(ctx, node),
        }
    };

    let function_decl = Statement::FunctionDeclaration {
        name,
        type_parameters,
        generic_signature,
        where_clause,
        parameters: params,
        return_type,
        primitive_return: None,
        body: Box::new(body),
        modifiers: Modifiers::default(),
        span: span_for_node(ctx, node),
    };

    let receiver_type = header_tokens.get(..name_pos).and_then(|before_name| {
        let dot_pos = before_name
            .iter()
            .rposition(|tok| tok.kind == crate::lexer::TokenKind::Dot)?;
        // `ReceiverType . functionName` の形のみを extension として扱う。
        if dot_pos + 1 != name_pos {
            return None;
        }
        let fun_idx = before_name
            .iter()
            .position(|t| t.kind == crate::lexer::TokenKind::Fun)
            .unwrap_or(0);
        let mut receiver_start = fun_idx.saturating_add(1);
        if before_name
            .get(receiver_start)
            .is_some_and(|t| t.kind == crate::lexer::TokenKind::Less)
        {
            let mut depth = 0usize;
            let mut idx = receiver_start;
            while idx < before_name.len() {
                match before_name[idx].kind {
                    crate::lexer::TokenKind::Less => depth += 1,
                    crate::lexer::TokenKind::Greater => {
                        depth = depth.saturating_sub(1);
                        if depth == 0 {
                            receiver_start = idx.saturating_add(1);
                            break;
                        }
                    }
                    _ => {}
                }
                idx += 1;
            }
        }
        let receiver_tokens = before_name.get(receiver_start..dot_pos).unwrap_or(&[]);
        if receiver_tokens.is_empty() {
            return None;
        }
        let end = find_end_of_type(receiver_tokens);
        lower_type_annotation(ctx, &receiver_tokens[..end])
    });

    if let Some(receiver_type) = receiver_type {
        Some(Statement::ExtensionFunction(
            jv_ast::statement::ExtensionFunction {
                receiver_type,
                function: Box::new(function_decl),
                span: span_for_node(ctx, node),
            },
        ))
    } else {
        Some(function_decl)
    }
}

fn lower_class_like(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let name_idx = tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::Identifier)?;
    let name = tokens.get(name_idx)?.lexeme_string();
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
                        params.push(tokens[idx].lexeme_string())
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
    if let Some(colon) = colon_idx
        && colon + 1 < inherit_end
    {
        let inheritance_slice = &tokens[colon + 1..inherit_end];
        let mut type_slices = split_types(inheritance_slice);
        if let Some(first) = type_slices.first() {
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
    let when_expr = lower_when_expression(ctx, node)?;
    let span = when_expr.span().clone();
    Some(Statement::Expression {
        span: span.clone(),
        expr: when_expr,
    })
}

fn lower_when_expression(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Expression> {
    let tokens = collect_tokens(node);
    let brace_idx = tokens
        .iter()
        .position(|t| t.kind == crate::lexer::TokenKind::LeftBrace)
        .unwrap_or(tokens.len());
    let condition_slice = tokens.get(1..brace_idx).unwrap_or(&[]);
    let condition = if condition_slice.is_empty() {
        None
    } else {
        lower_expression(ctx, condition_slice).map(Box::new)
    };

    let mut arms = Vec::new();
    let mut else_arm = None;
    for child in &node.children {
        let CstElement::Node(branch) = child else {
            continue;
        };
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

        let body = find_child(branch, SyntaxKind::Block)
            .map(|block| lower_block_expr(ctx, block))
            .or_else(|| lower_expression(ctx, body_tokens))
            .unwrap_or_else(|| {
                Expression::Literal(jv_ast::types::Literal::Null, span_for_node(ctx, branch))
            });

        if pat_tokens
            .iter()
            .any(|t| t.lexeme.eq_ignore_ascii_case("else"))
        {
            else_arm = Some(Box::new(body));
            continue;
        }

        let (pattern, guard) = lower_when_pattern(ctx, condition.as_deref(), branch, pat_tokens)
            .unwrap_or_else(|| (Pattern::Wildcard(span_for_node(ctx, branch)), None));
        arms.push(WhenArm {
            pattern,
            guard,
            body,
            span: span_for_node(ctx, branch),
        });
    }

    let span = span_for_node(ctx, node);
    Some(Expression::When {
        expr: condition,
        arms,
        else_arm,
        implicit_end: None,
        span,
    })
}

fn lower_when_pattern(
    ctx: &mut LoweringContext<'_>,
    subject: Option<&Expression>,
    branch: &CstNode,
    tokens: &[crate::parser::OwnedToken],
) -> Option<(Pattern, Option<Expression>)> {
    let span = span_for_node(ctx, branch);
    if subject.is_none() {
        return Some((Pattern::Wildcard(span), lower_expression(ctx, tokens)));
    }
    lower_pattern(ctx, tokens)
}

fn split_subjectless_is_guard(
    tokens: &[crate::parser::OwnedToken],
) -> Option<(&[crate::parser::OwnedToken], &[crate::parser::OwnedToken])> {
    let mut index = 0usize;
    while let Some(tok) = tokens.get(index) {
        if matches!(
            tok.kind,
            crate::lexer::TokenKind::Whitespace
                | crate::lexer::TokenKind::Newline
                | crate::lexer::TokenKind::LayoutComma
        ) {
            index += 1;
            continue;
        }
        break;
    }

    let tok = tokens.get(index)?;
    if tok.kind != crate::lexer::TokenKind::Identifier || !tok.lexeme_eq("is") {
        return None;
    }

    let after_is = tokens.get(index + 1..).unwrap_or(&[]);
    let type_end = find_guard_type_end(after_is);
    let (type_tokens, rest) = after_is.split_at(type_end);
    if type_tokens.is_empty() {
        return None;
    }
    Some((type_tokens, rest))
}

fn find_guard_type_end(tokens: &[crate::parser::OwnedToken]) -> usize {
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
            _ => {}
        }

        if depth == 0
            && angle == 0
            && matches!(
                tok.kind,
                crate::lexer::TokenKind::And | crate::lexer::TokenKind::Or
            )
        {
            return idx;
        }
    }
    tokens.len()
}

fn lower_guard_chain(
    ctx: &mut LoweringContext<'_>,
    head: &Expression,
    tokens: &[crate::parser::OwnedToken],
) -> Option<Expression> {
    let mut cursor = tokens;
    let mut acc = head.clone();

    loop {
        cursor = strip_trivia(cursor);
        let Some(op_tok) = cursor.first() else {
            break;
        };

        let op = match op_tok.kind {
            crate::lexer::TokenKind::And => jv_ast::types::BinaryOp::And,
            crate::lexer::TokenKind::Or => jv_ast::types::BinaryOp::Or,
            _ => break,
        };
        cursor = cursor.get(1..).unwrap_or(&[]);

        let rhs_end = find_chain_rhs_end(cursor);
        let rhs_tokens = cursor.get(..rhs_end).unwrap_or(&[]);
        let rhs_expr = lower_expression(ctx, rhs_tokens)?;

        let span = acc.span().merge(rhs_expr.span());
        acc = Expression::Binary {
            left: Box::new(acc),
            op,
            right: Box::new(rhs_expr),
            span,
            metadata: Default::default(),
        };

        cursor = cursor.get(rhs_end..).unwrap_or(&[]);
    }

    Some(acc)
}

fn strip_trivia(tokens: &[crate::parser::OwnedToken]) -> &[crate::parser::OwnedToken] {
    let mut index = 0usize;
    while let Some(tok) = tokens.get(index) {
        if matches!(
            tok.kind,
            crate::lexer::TokenKind::Whitespace
                | crate::lexer::TokenKind::Newline
                | crate::lexer::TokenKind::LayoutComma
        ) {
            index += 1;
            continue;
        }
        break;
    }
    tokens.get(index..).unwrap_or(&[])
}

fn find_chain_rhs_end(tokens: &[crate::parser::OwnedToken]) -> usize {
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
            _ => {}
        }

        if depth == 0
            && angle == 0
            && matches!(
                tok.kind,
                crate::lexer::TokenKind::And | crate::lexer::TokenKind::Or
            )
        {
            return idx;
        }
    }
    tokens.len()
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

    let (iterable, strategy) = match iterable {
        Expression::Binary {
            left,
            op:
                op @ (jv_ast::types::BinaryOp::RangeExclusive | jv_ast::types::BinaryOp::RangeInclusive),
            right,
            span,
            metadata,
        } => {
            let inclusive = matches!(op, jv_ast::types::BinaryOp::RangeInclusive);
            let range = jv_ast::statement::NumericRangeLoop {
                start: (*left).clone(),
                end: (*right).clone(),
                inclusive,
                span: span.clone(),
            };
            (
                Expression::Binary {
                    left,
                    op,
                    right,
                    span,
                    metadata,
                },
                jv_ast::statement::LoopStrategy::NumericRange(range),
            )
        }
        other => (other, jv_ast::statement::LoopStrategy::Iterable),
    };

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
        strategy,
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

    let dataset = extract_inline_test_dataset(ctx, &tokens).or_else(|| {
        dsl_test::extract_dataset(&tokens, ctx.source()).map(|name| {
            TestDataset::Sample(TestSampleMetadata {
                source: name,
                arguments: Vec::new(),
                span: span_for_node(ctx, node),
            })
        })
    });
    let parameters = extract_test_parameters(ctx, &tokens);
    let span = span_for_node(ctx, node);
    Some(Statement::TestDeclaration(
        jv_ast::statement::TestDeclaration {
            display_name: name.clone().unwrap_or_default(),
            normalized: name,
            dataset,
            parameters,
            annotations: Vec::new(),
            body,
            span,
        },
    ))
}

fn extract_inline_test_dataset(
    ctx: &mut LoweringContext<'_>,
    tokens: &[crate::parser::OwnedToken],
) -> Option<TestDataset> {
    let brace_idx = tokens
        .iter()
        .position(|t| t.kind == crate::lexer::TokenKind::LeftBrace)
        .unwrap_or(tokens.len());
    let header = tokens.get(..brace_idx).unwrap_or(tokens);
    let open = header.iter().enumerate().find_map(|(idx, tok)| {
        if tok.kind != crate::lexer::TokenKind::LeftBracket {
            return None;
        }
        let next_kind = header
            .get(idx + 1..)
            .unwrap_or(&[])
            .iter()
            .find(|t| {
                !matches!(
                    t.kind,
                    crate::lexer::TokenKind::Whitespace
                        | crate::lexer::TokenKind::Newline
                        | crate::lexer::TokenKind::LayoutComma
                        | crate::lexer::TokenKind::FieldNameLabel
                        | crate::lexer::TokenKind::LineComment
                        | crate::lexer::TokenKind::BlockComment
                        | crate::lexer::TokenKind::JavaDocComment
                )
            })
            .map(|t| t.kind);
        matches!(next_kind, Some(crate::lexer::TokenKind::LeftBracket)).then_some(idx)
    })?;
    let close = find_matching_bracket(header, open)?;
    let span = header
        .get(open)
        .and_then(|start| header.get(close).map(|end| ctx.span_for_range(start, end)))
        .unwrap_or_else(|| ctx.span_from_raw(crate::lexer::Span { start: 0, end: 0 }));
    let mut rows = Vec::new();
    let mut idx = open + 1;
    while idx < close {
        if header
            .get(idx)
            .is_some_and(|t| t.kind == crate::lexer::TokenKind::LeftBracket)
        {
            let row_close = match find_matching_bracket(header, idx) {
                Some(end) if end <= close => end,
                _ => break,
            };
            let row_tokens = header.get(idx..=row_close).unwrap_or(&[]);
            let row_span = row_tokens
                .first()
                .and_then(|start| row_tokens.last().map(|end| ctx.span_for_range(start, end)))
                .unwrap_or_else(|| span.clone());
            let inner = row_tokens
                .get(1..row_tokens.len().saturating_sub(1))
                .unwrap_or(&[]);

            let mut values = Vec::new();
            for value_tokens in split_test_dataset_row_values(inner) {
                if let Some(expr) = lower_expression(ctx, value_tokens) {
                    values.push(expr);
                }
            }
            rows.push(TestDatasetRow {
                values,
                span: row_span,
            });
            idx = row_close + 1;
            continue;
        }
        idx += 1;
    }
    (!rows.is_empty()).then_some(TestDataset::InlineArray { rows, span })
}

fn split_test_dataset_row_values(
    tokens: &[crate::parser::OwnedToken],
) -> Vec<&[crate::parser::OwnedToken]> {
    fn is_infix_operator(token: &crate::parser::OwnedToken) -> bool {
        use crate::lexer::TokenKind;
        matches!(
            token.kind,
            TokenKind::Or
                | TokenKind::And
                | TokenKind::Elvis
                | TokenKind::Equal
                | TokenKind::NotEqual
                | TokenKind::Less
                | TokenKind::LessEqual
                | TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::RangeExclusive
                | TokenKind::RangeInclusive
                | TokenKind::Multiply
                | TokenKind::Divide
                | TokenKind::Modulo
        ) || (token.kind == TokenKind::Identifier
            && (token.lexeme_eq("is") || token.lexeme_eq("as")))
    }

    fn is_postfix_continuation(token: &crate::parser::OwnedToken) -> bool {
        matches!(
            token.kind,
            crate::lexer::TokenKind::LeftParen
                | crate::lexer::TokenKind::LeftBracket
                | crate::lexer::TokenKind::LeftBrace
                | crate::lexer::TokenKind::Dot
                | crate::lexer::TokenKind::NullSafe
                | crate::lexer::TokenKind::Less
        )
    }

    let mut values = Vec::new();
    let mut start = 0usize;
    let mut paren_depth = 0isize;
    let mut bracket_depth = 0isize;
    let mut brace_depth = 0isize;

    let mut idx = 0usize;
    while idx < tokens.len() {
        let tok = &tokens[idx];
        match tok.kind {
            crate::lexer::TokenKind::LeftParen => paren_depth += 1,
            crate::lexer::TokenKind::RightParen => paren_depth -= 1,
            crate::lexer::TokenKind::LeftBracket => bracket_depth += 1,
            crate::lexer::TokenKind::RightBracket => bracket_depth -= 1,
            crate::lexer::TokenKind::LeftBrace => brace_depth += 1,
            crate::lexer::TokenKind::RightBrace => brace_depth -= 1,
            crate::lexer::TokenKind::Comma
                if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 =>
            {
                let slice = trim_trivia(tokens.get(start..idx).unwrap_or(&[]));
                if !slice.is_empty() {
                    values.push(slice);
                }
                start = idx + 1;
            }
            crate::lexer::TokenKind::Whitespace
            | crate::lexer::TokenKind::Newline
            | crate::lexer::TokenKind::LayoutComma
                if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 =>
            {
                let slice = trim_trivia(tokens.get(start..idx).unwrap_or(&[]));
                if slice.is_empty() {
                    start = idx + 1;
                    idx += 1;
                    continue;
                }

                values.push(slice);
                start = idx + 1;
                continue;
            }
            _ => {}
        }

        if idx > start
            && paren_depth == 0
            && bracket_depth == 0
            && brace_depth == 0
            && (tok.leading_trivia.newlines > 0 || tok.leading_trivia.spaces > 0)
            && !is_postfix_continuation(tok)
            && !is_infix_operator(tok)
        {
            let slice = trim_trivia(tokens.get(start..idx).unwrap_or(&[]));
            if !slice.is_empty() {
                values.push(slice);
            }
            start = idx;
        }

        idx += 1;
    }

    let slice = trim_trivia(tokens.get(start..).unwrap_or(&[]));
    if !slice.is_empty() {
        values.push(slice);
    }
    values
}

fn extract_test_parameters(
    ctx: &mut LoweringContext<'_>,
    tokens: &[crate::parser::OwnedToken],
) -> Vec<TestParameter> {
    let brace_idx = tokens
        .iter()
        .position(|t| t.kind == crate::lexer::TokenKind::LeftBrace)
        .unwrap_or(tokens.len());
    let header = tokens.get(..brace_idx).unwrap_or(tokens);
    let open = header
        .iter()
        .rposition(|t| t.kind == crate::lexer::TokenKind::LeftParen);
    let Some(open) = open else {
        return Vec::new();
    };
    let close = find_matching_paren(header, open).unwrap_or(header.len());
    if close <= open + 1 || close >= header.len() {
        return Vec::new();
    }
    lower_test_parameters(ctx, &header[open + 1..close])
}

fn lower_test_parameters(
    ctx: &mut LoweringContext<'_>,
    tokens: &[crate::parser::OwnedToken],
) -> Vec<TestParameter> {
    let mut params = Vec::new();
    let mut start = 0usize;
    let mut paren_depth = 0isize;
    let mut bracket_depth = 0isize;
    for (idx, tok) in tokens.iter().enumerate() {
        match tok.kind {
            crate::lexer::TokenKind::LeftParen => paren_depth += 1,
            crate::lexer::TokenKind::RightParen => paren_depth -= 1,
            crate::lexer::TokenKind::LeftBracket => bracket_depth += 1,
            crate::lexer::TokenKind::RightBracket => bracket_depth -= 1,
            crate::lexer::TokenKind::Comma
            | crate::lexer::TokenKind::LayoutComma
            | crate::lexer::TokenKind::Newline
                if paren_depth == 0 && bracket_depth == 0 =>
            {
                let slice = trim_trivia(tokens.get(start..idx).unwrap_or(&[]));
                if let Some(param) = lower_test_parameter(ctx, slice) {
                    params.push(param);
                }
                start = idx + 1;
            }
            _ => {}
        }
    }
    let slice = trim_trivia(tokens.get(start..).unwrap_or(&[]));
    if let Some(param) = lower_test_parameter(ctx, slice) {
        params.push(param);
    }
    params
}

fn lower_test_parameter(
    ctx: &mut LoweringContext<'_>,
    tokens: &[crate::parser::OwnedToken],
) -> Option<TestParameter> {
    let tokens = trim_trivia(tokens);
    if tokens.is_empty() {
        return None;
    }
    let colon = tokens
        .iter()
        .position(|t| t.kind == crate::lexer::TokenKind::Colon);
    let (pattern_tokens, type_tokens) = match colon {
        Some(idx) => (
            tokens.get(..idx).unwrap_or(&[]),
            tokens.get(idx + 1..).unwrap_or(&[]),
        ),
        None => (tokens, &[] as &[crate::parser::OwnedToken]),
    };
    let pattern = lower_binding_pattern(ctx, trim_trivia(pattern_tokens)).unwrap_or_else(|| {
        BindingPatternKind::wildcard(ctx.span_for_token(tokens.first().unwrap()))
    });
    let type_annotation = (!type_tokens.is_empty())
        .then(|| {
            let end = find_end_of_type(type_tokens);
            lower_type_annotation(ctx, &type_tokens[..end])
        })
        .flatten();
    let span = tokens
        .first()
        .and_then(|start| tokens.last().map(|end| ctx.span_for_range(start, end)))
        .unwrap_or_else(|| ctx.span_for_token(tokens.first().unwrap()));
    Some(TestParameter {
        pattern,
        type_annotation,
        span,
    })
}

fn find_matching_bracket(tokens: &[crate::parser::OwnedToken], open_idx: usize) -> Option<usize> {
    if open_idx >= tokens.len() || tokens[open_idx].kind != crate::lexer::TokenKind::LeftBracket {
        return None;
    }
    let mut depth = 0usize;
    for (idx, tok) in tokens.iter().enumerate().skip(open_idx + 1) {
        match tok.kind {
            crate::lexer::TokenKind::LeftBracket => depth += 1,
            crate::lexer::TokenKind::RightBracket => {
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

fn lower_use(ctx: &mut LoweringContext<'_>, node: &CstNode) -> Option<Statement> {
    let tokens = collect_tokens(node);
    let keyword = tokens
        .iter()
        .find(|tok| tok.lexeme_eq("use"))
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
        .position(|tok| tok.lexeme_eq("use"))
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
        .find(|tok| tok.lexeme_eq("defer"))
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
        .position(|tok| tok.lexeme_eq("defer"))
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

    if let Some(colon_idx) = lhs
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::Colon)
    {
        let (before_colon, after_colon) = lhs.split_at(colon_idx);
        let has_non_trivia = before_colon.iter().any(|tok| {
            !matches!(
                tok.kind,
                crate::lexer::TokenKind::Whitespace
                    | crate::lexer::TokenKind::Newline
                    | crate::lexer::TokenKind::LayoutComma
                    | crate::lexer::TokenKind::FieldNameLabel
                    | crate::lexer::TokenKind::LineComment
                    | crate::lexer::TokenKind::BlockComment
                    | crate::lexer::TokenKind::JavaDocComment
                    | crate::lexer::TokenKind::Identifier
            )
        });
        if !has_non_trivia {
            if let Some(name) = before_colon
                .iter()
                .find(|tok| tok.kind == crate::lexer::TokenKind::Identifier)
                .map(|tok| tok.lexeme_string())
            {
                let type_tokens = after_colon.get(1..).unwrap_or_default();
                let end = find_end_of_type(type_tokens);
                let type_annotation = lower_type_annotation(ctx, &type_tokens[..end]);
                let initializer = find_descendant(node, SyntaxKind::WhenStatement)
                    .and_then(|when_node| lower_when_expression(ctx, when_node))
                    .or_else(|| lower_expression(ctx, rhs))
                    .unwrap_or_else(|| {
                        Expression::Literal(jv_ast::types::Literal::Null, span_for_node(ctx, node))
                    });
                return Some(Statement::ValDeclaration {
                    name,
                    binding: None,
                    type_annotation,
                    initializer,
                    modifiers: Modifiers::default(),
                    origin: ValBindingOrigin::ImplicitTyped,
                    span: span_for_node(ctx, node),
                });
            }
        }
    }

    let target = lower_expression(ctx, lhs).unwrap_or_else(|| {
        Expression::Literal(jv_ast::types::Literal::Null, span_for_node(ctx, node))
    });
    let value = find_descendant(node, SyntaxKind::WhenStatement)
        .and_then(|when_node| lower_when_expression(ctx, when_node))
        .or_else(|| lower_expression(ctx, rhs))?;
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
    let tokens = trim_trivia(tokens);
    if tokens.is_empty() {
        return None;
    }

    fn split_top_level_guard(tokens: &[crate::parser::OwnedToken]) -> Option<usize> {
        let mut brace_depth: isize = 0;
        let mut paren_depth: isize = 0;
        let mut bracket_depth: isize = 0;
        for (idx, tok) in tokens.iter().enumerate() {
            match tok.kind {
                crate::lexer::TokenKind::LeftBrace => brace_depth += 1,
                crate::lexer::TokenKind::RightBrace => brace_depth -= 1,
                crate::lexer::TokenKind::LeftParen => paren_depth += 1,
                crate::lexer::TokenKind::RightParen => paren_depth -= 1,
                crate::lexer::TokenKind::LeftBracket => bracket_depth += 1,
                crate::lexer::TokenKind::RightBracket => bracket_depth -= 1,
                crate::lexer::TokenKind::And
                    if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 =>
                {
                    return Some(idx);
                }
                _ => {}
            }
        }
        None
    }

    fn span_for_tokens(
        ctx: &LoweringContext<'_>,
        tokens: &[crate::parser::OwnedToken],
    ) -> jv_ast::Span {
        tokens
            .first()
            .and_then(|start| tokens.last().map(|end| ctx.span_for_range(start, end)))
            .unwrap_or_else(|| ctx.span_from_raw(crate::lexer::Span { start: 0, end: 0 }))
    }

    fn split_constructor_args(
        tokens: &[crate::parser::OwnedToken],
    ) -> Vec<&[crate::parser::OwnedToken]> {
        let mut args = Vec::new();
        let mut start = 0usize;
        let mut paren_depth = 0isize;
        let mut bracket_depth = 0isize;
        let mut brace_depth = 0isize;
        for (idx, tok) in tokens.iter().enumerate() {
            match tok.kind {
                crate::lexer::TokenKind::LeftParen => paren_depth += 1,
                crate::lexer::TokenKind::RightParen => paren_depth -= 1,
                crate::lexer::TokenKind::LeftBracket => bracket_depth += 1,
                crate::lexer::TokenKind::RightBracket => bracket_depth -= 1,
                crate::lexer::TokenKind::LeftBrace => brace_depth += 1,
                crate::lexer::TokenKind::RightBrace => brace_depth -= 1,
                crate::lexer::TokenKind::Comma
                    if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 =>
                {
                    let slice = trim_trivia(tokens.get(start..idx).unwrap_or(&[]));
                    if !slice.is_empty() {
                        args.push(slice);
                    }
                    start = idx + 1;
                }
                _ => {}
            }
        }
        let slice = trim_trivia(tokens.get(start..).unwrap_or(&[]));
        if !slice.is_empty() {
            args.push(slice);
        }
        args
    }

    fn lower_base_pattern(
        ctx: &mut LoweringContext<'_>,
        tokens: &[crate::parser::OwnedToken],
    ) -> Pattern {
        let tokens = trim_trivia(tokens);
        if tokens.is_empty() {
            return Pattern::Wildcard(ctx.span_from_raw(crate::lexer::Span { start: 0, end: 0 }));
        }

        if tokens
            .first()
            .is_some_and(|t| t.kind == crate::lexer::TokenKind::In)
        {
            let after_in = trim_trivia(tokens.get(1..).unwrap_or(&[]));
            if let Some(expr) = lower_expression(ctx, after_in) {
                if let Expression::Binary {
                    op, left, right, ..
                } = expr
                    && matches!(
                        op,
                        jv_ast::types::BinaryOp::RangeExclusive
                            | jv_ast::types::BinaryOp::RangeInclusive
                    )
                {
                    return Pattern::Range {
                        start: left,
                        end: right,
                        inclusive_end: matches!(op, jv_ast::types::BinaryOp::RangeInclusive),
                        span: span_for_tokens(ctx, tokens),
                    };
                }
            }
            return Pattern::Wildcard(span_for_tokens(ctx, tokens));
        }

        if tokens
            .first()
            .is_some_and(|t| t.kind == crate::lexer::TokenKind::Identifier && t.lexeme_eq("is"))
        {
            let type_tokens = trim_trivia(tokens.get(1..).unwrap_or(&[]));
            let name = type_tokens
                .iter()
                .find(|t| t.kind == crate::lexer::TokenKind::Identifier)
                .map(|t| t.lexeme_string())
                .unwrap_or_default();
            return Pattern::Constructor {
                name,
                patterns: Vec::new(),
                span: span_for_tokens(ctx, tokens),
            };
        }

        if tokens.len() >= 2
            && tokens[0].kind == crate::lexer::TokenKind::Identifier
            && tokens
                .iter()
                .skip(1)
                .find(|t| {
                    !matches!(
                        t.kind,
                        crate::lexer::TokenKind::Whitespace
                            | crate::lexer::TokenKind::Newline
                            | crate::lexer::TokenKind::LayoutComma
                    )
                })
                .is_some_and(|t| t.kind == crate::lexer::TokenKind::LeftParen)
        {
            let name = tokens[0].lexeme_string();
            let open_idx = tokens
                .iter()
                .position(|t| t.kind == crate::lexer::TokenKind::LeftParen)
                .unwrap_or(1);
            if let Some(close_idx) = find_matching_paren(tokens, open_idx)
                && close_idx > open_idx
                && tokens
                    .get(close_idx)
                    .is_some_and(|t| t.kind == crate::lexer::TokenKind::RightParen)
            {
                let inner = tokens.get(open_idx + 1..close_idx).unwrap_or(&[]);
                let mut patterns = Vec::new();
                for arg in split_constructor_args(inner) {
                    patterns.push(lower_base_pattern(ctx, arg));
                }
                return Pattern::Constructor {
                    name,
                    patterns,
                    span: span_for_tokens(ctx, tokens),
                };
            }
        }

        if tokens.len() == 1 && tokens[0].kind == crate::lexer::TokenKind::Underscore {
            return Pattern::Wildcard(span_for_tokens(ctx, tokens));
        }

        if let Some(expr) = lower_expression(ctx, tokens) {
            match expr {
                Expression::Identifier(name, span) if name == "_" => Pattern::Wildcard(span),
                Expression::Identifier(name, span) => Pattern::Identifier(name, span),
                Expression::Literal(lit, span) => Pattern::Literal(lit, span),
                other => {
                    ctx.push_diagnostic(LoweringDiagnostic::warning(
                        "LWR003: when パターンを解釈できませんでした。ワイルドカードとして扱います。",
                        Some(other.span().clone()),
                    ));
                    Pattern::Wildcard(span_for_tokens(ctx, tokens))
                }
            }
        } else {
            Pattern::Wildcard(span_for_tokens(ctx, tokens))
        }
    }

    let (base_tokens, guard_tokens) = match split_top_level_guard(tokens) {
        Some(and_idx) => {
            let (lhs, rest) = tokens.split_at(and_idx);
            let rhs = trim_trivia(rest.get(1..).unwrap_or(&[]));
            (trim_trivia(lhs), rhs)
        }
        None => (tokens, &[][..]),
    };

    let pattern = lower_base_pattern(ctx, base_tokens);
    let guard = if guard_tokens.is_empty() {
        None
    } else {
        lower_expression(ctx, guard_tokens)
    };
    Some((pattern, guard))
}

fn lower_binding_pattern(
    ctx: &mut LoweringContext<'_>,
    tokens: &[crate::parser::OwnedToken],
) -> Option<BindingPatternKind> {
    let tokens = trim_trivia(tokens);
    if tokens.is_empty() {
        return None;
    }

    let fallback_span = tokens
        .first()
        .map(|t| ctx.span_for_token(t))
        .unwrap_or_else(|| ctx.span_from_raw(crate::lexer::Span { start: 0, end: 0 }));

    if tokens
        .first()
        .is_some_and(|t| t.kind == crate::lexer::TokenKind::LeftParen)
    {
        let close = find_matching_paren(tokens, 0).unwrap_or(tokens.len());
        if close < tokens.len()
            && tokens
                .get(close)
                .is_some_and(|t| t.kind == crate::lexer::TokenKind::RightParen)
        {
            let mut elements = Vec::new();
            for slice in split_binding_elements(tokens.get(1..close).unwrap_or(&[])) {
                if let Some(pattern) = lower_binding_pattern(ctx, slice) {
                    elements.push(pattern);
                }
            }
            let open_span = tokens
                .first()
                .map(|t| ctx.span_for_token(t))
                .unwrap_or_else(|| fallback_span.clone());
            let close_span = tokens
                .get(close)
                .map(|t| ctx.span_for_token(t))
                .unwrap_or_else(|| fallback_span.clone());
            let span = open_span.merge(&close_span);
            return Some(BindingPatternKind::Tuple { elements, span });
        }
    }

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

fn split_binding_elements(
    tokens: &[crate::parser::OwnedToken],
) -> Vec<&[crate::parser::OwnedToken]> {
    let mut elements = Vec::new();
    let mut start = 0usize;
    let mut paren_depth = 0isize;
    let mut bracket_depth = 0isize;
    let mut last_non_trivia: Option<crate::lexer::TokenKind> = None;
    for (idx, tok) in tokens.iter().enumerate() {
        match tok.kind {
            crate::lexer::TokenKind::LeftParen => paren_depth += 1,
            crate::lexer::TokenKind::RightParen => paren_depth -= 1,
            crate::lexer::TokenKind::LeftBracket => bracket_depth += 1,
            crate::lexer::TokenKind::RightBracket => bracket_depth -= 1,
            crate::lexer::TokenKind::Comma
            | crate::lexer::TokenKind::LayoutComma
            | crate::lexer::TokenKind::Newline
            | crate::lexer::TokenKind::Whitespace
                if paren_depth == 0 && bracket_depth == 0 =>
            {
                let slice = trim_trivia(tokens.get(start..idx).unwrap_or(&[]));
                if !slice.is_empty() {
                    elements.push(slice);
                }
                start = idx + 1;
            }
            crate::lexer::TokenKind::Identifier | crate::lexer::TokenKind::Underscore
                if paren_depth == 0
                    && bracket_depth == 0
                    && matches!(
                        last_non_trivia,
                        Some(
                            crate::lexer::TokenKind::Identifier
                                | crate::lexer::TokenKind::Underscore
                        )
                    ) =>
            {
                let slice = trim_trivia(tokens.get(start..idx).unwrap_or(&[]));
                if !slice.is_empty() {
                    elements.push(slice);
                }
                start = idx;
            }
            _ => {}
        }

        if !matches!(
            tok.kind,
            crate::lexer::TokenKind::Whitespace
                | crate::lexer::TokenKind::Newline
                | crate::lexer::TokenKind::LayoutComma
                | crate::lexer::TokenKind::FieldNameLabel
                | crate::lexer::TokenKind::LineComment
                | crate::lexer::TokenKind::BlockComment
                | crate::lexer::TokenKind::JavaDocComment
        ) {
            last_non_trivia = Some(tok.kind);
        }
    }
    let slice = trim_trivia(tokens.get(start..).unwrap_or(&[]));
    if !slice.is_empty() {
        elements.push(slice);
    }
    elements
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

fn trim_trivia(tokens: &[crate::parser::OwnedToken]) -> &[crate::parser::OwnedToken] {
    let mut start = 0usize;
    let mut end = tokens.len();
    while start < end {
        if matches!(
            tokens[start].kind,
            crate::lexer::TokenKind::Whitespace
                | crate::lexer::TokenKind::Newline
                | crate::lexer::TokenKind::LayoutComma
                | crate::lexer::TokenKind::FieldNameLabel
                | crate::lexer::TokenKind::LineComment
                | crate::lexer::TokenKind::BlockComment
                | crate::lexer::TokenKind::JavaDocComment
        ) {
            start += 1;
        } else {
            break;
        }
    }
    while end > start {
        if matches!(
            tokens[end - 1].kind,
            crate::lexer::TokenKind::Whitespace
                | crate::lexer::TokenKind::Newline
                | crate::lexer::TokenKind::LayoutComma
                | crate::lexer::TokenKind::FieldNameLabel
                | crate::lexer::TokenKind::LineComment
                | crate::lexer::TokenKind::BlockComment
                | crate::lexer::TokenKind::JavaDocComment
        ) {
            end -= 1;
        } else {
            break;
        }
    }
    tokens.get(start..end).unwrap_or(&[])
}

fn collect_identifiers(node: &CstNode) -> Vec<String> {
    collect_tokens(node)
        .into_iter()
        .filter(|tok| tok.kind == crate::lexer::TokenKind::Identifier)
        .map(|tok| tok.lexeme_string())
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
            lexeme: "".into(),
            leading_trivia: Default::default(),
            metadata: Vec::new(),
            diagnostic: None,
        });
    let end = tokens.last().unwrap_or(&start).clone();
    ctx.span_for_range(&start, &end)
}

fn find_child(node: &CstNode, kind: SyntaxKind) -> Option<&CstNode> {
    node.children.iter().find_map(|child| match child {
        CstElement::Node(n) if n.kind == kind => Some(n),
        _ => None,
    })
}

fn find_descendant(node: &CstNode, kind: SyntaxKind) -> Option<&CstNode> {
    for child in &node.children {
        let CstElement::Node(child_node) = child else {
            continue;
        };
        if child_node.kind == kind {
            return Some(child_node);
        }
        if let Some(found) = find_descendant(child_node, kind.clone()) {
            return Some(found);
        }
    }
    None
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

fn parse_generic_parameter(
    ctx: &mut LoweringContext<'_>,
    tokens: &[crate::parser::OwnedToken],
) -> Option<(String, GenericParameter)> {
    let tokens: Vec<_> = tokens
        .iter()
        .filter(|tok| {
            !matches!(
                tok.kind,
                crate::lexer::TokenKind::Whitespace
                    | crate::lexer::TokenKind::Newline
                    | crate::lexer::TokenKind::LayoutComma
                    | crate::lexer::TokenKind::FieldNameLabel
                    | crate::lexer::TokenKind::LineComment
                    | crate::lexer::TokenKind::BlockComment
                    | crate::lexer::TokenKind::JavaDocComment
            )
        })
        .cloned()
        .collect();
    let name_tok = tokens
        .iter()
        .find(|tok| tok.kind == crate::lexer::TokenKind::Identifier)?;
    let name = name_tok.lexeme_string();

    let bounds = tokens
        .iter()
        .position(|tok| tok.kind == crate::lexer::TokenKind::Colon)
        .and_then(|colon_idx| tokens.get(colon_idx + 1..))
        .and_then(|slice| {
            if slice.is_empty() {
                None
            } else {
                let end = find_end_of_type(slice);
                lower_type_annotation(ctx, &slice[..end])
            }
        })
        .into_iter()
        .collect();

    let span = ctx.span_for_token(name_tok);
    Some((
        name.clone(),
        GenericParameter {
            name,
            bounds,
            variance: None,
            default: None,
            kind: None,
            span,
        },
    ))
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
            | crate::lexer::TokenKind::Where
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
