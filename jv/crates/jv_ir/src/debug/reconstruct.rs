use std::borrow::Cow;

use crate::types::{
    IrCommentKind, IrExpression, IrImportDetail, IrModifiers, IrParameter, IrProgram, IrStatement,
    IrVisibility, JavaType, JavaWildcardKind,
};
use jv_ast::{
    Argument, CallArgumentMetadata, CommentKind, CommentStatement, CommentVisibility, Expression,
    Literal, Modifiers, Program, RegexLiteral, Span, Statement, StringPart, TypeAnnotation,
    ValBindingOrigin, Visibility,
};

fn render_type_annotation(annotation: TypeAnnotation) -> String {
    match annotation {
        TypeAnnotation::Simple(name) => name,
        TypeAnnotation::Array(inner) => format!("{}[]", render_type_annotation(*inner)),
        TypeAnnotation::Function {
            params,
            return_type,
        } => {
            let params = params
                .into_iter()
                .map(render_type_annotation)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({}) -> {}", params, render_type_annotation(*return_type))
        }
        other => format!("{:?}", other),
    }
}

use super::{
    ReconstructionError, ReconstructionOptions, ReconstructionStats, ReconstructionWarning,
    WarningKind,
};

pub(crate) struct ReconstructionOutput {
    pub program: Program,
    pub warnings: Vec<ReconstructionWarning>,
    pub stats: ReconstructionStats,
}

pub(crate) fn reconstruct_program(
    opts: &ReconstructionOptions,
    program: &IrProgram,
) -> Result<ReconstructionOutput, ReconstructionError> {
    let mut ctx = ReconstructionContext::new(opts);
    let program = ctx.with_segment("program", |ctx| ctx.convert_program(program))?;

    let ReconstructionContext {
        warnings, stats, ..
    } = ctx;

    Ok(ReconstructionOutput {
        program,
        warnings,
        stats,
    })
}

struct ReconstructionContext<'a> {
    opts: &'a ReconstructionOptions,
    warnings: Vec<ReconstructionWarning>,
    stats: ReconstructionStats,
    path: Vec<String>,
}

impl<'a> ReconstructionContext<'a> {
    fn new(opts: &'a ReconstructionOptions) -> Self {
        Self {
            opts,
            warnings: Vec::new(),
            stats: ReconstructionStats::default(),
            path: Vec::new(),
        }
    }

    fn visit_node(&mut self) {
        self.stats.total_nodes += 1;
    }

    fn record_success(&mut self) {
        self.stats.reconstructed_nodes += 1;
    }

    fn record_placeholder(&mut self) {
        self.stats.placeholder_nodes += 1;
    }

    fn current_path(&self) -> Cow<'_, str> {
        if self.path.is_empty() {
            Cow::Borrowed("program")
        } else {
            Cow::Owned(self.path.join("."))
        }
    }

    fn push_path(&mut self, segment: impl Into<String>) {
        self.path.push(segment.into());
    }

    fn pop_path(&mut self) {
        self.path.pop();
    }

    fn with_segment<F, T>(
        &mut self,
        segment: impl Into<String>,
        f: F,
    ) -> Result<T, ReconstructionError>
    where
        F: FnOnce(&mut Self) -> Result<T, ReconstructionError>,
    {
        self.push_path(segment);
        let result = f(self);
        self.pop_path();
        result
    }

    fn emit_warning(&mut self, kind: WarningKind, span: Option<Span>, message: impl Into<String>) {
        let warning = ReconstructionWarning::new(span, self.current_path(), kind, message);
        self.warnings.push(warning);
    }

    fn convert_program(&mut self, program: &IrProgram) -> Result<Program, ReconstructionError> {
        self.visit_node();

        let imports = program
            .imports
            .iter()
            .enumerate()
            .filter_map(|(idx, stmt)| {
                self.with_segment(format!("imports[{idx}]"), |ctx| ctx.convert_import(stmt))
                    .transpose()
            })
            .collect::<Result<Vec<_>, _>>()?;

        let statements = program
            .type_declarations
            .iter()
            .enumerate()
            .map(|(idx, stmt)| {
                self.with_segment(format!("statements[{idx}]"), |ctx| {
                    ctx.convert_statement(stmt)
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        self.record_success();

        Ok(Program {
            package: program.package.clone(),
            imports,
            statements,
            span: program.span.clone(),
        })
    }

    fn convert_import(
        &mut self,
        stmt: &IrStatement,
    ) -> Result<Option<Statement>, ReconstructionError> {
        self.visit_node();
        match stmt {
            IrStatement::Import(import) => {
                self.record_success();
                let (path, is_wildcard) = match &import.detail {
                    IrImportDetail::Type { fqcn } => (fqcn.clone(), false),
                    IrImportDetail::Package { name } => (name.clone(), true),
                    IrImportDetail::Static { owner, member } => {
                        if member == "*" {
                            (format!("{owner}.*"), true)
                        } else {
                            (format!("{owner}.{member}"), false)
                        }
                    }
                    IrImportDetail::Module { .. } => {
                        let placeholder = self.insert_placeholder_statement(
                            Some(import.span.clone()),
                            WarningKind::UnsupportedNode,
                            "module import を AST へ再構成する処理は未対応です",
                        )?;
                        return Ok(Some(placeholder));
                    }
                };

                Ok(Some(Statement::Import {
                    path,
                    alias: import.alias.clone(),
                    is_wildcard,
                    span: import.span.clone(),
                }))
            }
            IrStatement::Package { name, span } => {
                self.record_success();
                Ok(Some(Statement::Package {
                    name: name.clone(),
                    span: span.clone(),
                }))
            }
            other => {
                let span = extract_span(other);
                let placeholder = self.insert_placeholder_statement(
                    span,
                    WarningKind::UnsupportedNode,
                    "インポート領域でサポートされていない IR ノードを検出しました",
                )?;
                Ok(Some(placeholder))
            }
        }
    }

    fn convert_statement(&mut self, stmt: &IrStatement) -> Result<Statement, ReconstructionError> {
        self.visit_node();
        match stmt {
            IrStatement::Commented { statement, .. } => self.convert_statement(statement),
            IrStatement::Comment { kind, text, span } => {
                self.record_success();
                let comment_kind = match kind {
                    IrCommentKind::Line => CommentKind::Line,
                    IrCommentKind::Block => CommentKind::Block,
                };
                Ok(Statement::Comment(CommentStatement {
                    kind: comment_kind,
                    visibility: CommentVisibility::Passthrough,
                    text: text.clone(),
                    span: span.clone(),
                }))
            }
            IrStatement::VariableDeclaration {
                name,
                java_type,
                initializer,
                is_final,
                modifiers,
                span,
            } => {
                let type_annotation = Some(self.convert_java_type(java_type));
                let initializer_expr = match (is_final, initializer) {
                    (_, Some(expr)) => {
                        Some(self.with_segment("initializer", |ctx| ctx.convert_expression(expr))?)
                    }
                    (true, None) => {
                        self.emit_warning(
                            WarningKind::MissingMetadata,
                            Some(span.clone()),
                            "final 変数に初期化子がありません。Null プレースホルダーを挿入します",
                        );
                        Some(self.placeholder_expression(
                            span,
                            WarningKind::MissingMetadata,
                            "final 変数の初期化子が欠落しています",
                        )?)
                    }
                    (false, None) => None,
                };

                let modifiers = self.convert_modifiers(modifiers);

                self.record_success();

                if *is_final {
                    let initializer = initializer_expr
                        .unwrap_or_else(|| Expression::Literal(Literal::Null, span.clone()));
                    Ok(Statement::ValDeclaration {
                        name: name.clone(),
                        binding: None,

                        type_annotation,
                        initializer,
                        modifiers,
                        origin: ValBindingOrigin::ExplicitKeyword,
                        span: span.clone(),
                    })
                } else {
                    Ok(Statement::VarDeclaration {
                        name: name.clone(),
                        binding: None,
                        type_annotation,
                        initializer: initializer_expr,
                        modifiers,
                        span: span.clone(),
                    })
                }
            }
            IrStatement::Expression { expr, span } => {
                let expression = self.with_segment("expr", |ctx| ctx.convert_expression(expr))?;
                self.record_success();
                Ok(Statement::Expression {
                    expr: expression,
                    span: span.clone(),
                })
            }
            IrStatement::Return { value, span } => {
                let value = match value {
                    Some(expr) => {
                        Some(self.with_segment("value", |ctx| ctx.convert_expression(expr))?)
                    }
                    None => None,
                };
                self.record_success();
                Ok(Statement::Return {
                    label: None,
                    value,
                    span: span.clone(),
                })
            }
            IrStatement::Throw { expr, span } => {
                let value = self.with_segment("expr", |ctx| ctx.convert_expression(expr))?;
                self.record_success();
                Ok(Statement::Throw {
                    expr: value,
                    span: span.clone(),
                })
            }
            IrStatement::Block { statements, span } => {
                let block_expr =
                    self.with_segment("block", |ctx| ctx.convert_block(statements, span))?;
                self.record_success();
                Ok(Statement::Expression {
                    expr: block_expr,
                    span: span.clone(),
                })
            }
            IrStatement::Package { name, span } => {
                self.record_success();
                Ok(Statement::Package {
                    name: name.clone(),
                    span: span.clone(),
                })
            }
            IrStatement::MethodDeclaration {
                name,
                type_parameters,
                parameters,
                primitive_return,
                return_type,
                body,
                modifiers,
                span,
                ..
            } => {
                let params = parameters
                    .iter()
                    .enumerate()
                    .map(|(idx, param)| {
                        self.with_segment(format!("param[{idx}]"), |ctx| {
                            ctx.convert_parameter(param)
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let return_type = if matches!(return_type, JavaType::Void) {
                    None
                } else {
                    Some(self.convert_java_type(return_type))
                };

                let body_expr = match body {
                    Some(expr) => self.with_segment("body", |ctx| ctx.convert_expression(expr))?,
                    None => self.placeholder_expression(
                        span,
                        WarningKind::MissingMetadata,
                        "抽象メソッド本体が存在しないため空ブロックを生成しました",
                    )?,
                };

                let modifiers = self.convert_modifiers(modifiers);

                self.record_success();

                Ok(Statement::FunctionDeclaration {
                    name: name.clone(),
                    type_parameters: type_parameters.iter().map(|tp| tp.name.clone()).collect(),
                    generic_signature: None,
                    where_clause: None,
                    parameters: params,
                    return_type,
                    primitive_return: primitive_return.clone(),
                    body: Box::new(body_expr),
                    modifiers,
                    span: span.clone(),
                })
            }
            other => {
                let span = extract_span(other);
                self.insert_placeholder_statement(
                    span,
                    WarningKind::UnsupportedNode,
                    "未対応の IR ステートメントをプレースホルダーに置き換えました",
                )
            }
        }
    }

    fn convert_block(
        &mut self,
        statements: &[IrStatement],
        span: &Span,
    ) -> Result<Expression, ReconstructionError> {
        self.visit_node();

        let stmts = statements
            .iter()
            .enumerate()
            .map(|(idx, stmt)| {
                self.with_segment(format!("block[{idx}]"), |ctx| ctx.convert_statement(stmt))
            })
            .collect::<Result<Vec<_>, _>>()?;

        self.record_success();

        Ok(Expression::Block {
            statements: stmts,
            label: None,
            span: span.clone(),
        })
    }

    fn convert_expression(
        &mut self,
        expr: &IrExpression,
    ) -> Result<Expression, ReconstructionError> {
        self.visit_node();
        let expression = match expr {
            IrExpression::Literal(literal, span) => {
                self.record_success();
                Expression::Literal(literal.clone(), span.clone())
            }
            IrExpression::RegexPattern { pattern, span, .. } => {
                self.record_success();
                let escaped = pattern.replace('/', "\\/");
                Expression::RegexLiteral(RegexLiteral {
                    pattern: pattern.clone(),
                    raw: format!("/{}/", escaped),
                    span: span.clone(),
                })
            }
            IrExpression::Identifier { name, span, .. } => {
                self.record_success();
                Expression::Identifier(name.clone(), span.clone())
            }
            IrExpression::Binary {
                left,
                op,
                right,
                span,
                ..
            } => {
                let left = self.with_segment("left", |ctx| ctx.convert_expression(left))?;
                let right = self.with_segment("right", |ctx| ctx.convert_expression(right))?;
                self.record_success();
                Expression::Binary {
                    left: Box::new(left),
                    op: op.clone(),
                    right: Box::new(right),
                    span: span.clone(),
                }
            }
            IrExpression::Unary {
                op, operand, span, ..
            } => {
                let operand =
                    self.with_segment("operand", |ctx| ctx.convert_expression(operand))?;
                self.record_success();
                Expression::Unary {
                    op: op.clone(),
                    operand: Box::new(operand),
                    span: span.clone(),
                }
            }
            IrExpression::MethodCall {
                receiver,
                method_name,
                args,
                argument_style,
                span,
                ..
            } => {
                let function = if let Some(receiver) = receiver {
                    let object =
                        self.with_segment("receiver", |ctx| ctx.convert_expression(receiver))?;
                    Expression::MemberAccess {
                        object: Box::new(object),
                        property: method_name.clone(),
                        span: span.clone(),
                    }
                } else {
                    Expression::Identifier(method_name.clone(), span.clone())
                };

                let args = args
                    .iter()
                    .enumerate()
                    .map(|(idx, arg)| {
                        self.with_segment(format!("arg[{idx}]"), |ctx| ctx.convert_expression(arg))
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .map(Argument::Positional)
                    .collect();

                self.record_success();

                Expression::Call {
                    function: Box::new(function),
                    args,
                    type_arguments: Vec::new(),
                    argument_metadata: CallArgumentMetadata::with_style(*argument_style),
                    span: span.clone(),
                }
            }
            IrExpression::FieldAccess {
                receiver,
                field_name,
                span,
                ..
            } => {
                let object =
                    self.with_segment("receiver", |ctx| ctx.convert_expression(receiver))?;
                self.record_success();
                Expression::MemberAccess {
                    object: Box::new(object),
                    property: field_name.clone(),
                    span: span.clone(),
                }
            }
            IrExpression::ArrayAccess {
                array, index, span, ..
            } => {
                let object = self.with_segment("array", |ctx| ctx.convert_expression(array))?;
                let index = self.with_segment("index", |ctx| ctx.convert_expression(index))?;
                self.record_success();
                Expression::IndexAccess {
                    object: Box::new(object),
                    index: Box::new(index),
                    span: span.clone(),
                }
            }
            IrExpression::Conditional {
                condition,
                then_expr,
                else_expr,
                span,
                ..
            } => {
                let condition =
                    self.with_segment("condition", |ctx| ctx.convert_expression(condition))?;
                let then_branch =
                    self.with_segment("then", |ctx| ctx.convert_expression(then_expr))?;
                let else_branch =
                    self.with_segment("else", |ctx| ctx.convert_expression(else_expr))?;
                self.record_success();
                Expression::If {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Some(Box::new(else_branch)),
                    span: span.clone(),
                }
            }
            IrExpression::Block {
                statements, span, ..
            } => {
                let statements = statements
                    .iter()
                    .enumerate()
                    .map(|(idx, stmt)| {
                        self.with_segment(format!("block[{idx}]"), |ctx| {
                            ctx.convert_statement(stmt)
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                self.record_success();
                Expression::Block {
                    statements,
                    label: None,
                    span: span.clone(),
                }
            }
            IrExpression::ArrayCreation {
                initializer: Some(initializer),
                delimiter,
                span,
                ..
            } => {
                let elements = initializer
                    .iter()
                    .enumerate()
                    .map(|(idx, expr)| {
                        self.with_segment(format!("element[{idx}]"), |ctx| {
                            ctx.convert_expression(expr)
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                self.record_success();
                Expression::Array {
                    elements,
                    delimiter: *delimiter,
                    span: span.clone(),
                }
            }
            IrExpression::StringFormat {
                format_string,
                args,
                span,
            } => {
                if args.is_empty() {
                    self.record_success();
                    Expression::Literal(Literal::String(format_string.clone()), span.clone())
                } else {
                    let mut parts = Vec::new();
                    let mut remaining = format_string.as_str();
                    for (idx, arg) in args.iter().enumerate() {
                        if let Some(position) = remaining.find("{}") {
                            let (prefix, rest) = remaining.split_at(position);
                            if !prefix.is_empty() {
                                parts.push(StringPart::Text(prefix.to_string()));
                            }
                            remaining = &rest[2..];
                        }
                        let expr = self.with_segment(format!("interp[{idx}]"), |ctx| {
                            ctx.convert_expression(arg)
                        })?;
                        parts.push(StringPart::Expression(expr));
                    }
                    if !remaining.is_empty() {
                        parts.push(StringPart::Text(remaining.to_string()));
                    }
                    self.record_success();
                    Expression::StringInterpolation {
                        parts,
                        span: span.clone(),
                    }
                }
            }
            IrExpression::This { span, .. } => {
                self.record_success();
                Expression::This(span.clone())
            }
            IrExpression::Super { span, .. } => {
                self.record_success();
                Expression::Super(span.clone())
            }
            IrExpression::SequencePipeline { span, .. } => {
                return self.placeholder_expression(
                    &span,
                    WarningKind::UnsupportedNode,
                    "Sequence pipeline expression reconstruction is not implemented",
                );
            }
            other => {
                let span = extract_expr_span(other);
                return self.placeholder_expression(
                    &span,
                    WarningKind::UnsupportedNode,
                    "未対応の IR 式をプレースホルダーに置き換えました",
                );
            }
        };

        Ok(expression)
    }

    fn convert_parameter(
        &mut self,
        param: &IrParameter,
    ) -> Result<jv_ast::Parameter, ReconstructionError> {
        self.visit_node();

        let type_annotation = Some(self.convert_java_type(&param.java_type));

        self.record_success();

        Ok(jv_ast::Parameter {
            name: param.name.clone(),
            type_annotation,
            default_value: None,
            modifiers: jv_ast::ParameterModifiers::default(),
            span: param.span.clone(),
        })
    }

    fn convert_java_type(&mut self, java_type: &JavaType) -> TypeAnnotation {
        match java_type {
            JavaType::Primitive(name) | JavaType::Reference { name, .. } => {
                TypeAnnotation::Simple(name.clone())
            }
            JavaType::Array { element_type, .. } => {
                TypeAnnotation::Array(Box::new(self.convert_java_type(element_type)))
            }
            JavaType::Functional {
                param_types,
                return_type,
                ..
            } => TypeAnnotation::Function {
                params: param_types
                    .iter()
                    .map(|ty| self.convert_java_type(ty))
                    .collect(),
                return_type: Box::new(self.convert_java_type(return_type)),
            },
            JavaType::Wildcard { kind, bound } => {
                let text = match kind {
                    JavaWildcardKind::Unbounded => "?".to_string(),
                    JavaWildcardKind::Extends => format!(
                        "? extends {}",
                        bound
                            .as_ref()
                            .map(|inner| self.convert_java_type(inner))
                            .map(render_type_annotation)
                            .unwrap_or_else(|| "Object".to_string())
                    ),
                    JavaWildcardKind::Super => format!(
                        "? super {}",
                        bound
                            .as_ref()
                            .map(|inner| self.convert_java_type(inner))
                            .map(render_type_annotation)
                            .unwrap_or_else(|| "Object".to_string())
                    ),
                };
                TypeAnnotation::Simple(text)
            }
            JavaType::Void => TypeAnnotation::Simple("Void".into()),
        }
    }

    fn convert_modifiers(&mut self, modifiers: &IrModifiers) -> Modifiers {
        let visibility = match modifiers.visibility {
            IrVisibility::Public => Visibility::Public,
            IrVisibility::Protected => Visibility::Protected,
            IrVisibility::Private => Visibility::Private,
            IrVisibility::Package => Visibility::Internal,
        };

        Modifiers {
            visibility,
            is_abstract: modifiers.is_abstract,
            is_final: modifiers.is_final,
            is_static: modifiers.is_static,
            is_override: false,
            is_open: !modifiers.is_final && !modifiers.is_abstract,
            annotations: Vec::new(),
        }
    }

    fn insert_placeholder_statement(
        &mut self,
        span: Option<Span>,
        kind: WarningKind,
        message: impl Into<String>,
    ) -> Result<Statement, ReconstructionError> {
        let message = message.into();
        let span_clone = span.clone();
        if !self.opts.allow_placeholders {
            self.emit_warning(kind.clone(), span_clone, &message);
            return match kind {
                WarningKind::UnsupportedNode => {
                    Err(ReconstructionError::UnsupportedFormat(message.clone()))
                }
                _ => Err(ReconstructionError::InsufficientMetadata),
            };
        }
        self.record_placeholder();
        self.emit_warning(kind, span_clone.clone(), &message);
        let span = span_clone.unwrap_or_else(Span::dummy);
        Ok(Statement::Expression {
            expr: Expression::Literal(Literal::Null, span.clone()),
            span,
        })
    }

    fn placeholder_expression(
        &mut self,
        span: &Span,
        kind: WarningKind,
        message: impl Into<String>,
    ) -> Result<Expression, ReconstructionError> {
        let message = message.into();
        if !self.opts.allow_placeholders {
            self.emit_warning(kind.clone(), Some(span.clone()), &message);
            return match kind {
                WarningKind::UnsupportedNode => {
                    Err(ReconstructionError::UnsupportedFormat(message))
                }
                _ => Err(ReconstructionError::InsufficientMetadata),
            };
        }
        self.record_placeholder();
        self.emit_warning(kind, Some(span.clone()), &message);
        Ok(Expression::Literal(Literal::Null, span.clone()))
    }
}

fn extract_span(stmt: &IrStatement) -> Option<Span> {
    match stmt {
        IrStatement::VariableDeclaration { span, .. }
        | IrStatement::MethodDeclaration { span, .. }
        | IrStatement::ClassDeclaration { span, .. }
        | IrStatement::InterfaceDeclaration { span, .. }
        | IrStatement::RecordDeclaration { span, .. }
        | IrStatement::FieldDeclaration { span, .. }
        | IrStatement::Expression { span, .. }
        | IrStatement::Return { span, .. }
        | IrStatement::If { span, .. }
        | IrStatement::While { span, .. }
        | IrStatement::ForEach { span, .. }
        | IrStatement::For { span, .. }
        | IrStatement::Switch { span, .. }
        | IrStatement::Try { span, .. }
        | IrStatement::TryWithResources { span, .. }
        | IrStatement::Throw { span, .. }
        | IrStatement::Break { span, .. }
        | IrStatement::Continue { span, .. }
        | IrStatement::Block { span, .. }
        | IrStatement::Package { span, .. }
        | IrStatement::Comment { span, .. } => Some(span.clone()),
        IrStatement::Import(import) => Some(import.span.clone()),
        IrStatement::Commented { statement, .. } => extract_span(statement),
        IrStatement::SampleDeclaration(decl) => Some(decl.span.clone()),
    }
}

fn extract_expr_span(expr: &IrExpression) -> Span {
    match expr {
        IrExpression::Literal(_, span)
        | IrExpression::Identifier { span, .. }
        | IrExpression::MethodCall { span, .. }
        | IrExpression::FieldAccess { span, .. }
        | IrExpression::ArrayAccess { span, .. }
        | IrExpression::Binary { span, .. }
        | IrExpression::Unary { span, .. }
        | IrExpression::Assignment { span, .. }
        | IrExpression::Conditional { span, .. }
        | IrExpression::Block { span, .. }
        | IrExpression::ArrayCreation { span, .. }
        | IrExpression::ObjectCreation { span, .. }
        | IrExpression::Lambda { span, .. }
        | IrExpression::Switch { span, .. }
        | IrExpression::Cast { span, .. }
        | IrExpression::InstanceOf { span, .. }
        | IrExpression::This { span, .. }
        | IrExpression::Super { span, .. }
        | IrExpression::NullSafeOperation { span, .. }
        | IrExpression::StringFormat { span, .. }
        | IrExpression::CompletableFuture { span, .. }
        | IrExpression::VirtualThread { span, .. }
        | IrExpression::TryWithResources { span, .. }
        | IrExpression::RegexPattern { span, .. }
        | IrExpression::SequencePipeline { span, .. } => span.clone(),
    }
}
