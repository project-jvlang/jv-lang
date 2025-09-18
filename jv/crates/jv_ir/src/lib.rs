// jv_ir - Intermediate representation for desugaring jv language constructs
use jv_ast::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Java-compatible type representation after desugaring
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum JavaType {
    /// Primitive types: int, boolean, char, etc.
    Primitive(String),
    /// Reference types: String, Object, custom classes
    Reference {
        name: String,
        generic_args: Vec<JavaType>,
    },
    /// Array types: int[], String[][]
    Array {
        element_type: Box<JavaType>,
        dimensions: usize,
    },
    /// Function types represented as functional interfaces
    Functional {
        interface_name: String,
        param_types: Vec<JavaType>,
        return_type: Box<JavaType>,
    },
    /// Void type
    Void,
}

/// Desugared expressions - all jv sugar removed
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrExpression {
    // Basic expressions that map directly to Java
    Literal(Literal, Span),
    Identifier {
        name: String,
        java_type: JavaType,
        span: Span,
    },

    // Method calls (function calls become method calls or static calls)
    MethodCall {
        receiver: Option<Box<IrExpression>>, // None for static calls
        method_name: String,
        args: Vec<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Field access
    FieldAccess {
        receiver: Box<IrExpression>,
        field_name: String,
        java_type: JavaType,
        span: Span,
    },

    // Array access
    ArrayAccess {
        array: Box<IrExpression>,
        index: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Binary operations
    Binary {
        left: Box<IrExpression>,
        op: BinaryOp,
        right: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Unary operations
    Unary {
        op: UnaryOp,
        operand: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Assignment
    Assignment {
        target: Box<IrExpression>,
        value: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Conditional expression (ternary)
    Conditional {
        condition: Box<IrExpression>,
        then_expr: Box<IrExpression>,
        else_expr: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Block expression
    Block {
        statements: Vec<IrStatement>,
        java_type: JavaType,
        span: Span,
    },

    // Array creation
    ArrayCreation {
        element_type: JavaType,
        dimensions: Vec<Option<IrExpression>>, // None for unsized dimensions
        initializer: Option<Vec<IrExpression>>,
        span: Span,
    },

    // Object creation (new Constructor(args))
    ObjectCreation {
        class_name: String,
        generic_args: Vec<JavaType>,
        args: Vec<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Lambda expression (desugared to anonymous class or method reference)
    Lambda {
        functional_interface: String,
        param_names: Vec<String>,
        param_types: Vec<JavaType>,
        body: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Switch expression (Java 14+ switch expressions)
    Switch {
        discriminant: Box<IrExpression>,
        cases: Vec<IrSwitchCase>,
        java_type: JavaType,
        span: Span,
    },

    // Cast expression
    Cast {
        expr: Box<IrExpression>,
        target_type: JavaType,
        span: Span,
    },

    // instanceof check
    InstanceOf {
        expr: Box<IrExpression>,
        target_type: JavaType,
        span: Span,
    },

    // This reference
    This {
        java_type: JavaType,
        span: Span,
    },

    // Super reference
    Super {
        java_type: JavaType,
        span: Span,
    },

    // Null-safe operations become explicit null checks
    NullSafeOperation {
        expr: Box<IrExpression>,
        operation: Box<IrExpression>, // The operation to perform if not null
        default_value: Option<Box<IrExpression>>, // Default if null
        java_type: JavaType,
        span: Span,
    },

    // String formatting (from string interpolation)
    StringFormat {
        format_string: String,
        args: Vec<IrExpression>,
        span: Span,
    },

    // CompletableFuture operations (from async/await)
    CompletableFuture {
        operation: CompletableFutureOp,
        args: Vec<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Virtual thread operations (from spawn)
    VirtualThread {
        operation: VirtualThreadOp,
        args: Vec<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Try-with-resources (from use blocks)
    TryWithResources {
        resources: Vec<IrResource>,
        body: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },
}

/// Switch cases for desugared when expressions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrSwitchCase {
    /// Patterns become case labels or guards
    pub labels: Vec<IrCaseLabel>,
    /// Guard condition (for pattern guards)
    pub guard: Option<IrExpression>,
    /// Case body
    pub body: IrExpression,
    pub span: Span,
}

/// Case labels in switch expressions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrCaseLabel {
    /// Literal case: case 42:
    Literal(Literal),
    /// Type pattern: case String s:
    TypePattern { type_name: String, variable: String },
    /// Range becomes multiple cases or guard
    Range { start: Literal, end: Literal },
    /// Default case
    Default,
}

/// CompletableFuture operations
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CompletableFutureOp {
    /// CompletableFuture.supplyAsync(supplier)
    SupplyAsync,
    /// future.thenApply(function)
    ThenApply,
    /// future.thenCompose(function)
    ThenCompose,
    /// future.get() or future.join()
    Get,
    /// CompletableFuture.completedFuture(value)
    CompletedFuture,
}

/// Virtual thread operations
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VirtualThreadOp {
    /// Thread.ofVirtual().start(runnable)
    Start,
    /// Thread.ofVirtual().factory()
    Factory,
}

/// Resource for try-with-resources
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrResource {
    pub name: String,
    pub initializer: IrExpression,
    pub java_type: JavaType,
    pub span: Span,
}

/// Desugared statements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrStatement {
    // Variable declarations with explicit types
    VariableDeclaration {
        name: String,
        java_type: JavaType,
        initializer: Option<IrExpression>,
        is_final: bool,
        modifiers: IrModifiers,
        span: Span,
    },

    // Method declarations
    MethodDeclaration {
        name: String,
        parameters: Vec<IrParameter>,
        return_type: JavaType,
        body: Option<IrExpression>, // None for abstract methods
        modifiers: IrModifiers,
        throws: Vec<String>, // Exception types
        span: Span,
    },

    // Class declarations
    ClassDeclaration {
        name: String,
        type_parameters: Vec<IrTypeParameter>,
        superclass: Option<JavaType>,
        interfaces: Vec<JavaType>,
        fields: Vec<IrStatement>,         // FieldDeclaration statements
        methods: Vec<IrStatement>,        // MethodDeclaration statements
        nested_classes: Vec<IrStatement>, // ClassDeclaration statements
        modifiers: IrModifiers,
        span: Span,
    },

    // Interface declarations
    InterfaceDeclaration {
        name: String,
        type_parameters: Vec<IrTypeParameter>,
        superinterfaces: Vec<JavaType>,
        methods: Vec<IrStatement>, // MethodDeclaration statements (abstract)
        default_methods: Vec<IrStatement>, // MethodDeclaration statements (default)
        fields: Vec<IrStatement>,  // Field declarations (public static final)
        nested_types: Vec<IrStatement>,
        modifiers: IrModifiers,
        span: Span,
    },

    // Record declarations (from data classes)
    RecordDeclaration {
        name: String,
        type_parameters: Vec<IrTypeParameter>,
        components: Vec<IrRecordComponent>,
        interfaces: Vec<JavaType>,
        methods: Vec<IrStatement>, // Additional methods
        modifiers: IrModifiers,
        span: Span,
    },

    // Field declarations
    FieldDeclaration {
        name: String,
        java_type: JavaType,
        initializer: Option<IrExpression>,
        modifiers: IrModifiers,
        span: Span,
    },

    // Expression statements
    Expression {
        expr: IrExpression,
        span: Span,
    },

    // Return statements
    Return {
        value: Option<IrExpression>,
        span: Span,
    },

    // If statements
    If {
        condition: IrExpression,
        then_stmt: Box<IrStatement>,
        else_stmt: Option<Box<IrStatement>>,
        span: Span,
    },

    // While loops
    While {
        condition: IrExpression,
        body: Box<IrStatement>,
        span: Span,
    },

    // For loops (enhanced for)
    ForEach {
        variable: String,
        variable_type: JavaType,
        iterable: IrExpression,
        body: Box<IrStatement>,
        span: Span,
    },

    // Traditional for loops
    For {
        init: Option<Box<IrStatement>>,
        condition: Option<IrExpression>,
        update: Option<IrExpression>,
        body: Box<IrStatement>,
        span: Span,
    },

    // Switch statements
    Switch {
        discriminant: IrExpression,
        cases: Vec<IrSwitchCase>,
        span: Span,
    },

    // Try-catch-finally
    Try {
        body: Box<IrStatement>,
        catch_clauses: Vec<IrCatchClause>,
        finally_block: Option<Box<IrStatement>>,
        span: Span,
    },

    // Try-with-resources
    TryWithResources {
        resources: Vec<IrResource>,
        body: Box<IrStatement>,
        catch_clauses: Vec<IrCatchClause>,
        finally_block: Option<Box<IrStatement>>,
        span: Span,
    },

    // Throw statements
    Throw {
        expr: IrExpression,
        span: Span,
    },

    // Break/continue
    Break {
        label: Option<String>,
        span: Span,
    },
    Continue {
        label: Option<String>,
        span: Span,
    },

    // Block statements
    Block {
        statements: Vec<IrStatement>,
        span: Span,
    },

    // Import statements
    Import {
        path: String,
        is_static: bool,
        is_wildcard: bool,
        span: Span,
    },

    // Package declaration
    Package {
        name: String,
        span: Span,
    },
}

/// Method/constructor parameters
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrParameter {
    pub name: String,
    pub java_type: JavaType,
    pub modifiers: IrModifiers,
    pub span: Span,
}

/// Record components (for data classes -> records)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrRecordComponent {
    pub name: String,
    pub java_type: JavaType,
    pub span: Span,
}

/// Type parameters for generics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrTypeParameter {
    pub name: String,
    pub bounds: Vec<JavaType>,
    pub span: Span,
}

/// Catch clause for exception handling
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrCatchClause {
    pub exception_type: JavaType,
    pub variable_name: String,
    pub body: IrStatement,
    pub span: Span,
}

/// Java modifiers (public, private, static, etc.)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct IrModifiers {
    pub visibility: IrVisibility,
    pub is_static: bool,
    pub is_final: bool,
    pub is_abstract: bool,
    pub is_synchronized: bool,
    pub is_native: bool,
    pub is_strictfp: bool,
    pub annotations: Vec<String>, // Annotation names
}

/// Java visibility modifiers
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub enum IrVisibility {
    Public,
    Protected,
    #[default]
    Package, // Default (no modifier)
    Private,
}

/// Complete desugared program
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrProgram {
    pub package: Option<String>,
    pub imports: Vec<IrStatement>,
    pub type_declarations: Vec<IrStatement>, // Classes, interfaces, records
    pub span: Span,
}

/// Method overload generated from default/named parameters
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MethodOverload {
    pub name: String,
    pub parameters: Vec<IrParameter>,
    pub return_type: JavaType,
    pub body: IrExpression,
    pub modifiers: IrModifiers,
    pub span: Span,
}

/// Utility class generated from top-level functions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UtilityClass {
    pub name: String,
    pub methods: Vec<IrStatement>, // MethodDeclaration statements
    pub modifiers: IrModifiers,
    pub span: Span,
}

/// Static method call generated from extension functions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StaticMethodCall {
    pub class_name: String,
    pub method_name: String,
    pub args: Vec<IrExpression>, // First arg is the receiver
    pub java_type: JavaType,
    pub span: Span,
}

/// Transformation context for desugaring
#[derive(Debug, Clone)]
pub struct TransformContext {
    /// Type information gathered from analysis
    pub type_info: HashMap<String, JavaType>,
    /// Current scope for variable resolution
    pub scope_stack: Vec<HashMap<String, JavaType>>,
    /// Generated utility classes
    pub utility_classes: Vec<UtilityClass>,
    /// Generated method overloads
    pub method_overloads: Vec<MethodOverload>,
    /// Extension function mappings
    pub extension_methods: HashMap<String, StaticMethodCall>,
    /// Current package
    pub current_package: Option<String>,
}

impl TransformContext {
    pub fn new() -> Self {
        Self {
            type_info: HashMap::new(),
            scope_stack: vec![HashMap::new()],
            utility_classes: Vec::new(),
            method_overloads: Vec::new(),
            extension_methods: HashMap::new(),
            current_package: None,
        }
    }

    pub fn enter_scope(&mut self) {
        self.scope_stack.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn add_variable(&mut self, name: String, java_type: JavaType) {
        if let Some(current_scope) = self.scope_stack.last_mut() {
            current_scope.insert(name, java_type);
        }
    }

    pub fn lookup_variable(&self, name: &str) -> Option<&JavaType> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(java_type) = scope.get(name) {
                return Some(java_type);
            }
        }
        self.type_info.get(name)
    }
}

// Main transformation function
pub fn transform_program(program: Program) -> Result<IrProgram, TransformError> {
    let mut context = TransformContext::new();
    transform_program_with_context(program, &mut context)
}

pub fn transform_program_with_context(
    program: Program,
    context: &mut TransformContext,
) -> Result<IrProgram, TransformError> {
    let mut ir_statements = Vec::new();

    for stmt in program.statements {
        let mut transformed = transform_statement(stmt, context)?;
        ir_statements.append(&mut transformed);
    }

    Ok(IrProgram {
        package: program.package,
        imports: Vec::new(), // TODO: handle imports
        type_declarations: ir_statements,
        span: program.span,
    })
}

// Transformation functions for different AST nodes (all will panic in Red phase)
pub fn transform_statement(
    stmt: Statement,
    context: &mut TransformContext,
) -> Result<Vec<IrStatement>, TransformError> {
    match stmt {
        Statement::ValDeclaration {
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
        } => Ok(vec![desugar_val_declaration(
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
            context,
        )?]),
        Statement::VarDeclaration {
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
        } => Ok(vec![desugar_var_declaration(
            name,
            type_annotation,
            initializer,
            modifiers,
            span,
            context,
        )?]),
        Statement::Expression { expr, span } => {
            let ir_expr = transform_expression(expr, context)?;
            Ok(vec![IrStatement::Expression {
                expr: ir_expr,
                span,
            }])
        }
        _ => {
            // For now, return empty for unimplemented statements
            Ok(vec![])
        }
    }
}

pub fn transform_expression(
    expr: Expression,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    match expr {
        Expression::Literal(lit, span) => {
            let _java_type = match &lit {
                Literal::String(_) => JavaType::Reference {
                    name: "String".to_string(),
                    generic_args: vec![],
                },
                Literal::Number(_) => JavaType::Primitive("int".to_string()),
                Literal::Boolean(_) => JavaType::Primitive("boolean".to_string()),
                Literal::Character(_) => JavaType::Primitive("char".to_string()),
                Literal::Null => JavaType::Reference {
                    name: "Object".to_string(),
                    generic_args: vec![],
                },
            };
            Ok(IrExpression::Literal(lit, span))
        }
        Expression::Identifier(name, span) => {
            if let Some(java_type) = context.lookup_variable(&name).cloned() {
                Ok(IrExpression::Identifier {
                    name,
                    java_type,
                    span,
                })
            } else {
                Err(TransformError::ScopeError {
                    message: format!("Unknown identifier '{name}'"),
                    span,
                })
            }
        }
        Expression::NullSafeMemberAccess {
            object,
            property,
            span,
        } => desugar_null_safe_member_access(object, property, span, context),
        Expression::NullSafeIndexAccess {
            object,
            index,
            span,
        } => desugar_null_safe_index_access(object, index, span, context),
        Expression::Binary {
            left,
            op,
            right,
            span,
        } => {
            if matches!(op, BinaryOp::Elvis) {
                desugar_elvis_operator(left, right, span, context)
            } else {
                let left_ir = transform_expression(*left, context)?;
                let right_ir = transform_expression(*right, context)?;
                let java_type = JavaType::Primitive("int".to_string()); // TODO: proper type inference
                Ok(IrExpression::Binary {
                    left: Box::new(left_ir),
                    op,
                    right: Box::new(right_ir),
                    java_type,
                    span,
                })
            }
        }
        _ => {
            // For now, create a placeholder for unimplemented expressions
            Ok(IrExpression::Literal(Literal::Null, Span::default()))
        }
    }
}

// Desugaring functions for specific constructs
fn convert_modifiers(modifiers: &Modifiers) -> IrModifiers {
    let mut ir_modifiers = IrModifiers::default();

    ir_modifiers.visibility = match modifiers.visibility {
        Visibility::Public => IrVisibility::Public,
        Visibility::Protected => IrVisibility::Protected,
        Visibility::Private => IrVisibility::Private,
        Visibility::Internal => IrVisibility::Package,
    };

    ir_modifiers.is_static = modifiers.is_static;
    ir_modifiers.is_final = modifiers.is_final;
    ir_modifiers.is_abstract = modifiers.is_abstract;

    if modifiers.is_override {
        ir_modifiers.annotations.push("Override".to_string());
    }

    ir_modifiers
}

pub fn desugar_val_declaration(
    name: String,
    type_annotation: Option<TypeAnnotation>,
    initializer: Expression,
    modifiers: Modifiers,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    let ir_initializer = transform_expression(initializer, context)?;
    let java_type = infer_java_type(type_annotation, Some(&ir_initializer), context)?;

    let mut ir_modifiers = convert_modifiers(&modifiers);
    ir_modifiers.is_final = true;

    context.add_variable(name.clone(), java_type.clone());

    Ok(IrStatement::VariableDeclaration {
        name,
        java_type,
        initializer: Some(ir_initializer),
        is_final: true,
        modifiers: ir_modifiers,
        span,
    })
}

pub fn desugar_var_declaration(
    name: String,
    mut type_annotation: Option<TypeAnnotation>,
    initializer: Option<Expression>,
    modifiers: Modifiers,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    if type_annotation.is_none() && initializer.is_none() {
        return Err(TransformError::TypeInferenceError {
            message:
                "Cannot infer type for 'var' declaration without type annotation or initializer"
                    .to_string(),
            span,
        });
    }

    let ir_initializer = match initializer {
        Some(expr) => Some(transform_expression(expr, context)?),
        None => None,
    };

    let java_type = infer_java_type(type_annotation.take(), ir_initializer.as_ref(), context)?;

    let mut ir_modifiers = convert_modifiers(&modifiers);
    ir_modifiers.is_final = modifiers.is_final;

    context.add_variable(name.clone(), java_type.clone());

    Ok(IrStatement::VariableDeclaration {
        name,
        java_type,
        initializer: ir_initializer,
        is_final: false,
        modifiers: ir_modifiers,
        span,
    })
}

pub fn desugar_when_expression(
    expr: Option<Box<Expression>>,
    arms: Vec<WhenArm>,
    else_arm: Option<Box<Expression>>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let subject_expr = expr.ok_or_else(|| TransformError::UnsupportedConstruct {
        construct: "when expressions without subject".to_string(),
        span: span.clone(),
    })?;

    let discriminant = transform_expression(*subject_expr, context)?;

    let mut cases = Vec::new();
    let mut result_type: Option<JavaType> = None;
    let mut has_default_case = false;

    for arm in arms {
        let (labels, is_default) = convert_when_pattern(&arm.pattern, arm.span.clone())?;

        if is_default {
            if has_default_case {
                return Err(TransformError::UnsupportedConstruct {
                    construct: "Multiple default patterns in when expression".to_string(),
                    span: arm.span,
                });
            }
            has_default_case = true;
        }

        let body = transform_expression(arm.body, context)?;
        if result_type.is_none() {
            result_type = extract_java_type(&body);
        }

        cases.push(IrSwitchCase {
            labels,
            guard: None,
            body,
            span: arm.span,
        });
    }

    if let Some(else_expr) = else_arm {
        if has_default_case {
            return Err(TransformError::UnsupportedConstruct {
                construct: "when expression cannot have both default pattern and else arm"
                    .to_string(),
                span,
            });
        }

        let body = transform_expression(*else_expr, context)?;
        if result_type.is_none() {
            result_type = extract_java_type(&body);
        }

        cases.push(IrSwitchCase {
            labels: vec![IrCaseLabel::Default],
            guard: None,
            body,
            span: span.clone(),
        });
        has_default_case = true;
    }

    if cases.is_empty() {
        return Err(TransformError::UnsupportedConstruct {
            construct: "when expression must have at least one arm".to_string(),
            span,
        });
    }

    if !has_default_case {
        cases.push(IrSwitchCase {
            labels: vec![IrCaseLabel::Default],
            guard: None,
            body: IrExpression::Literal(Literal::Null, span.clone()),
            span: span.clone(),
        });
        if result_type.is_none() {
            result_type = Some(JavaType::object());
        }
    }

    let java_type = result_type.unwrap_or_else(JavaType::object);

    Ok(IrExpression::Switch {
        discriminant: Box::new(discriminant),
        cases,
        java_type,
        span,
    })
}

pub fn desugar_extension_function(
    receiver_type: TypeAnnotation,
    function: Box<Statement>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    panic!("not yet implemented: desugar_extension_function")
}

pub fn desugar_string_interpolation(
    parts: Vec<StringPart>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    panic!("not yet implemented: desugar_string_interpolation")
}

pub fn desugar_spawn_expression(
    body: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    panic!("not yet implemented: desugar_spawn_expression")
}

pub fn desugar_async_expression(
    body: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    panic!("not yet implemented: desugar_async_expression")
}

pub fn desugar_await_expression(
    expr: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    panic!("not yet implemented: desugar_await_expression")
}

pub fn desugar_use_expression(
    resource: Box<Expression>,
    body: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    panic!("not yet implemented: desugar_use_expression")
}

pub fn desugar_defer_expression(
    body: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    panic!("not yet implemented: desugar_defer_expression")
}

pub fn desugar_default_parameters(
    function_name: String,
    parameters: Vec<Parameter>,
    return_type: Option<TypeAnnotation>,
    body: Box<Expression>,
    modifiers: Modifiers,
    span: Span,
    context: &mut TransformContext,
) -> Result<Vec<MethodOverload>, TransformError> {
    panic!("not yet implemented: desugar_default_parameters")
}

pub fn desugar_named_arguments(
    function: Box<Expression>,
    args: Vec<Argument>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    panic!("not yet implemented: desugar_named_arguments")
}

pub fn desugar_top_level_function(
    function: Statement,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    panic!("not yet implemented: desugar_top_level_function")
}

pub fn desugar_data_class(
    name: String,
    parameters: Vec<Parameter>,
    type_parameters: Vec<String>,
    is_mutable: bool,
    modifiers: Modifiers,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    panic!("not yet implemented: desugar_data_class")
}

pub fn desugar_null_safe_member_access(
    object: Box<Expression>,
    property: String,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let ir_object = transform_expression(*object, context)?;
    let object_type = extract_java_type(&ir_object).unwrap_or_else(JavaType::object);

    if matches!(object_type, JavaType::Primitive(_)) {
        return Err(TransformError::NullSafetyError {
            message: "Cannot apply null-safe access to primitive types".to_string(),
            span,
        });
    }

    let result_type = JavaType::object();
    let operation = IrExpression::FieldAccess {
        receiver: Box::new(ir_object.clone()),
        field_name: property,
        java_type: result_type.clone(),
        span: span.clone(),
    };

    let default_value = Some(Box::new(IrExpression::Literal(Literal::Null, span.clone())));

    Ok(IrExpression::NullSafeOperation {
        expr: Box::new(ir_object),
        operation: Box::new(operation),
        default_value,
        java_type: result_type,
        span,
    })
}

pub fn desugar_null_safe_index_access(
    object: Box<Expression>,
    index: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let ir_object = transform_expression(*object, context)?;
    let object_type = extract_java_type(&ir_object).unwrap_or_else(JavaType::object);

    if matches!(object_type, JavaType::Primitive(_)) {
        return Err(TransformError::NullSafetyError {
            message: "Cannot apply null-safe index access to primitive types".to_string(),
            span,
        });
    }

    let ir_index = transform_expression(*index, context)?;

    let element_type = match &object_type {
        JavaType::Array { element_type, .. } => *element_type.clone(),
        _ => JavaType::object(),
    };

    let operation = IrExpression::ArrayAccess {
        array: Box::new(ir_object.clone()),
        index: Box::new(ir_index),
        java_type: element_type.clone(),
        span: span.clone(),
    };

    let default_value = if matches!(element_type, JavaType::Primitive(_)) {
        None
    } else {
        Some(Box::new(IrExpression::Literal(Literal::Null, span.clone())))
    };

    Ok(IrExpression::NullSafeOperation {
        expr: Box::new(ir_object),
        operation: Box::new(operation),
        default_value,
        java_type: element_type,
        span,
    })
}

pub fn desugar_elvis_operator(
    left: Box<Expression>,
    right: Box<Expression>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let left_expr = transform_expression(*left, context)?;
    let right_expr = transform_expression(*right, context)?;

    if let Some(JavaType::Primitive(_)) = extract_java_type(&left_expr) {
        return Err(TransformError::NullSafetyError {
            message: "Elvis operator requires nullable left-hand expression".to_string(),
            span,
        });
    }

    let left_type = extract_java_type(&left_expr);
    let right_type = extract_java_type(&right_expr);
    let result_type = left_type
        .or(right_type.clone())
        .unwrap_or_else(JavaType::object);

    Ok(IrExpression::NullSafeOperation {
        expr: Box::new(left_expr.clone()),
        operation: Box::new(left_expr),
        default_value: Some(Box::new(right_expr)),
        java_type: result_type,
        span,
    })
}

// Type inference and conversion
pub fn infer_java_type(
    type_annotation: Option<TypeAnnotation>,
    initializer: Option<&IrExpression>,
    _context: &TransformContext,
) -> Result<JavaType, TransformError> {
    if let Some(ta) = type_annotation {
        return convert_type_annotation(ta);
    }

    if let Some(expr) = initializer {
        if let Some(java_type) = extract_java_type(expr) {
            return Ok(java_type);
        }

        return Err(TransformError::TypeInferenceError {
            message: "Unable to infer Java type from initializer".to_string(),
            span: ir_expression_span(expr),
        });
    }

    Err(TransformError::TypeInferenceError {
        message: "Unable to infer Java type without type annotation or initializer".to_string(),
        span: Span::default(),
    })
}

pub fn convert_type_annotation(
    type_annotation: TypeAnnotation,
) -> Result<JavaType, TransformError> {
    match type_annotation {
        TypeAnnotation::Simple(name) => Ok(match name.as_str() {
            "Int" | "int" => JavaType::Primitive("int".to_string()),
            "String" => JavaType::Reference {
                name: "String".to_string(),
                generic_args: vec![],
            },
            "Boolean" | "boolean" => JavaType::Primitive("boolean".to_string()),
            "Double" | "double" => JavaType::Primitive("double".to_string()),
            "Float" | "float" => JavaType::Primitive("float".to_string()),
            "Long" | "long" => JavaType::Primitive("long".to_string()),
            "Char" | "char" => JavaType::Primitive("char".to_string()),
            "Byte" | "byte" => JavaType::Primitive("byte".to_string()),
            "Short" | "short" => JavaType::Primitive("short".to_string()),
            _ => JavaType::Reference {
                name,
                generic_args: vec![],
            },
        }),
        TypeAnnotation::Nullable(inner) => {
            // For now, treat nullable as the same as non-nullable since Java has nullable references by default
            convert_type_annotation(*inner)
        }
        TypeAnnotation::Array(element_type) => {
            let element_java_type = convert_type_annotation(*element_type)?;
            Ok(JavaType::Array {
                element_type: Box::new(element_java_type),
                dimensions: 1,
            })
        }
        _ => Ok(JavaType::Reference {
            name: "Object".to_string(),
            generic_args: vec![],
        }),
    }
}

// Utility functions
fn extract_java_type(expr: &IrExpression) -> Option<JavaType> {
    match expr {
        IrExpression::Literal(literal, _) => Some(literal_to_java_type(literal)),
        IrExpression::Identifier { java_type, .. }
        | IrExpression::MethodCall { java_type, .. }
        | IrExpression::FieldAccess { java_type, .. }
        | IrExpression::ArrayAccess { java_type, .. }
        | IrExpression::Binary { java_type, .. }
        | IrExpression::Unary { java_type, .. }
        | IrExpression::Assignment { java_type, .. }
        | IrExpression::Conditional { java_type, .. }
        | IrExpression::Block { java_type, .. }
        | IrExpression::ObjectCreation { java_type, .. }
        | IrExpression::Lambda { java_type, .. }
        | IrExpression::Switch { java_type, .. }
        | IrExpression::NullSafeOperation { java_type, .. }
        | IrExpression::CompletableFuture { java_type, .. }
        | IrExpression::VirtualThread { java_type, .. }
        | IrExpression::TryWithResources { java_type, .. }
        | IrExpression::This { java_type, .. }
        | IrExpression::Super { java_type, .. } => Some(java_type.clone()),
        IrExpression::Cast { target_type, .. } => Some(target_type.clone()),
        IrExpression::InstanceOf { .. } => Some(JavaType::boolean()),
        IrExpression::ArrayCreation {
            element_type,
            dimensions,
            ..
        } => Some(JavaType::Array {
            element_type: Box::new(element_type.clone()),
            dimensions: dimensions.len(),
        }),
        IrExpression::StringFormat { .. } => Some(JavaType::string()),
    }
}

fn literal_to_java_type(literal: &Literal) -> JavaType {
    match literal {
        Literal::String(_) => JavaType::string(),
        Literal::Number(_) => JavaType::Primitive("int".to_string()),
        Literal::Boolean(_) => JavaType::boolean(),
        Literal::Character(_) => JavaType::Primitive("char".to_string()),
        Literal::Null => JavaType::object(),
    }
}

fn ir_expression_span(expr: &IrExpression) -> Span {
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
        | IrExpression::TryWithResources { span, .. } => span.clone(),
    }
}

fn convert_when_pattern(
    pattern: &Pattern,
    span: Span,
) -> Result<(Vec<IrCaseLabel>, bool), TransformError> {
    match pattern {
        Pattern::Literal(literal, _) => Ok((vec![IrCaseLabel::Literal(literal.clone())], false)),
        Pattern::Wildcard(_) => Ok((vec![IrCaseLabel::Default], true)),
        _ => Err(TransformError::UnsupportedConstruct {
            construct: "Unsupported when pattern".to_string(),
            span,
        }),
    }
}

pub fn generate_utility_class_name(package: Option<&str>, source_file: &str) -> String {
    panic!("not yet implemented: generate_utility_class_name")
}

pub fn generate_extension_class_name(receiver_type: &TypeAnnotation) -> String {
    panic!("not yet implemented: generate_extension_class_name")
}

// Error types
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum TransformError {
    #[error("Type inference failed: {message}")]
    TypeInferenceError { message: String, span: Span },

    #[error("Unsupported construct: {construct}")]
    UnsupportedConstruct { construct: String, span: Span },

    #[error("Invalid pattern: {message}")]
    InvalidPattern { message: String, span: Span },

    #[error("Null safety violation: {message}")]
    NullSafetyError { message: String, span: Span },

    #[error("Scope resolution error: {message}")]
    ScopeError { message: String, span: Span },

    #[error("Invalid default parameter: {message}")]
    DefaultParameterError { message: String, span: Span },

    #[error("Extension function error: {message}")]
    ExtensionFunctionError { message: String, span: Span },

    #[error("Concurrency construct error: {message}")]
    ConcurrencyError { message: String, span: Span },

    #[error("Resource management error: {message}")]
    ResourceManagementError { message: String, span: Span },
}

// Helper implementations
impl Default for TransformContext {
    fn default() -> Self {
        Self::new()
    }
}

impl JavaType {
    pub fn int() -> Self {
        JavaType::Primitive("int".to_string())
    }

    pub fn boolean() -> Self {
        JavaType::Primitive("boolean".to_string())
    }

    pub fn string() -> Self {
        JavaType::Reference {
            name: "String".to_string(),
            generic_args: vec![],
        }
    }

    pub fn object() -> Self {
        JavaType::Reference {
            name: "Object".to_string(),
            generic_args: vec![],
        }
    }

    pub fn void() -> Self {
        JavaType::Void
    }

    pub fn is_nullable(&self) -> bool {
        match self {
            JavaType::Primitive(_) => false,
            _ => true,
        }
    }
}

// Include test module
#[cfg(test)]
mod tests;
