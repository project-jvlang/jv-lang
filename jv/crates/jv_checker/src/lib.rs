// jv_checker - Static analysis and validation
use jv_ast::*;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CheckError {
    #[error("Type error: {0}")]
    TypeError(String),
    #[error("Null safety violation: {0}")]
    NullSafetyError(String),
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),
    #[error("Invalid syntax: {0}")]
    SyntaxError(String),
    #[error("Validation error: {0}")]
    ValidationError(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum InferredType {
    String,
    Int,
    Double,
    Boolean,
    Null,
    Unknown,
    Optional(Box<InferredType>),
}

#[derive(Debug)]
pub struct CheckContext {
    variables: HashMap<String, InferredType>,
    scopes: Vec<HashMap<String, InferredType>>,
    errors: Vec<CheckError>,
    warnings: Vec<String>,
}

impl CheckContext {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            scopes: vec![HashMap::new()],
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare_variable(&mut self, name: String, var_type: InferredType) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, var_type);
        }
    }

    pub fn get_variable_type(&self, name: &str) -> Option<&InferredType> {
        // Check scopes from most recent to oldest
        for scope in self.scopes.iter().rev() {
            if let Some(var_type) = scope.get(name) {
                return Some(var_type);
            }
        }
        None
    }

    pub fn add_error(&mut self, error: CheckError) {
        self.errors.push(error);
    }

    pub fn add_warning(&mut self, warning: String) {
        self.warnings.push(warning);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn get_errors(&self) -> &[CheckError] {
        &self.errors
    }

    pub fn get_warnings(&self) -> &[String] {
        &self.warnings
    }
}

pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> Self {
        Self
    }

    /// Check a complete program for type safety and other validation rules
    pub fn check_program(&self, program: &Program) -> Result<(), Vec<CheckError>> {
        let mut context = CheckContext::new();

        // Check all statements
        for statement in &program.statements {
            self.check_statement(statement, &mut context);
        }

        if context.has_errors() {
            Err(context.errors)
        } else {
            Ok(())
        }
    }

    /// Check a single statement
    fn check_statement(&self, statement: &Statement, context: &mut CheckContext) {
        match statement {
            Statement::ValDeclaration {
                name, initializer, ..
            } => {
                let expr_type = self.infer_expression_type(initializer, context);
                context.declare_variable(name.clone(), expr_type);
            }
            Statement::VarDeclaration {
                name, initializer, ..
            } => {
                if let Some(init) = initializer {
                    let expr_type = self.infer_expression_type(init, context);
                    context.declare_variable(name.clone(), expr_type);
                } else {
                    context.declare_variable(name.clone(), InferredType::Unknown);
                }
            }
            Statement::FunctionDeclaration {
                name,
                parameters,
                body,
                ..
            } => {
                context.push_scope();

                // Add parameters to scope
                for param in parameters {
                    let param_type = self.type_annotation_to_inferred(&param.type_annotation);
                    context.declare_variable(param.name.clone(), param_type);
                }

                // Check function body
                self.check_expression(body, context);

                context.pop_scope();

                // Declare function in parent scope (simplified)
                context.declare_variable(name.clone(), InferredType::Unknown);
            }
            Statement::DataClassDeclaration { name, .. } => {
                // For data classes, we just declare the type exists
                context.declare_variable(name.clone(), InferredType::Unknown);
            }
            Statement::Expression { expr, .. } => {
                self.check_expression(expr, context);
            }
            _ => {
                // Other statement types - basic validation
            }
        }
    }

    /// Check an expression and return any errors found
    fn check_expression(&self, expression: &Expression, context: &mut CheckContext) {
        match expression {
            Expression::Identifier(name, _) => {
                if context.get_variable_type(name).is_none() {
                    context.add_error(CheckError::UndefinedVariable(name.clone()));
                }
            }
            Expression::Binary { left, right, .. } => {
                self.check_expression(left, context);
                self.check_expression(right, context);
                // Could add type compatibility checking here
            }
            Expression::Call { function, args, .. } => {
                self.check_expression(function, context);
                for arg in args {
                    match arg {
                        Argument::Positional(expr) => self.check_expression(expr, context),
                        Argument::Named { value, .. } => self.check_expression(value, context),
                    }
                }
            }
            Expression::MemberAccess { object, .. } => {
                self.check_expression(object, context);
            }
            Expression::NullSafeMemberAccess { object, .. } => {
                self.check_expression(object, context);
                // This is safe by design - no additional checks needed
            }
            _ => {
                // Other expression types
            }
        }
    }

    /// Infer the type of an expression
    fn infer_expression_type(
        &self,
        expression: &Expression,
        context: &CheckContext,
    ) -> InferredType {
        match expression {
            Expression::Literal(literal, _) => {
                match literal {
                    Literal::String(_) => InferredType::String,
                    Literal::Number(n) => {
                        if n.contains('.') {
                            InferredType::Double
                        } else {
                            InferredType::Int
                        }
                    }
                    Literal::Boolean(_) => InferredType::Boolean,
                    Literal::Character(_) => InferredType::String, // Treat char as string
                    Literal::Null => InferredType::Null,
                }
            }
            Expression::Identifier(name, _) => context
                .get_variable_type(name)
                .cloned()
                .unwrap_or(InferredType::Unknown),
            Expression::Binary {
                left, right, op, ..
            } => {
                let left_type = self.infer_expression_type(left, context);
                let right_type = self.infer_expression_type(right, context);

                match op {
                    BinaryOp::Add
                    | BinaryOp::Subtract
                    | BinaryOp::Multiply
                    | BinaryOp::Divide
                    | BinaryOp::Modulo => {
                        // Simplified: assume numeric operations return the same type
                        if matches!(left_type, InferredType::Double)
                            || matches!(right_type, InferredType::Double)
                        {
                            InferredType::Double
                        } else {
                            InferredType::Int
                        }
                    }
                    BinaryOp::Equal
                    | BinaryOp::NotEqual
                    | BinaryOp::Less
                    | BinaryOp::LessEqual
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEqual => InferredType::Boolean,
                    BinaryOp::And | BinaryOp::Or => InferredType::Boolean,
                    BinaryOp::Elvis => {
                        // Elvis operator returns the left type if non-null, right type otherwise
                        left_type
                    }
                    BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => InferredType::Int,
                    BinaryOp::PlusAssign
                    | BinaryOp::MinusAssign
                    | BinaryOp::MultiplyAssign
                    | BinaryOp::DivideAssign => {
                        // Assignment operators return the left operand type
                        left_type
                    }
                }
            }
            _ => InferredType::Unknown,
        }
    }

    /// Convert a type annotation to an inferred type
    fn type_annotation_to_inferred(
        &self,
        type_annotation: &Option<TypeAnnotation>,
    ) -> InferredType {
        match type_annotation {
            Some(TypeAnnotation::Simple(name)) => match name.as_str() {
                "String" => InferredType::String,
                "Int" | "Integer" => InferredType::Int,
                "Double" | "Float" => InferredType::Double,
                "Boolean" | "Bool" => InferredType::Boolean,
                _ => InferredType::Unknown,
            },
            Some(TypeAnnotation::Nullable(inner)) => {
                let inner_type = self.type_annotation_to_inferred(&Some(inner.as_ref().clone()));
                InferredType::Optional(Box::new(inner_type))
            }
            Some(TypeAnnotation::Generic { .. }) => InferredType::Unknown,
            Some(TypeAnnotation::Function { .. }) => InferredType::Unknown,
            Some(TypeAnnotation::Array(_)) => InferredType::Unknown,
            None => InferredType::Unknown,
        }
    }

    /// Validate null safety rules
    pub fn check_null_safety(&self, program: &Program) -> Vec<String> {
        let mut warnings = Vec::new();
        // Simplified null safety checking - would need more sophisticated analysis
        for statement in &program.statements {
            match statement {
                Statement::ValDeclaration { initializer, .. } => {
                    if matches!(initializer, Expression::Literal(Literal::Null, _)) {
                        warnings.push("Assigning null to val declaration".to_string());
                    }
                }
                _ => {}
            }
        }
        warnings
    }

    /// Check for forbidden Java syntax or patterns
    pub fn check_forbidden_syntax(&self, _program: &Program) -> Vec<String> {
        let violations = Vec::new();
        // This would check for patterns that shouldn't appear in jv code
        // For example: raw Java generics syntax, null checks without ?. operator, etc.
        violations
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_inference() {
        let checker = TypeChecker::new();
        let context = CheckContext::new();

        let string_literal =
            Expression::Literal(Literal::String("test".to_string()), Span::default());
        let inferred_type = checker.infer_expression_type(&string_literal, &context);

        assert_eq!(inferred_type, InferredType::String);
    }

    #[test]
    fn test_number_type_inference() {
        let checker = TypeChecker::new();
        let context = CheckContext::new();

        let int_literal = Expression::Literal(Literal::Number("42".to_string()), Span::default());
        let double_literal =
            Expression::Literal(Literal::Number("3.14".to_string()), Span::default());

        let int_type = checker.infer_expression_type(&int_literal, &context);
        let double_type = checker.infer_expression_type(&double_literal, &context);

        assert_eq!(int_type, InferredType::Int);
        assert_eq!(double_type, InferredType::Double);
    }

    #[test]
    fn test_context_variable_scope() {
        let mut context = CheckContext::new();

        context.declare_variable("x".to_string(), InferredType::String);
        assert_eq!(context.get_variable_type("x"), Some(&InferredType::String));

        context.push_scope();
        context.declare_variable("y".to_string(), InferredType::Int);
        assert_eq!(context.get_variable_type("y"), Some(&InferredType::Int));
        assert_eq!(context.get_variable_type("x"), Some(&InferredType::String)); // Should still be visible

        context.pop_scope();
        assert_eq!(context.get_variable_type("x"), Some(&InferredType::String));
        assert_eq!(context.get_variable_type("y"), None); // Should be out of scope
    }

    #[test]
    fn test_boolean_type_inference() {
        let checker = TypeChecker::new();
        let context = CheckContext::new();

        let true_literal = Expression::Literal(Literal::Boolean(true), Span::default());
        let false_literal = Expression::Literal(Literal::Boolean(false), Span::default());

        let true_type = checker.infer_expression_type(&true_literal, &context);
        let false_type = checker.infer_expression_type(&false_literal, &context);

        assert_eq!(true_type, InferredType::Boolean);
        assert_eq!(false_type, InferredType::Boolean);
    }

    #[test]
    fn test_null_type_inference() {
        let checker = TypeChecker::new();
        let context = CheckContext::new();

        let null_literal = Expression::Literal(Literal::Null, Span::default());
        let null_type = checker.infer_expression_type(&null_literal, &context);

        assert_eq!(null_type, InferredType::Null);
    }

    #[test]
    fn test_identifier_type_lookup() {
        let checker = TypeChecker::new();
        let mut context = CheckContext::new();

        context.declare_variable("x".to_string(), InferredType::String);
        context.declare_variable("y".to_string(), InferredType::Int);

        let x_identifier = Expression::Identifier("x".to_string(), Span::default());
        let y_identifier = Expression::Identifier("y".to_string(), Span::default());
        let unknown_identifier = Expression::Identifier("unknown".to_string(), Span::default());

        let x_type = checker.infer_expression_type(&x_identifier, &context);
        let y_type = checker.infer_expression_type(&y_identifier, &context);
        let unknown_type = checker.infer_expression_type(&unknown_identifier, &context);

        assert_eq!(x_type, InferredType::String);
        assert_eq!(y_type, InferredType::Int);
        assert_eq!(unknown_type, InferredType::Unknown);
    }

    #[test]
    fn test_binary_expression_type_inference() {
        let checker = TypeChecker::new();
        let context = CheckContext::new();

        let add_expr = Expression::Binary {
            left: Box::new(Expression::Literal(
                Literal::Number("1".to_string()),
                Span::default(),
            )),
            op: BinaryOp::Add,
            right: Box::new(Expression::Literal(
                Literal::Number("2".to_string()),
                Span::default(),
            )),
            span: Span::default(),
        };

        let result_type = checker.infer_expression_type(&add_expr, &context);
        assert_eq!(result_type, InferredType::Int);
    }

    #[test]
    fn test_type_annotation_conversion() {
        let checker = TypeChecker::new();

        let string_annotation = Some(TypeAnnotation::Simple("String".to_string()));
        let int_annotation = Some(TypeAnnotation::Simple("Int".to_string()));
        let bool_annotation = Some(TypeAnnotation::Simple("Boolean".to_string()));
        let double_annotation = Some(TypeAnnotation::Simple("Double".to_string()));
        let unknown_annotation = Some(TypeAnnotation::Simple("UnknownType".to_string()));
        let none_annotation = None;

        assert_eq!(
            checker.type_annotation_to_inferred(&string_annotation),
            InferredType::String
        );
        assert_eq!(
            checker.type_annotation_to_inferred(&int_annotation),
            InferredType::Int
        );
        assert_eq!(
            checker.type_annotation_to_inferred(&bool_annotation),
            InferredType::Boolean
        );
        assert_eq!(
            checker.type_annotation_to_inferred(&double_annotation),
            InferredType::Double
        );
        assert_eq!(
            checker.type_annotation_to_inferred(&unknown_annotation),
            InferredType::Unknown
        );
        assert_eq!(
            checker.type_annotation_to_inferred(&none_annotation),
            InferredType::Unknown
        );
    }

    #[test]
    fn test_nullable_type_annotation() {
        let checker = TypeChecker::new();

        let nullable_string = Some(TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple(
            "String".to_string(),
        ))));
        let nullable_int = Some(TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple(
            "Int".to_string(),
        ))));

        let string_type = checker.type_annotation_to_inferred(&nullable_string);
        let int_type = checker.type_annotation_to_inferred(&nullable_int);

        match string_type {
            InferredType::Optional(inner) => assert_eq!(**inner, InferredType::String),
            _ => panic!("Expected optional string type"),
        }

        match int_type {
            InferredType::Optional(inner) => assert_eq!(**inner, InferredType::Int),
            _ => panic!("Expected optional int type"),
        }
    }

    #[test]
    fn test_variable_declaration_checking() {
        let checker = TypeChecker::new();
        let mut context = CheckContext::new();

        let val_stmt = Statement::ValDeclaration {
            name: "x".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            initializer: Expression::Literal(Literal::String("hello".to_string()), Span::default()),
            span: Span::default(),
        };

        checker.check_statement(&val_stmt, &mut context);

        assert_eq!(context.get_variable_type("x"), Some(&InferredType::String));
        assert!(!context.has_errors());
    }

    #[test]
    fn test_var_declaration_checking() {
        let checker = TypeChecker::new();
        let mut context = CheckContext::new();

        let var_stmt = Statement::VarDeclaration {
            name: "y".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
            initializer: Some(Expression::Literal(
                Literal::Number("42".to_string()),
                Span::default(),
            )),
            span: Span::default(),
        };

        checker.check_statement(&var_stmt, &mut context);

        assert_eq!(context.get_variable_type("y"), Some(&InferredType::Int));
        assert!(!context.has_errors());
    }

    #[test]
    fn test_function_declaration_checking() {
        let checker = TypeChecker::new();
        let mut context = CheckContext::new();

        let func_stmt = Statement::FunctionDeclaration {
            name: "test".to_string(),
            type_parameters: vec![],
            parameters: vec![Parameter {
                name: "param".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
                default_value: None,
                span: Span::default(),
            }],
            return_type: Some(TypeAnnotation::Simple("Int".to_string())),
            body: Expression::Literal(Literal::Number("42".to_string()), Span::default()),
            modifiers: Modifiers::default(),
            span: Span::default(),
        };

        checker.check_statement(&func_stmt, &mut context);

        assert_eq!(
            context.get_variable_type("test"),
            Some(&InferredType::Unknown)
        );
        assert!(!context.has_errors());
    }

    #[test]
    fn test_program_checking() {
        let checker = TypeChecker::new();

        let program = Program {
            statements: vec![
                Statement::ValDeclaration {
                    name: "x".to_string(),
                    type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
                    initializer: Expression::Literal(
                        Literal::String("hello".to_string()),
                        Span::default(),
                    ),
                    span: Span::default(),
                },
                Statement::VarDeclaration {
                    name: "y".to_string(),
                    type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
                    initializer: Some(Expression::Literal(
                        Literal::Number("42".to_string()),
                        Span::default(),
                    )),
                    span: Span::default(),
                },
            ],
            span: Span::default(),
        };

        let result = checker.check_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_null_safety_checking() {
        let checker = TypeChecker::new();

        let program = Program {
            statements: vec![
                Statement::ValDeclaration {
                    name: "nullable".to_string(),
                    type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
                    initializer: Expression::Literal(Literal::Null, Span::default()),
                    span: Span::default(),
                },
                Statement::ValDeclaration {
                    name: "normal".to_string(),
                    type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
                    initializer: Expression::Literal(
                        Literal::String("hello".to_string()),
                        Span::default(),
                    ),
                    span: Span::default(),
                },
            ],
            span: Span::default(),
        };

        let warnings = checker.check_null_safety(&program);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("Assigning null to val declaration"));
    }

    #[test]
    fn test_forbidden_syntax_checking() {
        let checker = TypeChecker::new();
        let program = Program {
            statements: vec![],
            span: Span::default(),
        };

        let violations = checker.check_forbidden_syntax(&program);
        assert_eq!(violations.len(), 0); // Currently no forbidden syntax implemented
    }

    #[test]
    fn test_error_collection() {
        let mut context = CheckContext::new();

        let error1 = CheckError::TypeError("Type mismatch".to_string());
        let error2 = CheckError::UndefinedVariable("x".to_string());

        context.add_error(error1);
        context.add_error(error2);

        assert!(context.has_errors());
        assert_eq!(context.get_errors().len(), 2);
    }

    #[test]
    fn test_warning_collection() {
        let mut context = CheckContext::new();

        context.add_warning("Warning 1".to_string());
        context.add_warning("Warning 2".to_string());

        assert_eq!(context.get_warnings().len(), 2);
        assert_eq!(context.get_warnings()[0], "Warning 1");
        assert_eq!(context.get_warnings()[1], "Warning 2");
    }

    #[test]
    fn test_nested_scopes() {
        let mut context = CheckContext::new();

        // Outer scope
        context.declare_variable("outer".to_string(), InferredType::String);

        // Middle scope
        context.push_scope();
        context.declare_variable("middle".to_string(), InferredType::Int);

        // Inner scope
        context.push_scope();
        context.declare_variable("inner".to_string(), InferredType::Boolean);

        // All variables should be visible
        assert_eq!(
            context.get_variable_type("outer"),
            Some(&InferredType::String)
        );
        assert_eq!(
            context.get_variable_type("middle"),
            Some(&InferredType::Int)
        );
        assert_eq!(
            context.get_variable_type("inner"),
            Some(&InferredType::Boolean)
        );

        // Pop inner scope
        context.pop_scope();
        assert_eq!(
            context.get_variable_type("outer"),
            Some(&InferredType::String)
        );
        assert_eq!(
            context.get_variable_type("middle"),
            Some(&InferredType::Int)
        );
        assert_eq!(context.get_variable_type("inner"), None);

        // Pop middle scope
        context.pop_scope();
        assert_eq!(
            context.get_variable_type("outer"),
            Some(&InferredType::String)
        );
        assert_eq!(context.get_variable_type("middle"), None);
        assert_eq!(context.get_variable_type("inner"), None);
    }

    #[test]
    fn test_variable_shadowing() {
        let mut context = CheckContext::new();

        // Declare variable in outer scope
        context.declare_variable("x".to_string(), InferredType::String);
        assert_eq!(context.get_variable_type("x"), Some(&InferredType::String));

        // Shadow it in inner scope
        context.push_scope();
        context.declare_variable("x".to_string(), InferredType::Int);
        assert_eq!(context.get_variable_type("x"), Some(&InferredType::Int)); // Should see shadowed version

        // Pop scope - should see original again
        context.pop_scope();
        assert_eq!(context.get_variable_type("x"), Some(&InferredType::String));
    }

    #[test]
    fn test_inferred_type_equality() {
        assert_eq!(InferredType::String, InferredType::String);
        assert_eq!(InferredType::Int, InferredType::Int);
        assert_eq!(InferredType::Boolean, InferredType::Boolean);
        assert_eq!(InferredType::Null, InferredType::Null);
        assert_eq!(InferredType::Unknown, InferredType::Unknown);

        assert_ne!(InferredType::String, InferredType::Int);
        assert_ne!(InferredType::Boolean, InferredType::Null);

        let optional_string = InferredType::Optional(Box::new(InferredType::String));
        let optional_string2 = InferredType::Optional(Box::new(InferredType::String));
        let optional_int = InferredType::Optional(Box::new(InferredType::Int));

        assert_eq!(optional_string, optional_string2);
        assert_ne!(optional_string, optional_int);
    }

    #[test]
    fn test_check_error_display() {
        let type_error = CheckError::TypeError("Expected Int, found String".to_string());
        let null_error = CheckError::NullSafetyError("Null dereference".to_string());
        let undefined_error = CheckError::UndefinedVariable("x".to_string());
        let syntax_error = CheckError::SyntaxError("Invalid syntax".to_string());
        let validation_error = CheckError::ValidationError("Validation failed".to_string());

        assert!(type_error.to_string().contains("Type error"));
        assert!(null_error.to_string().contains("Null safety violation"));
        assert!(undefined_error.to_string().contains("Undefined variable"));
        assert!(syntax_error.to_string().contains("Invalid syntax"));
        assert!(validation_error.to_string().contains("Validation error"));
    }

    #[test]
    fn test_type_checker_default() {
        let checker = TypeChecker::default();
        let context = CheckContext::new();

        let literal = Expression::Literal(Literal::String("test".to_string()), Span::default());
        let inferred = checker.infer_expression_type(&literal, &context);

        assert_eq!(inferred, InferredType::String);
    }

    #[test]
    fn test_complex_expression_checking() {
        let checker = TypeChecker::new();
        let mut context = CheckContext::new();

        // Declare some variables
        context.declare_variable("name".to_string(), InferredType::String);
        context.declare_variable("age".to_string(), InferredType::Int);

        // Test complex binary expression
        let complex_expr = Expression::Binary {
            left: Box::new(Expression::Identifier("age".to_string(), Span::default())),
            op: BinaryOp::Greater,
            right: Box::new(Expression::Literal(
                Literal::Number("18".to_string()),
                Span::default(),
            )),
            span: Span::default(),
        };

        let result_type = checker.infer_expression_type(&complex_expr, &context);
        assert_eq!(result_type, InferredType::Boolean);
    }

    #[test]
    fn test_performance_with_many_variables() {
        let mut context = CheckContext::new();

        // Add many variables
        for i in 0..1000 {
            context.declare_variable(format!("var{}", i), InferredType::Int);
        }

        // Test lookup performance
        let start = std::time::Instant::now();
        for i in 0..1000 {
            let var_name = format!("var{}", i);
            assert_eq!(
                context.get_variable_type(&var_name),
                Some(&InferredType::Int)
            );
        }
        let duration = start.elapsed();

        // Should complete reasonably fast
        assert!(duration < std::time::Duration::from_millis(10));
    }
}
