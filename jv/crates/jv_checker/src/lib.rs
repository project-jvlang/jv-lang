// jv_checker - Static analysis and validation
pub mod diagnostics;

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
                self.check_expression(body.as_ref(), context);

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
mod tests;
