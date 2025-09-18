// jv_codegen_java - Java 25 code generation from desugared IR
use jv_ast::Span;
use jv_ir::*;
use std::collections::HashMap;
use std::fmt::Write as FmtWrite;

/// Main Java code generator
pub struct JavaCodeGenerator {
    /// Indentation level for formatting
    indent_level: usize,
    /// Import statements to include
    imports: HashMap<String, String>,
    /// Configuration options
    config: JavaCodeGenConfig,
}

/// Configuration for Java code generation
#[derive(Debug, Clone)]
pub struct JavaCodeGenConfig {
    /// Indentation string (spaces or tabs)
    pub indent: String,
    /// Generate additional null checks for safety
    pub extra_null_checks: bool,
    /// Use Java 25 features when possible
    pub use_modern_features: bool,
    /// Include source comments for debugging
    pub include_source_comments: bool,
}

impl Default for JavaCodeGenConfig {
    fn default() -> Self {
        Self {
            indent: "    ".to_string(), // 4 spaces
            extra_null_checks: true,
            use_modern_features: true,
            include_source_comments: false,
        }
    }
}

/// Builder for generating Java source code fragments
pub struct JavaSourceBuilder {
    content: String,
    indent_level: usize,
    indent: String,
}

impl JavaSourceBuilder {
    pub fn new(indent: String) -> Self {
        Self {
            content: String::new(),
            indent_level: 0,
            indent,
        }
    }

    pub fn push_line(&mut self, line: &str) {
        for _ in 0..self.indent_level {
            self.content.push_str(&self.indent);
        }
        self.content.push_str(line);
        self.content.push('\n');
    }

    pub fn push(&mut self, text: &str) {
        self.content.push_str(text);
    }

    pub fn push_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.content.push_str(&self.indent);
        }
    }

    pub fn indent(&mut self) {
        self.indent_level += 1;
    }

    pub fn dedent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    pub fn build(self) -> String {
        self.content
    }

    pub fn is_empty(&self) -> bool {
        self.content.trim().is_empty()
    }
}

/// Complete Java compilation unit
#[derive(Debug, Clone)]
pub struct JavaCompilationUnit {
    pub package_declaration: Option<String>,
    pub imports: Vec<String>,
    pub type_declarations: Vec<String>,
}

impl JavaCompilationUnit {
    pub fn new() -> Self {
        Self {
            package_declaration: None,
            imports: Vec::new(),
            type_declarations: Vec::new(),
        }
    }

    pub fn to_source(&self, config: &JavaCodeGenConfig) -> String {
        let mut builder = JavaSourceBuilder::new(config.indent.clone());

        // Package declaration
        if let Some(package) = &self.package_declaration {
            builder.push_line(&format!("package {};", package));
            builder.push_line("");
        }

        // Imports
        if !self.imports.is_empty() {
            for import in &self.imports {
                builder.push_line(&format!("import {};", import));
            }
            builder.push_line("");
        }

        // Type declarations
        for (i, type_decl) in self.type_declarations.iter().enumerate() {
            if i > 0 {
                builder.push_line("");
            }
            builder.push(type_decl);
        }

        builder.build()
    }
}

impl JavaCodeGenerator {
    pub fn new() -> Self {
        Self::with_config(JavaCodeGenConfig::default())
    }

    pub fn with_config(config: JavaCodeGenConfig) -> Self {
        Self {
            indent_level: 0,
            imports: HashMap::new(),
            config,
        }
    }

    /// Generate complete Java source file from IR program
    pub fn generate_compilation_unit(
        &mut self,
        program: &IrProgram,
    ) -> Result<JavaCompilationUnit, CodeGenError> {
        let mut code = String::new();

        // Generate package declaration
        if let Some(package) = &program.package {
            code.push_str(&format!("package {};\n\n", package));
        }

        // Generate imports (basic for now)
        code.push_str("import java.util.*;\n");
        code.push_str("import java.util.concurrent.*;\n\n");

        // Generate main class for top-level statements
        code.push_str("public class GeneratedMain {\n");
        code.push_str("    public static void main(String[] args) {\n");

        // Generate statements
        for stmt in &program.type_declarations {
            let stmt_code = self.generate_statement(stmt)?;
            code.push_str(&format!("        {}\n", stmt_code));
        }

        code.push_str("    }\n");
        code.push_str("}\n");

        Ok(JavaCompilationUnit {
            package_declaration: program.package.clone(),
            imports: vec![
                "java.util.*".to_string(),
                "java.util.concurrent.*".to_string(),
            ],
            type_declarations: vec![code],
        })
    }

    /// Generate Java class declaration
    pub fn generate_class(&mut self, class: &IrStatement) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_class")
    }

    /// Generate Java record declaration (from jv data class)
    pub fn generate_record(&mut self, record: &IrStatement) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_record")
    }

    /// Generate Java interface declaration
    pub fn generate_interface(&mut self, interface: &IrStatement) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_interface")
    }

    /// Generate Java method declaration
    pub fn generate_method(&mut self, method: &IrStatement) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_method")
    }

    /// Generate method overloads for default parameters
    pub fn generate_method_overloads(
        &mut self,
        overloads: &[MethodOverload],
    ) -> Result<Vec<String>, CodeGenError> {
        panic!("not yet implemented: generate_method_overloads")
    }

    /// Generate utility class for top-level functions
    pub fn generate_utility_class(
        &mut self,
        utility_class: &UtilityClass,
    ) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_utility_class")
    }

    /// Generate Java statement
    pub fn generate_statement(&mut self, stmt: &IrStatement) -> Result<String, CodeGenError> {
        match stmt {
            IrStatement::VariableDeclaration {
                name,
                java_type,
                initializer,
                is_final,
                modifiers: _,
                span: _,
            } => {
                let mut result = String::new();

                // Generate modifier (final if needed)
                if *is_final {
                    result.push_str("final ");
                }

                // Generate type
                result.push_str(&self.generate_type(java_type)?);
                result.push(' ');

                // Generate variable name
                result.push_str(name);

                // Generate initializer if present
                if let Some(init_expr) = initializer {
                    result.push_str(" = ");
                    result.push_str(&self.generate_expression(init_expr)?);
                }

                result.push(';');
                Ok(result)
            }
            _ => Ok("// TODO: Implement statement type".to_string()),
        }
    }

    /// Generate Java expression
    pub fn generate_expression(&mut self, expr: &IrExpression) -> Result<String, CodeGenError> {
        match expr {
            IrExpression::Literal(lit, _) => {
                use jv_ast::Literal;
                Ok(match lit {
                    Literal::String(s) => format!("\"{}\"", s), // Escape string properly in production
                    Literal::Number(n) => n.clone(),
                    Literal::Boolean(b) => b.to_string(),
                    Literal::Character(c) => format!("'{}'", c),
                    Literal::Null => "null".to_string(),
                })
            }
            IrExpression::Identifier { name, .. } => Ok(name.clone()),
            IrExpression::Binary {
                left, op, right, ..
            } => {
                let left_code = self.generate_expression(left)?;
                let right_code = self.generate_expression(right)?;
                let op_str = self.generate_binary_op(op)?;
                Ok(format!("{} {} {}", left_code, op_str, right_code))
            }
            _ => Ok("null /* TODO: Implement expression type */".to_string()),
        }
    }

    /// Generate Java type representation
    pub fn generate_type(&self, java_type: &JavaType) -> Result<String, CodeGenError> {
        match java_type {
            JavaType::Primitive(name) => Ok(name.clone()),
            JavaType::Reference { name, generic_args } => {
                if generic_args.is_empty() {
                    Ok(name.clone())
                } else {
                    let args = generic_args
                        .iter()
                        .map(|arg| self.generate_type(arg))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(format!("{}<{}>", name, args.join(", ")))
                }
            }
            JavaType::Array {
                element_type,
                dimensions,
            } => {
                let base_type = self.generate_type(element_type)?;
                let brackets = "[]".repeat(*dimensions);
                Ok(format!("{}{}", base_type, brackets))
            }
            JavaType::Void => Ok("void".to_string()),
            JavaType::Functional { interface_name, .. } => Ok(interface_name.clone()),
        }
    }

    /// Generate binary operator
    pub fn generate_binary_op(&self, op: &jv_ast::BinaryOp) -> Result<String, CodeGenError> {
        use jv_ast::BinaryOp;
        Ok(match op {
            BinaryOp::Add => "+".to_string(),
            BinaryOp::Subtract => "-".to_string(),
            BinaryOp::Multiply => "*".to_string(),
            BinaryOp::Divide => "/".to_string(),
            BinaryOp::Modulo => "%".to_string(),
            BinaryOp::Equal => "==".to_string(),
            BinaryOp::NotEqual => "!=".to_string(),
            BinaryOp::Less => "<".to_string(),
            BinaryOp::LessEqual => "<=".to_string(),
            BinaryOp::Greater => ">".to_string(),
            BinaryOp::GreaterEqual => ">=".to_string(),
            BinaryOp::And => "&&".to_string(),
            BinaryOp::Or => "||".to_string(),
            BinaryOp::BitAnd => "&".to_string(),
            BinaryOp::BitOr => "|".to_string(),
            BinaryOp::BitXor => "^".to_string(),
            BinaryOp::Elvis => "// Elvis operator TODO".to_string(), // Will need special handling
            BinaryOp::PlusAssign => "+=".to_string(),
            BinaryOp::MinusAssign => "-=".to_string(),
            BinaryOp::MultiplyAssign => "*=".to_string(),
            BinaryOp::DivideAssign => "/=".to_string(),
        })
    }

    /// Generate Java modifiers (public, static, final, etc.)
    pub fn generate_modifiers(&self, modifiers: &IrModifiers) -> String {
        panic!("not yet implemented: generate_modifiers")
    }

    /// Generate switch expression (Java 14+ pattern matching)
    pub fn generate_switch_expression(
        &mut self,
        switch: &IrExpression,
    ) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_switch_expression")
    }

    /// Generate null-safe operations with proper null checks
    pub fn generate_null_safe_operation(
        &mut self,
        operation: &IrExpression,
    ) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_null_safe_operation")
    }

    /// Generate CompletableFuture operations (from async/await)
    pub fn generate_completable_future(
        &mut self,
        cf_expr: &IrExpression,
    ) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_completable_future")
    }

    /// Generate Virtual Thread operations (from spawn)
    pub fn generate_virtual_thread(
        &mut self,
        vt_expr: &IrExpression,
    ) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_virtual_thread")
    }

    /// Generate try-with-resources (from use blocks)
    pub fn generate_try_with_resources(
        &mut self,
        twr_expr: &IrExpression,
    ) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_try_with_resources")
    }

    /// Generate string formatting (from string interpolation)
    pub fn generate_string_format(
        &mut self,
        format_expr: &IrExpression,
    ) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_string_format")
    }

    /// Generate lambda expressions
    pub fn generate_lambda(&mut self, lambda: &IrExpression) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_lambda")
    }

    /// Add import statement
    pub fn add_import(&mut self, import_path: &str) {
        self.imports
            .insert(import_path.to_string(), import_path.to_string());
    }

    /// Add standard Java imports for generated constructs
    pub fn add_standard_imports(&mut self, program: &IrProgram) {
        // Will analyze program and add necessary imports
        panic!("not yet implemented: add_standard_imports")
    }

    /// Format Java source code with proper indentation
    pub fn format_java_source(&self, source: &str) -> String {
        // Basic formatting - in real implementation would use a proper formatter
        panic!("not yet implemented: format_java_source")
    }

    /// Reset generator state for new compilation unit
    pub fn reset(&mut self) {
        self.indent_level = 0;
        self.imports.clear();
    }
}

/// Error types for Java code generation
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum CodeGenError {
    #[error("Unsupported IR construct: {construct}")]
    UnsupportedConstruct {
        construct: String,
        span: Option<Span>,
    },

    #[error("Type generation error: {message}")]
    TypeGenerationError { message: String, span: Option<Span> },

    #[error("Invalid method signature: {message}")]
    InvalidMethodSignature { message: String, span: Option<Span> },

    #[error("Null safety error: {message}")]
    NullSafetyError { message: String, span: Option<Span> },

    #[error("Generic type error: {message}")]
    GenericTypeError { message: String, span: Option<Span> },

    #[error("Import resolution error: {message}")]
    ImportError { message: String },

    #[error("Source formatting error: {message}")]
    FormattingError { message: String },

    #[error("Java 25 feature error: {message}")]
    Java25FeatureError { message: String, span: Option<Span> },

    #[error("Record generation error: {message}")]
    RecordGenerationError { message: String, span: Option<Span> },

    #[error("Pattern matching error: {message}")]
    PatternMatchingError { message: String, span: Option<Span> },
}

/// Type mapping utilities
pub struct JavaTypeMapper;

impl JavaTypeMapper {
    /// Map primitive types to Java equivalents
    pub fn map_primitive_type(type_name: &str) -> Result<String, CodeGenError> {
        panic!("not yet implemented: map_primitive_type")
    }

    /// Map collection types to Java equivalents
    pub fn map_collection_type(
        type_name: &str,
        element_types: &[JavaType],
    ) -> Result<String, CodeGenError> {
        panic!("not yet implemented: map_collection_type")
    }

    /// Generate generic type parameters
    pub fn generate_generic_parameters(
        type_params: &[IrTypeParameter],
    ) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_generic_parameters")
    }

    /// Generate type bounds for generic constraints
    pub fn generate_type_bounds(bounds: &[JavaType]) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_type_bounds")
    }
}

/// Null safety code generation utilities
pub struct NullSafetyGenerator;

impl NullSafetyGenerator {
    /// Generate null check for expression
    pub fn generate_null_check(expr: &str, java_type: &JavaType) -> String {
        panic!("not yet implemented: generate_null_check")
    }

    /// Generate optional chaining equivalent
    pub fn generate_optional_chaining(operations: &[String]) -> String {
        panic!("not yet implemented: generate_optional_chaining")
    }

    /// Generate elvis operator equivalent
    pub fn generate_elvis_operator(left_expr: &str, right_expr: &str) -> String {
        panic!("not yet implemented: generate_elvis_operator")
    }
}

/// Java 25 feature code generation
pub struct Java25FeatureGenerator;

impl Java25FeatureGenerator {
    /// Generate pattern matching switch
    pub fn generate_pattern_switch(
        discriminant: &str,
        cases: &[IrSwitchCase],
    ) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_pattern_switch")
    }

    /// Generate record pattern matching
    pub fn generate_record_patterns(patterns: &[IrCaseLabel]) -> Result<String, CodeGenError> {
        panic!("not yet implemented: generate_record_patterns")
    }

    /// Generate virtual thread creation
    pub fn generate_virtual_thread_creation(runnable_expr: &str) -> String {
        panic!("not yet implemented: generate_virtual_thread_creation")
    }

    /// Generate text blocks for multi-line strings
    pub fn generate_text_block(content: &str) -> String {
        panic!("not yet implemented: generate_text_block")
    }
}

/// Import management
pub struct ImportManager {
    imports: HashMap<String, String>,
    java_lang_types: std::collections::HashSet<String>,
}

impl ImportManager {
    pub fn new() -> Self {
        let mut java_lang_types = std::collections::HashSet::new();
        java_lang_types.insert("String".to_string());
        java_lang_types.insert("Object".to_string());
        java_lang_types.insert("Integer".to_string());
        java_lang_types.insert("Boolean".to_string());
        java_lang_types.insert("Character".to_string());
        java_lang_types.insert("Double".to_string());
        java_lang_types.insert("Float".to_string());
        java_lang_types.insert("Long".to_string());
        java_lang_types.insert("Short".to_string());
        java_lang_types.insert("Byte".to_string());

        Self {
            imports: HashMap::new(),
            java_lang_types,
        }
    }

    pub fn add_import(&mut self, class_name: &str) {
        // Don't import java.lang types or simple names without package
        if self.java_lang_types.contains(class_name)
            || (!class_name.contains('.') && !class_name.starts_with("java."))
        {
            return;
        }
        self.imports
            .insert(class_name.to_string(), class_name.to_string());
    }

    pub fn add_standard_import(&mut self, import: StandardImport) {
        match import {
            StandardImport::CompletableFuture => {
                self.add_import("java.util.concurrent.CompletableFuture");
            }
            StandardImport::VirtualThread => {
                self.add_import("java.lang.Thread");
            }
            StandardImport::Collections => {
                self.add_import("java.util.*");
            }
            StandardImport::Optional => {
                self.add_import("java.util.Optional");
            }
        }
    }

    pub fn get_imports(&self) -> Vec<String> {
        let mut imports: Vec<_> = self.imports.values().cloned().collect();
        imports.sort();
        imports
    }
}

#[derive(Debug, Clone)]
pub enum StandardImport {
    CompletableFuture,
    VirtualThread,
    Collections,
    Optional,
}

// Default implementations
impl Default for JavaCodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for ImportManager {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for JavaCompilationUnit {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::{Literal, Span};
    use jv_ir::{
        IrBinaryOperator, IrExpression, IrLiteral, IrModifiers, IrParameter, IrProgram,
        IrStatement, IrVisibility, JavaType,
    };

    mod test_utils {
        use super::*;

        pub fn dummy_span() -> Span {
            Span::new(1, 1, 1, 10)
        }

        pub fn create_test_program() -> IrProgram {
            IrProgram {
                package: Some("com.example".to_string()),
                imports: vec![],
                statements: vec![],
                span: dummy_span(),
            }
        }

        pub fn create_simple_variable_declaration() -> IrStatement {
            IrStatement::VariableDeclaration {
                name: "greeting".to_string(),
                java_type: JavaType::String,
                initializer: Some(IrExpression::Literal(IrLiteral::String(
                    "Hello".to_string(),
                ))),
                is_final: true,
                modifiers: IrModifiers::default(),
                span: dummy_span(),
            }
        }

        pub fn create_simple_method() -> IrStatement {
            IrStatement::MethodDeclaration {
                name: "add".to_string(),
                parameters: vec![
                    IrParameter {
                        name: "a".to_string(),
                        java_type: JavaType::Int,
                        modifiers: IrModifiers::default(),
                    },
                    IrParameter {
                        name: "b".to_string(),
                        java_type: JavaType::Int,
                        modifiers: IrModifiers::default(),
                    },
                ],
                return_type: JavaType::Int,
                body: vec![IrStatement::Return {
                    value: Some(IrExpression::BinaryOperation {
                        left: Box::new(IrExpression::Variable("a".to_string())),
                        operator: IrBinaryOperator::Add,
                        right: Box::new(IrExpression::Variable("b".to_string())),
                    }),
                    span: dummy_span(),
                }],
                modifiers: IrModifiers {
                    visibility: IrVisibility::Public,
                    is_static: true,
                    ..Default::default()
                },
                span: dummy_span(),
            }
        }
    }

    #[test]
    fn test_java_type_generation() {
        let mut generator = JavaCodeGenerator::new();

        assert_eq!(generator.generate_java_type(&JavaType::Int), "int");
        assert_eq!(generator.generate_java_type(&JavaType::String), "String");
        assert_eq!(generator.generate_java_type(&JavaType::Boolean), "boolean");
        assert_eq!(generator.generate_java_type(&JavaType::Double), "double");
        assert_eq!(generator.generate_java_type(&JavaType::Void), "void");

        let list_type = JavaType::Generic {
            name: "List".to_string(),
            type_args: vec![JavaType::String],
        };
        assert_eq!(generator.generate_java_type(&list_type), "List<String>");

        let nullable_string = JavaType::Nullable(Box::new(JavaType::String));
        assert_eq!(generator.generate_java_type(&nullable_string), "String");
    }

    #[test]
    fn test_variable_declaration_generation() {
        let mut generator = JavaCodeGenerator::new();
        let var_decl = test_utils::create_simple_variable_declaration();

        let result = generator.generate_statement(&var_decl).unwrap();

        assert!(result.contains("final String greeting"));
        assert!(result.contains("\"Hello\""));
    }

    #[test]
    fn test_method_declaration_generation() {
        let mut generator = JavaCodeGenerator::new();
        let method_decl = test_utils::create_simple_method();

        let result = generator.generate_statement(&method_decl).unwrap();

        assert!(result.contains("public static int add"));
        assert!(result.contains("int a, int b"));
        assert!(result.contains("return a + b"));
    }

    #[test]
    fn test_literal_generation() {
        let mut generator = JavaCodeGenerator::new();

        let string_lit = IrExpression::Literal(IrLiteral::String("hello".to_string()));
        assert_eq!(
            generator.generate_expression(&string_lit).unwrap(),
            "\"hello\""
        );

        let int_lit = IrExpression::Literal(IrLiteral::Integer(42));
        assert_eq!(generator.generate_expression(&int_lit).unwrap(), "42");

        let bool_lit = IrExpression::Literal(IrLiteral::Boolean(true));
        assert_eq!(generator.generate_expression(&bool_lit).unwrap(), "true");

        let null_lit = IrExpression::Literal(IrLiteral::Null);
        assert_eq!(generator.generate_expression(&null_lit).unwrap(), "null");
    }

    #[test]
    fn test_binary_operation_generation() {
        let mut generator = JavaCodeGenerator::new();

        let binary_op = IrExpression::BinaryOperation {
            left: Box::new(IrExpression::Literal(IrLiteral::Integer(5))),
            operator: IrBinaryOperator::Add,
            right: Box::new(IrExpression::Literal(IrLiteral::Integer(3))),
        };

        let result = generator.generate_expression(&binary_op).unwrap();
        assert_eq!(result, "5 + 3");
    }

    #[test]
    fn test_method_call_generation() {
        let mut generator = JavaCodeGenerator::new();

        let method_call = IrExpression::MethodCall {
            receiver: Some(Box::new(IrExpression::Variable("obj".to_string()))),
            method: "toString".to_string(),
            arguments: vec![],
        };

        let result = generator.generate_expression(&method_call).unwrap();
        assert_eq!(result, "obj.toString()");

        // Static method call
        let static_call = IrExpression::MethodCall {
            receiver: None,
            method: "Math.sqrt".to_string(),
            arguments: vec![IrExpression::Literal(IrLiteral::Double(16.0))],
        };

        let result = generator.generate_expression(&static_call).unwrap();
        assert_eq!(result, "Math.sqrt(16.0)");
    }

    #[test]
    fn test_class_declaration_generation() {
        let mut generator = JavaCodeGenerator::new();

        let class_decl = IrStatement::ClassDeclaration {
            name: "Person".to_string(),
            modifiers: IrModifiers {
                visibility: IrVisibility::Public,
                ..Default::default()
            },
            fields: vec![IrStatement::VariableDeclaration {
                name: "name".to_string(),
                java_type: JavaType::String,
                initializer: None,
                is_final: false,
                modifiers: IrModifiers {
                    visibility: IrVisibility::Private,
                    ..Default::default()
                },
                span: test_utils::dummy_span(),
            }],
            methods: vec![IrStatement::MethodDeclaration {
                name: "getName".to_string(),
                parameters: vec![],
                return_type: JavaType::String,
                body: vec![IrStatement::Return {
                    value: Some(IrExpression::FieldAccess {
                        object: Box::new(IrExpression::This),
                        field: "name".to_string(),
                    }),
                    span: test_utils::dummy_span(),
                }],
                modifiers: IrModifiers {
                    visibility: IrVisibility::Public,
                    ..Default::default()
                },
                span: test_utils::dummy_span(),
            }],
            span: test_utils::dummy_span(),
        };

        let result = generator.generate_statement(&class_decl).unwrap();

        assert!(result.contains("public class Person"));
        assert!(result.contains("private String name"));
        assert!(result.contains("public String getName()"));
        assert!(result.contains("return this.name"));
    }

    #[test]
    fn test_record_generation() {
        let mut generator = JavaCodeGenerator::new();

        let record_decl = IrStatement::RecordDeclaration {
            name: "Point".to_string(),
            components: vec![
                IrParameter {
                    name: "x".to_string(),
                    java_type: JavaType::Double,
                    modifiers: IrModifiers::default(),
                },
                IrParameter {
                    name: "y".to_string(),
                    java_type: JavaType::Double,
                    modifiers: IrModifiers::default(),
                },
            ],
            modifiers: IrModifiers {
                visibility: IrVisibility::Public,
                ..Default::default()
            },
            span: test_utils::dummy_span(),
        };

        let result = generator.generate_statement(&record_decl).unwrap();

        assert!(result.contains("public record Point"));
        assert!(result.contains("double x"));
        assert!(result.contains("double y"));
    }

    #[test]
    fn test_switch_expression_generation() {
        let mut generator = JavaCodeGenerator::new();

        let switch_expr = IrExpression::SwitchExpression {
            discriminant: Box::new(IrExpression::Variable("value".to_string())),
            cases: vec![
                SwitchCase {
                    pattern: IrExpression::Literal(IrLiteral::Integer(1)),
                    body: IrExpression::Literal(IrLiteral::String("one".to_string())),
                    guard: None,
                },
                SwitchCase {
                    pattern: IrExpression::Literal(IrLiteral::Integer(2)),
                    body: IrExpression::Literal(IrLiteral::String("two".to_string())),
                    guard: None,
                },
            ],
            default_case: Some(Box::new(IrExpression::Literal(IrLiteral::String(
                "other".to_string(),
            )))),
        };

        let result = generator.generate_expression(&switch_expr).unwrap();

        assert!(result.contains("switch"));
        assert!(result.contains("case 1 -> \"one\""));
        assert!(result.contains("case 2 -> \"two\""));
        assert!(result.contains("default -> \"other\""));
    }

    #[test]
    fn test_null_safe_operations_generation() {
        let mut generator = JavaCodeGenerator::new();

        // user?.name
        let null_safe_access = IrExpression::NullSafeOperation {
            operation: Box::new(IrExpression::FieldAccess {
                object: Box::new(IrExpression::Variable("user".to_string())),
                field: "name".to_string(),
            }),
        };

        let result = generator.generate_expression(&null_safe_access).unwrap();

        // Should generate ternary operator for null safety
        assert!(result.contains("user != null ?"));
        assert!(result.contains(": null"));
    }

    #[test]
    fn test_virtual_thread_generation() {
        let mut generator = JavaCodeGenerator::new();

        let virtual_thread = IrExpression::VirtualThreadCreation {
            runnable: Box::new(IrExpression::Lambda {
                parameters: vec![],
                body: Box::new(IrExpression::MethodCall {
                    receiver: None,
                    method: "System.out.println".to_string(),
                    arguments: vec![IrExpression::Literal(IrLiteral::String(
                        "Hello from virtual thread".to_string(),
                    ))],
                }),
            }),
        };

        let result = generator.generate_expression(&virtual_thread).unwrap();

        assert!(result.contains("Thread.ofVirtual().start"));
        assert!(result.contains("System.out.println"));
    }

    #[test]
    fn test_completable_future_generation() {
        let mut generator = JavaCodeGenerator::new();

        let future_expr = IrExpression::CompletableFutureChain {
            future: Box::new(IrExpression::MethodCall {
                receiver: None,
                method: "CompletableFuture.supplyAsync".to_string(),
                arguments: vec![IrExpression::Lambda {
                    parameters: vec![],
                    body: Box::new(IrExpression::Literal(IrLiteral::String(
                        "async result".to_string(),
                    ))),
                }],
            }),
            operations: vec![FutureOperation::ThenApply {
                function: IrExpression::Lambda {
                    parameters: vec![IrParameter {
                        name: "result".to_string(),
                        java_type: JavaType::String,
                        modifiers: IrModifiers::default(),
                    }],
                    body: Box::new(IrExpression::MethodCall {
                        receiver: Some(Box::new(IrExpression::Variable("result".to_string()))),
                        method: "toUpperCase".to_string(),
                        arguments: vec![],
                    }),
                },
            }],
        };

        let result = generator.generate_expression(&future_expr).unwrap();

        assert!(result.contains("CompletableFuture.supplyAsync"));
        assert!(result.contains(".thenApply"));
    }

    #[test]
    fn test_try_with_resources_generation() {
        let mut generator = JavaCodeGenerator::new();

        let twr_expr = IrExpression::TryWithResources {
            resources: vec![IrStatement::VariableDeclaration {
                name: "file".to_string(),
                java_type: JavaType::Reference {
                    name: "FileInputStream".to_string(),
                    generic_args: vec![],
                },
                initializer: Some(IrExpression::MethodCall {
                    receiver: None,
                    method: "new FileInputStream".to_string(),
                    arguments: vec![IrExpression::Literal(IrLiteral::String(
                        "test.txt".to_string(),
                    ))],
                }),
                is_final: true,
                modifiers: IrModifiers::default(),
                span: test_utils::dummy_span(),
            }],
            try_block: vec![IrStatement::Expression {
                expression: IrExpression::MethodCall {
                    receiver: Some(Box::new(IrExpression::Variable("file".to_string()))),
                    method: "read".to_string(),
                    arguments: vec![],
                },
                span: test_utils::dummy_span(),
            }],
            catch_blocks: vec![],
            finally_block: None,
        };

        let result = generator.generate_expression(&twr_expr).unwrap();

        assert!(result.contains("try ("));
        assert!(result.contains("FileInputStream file"));
        assert!(result.contains("file.read()"));
    }

    #[test]
    fn test_string_format_generation() {
        let mut generator = JavaCodeGenerator::new();

        let string_format = IrExpression::StringFormat {
            format: "Hello, %s! You are %d years old.".to_string(),
            arguments: vec![
                IrExpression::Variable("name".to_string()),
                IrExpression::Variable("age".to_string()),
            ],
        };

        let result = generator.generate_expression(&string_format).unwrap();

        assert!(result.contains("String.format"));
        assert!(result.contains("\"Hello, %s! You are %d years old.\""));
        assert!(result.contains("name, age"));
    }

    #[test]
    fn test_lambda_expression_generation() {
        let mut generator = JavaCodeGenerator::new();

        let lambda = IrExpression::Lambda {
            parameters: vec![IrParameter {
                name: "x".to_string(),
                java_type: JavaType::Int,
                modifiers: IrModifiers::default(),
            }],
            body: Box::new(IrExpression::BinaryOperation {
                left: Box::new(IrExpression::Variable("x".to_string())),
                operator: IrBinaryOperator::Multiply,
                right: Box::new(IrExpression::Literal(IrLiteral::Integer(2))),
            }),
        };

        let result = generator.generate_expression(&lambda).unwrap();

        assert!(result.contains("x -> x * 2"));
    }

    #[test]
    fn test_complex_nested_expression() {
        let mut generator = JavaCodeGenerator::new();

        // (a + b) * (c - d)
        let complex_expr = IrExpression::BinaryOperation {
            left: Box::new(IrExpression::BinaryOperation {
                left: Box::new(IrExpression::Variable("a".to_string())),
                operator: IrBinaryOperator::Add,
                right: Box::new(IrExpression::Variable("b".to_string())),
            }),
            operator: IrBinaryOperator::Multiply,
            right: Box::new(IrExpression::BinaryOperation {
                left: Box::new(IrExpression::Variable("c".to_string())),
                operator: IrBinaryOperator::Subtract,
                right: Box::new(IrExpression::Variable("d".to_string())),
            }),
        };

        let result = generator.generate_expression(&complex_expr).unwrap();

        assert_eq!(result, "(a + b) * (c - d)");
    }

    #[test]
    fn test_import_management() {
        let mut generator = JavaCodeGenerator::new();

        generator.add_import("java.util.List");
        generator.add_import("java.time.LocalDateTime");
        generator.add_import("java.util.List"); // Duplicate should be ignored

        let imports = generator.get_imports();

        assert_eq!(imports.len(), 2);
        assert!(imports.contains(&"java.util.List".to_string()));
        assert!(imports.contains(&"java.time.LocalDateTime".to_string()));
    }

    #[test]
    fn test_compilation_unit_generation() {
        let mut generator = JavaCodeGenerator::new();

        let program = IrProgram {
            package: Some("com.example.test".to_string()),
            imports: vec!["java.util.List".to_string()],
            statements: vec![IrStatement::ClassDeclaration {
                name: "Example".to_string(),
                modifiers: IrModifiers {
                    visibility: IrVisibility::Public,
                    ..Default::default()
                },
                fields: vec![],
                methods: vec![IrStatement::MethodDeclaration {
                    name: "main".to_string(),
                    parameters: vec![IrParameter {
                        name: "args".to_string(),
                        java_type: JavaType::Array {
                            element_type: Box::new(JavaType::String),
                            dimensions: 1,
                        },
                        modifiers: IrModifiers::default(),
                    }],
                    return_type: JavaType::Void,
                    body: vec![IrStatement::Expression {
                        expression: IrExpression::MethodCall {
                            receiver: None,
                            method: "System.out.println".to_string(),
                            arguments: vec![IrExpression::Literal(IrLiteral::String(
                                "Hello, World!".to_string(),
                            ))],
                        },
                        span: test_utils::dummy_span(),
                    }],
                    modifiers: IrModifiers {
                        visibility: IrVisibility::Public,
                        is_static: true,
                        ..Default::default()
                    },
                    span: test_utils::dummy_span(),
                }],
                span: test_utils::dummy_span(),
            }],
            span: test_utils::dummy_span(),
        };

        let result = generator.generate(&program).unwrap();

        assert!(result.contains("package com.example.test;"));
        assert!(result.contains("import java.util.List;"));
        assert!(result.contains("public class Example"));
        assert!(result.contains("public static void main(String[] args)"));
        assert!(result.contains("System.out.println(\"Hello, World!\")"));
    }

    #[test]
    fn test_error_handling() {
        let mut generator = JavaCodeGenerator::new();

        // Test with invalid expression
        let invalid_expr = IrExpression::MethodCall {
            receiver: None,
            method: "".to_string(), // Empty method name should cause error
            arguments: vec![],
        };

        let result = generator.generate_expression(&invalid_expr);
        assert!(result.is_err());
    }

    #[test]
    fn test_indentation_consistency() {
        let mut generator = JavaCodeGenerator::new();

        let nested_class = IrStatement::ClassDeclaration {
            name: "Outer".to_string(),
            modifiers: IrModifiers {
                visibility: IrVisibility::Public,
                ..Default::default()
            },
            fields: vec![],
            methods: vec![IrStatement::MethodDeclaration {
                name: "method".to_string(),
                parameters: vec![],
                return_type: JavaType::Void,
                body: vec![IrStatement::If {
                    condition: IrExpression::Literal(IrLiteral::Boolean(true)),
                    then_branch: vec![IrStatement::Expression {
                        expression: IrExpression::MethodCall {
                            receiver: None,
                            method: "System.out.println".to_string(),
                            arguments: vec![IrExpression::Literal(IrLiteral::String(
                                "nested".to_string(),
                            ))],
                        },
                        span: test_utils::dummy_span(),
                    }],
                    else_branch: None,
                    span: test_utils::dummy_span(),
                }],
                modifiers: IrModifiers {
                    visibility: IrVisibility::Public,
                    ..Default::default()
                },
                span: test_utils::dummy_span(),
            }],
            span: test_utils::dummy_span(),
        };

        let result = generator.generate_statement(&nested_class).unwrap();

        // Should have consistent 4-space indentation
        let lines: Vec<&str> = result.lines().collect();

        // Find method line
        let method_line = lines
            .iter()
            .find(|line| line.contains("public void method"))
            .unwrap();
        assert!(method_line.starts_with("    ")); // 4 spaces for method

        // Find if statement
        let if_line = lines
            .iter()
            .find(|line| line.trim().starts_with("if"))
            .unwrap();
        assert!(if_line.starts_with("        ")); // 8 spaces for if statement
    }

    pub fn create_simple_class() -> IrStatement {
        IrStatement::ClassDeclaration {
            name: "TestClass".to_string(),
            type_parameters: vec![],
            superclass: None,
            interfaces: vec![],
            fields: vec![],
            methods: vec![],
            nested_classes: vec![],
            modifiers: IrModifiers {
                visibility: IrVisibility::Public,
                ..Default::default()
            },
            span: dummy_span(),
        }
    }

    pub fn create_simple_record() -> IrStatement {
        IrStatement::RecordDeclaration {
            name: "Person".to_string(),
            type_parameters: vec![],
            components: vec![
                IrRecordComponent {
                    name: "name".to_string(),
                    java_type: JavaType::string(),
                    span: dummy_span(),
                },
                IrRecordComponent {
                    name: "age".to_string(),
                    java_type: JavaType::int(),
                    span: dummy_span(),
                },
            ],
            interfaces: vec![],
            methods: vec![],
            modifiers: IrModifiers {
                visibility: IrVisibility::Public,
                ..Default::default()
            },
            span: dummy_span(),
        }
    }

    pub fn create_simple_method() -> IrStatement {
        IrStatement::MethodDeclaration {
            name: "calculateSum".to_string(),
            parameters: vec![
                IrParameter {
                    name: "a".to_string(),
                    java_type: JavaType::int(),
                    modifiers: IrModifiers::default(),
                    span: dummy_span(),
                },
                IrParameter {
                    name: "b".to_string(),
                    java_type: JavaType::int(),
                    modifiers: IrModifiers::default(),
                    span: dummy_span(),
                },
            ],
            return_type: JavaType::int(),
            body: Some(IrExpression::Binary {
                left: Box::new(IrExpression::Identifier {
                    name: "a".to_string(),
                    java_type: JavaType::int(),
                    span: dummy_span(),
                }),
                op: jv_ast::BinaryOp::Add,
                right: Box::new(IrExpression::Identifier {
                    name: "b".to_string(),
                    java_type: JavaType::int(),
                    span: dummy_span(),
                }),
                java_type: JavaType::int(),
                span: dummy_span(),
            }),
            modifiers: IrModifiers {
                visibility: IrVisibility::Public,
                is_static: true,
                ..Default::default()
            },
            throws: vec![],
            span: dummy_span(),
        }
    }
}

mod compilation_unit_tests {
    use super::*;

    #[test]
    fn test_empty_compilation_unit() {
        let unit = JavaCompilationUnit::new();
        let config = JavaCodeGenConfig::default();
        let source = unit.to_source(&config);
        assert_eq!(source.trim(), "");
    }

    #[test]
    fn test_compilation_unit_with_package() {
        let mut unit = JavaCompilationUnit::new();
        unit.package_declaration = Some("com.example.test".to_string());
        let config = JavaCodeGenConfig::default();
        let source = unit.to_source(&config);
        assert!(source.contains("package com.example.test;"));
    }

    #[test]
    fn test_compilation_unit_with_imports() {
        let mut unit = JavaCompilationUnit::new();
        unit.imports.push("java.util.List".to_string());
        unit.imports.push("java.util.ArrayList".to_string());
        let config = JavaCodeGenConfig::default();
        let source = unit.to_source(&config);
        assert!(source.contains("import java.util.List;"));
        assert!(source.contains("import java.util.ArrayList;"));
    }

    #[test]
    fn test_complete_compilation_unit() {
        let mut unit = JavaCompilationUnit::new();
        unit.package_declaration = Some("com.example".to_string());
        unit.imports.push("java.util.*".to_string());
        unit.type_declarations
            .push("public class Test {}".to_string());

        let config = JavaCodeGenConfig::default();
        let source = unit.to_source(&config);

        assert!(source.contains("package com.example;"));
        assert!(source.contains("import java.util.*;"));
        assert!(source.contains("public class Test {}"));
    }
}

mod source_builder_tests {
    use super::*;

    #[test]
    fn test_empty_builder() {
        let builder = JavaSourceBuilder::new("    ".to_string());
        assert!(builder.is_empty());
        assert_eq!(builder.build(), "");
    }

    #[test]
    fn test_simple_line() {
        let mut builder = JavaSourceBuilder::new("    ".to_string());
        builder.push_line("public class Test {}");
        assert!(!builder.is_empty());
        assert_eq!(builder.build(), "public class Test {}\n");
    }

    #[test]
    fn test_indentation() {
        let mut builder = JavaSourceBuilder::new("    ".to_string());
        builder.push_line("public class Test {");
        builder.indent();
        builder.push_line("private int value;");
        builder.dedent();
        builder.push_line("}");

        let expected = "public class Test {\n    private int value;\n}\n";
        assert_eq!(builder.build(), expected);
    }

    #[test]
    fn test_nested_indentation() {
        let mut builder = JavaSourceBuilder::new("  ".to_string());
        builder.push_line("class Outer {");
        builder.indent();
        builder.push_line("class Inner {");
        builder.indent();
        builder.push_line("int field;");
        builder.dedent();
        builder.push_line("}");
        builder.dedent();
        builder.push_line("}");

        let expected = "class Outer {\n  class Inner {\n    int field;\n  }\n}\n";
        assert_eq!(builder.build(), expected);
    }
}

mod code_generator_tests {
    use super::test_utils::*;
    use super::*;

    #[test]
    fn test_new_generator() {
        let generator = JavaCodeGenerator::new();
        assert_eq!(generator.indent_level, 0);
        assert!(generator.imports.is_empty());
    }

    #[test]
    fn test_generator_with_custom_config() {
        let config = JavaCodeGenConfig {
            indent: "\t".to_string(),
            extra_null_checks: false,
            use_modern_features: false,
            include_source_comments: true,
        };
        let generator = JavaCodeGenerator::with_config(config.clone());
        assert_eq!(generator.config.indent, "\t");
        assert!(!generator.config.extra_null_checks);
        assert!(!generator.config.use_modern_features);
        assert!(generator.config.include_source_comments);
    }

    #[test]
    fn test_add_import() {
        let mut generator = JavaCodeGenerator::new();
        generator.add_import("java.util.List");
        assert!(generator.imports.contains_key("java.util.List"));
    }

    #[test]
    fn test_reset_generator() {
        let mut generator = JavaCodeGenerator::new();
        generator.add_import("java.util.List");
        generator.indent_level = 2;

        generator.reset();

        assert_eq!(generator.indent_level, 0);
        assert!(generator.imports.is_empty());
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_compilation_unit")]
    fn test_generate_compilation_unit_panics() {
        let mut generator = JavaCodeGenerator::new();
        let program = create_test_program();
        generator.generate_compilation_unit(&program).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_class")]
    fn test_generate_class_panics() {
        let mut generator = JavaCodeGenerator::new();
        let class = create_simple_class();
        generator.generate_class(&class).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_record")]
    fn test_generate_record_panics() {
        let mut generator = JavaCodeGenerator::new();
        let record = create_simple_record();
        generator.generate_record(&record).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_method")]
    fn test_generate_method_panics() {
        let mut generator = JavaCodeGenerator::new();
        let method = create_simple_method();
        generator.generate_method(&method).unwrap();
    }
}

mod type_generation_tests {
    use super::*;

    #[test]
    #[should_panic(expected = "not yet implemented: generate_type")]
    fn test_generate_primitive_type_panics() {
        let generator = JavaCodeGenerator::new();
        let java_type = JavaType::int();
        generator.generate_type(&java_type).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_type")]
    fn test_generate_reference_type_panics() {
        let generator = JavaCodeGenerator::new();
        let java_type = JavaType::string();
        generator.generate_type(&java_type).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_type")]
    fn test_generate_generic_type_panics() {
        let generator = JavaCodeGenerator::new();
        let java_type = JavaType::Reference {
            name: "List".to_string(),
            generic_args: vec![JavaType::string()],
        };
        generator.generate_type(&java_type).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_type")]
    fn test_generate_array_type_panics() {
        let generator = JavaCodeGenerator::new();
        let java_type = JavaType::Array {
            element_type: Box::new(JavaType::int()),
            dimensions: 1,
        };
        generator.generate_type(&java_type).unwrap();
    }
}

mod expression_generation_tests {
    use super::test_utils::*;
    use super::*;

    #[test]
    #[should_panic(expected = "not yet implemented: generate_expression")]
    fn test_generate_literal_expression_panics() {
        let mut generator = JavaCodeGenerator::new();
        let expr = IrExpression::Literal(Literal::Number("42".to_string()), dummy_span());
        generator.generate_expression(&expr).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_expression")]
    fn test_generate_binary_expression_panics() {
        let mut generator = JavaCodeGenerator::new();
        let expr = IrExpression::Binary {
            left: Box::new(IrExpression::Literal(
                Literal::Number("1".to_string()),
                dummy_span(),
            )),
            op: jv_ast::BinaryOp::Add,
            right: Box::new(IrExpression::Literal(
                Literal::Number("2".to_string()),
                dummy_span(),
            )),
            java_type: JavaType::int(),
            span: dummy_span(),
        };
        generator.generate_expression(&expr).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_expression")]
    fn test_generate_method_call_panics() {
        let mut generator = JavaCodeGenerator::new();
        let expr = IrExpression::MethodCall {
            receiver: None,
            method_name: "println".to_string(),
            args: vec![IrExpression::Literal(
                Literal::String("Hello".to_string()),
                dummy_span(),
            )],
            java_type: JavaType::void(),
            span: dummy_span(),
        };
        generator.generate_expression(&expr).unwrap();
    }
}

mod null_safety_tests {
    use super::test_utils::*;
    use super::*;

    #[test]
    #[should_panic(expected = "not yet implemented: generate_null_safe_operation")]
    fn test_generate_null_safe_operation_panics() {
        let mut generator = JavaCodeGenerator::new();
        let expr = IrExpression::NullSafeOperation {
            expr: Box::new(IrExpression::Identifier {
                name: "obj".to_string(),
                java_type: JavaType::string(),
                span: dummy_span(),
            }),
            operation: Box::new(IrExpression::FieldAccess {
                receiver: Box::new(IrExpression::Identifier {
                    name: "obj".to_string(),
                    java_type: JavaType::string(),
                    span: dummy_span(),
                }),
                field_name: "length".to_string(),
                java_type: JavaType::int(),
                span: dummy_span(),
            }),
            default_value: Some(Box::new(IrExpression::Literal(
                Literal::Number("0".to_string()),
                dummy_span(),
            ))),
            java_type: JavaType::int(),
            span: dummy_span(),
        };
        generator.generate_null_safe_operation(&expr).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_null_check")]
    fn test_null_safety_generator_panics() {
        NullSafetyGenerator::generate_null_check("obj", &JavaType::string());
    }
}

mod java25_features_tests {
    use super::test_utils::*;
    use super::*;

    #[test]
    #[should_panic(expected = "not yet implemented: generate_switch_expression")]
    fn test_generate_switch_expression_panics() {
        let mut generator = JavaCodeGenerator::new();
        let expr = IrExpression::Switch {
            discriminant: Box::new(IrExpression::Identifier {
                name: "value".to_string(),
                java_type: JavaType::int(),
                span: dummy_span(),
            }),
            cases: vec![],
            java_type: JavaType::string(),
            span: dummy_span(),
        };
        generator.generate_switch_expression(&expr).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_pattern_switch")]
    fn test_pattern_switch_generator_panics() {
        Java25FeatureGenerator::generate_pattern_switch("value", &[]).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_virtual_thread_creation")]
    fn test_virtual_thread_generator_panics() {
        Java25FeatureGenerator::generate_virtual_thread_creation("() -> {}");
    }
}

mod async_await_tests {
    use super::test_utils::*;
    use super::*;

    #[test]
    #[should_panic(expected = "not yet implemented: generate_completable_future")]
    fn test_generate_completable_future_panics() {
        let mut generator = JavaCodeGenerator::new();
        let expr = IrExpression::CompletableFuture {
            operation: CompletableFutureOp::SupplyAsync,
            args: vec![],
            java_type: JavaType::Reference {
                name: "CompletableFuture".to_string(),
                generic_args: vec![JavaType::string()],
            },
            span: dummy_span(),
        };
        generator.generate_completable_future(&expr).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_virtual_thread")]
    fn test_generate_virtual_thread_panics() {
        let mut generator = JavaCodeGenerator::new();
        let expr = IrExpression::VirtualThread {
            operation: VirtualThreadOp::Start,
            args: vec![],
            java_type: JavaType::Reference {
                name: "Thread".to_string(),
                generic_args: vec![],
            },
            span: dummy_span(),
        };
        generator.generate_virtual_thread(&expr).unwrap();
    }
}

mod resource_management_tests {
    use super::test_utils::*;
    use super::*;

    #[test]
    #[should_panic(expected = "not yet implemented: generate_try_with_resources")]
    fn test_generate_try_with_resources_panics() {
        let mut generator = JavaCodeGenerator::new();
        let expr = IrExpression::TryWithResources {
            resources: vec![IrResource {
                name: "file".to_string(),
                initializer: IrExpression::ObjectCreation {
                    class_name: "FileReader".to_string(),
                    generic_args: vec![],
                    args: vec![IrExpression::Literal(
                        Literal::String("test.txt".to_string()),
                        dummy_span(),
                    )],
                    java_type: JavaType::Reference {
                        name: "FileReader".to_string(),
                        generic_args: vec![],
                    },
                    span: dummy_span(),
                },
                java_type: JavaType::Reference {
                    name: "FileReader".to_string(),
                    generic_args: vec![],
                },
                span: dummy_span(),
            }],
            body: Box::new(IrExpression::Block {
                statements: vec![],
                java_type: JavaType::void(),
                span: dummy_span(),
            }),
            java_type: JavaType::void(),
            span: dummy_span(),
        };
        generator.generate_try_with_resources(&expr).unwrap();
    }
}

mod string_interpolation_tests {
    use super::test_utils::*;
    use super::*;

    #[test]
    #[should_panic(expected = "not yet implemented: generate_string_format")]
    fn test_generate_string_format_panics() {
        let mut generator = JavaCodeGenerator::new();
        let expr = IrExpression::StringFormat {
            format_string: "Hello, %s!".to_string(),
            args: vec![IrExpression::Identifier {
                name: "name".to_string(),
                java_type: JavaType::string(),
                span: dummy_span(),
            }],
            span: dummy_span(),
        };
        generator.generate_string_format(&expr).unwrap();
    }
}

mod lambda_tests {
    use super::test_utils::*;
    use super::*;

    #[test]
    #[should_panic(expected = "not yet implemented: generate_lambda")]
    fn test_generate_lambda_panics() {
        let mut generator = JavaCodeGenerator::new();
        let expr = IrExpression::Lambda {
            functional_interface: "Function".to_string(),
            param_names: vec!["x".to_string()],
            param_types: vec![JavaType::int()],
            body: Box::new(IrExpression::Binary {
                left: Box::new(IrExpression::Identifier {
                    name: "x".to_string(),
                    java_type: JavaType::int(),
                    span: dummy_span(),
                }),
                op: jv_ast::BinaryOp::Multiply,
                right: Box::new(IrExpression::Literal(
                    Literal::Number("2".to_string()),
                    dummy_span(),
                )),
                java_type: JavaType::int(),
                span: dummy_span(),
            }),
            java_type: JavaType::Functional {
                interface_name: "Function".to_string(),
                param_types: vec![JavaType::int()],
                return_type: Box::new(JavaType::int()),
            },
            span: dummy_span(),
        };
        generator.generate_lambda(&expr).unwrap();
    }
}

mod type_mapper_tests {
    use super::*;

    #[test]
    #[should_panic(expected = "not yet implemented: map_primitive_type")]
    fn test_map_primitive_type_panics() {
        JavaTypeMapper::map_primitive_type("int").unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: map_collection_type")]
    fn test_map_collection_type_panics() {
        JavaTypeMapper::map_collection_type("List", &[JavaType::string()]).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_generic_parameters")]
    fn test_generate_generic_parameters_panics() {
        JavaTypeMapper::generate_generic_parameters(&[]).unwrap();
    }
}

mod import_manager_tests {
    use super::*;

    #[test]
    fn test_new_import_manager() {
        let manager = ImportManager::new();
        assert!(manager.imports.is_empty());
        assert!(manager.java_lang_types.contains("String"));
        assert!(manager.java_lang_types.contains("Object"));
    }

    #[test]
    fn test_add_import() {
        let mut manager = ImportManager::new();
        manager.add_import("java.util.List");
        let imports = manager.get_imports();
        assert!(imports.contains(&"java.util.List".to_string()));
    }

    #[test]
    fn test_add_standard_import() {
        let mut manager = ImportManager::new();
        manager.add_standard_import(StandardImport::CompletableFuture);
        let imports = manager.get_imports();
        assert!(imports.contains(&"java.util.concurrent.CompletableFuture".to_string()));
    }

    #[test]
    fn test_sorted_imports() {
        let mut manager = ImportManager::new();
        manager.add_import("java.util.List");
        manager.add_import("java.io.File");
        manager.add_import("java.util.ArrayList");

        let imports = manager.get_imports();
        assert_eq!(
            imports,
            vec![
                "java.io.File".to_string(),
                "java.util.ArrayList".to_string(),
                "java.util.List".to_string(),
            ]
        );
    }

    #[test]
    fn test_java_lang_type_not_imported() {
        let mut manager = ImportManager::new();
        manager.add_import("String"); // Should not be added
        let imports = manager.get_imports();
        assert!(imports.is_empty());
    }
}

mod modifiers_tests {
    use super::*;

    #[test]
    #[should_panic(expected = "not yet implemented: generate_modifiers")]
    fn test_generate_modifiers_panics() {
        let generator = JavaCodeGenerator::new();
        let modifiers = IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            is_final: false,
            ..Default::default()
        };
        generator.generate_modifiers(&modifiers);
    }
}

mod integration_tests {
    use super::test_utils::*;
    use super::*;

    #[test]
    #[should_panic(expected = "not yet implemented: generate_method_overloads")]
    fn test_method_overloads_integration_panics() {
        let mut generator = JavaCodeGenerator::new();
        let overloads = vec![MethodOverload {
            name: "test".to_string(),
            parameters: vec![],
            return_type: JavaType::void(),
            body: IrExpression::Block {
                statements: vec![],
                java_type: JavaType::void(),
                span: dummy_span(),
            },
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }];
        generator.generate_method_overloads(&overloads).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: generate_utility_class")]
    fn test_utility_class_generation_panics() {
        let mut generator = JavaCodeGenerator::new();
        let utility_class = UtilityClass {
            name: "TestUtils".to_string(),
            methods: vec![create_simple_method()],
            modifiers: IrModifiers {
                visibility: IrVisibility::Public,
                is_final: true,
                ..Default::default()
            },
            span: dummy_span(),
        };
        generator.generate_utility_class(&utility_class).unwrap();
    }

    #[test]
    #[should_panic(expected = "not yet implemented: add_standard_imports")]
    fn test_standard_imports_analysis_panics() {
        let mut generator = JavaCodeGenerator::new();
        let program = create_test_program();
        generator.add_standard_imports(&program);
    }

    #[test]
    #[should_panic(expected = "not yet implemented: format_java_source")]
    fn test_source_formatting_panics() {
        let generator = JavaCodeGenerator::new();
        generator.format_java_source("public class Test{}");
    }
}
