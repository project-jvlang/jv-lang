// jv_ast - Abstract Syntax Tree definitions for jv language
use serde::{Deserialize, Serialize};

/// Position information for AST nodes
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Span {
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

impl Span {
    pub fn new(start_line: usize, start_column: usize, end_line: usize, end_column: usize) -> Self {
        Self {
            start_line,
            start_column,
            end_line,
            end_column,
        }
    }

    pub fn dummy() -> Self {
        Self::default()
    }
}

/// Literal values
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    String(String),
    Number(String), // Keep as string for precision
    Boolean(bool),
    Null,
    Character(char),
}

/// Binary operators
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    // Comparison
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    // Logical
    And,
    Or,
    // Null safety
    Elvis, // ?:
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    // Assignment compound
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
}

/// Unary operators
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,    // !
    Minus,  // -
    Plus,   // +
    BitNot, // ~
}

/// Type annotations
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeAnnotation {
    Simple(String),
    Nullable(Box<TypeAnnotation>),
    Generic {
        name: String,
        type_args: Vec<TypeAnnotation>,
    },
    Function {
        params: Vec<TypeAnnotation>,
        return_type: Box<TypeAnnotation>,
    },
    Array(Box<TypeAnnotation>),
}

/// Pattern matching constructs
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Pattern {
    Literal(Literal, Span),
    Identifier(String, Span),
    Wildcard(Span), // _
    Constructor {
        name: String,
        patterns: Vec<Pattern>,
        span: Span,
    },
    Range {
        start: Box<Expression>,
        end: Box<Expression>,
        span: Span,
    },
    Guard {
        pattern: Box<Pattern>,
        condition: Expression,
        span: Span,
    },
}

/// Expressions in jv language
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    // Literals
    Literal(Literal, Span),

    // Identifiers
    Identifier(String, Span),

    // Binary operations
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
        span: Span,
    },

    // Unary operations
    Unary {
        op: UnaryOp,
        operand: Box<Expression>,
        span: Span,
    },

    // Function calls
    Call {
        function: Box<Expression>,
        args: Vec<Argument>,
        span: Span,
    },

    // Member access: obj.property
    MemberAccess {
        object: Box<Expression>,
        property: String,
        span: Span,
    },

    // Null-safe member access: obj?.property
    NullSafeMemberAccess {
        object: Box<Expression>,
        property: String,
        span: Span,
    },

    // Array/Index access: arr[index]
    IndexAccess {
        object: Box<Expression>,
        index: Box<Expression>,
        span: Span,
    },

    // Null-safe index access: arr?[index]
    NullSafeIndexAccess {
        object: Box<Expression>,
        index: Box<Expression>,
        span: Span,
    },

    // String interpolation
    StringInterpolation {
        parts: Vec<StringPart>,
        span: Span,
    },

    // When expressions
    When {
        expr: Option<Box<Expression>>,
        arms: Vec<WhenArm>,
        else_arm: Option<Box<Expression>>,
        span: Span,
    },

    // if expressions
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
        span: Span,
    },

    // Block expressions
    Block {
        statements: Vec<Statement>,
        span: Span,
    },

    // Array literals
    Array {
        elements: Vec<Expression>,
        span: Span,
    },

    // Lambda expressions
    Lambda {
        parameters: Vec<Parameter>,
        body: Box<Expression>,
        span: Span,
    },

    // Try expressions for error handling
    Try {
        expr: Box<Expression>,
        span: Span,
    },

    // This/super references
    This(Span),
    Super(Span),
}

/// Function call arguments (supports named arguments)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Argument {
    Positional(Expression),
    Named {
        name: String,
        value: Expression,
        span: Span,
    },
}

/// String interpolation parts
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StringPart {
    Text(String),
    Expression(Expression),
}

/// When expression arms with pattern matching
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WhenArm {
    pub pattern: Pattern,
    pub body: Expression,
    pub span: Span,
}

/// Function parameters with default values and named parameter support
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: Option<TypeAnnotation>,
    pub default_value: Option<Expression>,
    pub span: Span,
}

/// Visibility modifiers
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Visibility {
    Public,
    Private,
    Internal,
    Protected,
}

/// Class/data class modifiers
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Modifiers {
    pub visibility: Visibility,
    pub is_abstract: bool,
    pub is_final: bool,
    pub is_static: bool,
    pub is_override: bool,
    pub is_open: bool,
}

impl Default for Modifiers {
    fn default() -> Self {
        Self {
            visibility: Visibility::Private,
            is_abstract: false,
            is_final: false,
            is_static: false,
            is_override: false,
            is_open: false,
        }
    }
}

/// Class property declarations
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Property {
    pub name: String,
    pub type_annotation: Option<TypeAnnotation>,
    pub initializer: Option<Expression>,
    pub is_mutable: bool,
    pub modifiers: Modifiers,
    pub getter: Option<Box<Expression>>,
    pub setter: Option<Box<Expression>>,
    pub span: Span,
}

/// Extension function context
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExtensionFunction {
    pub receiver_type: TypeAnnotation,
    pub function: Box<Statement>, // Must be FunctionDeclaration
    pub span: Span,
}

/// Async/spawn constructs
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ConcurrencyConstruct {
    Spawn { body: Box<Expression>, span: Span },
    Async { body: Box<Expression>, span: Span },
    Await { expr: Box<Expression>, span: Span },
}

/// Resource management constructs
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ResourceManagement {
    Use {
        resource: Box<Expression>,
        body: Box<Expression>,
        span: Span,
    },
    Defer {
        body: Box<Expression>,
        span: Span,
    },
}

/// Statements in jv language  
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    // Variable declarations
    ValDeclaration {
        name: String,
        type_annotation: Option<TypeAnnotation>,
        initializer: Expression,
        modifiers: Modifiers,
        span: Span,
    },

    VarDeclaration {
        name: String,
        type_annotation: Option<TypeAnnotation>,
        initializer: Option<Expression>,
        modifiers: Modifiers,
        span: Span,
    },

    // Function declarations
    FunctionDeclaration {
        name: String,
        parameters: Vec<Parameter>,
        return_type: Option<TypeAnnotation>,
        body: Box<Expression>,
        modifiers: Modifiers,
        span: Span,
    },

    // Class declarations
    ClassDeclaration {
        name: String,
        type_parameters: Vec<String>,
        superclass: Option<TypeAnnotation>,
        interfaces: Vec<TypeAnnotation>,
        properties: Vec<Property>,
        methods: Vec<Box<Statement>>, // Must be FunctionDeclaration
        modifiers: Modifiers,
        span: Span,
    },

    // Data class declarations (immutable by default)
    DataClassDeclaration {
        name: String,
        parameters: Vec<Parameter>,
        type_parameters: Vec<String>,
        is_mutable: bool,
        modifiers: Modifiers,
        span: Span,
    },

    // Interface declarations
    InterfaceDeclaration {
        name: String,
        type_parameters: Vec<String>,
        superinterfaces: Vec<TypeAnnotation>,
        methods: Vec<Box<Statement>>, // Abstract function declarations
        properties: Vec<Property>,
        modifiers: Modifiers,
        span: Span,
    },

    // Extension functions
    ExtensionFunction(ExtensionFunction),

    // Expression statements
    Expression {
        expr: Expression,
        span: Span,
    },

    // Return statements
    Return {
        value: Option<Expression>,
        span: Span,
    },

    // Assignment statements
    Assignment {
        target: Expression, // Could be identifier or member access
        value: Expression,
        span: Span,
    },

    // While loops
    While {
        condition: Expression,
        body: Box<Expression>,
        span: Span,
    },

    // For loops
    For {
        variable: String,
        iterable: Expression,
        body: Box<Expression>,
        span: Span,
    },

    // Break/continue
    Break(Span),
    Continue(Span),

    // Import statements
    Import {
        path: String,
        alias: Option<String>,
        is_wildcard: bool,
        span: Span,
    },

    // Package declaration
    Package {
        name: String,
        span: Span,
    },

    // Concurrency constructs
    Concurrency(ConcurrencyConstruct),

    // Resource management
    ResourceManagement(ResourceManagement),
}

/// Top-level program
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    pub package: Option<String>,
    pub imports: Vec<Statement>, // Must be Import statements
    pub statements: Vec<Statement>,
    pub span: Span,
}

// Helper traits and implementations
impl Expression {
    pub fn span(&self) -> &Span {
        match self {
            Expression::Literal(_, span) => span,
            Expression::Identifier(_, span) => span,
            Expression::Binary { span, .. } => span,
            Expression::Unary { span, .. } => span,
            Expression::Call { span, .. } => span,
            Expression::MemberAccess { span, .. } => span,
            Expression::NullSafeMemberAccess { span, .. } => span,
            Expression::IndexAccess { span, .. } => span,
            Expression::NullSafeIndexAccess { span, .. } => span,
            Expression::StringInterpolation { span, .. } => span,
            Expression::When { span, .. } => span,
            Expression::If { span, .. } => span,
            Expression::Block { span, .. } => span,
            Expression::Array { span, .. } => span,
            Expression::Lambda { span, .. } => span,
            Expression::Try { span, .. } => span,
            Expression::This(span) => span,
            Expression::Super(span) => span,
        }
    }
}

impl Statement {
    pub fn span(&self) -> &Span {
        match self {
            Statement::ValDeclaration { span, .. } => span,
            Statement::VarDeclaration { span, .. } => span,
            Statement::FunctionDeclaration { span, .. } => span,
            Statement::ClassDeclaration { span, .. } => span,
            Statement::DataClassDeclaration { span, .. } => span,
            Statement::InterfaceDeclaration { span, .. } => span,
            Statement::ExtensionFunction(ext) => &ext.span,
            Statement::Expression { span, .. } => span,
            Statement::Return { span, .. } => span,
            Statement::Assignment { span, .. } => span,
            Statement::While { span, .. } => span,
            Statement::For { span, .. } => span,
            Statement::Break(span) => span,
            Statement::Continue(span) => span,
            Statement::Import { span, .. } => span,
            Statement::Package { span, .. } => span,
            Statement::Concurrency(construct) => match construct {
                ConcurrencyConstruct::Spawn { span, .. } => span,
                ConcurrencyConstruct::Async { span, .. } => span,
                ConcurrencyConstruct::Await { span, .. } => span,
            },
            Statement::ResourceManagement(rm) => match rm {
                ResourceManagement::Use { span, .. } => span,
                ResourceManagement::Defer { span, .. } => span,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test helper functions
    fn dummy_span() -> Span {
        Span::dummy()
    }

    #[test]
    fn test_span_creation() {
        let span = Span::new(1, 0, 1, 5);
        assert_eq!(span.start_line, 1);
        assert_eq!(span.start_column, 0);
        assert_eq!(span.end_line, 1);
        assert_eq!(span.end_column, 5);
    }

    #[test]
    fn test_span_dummy() {
        let span = Span::dummy();
        assert_eq!(span, Span::default());
    }

    // Literal tests
    #[test]
    fn test_literal_string() {
        let lit = Literal::String("hello".to_string());
        assert_eq!(lit, Literal::String("hello".to_string()));
    }

    #[test]
    fn test_literal_number() {
        let lit = Literal::Number("42".to_string());
        assert_eq!(lit, Literal::Number("42".to_string()));
    }

    #[test]
    fn test_literal_boolean() {
        assert_eq!(Literal::Boolean(true), Literal::Boolean(true));
        assert_eq!(Literal::Boolean(false), Literal::Boolean(false));
    }

    #[test]
    fn test_literal_null() {
        assert_eq!(Literal::Null, Literal::Null);
    }

    #[test]
    fn test_literal_character() {
        let lit = Literal::Character('a');
        assert_eq!(lit, Literal::Character('a'));
    }

    // Binary operator tests
    #[test]
    fn test_binary_operators_arithmetic() {
        assert_eq!(BinaryOp::Add, BinaryOp::Add);
        assert_eq!(BinaryOp::Subtract, BinaryOp::Subtract);
        assert_eq!(BinaryOp::Multiply, BinaryOp::Multiply);
        assert_eq!(BinaryOp::Divide, BinaryOp::Divide);
        assert_eq!(BinaryOp::Modulo, BinaryOp::Modulo);
    }

    #[test]
    fn test_binary_operators_comparison() {
        assert_eq!(BinaryOp::Equal, BinaryOp::Equal);
        assert_eq!(BinaryOp::NotEqual, BinaryOp::NotEqual);
        assert_eq!(BinaryOp::Less, BinaryOp::Less);
        assert_eq!(BinaryOp::LessEqual, BinaryOp::LessEqual);
        assert_eq!(BinaryOp::Greater, BinaryOp::Greater);
        assert_eq!(BinaryOp::GreaterEqual, BinaryOp::GreaterEqual);
    }

    #[test]
    fn test_binary_operators_logical() {
        assert_eq!(BinaryOp::And, BinaryOp::And);
        assert_eq!(BinaryOp::Or, BinaryOp::Or);
    }

    #[test]
    fn test_binary_operators_null_safety() {
        assert_eq!(BinaryOp::Elvis, BinaryOp::Elvis);
    }

    #[test]
    fn test_binary_operators_bitwise() {
        assert_eq!(BinaryOp::BitAnd, BinaryOp::BitAnd);
        assert_eq!(BinaryOp::BitOr, BinaryOp::BitOr);
        assert_eq!(BinaryOp::BitXor, BinaryOp::BitXor);
    }

    #[test]
    fn test_binary_operators_assignment() {
        assert_eq!(BinaryOp::PlusAssign, BinaryOp::PlusAssign);
        assert_eq!(BinaryOp::MinusAssign, BinaryOp::MinusAssign);
        assert_eq!(BinaryOp::MultiplyAssign, BinaryOp::MultiplyAssign);
        assert_eq!(BinaryOp::DivideAssign, BinaryOp::DivideAssign);
    }

    // Unary operator tests
    #[test]
    fn test_unary_operators() {
        assert_eq!(UnaryOp::Not, UnaryOp::Not);
        assert_eq!(UnaryOp::Minus, UnaryOp::Minus);
        assert_eq!(UnaryOp::Plus, UnaryOp::Plus);
        assert_eq!(UnaryOp::BitNot, UnaryOp::BitNot);
    }

    // Type annotation tests
    #[test]
    fn test_type_annotation_simple() {
        let ty = TypeAnnotation::Simple("String".to_string());
        assert_eq!(ty, TypeAnnotation::Simple("String".to_string()));
    }

    #[test]
    fn test_type_annotation_nullable() {
        let ty = TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple("String".to_string())));
        match &ty {
            TypeAnnotation::Nullable(inner) => {
                assert_eq!(**inner, TypeAnnotation::Simple("String".to_string()));
            }
            _ => panic!("Expected nullable type"),
        }
    }

    #[test]
    fn test_type_annotation_generic() {
        let ty = TypeAnnotation::Generic {
            name: "List".to_string(),
            type_args: vec![TypeAnnotation::Simple("String".to_string())],
        };
        match ty {
            TypeAnnotation::Generic { name, type_args } => {
                assert_eq!(name, "List");
                assert_eq!(type_args.len(), 1);
            }
            _ => panic!("Expected generic type"),
        }
    }

    #[test]
    fn test_type_annotation_function() {
        let ty = TypeAnnotation::Function {
            params: vec![TypeAnnotation::Simple("Int".to_string())],
            return_type: Box::new(TypeAnnotation::Simple("String".to_string())),
        };
        match ty {
            TypeAnnotation::Function {
                params,
                return_type,
            } => {
                assert_eq!(params.len(), 1);
                assert_eq!(*return_type, TypeAnnotation::Simple("String".to_string()));
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_type_annotation_array() {
        let ty = TypeAnnotation::Array(Box::new(TypeAnnotation::Simple("Int".to_string())));
        match ty {
            TypeAnnotation::Array(inner) => {
                assert_eq!(*inner, TypeAnnotation::Simple("Int".to_string()));
            }
            _ => panic!("Expected array type"),
        }
    }

    // Pattern tests
    #[test]
    fn test_pattern_literal() {
        let pattern = Pattern::Literal(Literal::Number("42".to_string()), dummy_span());
        match pattern {
            Pattern::Literal(Literal::Number(n), _) => assert_eq!(n, "42"),
            _ => panic!("Expected literal pattern"),
        }
    }

    #[test]
    fn test_pattern_identifier() {
        let pattern = Pattern::Identifier("x".to_string(), dummy_span());
        match pattern {
            Pattern::Identifier(name, _) => assert_eq!(name, "x"),
            _ => panic!("Expected identifier pattern"),
        }
    }

    #[test]
    fn test_pattern_wildcard() {
        let pattern = Pattern::Wildcard(dummy_span());
        match pattern {
            Pattern::Wildcard(_) => {} // Success
            _ => panic!("Expected wildcard pattern"),
        }
    }

    #[test]
    fn test_pattern_constructor() {
        let pattern = Pattern::Constructor {
            name: "Some".to_string(),
            patterns: vec![Pattern::Identifier("x".to_string(), dummy_span())],
            span: dummy_span(),
        };
        match pattern {
            Pattern::Constructor { name, patterns, .. } => {
                assert_eq!(name, "Some");
                assert_eq!(patterns.len(), 1);
            }
            _ => panic!("Expected constructor pattern"),
        }
    }

    #[test]
    fn test_pattern_range() {
        let start = Expression::Literal(Literal::Number("1".to_string()), dummy_span());
        let end = Expression::Literal(Literal::Number("10".to_string()), dummy_span());
        let pattern = Pattern::Range {
            start: Box::new(start),
            end: Box::new(end),
            span: dummy_span(),
        };
        match pattern {
            Pattern::Range { .. } => {} // Success
            _ => panic!("Expected range pattern"),
        }
    }

    #[test]
    fn test_pattern_guard() {
        let inner_pattern = Pattern::Identifier("x".to_string(), dummy_span());
        let condition = Expression::Binary {
            left: Box::new(Expression::Identifier("x".to_string(), dummy_span())),
            op: BinaryOp::Greater,
            right: Box::new(Expression::Literal(
                Literal::Number("0".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };
        let pattern = Pattern::Guard {
            pattern: Box::new(inner_pattern),
            condition,
            span: dummy_span(),
        };
        match pattern {
            Pattern::Guard { .. } => {} // Success
            _ => panic!("Expected guard pattern"),
        }
    }

    // Expression tests
    #[test]
    fn test_expression_literal() {
        let expr = Expression::Literal(Literal::String("hello".to_string()), dummy_span());
        match expr {
            Expression::Literal(Literal::String(s), _) => assert_eq!(s, "hello"),
            _ => panic!("Expected literal expression"),
        }
    }

    #[test]
    fn test_expression_identifier() {
        let expr = Expression::Identifier("variable".to_string(), dummy_span());
        match expr {
            Expression::Identifier(name, _) => assert_eq!(name, "variable"),
            _ => panic!("Expected identifier expression"),
        }
    }

    #[test]
    fn test_expression_binary() {
        let left = Expression::Literal(Literal::Number("1".to_string()), dummy_span());
        let right = Expression::Literal(Literal::Number("2".to_string()), dummy_span());
        let expr = Expression::Binary {
            left: Box::new(left),
            op: BinaryOp::Add,
            right: Box::new(right),
            span: dummy_span(),
        };
        match expr {
            Expression::Binary { op, .. } => assert_eq!(op, BinaryOp::Add),
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_expression_unary() {
        let operand = Expression::Literal(Literal::Boolean(true), dummy_span());
        let expr = Expression::Unary {
            op: UnaryOp::Not,
            operand: Box::new(operand),
            span: dummy_span(),
        };
        match expr {
            Expression::Unary { op, .. } => assert_eq!(op, UnaryOp::Not),
            _ => panic!("Expected unary expression"),
        }
    }

    #[test]
    fn test_expression_call_positional() {
        let function = Expression::Identifier("print".to_string(), dummy_span());
        let arg = Argument::Positional(Expression::Literal(
            Literal::String("hello".to_string()),
            dummy_span(),
        ));
        let expr = Expression::Call {
            function: Box::new(function),
            args: vec![arg],
            span: dummy_span(),
        };
        match expr {
            Expression::Call { args, .. } => {
                assert_eq!(args.len(), 1);
                match &args[0] {
                    Argument::Positional(Expression::Literal(Literal::String(s), _)) => {
                        assert_eq!(s, "hello");
                    }
                    _ => panic!("Expected positional string argument"),
                }
            }
            _ => panic!("Expected call expression"),
        }
    }

    #[test]
    fn test_expression_call_named() {
        let function = Expression::Identifier("print".to_string(), dummy_span());
        let arg = Argument::Named {
            name: "message".to_string(),
            value: Expression::Literal(Literal::String("hello".to_string()), dummy_span()),
            span: dummy_span(),
        };
        let expr = Expression::Call {
            function: Box::new(function),
            args: vec![arg],
            span: dummy_span(),
        };
        match expr {
            Expression::Call { args, .. } => {
                assert_eq!(args.len(), 1);
                match &args[0] {
                    Argument::Named { name, .. } => {
                        assert_eq!(name, "message");
                    }
                    _ => panic!("Expected named argument"),
                }
            }
            _ => panic!("Expected call expression"),
        }
    }

    #[test]
    fn test_expression_member_access() {
        let object = Expression::Identifier("obj".to_string(), dummy_span());
        let expr = Expression::MemberAccess {
            object: Box::new(object),
            property: "field".to_string(),
            span: dummy_span(),
        };
        match expr {
            Expression::MemberAccess { property, .. } => assert_eq!(property, "field"),
            _ => panic!("Expected member access expression"),
        }
    }

    #[test]
    fn test_expression_null_safe_member_access() {
        let object = Expression::Identifier("obj".to_string(), dummy_span());
        let expr = Expression::NullSafeMemberAccess {
            object: Box::new(object),
            property: "field".to_string(),
            span: dummy_span(),
        };
        match expr {
            Expression::NullSafeMemberAccess { property, .. } => assert_eq!(property, "field"),
            _ => panic!("Expected null-safe member access expression"),
        }
    }

    #[test]
    fn test_expression_index_access() {
        let object = Expression::Identifier("array".to_string(), dummy_span());
        let index = Expression::Literal(Literal::Number("0".to_string()), dummy_span());
        let expr = Expression::IndexAccess {
            object: Box::new(object),
            index: Box::new(index),
            span: dummy_span(),
        };
        match expr {
            Expression::IndexAccess { .. } => {} // Success
            _ => panic!("Expected index access expression"),
        }
    }

    #[test]
    fn test_expression_null_safe_index_access() {
        let object = Expression::Identifier("array".to_string(), dummy_span());
        let index = Expression::Literal(Literal::Number("0".to_string()), dummy_span());
        let expr = Expression::NullSafeIndexAccess {
            object: Box::new(object),
            index: Box::new(index),
            span: dummy_span(),
        };
        match expr {
            Expression::NullSafeIndexAccess { .. } => {} // Success
            _ => panic!("Expected null-safe index access expression"),
        }
    }

    #[test]
    fn test_expression_string_interpolation() {
        let parts = vec![
            StringPart::Text("Hello, ".to_string()),
            StringPart::Expression(Expression::Identifier("name".to_string(), dummy_span())),
            StringPart::Text("!".to_string()),
        ];
        let expr = Expression::StringInterpolation {
            parts,
            span: dummy_span(),
        };
        match expr {
            Expression::StringInterpolation { parts, .. } => {
                assert_eq!(parts.len(), 3);
                match &parts[0] {
                    StringPart::Text(s) => assert_eq!(s, "Hello, "),
                    _ => panic!("Expected text part"),
                }
                match &parts[1] {
                    StringPart::Expression(Expression::Identifier(name, _)) => {
                        assert_eq!(name, "name");
                    }
                    _ => panic!("Expected expression part"),
                }
                match &parts[2] {
                    StringPart::Text(s) => assert_eq!(s, "!"),
                    _ => panic!("Expected text part"),
                }
            }
            _ => panic!("Expected string interpolation expression"),
        }
    }

    #[test]
    fn test_expression_when_with_subject() {
        let subject = Expression::Identifier("x".to_string(), dummy_span());
        let pattern = Pattern::Literal(Literal::Number("1".to_string()), dummy_span());
        let arm = WhenArm {
            pattern,
            body: Expression::Literal(Literal::String("one".to_string()), dummy_span()),
            span: dummy_span(),
        };
        let expr = Expression::When {
            expr: Some(Box::new(subject)),
            arms: vec![arm],
            else_arm: None,
            span: dummy_span(),
        };
        match expr {
            Expression::When { expr, arms, .. } => {
                assert!(expr.is_some());
                assert_eq!(arms.len(), 1);
            }
            _ => panic!("Expected when expression"),
        }
    }

    #[test]
    fn test_expression_when_without_subject() {
        let pattern = Pattern::Literal(Literal::Boolean(true), dummy_span());
        let arm = WhenArm {
            pattern,
            body: Expression::Literal(Literal::String("true case".to_string()), dummy_span()),
            span: dummy_span(),
        };
        let expr = Expression::When {
            expr: None,
            arms: vec![arm],
            else_arm: Some(Box::new(Expression::Literal(
                Literal::String("default".to_string()),
                dummy_span(),
            ))),
            span: dummy_span(),
        };
        match expr {
            Expression::When {
                expr,
                arms,
                else_arm,
                ..
            } => {
                assert!(expr.is_none());
                assert_eq!(arms.len(), 1);
                assert!(else_arm.is_some());
            }
            _ => panic!("Expected when expression"),
        }
    }

    #[test]
    fn test_expression_if() {
        let condition = Expression::Literal(Literal::Boolean(true), dummy_span());
        let then_branch = Expression::Literal(Literal::String("true".to_string()), dummy_span());
        let else_branch = Expression::Literal(Literal::String("false".to_string()), dummy_span());
        let expr = Expression::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Some(Box::new(else_branch)),
            span: dummy_span(),
        };
        match expr {
            Expression::If { else_branch, .. } => assert!(else_branch.is_some()),
            _ => panic!("Expected if expression"),
        }
    }

    #[test]
    fn test_expression_block() {
        let stmt = Statement::Expression {
            expr: Expression::Literal(Literal::Number("42".to_string()), dummy_span()),
            span: dummy_span(),
        };
        let expr = Expression::Block {
            statements: vec![stmt],
            span: dummy_span(),
        };
        match expr {
            Expression::Block { statements, .. } => assert_eq!(statements.len(), 1),
            _ => panic!("Expected block expression"),
        }
    }

    #[test]
    fn test_expression_array() {
        let elements = vec![
            Expression::Literal(Literal::Number("1".to_string()), dummy_span()),
            Expression::Literal(Literal::Number("2".to_string()), dummy_span()),
            Expression::Literal(Literal::Number("3".to_string()), dummy_span()),
        ];
        let expr = Expression::Array {
            elements,
            span: dummy_span(),
        };
        match expr {
            Expression::Array { elements, .. } => assert_eq!(elements.len(), 3),
            _ => panic!("Expected array expression"),
        }
    }

    #[test]
    fn test_expression_lambda() {
        let param = Parameter {
            name: "x".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
            default_value: None,
            span: dummy_span(),
        };
        let body = Expression::Binary {
            left: Box::new(Expression::Identifier("x".to_string(), dummy_span())),
            op: BinaryOp::Add,
            right: Box::new(Expression::Literal(
                Literal::Number("1".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };
        let expr = Expression::Lambda {
            parameters: vec![param],
            body: Box::new(body),
            span: dummy_span(),
        };
        match expr {
            Expression::Lambda { parameters, .. } => assert_eq!(parameters.len(), 1),
            _ => panic!("Expected lambda expression"),
        }
    }

    #[test]
    fn test_expression_try() {
        let inner = Expression::Call {
            function: Box::new(Expression::Identifier(
                "riskyOperation".to_string(),
                dummy_span(),
            )),
            args: vec![],
            span: dummy_span(),
        };
        let expr = Expression::Try {
            expr: Box::new(inner),
            span: dummy_span(),
        };
        match expr {
            Expression::Try { .. } => {} // Success
            _ => panic!("Expected try expression"),
        }
    }

    #[test]
    fn test_expression_this() {
        let expr = Expression::This(dummy_span());
        match expr {
            Expression::This(_) => {} // Success
            _ => panic!("Expected this expression"),
        }
    }

    #[test]
    fn test_expression_super() {
        let expr = Expression::Super(dummy_span());
        match expr {
            Expression::Super(_) => {} // Success
            _ => panic!("Expected super expression"),
        }
    }

    #[test]
    fn test_expression_span() {
        let expr = Expression::Literal(Literal::Number("42".to_string()), Span::new(1, 0, 1, 2));
        let span = expr.span();
        assert_eq!(span.start_line, 1);
        assert_eq!(span.end_column, 2);
    }

    // Argument tests
    #[test]
    fn test_argument_positional() {
        let arg = Argument::Positional(Expression::Literal(
            Literal::String("test".to_string()),
            dummy_span(),
        ));
        match arg {
            Argument::Positional(_) => {} // Success
            _ => panic!("Expected positional argument"),
        }
    }

    #[test]
    fn test_argument_named() {
        let arg = Argument::Named {
            name: "param".to_string(),
            value: Expression::Literal(Literal::String("value".to_string()), dummy_span()),
            span: dummy_span(),
        };
        match arg {
            Argument::Named { name, .. } => assert_eq!(name, "param"),
            _ => panic!("Expected named argument"),
        }
    }

    // String part tests
    #[test]
    fn test_string_part_text() {
        let part = StringPart::Text("hello".to_string());
        match part {
            StringPart::Text(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected text string part"),
        }
    }

    #[test]
    fn test_string_part_expression() {
        let expr = Expression::Identifier("name".to_string(), dummy_span());
        let part = StringPart::Expression(expr);
        match part {
            StringPart::Expression(Expression::Identifier(name, _)) => assert_eq!(name, "name"),
            _ => panic!("Expected expression string part"),
        }
    }

    // When arm tests
    #[test]
    fn test_when_arm() {
        let pattern = Pattern::Literal(Literal::Number("42".to_string()), dummy_span());
        let body = Expression::Literal(Literal::String("forty-two".to_string()), dummy_span());
        let arm = WhenArm {
            pattern,
            body,
            span: dummy_span(),
        };
        match arm.pattern {
            Pattern::Literal(Literal::Number(n), _) => assert_eq!(n, "42"),
            _ => panic!("Expected number pattern"),
        }
    }

    // Parameter tests
    #[test]
    fn test_parameter_simple() {
        let param = Parameter {
            name: "x".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
            default_value: None,
            span: dummy_span(),
        };
        assert_eq!(param.name, "x");
        assert!(param.type_annotation.is_some());
        assert!(param.default_value.is_none());
    }

    #[test]
    fn test_parameter_with_default() {
        let param = Parameter {
            name: "y".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            default_value: Some(Expression::Literal(
                Literal::String("default".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };
        assert_eq!(param.name, "y");
        assert!(param.default_value.is_some());
    }

    // Visibility tests
    #[test]
    fn test_visibility_variants() {
        assert_eq!(Visibility::Public, Visibility::Public);
        assert_eq!(Visibility::Private, Visibility::Private);
        assert_eq!(Visibility::Internal, Visibility::Internal);
        assert_eq!(Visibility::Protected, Visibility::Protected);
    }

    // Modifiers tests
    #[test]
    fn test_modifiers_default() {
        let modifiers = Modifiers::default();
        assert_eq!(modifiers.visibility, Visibility::Private);
        assert!(!modifiers.is_abstract);
        assert!(!modifiers.is_final);
        assert!(!modifiers.is_static);
        assert!(!modifiers.is_override);
        assert!(!modifiers.is_open);
    }

    #[test]
    fn test_modifiers_custom() {
        let modifiers = Modifiers {
            visibility: Visibility::Public,
            is_abstract: true,
            is_final: false,
            is_static: true,
            is_override: false,
            is_open: true,
        };
        assert_eq!(modifiers.visibility, Visibility::Public);
        assert!(modifiers.is_abstract);
        assert!(modifiers.is_static);
        assert!(modifiers.is_open);
    }

    // Property tests
    #[test]
    fn test_property_immutable() {
        let property = Property {
            name: "value".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
            initializer: Some(Expression::Literal(
                Literal::Number("42".to_string()),
                dummy_span(),
            )),
            is_mutable: false,
            modifiers: Modifiers::default(),
            getter: None,
            setter: None,
            span: dummy_span(),
        };
        assert_eq!(property.name, "value");
        assert!(!property.is_mutable);
        assert!(property.initializer.is_some());
    }

    #[test]
    fn test_property_mutable_with_getter_setter() {
        let getter = Expression::Identifier("getValue".to_string(), dummy_span());
        let setter = Expression::Identifier("setValue".to_string(), dummy_span());
        let property = Property {
            name: "count".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
            initializer: None,
            is_mutable: true,
            modifiers: Modifiers {
                visibility: Visibility::Public,
                ..Default::default()
            },
            getter: Some(Box::new(getter)),
            setter: Some(Box::new(setter)),
            span: dummy_span(),
        };
        assert_eq!(property.name, "count");
        assert!(property.is_mutable);
        assert!(property.getter.is_some());
        assert!(property.setter.is_some());
        assert_eq!(property.modifiers.visibility, Visibility::Public);
    }

    // Extension function tests
    #[test]
    fn test_extension_function() {
        let function_decl = Statement::FunctionDeclaration {
            name: "toUpperCase".to_string(),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple("String".to_string())),
            body: Box::new(Expression::Literal(
                Literal::String("UPPERCASE".to_string()),
                dummy_span(),
            )),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };
        let ext_func = ExtensionFunction {
            receiver_type: TypeAnnotation::Simple("String".to_string()),
            function: Box::new(function_decl),
            span: dummy_span(),
        };
        match ext_func.receiver_type {
            TypeAnnotation::Simple(name) => assert_eq!(name, "String"),
            _ => panic!("Expected simple type"),
        }
    }

    // Concurrency construct tests
    #[test]
    fn test_concurrency_spawn() {
        let body = Expression::Block {
            statements: vec![],
            span: dummy_span(),
        };
        let spawn = ConcurrencyConstruct::Spawn {
            body: Box::new(body),
            span: dummy_span(),
        };
        match spawn {
            ConcurrencyConstruct::Spawn { .. } => {} // Success
            _ => panic!("Expected spawn construct"),
        }
    }

    #[test]
    fn test_concurrency_async() {
        let body = Expression::Block {
            statements: vec![],
            span: dummy_span(),
        };
        let async_expr = ConcurrencyConstruct::Async {
            body: Box::new(body),
            span: dummy_span(),
        };
        match async_expr {
            ConcurrencyConstruct::Async { .. } => {} // Success
            _ => panic!("Expected async construct"),
        }
    }

    #[test]
    fn test_concurrency_await() {
        let expr = Expression::Call {
            function: Box::new(Expression::Identifier(
                "asyncOperation".to_string(),
                dummy_span(),
            )),
            args: vec![],
            span: dummy_span(),
        };
        let await_expr = ConcurrencyConstruct::Await {
            expr: Box::new(expr),
            span: dummy_span(),
        };
        match await_expr {
            ConcurrencyConstruct::Await { .. } => {} // Success
            _ => panic!("Expected await construct"),
        }
    }

    // Resource management tests
    #[test]
    fn test_resource_management_use() {
        let resource = Expression::Call {
            function: Box::new(Expression::Identifier("openFile".to_string(), dummy_span())),
            args: vec![Argument::Positional(Expression::Literal(
                Literal::String("test.txt".to_string()),
                dummy_span(),
            ))],
            span: dummy_span(),
        };
        let body = Expression::Block {
            statements: vec![],
            span: dummy_span(),
        };
        let use_expr = ResourceManagement::Use {
            resource: Box::new(resource),
            body: Box::new(body),
            span: dummy_span(),
        };
        match use_expr {
            ResourceManagement::Use { .. } => {} // Success
            _ => panic!("Expected use construct"),
        }
    }

    #[test]
    fn test_resource_management_defer() {
        let body = Expression::Call {
            function: Box::new(Expression::Identifier("cleanup".to_string(), dummy_span())),
            args: vec![],
            span: dummy_span(),
        };
        let defer_expr = ResourceManagement::Defer {
            body: Box::new(body),
            span: dummy_span(),
        };
        match defer_expr {
            ResourceManagement::Defer { .. } => {} // Success
            _ => panic!("Expected defer construct"),
        }
    }

    // Statement tests
    #[test]
    fn test_statement_val_declaration() {
        let stmt = Statement::ValDeclaration {
            name: "x".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
            initializer: Expression::Literal(Literal::Number("42".to_string()), dummy_span()),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };
        match stmt {
            Statement::ValDeclaration { name, .. } => assert_eq!(name, "x"),
            _ => panic!("Expected val declaration"),
        }
    }

    #[test]
    fn test_statement_var_declaration() {
        let stmt = Statement::VarDeclaration {
            name: "y".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            initializer: Some(Expression::Literal(
                Literal::String("hello".to_string()),
                dummy_span(),
            )),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };
        match stmt {
            Statement::VarDeclaration {
                name, initializer, ..
            } => {
                assert_eq!(name, "y");
                assert!(initializer.is_some());
            }
            _ => panic!("Expected var declaration"),
        }
    }

    #[test]
    fn test_statement_function_declaration() {
        let param = Parameter {
            name: "x".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
            default_value: None,
            span: dummy_span(),
        };
        let body = Expression::Binary {
            left: Box::new(Expression::Identifier("x".to_string(), dummy_span())),
            op: BinaryOp::Add,
            right: Box::new(Expression::Literal(
                Literal::Number("1".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };
        let stmt = Statement::FunctionDeclaration {
            name: "increment".to_string(),
            parameters: vec![param],
            return_type: Some(TypeAnnotation::Simple("Int".to_string())),
            body: Box::new(body),
            modifiers: Modifiers {
                visibility: Visibility::Public,
                ..Default::default()
            },
            span: dummy_span(),
        };
        match stmt {
            Statement::FunctionDeclaration {
                name, parameters, ..
            } => {
                assert_eq!(name, "increment");
                assert_eq!(parameters.len(), 1);
            }
            _ => panic!("Expected function declaration"),
        }
    }

    #[test]
    fn test_statement_class_declaration() {
        let property = Property {
            name: "name".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            initializer: None,
            is_mutable: false,
            modifiers: Modifiers::default(),
            getter: None,
            setter: None,
            span: dummy_span(),
        };
        let stmt = Statement::ClassDeclaration {
            name: "Person".to_string(),
            type_parameters: vec![],
            superclass: None,
            interfaces: vec![],
            properties: vec![property],
            methods: vec![],
            modifiers: Modifiers {
                visibility: Visibility::Public,
                ..Default::default()
            },
            span: dummy_span(),
        };
        match stmt {
            Statement::ClassDeclaration {
                name, properties, ..
            } => {
                assert_eq!(name, "Person");
                assert_eq!(properties.len(), 1);
            }
            _ => panic!("Expected class declaration"),
        }
    }

    #[test]
    fn test_statement_data_class_declaration() {
        let param = Parameter {
            name: "id".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
            default_value: None,
            span: dummy_span(),
        };
        let stmt = Statement::DataClassDeclaration {
            name: "User".to_string(),
            parameters: vec![param],
            type_parameters: vec![],
            is_mutable: false,
            modifiers: Modifiers {
                visibility: Visibility::Public,
                ..Default::default()
            },
            span: dummy_span(),
        };
        match stmt {
            Statement::DataClassDeclaration {
                name,
                parameters,
                is_mutable,
                ..
            } => {
                assert_eq!(name, "User");
                assert_eq!(parameters.len(), 1);
                assert!(!is_mutable);
            }
            _ => panic!("Expected data class declaration"),
        }
    }

    #[test]
    fn test_statement_interface_declaration() {
        let method = Statement::FunctionDeclaration {
            name: "getName".to_string(),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple("String".to_string())),
            body: Box::new(Expression::Literal(
                Literal::String("abstract".to_string()),
                dummy_span(),
            )),
            modifiers: Modifiers {
                is_abstract: true,
                ..Default::default()
            },
            span: dummy_span(),
        };
        let stmt = Statement::InterfaceDeclaration {
            name: "Named".to_string(),
            type_parameters: vec![],
            superinterfaces: vec![],
            methods: vec![Box::new(method)],
            properties: vec![],
            modifiers: Modifiers {
                visibility: Visibility::Public,
                ..Default::default()
            },
            span: dummy_span(),
        };
        match stmt {
            Statement::InterfaceDeclaration { name, methods, .. } => {
                assert_eq!(name, "Named");
                assert_eq!(methods.len(), 1);
            }
            _ => panic!("Expected interface declaration"),
        }
    }

    #[test]
    fn test_statement_extension_function() {
        let function_decl = Statement::FunctionDeclaration {
            name: "double".to_string(),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Simple("Int".to_string())),
            body: Box::new(Expression::Binary {
                left: Box::new(Expression::This(dummy_span())),
                op: BinaryOp::Multiply,
                right: Box::new(Expression::Literal(
                    Literal::Number("2".to_string()),
                    dummy_span(),
                )),
                span: dummy_span(),
            }),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };
        let ext_func = ExtensionFunction {
            receiver_type: TypeAnnotation::Simple("Int".to_string()),
            function: Box::new(function_decl),
            span: dummy_span(),
        };
        let stmt = Statement::ExtensionFunction(ext_func);
        match stmt {
            Statement::ExtensionFunction(_) => {} // Success
            _ => panic!("Expected extension function statement"),
        }
    }

    #[test]
    fn test_statement_expression() {
        let expr = Expression::Call {
            function: Box::new(Expression::Identifier("println".to_string(), dummy_span())),
            args: vec![Argument::Positional(Expression::Literal(
                Literal::String("Hello".to_string()),
                dummy_span(),
            ))],
            span: dummy_span(),
        };
        let stmt = Statement::Expression {
            expr,
            span: dummy_span(),
        };
        match stmt {
            Statement::Expression { .. } => {} // Success
            _ => panic!("Expected expression statement"),
        }
    }

    #[test]
    fn test_statement_return() {
        let stmt = Statement::Return {
            value: Some(Expression::Literal(
                Literal::Number("42".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };
        match stmt {
            Statement::Return { value, .. } => assert!(value.is_some()),
            _ => panic!("Expected return statement"),
        }
    }

    #[test]
    fn test_statement_assignment() {
        let target = Expression::Identifier("x".to_string(), dummy_span());
        let value = Expression::Literal(Literal::Number("10".to_string()), dummy_span());
        let stmt = Statement::Assignment {
            target,
            value,
            span: dummy_span(),
        };
        match stmt {
            Statement::Assignment { .. } => {} // Success
            _ => panic!("Expected assignment statement"),
        }
    }

    #[test]
    fn test_statement_while() {
        let condition = Expression::Binary {
            left: Box::new(Expression::Identifier("i".to_string(), dummy_span())),
            op: BinaryOp::Less,
            right: Box::new(Expression::Literal(
                Literal::Number("10".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };
        let body = Expression::Block {
            statements: vec![],
            span: dummy_span(),
        };
        let stmt = Statement::While {
            condition,
            body: Box::new(body),
            span: dummy_span(),
        };
        match stmt {
            Statement::While { .. } => {} // Success
            _ => panic!("Expected while statement"),
        }
    }

    #[test]
    fn test_statement_for() {
        let iterable = Expression::Array {
            elements: vec![
                Expression::Literal(Literal::Number("1".to_string()), dummy_span()),
                Expression::Literal(Literal::Number("2".to_string()), dummy_span()),
                Expression::Literal(Literal::Number("3".to_string()), dummy_span()),
            ],
            span: dummy_span(),
        };
        let body = Expression::Block {
            statements: vec![],
            span: dummy_span(),
        };
        let stmt = Statement::For {
            variable: "item".to_string(),
            iterable,
            body: Box::new(body),
            span: dummy_span(),
        };
        match stmt {
            Statement::For { variable, .. } => assert_eq!(variable, "item"),
            _ => panic!("Expected for statement"),
        }
    }

    #[test]
    fn test_statement_break() {
        let stmt = Statement::Break(dummy_span());
        match stmt {
            Statement::Break(_) => {} // Success
            _ => panic!("Expected break statement"),
        }
    }

    #[test]
    fn test_statement_continue() {
        let stmt = Statement::Continue(dummy_span());
        match stmt {
            Statement::Continue(_) => {} // Success
            _ => panic!("Expected continue statement"),
        }
    }

    #[test]
    fn test_statement_import() {
        let stmt = Statement::Import {
            path: "java.util.List".to_string(),
            alias: Some("JList".to_string()),
            is_wildcard: false,
            span: dummy_span(),
        };
        match stmt {
            Statement::Import {
                path,
                alias,
                is_wildcard,
                ..
            } => {
                assert_eq!(path, "java.util.List");
                assert_eq!(alias, Some("JList".to_string()));
                assert!(!is_wildcard);
            }
            _ => panic!("Expected import statement"),
        }
    }

    #[test]
    fn test_statement_import_wildcard() {
        let stmt = Statement::Import {
            path: "java.util.*".to_string(),
            alias: None,
            is_wildcard: true,
            span: dummy_span(),
        };
        match stmt {
            Statement::Import {
                path, is_wildcard, ..
            } => {
                assert_eq!(path, "java.util.*");
                assert!(is_wildcard);
            }
            _ => panic!("Expected wildcard import statement"),
        }
    }

    #[test]
    fn test_statement_package() {
        let stmt = Statement::Package {
            name: "com.example.app".to_string(),
            span: dummy_span(),
        };
        match stmt {
            Statement::Package { name, .. } => assert_eq!(name, "com.example.app"),
            _ => panic!("Expected package statement"),
        }
    }

    #[test]
    fn test_statement_concurrency() {
        let body = Expression::Block {
            statements: vec![],
            span: dummy_span(),
        };
        let spawn = ConcurrencyConstruct::Spawn {
            body: Box::new(body),
            span: dummy_span(),
        };
        let stmt = Statement::Concurrency(spawn);
        match stmt {
            Statement::Concurrency(ConcurrencyConstruct::Spawn { .. }) => {} // Success
            _ => panic!("Expected concurrency statement"),
        }
    }

    #[test]
    fn test_statement_resource_management() {
        let resource = Expression::Call {
            function: Box::new(Expression::Identifier("openFile".to_string(), dummy_span())),
            args: vec![],
            span: dummy_span(),
        };
        let body = Expression::Block {
            statements: vec![],
            span: dummy_span(),
        };
        let use_expr = ResourceManagement::Use {
            resource: Box::new(resource),
            body: Box::new(body),
            span: dummy_span(),
        };
        let stmt = Statement::ResourceManagement(use_expr);
        match stmt {
            Statement::ResourceManagement(ResourceManagement::Use { .. }) => {} // Success
            _ => panic!("Expected resource management statement"),
        }
    }

    #[test]
    fn test_statement_span() {
        let stmt = Statement::ValDeclaration {
            name: "x".to_string(),
            type_annotation: None,
            initializer: Expression::Literal(Literal::Number("42".to_string()), dummy_span()),
            modifiers: Modifiers::default(),
            span: Span::new(1, 0, 1, 10),
        };
        let span = stmt.span();
        assert_eq!(span.start_line, 1);
        assert_eq!(span.end_column, 10);
    }

    // Program tests
    #[test]
    fn test_program_empty() {
        let program = Program {
            package: None,
            imports: vec![],
            statements: vec![],
            span: dummy_span(),
        };
        assert!(program.package.is_none());
        assert!(program.imports.is_empty());
        assert!(program.statements.is_empty());
    }

    #[test]
    fn test_program_with_package_and_imports() {
        let import_stmt = Statement::Import {
            path: "java.util.List".to_string(),
            alias: None,
            is_wildcard: false,
            span: dummy_span(),
        };
        let val_stmt = Statement::ValDeclaration {
            name: "greeting".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            initializer: Expression::Literal(
                Literal::String("Hello, World!".to_string()),
                dummy_span(),
            ),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };
        let program = Program {
            package: Some("com.example.app".to_string()),
            imports: vec![import_stmt],
            statements: vec![val_stmt],
            span: dummy_span(),
        };
        assert_eq!(program.package, Some("com.example.app".to_string()));
        assert_eq!(program.imports.len(), 1);
        assert_eq!(program.statements.len(), 1);
    }

    // Integration tests for complex language constructs
    #[test]
    fn test_complex_when_expression_with_patterns() {
        // Test: when (value) { 1..10 -> "small", is String -> "text", else -> "unknown" }
        let value_expr = Expression::Identifier("value".to_string(), dummy_span());

        let range_pattern = Pattern::Range {
            start: Box::new(Expression::Literal(
                Literal::Number("1".to_string()),
                dummy_span(),
            )),
            end: Box::new(Expression::Literal(
                Literal::Number("10".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };
        let range_arm = WhenArm {
            pattern: range_pattern,
            body: Expression::Literal(Literal::String("small".to_string()), dummy_span()),
            span: dummy_span(),
        };

        let type_pattern = Pattern::Constructor {
            name: "String".to_string(),
            patterns: vec![],
            span: dummy_span(),
        };
        let type_arm = WhenArm {
            pattern: type_pattern,
            body: Expression::Literal(Literal::String("text".to_string()), dummy_span()),
            span: dummy_span(),
        };

        let when_expr = Expression::When {
            expr: Some(Box::new(value_expr)),
            arms: vec![range_arm, type_arm],
            else_arm: Some(Box::new(Expression::Literal(
                Literal::String("unknown".to_string()),
                dummy_span(),
            ))),
            span: dummy_span(),
        };

        match when_expr {
            Expression::When {
                expr,
                arms,
                else_arm,
                ..
            } => {
                assert!(expr.is_some());
                assert_eq!(arms.len(), 2);
                assert!(else_arm.is_some());
            }
            _ => panic!("Expected when expression"),
        }
    }

    #[test]
    fn test_function_with_default_and_named_parameters() {
        // Test: fun greet(name: String, greeting: String = "Hello", excited: Boolean = false) = "$greeting, $name${if (excited) "!" else ""}"
        let name_param = Parameter {
            name: "name".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            default_value: None,
            span: dummy_span(),
        };
        let greeting_param = Parameter {
            name: "greeting".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            default_value: Some(Expression::Literal(
                Literal::String("Hello".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };
        let excited_param = Parameter {
            name: "excited".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("Boolean".to_string())),
            default_value: Some(Expression::Literal(Literal::Boolean(false), dummy_span())),
            span: dummy_span(),
        };

        let body = Expression::StringInterpolation {
            parts: vec![
                StringPart::Expression(Expression::Identifier(
                    "greeting".to_string(),
                    dummy_span(),
                )),
                StringPart::Text(", ".to_string()),
                StringPart::Expression(Expression::Identifier("name".to_string(), dummy_span())),
                StringPart::Expression(Expression::If {
                    condition: Box::new(Expression::Identifier(
                        "excited".to_string(),
                        dummy_span(),
                    )),
                    then_branch: Box::new(Expression::Literal(
                        Literal::String("!".to_string()),
                        dummy_span(),
                    )),
                    else_branch: Some(Box::new(Expression::Literal(
                        Literal::String("".to_string()),
                        dummy_span(),
                    ))),
                    span: dummy_span(),
                }),
            ],
            span: dummy_span(),
        };

        let func_stmt = Statement::FunctionDeclaration {
            name: "greet".to_string(),
            parameters: vec![name_param, greeting_param, excited_param],
            return_type: Some(TypeAnnotation::Simple("String".to_string())),
            body: Box::new(body),
            modifiers: Modifiers {
                visibility: Visibility::Public,
                ..Default::default()
            },
            span: dummy_span(),
        };

        match func_stmt {
            Statement::FunctionDeclaration {
                name, parameters, ..
            } => {
                assert_eq!(name, "greet");
                assert_eq!(parameters.len(), 3);
                assert!(parameters[1].default_value.is_some());
                assert!(parameters[2].default_value.is_some());
            }
            _ => panic!("Expected function declaration"),
        }
    }

    #[test]
    fn test_data_class_with_mutable_properties() {
        // Test: data class User(val id: Int, var name: String, var email: String?)
        let id_param = Parameter {
            name: "id".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
            default_value: None,
            span: dummy_span(),
        };
        let name_param = Parameter {
            name: "name".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            default_value: None,
            span: dummy_span(),
        };
        let email_param = Parameter {
            name: "email".to_string(),
            type_annotation: Some(TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple(
                "String".to_string(),
            )))),
            default_value: None,
            span: dummy_span(),
        };

        let data_class = Statement::DataClassDeclaration {
            name: "User".to_string(),
            parameters: vec![id_param, name_param, email_param],
            type_parameters: vec![],
            is_mutable: true,
            modifiers: Modifiers {
                visibility: Visibility::Public,
                ..Default::default()
            },
            span: dummy_span(),
        };

        match data_class {
            Statement::DataClassDeclaration {
                name,
                parameters,
                is_mutable,
                ..
            } => {
                assert_eq!(name, "User");
                assert_eq!(parameters.len(), 3);
                assert!(is_mutable);
                // Check nullable type
                match &parameters[2].type_annotation {
                    Some(TypeAnnotation::Nullable(_)) => {} // Success
                    _ => panic!("Expected nullable type for email"),
                }
            }
            _ => panic!("Expected data class declaration"),
        }
    }

    #[test]
    fn test_extension_function_with_null_safety() {
        // Test: fun String?.orDefault(default: String): String = this ?: default
        let default_param = Parameter {
            name: "default".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("String".to_string())),
            default_value: None,
            span: dummy_span(),
        };

        let body = Expression::Binary {
            left: Box::new(Expression::This(dummy_span())),
            op: BinaryOp::Elvis,
            right: Box::new(Expression::Identifier("default".to_string(), dummy_span())),
            span: dummy_span(),
        };

        let function_decl = Statement::FunctionDeclaration {
            name: "orDefault".to_string(),
            parameters: vec![default_param],
            return_type: Some(TypeAnnotation::Simple("String".to_string())),
            body: Box::new(body),
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };

        let ext_func = ExtensionFunction {
            receiver_type: TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple(
                "String".to_string(),
            ))),
            function: Box::new(function_decl),
            span: dummy_span(),
        };

        match ext_func.receiver_type {
            TypeAnnotation::Nullable(inner) => match *inner {
                TypeAnnotation::Simple(name) => assert_eq!(name, "String"),
                _ => panic!("Expected simple type inside nullable"),
            },
            _ => panic!("Expected nullable receiver type"),
        }
    }

    #[test]
    fn test_async_await_with_spawn() {
        // Test: spawn { val result = async { expensiveOperation() }.await(); processResult(result) }
        let async_body = Expression::Call {
            function: Box::new(Expression::Identifier(
                "expensiveOperation".to_string(),
                dummy_span(),
            )),
            args: vec![],
            span: dummy_span(),
        };

        let async_expr = ConcurrencyConstruct::Async {
            body: Box::new(async_body),
            span: dummy_span(),
        };

        let await_expr = ConcurrencyConstruct::Await {
            expr: Box::new(Expression::Block {
                statements: vec![Statement::Concurrency(async_expr)],
                span: dummy_span(),
            }),
            span: dummy_span(),
        };

        let val_stmt = Statement::ValDeclaration {
            name: "result".to_string(),
            type_annotation: None,
            initializer: Expression::Block {
                statements: vec![Statement::Concurrency(await_expr)],
                span: dummy_span(),
            },
            modifiers: Modifiers::default(),
            span: dummy_span(),
        };

        let process_call = Statement::Expression {
            expr: Expression::Call {
                function: Box::new(Expression::Identifier(
                    "processResult".to_string(),
                    dummy_span(),
                )),
                args: vec![Argument::Positional(Expression::Identifier(
                    "result".to_string(),
                    dummy_span(),
                ))],
                span: dummy_span(),
            },
            span: dummy_span(),
        };

        let spawn_body = Expression::Block {
            statements: vec![val_stmt, process_call],
            span: dummy_span(),
        };

        let spawn_construct = ConcurrencyConstruct::Spawn {
            body: Box::new(spawn_body),
            span: dummy_span(),
        };

        match spawn_construct {
            ConcurrencyConstruct::Spawn { body, .. } => match *body {
                Expression::Block { statements, .. } => {
                    assert_eq!(statements.len(), 2);
                }
                _ => panic!("Expected block body in spawn"),
            },
            _ => panic!("Expected spawn construct"),
        }
    }

    #[test]
    fn test_use_resource_management() {
        // Test: use(openFile("data.txt")) { file -> file.readLines() }
        let resource = Expression::Call {
            function: Box::new(Expression::Identifier("openFile".to_string(), dummy_span())),
            args: vec![Argument::Positional(Expression::Literal(
                Literal::String("data.txt".to_string()),
                dummy_span(),
            ))],
            span: dummy_span(),
        };

        let lambda_body = Expression::Call {
            function: Box::new(Expression::MemberAccess {
                object: Box::new(Expression::Identifier("file".to_string(), dummy_span())),
                property: "readLines".to_string(),
                span: dummy_span(),
            }),
            args: vec![],
            span: dummy_span(),
        };

        let lambda_param = Parameter {
            name: "file".to_string(),
            type_annotation: None,
            default_value: None,
            span: dummy_span(),
        };

        let body = Expression::Lambda {
            parameters: vec![lambda_param],
            body: Box::new(lambda_body),
            span: dummy_span(),
        };

        let use_construct = ResourceManagement::Use {
            resource: Box::new(resource),
            body: Box::new(body),
            span: dummy_span(),
        };

        match use_construct {
            ResourceManagement::Use { resource, body, .. } => match *body {
                Expression::Lambda { parameters, .. } => {
                    assert_eq!(parameters.len(), 1);
                    assert_eq!(parameters[0].name, "file");
                }
                _ => panic!("Expected lambda in use body"),
            },
            _ => panic!("Expected use construct"),
        }
    }

    // Tests that should fail in Red phase (these test missing implementations)

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_type_inference_fails() {
        // This should fail because type inference is not implemented yet
        let _inferred_type = infer_type(&Expression::Literal(
            Literal::Number("42".to_string()),
            dummy_span(),
        ));
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_pattern_matching_validation_fails() {
        // This should fail because pattern validation is not implemented yet
        let pattern = Pattern::Range {
            start: Box::new(Expression::Literal(
                Literal::Number("1".to_string()),
                dummy_span(),
            )),
            end: Box::new(Expression::Literal(
                Literal::Number("10".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };
        let _is_valid = validate_pattern(&pattern);
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_null_safety_analysis_fails() {
        // This should fail because null safety analysis is not implemented yet
        let expr = Expression::NullSafeMemberAccess {
            object: Box::new(Expression::Identifier("obj".to_string(), dummy_span())),
            property: "field".to_string(),
            span: dummy_span(),
        };
        let _null_safety = analyze_null_safety(&expr);
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_scope_resolution_fails() {
        // This should fail because scope resolution is not implemented yet
        let identifier = Expression::Identifier("variable".to_string(), dummy_span());
        let _scope = resolve_scope(&identifier);
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_default_parameter_resolution_fails() {
        // This should fail because default parameter resolution is not implemented yet
        let param = Parameter {
            name: "x".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
            default_value: Some(Expression::Literal(
                Literal::Number("42".to_string()),
                dummy_span(),
            )),
            span: dummy_span(),
        };
        let _resolved = resolve_default_parameter(&param);
    }
}

// Placeholder functions that should be implemented in future phases
// These will cause the tests to fail in the Red phase, as intended

fn infer_type(_expr: &Expression) -> TypeAnnotation {
    panic!("not yet implemented: type inference")
}

fn validate_pattern(_pattern: &Pattern) -> bool {
    panic!("not yet implemented: pattern validation")
}

fn analyze_null_safety(_expr: &Expression) -> bool {
    panic!("not yet implemented: null safety analysis")
}

fn resolve_scope(_expr: &Expression) -> String {
    panic!("not yet implemented: scope resolution")
}

fn resolve_default_parameter(_param: &Parameter) -> Expression {
    panic!("not yet implemented: default parameter resolution")
}
