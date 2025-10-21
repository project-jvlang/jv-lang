//! Rowan 構文木で使用するシンボル定義と文法スケルトン。

mod kinds;

pub use kinds::{SyntaxKind, TokenKind};

/// package/import/val/var を中心とした文法スケルトン。
///
/// 今後 `ungrammar` や同等 DSL から自動生成することを想定しており、
/// 初期段階では設計の指針となる文字列表現として保持する。
/// `AsKw` トークンは import alias 支援のためのプレースホルダーで、Lexer への導入は後続タスクで行う。
pub const STATEMENT_GRAMMAR: &str = r#"
Root              = PackageDecl? ImportDecl* Statement*
PackageDecl       = AnnotationList? PackageKw QualifiedName
ImportDecl        = AnnotationList? ImportKw QualifiedName ImportClause?
ImportClause      = ImportWildcard | ImportAlias
ImportWildcard    = Dot Star
ImportAlias       = AsKw Identifier
Statement         = Declaration | ControlStatement
Declaration       = ValDecl | VarDecl | FunDecl | ClassDecl
ValDecl           = AnnotationList? ModifierList? ValKw BindingPattern TypeClause? InitializerClause?
VarDecl           = AnnotationList? ModifierList? VarKw BindingPattern TypeClause? InitializerClause?
BindingPattern    = Identifier
TypeClause        = Colon TypeAnnotation
InitializerClause = Assign Expression
AnnotationList    = Annotation+
Annotation        = At QualifiedName AnnotationArgumentList?
AnnotationArgumentList = LeftParen (AnnotationArgument (Comma AnnotationArgument)*)? RightParen
AnnotationArgument = Identifier Assign Expression | Expression
ModifierList      = Modifier+
Modifier          = Identifier
FunDecl           = AnnotationList? ModifierList? FunKw Identifier TypeParameterList? FunctionParameterList ReturnTypeClause? WhereClause? FunctionBody
TypeParameterList = Less (TypeParameter (Comma TypeParameter)*)? Greater
TypeParameter     = Identifier TypeBounds?
WhereClause       = WhereKw WherePredicate (Comma WherePredicate)*
WherePredicate    = Identifier Colon TypeAnnotation
FunctionParameterList = LeftParen (FunctionParameter (Comma FunctionParameter)*)? RightParen
FunctionParameter = ParameterModifierList? BindingPattern TypeClause?
ReturnTypeClause  = Colon TypeAnnotation
FunctionBody      = Block | FatArrow Expression
ClassDecl         = AnnotationList? ModifierList? (ClassKw | DataKw) Identifier TypeParameterList? ClassBody?
ClassBody         = LeftBrace Statement* RightBrace
Block             = LeftBrace Statement* RightBrace
ControlStatement  = IfStmt | WhenStmt | ForStmt | WhileStmt | DoWhileStmt | ReturnStmt | BreakStmt | ContinueStmt | ThrowStmt
IfStmt            = IfKw Expression Block ElseClause?
ElseClause        = ElseKw (Block | IfStmt)
WhenStmt          = WhenKw Expression LeftBrace WhenBranch* RightBrace
WhenBranch        = (Expression | ElseKw) FatArrow Statement
ForStmt           = ForKw LeftParen BindingPattern InKw Expression RightParen Block
WhileStmt         = WhileKw Expression Block
DoWhileStmt       = DoKw Block WhileKw Expression
ReturnStmt        = ReturnKw Expression?
BreakStmt         = BreakKw
ContinueStmt      = ContinueKw
ThrowStmt         = ThrowKw Expression
"#;
