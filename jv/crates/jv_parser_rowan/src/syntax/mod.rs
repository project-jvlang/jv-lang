//! Rowan 構文木で使用するシンボル定義と文法スケルトン。

mod kinds;

pub use kinds::{SyntaxKind, TokenKind};

/// package/import/val/var に対応した文法スケルトン。
///
/// 今後 `ungrammar` や同等 DSL から自動生成することを想定しており、
/// 初期段階では文字列として定義しておく。
pub const STATEMENT_GRAMMAR: &str = r#"
Root            = PackageDecl? ImportDecl* Statement*
PackageDecl     = PackageKw QualifiedName
ImportDecl      = ImportKw QualifiedName
Statement       = ValDecl | VarDecl | FunDecl | ClassDecl | IfStmt | WhenStmt | ForStmt | WhileStmt | DoWhileStmt | ReturnStmt | BreakStmt | ContinueStmt | ThrowStmt
ValDecl         = ValKw BindingPattern TypeClause? InitializerClause
VarDecl         = VarKw BindingPattern TypeClause? InitializerClause
BindingPattern  = Identifier
TypeClause      = Colon TypeAnnotation
InitializerClause = Assign Expression
QualifiedName   = Identifier (Dot Identifier)*
FunDecl         = FunKw Identifier ParameterList ReturnTypeClause? FunctionBody
ParameterList   = LeftParen (Parameter (Comma Parameter)*)? RightParen
Parameter       = Identifier TypeClause?
ReturnTypeClause = Colon TypeAnnotation
FunctionBody    = Block | FatArrow Expression
ClassDecl       = (ClassKw | DataKw) Identifier ClassBody?
ClassBody       = LeftBrace Statement* RightBrace
Block           = LeftBrace Statement* RightBrace
IfStmt          = IfKw Expression Block ElseClause?
ElseClause      = ElseKw (Block | IfStmt)
WhenStmt        = WhenKw Expression LeftBrace WhenBranch* RightBrace
WhenBranch      = (Expression | ElseKw) FatArrow Statement
ForStmt         = ForKw LeftParen Identifier InKw Expression RightParen Block
WhileStmt       = WhileKw Expression Block
DoWhileStmt     = DoKw Block WhileKw Expression
ReturnStmt      = ReturnKw Expression?
BreakStmt       = BreakKw
ContinueStmt    = ContinueKw
ThrowStmt       = ThrowKw Expression
"#;
