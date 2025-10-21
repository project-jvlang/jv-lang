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
Statement       = ValDecl | VarDecl
ValDecl         = ValKw BindingPattern TypeClause? InitializerClause
VarDecl         = VarKw BindingPattern TypeClause? InitializerClause
BindingPattern  = Identifier
TypeClause      = Colon TypeAnnotation
InitializerClause = Assign Expression
QualifiedName   = Identifier (Dot Identifier)*
"#;
