//! Salsa ベースのパーサーパイプライン基盤。
//!
//! 本クレートはインクリメンタルな salsa DB を中心に、Lexer/Parser/Lower/Solver
//! 各フェーズをモジュール単位で実装する。

pub mod constraints;
pub mod db;
pub mod diagnostics;
pub mod hir;
pub mod lexer;
pub mod lower;
pub mod parser;
pub mod pipeline;
pub mod solver;
pub mod support;

pub use db::{Database, FileInput, ParserDatabase, lower_to_hir, parse, solve_types};
pub use hir::HirFile;
pub use lexer::Span;
pub use parser::{OwnedToken, ParseResult};
pub use solver::TypeSolution;
