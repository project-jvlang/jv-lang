//! Salsa ベースのパーサーパイプライン基盤。
//!
//! 本クレートはインクリメンタルな salsa DB を中心に、Lexer/Parser/Lower/Solver
//! 各フェーズをモジュール単位で実装する。

pub mod constraints;
pub mod db;
pub mod lexer;
pub mod lower;
pub mod parser;
pub mod pipeline;
pub mod solver;

pub use db::{CrateId, Database, FileId, ParserJar, SalsaDatabase, source_text};
