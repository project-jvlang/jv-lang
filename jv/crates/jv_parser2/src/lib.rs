//! jv_parser2: 新しいパーサー基盤クレート
//!
//! Phase1ではメモリアロケータ、Span/Token定義、ソースカーソル、
//! DSL向けフィーチャーフラグを提供する。

pub mod allocator;
pub mod ast;
pub mod context;
pub mod diagnostics;
pub mod feature_flags;
pub mod lexer;
pub mod parser;
pub mod plugins;
pub mod source;
pub mod span;
pub mod token;

pub use allocator::{Arena, ArenaGuard};
pub use ast::builder::{AstBuilder, AstRoot};
pub use ast::to_ir::{lower_to_ir, lower_to_ir_profiled};
pub use context::JvContext;
pub use diagnostics::{Diagnostic, DiagnosticKind};
pub use lexer::Lexer;
pub use parser::{ParseResult, Parser};
pub use plugins::PluginRegistry;
pub use source::Source;
pub use span::{SourceLocation, Span};
pub use token::{Token, TokenKind};
