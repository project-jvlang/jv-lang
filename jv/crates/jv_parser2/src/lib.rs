//! jv_parser2: 新しいパーサー基盤クレート
//!
//! Phase1ではメモリアロケータ、Span/Token定義、ソースカーソル、
//! DSL向けフィーチャーフラグを提供する。

pub mod allocator;
pub mod feature_flags;
pub mod lexer;
pub mod source;
pub mod span;
pub mod token;

pub use allocator::{Arena, ArenaGuard};
pub use lexer::Lexer;
pub use source::Source;
pub use span::{SourceLocation, Span};
pub use token::{Token, TokenKind};
