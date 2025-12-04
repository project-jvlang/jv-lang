//! Arena 上で `jv_ast` ノードを構築するためのヘルパ。
//!
//! - ソース全体の行頭オフセットを事前計算し、バイトスパンから `jv_ast::Span` へ変換する。
//! - ASTノードは `Arena` に割り当て、1パースで確保したメモリをまとめて再利用できる。
//! - IR 変換など外部コンシューマ向けには `AstRoot::to_owned()` で所有権を移譲する。

use crate::{
    allocator::{Arena, ArenaGuard},
    span::{Span, compute_line_starts, offset_to_location},
};
use jv_ast::statement::Program;

/// AST割り当てとSpan変換を管理するビルダー。
pub struct AstBuilder<'src, 'alloc> {
    arena_guard: ArenaGuard<'alloc>,
    source: &'src str,
    line_starts: Vec<u32>,
}

impl<'src, 'alloc> AstBuilder<'src, 'alloc> {
    /// 新しいビルダーを作成する。
    pub fn new(arena: &'alloc Arena, source: &'src str) -> Self {
        let guard = arena.guard();
        let line_starts = compute_line_starts(source);
        Self {
            arena_guard: guard,
            source,
            line_starts,
        }
    }

    /// アリーナガードを取得する（ノード確保用）。
    pub fn guard(&'alloc self) -> &ArenaGuard<'alloc> {
        &self.arena_guard
    }

    /// 入力ソース全体。
    pub fn source(&self) -> &'src str {
        self.source
    }

    /// バイトスパンを `jv_ast::Span` に変換する。
    ///
    /// jv_ast のスパンは0-originの行・桁で管理する。
    pub fn span(&self, span: Span) -> jv_ast::Span {
        let start = offset_to_location(&self.line_starts, span.start);
        let end = offset_to_location(&self.line_starts, span.end);
        jv_ast::Span {
            start_line: start.line.saturating_sub(1) as usize,
            start_column: start.column.saturating_sub(1) as usize,
            end_line: end.line.saturating_sub(1) as usize,
            end_column: end.column.saturating_sub(1) as usize,
        }
    }

    /// `jv_ast::Span` をバイトオフセットベースの `Span` に変換する。
    pub fn span_to_byte(&self, span: &jv_ast::Span) -> Span {
        let line_count = self.line_starts.len().saturating_sub(1);
        let start_line = span.start_line.min(line_count);
        let end_line = span.end_line.min(line_count);
        let start_offset = self
            .line_starts
            .get(start_line)
            .copied()
            .unwrap_or(0)
            .saturating_add(span.start_column as u32);
        let end_offset = self
            .line_starts
            .get(end_line)
            .copied()
            .unwrap_or(*self.line_starts.last().unwrap_or(&0))
            .saturating_add(span.end_column as u32);
        let src_len = self.source.len() as u32;
        Span::new(start_offset.min(src_len), end_offset.min(src_len))
    }

    /// AST全体をArenaに確保し、ルート参照を返す。
    pub fn alloc_program(&'alloc self, program: Program) -> AstRoot<'alloc> {
        let program = self.arena_guard.alloc(program);
        AstRoot { program }
    }
}

/// Arena上に確保されたASTのルート。
#[derive(Clone, Copy)]
pub struct AstRoot<'alloc> {
    program: &'alloc Program,
}

impl<'alloc> AstRoot<'alloc> {
    /// Arena上の参照を返す。
    pub fn program(&self) -> &Program {
        self.program
    }

    /// 既存コンシューマ向けに所有権付きASTを生成する。
    pub fn to_owned(&self) -> Program {
        self.program.clone()
    }
}
