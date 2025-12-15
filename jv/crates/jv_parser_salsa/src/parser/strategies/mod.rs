use crate::lexer::TokenKind;

use super::{ParserContext, SyntaxKind};

mod binding;
mod class;
mod concurrent;
mod control;
mod function;
mod import;
mod jump;
mod log;
mod loops;
mod package;
mod resource;
mod test;
mod unit;
mod when;

pub use binding::{AssignmentStrategy, ValStrategy, VarStrategy};
pub use class::{ClassStrategy, DataClassStrategy};
pub use concurrent::{AsyncStrategy, SpawnStrategy};
pub use control::IfStrategy;
pub use function::FunctionStrategy;
pub use import::ImportStrategy;
pub use jump::{BreakStrategy, ContinueStrategy, ReturnStrategy};
pub use log::LogStrategy;
pub use loops::{ForStrategy, WhileStrategy};
pub use package::PackageStrategy;
pub use resource::{DeferStrategy, UseStrategy};
pub use test::TestStrategy;
pub use unit::UnitTypeDefStrategy;
pub use when::WhenStrategy;
pub(crate) use when::parse_when_statement;

/// ステートメント戦略トレイト。
pub trait StatementStrategy: Sync {
    fn name(&self) -> &'static str;
    fn matches(&self, ctx: &ParserContext, lookahead: TokenKind) -> bool;
    fn parse(&self, ctx: &mut ParserContext) -> bool;
}

/// レジストリ順序。design.md の並びに従う。
pub fn registry() -> Vec<&'static dyn StatementStrategy> {
    vec![
        &PackageStrategy,
        &ImportStrategy,
        &ValStrategy,
        &VarStrategy,
        &TestStrategy,
        &FunctionStrategy,
        &ClassStrategy,
        &DataClassStrategy,
        &IfStrategy,
        &WhenStrategy,
        &ForStrategy,
        &WhileStrategy,
        &ReturnStrategy,
        &BreakStrategy,
        &ContinueStrategy,
        &LogStrategy,
        &UseStrategy,
        &DeferStrategy,
        &SpawnStrategy,
        &AsyncStrategy,
        &UnitTypeDefStrategy,
        &AssignmentStrategy,
        &ExpressionStrategy,
    ]
}

/// 最終フォールバックの式戦略。
pub struct ExpressionStrategy;

impl StatementStrategy for ExpressionStrategy {
    fn name(&self) -> &'static str {
        "expression"
    }

    fn matches(&self, _ctx: &ParserContext, _lookahead: TokenKind) -> bool {
        true
    }

    fn parse(&self, ctx: &mut ParserContext) -> bool {
        ctx.parse_expression()
    }
}
