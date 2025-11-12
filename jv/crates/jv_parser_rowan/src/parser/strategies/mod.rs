mod assignment;
mod class;
mod control;
mod expression;
mod function;
mod import;
mod package;
mod resource;
mod test;
mod unit;
mod variable;

use crate::syntax::TokenKind;

use super::context::ParserContext;

/// ステートメント戦略インターフェース。
pub(crate) trait StatementStrategy: Sync {
    /// 識別名。
    fn name(&self) -> &'static str;
    /// 適用可能か判定する。
    fn matches(&self, ctx: &ParserContext<'_>, lookahead: TokenKind) -> bool;
    /// 解析を実行する。トークンを消費した場合は `true`。
    fn parse(&self, ctx: &mut ParserContext<'_>) -> bool;
}

type StrategyRef = &'static dyn StatementStrategy;

/// 戦略レジストリ。
pub(crate) fn registry() -> &'static [StrategyRef] {
    &STRATEGIES
}

static STRATEGIES: &[StrategyRef] = &[
    &package::PACKAGE_STRATEGY,
    &import::IMPORT_STRATEGY,
    &variable::VAL_STRATEGY,
    &variable::VAR_STRATEGY,
    &test::TEST_STRATEGY,
    &function::FUNCTION_STRATEGY,
    &class::CLASS_STRATEGY,
    &control::CONTROL_STRATEGY,
    &resource::RESOURCE_STRATEGY,
    &unit::UNIT_TYPE_DEF_STRATEGY,
    &assignment::ASSIGNMENT_STRATEGY,
    &expression::EXPRESSION_STRATEGY,
];
