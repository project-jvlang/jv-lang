//! Arena上のASTからIRへ変換する薄いアダプタ。

use super::builder::AstRoot;
use jv_ir::{
    IrProgram, PerfMetrics,
    prelude::{TransformError, TransformProfiler, transform_program, transform_program_profiled},
};

/// ASTを所有権付きに変換してIRへ落とす。
pub fn lower_to_ir(root: &AstRoot<'_>) -> Result<IrProgram, TransformError> {
    transform_program(root.to_owned())
}

/// パフォーマンス計測付きでIRへ変換する。
pub fn lower_to_ir_profiled(
    root: &AstRoot<'_>,
    profiler: &mut TransformProfiler,
) -> Result<(IrProgram, PerfMetrics), TransformError> {
    transform_program_profiled(root.to_owned(), profiler)
}
