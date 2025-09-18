use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::{IrExpression, IrStatement, MethodOverload};
use jv_ast::{Argument, Expression, Modifiers, Parameter, Span, Statement, TypeAnnotation};

pub fn desugar_default_parameters(
    _function_name: String,
    _parameters: Vec<Parameter>,
    _return_type: Option<TypeAnnotation>,
    _body: Box<Expression>,
    _modifiers: Modifiers,
    _span: Span,
    _context: &mut TransformContext,
) -> Result<Vec<MethodOverload>, TransformError> {
    panic!("not yet implemented: desugar_default_parameters")
}

pub fn desugar_named_arguments(
    _function: Box<Expression>,
    _args: Vec<Argument>,
    _span: Span,
    _context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    panic!("not yet implemented: desugar_named_arguments")
}

pub fn desugar_top_level_function(
    _function: Statement,
    _context: &mut TransformContext,
) -> Result<IrStatement, TransformError> {
    panic!("not yet implemented: desugar_top_level_function")
}
