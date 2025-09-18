use super::transform_expression;
use crate::context::TransformContext;
use crate::error::TransformError;
use crate::types::IrExpression;
use jv_ast::{Literal, Span, StringPart};

pub fn desugar_string_interpolation(
    parts: Vec<StringPart>,
    span: Span,
    context: &mut TransformContext,
) -> Result<IrExpression, TransformError> {
    let mut has_expression = false;
    let mut format_string = String::new();
    let mut raw_text = String::new();
    let mut args = Vec::new();

    for part in parts {
        match part {
            StringPart::Text(text) => {
                format_string.push_str(&escape_format_text(&text));
                raw_text.push_str(&text);
            }
            StringPart::Expression(expr) => {
                has_expression = true;
                format_string.push_str("%s");
                let ir_expr = transform_expression(expr, context)?;
                args.push(ir_expr);
            }
        }
    }

    if !has_expression {
        return Ok(IrExpression::Literal(Literal::String(raw_text), span));
    }

    Ok(IrExpression::StringFormat {
        format_string,
        args,
        span,
    })
}

fn escape_format_text(text: &str) -> String {
    text.replace('%', "%%")
}
