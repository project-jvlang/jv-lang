use crate::error::CodeGenError;
use crate::generator::JavaCodeGenerator;
use jv_ir::IrExpression;

impl JavaCodeGenerator {
    /// 分割代入で生成されたフィールドアクセスをJavaレコードのアクセサ呼び出しへ変換する。
    pub(crate) fn try_render_destructure_component(
        &mut self,
        expr: &IrExpression,
    ) -> Result<Option<String>, CodeGenError> {
        match expr {
            IrExpression::FieldAccess {
                receiver,
                field_name,
                is_record_component,
                ..
            } if *is_record_component => {
                let receiver_code = self.generate_expression(receiver)?;
                let accessor = Self::normalize_component_accessor(field_name);
                Ok(Some(format!("{receiver_code}.{accessor}()")))
            }
            _ => Ok(None),
        }
    }

    /// 分割代入展開後の代入式をJavaレベルのコードへ変換する。
    pub(crate) fn try_render_destructure_expression(
        &mut self,
        expr: &IrExpression,
    ) -> Result<Option<String>, CodeGenError> {
        if let Some(component) = self.try_render_destructure_component(expr)? {
            return Ok(Some(component));
        }

        if let IrExpression::Assignment { target, value, .. } = expr {
            if let Some(component) = self.try_render_destructure_component(value)? {
                if let IrExpression::Identifier { name, .. } = target.as_ref() {
                    if self.is_mutable_capture(name) {
                        return Ok(Some(format!("{}.set({})", name, component)));
                    }
                }
                let lhs = self.generate_expression(target)?;
                return Ok(Some(format!("{lhs} = {component}")));
            }
        }

        Ok(None)
    }

    fn normalize_component_accessor(field_name: &str) -> String {
        let trimmed = field_name.trim();
        if trimmed.is_empty() {
            "_1".to_string()
        } else {
            trimmed.to_string()
        }
    }
}
