use super::*;
use csv::StringRecord;
use jv_ast::Span;
use jv_ir::{DataFormat, PrimitiveType, SampleMode, SampleRecordDescriptor, Schema};
use serde_json::{Map, Number, Value};
use std::collections::{BTreeMap, HashMap};
use std::io::Cursor;
use std::str::FromStr;

impl JavaCodeGenerator {
    pub(super) fn generate_sample_declaration_artifacts(
        &mut self,
        declaration: &IrSampleDeclaration,
    ) -> Result<Vec<String>, CodeGenError> {
        let mut artifacts = self.generate_sample_declaration_records(declaration)?;

        if declaration.mode == SampleMode::Embed {
            let helper = self.generate_sample_embed_helper(declaration)?;
            artifacts.push(helper);
        }

        Ok(artifacts)
    }

    fn generate_sample_declaration_records(
        &mut self,
        declaration: &IrSampleDeclaration,
    ) -> Result<Vec<String>, CodeGenError> {
        let mut records = Vec::new();

        for descriptor in &declaration.records {
            let components = descriptor
                .fields
                .iter()
                .map(|field| IrRecordComponent {
                    name: field.name.clone(),
                    java_type: field.java_type.clone(),
                    span: declaration.span.clone(),
                })
                .collect::<Vec<_>>();

            let record_statement = IrStatement::RecordDeclaration {
                name: descriptor.name.clone(),
                type_parameters: Vec::new(),
                components,
                interfaces: Vec::new(),
                methods: Vec::new(),
                modifiers: IrModifiers {
                    visibility: IrVisibility::Public,
                    ..IrModifiers::default()
                },
                span: declaration.span.clone(),
            };

            records.push(self.generate_record(&record_statement)?);
        }

        Ok(records)
    }

    fn build_descriptor_lookup(
        records: &[SampleRecordDescriptor],
    ) -> HashMap<String, SampleRecordDescriptor> {
        records
            .iter()
            .map(|record| (record.name.clone(), record.clone()))
            .collect()
    }

    fn generate_sample_embed_helper(
        &mut self,
        declaration: &IrSampleDeclaration,
    ) -> Result<String, CodeGenError> {
        let data_bytes = declaration.embedded_data.as_ref().ok_or_else(|| {
            CodeGenError::UnsupportedConstruct {
                construct: "Embedモードでは埋め込みデータが必要です".to_string(),
                span: Some(declaration.span.clone()),
            }
        })?;

        let helper_name = self.embed_helper_class_name(declaration);
        let raw_field_name = match declaration.format {
            DataFormat::Json => "RAW_JSON".to_string(),
            DataFormat::Csv => "RAW_CSV".to_string(),
            DataFormat::Tsv => "RAW_TSV".to_string(),
        };
        let data_field_name = declaration.variable_name.to_ascii_uppercase();

        let descriptor_lookup = Self::build_descriptor_lookup(&declaration.records);

        let raw_string = String::from_utf8_lossy(data_bytes).to_string();
        let raw_field = IrStatement::FieldDeclaration {
            name: raw_field_name,
            java_type: JavaType::Reference {
                name: "String".to_string(),
                generic_args: Vec::new(),
            },
            initializer: Some(IrExpression::Literal(
                Literal::String(raw_string),
                declaration.span.clone(),
            )),
            modifiers: IrModifiers {
                visibility: IrVisibility::Private,
                is_static: true,
                is_final: true,
                ..IrModifiers::default()
            },
            span: declaration.span.clone(),
        };

        let data_initializer = match declaration.format {
            DataFormat::Json => {
                let json_value: Value = serde_json::from_slice(data_bytes).map_err(|error| {
                    CodeGenError::UnsupportedConstruct {
                        construct: format!("埋め込みデータのJSON解析に失敗しました: {}", error),
                        span: Some(declaration.span.clone()),
                    }
                })?;

                self.embed_json_value_expression(
                    &json_value,
                    &declaration.schema,
                    &declaration.java_type,
                    &descriptor_lookup,
                    &declaration.span,
                )?
            }
            DataFormat::Csv => {
                let json_value = self.parse_tabular_data(
                    data_bytes,
                    b',',
                    &declaration.schema,
                    &declaration.span,
                )?;
                self.embed_json_value_expression(
                    &json_value,
                    &declaration.schema,
                    &declaration.java_type,
                    &descriptor_lookup,
                    &declaration.span,
                )?
            }
            DataFormat::Tsv => {
                let json_value = self.parse_tabular_data(
                    data_bytes,
                    b'\t',
                    &declaration.schema,
                    &declaration.span,
                )?;
                self.embed_json_value_expression(
                    &json_value,
                    &declaration.schema,
                    &declaration.java_type,
                    &descriptor_lookup,
                    &declaration.span,
                )?
            }
        };

        let data_field = IrStatement::FieldDeclaration {
            name: data_field_name,
            java_type: declaration.java_type.clone(),
            initializer: Some(data_initializer),
            modifiers: IrModifiers {
                visibility: IrVisibility::Public,
                is_static: true,
                is_final: true,
                ..IrModifiers::default()
            },
            span: declaration.span.clone(),
        };

        let class_statement = IrStatement::ClassDeclaration {
            name: helper_name,
            type_parameters: Vec::new(),
            superclass: None,
            interfaces: Vec::new(),
            fields: vec![raw_field, data_field],
            methods: Vec::new(),
            nested_classes: Vec::new(),
            modifiers: IrModifiers {
                visibility: IrVisibility::Public,
                ..IrModifiers::default()
            },
            span: declaration.span.clone(),
        };

        self.generate_class(&class_statement)
    }

    fn parse_tabular_data(
        &self,
        data_bytes: &[u8],
        delimiter: u8,
        schema: &Schema,
        span: &Span,
    ) -> Result<Value, CodeGenError> {
        match schema {
            Schema::Array { element_type } => {
                let mut reader = csv::ReaderBuilder::new()
                    .has_headers(true)
                    .delimiter(delimiter)
                    .from_reader(Cursor::new(data_bytes));

                let headers = reader
                    .headers()
                    .map_err(|error| self.csv_error(error, span))?
                    .clone();

                let mut header_index = HashMap::new();
                for (index, header) in headers.iter().enumerate() {
                    header_index.insert(header.trim().to_string(), index);
                }

                let mut rows = Vec::new();
                for result in reader.records() {
                    let record = result.map_err(|error| self.csv_error(error, span))?;
                    let object =
                        self.record_to_json_map(&record, &header_index, element_type, span)?;
                    rows.push(Value::Object(object));
                }

                Ok(Value::Array(rows))
            }
            _ => Err(CodeGenError::UnsupportedConstruct {
                construct: "CSV/TSV埋め込みに対応していないスキーマです".to_string(),
                span: Some(span.clone()),
            }),
        }
    }

    fn record_to_json_map(
        &self,
        record: &StringRecord,
        header_index: &HashMap<String, usize>,
        element_schema: &Schema,
        span: &Span,
    ) -> Result<Map<String, Value>, CodeGenError> {
        let Schema::Object { fields, .. } = element_schema else {
            return Err(CodeGenError::UnsupportedConstruct {
                construct: "CSV/TSVではオブジェクト以外のレコード型は埋め込みできません"
                    .to_string(),
                span: Some(span.clone()),
            });
        };

        let mut object = Map::new();
        for (field_name, field_schema) in fields.iter() {
            let header_key = field_name.trim();
            let raw_value = header_index
                .get(header_key)
                .or_else(|| header_index.get(field_name))
                .and_then(|index| record.get(*index))
                .unwrap_or("");

            let parsed = self.convert_tabular_value(
                raw_value,
                field_schema,
                Some(field_name.as_str()),
                span,
            )?;
            object.insert(field_name.clone(), parsed);
        }

        Ok(object)
    }

    fn convert_tabular_value(
        &self,
        raw: &str,
        schema: &Schema,
        field_name: Option<&str>,
        span: &Span,
    ) -> Result<Value, CodeGenError> {
        let trimmed = raw.trim();
        match schema {
            Schema::Optional(inner) => {
                if trimmed.is_empty() {
                    Ok(Value::Null)
                } else {
                    self.convert_tabular_value(trimmed, inner, field_name, span)
                }
            }
            Schema::Union(variants) => {
                if trimmed.is_empty()
                    && variants
                        .iter()
                        .any(|variant| matches!(variant, Schema::Primitive(PrimitiveType::Null)))
                {
                    return Ok(Value::Null);
                }

                let mut last_error: Option<CodeGenError> = None;
                for variant in variants {
                    if matches!(variant, Schema::Primitive(PrimitiveType::Null)) {
                        continue;
                    }
                    match self.convert_tabular_value(trimmed, variant, field_name, span) {
                        Ok(value) => return Ok(value),
                        Err(error) => last_error = Some(error),
                    }
                }

                Err(
                    last_error.unwrap_or_else(|| CodeGenError::UnsupportedConstruct {
                        construct: format!(
                            "{}の値 '{}' を期待される型として解析できません",
                            Self::column_label(field_name),
                            trimmed
                        ),
                        span: Some(span.clone()),
                    }),
                )
            }
            Schema::Primitive(primitive) => {
                self.convert_tabular_primitive(trimmed, primitive, field_name, span)
            }
            Schema::Object { .. } => Err(CodeGenError::UnsupportedConstruct {
                construct: format!(
                    "{}にネストしたオブジェクト型はCSV/TSV埋め込みではサポートされません",
                    Self::column_label(field_name)
                ),
                span: Some(span.clone()),
            }),
            Schema::Array { .. } => Err(CodeGenError::UnsupportedConstruct {
                construct: format!(
                    "{}に配列型はCSV/TSV埋め込みではサポートされません",
                    Self::column_label(field_name)
                ),
                span: Some(span.clone()),
            }),
        }
    }

    fn convert_tabular_primitive(
        &self,
        raw: &str,
        primitive: &PrimitiveType,
        field_name: Option<&str>,
        span: &Span,
    ) -> Result<Value, CodeGenError> {
        if raw.is_empty() {
            return match primitive {
                PrimitiveType::Null => Ok(Value::Null),
                _ => Err(CodeGenError::UnsupportedConstruct {
                    construct: format!(
                        "{}に空の値は指定できません",
                        Self::column_label(field_name)
                    ),
                    span: Some(span.clone()),
                }),
            };
        }

        let column = Self::column_label(field_name);
        let normalized = raw.replace('_', "");

        match primitive {
            PrimitiveType::String => Ok(Value::String(raw.to_string())),
            PrimitiveType::Boolean => {
                let lower = raw.to_ascii_lowercase();
                match lower.as_str() {
                    "true" | "1" | "yes" => Ok(Value::Bool(true)),
                    "false" | "0" | "no" => Ok(Value::Bool(false)),
                    _ => Err(CodeGenError::UnsupportedConstruct {
                        construct: format!("{}の値 '{}' を真偽値として解析できません", column, raw),
                        span: Some(span.clone()),
                    }),
                }
            }
            PrimitiveType::Integer | PrimitiveType::Long => {
                let parsed: i64 =
                    normalized
                        .parse()
                        .map_err(|_| CodeGenError::UnsupportedConstruct {
                            construct: format!(
                                "{}の値 '{}' を整数として解析できません",
                                column, raw
                            ),
                            span: Some(span.clone()),
                        })?;
                Ok(Value::Number(parsed.into()))
            }
            PrimitiveType::BigInteger => {
                let number = Number::from_str(&normalized).map_err(|_| {
                    CodeGenError::UnsupportedConstruct {
                        construct: format!("{}の値 '{}' を整数として解析できません", column, raw),
                        span: Some(span.clone()),
                    }
                })?;
                Ok(Value::Number(number))
            }
            PrimitiveType::Double | PrimitiveType::BigDecimal => {
                let parsed: f64 =
                    normalized
                        .parse()
                        .map_err(|_| CodeGenError::UnsupportedConstruct {
                            construct: format!(
                                "{}の値 '{}' を小数として解析できません",
                                column, raw
                            ),
                            span: Some(span.clone()),
                        })?;
                let number =
                    Number::from_f64(parsed).ok_or_else(|| CodeGenError::UnsupportedConstruct {
                        construct: format!(
                            "{}の値 '{}' を有限小数として解析できません",
                            column, raw
                        ),
                        span: Some(span.clone()),
                    })?;
                Ok(Value::Number(number))
            }
            PrimitiveType::Null => {
                if raw.eq_ignore_ascii_case("null") {
                    Ok(Value::Null)
                } else {
                    Err(CodeGenError::UnsupportedConstruct {
                        construct: format!("{}の値 '{}' は null として解析できません", column, raw),
                        span: Some(span.clone()),
                    })
                }
            }
        }
    }

    fn column_label(field_name: Option<&str>) -> String {
        field_name
            .map(|name| format!("列 '{}'", name))
            .unwrap_or_else(|| "値".to_string())
    }

    fn csv_error(&self, error: csv::Error, span: &Span) -> CodeGenError {
        CodeGenError::UnsupportedConstruct {
            construct: format!("CSV/TSVデータの解析に失敗しました: {}", error),
            span: Some(span.clone()),
        }
    }

    fn embed_json_value_expression(
        &mut self,
        value: &Value,
        schema: &Schema,
        java_type: &JavaType,
        descriptors: &HashMap<String, SampleRecordDescriptor>,
        span: &Span,
    ) -> Result<IrExpression, CodeGenError> {
        match schema {
            Schema::Primitive(primitive) => {
                Ok(self.embed_primitive_expression(value, primitive, span)?)
            }
            Schema::Array { element_type } => {
                let JavaType::Reference { name, generic_args } = java_type else {
                    return Err(CodeGenError::UnsupportedConstruct {
                        construct: format!(
                            "配列スキーマに対応するJava型ではありません: {:?}",
                            java_type
                        ),
                        span: Some(span.clone()),
                    });
                };

                if name != "java.util.List" || generic_args.len() != 1 {
                    return Err(CodeGenError::UnsupportedConstruct {
                        construct: format!(
                            "List型への埋め込みに対応していない型です: {:?}",
                            java_type
                        ),
                        span: Some(span.clone()),
                    });
                }

                let Value::Array(elements) = value else {
                    return Err(CodeGenError::UnsupportedConstruct {
                        construct: "List型の埋め込みに配列以外のJSONが指定されました".to_string(),
                        span: Some(span.clone()),
                    });
                };

                let element_type_java = &generic_args[0];
                let mut args = Vec::new();
                for element in elements {
                    let expr = self.embed_json_value_expression(
                        element,
                        element_type,
                        element_type_java,
                        descriptors,
                        span,
                    )?;
                    args.push(expr);
                }

                Ok(IrExpression::MethodCall {
                    receiver: Some(Box::new(IrExpression::Identifier {
                        name: "java.util.List".to_string(),
                        java_type: java_type.clone(),
                        span: span.clone(),
                    })),
                    method_name: "of".to_string(),
                    args,
                    java_type: java_type.clone(),
                    span: span.clone(),
                })
            }
            Schema::Optional(inner) => {
                self.embed_optional_expression(value, inner, java_type, descriptors, span)
            }
            Schema::Object { fields, .. } => {
                self.embed_record_expression(value, fields, java_type, descriptors, span)
            }
            Schema::Union(variants) => {
                let has_null = variants
                    .iter()
                    .any(|variant| matches!(variant, Schema::Primitive(PrimitiveType::Null)));

                let non_null_variants: Vec<&Schema> = variants
                    .iter()
                    .filter(|variant| !matches!(variant, Schema::Primitive(PrimitiveType::Null)))
                    .collect();

                if has_null && Self::is_optional_type(java_type) {
                    let inner_schema = non_null_variants
                        .first()
                        .copied()
                        .unwrap_or(&Schema::Primitive(PrimitiveType::Null));
                    return self.embed_optional_expression(
                        value,
                        inner_schema,
                        java_type,
                        descriptors,
                        span,
                    );
                }

                if let Some(variant) = non_null_variants.first().copied() {
                    self.embed_json_value_expression(value, variant, java_type, descriptors, span)
                } else {
                    Err(CodeGenError::UnsupportedConstruct {
                        construct: "Unionスキーマの埋め込みに対応していません".to_string(),
                        span: Some(span.clone()),
                    })
                }
            }
        }
    }

    fn embed_optional_expression(
        &mut self,
        value: &Value,
        schema: &Schema,
        java_type: &JavaType,
        descriptors: &HashMap<String, SampleRecordDescriptor>,
        span: &Span,
    ) -> Result<IrExpression, CodeGenError> {
        let JavaType::Reference { name, generic_args } = java_type else {
            return Err(CodeGenError::UnsupportedConstruct {
                construct: format!("Optional型に対応しないJava型: {:?}", java_type),
                span: Some(span.clone()),
            });
        };

        if name != "java.util.Optional" || generic_args.len() != 1 {
            return Err(CodeGenError::UnsupportedConstruct {
                construct: format!("Optional型に対応しないJava型: {:?}", java_type),
                span: Some(span.clone()),
            });
        }

        if value.is_null() {
            return Ok(Self::optional_empty_expression(java_type, span));
        }

        let inner_java_type = &generic_args[0];
        let inner_expr =
            self.embed_json_value_expression(value, schema, inner_java_type, descriptors, span)?;

        Ok(IrExpression::MethodCall {
            receiver: Some(Box::new(IrExpression::Identifier {
                name: "java.util.Optional".to_string(),
                java_type: java_type.clone(),
                span: span.clone(),
            })),
            method_name: "ofNullable".to_string(),
            args: vec![inner_expr],
            java_type: java_type.clone(),
            span: span.clone(),
        })
    }

    fn embed_record_expression(
        &mut self,
        value: &Value,
        field_schemas: &BTreeMap<String, Schema>,
        java_type: &JavaType,
        descriptors: &HashMap<String, SampleRecordDescriptor>,
        span: &Span,
    ) -> Result<IrExpression, CodeGenError> {
        let Value::Object(map) = value else {
            return Err(CodeGenError::UnsupportedConstruct {
                construct: "レコード型にオブジェクト以外のJSONが割り当てられました".to_string(),
                span: Some(span.clone()),
            });
        };

        let JavaType::Reference { name, .. } = java_type else {
            return Err(CodeGenError::UnsupportedConstruct {
                construct: format!("レコード型に対応しないJava型: {:?}", java_type),
                span: Some(span.clone()),
            });
        };

        let descriptor =
            descriptors
                .get(name)
                .ok_or_else(|| CodeGenError::UnsupportedConstruct {
                    construct: format!("レコード記述子が見つかりません: {}", name),
                    span: Some(span.clone()),
                })?;

        let mut args = Vec::new();
        for field in &descriptor.fields {
            let field_schema = field_schemas
                .get(&field.name)
                .unwrap_or(&Schema::Primitive(PrimitiveType::Null));
            let field_value = map.get(&field.name).unwrap_or(&Value::Null);
            let expr = self.embed_json_value_expression(
                field_value,
                field_schema,
                &field.java_type,
                descriptors,
                span,
            )?;
            args.push(expr);
        }

        Ok(IrExpression::ObjectCreation {
            class_name: descriptor.name.clone(),
            generic_args: Vec::new(),
            args,
            java_type: JavaType::Reference {
                name: descriptor.name.clone(),
                generic_args: Vec::new(),
            },
            span: span.clone(),
        })
    }

    fn embed_primitive_expression(
        &self,
        value: &Value,
        primitive: &PrimitiveType,
        span: &Span,
    ) -> Result<IrExpression, CodeGenError> {
        match primitive {
            PrimitiveType::String => {
                let some = value
                    .as_str()
                    .ok_or_else(|| CodeGenError::UnsupportedConstruct {
                        construct: "文字列型に文字列以外が指定されました".to_string(),
                        span: Some(span.clone()),
                    })?;
                Ok(IrExpression::Literal(
                    Literal::String(some.to_string()),
                    span.clone(),
                ))
            }
            PrimitiveType::Boolean => {
                let some = value
                    .as_bool()
                    .ok_or_else(|| CodeGenError::UnsupportedConstruct {
                        construct: "boolean型に真偽値以外が指定されました".to_string(),
                        span: Some(span.clone()),
                    })?;
                Ok(IrExpression::Literal(Literal::Boolean(some), span.clone()))
            }
            PrimitiveType::Integer => {
                let some = value
                    .as_i64()
                    .ok_or_else(|| CodeGenError::UnsupportedConstruct {
                        construct: "int型に整数以外が指定されました".to_string(),
                        span: Some(span.clone()),
                    })?;
                Ok(IrExpression::Literal(
                    Literal::Number(some.to_string()),
                    span.clone(),
                ))
            }
            PrimitiveType::Long => {
                let some = value
                    .as_i64()
                    .ok_or_else(|| CodeGenError::UnsupportedConstruct {
                        construct: "long型に整数以外が指定されました".to_string(),
                        span: Some(span.clone()),
                    })?;
                Ok(IrExpression::Literal(
                    Literal::Number(format!("{}L", some)),
                    span.clone(),
                ))
            }
            PrimitiveType::Double => {
                let some = value
                    .as_f64()
                    .ok_or_else(|| CodeGenError::UnsupportedConstruct {
                        construct: "double型に数値以外が指定されました".to_string(),
                        span: Some(span.clone()),
                    })?;
                Ok(IrExpression::Literal(
                    Literal::Number(some.to_string()),
                    span.clone(),
                ))
            }
            PrimitiveType::BigInteger => {
                let number = value
                    .as_i64()
                    .map(|v| v.to_string())
                    .or_else(|| value.as_u64().map(|v| v.to_string()))
                    .or_else(|| value.as_f64().map(|v| v.trunc().to_string()))
                    .ok_or_else(|| CodeGenError::UnsupportedConstruct {
                        construct: "BigInteger型に数値以外が指定されました".to_string(),
                        span: Some(span.clone()),
                    })?;
                Ok(IrExpression::ObjectCreation {
                    class_name: "java.math.BigInteger".to_string(),
                    generic_args: Vec::new(),
                    args: vec![IrExpression::Literal(Literal::String(number), span.clone())],
                    java_type: JavaType::Reference {
                        name: "java.math.BigInteger".to_string(),
                        generic_args: Vec::new(),
                    },
                    span: span.clone(),
                })
            }
            PrimitiveType::BigDecimal => {
                let number = value
                    .as_f64()
                    .map(|v| v.to_string())
                    .or_else(|| value.as_i64().map(|v| v.to_string()))
                    .or_else(|| value.as_u64().map(|v| v.to_string()))
                    .ok_or_else(|| CodeGenError::UnsupportedConstruct {
                        construct: "BigDecimal型に数値以外が指定されました".to_string(),
                        span: Some(span.clone()),
                    })?;
                Ok(IrExpression::ObjectCreation {
                    class_name: "java.math.BigDecimal".to_string(),
                    generic_args: Vec::new(),
                    args: vec![IrExpression::Literal(Literal::String(number), span.clone())],
                    java_type: JavaType::Reference {
                        name: "java.math.BigDecimal".to_string(),
                        generic_args: Vec::new(),
                    },
                    span: span.clone(),
                })
            }
            PrimitiveType::Null => Ok(IrExpression::Literal(Literal::Null, span.clone())),
        }
    }

    fn optional_empty_expression(java_type: &JavaType, span: &Span) -> IrExpression {
        IrExpression::MethodCall {
            receiver: Some(Box::new(IrExpression::Identifier {
                name: "java.util.Optional".to_string(),
                java_type: java_type.clone(),
                span: span.clone(),
            })),
            method_name: "empty".to_string(),
            args: Vec::new(),
            java_type: java_type.clone(),
            span: span.clone(),
        }
    }

    fn is_optional_type(java_type: &JavaType) -> bool {
        matches!(
            java_type,
            JavaType::Reference {
                name,
                generic_args,
            } if name == "java.util.Optional" && generic_args.len() == 1
        )
    }

    fn embed_helper_class_name(&self, declaration: &IrSampleDeclaration) -> String {
        if let Some(root) = &declaration.root_record_name {
            return format!("{}Data", root);
        }

        let mut result = String::new();
        let mut uppercase_next = true;
        for ch in declaration.variable_name.chars() {
            if ch.is_ascii_alphanumeric() {
                if uppercase_next {
                    for upper in ch.to_uppercase() {
                        result.push(upper);
                    }
                    uppercase_next = false;
                } else {
                    for lower in ch.to_lowercase() {
                        result.push(lower);
                    }
                }
            } else {
                uppercase_next = true;
            }
        }

        if result.is_empty() {
            result.push_str("SampleData");
        } else if !result.ends_with("Data") {
            result.push_str("Data");
        }

        result
    }
}
