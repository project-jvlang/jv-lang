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
        } else if declaration.mode == SampleMode::Load {
            let helper = self.generate_sample_load_helper(declaration)?;
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

    fn generate_sample_load_helper(
        &mut self,
        declaration: &IrSampleDeclaration,
    ) -> Result<String, CodeGenError> {
        let descriptor_lookup = Self::build_descriptor_lookup(&declaration.records);
        let object_schema_map = self.build_object_schema_map(
            &declaration.schema,
            &declaration.java_type,
            &descriptor_lookup,
        );
        let mut list_types: Vec<ListTypeInfo> = Vec::new();
        Self::collect_list_types(
            &declaration.schema,
            &declaration.java_type,
            &descriptor_lookup,
            &mut list_types,
        );

        let mut generator = LoadHelperGenerator::new(
            self,
            declaration,
            descriptor_lookup,
            object_schema_map,
            list_types,
        )?;

        generator.build()
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
                    java_name: None,
                    resolved_target: None,
                    args,
                    argument_style: CallArgumentStyle::Comma,
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
            java_name: None,
            resolved_target: None,
            args: vec![inner_expr],
            argument_style: CallArgumentStyle::Comma,
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
            java_name: None,
            resolved_target: None,
            args: Vec::new(),
            argument_style: CallArgumentStyle::Comma,
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

    fn load_helper_class_name(&self, declaration: &IrSampleDeclaration) -> String {
        if let Some(root) = &declaration.root_record_name {
            return format!("{}Loader", root);
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
            result.push_str("Sample");
        } else if !result.ends_with("Loader") {
            result.push_str("Loader");
        }

        result
    }

    fn load_method_name(&self, declaration: &IrSampleDeclaration) -> String {
        let mut result = String::from("load");
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

        if result.len() <= 4 {
            result.push_str("Sample");
        }

        result
    }

    fn unwrap_optional_java_type<'a>(java_type: &'a JavaType) -> (&'a JavaType, bool) {
        match java_type {
            JavaType::Reference { name, generic_args }
                if name == "java.util.Optional" && generic_args.len() == 1 =>
            {
                (&generic_args[0], true)
            }
            _ => (java_type, false),
        }
    }

    fn list_element_type<'a>(java_type: &'a JavaType) -> Option<&'a JavaType> {
        match java_type {
            JavaType::Reference { name, generic_args }
                if name == "java.util.List" && generic_args.len() == 1 =>
            {
                Some(&generic_args[0])
            }
            _ => None,
        }
    }

    fn build_object_schema_map(
        &self,
        schema: &Schema,
        java_type: &JavaType,
        descriptors: &HashMap<String, SampleRecordDescriptor>,
    ) -> HashMap<String, ObjectSchemaInfo> {
        let mut map = HashMap::new();
        Self::collect_object_schema(schema, java_type, descriptors, &mut map);
        map
    }

    fn collect_object_schema(
        schema: &Schema,
        java_type: &JavaType,
        descriptors: &HashMap<String, SampleRecordDescriptor>,
        map: &mut HashMap<String, ObjectSchemaInfo>,
    ) {
        let (core_schema, _) = unwrap_schema_optional(schema);
        let (core_type, _) = Self::unwrap_optional_java_type(java_type);

        match (core_schema, core_type) {
            (
                Schema::Object {
                    fields,
                    required: _,
                },
                JavaType::Reference { name, .. },
            ) => {
                if !map.contains_key(name) {
                    map.insert(
                        name.clone(),
                        ObjectSchemaInfo {
                            fields: fields.clone(),
                        },
                    );

                    if let Some(descriptor) = descriptors.get(name) {
                        for field in &descriptor.fields {
                            if let Some(field_schema) = fields.get(&field.name) {
                                Self::collect_object_schema(
                                    field_schema,
                                    &field.java_type,
                                    descriptors,
                                    map,
                                );
                            }
                        }
                    }
                }
            }
            (Schema::Array { element_type }, _) => {
                if let Some(element_type_java) = Self::list_element_type(core_type) {
                    Self::collect_object_schema(
                        element_type.as_ref(),
                        element_type_java,
                        descriptors,
                        map,
                    );
                }
            }
            (Schema::Union(variants), _) => {
                for variant in variants {
                    if matches!(variant, Schema::Primitive(PrimitiveType::Null)) {
                        continue;
                    }
                    Self::collect_object_schema(variant, core_type, descriptors, map);
                }
            }
            (Schema::Optional(inner), _) => {
                Self::collect_object_schema(inner.as_ref(), core_type, descriptors, map);
            }
            _ => {}
        }
    }

    fn collect_list_types(
        schema: &Schema,
        java_type: &JavaType,
        descriptors: &HashMap<String, SampleRecordDescriptor>,
        results: &mut Vec<ListTypeInfo>,
    ) {
        let (core_schema, _) = unwrap_schema_optional(schema);
        let (core_type, _) = Self::unwrap_optional_java_type(java_type);

        match (core_schema, core_type.clone()) {
            (Schema::Array { element_type }, JavaType::Reference { name, generic_args })
                if name == "java.util.List" && generic_args.len() == 1 =>
            {
                let element_java_type = generic_args[0].clone();
                let element_schema_clone = element_type.as_ref().clone();
                if !results.iter().any(|info| info.list_type == *core_type) {
                    results.push(ListTypeInfo {
                        list_type: core_type.clone(),
                        element_type: element_java_type.clone(),
                        element_schema: element_schema_clone,
                    });
                }
                Self::collect_list_types(
                    element_type.as_ref(),
                    &element_java_type,
                    descriptors,
                    results,
                );
            }
            (Schema::Optional(inner), _) => {
                Self::collect_list_types(inner.as_ref(), core_type, descriptors, results);
            }
            (Schema::Union(variants), _) => {
                for variant in variants {
                    if matches!(variant, Schema::Primitive(PrimitiveType::Null)) {
                        continue;
                    }
                    Self::collect_list_types(variant, core_type, descriptors, results);
                }
            }
            (Schema::Object { fields, .. }, JavaType::Reference { name, .. }) => {
                if let Some(descriptor) = descriptors.get(&name) {
                    for field in &descriptor.fields {
                        if let Some(field_schema) = fields.get(&field.name) {
                            Self::collect_list_types(
                                field_schema,
                                &field.java_type,
                                descriptors,
                                results,
                            );
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

#[derive(Clone)]
struct ObjectSchemaInfo {
    fields: BTreeMap<String, Schema>,
}

#[derive(Clone)]
struct ListTypeInfo {
    list_type: JavaType,
    element_type: JavaType,
    element_schema: Schema,
}

struct LoadHelperGenerator<'a> {
    generator: &'a mut JavaCodeGenerator,
    declaration: &'a IrSampleDeclaration,
    descriptor_lookup: HashMap<String, SampleRecordDescriptor>,
    object_schema_map: HashMap<String, ObjectSchemaInfo>,
    list_types: Vec<ListTypeInfo>,
    builder: JavaSourceBuilder,
    helper_name: String,
    method_name: String,
    return_type: String,
}

impl<'a> LoadHelperGenerator<'a> {
    fn new(
        generator: &'a mut JavaCodeGenerator,
        declaration: &'a IrSampleDeclaration,
        descriptor_lookup: HashMap<String, SampleRecordDescriptor>,
        object_schema_map: HashMap<String, ObjectSchemaInfo>,
        list_types: Vec<ListTypeInfo>,
    ) -> Result<Self, CodeGenError> {
        let helper_name = generator.load_helper_class_name(declaration);
        let method_name = generator.load_method_name(declaration);
        let return_type = generator.generate_type(&declaration.java_type)?;
        let builder = generator.builder();

        Ok(Self {
            generator,
            declaration,
            descriptor_lookup,
            object_schema_map,
            list_types,
            builder,
            helper_name,
            method_name,
            return_type,
        })
    }

    fn build(&mut self) -> Result<String, CodeGenError> {
        self.ensure_imports();
        self.write_class_header();
        self.write_constants();
        self.write_constructor();
        self.write_loader_method();
        self.write_decode_methods()?;
        self.write_fetch_helpers();
        self.write_runtime_helpers();
        self.close_class();
        let output = std::mem::take(&mut self.builder).build();
        Ok(output)
    }

    fn ensure_imports(&mut self) {
        self.generator.add_import("java.io.ByteArrayOutputStream");
        self.generator.add_import("java.io.IOException");
        self.generator.add_import("java.io.InputStream");
        self.generator.add_import("java.net.URI");
        self.generator.add_import("java.net.URLDecoder");
        self.generator.add_import("java.net.http.HttpClient");
        self.generator.add_import("java.net.http.HttpRequest");
        self.generator.add_import("java.net.http.HttpResponse");
        self.generator
            .add_import("java.net.http.HttpResponse.BodyHandlers");
        self.generator
            .add_import("java.nio.charset.StandardCharsets");
        self.generator.add_import("java.nio.file.Files");
        self.generator.add_import("java.nio.file.Path");
        self.generator.add_import("java.nio.file.Paths");
        self.generator.add_import("java.security.MessageDigest");
        self.generator.add_import("java.util.ArrayList");
        self.generator.add_import("java.util.Collections");
        self.generator.add_import("java.util.Comparator");
        self.generator.add_import("java.util.HashMap");
        self.generator.add_import("java.util.List");
        self.generator.add_import("java.util.Locale");
        self.generator.add_import("java.util.Map");
        self.generator.add_import("java.util.Optional");
        self.generator.add_import("java.util.stream.Stream");
        self.generator.add_import("java.math.BigDecimal");
        self.generator.add_import("java.math.BigInteger");
    }

    fn write_class_header(&mut self) {
        self.builder
            .push_line(&format!("public final class {} {{", self.helper_name));
        self.builder.indent();
    }

    fn write_constants(&mut self) {
        let source_literal = JavaCodeGenerator::escape_string(&self.declaration.source);
        let expected_sha = self.declaration.sha256.to_ascii_lowercase();
        let cache_literal = self
            .declaration
            .cache_path
            .as_ref()
            .map(|path| JavaCodeGenerator::escape_string(&path.to_string_lossy()));
        let limit_bytes = self.declaration.limit_bytes.unwrap_or(0);

        self.builder.push_line(&format!(
            "private static final String SOURCE = \"{}\";",
            source_literal
        ));
        if !expected_sha.is_empty() {
            self.builder.push_line(&format!(
                "private static final String EXPECTED_SHA256 = \"{}\";",
                expected_sha
            ));
        } else {
            self.builder
                .push_line("private static final String EXPECTED_SHA256 = \"\";");
        }

        if let Some(cache) = cache_literal {
            self.builder.push_line(&format!(
                "private static final String CACHE_PATH = \"{}\";",
                cache
            ));
        } else {
            self.builder
                .push_line("private static final String CACHE_PATH = null;");
        }

        if limit_bytes > 0 {
            self.builder.push_line(&format!(
                "private static final long LIMIT_BYTES = {}L;",
                limit_bytes
            ));
        } else {
            self.builder
                .push_line("private static final long LIMIT_BYTES = -1L;");
        }

        self.builder.push_line(
            "private static final java.net.http.HttpClient HTTP_CLIENT = java.net.http.HttpClient\
                .newBuilder()\
                .followRedirects(java.net.http.HttpClient.Redirect.NORMAL)\
                .build();",
        );
        self.builder.push_line("");
    }

    fn write_constructor(&mut self) {
        self.builder
            .push_line(&format!("private {}() {{}}", self.helper_name));
        self.builder.push_line("");
    }

    fn write_loader_method(&mut self) {
        self.builder.push_line(&format!(
            "public static {} {}() throws java.io.IOException {{",
            self.return_type, self.method_name
        ));
        self.builder.indent();
        self.builder.push_line("byte[] bytes;");
        self.builder.push_line("try {");
        self.builder.indent();
        self.builder.push_line("bytes = fetchBytes();");
        self.builder.dedent();
        self.builder.push_line("} catch (InterruptedException e) {");
        self.builder.indent();
        self.builder
            .push_line("Thread.currentThread().interrupt();");
        self.builder.push_line(
            "throw new java.io.IOException(\"サンプルデータの取得が割り込みされました\", e);",
        );
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("try {");
        self.builder.indent();
        self.builder.push_line("return decodeData(bytes);");
        self.builder.dedent();
        self.builder
            .push_line("} catch (IllegalArgumentException e) {");
        self.builder.indent();
        self.builder.push_line(
            "throw new java.io.IOException(\"サンプルデータの解析に失敗しました: \" + e.getMessage(), e);",
        );
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");
    }

    fn write_decode_methods(&mut self) -> Result<(), CodeGenError> {
        self.write_decode_dispatch()?;
        match self.declaration.format {
            DataFormat::Json => self.write_json_decoders()?,
            DataFormat::Csv | DataFormat::Tsv => self.write_tabular_decoders()?,
        }
        Ok(())
    }

    fn write_decode_dispatch(&mut self) -> Result<(), CodeGenError> {
        self.builder.push_line(&format!(
            "private static {} decodeData(byte[] bytes) throws IllegalArgumentException {{",
            self.return_type
        ));
        self.builder.indent();
        match self.declaration.format {
            DataFormat::Json => {
                self.builder.push_line("return decodeJson(bytes);");
            }
            DataFormat::Csv => {
                self.builder.push_line("return decodeTabular(bytes, ',');");
            }
            DataFormat::Tsv => {
                self.builder
                    .push_line("return decodeTabular(bytes, '\\t');");
            }
        }
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");
        Ok(())
    }

    fn write_fetch_helpers(&mut self) {
        self.builder.push_line(
            "private static byte[] fetchBytes() throws java.io.IOException, InterruptedException {",
        );
        self.builder.indent();
        self.builder.push_line("try {");
        self.builder.indent();
        self.builder.push_line("byte[] data = fetchPrimary();");
        self.builder.push_line("verifySha(data);");
        self.builder.push_line("enforceLimit(data.length);");
        self.builder.push_line("return data;");
        self.builder.dedent();
        self.builder
            .push_line("} catch (java.io.IOException primaryError) {");
        self.builder.indent();
        self.builder.push_line("if (CACHE_PATH != null) {");
        self.builder.indent();
        self.builder.push_line("try {");
        self.builder.indent();
        self.builder.push_line(
            "byte[] cached = java.nio.file.Files.readAllBytes(java.nio.file.Path.of(CACHE_PATH));",
        );
        self.builder.push_line("verifySha(cached);");
        self.builder.push_line("enforceLimit(cached.length);");
        self.builder.push_line("return cached;");
        self.builder.dedent();
        self.builder
            .push_line("} catch (java.io.IOException cacheError) {");
        self.builder.indent();
        self.builder
            .push_line("primaryError.addSuppressed(cacheError);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("throw primaryError;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static byte[] fetchPrimary() throws java.io.IOException, InterruptedException {",
        );
        self.builder.indent();
        self.builder
            .push_line("String lower = SOURCE.toLowerCase(java.util.Locale.ROOT);");
        self.builder
            .push_line("if (lower.startsWith(\"http://\") || lower.startsWith(\"https://\")) {");
        self.builder.indent();
        self.builder.push_line("return fetchHttp(SOURCE);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("if (lower.startsWith(\"s3://\")) {");
        self.builder.indent();
        self.builder.push_line("return fetchS3(SOURCE);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder
            .push_line("if (lower.startsWith(\"git+ssh://\")) {");
        self.builder.indent();
        self.builder.push_line("return fetchGitSsh(SOURCE);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder
            .push_line("if (lower.startsWith(\"file://\")) {");
        self.builder.indent();
        self.builder.push_line("return fetchFileUri(SOURCE);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("return fetchLocalPath(SOURCE);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static byte[] fetchHttp(String uri) throws java.io.IOException, InterruptedException {",
        );
        self.builder.indent();
        self.builder.push_line(
            "java.net.http.HttpRequest request = java.net.http.HttpRequest.newBuilder(java.net.URI.create(uri)).GET().build();",
        );
        self.builder.push_line(
            "java.net.http.HttpResponse<byte[]> response = HTTP_CLIENT.send(request, java.net.http.HttpResponse.BodyHandlers.ofByteArray());",
        );
        self.builder
            .push_line("int status = response.statusCode();");
        self.builder.push_line("if (status / 100 != 2) {");
        self.builder.indent();
        self.builder.push_line(
            "throw new java.io.IOException(\"HTTP取得に失敗しました (status=\" + status + \")\");",
        );
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("return response.body();");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static byte[] fetchLocalPath(String path) throws java.io.IOException {",
        );
        self.builder.indent();
        self.builder
            .push_line("return java.nio.file.Files.readAllBytes(java.nio.file.Path.of(path));");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static byte[] fetchFileUri(String uri) throws java.io.IOException {",
        );
        self.builder.indent();
        self.builder.push_line("try {");
        self.builder.indent();
        self.builder.push_line(
            "java.nio.file.Path path = java.nio.file.Path.of(java.net.URI.create(uri));",
        );
        self.builder
            .push_line("return java.nio.file.Files.readAllBytes(path);");
        self.builder.dedent();
        self.builder
            .push_line("} catch (IllegalArgumentException ex) {");
        self.builder.indent();
        self.builder.push_line(
            "throw new java.io.IOException(\"file:// URI をパスに変換できませんでした\", ex);",
        );
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static byte[] fetchS3(String uri) throws java.io.IOException, InterruptedException {",
        );
        self.builder.indent();
        self.builder.push_line(
            "ProcessBuilder builder = new ProcessBuilder(\"aws\", \"s3\", \"cp\", uri, \"-\", \"--no-progress\");",
        );
        self.builder.push_line("Process process = builder.start();");
        self.builder.push_line("byte[] stdout; byte[] stderr;");
        self.builder
            .push_line("try (java.io.InputStream out = process.getInputStream()) {");
        self.builder.indent();
        self.builder.push_line("stdout = readAll(out);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder
            .push_line("try (java.io.InputStream err = process.getErrorStream()) {");
        self.builder.indent();
        self.builder.push_line("stderr = readAll(err);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("int exit = process.waitFor();");
        self.builder.push_line("if (exit != 0) {");
        self.builder.indent();
        self.builder.push_line(
            "throw new java.io.IOException(\"aws s3 cp が失敗しました (exit=\" + exit + \"), stderr=\" + new String(stderr, java.nio.charset.StandardCharsets.UTF_8));",
        );
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("return stdout;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static byte[] fetchGitSsh(String uri) throws java.io.IOException, InterruptedException {",
        );
        self.builder.indent();
        self.builder
            .push_line("java.net.URI parsed = java.net.URI.create(uri);");
        self.builder
            .push_line("java.util.Map<String, String> query = parseQuery(parsed.getRawQuery());");
        self.builder
            .push_line("String filePath = query.get(\"path\");");
        self.builder.push_line("if (filePath == null) {");
        self.builder.indent();
        self.builder.push_line(
            "throw new java.io.IOException(\"git+ssh URI には path= クエリが必要です\");",
        );
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder
            .push_line("String reference = query.get(\"ref\");");

        self.builder
            .push_line("StringBuilder repo = new StringBuilder();");
        self.builder.push_line("repo.append(\"ssh://\");");
        self.builder.push_line(
            "if (parsed.getUserInfo() != null) { repo.append(parsed.getUserInfo()).append('@'); }",
        );
        self.builder.push_line("repo.append(parsed.getHost());");
        self.builder.push_line(
            "if (parsed.getPort() != -1) { repo.append(':').append(parsed.getPort()); }",
        );
        self.builder.push_line("repo.append(parsed.getPath());");

        self.builder.push_line(
            "java.nio.file.Path tempDir = java.nio.file.Files.createTempDirectory(\"jv-sample-git\");",
        );
        self.builder
            .push_line("java.nio.file.Path repoDir = tempDir.resolve(\"repo\");");

        self.builder.push_line("try {");
        self.builder.indent();
        self.builder
            .push_line("java.util.List<String> command = new java.util.ArrayList<>();");
        self.builder.push_line("command.add(\"git\");");
        self.builder.push_line("command.add(\"clone\");");
        self.builder.push_line("command.add(\"--depth\");");
        self.builder.push_line("command.add(\"1\");");
        self.builder.push_line(
            "if (reference != null) { command.add(\"--branch\"); command.add(reference); }",
        );
        self.builder.push_line("command.add(repo.toString());");
        self.builder.push_line("command.add(repoDir.toString());");
        self.builder.push_line(
            "Process clone = new ProcessBuilder(command).redirectErrorStream(true).start();",
        );
        self.builder.push_line(
            "byte[] cloneOut; try (java.io.InputStream out = clone.getInputStream()) { cloneOut = readAll(out); }",
        );
        self.builder.push_line("int status = clone.waitFor();");
        self.builder.push_line("if (status != 0) {");
        self.builder.indent();
        self.builder.push_line(
            "throw new java.io.IOException(\"git clone が失敗しました (exit=\" + status + \"), output=\" + new String(cloneOut, java.nio.charset.StandardCharsets.UTF_8));",
        );
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder
            .push_line("java.nio.file.Path target = repoDir.resolve(filePath);");
        self.builder
            .push_line("if (!java.nio.file.Files.exists(target)) {");
        self.builder.indent();
        self.builder.push_line(
            "throw new java.io.IOException(\"指定されたパスがリポジトリ内に存在しません: \" + filePath);",
        );
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder
            .push_line("return java.nio.file.Files.readAllBytes(target);");
        self.builder.dedent();
        self.builder.push_line("} finally {");
        self.builder.indent();
        self.builder.push_line("deleteDirectory(tempDir);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static byte[] readAll(java.io.InputStream stream) throws java.io.IOException {",
        );
        self.builder.indent();
        self.builder.push_line("return stream.readAllBytes();");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private static void verifySha(byte[] data) throws java.io.IOException {");
        self.builder.indent();
        self.builder
            .push_line("if (EXPECTED_SHA256.isEmpty()) { return; }");
        self.builder
            .push_line("String actual = computeSha256(data);");
        self.builder
            .push_line("if (!EXPECTED_SHA256.equals(actual)) {");
        self.builder.indent();
        self.builder.push_line(
            "throw new java.io.IOException(\"SHA256不一致です: expected=\" + EXPECTED_SHA256 + \", actual=\" + actual);",
        );
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private static void enforceLimit(long size) throws java.io.IOException {");
        self.builder.indent();
        self.builder
            .push_line("if (LIMIT_BYTES > 0 && size > LIMIT_BYTES) {");
        self.builder.indent();
        self.builder.push_line(
            "throw new java.io.IOException(\"SampleデータがlimitBytesを超えています (limit=\" + LIMIT_BYTES + \", actual=\" + size + \")\");",
        );
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private static String computeSha256(byte[] data) {");
        self.builder.indent();
        self.builder.push_line("try {");
        self.builder.indent();
        self.builder.push_line(
            "java.security.MessageDigest digest = java.security.MessageDigest.getInstance(\"SHA-256\");",
        );
        self.builder.push_line("byte[] hash = digest.digest(data);");
        self.builder
            .push_line("StringBuilder hex = new StringBuilder(hash.length * 2);");
        self.builder.push_line("for (byte b : hash) {");
        self.builder.indent();
        self.builder
            .push_line("hex.append(String.format(java.util.Locale.ROOT, \"%02x\", b));");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("return hex.toString();");
        self.builder.dedent();
        self.builder
            .push_line("} catch (java.security.NoSuchAlgorithmException e) {");
        self.builder.indent();
        self.builder
            .push_line("throw new IllegalStateException(\"SHA-256 not available\", e);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private static java.util.Map<String, String> parseQuery(String query) {");
        self.builder.indent();
        self.builder
            .push_line("java.util.Map<String, String> map = new java.util.HashMap<>();");
        self.builder
            .push_line("if (query == null || query.isEmpty()) { return map; }");
        self.builder
            .push_line("String[] pairs = query.split(\"&\");");
        self.builder.push_line("for (String pair : pairs) {");
        self.builder.indent();
        self.builder.push_line("int idx = pair.indexOf('=');");
        self.builder.push_line("if (idx < 0) { continue; }");
        self.builder
            .push_line("String key = pair.substring(0, idx);");
        self.builder
            .push_line("String value = pair.substring(idx + 1);");
        self.builder.push_line("map.put(key, java.net.URLDecoder.decode(value, java.nio.charset.StandardCharsets.UTF_8));");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("return map;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static void deleteDirectory(java.nio.file.Path root) throws java.io.IOException {",
        );
        self.builder.indent();
        self.builder
            .push_line("if (root == null || !java.nio.file.Files.exists(root)) { return; }");
        self.builder.push_line(
            "try (java.util.stream.Stream<java.nio.file.Path> walk = java.nio.file.Files.walk(root)) {",
        );
        self.builder.indent();
        self.builder.push_line(
            "walk.sorted(java.util.Comparator.reverseOrder()).forEach(path -> { try { java.nio.file.Files.deleteIfExists(path); } catch (java.io.IOException ignored) {} });",
        );
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");
    }

    fn write_runtime_helpers(&mut self) {
        self.builder.push_line(
            "private static IllegalArgumentException error(String path, String expected, Object actual) {",
        );
        self.builder.indent();
        self.builder
            .push_line("String actualDesc = actual == null ? \"null\" : actual.toString();");
        self.builder.push_line(
            "return new IllegalArgumentException(path + \": \" + expected + \" を期待しましたが、実際は \" + actualDesc);",
        );
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private static boolean isJsonNull(Object value) {");
        self.builder.indent();
        self.builder
            .push_line("return value == JsonRuntime.JsonNull.INSTANCE;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private static String requireString(Object node, String path) {");
        self.builder.indent();
        self.builder
            .push_line("if (node instanceof String str) { return str; }");
        self.builder
            .push_line("throw error(path, \"文字列\", node);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private static boolean requireBoolean(Object node, String path) {");
        self.builder.indent();
        self.builder
            .push_line("if (node instanceof Boolean bool) { return bool; }");
        self.builder
            .push_line("throw error(path, \"真偽値\", node);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static JsonRuntime.JsonNumber requireNumber(Object node, String path) {",
        );
        self.builder.indent();
        self.builder
            .push_line("if (node instanceof JsonRuntime.JsonNumber num) { return num; }");
        self.builder.push_line("throw error(path, \"数値\", node);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private static int parseInt(JsonRuntime.JsonNumber number, String path) {");
        self.builder.indent();
        self.builder.push_line("try { return Integer.parseInt(number.literal()); } catch (NumberFormatException ex) { throw error(path, \"整数\", number.literal()); }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static long parseLong(JsonRuntime.JsonNumber number, String path) {",
        );
        self.builder.indent();
        self.builder.push_line("try { return Long.parseLong(number.literal()); } catch (NumberFormatException ex) { throw error(path, \"long\", number.literal()); }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static double parseDouble(JsonRuntime.JsonNumber number, String path) {",
        );
        self.builder.indent();
        self.builder.push_line("try { double value = Double.parseDouble(number.literal()); if (Double.isFinite(value)) { return value; } throw new NumberFormatException(); } catch (NumberFormatException ex) { throw error(path, \"有限の浮動小数\", number.literal()); }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static java.math.BigInteger parseBigInteger(JsonRuntime.JsonNumber number, String path) {",
        );
        self.builder.indent();
        self.builder.push_line("try { return new java.math.BigInteger(number.literal()); } catch (NumberFormatException ex) { throw error(path, \"大きな整数\", number.literal()); }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static java.math.BigDecimal parseBigDecimal(JsonRuntime.JsonNumber number, String path) {",
        );
        self.builder.indent();
        self.builder.push_line("try { return new java.math.BigDecimal(number.literal()); } catch (NumberFormatException ex) { throw error(path, \"高精度小数\", number.literal()); }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.write_json_runtime();
        self.write_tabular_runtime();
        self.write_tabular_parsers();
    }

    fn write_json_runtime(&mut self) {
        self.builder
            .push_line("private static final class JsonRuntime {");
        self.builder.indent();
        self.builder.push_line("private final String input;");
        self.builder.push_line("private int index;");
        self.builder.push_line("");

        self.builder
            .push_line("private JsonRuntime(String input) { this.input = input; }");
        self.builder.push_line("");

        self.builder
            .push_line("static Object parse(String input) {");
        self.builder.indent();
        self.builder
            .push_line("JsonRuntime parser = new JsonRuntime(input);");
        self.builder.push_line("parser.skipWhitespace();");
        self.builder
            .push_line("Object value = parser.parseValue();");
        self.builder.push_line("parser.skipWhitespace();");
        self.builder.push_line("if (!parser.isEnd()) {");
        self.builder.indent();
        self.builder
            .push_line("throw new IllegalArgumentException(\"JSONに余分なデータがあります\");");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("return value;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line("private Object parseValue() {");
        self.builder.indent();
        self.builder.push_line(
            "if (isEnd()) { throw new IllegalArgumentException(\"JSONが途中で終了しました\"); }",
        );
        self.builder.push_line("char ch = peek();");
        self.builder
            .push_line("if (ch == 0x22) { return parseString(); }");
        self.builder.push_line("switch (ch) {");
        self.builder.indent();
        self.builder.push_line("case '{': return parseObject();");
        self.builder.push_line("case '[': return parseArray();");
        self.builder.push_line("case 't': return parseTrue();");
        self.builder.push_line("case 'f': return parseFalse();");
        self.builder.push_line("case 'n': return parseNull();");
        self.builder.push_line("default: return parseNumber();");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line("private Object parseObject() {");
        self.builder.indent();
        self.builder.push_line("consume('{');");
        self.builder.push_line("skipWhitespace();");
        self.builder
            .push_line("java.util.Map<String, Object> map = new java.util.HashMap<>();");
        self.builder
            .push_line("if (peek() == '}') { consume('}'); return map; }");
        self.builder.push_line("while (true) {");
        self.builder.indent();
        self.builder.push_line("String key = parseString();");
        self.builder.push_line("skipWhitespace();");
        self.builder.push_line("consume(':');");
        self.builder.push_line("skipWhitespace();");
        self.builder.push_line("Object value = parseValue();");
        self.builder.push_line("map.put(key, value);");
        self.builder.push_line("skipWhitespace();");
        self.builder.push_line("char ch = consume(',', '}');");
        self.builder.push_line("if (ch == '}') { break; }");
        self.builder.push_line("skipWhitespace();");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("return map;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line("private Object parseArray() {");
        self.builder.indent();
        self.builder.push_line("consume('[');");
        self.builder.push_line("skipWhitespace();");
        self.builder
            .push_line("java.util.List<Object> list = new java.util.ArrayList<>();");
        self.builder
            .push_line("if (peek() == ']') { consume(']'); return list; }");
        self.builder.push_line("while (true) {");
        self.builder.indent();
        self.builder.push_line("Object value = parseValue();");
        self.builder.push_line("list.add(value);");
        self.builder.push_line("skipWhitespace();");
        self.builder.push_line("char ch = consume(',', ']');");
        self.builder.push_line("if (ch == ']') { break; }");
        self.builder.push_line("skipWhitespace();");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("return list;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line("private String parseString() {");
        self.builder.indent();
        self.builder.push_line("consume(0x22);");
        self.builder
            .push_line("StringBuilder sb = new StringBuilder();");
        self.builder.push_line("while (!isEnd()) {");
        self.builder.indent();
        self.builder.push_line("char ch = next();");
        self.builder.push_line("if (ch == 0x22) { break; }");
        self.builder.push_line("if (ch == '\\') {");
        self.builder.indent();
        self.builder.push_line("if (isEnd()) { throw new IllegalArgumentException(\"不完全なエスケープシーケンスです\"); }");
        self.builder.push_line("char esc = next();");
        self.builder.push_line("switch (esc) {");
        self.builder.indent();
        self.builder
            .push_line("case 0x22: sb.append((char) 0x22); break;");
        self.builder.push_line("case '\\': sb.append('\\'); break;");
        self.builder.push_line("case '/': sb.append('/'); break;");
        self.builder.push_line("case 'b': sb.append('\\b'); break;");
        self.builder.push_line("case 'f': sb.append('\\f'); break;");
        self.builder.push_line("case 'n': sb.append('\\n'); break;");
        self.builder.push_line("case 'r': sb.append('\\r'); break;");
        self.builder.push_line("case 't': sb.append('\\t'); break;");
        self.builder
            .push_line("case 'u': sb.append(parseUnicode()); break;");
        self.builder.push_line(
            "default: throw new IllegalArgumentException(\"未知のエスケープ: \" + esc);",
        );
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("continue;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("sb.append(ch);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("return sb.toString();");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line("private char parseUnicode() {");
        self.builder.indent();
        self.builder.push_line("if (index + 4 > input.length()) { throw new IllegalArgumentException(\"Unicodeエスケープが短すぎます\"); }");
        self.builder
            .push_line("String hex = input.substring(index, index + 4);");
        self.builder.push_line("index += 4;");
        self.builder
            .push_line("return (char) Integer.parseInt(hex, 16);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line("private JsonNumber parseNumber() {");
        self.builder.indent();
        self.builder.push_line("int start = index;");
        self.builder.push_line("if (peek() == '-') { next(); }");
        self.builder
            .push_line("while (!isEnd() && Character.isDigit(peek())) { next(); }");
        self.builder.push_line("if (!isEnd() && peek() == '.') { next(); while (!isEnd() && Character.isDigit(peek())) { next(); } }");
        self.builder
            .push_line("if (!isEnd() && (peek() == 'e' || peek() == 'E')) {");
        self.builder.indent();
        self.builder.push_line("next();");
        self.builder
            .push_line("if (!isEnd() && (peek() == '+' || peek() == '-')) { next(); }");
        self.builder
            .push_line("while (!isEnd() && Character.isDigit(peek())) { next(); }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder
            .push_line("String literal = input.substring(start, index);");
        self.builder.push_line("return new JsonNumber(literal);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line("private Boolean parseTrue() { expect('t'); expect('r'); expect('u'); expect('e'); return Boolean.TRUE; }");
        self.builder.push_line("private Boolean parseFalse() { expect('f'); expect('a'); expect('l'); expect('s'); expect('e'); return Boolean.FALSE; }");
        self.builder.push_line("private JsonNull parseNull() { expect('n'); expect('u'); expect('l'); expect('l'); return JsonNull.INSTANCE; }");
        self.builder.push_line("");

        self.builder
            .push_line("private char consume(char expected) {");
        self.builder.indent();
        self.builder.push_line("char ch = next();");
        self.builder.push_line("if (ch != expected) { throw new IllegalArgumentException(\"期待した文字 '\" + expected + \"' ではありません\"); }");
        self.builder.push_line("return ch;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private char consume(char option1, char option2) {");
        self.builder.indent();
        self.builder.push_line("char ch = next();");
        self.builder.push_line("if (ch != option1 && ch != option2) { throw new IllegalArgumentException(\"期待した文字 '\" + option1 + \"' または '\" + option2 + \"' ではありません\"); }");
        self.builder.push_line("return ch;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private void expect(char expected) {");
        self.builder.indent();
        self.builder.push_line("char ch = next();");
        self.builder.push_line("if (ch != expected) { throw new IllegalArgumentException(\"期待した文字 '\" + expected + \"' ではありません\"); }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private char peek() { return input.charAt(index); }");
        self.builder
            .push_line("private char next() { return input.charAt(index++); }");
        self.builder
            .push_line("private boolean isEnd() { return index >= input.length(); }");
        self.builder.push_line("private void skipWhitespace() { while (!isEnd()) { char ch = peek(); if (ch == ' ' || ch == '\\t' || ch == '\\r' || ch == '\\n') { index++; } else { break; } } }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private static final class JsonNumber {");
        self.builder.indent();
        self.builder.push_line("private final String literal;");
        self.builder
            .push_line("JsonNumber(String literal) { this.literal = literal; }");
        self.builder
            .push_line("String literal() { return literal; }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line("private static final class JsonNull { private JsonNull() {} static final JsonNull INSTANCE = new JsonNull(); }");
        self.builder.push_line("");
    }

    fn write_tabular_runtime(&mut self) {
        self.builder
            .push_line("private static final class TabularRuntime {");
        self.builder.indent();
        self.builder.push_line(
            "static java.util.List<java.util.Map<String, String>> parse(String raw, char delimiter) {",
        );
        self.builder.indent();
        self.builder
            .push_line("java.util.List<String[]> rows = split(raw, delimiter);");
        self.builder.push_line(
            "java.util.List<java.util.Map<String, String>> result = new java.util.ArrayList<>();",
        );
        self.builder
            .push_line("if (rows.isEmpty()) { return result; }");
        self.builder.push_line("String[] header = rows.get(0);");
        self.builder
            .push_line("for (int i = 1; i < rows.size(); i++) {");
        self.builder.indent();
        self.builder.push_line("String[] row = rows.get(i);");
        self.builder
            .push_line("java.util.Map<String, String> entry = new java.util.HashMap<>();");
        self.builder
            .push_line("for (int col = 0; col < header.length; col++) {");
        self.builder.indent();
        self.builder
            .push_line("String key = header[col] == null ? \"\" : header[col].trim();");
        self.builder.push_line("if (key.isEmpty()) { continue; }");
        self.builder
            .push_line("String value = col < row.length ? row[col] : \"\";");
        self.builder.push_line("entry.put(key, value);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("result.add(entry);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("return result;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static java.util.List<String[]> split(String raw, char delimiter) {",
        );
        self.builder.indent();
        self.builder
            .push_line("java.util.List<String[]> rows = new java.util.ArrayList<>();");
        self.builder
            .push_line("java.util.List<String> current = new java.util.ArrayList<>();");
        self.builder
            .push_line("StringBuilder field = new StringBuilder();");
        self.builder.push_line("boolean inQuotes = false;");
        self.builder
            .push_line("for (int i = 0; i < raw.length(); i++) {");
        self.builder.indent();
        self.builder.push_line("char ch = raw.charAt(i);");
        self.builder.push_line("if (inQuotes) {");
        self.builder.indent();
        self.builder.push_line("if (ch == 0x22) {");
        self.builder.indent();
        self.builder
            .push_line("if (i + 1 < raw.length() && raw.charAt(i + 1) == 0x22) {");
        self.builder.indent();
        self.builder.push_line("field.append((char) 0x22);");
        self.builder.push_line("i++;");
        self.builder.dedent();
        self.builder.push_line("} else {");
        self.builder.indent();
        self.builder.push_line("inQuotes = false;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("continue;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("field.append(ch);");
        self.builder.dedent();
        self.builder.push_line("} else {");
        self.builder.indent();
        self.builder.push_line("if (ch == 0x22) {");
        self.builder.indent();
        self.builder.push_line("inQuotes = true;");
        self.builder.push_line("continue;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("if (ch == delimiter) {");
        self.builder.indent();
        self.builder.push_line("current.add(field.toString());");
        self.builder.push_line("field.setLength(0);");
        self.builder.push_line("continue;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("if (ch == '\\r') {");
        self.builder.indent();
        self.builder
            .push_line("if (i + 1 < raw.length() && raw.charAt(i + 1) == '\\n') { i++; }");
        self.builder.push_line("current.add(field.toString());");
        self.builder.push_line("field.setLength(0);");
        self.builder
            .push_line("rows.add(current.toArray(new String[0]));");
        self.builder
            .push_line("current = new java.util.ArrayList<>();");
        self.builder.push_line("continue;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("if (ch == '\\n') {");
        self.builder.indent();
        self.builder.push_line("current.add(field.toString());");
        self.builder.push_line("field.setLength(0);");
        self.builder
            .push_line("rows.add(current.toArray(new String[0]));");
        self.builder
            .push_line("current = new java.util.ArrayList<>();");
        self.builder.push_line("continue;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("field.append(ch);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("current.add(field.toString());");
        self.builder
            .push_line("rows.add(current.toArray(new String[0]));");
        self.builder.push_line("return rows;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");
    }

    fn write_tabular_parsers(&mut self) {
        self.builder.push_line("private static String normalizeColumn(String value) { return value == null ? \"\" : value.trim(); }");
        self.builder.push_line("private static boolean isEmpty(String value) { return value == null || value.trim().isEmpty(); }");
        self.builder.push_line("");

        self.builder
            .push_line("private static int parseIntColumn(String raw, String label) {");
        self.builder.indent();
        self.builder
            .push_line("String normalized = normalizeColumn(raw);");
        self.builder.push_line("try { return Integer.parseInt(normalized); } catch (NumberFormatException ex) { throw new IllegalArgumentException(label + \" は整数として解析できません: \" + normalized); }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private static long parseLongColumn(String raw, String label) {");
        self.builder.indent();
        self.builder
            .push_line("String normalized = normalizeColumn(raw);");
        self.builder.push_line("try { return Long.parseLong(normalized); } catch (NumberFormatException ex) { throw new IllegalArgumentException(label + \" はlongとして解析できません: \" + normalized); }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private static boolean parseBooleanColumn(String raw, String label) {");
        self.builder.indent();
        self.builder.push_line(
            "String normalized = normalizeColumn(raw).toLowerCase(java.util.Locale.ROOT);",
        );
        self.builder.push_line("return switch (normalized) { case \"true\", \"1\", \"yes\" -> true; case \"false\", \"0\", \"no\" -> false; default -> throw new IllegalArgumentException(label + \" は真偽値として解析できません: \" + raw); };");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static java.math.BigInteger parseBigIntegerColumn(String raw, String label) {",
        );
        self.builder.indent();
        self.builder
            .push_line("String normalized = normalizeColumn(raw);");
        self.builder.push_line("try { return new java.math.BigInteger(normalized); } catch (NumberFormatException ex) { throw new IllegalArgumentException(label + \" は大きな整数として解析できません: \" + raw); }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder.push_line(
            "private static java.math.BigDecimal parseBigDecimalColumn(String raw, String label) {",
        );
        self.builder.indent();
        self.builder
            .push_line("String normalized = normalizeColumn(raw);");
        self.builder.push_line("try { return new java.math.BigDecimal(normalized); } catch (NumberFormatException ex) { throw new IllegalArgumentException(label + \" は小数として解析できません: \" + raw); }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.builder
            .push_line("private static double parseDoubleColumn(String raw, String label) {");
        self.builder.indent();
        self.builder
            .push_line("String normalized = normalizeColumn(raw);");
        self.builder.push_line("try { double value = Double.parseDouble(normalized); if (Double.isFinite(value)) { return value; } throw new NumberFormatException(); } catch (NumberFormatException ex) { throw new IllegalArgumentException(label + \" は浮動小数として解析できません: \" + raw); }");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");
    }

    fn close_class(&mut self) {
        self.builder.dedent();
        self.builder.push_line("}");
    }

    fn sanitize_identifier(name: &str) -> String {
        let mut result = String::new();
        for ch in name.chars() {
            if ch.is_ascii_alphanumeric() {
                result.push(ch);
            } else {
                result.push('_');
            }
        }
        if result.is_empty() {
            result.push('X');
        }
        result
    }

    fn json_record_decoder_name(&self, record_name: &str) -> String {
        format!("decodeJson{}", Self::sanitize_identifier(record_name))
    }

    fn json_list_decoder_name(&self, list_type: &JavaType) -> Result<String, CodeGenError> {
        match list_type {
            JavaType::Reference { name, .. } => {
                Ok(format!("decodeJsonList{}", Self::sanitize_identifier(name)))
            }
            _ => Err(CodeGenError::UnsupportedConstruct {
                construct: format!("Json list decoderに対応していない型です: {:?}", list_type),
                span: Some(self.declaration.span.clone()),
            }),
        }
    }

    fn json_value_expression(
        &self,
        value_expr: &str,
        path_expr: &str,
        schema: &Schema,
        java_type: &JavaType,
    ) -> Result<String, CodeGenError> {
        let (core_schema, schema_optional) = unwrap_schema_optional(schema);
        let (core_type, type_optional) = JavaCodeGenerator::unwrap_optional_java_type(java_type);

        if type_optional || schema_optional {
            let inner_expr =
                self.json_value_expression(value_expr, path_expr, core_schema, core_type)?;
            return Ok(format!(
                "(isJsonNull({0}) ? java.util.Optional.empty() : java.util.Optional.of({1}))",
                value_expr, inner_expr
            ));
        }

        match (core_schema, core_type) {
            (Schema::Primitive(PrimitiveType::String), _) => {
                Ok(format!("requireString({}, {})", value_expr, path_expr))
            }
            (Schema::Primitive(PrimitiveType::Boolean), _) => {
                Ok(format!("requireBoolean({}, {})", value_expr, path_expr))
            }
            (Schema::Primitive(PrimitiveType::Integer), _) => Ok(format!(
                "parseInt(requireNumber({}, {}), {})",
                value_expr, path_expr, path_expr
            )),
            (Schema::Primitive(PrimitiveType::Long), _) => Ok(format!(
                "parseLong(requireNumber({}, {}), {})",
                value_expr, path_expr, path_expr
            )),
            (Schema::Primitive(PrimitiveType::Double), _) => Ok(format!(
                "parseDouble(requireNumber({}, {}), {})",
                value_expr, path_expr, path_expr
            )),
            (Schema::Primitive(PrimitiveType::BigInteger), _) => Ok(format!(
                "parseBigInteger(requireNumber({}, {}), {})",
                value_expr, path_expr, path_expr
            )),
            (Schema::Primitive(PrimitiveType::BigDecimal), _) => Ok(format!(
                "parseBigDecimal(requireNumber({}, {}), {})",
                value_expr, path_expr, path_expr
            )),
            (Schema::Array { .. }, JavaType::Reference { .. }) => {
                let decoder = self.json_list_decoder_name(java_type)?;
                Ok(format!("{}({}, {})", decoder, value_expr, path_expr))
            }
            (Schema::Object { .. }, JavaType::Reference { name, .. }) => Ok(format!(
                "{}({}, {})",
                self.json_record_decoder_name(name),
                value_expr,
                path_expr
            )),
            (Schema::Union(variants), _) => {
                let mut variant_exprs = Vec::new();
                for variant in variants {
                    if matches!(variant, Schema::Primitive(PrimitiveType::Null)) {
                        continue;
                    }
                    variant_exprs.push(
                        self.json_value_expression(value_expr, path_expr, variant, core_type)?,
                    );
                }
                if variant_exprs.is_empty() {
                    return Err(CodeGenError::UnsupportedConstruct {
                        construct: "Union型のデコードに対応していません".to_string(),
                        span: Some(self.declaration.span.clone()),
                    });
                }
                Ok(variant_exprs.into_iter().next().unwrap())
            }
            _ => Err(CodeGenError::UnsupportedConstruct {
                construct: format!(
                    "JSONデコードに対応していないスキーマ/型の組み合わせです: schema={:?}, type={:?}",
                    schema, java_type
                ),
                span: Some(self.declaration.span.clone()),
            }),
        }
    }

    fn tabular_value_expression(
        &self,
        raw_expr: &str,
        schema: &Schema,
        java_type: &JavaType,
        label_expr: &str,
    ) -> Result<String, CodeGenError> {
        let (core_schema, schema_optional) = unwrap_schema_optional(schema);
        let (core_type, type_optional) = JavaCodeGenerator::unwrap_optional_java_type(java_type);

        if type_optional || schema_optional {
            let inner =
                self.tabular_value_expression(raw_expr, core_schema, core_type, label_expr)?;
            return Ok(format!("java.util.Optional.of({})", inner));
        }

        match core_schema {
            Schema::Primitive(PrimitiveType::String) => {
                Ok(format!("normalizeColumn({})", raw_expr))
            }
            Schema::Primitive(PrimitiveType::Boolean) => {
                Ok(format!("parseBooleanColumn({}, {})", raw_expr, label_expr))
            }
            Schema::Primitive(PrimitiveType::Integer) => {
                Ok(format!("parseIntColumn({}, {})", raw_expr, label_expr))
            }
            Schema::Primitive(PrimitiveType::Long) => {
                Ok(format!("parseLongColumn({}, {})", raw_expr, label_expr))
            }
            Schema::Primitive(PrimitiveType::BigInteger) => Ok(format!(
                "parseBigIntegerColumn({}, {})",
                raw_expr, label_expr
            )),
            Schema::Primitive(PrimitiveType::BigDecimal) => Ok(format!(
                "parseBigDecimalColumn({}, {})",
                raw_expr, label_expr
            )),
            Schema::Primitive(PrimitiveType::Double) => {
                Ok(format!("parseDoubleColumn({}, {})", raw_expr, label_expr))
            }
            _ => Err(CodeGenError::UnsupportedConstruct {
                construct: format!(
                    "CSV/TSVデコードに対応していないスキーマです: {:?}",
                    core_schema
                ),
                span: Some(self.declaration.span.clone()),
            }),
        }
    }

    fn write_json_decoders(&mut self) -> Result<(), CodeGenError> {
        self.builder.push_line(&format!(
            "private static {} decodeJson(byte[] bytes) {{",
            self.return_type
        ));
        self.builder.indent();
        self.builder
            .push_line("String raw = new String(bytes, java.nio.charset.StandardCharsets.UTF_8);");
        self.builder
            .push_line("Object node = JsonRuntime.parse(raw);");
        self.builder
            .push_line("return decodeJsonRoot(node, \"$\");");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.write_json_root_decoder()?;
        self.write_json_record_decoders()?;
        self.write_json_list_decoders()?;

        Ok(())
    }

    fn write_tabular_decoders(&mut self) -> Result<(), CodeGenError> {
        self.builder.push_line(&format!(
            "private static {} decodeTabular(byte[] bytes, char delimiter) {{",
            self.return_type
        ));
        self.builder.indent();
        self.builder
            .push_line("String raw = new String(bytes, java.nio.charset.StandardCharsets.UTF_8);");
        self.builder.push_line(
            "java.util.List<java.util.Map<String, String>> rows = TabularRuntime.parse(raw, delimiter);",
        );
        self.builder.push_line("return decodeTabularRoot(rows);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");

        self.write_tabular_root_decoder()?;
        self.write_tabular_record_decoders()?;

        Ok(())
    }

    fn write_json_root_decoder(&mut self) -> Result<(), CodeGenError> {
        let expr = self.json_value_expression(
            "node",
            "path",
            &self.declaration.schema,
            &self.declaration.java_type,
        )?;

        self.builder.push_line(&format!(
            "private static {} decodeJsonRoot(Object node, String path) {{",
            self.return_type
        ));
        self.builder.indent();
        self.builder.push_line(&format!("return {};", expr));
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");
        Ok(())
    }

    fn write_json_record_decoders(&mut self) -> Result<(), CodeGenError> {
        for descriptor in self.descriptor_lookup.values() {
            let method_name = format!("decodeJson{}", Self::sanitize_identifier(&descriptor.name));
            self.builder.push_line("@SuppressWarnings(\"unchecked\")");
            self.builder.push_line(&format!(
                "private static {} {}(Object node, String path) {{",
                descriptor.name, method_name
            ));
            self.builder.indent();
            self.builder
                .push_line("if (!(node instanceof java.util.Map<?, ?> raw)) {");
            self.builder.indent();
            self.builder
                .push_line("throw error(path, \"JSON object\", node);");
            self.builder.dedent();
            self.builder.push_line("}");
            self.builder.push_line(
                "java.util.Map<String, Object> object = (java.util.Map<String, Object>) raw;",
            );

            let schema_info = self
                .object_schema_map
                .get(&descriptor.name)
                .ok_or_else(|| CodeGenError::UnsupportedConstruct {
                    construct: format!(
                        "レコード {} のスキーマ情報が見つかりません",
                        descriptor.name
                    ),
                    span: Some(self.declaration.span.clone()),
                })?
                .clone();

            let mut field_exprs = Vec::new();
            for field in &descriptor.fields {
                let field_schema = schema_info.fields.get(&field.name).ok_or_else(|| {
                    CodeGenError::UnsupportedConstruct {
                        construct: format!(
                            "フィールド {} のスキーマ情報が見つかりません",
                            field.name
                        ),
                        span: Some(self.declaration.span.clone()),
                    }
                })?;
                let field_path = format!("path + \".{}\"", field.name);
                self.builder.push_line(&format!(
                    "Object {}Value = object.get(\"{}\");",
                    field.name, field.name
                ));
                self.builder.push_line(&format!(
                    "boolean has{} = object.containsKey(\"{}\");",
                    Self::sanitize_identifier(&field.name),
                    field.name
                ));
                if field.is_optional {
                    self.builder.push_line(&format!(
                        "{} {};",
                        self.generator.generate_type(&field.java_type)?,
                        field.name
                    ));
                    self.builder.push_line(&format!(
                        "if (!has{} || isJsonNull({}Value)) {{",
                        Self::sanitize_identifier(&field.name),
                        field.name
                    ));
                    self.builder.indent();
                    self.builder
                        .push_line(&format!("{} = java.util.Optional.empty();", field.name));
                    self.builder.dedent();
                    self.builder.push_line("} else {");
                    self.builder.indent();
                    let inner_type =
                        JavaCodeGenerator::unwrap_optional_java_type(&field.java_type).0;
                    let expr = self.json_value_expression(
                        &format!("{}Value", field.name),
                        &field_path,
                        field_schema,
                        inner_type,
                    )?;
                    self.builder.push_line(&format!(
                        "{} = java.util.Optional.of({});",
                        field.name, expr
                    ));
                    self.builder.dedent();
                    self.builder.push_line("}");
                } else {
                    self.builder.push_line(&format!(
                        "if (!has{} || isJsonNull({}Value)) {{",
                        Self::sanitize_identifier(&field.name),
                        field.name
                    ));
                    self.builder.indent();
                    self.builder.push_line(&format!(
                        "throw error({}, \"必須フィールド\", null);",
                        field_path
                    ));
                    self.builder.dedent();
                    self.builder.push_line("}");
                    let expr = self.json_value_expression(
                        &format!("{}Value", field.name),
                        &field_path,
                        field_schema,
                        &field.java_type,
                    )?;
                    self.builder.push_line(&format!(
                        "{} {} = {};",
                        self.generator.generate_type(&field.java_type)?,
                        field.name,
                        expr
                    ));
                }
                field_exprs.push(field.name.clone());
            }
            let args = field_exprs.join(", ");
            self.builder
                .push_line(&format!("return new {}({});", descriptor.name, args));
            self.builder.dedent();
            self.builder.push_line("}");
            self.builder.push_line("");
        }
        Ok(())
    }

    fn write_json_list_decoders(&mut self) -> Result<(), CodeGenError> {
        for info in &self.list_types {
            if let JavaType::Reference { name, .. } = &info.list_type {
                let method_name = format!("decodeJsonList{}", Self::sanitize_identifier(name));
                let list_type_str = self.generator.generate_type(&info.list_type)?;
                let element_type_str = self.generator.generate_type(&info.element_type)?;
                self.builder.push_line("@SuppressWarnings(\"unchecked\")");
                self.builder.push_line(&format!(
                    "private static {} {}(Object node, String path) {{",
                    list_type_str, method_name
                ));
                self.builder.indent();
                self.builder
                    .push_line("if (!(node instanceof java.util.List<?> raw)) {");
                self.builder.indent();
                self.builder
                    .push_line("throw error(path, \"JSON array\", node);");
                self.builder.dedent();
                self.builder.push_line("}");
                self.builder
                    .push_line("java.util.List<Object> array = (java.util.List<Object>) raw;");
                self.builder.push_line(&format!(
                    "java.util.List<{}> result = new java.util.ArrayList<>(array.size());",
                    element_type_str
                ));
                self.builder
                    .push_line("for (int i = 0; i < array.size(); i++) {");
                self.builder.indent();
                self.builder.push_line("Object element = array.get(i);");
                let element_expr = self.json_value_expression(
                    "element",
                    "path + \"[\" + i + \"]\"",
                    &info.element_schema,
                    &info.element_type,
                )?;
                self.builder
                    .push_line(&format!("result.add({});", element_expr));
                self.builder.dedent();
                self.builder.push_line("}");
                self.builder
                    .push_line("return java.util.Collections.unmodifiableList(result);");
                self.builder.dedent();
                self.builder.push_line("}");
                self.builder.push_line("");
            }
        }
        Ok(())
    }

    fn write_tabular_root_decoder(&mut self) -> Result<(), CodeGenError> {
        let (list_method, element_type_str) = match &self.declaration.java_type {
            JavaType::Reference { name, generic_args }
                if name == "java.util.List" && !generic_args.is_empty() =>
            {
                let element_type = &generic_args[0];
                let type_str = self.generator.generate_type(element_type)?;
                let method_name = match element_type {
                    JavaType::Reference {
                        name: record_name, ..
                    } => format!("decodeTabular{}", Self::sanitize_identifier(record_name)),
                    _ => {
                        return Err(CodeGenError::UnsupportedConstruct {
                            construct: "CSV/TSVルートはRecordリストのみサポートされています"
                                .to_string(),
                            span: Some(self.declaration.span.clone()),
                        });
                    }
                };
                (method_name, type_str)
            }
            _ => {
                return Err(CodeGenError::UnsupportedConstruct {
                    construct: "CSV/TSVルートはList<Record>型である必要があります".to_string(),
                    span: Some(self.declaration.span.clone()),
                });
            }
        };

        self.builder.push_line(&format!(
            "private static {} decodeTabularRoot(java.util.List<java.util.Map<String, String>> rows) {{",
            self.return_type
        ));
        self.builder.indent();
        self.builder.push_line(&format!(
            "java.util.List<{}> result = new java.util.ArrayList<>(rows.size());",
            element_type_str
        ));
        self.builder.push_line("int rowIndex = 0;");
        self.builder
            .push_line("for (java.util.Map<String, String> row : rows) {");
        self.builder.indent();
        self.builder
            .push_line(&format!("result.add({}(row, rowIndex));", list_method));
        self.builder.push_line("rowIndex++;");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder
            .push_line("return java.util.Collections.unmodifiableList(result);");
        self.builder.dedent();
        self.builder.push_line("}");
        self.builder.push_line("");
        Ok(())
    }

    fn write_tabular_record_decoders(&mut self) -> Result<(), CodeGenError> {
        for descriptor in self.descriptor_lookup.values() {
            let method_name = format!(
                "decodeTabular{}",
                Self::sanitize_identifier(&descriptor.name)
            );
            let schema_info = self
                .object_schema_map
                .get(&descriptor.name)
                .ok_or_else(|| CodeGenError::UnsupportedConstruct {
                    construct: format!(
                        "レコード {} のスキーマ情報が見つかりません",
                        descriptor.name
                    ),
                    span: Some(self.declaration.span.clone()),
                })?
                .clone();

            self.builder.push_line(&format!(
                "private static {} {}(java.util.Map<String, String> row, int rowIndex) {{",
                descriptor.name, method_name
            ));
            self.builder.indent();

            let mut field_names = Vec::new();
            for field in &descriptor.fields {
                let type_str = self.generator.generate_type(&field.java_type)?;
                let raw_var = format!("{}Raw", field.name);
                let label_var = format!("{}Label", field.name);
                self.builder.push_line(&format!(
                    "String {} = row.get(\"{}\");",
                    raw_var, field.name
                ));
                self.builder.push_line(&format!(
                    "String {} = String.format(java.util.Locale.ROOT, \"列 '{}' (row %d)\", rowIndex + 1);",
                    label_var,
                    field.name
                ));

                let field_schema = schema_info.fields.get(&field.name).ok_or_else(|| {
                    CodeGenError::UnsupportedConstruct {
                        construct: format!(
                            "フィールド {} のスキーマ情報が見つかりません",
                            field.name
                        ),
                        span: Some(self.declaration.span.clone()),
                    }
                })?;

                if field.is_optional {
                    self.builder
                        .push_line(&format!("{} {};", type_str, field.name));
                    self.builder
                        .push_line(&format!("if (isEmpty({})) {{", raw_var));
                    self.builder.indent();
                    self.builder
                        .push_line(&format!("{} = java.util.Optional.empty();", field.name));
                    self.builder.dedent();
                    self.builder.push_line("} else {");
                    self.builder.indent();
                    let inner_type =
                        JavaCodeGenerator::unwrap_optional_java_type(&field.java_type).0;
                    let expr = self.tabular_value_expression(
                        &raw_var,
                        field_schema,
                        inner_type,
                        &label_var,
                    )?;
                    self.builder.push_line(&format!(
                        "{} = java.util.Optional.of({});",
                        field.name, expr
                    ));
                    self.builder.dedent();
                    self.builder.push_line("}");
                } else {
                    self.builder.push_line(&format!(
                        "if ({} == null || {}.trim().isEmpty()) {{ throw new IllegalArgumentException({} + \" は必須です\"); }}",
                        raw_var, raw_var, label_var
                    ));
                    let expr = self.tabular_value_expression(
                        &raw_var,
                        field_schema,
                        &field.java_type,
                        &label_var,
                    )?;
                    self.builder
                        .push_line(&format!("{} {} = {};", type_str, field.name, expr));
                }
                field_names.push(field.name.clone());
            }

            let args = field_names.join(", ");
            self.builder
                .push_line(&format!("return new {}({});", descriptor.name, args));
            self.builder.dedent();
            self.builder.push_line("}");
            self.builder.push_line("");
        }
        Ok(())
    }
}

fn unwrap_schema_optional<'a>(schema: &'a Schema) -> (&'a Schema, bool) {
    match schema {
        Schema::Optional(inner) => (inner, true),
        Schema::Union(variants) => {
            let mut non_null = variants
                .iter()
                .filter(|variant| !matches!(variant, Schema::Primitive(PrimitiveType::Null)))
                .collect::<Vec<_>>();
            if non_null.len() == 1 && variants.len() == 2 {
                (non_null.remove(0), true)
            } else if variants
                .iter()
                .any(|variant| matches!(variant, Schema::Primitive(PrimitiveType::Null)))
            {
                (schema, true)
            } else {
                (schema, false)
            }
        }
        _ => (schema, false),
    }
}
