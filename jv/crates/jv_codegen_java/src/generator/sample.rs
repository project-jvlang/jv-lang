use super::*;

impl JavaCodeGenerator {
    pub(super) fn generate_sample_declaration_records(
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
}
