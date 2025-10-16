#![cfg(test)]

use super::*;
use crate::transform::transform_program;
use crate::types::{
    IrExpression, IrImport, IrImportDetail, IrModifiers, IrProgram, IrStatement, IrVisibility,
    JavaType,
};
use jv_ast::{
    Expression, Literal, Modifiers, Program, Span, Statement, TypeAnnotation, ValBindingOrigin,
    Visibility,
};

fn span() -> Span {
    Span::new(1, 0, 1, 5)
}

fn sample_program() -> Program {
    let span = span();
    let mut modifiers = Modifiers::default();
    modifiers.visibility = Visibility::Internal;

    Program {
        package: Some("demo.example".to_string()),
        imports: Vec::new(),
        statements: vec![
            Statement::ValDeclaration {
                name: "answer".to_string(),
                type_annotation: Some(TypeAnnotation::Simple("Int".to_string())),
                initializer: Expression::Literal(Literal::Number("42".to_string()), span.clone()),
                modifiers: modifiers.clone(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: span.clone(),
            },
            Statement::Expression {
                expr: Expression::Identifier("answer".to_string(), span.clone()),
                span: span.clone(),
            },
        ],
        span,
    }
}

#[test]
fn reconstructs_basic_program_with_variable_and_expression() {
    let program = IrProgram {
        package: Some("com.example".to_string()),
        imports: vec![IrStatement::Import(IrImport {
            original: "java.util.List".to_string(),
            alias: None,
            detail: IrImportDetail::Type {
                fqcn: "java.util.List".to_string(),
            },
            module_dependency: None,
            span: span(),
        })],
        type_declarations: vec![
            IrStatement::VariableDeclaration {
                name: "answer".to_string(),
                java_type: JavaType::Primitive("int".into()),
                initializer: Some(IrExpression::Literal(Literal::Number("42".into()), span())),
                is_final: true,
                modifiers: IrModifiers {
                    visibility: IrVisibility::Public,
                    ..IrModifiers::default()
                },
                span: span(),
            },
            IrStatement::Expression {
                expr: IrExpression::Identifier {
                    name: "answer".into(),
                    java_type: JavaType::Primitive("int".into()),
                    span: span(),
                },
                span: span(),
            },
        ],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: span(),
    };

    let rebuilder = IrAstRebuilder::default();
    let result = rebuilder
        .reconstruct_program(&program)
        .expect("should rebuild");

    assert_eq!(result.program.package.as_deref(), Some("com.example"));
    assert_eq!(result.program.imports.len(), 1);
    assert_eq!(result.program.statements.len(), 2);

    match &result.program.statements[0] {
        Statement::ValDeclaration {
            name, initializer, ..
        } => {
            assert_eq!(name, "answer");
            match initializer {
                Expression::Literal(Literal::Number(value), _) => assert_eq!(value, "42"),
                other => panic!("unexpected initializer: {other:?}"),
            }
        }
        other => panic!("expected val declaration, found {other:?}"),
    }

    assert!(result.warnings.is_empty(), "warnings were not expected");
    assert_eq!(
        result.stats.reconstructed_nodes,
        result.stats.total_nodes - result.stats.placeholder_nodes
    );
}

#[test]
fn round_trip_reconstruction_matches_transform_output() {
    let ast_program = sample_program();
    let ir_program = transform_program(ast_program.clone()).expect("transform IR");

    let rebuilder = IrAstRebuilder::default();
    let rebuilt = rebuilder
        .reconstruct_program(&ir_program)
        .expect("reconstruction succeeds");

    assert!(rebuilt.warnings.is_empty());
    assert_eq!(rebuilt.program.package, ast_program.package);
    assert_eq!(rebuilt.program.statements.len(), 2);

    match &rebuilt.program.statements[0] {
        Statement::ValDeclaration {
            name,
            type_annotation,
            initializer,
            modifiers,
            ..
        } => {
            assert_eq!(name, "answer");
            assert_eq!(
                type_annotation,
                &Some(TypeAnnotation::Simple("int".to_string()))
            );
            match initializer {
                Expression::Literal(Literal::Number(value), _) => assert_eq!(value, "42"),
                other => panic!("unexpected initializer: {other:?}"),
            }
            assert!(modifiers.is_final);
        }
        other => panic!("expected val declaration, got {other:?}"),
    }

    match &rebuilt.program.statements[1] {
        Statement::Expression { expr, .. } => match expr {
            Expression::Identifier(name, _) => assert_eq!(name, "answer"),
            other => panic!("unexpected expression: {other:?}"),
        },
        other => panic!("expected expression statement, got {other:?}"),
    }

    let round_trip_ir = transform_program(rebuilt.program.clone()).expect("round trip transform");
    assert_eq!(
        round_trip_ir.type_declarations,
        ir_program.type_declarations
    );
}

#[test]
fn emits_warning_for_missing_final_initializer() {
    let program = IrProgram {
        package: None,
        imports: vec![],
        type_declarations: vec![IrStatement::VariableDeclaration {
            name: "missing".to_string(),
            java_type: JavaType::Primitive("int".into()),
            initializer: None,
            is_final: true,
            modifiers: IrModifiers::default(),
            span: span(),
        }],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: span(),
    };

    let rebuilder = IrAstRebuilder::default();
    let result = rebuilder
        .reconstruct_program(&program)
        .expect("reconstruction should succeed with placeholder");

    let missing_metadata_count = result
        .warnings
        .iter()
        .filter(|warning| warning.kind == WarningKind::MissingMetadata)
        .count();
    assert_eq!(missing_metadata_count, 2);
    assert_eq!(result.stats.placeholder_nodes, 1);

    let statement = result
        .program
        .statements
        .first()
        .expect("placeholder statement present");
    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Literal(Literal::Null, _) => {}
            other => panic!("expected null placeholder, got {other:?}"),
        },
        other => panic!("expected val declaration, got {other:?}"),
    }
}

#[test]
fn errors_when_placeholders_disallowed() {
    let program = IrProgram {
        package: None,
        imports: vec![],
        type_declarations: vec![IrStatement::VariableDeclaration {
            name: "missing".to_string(),
            java_type: JavaType::Primitive("int".into()),
            initializer: None,
            is_final: true,
            modifiers: IrModifiers::default(),
            span: span(),
        }],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: span(),
    };

    let rebuilder = IrAstRebuilder::new(ReconstructionOptions {
        allow_placeholders: false,
        ..ReconstructionOptions::default()
    });

    let err = rebuilder
        .reconstruct_program(&program)
        .expect_err("placeholders should be rejected");

    assert!(matches!(err, ReconstructionError::InsufficientMetadata));
}

#[test]
fn produces_warning_for_unsupported_statement() {
    let program = IrProgram {
        package: None,
        imports: vec![],
        type_declarations: vec![IrStatement::While {
            condition: IrExpression::Literal(Literal::Boolean(true), span()),
            body: Box::new(IrStatement::Expression {
                expr: IrExpression::Literal(Literal::Number("1".into()), span()),
                span: span(),
            }),
            span: span(),
        }],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: span(),
    };

    let rebuilder = IrAstRebuilder::default();
    let result = rebuilder
        .reconstruct_program(&program)
        .expect("should rebuild with placeholder");

    assert_eq!(result.warnings.len(), 1);
    assert_eq!(result.stats.placeholder_nodes, 1);
    assert!(matches!(
        result.program.statements[0],
        Statement::Expression { .. }
    ));
    assert_eq!(result.warnings[0].kind, WarningKind::UnsupportedNode);
}
