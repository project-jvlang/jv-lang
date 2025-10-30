use std::collections::{BTreeSet, HashMap};
use std::sync::OnceLock;

use jv_ast::{
    Annotation, AnnotationName, Program, Property, ReservedConflictKind, Statement,
    detect_reserved_conflicts,
};

use crate::CheckError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnnotationSite {
    Type,
    Method,
    Field,
    Parameter,
}

impl AnnotationSite {
    fn display_name(self) -> &'static str {
        match self {
            AnnotationSite::Type => "type",
            AnnotationSite::Method => "method",
            AnnotationSite::Field => "field",
            AnnotationSite::Parameter => "parameter",
        }
    }
}

#[derive(Debug, Clone)]
pub struct AnnotationTargetRule {
    allowed_sites: BTreeSet<AnnotationSite>,
}

impl AnnotationTargetRule {
    fn new(sites: &[AnnotationSite]) -> Self {
        let mut allowed = BTreeSet::new();
        for site in sites {
            allowed.insert(*site);
        }
        Self {
            allowed_sites: allowed,
        }
    }

    fn allows(&self, site: AnnotationSite) -> bool {
        self.allowed_sites.contains(&site)
    }

    fn allowed_sites(&self) -> impl Iterator<Item = AnnotationSite> + '_ {
        self.allowed_sites.iter().copied()
    }
}

static TARGET_RULES: OnceLock<HashMap<&'static str, AnnotationTargetRule>> = OnceLock::new();

fn rules() -> &'static HashMap<&'static str, AnnotationTargetRule> {
    TARGET_RULES.get_or_init(|| {
        use AnnotationSite::*;

        let mut map = HashMap::new();

        // Java standard annotations
        insert_rule(&mut map, &["java.lang.Override", "Override"], &[Method]);
        insert_rule(
            &mut map,
            &["java.lang.Deprecated", "Deprecated"],
            &[Type, Method, Field, Parameter],
        );
        insert_rule(
            &mut map,
            &["java.lang.SuppressWarnings", "SuppressWarnings"],
            &[Type, Method, Field, Parameter],
        );

        // Spring Framework
        insert_rule(
            &mut map,
            &["org.springframework.stereotype.Component", "Component"],
            &[Type],
        );
        insert_rule(
            &mut map,
            &["org.springframework.stereotype.Service", "Service"],
            &[Type],
        );
        insert_rule(
            &mut map,
            &["org.springframework.stereotype.Repository", "Repository"],
            &[Type],
        );
        insert_rule(
            &mut map,
            &[
                "org.springframework.beans.factory.annotation.Autowired",
                "Autowired",
            ],
            &[Field, Method, Parameter],
        );
        insert_rule(
            &mut map,
            &[
                "org.springframework.beans.factory.annotation.Qualifier",
                "Qualifier",
            ],
            &[Field, Method, Parameter],
        );

        // Jakarta Persistence / Hibernate
        insert_rule(
            &mut map,
            &[
                "jakarta.persistence.Entity",
                "javax.persistence.Entity",
                "Entity",
            ],
            &[Type],
        );
        insert_rule(
            &mut map,
            &[
                "jakarta.persistence.Table",
                "javax.persistence.Table",
                "Table",
            ],
            &[Type],
        );
        insert_rule(
            &mut map,
            &[
                "jakarta.persistence.Column",
                "javax.persistence.Column",
                "Column",
            ],
            &[Field],
        );
        insert_rule(
            &mut map,
            &["jakarta.persistence.Id", "javax.persistence.Id", "Id"],
            &[Field],
        );

        // JUnit 5
        insert_rule(&mut map, &["org.junit.jupiter.api.Test", "Test"], &[Method]);
        insert_rule(
            &mut map,
            &["org.junit.jupiter.api.BeforeEach", "BeforeEach"],
            &[Method],
        );
        insert_rule(
            &mut map,
            &["org.junit.jupiter.api.AfterEach", "AfterEach"],
            &[Method],
        );
        insert_rule(
            &mut map,
            &[
                "org.junit.jupiter.params.ParameterizedTest",
                "ParameterizedTest",
            ],
            &[Method],
        );

        // Null-safety annotations
        insert_rule(
            &mut map,
            &[
                "javax.annotation.Nullable",
                "jakarta.annotation.Nullable",
                "org.jetbrains.annotations.Nullable",
                "org.checkerframework.checker.nullness.qual.Nullable",
                "org.springframework.lang.Nullable",
                "Nullable",
            ],
            &[Field, Method, Parameter],
        );
        insert_rule(
            &mut map,
            &[
                "javax.annotation.Nonnull",
                "jakarta.annotation.Nonnull",
                "org.jetbrains.annotations.NotNull",
                "org.jetbrains.annotations.NonNull",
                "org.checkerframework.checker.nullness.qual.NonNull",
                "org.springframework.lang.NonNull",
                "NonNull",
                "NotNull",
            ],
            &[Field, Method, Parameter],
        );

        map
    })
}

fn insert_rule(
    map: &mut HashMap<&'static str, AnnotationTargetRule>,
    names: &[&'static str],
    sites: &[AnnotationSite],
) {
    let rule = AnnotationTargetRule::new(sites);
    for name in names {
        map.insert(*name, rule.clone());
    }
}

pub fn validate_program(program: &Program) -> Vec<CheckError> {
    let mut errors = Vec::new();
    for statement in &program.statements {
        validate_statement(statement, &mut errors);
    }
    errors
}

fn validate_statement(statement: &Statement, errors: &mut Vec<CheckError>) {
    match statement {
        Statement::ClassDeclaration {
            modifiers,
            properties,
            methods,
            ..
        } => {
            check_annotations(&modifiers.annotations, AnnotationSite::Type, errors);
            for property in properties {
                validate_property(property, errors);
            }
            for method in methods {
                validate_statement(method, errors);
            }
        }
        Statement::InterfaceDeclaration {
            modifiers,
            methods,
            properties,
            ..
        } => {
            check_annotations(&modifiers.annotations, AnnotationSite::Type, errors);
            for property in properties {
                validate_property(property, errors);
            }
            for method in methods {
                validate_statement(method, errors);
            }
        }
        Statement::DataClassDeclaration { modifiers, .. } => {
            check_annotations(&modifiers.annotations, AnnotationSite::Type, errors);
        }
        Statement::ExtensionFunction(ext) => {
            validate_statement(&ext.function, errors);
        }
        Statement::FunctionDeclaration { modifiers, .. } => {
            check_annotations(&modifiers.annotations, AnnotationSite::Method, errors);
        }
        Statement::ValDeclaration { modifiers, .. }
        | Statement::VarDeclaration { modifiers, .. } => {
            check_annotations(&modifiers.annotations, AnnotationSite::Field, errors);
        }
        Statement::Comment(_)
        | Statement::Expression { .. }
        | Statement::Return { .. }
        | Statement::Throw { .. }
        | Statement::Assignment { .. }
        | Statement::ForIn(_)
        | Statement::Break(_)
        | Statement::Continue(_)
        | Statement::Import { .. }
        | Statement::Package { .. }
        | Statement::Concurrency(_)
        | Statement::ResourceManagement(_) => {}
    }
}

fn validate_property(property: &Property, errors: &mut Vec<CheckError>) {
    check_annotations(
        &property.modifiers.annotations,
        AnnotationSite::Field,
        errors,
    );
}

fn check_annotations(
    annotations: &[Annotation],
    site: AnnotationSite,
    errors: &mut Vec<CheckError>,
) {
    for annotation in annotations {
        if let Some(rule) = lookup_rule(&annotation.name) {
            if !rule.allows(site) {
                let allowed = rule
                    .allowed_sites()
                    .map(|s| s.display_name())
                    .collect::<Vec<_>>()
                    .join(", ");
                let message = format!(
                    "Annotation @{} cannot target {}; allowed targets: {}",
                    annotation.name.simple_name(),
                    site.display_name(),
                    allowed
                );
                errors.push(CheckError::ValidationError {
                    message,
                    span: Some(annotation.span.clone()),
                });
            }
        }
    }

    for conflict in detect_reserved_conflicts(annotations) {
        let message = match conflict.kind {
            ReservedConflictKind::DuplicateUsage => format!(
                "Reserved annotation @{} may only be used once per declaration",
                conflict.primary.name.simple_name()
            ),
            ReservedConflictKind::MutualExclusion { other } => {
                let other_name = other;
                format!(
                    "Reserved annotations @{} and @{} cannot be used together",
                    conflict.primary.name.simple_name(),
                    other_name
                )
            }
            ReservedConflictKind::NameShadowing { reserved } => format!(
                "Annotation @{} conflicts with reserved jv annotation @{}",
                conflict.primary.name.simple_name(),
                reserved
            ),
        };
        errors.push(CheckError::ValidationError {
            message,
            span: Some(conflict.primary.span.clone()),
        });
    }
}

fn lookup_rule(name: &AnnotationName) -> Option<&'static AnnotationTargetRule> {
    let rules = rules();
    let qualified = name.qualified_name();
    rules
        .get(qualified.as_str())
        .or_else(|| rules.get(name.simple_name()))
}
