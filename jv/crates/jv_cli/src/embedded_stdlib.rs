use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::{Cursor, Read};
use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock};

use anyhow::{Result, anyhow};
use jv_ast::{
    Argument, CallArgumentMetadata, Expression, JsonLiteral, JsonValue, Program, Statement,
    StringPart, Visibility,
    types::{Kind, Pattern},
};
use jv_build::{JavaTarget, metadata::SymbolIndex};
use jv_checker::diagnostics::{
    DiagnosticSeverity, DiagnosticStrategy, from_check_error, from_frontend_diagnostics,
    from_parse_error, from_transform_error,
};
use jv_checker::imports::resolution::{ResolvedImport, ResolvedImportKind};
use jv_checker::{ParallelInferenceConfig, TypeChecker};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator};
use jv_fmt::JavaFormatter;
use jv_ir::{
    IrGenericMetadata, IrProgram, IrStatement, IrTypeLevelValue, IrTypeParameter, IrVariance,
    JavaType, JavaWildcardKind, TransformContext, transform_program_with_context,
};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;
use serde::Serialize;

use crate::java_type_names::derive_type_name;
use crate::pipeline::generics::apply_type_facts;
use crate::tooling_failure;
use tracing::warn;

mod bundled_stdlib {
    include!(concat!(env!("OUT_DIR"), "/embedded_stdlib_data.rs"));
}

const EMBEDDED_STDLIB_ROOT: &str = "embedded-stdlib";

struct StdlibModule {
    path: PathBuf,
    source: String,
    script_main_class: String,
    package: Option<String>,
    dependencies: Vec<String>,
    metadata: StdlibModuleMetadata,
    emit_java: bool,
}

#[derive(Default)]
struct StdlibModuleMetadata {
    functions: BTreeSet<String>,
    extension_methods: BTreeSet<String>,
    type_names: BTreeSet<String>,
}

#[derive(Default)]
pub struct StdlibCatalog {
    packages: BTreeSet<String>,
    functions: BTreeMap<String, BTreeSet<String>>,
    functions_fq: BTreeMap<String, BTreeSet<String>>,
    extension_methods: BTreeMap<String, BTreeSet<String>>,
    types: BTreeMap<String, BTreeSet<String>>,
    types_fq: BTreeMap<String, BTreeSet<String>>,
}

struct StdlibInventory {
    modules: Vec<StdlibModule>,
    catalog: StdlibCatalog,
}

static STDLIB_INVENTORY: OnceLock<StdlibInventory> = OnceLock::new();

fn stdlib_inventory() -> Result<&'static StdlibInventory> {
    if let Some(inventory) = STDLIB_INVENTORY.get() {
        return Ok(inventory);
    }
    let inventory = collect_stdlib_inventory()?;
    let _ = STDLIB_INVENTORY.set(inventory);
    Ok(STDLIB_INVENTORY
        .get()
        .expect("stdlib inventory initialized"))
}

pub fn stdlib_catalog() -> Result<&'static StdlibCatalog> {
    Ok(&stdlib_inventory()?.catalog)
}

#[derive(Debug, Default, Clone)]
pub struct StdlibUsage {
    packages: BTreeSet<String>,
}

pub fn rewrite_collection_property_access(program: &mut Program) {
    for statement in &mut program.statements {
        rewrite_statement(statement);
    }
}

fn promote_stdlib_visibility(program: &mut Program) {
    for statement in &mut program.statements {
        promote_visibility(statement);
    }
}

fn promote_visibility(statement: &mut Statement) {
    match statement {
        Statement::FunctionDeclaration { modifiers, .. }
        | Statement::DataClassDeclaration { modifiers, .. } => promote_modifiers(modifiers),
        Statement::ClassDeclaration {
            modifiers, methods, ..
        }
        | Statement::InterfaceDeclaration {
            modifiers, methods, ..
        } => {
            promote_modifiers(modifiers);
            for method in methods {
                promote_visibility(method);
            }
        }
        Statement::ExtensionFunction(extension) => {
            promote_visibility(extension.function.as_mut());
        }
        Statement::Expression { .. }
        | Statement::Return { .. }
        | Statement::Throw { .. }
        | Statement::Assignment { .. }
        | Statement::ValDeclaration { .. }
        | Statement::VarDeclaration { .. }
        | Statement::ForIn(_)
        | Statement::Concurrency(_)
        | Statement::ResourceManagement(_)
        | Statement::Comment(_)
        | Statement::Import { .. }
        | Statement::Package { .. }
        | Statement::Break(_)
        | Statement::Continue(_) => {}
    }
}

fn promote_modifiers(modifiers: &mut jv_ast::Modifiers) {
    if matches!(modifiers.visibility, Visibility::Private) {
        modifiers.visibility = Visibility::Public;
    }
}

fn rewrite_statement(statement: &mut Statement) {
    match statement {
        Statement::ValDeclaration { initializer, .. } => rewrite_expression(initializer),
        Statement::VarDeclaration { initializer, .. } => {
            if let Some(expr) = initializer {
                rewrite_expression(expr);
            }
        }
        Statement::FunctionDeclaration {
            parameters, body, ..
        } => {
            for parameter in parameters {
                if let Some(default) = &mut parameter.default_value {
                    rewrite_expression(default);
                }
            }
            rewrite_expression(body.as_mut());
        }
        Statement::ClassDeclaration {
            properties,
            methods,
            ..
        } => {
            for property in properties {
                if let Some(initializer) = &mut property.initializer {
                    rewrite_expression(initializer);
                }
                if let Some(getter) = &mut property.getter {
                    rewrite_expression(getter.as_mut());
                }
                if let Some(setter) = &mut property.setter {
                    rewrite_expression(setter.as_mut());
                }
            }
            for method in methods {
                rewrite_statement(method.as_mut());
            }
        }
        Statement::DataClassDeclaration { parameters, .. } => {
            for parameter in parameters {
                if let Some(default) = &mut parameter.default_value {
                    rewrite_expression(default);
                }
            }
        }
        Statement::InterfaceDeclaration {
            methods,
            properties,
            ..
        } => {
            for property in properties {
                if let Some(initializer) = &mut property.initializer {
                    rewrite_expression(initializer);
                }
                if let Some(getter) = &mut property.getter {
                    rewrite_expression(getter.as_mut());
                }
                if let Some(setter) = &mut property.setter {
                    rewrite_expression(setter.as_mut());
                }
            }
            for method in methods {
                rewrite_statement(method.as_mut());
            }
        }
        Statement::ExtensionFunction(extension) => {
            rewrite_statement(extension.function.as_mut());
        }
        Statement::Expression { expr, .. } => rewrite_expression(expr),
        Statement::Return { value, .. } => {
            if let Some(expr) = value {
                rewrite_expression(expr);
            }
        }
        Statement::Throw { expr, .. } => {
            rewrite_expression(expr);
        }
        Statement::Assignment { target, value, .. } => {
            rewrite_expression(target);
            rewrite_expression(value);
        }
        Statement::ForIn(statement) => {
            rewrite_expression(&mut statement.iterable);
            rewrite_expression(statement.body.as_mut());
        }
        Statement::Concurrency(construct) => rewrite_concurrency(construct),
        Statement::ResourceManagement(resource) => rewrite_resource_management(resource),
        Statement::Comment(_)
        | Statement::Import { .. }
        | Statement::Package { .. }
        | Statement::Break(_)
        | Statement::Continue(_) => {}
    }
}

fn rewrite_concurrency(construct: &mut jv_ast::ConcurrencyConstruct) {
    match construct {
        jv_ast::ConcurrencyConstruct::Spawn { body, .. }
        | jv_ast::ConcurrencyConstruct::Async { body, .. } => rewrite_expression(body.as_mut()),
        jv_ast::ConcurrencyConstruct::Await { expr, .. } => rewrite_expression(expr.as_mut()),
    }
}

fn rewrite_resource_management(resource: &mut jv_ast::ResourceManagement) {
    match resource {
        jv_ast::ResourceManagement::Use { resource, body, .. } => {
            rewrite_expression(resource.as_mut());
            rewrite_expression(body.as_mut());
        }
        jv_ast::ResourceManagement::Defer { body, .. } => rewrite_expression(body.as_mut()),
    }
}

fn rewrite_expression(expression: &mut Expression) {
    match expression {
        Expression::Literal(_, _)
        | Expression::RegexLiteral(_)
        | Expression::Identifier(_, _)
        | Expression::This(_)
        | Expression::Super(_) => {}
        Expression::Binary { left, right, .. } => {
            rewrite_expression(left.as_mut());
            rewrite_expression(right.as_mut());
        }
        Expression::Unary { operand, .. } => rewrite_expression(operand.as_mut()),
        Expression::Call { function, args, .. } => {
            rewrite_expression(function.as_mut());
            for argument in args {
                rewrite_argument(argument);
            }
        }
        Expression::MemberAccess {
            object,
            property,
            span,
        } => {
            rewrite_expression(object.as_mut());
            if property == "size" {
                let function = Expression::MemberAccess {
                    object: Box::new((**object).clone()),
                    property: property.clone(),
                    span: span.clone(),
                };
                *expression = Expression::Call {
                    function: Box::new(function),
                    args: Vec::new(),
                    type_arguments: Vec::new(),
                    argument_metadata: CallArgumentMetadata::default(),
                    span: span.clone(),
                };
            }
        }
        Expression::NullSafeMemberAccess { object, .. } => rewrite_expression(object.as_mut()),
        Expression::IndexAccess { object, index, .. }
        | Expression::NullSafeIndexAccess { object, index, .. } => {
            rewrite_expression(object.as_mut());
            rewrite_expression(index.as_mut());
        }
        Expression::TypeCast { expr, .. } => rewrite_expression(expr.as_mut()),
        Expression::StringInterpolation { parts, .. } => {
            for part in parts {
                if let StringPart::Expression(expr) = part {
                    rewrite_expression(expr);
                }
            }
        }
        Expression::MultilineString(_) => {}
        Expression::JsonLiteral(JsonLiteral { value, .. }) => {
            rewrite_json_value(value);
        }
        Expression::When {
            expr,
            arms,
            else_arm,
            ..
        } => {
            if let Some(condition) = expr {
                rewrite_expression(condition.as_mut());
            }
            for arm in arms {
                rewrite_pattern(&mut arm.pattern);
                if let Some(guard) = &mut arm.guard {
                    rewrite_expression(guard);
                }
                rewrite_expression(&mut arm.body);
            }
            if let Some(else_branch) = else_arm {
                rewrite_expression(else_branch.as_mut());
            }
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            rewrite_expression(condition.as_mut());
            rewrite_expression(then_branch.as_mut());
            if let Some(else_branch) = else_branch {
                rewrite_expression(else_branch.as_mut());
            }
        }
        Expression::Block { statements, .. } => {
            for statement in statements {
                rewrite_statement(statement);
            }
        }
        Expression::Array { elements, .. } => {
            for element in elements {
                rewrite_expression(element);
            }
        }
        Expression::Lambda {
            parameters, body, ..
        } => {
            for param in parameters {
                if let Some(default) = &mut param.default_value {
                    rewrite_expression(default);
                }
            }
            rewrite_expression(body.as_mut());
        }
        Expression::Try {
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            rewrite_expression(body.as_mut());
            for clause in catch_clauses {
                if let Some(parameter) = &mut clause.parameter {
                    if let Some(default) = &mut parameter.default_value {
                        rewrite_expression(default);
                    }
                }
                rewrite_expression(clause.body.as_mut());
            }
            if let Some(finally_block) = finally_block {
                rewrite_expression(finally_block.as_mut());
            }
        }
    }
}

fn rewrite_argument(argument: &mut Argument) {
    match argument {
        Argument::Positional(expr) => rewrite_expression(expr),
        Argument::Named { value, .. } => rewrite_expression(value),
    }
}

fn rewrite_json_value(value: &mut JsonValue) {
    match value {
        JsonValue::Object { entries, .. } => {
            for entry in entries {
                rewrite_json_value(&mut entry.value);
            }
        }
        JsonValue::Array { elements, .. } => {
            for element in elements {
                rewrite_json_value(element);
            }
        }
        JsonValue::String { .. }
        | JsonValue::Number { .. }
        | JsonValue::Boolean { .. }
        | JsonValue::Null { .. } => {}
    }
}

fn rewrite_pattern(pattern: &mut Pattern) {
    match pattern {
        Pattern::Literal(_, _) | Pattern::Identifier(_, _) | Pattern::Wildcard(_) => {}
        Pattern::Constructor { patterns, .. } => {
            for nested in patterns {
                rewrite_pattern(nested);
            }
        }
        Pattern::Range { start, end, .. } => {
            rewrite_expression(start.as_mut());
            rewrite_expression(end.as_mut());
        }
        Pattern::Guard {
            pattern: inner,
            condition,
            ..
        } => {
            rewrite_pattern(inner.as_mut());
            rewrite_expression(condition);
        }
    }
}

impl StdlibUsage {
    pub fn from_resolved_imports(
        resolved_imports: &[ResolvedImport],
        catalog: &StdlibCatalog,
    ) -> Self {
        let mut usage = StdlibUsage::default();
        for import in resolved_imports {
            match &import.kind {
                ResolvedImportKind::Type { fqcn } => usage.record_type_reference(catalog, fqcn),
                ResolvedImportKind::Package { name } => {
                    usage.record_package_reference(catalog, name)
                }
                ResolvedImportKind::StaticMember { owner, .. } => {
                    usage.record_type_reference(catalog, owner)
                }
                ResolvedImportKind::Module { .. } => {}
            }

            if let Some(module_dependency) = import.module_dependency.as_deref() {
                usage.record_reference(catalog, module_dependency);
            }
        }
        usage
    }

    pub fn extend_with_java_imports<'a, I>(&mut self, imports: I, catalog: &StdlibCatalog)
    where
        I: IntoIterator<Item = &'a str>,
    {
        for import in imports {
            let trimmed = import.trim();
            let reference = trimmed.strip_prefix("static ").unwrap_or(trimmed);
            self.record_reference(catalog, reference);
        }
    }

    pub fn scan_java_source(&mut self, source: &str, catalog: &StdlibCatalog) {
        for token in source.split(|c: char| !(c.is_alphanumeric() || c == '.' || c == '_')) {
            if token.is_empty() {
                continue;
            }
            let mut current = token;
            loop {
                self.record_reference(catalog, current);
                if let Some((prefix, suffix)) = current.rsplit_once('.') {
                    self.record_reference(catalog, suffix);
                    current = prefix;
                } else {
                    break;
                }
            }
        }
    }

    pub fn record_program_usage(&mut self, program: &Program, catalog: &StdlibCatalog) {
        let mut detector = ProgramUsageDetector {
            usage: self,
            catalog,
        };
        detector.visit_program(program);
    }

    pub fn is_empty(&self) -> bool {
        self.packages.is_empty()
    }

    pub fn package_set(&self) -> &BTreeSet<String> {
        &self.packages
    }

    fn record_type_reference(&mut self, catalog: &StdlibCatalog, fqcn: &str) {
        let packages = catalog.packages_for_type_reference(fqcn);
        if packages.is_empty() {
            if let Some(package) = package_of(fqcn) {
                self.record_package_reference(catalog, package);
            }
            return;
        }
        self.packages.extend(packages);
    }

    fn record_package_reference(&mut self, catalog: &StdlibCatalog, package: &str) {
        if catalog.has_package(package) {
            self.packages.insert(package.to_string());
        }
    }

    fn record_reference(&mut self, catalog: &StdlibCatalog, reference: &str) {
        if reference.is_empty() {
            return;
        }

        let mut matched = false;
        for package in catalog.packages_for_reference(reference) {
            matched = true;
            self.packages.insert(package);
        }

        if matched {
            return;
        }

        if let Some(package) = package_of(reference) {
            self.record_package_reference(catalog, package);
        }
    }
}

struct ProgramUsageDetector<'a, 'b> {
    usage: &'a mut StdlibUsage,
    catalog: &'b StdlibCatalog,
}

impl<'a, 'b> ProgramUsageDetector<'a, 'b> {
    fn visit_program(&mut self, program: &Program) {
        for statement in &program.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::FunctionDeclaration { body, .. } => self.visit_expression(body),
            Statement::ClassDeclaration { methods, .. } => {
                for method in methods {
                    self.visit_statement(method);
                }
            }
            Statement::InterfaceDeclaration { methods, .. } => {
                for method in methods {
                    self.visit_statement(method);
                }
            }
            Statement::ExtensionFunction(extension) => {
                self.visit_statement(extension.function.as_ref());
            }
            Statement::Expression { expr, .. } => self.visit_expression(expr),
            Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.visit_expression(expr);
                }
            }
            Statement::Throw { expr, .. } => {
                self.visit_expression(expr);
            }
            Statement::Assignment { target, value, .. } => {
                self.visit_expression(target);
                self.visit_expression(value);
            }
            Statement::ValDeclaration { initializer, .. } => self.visit_expression(initializer),
            Statement::VarDeclaration { initializer, .. } => {
                if let Some(expr) = initializer {
                    self.visit_expression(expr);
                }
            }
            Statement::ForIn(statement) => {
                self.visit_expression(&statement.iterable);
                self.visit_expression(&statement.body);
            }
            Statement::Concurrency(construct) => self.visit_concurrency(construct),
            Statement::ResourceManagement(resource) => self.visit_resource_management(resource),
            Statement::DataClassDeclaration { .. }
            | Statement::Import { .. }
            | Statement::Package { .. }
            | Statement::Break(_)
            | Statement::Continue(_)
            | Statement::Comment(_) => {}
        }
    }

    fn visit_concurrency(&mut self, construct: &jv_ast::ConcurrencyConstruct) {
        match construct {
            jv_ast::ConcurrencyConstruct::Spawn { body, .. }
            | jv_ast::ConcurrencyConstruct::Async { body, .. } => self.visit_expression(body),
            jv_ast::ConcurrencyConstruct::Await { expr, .. } => self.visit_expression(expr),
        }
    }

    fn visit_resource_management(&mut self, resource: &jv_ast::ResourceManagement) {
        match resource {
            jv_ast::ResourceManagement::Use { resource, body, .. } => {
                self.visit_expression(resource);
                self.visit_expression(body);
            }
            jv_ast::ResourceManagement::Defer { body, .. } => self.visit_expression(body),
        }
    }

    fn visit_argument(&mut self, argument: &Argument) {
        match argument {
            Argument::Positional(expr) => self.visit_expression(expr),
            Argument::Named { value, .. } => self.visit_expression(value),
        }
    }

    fn visit_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Identifier(name, _) => {
                for package in self.catalog.packages_for_function_name(name) {
                    self.usage.packages.insert(package);
                }
            }
            Expression::Call { function, args, .. } => {
                self.visit_expression(function);
                for arg in args {
                    self.visit_argument(arg);
                }
                if let Expression::MemberAccess { property, .. }
                | Expression::NullSafeMemberAccess { property, .. } = function.as_ref()
                {
                    for package in self.catalog.packages_for_extension_method(property) {
                        self.usage.packages.insert(package);
                    }
                }
            }
            Expression::MemberAccess {
                object, property, ..
            }
            | Expression::NullSafeMemberAccess {
                object, property, ..
            } => {
                self.visit_expression(object);
                for package in self.catalog.packages_for_extension_method(property) {
                    self.usage.packages.insert(package);
                }
                for package in self.catalog.packages_for_type_name(property) {
                    self.usage.packages.insert(package);
                }
            }
            Expression::Binary { left, right, .. } => {
                self.visit_expression(left);
                self.visit_expression(right);
            }
            Expression::Unary { operand, .. } => self.visit_expression(operand),
            Expression::IndexAccess { object, index, .. }
            | Expression::NullSafeIndexAccess { object, index, .. } => {
                self.visit_expression(object);
                self.visit_expression(index);
            }
            Expression::TypeCast { expr, .. } => self.visit_expression(expr),
            Expression::StringInterpolation { parts, .. } => {
                for part in parts {
                    if let StringPart::Expression(expr) = part {
                        self.visit_expression(expr);
                    }
                }
            }
            Expression::When {
                expr,
                arms,
                else_arm,
                ..
            } => {
                if let Some(expr) = expr {
                    self.visit_expression(expr);
                }
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.visit_expression(guard);
                    }
                    self.visit_expression(&arm.body);
                }
                if let Some(else_arm) = else_arm {
                    self.visit_expression(else_arm);
                }
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.visit_expression(condition);
                self.visit_expression(then_branch);
                if let Some(else_branch) = else_branch {
                    self.visit_expression(else_branch);
                }
            }
            Expression::Block { statements, .. } => {
                for statement in statements {
                    self.visit_statement(statement);
                }
            }
            Expression::Array { elements, .. } => {
                for element in elements {
                    self.visit_expression(element);
                }
            }
            Expression::Lambda {
                parameters, body, ..
            } => {
                for param in parameters {
                    if let Some(default) = &param.default_value {
                        self.visit_expression(default);
                    }
                }
                self.visit_expression(body);
            }
            Expression::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                self.visit_expression(body);
                for clause in catch_clauses {
                    if let Some(parameter) = &clause.parameter {
                        if let Some(default) = &parameter.default_value {
                            self.visit_expression(default);
                        }
                    }
                    self.visit_expression(&clause.body);
                }
                if let Some(finally_block) = finally_block {
                    self.visit_expression(finally_block);
                }
            }
            Expression::JsonLiteral(_)
            | Expression::MultilineString(_)
            | Expression::Literal(_, _)
            | Expression::RegexLiteral(_)
            | Expression::This(_)
            | Expression::Super(_) => {}
        }
    }
}

impl StdlibModuleMetadata {
    fn from_program(program: &Program) -> Self {
        let mut metadata = StdlibModuleMetadata::default();
        let mut collector = MetadataCollector {
            metadata: &mut metadata,
        };
        collector.visit_program(program);
        metadata
    }
}

struct MetadataCollector<'a> {
    metadata: &'a mut StdlibModuleMetadata,
}

impl<'a> MetadataCollector<'a> {
    fn visit_program(&mut self, program: &Program) {
        for statement in &program.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::FunctionDeclaration { name, .. } => {
                self.metadata.functions.insert(name.clone());
            }
            Statement::ExtensionFunction(extension) => {
                if let Statement::FunctionDeclaration { name, .. } = extension.function.as_ref() {
                    self.metadata.extension_methods.insert(name.clone());
                }
            }
            Statement::ClassDeclaration { name, methods, .. } => {
                self.metadata.type_names.insert(name.clone());
                for method in methods {
                    self.visit_statement(method);
                }
            }
            Statement::InterfaceDeclaration { name, methods, .. } => {
                self.metadata.type_names.insert(name.clone());
                for method in methods {
                    self.visit_statement(method);
                }
            }
            Statement::DataClassDeclaration { name, .. } => {
                self.metadata.type_names.insert(name.clone());
            }
            _ => {}
        }
    }
}
impl StdlibCatalog {
    fn register_module(&mut self, package: &str, metadata: &StdlibModuleMetadata) {
        self.packages.insert(package.to_string());
        for name in &metadata.functions {
            self.functions
                .entry(name.clone())
                .or_default()
                .insert(package.to_string());
            self.functions_fq
                .entry(format!("{package}.{name}"))
                .or_default()
                .insert(package.to_string());
        }
        for name in &metadata.extension_methods {
            self.extension_methods
                .entry(name.clone())
                .or_default()
                .insert(package.to_string());
        }
        for name in &metadata.type_names {
            self.types
                .entry(name.clone())
                .or_default()
                .insert(package.to_string());
            self.types_fq
                .entry(format!("{package}.{name}"))
                .or_default()
                .insert(package.to_string());
        }
    }

    pub fn fully_qualified_type_names(&self) -> impl Iterator<Item = &str> + '_ {
        self.types_fq.keys().map(String::as_str)
    }

    fn has_package(&self, package: &str) -> bool {
        self.packages.contains(package)
    }

    fn packages_for_function_name(&self, name: &str) -> BTreeSet<String> {
        self.functions.get(name).cloned().unwrap_or_default()
    }

    fn packages_for_extension_method(&self, name: &str) -> BTreeSet<String> {
        self.extension_methods
            .get(name)
            .cloned()
            .unwrap_or_default()
    }

    fn packages_for_type_name(&self, name: &str) -> BTreeSet<String> {
        self.types.get(name).cloned().unwrap_or_default()
    }

    fn packages_for_type_reference(&self, reference: &str) -> BTreeSet<String> {
        self.types_fq
            .get(reference)
            .cloned()
            .unwrap_or_else(|| self.types.get(reference).cloned().unwrap_or_default())
    }

    fn packages_for_reference(&self, reference: &str) -> BTreeSet<String> {
        if self.packages.contains(reference) {
            return [reference.to_string()].into_iter().collect();
        }
        if let Some(packages) = self.functions_fq.get(reference) {
            return packages.clone();
        }
        if let Some(packages) = self.types_fq.get(reference) {
            return packages.clone();
        }
        if let Some(packages) = self.functions.get(reference) {
            return packages.clone();
        }
        if let Some(packages) = self.extension_methods.get(reference) {
            return packages.clone();
        }
        if let Some(packages) = self.types.get(reference) {
            return packages.clone();
        }

        if let Some((package, _)) = reference.rsplit_once('.') {
            if self.packages.contains(package) {
                return [package.to_string()].into_iter().collect();
            }
        }

        BTreeSet::new()
    }
}

#[derive(Debug, Serialize)]
struct GenericManifest {
    module: String,
    package: Option<String>,
    declarations: Vec<GenericManifestEntry>,
}

#[derive(Debug, Serialize)]
struct GenericManifestEntry {
    qualified_name: String,
    type_parameters: Vec<ManifestTypeParameter>,
    const_parameters: BTreeMap<String, IrTypeLevelValue>,
    type_level_bindings: BTreeMap<String, IrTypeLevelValue>,
}

#[derive(Debug, Serialize)]
struct ManifestTypeParameter {
    name: String,
    variance: IrVariance,
    bounds: Vec<String>,
    permits: Vec<String>,
    kind: Option<Kind>,
}

#[derive(Debug, Default)]
pub struct StdlibCompilationArtifacts {
    pub java_files: Vec<PathBuf>,
    pub metadata_files: Vec<PathBuf>,
}

#[derive(Debug, Default)]
struct StdlibModuleArtifacts {
    java_files: Vec<PathBuf>,
    metadata_files: Vec<PathBuf>,
}

pub fn compile_stdlib_modules(
    output_dir: &Path,
    target: JavaTarget,
    format: bool,
    parallel_config: ParallelInferenceConfig,
    usage: &StdlibUsage,
    symbol_index: Arc<SymbolIndex>,
) -> Result<StdlibCompilationArtifacts> {
    let inventory = stdlib_inventory()?;
    let modules = &inventory.modules;
    if usage.is_empty() {
        return Ok(StdlibCompilationArtifacts::default());
    }

    let required_indices = resolve_required_modules(modules, usage);
    if required_indices.is_empty() {
        return Ok(StdlibCompilationArtifacts::default());
    }

    let mut compilation_artifacts = StdlibCompilationArtifacts::default();

    for (index, module) in modules.iter().enumerate() {
        if !required_indices.contains(&index) || !module.emit_java {
            continue;
        }

        let module_artifacts = compile_module(
            module,
            output_dir,
            target,
            format,
            parallel_config,
            &symbol_index,
        )?;
        compilation_artifacts
            .java_files
            .extend(module_artifacts.java_files);
        compilation_artifacts
            .metadata_files
            .extend(module_artifacts.metadata_files);
    }

    Ok(compilation_artifacts)
}

fn collect_stdlib_inventory() -> Result<StdlibInventory> {
    let mut modules = Vec::new();
    let mut catalog = StdlibCatalog::default();
    visit_embedded_stdlib(&mut modules, &mut catalog)?;
    Ok(StdlibInventory { modules, catalog })
}

fn visit_embedded_stdlib(
    modules: &mut Vec<StdlibModule>,
    catalog: &mut StdlibCatalog,
) -> Result<()> {
    let pipeline = RowanPipeline::default();
    #[cfg(feature = "dump-sequence-ast")]
    let debug_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib");
    let cursor = Cursor::new(bundled_stdlib::STDLIB_ZIP);
    let mut archive = zip::ZipArchive::new(cursor)?;
    let mut entries = Vec::new();

    for index in 0..archive.len() {
        let mut file = archive.by_index(index)?;

        if file.is_dir() {
            continue;
        }

        if !file.name().ends_with(".jv") {
            continue;
        }

        if file
            .name()
            .split('/')
            .any(|segment| segment.eq_ignore_ascii_case("tests"))
        {
            continue;
        }

        let mut source = String::new();
        file.read_to_string(&mut source)?;
        let relative_path = PathBuf::from(file.name());
        entries.push((relative_path, source));
    }

    entries.sort_by(|(left, _), (right, _)| left.cmp(right));

    for (relative_path, source) in entries {
        let virtual_path = Path::new(EMBEDDED_STDLIB_ROOT).join(&relative_path);
        let frontend_output = pipeline.parse(&source).map_err(|error| {
            anyhow!(
                "Failed to parse embedded stdlib module {}: {:?}",
                virtual_path.display(),
                error
            )
        })?;
        let frontend_diagnostics =
            from_frontend_diagnostics(frontend_output.diagnostics().final_diagnostics());
        if let Some(error_diag) = frontend_diagnostics
            .iter()
            .find(|diag| diag.severity == DiagnosticSeverity::Error)
        {
            let rendered = error_diag
                .clone()
                .with_strategy(DiagnosticStrategy::Deferred);
            return Err(anyhow!(
                "{}",
                crate::format_tooling_diagnostic(&virtual_path, &rendered)
            ));
        }
        for diagnostic in frontend_diagnostics {
            let rendered = diagnostic
                .clone()
                .with_strategy(DiagnosticStrategy::Deferred);
            warn!(
                target: "jv::stdlib",
                "{}", crate::format_tooling_diagnostic(&virtual_path, &rendered)
            );
        }
        let mut program = frontend_output.into_program();
        promote_stdlib_visibility(&mut program);
        #[cfg(feature = "dump-sequence-ast")]
        if relative_path.ends_with("sequence.jv") {
            let dump_path = debug_root.join("../debug-sequence-ast.json");
            if let Ok(json) = serde_json::to_string_pretty(&program) {
                let _ = fs::write(&dump_path, json);
            }
        }
        let package = program.package.clone();
        let script_main_class = derive_script_name(&virtual_path, package.as_deref());
        let mut metadata = StdlibModuleMetadata::from_program(&program);
        if !metadata.type_names.contains(&script_main_class) {
            metadata.type_names.insert(script_main_class.clone());
        }
        let dependencies = extract_stdlib_dependencies(&program);
        let module = StdlibModule {
            path: virtual_path,
            source,
            script_main_class,
            package,
            dependencies,
            metadata,
            emit_java: true,
        };

        if let Some(pkg) = module.package.as_deref() {
            catalog.register_module(pkg, &module.metadata);
        }

        modules.push(module);
    }

    Ok(())
}

fn resolve_required_modules(modules: &[StdlibModule], usage: &StdlibUsage) -> BTreeSet<usize> {
    let mut required = usage.package_set().clone();
    if required.is_empty() {
        return BTreeSet::new();
    }

    let mut selected = BTreeSet::new();
    let mut changed = true;

    while changed {
        changed = false;

        for (index, module) in modules.iter().enumerate() {
            if selected.contains(&index) {
                continue;
            }

            let package = match module.package.as_deref() {
                Some(pkg) => pkg,
                None => continue,
            };

            if !required.contains(package) {
                continue;
            }

            selected.insert(index);
            changed = true;

            for dependency in &module.dependencies {
                if required.insert(dependency.clone()) {
                    changed = true;
                }
            }
        }
    }

    selected
}

fn extract_stdlib_dependencies(program: &Program) -> Vec<String> {
    let mut dependencies = BTreeSet::new();

    for statement in &program.imports {
        if let Statement::Import { path, .. } = statement {
            if let Some(package) = stdlib_dependency_from_path(path) {
                dependencies.insert(package);
            }
        }
    }

    dependencies.into_iter().collect()
}

fn stdlib_dependency_from_path(path: &str) -> Option<String> {
    if !path.starts_with("jv.") {
        return None;
    }

    if let Some(package) = package_of(path) {
        if package.starts_with("jv.") {
            return Some(package.to_string());
        }
    }

    Some(path.to_string())
}

fn package_of(name: &str) -> Option<&str> {
    name.rsplit_once('.').map(|(package, _)| package)
}

fn derive_script_name(path: &Path, package: Option<&str>) -> String {
    if let Some(stem) = path.file_stem().and_then(|value| value.to_str()) {
        let mut name = String::new();
        for segment in stem.split(|c: char| !c.is_alphanumeric()) {
            if segment.is_empty() {
                continue;
            }
            name.push_str(&capitalize(segment));
        }
        if !name.is_empty() {
            return name;
        }
    }

    if let Some(package) = package {
        // Fallback: use the last non-empty package segment when file name is not informative.
        if let Some(segment) = package
            .split('.')
            .rev()
            .find(|segment| !segment.is_empty() && !segment.eq_ignore_ascii_case("jv"))
        {
            return capitalize(segment);
        }
    }

    "StdlibModule".to_string()
}

fn capitalize(input: &str) -> String {
    let mut chars = input.chars();
    let mut result = String::new();
    if let Some(first) = chars.next() {
        result.extend(first.to_uppercase());
    }
    result.extend(chars.flat_map(|ch| ch.to_lowercase()));
    result
}

fn compile_module(
    module: &StdlibModule,
    output_dir: &Path,
    target: JavaTarget,
    format: bool,
    parallel_config: ParallelInferenceConfig,
    symbol_index: &Arc<SymbolIndex>,
) -> Result<StdlibModuleArtifacts> {
    let pipeline = RowanPipeline::default();
    let frontend_output = match pipeline.parse(&module.source) {
        Ok(output) => output,
        Err(error) => {
            if let Some(diagnostic) = from_parse_error(&error) {
                return Err(tooling_failure(
                    module.path.as_path(),
                    diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                ));
            }
            return Err(anyhow!("Parser error: {:?}", error));
        }
    };
    let frontend_diagnostics =
        from_frontend_diagnostics(frontend_output.diagnostics().final_diagnostics());
    if let Some(error_diag) = frontend_diagnostics
        .iter()
        .find(|diag| diag.severity == DiagnosticSeverity::Error)
    {
        let rendered = error_diag
            .clone()
            .with_strategy(DiagnosticStrategy::Deferred);
        return Err(tooling_failure(module.path.as_path(), rendered));
    }
    for diagnostic in frontend_diagnostics {
        let rendered = diagnostic
            .clone()
            .with_strategy(DiagnosticStrategy::Deferred);
        warn!(
            target: "jv::stdlib",
            "{}", crate::format_tooling_diagnostic(&module.path, &rendered)
        );
    }
    let mut program = frontend_output.into_program();

    promote_stdlib_visibility(&mut program);
    let mut type_checker = TypeChecker::with_parallel_config(parallel_config);
    type_checker.set_imports(Arc::clone(symbol_index), Vec::new());
    let inference_snapshot = match type_checker.check_program(&program) {
        Ok(()) => type_checker.take_inference_snapshot(),
        Err(errors) => {
            if let Some(diagnostic) = errors.iter().find_map(from_check_error) {
                return Err(tooling_failure(
                    module.path.as_path(),
                    diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                ));
            }
            let details = errors
                .iter()
                .map(|error| error.to_string())
                .collect::<Vec<_>>()
                .join("; ");
            return Err(anyhow!("Type checking failed: {}", details));
        }
    };

    let normalized = type_checker.take_normalized_program().unwrap_or(program);

    let mut context = TransformContext::new();
    let mut ir_program = match transform_program_with_context(normalized, &mut context) {
        Ok(ir) => ir,
        Err(error) => {
            if let Some(diagnostic) = from_transform_error(&error) {
                return Err(tooling_failure(
                    module.path.as_path(),
                    diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                ));
            }
            return Err(anyhow!("IR transformation error: {:?}", error));
        }
    };

    if let Some(snapshot) = inference_snapshot.as_ref() {
        apply_type_facts(&mut ir_program, snapshot.type_facts());
    }

    let mut codegen_config = JavaCodeGenConfig::for_target(target);
    codegen_config.script_main_class = module.script_main_class.clone();

    let mut generator = JavaCodeGenerator::with_config(codegen_config);
    generator.set_symbol_index(Some(Arc::clone(symbol_index)));
    let java_unit = generator
        .generate_compilation_unit(&ir_program)
        .map_err(|error| anyhow!("Code generation error: {:?}", error))?;

    #[cfg(debug_assertions)]
    if module.path.file_name().and_then(|name| name.to_str()) == Some("sequence.jv") {
        eprintln!(
            "[debug] stdlib module {:?} emitted {} type declarations",
            module.path,
            java_unit.type_declarations.len()
        );
    }

    let formatter = JavaFormatter::default();
    let package_path = java_unit
        .package_declaration
        .as_ref()
        .map(|package| package.replace('.', "/"));

    let mut java_files = Vec::new();
    let mut metadata_files = Vec::new();

    for (index, type_decl) in java_unit.type_declarations.iter().enumerate() {
        let file_name = derive_type_name(type_decl)
            .unwrap_or_else(|| format!("StdlibModule{}{}", module.script_main_class, index));

        let mut directory = PathBuf::from(output_dir);
        if let Some(package_dir) = package_path.as_ref() {
            directory.push(package_dir);
        }
        fs::create_dir_all(&directory).map_err(|error| {
            anyhow!(
                "Failed to create directory {}: {}",
                directory.display(),
                error
            )
        })?;

        let java_path = directory.join(format!("{}.java", file_name));
        let mut java_content = String::new();

        if let Some(package) = &java_unit.package_declaration {
            java_content.push_str("package ");
            java_content.push_str(package);
            java_content.push_str(";\n\n");
        }

        if !java_unit.imports.is_empty() {
            for import in &java_unit.imports {
                java_content.push_str("import ");
                java_content.push_str(import);
                java_content.push_str(";\n");
            }
            java_content.push('\n');
        } else if java_unit.package_declaration.is_some() {
            java_content.push('\n');
        }

        java_content.push_str(type_decl);

        let java_content = if format {
            formatter
                .format_compilation_unit(&java_content)
                .unwrap_or(java_content)
        } else {
            java_content
        };

        fs::write(&java_path, java_content)
            .map_err(|error| anyhow!("Failed to write {}: {}", java_path.display(), error))?;
        java_files.push(java_path);
    }

    let manifest = GenericManifest {
        module: module.script_main_class.clone(),
        package: ir_program.package.clone(),
        declarations: collect_generic_manifest(&ir_program),
    };

    if !manifest.declarations.is_empty() {
        let manifest_json = serde_json::to_string_pretty(&manifest).map_err(|error| {
            anyhow!(
                "Failed to serialize generic manifest for {}: {}",
                module.path.display(),
                error
            )
        })?;
        let manifest_path = output_dir.join(format!("{}-generics.json", module.script_main_class));
        fs::write(&manifest_path, manifest_json)
            .map_err(|error| anyhow!("Failed to write {}: {}", manifest_path.display(), error))?;
        metadata_files.push(manifest_path);
    }

    Ok(StdlibModuleArtifacts {
        java_files,
        metadata_files,
    })
}

fn collect_generic_manifest(program: &IrProgram) -> Vec<GenericManifestEntry> {
    let mut entries = Vec::new();
    let package = program.package.as_deref();
    let mut path = Vec::new();
    for declaration in &program.type_declarations {
        collect_manifest_from_statement(
            declaration,
            package,
            &program.generic_metadata,
            &mut path,
            &mut entries,
        );
    }
    entries
}

fn collect_manifest_from_statement(
    statement: &IrStatement,
    package: Option<&str>,
    metadata_map: &BTreeMap<String, IrGenericMetadata>,
    path: &mut Vec<String>,
    entries: &mut Vec<GenericManifestEntry>,
) {
    match statement {
        IrStatement::ClassDeclaration {
            name,
            type_parameters,
            nested_classes,
            ..
        }
        | IrStatement::InterfaceDeclaration {
            name,
            type_parameters,
            nested_types: nested_classes,
            ..
        } => {
            path.push(name.clone());
            let key = manifest_key(package, path);
            let metadata_entry = metadata_map.get(&key);
            if should_emit_manifest_entry(type_parameters, metadata_entry) {
                entries.push(GenericManifestEntry {
                    qualified_name: key.clone(),
                    type_parameters: type_parameters
                        .iter()
                        .map(|param| manifest_type_parameter(param, metadata_entry))
                        .collect(),
                    const_parameters: metadata_entry
                        .map(|entry| entry.const_parameter_values.clone())
                        .unwrap_or_default(),
                    type_level_bindings: metadata_entry
                        .map(|entry| entry.type_level_bindings.clone())
                        .unwrap_or_default(),
                });
            }
            for nested in nested_classes {
                collect_manifest_from_statement(nested, package, metadata_map, path, entries);
            }
            path.pop();
        }
        IrStatement::RecordDeclaration {
            name,
            type_parameters,
            ..
        } => {
            path.push(name.clone());
            let key = manifest_key(package, path);
            let metadata_entry = metadata_map.get(&key);
            if should_emit_manifest_entry(type_parameters, metadata_entry) {
                entries.push(GenericManifestEntry {
                    qualified_name: key,
                    type_parameters: type_parameters
                        .iter()
                        .map(|param| manifest_type_parameter(param, metadata_entry))
                        .collect(),
                    const_parameters: metadata_entry
                        .map(|entry| entry.const_parameter_values.clone())
                        .unwrap_or_default(),
                    type_level_bindings: metadata_entry
                        .map(|entry| entry.type_level_bindings.clone())
                        .unwrap_or_default(),
                });
            }
            path.pop();
        }
        _ => {}
    }
}

fn should_emit_manifest_entry(
    type_parameters: &[IrTypeParameter],
    metadata_entry: Option<&IrGenericMetadata>,
) -> bool {
    if !type_parameters.is_empty() {
        return true;
    }
    metadata_entry.map(manifest_has_metadata).unwrap_or(false)
}

fn manifest_has_metadata(entry: &IrGenericMetadata) -> bool {
    !(entry.type_parameter_kinds.is_empty()
        && entry.const_parameter_values.is_empty()
        && entry.type_level_bindings.is_empty())
}

fn manifest_type_parameter(
    param: &IrTypeParameter,
    metadata_entry: Option<&IrGenericMetadata>,
) -> ManifestTypeParameter {
    let kind = param.kind.clone().or_else(|| {
        metadata_entry.and_then(|entry| entry.type_parameter_kinds.get(&param.name).cloned())
    });
    ManifestTypeParameter {
        name: param.name.clone(),
        variance: param.variance,
        bounds: param.bounds.iter().map(render_java_type).collect(),
        permits: param.permits.clone(),
        kind,
    }
}

fn manifest_key(package: Option<&str>, path: &[String]) -> String {
    let mut segments: Vec<String> = Vec::new();
    if let Some(pkg) = package {
        if !pkg.is_empty() {
            segments.extend(pkg.split('.').map(|segment| segment.to_string()));
        }
    }
    segments.extend(path.iter().cloned());
    segments.join("::")
}

fn render_java_type(java_type: &JavaType) -> String {
    match java_type {
        JavaType::Primitive(name) => name.clone(),
        JavaType::Reference { name, generic_args } => {
            if generic_args.is_empty() {
                name.clone()
            } else {
                let rendered: Vec<String> = generic_args.iter().map(render_java_type).collect();
                format!("{}<{}>", name, rendered.join(", "))
            }
        }
        JavaType::Array {
            element_type,
            dimensions,
        } => {
            let base = render_java_type(element_type);
            let suffix = "[]".repeat(*dimensions);
            format!("{}{}", base, suffix)
        }
        JavaType::Functional { interface_name, .. } => interface_name.clone(),
        JavaType::Wildcard { kind, bound } => match kind {
            JavaWildcardKind::Unbounded => "?".to_string(),
            JavaWildcardKind::Extends => {
                let ty = bound
                    .as_ref()
                    .map(|inner| render_java_type(inner))
                    .unwrap_or_else(|| "Object".to_string());
                format!("? extends {}", ty)
            }
            JavaWildcardKind::Super => {
                let ty = bound
                    .as_ref()
                    .map(|inner| render_java_type(inner))
                    .unwrap_or_else(|| "Object".to_string());
                format!("? super {}", ty)
            }
        },
        JavaType::Void => "void".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::Span;
    use jv_checker::imports::resolution::{ResolvedImport, ResolvedImportKind};
    use jv_ir::{IrModifiers, IrStatement};

    use std::{
        fs,
        path::PathBuf,
        time::{SystemTime, UNIX_EPOCH},
    };

    fn test_catalog() -> StdlibCatalog {
        let mut catalog = StdlibCatalog::default();
        let mut metadata = StdlibModuleMetadata::default();
        metadata.type_names.insert("Stream".to_string());
        metadata.type_names.insert("Sequence".to_string());
        metadata.extension_methods.insert("map".to_string());
        metadata.functions.insert("toStream".to_string());
        catalog.register_module("jv.collections", &metadata);
        catalog
    }

    #[test]
    fn stdlib_usage_collects_packages_from_resolved_imports() {
        let import = ResolvedImport {
            source_span: Span::dummy(),
            original_path: "java.util.stream.Stream".to_string(),
            alias: None,
            is_wildcard: false,
            kind: ResolvedImportKind::Type {
                fqcn: "java.util.stream.Stream".to_string(),
            },
            module_dependency: Some("jv.collections".to_string()),
        };

        let catalog = test_catalog();
        let usage = StdlibUsage::from_resolved_imports(&[import], &catalog);
        assert!(
            usage.package_set().contains("jv.collections"),
            "expected stdlib usage to include package"
        );
    }

    #[test]
    fn resolve_required_modules_includes_dependencies() {
        let primary = StdlibModule {
            path: PathBuf::from("collections.jv"),
            source: String::new(),
            script_main_class: "StdlibCollections".to_string(),
            package: Some("jv.collections".to_string()),
            dependencies: vec!["jv.internal".to_string()],
            metadata: StdlibModuleMetadata::default(),
            emit_java: true,
        };
        let dependency = StdlibModule {
            path: PathBuf::from("internal.jv"),
            source: String::new(),
            script_main_class: "StdlibInternal".to_string(),
            package: Some("jv.internal".to_string()),
            dependencies: Vec::new(),
            metadata: StdlibModuleMetadata::default(),
            emit_java: true,
        };

        let import = ResolvedImport {
            source_span: Span::dummy(),
            original_path: "java.util.stream.Stream".to_string(),
            alias: None,
            is_wildcard: false,
            kind: ResolvedImportKind::Type {
                fqcn: "java.util.stream.Stream".to_string(),
            },
            module_dependency: Some("jv.collections".to_string()),
        };
        let catalog = test_catalog();
        let usage = StdlibUsage::from_resolved_imports(&[import], &catalog);

        let selected = resolve_required_modules(&[primary, dependency], &usage);
        assert_eq!(
            selected.len(),
            2,
            "expected dependency closure to include both modules"
        );
    }

    #[test]
    fn scan_java_source_picks_up_fully_qualified_references() {
        let catalog = test_catalog();
        assert!(
            catalog
                .packages_for_reference("map")
                .iter()
                .any(|pkg| pkg == "jv.collections")
        );
        let mut usage = StdlibUsage::default();
        usage.scan_java_source("return java.util.stream.Stream.of(values);", &catalog);
        assert!(
            usage.package_set().contains("jv.collections"),
            "scan should detect stdlib package"
        );
    }

    #[test]
    fn compile_module_returns_error_on_parse_failure() {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let temp_root =
            std::env::temp_dir().join(format!("jv-embedded-stdlib-parse-{}", timestamp));
        let output_dir = temp_root.join("out");
        fs::create_dir_all(&output_dir).expect("create temp output directory");

        let module = StdlibModule {
            path: temp_root.join("broken_sequence.jv"),
            source: "fun this is not valid syntax".to_string(),
            script_main_class: "BrokenSequence".to_string(),
            package: None,
            dependencies: Vec::new(),
            metadata: StdlibModuleMetadata::default(),
            emit_java: true,
        };

        let result = compile_module(
            &module,
            &output_dir,
            JavaTarget::Java25,
            false,
            ParallelInferenceConfig::default(),
            &Arc::new(SymbolIndex::default()),
        );

        assert!(result.is_err(), "expected parse failure, got {result:?}");

        let _ = fs::remove_dir_all(&temp_root);
    }

    #[test]
    fn collect_manifest_includes_generic_metadata() {
        let span = Span::dummy();
        let mut type_param = IrTypeParameter::new("T", span.clone());
        type_param.kind = Some(Kind::Star);
        type_param.bounds.push(JavaType::Reference {
            name: "java.lang.Comparable".to_string(),
            generic_args: Vec::new(),
        });

        let class_decl = IrStatement::ClassDeclaration {
            name: "Vector".to_string(),
            type_parameters: vec![type_param],
            superclass: None,
            interfaces: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
            nested_classes: Vec::new(),
            modifiers: IrModifiers::default(),
            span: span.clone(),
        };

        let mut program = IrProgram {
            package: Some("demo".to_string()),
            imports: Vec::new(),
            type_declarations: vec![class_decl],
            generic_metadata: BTreeMap::new(),
            conversion_metadata: Vec::new(),
            span,
        };

        let mut metadata_entry = IrGenericMetadata::default();
        metadata_entry
            .type_parameter_kinds
            .insert("T".to_string(), Kind::Star);
        metadata_entry
            .const_parameter_values
            .insert("SIZE".to_string(), IrTypeLevelValue::Int(3));
        metadata_entry.type_level_bindings.insert(
            "dimension".to_string(),
            IrTypeLevelValue::String("3D".to_string()),
        );

        program
            .generic_metadata
            .insert("demo::Vector".to_string(), metadata_entry);

        let entries = collect_generic_manifest(&program);
        assert_eq!(entries.len(), 1);
        let entry = &entries[0];
        assert_eq!(entry.qualified_name, "demo::Vector");
        assert_eq!(entry.type_parameters.len(), 1);
        let param_entry = &entry.type_parameters[0];
        assert_eq!(param_entry.name, "T");
        assert_eq!(param_entry.kind, Some(Kind::Star));
        assert!(matches!(
            entry.const_parameters.get("SIZE"),
            Some(IrTypeLevelValue::Int(3))
        ));
        assert!(matches!(
            entry.type_level_bindings.get("dimension"),
            Some(IrTypeLevelValue::String(value)) if value == "3D"
        ));
    }
}
