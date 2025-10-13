use std::collections::BTreeMap;
use std::fs;
use std::path::{Component, Path, PathBuf};

use anyhow::{anyhow, Result};
use jv_ast::types::Kind;
use jv_build::JavaTarget;
use jv_checker::diagnostics::{
    from_check_error, from_parse_error, from_transform_error, DiagnosticStrategy,
};
use jv_checker::{ParallelInferenceConfig, TypeChecker};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator};
use jv_fmt::JavaFormatter;
use jv_ir::{
    transform_program_with_context, IrGenericMetadata, IrProgram, IrStatement, IrTypeLevelValue,
    IrTypeParameter, IrVariance, JavaType, JavaWildcardKind, TransformContext,
};
use jv_parser::Parser as JvParser;
use serde::Serialize;

use crate::pipeline::generics::apply_type_facts;
use crate::tooling_failure;

const STDLIB_ROOT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../../stdlib");

struct StdlibModule {
    path: PathBuf,
    source: String,
    script_main_class: String,
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

pub fn compile_stdlib_modules(
    output_dir: &Path,
    target: JavaTarget,
    format: bool,
    parallel_config: ParallelInferenceConfig,
) -> Result<Vec<PathBuf>> {
    let modules = collect_stdlib_modules()?;
    let mut generated_files = Vec::new();

    for module in &modules {
        match compile_module(module, output_dir, target, format, parallel_config) {
            Ok(files) => generated_files.extend(files),
            Err(ModuleError::Unsupported(reason)) => {
                eprintln!(
                    "warning: falling back to prebuilt Sequence runtime: {}",
                    reason
                );
                let fallback = write_prebuilt_sequence(output_dir)?;
                generated_files.extend(fallback);
            }
            Err(ModuleError::Fatal(err)) => return Err(err),
        }
    }

    Ok(generated_files)
}

fn collect_stdlib_modules() -> Result<Vec<StdlibModule>> {
    let root = Path::new(STDLIB_ROOT);
    let mut modules = Vec::new();
    visit_stdlib(root, root, &mut modules)?;
    Ok(modules)
}

fn visit_stdlib(root: &Path, dir: &Path, modules: &mut Vec<StdlibModule>) -> Result<()> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            if path.file_name().map_or(false, |name| name == "tests") {
                continue;
            }
            visit_stdlib(root, &path, modules)?;
            continue;
        }

        if path.extension().and_then(|ext| ext.to_str()) != Some("jv") {
            continue;
        }

        if path
            .components()
            .any(|component| component.as_os_str() == "tests")
        {
            continue;
        }

        let source = fs::read_to_string(&path)?;
        let script_main_class = derive_script_name(root, &path);
        modules.push(StdlibModule {
            path,
            source,
            script_main_class,
        });
    }

    Ok(())
}

fn derive_script_name(root: &Path, path: &Path) -> String {
    let relative = path.strip_prefix(root).unwrap_or(path);
    let mut name = String::from("Stdlib");

    for component in relative.components() {
        if let Component::Normal(os) = component {
            let mut part = os.to_string_lossy().to_string();
            if part.ends_with(".jv") {
                part.truncate(part.len() - 3);
            }
            for segment in part.split(|c: char| !c.is_alphanumeric()) {
                if segment.is_empty() {
                    continue;
                }
                name.push_str(&capitalize(segment));
            }
        }
    }

    name
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
) -> Result<Vec<PathBuf>, ModuleError> {
    let program = match JvParser::parse(&module.source) {
        Ok(program) => program,
        Err(error) => {
            if let Some(diagnostic) = from_parse_error(&error) {
                return Err(ModuleError::Unsupported(tooling_failure(
                    module.path.as_path(),
                    diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                )));
            }
            return Err(ModuleError::Unsupported(anyhow!(
                "Parser error: {:?}",
                error
            )));
        }
    };

    let mut type_checker = TypeChecker::with_parallel_config(parallel_config);
    let inference_snapshot = match type_checker.check_program(&program) {
        Ok(()) => type_checker.take_inference_snapshot(),
        Err(errors) => {
            if let Some(diagnostic) = errors.iter().find_map(from_check_error) {
                return Err(ModuleError::Fatal(tooling_failure(
                    module.path.as_path(),
                    diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                )));
            }
            let details = errors
                .iter()
                .map(|error| error.to_string())
                .collect::<Vec<_>>()
                .join("; ");
            return Err(ModuleError::Fatal(anyhow!(
                "Type checking failed: {}",
                details
            )));
        }
    };

    let normalized = type_checker.take_normalized_program().unwrap_or(program);

    let mut context = TransformContext::new();
    let mut ir_program = match transform_program_with_context(normalized, &mut context) {
        Ok(ir) => ir,
        Err(error) => {
            if let Some(diagnostic) = from_transform_error(&error) {
                return Err(ModuleError::Fatal(tooling_failure(
                    module.path.as_path(),
                    diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                )));
            }
            return Err(ModuleError::Fatal(anyhow!(
                "IR transformation error: {:?}",
                error
            )));
        }
    };

    if let Some(snapshot) = inference_snapshot.as_ref() {
        apply_type_facts(&mut ir_program, snapshot.type_facts());
    }

    let mut codegen_config = JavaCodeGenConfig::for_target(target);
    codegen_config.script_main_class = module.script_main_class.clone();

    let mut generator = JavaCodeGenerator::with_config(codegen_config);
    let java_unit = generator
        .generate_compilation_unit(&ir_program)
        .map_err(|error| ModuleError::Fatal(anyhow!("Code generation error: {:?}", error)))?;

    let formatter = JavaFormatter::default();
    let package_path = java_unit
        .package_declaration
        .as_ref()
        .map(|package| package.replace('.', "/"));

    let mut emitted_paths = Vec::new();

    for (index, type_decl) in java_unit.type_declarations.iter().enumerate() {
        let file_name = derive_type_name(type_decl)
            .unwrap_or_else(|| format!("StdlibModule{}{}", module.script_main_class, index));

        let mut directory = PathBuf::from(output_dir);
        if let Some(package_dir) = package_path.as_ref() {
            directory.push(package_dir);
        }
        fs::create_dir_all(&directory).map_err(|error| {
            ModuleError::Fatal(anyhow!(
                "Failed to create directory {}: {}",
                directory.display(),
                error
            ))
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

        fs::write(&java_path, java_content).map_err(|error| {
            ModuleError::Fatal(anyhow!(
                "Failed to write {}: {}",
                java_path.display(),
                error
            ))
        })?;
        emitted_paths.push(java_path);
    }

    let manifest = GenericManifest {
        module: module.script_main_class.clone(),
        package: ir_program.package.clone(),
        declarations: collect_generic_manifest(&ir_program),
    };

    if !manifest.declarations.is_empty() {
        let manifest_json = serde_json::to_string_pretty(&manifest).map_err(|error| {
            ModuleError::Fatal(anyhow!(
                "Failed to serialize generic manifest for {}: {}",
                module.path.display(),
                error
            ))
        })?;
        let manifest_path = output_dir.join(format!("{}-generics.json", module.script_main_class));
        fs::write(&manifest_path, manifest_json).map_err(|error| {
            ModuleError::Fatal(anyhow!(
                "Failed to write {}: {}",
                manifest_path.display(),
                error
            ))
        })?;
        emitted_paths.push(manifest_path);
    }

    Ok(emitted_paths)
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

fn derive_type_name(type_decl: &str) -> Option<String> {
    let mut tokens = type_decl.split_whitespace().peekable();

    while let Some(token) = tokens.next() {
        match token {
            "class" | "record" | "interface" | "enum" => {
                let name_token = tokens.next()?;
                return Some(clean_type_token(name_token));
            }
            "sealed" | "non-sealed" => {
                if let Some(peek) = tokens.peek() {
                    if matches!(*peek, "class" | "record" | "interface" | "enum") {
                        continue;
                    }
                }
            }
            _ => continue,
        }
    }

    None
}

fn clean_type_token(raw: &str) -> String {
    let trimmed = raw.trim_matches(|c: char| c == '{' || c == ';');
    let split = trimmed
        .split(|c| matches!(c, '<' | '('))
        .next()
        .unwrap_or(trimmed);
    split.trim().to_string()
}

enum ModuleError {
    Unsupported(anyhow::Error),
    Fatal(anyhow::Error),
}

const SEQUENCE_FACTORY_JAVA: &str = r#"package jv.collections;

import java.util.Objects;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public final class SequenceFactory {
    private SequenceFactory() {}

    public static <T> SequenceCore<T> fromIterable(Iterable<T> source) {
        Objects.requireNonNull(source, "source");
        Stream<T> stream = StreamSupport.stream(source.spliterator(), false);
        return new SequenceCore<>(stream);
    }

    public static <T> SequenceCore<T> fromStream(Stream<T> stream) {
        Objects.requireNonNull(stream, "stream");
        return new SequenceCore<>(stream);
    }
}
"#;

const SEQUENCE_CORE_JAVA: &str = r#"package jv.collections;

import java.lang.IllegalArgumentException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public final class SequenceCore<T> implements AutoCloseable, Iterable<T> {
    private final Stream<T> delegate;

    SequenceCore(Stream<T> delegate) {
        this.delegate = Objects.requireNonNull(delegate, "delegate");
    }

    public Stream<T> toStream() {
        return delegate;
    }

    @Override
    public Iterator<T> iterator() {
        return delegate.iterator();
    }

    @Override
    public void close() {
        delegate.close();
    }

    public <R> SequenceCore<R> map(Function<? super T, ? extends R> transform) {
        Objects.requireNonNull(transform, "transform");
        return SequenceFactory.fromStream(delegate.map(transform));
    }

    public SequenceCore<T> filter(Predicate<? super T> predicate) {
        Objects.requireNonNull(predicate, "predicate");
        return SequenceFactory.fromStream(delegate.filter(predicate));
    }

    public SequenceCore<T> take(int count) {
        return SequenceFactory.fromStream(delegate.limit(count));
    }

    public SequenceCore<T> drop(int count) {
        return SequenceFactory.fromStream(delegate.skip(count));
    }

    public <R> SequenceCore<R> flatMap(Function<? super T, ? extends Iterable<R>> transform) {
        Objects.requireNonNull(transform, "transform");
        Stream<R> flattened = delegate.flatMap(value -> {
            Iterable<R> next = transform.apply(value);
            return StreamSupport.stream(next.spliterator(), false);
        });
        return SequenceFactory.fromStream(flattened);
    }

    public <R> SequenceCore<R> flatMapSequence(Function<? super T, SequenceCore<R>> transform) {
        Objects.requireNonNull(transform, "transform");
        Stream<R> flattened = delegate.flatMap(value -> transform.apply(value).toStream());
        return SequenceFactory.fromStream(flattened);
    }

    public SequenceCore<T> sorted() {
        return SequenceFactory.fromStream(delegate.sorted());
    }

    public <R extends Comparable<R>> SequenceCore<T> sortedBy(Function<? super T, ? extends R> selector) {
        Objects.requireNonNull(selector, "selector");
        Comparator<T> comparator = Comparator.comparing(selector);
        return SequenceFactory.fromStream(delegate.sorted(comparator));
    }

    public List<T> toList() {
        return delegate.toList();
    }

    public <R> R fold(R initial, BiFunction<R, ? super T, R> operation) {
        Objects.requireNonNull(operation, "operation");
        R accumulator = initial;
        Iterator<T> iterator = delegate.iterator();
        while (iterator.hasNext()) {
            accumulator = operation.apply(accumulator, iterator.next());
        }
        return accumulator;
    }

    public T reduce(BinaryOperator<T> operation) {
        Objects.requireNonNull(operation, "operation");
        Iterator<T> iterator = delegate.iterator();
        if (!iterator.hasNext()) {
            throw new IllegalArgumentException("Sequence reduce() on empty source");
        }
        T accumulator = iterator.next();
        while (iterator.hasNext()) {
            T value = iterator.next();
            accumulator = operation.apply(accumulator, value);
        }
        return accumulator;
    }

    public long count() {
        return delegate.count();
    }

    public long sum() {
        long total = 0;
        Iterator<T> iterator = delegate.iterator();
        while (iterator.hasNext()) {
            Object value = iterator.next();
            if (value instanceof Number number) {
                total += number.longValue();
            } else {
                throw new IllegalArgumentException("sum() requires numeric elements");
            }
        }
        return total;
    }

    public void forEach(Consumer<? super T> action) {
        delegate.forEach(action);
    }

    public <K> Map<K, List<T>> groupBy(Function<? super T, ? extends K> keySelector) {
        Objects.requireNonNull(keySelector, "keySelector");
        Iterator<T> iterator = delegate.iterator();
        LinkedHashMap<K, List<T>> result = new LinkedHashMap<>();
        while (iterator.hasNext()) {
            T element = iterator.next();
            K key = keySelector.apply(element);
            result.computeIfAbsent(key, ignored -> new ArrayList<>()).add(element);
        }
        return result;
    }

    public <K, V> Map<K, V> associate(Function<? super T, Map.Entry<K, V>> transform) {
        Objects.requireNonNull(transform, "transform");
        Iterator<T> iterator = delegate.iterator();
        LinkedHashMap<K, V> result = new LinkedHashMap<>();
        while (iterator.hasNext()) {
            Map.Entry<K, V> entry = transform.apply(iterator.next());
            result.put(entry.getKey(), entry.getValue());
        }
        return result;
    }
}
"#;

fn write_prebuilt_sequence(output_dir: &Path) -> Result<Vec<PathBuf>> {
    let package_dir = output_dir.join("jv/collections");
    fs::create_dir_all(&package_dir)?;

    let factory_path = package_dir.join("SequenceFactory.java");
    fs::write(&factory_path, SEQUENCE_FACTORY_JAVA)?;

    let core_path = package_dir.join("SequenceCore.java");
    fs::write(&core_path, SEQUENCE_CORE_JAVA)?;

    Ok(vec![factory_path, core_path])
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::Span;
    use jv_ir::{IrModifiers, IrStatement};

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
