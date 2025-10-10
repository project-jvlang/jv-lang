use std::fs;
use std::path::{Component, Path, PathBuf};

use anyhow::{anyhow, Result};
use jv_build::JavaTarget;
use jv_checker::diagnostics::{
    from_check_error, from_parse_error, from_transform_error, DiagnosticStrategy,
};
use jv_checker::{ParallelInferenceConfig, TypeChecker};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator};
use jv_fmt::JavaFormatter;
use jv_ir::transform_program_with_context;
use jv_ir::TransformContext;
use jv_parser::Parser as JvParser;

use crate::pipeline::generics::apply_type_facts;
use crate::tooling_failure;

const STDLIB_ROOT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../../stdlib");

struct StdlibModule {
    path: PathBuf,
    source: String,
    script_main_class: String,
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

    Ok(emitted_paths)
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
import java.util.stream.Collectors;
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
        return delegate.collect(Collectors.toList());
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
