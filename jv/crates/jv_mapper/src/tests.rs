use super::*;

use std::fs;

fn span(start_line: usize, start_column: usize, end_line: usize, end_column: usize) -> Span {
    Span::new(start_line, start_column, end_line, end_column)
}

#[test]
fn record_generated_segment_tracks_position() -> Result<(), MappingError> {
    let mut builder = SourceMapBuilder::new("src/main.jv", "Main.java");
    let ir_span = span(1, 0, 1, 10);
    let java_span = builder.record_generated_segment(
        ir_span.clone(),
        "final int x = 1;\n",
        MappingCategory::Statement,
        Some("IrStatement::VariableDeclaration".to_string()),
    )?;

    assert_eq!(java_span.start, JavaPosition::new(1, 0));
    assert_eq!(java_span.end, JavaPosition::new(2, 0));
    assert_eq!(builder.current_java(), JavaPosition::new(2, 0));
    Ok(())
}

#[test]
fn record_mapping_deduplicates_entries() -> Result<(), MappingError> {
    let mut builder = SourceMapBuilder::new("src/main.jv", "Main.java");
    let ir_span = span(2, 0, 2, 5);
    let java_span = JavaSpan::new(JavaPosition::new(3, 0), JavaPosition::new(3, 20))?;

    builder.record_mapping(
        ir_span.clone(),
        java_span.clone(),
        MappingCategory::Method,
        Some("IrStatement::MethodDeclaration".to_string()),
    )?;
    builder.record_mapping(
        ir_span.clone(),
        java_span.clone(),
        MappingCategory::Method,
        Some("IrStatement::MethodDeclaration".to_string()),
    )?;

    let map = builder.build();
    assert_eq!(map.entries.len(), 1);
    Ok(())
}

#[test]
fn find_by_positions_returns_expected_entry() -> Result<(), MappingError> {
    let mut builder = SourceMapBuilder::new("src/main.jv", "Main.java");
    let ir_span = span(5, 2, 5, 12);
    let java_span = builder.record_generated_segment(
        ir_span.clone(),
        "System.out.println(value);\n",
        MappingCategory::Statement,
        Some("IrStatement::Expression".to_string()),
    )?;
    let source_map = builder.build();

    let ir_hit = source_map.find_by_ir_position(5, 5).unwrap();
    assert_eq!(ir_hit.ir_span, ir_span);

    let java_hit = source_map
        .find_by_java_position(java_span.start.line, java_span.start.column)
        .unwrap();
    assert_eq!(java_hit.ir_span, ir_span);
    Ok(())
}

#[test]
fn json_roundtrip_preserves_entries() -> Result<(), MappingError> {
    let mut builder = SourceMapBuilder::new("src/main.jv", "Main.java");
    builder.record_generated_segment(
        span(1, 0, 1, 3),
        "class Main {\n",
        MappingCategory::TypeDeclaration,
        Some("IrStatement::ClassDeclaration".to_string()),
    )?;
    let map = builder.build();

    let json = map.to_json_pretty()?;
    let restored = SourceMap::from_json(&json)?;

    assert_eq!(map.source_file, restored.source_file);
    assert_eq!(map.generated_file, restored.generated_file);
    assert_eq!(map.entries, restored.entries);
    Ok(())
}

#[test]
fn invalid_ir_span_returns_error() {
    let mut builder = SourceMapBuilder::new("a.jv", "A.java");
    let result = builder.record_generated_segment(
        span(2, 5, 1, 0),
        "broken",
        MappingCategory::Statement,
        None,
    );

    assert!(matches!(result, Err(MappingError::InvalidPosition(_))));
}

#[test]
fn invalid_java_span_is_rejected() {
    let mut builder = SourceMapBuilder::new("a.jv", "A.java");
    let java_span = JavaSpan::new(JavaPosition::new(3, 5), JavaPosition::new(2, 0));
    assert!(java_span.is_err());

    let ir_span = span(1, 0, 1, 1);
    let result = builder.record_mapping(
        ir_span,
        JavaSpan {
            start: JavaPosition::new(3, 5),
            end: JavaPosition::new(2, 0),
        },
        MappingCategory::Statement,
        None,
    );
    assert!(matches!(result, Err(MappingError::InvalidPosition(_))));
}

#[test]
fn manager_saves_maps_to_disk() -> Result<(), MappingError> {
    let mut builder = SourceMapBuilder::new("src/main.jv", "Main.java");
    builder.record_generated_segment(
        span(1, 0, 1, 5),
        "class Main {}\n",
        MappingCategory::TypeDeclaration,
        None,
    )?;
    let map = builder.build();

    let mut manager = SourceMapManager::new();
    manager.add_source_map("src/main.jv".to_string(), map);

    let temp_dir = std::env::temp_dir().join("jv_mapper_test");
    if temp_dir.exists() {
        fs::remove_dir_all(&temp_dir).ok();
    }
    manager.save_all(temp_dir.to_str().unwrap())?;

    let saved_files = fs::read_dir(&temp_dir)
        .expect("temp dir should exist")
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.file_name().to_string_lossy().to_string())
        .collect::<Vec<_>>();

    assert!(saved_files.iter().any(|name| name.ends_with(".map.json")));
    fs::remove_dir_all(&temp_dir).ok();
    Ok(())
}
