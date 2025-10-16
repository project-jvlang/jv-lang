use crate::inference::conversions::{AppliedConversion, HelperSpec};
use jv_ir::ConversionHelper;
use jv_mapper::{ConversionMapping, JavaSpan};

/// Synchronize telemetry conversion events with conversion-aware source map entries.
///
/// Each conversion event is enriched with the corresponding IR span and Java span hint when a
/// matching source map entry is found. Matching is performed using the conversion kind label,
/// the human-readable `from_type`/`to_type` strings, and helper method metadata.
pub fn attach_conversion_spans(events: &mut [AppliedConversion], mappings: &[ConversionMapping]) {
    if events.is_empty() || mappings.is_empty() {
        return;
    }

    let mut used = vec![false; mappings.len()];

    for event in events {
        if event.source_span.is_some() && event.java_span_hint.is_some() {
            continue;
        }

        let from = event.from_type.describe();
        let to = event.to_type.describe();
        let kind_label = event.kind.label();

        if let Some((index, mapping)) = mappings.iter().enumerate().find(|(idx, mapping)| {
            !used[*idx]
                && mapping.metadata.kind.label() == kind_label
                && mapping.metadata.from_type == from
                && mapping.metadata.to_type == to
                && helpers_match(
                    event.helper_method.as_ref(),
                    mapping.metadata.helper.as_ref(),
                )
        }) {
            if event.source_span.is_none() {
                event.source_span = Some(mapping.ir_span.clone());
            }

            if event.java_span_hint.is_none() {
                event.java_span_hint = Some(format_java_span(
                    &mapping.generated_file,
                    &mapping.java_span,
                ));
            }

            used[index] = true;
        }
    }
}

fn helpers_match(
    event_helper: Option<&HelperSpec>,
    metadata_helper: Option<&ConversionHelper>,
) -> bool {
    match (event_helper, metadata_helper) {
        (None, None) => true,
        (Some(left), Some(right)) => {
            left.owner == right.owner
                && left.method == right.method
                && left.is_static == right.is_static
        }
        _ => false,
    }
}

fn format_java_span(file: &str, span: &JavaSpan) -> String {
    format!(
        "{}@{}:{}-{}:{}",
        file, span.start.line, span.start.column, span.end.line, span.end.column
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inference::conversions::{AppliedConversion, ConversionKind};
    use crate::inference::types::{PrimitiveType, TypeKind};
    use jv_ast::Span;
    use jv_ir::{ConversionKind as IrConversionKind, ConversionMetadata};
    use jv_mapper::JavaPosition;

    fn sample_mapping() -> ConversionMapping {
        ConversionMapping::new(
            "example.jv",
            "Example.java",
            Span::new(1, 0, 1, 3),
            JavaSpan::new(JavaPosition::new(2, 4), JavaPosition::new(2, 10)).unwrap(),
            ConversionMetadata::new(IrConversionKind::Boxing, "int", "java.lang.Integer"),
            None,
        )
    }

    #[test]
    fn attaches_span_information_when_mapping_matches() {
        let mut events = vec![AppliedConversion::new(
            TypeKind::primitive(PrimitiveType::Int),
            TypeKind::boxed(PrimitiveType::Int),
            ConversionKind::Boxing,
            None,
            None,
            false,
        )];

        attach_conversion_spans(&mut events, &[sample_mapping()]);

        assert_eq!(events[0].source_span, Some(Span::new(1, 0, 1, 3)));
        assert_eq!(
            events[0].java_span_hint.as_deref(),
            Some("Example.java@2:4-2:10")
        );
    }

    #[test]
    fn preserves_existing_annotations() {
        let mut event = AppliedConversion::new(
            TypeKind::primitive(PrimitiveType::Int),
            TypeKind::boxed(PrimitiveType::Int),
            ConversionKind::Boxing,
            None,
            None,
            false,
        )
        .with_source_span(Span::new(5, 2, 5, 6))
        .with_java_span_hint("ExistingHint");

        attach_conversion_spans(std::slice::from_mut(&mut event), &[sample_mapping()]);

        assert_eq!(event.source_span, Some(Span::new(5, 2, 5, 6)));
        assert_eq!(event.java_span_hint.as_deref(), Some("ExistingHint"));
    }
}
