use super::*;

pub fn java25() -> SequenceGoldenCase {
    let expression = build_pipeline_expression(
        collection_source("numbers"),
        vec![map_double_stage()],
        to_list_terminal(),
        java_list_type(),
    );

    SequenceGoldenCase {
        name: "sequence_inline_map_java25",
        target: JavaTarget::Java25,
        expression,
        expected: include_str!("expected/sequence_inline_map__java25.java"),
    }
}
