use super::*;

pub fn java25() -> SequenceGoldenCase {
    let expression = build_pipeline_expression(
        sequence_factory_stream_source("numbers"),
        vec![map_identity_stage()],
        to_list_terminal(),
        java_list_type(),
    );

    SequenceGoldenCase {
        name: "sequence_helper_from_factory_java25",
        target: JavaTarget::Java25,
        expression,
        expected: include_str!("expected/sequence_helper_from_factory__java25.java"),
    }
}
