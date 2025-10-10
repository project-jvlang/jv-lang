use super::*;

pub fn java25() -> SequenceGoldenCase {
    let expression = build_pipeline_expression(
        collection_source("numbers"),
        Vec::new(),
        reduce_terminal(),
        JavaType::object(),
    );

    SequenceGoldenCase {
        name: "sequence_inline_reduce_java25",
        target: JavaTarget::Java25,
        expression,
        expected: include_str!("expected/sequence_inline_reduce__java25.java"),
    }
}
