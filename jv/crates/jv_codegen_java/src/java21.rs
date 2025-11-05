use crate::record::{TupleRecord, render_tuple_record};

/// Render a tuple record definition compatible with Java 21 targets.
/// Records are available in Java 21, so we emit the same syntax as the default target.
pub fn render_tuple_record_java21(record: &TupleRecord, indent: &str) -> String {
    render_tuple_record(record, indent)
}
