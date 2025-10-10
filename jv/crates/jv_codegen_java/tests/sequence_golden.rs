mod golden;

use golden::{
    run_case, sequence_helper_from_factory, sequence_helper_multistage, sequence_inline_map,
    sequence_inline_reduce,
};

#[test]
fn sequence_inline_map_java25_golden() {
    run_case(sequence_inline_map::java25());
}

#[test]
fn sequence_inline_reduce_java25_golden() {
    run_case(sequence_inline_reduce::java25());
}

#[test]
fn sequence_helper_from_factory_java25_golden() {
    run_case(sequence_helper_from_factory::java25());
}

#[test]
fn sequence_helper_multistage_java25_golden() {
    run_case(sequence_helper_multistage::java25());
}
