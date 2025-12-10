use criterion::{criterion_group, criterion_main};

use crate::harness::criterion_config;

mod full_parse;
mod harness;
mod incremental;
mod lsp;
mod memory;

criterion_group!(
    name = benches;
    config = criterion_config();
    targets = full_parse::bench_full_parse, incremental::bench_incremental, memory::bench_memory, lsp::bench_lsp
);
criterion_main!(benches);
