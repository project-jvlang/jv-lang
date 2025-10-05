pub mod context;
pub mod pipeline;
pub mod types;

pub use context::LexerContext;
pub use pipeline::{
    CharScannerStage, ClassifierStage, EmitterStage, LexerPipeline, NormalizerStage,
    PipelineStages, TokenPlugin, TokenPluginManager, TokenSink, DEFAULT_LOOKAHEAD_LIMIT,
};
pub use types::{
    ClassifiedToken, NormalizedToken, PreMetadata, RawToken, RawTokenKind, ScannerPosition, Span,
};
