pub mod context;
pub mod pipeline;
pub mod stages;
pub mod types;

pub use context::LexerContext;
pub use pipeline::{
    CharScannerStage, ClassifierStage, EmitterStage, LexerPipeline, NormalizerStage,
    PipelineStages, TokenPlugin, TokenPluginManager, TokenSink, DEFAULT_LOOKAHEAD_LIMIT,
};
pub use stages::{CharScanner, Normalizer};
pub use types::{
    ClassifiedToken, EmissionPlan, NormalizedToken, PreMetadata, RawToken, RawTokenKind,
    ScannerPosition, Span,
};
