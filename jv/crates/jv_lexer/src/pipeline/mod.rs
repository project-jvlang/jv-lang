pub mod context;
pub mod pipeline;
pub mod stages;
pub mod trace;
pub mod types;

pub use context::LexerContext;
pub use pipeline::{
    CharScannerStage, ClassifierStage, DEFAULT_LOOKAHEAD_LIMIT, EmitterStage, LexerPipeline,
    NormalizerStage, PipelineStages, TokenPlugin, TokenPluginManager, TokenSink,
};
pub use stages::{CharScanner, Classifier, Emitter, Normalizer};
pub use types::{
    ClassifiedToken, EmissionPlan, NormalizedToken, PreMetadata, RawToken, RawTokenKind,
    ScannerPosition, Span,
};
