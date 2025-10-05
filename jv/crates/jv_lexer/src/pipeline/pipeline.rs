use crate::{LexError, Token, TokenType};

use super::context::LexerContext;
use super::types::{ClassifiedToken, NormalizedToken, RawToken, ScannerPosition};

pub const DEFAULT_LOOKAHEAD_LIMIT: usize = 1024;

/// 最終トークンの出力先を抽象化するシンク。
pub trait TokenSink {
    fn push(&mut self, token: Token) -> Result<(), LexError>;
}

impl TokenSink for Vec<Token> {
    fn push(&mut self, token: Token) -> Result<(), LexError> {
        self.push(token);
        Ok(())
    }
}

/// パイプライン各段の共通トレイト。
pub trait CharScannerStage {
    fn save_position(&mut self) -> ScannerPosition;
    fn restore_position(&mut self, position: ScannerPosition);
    fn discard_checkpoint(&mut self, position: ScannerPosition);
    fn commit_position(&mut self);

    fn scan_next_token<'source>(
        &mut self,
        ctx: &mut LexerContext<'source>,
    ) -> Result<RawToken<'source>, LexError>;

    fn peek_char<'source>(&mut self, ctx: &mut LexerContext<'source>) -> Option<char>;

    fn peek_chars<'source>(
        &mut self,
        ctx: &mut LexerContext<'source>,
        count: usize,
    ) -> &'source str;

    fn peek_until<'source, F>(
        &mut self,
        ctx: &mut LexerContext<'source>,
        predicate: F,
    ) -> &'source str
    where
        F: Fn(char) -> bool;

    fn needs_lookahead(&self, token: &RawToken<'_>) -> bool {
        let _ = token;
        false
    }

    fn should_reinterpret(&self, token: &RawToken<'_>, lookahead: &str) -> bool {
        let _ = (token, lookahead);
        false
    }
}

pub trait NormalizerStage {
    fn normalize<'source>(
        &mut self,
        token: RawToken<'source>,
        ctx: &mut LexerContext<'source>,
    ) -> Result<NormalizedToken<'source>, LexError>;
}

pub trait ClassifierStage {
    fn classify<'source>(
        &mut self,
        token: NormalizedToken<'source>,
        ctx: &mut LexerContext<'source>,
    ) -> Result<ClassifiedToken<'source>, LexError>;
}

pub trait EmitterStage {
    fn emit<'source>(
        &mut self,
        token: ClassifiedToken<'source>,
        ctx: &mut LexerContext<'source>,
    ) -> Result<Token, LexError>;
}

pub trait TokenPlugin: Send + Sync {
    fn name(&self) -> &'static str;

    fn apply<'source>(
        &self,
        token: &mut ClassifiedToken<'source>,
        ctx: &mut LexerContext<'source>,
    ) -> Result<(), LexError>;
}

#[derive(Default)]
pub struct TokenPluginManager {
    plugins: Vec<Box<dyn TokenPlugin>>,
}

impl TokenPluginManager {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_plugins(plugins: Vec<Box<dyn TokenPlugin>>) -> Self {
        Self { plugins }
    }

    pub fn is_empty(&self) -> bool {
        self.plugins.is_empty()
    }

    pub fn add_plugin(&mut self, plugin: Box<dyn TokenPlugin>) {
        self.plugins.push(plugin);
    }

    pub fn apply<'source>(
        &self,
        token: &mut ClassifiedToken<'source>,
        ctx: &mut LexerContext<'source>,
    ) -> Result<(), LexError> {
        for plugin in &self.plugins {
            plugin.apply(token, ctx)?;
        }
        Ok(())
    }
}

pub struct PipelineStages<S, N, C, E> {
    pub scanner: S,
    pub normalizer: N,
    pub classifier: C,
    pub emitter: E,
}

impl<S, N, C, E> PipelineStages<S, N, C, E> {
    pub fn new(scanner: S, normalizer: N, classifier: C, emitter: E) -> Self {
        Self {
            scanner,
            normalizer,
            classifier,
            emitter,
        }
    }
}

pub struct LexerPipeline<S, N, C, E> {
    stages: PipelineStages<S, N, C, E>,
    plugins: TokenPluginManager,
    lookahead_limit: usize,
}

impl<S, N, C, E> LexerPipeline<S, N, C, E> {
    pub fn new(stages: PipelineStages<S, N, C, E>, plugins: TokenPluginManager) -> Self {
        Self {
            stages,
            plugins,
            lookahead_limit: DEFAULT_LOOKAHEAD_LIMIT,
        }
    }

    pub fn with_lookahead_limit(mut self, limit: usize) -> Self {
        self.lookahead_limit = limit.max(1);
        self
    }

    pub fn run<'source, Sink>(
        &mut self,
        ctx: &mut LexerContext<'source>,
        sink: &mut Sink,
    ) -> Result<(), LexError>
    where
        Sink: TokenSink,
        S: CharScannerStage,
        N: NormalizerStage,
        C: ClassifierStage,
        E: EmitterStage,
    {
        loop {
            let checkpoint = self.stages.scanner.save_position();
            let raw = self.stages.scanner.scan_next_token(ctx)?;
            let is_raw_eof = raw.is_eof();

            if !is_raw_eof && self.stages.scanner.needs_lookahead(&raw) {
                let preview = self.stages.scanner.peek_chars(ctx, self.lookahead_limit);
                ctx.set_lookahead_window(preview);

                if self.stages.scanner.should_reinterpret(&raw, preview) {
                    self.stages.scanner.restore_position(checkpoint);
                    self.stages.scanner.discard_checkpoint(checkpoint);
                    continue;
                }
            }

            let normalized = self.stages.normalizer.normalize(raw, ctx)?;
            let mut classified = self.stages.classifier.classify(normalized, ctx)?;
            self.plugins.apply(&mut classified, ctx)?;
            let token = self.stages.emitter.emit(classified, ctx)?;
            let reached_end = matches!(token.token_type, TokenType::Eof);
            sink.push(token)?;
            ctx.increment_emitted();
            ctx.clear_lookahead_window();
            self.stages.scanner.commit_position();
            self.stages.scanner.discard_checkpoint(checkpoint);

            if is_raw_eof || reached_end {
                break;
            }
        }

        Ok(())
    }
}
