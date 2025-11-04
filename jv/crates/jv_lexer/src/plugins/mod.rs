use crate::pipeline::pipeline::TokenPlugin;

mod legacy_loop;

use legacy_loop::LegacyLoopPlugin;

pub fn load_static_plugins() -> Vec<Box<dyn TokenPlugin>> {
    vec![Box::new(LegacyLoopPlugin::default())]
}

#[cfg(test)]
mod tests {
    use crate::pipeline::{
        context::LexerContext,
        pipeline::{ClassifierStage, TokenPluginManager},
        stages::classifier::Classifier,
        types::{NormalizedToken, PreMetadata, RawToken, RawTokenKind, Span},
    };

    fn make_raw_token(kind: RawTokenKind, text: &str) -> RawToken<'static> {
        let leaked: &'static str = Box::leak(text.to_string().into_boxed_str());
        RawToken {
            kind,
            text: leaked,
            span: Span::empty(Default::default()),
            trivia: None,
            carry_over: None,
            field_label: None,
            field_label_issue: None,
        }
    }

    #[test]
    fn legacy_loop_plugin_adds_diagnostic() {
        let mut classifier = Classifier::new();
        let raw = make_raw_token(RawTokenKind::Identifier, "while");
        let token = NormalizedToken::new(raw, "while".to_string(), PreMetadata::default());
        let mut ctx = LexerContext::new("while");
        let mut classified = classifier.classify(token, &mut ctx).unwrap();

        let mut ctx = LexerContext::new("while");
        let manager = TokenPluginManager::with_default_plugins();
        // apply should append diagnostic
        manager.apply(&mut classified, &mut ctx).unwrap();
        assert!(!classified.diagnostics.is_empty());
    }
}
