use crate::parser::OwnedToken;
use jv_lexer::{JsonCommentTriviaKind, SourceCommentKind, TokenTrivia};
use std::collections::HashMap;

/// トリビア種別。
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Trivia {
    Whitespace(String),
    LineComment(String),
    BlockComment(String),
    Newline,
}

/// トークンインデックスごとのトリビアマップ。
#[derive(Clone, Debug, Default)]
pub struct TriviaMap {
    leading: HashMap<usize, Vec<Trivia>>,
    trailing: HashMap<usize, Vec<Trivia>>,
}

impl TriviaMap {
    /// トークン列からトリビアマップを生成する。
    pub fn from_tokens(tokens: &[OwnedToken]) -> Self {
        let mut leading = HashMap::new();
        for (idx, token) in tokens.iter().enumerate() {
            let trivia = trivias_from_token_trivia(&token.leading_trivia);
            if !trivia.is_empty() {
                leading.insert(idx, trivia);
            }
        }

        Self {
            leading,
            trailing: HashMap::new(),
        }
    }

    /// 先行トリビアを取得する。
    pub fn leading(&self, index: usize) -> Option<&[Trivia]> {
        self.leading.get(&index).map(|v| v.as_slice())
    }

    /// 後続トリビアを取得する。
    pub fn trailing(&self, index: usize) -> Option<&[Trivia]> {
        self.trailing.get(&index).map(|v| v.as_slice())
    }

    /// 何も保持していないかを返す。
    pub fn is_empty(&self) -> bool {
        self.leading.is_empty() && self.trailing.is_empty()
    }
}

/// TokenTrivia から Trivia 列へ変換する。
fn trivias_from_token_trivia(trivia: &TokenTrivia) -> Vec<Trivia> {
    let mut out = Vec::new();

    if trivia.spaces > 0 {
        out.push(Trivia::Whitespace(" ".repeat(trivia.spaces as usize)));
    }

    for _ in 0..trivia.newlines {
        out.push(Trivia::Newline);
    }

    if let Some(doc) = trivia.doc_comment.as_ref() {
        out.push(Trivia::BlockComment(doc.clone()));
    }

    for comment in trivia
        .passthrough_comments
        .iter()
        .chain(trivia.jv_comments.iter())
    {
        match comment.kind {
            SourceCommentKind::Line => out.push(Trivia::LineComment(comment.text.clone())),
            SourceCommentKind::Block => out.push(Trivia::BlockComment(comment.text.clone())),
        }
    }

    for json in &trivia.json_comments {
        match json.kind {
            JsonCommentTriviaKind::Line => out.push(Trivia::LineComment(json.text.clone())),
            JsonCommentTriviaKind::Block => out.push(Trivia::BlockComment(json.text.clone())),
        }
    }

    out
}
