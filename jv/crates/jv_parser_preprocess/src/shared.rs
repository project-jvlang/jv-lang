use std::collections::HashSet;

/// Stage 0 ステージ間で共有する一時状態。
#[derive(Debug, Default)]
pub(crate) struct StageSharedState {
    /// 現在のトークン列に対応する、元のインデックス写像。
    pub(crate) origin_indices: Vec<usize>,
    /// 呼び出しコンテキストとして扱う `(` トークンの元インデックス集合。
    pub(crate) call_paren_origins: HashSet<usize>,
}

impl StageSharedState {
    pub(crate) fn reset(&mut self) {
        self.origin_indices.clear();
        self.call_paren_origins.clear();
    }

    pub(crate) fn set_origin_indices(&mut self, len: usize) {
        self.origin_indices = (0..len).collect();
    }

    pub(crate) fn update_origin_indices(&mut self, origins: Vec<usize>) {
        self.origin_indices = origins;
    }
}
