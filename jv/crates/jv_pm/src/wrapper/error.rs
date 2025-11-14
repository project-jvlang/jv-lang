use thiserror::Error;

/// Errors that can occur while running in Maven wrapper mode.
#[derive(Error, Debug)]
pub enum WrapperError {
    #[error(
        "このプロジェクトは JV ネイティブプロジェクトです。`jv pm` コマンドを使用してください。"
    )]
    NativeProjectDetected,

    #[error(
        "プロジェクト構成の混在を検出しました。JV ネイティブプロジェクトでは `jv pm` を、Maven プロジェクトでは `jvpm` を使用してください。"
    )]
    MixedProjectConfiguration,

    #[error("ラッパーモード操作でエラーが発生しました: {0}")]
    OperationFailed(String),
}
