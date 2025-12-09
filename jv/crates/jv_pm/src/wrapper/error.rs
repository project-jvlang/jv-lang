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

    #[error("既存のプロジェクトが検出されました。新しいディレクトリで init を実行してください。")]
    ProjectAlreadyExists,

    #[error("destination `{0}` is not empty")]
    DestinationNotEmpty(std::path::PathBuf),

    #[error("pom.xml が見つかりません。`jvpm init` でプロジェクトを初期化してください。")]
    PomNotFound,

    #[error("ファイル書き込み権限がありません: {0}")]
    PermissionDenied(std::path::PathBuf),
}
