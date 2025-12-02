//! DSLプラグインに関するフィーチャーフラグ管理。

/// `dsl-log` フィーチャーが有効かを返す。
pub const fn dsl_log() -> bool {
    cfg!(feature = "dsl-log")
}

/// `dsl-io` フィーチャーが有効かを返す。
pub const fn dsl_io() -> bool {
    cfg!(feature = "dsl-io")
}

/// `dsl-lock` フィーチャーが有効かを返す。
pub const fn dsl_lock() -> bool {
    cfg!(feature = "dsl-lock")
}

/// `dsl-cron` フィーチャーが有効かを返す。
pub const fn dsl_cron() -> bool {
    cfg!(feature = "dsl-cron")
}

/// `dsl-assert` フィーチャーが有効かを返す。
pub const fn dsl_assert() -> bool {
    cfg!(feature = "dsl-assert")
}

/// いずれかのDSLプラグイン機能が有効かを返す。
pub const fn any_dsl_enabled() -> bool {
    dsl_log() || dsl_io() || dsl_lock() || dsl_cron() || dsl_assert()
}
