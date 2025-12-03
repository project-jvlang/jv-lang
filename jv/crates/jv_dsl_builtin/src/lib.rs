//! 組み込みDSLプラグイン群（LOG, IO, LOCK, CRON, ASSERT）。

#[cfg(feature = "dsl-assert")]
pub mod assert;
#[cfg(feature = "dsl-cron")]
pub mod cron;
#[cfg(feature = "dsl-io")]
pub mod io;
#[cfg(feature = "dsl-lock")]
pub mod lock;
#[cfg(feature = "dsl-log")]
pub mod log;

#[cfg(feature = "dsl-assert")]
pub use assert::AssertPlugin;
#[cfg(feature = "dsl-cron")]
pub use cron::CronPlugin;
#[cfg(feature = "dsl-io")]
pub use io::IoPlugin;
#[cfg(feature = "dsl-lock")]
pub use lock::LockPlugin;
#[cfg(feature = "dsl-log")]
pub use log::LogPlugin;
