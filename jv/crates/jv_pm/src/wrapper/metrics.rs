use std::fmt::Debug;
use std::time::Instant;

use tracing::{info, warn};

/// Measures the duration of a phase and logs it for comparison with `/usr/bin/time -p`.
pub fn measure<T, E, F>(phase: &'static str, operation: F) -> Result<T, E>
where
    F: FnOnce() -> Result<T, E>,
    E: Debug,
{
    let start = Instant::now();
    let result = operation();
    let elapsed = start.elapsed();
    let elapsed_s = elapsed.as_secs_f64();
    match &result {
        Ok(_) => info!(
            phase = phase,
            elapsed_s,
            elapsed_ms = elapsed_s * 1000.0,
            "phase completed"
        ),
        Err(error) => warn!(
            phase = phase,
            elapsed_s,
            elapsed_ms = elapsed_s * 1000.0,
            error = ?error,
            "phase failed"
        ),
    }
    result
}
