use std::borrow::Cow;
use std::time::{Duration, Instant};

use crate::context::TransformContext;
use crate::{TransformPoolMetrics, TransformPoolSessionMetrics};

/// Captures the elapsed time for a logical stage within the lowering pipeline.
#[derive(Debug, Clone)]
pub struct StageTiming {
    name: Cow<'static, str>,
    elapsed: Duration,
}

impl StageTiming {
    /// Returns the stage name as a borrowed string slice.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the elapsed duration for the stage.
    pub fn elapsed(&self) -> Duration {
        self.elapsed
    }

    /// Returns the elapsed time in milliseconds.
    pub fn elapsed_millis(&self) -> f64 {
        duration_to_millis(self.elapsed)
    }
}

/// Aggregated performance metrics produced by AST→IR lowering.
#[derive(Debug, Clone, Default)]
pub struct PerfMetrics {
    total_duration: Duration,
    stages: Vec<StageTiming>,
    pool_metrics: Option<TransformPoolMetrics>,
    pool_session: Option<TransformPoolSessionMetrics>,
    warm_start: Option<bool>,
    peak_rss_bytes: Option<u64>,
}

impl PerfMetrics {
    /// Total elapsed time for the profiled session.
    pub fn total_duration(&self) -> Duration {
        self.total_duration
    }

    /// Total elapsed time in milliseconds.
    pub fn total_millis(&self) -> f64 {
        duration_to_millis(self.total_duration)
    }

    /// Returns recorded stage timings.
    pub fn stages(&self) -> &[StageTiming] {
        &self.stages
    }

    /// Looks up a stage by name.
    pub fn stage(&self, name: &str) -> Option<&StageTiming> {
        self.stages.iter().find(|stage| stage.name() == name)
    }

    /// 指定したステージの経過時間（ミリ秒）を返す。
    pub fn stage_millis(&self, name: &str) -> Option<f64> {
        self.stage(name).map(|stage| stage.elapsed_millis())
    }

    /// ステージ計測が存在しない場合は合計時間（ミリ秒）を返す。
    pub fn stage_millis_or_total(&self, name: &str) -> f64 {
        self.stage_millis(name)
            .unwrap_or_else(|| self.total_millis())
    }

    /// Returns cumulative pool metrics captured so far.
    pub fn pool_metrics(&self) -> Option<&TransformPoolMetrics> {
        self.pool_metrics.as_ref()
    }

    /// Returns metrics recorded for the most recent lowering session.
    pub fn pool_session(&self) -> Option<&TransformPoolSessionMetrics> {
        self.pool_session.as_ref()
    }

    /// Indicates whether the session reused arena allocations.
    pub fn was_warm_start(&self) -> Option<bool> {
        self.warm_start
    }

    /// Convenience helper exposing the current reuse ratio.
    pub fn reuse_ratio(&self) -> Option<f64> {
        self.pool_metrics
            .as_ref()
            .map(|metrics| metrics.reuse_ratio())
    }

    /// Returns the peak resident set size in bytes when available.
    pub fn peak_rss_bytes(&self) -> Option<u64> {
        self.peak_rss_bytes
    }
}

/// Profiler responsible for capturing timing and allocation metrics.
#[derive(Debug, Default)]
pub struct TransformProfiler {
    active: Option<ActiveSession>,
    latest: Option<PerfMetrics>,
}

impl TransformProfiler {
    /// Creates a new profiler with no active session.
    pub fn new() -> Self {
        Self::default()
    }

    /// Starts a fresh profiling session.
    pub fn begin_session(&mut self) {
        self.active = Some(ActiveSession::new());
        self.latest = None;
    }

    /// Measures a logical stage, recording its elapsed duration.
    pub fn measure_stage<F, R>(&mut self, stage: impl Into<Cow<'static, str>>, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        if self.active.is_none() {
            self.begin_session();
        }

        let start = Instant::now();
        let result = f();
        let elapsed = start.elapsed();

        if let Some(active) = self.active.as_mut() {
            active.stages.push(StageTiming {
                name: stage.into(),
                elapsed,
            });
        }

        result
    }

    /// Completes the current session and produces metrics.
    pub fn finish_session(&mut self, context: &TransformContext) -> PerfMetrics {
        let Some(active) = self.active.take() else {
            // No active session; return empty metrics snapshot.
            let metrics = PerfMetrics::default();
            self.latest = Some(metrics.clone());
            return metrics;
        };

        let metrics = PerfMetrics {
            total_duration: active.start.elapsed(),
            stages: active.stages,
            pool_metrics: context.pool_metrics(),
            pool_session: context.last_pool_session(),
            warm_start: context.last_pool_warm_start(),
            peak_rss_bytes: capture_peak_rss_bytes(),
        };

        self.latest = Some(metrics.clone());
        metrics
    }

    /// Returns the most recent metrics snapshot, if any.
    pub fn latest_metrics(&self) -> Option<&PerfMetrics> {
        self.latest.as_ref()
    }

    /// Cancels the current session without recording metrics.
    pub fn abort_session(&mut self) {
        self.active = None;
        self.latest = None;
    }

    /// Indicates whether a profiling session is currently active.
    pub fn is_active(&self) -> bool {
        self.active.is_some()
    }
}

#[derive(Debug)]
struct ActiveSession {
    start: Instant,
    stages: Vec<StageTiming>,
}

impl ActiveSession {
    fn new() -> Self {
        Self {
            start: Instant::now(),
            stages: Vec::new(),
        }
    }
}

fn duration_to_millis(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1_000.0
}

fn capture_peak_rss_bytes() -> Option<u64> {
    capture_peak_rss_bytes_impl()
}

#[cfg(all(feature = "perf-metrics", target_os = "linux"))]
fn capture_peak_rss_bytes_impl() -> Option<u64> {
    use std::fs;

    let contents = fs::read_to_string("/proc/self/status").ok()?;
    for line in contents.lines() {
        if let Some(rest) = line.strip_prefix("VmHWM:") {
            let mut parts = rest.split_whitespace();
            let value = parts.next()?.parse::<u64>().ok()?;
            let unit = parts.next().unwrap_or("kB");
            return Some(match unit {
                "kB" => value * 1024,
                "mB" | "MB" => value * 1024 * 1024,
                _ => value,
            });
        }
    }
    None
}

#[cfg(not(all(feature = "perf-metrics", target_os = "linux")))]
fn capture_peak_rss_bytes_impl() -> Option<u64> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TransformContext;
    use std::time::Duration;

    #[test]
    fn records_stage_timings_and_metrics() {
        let mut profiler = TransformProfiler::new();
        profiler.begin_session();

        let value: u64 = profiler.measure_stage("lowering", || {
            std::thread::sleep(Duration::from_millis(5));
            42
        });
        assert_eq!(value, 42);

        let context = TransformContext::new();
        let metrics = profiler.finish_session(&context);

        assert!(metrics.total_duration() >= Duration::from_millis(5));
        let stage = metrics.stage("lowering").expect("recorded stage");
        assert!(stage.elapsed() >= Duration::from_millis(5));
        assert!(metrics.reuse_ratio().is_none());
    }

    #[test]
    fn aborting_session_discards_measurements() {
        let mut profiler = TransformProfiler::new();
        profiler.begin_session();
        profiler.measure_stage("lowering", || 1 + 1);
        profiler.abort_session();

        let context = TransformContext::new();
        let metrics = profiler.finish_session(&context);
        assert_eq!(metrics.stages().len(), 0);
    }
}
