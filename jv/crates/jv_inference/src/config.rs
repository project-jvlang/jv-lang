//! Parallel execution configuration for the inference engine.
//!
//! The design document describes a configurable pipeline that can switch between
//! deterministic single-threaded execution and parallel strategies for module
//! and constraint processing. At this stage the configuration keeps track of
//! the requested behaviour so that downstream crates (checker, CLI, LSP) can
//! pass user preferences into the solver. Future tasks will extend the engine
//! to honour these settings with actual task scheduling.

/// Controls the parallelism strategy applied by the inference engine.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParallelInferenceConfig {
    /// Enables module-level parallel execution when `true`.
    pub module_parallelism: bool,
    /// Number of constraints processed together in a batch.
    pub constraint_batching: usize,
    /// Maximum worker threads available for parallel processing.
    pub worker_threads: usize,
}

impl ParallelInferenceConfig {
    /// Builds a configuration with explicit parameters.
    pub fn new(
        module_parallelism: bool,
        constraint_batching: usize,
        worker_threads: usize,
    ) -> Self {
        Self {
            module_parallelism,
            constraint_batching,
            worker_threads,
        }
        .sanitized()
    }

    /// Returns a configuration that guarantees deterministic single-threaded
    /// execution. Useful for CI and regression testing.
    pub fn deterministic() -> Self {
        Self {
            module_parallelism: false,
            constraint_batching: 1,
            worker_threads: 1,
        }
    }

    /// Enforces invariants such as minimum batch/worker counts and ensures we
    /// never advertise parallel workers when module parallelism is disabled.
    pub fn sanitized(mut self) -> Self {
        if self.constraint_batching == 0 {
            self.constraint_batching = 1;
        }

        if self.worker_threads == 0 {
            self.worker_threads = 1;
        }

        if !self.module_parallelism {
            self.worker_threads = 1;
        }

        self
    }

    /// Returns the number of workers that are actually effective after
    /// sanitisation (module parallelism may clamp it to one).
    pub fn effective_workers(&self) -> usize {
        self.sanitized().worker_threads
    }
}

impl Default for ParallelInferenceConfig {
    fn default() -> Self {
        Self::deterministic()
    }
}

#[cfg(test)]
mod tests {
    use super::ParallelInferenceConfig;

    #[test]
    fn deterministic_defaults() {
        let config = ParallelInferenceConfig::default();
        assert!(!config.module_parallelism);
        assert_eq!(config.constraint_batching, 1);
        assert_eq!(config.worker_threads, 1);
    }

    #[test]
    fn sanitizes_zero_values() {
        let config = ParallelInferenceConfig::new(true, 0, 0);
        assert_eq!(config.constraint_batching, 1);
        assert_eq!(config.worker_threads, 1);
    }

    #[test]
    fn disables_workers_when_parallelism_off() {
        let config = ParallelInferenceConfig::new(false, 4, 8);
        assert_eq!(config.effective_workers(), 1);
        assert_eq!(config.worker_threads, 1);
    }
}
