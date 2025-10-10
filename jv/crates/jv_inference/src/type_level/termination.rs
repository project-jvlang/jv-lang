use crate::constraint::AstId;
use std::fmt;

/// Configuration governing termination checks for type-level evaluation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TerminationConfig {
    max_depth: usize,
}

impl TerminationConfig {
    /// Default maximum recursion depth enforced during evaluation.
    pub const DEFAULT_MAX_DEPTH: usize = 100;

    /// Creates a configuration with the supplied depth limit.
    pub fn new(max_depth: usize) -> Self {
        Self { max_depth }
    }

    /// Returns the configured maximum evaluation depth.
    pub fn max_depth(&self) -> usize {
        self.max_depth
    }
}

impl Default for TerminationConfig {
    fn default() -> Self {
        Self::new(Self::DEFAULT_MAX_DEPTH)
    }
}

/// Error produced when evaluation fails to terminate.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TerminationError {
    /// The evaluator exceeded the configured recursion depth.
    DepthLimitExceeded { limit: usize },
    /// A dependency cycle was detected while traversing expressions.
    CycleDetected { cycle: Vec<AstId> },
}

impl fmt::Display for TerminationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TerminationError::DepthLimitExceeded { limit } => {
                write!(f, "型レベル評価が最大深度 {limit} を超過しました。")
            }
            TerminationError::CycleDetected { cycle } => {
                write!(
                    f,
                    "型レベル評価が循環参照を検出しました (経路: {:?}).",
                    cycle
                )
            }
        }
    }
}

/// Guard tracking recursion depth and visited nodes to enforce termination.
#[derive(Debug)]
pub struct TerminationGuard {
    config: TerminationConfig,
    depth: usize,
    stack: Vec<AstId>,
}

impl TerminationGuard {
    /// Creates a guard bound to the provided configuration.
    pub fn new(config: TerminationConfig) -> Self {
        Self {
            config,
            depth: 0,
            stack: Vec::new(),
        }
    }

    /// Enters a new evaluation frame, updating depth and detecting cycles.
    pub fn enter(&mut self, node: Option<AstId>) -> Result<(), TerminationError> {
        let next_depth = self.depth + 1;
        if next_depth > self.config.max_depth() {
            return Err(TerminationError::DepthLimitExceeded {
                limit: self.config.max_depth(),
            });
        }

        if let Some(node) = node {
            if self.stack.contains(&node) {
                let mut cycle = self.stack.clone();
                cycle.push(node);
                return Err(TerminationError::CycleDetected { cycle });
            }
            self.stack.push(node);
        }

        self.depth = next_depth;
        Ok(())
    }

    /// Leaves the current evaluation frame, unwinding depth and stack.
    pub fn exit(&mut self, node: Option<AstId>) {
        if self.depth > 0 {
            self.depth -= 1;
        }
        if let Some(node) = node {
            if let Some(position) = self.stack.iter().rposition(|candidate| *candidate == node) {
                self.stack.remove(position);
            }
        }
    }

    /// Returns the current recursion depth (testing visibility only).
    #[cfg(test)]
    pub fn depth(&self) -> usize {
        self.depth
    }

    /// Returns the current evaluation stack (testing visibility only).
    #[cfg(test)]
    pub fn stack(&self) -> &[AstId] {
        &self.stack
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_config_uses_depth_100() {
        let config = TerminationConfig::default();
        assert_eq!(config.max_depth(), 100);
    }

    #[test]
    fn guard_tracks_depth_and_cycles() {
        let config = TerminationConfig::new(3);
        let mut guard = TerminationGuard::new(config);

        guard.enter(Some(1)).expect("enter root");
        assert_eq!(guard.depth(), 1);
        assert_eq!(guard.stack(), &[1]);

        guard.enter(Some(2)).expect("enter child");
        assert_eq!(guard.depth(), 2);
        assert_eq!(guard.stack(), &[1, 2]);

        let cycle = guard.enter(Some(1)).expect_err("cycle detection");
        assert!(matches!(cycle, TerminationError::CycleDetected { .. }));

        guard.exit(Some(2));
        assert_eq!(guard.depth(), 1);
        assert_eq!(guard.stack(), &[1]);

        guard.exit(Some(1));
        assert_eq!(guard.depth(), 0);
        assert!(guard.stack().is_empty());
    }

    #[test]
    fn guard_enforces_depth_limit() {
        let config = TerminationConfig::new(1);
        let mut guard = TerminationGuard::new(config);

        guard.enter(None).expect("first level");
        let error = guard.enter(None).expect_err("limit exceeded");
        assert_eq!(
            error,
            TerminationError::DepthLimitExceeded {
                limit: config.max_depth()
            }
        );
    }
}
