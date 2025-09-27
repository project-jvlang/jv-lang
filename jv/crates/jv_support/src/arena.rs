use bumpalo::Bump;
use std::num::NonZeroUsize;

const DEFAULT_CHUNK_BYTES: usize = 64 * 1024;
const MIN_CHUNK_BYTES: usize = 4 * 1024;

/// Aggregated statistics collected across all pool sessions.
#[derive(Debug, Clone, Default)]
pub struct PoolMetrics {
    pub sessions: u64,
    pub warm_sessions: u64,
    pub total_allocations: u64,
    pub total_bytes: u64,
}

impl PoolMetrics {
    /// Returns the fraction of sessions that reused an existing arena.
    pub fn reuse_ratio(&self) -> f64 {
        if self.sessions == 0 {
            0.0
        } else {
            self.warm_sessions as f64 / self.sessions as f64
        }
    }
}

/// Per-arena metrics captured during a single lowering session.
#[derive(Debug, Clone, Default)]
pub struct ArenaMetrics {
    allocations: u64,
    bytes: u64,
}

impl ArenaMetrics {
    fn record(&mut self, bytes: usize) {
        self.allocations += 1;
        self.bytes += bytes as u64;
    }

    /// Total allocations served by the arena during the session.
    pub fn allocations(&self) -> u64 {
        self.allocations
    }

    /// Total bytes reserved (best-effort) during the session.
    pub fn bytes(&self) -> u64 {
        self.bytes
    }
}

/// Combined metrics produced for a single pool guard lifetime.
#[derive(Debug, Clone, Default)]
pub struct PoolSessionMetrics {
    pub ast: ArenaMetrics,
    pub ir: ArenaMetrics,
}

impl PoolSessionMetrics {
    pub fn total_allocations(&self) -> u64 {
        self.ast.allocations() + self.ir.allocations()
    }

    pub fn total_bytes(&self) -> u64 {
        self.ast.bytes() + self.ir.bytes()
    }
}

/// Shared memory pools used by AST→IR lowering.
#[derive(Debug)]
pub struct TransformPools {
    ast_pool: Bump,
    ir_pool: Bump,
    metrics: PoolMetrics,
    chunk_bytes: NonZeroUsize,
    last_session: Option<PoolSessionMetrics>,
}

impl TransformPools {
    /// Default chunk size used when pre-allocating arenas.
    pub const DEFAULT_CHUNK_BYTES: usize = DEFAULT_CHUNK_BYTES;

    /// Constructs pools with the default chunk size (64KiB).
    pub fn new() -> Self {
        Self::with_chunk_capacity(Self::DEFAULT_CHUNK_BYTES)
    }

    /// Constructs pools with a customised initial chunk size.
    pub fn with_chunk_capacity(bytes: usize) -> Self {
        let chunk = bytes.max(MIN_CHUNK_BYTES);
        let chunk_bytes = NonZeroUsize::new(chunk).expect("chunk size must be non-zero");
        Self {
            ast_pool: Bump::with_capacity(chunk),
            ir_pool: Bump::with_capacity(chunk),
            metrics: PoolMetrics::default(),
            chunk_bytes,
            last_session: None,
        }
    }

    /// Returns the configured chunk size used when growing arenas.
    pub fn chunk_bytes(&self) -> usize {
        self.chunk_bytes.get()
    }

    /// Returns aggregated metrics collected so far.
    pub fn metrics(&self) -> &PoolMetrics {
        &self.metrics
    }

    /// Returns the metrics from the most recent session, if any.
    pub fn last_session(&self) -> Option<&PoolSessionMetrics> {
        self.last_session.as_ref()
    }

    /// Borrows the pools for a single lowering session.
    pub fn acquire(&mut self) -> TransformPoolsGuard<'_> {
        let warm_start = self.metrics.sessions > 0;
        TransformPoolsGuard {
            parent: self,
            session_metrics: PoolSessionMetrics::default(),
            warm_start,
        }
    }
}

/// Session guard that provides scoped access to the pools.
#[derive(Debug)]
pub struct TransformPoolsGuard<'a> {
    parent: &'a mut TransformPools,
    session_metrics: PoolSessionMetrics,
    warm_start: bool,
}

impl<'a> TransformPoolsGuard<'a> {
    /// Returns true when this session reused an arena from a prior run.
    pub fn is_warm_start(&self) -> bool {
        self.warm_start
    }

    /// Provides access to the AST arena for allocations within the session.
    pub fn ast(&mut self) -> ArenaAccessor<'_, '_> {
        ArenaAccessor {
            bump: &self.parent.ast_pool,
            metrics: &mut self.session_metrics.ast,
        }
    }

    /// Provides access to the IR arena for allocations within the session.
    pub fn ir(&mut self) -> ArenaAccessor<'_, '_> {
        ArenaAccessor {
            bump: &self.parent.ir_pool,
            metrics: &mut self.session_metrics.ir,
        }
    }

    /// Returns the live metrics collected so far during this session.
    pub fn metrics(&self) -> &PoolSessionMetrics {
        &self.session_metrics
    }
}

impl Drop for TransformPoolsGuard<'_> {
    fn drop(&mut self) {
        self.parent.metrics.sessions += 1;
        if self.warm_start {
            self.parent.metrics.warm_sessions += 1;
        }
        self.parent.metrics.total_allocations += self.session_metrics.total_allocations();
        self.parent.metrics.total_bytes += self.session_metrics.total_bytes();
        self.parent.last_session = Some(self.session_metrics.clone());

        // All references issued from the arenas must be dropped before this runs.
        self.parent.ast_pool.reset();
        self.parent.ir_pool.reset();
    }
}

/// Accessor around a bump arena with bookkeeping helpers.
#[derive(Debug)]
pub struct ArenaAccessor<'arena, 'metrics> {
    bump: &'arena Bump,
    metrics: &'metrics mut ArenaMetrics,
}

impl<'arena, 'metrics> ArenaAccessor<'arena, 'metrics> {
    /// Allocates a single value inside the arena, returning a mutable reference.
    pub fn alloc<T>(&mut self, value: T) -> &'arena mut T {
        let bytes = std::mem::size_of_val(&value);
        self.metrics.record(bytes);
        self.bump.alloc(value)
    }

    /// Allocates a slice filled by repeatedly invoking the generator.
    pub fn alloc_slice_with<T, F>(&mut self, len: usize, mut generator: F) -> &'arena mut [T]
    where
        F: FnMut(usize) -> T,
    {
        let bytes = std::mem::size_of::<T>() * len;
        self.metrics.record(bytes);
        self.bump.alloc_slice_fill_with(len, |idx| generator(idx))
    }

    /// Allocates a slice by cloning a template value.
    pub fn alloc_slice_clone<T: Clone>(&mut self, len: usize, value: &T) -> &'arena mut [T] {
        self.alloc_slice_with(len, |_| value.clone())
    }

    /// Allocates a slice by copying a `Copy` value.
    pub fn alloc_slice_copy<T: Copy>(&mut self, len: usize, value: T) -> &'arena mut [T] {
        let bytes = std::mem::size_of::<T>() * len;
        self.metrics.record(bytes);
        self.bump.alloc_slice_fill_copy(len, value)
    }

    /// Allocates a UTF-8 string copy inside the arena.
    pub fn alloc_str(&mut self, value: &str) -> &'arena mut str {
        self.metrics.record(value.len());
        self.bump.alloc_str(value)
    }

    /// Manually records an allocation when using raw `bumpalo` collections.
    pub fn record_manual_allocation(&mut self, bytes: usize, allocations: usize) {
        if allocations > 0 {
            self.metrics.allocations += allocations as u64;
        }
        self.metrics.bytes += bytes as u64;
    }

    /// Exposes the underlying bump arena for advanced use cases.
    /// Call [`record_manual_allocation`] to keep metrics accurate when
    /// performing manual allocations.
    pub fn bump(&self) -> &'arena Bump {
        self.bump
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn records_metrics_and_reuse_across_sessions() {
        let mut pools = TransformPools::new();

        {
            let mut session = pools.acquire();
            {
                let mut ast = session.ast();
                let greeting = ast.alloc(String::from("hello"));
                assert_eq!(greeting, "hello");
                let numbers = ast.alloc_slice_copy(4, 7u32);
                assert_eq!(numbers, &[7, 7, 7, 7]);
            }
            {
                let mut ir = session.ir();
                let flag = ir.alloc(true);
                assert!(*flag);
                let template = String::from("ir");
                let words = ir.alloc_slice_clone(3, &template);
                assert_eq!(words.len(), 3);
            }
            assert!(!session.is_warm_start());
            assert!(session.metrics().total_allocations() >= 3);
        }

        {
            let mut session = pools.acquire();
            assert!(session.is_warm_start());
            let mut ast = session.ast();
            let reused = ast.alloc(String::from("reuse"));
            assert_eq!(reused, "reuse");
        }

        let metrics = pools.metrics();
        assert_eq!(metrics.sessions, 2);
        assert_eq!(metrics.warm_sessions, 1);
        assert!(metrics.total_allocations >= 4);
        assert!(metrics.total_bytes > 0);

        let last = pools.last_session().expect("last session metrics");
        assert!(last.total_allocations() >= 1);
    }

    #[test]
    fn respects_minimum_chunk_size() {
        let pools = TransformPools::with_chunk_capacity(512);
        assert_eq!(pools.chunk_bytes(), MIN_CHUNK_BYTES);
    }
}
