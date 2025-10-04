use std::borrow::Cow;

#[cfg(feature = "profiling")]
mod enabled {
    use super::Cow;
    use std::time::Instant;

    pub struct ProfilerGuard {
        label: Cow<'static, str>,
        start: Instant,
    }

    impl ProfilerGuard {
        pub fn new(label: Cow<'static, str>) -> Self {
            Self {
                label,
                start: Instant::now(),
            }
        }
    }

    impl Drop for ProfilerGuard {
        fn drop(&mut self) {
            let elapsed = self.start.elapsed();
            eprintln!(
                "[profiling] {}: {:.3}ms",
                self.label,
                elapsed.as_secs_f64() * 1000.0
            );
        }
    }

    pub fn start(label: impl Into<Cow<'static, str>>) -> ProfilerGuard {
        ProfilerGuard::new(label.into())
    }
}

#[cfg(not(feature = "profiling"))]
mod disabled {
    use super::Cow;

    #[derive(Debug, Default, Clone, Copy)]
    pub struct NoopGuard;

    pub fn start<T>(_label: T) -> NoopGuard
    where
        T: Into<Cow<'static, str>>,
    {
        NoopGuard
    }
}

#[cfg(feature = "profiling")]
pub use enabled::start;

#[cfg(not(feature = "profiling"))]
pub use disabled::start;
