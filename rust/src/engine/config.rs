use std::path::{Path, PathBuf};
use std::time::Duration;

use super::version::MIN_SWIPL_VERSION;

/// Backend implementation to use for MeTTa execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Backend {
    /// MORK - Native Rust zipper-based execution backend (opt-in, requires nightly).
    Mork,
    /// SWI-Prolog - Persistent subprocess backend (default, stable Rust).
    #[default]
    #[allow(dead_code)]
    Swipl,
}

impl std::fmt::Display for Backend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Backend::Mork => write!(f, "Mork"),
            Backend::Swipl => write!(f, "Swipl"),
        }
    }
}

/// Configuration options for the [`PeTTaEngine`](super::PeTTaEngine).
///
/// Uses the builder pattern:
/// ```ignore
/// let config = EngineConfig::new(&project_root)
///     .verbose(true)
///     .query_timeout(Duration::from_secs(30))
///     .max_restarts(3);
/// ```
#[derive(Debug, Clone)]
pub struct EngineConfig {
    /// Backend implementation to use (default: Mork).
    pub backend: Backend,
    /// Path to the SWI-Prolog binary (defaults to "swipl").
    pub swipl_path: PathBuf,
    /// Source directory containing .pl files (defaults to `<project_root>/src`).
    pub src_dir: Option<PathBuf>,
    /// Enable verbose debug output from Prolog.
    pub verbose: bool,
    /// Enable query profiling.
    pub profile: bool,
    /// Timeout for query execution. None means no timeout.
    pub query_timeout: Option<Duration>,
    /// Maximum number of automatic restarts on subprocess crash (default: 0).
    pub max_restarts: u32,
    /// Minimum SWI-Prolog version required (major, minor).
    pub min_swipl_version: (u32, u32),
}

impl Default for EngineConfig {
    fn default() -> Self {
        Self {
            // Default to the SWI-Prolog subprocess backend which works on stable Rust.
            backend: Backend::Swipl,
            swipl_path: PathBuf::from("swipl"),
            src_dir: None,
            verbose: false,
            profile: false,
            query_timeout: None,
            max_restarts: 0,
            min_swipl_version: MIN_SWIPL_VERSION,
        }
    }
}

impl EngineConfig {
    /// Create a new config with the given project root.
    /// The Prolog source files are expected to be in `<project_root>/prolog/`.
    pub fn new(project_root: &Path) -> Self {
        Self {
            src_dir: Some(project_root.join("prolog")),
            ..Default::default()
        }
    }

    /// Set the SWI-Prolog binary path.
    pub fn swipl_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.swipl_path = path.into();
        self
    }

    /// Set the source directory for .pl files.
    pub fn src_dir(mut self, dir: impl Into<PathBuf>) -> Self {
        self.src_dir = Some(dir.into());
        self
    }

    /// Enable or disable query profiling.
    pub fn profile(mut self, p: bool) -> Self {
        self.profile = p;
        self
    }

    /// Enable or disable verbose Prolog debug output.
    pub fn verbose(mut self, v: bool) -> Self {
        self.verbose = v;
        self
    }

    /// Set a timeout for queries.
    pub fn query_timeout(mut self, timeout: Duration) -> Self {
        self.query_timeout = Some(timeout);
        self
    }

    /// Set the maximum number of automatic restarts on crash.
    pub fn max_restarts(mut self, n: u32) -> Self {
        self.max_restarts = n;
        self
    }

    /// Set the backend implementation to use.
    pub fn backend(mut self, backend: Backend) -> Self {
        self.backend = backend;
        self
    }
}
