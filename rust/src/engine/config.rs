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

/// Backend capabilities for feature detection
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BackendCapabilities {
    /// Supports parallel execution
    pub supports_parallel: bool,
    /// Supports persistent storage
    pub supports_persistence: bool,
    /// Supports incremental updates
    pub supports_incremental: bool,
    /// Maximum number of atoms (if limited)
    pub max_atoms: Option<usize>,
    /// List of supported features
    pub features: Vec<&'static str>,
}

impl BackendCapabilities {
    /// Capabilities for MORK backend
    pub fn mork() -> Self {
        Self {
            supports_parallel: true,
            supports_persistence: true,
            supports_incremental: true,
            max_atoms: None,
            features: vec!["native-rust", "zipper-execution", "pathmap-storage"],
        }
    }

    /// Capabilities for SWI-Prolog backend
    pub fn swipl() -> Self {
        Self {
            supports_parallel: false,
            supports_persistence: true,
            supports_incremental: true,
            max_atoms: None,
            features: vec!["mature", "prolog-based", "subprocess"],
        }
    }

    /// Get capabilities for a backend
    pub fn for_backend(backend: Backend) -> Self {
        match backend {
            Backend::Mork => Self::mork(),
            Backend::Swipl => Self::swipl(),
        }
    }

    /// Check if a feature is supported
    pub fn has_feature(&self, feature: &str) -> bool {
        self.features.contains(&feature)
    }
}

/// Backend-specific configuration options
#[derive(Debug, Clone, PartialEq)]
pub enum BackendConfig {
    /// MORK backend configuration
    Mork {
        /// Arena size for MORK (default: 1MB)
        arena_size: usize,
        /// Enable garbage collection
        enable_gc: bool,
    },
    /// SWI-Prolog backend configuration
    Swipl {
        /// Path to SWI-Prolog binary
        path: PathBuf,
    },
}

impl Default for BackendConfig {
    fn default() -> Self {
        Self::Mork {
            arena_size: 1024 * 1024,
            enable_gc: true,
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
#[derive(Debug, Clone, PartialEq)]
pub struct EngineConfig {
    /// Backend implementation to use (default: auto-detect).
    pub backend: Backend,
    /// Backend-specific configuration
    pub backend_config: BackendConfig,
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
    /// Enable parallel execution
    pub parallel: bool,
    /// Auto-detect best backend
    pub auto_detect: bool,
}

impl Default for EngineConfig {
    fn default() -> Self {
        Self {
            backend: Backend::Swipl,
            backend_config: BackendConfig::default(),
            swipl_path: PathBuf::from("swipl"),
            src_dir: None,
            verbose: false,
            profile: false,
            query_timeout: None,
            max_restarts: 0,
            min_swipl_version: MIN_SWIPL_VERSION,
            parallel: false,
            auto_detect: true,
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

    /// Enable or disable parallel execution.
    pub fn parallel(mut self, p: bool) -> Self {
        self.parallel = p;
        self
    }

    /// Enable auto-detection of best backend
    pub fn auto_detect(mut self, auto: bool) -> Self {
        self.auto_detect = auto;
        self
    }

    /// Set MORK backend options
    pub fn mork_opts(mut self, arena_size: usize, enable_gc: bool) -> Self {
        self.backend_config = BackendConfig::Mork { arena_size, enable_gc };
        self
    }

    /// Detect available backends and choose the best one
    pub fn detect_backend() -> Backend {
        if Self::is_mork_available() {
            Backend::Mork
        } else {
            Backend::Swipl
        }
    }

    /// Check if MORK backend is available
    fn is_mork_available() -> bool {
        #[cfg(feature = "mork")]
        {
            true
        }
        #[cfg(not(feature = "mork"))]
        {
            false
        }
    }

    /// Get backend capabilities
    pub fn capabilities(&self) -> BackendCapabilities {
        BackendCapabilities::for_backend(self.backend)
    }
}
