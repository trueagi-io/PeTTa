//! Engine configuration with smart defaults and builder pattern
//!
//! # Example
//!
//! ```rust,no_run
//! use petta::{EngineConfig, Backend};
//!
//! // Simple configuration
//! let config = EngineConfig::new(std::path::Path::new("."));
//!
//! // Custom configuration with builder
//! let config = EngineConfig::builder()
//!     .backend(Backend::Mork)
//!     .src_dir(std::path::PathBuf::from("my_lib"))
//!     .verbose(true)
//!     .max_restarts(5)
//!     .build();
//! ```

use std::fmt;
use std::path::{Path, PathBuf};
use std::time::Duration;

/// Backend selection for the PeTTa engine
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Backend {
    #[default]
    Swipl,
    Mork,
}

impl Backend {
    /// Auto-detect the best backend based on available features
    ///
    /// Returns Mork if the `mork` feature is enabled, otherwise Swipl.
    pub fn auto_detect() -> Self {
        #[cfg(feature = "mork")]
        return Backend::Mork;
        #[cfg(not(feature = "mork"))]
        return Backend::Swipl;
    }

    /// Check if this is the Mork backend
    #[inline]
    pub fn is_mork(self) -> bool {
        matches!(self, Backend::Mork)
    }

    /// Check if this is the Swipl backend
    #[inline]
    pub fn is_swipl(self) -> bool {
        matches!(self, Backend::Swipl)
    }
}

impl fmt::Display for Backend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Backend::Mork => write!(f, "Mork"),
            Backend::Swipl => write!(f, "Swipl"),
        }
    }
}

/// Engine configuration with smart defaults and builder pattern.
///
/// Configuration controls backend selection, paths, and runtime behavior.
/// All fields have sensible defaults for quick setup.
///
/// # Example
///
/// ```rust,no_run
/// use petta::{EngineConfig, Backend};
/// use std::path::Path;
///
/// // Quick start with defaults
/// let config = EngineConfig::new(Path::new("."));
///
/// // Custom configuration with builder
/// let config = EngineConfig::builder()
///     .backend(Backend::Mork)
///     .src_dir(Path::new("my_lib").to_path_buf())
///     .verbose(true)
///     .max_restarts(5)
///     .build();
/// ```
#[derive(Debug, Clone)]
pub struct EngineConfig {
    /// Backend to use for execution (Swipl or Mork)
    pub backend: Backend,
    /// Path to SWI-Prolog executable (used for Swipl backend)
    pub swipl_path: PathBuf,
    /// Source directory for MeTTa libraries
    pub src_dir: PathBuf,
    /// Enable verbose output and logging
    pub verbose: bool,
    /// Maximum number of backend restarts on crash
    pub max_restarts: u32,
    /// Optional timeout for operations
    pub timeout: Option<Duration>,
    /// WebSocket extension server port (set at runtime)
    #[cfg(feature = "websocket")]
    pub ws_port: Option<u16>,
    /// Extra command-line arguments forwarded to the Prolog subprocess
    pub extra_args: Vec<String>,
}

impl Default for EngineConfig {
    fn default() -> Self {
        Self {
            backend: Backend::auto_detect(),
            swipl_path: PathBuf::from("swipl"),
            src_dir: PathBuf::from("prolog"),
            verbose: false,
            max_restarts: 3,
            timeout: None,
            #[cfg(feature = "websocket")]
            ws_port: None,
            extra_args: Vec::new(),
        }
    }
}

impl EngineConfig {
    /// Create a new configuration for the given project root
    ///
    /// Sets the source directory to `{project_root}/prolog`
    pub fn new(project_root: &Path) -> Self {
        Self {
            src_dir: project_root.join("prolog"),
            ..Default::default()
        }
    }

    /// Create a new configuration builder
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use petta::{EngineConfig, Backend};
    ///
    /// let config = EngineConfig::builder()
    ///     .backend(Backend::Mork)
    ///     .verbose(true)
    ///     .build();
    /// ```
    pub fn builder() -> EngineConfigBuilder {
        EngineConfigBuilder::default()
    }

    /// Create a copy with verbose flag set
    #[inline]
    pub fn verbose(mut self, v: bool) -> Self {
        self.verbose = v;
        self
    }

    /// Create a copy with max_restarts set
    #[inline]
    pub fn max_restarts(mut self, n: u32) -> Self {
        self.max_restarts = n;
        self
    }

    /// Create a copy with backend set
    #[inline]
    pub fn backend(mut self, b: Backend) -> Self {
        self.backend = b;
        self
    }

    /// Create a copy with timeout set
    #[inline]
    pub fn timeout(mut self, t: Duration) -> Self {
        self.timeout = Some(t);
        self
    }

    /// Check if Mork backend is enabled
    #[inline]
    pub fn is_mork(&self) -> bool {
        self.backend.is_mork()
    }

    /// Check if Swipl backend is enabled
    #[inline]
    pub fn is_swipl(&self) -> bool {
        self.backend.is_swipl()
    }
}

/// Builder for [`EngineConfig`]
///
/// # Example
///
/// ```rust,no_run
/// use petta::{EngineConfig, EngineConfigBuilder, Backend};
///
/// let config: EngineConfig = EngineConfig::builder()
///     .backend(Backend::Mork)
///     .src_dir(std::path::PathBuf::from("my_lib"))
///     .verbose(true)
///     .max_restarts(5)
///     .build();
/// ```
#[derive(Debug, Default)]
pub struct EngineConfigBuilder {
    backend: Option<Backend>,
    src_dir: Option<PathBuf>,
    verbose: bool,
    max_restarts: u32,
    timeout: Option<Duration>,
}

impl EngineConfigBuilder {
    /// Set the backend (Mork or Swipl)
    #[inline]
    pub fn backend(mut self, backend: Backend) -> Self {
        self.backend = Some(backend);
        self
    }

    /// Set the source directory for MeTTa libraries
    #[inline]
    pub fn src_dir(mut self, path: PathBuf) -> Self {
        self.src_dir = Some(path);
        self
    }

    /// Enable verbose output
    #[inline]
    pub fn verbose(mut self, v: bool) -> Self {
        self.verbose = v;
        self
    }

    /// Set maximum number of restarts on crash
    #[inline]
    pub fn max_restarts(mut self, n: u32) -> Self {
        self.max_restarts = n;
        self
    }

    /// Set timeout for operations
    #[inline]
    pub fn timeout(mut self, t: Duration) -> Self {
        self.timeout = Some(t);
        self
    }

    /// Build the configuration
    ///
    /// Uses defaults for any fields not explicitly set:
    /// - `backend`: Auto-detected (Mork if available, otherwise Swipl)
    /// - `src_dir`: "prolog"
    /// - `verbose`: false
    /// - `max_restarts`: 3
    /// - `timeout`: None
    pub fn build(self) -> EngineConfig {
        EngineConfig {
            backend: self.backend.unwrap_or_else(Backend::auto_detect),
            swipl_path: PathBuf::from("swipl"),
            src_dir: self.src_dir.unwrap_or_else(|| PathBuf::from("prolog")),
            verbose: self.verbose,
            max_restarts: self.max_restarts,
            timeout: self.timeout,
            #[cfg(feature = "websocket")]
            ws_port: None,
            extra_args: Vec::new(),
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backend_display() {
        assert_eq!(Backend::Mork.to_string(), "Mork");
        assert_eq!(Backend::Swipl.to_string(), "Swipl");
    }

    #[test]
    fn test_backend_predicates() {
        assert!(Backend::Mork.is_mork());
        assert!(!Backend::Mork.is_swipl());
        assert!(Backend::Swipl.is_swipl());
        assert!(!Backend::Swipl.is_mork());
    }

    #[test]
    fn test_builder_pattern() {
        let config = EngineConfig::builder()
            .backend(Backend::Mork)
            .verbose(true)
            .max_restarts(5)
            .build();

        assert_eq!(config.backend, Backend::Mork);
        assert!(config.verbose);
        assert_eq!(config.max_restarts, 5);
    }

    #[test]
    fn test_default_config() {
        let config = EngineConfig::default();
        assert_eq!(config.max_restarts, 3);
        assert!(!config.verbose);
    }

    #[test]
    fn test_config_methods() {
        let config = EngineConfig::default()
            .verbose(true)
            .max_restarts(10);

        assert!(config.verbose);
        assert_eq!(config.max_restarts, 10);
    }

    #[test]
    fn test_backend_methods() {
        assert!(EngineConfig::default().backend.is_mork() || EngineConfig::default().backend.is_swipl());
    }
}
