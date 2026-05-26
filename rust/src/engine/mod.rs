//! Unified PeTTa engine with backend abstraction and automatic recovery.
//!
//! This module provides the core [`PeTTaEngine`] that unifies multiple backends
//! (SWI-Prolog, MORK) with automatic crash recovery, restart management, and
//! ergonomic APIs for MeTTa execution.
//!
//! # Architecture
//!
//! The engine architecture provides:
//! - **Backend Abstraction**: Trait-based design for multiple backends
//! - **Automatic Recovery**: Crash detection and automatic restart
//! - **Unified Errors**: Consistent error handling across backends
//! - **Configuration**: Flexible configuration with smart defaults
//!
//! # Example
//!
//! ```rust,no_run
//! use petta::{PeTTaEngine, EngineConfig};
//! use std::path::Path;
//!
//! // Create engine with configuration
//! let config = EngineConfig::new(Path::new("."));
//! let mut engine = PeTTaEngine::with_config(&config)?;
//!
//! // Execute MeTTa code
//! let result = engine.eval("!(+ 1 2)")?;
//! assert_eq!(result, "3");
//! # Ok::<_, petta::Error>(())
//! ```
//!
//! # Backend Selection
//!
//! Backend is auto-selected based on features:
//! - `mork` feature enabled → MORK backend (if available)
//! - Default → SWI-Prolog backend

mod backend;
pub mod backends;
mod client;
mod config;
mod errors;
mod formatters;
mod server;
mod subprocess;
mod version;

// Core types
pub use backend::BackendImpl;
pub use backends::SwiplBackend;
#[cfg(feature = "mork")]
pub use backends::MorkBackend;
pub use config::{Backend, EngineConfig, EngineConfigBuilder};
pub use errors::{BackendError, Error, parse_backend_error};

// Re-export for backward compatibility
#[deprecated(since = "0.5.0", note = "use Error instead")]
pub use errors::Error as PeTTaError;
pub use formatters::{create_formatter, CompactFormatter, JsonFormatter, OutputFormatter, PrettyFormatter, SExprFormatter};
pub use version::{MIN_SWIPL_VERSION, swipl_available};

// Internal imports
use crate::values::MettaResult;
use std::path::Path;

// ============================================================================
// Backend State Management
// ============================================================================

/// Unified backend state using trait objects for runtime polymorphism.
///
/// This enum wraps the concrete backend implementations, allowing the engine
/// to work with any backend through a common interface.
enum BackendState {
    Swipl(backends::SwiplBackend),
    #[cfg(feature = "mork")]
    Mork(backends::MorkBackend),
}

impl BackendState {
    /// Create new backend state from configuration
    fn new(config: &EngineConfig) -> Result<Self, Error> {
        match config.backend {
            #[cfg(feature = "mork")]
            Backend::Mork => Ok(Self::Mork(backends::MorkBackend::new())),
            #[cfg(not(feature = "mork"))]
            Backend::Mork => Err(Error::Mork(
                "Mork backend not available (requires nightly Rust)".into()
            )),
            Backend::Swipl => backends::SwiplBackend::new(config).map(Self::Swipl),
        }
    }

    /// Get backend name
    fn name(&self) -> &'static str {
        match self {
            #[cfg(feature = "mork")]
            Self::Mork(backend) => backend.name(),
            Self::Swipl(backend) => backend.name(),
        }
    }

    /// Check if backend is alive
    fn is_alive(&mut self) -> bool {
        match self {
            #[cfg(feature = "mork")]
            Self::Mork(backend) => backend.is_alive(),
            Self::Swipl(backend) => backend.is_alive(),
        }
    }

    /// Get stderr output
    fn stderr(&self) -> String {
        match self {
            #[cfg(feature = "mork")]
            Self::Mork(backend) => backend.stderr_output(),
            Self::Swipl(backend) => backend.stderr_output(),
        }
    }

    /// Restart backend
    fn restart(&mut self, config: &EngineConfig) -> Result<(), Error> {
        match self {
            #[cfg(feature = "mork")]
            Self::Mork(backend) => backend.restart(config),
            Self::Swipl(backend) => backend.restart(config),
        }
    }

    /// Shutdown backend
    fn shutdown(&mut self) {
        match self {
            #[cfg(feature = "mork")]
            Self::Mork(backend) => backend.shutdown(),
            Self::Swipl(backend) => backend.shutdown(),
        }
    }

    /// Load single file
    fn load_metta_file(&mut self, path: &Path, config: &EngineConfig) -> Result<Vec<MettaResult>, Error> {
        match self {
            #[cfg(feature = "mork")]
            Self::Mork(backend) => backend.load_metta_file(path, config),
            Self::Swipl(backend) => backend.load_metta_file(path, config),
        }
    }

    /// Load multiple files
    fn load_metta_files(&mut self, paths: &[&Path], config: &EngineConfig) -> Result<Vec<MettaResult>, Error> {
        match self {
            #[cfg(feature = "mork")]
            Self::Mork(backend) => backend.load_metta_files(paths, config),
            Self::Swipl(backend) => backend.load_metta_files(paths, config),
        }
    }

    /// Process MeTTa string
    fn process_metta_string(&mut self, code: &str, config: &EngineConfig) -> Result<Vec<MettaResult>, Error> {
        match self {
            #[cfg(feature = "mork")]
            Self::Mork(backend) => backend.process_metta_string(code, config),
            Self::Swipl(backend) => backend.process_metta_string(code, config),
        }
    }
}

// ============================================================================
// PeTTa Engine - Main Interface
// ============================================================================

/// Main PeTTa execution engine with automatic crash recovery.
///
/// The engine provides a unified interface to MeTTa execution supporting:
/// - **Multiple Backends**: SWI-Prolog and MORK
/// - **Automatic Recovery**: Crash detection and restart
/// - **Ergonomic API**: Simple methods for common operations
/// - **Error Handling**: Rich error types with suggestions
///
/// # Example
///
/// ```rust,no_run
/// use petta::{PeTTaEngine, EngineConfig};
/// use std::path::Path;
///
/// // Create engine
/// let config = EngineConfig::new(Path::new("."));
/// let mut engine = PeTTaEngine::with_config(&config)?;
///
/// // Execute code
/// let result = engine.eval("!(+ 1 2)")?;
/// assert_eq!(result, "3");
/// # Ok::<_, petta::Error>(())
/// ```
pub struct PeTTaEngine {
    backend: BackendState,
    config: EngineConfig,
    restarts: u32,
    #[cfg(feature = "websocket")]
    _ws_server: Option<crate::ws_ext::WsExtensionServer>,
}

impl PeTTaEngine {
    /// Create engine with explicit configuration.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use petta::{PeTTaEngine, EngineConfig};
    /// use std::path::Path;
    ///
    /// let config = EngineConfig::new(Path::new("."));
    /// let mut engine = PeTTaEngine::with_config(&config)?;
    /// # Ok::<_, petta::Error>(())
    /// ```
    pub fn with_config(config: &EngineConfig) -> Result<Self, Error> {
        #[allow(unused_mut)]
        let mut config = config.clone();

        #[cfg(feature = "websocket")]
        let ws_server = {
            match crate::ws_ext::WsExtensionServer::spawn(
                "repos/OmegaClaw-Core/memory/vector_store.json".into()
            ) {
                Ok(server) => {
                    config.ws_port = Some(server.port);
                    Some(server)
                }
                Err(e) => {
                    eprintln!("Warning: WS extension server failed to start: {}", e);
                    None
                }
            }
        };

        Ok(Self {
            backend: BackendState::new(&config)?,
            config,
            restarts: 0,
            #[cfg(feature = "websocket")]
            _ws_server: ws_server,
        })
    }

    /// Create engine with default configuration for a project root.
    ///
    /// # Arguments
    ///
    /// * `project_root` - Root directory for the project
    /// * `verbose` - Enable verbose output
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use petta::PeTTaEngine;
    /// use std::path::Path;
    ///
    /// let mut engine = PeTTaEngine::new(Path::new("."), false)?;
    /// # Ok::<_, petta::Error>(())
    /// ```
    pub fn new(project_root: &Path, verbose: bool) -> Result<Self, Error> {
        let config = EngineConfig::new(project_root).verbose(verbose);
        Self::with_config(&config)
    }

    /// Get the backend name (e.g., "SWI-Prolog" or "MORK")
    pub fn backend_name(&self) -> &'static str {
        self.backend.name()
    }

    /// Check if backend is alive and responsive
    pub fn is_alive(&mut self) -> bool {
        self.backend.is_alive()
    }

    /// Load and execute a single MeTTa file with crash recovery.
    pub fn load_metta_file(&mut self, path: &Path) -> Result<Vec<MettaResult>, Error> {
        self.retry_on_crash(|backend, cfg| backend.load_metta_file(path, cfg))
    }

    /// Load and execute multiple MeTTa files with crash recovery.
    pub fn load_metta_files(&mut self, paths: &[&Path]) -> Result<Vec<MettaResult>, Error> {
        self.retry_on_crash(|backend, cfg| backend.load_metta_files(paths, cfg))
    }

    /// Process a MeTTa code string with crash recovery.
    pub fn process_metta_string(&mut self, code: &str) -> Result<Vec<MettaResult>, Error> {
        self.retry_on_crash(|backend, cfg| backend.process_metta_string(code, cfg))
    }

    /// Get stderr output from the backend
    pub fn stderr_output(&self) -> String {
        self.backend.stderr()
    }

    /// Get current configuration
    pub fn config(&self) -> &EngineConfig {
        &self.config
    }

    /// Shutdown backend gracefully
    pub fn shutdown(&mut self) {
        self.backend.shutdown();
    }

    // =========================================================================
    // High-Level Convenience Methods
    // =========================================================================

    /// Evaluate MeTTa expression and return first result as string.
    ///
    /// Convenience method for simple queries returning single results.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use petta::PeTTaEngine;
    /// use std::path::Path;
    ///
    /// let mut engine = PeTTaEngine::new(Path::new("."), false)?;
    /// let result = engine.eval("!(+ 1 2)")?;
    /// assert_eq!(result, "3");
    /// # Ok::<_, petta::Error>(())
    /// ```
    pub fn eval(&mut self, code: &str) -> Result<String, Error> {
        self.process_metta_string(code)
            .and_then(|mut r| r.pop().ok_or_else(|| Error::Protocol("No results".into())))
            .map(|r| r.value)
    }

    /// Evaluate expression and parse as integer.
    ///
    /// Convenience for arithmetic expressions.
    pub fn eval_int(&mut self, code: &str) -> Result<i64, Error> {
        self.eval(code)?.parse().map_err(|_| Error::Protocol("Not an integer".into()))
    }

    /// Load and execute multiple files (generic over path types).
    pub fn load_files<P: AsRef<Path>>(&mut self, paths: &[P]) -> Result<Vec<MettaResult>, Error> {
        let refs: Vec<&Path> = paths.iter().map(|p| p.as_ref()).collect();
        self.load_metta_files(&refs)
    }

    /// Health check - verify engine is responsive.
    pub fn health_check(&mut self) -> bool {
        self.is_alive() || self.process_metta_string("!(id 1)").is_ok()
    }

    // =========================================================================
    // Internal Implementation
    // =========================================================================

    /// Retry operation on crash with automatic restart
    fn retry_on_crash<F>(&mut self, mut f: F) -> Result<Vec<MettaResult>, Error>
    where
        F: FnMut(&mut BackendState, &EngineConfig) -> Result<Vec<MettaResult>, Error>,
    {
        let mut attempts = 0u32;
        loop {
            match f(&mut self.backend, &self.config) {
                Err(Error::Protocol(ref m)) if m.contains("child closed") => {
                    if attempts >= self.config.max_restarts {
                        return Err(Error::Crash { restarts: self.restarts });
                    }
                    attempts += 1;
                    self.backend.restart(&self.config)?;
                    self.restarts = self.restarts.saturating_add(1);
                }
                other => return other,
            }
        }
    }
}

impl Drop for PeTTaEngine {
    fn drop(&mut self) {
        self.shutdown();
    }
}
