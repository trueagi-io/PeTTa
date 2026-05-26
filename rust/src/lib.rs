//! # PeTTa - Production MeTTa Runtime
//!
//! PeTTa is a production-grade MeTTa runtime with dual backends:
//! - **SWI-Prolog WAM**: Mature, stable backend with full MeTTa semantics
//! - **MORK**: High-performance native Rust backend with parallel execution
//!
//! ## Quick Start
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
//! ## Features
//!
//! - `mork` - High-performance MORK backend (requires nightly Rust)
//! - `repl` - Interactive REPL with history
//! - `clap` - CLI argument parsing
//! - `fast-hasher` - GXHash acceleration (AES/SSE2 required)
//! - `parallel` - Parallel batch execution
//! - `async` - Async/await with Tokio
//!
//! ## Architecture
//!
//! PeTTa provides unified backend abstraction through [`PeTTaEngine`]:
//! - Backend lifecycle management
//! - Automatic crash recovery  
//! - Rich error handling with suggestions
//! - Multiple output formatters
//!
//! ## Example: File Operations
//!
//! ```rust,no_run
//! use petta::PeTTaEngine;
//! use std::path::Path;
//!
//! let mut engine = PeTTaEngine::new(Path::new("."), false)?;
//!
//! // Load MeTTa files
//! engine.load_files(&["defs.metta", "rules.metta"])?;
//!
//! // Execute query
//! let result = engine.eval("!(your-query)")?;
//! println!("Result: {}", result);
//! # Ok::<_, petta::Error>(())
//! ```
//!
//! ## Example: MORK Backend
//!
//! ```rust,no_run
//! # #[cfg(feature = "mork")]
//! # {
//! use petta::{PeTTaEngine, EngineConfig, Backend};
//!
//! let config = EngineConfig::builder()
//!     .backend(Backend::Mork)
//!     .build();
//!
//! let mut engine = PeTTaEngine::with_config(&config)?;
//! # }
//! # Ok::<_, petta::Error>(())
//! ```

#![cfg_attr(feature = "mork", feature(core_intrinsics))]
#![cfg_attr(feature = "mork", feature(portable_simd))]
#![cfg_attr(feature = "mork", feature(allocator_api))]
#![warn(missing_docs)]

// Core modules
pub mod engine;
pub mod parser;
pub mod utils;
pub mod optimize;
pub mod values;
pub mod core;

#[cfg(feature = "mork")]
pub mod mork;

// Public API
pub mod api;
pub mod backends;

// Internal modules
mod cli;
mod repl;

#[cfg(feature = "websocket")]
pub mod ws_ext;
mod profiler;
mod observability;
mod reliability;
pub mod differential;
mod viz;
mod gxhash;

// ============================================================================
// Core Engine Exports
// ============================================================================

pub use engine::{
Backend, BackendError, EngineConfig, EngineConfigBuilder,
Error, PeTTaEngine,
};
pub use values::{MettaResult, MettaValue};

// Ergonomic API (re-export from api module)
pub use api::{PeTTa, PeTTaBuilder, PeTTaEngine as ApiEngine, EngineConfig as ApiEngineConfig};

// Deprecated exports for backward compatibility
#[deprecated(since = "0.5.0", note = "use Error instead")]
pub use engine::Error as PeTTaError;

// ============================================================================
// Output Formatting
// ============================================================================

pub use engine::{
    create_formatter, CompactFormatter, JsonFormatter, OutputFormatter,
    PrettyFormatter, SExprFormatter,
};

// ============================================================================
// CLI and REPL
// ============================================================================

pub use cli::{BackendArg, Cli, OutputFormat};
pub use repl::{run_repl, ReplConfig};

// ============================================================================
// Profiling and Monitoring
// ============================================================================

pub use profiler::{ProfileStats, QueryProfile};
pub use observability::{Metrics, ObservabilityConfig, ServiceStatus};
pub use reliability::{CircuitBreaker, CircuitState, ReliabilityConfig};

// ============================================================================
// Differential Testing
// ============================================================================

pub use differential::{
    assert_backend_parity, compare_results, BackendResult, DifferentialTest,
    ParityTestSuite, TestSuiteResult,
};

// ============================================================================
// Visualization
// ============================================================================

pub use viz::{format_stats_table, visualize_expression};

// ============================================================================
// Optional Exports
// ============================================================================

/// GXHash hasher (available with `fast-hasher` feature)
#[cfg(feature = "fast-hasher")]
pub use gxhash::GxHasher;

// ============================================================================
// Re-exports for convenience
// ============================================================================

/// Parser utilities for MeTTa S-expressions
pub mod parse {
    pub use crate::parser::{parse_metta, serialize_metta};
}

/// Utility functions
pub mod util {
    pub use crate::utils::{format_duration_ms, find_best_match, levenshtein, truncate};
}
