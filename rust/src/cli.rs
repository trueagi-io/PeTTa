//! Command-line interface for PeTTa
//!
//! Provides a structured CLI using clap with support for:
//! - File execution
//! - Interactive REPL mode
//! - Backend selection (MORK or SWI-Prolog)
//! - Verbose output and timing

use clap::{Parser, ValueEnum};
use petta::Backend;

/// PeTTa - Production MeTTa Runtime
    #[derive(Parser, Debug, Clone)]
    #[command(name = "petta")]
    #[command(author = "Patrick Hammer")]
    #[command(version = "0.5.0")]
    #[command(about = "🦀 PeTTa v0.5.0 - Production MeTTa Runtime with dual-backend intelligence",
        long_about = "PeTTa (Production MeTTa Runtime) is the definitive implementation of the MeTTa language\nfor advanced cognitive agent research.\n\nFeatures:\n  ✨ Dual-backend intelligence (MORK native Rust + SWI-Prolog)\n  🎨 Beautiful error messages with emoji support\n  ⚡ High performance execution\n  🔧 Effortless extensibility")]
    pub struct Cli {
    /// MeTTa files to execute
    #[arg(required = false, value_name = "FILES")]
    pub files: Vec<String>,

    /// Enable verbose debug output
    #[arg(short, long, default_value_t = false)]
    pub verbose: bool,

    /// Show execution timing information
    #[arg(short, long, default_value_t = false)]
    pub time: bool,

    /// Backend to use for execution [default: auto]
    #[arg(short, long, default_value = "prolog", value_name = "BACKEND")]
    pub backend: BackendArg,

    /// Start interactive REPL mode
    #[arg(short = 'i', long, default_value_t = false)]
    pub interactive: bool,

    /// Output format for results
    #[arg(short = 'O', long, default_value = "pretty", value_name = "FORMAT")]
    pub output_format: OutputFormat,

    /// Enable performance profiling
    #[arg(long, default_value_t = false)]
    pub profile: bool,

    /// Enable step-by-step trace output
    #[arg(long, default_value_t = false)]
    pub trace: bool,

    /// Show execution statistics
    #[arg(long, default_value_t = false)]
    pub stats: bool,
    }

/// Backend implementation selection
    #[derive(ValueEnum, Clone, Debug, PartialEq)]
    pub enum BackendArg {
    /// MORK - Native Rust zipper-based execution backend (requires nightly)
    Mork,
    /// SWI-Prolog - Persistent subprocess backend (default, stable Rust)
    Prolog,
    }

    /// Output format for results
    #[derive(ValueEnum, Clone, Debug, PartialEq, Default)]
    pub enum OutputFormat {
    /// Human-readable output with colors (default)
    #[default]
    Pretty,
    /// Minimal whitespace, compact output
    Compact,
    /// JSON format for machine processing
    Json,
    /// Canonical S-expression format
    SExpr,
    }

impl std::fmt::Display for BackendArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BackendArg::Mork => write!(f, "Mork"),
            BackendArg::Prolog => write!(f, "Swipl"),
        }
    }
}

impl BackendArg {
    pub fn to_backend(&self) -> Backend {
        match self {
            BackendArg::Mork => Backend::Mork,
            BackendArg::Prolog => Backend::Swipl,
        }
    }
}

impl Default for Cli {
fn default() -> Self {
Self {
files: Vec::new(),
verbose: false,
time: false,
backend: BackendArg::Prolog,
interactive: false,
output_format: OutputFormat::Pretty,
profile: false,
trace: false,
stats: false,
}
}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cli_default() {
        let cli = Cli::default();
        assert!(cli.files.is_empty());
        assert!(!cli.verbose);
        assert!(!cli.time);
        assert_eq!(cli.backend, BackendArg::Prolog);
        assert!(!cli.interactive);
    }

    #[test]
    fn test_backend_conversion() {
        assert_eq!(BackendArg::Mork.to_backend(), Backend::Mork);
        assert_eq!(BackendArg::Prolog.to_backend(), Backend::Swipl);
    }

    #[test]
    fn test_parse_args() {
        let cli = Cli::parse_from(["petta", "file.metta"]);
        assert_eq!(cli.files, vec!["file.metta"]);
    }

    #[test]
    fn test_parse_verbose() {
        let cli = Cli::parse_from(["petta", "-v", "file.metta"]);
        assert!(cli.verbose);

        let cli = Cli::parse_from(["petta", "--verbose", "file.metta"]);
        assert!(cli.verbose);
    }

    #[test]
    fn test_parse_backend() {
        let cli = Cli::parse_from(["petta", "-b", "mork", "file.metta"]);
        assert_eq!(cli.backend, BackendArg::Mork);

        let cli = Cli::parse_from(["petta", "--backend", "prolog", "file.metta"]);
        assert_eq!(cli.backend, BackendArg::Prolog);
    }

    #[test]
    fn test_parse_interactive() {
        let cli = Cli::parse_from(["petta", "-i"]);
        assert!(cli.interactive);

        let cli = Cli::parse_from(["petta", "--interactive"]);
        assert!(cli.interactive);
    }
}
