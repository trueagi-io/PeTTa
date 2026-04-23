//! Command-line interface for PeTTa
//!
//! Provides a structured CLI using clap with support for:
//! - File execution
//! - Interactive REPL mode
//! - Backend selection (MORK or SWI-Prolog)
//! - Verbose output and timing

use clap::{Parser, ValueEnum};
use petta::Backend;

/// PeTTa - MeTTa language runtime
#[derive(Parser, Debug, Clone)]
#[command(name = "petta")]
#[command(author = "Patrick Hammer")]
#[command(version = "0.5.0")]
#[command(about = "Efficient MeTTa language implementation in Rust + persistent SWI-Prolog subprocess", long_about = None)]
pub struct Cli {
    /// Files to execute
    #[arg(required = false)]
    pub files: Vec<String>,

    /// Enable verbose output
    #[arg(short, long, default_value_t = false)]
    pub verbose: bool,

    /// Show execution time
    #[arg(short, long, default_value_t = false)]
    pub time: bool,

    /// Backend to use
    #[arg(short, long, default_value = "prolog")]
    pub backend: BackendArg,

    /// Start interactive REPL
    #[arg(short = 'i', long, default_value_t = false)]
    pub interactive: bool,
}

/// Backend implementation selection
#[derive(ValueEnum, Clone, Debug, PartialEq)]
pub enum BackendArg {
    /// MORK - Native Rust zipper-based execution backend (opt-in, requires nightly)
    Mork,
    /// SWI-Prolog - Persistent subprocess backend (default, stable Rust)
    Prolog,
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
    /// Convert to engine Backend enum
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
        assert_eq!(
            BackendArg::Mork.to_backend(),
            Backend::Mork
        );
        assert_eq!(
            BackendArg::Prolog.to_backend(),
            Backend::Swipl
        );
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
