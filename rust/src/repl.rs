//! Interactive REPL for PeTTa
//!
//! Provides an interactive Read-Eval-Print Loop for MeTTa execution
//! with support for commands, history, and syntax highlighting.

use std::path::PathBuf;

#[cfg(feature = "repl")]
use rustyline::DefaultEditor;

use petta::{Backend, EngineConfig, PeTTaEngine};
use crate::utils::{cyan, green, red, yellow};

/// REPL configuration
#[derive(Debug, Clone)]
pub struct ReplConfig {
    pub project_root: PathBuf,
    pub verbose: bool,
    pub backend: Backend,
    pub history_file: PathBuf,
}

impl Default for ReplConfig {
    fn default() -> Self {
        Self {
            project_root: PathBuf::from("."),
            verbose: false,
            backend: Backend::Swipl,
            history_file: PathBuf::from("/tmp/petta_history.txt"),
        }
    }
}

impl ReplConfig {
    pub fn new(project_root: impl Into<PathBuf>) -> Self {
        Self {
            project_root: project_root.into(),
            ..Default::default()
        }
    }

    pub fn verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    pub fn backend(mut self, backend: Backend) -> Self {
        self.backend = backend;
        self
    }

    pub fn history_file(mut self, path: impl Into<PathBuf>) -> Self {
        self.history_file = path.into();
        self
    }
}

/// Start the interactive REPL
#[cfg(feature = "repl")]
pub fn run_repl(config: &ReplConfig) {
    let engine_config = EngineConfig::new(&config.project_root)
        .verbose(config.verbose)
        .backend(config.backend);

    let mut engine = match PeTTaEngine::with_config(&engine_config) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("Failed to initialize PeTTa engine: {}", e);
            return;
        }
    };

    let mut rl = DefaultEditor::new().unwrap_or_else(|_| {
        eprintln!("Warning: Failed to initialize line editor");
        DefaultEditor::new().unwrap()
    });

    // Load history if available
    if config.history_file.exists() {
        let _ = rl.load_history(&config.history_file);
    }

    println!(
        "{} - {} backend",
        cyan("PeTTa REPL"),
        yellow(&config.backend.to_string())
    );
    println!("Type {} for available commands.\n", cyan(":help"));

    loop {
        let readline = rl.readline(&format!("{} ", green("metta")));
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                let _ = rl.add_history_entry(line);

                // Handle REPL commands
                if handle_repl_command(line, &mut engine, config.backend) {
                    break;
                }
            }
            Err(rustyline::error::ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(rustyline::error::ReadlineError::Eof) => {
                println!("{}", green("Goodbye!"));
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    // Save history
    let _ = rl.save_history(&config.history_file);
}

/// Handle REPL commands (returns true to quit)
#[cfg(feature = "repl")]
fn handle_repl_command(
    line: &str,
    engine: &mut PeTTaEngine,
    backend: Backend,
) -> bool {
    match line {
        ":quit" | ":q" => {
            println!("{}", green("Goodbye!"));
            return true;
        }

        ":help" | ":h" => {
            print_help();
            return false;
        }

        ":clear" | ":cl" => {
            print!("\x1b[2J\x1b[1;1H");
            println!(
                "{} - {} backend",
                cyan("PeTTa REPL"),
                yellow(&backend.to_string())
            );
            return false;
        }

        ":backend" | ":b" => {
            println!("Current backend: {}", yellow(&backend.to_string()));
            return false;
        }

        ":info" | ":i" => {
            println!("Backend: {}", yellow(&backend.to_string()));
            println!(
                "Engine alive: {}",
                green(if engine.is_alive() { "yes" } else { "no" })
            );
            return false;
        }

        _ if line.starts_with(":load ") || line.starts_with(":l ") => {
            let path = line
                .trim_start_matches(":load")
                .trim_start_matches(":l")
                .trim();
            let path_buf = PathBuf::from(path);
            if !path_buf.exists() {
                eprintln!("{}: file not found: {}", red("Error"), path);
                return false;
            }
            match engine.load_metta_file(&path_buf) {
                Ok(results) => {
                    for r in &results {
                        println!("{}", r.value);
                    }
                    println!("\n{} loaded successfully", green(path));
                }
                Err(e) => eprintln!("{}: {}", red("Error"), e),
            }
            return false;
        }

        _ if line.starts_with(':') => {
            eprintln!(
                "{}: unknown command '{}'. Type {} for help.",
                red("Error"),
                line,
                cyan(":help")
            );
            return false;
        }

        // Execute MeTTa expression
        _ => {
            match engine.process_metta_string(line) {
                Ok(results) => {
                    for r in &results {
                        if let Some(t) = r.parsed_value() {
                            println!(" {} ({:?})", r.value, t);
                        } else {
                            println!(" {}", r.value);
                        }
                    }
                }
                Err(e) => eprintln!(" {}", red(&e.to_string())),
            }
            return false;
        }
    }
}

/// Print REPL help message
#[cfg(feature = "repl")]
fn print_help() {
    println!("{}:", cyan("Available Commands"));
    println!(" {:<12} - Show this help message", cyan(":help"));
    println!(" {:<12} - Quit the REPL", cyan(":quit"));
    println!(" {:<12} - Clear the screen", cyan(":clear"));
    println!(" {:<12} - Show engine info", cyan(":info"));
    println!(" {:<12} - Show current backend", cyan(":backend"));
    println!(" {:<12} - Load a MeTTa file", cyan(":load <file>"));
    println!();
    println!("{}:", cyan("Examples"));
    println!(" {:<12} - Evaluate expression", cyan("!(+ 1 2)"));
    println!(" {:<12} - Define a rule", cyan("(= (foo $x) $x)"));
    println!();
    println!("{}:", cyan("CLI Options"));
    println!(" {:<12} - Enable verbose output", cyan("-v"));
    println!(" {:<12} - Show execution time", cyan("-t"));
    println!(" {:<12} - Set backend (mork/prolog)", cyan("-b mork"));
}

/// Fallback when repl feature is disabled
#[cfg(not(feature = "repl"))]
pub fn run_repl(_config: &ReplConfig) {
    eprintln!("REPL support is disabled. Enable the 'repl' feature to use interactive mode.");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_repl_config_default() {
        let config = ReplConfig::default();
        assert!(!config.verbose);
        assert_eq!(config.backend, Backend::Swipl);
    }

    #[test]
    fn test_repl_config_builder() {
        let config = ReplConfig::new("/tmp")
            .verbose(true)
            .backend(Backend::Mork)
            .history_file("/tmp/test_history.txt");

        assert!(config.verbose);
        assert_eq!(config.backend, Backend::Mork);
        assert_eq!(config.history_file, PathBuf::from("/tmp/test_history.txt"));
    }
}
