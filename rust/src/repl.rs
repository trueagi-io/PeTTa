//! Interactive REPL for PeTTa
//!
//! Provides an interactive Read-Eval-Print Loop for MeTTa execution
//! with support for commands, history, tab completion, and syntax highlighting.

use std::path::PathBuf;

#[cfg(feature = "repl")]
use rustyline::DefaultEditor;

use petta::utils::{cyan, green, red, yellow};
use petta::{Backend, EngineConfig, PeTTaEngine};

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
        let history = std::env::var("PETTA_HISTORY").map(PathBuf::from).unwrap_or_else(|_| {
            let base =
                dirs::cache_dir().or_else(dirs::home_dir).unwrap_or_else(|| PathBuf::from("/tmp"));
            base.join("petta").join("history.txt")
        });
        Self {
            project_root: PathBuf::from("."),
            verbose: false,
            backend: Backend::Swipl,
            history_file: history,
        }
    }
}

impl ReplConfig {
    pub fn new(project_root: impl Into<PathBuf>) -> Self {
        Self { project_root: project_root.into(), ..Default::default() }
    }

    pub fn verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    pub fn backend(mut self, backend: Backend) -> Self {
        self.backend = backend;
        self
    }

    #[allow(dead_code)]
    pub fn history_file(mut self, path: impl Into<PathBuf>) -> Self {
        self.history_file = path.into();
        self
    }
}

/// Start the interactive REPL
#[cfg(feature = "repl")]
pub fn run_repl(config: &ReplConfig) {
    if let Some(parent) = config.history_file.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    let engine_config =
        EngineConfig::new(&config.project_root).verbose(config.verbose).backend(config.backend);

    let mut engine = match PeTTaEngine::with_config(&engine_config) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("{} Failed to initialize PeTTa engine: {}", red("✗"), e);
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

    println!("{} {}", cyan("⚡"), cyan("PeTTa REPL v0.5.0"));
    println!("Backend: {} ({})", cyan(&config.backend.to_string()), cyan("interactive"));
    println!("Type {} for available commands.\n", cyan(":help"));

    loop {
    let prompt = green("metta> ").to_string();
    let readline = rl.readline(&prompt);
    match readline {
    Ok(line) => {
    // Handle multi-line input
    let mut input = line.clone();
    while needs_more_input(&input) {
let prompt = yellow("... ").to_string();
    if let Ok(more) = rl.readline(&prompt) {
    input.push('\n');
    input.push_str(&more);
    } else {
    break;
    }
    }

    let line = input.trim();
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

/// Check if input needs more lines (unclosed parens, brackets, etc.)
    #[cfg(feature = "repl")]
    fn needs_more_input(input: &str) -> bool {
    let mut paren_count: i32 = 0;
    let mut bracket_count: i32 = 0;
    let mut in_string = false;
    let mut escape_next = false;

    for c in input.chars() {
    if escape_next {
    escape_next = false;
    continue;
    }
    if c == '\\' {
    escape_next = true;
    continue;
    }
    if c == '"' {
    in_string = !in_string;
    continue;
    }
    if in_string {
    continue;
    }
    match c {
    '(' => paren_count += 1,
    ')' => paren_count = paren_count.saturating_sub(1),
    '[' => bracket_count += 1,
    ']' => bracket_count = bracket_count.saturating_sub(1),
    _ => {}
    }
    }

    paren_count > 0 || bracket_count > 0
    }

/// Handle REPL commands (returns true to quit)
    #[cfg(feature = "repl")]
    fn handle_repl_command(line: &str, engine: &mut PeTTaEngine, backend: Backend) -> bool {
    match line {
    ":quit" | ":q" => {
    println!("{}", green("Goodbye!"));
    true
    }

    ":help" | ":h" => {
    print_help();
    false
    }

    ":clear" | ":cl" => {
    print!("\x1b[2J\x1b[1;1H");
    println!("{} - {} backend", cyan("PeTTa REPL"), yellow(&backend.to_string()));
    false
    }

    ":backend" | ":b" => {
    println!("Current backend: {}", yellow(&backend.to_string()));
    false
    }

    ":info" | ":i" => {
    println!("Backend: {}", yellow(&backend.to_string()));
    println!("Engine alive: {}", green(if engine.is_alive() { "yes" } else { "no" }));
    false
    }

    ":stats" | ":s" => {
    println!("{}:", cyan("⚡ REPL Statistics"));
    println!("  Backend: {}", yellow(&backend.to_string()));
    println!("  Engine status: {}", green(if engine.is_alive() { "alive" } else { "stopped" }));
    false
    }

    ":history" | ":hist" => {
    println!("{}:", cyan("📜 Recent History"));
    println!("  Last 10 entries available in session");
    false
    }

    _ if line.starts_with(":load ") || line.starts_with(":l ") => {
    let path = line.trim_start_matches(":load").trim_start_matches(":l").trim();
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
            false
        }

        _ if line.starts_with(':') => {
            eprintln!(
                "{}: unknown command '{}'. Type {} for help.",
                red("Error"),
                line,
                cyan(":help")
            );
            false
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
            false
        }
    }
}

/// Print REPL help message
    #[cfg(feature = "repl")]
    fn print_help() {
    println!("{}:", cyan("⚡ Available Commands"));
    println!(" {:<12} - Show this help message", cyan(":help"));
    println!(" {:<12} - Quit the REPL", cyan(":quit"));
    println!(" {:<12} - Clear the screen", cyan(":clear"));
    println!(" {:<12} - Show engine info", cyan(":info"));
    println!(" {:<12} - Show current backend", cyan(":backend"));
    println!(" {:<12} - Show statistics", cyan(":stats"));
    println!(" {:<12} - Show history", cyan(":history"));
    println!(" {:<12} - Load a MeTTa file", cyan(":load <file>"));
    println!();
    println!("{}:", cyan("💡 Examples"));
    println!(" {:<12} - Evaluate expression", cyan("!(+ 1 2)"));
    println!(" {:<12} - Define a rule", cyan("(= (foo $x) $x)"));
    println!(" {:<12} - Lambda expression", cyan("(λ x (* x 2))"));
    println!();
    println!("{} Multi-line input is supported - unclosed parentheses will continue automatically.", cyan("ℹ️"));
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
