//! Simplified interactive REPL

use std::path::PathBuf;
#[cfg(feature = "repl")]
use rustyline::DefaultEditor;

use petta::utils::{cyan, green, red, yellow};
use petta::{Backend, EngineConfig, PeTTaEngine};

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
dirs::cache_dir().or_else(dirs::home_dir).unwrap_or_else(|| PathBuf::from("/tmp")).join("petta").join("history.txt")
});
Self { project_root: PathBuf::from("."), verbose: false, backend: Backend::Swipl, history_file: history }
}
}

impl ReplConfig {
pub fn new(project_root: impl Into<PathBuf>) -> Self {
Self { project_root: project_root.into(), ..Default::default() }
}

pub fn verbose(mut self, verbose: bool) -> Self { self.verbose = verbose; self }
pub fn backend(mut self, backend: Backend) -> Self { self.backend = backend; self }
pub fn history_file(mut self, path: impl Into<PathBuf>) -> Self { self.history_file = path.into(); self }
}

#[cfg(feature = "repl")]
pub fn run_repl(config: &ReplConfig) {
let _ = std::fs::create_dir_all(config.history_file.parent().unwrap_or(&PathBuf::from(".")));

let Ok(mut engine) = PeTTaEngine::with_config(
&EngineConfig::new(&config.project_root).verbose(config.verbose).backend(config.backend)
) else {
eprintln!("{} Failed to initialize PeTTa engine", red("✗"));
return;
};

let mut rl = DefaultEditor::new().unwrap_or_else(|_| {
eprintln!("Warning: Failed to initialize line editor");
DefaultEditor::new().unwrap()
});

let _ = rl.load_history(&config.history_file);

println!("{} {}", cyan("⚡"), cyan("PeTTa REPL v0.5.0"));
println!("Backend: {} ({})", cyan(&config.backend.to_string()), cyan("interactive"));
println!("Type {} for available commands.\n", cyan(":help"));

loop {
let readline = rl.readline(&green("metta> ").to_string());
match readline {
Ok(line) => {
if line.trim().is_empty() { continue; }
let _ = rl.add_history_entry(&line);

if handle_command(&line, &mut engine, config.backend) { break; }
}
Err(rustyline::error::ReadlineError::Interrupted) => continue,
Err(rustyline::error::ReadlineError::Eof) => {
println!("{}", green("Goodbye!"));
break;
}
Err(err) => {
eprintln!("Error: {err:?}");
break;
}
}
}

let _ = rl.save_history(&config.history_file);
}

#[cfg(feature = "repl")]
fn handle_command(line: &str, engine: &mut PeTTaEngine, backend: Backend) -> bool {
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
println!(" Backend: {}", yellow(&backend.to_string()));
println!(" Engine status: {}", green(if engine.is_alive() { "alive" } else { "stopped" }));
false
}
_ => {
if line.starts_with(":load ") || line.starts_with(":l ") {
let path = line.trim_start_matches(":load ").trim_start_matches(":l ");
match engine.load_metta_file(&PathBuf::from(path)) {
Ok(results) => {
for r in &results { println!("{}", r.value); }
println!("\n{} loaded successfully", green(path));
}
Err(e) => eprintln!("{}: {e}", red("Error")),
}
} else if line.starts_with(':') {
eprintln!("{}: unknown command '{line}'. Type {} for help.", red("Error"), cyan(":help"));
} else {
match engine.process_metta_string(line) {
Ok(results) => {
for r in &results {
println!(" {} {}", r.value, r.parsed_value().map(|v| format!("({v:?})")).unwrap_or_default());
}
}
Err(e) => eprintln!(" {e}"),
}
}
false
}
}
}

#[cfg(feature = "repl")]
fn print_help() {
println!("{}:", cyan("⚡ Available Commands"));
println!(" {:<12} - Show this help message", cyan(":help"));
println!(" {:<12} - Quit the REPL", cyan(":quit"));
println!(" {:<12} - Clear the screen", cyan(":clear"));
println!(" {:<12} - Show engine info", cyan(":info"));
println!(" {:<12} - Show current backend", cyan(":backend"));
println!(" {:<12} - Show statistics", cyan(":stats"));
println!(" {:<12} - Load a MeTTa file", cyan(":load <file>"));
println!();
println!("{}:", cyan("💡 Examples"));
println!(" {:<12} - Evaluate expression", cyan("!(+ 1 2)"));
println!(" {:<12} - Define a rule", cyan("(= (foo $x) $x)"));
println!(" {:<12} - Lambda expression", cyan("(λ x (* x 2))"));
}

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
