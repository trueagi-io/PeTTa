//! PeTTa CLI — streamlined command-line interface

mod cli;
mod repl;

use cli::{BackendArg, Cli, OutputFormat};
use petta::utils::{cyan, green, red, yellow};
use clap::Parser;
use std::path::Path;

fn main() {
let args = Cli::parse();

if args.files.is_empty() && !args.interactive {
print_banner();
run_demo(&find_project_root(), args.backend);
return;
}

if args.interactive {
run_repl_mode(&find_project_root(), args.backend, args.verbose);
return;
}

run_files(&args);
}

fn print_banner() {
println!("{} {}", cyan("⚡ PeTTa"), yellow("v0.5.0"));
println!("{}", cyan("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"));
println!("{} {}", green("🧠"), yellow("Production MeTTa Runtime"));
}

fn find_project_root() -> std::path::PathBuf {
std::env::var("PETTA_PATH")
.and_then(|p| Ok(Path::new(&p).to_path_buf()))
.ok()
.and_then(|p| p.join("prolog").join("metta.pl").exists().then_some(p))
.or_else(|| std::env::current_dir().ok().filter(|d| d.join("prolog").join("metta.pl").exists()))
.or_else(|| {
std::env::current_exe().ok().and_then(|exe| {
exe.parent()
.and_then(|d| d.join("prolog").join("metta.pl").exists().then(|| d.to_path_buf()))
.or_else(|| exe.parent().and_then(|d| d.parent().and_then(|p| p.join("prolog").join("metta.pl").exists().then(|| p.to_path_buf()))))
})
})
.unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| Path::new(".").to_path_buf()))
}

fn run_demo(project_root: &Path, backend: BackendArg) {
use petta::PeTTaEngine;

let Ok(mut engine) = PeTTaEngine::with_config(
&petta::EngineConfig::new(project_root).backend(backend.to_backend())
) else {
eprintln!("{} Failed to initialize PeTTa engine", red("✗"));
std::process::exit(1);
};

println!(
"{}\nBackend: {}\n{}",
cyan("⚡ PeTTa Demo"),
yellow(&backend.to_string()),
cyan("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
);

for (name, code) in [("Identity", "(= (myid $x) $x) !(myid 42)"), ("Arithmetic", "!(+ 1 2)"), ("Boolean", "!(and true false)")] {
match engine.process_metta_string(code) {
Ok(results) => {
println!("\n{} {}:", green("✓"), cyan(name));
for r in &results {
println!(" {} {}", r.value, r.parsed_value().map(|v| format!("({v:?})")).unwrap_or_default());
}
}
Err(e) => eprintln!(" {} {e}", red("✗")),
}
}
println!("\n{}.", green("✓ Done"));
}

fn run_files(args: &Cli) {
use petta::PeTTaEngine;
use std::time::Instant;

let start = args.time.then(Instant::now);
let paths: Vec<_> = args.files.iter()
.filter_map(|f| {
let p = std::path::PathBuf::from(f);
p.exists().then_some(p)
}).collect();

if paths.is_empty() { std::process::exit(1); }

let config = petta::EngineConfig::new(&find_project_root())
.verbose(args.verbose)
.backend(args.backend.to_backend());

let Ok(mut engine) = PeTTaEngine::with_config(&config) else {
eprintln!("{} Failed to initialize engine", red("✗"));
std::process::exit(1);
};

let mut failed = false;
for path in &paths {
if let Err(e) = process_file(&mut engine, path, args.output_format.clone()) {
eprintln!("{} {e}", red("✗ Error processing"));
failed = true;
}
}

if let Some(start) = start {
eprintln!("\n{} {} backend: {:.3}ms", yellow("⏱ Timing:"), args.backend, start.elapsed().as_millis() as f64);
}

if failed { std::process::exit(1); }
}

fn process_file(engine: &mut petta::PeTTaEngine, path: &std::path::PathBuf, fmt: OutputFormat) -> Result<(), String> {
let results = engine.load_metta_file(path).map_err(|e| e.to_string())?;
let stderr = engine.stderr_output();

if !stderr.is_empty() { eprintln!("{}:\n{}", red("Prolog stderr"), red(&stderr)); }

for r in &results {
println!("{}", match fmt {
OutputFormat::Json => serde_json::to_string(&r.value).unwrap_or_default(),
_ => r.value.clone(),
});
}
Ok(())
}

fn run_repl_mode(project_root: &Path, backend: BackendArg, verbose: bool) {
let config = repl::ReplConfig::new(project_root).verbose(verbose).backend(backend.to_backend());
repl::run_repl(&config);
}
