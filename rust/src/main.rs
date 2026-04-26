//! PeTTa CLI — Command-line interface for MeTTa execution

mod cli;
mod repl;

use cli::{BackendArg, Cli};
use petta::utils::{cyan, green, red, yellow};

use clap::Parser;
use std::path::Path;
use std::time::Instant;

fn main() {
    // Parse CLI arguments
    let cli = Cli::parse();

    // Handle demo mode (no files provided)
    if cli.files.is_empty() && !cli.interactive {
        run_demo(&find_project_root(), cli.backend);
        return;
    }

    // Handle REPL mode
    if cli.interactive {
        run_repl_mode(&find_project_root(), cli.backend, cli.verbose);
        return;
    }

    // Execute files
    run_files(&find_project_root(), &cli.files, cli.verbose, cli.time, cli.backend);
}

/// Find the project root directory
fn find_project_root() -> std::path::PathBuf {
    // Check PETTA_PATH environment variable
    if let Ok(path) = std::env::var("PETTA_PATH") {
        let p = Path::new(&path);
        if p.join("prolog").join("metta.pl").exists() {
            return p.to_path_buf();
        }
    }

    // Check current directory
    let cwd = std::env::current_dir().unwrap_or_else(|_| Path::new(".").to_path_buf());
    if cwd.join("prolog").join("metta.pl").exists() {
        return cwd;
    }

    // Check executable parent directory
    if let Ok(exe) = std::env::current_exe() {
        if let Some(dir) = exe.parent() {
            if dir.join("prolog").join("metta.pl").exists() {
                return dir.to_path_buf();
            }
            if let Some(parent) = dir.parent() {
                if parent.join("prolog").join("metta.pl").exists() {
                    return parent.to_path_buf();
                }
            }
        }
    }

    // Walk up directory tree
    let mut current = cwd.clone();
    loop {
        if current.join("prolog").join("metta.pl").exists() {
            return current;
        }
        if !current.pop() {
            break;
        }
    }

    // Fallback with warning
    eprintln!(
        "{}: Could not locate PeTTa installation (prolog/metta.pl not found).",
        red("Warning")
    );
    eprintln!("Set PETTA_PATH or run from a directory containing prolog/metta.pl.");
    cwd
}

/// Run demo examples
fn run_demo(project_root: &Path, backend: BackendArg) {
    use petta::PeTTaEngine;

    let config =
        petta::EngineConfig::new(project_root).verbose(false).backend(backend.to_backend());

    match PeTTaEngine::with_config(&config) {
        Ok(mut engine) => {
            println!(
                "{}\n{}\n===========",
                cyan("PeTTa Demo"),
                yellow(&format!("{} backend", backend))
            );

            for (name, code) in &[
                ("Identity", "(= (myid $x) $x) !(myid 42)"),
                ("Arithmetic", "!(+ 1 2)"),
                ("Boolean", "!(and true false)"),
            ] {
                match engine.process_metta_string(code) {
                    Ok(results) => {
                        println!("\n{}:", cyan(name));
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
            }
            println!("\n{}.", green("Done"));
        }
        Err(e) => {
            eprintln!("Failed to initialize PeTTa engine: {}", e);
            std::process::exit(1);
        }
    }
}

/// Filter and validate file paths
fn filter_valid_paths(files: &[String]) -> Vec<std::path::PathBuf> {
    files
        .iter()
        .map(std::path::PathBuf::from)
        .filter(|p| {
            if !p.exists() {
                eprintln!("Error: file not found: {}", p.display());
                false
            } else {
                true
            }
        })
        .collect()
}

/// Execute MeTTa files
fn run_engine_files(
    project_root: &Path,
    paths: &[std::path::PathBuf],
    verbose: bool,
    backend: BackendArg,
) -> Result<(), ()> {
    use petta::PeTTaEngine;

    let config =
        petta::EngineConfig::new(project_root).verbose(verbose).backend(backend.to_backend());

    let mut engine = match PeTTaEngine::with_config(&config) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("{}", red(&e.to_string()));
            return Err(());
        }
    };

    let mut had_failure = false;
    for path in paths {
        match engine.load_metta_file(path) {
            Ok(results) => {
                let stderr = engine.stderr_output();
                if !stderr.is_empty() {
                    eprintln!("{}", red(&stderr));
                }
                for r in &results {
                    println!("{}", r.value);
                }
            }
            Err(e) => {
                let stderr = engine.stderr_output();
                if !stderr.is_empty() {
                    eprintln!("{}:\n{}", red("Prolog stderr"), red(&stderr));
                }
                eprintln!("{} {}: {}", red("Error processing"), path.display(), e);
                had_failure = true;
            }
        }
    }

    if had_failure {
        Err(())
    } else {
        Ok(())
    }
}

/// Main file execution function
fn run_files(
    project_root: &Path,
    files: &[String],
    verbose: bool,
    show_time: bool,
    backend: BackendArg,
) {
    let start = show_time.then(Instant::now);
    let paths = filter_valid_paths(files);

    if paths.is_empty() {
        std::process::exit(1);
    }

    let result = run_engine_files(project_root, &paths, verbose, backend.clone());

    if let Some(start) = start {
        let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;
        eprintln!("\n{} {} backend: {:.3}ms", yellow("Timing:"), backend, elapsed_ms);
    }

    if result.is_err() {
        std::process::exit(1);
    }
}

/// Run REPL mode
fn run_repl_mode(project_root: &Path, backend: BackendArg, verbose: bool) {
    let config = repl::ReplConfig::new(project_root).verbose(verbose).backend(backend.to_backend());
    repl::run_repl(&config);
}
