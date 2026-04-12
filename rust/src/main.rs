//! PeTTa CLI — Command-line interface for MeTTa execution

use std::path::Path;
use std::time::Instant;

// ---------------------------------------------------------------------------
// ANSI color helpers (no external dependency)
// ---------------------------------------------------------------------------

fn green(s: &str) -> String { format!("\x1b[32m{}\x1b[0m", s) }
fn red(s: &str) -> String { format!("\x1b[31m{}\x1b[0m", s) }
fn yellow(s: &str) -> String { format!("\x1b[33m{}\x1b[0m", s) }
fn cyan(s: &str) -> String { format!("\x1b[36m{}\x1b[0m", s) }

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let project_root = find_project_root();

    if args.is_empty() {
        run_demo(&project_root);
        return;
    }

    let verbose = args.iter().any(|a| a == "-v" || a == "--verbose");
    let show_time = args.iter().any(|a| a == "-t" || a == "--time");
    let files: Vec<&String> = args
        .iter()
        .filter(|a| *a != "-v" && *a != "--verbose" && *a != "-t" && *a != "--time")
        .collect();

    if files.is_empty() {
        eprintln!("PeTTa: MeTTa language implementation (Rust + SWI-Prolog embedded)");
        eprintln!("Usage: petta [-v] [-t] <file.metta> [file2.metta] ...");
        eprintln!("       petta                    (run demo)");
        eprintln!("");
        eprintln!("  -v, --verbose    Show debug output");
        eprintln!("  -t, --time       Show execution time");
        std::process::exit(1);
    }

    if show_time {
        run_files_timed(&project_root, &files, verbose);
    } else {
        run_files(&project_root, &files, verbose);
    }
}

fn find_project_root() -> std::path::PathBuf {
    let cwd = std::env::current_dir().unwrap_or_else(|_| Path::new(".").to_path_buf());
    if cwd.join("src").join("metta.pl").exists() {
        return cwd;
    }
    if let Ok(exe) = std::env::current_exe() {
        if let Some(dir) = exe.parent() {
            if dir.join("src").join("metta.pl").exists() {
                return dir.to_path_buf();
            }
            if let Some(parent) = dir.parent() {
                if parent.join("src").join("metta.pl").exists() {
                    return parent.to_path_buf();
                }
            }
        }
    }
    if let Ok(path) = std::env::var("PETTA_PATH") {
        let p = Path::new(&path);
        if p.join("src").join("metta.pl").exists() {
            return p.to_path_buf();
        }
    }
    let mut current = cwd.clone();
    loop {
        if current.join("src").join("metta.pl").exists() {
            return current;
        }
        if !current.pop() {
            break;
        }
    }
    cwd
}

// ---------------------------------------------------------------------------
// Demo
// ---------------------------------------------------------------------------

fn run_demo(project_root: &Path) {
    use petta::PeTTaEngine;

    match PeTTaEngine::new(project_root, false) {
        Ok(engine) => {
            println!("{}\n{}\n===========",
                     cyan("PeTTa Demo"),
                     yellow("Rust + SWI-Prolog (embedded)"));
            let cases = [
                ("Identity", "(= (myid $x) $x) !(myid 42)"),
                ("Arithmetic", "!(+ 1 2)"),
                ("Boolean", "!(and true false)"),
            ];
            for (name, code) in &cases {
                match engine.process_metta_string(code) {
                    Ok(results) => {
                        println!("\n{}:", cyan(name));
                        for r in &results {
                            if let Some(typed) = r.parsed_value() {
                                println!("  {}  ({:?})", r.value, typed);
                            } else {
                                println!("  {}", r.value);
                            }
                        }
                    }
                    Err(e) => eprintln!("  {}", red(&e.to_string())),
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

// ---------------------------------------------------------------------------
// File execution
// ---------------------------------------------------------------------------

fn run_files(project_root: &Path, files: &[&String], verbose: bool) {
    use petta::PeTTaEngine;

    let engine = match PeTTaEngine::new(project_root, verbose) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("{}", red(&e.to_string()));
            std::process::exit(1);
        }
    };

    let mut had_failure = false;
    for file_path in files {
        let path = Path::new(file_path);
        if !path.exists() {
            eprintln!("Error: file not found: {}", path.display());
            had_failure = true;
            continue;
        }
        match engine.load_metta_file(path) {
            Ok(results) => {
                for r in &results {
                    println!("{}", r.value);
                }
            }
            Err(e) => {
                eprintln!("Error processing {}: {}", path.display(), e);
                had_failure = true;
            }
        }
    }
    if had_failure {
        std::process::exit(1);
    }
}

fn run_files_timed(project_root: &Path, files: &[&String], verbose: bool) {
    use petta::PeTTaEngine;

    let start = Instant::now();
    let engine = match PeTTaEngine::new(project_root, verbose) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("{}", red(&e.to_string()));
            std::process::exit(1);
        }
    };
    let engine_time = start.elapsed();

    let mut had_failure = false;
    for file_path in files {
        let path = Path::new(file_path);
        if !path.exists() {
            eprintln!("Error: file not found: {}", path.display());
            had_failure = true;
            continue;
        }
        match engine.load_metta_file(path) {
            Ok(results) => {
                for r in &results {
                    println!("{}", r.value);
                }
            }
            Err(e) => {
                eprintln!("Error processing {}: {}", path.display(), e);
                had_failure = true;
            }
        }
    }

    let elapsed = start.elapsed();
    eprintln!("\n{} engine: {:.3}ms, total: {:.3}ms",
              yellow("Timing:"),
              engine_time.as_secs_f64() * 1000.0,
              elapsed.as_secs_f64() * 1000.0);

    if had_failure {
        std::process::exit(1);
    }
}
