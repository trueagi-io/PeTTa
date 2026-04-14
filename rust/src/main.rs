//! PeTTa CLI — Command-line interface for MeTTa execution

use std::path::Path;
use std::time::Instant;

fn ansi_color(s: &str, code: u8) -> String {
    format!("\x1b[{}m{}\x1b[0m", code, s)
}
fn green(s: &str) -> String { ansi_color(s, 32) }
fn red(s: &str) -> String { ansi_color(s, 31) }
fn yellow(s: &str) -> String { ansi_color(s, 33) }
fn cyan(s: &str) -> String { ansi_color(s, 36) }

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
        eprintln!("PeTTa: MeTTa language (Rust + persistent SWI-Prolog)");
        eprintln!("Usage: petta [-v] [-t] <file.metta> [file2.metta] ...");
        eprintln!("       petta                    (run demo)");
        eprintln!("  -v, --verbose    Show debug output");
        eprintln!("  -t, --time       Show execution time");
        std::process::exit(1);
    }

    run_files(&project_root, &files, verbose, show_time);
}

fn find_project_root() -> std::path::PathBuf {
    // Check if PETTA_PATH environment variable is set
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

    // Check executable parent directory and its parent
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

    // Walk up the directory tree looking for prolog/metta.pl
    let mut current = cwd.clone();
    loop {
        if current.join("prolog").join("metta.pl").exists() {
            return current;
        }
        if !current.pop() {
            break;
        }
    }

    // Fallback to cwd with a warning
    eprintln!(
        "{}: Could not locate PeTTa installation (prolog/metta.pl not found).",
        red("Warning")
    );
    eprintln!("Set PETTA_PATH or run from a directory containing prolog/metta.pl.");
    cwd
}

fn run_demo(project_root: &Path) {
    use petta::PeTTaEngine;
    match PeTTaEngine::new(project_root, false) {
        Ok(mut engine) => {
            println!(
                "{}\n{}\n===========",
                cyan("PeTTa Demo"),
                yellow("Persistent SWI-Prolog engine")
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
                                println!("  {}  ({:?})", r.value, t);
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

fn filter_valid_paths<'a>(files: &[&'a String]) -> Vec<&'a Path> {
    files
        .iter()
        .map(|f| Path::new(f.as_str()))
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

fn run_engine_files(project_root: &Path, paths: &[&Path], verbose: bool) -> Result<(), ()> {
    use petta::PeTTaEngine;
    let mut engine = match PeTTaEngine::new(project_root, verbose) {
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
                // Surface Prolog stderr on error — this often contains the crash reason
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

fn run_files(project_root: &Path, files: &[&String], verbose: bool, show_time: bool) {
    let start = show_time.then(Instant::now);
    let paths = filter_valid_paths(files);
    if paths.is_empty() {
        std::process::exit(1);
    }
    let result = run_engine_files(project_root, &paths, verbose);
    if let Some(start) = start {
        let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;
        eprintln!(
            "\n{} engine: {:.3}ms",
            yellow("Timing:"),
            elapsed_ms
        );
    }
    if result.is_err() {
        std::process::exit(1);
    }
}
