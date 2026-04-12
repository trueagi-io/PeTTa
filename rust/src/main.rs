//! PeTTa CLI — Command-line interface for MeTTa execution

use std::path::Path;
use std::time::Instant;

/// Apply ANSI color code to string
fn color(s: &str, code: u8) -> String {
    format!(concat!("\x1b[{}m{}\x1b[0m"), code, s)
}
fn green(s: &str) -> String {
    color(s, 32)
}
fn red(s: &str) -> String {
    color(s, 31)
}
fn yellow(s: &str) -> String {
    color(s, 33)
}
fn cyan(s: &str) -> String {
    color(s, 36)
}

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

fn run_files(project_root: &Path, files: &[&String], verbose: bool) {
    use petta::PeTTaEngine;
    let paths: Vec<&Path> = files
        .iter()
        .map(|f| Path::new(f))
        .filter(|p| {
            if !p.exists() {
                eprintln!("Error: file not found: {}", p.display());
                false
            } else {
                true
            }
        })
        .collect();
    if paths.is_empty() {
        std::process::exit(1);
    }
    let mut engine = match PeTTaEngine::new(project_root, verbose) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("{}", red(&e.to_string()));
            std::process::exit(1);
        }
    };
    let mut had_failure = false;
    for path in &paths {
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
    let paths: Vec<&Path> = files
        .iter()
        .map(|f| Path::new(f))
        .filter(|p| {
            if !p.exists() {
                eprintln!("Error: file not found: {}", p.display());
                false
            } else {
                true
            }
        })
        .collect();
    if paths.is_empty() {
        std::process::exit(1);
    }
    let mut engine = match PeTTaEngine::new(project_root, verbose) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("{}", red(&e.to_string()));
            std::process::exit(1);
        }
    };
    let engine_time = start.elapsed();
    let mut had_failure = false;
    for path in &paths {
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
    eprintln!(
        "\n{} engine: {:.3}ms, total: {:.3}ms",
        yellow("Timing:"),
        engine_time.as_secs_f64() * 1000.0,
        elapsed.as_secs_f64() * 1000.0
    );
    if had_failure {
        std::process::exit(1);
    }
}
