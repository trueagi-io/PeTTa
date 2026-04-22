//! PeTTa CLI — Command-line interface for MeTTa execution

use std::path::Path;
use std::time::Instant;
use petta::Backend;

#[cfg(feature = "repl")]
use rustyline::DefaultEditor;

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
        run_demo(&project_root, Backend::default());
        return;
    }

    let mut verbose = false;
    let mut show_time = false;
    let mut backend = Backend::default();
    let mut run_repl_mode = false;
    let mut files: Vec<String> = Vec::new();

    let mut i = 0;
    while i < args.len() {
        let arg = &args[i];
        match arg.as_str() {
            "-v" | "--verbose" => verbose = true,
            "-t" | "--time" => show_time = true,
            "--repl" | "-i" | "--interactive" => run_repl_mode = true,
            "-b" | "--backend" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: --backend requires an argument (mork or prolog)");
                    std::process::exit(1);
                }
                match args[i].to_lowercase().as_str() {
                    "mork" | "m" => backend = Backend::Mork,
                    "prolog" | "p" | "swipl" | "s" => backend = Backend::Swipl,
                    _ => {
                        eprintln!("Error: unknown backend '{}'. Use 'mork' or 'prolog'", args[i]);
                        std::process::exit(1);
                    }
                }
            }
            _ => files.push(arg.clone()),
        }
        i += 1;
    }

    if files.is_empty() || run_repl_mode {
        #[cfg(feature = "repl")]
        {
            if run_repl_mode {
                eprintln!("PeTTa: MeTTa language (Rust + MORK/SWI-Prolog backends)");
                eprintln!("Usage: petta [-v] [-t] [-b mork|prolog] [--repl] <file.metta> ...");
                eprintln!("       petta                    (run demo)");
                eprintln!("       petta --repl             (start interactive REPL)");
                eprintln!("  -v, --verbose    Show debug output");
                eprintln!("  -t, --time       Show execution time");
                eprintln!("  -b, --backend    Backend to use: mork (default) or prolog");
                eprintln!("  -i, --repl       Start interactive REPL");
                run_repl(&project_root, backend);
                return;
            }
            if files.is_empty() {
                run_demo(&project_root, backend);
                return;
            }
        }
        #[cfg(not(feature = "repl"))]
        {
            if files.is_empty() {
                run_demo(&project_root, backend);
                return;
            }
        }
    }

    run_files(&project_root, &files, verbose, show_time, backend);
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
    if let Ok(exe) = std::env::current_exe()
        && let Some(dir) = exe.parent()
    {
        if dir.join("prolog").join("metta.pl").exists() {
            return dir.to_path_buf();
        }
        if let Some(parent) = dir.parent()
            && parent.join("prolog").join("metta.pl").exists()
        {
            return parent.to_path_buf();
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

fn run_demo(project_root: &Path, backend: Backend) {
    use petta::PeTTaEngine;
    let config = petta::EngineConfig::new(project_root)
        .verbose(false)
        .backend(backend);
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

fn run_engine_files(project_root: &Path, paths: &[std::path::PathBuf], verbose: bool, backend: Backend) -> Result<(), ()> {
    use petta::PeTTaEngine;
    let config = petta::EngineConfig::new(project_root)
        .verbose(verbose)
        .backend(backend);
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

fn run_files(project_root: &Path, files: &[String], verbose: bool, show_time: bool, backend: Backend) {
    let start = show_time.then(Instant::now);
    let paths = filter_valid_paths(files);
    if paths.is_empty() {
        std::process::exit(1);
    }
    let result = run_engine_files(project_root, &paths, verbose, backend);
    if let Some(start) = start {
        let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;
        eprintln!(
            "\n{} {} backend: {:.3}ms",
            yellow("Timing:"),
            backend,
            elapsed_ms
        );
    }
    if result.is_err() {
        std::process::exit(1);
    }
}

#[cfg(feature = "repl")]
fn run_repl(project_root: &Path, backend: Backend) {
    use petta::PeTTaEngine;

    let config = petta::EngineConfig::new(project_root)
        .verbose(false)
        .backend(backend);
    let mut engine = match PeTTaEngine::with_config(&config) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("Failed to initialize PeTTa engine: {}", e);
            std::process::exit(1);
        }
    };

    let mut rl = DefaultEditor::new().unwrap();
    rl.load_history("/tmp/petta_history.txt").ok();

    println!("{} - {} backend", cyan("PeTTa REPL"), yellow(&backend.to_string()));
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

                if line == ":quit" || line == ":q" {
                    println!("{}", green("Goodbye!"));
                    break;
                }

                if line == ":help" || line == ":h" {
                    print_help();
                    continue;
                }

                if line == ":clear" || line == ":cl" {
                    print!("{}[2J{}[1;1H", 27 as char, 27 as char);
                    println!("{} - {} backend", cyan("PeTTa REPL"), yellow(&backend.to_string()));
                    continue;
                }

                if line == ":backend" || line == ":b" {
                    println!("Current backend: {}", yellow(&backend.to_string()));
                    continue;
                }

                if line.starts_with(":load ") || line.starts_with(":l ") {
                    let path = line.trim_start_matches(":load ").trim_start_matches(":l ").trim();
                    let path_buf = std::path::PathBuf::from(path);
                    if !path_buf.exists() {
                        eprintln!("{}: file not found: {}", red("Error"), path);
                        continue;
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
                    continue;
                }

                if line == ":info" || line == ":i" {
                    println!("Backend: {}", yellow(&backend.to_string()));
                    println!("Engine alive: {}", green(if engine.is_alive() { "yes" } else { "no" }));
                    continue;
                }

                if line.starts_with(':') {
                    eprintln!("{}: unknown command '{}'. Type {} for help.",
                        red("Error"), line, cyan(":help"));
                    continue;
                }

                match engine.process_metta_string(line) {
                    Ok(results) => {
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

    rl.save_history("/tmp/petta_history.txt").ok();
}

#[cfg(feature = "repl")]
fn print_help() {
    println!("{}:", cyan("Available Commands"));
    println!("  {:<12} - Show this help message", cyan(":help"));
    println!("  {:<12} - Quit the REPL", cyan(":quit"));
    println!("  {:<12} - Clear the screen", cyan(":clear"));
    println!("  {:<12} - Show engine info", cyan(":info"));
    println!("  {:<12} - Show current backend", cyan(":backend"));
    println!("  {:<12} - Load a MeTTa file", cyan(":load <file>"));
    println!();
    println!("{}:", cyan("Examples"));
    println!("  {:<12} - Evaluate expression", cyan("!(+ 1 2)"));
    println!("  {:<12} - Define a rule", cyan("(= (foo $x) $x)"));
    println!();
    println!("{}:", cyan("CLI Options"));
    println!("  {:<12} - Enable verbose output", cyan("-v"));
    println!("  {:<12} - Show execution time", cyan("-t"));
    println!("  {:<12} - Set backend (mork/prolog)", cyan("-b mork"));
}
