//! PeTTa CLI - Production MeTTa Runtime

use clap::Parser;
use petta::{
    Backend, Cli, EngineConfig, PeTTaEngine, ReplConfig, run_repl,
    utils::{cyan, red},
};
use std::path::Path;

fn main() {
    let args = Cli::parse();

    if args.files.is_empty() && !args.interactive {
        print_banner(&args);
        run_demo();
        return;
    }

    if args.interactive {
        run_repl_mode(&args);
        return;
    }

    run_files(&args);
}

fn print_banner(args: &Cli) {
    println!("{} {}", cyan("⚡"), cyan("PeTTa v0.5.0"));
    println!("{}", cyan("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"));
    println!("{} {}", cyan("🧠"), cyan("Production MeTTa Runtime"));
    if args.verbose {
        println!("{} {}", cyan("Backend:"), args.backend);
        println!("{} {}", cyan("Output:"), args.output_format);
    }
}

fn run_demo() {
    println!("\n{} {}", cyan("⚡"), cyan("PeTTa Demo"));
    println!("Backend: Swipl");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

    let demos = [
        ("Identity", "(= (myid $x) $x) !(myid 42)"),
        ("Arithmetic", "!(+ 1 2)"),
        ("Boolean", "!(and true false)"),
    ];

    for (name, code) in &demos {
        println!("{name}: {code}");
    }
    println!("\n✓ Done");
}

fn run_repl_mode(args: &Cli) {
    let config = ReplConfig::new(".")
        .verbose(args.verbose)
        .backend(parse_backend(&args.backend.to_string()));
    run_repl(&config);
}

fn run_files(args: &Cli) {
    let timing = args.time.then(std::time::Instant::now);
    let backend = parse_backend(&args.backend.to_string());

    // Separate actual files from key=value config overrides
    let mut files = Vec::new();
    let mut extra_args = Vec::new();
    for arg in &args.files {
        if arg.contains('=') && !Path::new(arg).exists() {
            extra_args.push(arg.clone());
        } else {
            files.push(arg.clone());
        }
    }

    let mut config = EngineConfig::new(Path::new("."))
        .verbose(args.verbose)
        .backend(backend);
    config.extra_args = extra_args;

    let mut engine = match PeTTaEngine::with_config(&config) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("{} Failed to initialize engine: {}", red("✗"), e);
            std::process::exit(1);
        }
    };

    let mut exit_code = 0;
    for file in &files {
        let path = Path::new(file);
        if !path.exists() {
            eprintln!("{} File not found: {}", red("✗"), file);
            exit_code = 1;
            continue;
        }

        match engine.load_metta_file(path) {
            Ok(results) => {
                for r in &results {
                    println!("{}", r.value);
                }
            }
            Err(e) => {
                eprintln!("{} Error loading {}: {}", red("✗"), file, e);
                exit_code = 1;
            }
        }
    }

    if let Some(start) = timing {
        eprintln!("\n⏱ Timing: {:.3}ms", start.elapsed().as_millis() as f64);
    }

    if exit_code != 0 {
        std::process::exit(exit_code);
    }
}

fn parse_backend(s: &str) -> Backend {
    match s.to_lowercase().as_str() {
        "mork" => Backend::Mork,
        "swipl" | "prolog" => Backend::Swipl,
        _ => Backend::Swipl,
    }
}
