//! Integration tests for MeTTa example files.
//!
//! - Smoke tests run on every `cargo test` for quick validation.
//! - Full suite runs all 138 examples via the release CLI binary
//!   and should be invoked explicitly: `cargo test --test examples full`
//! - `test.sh` delegates to this full suite test.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Files to skip (interactive, require external deps, or computationally infeasible).
const SKIP_LIST: &[&str] = &[
    "repl.metta",
    "llm_cities.metta",
    "torch.metta",
    "greedy_chess.metta",
    "git_import2.metta",
    "matespacefast.metta",
    "fibadd.metta", // fib(30) without memoization: O(2^30) calls
];

fn project_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf()
}

fn discover_examples() -> Vec<PathBuf> {
    let dir = project_root().join("examples");
    let mut files: Vec<_> = std::fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("Cannot read {:?}: {}", dir, e))
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|ext| ext == "metta"))
        .filter(|p| {
            let name = p.file_name().unwrap().to_str().unwrap();
            !SKIP_LIST.contains(&name)
        })
        .collect();
    files.sort();
    files
}

/// Run a single example via the release CLI binary.
/// Returns (success, stdout+stderr).
fn run_example_cli(file: &Path) -> (bool, String) {
    let output = Command::new(project_root().join("target/release/petta"))
        .arg(file)
        .output()
        .expect("Failed to execute petta binary (build with `cargo build --release` first)");
    let combined = format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    (output.status.success(), combined)
}

// ---------------------------------------------------------------------------
// Smoke tests — always run, quick validation
// ---------------------------------------------------------------------------

#[test]
fn test_smoke_identity() {
    let path = project_root().join("examples/identity.metta");
    let (ok, _) = run_example_cli(&path);
    assert!(ok, "identity example failed");
}

#[test]
fn test_smoke_fib() {
    let path = project_root().join("examples/fib.metta");
    let (ok, _) = run_example_cli(&path);
    assert!(ok, "fib example failed");
}

#[test]
fn test_smoke_math() {
    let path = project_root().join("examples/math.metta");
    let (ok, _) = run_example_cli(&path);
    assert!(ok, "math example failed");
}

// ---------------------------------------------------------------------------
// Full suite — runs all examples via release CLI, with per-file progress.
// Marked #[ignore] so it doesn't slow down `cargo test` by default.
// Run explicitly: cargo test --test examples -- --ignored --nocapture
// ---------------------------------------------------------------------------

/// Run all examples through the release CLI with per-file progress.
/// Each example is independent — a failure in one doesn't affect others.
#[test]
#[ignore = "slow; run explicitly with: cargo test --test examples -- --ignored --nocapture"]
fn test_full_suite() {
    let files = discover_examples();
    let total = files.len();
    eprintln!("Found {} example files", total);

    let mut passed = 0;
    let mut errors: Vec<(String, String)> = Vec::new();

    for (i, file) in files.iter().enumerate() {
        let name = file.file_name().unwrap().to_str().unwrap();
        let (ok, output) = run_example_cli(file);

        if ok {
            eprintln!("[{}/{}] ✅ {}", i + 1, total, name);
            passed += 1;
        } else {
            eprintln!("[{}/{}] ❌ {}", i + 1, total, name);
            // Truncate output for readability
            let detail = output.lines().take(20).collect::<Vec<_>>().join("\n");
            errors.push((name.to_string(), detail));
        }
    }

    eprintln!("\n{}/{} examples passed", passed, total);
    if !errors.is_empty() {
        eprintln!("\nFailed examples:");
        for (name, err) in &errors {
            eprintln!("  {}:\n{}", name, err);
        }
    }

    assert!(errors.is_empty(), "{} example(s) failed", errors.len());
}

// ---------------------------------------------------------------------------
// Discovery verification
// ---------------------------------------------------------------------------

#[test]
fn test_examples_discovery() {
    let files = discover_examples();
    assert!(!files.is_empty(), "No example files found");
    eprintln!("Found {} example files", files.len());
}
