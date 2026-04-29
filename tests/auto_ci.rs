use std::env;
use std::process::Command;
// std::time::Duration imported previously but not needed in this test helper file.
// kept as a commented reference for potential future timeout tests.
// use std::time::Duration;

/// This integration test makes the repository "self-testing".
///
/// Behavior:
/// - If SWI-Prolog (`swipl`) is available on PATH, the test does nothing (other tests exercise SWI paths).
/// - If SWI is missing, it will attempt to run the test-suite compiled with the `mork` feature.
///   It will first try `cargo test --features mork`; if that fails and `rustup` is available,
///   it will attempt `rustup run nightly cargo test --features mork` to allow nightly-only features.
///
/// To avoid recursion (this test spawning `cargo test` which would run this test again),
/// the child cargo process is run with the environment variable `PETTA_AUTO_TEST_CHILD=1`.
/// When the variable is set this test returns immediately.
#[test]
fn auto_ci_orchestrator() {
    // Prevent recursion when we spawn cargo from within the test
    if env::var("PETTA_AUTO_TEST_CHILD").ok().as_deref() == Some("1") {
        // Child run: do nothing to avoid infinite recursion
        return;
    }

    // Quick check for swipl
    match Command::new("swipl").arg("--version").output() {
        Ok(output) if output.status.success() => {
            println!("auto-ci: detected swipl on PATH; no alternate backend required");
            return;
        }
        _ => {
            println!("auto-ci: swipl not found; attempting to run tests with `mork` feature");
        }
    }

    // Build & run tests with the mork feature in a child process. Set env var so child exits early.
    let run_cargo_with = |cmd: &str, args: &[&str]| -> Result<(), (i32, String)> {
        let mut c = Command::new(cmd);
        c.args(args).env("PETTA_AUTO_TEST_CHILD", "1").env("RUST_BACKTRACE", "1");
        // Inherit stdio so output is visible in CI logs
        let status = c.status().expect("failed to spawn child cargo process");
        if status.success() {
            Ok(())
        } else {
            let code = status.code().unwrap_or(-1);
            Err((code, format!("{} {:?} exited with {}", cmd, args, code)))
        }
    };

    // First try with the default toolchain
    match run_cargo_with("cargo", &["test", "--workspace", "--all", "--features", "mork"]) {
        Ok(()) => return,
        Err((_, _)) => {
            println!(
                "auto-ci: cargo test --features mork failed; trying 'rustup run nightly' fallback"
            );
        }
    }

    // Try using rustup nightly if available
    match Command::new("rustup").arg("--version").output() {
        Ok(output) if output.status.success() => {
            match run_cargo_with(
                "rustup",
                &["run", "nightly", "cargo", "test", "--workspace", "--all", "--features", "mork"],
            ) {
Ok(()) => {}
                Err((_, msg)) => panic!("auto-ci: nightly run failed: {}", msg),
            }
        }
        _ => {
            panic!(
                "auto-ci: swipl not found and rustup unavailable; cannot run mork tests automatically"
            )
        }
    }
}
