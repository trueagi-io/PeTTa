//! Differential testing harness for backend parity
//!
//! This module provides tools to ensure that different backends
//! produce identical results for the same input.

use crate::engine::{Backend, EngineConfig};
use crate::PeTTaEngine;
use std::path::Path;

/// Result of running a test on a backend
#[derive(Debug, Clone, PartialEq)]
pub struct BackendResult {
    pub backend: Backend,
    pub output: Result<String, String>,
    pub execution_time_ms: u128,
}

/// Test case for differential testing
#[derive(Debug, Clone)]
pub struct DifferentialTest {
    pub name: &'static str,
    pub metta_code: String,
    pub expected_success: bool,
}

impl DifferentialTest {
    pub fn new(name: &'static str, code: &'static str) -> Self {
        Self {
            name,
            metta_code: code.to_string(),
            expected_success: true,
        }
    }

    pub fn expect_failure(mut self) -> Self {
        self.expected_success = false;
        self
    }
}

/// Run a differential test across all available backends
pub fn run_differential_test(
    test: &DifferentialTest,
    backends: &[Backend],
    project_root: &Path,
) -> Result<Vec<BackendResult>, String> {
    let mut results = Vec::new();

    for &backend in backends {
        let config = EngineConfig::new(project_root)
            .backend(backend)
            .verbose(false);

        let mut engine = match PeTTaEngine::with_config(&config) {
            Ok(e) => e,
            Err(e) => {
                results.push(BackendResult {
                    backend,
                    output: Err(format!("Failed to initialize: {:?}", e)),
                    execution_time_ms: 0,
                });
                continue;
            }
        };

        let start = std::time::Instant::now();
        let output = engine.process_metta_string(&test.metta_code);
        let elapsed = start.elapsed();

        let result = match output {
            Ok(results) => {
                let output_str = results
                    .iter()
                    .map(|r| r.value.clone())
                    .collect::<Vec<_>>()
                    .join("\n");
                Ok(output_str)
            }
            Err(e) => Err(format!("{:?}", e)),
        };

        results.push(BackendResult {
            backend,
            output: result.map_err(|e| e.to_string()),
            execution_time_ms: elapsed.as_millis(),
        });
    }

    Ok(results)
}

/// Compare results from different backends
pub fn compare_results(results: &[BackendResult]) -> bool {
    if results.is_empty() {
        return true;
    }

    let first = match &results[0].output {
        Ok(s) => s,
        Err(_) => return false,
    };

    results.iter().all(|r| match &r.output {
        Ok(s) => s == first,
        Err(_) => false,
    })
}

/// Assert that all backends produce the same result
pub fn assert_backend_parity(
    test: &DifferentialTest,
    project_root: &Path,
) -> Result<(), String> {
    let backends = vec![Backend::Mork, Backend::Swipl];
    let results = run_differential_test(test, &backends, project_root)?;

    if !compare_results(&results) {
        let mut msg = format!("Backend parity failed for test '{}':\n", test.name);
        for result in &results {
            msg.push_str(&format!(
                "  {:?}: {:?}\n",
                result.backend, result.output
            ));
        }
        return Err(msg);
    }

    Ok(())
}

/// Test suite for backend parity
pub struct ParityTestSuite {
    tests: Vec<DifferentialTest>,
    project_root: std::path::PathBuf,
}

impl ParityTestSuite {
    pub fn new(project_root: &Path) -> Self {
        Self {
            tests: Vec::new(),
            project_root: project_root.to_path_buf(),
        }
    }

    pub fn add_test(&mut self, test: DifferentialTest) {
        self.tests.push(test);
    }

    pub fn run_all(&self) -> TestSuiteResult {
        let mut passed = 0;
        let mut failed = 0;
        let mut failures = Vec::new();

        for test in &self.tests {
            match assert_backend_parity(test, &self.project_root) {
                Ok(_) => passed += 1,
                Err(e) => {
                    failed += 1;
                    failures.push((test.name.to_string(), e));
                }
            }
        }

        TestSuiteResult {
            passed,
            failed,
            failures,
        }
    }
}

#[derive(Debug)]
pub struct TestSuiteResult {
    pub passed: usize,
    pub failed: usize,
    pub failures: Vec<(String, String)>,
}

impl std::fmt::Display for TestSuiteResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Test Suite Results:")?;
        writeln!(f, "  Passed: {}", self.passed)?;
        writeln!(f, "  Failed: {}", self.failed)?;

        if !self.failures.is_empty() {
            writeln!(f, "\nFailures:")?;
            for (name, error) in &self.failures {
                writeln!(f, "  - {}: {}", name, error)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    #[test]
    #[ignore]
    fn test_backend_parity_simple() {
        let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
        let project_root = Path::new(&manifest_dir);

        let test = DifferentialTest::new("simple_addition", "!(+ 1 2)");
        let result = assert_backend_parity(&test, project_root);

        assert!(
            result.is_ok(),
            "Backend parity test failed: {:?}",
            result.err().unwrap()
        );
    }

    #[test]
    #[ignore]
    fn test_backend_parity_fibonacci() {
        let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
        let project_root = Path::new(&manifest_dir);

        let test = DifferentialTest::new(
            "fibonacci",
            r#"
            (= (fib 0) 0)
            (= (fib 1) 1)
            (= (fib n) (+ (fib (- n 1)) (fib (- n 2))))
            !(fib 5)
        "#,
        );

        let result = assert_backend_parity(&test, project_root);
        assert!(result.is_ok(), "Fibonacci test failed");
    }
}
