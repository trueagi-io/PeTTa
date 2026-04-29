//! Integration tests for backend parity
//!
//! These tests ensure that both MORK and SWI-Prolog backends
//! produce identical results for the same MeTTa code.

use petta::differential::{DifferentialTest, ParityTestSuite};
use std::env;
use std::path::Path;

#[test]
#[ignore]
fn test_differential_simple_arithmetic() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
    let project_root = Path::new(&manifest_dir);

    let test = DifferentialTest::new("simple_addition", "!(+ 1 2)");
    let mut suite = ParityTestSuite::new(project_root);
    suite.add_test(test);

    let result = suite.run_all();
    assert!(
        result.failed == 0,
        "Differential test failed: {}",
        result
    );
}

#[test]
#[ignore]
fn test_differential_boolean_logic() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
    let project_root = Path::new(&manifest_dir);

    let test = DifferentialTest::new("boolean_and", "!(and true false)");
    let mut suite = ParityTestSuite::new(project_root);
    suite.add_test(test);

    let result = suite.run_all();
    assert!(
        result.failed == 0,
        "Boolean logic test failed: {}",
        result
    );
}

#[test]
#[ignore]
fn test_differential_comparisons() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
    let project_root = Path::new(&manifest_dir);

    let tests = vec![
        DifferentialTest::new("less_than", "!(< 1 2)"),
        DifferentialTest::new("greater_than", "!(> 2 1)"),
        DifferentialTest::new("equality", "!(== 5 5)"),
    ];

    let mut suite = ParityTestSuite::new(project_root);
    for test in tests {
        suite.add_test(test);
    }

    let result = suite.run_all();
    assert!(
        result.failed == 0,
        "Comparison tests failed: {}",
        result
    );
}

#[test]
#[ignore]
fn test_differential_fibonacci() {
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

    let mut suite = ParityTestSuite::new(project_root);
    suite.add_test(test);

    let result = suite.run_all();
    assert!(
        result.failed == 0,
        "Fibonacci test failed: {}",
        result
    );
}
