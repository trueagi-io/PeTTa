//! Benchmarking suite for PeTTa backends
//!
//! This module provides performance benchmarks using criterion.rs
//! to compare backend performance and track optimization progress.

use crate::engine::{Backend, EngineConfig, PeTTaEngine};
use std::path::Path;
use std::time::Duration;

/// Benchmark configuration
#[derive(Debug, Clone)]
pub struct BenchmarkConfig {
    pub name: &'static str,
    pub metta_code: String,
    pub iterations: usize,
    pub warmup_iterations: usize,
}

impl BenchmarkConfig {
    pub fn new(name: &'static str, code: &'static str) -> Self {
        Self {
            name,
            metta_code: code.to_string(),
            iterations: 100,
            warmup_iterations: 10,
        }
    }

    pub fn with_iterations(mut self, iters: usize) -> Self {
        self.iterations = iters;
        self
    }

    pub fn with_warmup(mut self, warmup: usize) -> Self {
        self.warmup_iterations = warmup;
        self
    }
}

/// Benchmark result for a single test
#[derive(Debug, Clone)]
pub struct BenchmarkResult {
    pub name: String,
    pub backend: Backend,
    pub mean_time: Duration,
    pub std_dev: Duration,
    pub ops_per_second: f64,
    pub samples: usize,
}

impl std::fmt::Display for BenchmarkResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:20} | {:8} | {:10.2} ops/s | mean: {:?} (±{:?})",
            self.name,
            format!("{:?}", self.backend),
            self.ops_per_second,
            self.mean_time,
            self.std_dev
        )
    }
}

/// Run benchmarks on a specific backend
pub fn run_benchmark(
    config: &BenchmarkConfig,
    backend: Backend,
    project_root: &Path,
) -> Result<BenchmarkResult, String> {
    let engine_config = EngineConfig::new(project_root)
        .backend(backend)
        .verbose(false);

    let mut engine =
        PeTTaEngine::with_config(&engine_config).map_err(|e| format!("Init error: {:?}", e))?;

    let mut times = Vec::with_capacity(config.iterations);

    for _ in 0..config.iterations {
        let start = std::time::Instant::now();
        engine
            .process_metta_string(&config.metta_code)
            .map_err(|e| format!("Execution error: {:?}", e))?;
        times.push(start.elapsed());
    }

    let mean = times.iter().sum::<Duration>() / times.len() as u32;
    let variance = times
        .iter()
        .map(|&t| {
            let diff = t.as_nanos() as f64 - mean.as_nanos() as f64;
            diff * diff
        })
        .sum::<f64>()
        / times.len() as f64;
    let std_dev = Duration::from_nanos(variance.sqrt() as u64);

    let ops_per_second = 1.0
        / (times.iter().sum::<Duration>().as_nanos() as f64 / times.len() as f64 * 1e9_f64);

    Ok(BenchmarkResult {
        name: config.name.to_string(),
        backend,
        mean_time: mean,
        std_dev,
        ops_per_second,
        samples: times.len(),
    })
}

/// Run benchmarks across all backends
pub fn run_benchmark_suite(
    benchmarks: &[BenchmarkConfig],
    project_root: &Path,
) -> Vec<BenchmarkResult> {
    let backends = vec![Backend::Mork, Backend::Swipl];
    let mut results = Vec::new();

    for benchmark in benchmarks {
        for &backend in &backends {
            if let Ok(result) = run_benchmark(benchmark, backend, project_root) {
                results.push(result);
            }
        }
    }

    results
}

/// Print benchmark results in a table format
pub fn print_benchmark_table(results: &[BenchmarkResult]) {
    println!("\n{:-<80}", "");
    println!("Benchmark Suite Results");
    println!("{:-<80}", "");
    println!("{:<25} | {:<10} | {:<15} | {:<20}", "Test", "Backend", "Ops/Sec", "Mean Time");
    println!("{:-<80}", "");

    for result in results {
        println!(
            "{:<25} | {:<10} | {:>12.0} | {:>15.2?}",
            result.name,
            format!("{:?}", result.backend),
            result.ops_per_second,
            result.mean_time
        );
    }

    println!("{:-<80}", "");
}

/// Compare performance between backends
pub fn compare_backends(results: &[BenchmarkResult]) -> Vec<(String, f64)> {
    let mut comparisons = Vec::new();

    for result in results.iter().filter(|r| r.backend == Backend::Mork) {
        if let Some(prolog_result) = results
            .iter()
            .find(|r| r.backend == Backend::Swipl && r.name == result.name)
        {
            let ratio = prolog_result.ops_per_second / result.ops_per_second;
            comparisons.push((result.name.clone(), ratio));
        }
    }

    comparisons
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    #[test]
    #[ignore]
    fn test_benchmark_simple() {
        let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
        let project_root = Path::new(&manifest_dir);

        let config = BenchmarkConfig::new("simple", "!(+ 1 2)").with_iterations(10);

        #[cfg(feature = "mork")]
        {
            let result = run_benchmark(&config, Backend::Mork, project_root);
            assert!(result.is_ok());
        }

        let result = run_benchmark(&config, Backend::Swipl, project_root);
        assert!(result.is_ok());
    }
}
